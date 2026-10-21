;;; nngmail-testdrive.el --- Interactive live test drive for nngmail  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov
;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; `M-x nngmail-testdrive' walks through the nngmail test plan against a
;; real Gmail account, doing the mechanical parts itself and asking you
;; to confirm what needs eyes (did the web UI change?  does the article
;; render?).
;;
;; Safety model
;; ------------
;; The drive NEVER modifies a message it did not create.  Every mutating
;; test operates inside a sandbox:
;;
;;   - a throwaway label, `nngmail-testdrive-label' (default
;;     "nngmail-testdrive"), created by the drive and deleted at the end;
;;   - messages the drive sends from you to you, with a recognisable
;;     subject prefix and an X-Nngmail-Testdrive header, filed under that
;;     label and removed from INBOX immediately on arrival.
;;
;; Marks, moves, copies, trash, expiry, permanent deletion, label rename
;; and label delete are all exercised on those messages only.  Your real
;; mail is only ever *read* (headers, one article displayed, live labels
;; compared).  Even with modifications approved, an operation on an
;; article that is not one of the drive's own is refused by a guard.
;;
;; Consent is asked once up front; without it every mutating test is
;; skipped and the drive is read-only.
;;
;; Results accumulate in *nngmail-testdrive* as an Org document you can
;; paste into a bug report: one heading per test with PASS / FAIL /
;; SKIP, notes, timings, and the tail of *nngmail-log* for failures.
;;
;; Answers at each step:  y = pass, n = fail, s = skip, q = quit (results
;; so far are kept).  `C-u M-x nngmail-testdrive' runs a single section.

;;; Code:

(require 'cl-lib)
(require 'nngmail)
(require 'nngmail-reload)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-group)
(require 'message)
(require 'mml)
(require 'range)
(declare-function nngmail-efrit-ensure-tools "nngmail-efrit")
(declare-function nngmail-efrit--tool-search "nngmail-efrit")
(declare-function nngmail-efrit--refs "nngmail-efrit")
(declare-function efrit-gnus--payload "efrit-gnus")
(declare-function efrit-gnus--submit "efrit-gnus")
(declare-function nngmail-background-kick "nngmail-background")
(declare-function nngmail-job-state "nngmail-background")
(declare-function nngmail-job-last-error "nngmail-background")
(defvar efrit-gnus-confirm)
(defvar nngmail-background)

(defgroup nngmail-testdrive nil
  "Live test drive for nngmail."
  :group 'nngmail)

(defcustom nngmail-testdrive-label "nngmail-testdrive"
  "Sandbox label.  Created at the start of the drive and deleted at the end.
Anything already carrying this label is treated as the drive's own."
  :type 'string)

(defcustom nngmail-testdrive-attachment nil
  "File to attach in the sending test, or nil for a generated one.
The drive never reaches for the user's own files: with nil it writes a
small text file of its own and deletes it afterwards."
  :type '(choice (const :tag "Generated" nil) file))

(defun nngmail-testdrive--attachment ()
  "The attachment for the sending test: a temp file the drive owns, or
`nngmail-testdrive-attachment' when set.  The caller deletes a temp one."
  (if nngmail-testdrive-attachment
      (cons nngmail-testdrive-attachment nil)
    (let ((file (make-temp-file "nngmail-testdrive-" nil ".txt"
                                (format "nngmail test drive attachment\n%s\n%s\n"
                                        (format-time-string "%F %T")
                                        (make-string 400 ?x)))))
      (cons file t))))

(defcustom nngmail-testdrive-subject-prefix "[nngmail-testdrive] "
  "Subject prefix for every message the drive sends."
  :type 'string)

;;;; State

(defvar nngmail-testdrive--server nil)
(defvar nngmail-testdrive--account nil)
(defvar nngmail-testdrive--method nil)
(defvar nngmail-testdrive--may-modify nil)
(defvar nngmail-testdrive--results nil "List of (SECTION NAME STATUS NOTE SECONDS).")
(defvar nngmail-testdrive--buffer "*nngmail-testdrive*")
(defvar nngmail-testdrive--own (make-hash-table :test #'eql)
  "Article numbers of messages the drive created.  Only these may be modified.")
(defvar nngmail-testdrive--label-id nil "Gmail id of the sandbox label.")

(define-error 'nngmail-testdrive-quit "test drive stopped")
(define-error 'nngmail-testdrive-guard "refused: article is not owned by the test drive")

;;;; Reporting

(defvar nngmail-testdrive-report-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map)
  "Keys in the test drive report: `q' closes its window.")

(define-minor-mode nngmail-testdrive-report-mode
  "Minor mode for the test drive report buffer: `q' closes the window.
The report is an Org buffer so it folds and pastes well; this only adds
the key, and makes the buffer read-only once the drive is over."
  :lighter " drive"
  :keymap nngmail-testdrive-report-mode-map)

(defun nngmail-testdrive--buf ()
  (let ((buf (get-buffer-create nngmail-testdrive--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (ignore-errors (org-mode)))
      (nngmail-testdrive-report-mode 1))
    buf))

(defvar nngmail-testdrive--layout nil
  "Window configuration at the start of the drive, restored after every step.
Hooks that run during a step -- `message-sent-hook' functions that
visit a diary file, Gnus opening summaries, `display-buffer' from
anywhere -- would otherwise rearrange the frame a little more with each
test.")

(defun nngmail-testdrive--show-report ()
  "Make sure the report is visible in its side window, without stealing focus."
  (let ((buf (nngmail-testdrive--buf)))
    (unless (get-buffer-window buf)
      (display-buffer buf '(display-buffer-in-side-window (side . right) (window-width . 0.4))))
    (when-let* ((w (get-buffer-window buf)))
      (with-current-buffer buf (set-window-point w (point-max))))))

(defun nngmail-testdrive--restore-layout ()
  "Put the frame back the way it was when the drive started, report included."
  (when (and nngmail-testdrive--layout
             (window-configuration-p nngmail-testdrive--layout))
    (set-window-configuration nngmail-testdrive--layout))
  (nngmail-testdrive--show-report))

(defun nngmail-testdrive--out (fmt &rest args)
  "Append FMT/ARGS to the report buffer and keep it visible."
  (with-current-buffer (nngmail-testdrive--buf)
    (goto-char (point-max))
    (insert (apply #'format fmt args) "\n")
    (when-let* ((w (get-buffer-window (current-buffer))))
      (set-window-point w (point-max)))))

(defun nngmail-testdrive--log-tail (&optional n)
  (when-let* ((buf (get-buffer "*nngmail-log*")))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (forward-line (- (or n 15)))
        (buffer-substring-no-properties (point) (point-max))))))

(defun nngmail-testdrive--record (section name status &optional note secs)
  (push (list section name status note secs) nngmail-testdrive--results)
  (nngmail-testdrive--out "** %s %s%s" status name (if secs (format "  (%.1fs)" secs) ""))
  (when (and note (not (string-empty-p note)))
    (nngmail-testdrive--out "   %s" note))
  (when (eq status 'FAIL)
    (when-let* ((tail (nngmail-testdrive--log-tail)))
      (nngmail-testdrive--out "#+begin_example\n%s#+end_example" tail))))

;;;; Interaction

(defvar nngmail-testdrive--waited 0
  "Seconds the current step spent waiting for the user.
Subtracted from the step's time, so a slow answer is not a SLOW step.")

(defun nngmail-testdrive--read-char (prompt choices)
  "`read-char-choice' PROMPT CHOICES, keeping the wait out of the step's clock."
  (let ((t0 (float-time)))
    (unwind-protect (read-char-choice prompt choices)
      (cl-incf nngmail-testdrive--waited (- (float-time) t0)))))

(defun nngmail-testdrive--ask (prompt)
  "Ask PROMPT; return PASS, FAIL (with a note) or SKIP; q signals quit."
  (let ((c (nngmail-testdrive--read-char (concat prompt "  [y]es/[n]o/[s]kip/[q]uit ") '(?y ?n ?s ?q))))
    (pcase c
      (?y 'PASS)
      (?n (cons 'FAIL (let ((t0 (float-time)))
                        (unwind-protect (read-string "What did you see? ")
                          (cl-incf nngmail-testdrive--waited (- (float-time) t0))))))
      (?s 'SKIP)
      (_ (signal 'nngmail-testdrive-quit nil)))))

(defun nngmail-testdrive--confirm (prompt)
  "For steps where the user must do something first.  Return non-nil to proceed."
  (let ((c (nngmail-testdrive--read-char (concat prompt "  [RET/y] done, [s]kip, [q]uit ") '(?y ?\r ?s ?q))))
    (pcase c
      ((or ?y ?\r) t)
      (?s nil)
      (_ (signal 'nngmail-testdrive-quit nil)))))

(defcustom nngmail-testdrive-test-budget 60
  "Seconds a single test drive step may take before the report flags it.
The whole drive is meant to finish in about ten minutes; a step over
this budget is a performance finding in itself and is marked SLOW in
the report."
  :type 'integer)

(defmacro nngmail-testdrive--test (section name &rest body)
  "Run BODY as test NAME in SECTION, recording outcome and timing.
BODY returns PASS/FAIL/SKIP or (STATUS . NOTE).  Errors become FAIL.
A step over `nngmail-testdrive-test-budget' seconds gets a SLOW note."
  (declare (indent 2))
  `(let ((t0 (float-time))
         (nngmail-testdrive--waited 0))
     (nngmail-testdrive--out "\n*** %s" ,name)
     (message "nngmail-testdrive: %s" ,name)
     (condition-case err
         (let* ((r (unwind-protect (progn ,@body)
                     (nngmail-testdrive--restore-layout)))
                (secs (- (float-time) t0 nngmail-testdrive--waited))
                (slow (and (> secs nngmail-testdrive-test-budget)
                           (format "SLOW: %.0fs, budget %ds" secs nngmail-testdrive-test-budget))))
           (pcase r
             (`(,st . ,note) (nngmail-testdrive--record ,section ,name st
                                                        (if slow (concat note "  " slow) note) secs))
             (st (nngmail-testdrive--record ,section ,name (or st 'PASS) slow secs))))
       (nngmail-testdrive-quit (signal 'nngmail-testdrive-quit nil))
       (quit (signal 'nngmail-testdrive-quit nil))
       (error (nngmail-testdrive--record ,section ,name 'FAIL
                                         (format "error: %s" (error-message-string err))
                                         (- (float-time) t0 nngmail-testdrive--waited))))))

(defun nngmail-testdrive--check (ok &optional note)
  (if ok 'PASS (cons 'FAIL note)))

(defmacro nngmail-testdrive--with-subscribed-sandbox (&rest body)
  "Run BODY with the sandbox group subscribed in Gnus, unsubscribing after.
Gnus only propagates ticks and the like for groups with an info."
  (declare (indent 0))
  `(let* ((full (nngmail-testdrive--group nngmail-testdrive-label))
          (had-info (gnus-get-info full)))
     (unless had-info
       ;; The method is taken from the prefixed name (nngmail+SERVER:...).
       (gnus-group-change-level full gnus-level-default-subscribed gnus-level-killed))
     (unwind-protect (progn ,@body)
       (unless had-info
         (gnus-group-change-level full gnus-level-killed gnus-level-default-subscribed)
         (setq gnus-killed-list (delete full gnus-killed-list))))))

(defmacro nngmail-testdrive--modifying (&rest body)
  "Run BODY only if modifications were approved; otherwise SKIP."
  (declare (indent 0))
  `(if nngmail-testdrive--may-modify
       (progn ,@body)
     (cons 'SKIP "modifications not approved")))

;;;; Sandbox guard
;;
;; Every mutating backend call the drive makes goes through these wrappers,
;; which refuse any article number not in `nngmail-testdrive--own'.

(defun nngmail-testdrive--own-p (num)
  (gethash num nngmail-testdrive--own))

(defun nngmail-testdrive--guard (nums)
  (dolist (n nums)
    (unless (nngmail-testdrive--own-p n)
      (signal 'nngmail-testdrive-guard (list n)))))

(defun nngmail-testdrive--local (num group)
  "GROUP's local article number for server article NUM (what Gnus would pass)."
  (or (nngmail--num->local nngmail-testdrive--account group num)
      (error "article %d is not in %s" num group)))

(defun nngmail-testdrive--set-mark (nums action marks &optional group)
  "Apply ACTION MARKS to server articles NUMS through the nnoo entry point.
The backend speaks GROUP's local numbers (default: the sandbox label),
exactly as Gnus does, so translate first."
  (nngmail-testdrive--guard nums)
  (let* ((group (or group nngmail-testdrive-label))
         (locals (mapcar (lambda (n) (nngmail-testdrive--local n group)) nums)))
    (nngmail-request-set-mark group
                              `((,(gnus-compress-sequence (sort locals #'<)) ,action ,marks))
                              nngmail-testdrive--server))
  (nngmail-testdrive--settle))

(defun nngmail-testdrive--move (num from to)
  "Move server article NUM from group FROM to group TO via the nnoo entry point."
  (nngmail-testdrive--guard (list num))
  (nngmail-request-move-article (nngmail-testdrive--local num from) from
                                nngmail-testdrive--server nil nil to))

(defun nngmail-testdrive--relabel (num add remove)
  (nngmail-testdrive--guard (list num))
  (nngmail--modify nngmail-testdrive--account (list num) add remove))

(defun nngmail-testdrive--expire (num group action)
  (nngmail-testdrive--guard (list num))
  (let ((nngmail-expire-action action))
    (nngmail--expire-one nngmail-testdrive--account num group)))

;;;; Helpers

(defun nngmail-testdrive--group (short)
  (gnus-group-full-name short nngmail-testdrive--method))

(defun nngmail-testdrive--sync ()
  "Sync the account now, inline: the drive checks results right after."
  (let ((nngmail-background nil))
    (nngmail--incremental-sync nngmail-testdrive--account t)))

(defun nngmail-testdrive--labels (num)
  (gethash num (nngmail-account-labels nngmail-testdrive--account)))

(defun nngmail-testdrive--header (num)
  "The header of article NUM, fetching it if the lazy sync has not yet.
With `nngmail-sync-headers' `lazy' an arrival is filed from history
alone; its subject and Message-ID exist only after a fetch."
  (or (nngmail--header nngmail-testdrive--account num)
      (when-let* ((id (gethash num (nngmail-account-num->id nngmail-testdrive--account))))
        (nngmail--fetch-metadata nngmail-testdrive--account (list id))
        (nngmail--header nngmail-testdrive--account num))))

(defun nngmail-testdrive--live-labels (num)
  "Ask Gmail for the current labels of article NUM (nil if it no longer exists)."
  (when-let* ((id (gethash num (nngmail-account-num->id nngmail-testdrive--account)))
              (msg (nngmail--get nngmail-testdrive--account (concat "messages/" id)
                                 '(("format" . "minimal")))))
    (alist-get 'labelIds msg)))

(defun nngmail-testdrive--exists-p (num)
  (let ((id (gethash num (nngmail-account-num->id nngmail-testdrive--account))))
    (and id (nngmail--get nngmail-testdrive--account (concat "messages/" id)
                          '(("format" . "minimal"))))))

(defun nngmail-testdrive--wait-for (pred &optional timeout what)
  "Poll PRED (syncing between polls) until non-nil or TIMEOUT seconds.
Shows the wait as an nngmail progress bar, seconds elapsed over the
timeout, so a slow Gmail is visible rather than a frozen Emacs."
  (let* ((timeout (or timeout 60))
         (start (float-time))
         (deadline (+ start timeout))
         (account nngmail-testdrive--account)
         (phase (format "waiting for %s" (or what "Gmail")))
         v)
    (unwind-protect
        (while (and (not (setq v (funcall pred))) (< (float-time) deadline))
          (nngmail--progress account phase (round (- (float-time) start)) timeout t)
          (sit-for 3)
          (nngmail-testdrive--sync)
          (nngmail--progress account phase (round (- (float-time) start)) timeout t))
      (nngmail--progress-clear))
    v))

(defun nngmail-testdrive--sandbox-nums ()
  "Article numbers currently in the sandbox label group."
  (nngmail--group-numbers nngmail-testdrive--account nngmail-testdrive-label))

(defun nngmail-testdrive--adopt-sandbox ()
  "Mark everything in the sandbox label as owned (it was created by a previous drive)."
  (dolist (n (nngmail-testdrive--sandbox-nums))
    (puthash n t nngmail-testdrive--own)))

(defun nngmail-testdrive--pick-real (group &optional pred)
  "Return a REAL (not drive-owned) article in GROUP satisfying PRED.  Read-only use.
An already-read article is preferred: looking at one in a summary marks
it read, and that reaches Gmail.  An unread one is returned only when
no read one qualifies; `nngmail-testdrive--show' then restores its
state after the look."
  (let* ((labels (nngmail-account-labels nngmail-testdrive--account))
         (ok (lambda (n) (and (not (nngmail-testdrive--own-p n))
                              (funcall (or pred #'always) n (gethash n labels)))))
         (nums (nngmail--group-numbers nngmail-testdrive--account group)))
    (or (seq-find (lambda (n) (and (not (member "UNREAD" (gethash n labels))) (funcall ok n))) nums)
        (seq-find ok nums))))

(defun nngmail-testdrive--find-sent (gmail-id &optional timeout)
  "Sync until the message with GMAIL-ID is indexed; return its article number.
The id comes from `nngmail-last-sent-id', so this is one hash lookup per
poll, not a scan of the index."
  (nngmail-testdrive--wait-for
   (lambda () (gethash gmail-id (nngmail-account-id->num nngmail-testdrive--account)))
   (or timeout 90) (format "sent message %s to arrive" gmail-id)))

(defvar nngmail-testdrive--send-threads nil
  "threadId values the drive's sends carried, newest first (nil = none).")

(defun nngmail-testdrive--last-send-thread ()
  (car nngmail-testdrive--send-threads))

(defun nngmail-testdrive--record-send-thread (orig account path body &rest rest)
  "Around `nngmail--post': note the threadId of each messages/send."
  (when (equal path "messages/send")
    (push (alist-get 'threadId body) nngmail-testdrive--send-threads))
  (apply orig account path body rest))

(defun nngmail-testdrive--send (subject body &optional attach reply-to-num)
  "Send SUBJECT/BODY from me to me via nngmail; file it in the sandbox.
Return the article number once it has arrived and been filed, or nil."
  (let* ((me (nngmail-account-user nngmail-testdrive--account))
         (mid (message-make-message-id))
         (message-send-mail-function #'nngmail-message-send-mail)
         (message-confirm-send nil)
         (message-interactive nil)
         (gnus-message-archive-group nil)
         (message-required-mail-headers (delq 'Message-ID (copy-sequence message-required-mail-headers)))
         (headers (list (cons 'Message-ID mid)
                        (cons 'X-Nngmail-Testdrive (format-time-string "%F %T")))))
    (when reply-to-num
      (let ((h (nngmail-testdrive--header reply-to-num)))
        (push (cons 'In-Reply-To (mail-header-id h)) headers)
        (push (cons 'References (mail-header-id h)) headers)))
    (advice-add 'nngmail--post :around #'nngmail-testdrive--record-send-thread)
    (with-current-buffer (generate-new-buffer " *nngmail-testdrive-send*")
      (unwind-protect
          (save-window-excursion
            (message-mode)
            (message-setup (append (list (cons 'To me) (cons 'From me)
                                         (cons 'Subject (concat nngmail-testdrive-subject-prefix subject)))
                                   headers))
            (message-goto-body)
            (insert body "\n")
            (when attach
              (mml-attach-file attach (mm-default-file-type attach) "test attachment" "attachment"))
            (message-send))
        (advice-remove 'nngmail--post #'nngmail-testdrive--record-send-thread)
        (set-buffer-modified-p nil)
        (kill-buffer (current-buffer))))
    (when-let* ((id nngmail-last-sent-id)
                (num (nngmail-testdrive--find-sent id)))
      ;; Claim it, then move it out of INBOX into the sandbox right away so
      ;; it never sits in the user's inbox.
      (puthash num t nngmail-testdrive--own)
      (nngmail-testdrive--relabel num (list nngmail-testdrive--label-id) '("INBOX" "UNREAD"))
      num)))

(defvar nngmail-testdrive--restore-marks nil
  "Articles whose UNREAD/STARRED state a look changed: ((NUM . LABELS) ...).
`nngmail-testdrive--leave-summary' puts them back at Gmail.")

(defun nngmail-testdrive--show (num group)
  "Display server article NUM of GROUP for the user to look at.
Selecting an article marks it read and Gnus pushes that to Gmail.  For
a real (not drive-owned) article that was unread, or starred, the state
before the look is remembered and restored when the summary is left, so
looking leaves no trace in the account."
  (let ((local (nngmail--num->local nngmail-testdrive--account group num)))
    (unless local (error "article %d is not in %s" num group))
    (unless (nngmail-testdrive--own-p num)
      (let ((labels (nngmail-testdrive--labels num)))
        (when (or (member "UNREAD" labels) (member "STARRED" labels))
          (push (cons num (seq-filter (lambda (l) (member l '("UNREAD" "STARRED"))) labels))
                nngmail-testdrive--restore-marks))))
    (gnus-summary-read-group (nngmail-testdrive--group group) nil t nil nil (list local))
    (gnus-summary-goto-article local nil t)
    (gnus-summary-select-article)
    (sit-for 0.2)))

(defun nngmail-testdrive--restore-looked-at ()
  "Give back UNREAD/STARRED to real articles a look took them from."
  (when nngmail-testdrive--restore-marks
    (let ((account nngmail-testdrive--account))
      (pcase-dolist (`(,num . ,wanted) nngmail-testdrive--restore-marks)
        (let* ((now (gethash num (nngmail-account-labels account)))
               (add (seq-remove (lambda (l) (member l now)) wanted)))
          (when add
            (nngmail--modify account (list num) add nil)
            (nngmail--log "testdrive: restored %s on article %d after looking at it"
                          (string-join add ", ") num))))
      (setq nngmail-testdrive--restore-marks nil))))

(defun nngmail-testdrive--leave-summary ()
  ;; The user may already have left with q (the UI-path test asks them
  ;; to); then the summary buffer is gone and there is nothing to exit.
  (when (and (buffer-live-p (current-buffer))
             (derived-mode-p 'gnus-summary-mode)
             (buffer-live-p gnus-summary-buffer))
    (gnus-summary-exit))
  (when (and gnus-group-buffer (buffer-live-p (get-buffer gnus-group-buffer)))
    (set-buffer gnus-group-buffer))
  ;; Eager marks are sent on an idle timer; the drive reads live labels
  ;; right after, so flush synchronously to avoid racing it.
  (when (fboundp 'nngmail-flush-marks)
    (nngmail-flush-marks))
  ;; Then undo what looking did to real articles.
  (nngmail-testdrive--restore-looked-at))

(defun nngmail-testdrive--settle ()
  "Flush any queued mark changes so live-label checks see them."
  (when (fboundp 'nngmail-flush-marks)
    (nngmail-flush-marks)))

;;;; Sections

(defun nngmail-testdrive--section-0 ()
  (nngmail-testdrive--out "\n* 0. Setup sanity (read-only)")
  (nngmail-testdrive--test 0 "server opens and syncs"
    (nngmail-testdrive--sync)
    (nngmail-testdrive--check (nngmail-account-history-id nngmail-testdrive--account)
                              "no historyId after sync"))
  (nngmail-testdrive--test 0 "background job: a kick schedules a step, a drain brings it to idle"
    (if (not (and nngmail-use-plz (bound-and-true-p nngmail-background)))
        (cons 'SKIP "background sync is off (nngmail-background nil or no plz)")
      (require 'nngmail-background)
      (let* ((a nngmail-testdrive--account)
             (job (nngmail-background-kick a t))
             (deadline (+ (float-time) 90)))
        ;; Let the timers and callbacks run; the job does the work.
        (while (and (not (eq (nngmail-job-state job) 'idle)) (< (float-time) deadline))
          (nngmail-testdrive--restore-layout)
          (sit-for 0.5))
        (nngmail-testdrive--check (eq (nngmail-job-state job) 'idle)
                                  (format "job still %s after 90s%s" (nngmail-job-state job)
                                          (if (nngmail-job-last-error job)
                                              (format "; last error: %s" (nngmail-job-last-error job))
                                            ""))))))
  (nngmail-testdrive--test 0 "HTTP transport"
    (cons 'PASS (format "nngmail-use-plz=%S curl=%S plz=%S"
                        nngmail-use-plz (executable-find "curl") (and (locate-library "plz") t))))
  (nngmail-testdrive--test 0 "INBOX local vs live agree (newest 20)"
    (let ((bad nil))
      (dolist (id (nngmail--list-ids nngmail-testdrive--account nil '("INBOX") 20))
        (let* ((num (gethash id (nngmail-account-id->num nngmail-testdrive--account)))
               (local (and num (nngmail-testdrive--labels num)))
               (live (and num (nngmail-testdrive--live-labels num))))
          (unless (and num
                       (nngmail--group-member-p nngmail-testdrive--account "INBOX" num)
                       (null (seq-difference local live)) (null (seq-difference live local)))
            (push (format "%s: local=%S live=%S" id local live) bad))))
      (nngmail-testdrive--check (null bad) (string-join bad "; "))))
  (nngmail-testdrive--test 0 "lazy sync: recent arrivals filed without header fetches"
    ;; With `nngmail-sync-headers' lazy, history filing leaves headers
    ;; empty until a summary asks.  Count articles that have labels but no
    ;; header: after a fresh sync there should be some unless every
    ;; recent arrival has already been displayed, and none of them may be
    ;; missing from their groups.
    (if (not (eq nngmail-sync-headers 'lazy))
        (cons 'SKIP (format "nngmail-sync-headers is %S" nngmail-sync-headers))
      (let* ((a nngmail-testdrive--account)
             (db (nngmail--db a))
             ;; Two queries instead of a probe per message: articles with
             ;; no header, and among those, ones no group numbers.
             (lazy (car (car (sqlite-select db "SELECT count(*) FROM messages WHERE header IS NULL"))))
             (unfiled (car (car (sqlite-select
                                 db "SELECT count(*) FROM messages m WHERE header IS NULL
                                     AND NOT EXISTS (SELECT 1 FROM group_articles g WHERE g.num = m.num)")))))
        (cons (if (zerop unfiled) 'PASS 'FAIL)
              (format "%s articles indexed without headers, %s of them not in any group"
                      (nngmail--fmt-num lazy) (nngmail--fmt-num unfiled))))))
  (nngmail-testdrive--test 0 "sandbox label ready"
    (nngmail-testdrive--modifying
      (unless (nngmail--group-label nngmail-testdrive--account nngmail-testdrive-label)
        (nngmail-request-create-group nngmail-testdrive-label nngmail-testdrive--server))
      (setq nngmail-testdrive--label-id
            (nngmail--group-label nngmail-testdrive--account nngmail-testdrive-label))
      (nngmail-testdrive--adopt-sandbox)
      (nngmail-testdrive--check nngmail-testdrive--label-id "could not create the sandbox label"))))

(defun nngmail-testdrive--section-1 ()
  (nngmail-testdrive--out "\n* 1. Reading (read-only)")
  (nngmail-testdrive--test 1 "C-u 100 RET on INBOX fetches and shows only INBOX"
    (let ((before (nngmail--group-size nngmail-testdrive--account "INBOX")))
      (gnus-summary-read-group (nngmail-testdrive--group "INBOX") 100 t)
      (let* ((a nngmail-testdrive--account)
             (shown gnus-newsgroup-articles)      ; INBOX's local numbers
             (labels (nngmail-account-labels a))
             ;; Foreign = a shown number that maps to no INBOX article, or to
             ;; one whose labels do not include INBOX.
             (foreign (seq-remove (lambda (l)
                                    (when-let* ((n (nngmail--local->num a "INBOX" l)))
                                      (member "INBOX" (gethash n labels))))
                                  shown))
             (size (nngmail--group-size a "INBOX")))
        (nngmail-testdrive--leave-summary)
        (cons (if (and (null foreign) (>= (length shown) (min 100 size))) 'PASS 'FAIL)
              (format "shown %d, foreign %d, INBOX indexed %d -> %d"
                      (length shown) (length foreign) before size)))))
  (nngmail-testdrive--test 1 "C-u N RET selects N existing articles despite retired numbers"
    ;; Local numbers of messages that left INBOX are retired, never
    ;; reused.  Gnus's own choice for a prefix is the top N of the active
    ;; range, which would include them; the advice hands it existing ones.
    (let* ((a nngmail-testdrive--account)
           (g (nngmail--group a "INBOX"))
           (high (nngmail--group-high a "INBOX"))
           (size (nngmail--group-size a "INBOX")))
      (if (or (null g) (< size 5)) (cons 'SKIP "INBOX too small")
        (let* ((retired (- high size))
               (chosen (nngmail--newest-locals (nngmail-testdrive--group "INBOX") 5)))
          (cons (if (and (= 5 (length chosen))
                         (cl-every (lambda (l) (nngmail--local->num a "INBOX" l)) chosen))
                    'PASS 'FAIL)
                (format "%d of %d local numbers retired in INBOX; chose %S" retired high chosen))))))
  (nngmail-testdrive--test 1 "open an article (look at it)"
    (let ((num (nngmail-testdrive--pick-real "INBOX")))
      (if (null num) (cons 'SKIP "INBOX empty")
        (nngmail-testdrive--show num "INBOX")
        (let ((mb (with-current-buffer gnus-article-buffer enable-multibyte-characters)))
          (prog1 (if (not mb)
                     (cons 'FAIL "article buffer is unibyte: charset decoding is off")
                   (nngmail-testdrive--ask "Does the article render correctly?"))
            (nngmail-testdrive--leave-summary))))))
  (nngmail-testdrive--test 1 "non-ASCII subject/from decodes"
    (let ((num (nngmail-testdrive--pick-real
                "[Gmail]/All Mail"
                (lambda (n _l)
                  ;; Only messages whose headers are already fetched.
                  (when-let* ((h (nngmail--header nngmail-testdrive--account n)))
                    (string-match-p "[^[:ascii:]]" (concat (mail-header-subject h) (mail-header-from h))))))))
      (if (null num) (cons 'SKIP "no indexed message with non-ASCII subject/from")
        (nngmail-testdrive--show num "[Gmail]/All Mail")
        (prog1 (nngmail-testdrive--ask "Non-ASCII subject and sender correct (summary line and article)?")
          (nngmail-testdrive--leave-summary)))))
  (nngmail-testdrive--test 1 "opening a group fetches only the headers it shows"
    ;; Lazy mode: RET on a group must batch-fetch the displayed articles'
    ;; headers, and nothing else.
    (let* ((a nngmail-testdrive--account)
           (inbox (nngmail--group-numbers a "INBOX"))
           (missing-before (cl-count-if-not (lambda (n) (nngmail--has-header-p a n)) inbox)))
      (if (zerop missing-before)
          (cons 'SKIP "every INBOX header is already fetched")
        (let ((nngmail-debug t) (before (buffer-size (get-buffer-create "*nngmail-log*"))))
          (gnus-summary-read-group (nngmail-testdrive--group "INBOX") 50 t)
          (let* ((shown gnus-newsgroup-articles)
                 (still-missing (cl-count-if-not (lambda (n) (nngmail--has-header-p a n)) shown))
                 (batches (with-current-buffer "*nngmail-log*"
                            (how-many "POST .*batch" before (point-max)))))
            (nngmail-testdrive--leave-summary)
            (cons (if (and (zerop still-missing) (<= batches 2)) 'PASS 'FAIL)
                  (format "%d shown, %d without header after open, %d batch calls, %d INBOX headers were missing before"
                          (length shown) still-missing batches missing-before)))))))
  (nngmail-testdrive--test 1 "label icons in the summary line (look at it)"
    ;; The stock icon table, not the user's: the step must not depend on
    ;; what they customized.
    (let* ((nngmail-label-display-alist (default-value 'nngmail-label-display-alist))
           (num (nngmail-testdrive--pick-real
                 "INBOX"
                 (lambda (_n l) (seq-some (lambda (id) (assoc id nngmail-label-display-alist)) l)))))
      (if (null num)
          (cons 'SKIP "no INBOX message carries a superstar label (YELLOW_STAR, RED_BANG, ...)")
        (let ((gnus-summary-line-format
               "%U%R%u&gmail-label-mark; %10&user-date; %(%-20,20a%) %-14,14u&gmail-labels; %s\n"))
          (gnus-update-format-specifications t 'summary)
          (nngmail-testdrive--show num "INBOX")
          (prog1 (nngmail-testdrive--ask
                  "Summary line: is there an icon column (star etc.) that keeps every line aligned, with the label names after the sender?")
            (nngmail-testdrive--leave-summary)
            (gnus-update-format-specifications t 'summary))))))
  (nngmail-testdrive--test 1 "second open is served from cache"
    (let ((num (nngmail-testdrive--pick-real "INBOX")))
      (if (null num) (cons 'SKIP "no article")
        (let ((nngmail-debug t) (before (buffer-size (get-buffer-create "*nngmail-log*"))))
          (nnheader-init-server-buffer)
          (nngmail-request-article num "INBOX" nngmail-testdrive--server)
          (with-current-buffer "*nngmail-log*"
            (nngmail-testdrive--check
             (not (string-match-p "format=raw" (buffer-substring before (point-max))))
             "raw fetch happened again despite the cache"))))))
  (nngmail-testdrive--test 1 "thread fetch (A T) works"
    (let* ((h nil)
           (num (nngmail-testdrive--pick-real
                 "[Gmail]/All Mail"
                 (lambda (n _l)
                   (setq h (nngmail--header nngmail-testdrive--account n))
                   (and h (not (string-empty-p (or (mail-header-references h) ""))))))))
      (if (null num) (cons 'SKIP "no threaded message indexed")
        (nnheader-init-server-buffer)
        (nngmail-testdrive--check
         (nngmail-request-thread h "[Gmail]/All Mail" nngmail-testdrive--server)
         "request-thread returned nil")))))

(defun nngmail-testdrive--section-2 ()
  (nngmail-testdrive--out "\n* 2. Marks (on sandbox messages only)")
  (let (a b nums)
    (nngmail-testdrive--test 2 "seed: send three sandbox messages"
      (nngmail-testdrive--modifying
        (setq a (nngmail-testdrive--send "marks A" "Sandbox message A.")
              b (nngmail-testdrive--send "marks B" "Sandbox message B.")
              nums (delq nil (list a b (nngmail-testdrive--send "marks C" "Sandbox message C."))))
        (nngmail-testdrive--check (= 3 (length nums))
                                  (format "only %d of 3 arrived within the timeout" (length nums)))))
    (nngmail-testdrive--test 2 "mark unread adds UNREAD at Gmail"
      (nngmail-testdrive--modifying
        (if (null a) 'SKIP
          (nngmail-testdrive--set-mark (list a) 'del '(read))
          (nngmail-testdrive--check (member "UNREAD" (nngmail-testdrive--live-labels a))
                                    (format "live: %S" (nngmail-testdrive--live-labels a))))))
    (nngmail-testdrive--test 2 "mark read removes UNREAD"
      (nngmail-testdrive--modifying
        (if (null a) 'SKIP
          (nngmail-testdrive--set-mark (list a) 'add '(read))
          (nngmail-testdrive--check (not (member "UNREAD" (nngmail-testdrive--live-labels a)))
                                    (format "live: %S" (nngmail-testdrive--live-labels a))))))
    (nngmail-testdrive--test 2 "tick adds STARRED (and reads)"
      (nngmail-testdrive--modifying
        (if (null b) 'SKIP
          (nngmail-testdrive--set-mark (list b) 'del '(read))     ; make it unread first
          (nngmail-testdrive--set-mark (list b) 'add '(tick))
          (let ((live (nngmail-testdrive--live-labels b)))
            (nngmail-testdrive--check (and (member "STARRED" live) (not (member "UNREAD" live)))
                                      (format "live: %S" live))))))
    (nngmail-testdrive--test 2 "untick removes STARRED"
      (nngmail-testdrive--modifying
        (if (null b) 'SKIP
          (nngmail-testdrive--set-mark (list b) 'del '(tick))
          (nngmail-testdrive--check (not (member "STARRED" (nngmail-testdrive--live-labels b)))
                                    (format "live: %S" (nngmail-testdrive--live-labels b))))))
    (nngmail-testdrive--test 2 "one action on N articles = one batchModify"
      (nngmail-testdrive--modifying
        (if (< (length nums) 2) 'SKIP
          (let ((nngmail-debug t) (before (buffer-size (get-buffer-create "*nngmail-log*"))))
            (nngmail-testdrive--set-mark nums 'del '(read))
            (with-current-buffer "*nngmail-log*"
              ;; One request logs two lines ("SLOW HTTP POST .../batchModify"
              ;; and "batchModify N id(s)"); count the latter only.
              (let ((calls (cl-count-if (lambda (l) (string-match-p " batchModify [0-9]+ id(s)" l))
                                        (split-string (buffer-substring before (point-max)) "\n"))))
                (nngmail-testdrive--set-mark nums 'add '(read))
                (cons (if (= calls 1) 'PASS 'FAIL) (format "%d batchModify call(s) for %d articles" calls (length nums)))))))))
    (nngmail-testdrive--test 2 "a mark made in one group shows in the message's other groups"
      ;; One Gmail message under two labels is one object: read in the
      ;; sandbox, it is read in [Gmail]/Sent Mail too, and the info of
      ;; that other group reflects it after the update.
      (nngmail-testdrive--modifying
        (if (null a) 'SKIP
          ;; The other group must be one Gnus knows (subscribed, so it
          ;; has an info); All Mail usually is not.
          (let* ((others (seq-remove (lambda (g) (equal g nngmail-testdrive-label))
                                     (nngmail--message-groups nngmail-testdrive--account
                                                              (nngmail-testdrive--labels a))))
                 (other (seq-find (lambda (g) (gnus-get-info (nngmail-testdrive--group g))) others))
                 (other-local (and other (nngmail--num->local nngmail-testdrive--account other a))))
            (if (not other-local)
                (cons 'SKIP (format "no subscribed group besides the sandbox holds it (it is in %s)"
                                    (string-join others ", ")))
              (nngmail-testdrive--set-mark (list a) 'del '(read))
              (let ((info (gnus-get-info (nngmail-testdrive--group other))))
                (nngmail--update-info nngmail-testdrive--account other info)
                (let ((unread-before (not (range-member-p other-local (gnus-info-read info)))))
                  (nngmail-testdrive--set-mark (list a) 'add '(read))
                  (nngmail--update-info nngmail-testdrive--account other info)
                  (let ((read-after (range-member-p other-local (gnus-info-read info))))
                    (nngmail-testdrive--check
                     (and unread-before read-after)
                     (format "in %s: unread before=%S read after=%S"
                             other unread-before read-after))))))))))
    (nngmail-testdrive--test 2 "c in *Group* on the sandbox label reaches Gmail"
      (nngmail-testdrive--modifying
        (if (< (length nums) 2) 'SKIP
          ;; Make two sandbox messages unread at Gmail, then catch up from *Group*.
          (nngmail-testdrive--set-mark nums 'del '(read))
          (unless (cl-every (lambda (n) (member "UNREAD" (nngmail-testdrive--live-labels n))) nums)
            (error "precondition failed: could not make sandbox messages unread at Gmail"))
          (require 'gnus-group)
          (cl-letf (((symbol-function 'gnus-group-update-group) #'ignore))
            (gnus-group-catchup (nngmail-testdrive--group nngmail-testdrive-label)))
          (nngmail-testdrive--settle)
          (let ((still (seq-filter (lambda (n) (member "UNREAD" (nngmail-testdrive--live-labels n))) nums)))
            (nngmail-testdrive--check (null still)
                                      (format "%d of %d still UNREAD at Gmail after *Group* catch-up"
                                              (length still) (length nums)))))))
    (nngmail-testdrive--test 2 "on-exit propagation: read is sent by q, dropped by Q"
      ;; Under `on-exit' nothing leaves while the summary is open; q
      ;; sends the diff, Q sends nothing.  Both on a sandbox message.
      (nngmail-testdrive--modifying
        (if (null a) 'SKIP
          (let ((nngmail-mark-propagation 'on-exit))
            (nngmail-testdrive--set-mark (list a) 'del '(read))
            (nngmail-testdrive--with-subscribed-sandbox
              ;; Q: read it, quit without update -> still UNREAD at Gmail.
              (let ((local (nngmail-testdrive--local a nngmail-testdrive-label)))
                (gnus-summary-read-group (nngmail-testdrive--group nngmail-testdrive-label) nil t nil nil (list local))
                (gnus-summary-goto-article local nil t)
                (gnus-summary-mark-article-as-read gnus-read-mark)
                (gnus-summary-exit-no-update t))
              (nngmail-testdrive--settle)
              (let ((after-Q (nngmail-testdrive--live-labels a)))
                ;; q: read it, quit normally -> read at Gmail.
                (let ((local (nngmail-testdrive--local a nngmail-testdrive-label)))
                  (gnus-summary-read-group (nngmail-testdrive--group nngmail-testdrive-label) nil t nil nil (list local))
                  (gnus-summary-goto-article local nil t)
                  (gnus-summary-mark-article-as-read gnus-read-mark)
                  (gnus-summary-exit))
                (nngmail-testdrive--settle)
                (let ((after-q (nngmail-testdrive--live-labels a)))
                  (nngmail-testdrive--check (and (member "UNREAD" after-Q) (not (member "UNREAD" after-q)))
                                            (format "after Q: %S; after q: %S" after-Q after-q)))))))))
    (nngmail-testdrive--test 2 "summary buffer ! and M-u reach Gmail (UI path)"
      ;; The summary commands themselves, run in a real summary buffer:
      ;; `!' (tick) reaches Gmail when the summary is left, through the
      ;; server-marks diff; `M-u' goes through the eager mark advice.
      ;; A tick reaches Gmail when the summary is left, through Gnus's
      ;; exit-time diff of the group info's marks -- which needs the
      ;; group to have an info, i.e. be subscribed.  The drive's sandbox
      ;; label is a fresh, unsubscribed group, so the summary is opened
      ;; on a temporary info for this step.
      (nngmail-testdrive--modifying
        (if (null a) 'SKIP
          (nngmail-testdrive--with-subscribed-sandbox
            (nngmail-testdrive--in-summary a nngmail-testdrive-label
                                           (lambda (_) (call-interactively #'gnus-summary-tick-article-forward)))
            (let ((ticked (nngmail-testdrive--live-labels a)))
              ;; M-u clears the tick and leaves the article unread.
              (nngmail-testdrive--in-summary a nngmail-testdrive-label
                                             (lambda (_) (call-interactively #'gnus-summary-clear-mark-forward)))
              (let ((unread (nngmail-testdrive--live-labels a)))
                (nngmail-testdrive--set-mark (list a) 'add '(read))
                (nngmail-testdrive--check (and (member "STARRED" ticked)
                                               (member "UNREAD" unread) (not (member "STARRED" unread)))
                                          (format "after !: %S; after M-u: %S" ticked unread))))))))))

(defun nngmail-testdrive--section-3 ()
  (nngmail-testdrive--out "\n* 3. Gmail -> Gnus (you act in the web UI, on sandbox messages)")
  (let ((num (car (nngmail-testdrive--sandbox-nums))))
    (nngmail-testdrive--test 3 "web UI star + unread show up after g"
      (cond
       ((null num) (cons 'SKIP (format "nothing in %s (run section 2 first)" nngmail-testdrive-label)))
       ((not (nngmail-testdrive--confirm
              (format "In the Gmail web UI, under label %s: mark the message %S as UNREAD and STAR it. Done?"
                      nngmail-testdrive-label
                      (mail-header-subject (nngmail-testdrive--header num)))))
        'SKIP)
       (t
        (nngmail-testdrive--wait-for
         (lambda () (let ((l (nngmail-testdrive--labels num)))
                      (and (member "UNREAD" l) (member "STARRED" l))))
         90 "history to report the change")
        (let ((l (nngmail-testdrive--labels num)))
          (prog1 (nngmail-testdrive--check (and (member "UNREAD" l) (member "STARRED" l))
                                           (format "local labels after sync: %S" l))
            (nngmail-testdrive--set-mark (list num) 'add '(read))
            (nngmail-testdrive--set-mark (list num) 'del '(tick)))))))
    (nngmail-testdrive--test 3 "web UI move to Inbox and back is tracked"
      (cond
       ((null num) 'SKIP)
       ((not (nngmail-testdrive--confirm
              "In the web UI: 'Move to Inbox' that same message. Done?"))
        'SKIP)
       (t
        (let ((seen (nngmail-testdrive--wait-for
                     (lambda () (member "INBOX" (nngmail-testdrive--labels num))) 90 "INBOX label")))
          (when seen
            ;; Put it back into the sandbox ourselves (it is our message).
            (nngmail-testdrive--relabel num nil '("INBOX")))
          (nngmail-testdrive--check seen "INBOX label never appeared locally")))))))

(defun nngmail-testdrive--section-4 ()
  (nngmail-testdrive--out "\n* 4. Labels as groups (sandbox label and a second throwaway)")
  (let* ((l1 nngmail-testdrive-label)
         (l2 (concat nngmail-testdrive-label "-2"))
         (num (car (nngmail-testdrive--sandbox-nums))))
    (nngmail-testdrive--test 4 "G m creates a label"
      (nngmail-testdrive--modifying
        (unless (nngmail--group-label nngmail-testdrive--account l2)
          (nngmail-request-create-group l2 nngmail-testdrive--server))
        (nngmail-testdrive--check (nngmail--group-label nngmail-testdrive--account l2) "label not created")))
    (nngmail-testdrive--test 4 "B m moves between labels"
      (nngmail-testdrive--modifying
        (if (null num) (cons 'SKIP "no sandbox message (run section 2)")
          (nngmail-testdrive--move num l1 l2)
          (let ((live (nngmail-testdrive--live-labels num))
                (id2 (nngmail--group-label nngmail-testdrive--account l2)))
            (nngmail-testdrive--check (and (member id2 live) (not (member nngmail-testdrive--label-id live)))
                                      (format "live after move: %S" live))))))
    (nngmail-testdrive--test 4 "B c copies (message in both groups)"
      (nngmail-testdrive--modifying
        (if (null num) 'SKIP
          (nngmail-testdrive--relabel num (list nngmail-testdrive--label-id) nil)
          (let ((live (nngmail-testdrive--live-labels num))
                (id2 (nngmail--group-label nngmail-testdrive--account l2)))
            (nngmail-testdrive--check
             (and (member nngmail-testdrive--label-id live) (member id2 live)
                  (memq num (nngmail--group-numbers nngmail-testdrive--account l1))
                  (memq num (nngmail--group-numbers nngmail-testdrive--account l2)))
             (format "live: %S" live))))))
    (nngmail-testdrive--test 4 "G r renames a label: fast, local numbers kept"
      ;; Since 2026-09-20 a rename moves the group's rows instead of
      ;; refiling the whole index (43 s on 2.5M messages), so the local
      ;; number of the sandbox message in the group must not change, and
      ;; the step must be quick.
      (nngmail-testdrive--modifying
        (let* ((l3 (format "%s-renamed-%s" l2 (format-time-string "%H%M%S")))
               (before (and num (nngmail--num->local nngmail-testdrive--account l2 num)))
               (t0 (float-time)))
          ;; A stale target from an interrupted earlier run would make
          ;; the rename fall back to a full rebuild; remove it first.
          (dolist (stale (seq-filter (lambda (name) (string-prefix-p (concat l2 "-renamed") name))
                                     (mapcar #'cdr (nngmail-account-label-names nngmail-testdrive--account))))
            (ignore-errors (nngmail-request-delete-group stale nil nngmail-testdrive--server)))
          (nngmail-request-rename-group l2 l3 nngmail-testdrive--server)
          (let ((secs (- (float-time) t0))
                (after (and num (nngmail--num->local nngmail-testdrive--account l3 num))))
            (prog1 (nngmail-testdrive--check
                    (and (nngmail--group-label nngmail-testdrive--account l3)
                         (not (nngmail--group-label nngmail-testdrive--account l2))
                         (not (nngmail--group nngmail-testdrive--account l2))
                         (or (null num) (and after (= after before)))
                         (< secs 10))
                    (format "local %S -> %S, took %s" before after (nngmail--fmt-duration secs)))
              (setq l2 l3))))))
    (nngmail-testdrive--test 4 "G DEL deletes the label: stripped locally, message survives in sandbox"
      (nngmail-testdrive--modifying
        (let ((id2 (nngmail--group-label nngmail-testdrive--account l2))
              (t0 (float-time)))
          (nngmail-request-delete-group l2 nil nngmail-testdrive--server)
          (let ((secs (- (float-time) t0)))
            (nngmail-testdrive--check
             (and (not (nngmail--group-label nngmail-testdrive--account l2))
                  (= 0 (nngmail--group-size nngmail-testdrive--account l2))
                  (or (null num)
                      (and (not (member id2 (nngmail-testdrive--labels num)))
                           (member nngmail-testdrive--label-id (nngmail-testdrive--labels num))
                           (member nngmail-testdrive--label-id (nngmail-testdrive--live-labels num)))))
             (format "label %s still known, or still on the message locally (%S), or sandbox label lost; took %s"
                     id2 (and num (nngmail-testdrive--labels num)) (nngmail--fmt-duration secs)))))))
    (nngmail-testdrive--test 4 "label renamed in the web UI is picked up by the next sync"
      (nngmail-testdrive--modifying
        (if (null num) (cons 'SKIP "no sandbox message (run section 2)")
          ;; Make a label, put the sandbox message in it, then ask the
          ;; user to rename it at Gmail; the sync must move the group.
          (let ((l4 (concat nngmail-testdrive-label "-web"))
                (l5 (concat nngmail-testdrive-label "-web-renamed")))
            (unless (nngmail--group-label nngmail-testdrive--account l4)
              (nngmail-request-create-group l4 nngmail-testdrive--server))
            (nngmail-testdrive--relabel num (list (nngmail--group-label nngmail-testdrive--account l4)) nil)
            (nngmail-testdrive--sync)
            (let ((before (nngmail--num->local nngmail-testdrive--account l4 num)))
              (if (not (nngmail-testdrive--confirm
                        (format "In the Gmail web UI: rename label %s to %s. Done?" l4 l5)))
                  (progn (ignore-errors (nngmail-request-delete-group l4 nil nngmail-testdrive--server))
                         'SKIP)
                (nngmail-testdrive--wait-for
                 (lambda () (nngmail--group nngmail-testdrive--account l5)) 90
                 (format "group %s to appear" l5))
                (let ((after (nngmail--num->local nngmail-testdrive--account l5 num)))
                  (prog1 (nngmail-testdrive--check
                          (and after (equal after before)
                               (not (nngmail--group nngmail-testdrive--account l4)))
                          (format "local %S -> %S; old group %s" before after
                                  (if (nngmail--group nngmail-testdrive--account l4) "still there" "gone")))
                    (ignore-errors (nngmail-request-delete-group l5 nil nngmail-testdrive--server))))))))))
    (nngmail-testdrive--test 4 "nested label appears as a group"
      (cond
       ((null (nngmail-testdrive--sandbox-nums))
        (cons 'SKIP "no sandbox message to label (run section 2 first)"))
       ((not (nngmail-testdrive--confirm
              (format "In the web UI: create label %s/sub and apply it to the sandbox message. Done?" l1)))
        'SKIP)
       (t
        (let ((g (concat l1 "/sub")))
          (nngmail-testdrive--wait-for (lambda () (nngmail--group-numbers nngmail-testdrive--account g)) 60 g)
          (nngmail-testdrive--check (nngmail--group-numbers nngmail-testdrive--account g)
                                    (format "group %s empty or missing" g))))))
    (nngmail-testdrive--test 4 "label deleted in the web UI is stripped by the next sync"
      (let ((g (concat l1 "/sub"))
            (num (car (nngmail-testdrive--sandbox-nums))))
        (cond
         ((null (nngmail--group nngmail-testdrive--account g))
          (cons 'SKIP "no nested label from the previous step"))
         ((not (nngmail-testdrive--confirm
                (format "In the web UI: delete label %s (the message keeps %s). Done?" g l1)))
          (when nngmail-testdrive--may-modify
            (ignore-errors (nngmail-request-delete-group g nil nngmail-testdrive--server)))
          'SKIP)
         (t
          (let ((id (nngmail--group-label-or-nil nngmail-testdrive--account g)))
            (nngmail-testdrive--wait-for
             (lambda () (not (nngmail--group-label-or-nil nngmail-testdrive--account g))) 90
             (format "label %s to disappear" g))
            (nngmail-testdrive--check
             (and (not (nngmail--group-label-or-nil nngmail-testdrive--account g))
                  (= 0 (nngmail--group-size nngmail-testdrive--account g))
                  (or (null num) (not (member id (nngmail-testdrive--labels num)))))
             (format "group size %d, message labels %S"
                     (nngmail--group-size nngmail-testdrive--account g)
                     (and num (nngmail-testdrive--labels num)))))))))))

(defun nngmail-testdrive--in-summary (num group fn)
  "Open GROUP's summary on server article NUM and call FN there; then leave."
  (let ((local (nngmail--num->local nngmail-testdrive--account group num)))
    (unless local (error "article %d is not in %s" num group))
    (gnus-summary-read-group (nngmail-testdrive--group group) nil t nil nil (list local))
    (unwind-protect
        (progn (gnus-summary-goto-article local nil t)
               (funcall fn local))
      (nngmail-testdrive--leave-summary))))

(defun nngmail-testdrive--section-4b ()
  (nngmail-testdrive--out "\n* 4b. Label editing from the summary (sandbox messages only)")
  (let* ((sandbox nngmail-testdrive-label)
         (extra (concat nngmail-testdrive-label "-edit"))
         (extra-id nil)
         (num (car (nngmail-testdrive--sandbox-nums))))
    (nngmail-testdrive--test 4 "setup: a second throwaway label for editing"
      (nngmail-testdrive--modifying
        (if (null num) (cons 'SKIP "no sandbox message (run section 2)")
          (unless (nngmail--group-label nngmail-testdrive--account extra)
            (nngmail-request-create-group extra nngmail-testdrive--server))
          (setq extra-id (nngmail--group-label nngmail-testdrive--account extra))
          (nngmail-testdrive--check extra-id "could not create the label"))))
    (nngmail-testdrive--test 4 "L + adds a label; other labels untouched"
      (nngmail-testdrive--modifying
        (if (not (and num extra-id)) 'SKIP
          (let ((before (nngmail-testdrive--labels num)))
            (nngmail-testdrive--in-summary num sandbox
                                           (lambda (_) (nngmail-summary-add-label extra-id)))
            (nngmail-testdrive--settle)
            (let ((live (nngmail-testdrive--live-labels num)))
              (nngmail-testdrive--check
               (and (member extra-id live)
                    (cl-every (lambda (l) (member l live)) before))
               (format "before=%S live=%S" before live)))))))
    (nngmail-testdrive--test 4 "L - removes only that label"
      (nngmail-testdrive--modifying
        (if (not (and num extra-id)) 'SKIP
          (nngmail-testdrive--in-summary num sandbox
                                         (lambda (_) (nngmail-summary-remove-label extra-id)))
          (nngmail-testdrive--settle)
          (let ((live (nngmail-testdrive--live-labels num)))
            (nngmail-testdrive--check
             (and (not (member extra-id live))
                  (member nngmail-testdrive--label-id live))
             (format "live=%S" live))))))
    (nngmail-testdrive--test 4 "L e edits the whole set in one go"
      (nngmail-testdrive--modifying
        (if (not (and num extra-id)) 'SKIP
          ;; Answer the completing read with sandbox + extra + INBOX.
          (cl-letf (((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) (list sandbox extra "INBOX"))))
            (nngmail-testdrive--in-summary num sandbox
                                           (lambda (_) (nngmail-summary-edit-labels))))
          (nngmail-testdrive--settle)
          (let ((live (nngmail-testdrive--live-labels num)))
            (prog1 (nngmail-testdrive--check
                    (and (member extra-id live) (member "INBOX" live)
                         (member nngmail-testdrive--label-id live))
                    (format "live=%S" live))
              ;; Put it back out of INBOX right away.
              (nngmail-testdrive--relabel num nil '("INBOX")))))))
    (nngmail-testdrive--test 4 "L a archives: removes INBOX, keeps the rest"
      (nngmail-testdrive--modifying
        (if (not (and num extra-id)) 'SKIP
          (nngmail-testdrive--relabel num '("INBOX") nil)
          (nngmail-testdrive--in-summary num sandbox
                                         (lambda (_) (nngmail-summary-archive)))
          (nngmail-testdrive--settle)
          (let ((live (nngmail-testdrive--live-labels num)))
            (nngmail-testdrive--check
             (and (not (member "INBOX" live))
                  (member extra-id live)
                  (member nngmail-testdrive--label-id live))
             (format "live=%S" live))))))
    (nngmail-testdrive--test 4 "B m moves relative to the current group only"
      (nngmail-testdrive--modifying
        (if (not (and num extra-id)) 'SKIP
          ;; In the extra group, move to INBOX: extra goes, sandbox stays.
          (nngmail-request-move-article
           (nngmail--num->local nngmail-testdrive--account extra num)
           extra nngmail-testdrive--server nil nil "INBOX")
          (nngmail-testdrive--settle)
          (let ((live (nngmail-testdrive--live-labels num)))
            (prog1 (nngmail-testdrive--check
                    (and (member "INBOX" live)
                         (not (member extra-id live))
                         (member nngmail-testdrive--label-id live))
                    (format "live=%S" live))
              (nngmail-testdrive--relabel num nil '("INBOX")))))))
    (nngmail-testdrive--test 4 "summary line shows the other labels"
      (if (not (and num extra-id)) 'SKIP
        (nngmail-testdrive--relabel num (list extra-id) nil)
        (nngmail-testdrive--in-summary
         num sandbox
         (lambda (_)
           (let ((h (gnus-summary-article-header)))
             (nngmail-testdrive--check
              (string-match-p (regexp-quote extra) (gnus-user-format-function-gmail-labels h))
              (format "labels column: %S" (gnus-user-format-function-gmail-labels h))))))))
    (nngmail-testdrive--test 4 "cleanup: delete the editing label"
      (nngmail-testdrive--modifying
        (if (null extra-id) 'SKIP
          (when num (nngmail-testdrive--relabel num nil (list extra-id)))
          (nngmail-request-delete-group extra nil nngmail-testdrive--server)
          (nngmail-testdrive--check
           (not (nngmail--group-label nngmail-testdrive--account extra))
           "label still present"))))))

(defun nngmail-testdrive--section-5 ()
  (nngmail-testdrive--out "\n* 5. Trash, expiry, permanent delete (sandbox messages only)")
  (let ((l1 nngmail-testdrive-label) num)
    (nngmail-testdrive--test 5 "seed: a fresh sandbox message"
      (nngmail-testdrive--modifying
        (setq num (nngmail-testdrive--send "trash me" "This message exists to be trashed."))
        (nngmail-testdrive--check num "did not arrive within 90s")))
    (nngmail-testdrive--test 5 "B DEL trashes"
      (nngmail-testdrive--modifying
        (if (null num) 'SKIP
          (nngmail-testdrive--expire num l1 'trash)
          (let ((live (nngmail-testdrive--live-labels num)))
            (nngmail-testdrive--check (and (member "TRASH" live)
                                           (nngmail--group-member-p nngmail-testdrive--account "[Gmail]/Trash" num)
                                           (not (nngmail--group-member-p nngmail-testdrive--account "[Gmail]/All Mail" num)))
                                      (format "live: %S" live))))))
    (nngmail-testdrive--test 5 "B m out of Trash restores"
      (nngmail-testdrive--modifying
        (if (null num) 'SKIP
          (nngmail-testdrive--move num "[Gmail]/Trash" l1)
          (let ((live (nngmail-testdrive--live-labels num)))
            (nngmail-testdrive--check (and (not (member "TRASH" live))
                                           (member nngmail-testdrive--label-id live))
                                      (format "live: %S" live))))))
    (nngmail-testdrive--test 5 "expire with 'archive removes the label only"
      (nngmail-testdrive--modifying
        (if (null num) 'SKIP
          (nngmail-testdrive--expire num l1 'archive)
          (let ((live (nngmail-testdrive--live-labels num)))
            (prog1 (nngmail-testdrive--check (and (not (member "TRASH" live))
                                                  (not (member nngmail-testdrive--label-id live)))
                                             (format "live: %S" live))
              ;; back into the sandbox
              (nngmail-testdrive--relabel num (list nngmail-testdrive--label-id) nil))))))
    (nngmail-testdrive--test 5 "expire from Trash deletes permanently"
      (nngmail-testdrive--modifying
        (if (null num) 'SKIP
          (nngmail-testdrive--expire num l1 'trash)
          (nngmail-testdrive--expire num "[Gmail]/Trash" 'trash)
          (nngmail-testdrive--check (not (nngmail-testdrive--exists-p num))
                                    "message still exists at Gmail"))))))

(defun nngmail-testdrive--section-6 ()
  (nngmail-testdrive--out "\n* 6. Sending (from you to you, filed in the sandbox)")
  (let (num)
    (nngmail-testdrive--test 6 "plain message round-trips"
      (nngmail-testdrive--modifying
        (setq num (nngmail-testdrive--send "plain" "Plain text body.\n"))
        (nngmail-testdrive--check num "did not arrive within 90s")))
    (nngmail-testdrive--test 6 "has SENT label"
      (if (null num) 'SKIP
        (nngmail-testdrive--check
         (nngmail-testdrive--wait-for (lambda () (member "SENT" (nngmail-testdrive--labels num))) 30 "SENT")
         "no SENT label")))
    (nngmail-testdrive--test 6 "reply threads with the original"
      (nngmail-testdrive--modifying
        (if (null num) 'SKIP
          ;; Gmail threads on References *and* a matching subject; the
          ;; drive's prefix is part of the subject, so "Re: " + the same.
          (let ((rnum (let ((nngmail-testdrive-subject-prefix "Re: "))
                        (nngmail-testdrive--send (concat (string-trim nngmail-testdrive-subject-prefix) " plain")
                                                 "Reply body.\n" nil num))))
            (if (null rnum) (cons 'FAIL "reply did not arrive")
              (let* ((h (nngmail-testdrive--header rnum))
                     (o (nngmail-testdrive--header num))
                     (ot (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra o))))
                     (rt (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra h)))))
                (nngmail-testdrive--check
                 (equal rt ot)
                 (format "Gmail put the reply in thread %s, original is %s (threadId sent: %S)"
                         rt ot (nngmail-testdrive--last-send-thread)))))))))
    (nngmail-testdrive--test 6 "attachment survives"
      (nngmail-testdrive--modifying
        (pcase-let ((`(,file . ,temporary) (nngmail-testdrive--attachment)))
          (unwind-protect
              (if (not (file-readable-p file))
                  (cons 'SKIP (format "no attachment file: %s" file))
                (let ((anum (nngmail-testdrive--send "attachment" "See attached.\n" file)))
                  (if (null anum) (cons 'FAIL "did not arrive")
                    (nngmail-testdrive--show anum nngmail-testdrive-label)
                    (prog1 (nngmail-testdrive--ask
                            (format "Is the attachment (%s) present and openable?"
                                    (file-name-nondirectory file)))
                      (nngmail-testdrive--leave-summary)))))
            (when temporary (ignore-errors (delete-file file)))))))
    (nngmail-testdrive--test 6 "non-ASCII subject and body"
      (nngmail-testdrive--modifying
        (let ((unum (nngmail-testdrive--send "Tëst ✓ 日本" "Körper mit Umlauten und ✓.\n")))
          (if (null unum) (cons 'FAIL "did not arrive")
            (let ((h (nngmail-testdrive--header unum)))
              (if (not (string-match-p "Tëst ✓ 日本" (mail-header-subject h)))
                  (cons 'FAIL (format "subject came back as %S" (mail-header-subject h)))
                (nngmail-testdrive--show unum nngmail-testdrive-label)
                (prog1 (nngmail-testdrive--ask "Subject decoded; is the body (umlauts, ✓) correct too?")
                  (nngmail-testdrive--leave-summary))))))))))

(defun nngmail-testdrive--section-7 ()
  (nngmail-testdrive--out "\n* 7. Search (read-only)")
  (nngmail-testdrive--test 7 "raw Gmail query returns results"
    (let* ((me (nngmail-account-user nngmail-testdrive--account))
           (nums (nngmail--search nngmail-testdrive--account (format "to:%s newer_than:30d" me) 20)))
      (nngmail-testdrive--check nums "no results for a query that should match recent mail")))
  (nngmail-testdrive--test 7 "parsed query transforms"
    (let* ((e (make-instance 'gnus-search-gmail))
           (q (gnus-search-transform e '((subject . "testdrive") (since 1 1 2026)))))
      (nngmail-testdrive--check (equal q "subject:testdrive after:2026/01/01") (format "got %S" q))))
  (nngmail-testdrive--test 7 "gnus-search engine end to end"
    (let* ((srv (gnus-method-to-server nngmail-testdrive--method))
           (engine (gnus-search-server-to-engine srv))
           (res (and engine (gnus-search-run-search
                             engine srv `((query . ,(string-trim nngmail-testdrive-subject-prefix)) (raw . t)) nil))))
      (nngmail-testdrive--check (and engine (vectorp res))
                                (format "engine=%S results=%S" (and engine (eieio-object-class engine))
                                        (and res (length res)))))))

(defun nngmail-testdrive--section-8 ()
  (nngmail-testdrive--out "\n* 8. Robustness (read-only; fetches more headers)")
  (nngmail-testdrive--test 8 "full group rebuild agrees with the incremental state"
    ;; After all the label churn above, a full refile from the labels
    ;; table must change no group's size: proves the incremental paths
    ;; left nothing behind.  Walks the index; progress is shown.
    (let* ((a nngmail-testdrive--account)
           (sizes (let (out) (maphash (lambda (n g) (push (cons n (hash-table-count (nngmail-group-local->num g))) out))
                                      (nngmail-account-groups a))
                       out))
           (t0 (float-time)))
      (nngmail-rebuild-groups nngmail-testdrive--server)
      (let ((diff (seq-remove (lambda (c) (= (cdr c) (nngmail--group-size a (car c)))) sizes))
            (secs (- (float-time) t0)))
        (cons (if diff 'FAIL 'PASS)
              (format "%d groups, %s%s" (length sizes) (nngmail--fmt-duration secs)
                      (if diff (format "; sizes changed: %S"
                                       (mapcar (lambda (c) (list (car c) (cdr c) (nngmail--group-size a (car c))))
                                               (seq-take diff 5)))
                        ""))))))
  (nngmail-testdrive--test 8 "token refresh is transparent"
    (nngmail--force-refresh nngmail-testdrive--account)
    (let ((nngmail-debug t) (before (buffer-size (get-buffer-create "*nngmail-log*"))))
      (nngmail--get nngmail-testdrive--account "profile")
      (with-current-buffer "*nngmail-log*"
        (nngmail-testdrive--check (not (string-match-p "401" (buffer-substring before (point-max))))
                                  "saw a 401 after a forced refresh"))))
  (nngmail-testdrive--test 8 "a full-size batch round-trips through curl"
    ;; `nngmail-batch-size' parts in one multipart body: exercises
    ;; --data-binary (CRLF must survive) and the size Gmail accepts.
    (let* ((a nngmail-testdrive--account)
           (size (or nngmail--batch-size-current nngmail-batch-size))
           (ids (nngmail--list-ids a nil '("INBOX") size)))
      (if (< (length ids) 2) (cons 'SKIP "INBOX too small")
        (let* ((nngmail--batch-part-failures nil)
               (t0 (float-time))
               (res (nngmail--batch a (mapcar (lambda (id) (list id "GET" (nngmail--metadata-path id))) ids)))
               (ok (cl-count-if #'cdr res)))
          (cons (if (= ok (length ids)) 'PASS 'FAIL)
                (format "%d/%d parts answered in %.1fs; part failures: %d; batch size now %d, ceiling %s"
                        ok (length ids) (- (float-time) t0) (length nngmail--batch-part-failures)
                        (or nngmail--batch-size-current nngmail-batch-size)
                        (or nngmail--batch-size-ceiling "none")))))))
  (nngmail-testdrive--test 8 "a timer firing mid-request does not deadlock or double up"
    ;; Simulates the Gnus demon: a sync requested while a batch is out
    ;; must be skipped (nngmail-busy), not waited for, and the batch must
    ;; still complete.
    (let* ((a nngmail-testdrive--account)
           (ids (nngmail--list-ids a nil '("INBOX") 5))
           (inner nil) (t0 (float-time)))
      (if (null ids) (cons 'SKIP "INBOX empty")
        (run-at-time 0.05 nil
                     (lambda ()
                       (setq inner (condition-case e
                                       (progn (nngmail--batch a (list (list (car ids) "GET" (nngmail--metadata-path (car ids)))))
                                              'sent)
                                     (nngmail-busy 'refused)
                                     (error (list 'error (error-message-string e)))))))
        (let ((res (nngmail--batch a (mapcar (lambda (id) (list id "GET" (nngmail--metadata-path id))) ids))))
          ;; Give the timer a moment if it has not fired yet.
          (let ((deadline (+ (float-time) 3)))
            (while (and (null inner) (< (float-time) deadline)) (sit-for 0.1)))
          (cons (if (and (eq inner 'refused) (= (length res) (length ids)) (< (- (float-time) t0) 60)) 'PASS 'FAIL)
                (format "outer batch %d/%d in %.1fs; nested call from timer: %S (want refused)"
                        (cl-count-if #'cdr res) (length ids) (- (float-time) t0) inner))))))
  (nngmail-testdrive--test 8 "interrupted fetch resumes on next sync"
    ;; Interrupt a small, bounded fetch: the sandbox label's newest 40
    ;; (there are only a handful), forced to refetch so batches happen.
    (let* ((account nngmail-testdrive--account)
           (nums (seq-take (nngmail-testdrive--sandbox-nums) 40))
           (ids (delq nil (mapcar (lambda (n) (gethash n (nngmail-account-num->id account))) nums)))
           (interrupted nil))
      (if (< (length ids) 2)
          (cons 'SKIP "needs at least 2 sandbox messages (run section 2)")
        (cl-letf* ((orig (symbol-function 'nngmail--batch))
                   (calls 0)
                   (nngmail-batch-size 1)          ; one message per batch
                   ((symbol-function 'nngmail--batch)
                    (lambda (&rest args)
                      (cl-incf calls)
                      (if (= calls 2)
                          (progn (setq interrupted t) (signal 'nngmail-aborted (list 0 0)))
                        (apply orig args)))))
          (condition-case nil
              (nngmail--fetch-in-chunks account ids "interrupt drill" t)
            (nngmail-aborted nil)))
        (if (not interrupted)
            (cons 'SKIP "fewer than 2 batches were needed; nothing to interrupt")
          (let ((pending (nngmail-account-pending account)))
            (nngmail-testdrive--sync)
            (nngmail-testdrive--check (and pending (null (nngmail-account-pending account)))
                                      (format "pending before=%S after=%S"
                                              (and pending (length (nth 2 pending)))
                                              (nngmail-account-pending account))))))))
  (nngmail-testdrive--test 8 "quota prompt and countdown (interactive, ~20 s)"
    (if (not (nngmail-testdrive--confirm
              "Next: a tiny quota budget forces the prompt while re-fetching the sandbox messages. Answer w once, then W. Ready?"))
        'SKIP
      (let* ((account nngmail-testdrive--account)
             (ids (delq nil (mapcar (lambda (n) (gethash n (nngmail-account-num->id account)))
                                    (nngmail-testdrive--sandbox-nums))))
             ;; Budget of 20 units: the second 5-unit fetch overruns at once.
             (nngmail-quota-pacing 'always)
             (nngmail-quota-per-minute 20)
             (nngmail-quota-burst 20)
             (nngmail-quota-policy 'ask)
             (nngmail--session-quota-policy nil)
             (nngmail--throttled nil)
             (nngmail--quota-log nil)
             (nngmail-batch-size 1))
        (if (< (length ids) 3)
            (cons 'SKIP "needs at least 3 sandbox messages (run section 2)")
          (unwind-protect
              (progn
                (nngmail--fetch-in-chunks account ids "quota drill" t)
                (nngmail-testdrive--ask "Did you see the prompt, a live countdown after w, and no prompt after W?"))
            (nngmail-quota-reset-policy)))))))

(defun nngmail-testdrive--cleanup ()
  (nngmail-testdrive--out "\n* Cleanup")
  (when nngmail-testdrive--may-modify
    (nngmail-testdrive--test 9 "trash the drive's own messages"
      (let ((n 0))
        (dolist (num (copy-sequence (nngmail-testdrive--sandbox-nums)))
          (when (nngmail-testdrive--own-p num)
            (ignore-errors (nngmail-testdrive--expire num nngmail-testdrive-label 'trash))
            (cl-incf n)))
        (cons 'PASS (format "%d sandbox messages moved to Trash (recoverable for 30 days)" n))))
    (nngmail-testdrive--test 9 "delete the sandbox label"
      (if (nngmail-testdrive--sandbox-nums)
          (cons 'SKIP "label still has messages the drive does not own; left in place")
        (nngmail-request-delete-group nngmail-testdrive-label nil nngmail-testdrive--server)
        (nngmail-testdrive--check (not (nngmail--group-label nngmail-testdrive--account nngmail-testdrive-label))
                                  "label still present")))
    (nngmail--save nngmail-testdrive--account)))

;;;; Driver

(defun nngmail-testdrive--section-9 ()
  (nngmail-testdrive--out "\n* 9. efrit integration (sandbox messages only)")
  (nngmail-testdrive--test 9 "efrit-gnus and nngmail-efrit load and register their tools"
    (if (not (and (locate-library "efrit-tool-registry") (locate-library "efrit-gnus")))
        (cons 'SKIP "efrit (with efrit-tool-registry and efrit-gnus) not on load-path")
      (require 'nngmail-efrit)
      (nngmail-efrit-ensure-tools)
      (let ((names (mapcar #'car (symbol-value 'efrit-tool-registry))))
        (nngmail-testdrive--check
         (cl-every (lambda (n) (member n names))
                   '("gnus_groups" "gnus_search" "gnus_articles" "gmail_search" "gmail_thread" "gmail_labels"))
         (format "registered: %S" names)))))
  (nngmail-testdrive--test 9 "sandbox messages render as text for the model"
    (if (not (featurep 'efrit-gnus)) 'SKIP
      (let ((nums (nngmail-testdrive--sandbox-nums)))
        (if (null nums) (cons 'SKIP "no sandbox messages (run section 2)")
          (pcase-let ((`(,text ,sent ,chars)
                       (efrit-gnus--payload (nngmail-efrit--refs nngmail-testdrive--account nums))))
            (nngmail-testdrive--check
             (and (= sent (length nums))
                  (string-match-p "=== Article 1 of" text)
                  (string-match-p "^Article: nngmail" text)
                  (string-match-p "^Subject: " text))
             (format "%d of %d rendered, %s chars" sent (length nums) (nngmail--fmt-num chars))))))))
  (nngmail-testdrive--test 9 "gmail_search tool answers for the sandbox subject"
    (if (not (featurep 'nngmail-efrit)) 'SKIP
      (let ((out (nngmail-efrit--tool-search
                  `(("query" . ,(format "subject:%s" (string-trim nngmail-testdrive-subject-prefix)))
                    ("account" . ,nngmail-testdrive--server)))))
        (nngmail-testdrive--check (string-match-p "gmail-id=" out)
                                  (truncate-string-to-width out 200)))))
  (nngmail-testdrive--test 9 "a Google Docs link in a sandbox message expands for efrit"
    ;; Sends a sandbox message whose body links a doc you name; the
    ;; render for efrit must contain the doc's text after the message.
    ;; Needs the Drive scope on the token; without it the step reports
    ;; the exact error the user would see.
    (cond
     ((not (featurep 'efrit-gnus)) 'SKIP)
     ((not nngmail-testdrive--may-modify) (cons 'SKIP "modifications not approved"))
     (t
      (let ((url (read-string "URL of a Google Doc shared with you (empty to skip): ")))
        (if (string-empty-p url) 'SKIP
          (let ((num (nngmail-testdrive--send "gdoc" (format "Notes here: %s\n" url))))
            (if (null num) (cons 'FAIL "message did not arrive")
              (let* ((refs (nngmail-efrit--refs nngmail-testdrive--account (list num)))
                     (text (car (efrit-gnus--payload refs))))
                (cond
                 ((string-match "\\[Google Doc not fetched: \\(.*\\)\\]" text)
                  (cons 'FAIL (format "doc not fetched: %s" (match-string 1 text))))
                 ((string-match-p "--- Linked document: .*\nTitle: " text)
                  (cons 'PASS (format "doc text present (%d chars total)" (length text))))
                 (t (cons 'FAIL "no linked-document block in the render")))))))))))
  (nngmail-testdrive--test 9 "analyze sandbox messages with efrit (look at the answer)"
    (cond
     ((not (featurep 'efrit-gnus)) 'SKIP)
     ((null (nngmail-testdrive--sandbox-nums)) (cons 'SKIP "no sandbox messages"))
     ((not (nngmail-testdrive--confirm
            "Send the sandbox messages to efrit's model with the 'summarize' prompt?  (costs tokens)"))
      'SKIP)
     (t
      (let ((efrit-gnus-confirm nil))
        (efrit-gnus--submit (nngmail-efrit--refs nngmail-testdrive--account (nngmail-testdrive--sandbox-nums))
                            "summarize"
                            (format "group %s" nngmail-testdrive-label) nngmail-testdrive-label))
      (nngmail-testdrive--ask "Did efrit answer about the sandbox messages (subjects, senders right)?")))))

(defconst nngmail-testdrive--sections
  '((0 "Setup sanity" nngmail-testdrive--section-0)
    (1 "Reading" nngmail-testdrive--section-1)
    (2 "Marks on sandbox messages" nngmail-testdrive--section-2)
    (3 "Gmail -> Gnus (you act in the web UI)" nngmail-testdrive--section-3)
    (4 "Labels as groups" nngmail-testdrive--section-4)
    (4.5 "Label editing from the summary" nngmail-testdrive--section-4b)
    (5 "Trash and expiry" nngmail-testdrive--section-5)
    (6 "Sending" nngmail-testdrive--section-6)
    (7 "Search" nngmail-testdrive--section-7)
    (8 "Robustness" nngmail-testdrive--section-8)
    (9 "efrit (needs efrit; sends sandbox text to the model)" nngmail-testdrive--section-9)))

(defun nngmail-testdrive--summary ()
  (let* ((rs (reverse nngmail-testdrive--results))
         (count (lambda (st) (cl-count st rs :key #'caddr))))
    (nngmail-testdrive--out "\n* Summary: %d PASS, %d FAIL, %d SKIP in %s"
                            (funcall count 'PASS) (funcall count 'FAIL) (funcall count 'SKIP)
                            (nngmail--fmt-duration (apply #'+ (mapcar (lambda (r) (or (nth 4 r) 0)) rs))))
    (dolist (r rs)
      (when (and (nth 4 r) (> (nth 4 r) nngmail-testdrive-test-budget))
        (nngmail-testdrive--out "- SLOW [%s] %s: %.0fs" (car r) (cadr r) (nth 4 r))))
    (dolist (r rs)
      (when (eq (caddr r) 'FAIL)
        (nngmail-testdrive--out "- FAIL [%s] %s%s" (car r) (cadr r)
                                (if (nth 3 r) (concat ": " (nth 3 r)) ""))))
    (nngmail-testdrive--out "\nThis buffer contains no message bodies and can be pasted into a bug report.")))

;;;###autoload
(defun nngmail-testdrive (&optional one-section)
  "Walk through the nngmail live test plan interactively.
With prefix ONE-SECTION, run just one chosen section.

You are asked once whether the drive may modify the account.  If you
decline, only read-only tests run.  If you accept, the drive still never
touches a message it did not send: mutating tests operate on a sandbox
label (`nngmail-testdrive-label') and on messages it sends from you to
you, and a guard refuses any mutation on other articles.  Sandbox
messages are trashed (not deleted) and the label removed at the end.
The read-only steps that show you a real article prefer one that is
already read; if an unread or starred one had to be shown, its UNREAD
and STARRED are put back at Gmail when the summary is left."
  (interactive "P")
  (let* ((server (nngmail--read-server)))
    (setq nngmail-testdrive--server server
          nngmail-testdrive--method (list 'nngmail server)
          nngmail-testdrive--account (nngmail--account server)
          nngmail-testdrive--results nil
          nngmail-testdrive--own (make-hash-table :test #'eql)
          nngmail-testdrive--restore-marks nil
          nngmail-testdrive--label-id nil)
    (with-current-buffer (nngmail-testdrive--buf)
      (let ((inhibit-read-only t)) (erase-buffer))
      (setq buffer-read-only nil))
    ;; Show the report once at the right, then remember this layout;
    ;; every step restores it, so nothing a hook opens (a diary, a
    ;; summary) can pile up.
    (nngmail-testdrive--show-report)
    (setq nngmail-testdrive--layout (current-window-configuration))
    (nngmail-testdrive--out "#+TITLE: nngmail test drive: %s (%s)\n#+DATE: %s\nEmacs %s, nngmail-use-plz=%S\n%s"
                            server (nngmail-account-user nngmail-testdrive--account)
                            (format-time-string "%F %T") emacs-version nngmail-use-plz
                            (nngmail-build-info))
    (setq nngmail-testdrive--may-modify
          (yes-or-no-p
           (format (concat "Allow the drive to MODIFY %s?  It will: create label %S (deleted at the end), "
                           "send a handful of messages from you to you and file them there, and mark/move/"
                           "trash/delete ONLY those messages.  Your existing mail is never modified. ")
                   (nngmail-account-user nngmail-testdrive--account) nngmail-testdrive-label)))
    (nngmail-testdrive--out "Modifications: %s"
                            (if nngmail-testdrive--may-modify
                                (format "ALLOWED, confined to label %S and messages the drive sends" nngmail-testdrive-label)
                              "NOT allowed (read-only run)"))
    (let ((sections (if one-section
                        (let* ((names (mapcar (lambda (s) (format "%s. %s" (car s) (cadr s))) nngmail-testdrive--sections))
                               (pick (completing-read "Section: " names nil t)))
                          (list (nth (cl-position pick names :test #'equal) nngmail-testdrive--sections)))
                      nngmail-testdrive--sections)))
      ;; Section 0 sets up the sandbox; a single-section run still needs it.
      (when (and one-section (not (eql (caar sections) 0)))
        (nngmail-testdrive--section-0))
      (condition-case nil
          (progn
            (dolist (s sections)
              (pcase-let ((`(,n ,title ,fn) s))
                (when (or one-section
                          (nngmail-testdrive--confirm (format "Run section %s: %s?" n title)))
                  (funcall fn))))
            (nngmail-testdrive--cleanup))
        (nngmail-testdrive-quit
         (nngmail-testdrive--out "\n(stopped by user)")
         (ignore-errors (nngmail-testdrive--cleanup))))
      (nngmail-testdrive--summary)
      (pop-to-buffer (nngmail-testdrive--buf))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (let* ((rs nngmail-testdrive--results)
             (count (lambda (st) (cl-count st rs :key #'caddr))))
        (message "nngmail test drive finished: %d PASS, %d FAIL, %d SKIP.  The report is in %s; thank you for driving."
                 (funcall count 'PASS) (funcall count 'FAIL) (funcall count 'SKIP)
                 (buffer-name (nngmail-testdrive--buf)))))))

(provide 'nngmail-testdrive)
;;; nngmail-testdrive.el ends here
