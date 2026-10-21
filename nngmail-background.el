;;; nngmail-background.el --- Keep Gmail in sync without blocking Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov
;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; The web Gmail client is always current because it fetches in the
;; background and the view reads a local copy.  This file gives nngmail
;; the same shape.  Each account has one background JOB: a small state
;; machine that advances one step at a time from a timer.  A step either
;; sends one HTTP request through plz with a callback that schedules the
;; next step, or does a bounded piece of local work (apply one page of
;; history, store one batch of headers) and returns.  Nothing in it
;; waits, so typing, scrolling and Gnus itself never stall on Gmail.
;;
;; What Gnus sees: `g', entering a group, `M-g' read the local index at
;; once and kick the job.  The *Group* buffer shows what the index knew
;; a moment ago; the job brings it up to date and Gnus picks that up on
;; the next refresh.  Marks, article bodies, sends and searches stay on
;; the synchronous path: they are small and the user is waiting for
;; exactly that result.
;;
;; States, in order:
;;
;;   idle          nothing to do; the job sleeps `nngmail-background-interval'
;;   history       GET history.list from the saved history id, one page
;;   apply         apply that page, checkpoint (see `nngmail--apply-history-page')
;;   fetch         POST one batch of metadata requests for the ids history
;;                 left to fetch, store the parts, continue
;;
;; After an error the job keeps its state and retries the same request
;; after an exponential delay (up to `nngmail-background-backoff-max').
;; Before any request the job asks the quota bucket how long to wait and
;; sleeps that long (the status line says "pacing Ns"); that is a delay,
;; not a state.
;;
;; Quota is handled the way the synchronous path does it, minus the
;; prompt: a background job never asks.  It paces at the configured
;; budget and throttles itself when Gmail rejects parts for the
;; per-minute limit; `nngmail-background-stop' is the abort.
;;
;; Only one request is in flight per account, and it holds the same
;; slot the synchronous batch code uses (`nngmail--batch-in-flight'), so
;; a synchronous caller that runs while the job has a request out
;; backs off exactly as it does today.
;;
;; A brand-new account (no history id) still does its first sync
;; synchronously: there is no index to show while it runs.

;;; Code:

(require 'cl-lib)
(require 'nngmail)

(declare-function plz "plz")
(declare-function plz-response-p "plz")
(declare-function plz-response-status "plz")
(declare-function plz-response-body "plz")
(declare-function plz-response-headers "plz")
(declare-function gnus-alive-p "gnus-util")
(declare-function gnus-get-info "gnus")
(declare-function gnus-group-full-name "gnus")
(declare-function gnus-group-update-group "gnus-group")
(declare-function timer--time "timer")
(defvar nngmail--spinner-idle-function)
(defvar nngmail--spinner-dim)
(defvar nngmail--request-out-since)
(defvar nngmail--progress-quiet)
(defvar nngmail-background-status)
(defvar gnus-group-buffer)

(defgroup nngmail-background nil
  "Background synchronisation for nngmail."
  :group 'nngmail)

(defcustom nngmail-background t
  "Whether syncs run in the background.
When non-nil, `g' and group entry read the local index and let the
background job bring it up to date.  When nil, syncs run inline as
they always did and Emacs waits for them."
  :type 'boolean)

(defcustom nngmail-background-interval 120
  "Seconds between polls of Gmail history while nothing is happening.
A `g' or group entry polls at once regardless."
  :type 'integer)

(defcustom nngmail-background-backoff-max 300
  "Longest wait, in seconds, between retries after errors."
  :type 'integer)

(defcustom nngmail-background-step-delay 0
  "Seconds between consecutive steps while there is work.
Zero keeps the job busy; a small value such as 0.05 leaves more room for
redisplay on a slow machine."
  :type 'number)

;;;; The job

(cl-defstruct (nngmail-job (:constructor nngmail-job--make))
  account              ; the nngmail-account
  (state 'idle)        ; see Commentary
  timer                ; the pending `run-at-time', or nil
  proc                 ; the plz process in flight, or nil
  page-token           ; history.list nextPageToken within a pass
  (pages 0)            ; pages applied in the current pass
  (records 0)          ; history records applied in the current pass
  pass-started         ; `float-time' when the current history pass began
  to-fetch             ; message ids still to fetch metadata for
  fetch-total          ; how many the fetch phase started with (progress)
  fetch-force          ; FORCE flag for the fetch (labels refreshed too)
  touched              ; group names changed in this pass, for the *Group* refresh
  (errors 0)           ; consecutive errors, for backoff
  backing-off          ; non-nil while waiting out an error
  last-error           ; text of the last error
  stopped              ; non-nil after `nngmail-background-stop'
  wake-after)          ; `float-time' before which the job must not run

(defun nngmail--job-set-timer (job timer)
  "Set JOB's TIMER slot (for callers outside this file, such as the reloader)."
  (setf (nngmail-job-timer job) timer))

(defun nngmail--job-set-state (job state)
  "Set JOB's STATE slot (for callers outside this file, such as the reloader)."
  (setf (nngmail-job-state job) state))

(defvar nngmail--jobs (make-hash-table :test #'equal)
  "Account name -> `nngmail-job'.")

(defun nngmail-background-job (account)
  "The job for ACCOUNT, created if needed."
  (or (gethash (nngmail-account-name account) nngmail--jobs)
      (puthash (nngmail-account-name account)
               (nngmail-job--make :account account)
               nngmail--jobs)))

(defun nngmail--job-log (job fmt &rest args)
  "Log FMT/ARGS prefixed with JOB's account and state."
  (nngmail--log "bg %s [%s]: %s" (nngmail-account-name (nngmail-job-account job))
                (nngmail-job-state job) (apply #'format fmt args)))

(defun nngmail--job-trace (job fmt &rest args)
  "Trace FMT/ARGS prefixed with JOB's account and state."
  (nngmail--trace "bg %s [%s]: %s" (nngmail-account-name (nngmail-job-account job))
                  (nngmail-job-state job) (apply #'format fmt args)))

;;;; Scheduling

(defun nngmail--job-schedule (job delay)
  "Run JOB's next step after DELAY seconds, replacing any pending timer."
  (when (nngmail-job-timer job)
    (cancel-timer (nngmail-job-timer job)))
  (setf (nngmail-job-timer job)
        (run-at-time (max 0 delay) nil #'nngmail--job-fire job)))

(defun nngmail--job-fire (job)
  "Timer entry: one step of JOB, errors reported, never signalled."
  (setf (nngmail-job-timer job) nil)
  (unless (nngmail-job-stopped job)
    (condition-case err
        (nngmail--job-step job)
      (quit
       (nngmail--job-log job "quit during a step; retrying later")
       (nngmail--job-schedule job nngmail-background-interval))
      (error
       (nngmail--job-fail job (error-message-string err))))))

(defun nngmail--job-fail (job text)
  "Record an error TEXT for JOB and schedule a retry with backoff."
  (cl-incf (nngmail-job-errors job))
  (setf (nngmail-job-last-error job) text
        (nngmail-job-proc job) nil)
  (when (eq nngmail--batch-in-flight 'nngmail-background)
    (setq nngmail--batch-in-flight nil))
  (setf (nngmail-account-last-error (nngmail-job-account job)) text)
  (let ((wait (min nngmail-background-backoff-max
                   (* 5 (expt 2 (1- (min 8 (nngmail-job-errors job))))))))
    (nngmail--job-log job "error: %s; retry in %s" text (nngmail--fmt-duration wait))
    (nngmail--set-status (format "%s, retry %ds"
                                 (if (string-prefix-p "HTTP" text) (car (split-string text ":")) "error")
                                 (round wait)))
    ;; Keep the state: a page or batch that failed is simply sent again.
    ;; The history id and the fetch list are checkpointed, so resending
    ;; is idempotent.
    (setf (nngmail-job-backing-off job) t)
    (nngmail--job-schedule job wait)
    (nngmail--spinner-refresh-idle)))

(defun nngmail-background-kick (account &optional force)
  "Make ACCOUNT's job look at Gmail now (or as soon as it is free).
With FORCE a stopped job is restarted.  Return the job."
  (let ((job (nngmail-background-job account)))
    (when force (setf (nngmail-job-stopped job) nil
                      (nngmail-job-errors job) 0))
    (unless (nngmail-job-stopped job)
      (cond
       ((eq (nngmail-job-state job) 'idle)
        (nngmail--job-schedule job 0))
       ((and (null (nngmail-job-timer job)) (null (nngmail-job-proc job)))
        ;; Running on paper but nothing pending: a callback died.  Resume.
        (nngmail--job-trace job "kick: no step pending; resuming")
        (nngmail--job-schedule job 0))
       (t (nngmail--job-trace job "kick: already running"))))
    job))

;;;###autoload
(defun nngmail-background-stop (server)
  "Stop SERVER's background job; progress so far is kept.
`nngmail-background-start' or the next `g' starts it again."
  (interactive (list (nngmail--read-server)))
  (let ((job (nngmail-background-job (nngmail--account server))))
    (setf (nngmail-job-stopped job) t)
    (when (nngmail-job-timer job) (cancel-timer (nngmail-job-timer job)))
    (setf (nngmail-job-timer job) nil)
    (when-let* ((proc (nngmail-job-proc job)))
      (when (process-live-p proc) (delete-process proc))
      (setf (nngmail-job-proc job) nil))
    (when (eq nngmail--batch-in-flight 'nngmail-background)
      (setq nngmail--batch-in-flight nil))
    (setq nngmail--request-out-since nil)
    (nngmail--save (nngmail-job-account job))
    (setf (nngmail-job-state job) 'idle)
    (nngmail--progress (nngmail-job-account job) nil)
    (nngmail--spinner-refresh-idle)
    (nngmail--message 5 "%s: background sync stopped%s" server
                      (if (nngmail-job-to-fetch job)
                          (format ", %s messages still to fetch" (nngmail--fmt-num (length (nngmail-job-to-fetch job))))
                        ""))))

;;;###autoload
(defun nngmail-background-start (server)
  "Start (or restart) SERVER's background job."
  (interactive (list (nngmail--read-server)))
  (nngmail-background-kick (nngmail--account server) t)
  (nngmail--spinner-refresh-idle)
  (nngmail--message 5 "%s: background sync running" server))

;;;; Steps

(defun nngmail--job-step (job)
  "Advance JOB by one step."
  (let ((account (nngmail-job-account job)))
    (setf (nngmail-job-backing-off job) nil)
    (cond
     ;; A synchronous caller is using the connection: wait our turn.
     ((and nngmail--batch-in-flight (not (eq nngmail--batch-in-flight 'nngmail-background)))
      (nngmail--job-trace job "connection busy with %S; retry in 2s" nngmail--batch-in-flight)
      (nngmail--job-schedule job 2))
     ;; Quota pacing: how long must we wait before the next request?
     ((let ((wait (nngmail--job-pacing-wait job)))
        (when (> wait 0)
          (nngmail--set-status (format "pacing %ds" (ceiling wait)))
          (nngmail--job-schedule job (min wait 60))
          t)))
     (t
      (pcase (nngmail-job-state job)
        ('idle
         (if (null (nngmail-account-history-id account))
             ;; First sync: synchronous, there is nothing to show meanwhile.
             (progn
               (nngmail--job-log job "no history id; running the first sync inline")
               (setq nngmail--progress-quiet nil)
               (nngmail--incremental-sync account t)
               (nngmail--job-idle job))
           (nngmail--job-start-pass job)))
        ;; Mid-pass (next page), or retrying the page after a backoff
        ;; that kept the state.
        ('history (nngmail--job-request-page job))
        ('fetch (nngmail--job-fetch job))
        (other (error "nngmail: Unknown background state %S" other)))))))

(defun nngmail--job-pacing-wait (job)
  "Seconds JOB must wait before its next request, per the quota bucket.
Zero when none.
The cost assumed is the larger of a history page and a batch; being a
little early is harmless since the request itself is metered again."
  (ignore job)
  (if (not (pcase nngmail-quota-pacing
             ('always t)
             ('adaptive nngmail--quota-enforced)
             (_ nil)))
      0
    (let* ((units (* 5 (or nngmail--batch-size-current nngmail-batch-size)))
           (rate (/ (nngmail--quota-budget) 60.0))
           (cap (max nngmail-quota-burst units))
           (have (nngmail--bucket-level cap rate)))
      (if (>= have units) 0 (/ (- units have) rate)))))

(defun nngmail--job-refresh-group-lines (job)
  "Redraw the *Group* lines of the groups JOB's pass changed, if Gnus shows them."
  (when-let* ((groups (nngmail-job-touched job))
              (buf (and (fboundp 'gnus-alive-p) (gnus-alive-p)
                        (get-buffer gnus-group-buffer))))
    (let* ((account (nngmail-job-account job))
           (method (list 'nngmail (nngmail-account-name account))))
      (nngmail--job-trace job "refreshing %d group lines" (length groups))
      (with-current-buffer buf
        (save-excursion
          (dolist (group groups)
            (let ((full (gnus-group-full-name group method)))
              (when-let* ((info (gnus-get-info full)))
                (unless (nngmail--summary-open-p full)
                  (nngmail--update-info account group info)
                  (ignore-errors (gnus-group-update-group full t)))))))))
    (setf (nngmail-job-touched job) nil)))

(defun nngmail--job-idle (job)
  "Put JOB to sleep until the next poll."
  (nngmail--job-refresh-group-lines job)
  (let ((account (nngmail-job-account job)))
    (setf (nngmail-job-state job) 'idle
          (nngmail-job-errors job) 0
          (nngmail-job-backing-off job) nil
          (nngmail-job-proc job) nil)
    (when (eq nngmail--batch-in-flight 'nngmail-background)
      (setq nngmail--batch-in-flight nil))
    (puthash (nngmail-account-name account) (float-time) nngmail--last-sync)
    (nngmail--progress account nil)
    (nngmail--job-schedule job nngmail-background-interval)
    (nngmail--spinner-refresh-idle)))

;;; history

(defun nngmail--job-start-pass (job)
  "Begin a history pass for JOB: carry over an interrupted fetch, request page 1."
  (let ((account (nngmail-job-account job)))
    (setf (nngmail-job-state job) 'history
          (nngmail-job-page-token job) nil
          (nngmail-job-touched job) nil
          (nngmail-job-pages job) 0
          (nngmail-job-records job) 0
          (nngmail-job-pass-started job) (float-time)
          (nngmail-job-to-fetch job) (nngmail--pending-history-ids account)
          (nngmail-job-fetch-force job) t)
    (setq nngmail--progress-quiet (eq nngmail-background-status 'mode-line))
    (nngmail--progress account "checking for changes" nil nil t)
    (nngmail--job-request-page job)))

(defun nngmail--job-request-page (job)
  "Send the history.list request for JOB's current page."
  (let* ((account (nngmail-job-account job))
         (params (append `(("startHistoryId" . ,(nngmail-account-history-id account))
                           ("maxResults" . 500))
                         (when (nngmail-job-page-token job)
                           `(("pageToken" . ,(nngmail-job-page-token job)))))))
    (nngmail--job-send job "GET" (nngmail--url "history" params) nil nil
                       (lambda (status _headers text)
                         (nngmail--job-got-page job status text)))))

(defun nngmail--job-got-page (job status text)
  "Handle a history.list response with STATUS and body TEXT for JOB."
  (let ((account (nngmail-job-account job)))
    (cond
     ((= status 404)
      ;; History expired: Gmail says start over.  That is a full sync,
      ;; synchronous (nothing meaningful to show while the index rebuilds).
      (nngmail--job-log job "history expired; full sync inline")
      (setf (nngmail-account-history-id account) nil)
      (setq nngmail--progress-quiet nil)
      (nngmail--full-sync account)
      (nngmail--job-idle job))
     ((not (<= 200 status 299))
      (nngmail--job-http-failure job status text))
     (t
      (let* ((resp (nngmail--json-parse text))
             (history (alist-get 'history resp))
             (token (alist-get 'nextPageToken resp)))
        (setf (nngmail-job-state job) 'apply
              (nngmail-job-last-error job) nil)
        (cl-incf (nngmail-job-pages job))
        (let* ((nngmail--touched-groups (nngmail-job-touched job))
               (ids (nngmail--apply-history-page account history token (alist-get 'historyId resp)
                                                 (nngmail-job-to-fetch job))))
          (cl-incf (nngmail-job-records job) (length history))
          (setf (nngmail-job-to-fetch job) ids
                (nngmail-job-page-token job) token
                (nngmail-job-touched job) nngmail--touched-groups))
        (nngmail--history-progress account (nngmail-job-pages job) (nngmail-job-records job)
                                   (nngmail-job-pass-started job))
        (if token
            (progn (setf (nngmail-job-state job) 'history)
                   (nngmail--job-schedule job nngmail-background-step-delay))
          (nngmail--job-pass-done job)))))))

(defun nngmail--job-pass-done (job)
  "The history pass of JOB ended: start fetching what it left, or sleep."
  (let* ((account (nngmail-job-account job))
         (ids (seq-uniq (nngmail-job-to-fetch job))))
    (nngmail--log "%s history records in %d page%s%s"
                  (nngmail--fmt-num (nngmail-job-records job)) (nngmail-job-pages job)
                  (if (= 1 (nngmail-job-pages job)) "" "s")
                  (if ids (format "; %s messages to fetch" (nngmail--fmt-num (length ids)))
                    " applied, nothing to fetch"))
    (if (null ids)
        (nngmail--job-idle job)
      (setf (nngmail-job-state job) 'fetch
            (nngmail-job-to-fetch job) ids
            (nngmail-job-fetch-total job) (length ids)
            (nngmail-account-pending account) (list "fetching changed messages" t ids)
            (nngmail-account-dirty account) t)
      (nngmail--save account)
      (nngmail--job-schedule job nngmail-background-step-delay))))

;;; fetch

(defun nngmail--job-fetch (job)
  "Send one batch of metadata requests for JOB's remaining ids."
  (let* ((account (nngmail-job-account job))
         (size (max 1 (min (or nngmail--batch-size-current nngmail-batch-size) nngmail-sync-chunk)))
         (ids (nngmail-job-to-fetch job)))
    (if (null ids)
        (nngmail--job-fetch-done job)
      (let* ((chunk (seq-take ids size))
             (requests (mapcar (lambda (id) (list id "GET" (nngmail--metadata-path id))) chunk))
             (bb (nngmail--batch-body requests)))
        (nngmail--progress account "fetching changed messages"
                           (- (nngmail-job-fetch-total job) (length ids))
                           (nngmail-job-fetch-total job))
        (nngmail--job-send job "POST" nngmail--batch-api (cdr bb)
                           (concat "multipart/mixed; boundary=" (car bb))
                           (lambda (status _headers text)
                             (nngmail--job-got-batch job chunk status text)))))))

(defun nngmail--job-got-batch (job chunk status text)
  "Store the parts of JOB's metadata batch for CHUNK.
STATUS and TEXT are the response."
  (let ((account (nngmail-job-account job)))
    (if (not (<= 200 status 299))
        (nngmail--job-http-failure job status text)
      (let* ((parts (nngmail--parse-batch text))
             (failures nngmail--batch-part-failures)
             (limited (seq-filter (lambda (f) (memq (nth 1 f) '(429 403 500 502 503 504))) failures))
             (limited-ids (mapcar #'car limited))
             (stored 0))
        (nngmail--with-transaction account
          (let ((nngmail--touched-groups (nngmail-job-touched job)))
            (pcase-dolist (`(,id . ,msg) parts)
              (when msg
                (let ((num (nngmail--assign account id)))
                  (nngmail--store-header account num (nngmail--make-header num msg)
                                         (alist-get 'labelIds msg))
                  (cl-incf stored))))
            (setf (nngmail-job-touched job) nngmail--touched-groups))
          ;; Parts that bounced go to the back of the queue; the rest are done
          ;; (stored, or 404: the message is gone, nothing to fetch).
          (setf (nngmail-job-to-fetch job)
                (append (seq-remove (lambda (id) (member id chunk)) (nngmail-job-to-fetch job))
                        limited-ids))
          (setf (nngmail-account-pending account)
                (and (nngmail-job-to-fetch job)
                     (list "fetching changed messages" t (nngmail-job-to-fetch job)))
                (nngmail-account-dirty account) t)
          (nngmail--save account))
        (nngmail--job-trace job "batch: %d stored, %d bounced, %d left"
                            stored (length limited-ids) (length (nngmail-job-to-fetch job)))
        (setf (nngmail-job-errors job) 0)
        (if limited
            (nngmail--job-limited job limited (length chunk))
          ;; Grow the batch back gently after a clean one.
          (let ((cap (min nngmail-batch-size (or nngmail--batch-size-ceiling nngmail-batch-size))))
            (when (and nngmail--batch-size-current (< nngmail--batch-size-current cap))
              (setq nngmail--batch-size-current (min cap (+ nngmail--batch-size-current 5)))))
          (nngmail--job-schedule job nngmail-background-step-delay))))))

(defun nngmail--job-limited (job limited taken)
  "Some parts of JOB's batch bounced: adjust size and pacing, wait, retry.
LIMITED are the failures (ID STATUS REASON MESSAGE); TAKEN the batch size."
  (let* ((size (or nngmail--batch-size-current nngmail-batch-size))
         (quota-p (nngmail--batch-failures-quota-p limited))
         (concurrent-p (seq-some (lambda (f) (string-match-p "concurrent" (or (nth 3 f) ""))) limited))
         (wait
          (cond
           (quota-p
            (unless nngmail--quota-enforced
              (nngmail--message 5 "Gmail per-user quota reached; pacing at %s units/min (M-x nngmail-quota-help)"
                                (nngmail--fmt-num nngmail-quota-per-minute)))
            (setq nngmail--quota-enforced t
                  nngmail--batch-size-current (max 10 (/ size 2)))
            (nngmail--bucket-drain)
            (let ((per-sec (/ nngmail-quota-per-minute 60.0)))
              (max 3 (min 30 (ceiling (/ (* 5 (length limited)) per-sec))))))
           (concurrent-p
            (setq nngmail--batch-parallel-current 1
                  nngmail--batch-size-ceiling (max 10 (min (or nngmail--batch-size-ceiling size) (1- size))))
            (when (> size 10)
              (setq nngmail--batch-size-current (max 10 (/ (* size 2) 3))))
            2)
           (t
            (setq nngmail--batch-size-current (max 5 (/ size 2)))
            5))))
    (nngmail--job-log job "%d/%d parts limited (%s); batch size %d, retry in %ds"
                      (length limited) taken
                      (cond (quota-p "per-minute quota") (concurrent-p "concurrency") (t "rate limit"))
                      nngmail--batch-size-current wait)
    (nngmail--set-status (format "%d bounced, retry %ds" (length limited) wait))
    (nngmail--job-schedule job wait)))

(defun nngmail--job-fetch-done (job)
  "All metadata fetched for JOB: clear the pending list and sleep."
  (let ((account (nngmail-job-account job)))
    (setf (nngmail-account-pending account) nil
          (nngmail-account-dirty account) t)
    (nngmail--save account)
    (nngmail--job-log job "fetch complete")
    (nngmail--job-idle job)))

;;;; HTTP, asynchronous

(defun nngmail--job-send (job method url body content-type callback)
  "Send METHOD URL with BODY for JOB; call CALLBACK with (STATUS HEADERS TEXT).
Transport failures and curl errors arrive as status 0.  A 401 is retried
once after a token refresh before CALLBACK sees it.  Takes the shared
in-flight slot while the request is out."
  (require 'plz)
  (let* ((account (nngmail-job-account job))
         (access (nngmail--obtain-token account))
         (headers `(("Authorization" . ,(concat "Bearer " access))
                    ,@(when body `(("Content-Type" . ,(or content-type "application/json"))))
                    ("Accept" . "application/json")))
         (started (float-time))
         (deliver
          (lambda (status hdrs text)
            (setf (nngmail-job-proc job) nil)
            (setq nngmail--request-out-since nil)
            (when (and nngmail--status (string-prefix-p "waiting" nngmail--status))
              (nngmail--set-status nil))
            (when (eq nngmail--batch-in-flight 'nngmail-background)
              (setq nngmail--batch-in-flight nil))
            (let ((took (- (float-time) started)))
              (when (>= took nngmail-slow-threshold)
                (nngmail--log "SLOW HTTP %s %s -> %s in %s (%s) [bg]"
                              method (substring url 0 (min 70 (length url))) status
                              (nngmail--fmt-duration took) (file-size-human-readable (length (or text ""))))))
            (nngmail--quota-charge method url body)
            (unless (nngmail-job-stopped job)
              (condition-case err
                  (funcall callback status hdrs text)
                (error (nngmail--job-fail job (error-message-string err))))))))
    (setq nngmail--batch-in-flight 'nngmail-background
          nngmail--request-out-since started)
    (nngmail--job-trace job "%s %s%s" method url (if body " [body]" ""))
    (setf (nngmail-job-proc job)
          (nngmail--track-request
           (plz (intern (downcase method)) url
             :headers headers :body body :body-type 'binary :as 'response
             :timeout nngmail-request-timeout :decode nil
             :then (lambda (r)
                     (pcase-let ((`(,status ,hdrs ,text) (nngmail--plz-unpack r)))
                       (funcall deliver status hdrs text)))
             :else (lambda (e)
                     (condition-case _
                         (pcase-let ((`(,status ,hdrs ,text) (nngmail--plz-unpack e)))
                           (funcall deliver status hdrs text))
                       (nngmail-http-error
                        (funcall deliver 0 nil (nngmail--plz-error-text e))))))))
    (process-put (nngmail-job-proc job) :nngmail-state 'pending)))

(defun nngmail--quota-charge (method url body)
  "Record the quota units of METHOD URL BODY as spent, without waiting."
  (push (cons (float-time) (nngmail--quota-cost method url body)) nngmail--quota-log))

(defun nngmail--job-http-failure (job status text)
  "A non-2xx STATUS with TEXT on JOB's request: refresh, back off, or pace."
  (let ((account (nngmail-job-account job)))
    (cond
     ((= status 401)
      (nngmail--job-log job "401; refreshing the token")
      (nngmail--force-refresh account)
      (nngmail--job-schedule job 1))
     ((and (memq status '(429 403))
           (string-match-p "quota\\|rate ?limit\\|RateLimitExceeded" (or text "")))
      (let ((minute (string-match-p "per minute\\|Total Query Cost\\|dailyLimit" (or text ""))))
        (when minute
          (setq nngmail--quota-enforced t)
          (nngmail--bucket-drain))
        (nngmail--job-log job "throttled (%d): %s" status (truncate-string-to-width (or text "") 120))
        (nngmail--set-status "rate limit, retry 15s")
        (nngmail--job-schedule job 15)))
     (t
      (nngmail--job-fail job (format "HTTP %s: %s" status
                                     (truncate-string-to-width
                                      (or (ignore-errors (alist-get 'message (alist-get 'error (nngmail--json-parse text))))
                                          text "")
                                      140)))))))

;;;; Hooking into the synchronous entry points

(defun nngmail--background-sync (account)
  "Sync ACCOUNT the background way: kick its job and return.
The caller reads the local index.  A first sync (no
history id) runs inline so there is something to read."
  (if (null (nngmail-account-history-id account))
      (nngmail--sync-safely-inline account)
    (nngmail-background-kick account)
    t))

(defun nngmail--background-active-p ()
  "Whether syncs go to the background job in this session."
  (and nngmail-background nngmail-use-plz
       (or (not noninteractive) nngmail--background-in-batch)))

(defvar nngmail--background-in-batch nil
  "Non-nil lets the background job run under `noninteractive' (tests).")

(defun nngmail--sync-safely-around (orig account)
  "Around `nngmail--sync-safely': ACCOUNT goes to the background job if enabled.
Otherwise ORIG runs."
  (if (nngmail--background-active-p)
      (nngmail--background-sync account)
    (funcall orig account)))

(defun nngmail--sync-safely-inline (account)
  "`nngmail--sync-safely' on ACCOUNT without the background advice."
  (let ((nngmail-background nil))
    (nngmail--sync-safely account)))

(advice-add 'nngmail--sync-safely :around #'nngmail--sync-safely-around)

;;;; The mode line

;; nngmail's spinning G runs while a sync works.  With a background job
;; there is always a sync "around", so between passes the G stays, still
;; and muted, with the job's state as its tooltip; red-tinted text when
;; the job is backing off after an error.  `nngmail--spinner-idle-function'
;; is the hook nngmail.el offers for exactly this.

(defun nngmail--background-idle-indicator ()
  "The mode-line text between syncs, or nil when there is nothing to show.
A sleeping job: a still, muted G with the job's state as tooltip.  A
job backing off after an error: `nngmail!' in red."
  (let (jobs)
    (maphash (lambda (_ job) (unless (nngmail-job-stopped job) (push job jobs))) nngmail--jobs)
    (let* ((trouble (seq-find (lambda (j) (or (nngmail-job-backing-off j) (nngmail-job-last-error j))) jobs))
           (tip (mapconcat (lambda (j) (nngmail--job-describe (nngmail-account-name (nngmail-job-account j)) j))
                           jobs "\n")))
      (cond
       (trouble
        (nngmail--mode-name-show (propertize (nngmail--spinner-glyph nil) 'help-echo tip))
        (concat " " (nngmail--spinner-glyph nil)
                (propertize "nngmail!" 'face 'error 'help-echo tip) " "))
       ;; One account: its pills; several: the first's (the tooltip names all).
       (jobs (nngmail--spinner-still tip t (nngmail-job-account (car (last jobs)))))))))

(setq nngmail--spinner-idle-function #'nngmail--background-idle-indicator)

;;;; Status

;;;###autoload
(defun nngmail-background-status ()
  "Say what each account's background job is doing."
  (interactive)
  (if (zerop (hash-table-count nngmail--jobs))
      (message "nngmail: no background jobs")
    (let (lines)
      (maphash (lambda (name job) (push (nngmail--job-describe name job) lines)) nngmail--jobs)
      (message "%s" (string-join (nreverse lines) "\n")))))

(defun nngmail--job-describe (name job)
  "One line about JOB of account NAME, in the status line's words."
  (let* ((state (nngmail-job-state job))
         (what (cond ((nngmail-job-stopped job) "stopped")
                     ((nngmail-job-backing-off job)
                      (format "waiting after an error, retry in %s"
                              (nngmail--job-time-left job)))
                     ((eq state 'idle)
                      (format "up to date, next check in %s" (nngmail--job-time-left job)))
                     ((memq state '(history apply))
                      (format "applying changes: %s records in %d page%s"
                              (nngmail--fmt-num (nngmail-job-records job))
                              (nngmail-job-pages job) (if (= 1 (nngmail-job-pages job)) "" "s")))
                     ((eq state 'fetch)
                      (format "fetching changed messages: %s/%s"
                              (nngmail--fmt-num (- (or (nngmail-job-fetch-total job) 0)
                                                   (length (nngmail-job-to-fetch job))))
                              (nngmail--fmt-num (or (nngmail-job-fetch-total job) 0))))
                     (t (symbol-name state)))))
    (concat "nngmail: " name ": " what
            (if (nngmail-job-proc job) "  (request out)" "")
            (if (nngmail-job-last-error job)
                (format "  [last error: %s]" (nngmail-job-last-error job))
              ""))))

(defun nngmail--job-time-left (job)
  "How long until JOB's pending timer fires, as text; \"now\" when none."
  (if-let* ((timer (nngmail-job-timer job))
            (left (ignore-errors (- (float-time (timer--time timer)) (float-time)))))
      (nngmail--fmt-duration (max 0 left))
    "now"))

(provide 'nngmail-background)

;;; nngmail-background.el ends here
