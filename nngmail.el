;;; nngmail.el --- Gnus backend for the Gmail REST API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (oauth2 "0.19"))
;; Emacs must be built with SQLite (--with-sqlite3); see `nngmail--require-sqlite'.
;; Keywords: mail, news, gnus
;; URL: https://github.com/tzz/nngmail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; nngmail is a native Gnus backend for Gmail.  It talks to the Gmail
;; REST API (https://gmail.googleapis.com/gmail/v1/) instead of IMAP,
;; authenticating with the same OAuth2 token that oauth2.el and
;; auth-source-xoauth2-plugin already obtain for IMAP/SMTP.
;;
;; Model
;; -----
;; Every Gmail message gets one server-wide Gnus article number, assigned
;; in the order nngmail first sees it.  Gmail labels are exposed as Gnus
;; groups; a message with three labels appears in three groups under the
;; same article number.  A synthetic group named by `nngmail-all-mail-group'
;; ("[Gmail]/All Mail" by default) contains every non-trash, non-spam
;; message and is the target for search results.
;;
;; Read state is Gmail's UNREAD label and ticked is STARRED; both are
;; synced in both directions.  Moving an article between label groups
;; changes its labels; expiring (or `B DEL') trashes it; sending goes
;; through messages.send with the same token, so no SMTP setup is needed.
;;
;; Incremental sync uses history.list keyed by the last seen historyId,
;; with a full relisting fallback when Gmail says the history has expired.
;; State is kept in `nngmail-directory' (one SQLite file per account).
;;
;; Companion libraries in this package: nngmail-background.el (keep
;; Gmail in sync from a timer, never blocking Emacs), nngmail-menu.el
;; (a transient menu under `L L'), nngmail-gdocs.el (Google Docs linked
;; from mail, as text), nngmail-efrit.el (Gmail tools and keys for
;; efrit 0.13+, which is optional).  The pills package, when loaded,
;; draws nngmail's pills.
;;
;; Setup
;; -----
;; In ~/.authinfo(.gpg), the same entry the xoauth2 plugin uses:
;;
;;   machine work-gmail login you@example.com port imaps auth xoauth2
;;     auth-url https://accounts.google.com/o/oauth2/auth
;;     token-url https://accounts.google.com/o/oauth2/token
;;     client-id ...apps.googleusercontent.com client-secret GOCSPX-...
;;     redirect-uri http://localhost:8999 scope https://mail.google.com/
;;
;; For Google Docs linked from mail (nngmail-gdocs.el) the scope is
;;   "https://mail.google.com/ https://www.googleapis.com/auth/drive.readonly"
;; (space-separated); run M-x nngmail-reauthorize after changing it.
;;
;; Or, in auth-source's JSON format (a file in `auth-sources' ending in
;; .json, e.g. ~/.authinfo.json.gpg):
;;
;;   [
;;     {
;;       "machine": "gmail",
;;       "login": "you@example.com",
;;       "auth": "xoauth2",
;;       "auth-url": "https://accounts.google.com/o/oauth2/auth",
;;       "token-url": "https://accounts.google.com/o/oauth2/token",
;;       "client-id": "123.apps.googleusercontent.com",
;;       "client-secret": "GOCSPX-...",
;;       "redirect-uri": "http://localhost:8999",
;;       "scope": "https://mail.google.com/"
;;     }
;;   ]
;;
;; "machine" is :host and "login" is :user; other keys become keywords of
;; the same name.  No "port" is needed for nngmail, but "port": "imaps"
;; lets the same entry serve nnimap and auth-source-xoauth2-plugin too.
;;
;; nngmail looks the entry up with :host set to the nngmail server name,
;; falling back to "gmail.com", "imap.gmail.com" and "smtp.gmail.com".
;; The lookup bypasses auth-source-xoauth2-plugin's advice and
;; auth-source's result cache, so a stale negative cache entry cannot
;; hide the credentials.
;;
;; In ~/.gnus:
;;
;;   (require 'nngmail)
;;   (add-to-list 'gnus-secondary-select-methods
;;                '(nngmail "work-gmail" (nngmail-user "you@example.com")))
;;
;; and optionally
;;
;;   (setq message-send-mail-function #'nngmail-message-send-mail)
;;
;; Searching (G G in *Group*) accepts Gmail's own search syntax, e.g.
;; "from:alice newer_than:7d has:attachment", or Gnus' parsed syntax.
;;
;; HTTP transport is plz.el (curl) when installed, else url.el.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'url-util)
(require 'auth-source)
(require 'nnheader)
(require 'nnoo)
(require 'nnmail)
(require 'gnus)
(require 'gnus-int)
(require 'gnus-range)
(require 'gnus-sum)
(require 'mail-utils)
(require 'rfc2047)
(require 'oauth2)
(require 'eieio)
(require 'gnus-search)
(require 'svg)
(require 'dom)

(declare-function plz "plz")
(declare-function plz-error-response "plz")
(declare-function plz-error-message "plz")
(declare-function plz-error-p "plz")
(declare-function plz-response-p "plz")
(declare-function plz-error-curl-error "plz")
(declare-function plz-response-status "plz")
(declare-function plz-response-body "plz")
(declare-function plz-response-headers "plz")
(declare-function message-narrow-to-headers "message")
(declare-function message-fetch-field "message")
(declare-function message-field-value "message")
(declare-function message-goto-body "message")
(declare-function message-encode-message-body "message")
(declare-function mail-envelope-from "sendmail")

(nnoo-declare nngmail)

;;;; User options

(defgroup nngmail nil
  "Gnus backend for the Gmail REST API."
  :group 'gnus
  :prefix "nngmail-")

(defcustom nngmail-directory (nnheader-concat gnus-directory "nngmail/")
  "Directory where nngmail stores its per-account state."
  :type 'directory)

(defcustom nngmail-initial-limit 2000
  "How many recent messages to fetch on the first sync of an account.
Older mail is added lazily as `nngmail-backfill' is called (or from
search results).  nil means fetch everything, which can take a long
time for a large mailbox."
  :type '(choice (const :tag "Everything" nil) integer))

(defcustom nngmail-label-limit 100
  "Per-label top-up on the initial sync.
After the newest `nngmail-initial-limit' messages are listed, each
visible label is listed too so that small or old labels aren't empty,
but only this many per label.  With many labels this adds up, so it is
also bounded by `nngmail-label-total-limit'.  nil disables the top-up."
  :type '(choice (const nil) integer))

(defcustom nngmail-label-total-limit 2000
  "Upper bound on messages added by the per-label top-up, across all labels."
  :type '(choice (const :tag "No bound" nil) integer))

(defcustom nngmail-sync-chunk 500
  "Fetch and save headers in chunks of this many messages during a sync.
Smaller chunks give more frequent progress updates and checkpoints."
  :type 'integer)

(defcustom nngmail-all-mail-group "[Gmail]/All Mail"
  "Name of the synthetic group holding every non-trash, non-spam message."
  :type 'string)

(defcustom nngmail-hidden-labels '("CHAT" "CATEGORY_PERSONAL" "CATEGORY_SOCIAL"
                                   "CATEGORY_PROMOTIONS" "CATEGORY_UPDATES"
                                   "CATEGORY_FORUMS" "UNREAD" "STARRED"
                                   "IMPORTANT")
  "Gmail label ids that are not exposed as groups.
UNREAD and STARRED are mapped to Gnus marks instead."
  :type '(repeat string))

(defcustom nngmail-label-group-alist
  '(("INBOX" . "INBOX") ("SENT" . "[Gmail]/Sent Mail") ("DRAFT" . "[Gmail]/Drafts")
    ("SPAM" . "[Gmail]/Spam") ("TRASH" . "[Gmail]/Trash"))
  "Mapping from Gmail system label ids to Gnus group names.
User labels use their Gmail display name (nested labels keep the slash)."
  :type '(alist :key-type string :value-type string))

(defcustom nngmail-batch-size 50
  "Number of requests per Gmail batch call.
Gmail's hard maximum is 100, but it runs a batch's parts concurrently and
counts them against the per-user concurrency limit, which in practice
trips somewhere above 50.  Google's own guidance is 50.  nngmail lowers
the size automatically when the limit is reported and raises it again
after clean batches, so this is a starting point, not a ceiling."
  :type 'integer)

(defcustom nngmail-batch-parallel 2
  "How many batch requests to keep in flight at once (plz only).
Each batch is a separate HTTP request; issuing several concurrently
overlaps network latency.  Gmail's per-user concurrency limit counts
the parts of all in-flight batches together, so with 50-part batches
more than 2 tends to trip it; nngmail drops to 1 for the session when
that happens.  Ignored when using url.el."
  :type 'integer)

(defcustom nngmail-expire-action 'trash
  "What `gnus-summary-expire-articles' does to an article.
`trash' moves it to Gmail's Trash (recoverable for 30 days), `archive'
removes it from the current label only, `delete' deletes it permanently."
  :type '(choice (const trash) (const archive) (const delete)))

(defcustom nngmail-use-plz (and (locate-library "plz") (executable-find "curl") t)
  "If non-nil, use plz.el (curl) for HTTP; otherwise use url.el.
plz is strongly recommended for anything beyond light use: url.el's
synchronous retrieval is prone to returning no buffer at all on a
transient network hiccup, and nngmail can only retry those, not avoid
them.  Install plz from GNU ELPA and make sure curl is on the variable
`exec-path' (on macOS, `exec-path-from-shell' or an explicit entry may
be needed for Emacs.app to see /usr/bin/curl)."
  :type 'boolean)

(defcustom nngmail-debug nil
  "Controls the *nngmail-log* buffer.
nil: no log.  t: phase changes, pauses, errors, batch failure summaries
and state saves -- enough to see what happened without drowning in it.
`trace': additionally every HTTP request line and per-chunk detail."
  :type '(choice (const :tag "Off" nil) (const :tag "Normal" t) (const :tag "Trace" trace)))

(defun nngmail--trace (fmt &rest args)
  "Log FMT/ARGS only when `nngmail-debug' is `trace'."
  (when (eq nngmail-debug 'trace)
    (apply #'nngmail--log fmt args)))

;;;; Server variables (per nnoo server)

(defvoo nngmail-user nil
  "Gmail address of the account; also used to find the auth-source entry.")

(defvoo nngmail-auth-host nil
  "Override the :host used to look up credentials in auth-source.
Defaults to the nngmail server name.")

(defvoo nngmail-cache-articles t
  "If non-nil, keep raw messages in the account's database once fetched.
`nngmail-article-cache-max' bounds the space they take.")

(defvoo nngmail-status-string "")
(defvoo nngmail-state nil
  "The `nngmail-account' struct for the current server.")

;;;; Data structures

(cl-defstruct (nngmail-account (:constructor nngmail--make-account))
  name                ; server name
  user                ; email
  token               ; oauth2-token
  auth                ; plist from auth-source
  history-id          ; last processed historyId (string), nil = never synced
  next-article        ; next free article number
  id->num             ; hash: gmail id (string) -> article number
  num->id             ; hash: article number -> gmail id
  labels              ; hash: article number -> list of label ids
  headers             ; hash: article number -> NOV vector, a bounded cache of the DB
  groups              ; hash: group name -> `nngmail-group' (see Numbering)
  label-names         ; alist: label id -> gmail name
  name->label         ; hash: group name -> label id
  pending             ; (PHASE FORCE IDS) of an interrupted fetch, or nil
  pending-history-id  ; historyId to adopt once a full sync's fetch completes
  backfill            ; (:spec :page :found :scanned) of an interrupted backfill
  mailbox-total       ; messagesTotal from the last profile call, or nil
  dirty               ; non-nil if meta needs writing
  last-error
  db                  ; open sqlite object, see "Persistence"
  header-lru)         ; list of article numbers, most recently used first

(defconst nngmail-version "1.0.0"
  "The nngmail code version.  Bump on every change that a test drive can see.
`nngmail-build-info' (nngmail-reload.el) prints it with the source
dates, so a report says which code produced it.")

(defconst nngmail--api "https://gmail.googleapis.com/gmail/v1/users/me/")
(defconst nngmail--upload-api "https://gmail.googleapis.com/upload/gmail/v1/users/me/")
(defconst nngmail--batch-api "https://gmail.googleapis.com/batch/gmail/v1")

(defvar nngmail--accounts (make-hash-table :test #'equal)
  "Server name -> `nngmail-account'.")

(defvar nngmail--current nil
  "The `nngmail-account' most recently opened; used when SERVER is nil.")

;;;; Logging

(defvar nngmail--log-last nil "Last line written to the log (without timestamp).")
(defvar nngmail--log-repeat 0 "How many times `nngmail--log-last' has repeated.")

(defcustom nngmail-slow-threshold 0.5
  "Seconds above which a backend operation is logged with its duration.
Every nnoo entry point and every sync phase is timed; ones faster than
this are logged only at trace level.  Set to 0 to log all of them."
  :type 'number)

(defmacro nngmail--timed (what &rest body)
  "Run BODY; log WHAT and how long it took if over `nngmail-slow-threshold'."
  (declare (indent 1))
  (let ((t0 (make-symbol "t0")) (w (make-symbol "what")))
    `(let ((,t0 (float-time)) (,w ,what))
       (prog1 (progn ,@body)
         (let ((took (- (float-time) ,t0)))
           (if (>= took nngmail-slow-threshold)
               (nngmail--log "SLOW %s: %s" ,w (nngmail--fmt-duration took))
             (nngmail--trace "%s: %s" ,w (nngmail--fmt-duration took))))))))

(defun nngmail--log (fmt &rest args)
  "Log FMT/ARGS to *nngmail-log* when `nngmail-debug' is on.
Consecutive identical lines are collapsed into one \"(repeated N times)\"."
  (when nngmail-debug
    (let ((line (apply #'format fmt args)))
      (with-current-buffer (get-buffer-create "*nngmail-log*")
        (goto-char (point-max))
        (if (equal line nngmail--log-last)
            (progn
              (cl-incf nngmail--log-repeat)
              ;; Rewrite the trailing repeat marker in place.
              (forward-line -1)
              (when (looking-at ".*  (repeated [0-9]+ times)$")
                (delete-region (line-beginning-position) (progn (forward-line 1) (point))))
              (goto-char (point-max))
              (insert (format-time-string "%H:%M:%S ") line
                      (format "  (repeated %d times)\n" (1+ nngmail--log-repeat))))
          (setq nngmail--log-last line
                nngmail--log-repeat 0)
          (insert (format-time-string "%H:%M:%S ") line "\n"))))))

(defun nngmail--message (level fmt &rest args)
  "Report FMT/ARGS at gnus verbosity LEVEL; log them unless LEVEL > 5.
Progress and countdown chatter is emitted at level 6+ or via
`nnheader-message' directly so it does not swamp the log."
  (when (<= level 5)
    (apply #'nngmail--log fmt args))
  (apply #'nnheader-message level (concat "nngmail: " fmt) args))

(defvar nngmail--progress nil
  "Current sync progress: (ACCOUNT-NAME PHASE DONE TOTAL EXTRA) or nil.
EXTRA is an optional short string shown in its own column (e.g. running
scanned/new counters for a backfill).")

(defvar nngmail--progress-extra nil
  "Dynamically bound short string to show in the status line's extra column.")

(defvar nngmail--progress-last-shown 0
  "`float-time' of the last progress redisplay, to throttle the echo area.")

(defvar nngmail--progress-last-phase nil
  "Phase string most recently logged by the function `nngmail--progress'.")

(defvar nngmail--phase-started nil
  "`float-time' when the phase in `nngmail--progress-last-phase' began.")

(defvar nngmail--status nil
  "Transient status shown alongside progress, or nil when running normally.
A string such as \"pacing 3s\" or \"concurrency limit, retry 1s\".")

(defvar nngmail--status-last-line nil
  "The last line drawn by `nngmail--draw-status', to avoid redundant redraws.")

;; Time estimates.
;;
;; Every phase that reports DONE/TOTAL gets an ETA.  The rate is not
;; DONE / elapsed: Gmail's throughput swings between paused, throttled
;; and full speed, and a burst at the start would make the estimate
;; optimistic for minutes.  Instead the rate is smoothed exponentially
;; over recent samples (`nngmail-eta-smoothing'), so it follows the
;; current speed with a lag of a few samples.  Pauses count against the
;; rate -- they are part of how long the job will take.

(defvar nngmail--rate nil
  "Rate tracker for the current phase: (PHASE START LAST-TIME LAST-DONE RATE).
RATE is items per second, smoothed; nil until two samples exist.")

(defcustom nngmail-eta-smoothing 0.3
  "Weight of the newest rate sample in the ETA (0 < w <= 1).
Higher follows speed changes faster but makes the estimate jumpier."
  :type 'number)

(defcustom nngmail-eta-after 3
  "Seconds a phase must run before an estimate is shown."
  :type 'number)

(defun nngmail--rate-sample (phase done)
  "Feed DONE items of PHASE to the rate tracker; return the smoothed rate or nil."
  (let ((now (float-time)))
    (pcase nngmail--rate
      ((and `(,p ,start ,last-t ,last-d ,rate) (guard (equal p phase)))
       (let ((dt (- now last-t)) (dd (- (or done 0) (or last-d 0))))
         (cond
          ;; Backwards or no movement in time: keep what we have.
          ((< dt 0.5) rate)
          ;; Progress went backwards (phase restarted with the same name).
          ((< dd 0)
           (setq nngmail--rate (list phase now now done nil)) nil)
          (t
           (let* ((sample (/ dd dt))
                  (new (if rate
                           (+ (* nngmail-eta-smoothing sample)
                              (* (- 1 nngmail-eta-smoothing) rate))
                         sample)))
             (setq nngmail--rate (list phase start now done new))
             new)))))
      (_
       (setq nngmail--rate (list phase now now done nil))
       nil))))

(defun nngmail--rate-elapsed ()
  "Seconds since the current phase started, or nil."
  (and nngmail--rate (- (float-time) (nth 1 nngmail--rate))))

(defun nngmail--fmt-duration (seconds)
  "Format SECONDS Prometheus style: 250ms, 42s, 3m10s, 2h5m, 1d3h.
Two units at most, the larger first, zero parts left out."
  (let* ((x (max 0.0 (float (nngmail--finite seconds))))
         (s (round x)))
    (cond ((< x 1) (format "%dms" (round (* x 1000))))
          ((< s 60) (format "%ds" s))
          ((< s 3600) (nngmail--fmt-duration-2 (/ s 60) "m" (% s 60) "s"))
          ((< s 86400) (nngmail--fmt-duration-2 (/ s 3600) "h" (% (/ s 60) 60) "m"))
          (t (nngmail--fmt-duration-2 (/ s 86400) "d" (% (/ s 3600) 24) "h")))))

(defun nngmail--fmt-duration-2 (big big-unit small small-unit)
  "BIG BIG-UNIT and SMALL SMALL-UNIT as \"2h 5m\"; SMALL is dropped when zero."
  (if (zerop small)
      (format "%d%s" big big-unit)
    (format "%d%s%d%s" big big-unit small small-unit)))

(defun nngmail--fmt-eta (phase done total)
  "The time column for PHASE at DONE of TOTAL: an ETA, or elapsed time.
Returns a string of at most 14 columns, or nil when nothing is known."
  (let* ((rate (nngmail--rate-sample phase done))
         (elapsed (nngmail--rate-elapsed)))
    (cond
     ((or (null elapsed) (< elapsed nngmail-eta-after)) nil)
     ((and total (> total 0) done rate (> rate 1e-9))
      (let ((left (/ (- total done) rate)))
        (cond ((<= left 0) "~done")
              ;; Beyond a month the estimate is noise; say so instead of
              ;; printing a huge or infinite figure.
              ((> left (* 40 86400)) "~weeks left")
              (t (format "~%s left" (nngmail--fmt-duration left))))))
     (t (format "%s so far" (nngmail--fmt-duration elapsed))))))

(defcustom nngmail-status-width nil
  "Width of the single echo-area status line nngmail maintains during syncs.
nil (the default) uses the width of the echo area of the selected frame
at each redraw, minus one cell so the line never wraps.  An integer
fixes the width instead.  The line is laid out in fixed columns --
account+phase | progress bar | scan bar | gauges | pause/retry state --
and the last column takes whatever width is left."
  :type '(choice (const :tag "Echo area width" nil) integer))

(defun nngmail--status-width ()
  "Columns available to the status line in the frame it is drawn in."
  (or nngmail-status-width
      (if noninteractive
          120
        ;; The echo area is as wide as the frame; leave one cell so the
        ;; last character does not trigger a continuation line.
        (max 60 (1- (frame-width (selected-frame)))))))

(defun nngmail--status-frame ()
  "The frame to draw the status line in: the one with keyboard focus.
A timer runs with whatever frame happened to be selected when the
command loop last yielded, which with several frames is not the one
being looked at.  Falls back to the selected frame when no frame
reports focus (a terminal, an old Emacs)."
  (or (and (fboundp 'frame-focus-state)
           (seq-find (lambda (f) (and (frame-live-p f) (frame-visible-p f)
                                      (eq (frame-focus-state f) t)))
                     (frame-list)))
      (selected-frame)))

(defvar nngmail--gauge-cache (make-hash-table :test #'equal)
  "Rendered status-line images (bars, gauges, pills, spinner frames) by state.")

(defcustom nngmail-background-status 'mode-line
  "Where the background sync job reports progress.
`mode-line': next to the spinning G in `global-mode-string', as a short
text (phase, counts, a transient condition), with the final \"sync
done\" shown there for a few seconds; the echo area is left alone.
`echo-area': the full status line (bars and gauges) in the echo area,
and the done message as a regular message, as for synchronous syncs.
Synchronous operations the user started (a backfill, a send, a search)
always use the echo-area status line, since the user is waiting on them."
  :type '(choice (const mode-line) (const echo-area)))

(defvar nngmail--progress-quiet nil
  "Non-nil while progress belongs to the background job in `mode-line' mode.
The status line is then not drawn in the echo area and the done message
goes to the mode line.")

(defcustom nngmail-status-bar 'auto
  "How to draw the progress bar in the status line.
`auto' uses an SVG bar on graphical displays that support it and text
blocks otherwise; `svg' or `text' forces one; nil shows no bar."
  :type '(choice (const auto) (const svg) (const text) (const :tag "None" nil)))

(defcustom nngmail-status-bar-width 30
  "Width of a progress bar in character cells.
The bar carries its own label (done/total, percentage and time
estimate) inside the lozenge, so it needs room: 30 cells fits
\"123,456/234,567  53%  ~2h05m left\"."
  :type 'integer)

(defun nngmail--svg (width height)
  "An SVG canvas WIDTH by HEIGHT with a matching viewBox."
  (let ((svg (svg-create width height)))
    (dom-set-attribute svg 'viewBox (format "0 0 %s %s" width height))
    svg))

;; Sizing rule for everything nngmail draws into text: an image is a
;; whole number of character cells wide and one line high, and the text
;; it covers is exactly that many columns.  A mode line built from a `%'
;; format (Gnus's) stretches an image to the width of the text under it;
;; a 75 px pill over 9 columns of 12 px cells came out 108 px wide,
;; flat-ended, and three pills got three heights (2026-10-15).  When
;; image and text agree there is nothing to stretch, and a different
;; font or frame just rescales.  `nngmail--cell-image' is the one place
;; that pairs a canvas with its text.

(defun nngmail--cells-for (pixels)
  "The number of character cells needed to hold PIXELS of width, at least 1."
  (max 1 (ceiling (/ (float pixels) (frame-char-width)))))

(defun nngmail--mode-line-background ()
  "The mode line's background colour as #rrggbb, for painting behind images.
A mode line whose format contains a `%' construct (the stock one does,
\"%e\" first) draws an image's transparent pixels as black on Emacs
32.0.50 (observed 2026-10-16: the same image over the same text was
round in a `%'-free format and flat-ended after \"%e\").  Painting
the canvas in the mode line's own colour first makes transparency
irrelevant."
  ;; Emacs 29+ draws the selected window's mode line in `mode-line-active',
  ;; which themes colour separately from `mode-line'; reading `mode-line'
  ;; painted the canvas a shade off and left a band above and below each
  ;; pill (2026-10-20).  `face-background' with INHERIT resolves the chain.
  (let* ((face (cond ((and (fboundp 'mode-line-window-selected-p)
                           (not (mode-line-window-selected-p)))
                      'mode-line-inactive)
                     ((facep 'mode-line-active) 'mode-line-active)
                     (t 'mode-line)))
         ;; A mode-line face with no background of its own (a theme that
         ;; leaves all three faces nil) is drawn in the frame's
         ;; background, so fall through to that, never to black.
         (bg (or (face-background face nil t)
                 (face-background 'default nil t)
                 (frame-parameter nil 'background-color))))
    (if (and (stringp bg) (or (string-prefix-p "#" bg) (color-defined-p bg)))
        (apply #'color-rgb-to-hex (append (nngmail--color-rgb bg) '(2)))
      "#000000")))

(defun nngmail--cell-image (columns draw &optional _text)
  "An image COLUMNS cells wide and one line high, over a single space.
For the mode line.  (The echo-area status line keeps covers as wide as
their images: it is not a `%' format, and it measures them.)
DRAW is called with (SVG WIDTH-PX HEIGHT-PX) and draws into the canvas.
The result carries the image as a `display' property on ONE character:
a mode line whose format starts with a `%' construct (the stock
`mode-line-format' does, with \"%e\") lays an image over a
multi-character string out per character, stretching it to the run and
clipping it to the cells, which flattened the pills' ends and gave each
a different height (probed 2026-10-15: the same image was round over
\"x\" and in a `%'-free format).  Over one character there is nothing
to tile.  Callers add faces and help; the words go in the tooltip."
  (let* ((cw (frame-char-width)) (lh (frame-char-height))
         (w (* columns cw))
         (svg (nngmail--svg w lh)))
    (svg-rectangle svg 0 0 w lh :fill (nngmail--mode-line-background))
    (funcall draw svg w lh)
    (propertize " " 'display (svg-image svg :ascent 'center :scale 1))))

(defun nngmail--svg-p ()
  "Non-nil if we can draw SVG images in the echo area right now."
  (and (not noninteractive)
       (display-graphic-p)
       (image-type-available-p 'svg)
       (fboundp 'svg-create)))

(defun nngmail--status-bar (fraction &optional paused label)
  "Return a propertized string drawing a bar FRACTION (0..1) full.
PAUSED non-nil tints the bar to show we are waiting rather than working.
LABEL, a short string, is drawn inside the lozenge (SVG) or after the
blocks (text).  Uses SVG when available and allowed, otherwise Unicode
block characters."
  (let* ((w nngmail-status-bar-width)
         (f (max 0.0 (min 1.0 (or fraction 0.0))))
         (mode (pcase nngmail-status-bar
                 ('auto (if (nngmail--svg-p) 'svg 'text))
                 (m m))))
    (pcase mode
      ('nil (or label ""))
      ('svg
       (require 'svg)
       (condition-case err
           (nngmail--status-bar-svg w (list (cons f (if paused "#d29922" "#3fb950"))) label
                                    (format "%d%%" (round (* 100 f))))
         (error
          ;; Image support missing or broken: fall back to text for good.
          (nngmail--log "SVG status bar unavailable (%s); using text" (error-message-string err))
          (setq nngmail-status-bar 'text)
          (nngmail--status-bar fraction paused label))))
      (_
       (nngmail--status-bar-text w (list f) label)))))

(defvar nngmail-pill-style)

(defcustom nngmail-status-font-scale 0.62
  "Height of bar labels as a fraction of the line height.
Monospace glyphs are about 0.6 em wide, so at this size a label
character takes roughly 0.6 * 0.62 = 0.37 of a line height, which is
a little under one text column on most fonts."
  :type 'number)

(defun nngmail--status-bar-svg (w layers label tooltip)
  "One lozenge W cells wide with LAYERS and LABEL text inside.
LAYERS is a list of (FRACTION . COLOR), drawn in order so later layers
sit on top of earlier ones.  LABEL is centred in the bar; fields are
dropped from its right until it fits, and the text is additionally
pinned to its computed width with SVG textLength so a font with wider
glyphs than assumed cannot push it past the ends.  TOOLTIP is the
help-echo."
  (let* ((ch (frame-char-width))
         (lh (frame-char-height))
         (px (* w ch))
         (h (nngmail--status-pill-height lh))
         (top (/ (- lh h) 2.0))
         (dark (eq (frame-parameter nil 'background-mode) 'dark))
         (track (if dark "#30363d" "#d0d7de"))
         (text (if dark "#f0f6fc" "#1f2328"))
         (svg (nngmail--svg px lh)))
    (nngmail--pill-body svg 0 top px h track)
    (dolist (layer layers)
      (let ((f (max 0.0 (min 1.0 (car layer)))))
        (when (> f 0)
          (nngmail--pill-body svg 0 top (max h (round (* f px))) h (cdr layer)))))
    (when (and label (not (string-empty-p label)))
      (let* ((font-size (* nngmail-status-font-scale lh))
             ;; Advance of one monospace glyph at this size; 0.62 em is
             ;; a safe upper bound for Menlo/Monaco/DejaVu/SF Mono.
             (glyph (* 0.62 font-size))
             ;; Room inside the rounded ends.
             (inner (- px h))
             (max-chars (max 4 (floor (/ inner glyph))))
             (label (nngmail--fit-label label max-chars))
             (family (format "'%s', ui-monospace, Menlo, monospace"
                             (or (face-attribute 'default :family) "monospace")))
             (width (min inner (* glyph (string-width label))))
             (y (+ top (* h 0.5) (* font-size 0.35)))
             (shadow (if dark "#0d1117" "#ffffff")))
        (dolist (pass '(halo text))
          (apply #'svg-text svg label
                 :x (/ px 2.0) :y y
                 :font-size font-size :font-family family :text-anchor "middle"
                 :textLength width :lengthAdjust "spacingAndGlyphs"
                 (if (eq pass 'halo)
                     (list :fill "none" :stroke shadow :stroke-width 1.2
                           :stroke-opacity 0.6 :stroke-linejoin "round")
                   (list :fill text))))))
    (propertize (make-string w ?\s)
                'display (svg-image svg :ascent 'center :scale 1)
                'help-echo tooltip)))

(defun nngmail--status-pill (label width &optional tooltip)
  "LABEL on a plain lozenge WIDTH cells wide (SVG), or padded text.
The phase column and any text that lands in the extra column go through
this, so every cell of the status line has the same shape.  LABEL is
cut from the end with an ellipsis when it does not fit.
TOOLTIP is the help-echo."
  (let ((mode (pcase nngmail-status-bar
                ('auto (if (nngmail--svg-p) 'svg 'text))
                (m m))))
    (if (eq mode 'svg)
        (condition-case nil
            (nngmail--status-pill-svg label width tooltip)
          (error (truncate-string-to-width label width nil ?\s "…")))
      (truncate-string-to-width label width nil ?\s "…"))))

(defun nngmail--status-pill-svg (label w tooltip)
  "LABEL left-aligned in a track W cells wide; cached by LABEL and W.
TOOLTIP is the help-echo."
  (let* ((key (list 'pill label w (frame-char-width) (frame-char-height) nngmail-pill-style))
         (cached (gethash key nngmail--gauge-cache)))
    (or (and cached (propertize cached 'help-echo tooltip))
        (let* ((ch (frame-char-width)) (lh (frame-char-height))
               (px (* w ch))
               (h (nngmail--status-pill-height lh))
               (top (/ (- lh h) 2.0))
               (r (/ h 2.0))
               (dark (eq (frame-parameter nil 'background-mode) 'dark))
               (track (if dark "#30363d" "#d0d7de"))
               (text (if dark "#f0f6fc" "#1f2328"))
               (svg (nngmail--svg px lh))
               (font-size (* nngmail-status-font-scale lh))
               (glyph (* 0.62 font-size))
               (x0 r)
               (inner (- px (* 2 r)))
               (max-chars (max 3 (floor (/ inner glyph))))
               (label (nngmail--fit-label label max-chars))
               (width (min inner (* glyph (string-width label))))
               (family (format "'%s', ui-monospace, Menlo, monospace"
                               (or (face-attribute 'default :family) "monospace")))
               (y (+ top (* h 0.5) (* font-size 0.35))))
          (nngmail--pill-body svg 0 top px h track)
          (unless (string-empty-p label)
            (svg-text svg label :x x0 :y y :font-size font-size :font-family family
                      :text-anchor "start" :fill text
                      :textLength width :lengthAdjust "spacingAndGlyphs"))
          (let ((str (propertize (make-string w ?\s)
                                 'display (svg-image svg :ascent 'center :scale 1))))
            (puthash key str nngmail--gauge-cache)
            (propertize str 'help-echo tooltip))))))

(defun nngmail--status-cell (content width)
  "CONTENT as one status-line cell WIDTH wide.
A string that already carries an image (a bar, a gauge) is used as is;
plain text and nil become a lozenge with the text (or nothing) in it."
  (if (and (stringp content) (get-text-property 0 'display content))
      content
    (nngmail--status-pill (or content "") width)))

(defun nngmail--fit-label (label max-cols)
  "Shorten LABEL to MAX-COLS columns by dropping trailing fields.
Fields are separated by two spaces; the last one goes first, so
\"450/500 90%  ~12s left\" becomes \"450/500 90%\" before it is cut."
  (let ((parts (split-string label "  " t)))
    (while (and (cdr parts)
                (> (string-width (mapconcat #'identity parts "  ")) max-cols))
      (setq parts (butlast parts)))
    (truncate-string-to-width (mapconcat #'identity parts "  ") max-cols nil nil "…")))

(defun nngmail--status-bar-text (w fractions label)
  "Text progress bar W cells wide using block characters, LABEL after it.
FRACTIONS is a list of (0..1) values drawn as nested bars: the first is
the outer extent (█), the second, if any, the inner one (▓)."
  (let* ((o (max 0.0 (min 1.0 (or (car fractions) 0.0))))
         (i (max 0.0 (min o (or (cadr fractions) 0.0))))
         (label-w (if (and label (not (string-empty-p label))) (+ 1 (string-width label)) 0))
         (bw (max 4 (- w label-w)))
         (ic (floor (* i bw)))
         (full (floor (* o bw)))
         (part (- (* o bw) full))
         (eighth (floor (* part 8)))
         (eighths "▏▎▍▌▋▊▉")
         ;; A partial cell only when there is a visible fraction; a
         ;; zero-eighth remainder shows as track, not as a blank.
         (tail (if (and (< full bw) (> eighth 0)) (string (aref eighths (1- eighth))) "")))
    (concat (make-string ic ?▓)
            (make-string (max 0 (- full ic)) ?█) tail
            (make-string (max 0 (- bw full (length tail))) ?░)
            (if (> label-w 0) (concat " " label) ""))))

;; Gauges: two small analog-looking SVG meters in the status line.
;;
;;   connections  a vertical stack, one segment per request that can be
;;                in flight (`nngmail-batch-parallel', at least 3
;;                segments so it reads as a gauge), lit from the bottom
;;                for each request currently out.
;;   quota        the pacer's token bucket as a vertical level meter:
;;                full is a full budget, empty means the next request
;;                waits.  Amber below a quarter, red when empty.
;;
;; Both are drawn at cell size and cached by their discrete state so a
;; redraw costs a hash lookup.

(defvar nngmail-quota-burst)
(defvar nngmail--quota-log)
(defvar nngmail--quota-alert)
(defvar nngmail--throttled)

(defcustom nngmail-gauge-width 22
  "Width in character cells of the quota gauge in the status line.
A gauge is a small lozenge with a vertical meter at its left and its
figures inside: \"quota 82%\", and while pacing \"pacing 12s\" or
\"quota 40% (throttled)\".  The width is constant so the line does not
shift when the text changes."
  :type 'integer)

(defun nngmail--gauge (kind fraction label tooltip &optional segments color tone width)
  "One gauge lozenge WIDTH cells wide with LABEL inside.
WIDTH defaults to `nngmail-gauge-width'.
KIND is `stack' (SEGMENTS discrete bars, FRACTION of them lit, in
COLOR) or `level' (a continuous meter FRACTION full, coloured by
level).  TONE nil is the normal track; `warn' tints it amber and `alert'
red, for a gauge that is carrying a problem report.  Cached by its
discrete state."
  (let* ((w (or width nngmail-gauge-width))
         (steps (if (eq kind 'stack)
                    (round (* fraction segments))
                  (round (* (max 0.0 (min 1.0 fraction)) 16))))
         (key (list 'gauge kind steps segments color label tone w (frame-char-width) (frame-char-height) nngmail-pill-style))
         (cached (gethash key nngmail--gauge-cache)))
    (or (and cached (propertize cached 'help-echo tooltip))
        (let* ((ch (frame-char-width)) (lh (frame-char-height))
               (px (* w ch))
               (h (nngmail--status-pill-height lh))
               (top (/ (- lh h) 2.0))
               (r (/ h 2.0))
               (dark (eq (frame-parameter nil 'background-mode) 'dark))
               (track (pcase tone
                        ('warn (if dark "#5a3d0a" "#fff1c2"))
                        ('alert (if dark "#5c1a1a" "#ffd7d5"))
                        (_ (if dark "#30363d" "#d0d7de"))))
               (text (if dark "#f0f6fc" "#1f2328"))
               (meter-w (* 0.55 ch))
               (meter-x (+ r 1))
               (svg (nngmail--svg px lh)))
          (nngmail--pill-body svg 0 top px h track)
          (pcase kind
            ('stack
             ;; SEGMENTS bars stacked in the meter slot, lit from the bottom.
             (let* ((gap 1.0)
                    (usable (- h 4))
                    (seg (/ (- usable (* gap (1- segments))) (float segments)))
                    (on (or color (if dark "#58a6ff" "#0969da")))
                    (off (if dark "#484f58" "#afb8c1")))
               (dotimes (i segments)
                 (svg-rectangle svg meter-x (- (+ top h 2) (* (1+ i) seg) (* i gap) 4)
                                meter-w seg :rx 1 :ry 1
                                :fill (if (< i steps) on off)))))
            (_
             (let* ((frac (/ steps 16.0))
                    (fill (cond ((= steps 0) "#f85149") ((< steps 4) "#d29922") (t "#3fb950")))
                    (usable (- h 4))
                    (fh (* usable frac)))
               (svg-rectangle svg meter-x (+ top 2) meter-w usable :rx 1.5 :ry 1.5
                              :fill (if dark "#484f58" "#afb8c1"))
               (when (> fh 0)
                 (svg-rectangle svg meter-x (- (+ top 2 usable) fh) meter-w fh :rx 1.5 :ry 1.5 :fill fill)))))
          ;; Label to the right of the meter, same type as the bars.
          (let* ((font-size (* nngmail-status-font-scale lh))
                 (glyph (* 0.62 font-size))
                 (x0 (+ meter-x meter-w (* 0.6 ch)))
                 (inner (- px x0 r))
                 (max-chars (max 3 (floor (/ inner glyph))))
                 ;; A status label ends in the figure that matters (the
                 ;; countdown); when it does not fit, cut the front.
                 (label (if (<= (string-width label) max-chars)
                            label
                          (concat "…" (truncate-string-to-width
                                       label (string-width label) (- (string-width label) (1- max-chars))))))
                 (width (min inner (* glyph (string-width label))))
                 (family (format "'%s', ui-monospace, Menlo, monospace"
                                 (or (face-attribute 'default :family) "monospace")))
                 (y (+ top (* h 0.5) (* font-size 0.35))))
            (svg-text svg label :x x0 :y y :font-size font-size :font-family family
                      :text-anchor "start" :fill text
                      :textLength width :lengthAdjust "spacingAndGlyphs"))
          (let ((str (propertize (make-string w ?\s)
                                 'display (svg-image svg :ascent 'center :scale 1))))
            (puthash key str nngmail--gauge-cache)
            (propertize str 'help-echo tooltip))))))

(defcustom nngmail-connection-gauge-width 30
  "Width in cells of the connections gauge.
It holds \"N/M conns\" normally and, while a request is waiting,
retrying or paused, the reason with its countdown (see
`nngmail--status'), so it is wider than the quota gauge."
  :type 'integer)

(defun nngmail--status-tone (status)
  "How serious STATUS (the transient status text) is: nil, `warn' or `alert'."
  (cond ((null status) nil)
        ((string-match-p "\\`\\(net\\(work\\)? error\\|curl\\|HTTP [45]\\|no answer\\)" status) 'alert)
        (t 'warn)))

(defun nngmail--gauges ()
  "The connections and quota gauges as one string, or text equivalents.
The connections gauge shows \"N/M conns\" (requests in flight over the
allowed parallel batches); while `nngmail--status' is set (a pause, a
retry countdown, a slow answer) it shows that instead, tinted amber or
red by severity, so every transient condition lives in one place.  The
quota gauge shows the pacer's token bucket."
  (let* ((in-flight (nngmail--in-flight-count))
         (total (max 1 nngmail-batch-parallel))
         (budget (nngmail--quota-budget))
         (cap (min nngmail-quota-burst budget))
         (level (if nngmail--quota-log
                    (/ (nngmail--bucket-level cap (/ budget 60.0)) (float cap))
                  1.0))
         (conns (format "%d/%d conn%s" (min in-flight total) total (if (= total 1) "" "s")))
         (status nngmail--status)
         ;; Pacing countdowns belong to the quota gauge, not the connections.
         (pacing (and status (string-prefix-p "pacing" status) status))
         (conn-status (and status (not pacing) status))
         (tone (nngmail--status-tone conn-status))
         (conn-label (if conn-status (concat conns "  " conn-status) conns))
         (quota (cond (nngmail--quota-alert nngmail--quota-alert)
                      (pacing pacing)
                      (nngmail--throttled (format "quota %d%% (throttled)" (round (* 100 level))))
                      (t (format "quota %d%%" (round (* 100 level))))))
         (quota-tone (cond (nngmail--quota-alert 'alert)
                           ((or pacing nngmail--throttled) 'warn)))
         (quota-width nngmail-gauge-width))
    (if (and nngmail-status-bar (nngmail--svg-p))
        (concat (nngmail--gauge 'stack (/ (float (min in-flight total)) total) conn-label
                                (format "%d of %d requests in flight%s" in-flight total
                                        (if conn-status (concat "\n" conn-status) ""))
                                (max 2 total) nil tone nngmail-connection-gauge-width)
                " "
                (nngmail--gauge 'level level quota
                                (format "quota bucket %d%% of %s units (%s/min)%s"
                                        (round (* 100 level)) (nngmail--fmt-num cap)
                                        (nngmail--fmt-num budget)
                                        (if nngmail--throttled " -- throttled" ""))
                                nil nil quota-tone quota-width))
      (format "%s %s"
              (truncate-string-to-width conn-label nngmail-connection-gauge-width nil ?\s "…")
              (truncate-string-to-width quota quota-width nil ?\s "…")))))

;; Mode-line spinner: a rotating "G" ring in `global-mode-string'
;; while nngmail is talking to Gmail, and a still G with count pills
;; between syncs.  The echo area is shared with every
;; other message in Emacs, so this is the one indicator that is always
;; visible.  The frames are SVG arcs (a spinning G, if you squint) with a
;; text fallback.

(defvar nngmail--spinner-frame 0)
(defvar nngmail--spinner-timer nil)
(defvar nngmail--spinner-string nil
  "Current spinner text for the mode line; nil when idle.")
(defconst nngmail--spinner-text-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"])

(defvar nngmail--spinner-dim nil
  "Bound non-nil while rendering a muted (idle) G.")

(defun nngmail--spinner-color ()
  "Stroke colour for the G: accent when active, the mode line's own text when idle.
The idle ring takes the `mode-line' face's foreground, so it sits with
the rest of the mode line whatever the theme, instead of a fixed grey
that can vanish against some backgrounds."
  (if nngmail--spinner-dim
      (let ((fg (face-attribute 'mode-line :foreground nil 'default)))
        (if (and (stringp fg) (color-defined-p fg)) fg "gray60"))
    (if (eq (frame-parameter nil 'background-mode) 'dark) "#58a6ff" "#0969da")))

(defcustom nngmail-pill-style 'raised
  "How count pills are drawn.
`raised': a vertical gradient (lit from above), a soft shadow below and
a hairline highlight along the top, so the pill stands off the mode
line; the text is flat on top.  `flat': one colour.  Both keep the
text's contrast: the gradient stays within a few percent of the base
colour's lightness."
  :type '(choice (const raised) (const flat)))

(defcustom nngmail-spinner-interval 0.12
  "Seconds between spinner frames in the mode line."
  :type 'number)

(defun nngmail--spinner-svg (frame)
  "One spinner frame: a G rotated by FRAME twelfths of a turn.
The G is an arc that starts at the upper right, runs counter-clockwise
round to the right-hand side, and ends in a bar pointing at the centre."
  (let* ((fg (nngmail--spinner-color))
         (key (list 'spin (% frame 12) (frame-char-height) (frame-char-width) fg (nngmail--mode-line-background) nngmail-pill-style))
         (cached (gethash key nngmail--gauge-cache)))
    (or cached
        ;; A square the line's height, over as many cells as that takes
        ;; (two at most fonts), centred in them.
        (let* ((lh (frame-char-height))
               (columns (nngmail--cells-for lh))
               (angle (* 30 (% frame 12)))
               (color fg))
          (puthash key
                   (nngmail--cell-image
                    columns
                    (lambda (svg w h)
                      (let* ((size h)
                             (cx (/ w 2.0)) (cy (/ h 2.0))
                             (r (* 0.34 size))
                             (stroke (* 0.15 size))
                             (rad (lambda (deg) (* float-pi (/ deg 180.0))))
                             (px (lambda (deg) (+ cx (* r (cos (funcall rad deg))))))
                             (py (lambda (deg) (+ cy (* r (sin (funcall rad deg))))))
                             ;; SVG's y axis points down, so angles run clockwise.
                             ;; The G opens at the upper right: start at -40deg,
                             ;; sweep the long way round (large-arc) to 0deg.
                             (x0 (funcall px -40)) (y0 (funcall py -40))
                             (x1 (funcall px 0)) (y1 (funcall py 0))
                             (d (format "M%.2f,%.2f A%.2f,%.2f 0 1 0 %.2f,%.2f L%.2f,%.2f"
                                        x0 y0 r r x1 y1 (- x1 (* 0.55 r)) y1))
                             (g (dom-node 'g `((transform . ,(format "rotate(%d %s %s)" angle cx cy))))))
                        (when (eq nngmail-pill-style 'raised)
                          ;; A soft dark drop under the ring, so it lifts like the
                          ;; pills (drawn unrotated: light comes from above the
                          ;; mode line, not from above the G's own frame).
                          (dom-append-child
                           svg (dom-node 'path `((d . ,d)
                                                 (transform . ,(format "translate(0 %.2f) rotate(%d %s %s)" (* 0.09 size) angle cx cy))
                                                 (fill . "none") (stroke . "#000000") (stroke-opacity . "0.5")
                                                 (stroke-width . ,(* 1.15 stroke)) (stroke-linecap . "round")))))
                        (dom-append-child
                         g (dom-node 'path `((d . ,d)
                                             (fill . "none") (stroke . ,color)
                                             (stroke-width . ,stroke) (stroke-linecap . "round")
                                             (stroke-linejoin . "round"))))
                        (svg--append svg g)
                        (when (eq nngmail-pill-style 'raised)
                          ;; Hairline highlight along the upper-left of the ring,
                          ;; in screen space so it stays on top as the G turns.
                          (let* ((hr (- r (* 0.3 stroke)))
                                 (hx (lambda (deg) (+ cx (* hr (cos (* float-pi (/ deg 180.0)))))))
                                 (hy (lambda (deg) (+ cy (* hr (sin (* float-pi (/ deg 180.0))))))))
                            (dom-append-child
                             svg (dom-node 'path `((d . ,(format "M%.2f,%.2f A%.2f,%.2f 0 0 1 %.2f,%.2f"
                                                                (funcall hx 200) (funcall hy 200) hr hr
                                                                (funcall hx 300) (funcall hy 300)))
                                                   (fill . "none") (stroke . "#ffffff") (stroke-opacity . "0.55")
                                                   (stroke-width . ,(max 0.8 (* 0.3 stroke)))
                                                   (stroke-linecap . "round"))))))))
                    "G")
                   nngmail--gauge-cache)))))

(defvar nngmail--spinner-idle-function nil
  "Function returning the idle mode-line text, or nil to show nothing when idle.
`nngmail-background' sets it so the G stays visible, still, between syncs.")

(defun nngmail--spinner-idle-string ()
  "The mode-line text while nothing is in progress, or nil.
A current flash (the variable `nngmail--mode-line-flash') comes first,
then whatever `nngmail--spinner-idle-function' provides."
  (or (nngmail--mode-line-flash-string)
      (and nngmail--spinner-idle-function
           (condition-case nil (funcall nngmail--spinner-idle-function) (error nil)))))

(defcustom nngmail-mode-line-images 'auto
  "Whether the G in the mode line is an SVG image or a letter.
`auto': an image on graphic displays, as elsewhere in nngmail.  nil: a
letter G in a face (bold, accent colour while working, muted when idle)
and Braille dots as the spinner, for a terminal-like mode line or taste.
nngmail keeps its element in `global-mode-string' as a literal string
object (see `nngmail--mode-line-element'), the one form every Emacs
31+ renders with images."
  :type '(choice (const :tag "Image when possible" auto)
                 (const :tag "Text" nil)))

(defface nngmail-mode-line-active
  '((((background dark)) :foreground "#58a6ff" :weight bold)
    (t :foreground "#0969da" :weight bold))
  "The text G in the mode line while nngmail works.")

(defface nngmail-mode-line-idle
  '((t :inherit shadow :weight bold))
  "The text G in the mode line while nngmail's background job sleeps.")

(defun nngmail--mode-line-images-p ()
  "Whether the mode line gets SVG images (see `nngmail-mode-line-images')."
  (and nngmail-mode-line-images (nngmail--svg-p)))

(defun nngmail--spinner-glyph (frame &optional dim)
  "The G for the mode line at spinner FRAME; DIM draws the idle one.
An SVG when `nngmail-mode-line-images' allows, else a letter G in a
face (idle) or the Braille dot at FRAME (working); FRAME nil is still."
  (if (nngmail--mode-line-images-p)
      (let ((nngmail--spinner-dim dim)) (nngmail--spinner-svg (or frame 0)))
    (cond (dim (propertize "G" 'face 'nngmail-mode-line-idle))
          ((null frame) (propertize "G" 'face 'nngmail-mode-line-active))
          (t (propertize (aref nngmail--spinner-text-frames
                               (% frame (length nngmail--spinner-text-frames)))
                         'face 'nngmail-mode-line-active)))))

(defun nngmail--spinner-still (tooltip &optional dim account)
  "A non-spinning G for the mode line with TOOLTIP; DIM draws it muted.
With ACCOUNT, its count pills (`nngmail-mode-line-counts') follow.
Also sets the G-only element Gnus mode names carry."
  (let ((pills (and account (nngmail--mode-line-count-pills account)))
        (g (propertize (nngmail--spinner-glyph nil dim) 'help-echo tooltip)))
    (nngmail--mode-name-show g)
    ;; A trailing space: the clock follows this element directly.
    (concat " " g (if pills (concat " " pills) "") " ")))

(defvar nngmail--spinner-last-error nil)

(defun nngmail--spinner-tick ()
  "Advance the spinner one frame and redraw the mode line and status.
A display error must not stop the timer, or the spinner freezes on one
frame while the sync continues; it is logged once and the tick goes on."
  (condition-case err
      (nngmail--spinner-tick-1)
    (error
     (unless (equal err nngmail--spinner-last-error)
       (setq nngmail--spinner-last-error err)
       (nngmail--log "spinner/status redraw failed: %s" (error-message-string err))))))

(defun nngmail--graphic-frame ()
  "A visible graphic frame to measure and draw for, or nil when there is none.
Timers run with an arbitrary selected frame; under a daemon that can be
the initial terminal frame, where `display-graphic-p' is nil and every
image check fails although the user is looking at a GUI frame.  Prefer
the focused frame, then any visible graphic one."
  (let ((focused (and (fboundp 'frame-focus-state)
                      (seq-find (lambda (f) (and (frame-live-p f) (frame-visible-p f)
                                                 (eq (frame-focus-state f) t)))
                                (frame-list)))))
    (cond ((and focused (display-graphic-p focused)) focused)
          ((display-graphic-p) (selected-frame))
          (t (seq-find (lambda (f) (and (frame-live-p f) (frame-visible-p f) (display-graphic-p f)))
                       (frame-list))))))

(defmacro nngmail--with-graphic-frame (&rest body)
  "Run BODY with a graphic frame selected when one exists, else as is.
For code that builds mode-line images from a timer."
  (declare (indent 0))
  (let ((f (make-symbol "frame")))
    `(let ((,f (nngmail--graphic-frame)))
       (if (and ,f (not (eq ,f (selected-frame))))
           (with-selected-frame ,f ,@body)
         ,@body))))

(defun nngmail--spinner-tick-1 ()
  "One spinner frame, drawn against a graphic frame when there is one."
  (nngmail--with-graphic-frame (nngmail--spinner-tick-2)))

(defun nngmail--spinner-tick-2 ()
  "One spinner frame, in a frame whose metrics and image support are right."
  (if (null nngmail--progress)
      (nngmail--spinner-stop)
    (cl-incf nngmail--spinner-frame)
    (nngmail--tick-wait-status)
    (let* ((account (gethash (car nngmail--progress) nngmail--accounts))
           (pills (and account (nngmail--mode-line-count-pills account))))
      (nngmail--mode-name-show (propertize (nngmail--spinner-glyph nngmail--spinner-frame)
                                           'help-echo (nngmail--spinner-tooltip)))
      (nngmail--spinner-show
       (concat " " (nngmail--spinner-glyph nngmail--spinner-frame)
               (propertize (if nngmail--progress-quiet
                               (nngmail--mode-line-progress-text)
                             "nngmail")
                           'help-echo (nngmail--spinner-tooltip))
               (if pills (concat " " pills) "") " ")))
    ;; The status line too: a long request otherwise leaves it stale.
    (nngmail--draw-status)))

(defcustom nngmail-mode-line-width 18
  "Most columns the background job's text beside the G takes in the mode line."
  :type 'integer)

(defcustom nngmail-mode-line-glyphs
  '((checking . "…") (applying . "↻") (fetching . "↓") (indexing . "↓")
    (listing . "…") (backfilling . "↓") (sending . "↑") (done . "✓") (waiting . "…"))
  "Glyph per phase for the text beside the G in the mode line.
The key is the first word of the phase (`applying' for \"applying
changes\"); `done' marks a finished pass, `waiting' a long request.
Phases with no entry show their first word.  The defaults stay within
the arrows and Latin-1 blocks that monospace fonts carry, so the mode
line does not change width when a glyph falls back to another font."
  :type '(alist :key-type symbol :value-type string))

(defun nngmail--mode-line-glyph (key)
  "The glyph for KEY in `nngmail-mode-line-glyphs', or KEY's name."
  (or (alist-get key nngmail-mode-line-glyphs) (symbol-name key)))

(defun nngmail--mode-line-account-prefix (name)
  "NAME followed by a space when more than one account is open, else nothing.
The G already says Gmail; the account name is only needed to tell two apart."
  (if (> (hash-table-count nngmail--accounts) 1) (concat name " ") ""))

(defun nngmail--mode-line-progress-text ()
  "Short progress text for the mode line: a glyph for the phase, then counts.
For example \"↻ 6,000\", \"↓ 120/340\", \"… 7s\" (a long wait)."
  (pcase-let* ((`(,name ,phase ,done ,total ,_extra) nngmail--progress)
               (root (intern (or (car (split-string (or phase "") "[ ,]" t)) "sync")))
               (waiting (and nngmail--status (string-match "\\`waiting \\([0-9]+\\)s" nngmail--status)
                             (match-string 1 nngmail--status)))
               (text
                (cond
                 (waiting (format "%s %ss" (nngmail--mode-line-glyph 'waiting) waiting))
                 ((and done total (> total 0))
                  (format "%s %s/%s" (nngmail--mode-line-glyph root)
                          (nngmail--fmt-compact done) (nngmail--fmt-compact total)))
                 (done (format "%s %s" (nngmail--mode-line-glyph root) (nngmail--fmt-compact done)))
                 (nngmail--status (format "%s %s" (nngmail--mode-line-glyph root) nngmail--status))
                 (t (nngmail--mode-line-glyph root)))))
    (truncate-string-to-width (concat (nngmail--mode-line-account-prefix name) text)
                              nngmail-mode-line-width nil nil "…")))

(defcustom nngmail-mode-line-counts '((unread . "INBOX") (starred . t) (indexed . t))
  "Figures shown as pills beside the G in the mode line when nothing is running.
Each entry is (KIND . SCOPE): KIND `unread' counts messages carrying
UNREAD, `starred' those carrying STARRED, `indexed' the messages in the
index (what the sync-done message used to say); SCOPE is a group name
to count within (\"INBOX\") or t for the whole index.  A zero count
shows no pill.  nil shows no pills."
  :type '(repeat (cons (choice (const unread) (const starred) (const indexed))
                       (choice (const :tag "Whole index" t) string))))

(defface nngmail-pill-indexed
  '((((background dark)) :foreground "#c9d1d9" :background "#30363d")
    (t :foreground "#424a53" :background "#d0d7de"))
  "The indexed-total pill beside the G: neutral, it is a size not a state.")

(defface nngmail-pill-unread
  '((((background dark)) :foreground "#6c3fc5" :background "#e3d9f6")
    (t :foreground "#6c3fc5" :background "#e3d9f6"))
  "The unread-count pill beside the G: the same purple as efrit's `unread'.")

(defface nngmail-pill-starred
  '((((background dark)) :foreground "#b7791f" :background "#f6e8c8")
    (t :foreground "#b7791f" :background "#f6e8c8"))
  "The starred-count pill beside the G.")

(defun nngmail--count-labelled (account label &optional group)
  "How many messages of ACCOUNT carry LABEL, within GROUP when given."
  (let ((labels (nngmail-account-labels account))
        (n 0))
    (if (stringp group)
        (when-let* ((g (nngmail--group account group)))
          (maphash (lambda (_local num)
                     (when (member label (gethash num labels)) (cl-incf n)))
                   (nngmail-group-local->num g)))
      (maphash (lambda (_num l) (when (member label l) (cl-incf n))) labels))
    n))

(defun nngmail--lozenge-path (x y w h)
  "The path data of a stadium (pill) at X Y, W by H.
Explicit semicircular arcs at both ends; immune to renderers that drop
`rx' on small rectangles."
  (let ((r (/ h 2.0)))
    (format "M%.2f,%.2f H%.2f A%.2f,%.2f 0 0 1 %.2f,%.2f H%.2f A%.2f,%.2f 0 0 1 %.2f,%.2f Z"
            (+ x r) y (- (+ x w) r)
            r r (- (+ x w) r) (+ y h)
            (+ x r)
            r r (+ x r) y)))

(defun nngmail--svg-lozenge (svg x y w h fill)
  "Append to SVG a flat stadium (pill) at X Y, W by H, filled FILL."
  (svg-node svg 'path :d (nngmail--lozenge-path x y w h) :fill fill))

(defun nngmail--color-rgb (color)
  "COLOR as (R G B) in 0..1.  #rrggbb is parsed here; names go to the display."
  (if (string-match "\\`#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\'" color)
      (mapcar (lambda (i) (/ (string-to-number (match-string i color) 16) 255.0)) '(1 2 3))
    (or (color-name-to-rgb color) '(0.5 0.5 0.5))))

(defun nngmail--shade (color amount)
  "COLOR lightened (AMOUNT > 0) or darkened (AMOUNT < 0) in HSL lightness."
  (pcase-let* ((`(,r ,g ,b) (nngmail--color-rgb color))
               (`(,h ,s ,l) (color-rgb-to-hsl r g b))
               (l (max 0.0 (min 1.0 (+ l amount)))))
    (apply #'color-rgb-to-hex (append (color-hsl-to-rgb h s l) '(2)))))

(defvar nngmail--svg-id 0 "Counter for unique ids inside one SVG document.")

(defun nngmail--svg-raised-lozenge (svg x y w h fill)
  "Append to SVG a stadium at X Y, W by H in FILL with depth.
A `linearGradient' from a lighter FILL at the top to a darker one at
the bottom, a blurred darker copy offset downward as a shadow, and a
hairline of near-white at low opacity along the upper arc.  Text drawn
afterwards sits flat on it.  Ids are unique per document so several
pills can share a cache or a canvas."
  (let* ((n (cl-incf nngmail--svg-id))
         (grad (format "nngmail-g%d" n))
         (blur (format "nngmail-b%d" n))
         (defs (or (car (dom-by-tag svg 'defs))
                   (let ((d (dom-node 'defs))) (svg--append svg d) d)))
         ;; Light at the top, darker at the bottom; dark pills get a
         ;; wider spread (they have room) and a weaker highlight.
         (dark-pill (< (nth 2 (apply #'color-rgb-to-hsl (nngmail--color-rgb fill))) 0.5))
         (light (nngmail--shade fill (if dark-pill 0.14 0.07)))
         (dark (nngmail--shade fill (if dark-pill -0.10 -0.16)))
         (shadow (nngmail--shade fill -0.55))
         (highlight-opacity (if dark-pill "0.25" "0.6")))
    ;; gradient and blur filter
    (dom-append-child
     defs (dom-node 'linearGradient `((id . ,grad) (x1 . "0") (y1 . "0") (x2 . "0") (y2 . "1"))
                    (dom-node 'stop `((offset . "0%") (stop-color . ,light)))
                    (dom-node 'stop `((offset . "100%") (stop-color . ,dark)))))
    (dom-append-child
     defs (dom-node 'filter `((id . ,blur) (x . "-10%") (y . "-30%") (width . "120%") (height . "180%"))
                    (dom-node 'feGaussianBlur `((stdDeviation . ,(format "%.2f" (* 0.09 h)))))))
    ;; shadow: the same shape, darker, a little lower, blurred and faint
    (svg-node svg 'path :d (nngmail--lozenge-path x (+ y (* 0.12 h)) w h)
              :fill shadow :fill-opacity "0.6" :filter (format "url(#%s)" blur))
    ;; body
    (svg-node svg 'path :d (nngmail--lozenge-path x y w h) :fill (format "url(#%s)" grad))
    ;; top highlight: a thin arc inset from the upper edge
    (let* ((r (/ h 2.0)) (inset (* 0.08 h)))
      (svg-node svg 'path
                :d (format "M%.2f,%.2f H%.2f" (+ x r) (+ y inset) (- (+ x w) r))
                :fill "none" :stroke "#ffffff" :stroke-opacity highlight-opacity
                :stroke-width (format "%.2f" (max 0.8 (* 0.06 h))) :stroke-linecap "round"))))

(defun nngmail--pill-body (svg x y w h fill)
  "Append to SVG the pill shape at X Y, W by H in FILL, per `nngmail-pill-style'.
The one drawer for every pill nngmail puts on the screen (mode line,
echo-area status line): `pills-draw-lozenge' from the pills package
when it is loaded, so all pills on the screen match; nngmail's own
otherwise."
  (cond ((fboundp 'pills-draw-lozenge)
         (funcall 'pills-draw-lozenge svg x y w h fill nngmail-pill-style))
        ((eq nngmail-pill-style 'raised)
         (nngmail--svg-raised-lozenge svg x y w h fill))
        (t (nngmail--svg-lozenge svg x y w h fill))))

(defun nngmail--status-pill-height (lh)
  "Height of a status-line pill for line height LH, with room for its shadow."
  (max 8 (round (* (if (eq nngmail-pill-style 'raised) 0.80 0.88) lh))))

(defun nngmail--mode-line-count-pill (text face tooltip)
  "TEXT as a small pill in FACE's colours for the mode line, TOOLTIP as help."
  (let ((word (propertize text 'face face 'help-echo tooltip)))
    (if (not (nngmail--mode-line-images-p))
        (concat "[" word "]")
      (require 'svg)
      (condition-case nil
          (let* ((lh (frame-char-height))
                 (pill-h (max 10 (round (* (if (eq nngmail-pill-style 'raised) 0.82 0.88) lh))))
                 (font-size (round (* 0.6 pill-h)))
                 (glyph (* 0.62 font-size))
                 ;; Room for the text plus a cell of air (the inset ends).
                 (columns (max (1+ (string-width text))
                               (nngmail--cells-for (+ (* (string-width text) glyph) (* 2 (frame-char-width))))))
                 (family (format "'%s', ui-monospace, Menlo, monospace"
                                 (or (face-attribute 'default :family) "monospace")))
                 (bg (face-attribute face :background nil 'default))
                 (fg (face-attribute face :foreground nil 'default))
                 (key (list 'count text face lh (frame-char-width) (nngmail--mode-line-background) nngmail-pill-style))
                 (img (or (gethash key nngmail--gauge-cache)
                          (puthash key
                                   (nngmail--cell-image
                                    columns
                                    (lambda (svg w h)
                                      ;; The pill sits inset in the full-height
                                      ;; canvas, so it centres with the G; the
                                      ;; text is centred in the pill, baseline
                                      ;; nudged for the font's descent.
                                      ;; Half a cell of air at each end: a lozenge
                                      ;; flush with the canvas edge has its arcs
                                      ;; clipped and reads as flat-ended.
                                      (let ((top (/ (- h pill-h) 2.0))
                                            (inset (/ (frame-char-width) 3.0)))
                                        ;; One pill drawer for the whole screen when
                                        ;; the user's is loaded; nngmail's own otherwise.
                                        (nngmail--pill-body svg inset top (- w (* 2 inset)) pill-h bg)
                                        (svg-text svg text :x (/ w 2.0) :y (+ top (* pill-h 0.5) (* font-size 0.35))
                                                  :text-anchor "middle"
                                                  :font-family family :font-weight "bold" :font-size font-size
                                                  :fill fg)))
                                    text)
                                   nngmail--gauge-cache))))
            ;; No face on the covered character: the mode line is taller
            ;; than the image (26 px over 24 here) and Emacs fills the gap
            ;; above and below with the character's face background, which
            ;; drew a band in the pill's colour (probed 2026-10-20).  The
            ;; colours live in the image; the face stays on the tooltip text.
            (propertize (copy-sequence img) 'help-echo tooltip))
        (error (concat "[" word "]"))))))

(defvar nngmail--count-pills-cache nil
  "(ACCOUNT-NAME GENERATION . PILLS): the last pills built, to reuse between label changes.
The spinner asks for them several times a second; counting walks the
index, so the result is kept until `nngmail--count-pills-generation'
moves, which every label change does.")

(defvar nngmail--count-pills-generation 0
  "Bumped on every label change; invalidates `nngmail--count-pills-cache'.")

(defun nngmail--mode-line-count-pills (account)
  "The count pills for ACCOUNT per `nngmail-mode-line-counts', joined, or nil.
Cached until the next label change."
  (let ((key (list (nngmail-account-name account) nngmail--count-pills-generation
                   nngmail-mode-line-counts)))
    (if (equal (car nngmail--count-pills-cache) key)
        (cdr nngmail--count-pills-cache)
      (let ((pills (nngmail--mode-line-count-pills-1 account)))
        (setq nngmail--count-pills-cache (cons key pills))
        pills))))

(defun nngmail--mode-line-count-pills-1 (account)
  "Build the count pills for ACCOUNT (see `nngmail--mode-line-count-pills')."
  (let ((pills nil))
    (pcase-dolist (`(,kind . ,scope) nngmail-mode-line-counts)
      (let ((n (pcase kind
                 ('unread (nngmail--count-labelled account "UNREAD" scope))
                 ('starred (nngmail--count-labelled account "STARRED" scope))
                 ('indexed (if (stringp scope)
                               (nngmail--group-size account scope)
                             (hash-table-count (nngmail-account-num->id account)))))))
        (when (and n (> n 0))
          (push (nngmail--mode-line-count-pill
                 (pcase kind
                   ('starred (format "%s ★" (nngmail--fmt-compact n)))
                   ('unread (format "%s unread" (nngmail--fmt-compact n)))
                   (_ (nngmail--fmt-compact n)))
                 (pcase kind ('starred 'nngmail-pill-starred) ('unread 'nngmail-pill-unread)
                        (_ 'nngmail-pill-indexed))
                 (format "%s: %s %s%s" (nngmail-account-name account) (nngmail--fmt-num n)
                         (pcase kind ('starred "starred") ('unread "unread") (_ "messages indexed"))
                         (if (stringp scope) (format " in %s" scope) "")))
                pills))))
    ;; No separator: each pill carries a third of a cell of air at both
    ;; ends, which is the gap between them.
    (and pills (mapconcat #'identity (nreverse pills) ""))))

(defvar nngmail--mode-line-flash nil
  "(TEXT TOOLTIP UNTIL) while a short notice is shown beside the G, else nil.
`nngmail--spinner-idle-string' shows it in place of the idle indicator
until UNTIL (a `float-time'); nothing else needs to know about it.")

(defvar nngmail--mode-line-flash-timer nil)

(defun nngmail--mode-line-flash (text &optional tooltip seconds)
  "Show TEXT beside the G in the mode line for SECONDS (default 10), then revert.
TOOLTIP (default TEXT) is the help-echo.  Used for the background job's
done message so the echo area stays free.  The flash is state that the
idle indicator reads, so whichever code redraws the spinner string
meanwhile draws the flash, not the plain idle G."
  (when (timerp nngmail--mode-line-flash-timer)
    (cancel-timer nngmail--mode-line-flash-timer))
  (setq nngmail--mode-line-flash (list text (or tooltip text) (+ (float-time) (or seconds 10))))
  (setq nngmail--mode-line-flash-timer
        (run-at-time (or seconds 10) nil
                     (lambda ()
                       (setq nngmail--mode-line-flash nil
                             nngmail--mode-line-flash-timer nil)
                       (nngmail--spinner-refresh-idle))))
  (nngmail--spinner-refresh-idle))

(defun nngmail--mode-line-flash-string ()
  "The flash as mode-line text, or nil when none is current."
  (pcase nngmail--mode-line-flash
    (`(,text ,tooltip ,until)
     (if (< (float-time) until)
         (concat " " (nngmail--spinner-glyph nil t)
                 (propertize (truncate-string-to-width text nngmail-mode-line-width nil nil "…")
                             'help-echo tooltip))
       (setq nngmail--mode-line-flash nil)
       nil))))

(defun nngmail--spinner-tooltip ()
  "Help text for the mode-line G: the current phase and its figures."
  (pcase-let ((`(,name ,phase ,done ,total ,_) nngmail--progress))
    (format "%s: %s%s" name phase
            (if (and done total) (format " %s/%s" (nngmail--fmt-num done) (nngmail--fmt-num total)) ""))))

(defcustom nngmail-mode-line-position 'front
  "Where nngmail's indicator goes in `global-mode-string'.
`front': right after the leading \"\" (before the clock, load and
other packages), so it survives when a narrow window truncates the mode
line from the right.  `end': appended after everything else."
  :type '(choice (const front) (const end)))

(defvar nngmail-indicator-changed-hook nil
  "Run after the mode-line indicator string changed.
For other displays that copy it (efrit's agent header) and need a redraw.")

(defun nngmail--mode-line-empty ()
  "A fresh element that displays nothing.
Not the literal \"\": that object is shared by everyone, and removing
or replacing it by identity would hit other people's entries."
  (propertize " " 'invisible t 'nngmail t))

(defvar nngmail--mode-line-element (nngmail--mode-line-empty)
  "The string object nngmail keeps in `global-mode-string'.
A literal string element is the one form whose `display' images the
mode line renders on every Emacs tested (an `:eval' returning the same
string, or a symbol holding it, lose them on an Emacs 32.0.50
snapshot: probed 2026-10-14).  `nngmail--spinner-show' puts a fresh
copy in its place on every change.")

(defun nngmail--mode-line-element-p (element)
  "Whether ELEMENT of `global-mode-string' is nngmail's string."
  (and (stringp element) (> (length element) 0) (get-text-property 0 'nngmail element)))

(defun nngmail--spinner-show (text)
  "Make TEXT (a propertized string or nil) the mode line's nngmail element.
Replaces the string object held in `global-mode-string' with a fresh
copy of TEXT, in the same position."
  (setq nngmail--spinner-string text)
  (let ((old nngmail--mode-line-element)
        (new (if (and text (not (string-empty-p text)))
                 (propertize (copy-sequence text) 'nngmail t)
               (nngmail--mode-line-empty))))
    (setq nngmail--mode-line-element new)
    (when (listp global-mode-string)
      (let ((cell (or (memq old global-mode-string)
                      (seq-drop-while (lambda (e) (not (nngmail--mode-line-element-p e)))
                                      global-mode-string))))
        (when cell (setcar cell new)))))
  (force-mode-line-update t)
  (run-hooks 'nngmail-indicator-changed-hook))

(defcustom nngmail-mode-line-modes '(gnus-group-mode gnus-summary-mode gnus-article-mode)
  "Major modes whose mode lines also show the G alone, after the mode name.
In these buffers the mode list reads \"(Group G Plugged Topic ...)\",
where efrit puts its own mark; the G spins while nngmail works.  The
full indicator (G, phase, count pills) is in `global-mode-string' for
every buffer.  nil puts nothing after mode names."
  :type '(repeat symbol))

(defvar nngmail--mode-name-element (nngmail--mode-line-empty)
  "The G-only string object held in Gnus buffers' `mode-name'.
Same rule as `nngmail--mode-line-element': a literal string, replaced
in place on change, so its image renders.")

(defun nngmail--mode-name-show (glyph)
  "Make GLYPH (the G string, or nil) the element after Gnus mode names."
  (let ((old nngmail--mode-name-element)
        (new (if glyph (propertize (concat " " glyph) 'nngmail t) (nngmail--mode-line-empty))))
    (setq nngmail--mode-name-element new)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (memq major-mode nngmail-mode-line-modes) (listp mode-name))
          (let ((cell (or (memq old mode-name)
                          (seq-drop-while (lambda (e) (not (nngmail--mode-line-element-p e))) mode-name))))
            (when cell (setcar cell new))))))))

(defun nngmail--mode-name-install ()
  "In the current buffer, put the G string after `mode-name' once."
  (when (and (memq major-mode nngmail-mode-line-modes)
             (not (and (listp mode-name) (seq-some #'nngmail--mode-line-element-p mode-name))))
    (setq mode-name (list (if (listp mode-name) mode-name (or mode-name (symbol-name major-mode)))
                          nngmail--mode-name-element))))

(dolist (mode nngmail-mode-line-modes)
  (add-hook (intern (format "%s-hook" mode)) #'nngmail--mode-name-install))

(defun nngmail--spinner-installed-p ()
  "Whether nngmail's element is in `global-mode-string'."
  (and (listp global-mode-string)
       (seq-some #'nngmail--mode-line-element-p global-mode-string)))

(defun nngmail--spinner-install ()
  "Put nngmail's string element in `global-mode-string', replacing older forms.
Placed per `nngmail-mode-line-position'.  Gnus buffers also get the
G-only element after their mode name (`nngmail-mode-line-modes')."
  (let* ((others (seq-remove (lambda (e) (or (eq e 'nngmail--spinner-string)
                                             (equal e '(:eval (nngmail--spinner-mode-line)))
                                             (nngmail--mode-line-element-p e)))
                             (if (listp global-mode-string) global-mode-string
                               (list global-mode-string))))
         (lead (and (equal (car others) "") (list (pop others)))))
    (setq global-mode-string
          (if (eq nngmail-mode-line-position 'front)
              (append lead (list nngmail--mode-line-element) others)
            (append lead others (list nngmail--mode-line-element)))))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      ;; An old `:eval' form from 0.6.0 goes; the G string goes in.
      (when (and (listp mode-name) (member '(:eval (nngmail--mode-line-indicator)) mode-name))
        (setq mode-name (delete '(:eval (nngmail--mode-line-indicator)) mode-name)))
      (nngmail--mode-name-install))))

(defun nngmail--spinner-start ()
  "Show the spinner in the mode line and start its timer."
  (unless (nngmail--spinner-installed-p)
    (nngmail--spinner-install))
  (unless nngmail--spinner-timer
    (setq nngmail--spinner-timer
          (run-at-time 0 nngmail-spinner-interval #'nngmail--spinner-tick))))

(defun nngmail--spinner-stop ()
  "Stop spinning: show the idle G when something provides one, else nothing."
  (when nngmail--spinner-timer
    (cancel-timer nngmail--spinner-timer)
    (setq nngmail--spinner-timer nil))
  (nngmail--with-graphic-frame
    (let ((idle (nngmail--spinner-idle-string)))
      (when (and idle (not (nngmail--spinner-installed-p)))
        (nngmail--spinner-install))
      (unless idle (nngmail--mode-name-show nil))
      (nngmail--spinner-show idle))))

(defun nngmail--spinner-refresh-idle ()
  "Redraw the idle G (state may have changed) unless the spinner is running."
  (unless nngmail--spinner-timer
    (nngmail--spinner-stop)))

;; A mode line is drawn per frame, and a frame that was not focused
;; when the idle indicator changed (the done flash reverting, a count
;; moving) keeps its last rendering until something forces a redraw.
;; Refresh on every focus change, and on a slow heartbeat while idle,
;; so no frame can hold a stale G.

(defun nngmail--spinner-on-focus-change ()
  "On `after-focus-change-function': redraw the idle indicator."
  (when (and (not nngmail--spinner-timer) nngmail--spinner-string)
    (nngmail--spinner-refresh-idle)))

(add-function :after after-focus-change-function #'nngmail--spinner-on-focus-change)

(defvar nngmail--idle-heartbeat-timer
  (run-with-timer 30 30 (lambda ()
                          (when (and (not nngmail--spinner-timer) nngmail--spinner-string)
                            (nngmail--spinner-refresh-idle))))
  "Redraws the idle indicator twice a minute, for frames that missed a change.")

(defun nngmail--draw-status (&optional force)
  "Redraw the one-line status line.
It is built from the variables `nngmail--progress' and `nngmail--status'.
Throttled to a few redraws per second unless FORCE.  Never logs.
Drawn in, and laid out for, the frame that has focus
\(`nngmail--status-frame'): the images are sized by that frame's font
and the line by its width, so a second frame of another size gets its
own rendering (the image cache keys on the cell size)."
  (when (and nngmail--progress
             (not nngmail--progress-quiet)
             (or force (> (- (float-time) nngmail--progress-last-shown) 0.25)))
    (setq nngmail--progress-last-shown (float-time))
    (let ((frame (nngmail--status-frame)))
      (if (eq frame (selected-frame))
          (nngmail--draw-status-1)
        (with-selected-frame frame
          (nngmail--draw-status-1))))))

(defun nngmail--draw-status-1 ()
  "Lay out and show the status line in the selected frame."
  (progn
    (pcase-let* ((`(,name ,phase ,done ,total ,extra) nngmail--progress)
                 (have-total (and done total (> total 0)))
                 (frac (and have-total (/ (float done) total)))
                 ;; Fixed columns: phase | bar (with its label inside) |
                 ;; extra bar | gauges | status.  The phase column has a
                 ;; fixed width; the status column takes the rest of the
                 ;; line, so on a wide frame nothing is cut short.
                 (width (nngmail--status-width))
                 (gauges (nngmail--gauges))
                 (gauges-w (string-width gauges))
                 (eta (if (and extra (consp extra))
                          nil
                        (nngmail--fmt-eta phase done total)))
                 (label (nngmail--bar-label done total eta))
                 ;; The bar column is always reserved; without a total it is
                 ;; shown empty (or with a bare count) so nothing shifts.
                 (bar (cond ((null nngmail-status-bar) label)
                            (have-total (nngmail--status-bar frac (and nngmail--status t) label))
                            (t (nngmail--status-bar 0.0 nil label))))
                 (bar-w (if nngmail-status-bar nngmail-status-bar-width (+ 2 (string-width label))))
                 ;; The extra column (a backfill's mailbox scan, a history
                 ;; pass's page count) is a second lozenge of the same
                 ;; width; empty track when there is nothing to show, so
                 ;; the line keeps its shape.
                 (extra-w (if nngmail-status-bar nngmail-status-bar-width 40))
                 (extra-s (nngmail--fmt-extra extra phase))
                 (extra-cell (nngmail--status-cell extra-s extra-w))
                 ;; Four single-space separators.  The phase lozenge takes
                 ;; what the bars and gauges leave, with a floor.
                 (phase-w (max 14 (- width bar-w extra-w gauges-w
                                     (if (and nngmail-status-bar (nngmail--svg-p)) 3 9))))
                 (phase-cell (nngmail--status-pill (concat name ": " phase) phase-w
                                                   (format "%s: %s" name phase)))
                 ;; Lozenges separate themselves; text cells need a rule.
                 (sep (if (and nngmail-status-bar (nngmail--svg-p)) " " " │ "))
                 (line (concat phase-cell sep bar sep extra-cell sep gauges)))
      ;; Redraw when the line changed, and also when something else has
      ;; since written to the echo area (a Gnus message, a minibuffer
      ;; exit): otherwise a status line wiped by another message stays
      ;; gone until the next change, which during a long request is a
      ;; long time.
      (unless (and (equal line nngmail--status-last-line)
                   (equal (current-message) line))
        (setq nngmail--status-last-line line)
        (let ((message-log-max nil))   ; echo area only, never *Messages*
          (message "%s" line))
        (unless noninteractive (redisplay))))))

(defun nngmail--fmt-compact (n)
  "N as a short figure: 950, 12.3k, 2.50M.
Below 10,000 the exact figure; thousands with one decimal; millions
with two, so a change of ten thousand messages still shows."
  (let ((n (nngmail--finite n)))
    (cond ((< n 10000) (nngmail--fmt-num n))
          ((< n 1000000) (format "%.1fk" (/ n 1000.0)))
          (t (format "%.2fM" (/ n 1000000.0))))))

(defun nngmail--bar-label (done total eta)
  "Text drawn inside a progress bar: DONE/TOTAL, percentage and ETA.
Fields are separated by two spaces so `nngmail--fit-label' can drop
them from the right; the first field uses compact figures when the bar
is narrower than 24 cells so it survives on its own."
  (let ((num (if (< nngmail-status-bar-width 24) #'nngmail--fmt-compact #'nngmail--fmt-num)))
    (cond
     ((and done total (> total 0))
      (concat (format "%s/%s" (funcall num done) (funcall num total))
              (format "  %d%%" (/ (* 100 done) total))
              (if eta (concat "  " eta) "")))
     (done (concat (funcall num done) (if eta (concat "  " eta) "")))
     (t (or eta "")))))

(defun nngmail--fmt-extra (extra phase)
  "Render the status line's extra column from EXTRA during PHASE.
A string is used as is.  A list (SCANNED FOUND TOTAL) becomes a two-tone
bar of the mailbox -- the part scanned so far, with the fraction of it
that was new shaded differently -- labelled inside with the counts and
the time left for the scan.  TOTAL nil means the mailbox size is
unknown: the bar is empty and only the counts show."
  (pcase extra
    ((pred null) nil)
    ((pred stringp) extra)
    (`(,scanned ,found ,total)
     (let* ((known (and total (> total 0)))
            (eta (nngmail--fmt-eta (concat phase "/scan") scanned (and known total)))
            ;; Fields in the order they may be dropped from the right when
            ;; the bar is narrow: the ETA matters more than the new count.
            (label (concat (format "%s scanned" (nngmail--fmt-num scanned))
                           (if eta (concat "  " eta) "")
                           (if found (format "  %s new" (nngmail--fmt-num found)) ""))))
       (if known
           (nngmail--status-bar-dual (/ (float (min scanned total)) total)
                                     (/ (float (min (or found 0) total)) total)
                                     label)
         (nngmail--status-bar-dual 0.0 0.0 label))))
    (_ (format "%s" extra))))

(defun nngmail--status-bar-dual (outer inner &optional label)
  "A bar OUTER (0..1) full in the muted colour with INNER (0..1) of it in accent.
OUTER is how much of the mailbox has been scanned; INNER how much of the
mailbox was new and got indexed.  LABEL is drawn inside.  Uses SVG when
available, else text."
  (let* ((w nngmail-status-bar-width)
         (o (max 0.0 (min 1.0 (or outer 0.0))))
         (i (max 0.0 (min o (or inner 0.0))))
         (mode (pcase nngmail-status-bar
                 ('auto (if (nngmail--svg-p) 'svg 'text))
                 (m m))))
    (pcase mode
      ('nil (or label ""))
      ('svg
       (require 'svg)
       (condition-case err
           (let ((dark (eq (frame-parameter nil 'background-mode) 'dark)))
             (nngmail--status-bar-svg
              w (list (cons o (if dark "#58a6ff" "#0969da"))
                      (cons i (if dark "#1f6feb" "#54aeff")))
              label
              (format "%d%% of mailbox scanned, %d%% new" (round (* 100 o)) (round (* 100 i)))))
         (error
          (nngmail--log "SVG status bar unavailable (%s); using text" (error-message-string err))
          (setq nngmail-status-bar 'text)
          (nngmail--status-bar-dual outer inner label))))
      (_
       (nngmail--status-bar-text w (list o i) label)))))

(defun nngmail--set-status (status)
  "Set the transient STATUS (a string or nil) and redraw immediately."
  (setq nngmail--status status)
  (nngmail--draw-status t))

(defun nngmail--progress (account phase &optional done total force)
  "Record sync progress for ACCOUNT: PHASE with DONE of TOTAL, and redraw.
PHASE nil clears the state and prints \"done\" (which is logged).
FORCE redraws even when nothing changed."
  (let ((name (nngmail-account-name account)))
    (if (null phase)
        (progn
          (nngmail--log-phase-done)
          (setq nngmail--progress nil
                nngmail--status nil
                nngmail--status-last-line nil
                nngmail--rate nil)
          (nngmail--spinner-stop)
          (let* ((indexed (nngmail--fmt-compact (hash-table-count (nngmail-account-num->id account))))
                 (text (format "%s: sync done (%s messages indexed)" name indexed)))
            (if nngmail--progress-quiet
                ;; The idle indicator's pills carry the total; no flash.
                (nngmail--log "%s" text)
              (nngmail--message 5 "%s" text)))
          (setq nngmail--progress-quiet nil))
      (setq nngmail--progress (list name phase done total nngmail--progress-extra))
      (unless noninteractive (nngmail--spinner-start))
      ;; Log phase changes once, not every tick, and ignore sub-phase
      ;; suffixes like ", listing" that alternate every page.
      (let ((root (car (split-string phase "," t))))
        (unless (equal root nngmail--progress-last-phase)
          (nngmail--log-phase-done)
          (setq nngmail--progress-last-phase root
                nngmail--phase-started (float-time))
          (nngmail--log "%s: %s%s" name root
                        (if (and total (> total 0))
                            (format " (%s)" (nngmail--fmt-num total))
                          ""))))
      (nngmail--draw-status force))))

(defmacro nngmail--with-progress (account phase &rest body)
  "Run BODY with PHASE shown as ACCOUNT's progress.
A sync that already shows progress keeps it.
For one-off synchronous operations (a send, a search) so the spinner
runs and the status line redraws while they wait on Gmail.  The
display is cleared quietly afterwards; a sync's own display is left
alone."
  (declare (indent 2))
  (let ((own (make-symbol "own")))
    `(let ((,own (null nngmail--progress)))
       (when ,own (nngmail--progress ,account ,phase nil nil t))
       (unwind-protect (progn ,@body)
         (when ,own (nngmail--progress-clear))))))

(defun nngmail--log-phase-done ()
  "Log how long the phase that is ending took, if it ran long enough to matter."
  (when (and nngmail--progress-last-phase nngmail--phase-started)
    (let ((took (- (float-time) nngmail--phase-started)))
      (when (>= took 5)
        (nngmail--log "%s took %s" nngmail--progress-last-phase (nngmail--fmt-duration took)))))
  (setq nngmail--phase-started nil))

(defun nngmail--progress-clear ()
  "Drop the progress display without announcing a completed sync."
  (nngmail--log-phase-done)
  (setq nngmail--progress nil
        nngmail--progress-quiet nil
        nngmail--status nil
        nngmail--status-last-line nil
        nngmail--progress-last-phase nil
        nngmail--rate nil)
  (nngmail--spinner-stop)
  (nngmail--draw-status t))

(defun nngmail--finite (n)
  "N if it is a finite number, else 0.
A rate of zero divided into a remainder is infinite, and `truncate' of
an infinite float signals `overflow-error' -- inside a timer that took
the spinner down.  Every formatter goes through this."
  (if (and (numberp n) (or (integerp n) (and (= n n) (not (= n 1.0e+INF)) (not (= n -1.0e+INF)))))
      n
    0))

(defun nngmail--fmt-num (n &optional width)
  "Format integer N with thousands separators, right-aligned in WIDTH."
  (let* ((n (nngmail--finite n))
         (s (number-to-string (abs (truncate n))))
         (parts nil))
    (while (> (length s) 3)
      (push (substring s -3) parts)
      (setq s (substring s 0 -3)))
    (push s parts)
    (let ((out (concat (if (< n 0) "-" "") (mapconcat #'identity parts ","))))
      (if width (format (format "%%%ds" width) out) out))))

;;;; Authentication

(defun nngmail--auth-hosts (account)
  "Candidate :host values for looking up ACCOUNT's credentials."
  (delete-dups
   (delq nil (list (or nngmail-auth-host (nngmail-account-name account))
                   "gmail.com" "imap.gmail.com" "smtp.gmail.com"))))

(defconst nngmail--auth-required-keys '(:client-id :client-secret :auth-url :token-url)
  "Keys an auth-source entry must carry to be usable by nngmail.")

(defun nngmail--auth-search (host user)
  "Search auth-source for HOST/USER entries carrying OAuth2 fields.
The search is done with `auth-source-xoauth2-plugin' advice disabled,
so the plugin does not run its own token dance (or drop entries whose
user differs from `smtpmail-smtp-user') while we are only looking for
credentials.  Filtering on the required keys is done here rather than
with :require so that backends that treat :require differently all
behave the same."
  (let* (;; auth-source caches empty results too (for
         ;; `auth-source-cache-expiry'), and an earlier search that went
         ;; through the xoauth2 plugin may well have cached one for this
         ;; very spec.  Never consult or populate the cache here.
         (auth-source-do-cache nil)
         (results
          (if (advice-member-p 'auth-source-xoauth2-plugin--search-backends
                               'auth-source-search-backends)
              (unwind-protect
                  (progn
                    (advice-remove 'auth-source-search-backends
                                   'auth-source-xoauth2-plugin--search-backends)
                    (auth-source-search :host host :user user :max 20))
                (advice-add 'auth-source-search-backends :around
                            'auth-source-xoauth2-plugin--search-backends))
            (auth-source-search :host host :user user :max 20))))
    (seq-find (lambda (e)
                (cl-every (lambda (k) (plist-get e k)) nngmail--auth-required-keys))
              results)))

(defun nngmail--find-auth (account)
  "Return the auth-source plist for ACCOUNT, or signal an error."
  (let ((user (nngmail-account-user account))
        found)
    (dolist (host (nngmail--auth-hosts account))
      (unless found
        (nngmail--trace "auth-source lookup host=%s user=%s" host user)
        (setq found (nngmail--auth-search host user))))
    (unless found
      (error "nngmail: No auth-source entry with %S for %s (tried hosts %S)"
             nngmail--auth-required-keys user (nngmail--auth-hosts account)))
    found))

(defun nngmail--auth-get (auth key)
  "Return KEY from AUTH plist, resolving secret functions."
  (let ((v (plist-get auth key)))
    (if (functionp v) (funcall v) v)))

(defun nngmail--obtain-token (account)
  "Get (or refresh) the OAuth2 token for ACCOUNT; return the access token string.
Shares the plstore entry with auth-source-xoauth2-plugin: same auth-url,
token-url, scope, client-id and user-name produce the same
`oauth2-compute-id'."
  (let* ((auth (or (nngmail-account-auth account)
                   (setf (nngmail-account-auth account) (nngmail--find-auth account))))
         (host (or (nngmail--auth-get auth :host) (nngmail-account-name account)))
         (scope (or (nngmail--auth-get auth :scope) "https://mail.google.com/"))
         (token (or (nngmail-account-token account)
                    (setf (nngmail-account-token account)
                          (oauth2-auth-and-store
                           (nngmail--auth-get auth :auth-url)
                           (nngmail--auth-get auth :token-url)
                           scope
                           (nngmail--auth-get auth :client-id)
                           (nngmail--auth-get auth :client-secret)
                           (nngmail--auth-get auth :redirect-uri)
                           (nngmail--auth-get auth :state)
                           (nngmail-account-user account)
                           host
                           (let ((p (nngmail--auth-get auth :use-pkce)))
                             (and p (not (member p '("false" "nil" "no")))))))))
         (refreshed (progn
                      ;; A refresh rewrites the plstore with the cached
                      ;; passphrase; prove that passphrase first.  Only
                      ;; when a refresh is due: this runs on every request.
                      (when (nngmail--token-refresh-due-p token host)
                        (nngmail--verify-token-store))
                      (nngmail--refresh-token token host))))
    (setf (nngmail-account-token account) refreshed)
    (oauth2-token-access-token refreshed)))

;;;; The oauth2 token store (symmetric plstore)

;; `oauth2-refresh-access' rewrites `oauth2-token-file' about once an
;; hour, encrypting with whatever passphrase `plstore-passphrase-alist'
;; holds for it.  plstore caches a passphrase at the first prompt without
;; checking it, so one mistyped answer silently re-encrypts the store
;; with the wrong key and the next session cannot read it.  Two guards:
;; before a refresh the store is decrypted with the cached passphrase (a
;; failure clears the cache and prompts again), and the last plaintext
;; that decrypted is kept in memory so a poisoned file can be rewritten
;; from it.  An empty passphrase is refused at the prompt.

(defvar plstore-passphrase-alist)
(defvar oauth2-token-file)
(declare-function plstore-open "plstore")
(declare-function plstore-close "plstore")
(declare-function plstore-get "plstore")
(declare-function plstore-put "plstore")
(declare-function plstore-save "plstore")
(declare-function plstore-get-file "plstore")
(declare-function plstore--decrypt "plstore")
(declare-function plstore--get-alist "plstore")
(declare-function plstore--get-secret-alist "plstore")
(declare-function plstore--get-encrypted-data "plstore")
(declare-function plstore--set-alist "plstore")
(declare-function plstore--set-secret-alist "plstore")
(declare-function plstore--set-encrypted-data "plstore")
(declare-function plstore--merge-secret "plstore")

(defun nngmail--token-refresh-due-p (token host)
  "Whether `oauth2-refresh-access' will request a new access token for TOKEN.
The same test it makes: no cached access token for HOST, or the cached
one older than the `expires_in' of the last access response."
  (let* ((cache (oauth2-token-request-cache token))
         (cached (and cache (plist-get cache (intern host))))
         (stamp (plist-get cached :request-timestamp))
         (expires (cdr (assq 'expires_in (oauth2-token-access-response token)))))
    (not (and (plist-get cached :access-token)
              (numberp stamp) (numberp expires)
              (< (- (time-convert nil 'integer) stamp) expires)))))

(defvar nngmail--token-store-good nil
  "The last state of `oauth2-token-file' that decrypted.
A pair (PUBLIC-ALIST . SECRET-ALIST).
`nngmail-restore-token-store' writes it back when the file is poisoned.")

(defun nngmail--token-store-passphrase-forget ()
  "Drop the cached plstore passphrase for `oauth2-token-file'."
  (when (boundp 'plstore-passphrase-alist)
    (setq plstore-passphrase-alist
          (cl-remove (file-truename oauth2-token-file) plstore-passphrase-alist
                     :key #'car :test #'equal))))

(defun nngmail--token-store-decrypt (store)
  "Decrypt STORE, prompting again if the cached passphrase is wrong.
Signal `nngmail-error' when it still fails.  Non-nil on success."
  (condition-case first
      (progn (plstore--decrypt store) t)
    (error
     (nngmail--log "WARN: %s did not decrypt with the cached passphrase (%s); asking again"
                   oauth2-token-file (error-message-string first))
     (nngmail--token-store-passphrase-forget)
     (condition-case second
         (progn (plstore--decrypt store) t)
       (error
        (nngmail--token-store-passphrase-forget)
        (signal 'nngmail-error
                (list (format "%s cannot be decrypted: %s.  %s"
                              oauth2-token-file (error-message-string second)
                              (if nngmail--token-store-good
                                  "M-x nngmail-restore-token-store rewrites it from the copy in memory"
                                "No good copy in memory: move the file aside and M-x nngmail-reauthorize")))))))))

(defun nngmail--verify-token-store ()
  "Make sure `oauth2-token-file' decrypts with the cached passphrase.
Keeps the decrypted contents in `nngmail--token-store-good'.  A store
without secret entries (first run) is left alone."
  (when (and (boundp 'oauth2-token-file) (file-exists-p oauth2-token-file)
             (fboundp 'plstore-open))
    (let ((store (plstore-open oauth2-token-file)))
      (unwind-protect
          (when (plstore--get-encrypted-data store)
            (nngmail--token-store-decrypt store)
            (setq nngmail--token-store-good
                  (cons (copy-tree (plstore--get-alist store))
                        (copy-tree (plstore--get-secret-alist store))))
            (nngmail--trace "token store %s decrypted; %d entries kept in memory"
                            oauth2-token-file (length (car nngmail--token-store-good))))
        (plstore-close store)))))

;;;###autoload
(defun nngmail-restore-token-store ()
  "Rewrite `oauth2-token-file' from the last contents that decrypted this session.
For when a refresh saved the store with a wrong passphrase.  Asks for
the passphrase to encrypt with, then checks that the result decrypts."
  (interactive)
  (unless nngmail--token-store-good
    (user-error "nngmail: No decrypted copy of %s in memory this session" oauth2-token-file))
  (unless (yes-or-no-p (format "Rewrite %s from the copy in memory (%d entries)? "
                               oauth2-token-file (length (car nngmail--token-store-good))))
    (user-error "nngmail: Not restored"))
  (nngmail--token-store-passphrase-forget)
  (let ((store (plstore-open oauth2-token-file)))
    (unwind-protect
        (progn
          ;; Replace the unreadable cipher text with the good plaintext;
          ;; `plstore-save' then encrypts afresh with the passphrase asked for.
          (plstore--set-alist store (copy-tree (car nngmail--token-store-good)))
          (plstore--set-secret-alist store (copy-tree (cdr nngmail--token-store-good)))
          (plstore--set-encrypted-data store nil)
          (plstore--merge-secret store)
          (plstore-save store))
      (plstore-close store)))
  (nngmail--verify-token-store)
  (message "nngmail: %s rewritten and verified" oauth2-token-file))

(defun nngmail--plstore-refuse-empty-passphrase (orig &rest args)
  "Call ORIG (`plstore-passphrase-callback-function') with ARGS.
An empty answer is refused.
A stray RET would otherwise be cached and used to encrypt the store."
  (let ((answer (apply orig args)))
    (while (and (stringp answer) (string-empty-p answer))
      (nngmail--token-store-passphrase-forget)
      (message "Empty passphrase refused for %s" oauth2-token-file)
      (sit-for 1)
      (setq answer (apply orig args)))
    answer))

(with-eval-after-load 'plstore
  (advice-add 'plstore-passphrase-callback-function :around
              #'nngmail--plstore-refuse-empty-passphrase))

(defvar nngmail--token-persist-warned nil
  "Hosts whose token could not be written back to the oauth2 plstore.
Warned once per host per session; the token still works from memory.")

(defun nngmail--refresh-token (token host)
  "Refresh TOKEN for HOST through `oauth2-refresh-access'; the refreshed token.
The oauth2 library gets the new access token from Google, puts it in
TOKEN, and only then rewrites its plstore.  When that write fails (the
store cannot be decrypted, pinentry is unavailable from a timer) the
refreshed TOKEN is still good for the next hour, so a persist error is
logged once and TOKEN is used.  An error before TOKEN got a new access
token is a real refresh failure and is re-signalled."
  (let ((before (oauth2-token-access-token token)))
    (condition-case err
        (oauth2-refresh-access token host)
      (error
       (let ((after (oauth2-token-access-token token)))
         (if (and after (not (equal after before)))
             (progn
               (unless (member host nngmail--token-persist-warned)
                 (push host nngmail--token-persist-warned)
                 (nngmail--log "WARN: %s: token refreshed but not saved to %s: %s (using it from memory; set epg-debug to see why)"
                               host (if (boundp 'oauth2-token-file) (symbol-value 'oauth2-token-file) "the oauth2 plstore")
                               (error-message-string err)))
               token)
           (signal (car err) (cdr err))))))))

;;;###autoload
(defun nngmail-reauthorize (server)
  "Re-read SERVER's auth-source entry and obtain a token for it afresh.
For after the entry changed -- a new scope (Drive access for
`nngmail-gdocs'), a new client -- when the token held in memory is for
the old one.  The oauth2 store keys tokens by scope and client, so a
changed entry gets its own consent flow; the old token stays stored
and unused.  Opens the browser if consent is needed."
  (interactive (list (nngmail--read-server)))
  (let ((account (nngmail--account server)))
    (setf (nngmail-account-auth account) nil
          (nngmail-account-token account) nil)
    (nngmail--obtain-token account)
    (message "nngmail: %s authorized with scope %s" server
             (or (nngmail--auth-get (nngmail-account-auth account) :scope) "https://mail.google.com/"))))

(defun nngmail--force-refresh (account)
  "Discard the cached access token for ACCOUNT so the next call refreshes."
  (when-let* ((token (nngmail-account-token account)))
    ;; Emptying the request cache makes `oauth2-refresh-access' refresh.
    (setf (oauth2-token-request-cache token) nil)
    (setf (oauth2-token-access-token token) nil)))

;;;; HTTP

(define-error 'nngmail-error "nngmail error")
(define-error 'nngmail-http-error "nngmail HTTP error" 'nngmail-error)

(defun nngmail--url (path &optional params)
  "Build the API URL for PATH with query PARAMS (alist of string pairs)."
  (let ((base (if (string-prefix-p "http" path) path (concat nngmail--api path)))
        (qs (mapconcat (lambda (kv)
                         (concat (url-hexify-string (car kv)) "="
                                 (url-hexify-string (format "%s" (cdr kv)))))
                       (seq-remove (lambda (kv) (null (cdr kv))) params)
                       "&")))
    (if (string-empty-p qs) base
      (concat base (if (string-search "?" base) "&" "?") qs))))

(defun nngmail--json-encode (obj)
  "Encode OBJ (alist/plist/vector) as a JSON string."
  (json-serialize obj :null-object nil :false-object :false))

(defun nngmail--json-parse (string)
  "Parse JSON STRING to alists with symbol keys, lists for arrays."
  (if (or (null string) (string-empty-p string))
      nil
    (json-parse-string string :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defvar url-http-attempt-keepalives)    ; url-http.el, loaded by url-retrieve

(defun nngmail--http-url-el (method url headers body)
  "Perform METHOD on URL with HEADERS and BODY using url.el.
Return (STATUS HEADERS-ALIST BODY-STRING)."
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data (and body (encode-coding-string body 'utf-8)))
        (url-show-status nil)
        (url-mime-charset-string nil)
        (url-http-attempt-keepalives nil))
    (let ((buf (url-retrieve-synchronously url t t 120)))
      (unless buf (signal 'nngmail-http-error (list "no response" url)))
      (with-current-buffer buf
        (unwind-protect
            (let (status hdrs)
              (goto-char (point-min))
              (when (re-search-forward "\\`HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                (setq status (string-to-number (match-string 1))))
              (forward-line 1)
              (while (looking-at "\\([^:\r\n]+\\): *\\(.*?\\)\r?$")
                (push (cons (downcase (match-string 1)) (match-string 2)) hdrs)
                (forward-line 1))
              (re-search-forward "^\r?$" nil t)
              (forward-line 1)
              (list (or status 0) hdrs
                    (decode-coding-string
                     (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
          (kill-buffer buf))))))

(defvar nngmail--plz-last-proc nil
  "The plz process most recently started by `nngmail--http-plz'.
The callbacks capture it via `nngmail--plz-self', since plz calls them
in the process buffer without passing the process.")

(defcustom nngmail-request-timeout 60
  "Seconds to wait for Gmail to answer one HTTP request before giving up.
A request that times out is retried with backoff like any transport
error.  Gmail can take tens of seconds to answer a large batch right
after it has rejected some parts for concurrency; keep this well above
what a normal batch needs, but low enough that a stuck connection does
not look like a frozen Emacs."
  :type 'integer)

(defun nngmail--plz-self ()
  "Return the plz process whose callback is currently running."
  (or (get-buffer-process (current-buffer)) nngmail--plz-last-proc))

(defun nngmail--plz-unpack (r)
  "Turn a `plz-response' or `plz-error' R into (STATUS HEADERS BODY) or signal."
  (cond
   ((and (recordp r) (plz-response-p r))
    (list (plz-response-status r)
          (mapcar (lambda (kv) (cons (downcase (format "%s" (car kv))) (cdr kv)))
                  (plz-response-headers r))
          (decode-coding-string (or (plz-response-body r) "") 'utf-8)))
   ((and (recordp r) (plz-error-p r) (plz-error-response r))
    (nngmail--plz-unpack (plz-error-response r)))
   (t
    (signal 'nngmail-http-error
            (list (format "curl: %s" (nngmail--plz-error-text r)) "")))))

(defun nngmail--plz-error-text (r)
  "Human-readable text for a plz failure R.
A curl failure (timeout, connection refused, DNS) arrives as a
`plz-error' whose `message' slot is nil and whose `curl-error' slot is
\(EXIT-CODE . TEXT); a timeout is code 28."
  (cond
   ((and (recordp r) (plz-error-p r))
    (or (plz-error-message r)
        (pcase (plz-error-curl-error r)
          (`(28 . ,_) (format "no answer within %ds (curl 28: timeout)"
                              nngmail-request-timeout))
          (`(,code . ,text) (format "%s (curl %s)" text code))
          (_ "unknown curl failure"))))
   (t (format "%s" r))))

(defcustom nngmail-slow-request-notice 5
  "After this many seconds waiting on one request, show it in the status line."
  :type 'number)

(defvar nngmail--request-out-since nil
  "`float-time' when the request now out on the wire was sent, or nil.
Set by the background job around each request; the spinner tick turns
it into a live \"waiting Ns\" in the connections gauge once the wait
passes `nngmail-slow-request-notice', as the synchronous wait does.")

(defun nngmail--wait-status (started)
  "Show how long the current request has been waiting since STARTED, or nothing."
  (let ((elapsed (- (float-time) started)))
    (when (>= elapsed nngmail-slow-request-notice)
      (nngmail--set-status (format "waiting %ds" (round elapsed))))))

(defun nngmail--tick-wait-status ()
  "From the spinner tick: show the background request's wait, if it is long."
  (when nngmail--request-out-since
    (let ((elapsed (- (float-time) nngmail--request-out-since)))
      (cond ((>= elapsed nngmail-slow-request-notice)
             (setq nngmail--status (format "waiting %ds" (round elapsed))))
            ((and nngmail--status (string-prefix-p "waiting" nngmail--status))
             (setq nngmail--status nil))))))

(defun nngmail--plz-wait (proc)
  "Wait for the plz request PROC to finish; return its result.
Unlike plz's own synchronous mode, this wait is not wrapped in
`with-local-quit', so a single \\[keyboard-quit] interrupts immediately (the curl
process is killed) instead of being swallowed and turned into a nil
response.  The result is stored on PROC by the callbacks.  A wait longer
than `nngmail-slow-request-notice' is shown in the status line so a slow
server is never mistaken for a hang.

The wait is `(accept-process-output nil ...)', on purpose.  plz's
sentinel does not call the callbacks itself; it schedules them with a
zero-delay timer.  Timers only run inside a wait that really waits, and
`accept-process-output' on a process that has exited returns at once
without waiting.  Waiting on PROC alone therefore spins at full speed
forever once curl exits, with the callback never run -- a hot CPU and a
waiting counter that climbs past the curl timeout."
  (let ((started (float-time))
        (shown nil))
    (unwind-protect
        (progn
          (while (eq (process-get proc :nngmail-state) 'pending)
            (cond
             ((process-live-p proc)
              (nngmail--plz-wait-slice proc))
             ((nngmail--plz-deliver-now proc))
             ((eq (process-get proc :nngmail-state) 'pending)
              ;; Exited, nothing queued yet: the sentinel has not run.
              ;; Let it run; if still nothing, plz lost it.
              (unless (nngmail--plz-await-sentinel proc)
                (process-put proc :nngmail-state 'error)
                (process-put proc :nngmail-result
                             (format "curl exited (%s) without a response"
                                     (process-status proc))))))
            (unless noninteractive
              (let ((sec (floor (- (float-time) started))))
                (unless (eql sec shown)
                  (setq shown sec)
                  (nngmail--wait-status started)))))
          (process-get proc :nngmail-result))
      (when shown
        (nngmail--set-status nil))
      (when (process-live-p proc)
        (delete-process proc)))))

(defun nngmail--plz-callback-timer (proc)
  "The plz timer queued to deliver PROC's response, or nil."
  (seq-find (lambda (timer)
              (and (eq (timer--function timer) 'plz--respond)
                   (eq (car (timer--args timer)) proc)))
            timer-list))

(defun nngmail--plz-callback-pending-p (proc)
  "Non-nil if a plz timer is queued to deliver PROC's response."
  (and (nngmail--plz-callback-timer proc) t))

(defun nngmail--plz-deliver-now (proc)
  "If plz queued PROC's response on a timer, run that timer at once.
Return non-nil if it did.  plz's sentinel does not call the callbacks
itself: it schedules `plz--respond' with a zero-delay timer.  Waiting
for the timer to fire on its own costs up to a whole wait slice per
request (measured: every batch took exactly 0.50 s); running it here
costs nothing."
  (when-let* ((timer (nngmail--plz-callback-timer proc)))
    (cancel-timer timer)
    (apply (timer--function timer) (timer--args timer))
    t))

(defun nngmail--plz-await-sentinel (proc)
  "PROC has exited but plz has not queued its response yet: make it.
Return non-nil once the response timer is queued (or the response has
been delivered).

Emacs sometimes never calls a process sentinel after the process was
waited on specifically (debbugs#50166); plz's own synchronous path
works around it by calling `plz--sentinel' itself with the status
\"workaround\", and the call is idempotent (a `:plz-result' guard).
We do the same for our asynchronous requests, after a brief wait on nil
that gives a merely late sentinel its normal chance.  Without this,
every response whose curl exited a moment before we looked was reported
as \"curl exited without a response\" and retried (seen as bursts of
network errors on a healthy connection)."
  (let ((deadline (+ (float-time) 0.2)))
    (while (and (not (nngmail--plz-callback-pending-p proc))
                (eq (process-get proc :nngmail-state) 'pending)
                (< (float-time) deadline))
      (accept-process-output nil 0.02)))
  (when (and (not (nngmail--plz-callback-pending-p proc))
             (eq (process-get proc :nngmail-state) 'pending)
             (fboundp 'plz--sentinel)
             (eq :plz-result (process-get proc :plz-result)))
    (nngmail--trace "plz sentinel never ran for %s; calling it" (process-name proc))
    (plz--sentinel proc "workaround"))
  (or (nngmail--plz-callback-pending-p proc)
      (not (eq (process-get proc :nngmail-state) 'pending))))

(defun nngmail--plz-wait-slice (proc)
  "Wait a moment for PROC: on its own output while it runs, else deliver.
Returns when PROC produced output, exited, or 0.5 s passed."
  (if (process-live-p proc)
      ;; Wakes the instant curl writes or exits.
      (accept-process-output proc 0.5)
    ;; Exited: the response is sitting on a timer.  Deliver it.
    (unless (nngmail--plz-deliver-now proc)
      ;; Sentinel not run yet (it runs from accept-process-output).
      (accept-process-output proc 0.05))))

(defvar nngmail--requests-in-flight nil
  "The plz processes currently waiting on Gmail, for the connections gauge.")

(defun nngmail--in-flight-count ()
  "Number of requests currently out, dropping finished processes."
  (setq nngmail--requests-in-flight
        (seq-filter (lambda (p) (and (process-live-p p)
                                     (eq (process-get p :nngmail-state) 'pending)))
                    nngmail--requests-in-flight))
  (length nngmail--requests-in-flight))

(defun nngmail--track-request (proc)
  "Remember PROC as an outstanding request; return PROC."
  (push proc nngmail--requests-in-flight)
  proc)

(defun nngmail--http-plz (method url headers body)
  "Perform METHOD on URL with HEADERS and BODY using plz.el.
Return (STATUS HEADERS-ALIST BODY-STRING)."
  (require 'plz)
  ;; `binary' means curl's --data-binary.  The default (--data) strips
  ;; CR and LF from a body read on stdin, which turns a multipart batch
  ;; into one line and makes Gmail answer 400 "0 items".
  ;; PATCH: released plz (0.9.x) does not know the method and sends the
  ;; body without a --data flag, so curl waits on stdin and fails with a
  ;; read error.  Google accepts POST plus X-HTTP-Method-Override for
  ;; every verb, so tunnel PATCH (and DELETE with a body) that way.
  (let* ((tunnel (member (upcase method) '("PATCH")))
         (method (if tunnel "POST" method))
         (headers (if tunnel (cons (cons "X-HTTP-Method-Override" "PATCH") headers) headers))
         (proc (plz (intern (downcase method)) url
                :headers headers :body body :body-type 'binary :as 'response
                :timeout nngmail-request-timeout :decode nil
                :then (lambda (r) (process-put (nngmail--plz-self) :nngmail-result r)
                        (process-put (nngmail--plz-self) :nngmail-state 'done))
                :else (lambda (e) (process-put (nngmail--plz-self) :nngmail-result e)
                        (process-put (nngmail--plz-self) :nngmail-state 'error)))))
    (process-put proc :nngmail-state (or (process-get proc :nngmail-state) 'pending))
    (setq nngmail--plz-last-proc proc)
    (nngmail--track-request proc)
    (nngmail--plz-unpack (nngmail--plz-wait proc))))


;;;; Quota pacing
;;
;; Gmail meters "quota units" per user per minute (15,000 by default).
;; messages.get costs 5, messages.list 5, history.list 2, batchModify 50,
;; send 100.  A batch of 50 metadata fetches is therefore 250 units, and an
;; unthrottled initial sync of a few thousand messages blows the budget in
;; well under a minute.  We keep a sliding one-minute window of spent units
;; and sleep before a request that would overshoot the configured budget.

(defcustom nngmail-quota-burst 1500
  "Token-bucket burst capacity used by the pacer, in quota units.
Gmail's per-user limit refills at `nngmail-quota-per-minute' per minute
but only lets a small amount accumulate: in practice a batch's worth or
so, which is why at the limit you see one or two parts of every batch
rejected rather than a whole minute of rejections followed by a clear
minute.  1,500 units is 300 message-header fetches.  If you see steady
per-part rejections even with pacing on, lower this; if you never do and
throughput is below the per-minute figure, raise it."
  :type 'integer)

(defcustom nngmail-quota-per-minute 15000
  "Gmail quota units nngmail allows itself per minute when pacing is on.
Google's default per-user limit is 15,000 (the project-wide limit is
far higher and is not the constraint).  Pacing is normally off: ordinary
reading, marking and syncing never come near the limit.  It is turned on
per run by `nngmail-backfill', which asks, or globally via
`nngmail-quota-pacing'."
  :type 'integer)

(defcustom nngmail-quota-pacing nil
  "Client-side pacing against `nngmail-quota-per-minute' outside backfills.
nil (default): never pace; Gmail's own rejections are still handled with
backoff and retry, which is all that everyday use needs.  `adaptive':
run unpaced until Gmail rejects a request for the per-minute quota, then
pace for the rest of the session.  `always': pace from the first request.
`nngmail-backfill' asks its own question and overrides this for the
duration of the backfill."
  :type '(choice (const :tag "Never (default)" nil) (const adaptive) (const always)))

(defcustom nngmail-backfill-pace-choices
  '(("adaptive: full speed until Gmail pushes back, then pace at the limit" . adaptive)
    ("15,000 units/min from the start (Gmail's per-user limit)" . 15000)
    ("12,000 units/min (leave headroom for other clients)" . 12000)
    ("7,500 units/min (half; gentle on a shared project)" . 7500)
    ("no pacing (react to Gmail's rejections only)" . none))
  "Choices offered by `nngmail-backfill' for pacing.
Each entry is (LABEL . VALUE) where VALUE is `adaptive', an integer
number of units per minute, or `none'.  A number typed at the prompt is
also accepted."
  :type '(alist :key-type string
                :value-type (choice (const adaptive) integer (const none))))

(defcustom nngmail-backfill-pace-default 'ask
  "What `nngmail-backfill' does about pacing.
`ask' (default) presents `nngmail-backfill-pace-choices', whose first
entry -- `adaptive' -- is preselected, so RET takes it.  Any other value
here is used silently without a prompt: `adaptive', an integer number
of units per minute, or `none' for no pacing."
  :type '(choice (const :tag "Ask (adaptive preselected)" ask)
                 (const adaptive) integer (const :tag "Never pace" none)))

(defun nngmail--backfill-pace-for (answer)
  "Normalise ANSWER from the pace prompt or `nngmail-backfill-pace-default'.
Non-interactive callers passing nil for PACE get `adaptive'."
  (pcase answer
    ('ask 'adaptive)
    (_ answer)))

(defvar nngmail--quota-enforced nil
  "Non-nil once Gmail has reported a per-minute quota error this session.")

(defun nngmail--parse-duration (s)
  "Parse S as a duration; return (N . UNIT) with UNIT in (d m y), or nil.
Accepts 30d, 6m, 2y, 3w (weeks become days), \"2 weeks\", \"1 year\",
\"18 months\" and the bare words week, month, year."
  (let ((s (downcase (string-trim s))))
    (cond
     ((string-match "\\`\\([0-9]+\\)\\s-*\\(d\\|days?\\)\\'" s)
      (cons (string-to-number (match-string 1 s)) 'd))
     ((string-match "\\`\\([0-9]+\\)\\s-*\\(w\\|weeks?\\)\\'" s)
      (cons (* 7 (string-to-number (match-string 1 s))) 'd))
     ((string-match "\\`\\([0-9]+\\)\\s-*\\(m\\|mo\\|months?\\)\\'" s)
      (cons (string-to-number (match-string 1 s)) 'm))
     ((string-match "\\`\\([0-9]+\\)\\s-*\\(y\\|yrs?\\|years?\\)\\'" s)
      (cons (string-to-number (match-string 1 s)) 'y))
     ((member s '("week" "a week")) (cons 7 'd))
     ((member s '("month" "a month")) (cons 1 'm))
     ((member s '("year" "a year")) (cons 1 'y))
     (t nil))))

(defun nngmail--parse-since (s)
  "Parse S as an absolute date; return YYYY/MM/DD (Gmail's after: form), or nil.
Accepts YYYY-MM-DD, YYYY/MM/DD or YYYY-MM.  A bare number is never a
date: it is always a message count."
  (let ((s (string-trim s)))
    (cond
     ((string-match "\\`\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{1,2\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'" s)
      (format "%s/%02d/%02d" (match-string 1 s)
              (string-to-number (match-string 2 s)) (string-to-number (match-string 3 s))))
     ((string-match "\\`\\([0-9]\\{4\\}\\)[-/]\\([0-9]\\{1,2\\}\\)\\'" s)
      (format "%s/%02d/01" (match-string 1 s) (string-to-number (match-string 2 s))))
     (t nil))))

(defcustom nngmail-backfill-choices
  '(("last 30 days"            . "30d")
    ("last 6 months"           . "6m")
    ("last year"               . "1y")
    ("last 2 years"            . "2y")
    ("1,000 more messages"     . "1000")
    ("10,000 more messages"    . "10000")
    ("everything (the whole mailbox)" . "all"))
  "Choices offered by `nngmail-backfill' for how much to fetch.
Each entry is (LABEL . ANSWER) where ANSWER is what you could also type:
a number of messages, a duration (30d, 6m, 2y, 3 weeks), a date
\(2024-01-01, 2024-06, 2023) or \"all\".  The prompt appends the
transport each choice will use (IMAP or REST) when it is shown, so do
not put that in the labels.  Free-form input is accepted alongside the
menu."
  :type '(alist :key-type string :value-type string))

(defvar nngmail--imap-unavailable-reason)

(defun nngmail--answer-queryless-p (answer)
  "Non-nil if how-much ANSWER carries no Gmail query (a count, or everything)."
  (let ((s (downcase (string-trim answer))))
    (or (string-empty-p s)
        (member s '("all" "unlimited" "everything"))
        (string-match-p "\\`[0-9,]+\\'" s))))

(defun nngmail--backfill-choices-for (transport)
  "`nngmail-backfill-choices' usable over TRANSPORT (`imap' or `rest').
IMAP cannot evaluate Gmail search syntax, so time windows and dates are
left out for it; REST offers everything."
  (if (eq transport 'imap)
      (seq-filter (lambda (c) (nngmail--answer-queryless-p (cdr c))) nngmail-backfill-choices)
    nngmail-backfill-choices))

(defun nngmail--read-how-much (&optional transport)
  "Ask how much to backfill; return (COUNT . QUERY-FRAGMENT).
COUNT is an integer or nil (no limit).  QUERY-FRAGMENT is a Gmail search
clause restricting by time (\"newer_than:6m\" or \"after:2024/01/01\") or
nil.  Offers `nngmail-backfill-choices' and also accepts a number of
messages, a duration, a date, or \"all\" typed directly.  Over
TRANSPORT `imap' only counts and \"all\" are offered or accepted."
  (let* ((choices (nngmail--backfill-choices-for (or transport 'rest)))
         (labels (mapcar #'car choices))
         (default (car labels))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity)
                                 (category . nngmail-how-much))
                    (complete-with-action action labels string pred))))
         (raw (string-trim
               (completing-read
                (format-prompt (if (eq transport 'imap)
                                   "How much to import over IMAP (or type a number)"
                                 "How much to backfill (or type e.g. 5000, 30d, 2y, 2024-01-01)")
                               default)
                table nil nil nil nil default)))
         ;; A menu label maps to its answer; anything else is taken literally.
         (s (or (cdr (assoc raw choices))
                (cdr (assoc raw nngmail-backfill-choices))
                raw))
         (dur (nngmail--parse-duration s))
         (since (and (not dur) (nngmail--parse-since s))))
    (when (and (eq transport 'imap) (or dur since))
      (user-error "nngmail: IMAP cannot filter by time; use the REST server for %S, or give a count" s))
    (cond
     ((or (string-empty-p s) (member (downcase s) '("all" "unlimited" "everything")))
      (cons nil nil))
     (dur
      ;; Gmail's newer_than: takes d, m or y.
      (cons nil (format "newer_than:%d%s" (car dur) (cdr dur))))
     (since
      (cons nil (format "after:%s" since)))
     ((string-match-p "\\`[0-9][0-9,]*\\'" s)
      (let ((n (string-to-number (replace-regexp-in-string "," "" s))))
        (if (> n 0) (cons n nil)
          (user-error "nngmail: Expected a positive number, got %S" s))))
     (t
      (user-error "nngmail: Expected a number of messages, a duration like 6m or 2y, a date, or \"all\", got %S" s)))))

(defcustom nngmail-backfill-pace-preselected 'adaptive
  "Which pace the backfill prompt preselects (what RET chooses).
A VALUE from `nngmail-backfill-pace-choices': `adaptive', an integer, or
`none'.  The prompt lists choices in a fixed order regardless of the
completion framework's sorting, with this one first."
  :type '(choice (const adaptive) integer (const none)))

(defun nngmail--read-pace ()
  "Ask how to pace a backfill; return `adaptive', an integer, or `none'.
The choice matching `nngmail-backfill-pace-preselected' is the default
and listed first; the rest follow in `nngmail-backfill-pace-choices'
order.  Sorting by the completion UI is suppressed so the menu reads the
same everywhere."
  (let* ((choices nngmail-backfill-pace-choices)
         (pre (or (car (rassoc nngmail-backfill-pace-preselected choices))
                  (caar choices)))
         (labels (cons pre (remove pre (mapcar #'car choices))))
         ;; Present in our order: a completion table with display-sort
         ;; and cycle-sort set to identity.
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity)
                                 (category . nngmail-pace))
                    (complete-with-action action labels string pred))))
         (answer (string-trim
                  (completing-read (format-prompt "Pace the backfill" pre)
                                   table nil nil nil nil pre))))
    (cond
     ((string-empty-p answer) (cdr (assoc pre choices)))
     ((assoc answer choices) (cdr (assoc answer choices)))
     ((string-match-p "\\`[0-9][0-9,._]*[kK]?\\'" answer)
      ;; Accept "9000", "9,000", "9k", "9K".
      (let* ((digits (replace-regexp-in-string "[,._]" "" answer))
             (k (string-match-p "[kK]\\'" digits))
             (n (string-to-number (replace-regexp-in-string "[kK]\\'" "" digits))))
        (if k (* 1000 n) n)))
     ((member (downcase answer) '("no" "none" "off" "nil")) 'none)
     ((member (downcase answer) '("adaptive" "auto")) 'adaptive)
     (t (user-error "nngmail: Pace must be a choice, a number of units/min, \"no\" or \"adaptive\"")))))

(defvar nngmail--quota-log nil
  "List of (TIME . UNITS) for requests in the last minute, newest first.")

(defvar nngmail--batch-progress-base nil
  "When non-nil, (DONE-SO-FAR . GRAND-TOTAL) of the enclosing fetch.
`nngmail--batch' adds its own count to DONE-SO-FAR and reports against
GRAND-TOTAL, so nested progress shows a single counter.")

(defvar nngmail--batch-size-current nil
  "Adaptive batch size.
Starts at `nngmail-batch-size', halves on rate limits
and creeps back up after clean batches, but never past
`nngmail--batch-size-ceiling'.")

(defvar nngmail--batch-size-ceiling nil
  "Largest batch size Gmail has accepted without a concurrency rejection.
Set to the size that failed minus one the first time \"Too many
concurrent requests\" is seen, and lowered whenever it is seen again.
Without it the size creeps back up after every clean batch, trips the
same limit, and the run alternates between a pause and a slow, rejected
batch.  Reset with `nngmail-reset-limits'.")

(defvar nngmail--batch-parallel-current nil
  "Adaptive number of concurrent batches.
Starts at `nngmail-batch-parallel'
and steps down when Gmail reports too many concurrent requests.")

(defvar nngmail--batch-part-failures nil
  "Alist (ID . STATUS) of parts that failed in the most recent batch parse.
Set by `nngmail--parse-batch' and consumed by `nngmail--batch'.")

(defun nngmail--quota-cost (method path body)
  "Estimate the quota units of METHOD on PATH with BODY."
  (cond
   ((string-prefix-p nngmail--batch-api path)
    ;; One metadata GET per part, 5 units each.
    (* 5 (max 1 (if (stringp body)
                    (cl-count-if (lambda (l) (string-prefix-p "Content-ID:" l))
                                 (split-string body "\r\n"))
                  1))))
   ((string-match-p "\\`messages/send" path) 100)
   ((string-match-p "\\`messages/batchModify" path) 50)
   ((string-match-p "\\`messages/[^/]+/modify" path) 5)
   ((string-match-p "\\`messages/[^/]+/trash" path) 5)
   ((string-match-p "\\`messages/[^/]+\\'" path) (if (equal method "DELETE") 10 5))
   ((string-match-p "\\`messages\\'" path) (if (equal method "POST") 25 5))
   ((string-match-p "\\`threads/" path) 10)
   ((string-match-p "\\`history" path) 2)
   ((string-match-p "\\`labels" path) (if (equal method "GET") 1 5))
   ((string-match-p "\\`profile" path) 1)
   (t 5)))

(defcustom nngmail-quota-policy 'ask
  "What to do when the Gmail per-minute quota is exhausted mid-sync.
`ask' shows the limit and our rate and offers: [t]hrottle (continue at
a reduced rate for the session), [w]ait (pause until this window
clears, ask again next time), [W] keep waiting (pause for every window
for the rest of the session without asking), or [a]bort (stop this
sync; everything indexed so far stays saved and the next sync resumes).
`throttle', `wait' and `abort' apply that answer without asking.  When
Emacs is non-interactive `ask' behaves as `wait'."
  :type '(choice (const ask) (const throttle) (const wait) (const abort)))

(defcustom nngmail-throttle-fraction 0.5
  "When throttling, spend at most this fraction of `nngmail-quota-per-minute'."
  :type 'float)

(defvar nngmail--throttled nil
  "Non-nil while the user has chosen to throttle for the rest of the session.")

(defvar nngmail--session-quota-policy nil
  "Answer remembered from the quota prompt for this session, or nil.
Set to `wait' by the [W] choice; `nngmail-quota-reset-policy' clears it.")

;;;###autoload
(defun nngmail-quota-help ()
  "Explain Gmail's per-user rate limit and how to request an increase."
  (interactive)
  (with-help-window "*nngmail quota*"
    (princ (format-message "\
Gmail enforces a per-user limit of 250 quota units per second averaged over
a minute: 15,000 units/min.  Fetching one message's headers costs 5 units,
so indexing tops out around 3,000 messages a minute.  The limiter is a
token bucket: at the limit a steady trickle of requests is rejected with
403 \"Units per minute per user\" while the rest go through.  nngmail
retries those and, when pacing is on, models the same bucket to sit just
under the limit -- the fastest legal rate at a given limit.

Requesting a higher limit:

  1. https://console.cloud.google.com/apis/api/gmail.googleapis.com/quotas
     -- choose the project whose number appears in the error message.
  2. Two rows look relevant.  \"Units per minute per user\" shows Unlimited
     and is a different, project-level metric: ignore it.  \"Previous
     quota: Units per minute per user\" shows 15,000, matching the enforced
     limit: that is the row to edit.
  3. Its dialog accepts up to 15,000 unless the project is linked to a
     billing account (it says so).  Linking one is free for Gmail API use
     but needs a payment method.  After linking, enter the value you want
     with a one-line justification and submit.  Per-user increases are
     reviewed; treat it as worth trying, not guaranteed.
  4. If granted: (setq nngmail-quota-per-minute NEW-LIMIT).

Current setting: %s units/min.  Pacing outside backfills: %s.
Session: %s

The background sync job (`nngmail-background') never asks: it paces at
the budget, throttles when Gmail rejects parts, and shows the wait in
the status line.  M-x nngmail-background-stop is its abort.  The
prompt above applies to synchronous work (backfill, imports).

The console's usage graph is a smoothed average and understates bursts.
\"Too many concurrent requests for user\" is a separate concurrency limit
that nngmail handles by reducing parallel batches.  OAuth clients still in
\"Testing\" status have lower daily quotas until published; that appears
as a daily error, not this one."
                           (nngmail--fmt-num nngmail-quota-per-minute)
                           (or nngmail-quota-pacing "off")
                           (if nngmail--quota-enforced
                               "Gmail has rejected a request for quota this session"
                             "no quota rejections this session")))))

;;;###autoload
(defun nngmail-quota-reset-policy ()
  "Forget the session-wide quota answer (throttle / keep waiting)."
  (interactive)
  (setq nngmail--session-quota-policy nil
        nngmail--throttled nil)
  (message "nngmail: quota policy reset; you will be asked again"))

(define-error 'nngmail-aborted "nngmail: sync aborted by user" 'nngmail-error)

(defun nngmail--quota-spent ()
  "Prune `nngmail--quota-log' to the last minute and return units spent."
  (let ((cutoff (- (float-time) 60)))
    (setq nngmail--quota-log (seq-take-while (lambda (e) (> (car e) cutoff))
                                             nngmail--quota-log))
    (apply #'+ (mapcar #'cdr nngmail--quota-log))))

(defun nngmail--quota-budget ()
  "Units per minute we currently allow ourselves."
  (if nngmail--throttled
      (floor (* nngmail-quota-per-minute nngmail-throttle-fraction))
    nngmail-quota-per-minute))

(defvar nngmail--quota-alert nil
  "Text shown inside the quota gauge while the quota prompt is up, or nil.")

(defun nngmail--quota-ask (spent budget)
  "Ask the user how to proceed when SPENT units exceed BUDGET.
Return `throttle', `wait', `keep-waiting' or `abort'.  The figures go
in the quota gauge (red while the prompt is up); the prompt itself is
one short line of keys, and `?' explains them."
  (let* ((elapsed (if nngmail--quota-log
                      (max 1 (- (float-time) (car (car (last nngmail--quota-log)))))
                    60))
         (rate (/ (* 60.0 spent) elapsed))
         (nngmail--quota-alert (format "%s/%s per min"
                                       (nngmail--fmt-compact (round rate))
                                       (nngmail--fmt-compact nngmail-quota-per-minute)))
         (prompt (format "Gmail quota reached: [t]hrottle %d%%  [w]ait  [W]ait always  [a]bort  [?] "
                         (round (* 100 nngmail-throttle-fraction))))
         (help (format (concat "Gmail allows %s units/min per user (budget %s); nngmail used %s in the last %s, about %s/min.\n"
                               "t  slow down to %d%% of the limit for this session\n"
                               "w  wait for the bucket to refill, ask again next time\n"
                               "W  keep waiting whenever needed, do not ask again this session\n"
                               "a  stop now; progress is saved and resumes on the next sync")
                       (nngmail--fmt-num nngmail-quota-per-minute) (nngmail--fmt-num budget)
                       (nngmail--fmt-num spent) (nngmail--fmt-duration elapsed)
                       (nngmail--fmt-compact (round rate))
                       (round (* 100 nngmail-throttle-fraction))))
         choice)
    (nngmail--draw-status t)
    (while (eq (setq choice (read-char-choice prompt '(?t ?w ?W ?a ??))) ??)
      (with-help-window "*nngmail quota*" (princ help)))
    (pcase choice (?t 'throttle) (?w 'wait) (?W 'keep-waiting) (_ 'abort))))

(defun nngmail--quota-wait (units)
  "Account for spending UNITS, pausing if the token bucket cannot cover them.
Gmail's per-user limit behaves like a token bucket: capacity
`nngmail-quota-per-minute', refilling continuously at capacity/60 per
second.  We mirror that exactly instead of using a fixed window, which
gives a smooth rate right at the limit rather than stall-and-burst.
Under `nngmail-quota-pacing' `adaptive', no pausing happens until Gmail
has rejected a request for the per-minute quota.  The quota prompt
\(`nngmail-quota-policy') is only shown when the wait would be long."
  (let ((pace (pcase nngmail-quota-pacing
                ('always t)
                ('adaptive nngmail--quota-enforced)
                (_ nil))))
    (when pace
     (nngmail--timed (format "quota wait for %d units" units)
      (let* ((rate (/ (nngmail--quota-budget) 60.0))
             (cap (max nngmail-quota-burst units))
             (asked nil))
        (while (let ((have (nngmail--bucket-level cap rate)))
                 (when (< have units)
                   (let* ((wait (/ (- units have) rate))
                          (action
                           (cond
                            ;; Short waits are just pacing; don't bother the user.
                            ((< wait 10) 'wait)
                            (asked 'wait)
                            (nngmail--session-quota-policy nngmail--session-quota-policy)
                            ((or noninteractive (eq nngmail-quota-policy 'wait)) 'wait)
                            ((eq nngmail-quota-policy 'throttle) 'throttle)
                            ((eq nngmail-quota-policy 'abort) 'abort)
                            (t (nngmail--quota-ask (round (- cap have)) (nngmail--quota-budget))))))
                     (setq asked t)
                     (pcase action
                       ('abort
                        (signal 'nngmail-aborted (list (round (- cap have)) nngmail-quota-per-minute)))
                       ('keep-waiting
                        (setq nngmail--session-quota-policy 'wait)
                        (nngmail--message 5 "will keep waiting for quota this session (M-x nngmail-quota-reset-policy to change)"))
                       ('throttle
                        (setq nngmail--throttled t
                              nngmail--session-quota-policy 'throttle)
                        (nngmail--message 5 "throttling to %s units/min"
                                          (nngmail--fmt-num (nngmail--quota-budget)))
                        (setq rate (/ (nngmail--quota-budget) 60.0))))
                     (nngmail--countdown
                      (max 0.2 (min wait 60))
                      (lambda (left)
                        (format "pacing %ds  %s/%s"
                                left (nngmail--fmt-compact (round have)) (nngmail--fmt-compact cap)))
                      "client-side quota pacing")
                     t)))))))
    (push (cons (float-time) units) nngmail--quota-log)))

(defun nngmail--bucket-level (cap rate)
  "Tokens available now in a bucket of CAP refilling at RATE per second.
Derived from `nngmail--quota-log': replay the last minute's spend against
a continuously refilling bucket."
  (let* ((now (float-time))
         (events (nreverse (seq-take-while (lambda (e) (> (car e) (- now 60)))
                                           nngmail--quota-log)))
         (level (float cap))
         (t0 (if events (car (car events)) now)))
    (dolist (e events)
      (setq level (min cap (+ level (* rate (- (car e) t0)))))
      (setq level (- level (cdr e))
            t0 (car e)))
    (min cap (+ level (* rate (- now t0))))))

(defun nngmail--bucket-drain ()
  "Record that Gmail just told us the bucket is empty.
Pushes a synthetic spend equal to whatever the model thinks is left, so
the next `nngmail--quota-wait' starts from zero."
  (let* ((rate (/ (nngmail--quota-budget) 60.0))
         (cap nngmail-quota-burst)
         (left (nngmail--bucket-level cap rate)))
    (when (> left 0)
      (push (cons (float-time) (ceiling left)) nngmail--quota-log))))

(defvar nngmail--pause-run nil
  "(REASON COUNT SECONDS SINCE) for the current run of same-reason pauses.")

(defun nngmail--log-pause (reason seconds label)
  "Log a pause of SECONDS for REASON, coalescing runs into one summary line.
LABEL names the request or phase being paused.
The first pause of a run is logged immediately; subsequent ones with the
same reason are accumulated and flushed (as one line with count and
total) when the reason changes or after 60 seconds.
SECONDS is the pause length."
  (pcase-let ((`(,r ,n ,secs ,since) nngmail--pause-run))
    (cond
     ((and r (equal r reason) (< (- (float-time) since) 60))
      (setq nngmail--pause-run (list r (1+ n) (+ secs seconds) since)))
     (t
      (when (and r (> n 1))
        (nngmail--log "PAUSE x%d totalling %s reason=%s (over %s)" n (nngmail--fmt-duration secs) r (nngmail--fmt-duration (- (float-time) since))))
      (nngmail--log "PAUSE %s reason=%s :: %s" (nngmail--fmt-duration seconds) reason label)
      (setq nngmail--pause-run (list reason 1 seconds (float-time)))))))

(defvar nngmail--pause-stats (make-hash-table :test #'equal)
  "Reason string -> (COUNT . TOTAL-SECONDS) for every pause this session.
Inspect with `nngmail-pause-report'.")

(defun nngmail--countdown (seconds label-fn &optional reason)
  "Sleep SECONDS, showing LABEL-FN's text in the status field once a second.
LABEL-FN is called with the whole seconds remaining and returns a short
string (it is shown in a ~28-column field next to the progress; keep it
terse).  REASON (a short string) is recorded in `nngmail--pause-stats' and
written to the log so every pause is attributable.  \\[keyboard-quit] works as usual.
When no sync progress is being shown, falls back to a plain echo-area
message.  In batch mode just sleeps once."
  (let* ((reason (or reason "unspecified"))
         (entry (gethash reason nngmail--pause-stats '(0 . 0))))
    (puthash reason (cons (1+ (car entry)) (+ (cdr entry) seconds)) nngmail--pause-stats)
    (nngmail--log-pause reason seconds (funcall label-fn (ceiling seconds))))
  (if noninteractive
      (sleep-for seconds)
    (let ((deadline (+ (float-time) seconds))
          (shown nil))
      (unwind-protect
          (while (< (float-time) deadline)
            (let ((left (ceiling (- deadline (float-time)))))
              (unless (eql left shown)
                (setq shown left)
                (if nngmail--progress
                    (nngmail--set-status (funcall label-fn left))
                  (let ((message-log-max nil))
                    (message "nngmail: %s" (funcall label-fn left)))
                  (redisplay)))
              (sleep-for (min 1 (max 0.05 (- deadline (float-time)))))))
        ;; Clear the status field when the pause ends (or is interrupted).
        (when nngmail--progress
          (nngmail--set-status nil))))))

;;;###autoload
(defun nngmail-profile-sync (server)
  "Run one sync of SERVER under the Emacs CPU profiler and show the report.
For when a sync is slow and the SLOW lines in *nngmail-log* do not say
where the time goes: the report shows which functions Emacs was in,
including redisplay and timers that run between our own steps."
  (interactive (list (nngmail--read-server)))
  (require 'profiler)
  (declare-function profiler-stop "profiler")
  (declare-function profiler-report "profiler")
  (profiler-start 'cpu)
  (unwind-protect
      (let ((account (nngmail--account server)))
        (nngmail--incremental-sync account t))
    (profiler-stop)
    (profiler-report)))

;;;###autoload
(defun nngmail-pause-report ()
  "Show why nngmail has paused this session, by reason, with counts and time."
  (interactive)
  (let (rows (total 0.0))
    (maphash (lambda (r e) (push (list r (car e) (cdr e)) rows)
               (setq total (+ total (cdr e))))
             nngmail--pause-stats)
    (if (null rows)
        (message "nngmail: no pauses this session")
      (with-current-buffer (get-buffer-create "*nngmail-pauses*")
        (erase-buffer)
        (insert (format "nngmail pauses this session: %s total\n\n" (nngmail--fmt-duration total)))
        (insert (format "%-40s %6s %10s\n" "reason" "count" "seconds"))
        (dolist (r (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b)))))
          (insert (format "%-40s %6d %10.1f\n" (nth 0 r) (nth 1 r) (nth 2 r))))
        (insert "\nEach pause is also in *nngmail-log* as a line starting with PAUSE.\n")
        (goto-char (point-min)))
      (pop-to-buffer "*nngmail-pauses*"))))

(defvar nngmail--request-started 0.0)

(defun nngmail--request (account method path &optional params body content-type raw)
  "Do METHOD on API PATH for ACCOUNT with query PARAMS and JSON BODY.
CONTENT-TYPE overrides application/json.  Return the parsed JSON body,
or the raw body string when RAW is non-nil.  Retries once on 401 after
refreshing the token, and backs off on 429/5xx and on 403 quota errors."
  (let* ((url (nngmail--url path params))
         (attempt 0)
         (max-attempts 6)
         result)
    (while (and (null result) (< attempt max-attempts))
      (cl-incf attempt)
      (nngmail--quota-wait (nngmail--quota-cost method path body))
      (let* ((access (nngmail--obtain-token account))
             (headers `(("Authorization" . ,(concat "Bearer " access))
                        ,@(when body
                            `(("Content-Type" . ,(or content-type "application/json"))))
                        ("Accept" . "application/json")))
             (body-string (cond ((null body) nil)
                                ((stringp body) body)
                                (t (nngmail--json-encode body)))))
        (nngmail--trace "%s %s%s" method url (if body " [body]" ""))
        (setq nngmail--request-started (float-time))
        (pcase-let ((`(,status ,hdrs ,text)
                     ;; Transport-level failures (no response, connection
                     ;; reset, DNS hiccup, curl exit) are reported as a
                     ;; pseudo status 0 so they go through the same
                     ;; backoff as a 5xx instead of aborting the sync.
                     (condition-case err
                         (if nngmail-use-plz
                             (nngmail--http-plz method url headers body-string)
                           (nngmail--http-url-el method url headers body-string))
                       (nngmail-http-error
                        (list 0 nil (format "%s" (cadr err))))
                       (error
                        (list 0 nil (error-message-string err))))))
          (let ((took (- (float-time) nngmail--request-started)))
            (when (>= took nngmail-slow-threshold)
              (nngmail--log "SLOW HTTP %s %s -> %s in %s (%s)"
                            method (substring path 0 (min 60 (length path))) status (nngmail--fmt-duration took)
                            (file-size-human-readable (length (or text ""))))))
          (cond
           ((and (>= status 200) (< status 300))
            (setq result (list (if raw text (nngmail--json-parse text)))))
           ((and (= status 0) (< attempt max-attempts))
            (nngmail--log "network error: %s" (truncate-string-to-width (or text "") 120))
            ;; Short, capped backoff: 2, 4, 8, 15, 15 s.  A transport
            ;; error is a broken or stalled connection; curl opens a new
            ;; one on retry, so waiting long buys nothing.  (An earlier
            ;; 5..120 s ladder left a label rename stuck for minutes.)
            (let ((wait (min 15 (* 2 (expt 2 (1- attempt))))))
              (nngmail--countdown wait (lambda (left)
                                         (format "net error, retry %ds" left))
                                  "network error")))
           ((and (= status 401) (< attempt max-attempts))
            (nngmail--message 6 "401 from Gmail, refreshing token")
            (nngmail--force-refresh account))
           ((and (memq status '(500 502 503 504)) (< attempt max-attempts))
            (let ((wait (* 1.5 (expt 2 attempt))))
              (nngmail--countdown wait (lambda (left)
                                         (format "HTTP %d, retry %ds" status left))
                                  (format "HTTP %d" status))))
           ((and (memq status '(429 403))
                 (< attempt max-attempts)
                 (or (= status 429)
                     (string-match-p "quota\\|rate ?limit\\|RateLimitExceeded"
                                     (or text ""))))
            ;; Gmail throttles for several distinct reasons and they need
            ;; different responses:
            ;;  - "Units per minute per user" / "Total Query Cost": the
            ;;    per-minute quota really is spent; go through the policy.
            ;;  - "rateLimitExceeded" / 429 with a Retry-After: a concurrency
            ;;    or burst limit, NOT the minute budget.  Just back off for
            ;;    the advertised time; do not charge the quota window.
            (let* ((reason (or (ignore-errors
                                 (alist-get 'reason
                                            (car (alist-get 'errors
                                                            (alist-get 'error (nngmail--json-parse text))))))
                               ""))
                   (msg (or (ignore-errors
                              (alist-get 'message (alist-get 'error (nngmail--json-parse text))))
                            text))
                   (retry-after (let ((h (cdr (assoc "retry-after" hdrs))))
                                  (and h (string-match-p "\\`[0-9]+\\'" h) (string-to-number h))))
                   (minute-quota (string-match-p "per minute\\|Total Query Cost\\|dailyLimit" msg)))
              (nngmail--log "throttled: status=%d reason=%s retry-after=%s msg=%s"
                            status reason retry-after msg)
              (cond
               (minute-quota
                (nngmail--message 5 "Gmail says: %s" msg)
                ;; From now on, pace client-side too.  Top the window up to
                ;; full (spread over the last 30s) rather than resetting it,
                ;; so the wait is proportional, not a flat minute.
                (setq nngmail--quota-enforced t)
                (nngmail--bucket-drain)
                ;; Re-check with the same units: this pauses (or signals abort).
                (nngmail--quota-wait (nngmail--quota-cost method path body)))
               (t
                ;; Burst/concurrency limit: honour Retry-After, else short
                ;; exponential backoff, and shrink batches for a while.
                (let ((wait (or retry-after (min 60 (* 2 (expt 2 attempt))))))
                  (setq nngmail--batch-size-current
                        (max 5 (/ (or nngmail--batch-size-current nngmail-batch-size) 2)))
                  (nngmail--countdown
                   wait (lambda (left)
                          (format "rate limit, retry %ds" left))
                   (format "Gmail rate limit %s" (if (string-empty-p reason) status reason))))))))
           ((= status 404)
            (setq result (list :not-found)))
           (t
            (let ((msg (or (ignore-errors
                             (alist-get 'message (alist-get 'error
                                                            (nngmail--json-parse text))))
                           text)))
              (setf (nngmail-account-last-error account) msg)
              (signal 'nngmail-http-error (list status msg url))))))))
    (unless result
      (signal 'nngmail-http-error (list "too many retries" url)))
    (let ((v (car result)))
      (if (eq v :not-found) nil v))))

(defvar nngmail--inhibit-prefetch nil
  "Non-nil disables background prefetching (tests bind this).")

(defun nngmail--prefetch-allowed-p ()
  "Non-nil when a background request may be started."
  (and (not nngmail--inhibit-prefetch)
       (not noninteractive)))

(defun nngmail--get-async (account path &optional params)
  "Start GET PATH with PARAMS in the background (plz only); return a handle.
`nngmail--get-async-result' collects it.  Falls back to a synchronous
fetch done at collection time when plz is not in use, so callers can
always treat the handle the same way.  No retries: a failed background
request is redone synchronously by the collector, which has them.
ACCOUNT is the nngmail account."
  (if (or (not nngmail-use-plz) (not (nngmail--prefetch-allowed-p)))
      (list 'sync account path params)
    (require 'plz)
    (nngmail--quota-wait (nngmail--quota-cost "GET" path nil))
    (let* ((url (nngmail--url path params))
           (access (nngmail--obtain-token account))
           (proc (plz 'get url
                   :headers `(("Authorization" . ,(concat "Bearer " access))
                              ("Accept" . "application/json"))
                   :as 'response :decode nil :timeout nngmail-request-timeout
                   :then (lambda (r) (process-put (nngmail--plz-self) :nngmail-result r)
                           (process-put (nngmail--plz-self) :nngmail-state 'done))
                   :else (lambda (e) (process-put (nngmail--plz-self) :nngmail-result e)
                           (process-put (nngmail--plz-self) :nngmail-state 'error)))))
      (process-put proc :nngmail-state (or (process-get proc :nngmail-state) 'pending))
      (nngmail--track-request proc)
      (nngmail--trace "GET %s [prefetch]" url)
      (list 'async account path params proc))))

(defun nngmail--get-async-result (handle)
  "Wait for HANDLE from `nngmail--get-async' and return the parsed JSON.
Anything but a clean 2xx is retried through `nngmail--get', which has
the full backoff and token-refresh logic."
  (pcase handle
    (`(sync ,account ,path ,params) (nngmail--get account path params))
    (`(async ,account ,path ,params ,proc)
     (let ((r (nngmail--plz-wait proc)))
       (if (and (recordp r) (plz-response-p r)
                (<= 200 (plz-response-status r) 299))
           (nngmail--json-parse (decode-coding-string (or (plz-response-body r) "") 'utf-8))
         (nngmail--trace "prefetch of %s failed (%s); refetching" path
                         (if (and (recordp r) (plz-response-p r)) (plz-response-status r) "no response"))
         (nngmail--get account path params))))))

(defun nngmail--get (account path &optional params)
  "GET PATH with PARAMS for ACCOUNT."
  (nngmail--request account "GET" path params))

(defun nngmail--post (account path &optional body params)
  "POST BODY to PATH with PARAMS for ACCOUNT."
  (nngmail--request account "POST" path params (or body "{}")))

(defun nngmail--paged (account path params key &optional limit phase)
  "GET all pages of PATH/PARAMS for ACCOUNT, concatenating list KEY.
Stop after LIMIT items when LIMIT is non-nil.  When PHASE is a string,
report progress with it after every page."
  (let (items token done (estimate nil))
    (while (not done)
      (let* ((resp (nngmail--get account path
                                 (append params
                                         (when token `(("pageToken" . ,token))))))
             (page (alist-get key resp)))
        (setq items (nconc items page)
              token (alist-get 'nextPageToken resp)
              estimate (or (alist-get 'resultSizeEstimate resp) estimate))
        (when phase
          (nngmail--progress account phase (length items)
                             (cond (limit (min limit (or estimate limit)))
                                   (estimate estimate))))
        (when (or (null token) (null page)
                  (and limit (>= (length items) limit)))
          (setq done t))))
    (if (and limit (> (length items) limit)) (seq-take items limit) items)))

;;;; Batch requests

(defun nngmail--batch-body (chunk)
  "Return (BOUNDARY . BODY) for a multipart/mixed batch of CHUNK requests."
  (let* ((boundary (format "nngmail_%s_%d" (format-time-string "%s") (random 100000)))
         (body (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (pcase-dolist (`(,id ,method ,path) chunk)
                   (let ((full (if (string-prefix-p "http" path)
                                   path
                                 (concat "/gmail/v1/users/me/" path))))
                     (insert "--" boundary "\r\n"
                             "Content-Type: application/http\r\n"
                             "Content-ID: <" (format "%s" id) ">\r\n\r\n"
                             method " " full " HTTP/1.1\r\n"
                             "Accept: application/json\r\n\r\n")))
                 (insert "--" boundary "--\r\n")
                 (buffer-string))))
    (cons boundary body)))

(defun nngmail--batch-one (account chunk)
  "Send CHUNK of (ID METHOD PATH) as one batch call; return parsed parts.
ACCOUNT is the nngmail account."
  (pcase-let ((`(,boundary . ,body) (nngmail--batch-body chunk)))
    (let ((text (nngmail--request account "POST" nngmail--batch-api nil body
                                  (concat "multipart/mixed; boundary=" boundary) t)))
      (nngmail--timed (format "parse batch of %d" (length chunk))
        (nngmail--parse-batch text)))))

(defun nngmail--batch-many-parallel (account chunks)
  "Send CHUNKS (list of request lists) concurrently with plz.
Return
a list of parsed-part lists in the same order.  Falls back to sequential
when plz is not in use or a chunk needs the full retry machinery.
ACCOUNT is the nngmail account."
  (require 'plz)
  (let* ((access (nngmail--obtain-token account))
         (headers `(("Authorization" . ,(concat "Bearer " access))
                    ("Accept" . "application/json")))
         (n (length chunks))
         (results (make-vector n nil))
         (states (make-vector n 'pending))
         (procs nil))
    (dotimes (i n)
      (pcase-let* ((chunk (nth i chunks))
                   (`(,boundary . ,body) (nngmail--batch-body chunk)))
        (nngmail--quota-wait (nngmail--quota-cost "POST" nngmail--batch-api body))
        (nngmail--trace "POST %s [batch %d parts, parallel %d/%d]"
                      nngmail--batch-api (length chunk) (1+ i) n)
        (let ((idx i))
          (push (nngmail--track-request
                 (plz 'post nngmail--batch-api
                  :headers (cons (cons "Content-Type"
                                       (concat "multipart/mixed; boundary=" boundary))
                                 headers)
                  :body body :body-type 'binary :as 'response :decode nil
                  :timeout nngmail-request-timeout
                  :then (lambda (resp)
                          (aset results idx resp)
                          (aset states idx 'done))
                  :else (lambda (err)
                          (aset results idx err)
                          (aset states idx 'error))))
                procs))))
    ;; Wait for all of them, showing a slow wait in the status line.
    ;; `accept-process-output nil' so plz's zero-delay callback timers
    ;; run; see `nngmail--plz-wait'.  A chunk whose curl exited with no
    ;; callback queued is marked failed so the loop cannot spin.
    (let ((started (float-time)) (shown nil))
      (unwind-protect
          (while (seq-some (lambda (st) (eq st 'pending)) states)
            ;; Deliver any finished response now; a dead process with
            ;; nothing queued means plz lost it.
            (dotimes (i n)
              (let ((proc (nth (- n 1 i) procs)))
                (when (and (eq (aref states i) 'pending) (processp proc)
                           (not (process-live-p proc)))
                  (unless (nngmail--plz-deliver-now proc)
                    (unless (or (nngmail--plz-await-sentinel proc)
                                (not (eq (aref states i) 'pending)))
                      (aset results i (format "curl exited (%s) without a response"
                                              (process-status proc)))
                      (aset states i 'error))))))
            (when (seq-some (lambda (st) (eq st 'pending)) states)
              ;; Wait on one live process: wakes on any output or exit.
              (if-let* ((live (seq-find (lambda (p) (and (processp p) (process-live-p p))) procs)))
                  (accept-process-output live 0.5)
                (accept-process-output nil 0.05)))
            (let ((sec (floor (- (float-time) started))))
              (unless (eql sec shown)
                (setq shown sec)
                (nngmail--wait-status started))))
        (when shown (nngmail--set-status nil))))
    ;; Turn each into parsed parts, routing failures through the normal
    ;; sequential path so that 401/429/403/5xx handling applies.  Part
    ;; failures from all chunks are accumulated so the caller sees them all.
    (let* ((all-failures nil)
           (parsed
            (cl-loop for i from 0 below n
                    collect (let ((r (aref results i))
                                  (chunk (nth i chunks)))
                              (prog1
                                  (cond
                                   ((and (eq (aref states i) 'done)
                                         (recordp r)
                                         (let ((st (plz-response-status r))) (and (>= st 200) (< st 300))))
                                    (nngmail--parse-batch
                                     (decode-coding-string (or (plz-response-body r) "") 'utf-8)))
                                   (t
                                    (nngmail--log "parallel batch %d failed (%S); retrying sequentially"
                                                  (1+ i)
                                                  (cond ((and (recordp r) (plz-response-p r)) (plz-response-status r))
                                                        ((and (recordp r) (plz-error-p r)) (plz-error-message r))
                                                        (t r)))
                                    (nngmail--batch-one account chunk)))
                                (setq all-failures (append all-failures nngmail--batch-part-failures)))))))
      (setq nngmail--batch-part-failures all-failures)
      parsed)))

(defvar nngmail--batch-in-flight nil
  "Non-nil while `nngmail--batch' is waiting on Gmail.
Emacs is single-threaded, so the only way another nngmail entry point
can run while a batch is out is from a timer or process filter that
fires *inside* the batch's own `accept-process-output': the Gnus
demon, a timed `g', the idle mark flush.  Such a caller must not wait
for the slot -- the holder is below it on the stack and cannot
continue until it returns -- and must not send either, since Gmail
counts the parts of both batches against one concurrency limit.  It
gives up with `nngmail-busy' and the sync it belongs to is retried on
the next scan.")

(define-error 'nngmail-busy "nngmail: a request is already in flight" 'nngmail-error)

(defun nngmail--claim-batch-slot (phase)
  "Take the in-flight slot for PHASE, or signal `nngmail-busy'."
  (when nngmail--batch-in-flight
    (nngmail--trace "batch %S refused: %S already in flight" phase nngmail--batch-in-flight)
    (signal 'nngmail-busy (list nngmail--batch-in-flight)))
  (setq nngmail--batch-in-flight (or phase "batch")))

(defun nngmail--batch (account requests &optional phase)
  "Send REQUESTS as Gmail batch calls for ACCOUNT.
Only one batch is in flight at a time (see `nngmail--batch-in-flight').
Each request is (ID METHOD PATH) where PATH is relative to the API root
or a full URL.  Return an alist (ID . PARSED-JSON-OR-NIL).  When PHASE
is a string, report progress with it after every round.

With plz, up to `nngmail-batch-parallel' batches are sent concurrently.

Gmail applies its limits *inside* a batch: the outer call returns 200
while individual parts come back 403/429.  Two cases are told apart by
the part's message.  A per-minute quota rejection means the per-user
budget really is spent: the client-side pacer is switched on (see
`nngmail-quota-pacing') and charged for the whole window, and the parts
are retried once it says there is room.  A burst/concurrency rejection
\\(\"rateLimitExceeded\" without a per-minute metric) gets a short pause
and a smaller batch.  Either way parts are retried, never dropped."
  (nngmail--claim-batch-slot phase)
  (unwind-protect
      (nngmail--batch-1 account requests phase)
    (setq nngmail--batch-in-flight nil)))

(defun nngmail--batch-1 (account requests phase)
  "The body of `nngmail--batch'.
Send REQUESTS in PHASE with the in-flight slot held.
ACCOUNT is the nngmail account."
  (let ((results nil)
        (total (length requests))
        (done 0)
        (pending requests)
        (size (or nngmail--batch-size-current nngmail-batch-size))
        (parallel (progn
                    (unless nngmail--batch-parallel-current
                      (setq nngmail--batch-parallel-current nngmail-batch-parallel))
                    (if (and nngmail-use-plz (> nngmail--batch-parallel-current 1))
                        nngmail--batch-parallel-current
                      1)))
        (rounds 0)
        (stalled 0))
    (while pending
      (when phase
        (if nngmail--batch-progress-base
            (nngmail--progress account phase
                               (+ (car nngmail--batch-progress-base) done)
                               (cdr nngmail--batch-progress-base))
          (nngmail--progress account phase done total)))
      (let* ((chunks (let (cs (rest pending))
                       (dotimes (_ parallel)
                         (when rest
                           (push (seq-take rest size) cs)
                           (setq rest (nthcdr size rest))))
                       (nreverse cs)))
             (taken (apply #'+ (mapcar #'length chunks)))
             (all-failures nil)
             (parts-lists
              (if (= (length chunks) 1)
                  (prog1 (list (nngmail--batch-one account (car chunks)))
                    (setq all-failures nngmail--batch-part-failures))
                (prog1 (nngmail--batch-many-parallel account chunks)
                  ;; `nngmail--batch-many-parallel' accumulates failures
                  ;; across its chunks into `nngmail--batch-part-failures'.
                  (setq all-failures nngmail--batch-part-failures))))
             (limited (seq-filter (lambda (f) (memq (nth 1 f) '(429 403 500 502 503 504))) all-failures))
             (limited-ids (mapcar #'car limited))
             (quota-p (nngmail--batch-failures-quota-p limited)))
        (dolist (parts parts-lists)
          (dolist (p parts)
            (unless (member (car p) limited-ids)
              (push p results))))
        (let ((sent (seq-take pending taken)))
          (setq pending (nconc (nthcdr taken pending)
                               (seq-filter (lambda (r) (member (car r) limited-ids)) sent))))
        (setq done (+ done (- taken (length limited-ids))))
        (when limited-ids
          (cl-incf rounds)
          ;; Give up only when we are making no headway at all: a whole
          ;; round in which *every* part bounced, several times running.
          ;; A steady trickle of one bounced part per batch (Gmail's
          ;; token bucket saying "one over") is normal at the limit and
          ;; must not count against us.
          (if (= (length limited-ids) taken)
              (cl-incf stalled)
            (setq stalled 0))
          (when (> stalled 6)
            (signal 'nngmail-http-error
                    (list 429 (format "%d batch parts still rate-limited after %d stalled rounds"
                                      (length limited-ids) stalled)
                          nngmail--batch-api)))
          (cond
           (quota-p
            ;; Per-user budget exhausted.  Gmail meters this as a token
            ;; bucket, not a fixed minute window: a small number of parts
            ;; over the line bounce while the rest go through, and the
            ;; bucket refills at roughly the limit / 60 per second.  So:
            ;;  - mark enforcement so the pacer meters from here on;
            ;;  - don't retry the bounced parts immediately -- wait long
            ;;    enough for the bucket to refill their cost (5 units per
            ;;    part, at limit/60 units per second), with a floor so we
            ;;    don't hammer;
            ;;  - and reduce batch size so each batch is a smaller bite,
            ;;    which lets the bucket absorb it without spilling.
            (unless nngmail--quota-enforced
              (nngmail--message 5 "Gmail per-user quota reached; pacing at %s units/min (raise it in the Cloud console: see README, or M-x nngmail-quota-help)"
                                (nngmail--fmt-num nngmail-quota-per-minute)))
            (setq nngmail--quota-enforced t
                  parallel 1)
            ;; Tell the pacer the bucket is empty right now.  (Message text
            ;; above still says "raise it in the Cloud console"; see
            ;; `nngmail-quota-help' for the actual situation.)
            (nngmail--bucket-drain)
            ;; Smaller bites while at the limit.
            (setq size (max 10 (/ size 2))
                  nngmail--batch-size-current size)
            (let* ((per-sec (/ nngmail-quota-per-minute 60.0))
                   (need (* 5 (length limited-ids)))
                   (wait (max 3 (min 30 (ceiling (/ need per-sec))))))
              (nngmail--countdown
               wait (lambda (left)
                      (format "%d bounced, retry %ds" (length limited-ids) left))
               "at Gmail per-user limit")))
           ((seq-some (lambda (f) (string-match-p "concurrent" (or (nth 3 f) ""))) limited)
            ;; "Too many concurrent requests for user".  Gmail executes the
            ;; parts of a batch concurrently and counts them against this
            ;; limit, so it fires even with a single batch in flight.  Two
            ;; levers: no parallel batches, and smaller batches (Google's
            ;; own guidance is ~50 and lower when this appears).  Remember
            ;; both for the session; the size creeps back up after clean
            ;; batches.  Retry the bounced parts after a short pause.
            (setq parallel 1)
            (when (> nngmail--batch-parallel-current 1)
              (setq nngmail--batch-parallel-current 1))
            (setq nngmail--batch-size-ceiling
                  (max 10 (min (or nngmail--batch-size-ceiling size) (1- size))))
            (when (> size 10)
              (setq size (max 10 (/ (* size 2) 3))
                    nngmail--batch-size-current size))
            (nngmail--log "concurrency limit hit; batch size now %d (ceiling %d), sequential"
                          size nngmail--batch-size-ceiling)
            (nngmail--countdown
             2 (lambda (left) (format "concurrency, retry %ds" left))
             "concurrency limit"))
           (t
            ;; Other burst limit (or a stray 503): shrink the batch and
            ;; back off briefly.
            (setq size (max 5 (/ size 2))
                  nngmail--batch-size-current size
                  parallel 1)
            (nngmail--countdown
             (min 30 (* 5 rounds))
             (lambda (left)
               (format "%d/%d limited, retry %ds" (length limited-ids) taken left))
             "rate-limited batch parts"))))))
    ;; Grow back towards the configured size after a clean call, but
    ;; gently: one step per whole `nngmail--batch' invocation, not per
    ;; chunk, so a limit that bit us stays respected for a while.
    (let ((cap (min nngmail-batch-size (or nngmail--batch-size-ceiling nngmail-batch-size))))
      (when (and (null nngmail--batch-part-failures)
                 nngmail--batch-size-current
                 (< nngmail--batch-size-current cap))
        (setq nngmail--batch-size-current
              (min cap (+ nngmail--batch-size-current 5)))))
    (when phase
      (if nngmail--batch-progress-base
          (nngmail--progress account phase
                             (+ (car nngmail--batch-progress-base) done)
                             (cdr nngmail--batch-progress-base) t)
        (nngmail--progress account phase done total t)))
    (nreverse results)))

(defun nngmail--parse-batch (text)
  "Parse a multipart/mixed batch response TEXT into ((ID . JSON) ...).
Parts with a non-2xx status yield nil and are recorded in
`nngmail--batch-part-failures' as (ID STATUS REASON MESSAGE)."
  (let (results failures)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      ;; Each part: headers (with Content-ID: <response-ID>), blank, HTTP status
      ;; line, headers, blank, body, then next boundary.
      (while (re-search-forward "^Content-ID: *<\\(?:response-\\)?\\([^>]+\\)>" nil t)
        (let ((id (match-string 1))
              status json)
          (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
            (setq status (string-to-number (match-string 1)))
            (when (re-search-forward "^\r?$" nil t)
              (forward-line 1)
              (let* ((start (point))
                     (end (if (re-search-forward "^--" nil t)
                              (match-beginning 0)
                            (point-max)))
                     (raw (string-trim (buffer-substring-no-properties start end))))
                (goto-char end)
                (cond
                 ((and (>= status 200) (< status 300))
                  (setq json (ignore-errors (nngmail--json-parse raw))))
                 ((>= status 400)
                  (let* ((err (ignore-errors (alist-get 'error (nngmail--json-parse raw))))
                         (reason (or (ignore-errors (alist-get 'reason (car (alist-get 'errors err)))) ""))
                         (msg (or (alist-get 'message err) (truncate-string-to-width raw 120))))
                    (push (list id status reason msg) failures)))))))
          (push (cons id json) results))))
    (setq nngmail--batch-part-failures (nreverse failures))
    ;; One summary line per batch instead of one per part -- and the
    ;; expected trickle of quota/concurrency bounces at the limit only at
    ;; trace level, since it is the steady state, not an event.
    (when failures
      (let ((by (make-hash-table :test #'equal))
            (routine (cl-every (lambda (f)
                                 (and (memq (nth 1 f) '(403 429))
                                      (string-match-p "per minute\\|concurrent\\|rateLimit"
                                                      (or (nth 3 f) (nth 2 f) ""))))
                               failures)))
        (dolist (f failures)
          (cl-incf (gethash (format "%d %s" (nth 1 f) (nth 2 f)) by 0)))
        (funcall (if routine #'nngmail--trace #'nngmail--log)
                 "batch: %d/%d parts failed: %s%s"
                 (length failures) (length results)
                 (mapconcat (lambda (k) (format "%s x%d" k (gethash k by)))
                            (hash-table-keys by) ", ")
                 (let ((m (nth 3 (car failures))))
                   (if m (format " -- %s" (truncate-string-to-width m 140)) "")))))
    (nreverse results)))

(defun nngmail--batch-failures-quota-p (failures)
  "Non-nil if any of FAILURES is a per-minute quota rejection (not a burst limit)."
  (seq-some (lambda (f)
              (and (memq (nth 1 f) '(403 429))
                   (string-match-p "per minute\\|Total Query Cost\\|dailyLimit" (or (nth 3 f) ""))))
            failures))

;;;; Persistence
;;
;; Everything nngmail knows about an account lives in one SQLite file,
;; NAME.sqlite under `nngmail-directory':
;;
;;   meta      key/value: history id, next article number, label names,
;;             the checkpoints of an interrupted fetch or import.
;;   messages  one row per indexed message: article number, Gmail id,
;;             labels, thread id, and the NOV header as printed Lisp.
;;   articles  raw RFC 822 text of messages that have been read, as
;;             BLOBs, with size and last-read time for eviction.
;;
;; In memory the account keeps only what every group operation needs:
;; id<->number, number->labels and the per-group article lists (about
;; 150 bytes a message).  Headers are read from the database when a
;; summary asks for them and kept in a small LRU; at 2.5 million
;; messages that is the difference between 400 MB and 2 GB resident.
;;
;; Writes are transactional.  A sync or import commits after every
;; chunk, so a crash loses at most the chunk in flight and there is no
;; periodic whole-state rewrite.  `nngmail--save' only writes meta.
;;
;; SQLite is required: Emacs 29+ built with --with-sqlite3 (every
;; official and Homebrew build).  Without it `nngmail-open-server'
;; fails with a message saying so.  Older nngmail versions kept the
;; same data in NAME.eld; that file is imported once and renamed.

(defcustom nngmail-article-cache-max 512
  "Upper bound, in megabytes, on cached raw articles per account.
When the articles table grows past this the least recently read ones
are deleted until it fits.  nil means no bound."
  :type '(choice (const :tag "Unbounded" nil) integer))

(defcustom nngmail-header-cache-size 5000
  "How many NOV headers to keep in memory per account.
A summary buffer's headers are read from the database in one query and
kept here, most recently used first; a threaded summary of a big group
is a few thousand."
  :type 'integer)

(defconst nngmail--db-schema
  '("CREATE TABLE IF NOT EXISTS meta (key TEXT PRIMARY KEY, value TEXT)"
    "CREATE TABLE IF NOT EXISTS messages (
       num INTEGER PRIMARY KEY,
       id TEXT NOT NULL UNIQUE,
       labels TEXT NOT NULL DEFAULT '',
       thread TEXT,
       header TEXT)"
    "CREATE TABLE IF NOT EXISTS articles (
       num INTEGER PRIMARY KEY,
       raw BLOB NOT NULL,
       size INTEGER NOT NULL,
       atime REAL NOT NULL)"
    "CREATE INDEX IF NOT EXISTS articles_atime ON articles (atime)"
    ;; Per-group article numbering: LOCAL is what Gnus sees for GRP,
    ;; NUM the server-wide number.  Assigned once, never reused.
    "CREATE TABLE IF NOT EXISTS group_articles (
       grp TEXT NOT NULL,
       local INTEGER NOT NULL,
       num INTEGER NOT NULL,
       PRIMARY KEY (grp, local))"
    "CREATE INDEX IF NOT EXISTS group_articles_num ON group_articles (grp, num)")
  "Statements that bring a database to the current schema.  Idempotent.")

(defun nngmail--db-file (account)
  "The SQLite file of ACCOUNT."
  (expand-file-name (concat (nngmail-account-name account) ".sqlite") nngmail-directory))

(defun nngmail--legacy-state-file (account)
  "The pre-SQLite state file of ACCOUNT (imported once, then renamed)."
  (expand-file-name (concat (nngmail-account-name account) ".eld") nngmail-directory))

(defun nngmail--require-sqlite ()
  "Signal a clear error unless this Emacs can use SQLite."
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (signal 'nngmail-error
            (list (concat "nngmail needs SQLite, and this Emacs has none: "
                          "(sqlite-available-p) is nil.  Emacs 29 or later built with "
                          "--with-sqlite3 (the official and Homebrew builds are) is required.")))))

(defun nngmail--db (account)
  "The open database of ACCOUNT, opening it if needed."
  (or (let ((db (nngmail-account-db account)))
        (and db (sqlitep db) db))
      (nngmail--db-open account)))

(defun nngmail--db-open (account)
  "Open (creating if needed) ACCOUNT's database; return it."
  (nngmail--require-sqlite)
  (make-directory nngmail-directory t)
  (let ((db (sqlite-open (nngmail--db-file account))))
    (sqlite-execute db "PRAGMA journal_mode = WAL")
    (sqlite-execute db "PRAGMA synchronous = NORMAL")
    (dolist (stmt nngmail--db-schema) (sqlite-execute db stmt))
    (setf (nngmail-account-db account) db)
    db))

(defun nngmail--db-close (account)
  "Close ACCOUNT's database, writing pending meta first."
  (when-let* ((db (nngmail-account-db account)))
    (when (sqlitep db)
      (ignore-errors (nngmail--save account))
      (ignore-errors (sqlite-close db)))
    (setf (nngmail-account-db account) nil)))

(defmacro nngmail--with-transaction (account &rest body)
  "Run BODY inside one transaction on ACCOUNT's database.
Nested uses join the outer transaction."
  (declare (indent 1))
  (let ((db (make-symbol "db")) (outer (make-symbol "outer")))
    `(let* ((,db (nngmail--db ,account))
            (,outer nngmail--in-transaction)
            (nngmail--in-transaction t))
       (if ,outer
           (progn ,@body)
         (sqlite-transaction ,db)
         (condition-case err
             (prog1 (progn ,@body)
               (sqlite-commit ,db))
           ((error quit)
            (ignore-errors (sqlite-rollback ,db))
            (signal (car err) (cdr err))))))))

(defvar nngmail--in-transaction nil)

;; meta

(defun nngmail--meta-get (account key)
  "Read Lisp value KEY from ACCOUNT's meta table, or nil."
  (when-let* ((row (car (sqlite-select (nngmail--db account)
                                       "SELECT value FROM meta WHERE key = ?" (list key)))))
    (car (read-from-string (car row)))))

(defun nngmail--meta-put (account key value)
  "Store Lisp VALUE under KEY in ACCOUNT's meta table."
  (let ((print-length nil) (print-level nil))
    (sqlite-execute (nngmail--db account)
                    "INSERT OR REPLACE INTO meta (key, value) VALUES (?, ?)"
                    (list key (prin1-to-string value)))))

(defun nngmail--save (account &optional force)
  "Write ACCOUNT's meta (history id, checkpoints, label names) if dirty or FORCE.
Message rows are written as they change; this is cheap and safe to call
often."
  (when (and (or force (nngmail-account-dirty account))
             (nngmail-account-db account))
    (nngmail--with-transaction account
      (nngmail--meta-put account "history-id" (nngmail-account-history-id account))
      (nngmail--meta-put account "pending" (nngmail-account-pending account))
      (nngmail--meta-put account "pending-history-id" (nngmail-account-pending-history-id account))
      (nngmail--meta-put account "backfill" (nngmail-account-backfill account))
      (nngmail--meta-put account "mailbox-total" (nngmail-account-mailbox-total account))
      (nngmail--meta-put account "next-article" (nngmail-account-next-article account))
      (nngmail--meta-put account "label-names" (nngmail-account-label-names account))
      (nngmail--save-group-counters account))
    (setf (nngmail-account-dirty account) nil)
    (nngmail--trace "meta saved for %s" (nngmail-account-name account))))

(defun nngmail--checkpoint (account)
  "Persist ACCOUNT's counters.  Kept for callers; rows are already committed."
  (nngmail--save account))

;; message rows

(defun nngmail--header-string (header)
  "HEADER (a `mail-header' vector) as a string for the database."
  (let ((print-length nil) (print-level nil) (print-escape-newlines nil))
    (prin1-to-string header)))

(defun nngmail--db-put-message (account num id labels header)
  "Insert or update the row for article NUM in ACCOUNT's database.
HEADER may be nil (a lazily filed arrival); an existing header is kept
then."
  (sqlite-execute (nngmail--db account)
                  (if header
                      "INSERT INTO messages (num, id, labels, thread, header) VALUES (?, ?, ?, ?, ?)
                       ON CONFLICT(num) DO UPDATE SET labels = excluded.labels, thread = excluded.thread, header = excluded.header"
                    "INSERT INTO messages (num, id, labels, thread, header) VALUES (?, ?, ?, ?, NULL)
                       ON CONFLICT(num) DO UPDATE SET labels = excluded.labels")
                  (append (list num id (mapconcat #'identity labels ",")
                                (and header (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra header)))))
                          (and header (list (nngmail--header-string header))))))

(defun nngmail--db-put-labels (account num labels)
  "Update only the labels column of article NUM.
ACCOUNT is the nngmail account."
  (sqlite-execute (nngmail--db account)
                  "UPDATE messages SET labels = ? WHERE num = ?"
                  (list (mapconcat #'identity labels ",") num)))

(defun nngmail--db-delete-message (account num)
  "Remove article NUM and its cached raw text from ACCOUNT's database."
  (let ((db (nngmail--db account)))
    (sqlite-execute db "DELETE FROM messages WHERE num = ?" (list num))
    (sqlite-execute db "DELETE FROM articles WHERE num = ?" (list num))))

(defun nngmail--db-headers (account nums)
  "Read the NOV headers of articles NUMS from ACCOUNT's database.
Return an alist (NUM . HEADER) for the ones that have one.  The
X-Gmail-Labels extra is taken from the labels column, the source of
truth, so a header stored before a label change is still right."
  (let (out)
    ;; SQLite's parameter limit is generous (32k) but keep statements sane.
    (dolist (batch (seq-partition nums 500))
      (dolist (row (sqlite-select
                    (nngmail--db account)
                    (format "SELECT num, header, labels FROM messages WHERE header IS NOT NULL AND num IN (%s)"
                            (mapconcat (lambda (_) "?") batch ","))
                    batch))
        (let ((h (car (read-from-string (cadr row)))))
          (setf (alist-get 'X-Gmail-Labels (mail-header-extra h)) (nth 2 row))
          (push (cons (car row) h) out))))
    out))

(defun nngmail--db-count (account)
  "Number of indexed messages in ACCOUNT's database."
  (car (car (sqlite-select (nngmail--db account) "SELECT count(*) FROM messages"))))

;; header cache

(defun nngmail--header-cache-put (account num header)
  "Remember HEADER for NUM in ACCOUNT's bounded in-memory cache."
  (let ((cache (nngmail-account-headers account)))
    (unless (gethash num cache)
      (push num (nngmail-account-header-lru account))
      (when (> (hash-table-count cache) nngmail-header-cache-size)
        ;; Drop the oldest quarter in one go rather than one per insert.
        (let* ((lru (nngmail-account-header-lru account))
               (keep (max 1 (- (length lru) (/ nngmail-header-cache-size 4))))
               (tail (nthcdr keep lru)))
          (dolist (old tail) (remhash old cache))
          (setf (nngmail-account-header-lru account) (seq-take lru keep)))))
    (puthash num header cache)))

(defun nngmail--header (account num)
  "The NOV header of article NUM in ACCOUNT, from cache or database, or nil."
  (or (gethash num (nngmail-account-headers account))
      (when-let* ((h (cdr (assq num (nngmail--db-headers account (list num))))))
        (nngmail--header-cache-put account num h)
        h)))

(defun nngmail--headers (account nums)
  "NOV headers for NUMS in ACCOUNT as an alist (NUM . HEADER), cache-first.
One database query for whatever the cache lacks."
  (let ((cache (nngmail-account-headers account))
        (missing nil) (out nil))
    (dolist (n nums)
      (if-let* ((h (gethash n cache)))
          (push (cons n h) out)
        (push n missing)))
    (when missing
      (dolist (pair (nngmail--db-headers account missing))
        (nngmail--header-cache-put account (car pair) (cdr pair))
        (push pair out)))
    out))

(defun nngmail--has-header-p (account num)
  "Non-nil if article NUM has a stored header (fetched, not just filed).
ACCOUNT is the nngmail account."
  (or (gethash num (nngmail-account-headers account))
      (car (car (sqlite-select (nngmail--db account)
                               "SELECT 1 FROM messages WHERE num = ? AND header IS NOT NULL"
                               (list num))))))

(defun nngmail--find-header (account pred)
  "Return the first (NUM . HEADER) in ACCOUNT for which PRED is non-nil.
Walks the database in article order; for diagnostics and tests."
  (catch 'found
    (let ((num 0) rows)
      (while (setq rows (sqlite-select (nngmail--db account)
                                       "SELECT num, header FROM messages WHERE num > ? AND header IS NOT NULL ORDER BY num LIMIT 2000"
                                       (list num)))
        (dolist (row rows)
          (setq num (car row))
          (let ((h (car (read-from-string (cadr row)))))
            (when (funcall pred num h) (throw 'found (cons num h)))))))
    nil))

;; raw articles

(defun nngmail--db-get-article (account num)
  "Cached raw text of article NUM in ACCOUNT, or nil.  Touches its atime."
  (when-let* ((row (car (sqlite-select (nngmail--db account)
                                       "SELECT raw FROM articles WHERE num = ?" (list num)))))
    (sqlite-execute (nngmail--db account) "UPDATE articles SET atime = ? WHERE num = ?"
                    (list (float-time) num))
    (car row)))

(defun nngmail--db-put-article (account num raw)
  "Cache RAW (unibyte RFC 822 text) as article NUM; evict if over the bound.
ACCOUNT is the nngmail account."
  (let ((blob (propertize raw 'coding-system 'binary)))
    (nngmail--with-transaction account
      (sqlite-execute (nngmail--db account)
                      "INSERT OR REPLACE INTO articles (num, raw, size, atime) VALUES (?, ?, ?, ?)"
                      (list num blob (length raw) (float-time)))
      (nngmail--db-evict-articles account))))

(defun nngmail--db-evict-articles (account)
  "Delete least recently read articles until the cache fits its bound.
The bound is `nngmail-article-cache-max'.
ACCOUNT is the nngmail account."
  (when nngmail-article-cache-max
    (let* ((db (nngmail--db account))
           (max (* nngmail-article-cache-max 1024 1024))
           (total (or (car (car (sqlite-select db "SELECT sum(size) FROM articles"))) 0)))
      (when (> total max)
        ;; Walk the oldest rows and delete just enough of them.
        (let ((rows (sqlite-select db "SELECT num, size FROM articles ORDER BY atime LIMIT 200"))
              (victims nil))
          (while (and rows (> total max))
            (let ((row (pop rows)))
              (push (car row) victims)
              (setq total (- total (cadr row)))))
          (when victims
            (sqlite-execute db (format "DELETE FROM articles WHERE num IN (%s)"
                                       (mapconcat #'number-to-string victims ","))))
          ;; More than 200 rows over: come back for the rest.
          (when (> total max) (nngmail--db-evict-articles account)))
        (nngmail--trace "article cache trimmed to %s" (file-size-human-readable total))))))

(defun nngmail--article-cache-size (account)
  "Bytes of cached raw articles in ACCOUNT."
  (or (car (car (sqlite-select (nngmail--db account) "SELECT sum(size) FROM articles"))) 0))

;; loading and migration

(defun nngmail--new-tables (account)
  "Initialise the in-memory tables of ACCOUNT."
  (setf (nngmail-account-id->num account) (make-hash-table :test #'equal :size 4096)
        (nngmail-account-num->id account) (make-hash-table :test #'eql :size 4096)
        (nngmail-account-labels account) (make-hash-table :test #'eql :size 4096)
        (nngmail-account-headers account) (make-hash-table :test #'eql :size 1024)
        (nngmail-account-header-lru account) nil
        (nngmail-account-groups account) (make-hash-table :test #'equal)
        (nngmail-account-name->label account) (make-hash-table :test #'equal)
        (nngmail-account-next-article account) 1
        (nngmail-account-history-id account) nil)
  account)

(defun nngmail--load (account)
  "Open ACCOUNT's database and load the in-memory tables from it.
Return non-nil if the account had state.  A pre-SQLite NAME.eld file
is imported first and renamed to NAME.eld.migrated."
  (nngmail--new-tables account)
  (nngmail--db-open account)
  (when (file-readable-p (nngmail--legacy-state-file account))
    (nngmail--migrate-legacy account))
  (let ((had (> (nngmail--db-count account) 0)))
    (setf (nngmail-account-history-id account) (nngmail--meta-get account "history-id")
          (nngmail-account-pending account) (nngmail--meta-get account "pending")
          (nngmail-account-pending-history-id account) (nngmail--meta-get account "pending-history-id")
          (nngmail-account-backfill account) (nngmail--meta-get account "backfill")
          (nngmail-account-mailbox-total account) (nngmail--meta-get account "mailbox-total")
          (nngmail-account-next-article account) (or (nngmail--meta-get account "next-article") 1)
          (nngmail-account-label-names account) (nngmail--meta-get account "label-names"))
    (nngmail--rebuild-name-map account)
    (let ((t0 (float-time)) (n 0))
      (dolist (row (sqlite-select (nngmail--db account) "SELECT num, id, labels FROM messages"))
        (pcase-let ((`(,num ,id ,labels) row))
          (puthash id num (nngmail-account-id->num account))
          (puthash num id (nngmail-account-num->id account))
          (puthash num (and (not (string-empty-p labels)) (split-string labels ","))
                   (nngmail-account-labels account))
          (cl-incf n)))
      ;; next-article must be past every stored number even if meta lagged.
      (when-let* ((maxnum (car (car (sqlite-select (nngmail--db account) "SELECT max(num) FROM messages")))))
        (setf (nngmail-account-next-article account)
              (max (nngmail-account-next-article account) (1+ maxnum))))
      (nngmail--load-groups account)
      ;; Anything filed in `messages' but not yet numbered in a group
      ;; (a database from before per-group numbering, or an import that
      ;; stopped before its final filing pass).
      (when (< (apply #'+ (mapcar (lambda (g) (hash-table-count (nngmail-group-local->num g)))
                                  (hash-table-values (nngmail-account-groups account))))
               n)
        (nngmail--rebuild-groups account))
      (nngmail--trace "loaded %d messages for %s in %.2fs" n (nngmail-account-name account)
                      (- (float-time) t0)))
    (or had (nngmail-account-history-id account))))

(defun nngmail--migrate-legacy (account)
  "Import ACCOUNT's pre-SQLite NAME.eld into the database, then rename it.
Cached article files under NAME/articles/ are imported too and removed."
  (let ((file (nngmail--legacy-state-file account))
        (name (nngmail-account-name account)))
    (nngmail--message 5 "%s: migrating %s to SQLite (one time)" name (file-name-nondirectory file))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-emacs))
        (insert-file-contents file))
      (goto-char (point-min))
      (forward-line 1)
      (let ((data (ignore-errors (read (current-buffer)))))
        (if (not (and data (eq (plist-get data :version) 1)))
            (nngmail--message 3 "%s: %s is not a readable nngmail state file; left alone" name file)
          (nngmail--with-transaction account
            (dolist (key '(:history-id :pending :pending-history-id :backfill :mailbox-total :next-article :label-names))
              (nngmail--meta-put account (substring (symbol-name key) 1) (plist-get data key)))
            (let ((n 0))
              (pcase-dolist (`(,num ,id ,labels ,header) (plist-get data :messages))
                (nngmail--db-put-message account num id labels header)
                (cl-incf n))
              (nngmail--log "%s: migrated %d messages" name n)))
          (let ((dir (expand-file-name (concat name "/articles") nngmail-directory)))
            (when (file-directory-p dir)
              (let ((n 0))
                (nngmail--with-transaction account
                  (dolist (f (directory-files-recursively dir "\\`[0-9]+\\'"))
                    (let ((num (string-to-number (file-name-nondirectory f))))
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally f)
                        (sqlite-execute (nngmail--db account)
                                        "INSERT OR REPLACE INTO articles (num, raw, size, atime) VALUES (?, ?, ?, ?)"
                                        (list num (propertize (buffer-string) 'coding-system 'binary)
                                              (buffer-size) (float-time))))
                      (cl-incf n))))
                (nngmail--log "%s: migrated %d cached articles" name n)
                (delete-directory (expand-file-name name nngmail-directory) t))))
          (rename-file file (concat file ".migrated") t)
          (nngmail--message 5 "%s: migration done; %s renamed to %s.migrated" name
                            (file-name-nondirectory file) (file-name-nondirectory file)))))))

;;;; Labels and groups

(defun nngmail--label-group-name (id name type)
  "Gnus group name for Gmail label ID with display NAME and TYPE."
  (or (cdr (assoc id nngmail-label-group-alist))
      (if (equal type "system") id name)))

(defun nngmail--fetch-labels (account)
  "Refresh ACCOUNT's label list from Gmail.
Return a list of (ID OLD-NAME . NEW-NAME) for labels whose group name
changed or that disappeared (NEW-NAME nil) since the last list; nil when
nothing a group depends on changed.  New labels are not in the list:
nothing is filed under them yet.  Pass the result to
`nngmail--relabel-groups'."
  (let* ((resp (nngmail--get account "labels"))
         (labels (alist-get 'labels resp))
         (old (nngmail-account-label-names account))
         names changed)
    (dolist (l labels)
      (let ((id (alist-get 'id l))
            (name (alist-get 'name l))
            (type (alist-get 'type l)))
        (unless (member id nngmail-hidden-labels)
          (push (cons id (nngmail--label-group-name id name type)) names))))
    (setq names (nreverse names))
    (pcase-dolist (`(,id . ,name) old)
      (let ((new (cdr (assoc id names))))
        (unless (equal name new)
          (push (cons id (cons name new)) changed))))
    (setf (nngmail-account-label-names account) names)
    (nngmail--rebuild-name-map account)
    (setf (nngmail-account-dirty account) t)
    changed))

(defun nngmail--rebuild-name-map (account)
  "Rebuild the table from group name to label id of ACCOUNT."
  (let ((h (make-hash-table :test #'equal)))
    (pcase-dolist (`(,id . ,name) (nngmail-account-label-names account))
      (puthash name id h))
    (setf (nngmail-account-name->label account) h)))

(defun nngmail--group-label (account group)
  "Label id for GROUP in ACCOUNT, or nil (for the all-mail group)."
  (unless (equal group nngmail-all-mail-group)
    (or (gethash group (nngmail-account-name->label account))
        ;; Maybe a label we haven't seen yet.
        (progn (nngmail--relabel-groups account (nngmail--fetch-labels account))
               (gethash group (nngmail-account-name->label account))))))

(defun nngmail--label-group (account label-id)
  "Group name for LABEL-ID in ACCOUNT, or nil if hidden."
  (cdr (assoc label-id (nngmail-account-label-names account))))

(defcustom nngmail-quarantine-labels '("SPAM" "TRASH")
  "Gmail labels that confine a message to their own group.
A message carrying any of these appears only in that label's group
\([Gmail]/Spam or [Gmail]/Trash) -- never in All Mail, INBOX or any
user label it also happens to carry.  This mirrors the Gmail web UI,
where spam and trash are hidden everywhere except their own views."
  :type '(repeat string))

(defun nngmail--message-groups (account labels)
  "Group names that a message with LABELS belongs to in ACCOUNT.
Quarantined messages (see `nngmail-quarantine-labels') are filed only
under their quarantine group(s)."
  (let ((quarantine (seq-filter (lambda (l) (member l nngmail-quarantine-labels)) labels)))
    (if quarantine
        (delq nil (mapcar (lambda (l) (nngmail--label-group account l)) quarantine))
      (let (groups)
        (dolist (l labels)
          (when-let* ((g (nngmail--label-group account l)))
            (push g groups)))
        (push nngmail-all-mail-group groups)
        groups))))

;;;; Numbering
;;
;; Every message has one server-wide number (its row in `messages'),
;; assigned in the order nngmail first saw it.  Gnus, however, is shown a
;; dense per-group numbering: article 1..N of a group, in the order
;; messages entered that group.  That is what nnimap does too (IMAP UIDs
;; are per mailbox) and it is what keeps Gnus fast: a group's read range
;; is a few spans instead of one span per article, there are no gaps to
;; report, and updating a group's info costs O(unread) not O(size).
;;
;; The map lives in `group_articles' and in memory as an `nngmail-group':
;; two hashes (local -> num, num -> local) plus the next local number.
;; Local numbers are handed out once and never reused, so a message that
;; leaves a group and comes back gets a new number -- exactly like a
;; message moved out of and back into an IMAP folder.
;;
;; Everything inside nngmail works in server numbers.  Translation
;; happens only at the nnoo boundary: `nngmail--local->num' on the way
;; in, `nngmail--num->local' on the way out.

(cl-defstruct (nngmail-group (:constructor nngmail--make-group))
  name
  (local->num (make-hash-table :test #'eql))
  (num->local (make-hash-table :test #'eql))
  (next 1))

(defun nngmail--group (account name &optional create)
  "The `nngmail-group' record for group NAME in ACCOUNT, or nil.
With CREATE, make an empty one if there is none."
  (or (gethash name (nngmail-account-groups account))
      (and create
           (puthash name (nngmail--make-group :name name) (nngmail-account-groups account)))))

(defun nngmail--group-size (account name)
  "Number of articles in group NAME of ACCOUNT."
  (if-let* ((g (nngmail--group account name)))
      (hash-table-count (nngmail-group-local->num g))
    0))

(defun nngmail--group-numbers (account name)
  "Server-wide article numbers of group NAME in ACCOUNT, descending.
Consing; prefer `nngmail--group-size' or `nngmail--group-member-p' when
only the count or a membership test is needed."
  (when-let* ((g (nngmail--group account name)))
    (let (out)
      (maphash (lambda (_local num) (push num out)) (nngmail-group-local->num g))
      (sort out #'>))))

(defun nngmail--group-member-p (account name num)
  "Non-nil if server article NUM is in group NAME of ACCOUNT."
  (when-let* ((g (nngmail--group account name)))
    (gethash num (nngmail-group-num->local g))))

(defun nngmail--group-locals (account name)
  "Local article numbers of group NAME in ACCOUNT, ascending."
  (when-let* ((g (nngmail--group account name)))
    (sort (hash-table-keys (nngmail-group-local->num g)) #'<)))

(defun nngmail--group-high (account name)
  "Highest local number ever assigned in group NAME (0 for none).
ACCOUNT is the nngmail account."
  (if-let* ((g (nngmail--group account name))) (1- (nngmail-group-next g)) 0))

(defun nngmail--local->num (account name local)
  "Server number of LOCAL article in group NAME, or nil.
ACCOUNT is the nngmail account."
  (when-let* ((g (nngmail--group account name)))
    (gethash local (nngmail-group-local->num g))))

(defun nngmail--num->local (account name num)
  "Local number of server article NUM in group NAME, or nil.
ACCOUNT is the nngmail account."
  (when-let* ((g (nngmail--group account name)))
    (gethash num (nngmail-group-num->local g))))

(defun nngmail--locals->nums (account name locals)
  "Server numbers for LOCALS in group NAME, dropping unknown ones.
ACCOUNT is the nngmail account."
  (when-let* ((g (nngmail--group account name)))
    (let ((h (nngmail-group-local->num g)) out)
      (dolist (l locals) (when-let* ((n (gethash l h))) (push n out)))
      (nreverse out))))

(defun nngmail--group-add (account name num &optional no-db)
  "File server article NUM into group NAME; return its local number.
A new local number is taken from the group's counter and written to the
database unless NO-DB (bulk filing writes rows itself).
ACCOUNT is the nngmail account."
  (let ((g (nngmail--group account name t)))
    (or (gethash num (nngmail-group-num->local g))
        (let ((local (nngmail-group-next g)))
          (setf (nngmail-group-next g) (1+ local))
          (puthash local num (nngmail-group-local->num g))
          (puthash num local (nngmail-group-num->local g))
          (unless no-db
            (sqlite-execute (nngmail--db account)
                            "INSERT OR IGNORE INTO group_articles (grp, local, num) VALUES (?, ?, ?)"
                            (list name local num)))
          local))))

(defun nngmail--group-remove (account name num)
  "Take server article NUM out of group NAME.  Its local number is retired.
ACCOUNT is the nngmail account."
  (when-let* ((g (nngmail--group account name))
              (local (gethash num (nngmail-group-num->local g))))
    (remhash local (nngmail-group-local->num g))
    (remhash num (nngmail-group-num->local g))
    (sqlite-execute (nngmail--db account)
                    "DELETE FROM group_articles WHERE grp = ? AND num = ?" (list name num))
    local))

(defun nngmail--load-groups (account)
  "Load the per-group numbering of ACCOUNT from the database.
One select, rows ordered by group so the group record is looked up once
per run of rows, and the hash tables are presized: 2.35 million
messages in about 4 s."
  (let ((groups (make-hash-table :test #'equal))
        (db (nngmail--db account))
        (last-name nil) l->n n->l)
    ;; Presize from the counts; rehashing a 5M-entry table is the slow part.
    (dolist (row (sqlite-select db "SELECT grp, count(*), max(local) FROM group_articles GROUP BY grp"))
      (let* ((name (car row)) (count (nth 1 row)))
        (puthash name (nngmail--make-group
                       :name name
                       :local->num (make-hash-table :test #'eql :size (1+ count))
                       :num->local (make-hash-table :test #'eql :size (1+ count))
                       :next (1+ (or (nth 2 row) 0)))
                 groups)))
    (dolist (row (sqlite-select db "SELECT grp, local, num FROM group_articles ORDER BY grp"))
      (let ((name (car row)))
        (unless (equal name last-name)
          (let ((g (gethash name groups)))
            (setq last-name name
                  l->n (nngmail-group-local->num g)
                  n->l (nngmail-group-num->local g))))
        (let ((local (nth 1 row)) (num (nth 2 row)))
          (puthash local num l->n)
          (puthash num local n->l))))
    ;; Counters must also clear numbers that were retired (removed rows).
    (dolist (row (sqlite-select (nngmail--db account)
                                "SELECT key, value FROM meta WHERE key LIKE 'group-next:%'"))
      (let* ((name (substring (car row) (length "group-next:")))
             (g (or (gethash name groups) (puthash name (nngmail--make-group :name name) groups)))
             (next (car (read-from-string (cadr row)))))
        (when (> next (nngmail-group-next g)) (setf (nngmail-group-next g) next))))
    (setf (nngmail-account-groups account) groups)))

(defun nngmail--save-group-counters (account)
  "Persist each group's next local number, so retired numbers stay retired.
ACCOUNT is the nngmail account."
  (maphash (lambda (name g)
             (nngmail--meta-put account (concat "group-next:" name) (nngmail-group-next g)))
           (nngmail-account-groups account)))

(defun nngmail--rebuild-groups (account)
  "File every message of ACCOUNT into its groups.
Local numbers are assigned
to any that have none.  Existing local numbers are kept.  Used after a
bulk import (which files labels without touching groups) and after a
label refresh.  Linear in the index plus one sort."
  (let ((labels (nngmail-account-labels account))
        (nums nil))
    (maphash (lambda (num _) (push num nums)) labels)
    (setq nums (sort nums #'<))
    (nngmail--with-transaction account
      (let ((db (nngmail--db account))
            (batch nil) (count 0))
        (cl-flet ((flush ()
                    (when batch
                      (sqlite-execute db
                                      (concat "INSERT OR IGNORE INTO group_articles (grp, local, num) VALUES "
                                              (mapconcat (lambda (_) "(?, ?, ?)") batch ","))
                                      (apply #'append (nreverse batch)))
                      (setq batch nil count 0))))
          (dolist (num nums)
            (let ((wanted (nngmail--message-groups account (gethash num labels))))
              (dolist (g wanted)
                (unless (nngmail--group-member-p account g num)
                  (let ((local (nngmail--group-add account g num t)))
                    (push (list g local num) batch)
                    ;; SQLite allows 32k bound parameters per statement.
                    (when (>= (cl-incf count) 5000) (flush)))))))
          (flush))
        ;; Messages whose labels no longer put them in a group they are
        ;; filed under (label refresh renamed or dropped a label).
        (maphash (lambda (name g)
                   (let (stale)
                     (maphash (lambda (num _)
                                (unless (member name (nngmail--message-groups account (gethash num labels)))
                                  (push num stale)))
                              (nngmail-group-num->local g))
                     (dolist (num stale) (nngmail--group-remove account name num))))
                 (nngmail-account-groups account))
        (nngmail--save-group-counters account)))))

(defun nngmail--group-rename (account from to)
  "Move group FROM to TO in ACCOUNT, keeping every local number.
Rows and the counter move as they are, so Gnus's read ranges and ticks
for the group, which `gnus-group-rename-group' carries over to the new
name itself, still fit.  TO must not exist yet."
  (when-let* ((g (nngmail--group account from)))
    (when (nngmail--group account to)
      (error "nngmail: Group %s already exists" to))
    (nngmail--with-transaction account
      (let ((db (nngmail--db account)))
        (sqlite-execute db "UPDATE group_articles SET grp = ? WHERE grp = ?" (list to from))
        (sqlite-execute db "DELETE FROM meta WHERE key = ?" (list (concat "group-next:" from)))
        (nngmail--meta-put account (concat "group-next:" to) (nngmail-group-next g))))
    (remhash from (nngmail-account-groups account))
    (setf (nngmail-group-name g) to)
    (puthash to g (nngmail-account-groups account))
    g))

(defun nngmail--strip-label (account label-id group)
  "Remove LABEL-ID from every message of ACCOUNT filed under GROUP.
Only that group is walked, not the index.  Return how many messages."
  (let ((nums (nngmail--group-numbers account group))
        (labels (nngmail-account-labels account)))
    (nngmail--with-transaction account
      (dolist (num nums)
        (nngmail--set-labels account num (remove label-id (gethash num labels)))
        ;; The label's name is already gone from the map, so
        ;; `nngmail--set-labels' no longer knows the message was filed
        ;; under GROUP; take it out ourselves.
        (nngmail--group-remove account group num)))
    (length nums)))

;;;###autoload
(defun nngmail-rebuild-groups (server)
  "Refile every message of SERVER into its groups from its labels.
Label changes are applied incrementally (see `nngmail--relabel-groups');
this is the full pass, for when group contents look wrong: it fixes any
drift between the labels table and the per-group numbering.  Existing
local numbers are kept.  Walks the whole index (about 45 s for 2.5M
messages) and shows progress."
  (interactive (list (nngmail--read-server)))
  (let ((account (nngmail--account server))
        (t0 (float-time)))
    (nngmail--progress account "refiling groups" nil nil t)
    (unwind-protect
        (nngmail--timed "nngmail-rebuild-groups"
          (nngmail--relabel-groups account (nngmail--fetch-labels account))
          (nngmail--rebuild-groups account)
          (nngmail--save account))
      (nngmail--progress account nil))
    (message "nngmail: %s groups refiled from labels in %s; press g in *Group*"
             server (nngmail--fmt-duration (- (float-time) t0)))))

(defun nngmail--relabel-groups (account changed)
  "Bring ACCOUNT's groups in step with label changes CHANGED.
CHANGED is what `nngmail--fetch-labels' returned: (ID OLD . NEW) per
renamed (NEW a name) or deleted (NEW nil) label.  A rename moves the
group's rows so local numbers survive; a deletion strips the label from
the group's messages, which empties the group.  Only those groups are
touched, so this is proportional to the labels that changed, not to the
index.  Falls back to `nngmail--rebuild-groups' when the target name of
a rename is already taken (two labels swapped names, say)."
  (when changed
    (let ((full nil))
      (pcase-dolist (`(,id ,old . ,new) changed)
        (cond
         ((null new)
          (nngmail--log "%s: label %s (%s) deleted; stripping it from %d messages"
                        (nngmail-account-name account) id old
                        (nngmail--strip-label account id old)))
         ((null (nngmail--group account old)))  ; nothing filed under it
         ((nngmail--group account new)
          (setq full t))
         (t
          (nngmail--log "%s: label %s renamed %s -> %s; moving the group"
                        (nngmail-account-name account) id old new)
          (nngmail--group-rename account old new))))
      (when full
        (nngmail--log "%s: rename target exists; rebuilding all groups"
                      (nngmail-account-name account))
        (nngmail--rebuild-groups account))
      (setf (nngmail-account-dirty account) t))))

(defvar nngmail--bulk-filing nil
  "Non-nil while a bulk import files thousands of articles.
`nngmail--set-labels' then records labels only and leaves the group
numbering alone; the importer calls `nngmail--rebuild-groups' once at
the end, which assigns local numbers in server order in one pass.")

(defun nngmail--set-labels (account num labels &optional header)
  "Record LABELS (and HEADER, if given) for article NUM in ACCOUNT.
Writes the row to the database and updates group membership.  Under
`nngmail--bulk-filing' the group tables are left for
`nngmail--rebuild-groups' at the end of the job."
  (let ((id (gethash num (nngmail-account-num->id account))))
    (cond
     (header
      (nngmail--db-put-message account num id labels header)
      (nngmail--header-cache-put account num header))
     ((gethash num (nngmail-account-labels account))
      (nngmail--db-put-labels account num labels)
      ;; The header carries the labels too (X-Gmail-Labels in its extra),
      ;; and the summary line reads them from there.  Keep the cached
      ;; and stored header in step, without fetching one we do not have.
      (when-let* ((h (and (not nngmail--bulk-filing)
                          (gethash num (nngmail-account-headers account)))))
        (setf (alist-get 'X-Gmail-Labels (mail-header-extra h))
              (mapconcat #'identity labels ","))
        (nngmail--db-put-message account num id labels h)))
     (t
      (nngmail--db-put-message account num id labels nil))))
  (if nngmail--bulk-filing
      (puthash num labels (nngmail-account-labels account))
    (nngmail--set-labels-1 account num labels)))

(defvar nngmail--touched-groups 'off
  "When a list is bound here, groups whose membership changed are pushed to it.
The background job binds it around a pass and refreshes those *Group*
lines afterwards.")

(defun nngmail--set-labels-1 (account num labels)
  "Record LABELS for NUM in the in-memory tables and update group membership.
ACCOUNT is the nngmail account."
  (let* ((old (gethash num (nngmail-account-labels account)))
         (old-groups (and old (nngmail--message-groups account old)))
         (new-groups (nngmail--message-groups account labels)))
    (puthash num labels (nngmail-account-labels account))
    (dolist (g old-groups)
      (unless (member g new-groups)
        (nngmail--group-remove account g num)
        (nngmail--note-touched g)))
    (dolist (g new-groups)
      (unless (member g old-groups)
        (nngmail--group-add account g num)
        (nngmail--note-touched g)))
    ;; A label change within the same groups (UNREAD, STARRED) changes
    ;; the counts Gnus shows too.
    (when (and old (equal old-groups new-groups) (not (equal old labels)))
      (mapc #'nngmail--note-touched new-groups))))

(defun nngmail--note-touched (group)
  "Remember GROUP as changed, when someone is collecting; and refresh the counts."
  (when (listp nngmail--touched-groups)
    (unless (member group nngmail--touched-groups)
      (push group nngmail--touched-groups)))
  (cl-incf nngmail--count-pills-generation)
  (nngmail--schedule-idle-refresh))

(defvar nngmail--idle-refresh-timer nil)

(defun nngmail--schedule-idle-refresh ()
  "Redraw the idle mode-line indicator soon, once, after label changes.
Reading a message, starring, a sync pass: each changes the counts the
pills show.  Coalesced on a short idle timer so a page of history or a
summary exit costs one redraw, not one per message."
  (unless nngmail--idle-refresh-timer
    (setq nngmail--idle-refresh-timer
          (run-with-idle-timer 0.5 nil
                               (lambda ()
                                 (setq nngmail--idle-refresh-timer nil)
                                 (nngmail--spinner-refresh-idle))))))

(defun nngmail--store-header (account num header labels)
  "Store HEADER and LABELS for article NUM.
The row in the database, the cache and the groups are all updated.
ACCOUNT is the nngmail account."
  (nngmail--set-labels account num labels header))

(defun nngmail--forget (account num)
  "Forget article NUM of ACCOUNT entirely (message deleted at Gmail)."
  (when-let* ((labels (gethash num (nngmail-account-labels account))))
    (dolist (g (nngmail--message-groups account labels))
      (nngmail--group-remove account g num)))
  (let ((id (gethash num (nngmail-account-num->id account))))
    (when id (remhash id (nngmail-account-id->num account))))
  (remhash num (nngmail-account-num->id account))
  (remhash num (nngmail-account-labels account))
  (remhash num (nngmail-account-headers account))
  (nngmail--db-delete-message account num))

(defun nngmail--assign (account gmail-id)
  "Return the article number for GMAIL-ID in ACCOUNT, allocating if new.
A new number is recorded in memory only; the row appears in the
database with its first `nngmail--set-labels'."
  (or (gethash gmail-id (nngmail-account-id->num account))
      (let ((num (nngmail-account-next-article account)))
        (setf (nngmail-account-next-article account) (1+ num))
        (puthash gmail-id num (nngmail-account-id->num account))
        (puthash num gmail-id (nngmail-account-num->id account))
        (setf (nngmail-account-dirty account) t)
        num)))

;;;; Headers (NOV) from metadata

(defconst nngmail--metadata-headers
  '("Subject" "From" "Date" "Message-ID" "References" "In-Reply-To" "To" "Cc" "Newsgroups")
  "Headers requested with format=metadata.")

(defun nngmail--metadata-path (id)
  "API path fetching metadata for message ID."
  (concat "messages/" id
          "?format=metadata"
          (mapconcat (lambda (h) (concat "&metadataHeaders=" h))
                     nngmail--metadata-headers "")))

(defun nngmail--header-value (payload name)
  "Return header NAME from a Gmail message PAYLOAD alist, or nil."
  (let ((headers (alist-get 'headers payload)))
    (cl-loop for h in headers
             when (string-equal (alist-get 'name h) name)
             return (alist-get 'value h))))

(defun nngmail--decode-header (value)
  "RFC 2047-decode VALUE for display; Gmail sometimes pre-decodes."
  (if (null value) ""
    (let ((v (mail-decode-encoded-word-string value)))
      (replace-regexp-in-string "[\n\r\t]+" " " v))))

(defun nngmail--make-header (num msg)
  "Build a `mail-header' vector for article NUM from Gmail message MSG."
  (let* ((payload (alist-get 'payload msg))
         (labels (alist-get 'labelIds msg))
         (message-id (or (nngmail--header-value payload "Message-ID")
                         (format "<%s@nngmail.gmail>" (alist-get 'id msg))))
         (refs (or (nngmail--header-value payload "References")
                   (nngmail--header-value payload "In-Reply-To")
                   ""))
         (size (or (alist-get 'sizeEstimate msg) 0))
         (extra `((To . ,(nngmail--decode-header (nngmail--header-value payload "To")))
                  (Cc . ,(nngmail--decode-header (nngmail--header-value payload "Cc")))
                  (X-Gmail-Labels . ,(mapconcat #'identity labels ","))
                  (X-Gmail-Thread-Id . ,(or (alist-get 'threadId msg) "")))))
    (make-full-mail-header
     num
     (nngmail--decode-header (nngmail--header-value payload "Subject"))
     (nngmail--decode-header (nngmail--header-value payload "From"))
     (or (nngmail--header-value payload "Date")
         (let ((ms (alist-get 'internalDate msg)))
           (and ms (format-time-string "%a, %d %b %Y %T %z"
                                       (seconds-to-time (/ (string-to-number ms) 1000))))))
     message-id
     refs
     size
     0
     nil
     extra)))

(defun nngmail--fetch-metadata (account ids &optional phase force)
  "Fetch metadata for Gmail IDS in ACCOUNT via batch; store headers and labels.
Return the list of article numbers for IDS (fetched or already known).
PHASE, a string, labels the progress display.  Ids whose headers are
already stored are skipped unless FORCE, in which case their labels
are refreshed from Gmail too."
  (let ((seen (make-hash-table :test #'equal :size (length ids)))
        (pending nil))
    ;; Dedupe and (unless FORCE) drop ids whose headers we already have.
    (dolist (id ids)
      (unless (gethash id seen)
        (puthash id t seen)
        (let ((n (gethash id (nngmail-account-id->num account))))
          (when (or force
                    (not (and n (nngmail--has-header-p account n))))
            (push id pending)))))
    (setq pending (nreverse pending))
    (when pending
      (nngmail--trace "fetching headers for %d messages" (length pending))
      (let ((results (nngmail--batch
                      account
                      (mapcar (lambda (id) (list id "GET" (nngmail--metadata-path id)))
                              pending)
                      (or phase "fetching headers")))
            (count 0))
        ;; One transaction per batch: a crash loses at most this batch.
        (nngmail--timed (format "store %d headers" (length results))
          (nngmail--with-transaction account
            (pcase-dolist (`(,id . ,msg) results)
              (when msg
                (let ((num (nngmail--assign account id)))
                  (nngmail--store-header account num (nngmail--make-header num msg)
                                         (alist-get 'labelIds msg))
                  (cl-incf count))))))
        (when (> count 0)
          (setf (nngmail-account-dirty account) t))))
    (let ((nums nil))
      (maphash (lambda (id _)
                 (when-let* ((n (gethash id (nngmail-account-id->num account))))
                   (push n nums)))
               seen)
      nums)))

;;;; Sync

(defun nngmail--list-ids (account &optional query label-ids limit phase)
  "List message ids in ACCOUNT matching QUERY and LABEL-IDS, up to LIMIT.
PHASE, if a string, is shown as progress while paging."
  (mapcar (lambda (m) (alist-get 'id m))
          (nngmail--paged account "messages"
                          (append `(("maxResults" . 500)
                                    ("includeSpamTrash" . "true"))
                                  (when query `(("q" . ,query)))
                                  (mapcar (lambda (l) (cons "labelIds" l)) label-ids))
                          'messages limit phase)))

(defun nngmail--fetch-in-chunks (account ids phase &optional force)
  "Fetch metadata for IDS in ACCOUNT, `nngmail-sync-chunk' at a time.
Save state after each chunk and show PHASE progress.  FORCE is passed
to `nngmail--fetch-metadata'.

The not-yet-fetched ids are kept in the account's `pending' slot and
persisted with every checkpoint, so if the sync is aborted (quota, \\[keyboard-quit],
Emacs dying) the next sync picks up exactly where this one stopped
instead of relisting."
  (let* ((chunk-size (max 1 (min nngmail-sync-chunk nngmail-batch-size)))
         (total (length ids))
         (done 0))
    (setf (nngmail-account-pending account) (list phase force ids)
          (nngmail-account-dirty account) t)
    (unwind-protect
        (progn
          (while ids
            (let ((chunk (seq-take ids chunk-size)))
              (nngmail--progress account phase done total)
              ;; Inner batches report their own sub-progress via the
              ;; `nngmail--batch-progress-base' offset so the line shows
              ;; one done/total for the whole task, never two counters.
              (let ((nngmail--batch-progress-base (cons done total)))
                (nngmail--timed (format "chunk of %d: fetch+store" (length chunk))
                  (nngmail--fetch-metadata account chunk phase force)))
              (setq ids (nthcdr chunk-size ids)
                    done (+ done (length chunk)))
              (setf (nngmail-account-pending account) (and ids (list phase force ids))
                    (nngmail-account-dirty account) t)
              ;; Rows are committed per batch; this only writes counters.
              (nngmail--timed "chunk: save meta"
                (if (null ids)
                    (nngmail--save account)
                  (nngmail--checkpoint account)))))
          (setf (nngmail-account-pending account) nil)
          (nngmail--progress account phase done total t))
      ;; On any non-local exit, persist whatever we have plus the remainder.
      (setf (nngmail-account-dirty account) t)
      (nngmail--save account))))

(defun nngmail--resume-pending (account)
  "If ACCOUNT has a `pending' fetch from an interrupted sync, finish it.
Return non-nil if there was something to resume."
  (pcase (nngmail-account-pending account)
    (`(,phase ,force ,ids)
     (when ids
       (nngmail--message 5 "%s: resuming interrupted sync (%d messages left)"
                         (nngmail-account-name account) (length ids))
       (nngmail--fetch-in-chunks account ids (or phase "indexing") force)
       t))))

(defun nngmail--full-sync (account)
  "Initial or recovery sync of ACCOUNT: list recent mail and fetch metadata.
On a brand-new account this first offers a whole-mailbox IMAP import
\(`nngmail-offer-import'); the REST listing that follows then only has
to find what arrived since, plus the history id to sync from."
  (nngmail--maybe-offer-import account)
  (let* ((name (nngmail-account-name account))
         (profile (progn (nngmail--progress account "reading profile" nil nil t)
                         (nngmail--get account "profile")))
         (history-id (alist-get 'historyId profile))
         (seen (make-hash-table :test #'equal :size 4096))
         (ids nil))
    (when-let* ((n (alist-get 'messagesTotal profile)))
      (setf (nngmail-account-mailbox-total account) n))
    (nngmail--progress account "listing labels" nil nil t)
    (nngmail--relabel-groups account (nngmail--fetch-labels account))
    ;; Newest N overall.
    (dolist (id (nngmail--list-ids account nil nil nngmail-initial-limit
                                   "listing recent messages"))
      (unless (gethash id seen)
        (puthash id t seen)
        (push id ids)))
    ;; Top up each visible label so small labels aren't empty.
    (when nngmail-label-limit
      (let* ((labels (nngmail-account-label-names account))
             (n (length labels))
             (i 0)
             (added 0))
        (cl-loop for (label-id . gname) in labels
                 while (or (null nngmail-label-total-limit)
                           (< added nngmail-label-total-limit))
                 do (cl-incf i)
                 (nngmail--progress account (format "listing label %s" gname) i n t)
                 (dolist (id (nngmail--list-ids account nil (list label-id)
                                                nngmail-label-limit))
                   (unless (gethash id seen)
                     (puthash id t seen)
                     (push id ids)
                     (cl-incf added))))))
    ;; Each listing is newest-first and we `push'ed, so `ids' is now
    ;; oldest-first within each listing but the label top-ups (which are
    ;; older on average) come after the recent ones.  Sort by Gmail id:
    ;; ids are hex of a monotonically increasing value, so this yields true
    ;; chronological order and article numbers ascend with time.
    (setq ids (sort ids (lambda (a b)
                          (or (< (length a) (length b))
                              (and (= (length a) (length b)) (string< a b))))))
    (nngmail--message 5 "%s: %d messages to index" name (length ids))
    ;; Remember which historyId this listing corresponds to, so that a
    ;; resumed run adopts it once the fetch completes rather than relisting.
    (setf (nngmail-account-pending-history-id account) history-id
          (nngmail-account-dirty account) t)
    (nngmail--fetch-in-chunks account ids "indexing")
    (nngmail--finish-full-sync account)))

(defun nngmail--finish-full-sync (account)
  "Adopt the pending historyId of ACCOUNT after its fetch completed."
  (when-let* ((hid (nngmail-account-pending-history-id account)))
    (setf (nngmail-account-history-id account) hid
          (nngmail-account-pending-history-id account) nil))
  (setf (nngmail-account-dirty account) t)
  (nngmail--save account)
  (nngmail--progress account nil))

(defcustom nngmail-offer-import 'ask
  "What to do the first time a server opens with an empty index.
`ask' offers to import the whole mailbox over IMAP right away (fast,
no quota) before the normal first sync.  `always' does it without
asking.  nil skips the offer: the first sync indexes only the newest
`nngmail-initial-limit' messages plus a top-up per label, and older
mail stays out of Gnus until `M-x nngmail-backfill'."
  :type '(choice (const ask) (const always) (const :tag "Never offer" nil)))

(defvar nngmail--imap-unavailable-reason)

(defun nngmail--maybe-offer-import (account)
  "On an empty index, offer (or do) a full IMAP import per `nngmail-offer-import'.
Return non-nil if an import ran.
ACCOUNT is the nngmail account."
  (when (and nngmail-offer-import
             (not noninteractive)
             (zerop (hash-table-count (nngmail-account-num->id account)))
             (null (nngmail-account-history-id account))
             (or (eq nngmail-offer-import 'always)
                 (y-or-n-p
                  (format "Nngmail %s: the index is empty.  Without a full import only the newest %s messages are indexed (M-x nngmail-backfill adds the rest later).  Import the whole mailbox now over IMAP (fast, no API quota, resumable, C-g stops)? "
                          (nngmail-account-name account)
                          (nngmail--fmt-num (or nngmail-initial-limit 0))))))
    (condition-case err
        (progn
          (nngmail--message 5 "%s: importing the whole mailbox over IMAP" (nngmail-account-name account))
          (nngmail--imap-import account nil "importing (IMAP)" nil t)
          t)
      (nngmail-imap-unavailable
       (setq nngmail--imap-unavailable-reason (cadr err))
       (nngmail--imap-close)
       (display-warning 'nngmail
                        (format "%s: %s.  Continuing with the normal first sync (newest %s messages); M-x nngmail-backfill later, or enable IMAP and M-x nngmail-imap-retry."
                                (nngmail-account-name account) (cadr err)
                                (nngmail--fmt-num (or nngmail-initial-limit 0)))
                        :warning)
       nil)
      (quit
       (nngmail--imap-close)
       (nngmail--message 5 "%s: import stopped; what was imported is kept, M-x nngmail-backfill resumes it"
                         (nngmail-account-name account))
       nil))))

(defcustom nngmail-sync-headers 'lazy
  "When to fetch headers (subject, from, date) for newly arrived messages.
`lazy' indexes new arrivals from history alone -- Gmail's history
records carry each message's id and current labels, which is all that
group membership and unread counts need -- and fetches headers only when
a summary buffer shows the article, in one batch for exactly the
articles displayed.  This is how IMAP backends behave and it makes
startup take a couple of history.list calls instead of thousands of
messages.get calls.

`eager' fetches every new arrival's headers during the sync, so opening
a group never has to wait for the network.  Slower start, faster first
summary.

Messages whose labels changed (read, starred, archived) never need a
header fetch in either mode: their labels come from the history record."
  :type '(choice (const lazy) (const eager)))

(defvar nngmail--history-unknowns nil
  "Per-sync counts of history records that did not carry current labels.
They are kept
by record shape, so the log can say why a sync had to fetch.")

(defun nngmail--apply-history (account history)
  "Apply one history.list record HISTORY to ACCOUNT.
Return the ids of messages whose labels need refetching.

Every history record also carries a `messages' list with the *current*
labelIds of each affected message.  Rather than replaying labelsAdded
and labelsRemoved deltas (which arrive out of order relative to our
snapshot and drift), we take the authoritative current labels from
there.  Messages we have not indexed yet, or that Gmail omits current
labels for, are returned so their metadata gets fetched."
  (let ((touched (make-hash-table :test #'equal))
        (added (make-hash-table :test #'equal))
        refetch)
    (dolist (rec (alist-get 'messagesAdded history))
      (puthash (alist-get 'id (alist-get 'message rec)) t added))
    ;; Deleted messages go away regardless.
    (dolist (m (alist-get 'messagesDeleted history))
      (let ((id (alist-get 'id (alist-get 'message m))))
        (puthash id 'deleted touched)
        (when-let* ((num (gethash id (nngmail-account-id->num account))))
          (nngmail--forget account num))))
    ;; The record's plain `messages' list names what changed but carries
    ;; only id and threadId (labelIds only when Gmail chooses to).  A
    ;; label change always comes as a labelsAdded/labelsRemoved record
    ;; as well, and an arrival as messagesAdded; a record that has only
    ;; `messages' is a change to something we do not index (thread
    ;; bookkeeping, importance recomputation), so it is no reason to
    ;; fetch.  Treating it as "labels unknown" cost 3,600 metadata gets
    ;; (18k quota units) after one night.
    (dolist (m (alist-get 'messages history))
      (let ((id (alist-get 'id m)))
        (unless (eq (gethash id touched) 'deleted)
          (when-let* ((labels (alist-get 'labelIds m)))
            (puthash id labels touched)))))
    (dolist (key '(messagesAdded labelsAdded labelsRemoved))
      (dolist (rec (alist-get key history))
        (let* ((msg (alist-get 'message rec))
               (id (alist-get 'id msg)))
          (unless (eq (gethash id touched) 'deleted)
            ;; Prefer the labels embedded in the record's own message.
            (when-let* ((labels (alist-get 'labelIds msg)))
              (puthash id labels touched))
            (unless (gethash id touched)
              (puthash id 'unknown touched))))))
    ;; Account for what will need a fetch, so the log says why.
    (maphash (lambda (id labels)
               (when (eq labels 'unknown)
                 (cl-incf (alist-get (if (gethash id added) 'arrival-without-labels
                                       'label-change-without-labels)
                                     nngmail--history-unknowns 0))))
             touched)
    (maphash
     (lambda (id labels)
       (unless (eq labels 'deleted)
         (let ((num (gethash id (nngmail-account-id->num account))))
           (cond
            ;; Already indexed and Gmail told us the current labels: just
            ;; record them (whether or not the header was fetched yet).
            ;; No request needed.
            ((and num (listp labels))
             (nngmail--set-labels account num labels))
            ;; Already indexed but labels not supplied: refresh.
            (num (push id refetch))
            ;; Not indexed.  Admit only genuine new arrivals
            ;; (`messagesAdded').  Label churn on the millions of messages
            ;; we never indexed -- filters relabelling old mail, spam
            ;; sweeps, category shuffles -- is not our concern: history
            ;; *maintains* the index and admits new mail; it never pulls
            ;; old mail in.  That is what backfill and C-u N RET are for.
            ((gethash id added)
             (if (and (eq nngmail-sync-headers 'lazy) (listp labels))
                 ;; Enough to file it: number it and set its labels now;
                 ;; `nngmail-retrieve-headers' fetches the rest on demand.
                 (nngmail--set-labels account (nngmail--assign account id) labels)
               (push id refetch)))))))
     touched)
    refetch))

(defcustom nngmail-sync-debounce 20
  "Seconds during which repeated sync requests are collapsed into one.
Gnus asks a backend to scan several times per `g' (once for the server,
once per group); one history.list call per burst is plenty."
  :type 'integer)

(defvar nngmail--last-sync (make-hash-table :test #'equal)
  "Account name -> `float-time' of the last completed sync.")

(defun nngmail--incremental-sync (account &optional force)
  "Bring ACCOUNT up to date via history.list; fall back to a full sync.
Unless FORCE, do nothing if a sync completed within `nngmail-sync-debounce'."
  (let ((start (nngmail-account-history-id account))
        (last (gethash (nngmail-account-name account) nngmail--last-sync 0)))
    (cond
     ((and (not force) start
           (< (- (float-time) last) nngmail-sync-debounce))
      (nngmail--trace "sync debounced (%s ago)" (nngmail--fmt-duration (- (float-time) last))))
     (t
      (nngmail--incremental-sync-1 account start)
      (puthash (nngmail-account-name account) (float-time) nngmail--last-sync)))))

(defun nngmail--incremental-sync-1 (account start)
  "Do the actual sync of ACCOUNT from history id START (nil = full sync).
An interrupted fetch left over from a previous run is finished first."
  (when (nngmail--timed "sync: resume pending fetch" (nngmail--resume-pending account))
    ;; If that was the tail of a full sync, adopt its historyId now.
    (when (and (null start) (nngmail-account-pending-history-id account))
      (nngmail--finish-full-sync account)
      (setq start (nngmail-account-history-id account))))
  (progn
    (if (null start)
        (nngmail--full-sync account)
      (condition-case err
          (let ((new-ids (nngmail--pending-history-ids account))
                (latest start)
                (records 0)
                (pages 0)
                (started (float-time))
                (nngmail--history-unknowns nil)
                token done)
            (when new-ids
              (nngmail--log "%d messages still to fetch from an interrupted history pass" (length new-ids)))
            (nngmail--progress account "checking for changes" nil nil t)
            (nngmail--timed "sync: history.list + apply"
            (while (not done)
              (let* ((resp (nngmail--get account "history"
                                         (append `(("startHistoryId" . ,start)
                                                   ("maxResults" . 500))
                                                 (when token `(("pageToken" . ,token))))))
                     (history (alist-get 'history resp)))
                (cl-incf pages)
                (cl-incf records (length history))
                (setq token (alist-get 'nextPageToken resp)
                      new-ids (nngmail--apply-history-page account history token
                                                           (alist-get 'historyId resp) new-ids)
                      start (nngmail-account-history-id account)
                      latest start)
                (nngmail--history-progress account pages records started t)
                (unless token (setq done t)))))
            (setq new-ids (seq-uniq new-ids))
            ;; Advance to the mailbox's current history id now and
            ;; persist it with the list of messages still to fetch: the
            ;; fetch can take minutes (5 quota units each, paced), and
            ;; if it is interrupted the next sync resumes that list.
            (setf (nngmail-account-history-id account) latest
                  (nngmail-account-pending account)
                  (and new-ids (list "fetching changed messages" t new-ids))
                  (nngmail-account-dirty account) t)
            (nngmail--timed "sync: save meta" (nngmail--save account))
            (if new-ids
                ;; Only messages whose current labels history did not
                ;; supply reach here (plus new arrivals in `eager' mode).
                ;; FORCE so their labels are refreshed, not just headers.
                (progn
                  (nngmail--log "%d history records; %d messages to fetch (labels not in history: %s)"
                                records (length new-ids)
                                (mapconcat (lambda (c) (format "%s %d" (car c) (cdr c)))
                                           nngmail--history-unknowns ", "))
                  (nngmail--timed (format "sync: fetch %d changed messages" (length new-ids))
                    (nngmail--fetch-in-chunks account new-ids "fetching changed messages" t)))
              (nngmail--log "%d history records applied, nothing to fetch" records))
            (if (or new-ids (> records 0))
                (nngmail--progress account nil)
              (nngmail--message 6 "%s: up to date" (nngmail-account-name account))))
        (nngmail-http-error
         (if (equal (cadr err) 404)
             ;; History expired; Gmail says start over.
             (progn (nngmail--message 5 "history expired, doing a full sync")
                    (nngmail--full-sync account))
           (signal (car err) (cdr err))))))))

(defun nngmail--apply-history-page (account history next-token response-history-id carried)
  "Apply one page of history.list records to ACCOUNT and checkpoint.
HISTORY is the page's record list, NEXT-TOKEN its nextPageToken (nil on
the last page), RESPONSE-HISTORY-ID the mailbox's current id from the
response, CARRIED the message ids earlier pages left to fetch.  Return
CARRIED plus this page's ids to fetch.

Everything up to the page's last record is applied inside one
transaction, and the history id and the fetch list are saved with it, so
an interruption resumes from this page instead of re-reading all of
them.  Gmail orders history by id; the id of the page's last record is
what has been applied, while RESPONSE-HISTORY-ID is the mailbox's
current one and is only safe to adopt on the last page."
  (let ((ids carried))
    (nngmail--with-transaction account
      (dolist (h history)
        (setq ids (nconc ids (nngmail--apply-history account h))))
      (let* ((last-applied (and history (alist-get 'id (car (last history)))))
             (checkpoint (if next-token
                             (or last-applied (nngmail-account-history-id account))
                           (or response-history-id last-applied (nngmail-account-history-id account)))))
        (setf (nngmail-account-history-id account) checkpoint
              (nngmail-account-pending account)
              (and ids (list "fetching changed messages" t (seq-uniq ids)))
              (nngmail-account-dirty account) t))
      (nngmail--save account))
    ids))

(defun nngmail--pending-history-ids (account)
  "Message ids an interrupted history pass still had to fetch, and clear them.
They are carried into the current pass so the checkpoint keeps them.
ACCOUNT is the nngmail account."
  (pcase (nngmail-account-pending account)
    (`("fetching changed messages" ,_force ,ids)
     (setf (nngmail-account-pending account) nil)
     (copy-sequence ids))))

(defun nngmail--history-progress (account pages records started &optional force)
  "Show a history pass in the status line: PAGES and RECORDS since STARTED.
History has no total, so the bar carries the record count and the extra
column the page rate and elapsed time.  FORCE redraws at once (the
synchronous loop has no timer to do it).
ACCOUNT is the nngmail account."
  (let* ((elapsed (- (float-time) started))
         (rate (if (> elapsed 0.5) (/ pages elapsed) 0))
         (nngmail--progress-extra
          (string-join
           (delq nil (list (format "%d page%s" pages (if (= pages 1) "" "s"))
                           (and (> rate 0) (format "%.1f pages/s" rate))
                           (and (>= elapsed nngmail-eta-after)
                                (format "%s so far" (nngmail--fmt-duration elapsed)))))
           "  ")))
    (nngmail--progress account "applying changes" records nil force)))

(defun nngmail--refresh-mailbox-total (account)
  "Fetch messagesTotal for ACCOUNT (1 quota unit) and remember it."
  (ignore-errors
    (when-let* ((p (nngmail--get account "profile"))
                (n (alist-get 'messagesTotal p)))
      (setf (nngmail-account-mailbox-total account) n))))

(defun nngmail--backfill (account count &optional query label-ids phase)
  "Index up to COUNT older messages of ACCOUNT not yet known locally.
COUNT nil means no limit.  QUERY is a Gmail search; LABEL-IDS restricts
to those labels.  Return the number of messages newly indexed.  PHASE
labels the progress display.

Works one listing page (500 ids) at a time: list, fetch the unknown
ones, checkpoint, next page.  The page token is kept in the account's
`backfill' slot, so if the run is interrupted (network, quota abort,
\\[keyboard-quit], Emacs exit) calling this again with the same COUNT/QUERY/LABEL-IDS
continues from that page instead of rescanning from the newest.

Note that within a page ids are newest-first; we number oldest-first
inside the page, but successive pages go further back in time, so
article numbers from a long backfill descend in time as they ascend in
number.  Sort summaries by date if that matters."
  (let* ((known (nngmail-account-id->num account))
         (phase (or phase "backfilling"))
         (spec (list query label-ids))
         (resume (nngmail-account-backfill account))
         ;; Resume only if the previous run had the same shape.
         (page (and resume (equal (plist-get resume :spec) spec)
                    (plist-get resume :page)))
         (found (or (and page (plist-get resume :found)) 0))
         (scanned (or (and page (plist-get resume :scanned)) 0))
         (fetched 0)
         ;; Denominator for the mailbox-wide bar: the whole mailbox for an
         ;; unfiltered scan, else Gmail's estimate of the filtered result
         ;; set (resultSizeEstimate, refined as we page).
         (whole (and (null query) (null label-ids)))
         (estimate nil)
         (next-page-handle nil)
         done)
    (when page
      (nngmail--message 5 "%s: resuming backfill from page %d (%d found so far)"
                        (nngmail-account-name account) (1+ (/ scanned 500)) found))
    (when whole (nngmail--refresh-mailbox-total account))
    (unwind-protect
        (while (not done)
          ;; Show the listing itself as activity: the page bar tracks the
          ;; scan against the best-known total while no chunk is fetching.
          (let ((nngmail--progress-extra
                 (list scanned found
                       (if whole (nngmail-account-mailbox-total account) estimate))))
            (nngmail--progress account (concat phase ", listing") scanned
                               (if whole (nngmail-account-mailbox-total account) estimate)))
          (let* ((list-params (lambda (token)
                                (append `(("maxResults" . 500) ("includeSpamTrash" . "true"))
                                        (when query `(("q" . ,query)))
                                        (mapcar (lambda (l) (cons "labelIds" l)) label-ids)
                                        (when token `(("pageToken" . ,token))))))
                 (resp (if next-page-handle
                           (nngmail--get-async-result next-page-handle)
                         (nngmail--get account "messages" (funcall list-params page))))
                 (msgs (alist-get 'messages resp))
                 (ids nil))
            (setq next-page-handle nil)
            ;; Start listing the next page now, so it is answered while
            ;; this page's headers are being fetched.
            (when-let* ((token (alist-get 'nextPageToken resp)))
              (unless (and count (>= found count))
                (setq next-page-handle
                      (nngmail--get-async account "messages" (funcall list-params token)))))
            ;; Gmail's estimate can be low early on and never below what
            ;; we have already seen; keep the max.
            (when-let* ((e (alist-get 'resultSizeEstimate resp)))
              (setq estimate (max (or estimate 0) e (+ scanned (length msgs)))))
            (dolist (m msgs)
              (let ((id (alist-get 'id m)))
                (cl-incf scanned)
                (unless (gethash id known)
                  (when (or (null count) (< found count))
                    (push id ids)
                    (cl-incf found)))))
            ;; `ids' was pushed newest-first, so it is already oldest-first.
            (let ((nngmail--progress-extra
                   (list scanned found
                         (if whole (nngmail-account-mailbox-total account) estimate))))
              (when ids
                (nngmail--fetch-in-chunks account ids phase)
                (setq fetched (+ fetched (length ids))))
              (setq page (alist-get 'nextPageToken resp))
              (nngmail--progress account (concat phase ", listing") scanned
                                 (if whole (nngmail-account-mailbox-total account) estimate)))
            (if (or (null page) (null msgs) (and count (>= found count)))
                (setq done t)
              ;; Checkpoint the listing position (throttled: the save
              ;; rewrites the whole state file).
              (setf (nngmail-account-backfill account)
                    (list :spec spec :page page :found found :scanned scanned)
                    (nngmail-account-dirty account) t)
              (nngmail--checkpoint account))))
      ;; Normal completion clears the checkpoint; abnormal exit keeps it.
      (pcase next-page-handle
        (`(async ,_ ,_ ,_ ,proc) (when (process-live-p proc) (delete-process proc))))
      (when done
        (setf (nngmail-account-backfill account) nil
              (nngmail-account-dirty account) t))
      (nngmail--save account))
    fetched))

;;;###autoload
(defun nngmail-backfill-status (server)
  "Show whether an interrupted backfill is waiting to be resumed on SERVER."
  (interactive
   (list (nngmail--read-server)))
  (let* ((account (nngmail--account server))
         (bf (nngmail-account-backfill account)))
    (if (null bf)
        (message "nngmail %s: no interrupted backfill" server)
      (let ((spec (plist-get bf :spec)))
        (message "nngmail %s: %s paused, %s scanned, %s new; run nngmail-backfill again to continue"
                 server
                 (if (eq (car spec) 'imap)
                     (format "IMAP import of %s" (nth 1 spec))
                   (format "backfill%s at page %d"
                           (if (car spec) (format " for %S" (car spec)) "")
                           (1+ (/ (plist-get bf :scanned) 500))))
                 (nngmail--fmt-num (plist-get bf :scanned))
                 (nngmail--fmt-num (plist-get bf :found)))))))

(defvar nngmail--imap-unavailable-reason)

(defun nngmail--imap-usable-p ()
  "Non-nil unless IMAP was found unusable this session.
\(See `nngmail--imap-unavailable-reason' and `nngmail-imap-retry'.)"
  (null nngmail--imap-unavailable-reason))

(defun nngmail-configured-servers ()
  "Names of every nngmail server Gnus knows about, opened or not.
Looks in `gnus-select-method', `gnus-secondary-select-methods' and
`gnus-server-alist' (servers added with `^' in the server buffer live
only there), plus any account already open."
  (let (names)
    (dolist (m (cons gnus-select-method gnus-secondary-select-methods))
      (when (eq (car-safe m) 'nngmail) (push (cadr m) names)))
    (dolist (entry (and (boundp 'gnus-server-alist) gnus-server-alist))
      (let ((m (cdr entry)))
        (when (eq (car-safe m) 'nngmail) (push (cadr m) names))))
    (maphash (lambda (name _) (push name names)) nngmail--accounts)
    (sort (seq-uniq (delq nil names)) #'string<)))

(defun nngmail--read-server (&optional prompt)
  "Ask for an nngmail server name, offering every configured one.
PROMPT replaces the default prompt."
  (let ((servers (nngmail-configured-servers)))
    (cond ((null servers) (user-error "No nngmail server configured (add one with ^ in the server buffer, or in gnus-secondary-select-methods)"))
          ((cdr servers) (completing-read (or prompt "nngmail server: ") servers nil t nil nil (car servers)))
          (t (car servers)))))

(defun nngmail--backfill-servers ()
  "Choices for the backfill server prompt: (LABEL ACCOUNT-NAME . TRANSPORT).
Every account is offered twice, as \"NAME (IMAP)\" and \"NAME (REST)\".
IMAP is listed first (it is the fast one) unless it was found unusable,
in which case its label says so and REST comes first."
  (let (out)
    (dolist (name (nngmail-configured-servers))
      (let ((imap (cons (if (nngmail--imap-usable-p)
                            (format "%s (IMAP: fast, whole mailbox or a count)" name)
                          (format "%s (IMAP: unavailable, see nngmail-imap-retry)" name))
                        (cons name 'imap)))
            (rest (cons (format "%s (REST: Gmail queries, time windows; ~3,000 msg/min)" name)
                        (cons name 'rest))))
        (if (nngmail--imap-usable-p)
            (setq out (append out (list imap rest)))
          (setq out (append out (list rest imap))))))
    out))

(defun nngmail--read-backfill-server ()
  "Ask which account and transport to backfill; return (ACCOUNT-NAME . TRANSPORT)."
  (let* ((choices (nngmail--backfill-servers))
         (labels (mapcar #'car choices))
         (table (lambda (string pred action)
                  (if (eq action 'metadata)
                      '(metadata (display-sort-function . identity)
                                 (cycle-sort-function . identity))
                    (complete-with-action action labels string pred))))
         (pick (completing-read "Backfill: " table nil t nil nil (car labels))))
    (or (cdr (assoc pick choices))
        ;; A bare account name typed in: default to its first listed transport.
        (cdr (seq-find (lambda (c) (equal (cadr c) pick)) choices))
        (user-error "nngmail: No such backfill server %S" pick))))

(defun nngmail--imap-warn-fallback (server reason pace)
  "Warn that SERVER's backfill falls back to REST because of REASON.
PACE is the normalised pacing for the run: nil, `adaptive' or an integer."
  (let ((msg (format "%s: %s.  Falling back to the REST API for this backfill: about 3,000 messages a minute at most%s.  Enable IMAP for the account to make bulk imports fast; M-x nngmail-imap-retry after you do."
                     server reason
                     (pcase pace
                       ('nil " (no pacing)")
                       ((pred integerp) (format " (paced at %s units/min)" (nngmail--fmt-num pace)))
                       (_ " (adaptive pacing)")))))
    (nngmail--log "%s" msg)
    (display-warning 'nngmail msg :warning)))

;;;###autoload
(defun nngmail-imap-retry ()
  "Let the next backfill try IMAP again after it was found unusable."
  (interactive)
  (if nngmail--imap-unavailable-reason
      (progn (setq nngmail--imap-unavailable-reason nil)
             (message "nngmail: IMAP will be tried again on the next backfill"))
    (message "nngmail: IMAP was not marked unavailable")))

;;;###autoload
(defun nngmail-backfill (server &optional count query pace transport)
  "Index older messages for nngmail SERVER that the initial sync skipped.
COUNT is how many not-yet-indexed messages to add; nil means no limit.
QUERY is a Gmail search restricting what is considered; nil means the
whole mailbox including Spam and Trash.

TRANSPORT is `imap' or `rest' (nil means `rest' unless QUERY is nil, in
which case `imap' when it is usable).  Interactively the first prompt
offers each account twice, as \"NAME (IMAP)\" and \"NAME (REST)\".
IMAP has no per-message quota and imports a whole mailbox at network
speed, but it cannot evaluate Gmail search syntax, so on the IMAP
server only counts and \"all\" are offered, and no query is asked.
REST accepts everything and is paced.

Interactively the first prompt takes either a number of messages
\(\"1000\"), a time span (\"30d\", \"6m\", \"2y\", \"3 weeks\") which
becomes a newer_than: clause, or a date (\"2024-01-01\", \"2024-06\")
which becomes an after: clause; a bare number is always a count, never
a year.  Empty or \"all\" means everything.  A time window is far cheaper
than a count on a large mailbox, because Gmail filters server-side and
the scan never touches older messages.  The second prompt takes an
additional Gmail query (\"label:inbox\", \"from:alice\") that is combined
with the time window.

PACE controls client-side pacing for this run only: `adaptive' (the
default: full speed until Gmail pushes back, then pace at the limit),
an integer number of quota units per minute, or `none' for no pacing.
Interactively it is chosen from `nngmail-backfill-pace-choices' unless
`nngmail-backfill-pace-default' says otherwise; a number can also be
typed.  Everyday nngmail use is never paced, only backfills get near
the limit.

A big unlimited backfill can be interrupted; it resumes on the next
call.  Answer W at any quota prompt to let it run."
  (interactive
   (pcase-let* ((`(,server . ,transport) (nngmail--read-backfill-server))
                (how (nngmail--read-how-much transport))
                (count (car how))
                (time-q (cdr how))
                (q (if (eq transport 'imap)
                       nil
                     (string-trim
                      (read-string
                       (format "Gmail query%s (empty = %s): "
                               (if time-q (format " to combine with %s" time-q) "")
                               (if time-q "just the time window" "whole mailbox, incl. Spam/Trash"))))))
                (q (if (or (null q) (string-empty-p q) (member (downcase q) '("everything" "all" "*"))) nil q))
                (query (cond ((and time-q q) (concat time-q " " q))
                             (time-q time-q)
                             (t q))))
     (list server count query
           (if (and (eq nngmail-backfill-pace-default 'ask) (eq transport 'rest))
               (nngmail--read-pace)
             nngmail-backfill-pace-default)
           transport)))
  (let* ((account (nngmail--account server))
         (count (and count (> count 0) count))
         (transport (or transport
                        (if (and (null query) (nngmail--imap-usable-p)) 'imap 'rest)))
         ;; IMAP asked for after it was found unusable: the server list
         ;; already says so, do the run over REST without trying again.
         (use-imap (and (eq transport 'imap) (nngmail--imap-usable-p)))
         ;; nil (omitted from Lisp) means the default, adaptive; `none'
         ;; is the explicit way to switch pacing off.
         (pace (pcase (nngmail--backfill-pace-for pace)
                 ('nil 'adaptive)
                 ('none nil)
                 (p p)))
         ;; Pacing is scoped to this backfill: everyday use stays unpaced.
         (nngmail-quota-pacing (pcase pace
                                 ('nil nil)
                                 ('adaptive 'adaptive)
                                 ((pred integerp) 'always)
                                 (_ nngmail-quota-pacing)))
         (nngmail-quota-per-minute (if (integerp pace) pace nngmail-quota-per-minute))
         (nngmail--quota-enforced nil)
         (nngmail--quota-log nil)
         (nngmail--throttled nil))
    (nngmail--message 5 "%s: backfill%s%s, %s"
                      server
                      (if count (format " of up to %s" (nngmail--fmt-num count)) " (unlimited)")
                      (if query (format " matching %S" query) "")
                      (if use-imap
                          "over IMAP (no quota)"
                        (pcase pace
                          ('nil "pacing: none")
                          ('adaptive (format "pacing: adaptive (%s/min once Gmail pushes back)"
                                             (nngmail--fmt-num nngmail-quota-per-minute)))
                          (_ (format "pacing: %s units/min" (nngmail--fmt-num pace))))))
    (when (and use-imap query)
      (user-error "nngmail: IMAP cannot apply the Gmail query %S; use the REST server" query))
    (let ((n (condition-case err
                 (if use-imap
                     (condition-case imap-err
                         ;; Unlimited: oldest first, so article numbers
                         ;; ascend with time like the rest of the index.
                         ;; Counted: newest first, so "1,000 more" means
                         ;; the most recent thousand.
                         (nngmail--imap-import account count "backfilling (IMAP)" (and count t) t)
                       (nngmail-imap-unavailable
                        ;; Degrade: say why, remember it, and do the same
                        ;; job over REST with the pacing the user chose.
                        (setq nngmail--imap-unavailable-reason (cadr imap-err))
                        (nngmail--imap-close)
                        (nngmail--imap-warn-fallback server (cadr imap-err) pace)
                        (nngmail--backfill account count query nil "backfilling")))
                   (nngmail--backfill account count query nil "backfilling"))
               (nngmail-aborted
                (nngmail--message 5 "%s: backfill paused; run nngmail-backfill again to continue"
                                  server)
                nil)
               (nngmail-busy
                (nngmail--message 5 "%s: a sync is already talking to Gmail; run nngmail-backfill again when it finishes"
                                  server)
                nil)
               (nngmail-error
                (nngmail--message 3 "%s: backfill stopped: %s (run nngmail-backfill again to continue)"
                                  server (error-message-string err))
                nil))))
      (when n
        (nngmail--progress account nil)
        (nngmail--message 5 "%s: backfilled %s message%s%s"
                          server (nngmail--fmt-num n) (if (= n 1) "" "s")
                          (cond (query (format " matching %S" query))
                                (count "")
                                (t " (whole mailbox reached)"))))
      n)))

;;;; Fetching more on demand (C-u N RET)

(defcustom nngmail-fetch-on-demand t
  "If non-nil, entering a group with a numeric prefix backfills that label.
`C-u 1000 RET' on an nngmail group makes sure the newest 1000 messages
carrying the group's label are indexed (fetching any that aren't) before
the summary is built.  Set to nil to get plain Gnus behaviour, where the
prefix only selects among already-indexed articles."
  :type 'boolean)

(defun nngmail--ensure-newest (account group want)
  "Make sure the newest WANT messages with GROUP's label are indexed.
Return the number newly indexed.  Cheap when nothing is missing: one
messages.list page per 500 wanted, no metadata fetches.
ACCOUNT is the nngmail account."
  (let ((label (nngmail--group-label account group)))
    (if (null label)
        ;; All-Mail: everything except spam/trash.
        (nngmail--backfill account want "-in:spam -in:trash" nil
                           (format "fetching newest %d" want))
      ;; Walk Gmail's newest WANT for the label, stop early once we hit
      ;; a run of known ids that reaches WANT.
      (let ((known (nngmail-account-id->num account))
            (missing nil) (seen 0) (page nil) done)
        (while (not done)
          (let* ((resp (nngmail--get account "messages"
                                     (append `(("maxResults" . ,(min 500 (- want seen)))
                                               ("labelIds" . ,label)
                                               ("includeSpamTrash" . "true"))
                                             (when page `(("pageToken" . ,page))))))
                 (msgs (alist-get 'messages resp)))
            (dolist (m msgs)
              (cl-incf seen)
              (let* ((id (alist-get 'id m))
                     (num (gethash id known)))
                (unless (and num (nngmail--group-member-p account group num))
                  (push id missing))))
            (setq page (alist-get 'nextPageToken resp))
            (nngmail--progress account (format "checking newest %s in %s" (nngmail--fmt-num want) group) seen want)
            (when (or (null page) (null msgs) (>= seen want))
              (setq done t))))
        (when missing
          (setq missing (nreverse missing))
          (nngmail--fetch-in-chunks account missing
                                    (format "fetching %s older messages for %s" (nngmail--fmt-num (length missing)) group)
                                    t)
          (nngmail--save account))
        (length missing)))))

(defun nngmail--maybe-fetch-on-demand (group read-all)
  "If READ-ALL asks for N articles of nngmail GROUP, backfill first."
  (when (and nngmail-fetch-on-demand
             (numberp read-all)
             (> read-all 0))
    (let ((method (gnus-find-method-for-group group)))
      (when (eq (car method) 'nngmail)
        (let* ((account (nngmail--account (cadr method)))
               (short (gnus-group-real-name group))
               (have (nngmail--group-size account short)))
          (when (< have read-all)
            (condition-case err
                (let ((n (nngmail--ensure-newest account short read-all)))
                  (when (> n 0)
                    ;; Refresh the info so the new articles are visible now.
                    (when-let* ((info (gnus-get-info group)))
                      (nngmail--update-info account short info))
                    (nngmail--message 5 "%s: fetched %d older message%s"
                                      short n (if (= n 1) "" "s"))))
              (nngmail-aborted
               (nngmail--message 5 "%s: fetch aborted; showing what is indexed" short))
              (nngmail-error
               (nngmail--message 3 "%s: fetch failed: %s" short (error-message-string err))))))))))

(defun nngmail--newest-locals (group n)
  "The N newest (N < 0: oldest) existing local numbers of nngmail GROUP, ascending.
Gnus's own choice for a numeric prefix is the top N of the active range,
and in a group that messages leave (INBOX, most of all) many of those
numbers are retired, so fewer than N articles would show."
  (let ((method (gnus-find-method-for-group group)))
    (when (eq (car method) 'nngmail)
      (when-let* ((account (nngmail--account (cadr method)))
                  (g (nngmail--group account (gnus-group-real-name group))))
        ;; Walk the counter down (or up) collecting live numbers; local
        ;; numbers are dense, so this touches about N entries, not the
        ;; whole group (All Mail is millions).
        (let ((live (nngmail-group-local->num g))
              (want (abs n))
              (out nil))
          (if (< n 0)
              (let ((local 1) (high (1- (nngmail-group-next g))))
                (while (and (<= local high) (< (length out) want))
                  (when (gethash local live) (push local out))
                  (cl-incf local))
                (nreverse out))
            (let ((local (1- (nngmail-group-next g))))
              (while (and (>= local 1) (< (length out) want))
                (when (gethash local live) (push local out))
                (cl-decf local))
              out)))))))

(defun nngmail--select-newsgroup-advice (orig group &optional read-all select-articles)
  "Around `gnus-select-newsgroup' of GROUP: a numeric prefix is a fetch request.
Fetch that many, then select that many existing articles (see
`nngmail--newest-locals').  ORIG is the advised function; READ-ALL and
SELECT-ARTICLES are its arguments."
  (nngmail--maybe-fetch-on-demand group read-all)
  (when (and (numberp read-all) (not (zerop read-all)) (null select-articles))
    (when-let* ((chosen (nngmail--newest-locals group read-all)))
      (setq select-articles chosen)))
  (funcall orig group read-all select-articles))

(advice-add 'gnus-select-newsgroup :around #'nngmail--select-newsgroup-advice)

(defcustom nngmail-mark-propagation 'on-exit
  "When read marks made in a summary reach Gmail.
`on-exit' (the default): plain Gnus semantics; marks are sent when you
leave the summary with `q' and discarded with `Q'.  `immediate': as
soon as Gnus records them (coalesced on a short idle timer), like an
IMAP client; `Q' then undoes the ones made in that summary.  Either way
the group's info is frozen while its summary is open, so a sync in
between cannot lose marks.  Ticks always travel on exit."
  :type '(choice (const :tag "When leaving the summary (Gnus)" on-exit)
                 (const :tag "As they happen (IMAP-like)" immediate)))

(defun nngmail--summary-open-p (group)
  "Non-nil while a summary for GROUP (full name) is open and not exiting.
Looks at every live summary buffer, not only `gnus-summary-buffer', so
it is right from timers and from other groups' code paths too."
  (catch 'open
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (derived-mode-p 'gnus-summary-mode)
                     (equal gnus-newsgroup-name group)
                     (bound-and-true-p gnus-newsgroup-prepared)
                     (not (bound-and-true-p gnus-group-is-exiting-p)))
            (throw 'open t)))))
    nil))

;;;; Duplicate suppression and server marks
;;
;; With `gnus-suppress-duplicates', Gnus remembers the Message-IDs of
;; articles you have read and, when the same Message-ID shows up in another
;; group, marks that copy read with `gnus-duplicate-mark' (M).  It does so
;; by pushing onto `gnus-newsgroup-reads' directly, which bypasses
;; `request-set-mark', so the backend never hears about it.  For most
;; backends a "duplicate" is a genuinely separate article and that is fine.
;; For nngmail the "duplicate" is the *same* Gmail message seen through a
;; different label, and leaving it unread at Gmail means the next sync
;; resurrects it as unread everywhere -- the M marks never stick.
;;
;; So after Gnus's suppression runs, push those reads to Gmail ourselves.

(defun nngmail--dup-suppress-advice (&rest _)
  "After `gnus-dup-suppress-articles', propagate M marks to Gmail.
Under `on-exit' propagation the M marks ride the exit diff like any
other read, so nothing is done here."
  (when (and (eq nngmail-mark-propagation 'immediate)
             (boundp 'gnus-newsgroup-name) gnus-newsgroup-name
             (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nngmail))
    (let ((dups (delq nil (mapcar (lambda (r)
                                    (and (eql (cdr r) gnus-duplicate-mark) (car r)))
                                  gnus-newsgroup-reads))))
      (when dups
        (nngmail--log "propagating %d duplicate-suppressed read mark(s) in %s"
                      (length dups) gnus-newsgroup-name)
        (gnus-request-set-mark gnus-newsgroup-name
                               (list (list (gnus-compress-sequence (sort dups #'<))
                                           'add '(read))))))))

(with-eval-after-load 'gnus-dup
  (advice-add 'gnus-dup-suppress-articles :after #'nngmail--dup-suppress-advice))

;;;; Mark propagation: when a read mark reaches Gmail
;;
;; Gnus's own model: marks made in a summary live in the summary until
;; you leave it; `q' diffs them against the group info and sends the
;; difference to the backend (`gnus-update-read-articles' ->
;; `request-set-mark'); `Q' sends nothing.  That relies on the info
;; being a record of what Gnus last wrote.  nngmail rebuilds the info
;; from Gmail's labels on every sync, including syncs while a summary is
;; open, so without care the exit diff would compare against an info
;; that already says "read" and send nothing.
;;
;; `nngmail-mark-propagation' picks one of two behaviours, both built on
;; the same fix for that: while a summary for a group is open, its info
;; is frozen (`nngmail--update-info' leaves it alone), so the exit diff
;; always has the base from summary entry.
;;
;;  - `on-exit' (Gnus semantics, the default): nothing else.  `q' pushes the diff,
;;    `Q' pushes nothing; `c' in *Group* pushes at once (there is no
;;    summary to defer to).  A crash with a summary open loses that
;;    summary's marks, as with any Gnus backend.
;;
;;  - `immediate' (like an IMAP client): in addition, every
;;    read/unread change is sent the moment Gnus records it, coalesced on
;;    a short idle timer.  The exit diff then finds nothing left.  So
;;    that `Q' still means something, each push made while a summary is
;;    open is journaled with the article's prior UNREAD state and `Q'
;;    reverts the journal at Gmail.  Ticks are not pushed eagerly in
;;    either mode: Gnus sends them through the exit diff.

(defvar nngmail--pending-marks nil
  "Alist of (GROUP . ((ARTICLE . ACTION) ...)) awaiting a coalesced send.
ACTION is `read' or `unread'.")

(defvar nngmail--pending-marks-timer nil)

(defcustom nngmail-mark-flush-delay 0.5
  "Seconds of idle time before queued mark changes are sent to Gmail.
Lets a catch-up over hundreds of articles go out as one request."
  :type 'number)

(defun nngmail--queue-mark (group article action)
  "Queue ACTION (`read' or `unread') for ARTICLE in GROUP; schedule a flush.
Under `on-exit' propagation this does nothing: the exit diff carries the
change."
  (when (eq nngmail-mark-propagation 'immediate)
    (nngmail--queue-mark-1 group article action)))

(defun nngmail--queue-mark-1 (group article action)
  "Queue ACTION (`read' or `unread') on ARTICLE of GROUP for the next push."
  (let ((cell (assoc group nngmail--pending-marks)))
    (if cell
        (setf (alist-get article (cdr cell)) action)
      (push (list group (cons article action)) nngmail--pending-marks)))
  (when nngmail--pending-marks-timer
    (cancel-timer nngmail--pending-marks-timer))
  (setq nngmail--pending-marks-timer
        (run-with-idle-timer nngmail-mark-flush-delay nil #'nngmail-flush-marks)))

(defcustom nngmail-announce-cross-group-marks t
  "If non-nil, say when a mark change also affected other groups.
In Gmail a message is one object under several labels, so reading it in
one nngmail group reads it everywhere.  When that happens the echo area
says so (also marked read in INBOX, dev/review), so the effect on groups
you are not looking at is not a surprise later."
  :type 'boolean)

(defun nngmail--announce-cross-group (account group articles action)
  "Report other groups affected by ACTION on ARTICLES (local numbers) of GROUP.
ACCOUNT is the nngmail account."
  (when (and nngmail-announce-cross-group-marks articles)
    (let ((here (gnus-group-real-name group))
          (others (make-hash-table :test #'equal)))
      (dolist (n (nngmail--locals->nums account (gnus-group-real-name group) articles))
        (dolist (g (nngmail--message-groups account (gethash n (nngmail-account-labels account))))
          (unless (or (equal g here) (equal g nngmail-all-mail-group))
            (cl-incf (gethash g others 0)))))
      (when (> (hash-table-count others) 0)
        (let (rows)
          (maphash (lambda (g n) (push (if (> n 1) (format "%s (%d)" g n) g) rows)) others)
          (nngmail--message 5 "also marked %s in %s"
                            (if (eq action 'read) "read" "unread")
                            (mapconcat #'identity (sort rows #'string<) ", ")))))))

(defun nngmail-flush-marks ()
  "Send all queued mark changes to Gmail now.
When called from the idle timer while a sync has a request out, the
flush is postponed by `nngmail-mark-flush-delay' rather than sending a
second request on top of the first (Gmail counts that against its
concurrency limit) or blocking the timer."
  (interactive)
  (setq nngmail--pending-marks-timer nil)
  (if (and nngmail--batch-in-flight (not (called-interactively-p 'any)))
      (setq nngmail--pending-marks-timer
            (run-with-idle-timer nngmail-mark-flush-delay nil #'nngmail-flush-marks))
    (nngmail--flush-marks-now)))

(defvar nngmail--mark-journal nil
  "What eager mark pushes changed in the open summaries.
Entries are (GROUP . ((NUM . WAS-UNREAD) ...)).
NUM is the server number; WAS-UNREAD the article's UNREAD state before
the first push touched it in this summary session.  `Q'
\(`gnus-summary-exit-no-update') restores it; `q' forgets it.")

(defun nngmail--journal-marks (account group locals)
  "Remember the UNREAD state of LOCALS (local numbers) of GROUP before a push.
Only while a summary for GROUP is open: that is the span `Q' can discard.
The first record for an article wins, so toggling twice still restores
the state at summary entry.
ACCOUNT is the nngmail account."
  (when (nngmail--summary-open-p group)
    (let* ((short (gnus-group-real-name group))
           (labels (nngmail-account-labels account))
           (cell (or (assoc group nngmail--mark-journal)
                     (car (push (list group) nngmail--mark-journal)))))
      (dolist (num (nngmail--locals->nums account short locals))
        (unless (assq num (cdr cell))
          (push (cons num (and (member "UNREAD" (gethash num labels)) t)) (cdr cell)))))))

(defun nngmail--forget-journal (group)
  "Drop the mark journal of GROUP (the summary was left with `q')."
  (setq nngmail--mark-journal (assoc-delete-all group nngmail--mark-journal)))

(defun nngmail--revert-journal (group)
  "Put back the UNREAD state the journal recorded for GROUP, at Gmail.
This is what `Q' means for an nngmail group: the reads pushed while the
summary was open are undone.  Returns how many articles were restored."
  (let ((entries (cdr (assoc group nngmail--mark-journal)))
        (method (gnus-find-method-for-group group))
        (restored 0))
    (when (and entries (eq (car-safe method) 'nngmail))
      (let* ((account (nngmail--account (cadr method)))
             (labels (nngmail-account-labels account))
             make-unread make-read)
        (pcase-dolist (`(,num . ,was-unread) entries)
          (let ((is-unread (and (member "UNREAD" (gethash num labels)) t)))
            (cond ((and was-unread (not is-unread)) (push num make-unread))
                  ((and (not was-unread) is-unread) (push num make-read)))))
        (condition-case err
            (progn
              (when make-unread (nngmail--modify account make-unread '("UNREAD") nil))
              (when make-read (nngmail--modify account make-read nil '("UNREAD"))))
          (nngmail-error
           (nngmail--message 3 "failed to revert marks for %s: %s" group (error-message-string err))))
        (setq restored (+ (length make-unread) (length make-read)))
        (when (> restored 0)
          (nngmail--message 5 "%s: Q discarded %d read mark%s at Gmail"
                            (gnus-group-real-name group) restored (if (= restored 1) "" "s")))))
    (nngmail--forget-journal group)
    restored))

(defun nngmail--summary-exit ()
  "On leaving a summary: `q' flushes queued marks; `Q' drops them and reverts.
Runs from `gnus-summary-prepare-exit-hook', where
`gnus-group-is-exiting-without-update-p' tells the two apart."
  (let ((group gnus-newsgroup-name))
    (if (bound-and-true-p gnus-group-is-exiting-without-update-p)
        (progn
          ;; Nothing queued for this group may go out.
          (setq nngmail--pending-marks (assoc-delete-all group nngmail--pending-marks))
          (nngmail-flush-marks)          ; other groups' queues, if any
          (nngmail--revert-journal group))
      (nngmail-flush-marks)
      (nngmail--forget-journal group))))

(defun nngmail--flush-marks-now ()
  "Send every queued mark change to Gmail."
  (let ((pending nngmail--pending-marks))
    (setq nngmail--pending-marks nil)
    (pcase-dolist (`(,group . ,changes) pending)
      (let ((read (delq nil (mapcar (lambda (c) (and (eq (cdr c) 'read) (car c))) changes)))
            (unread (delq nil (mapcar (lambda (c) (and (eq (cdr c) 'unread) (car c))) changes))))
        (condition-case err
            (progn
              (when-let* ((m (gnus-find-method-for-group group))
                          (account (and (eq (car m) 'nngmail) (gethash (cadr m) nngmail--accounts))))
                (nngmail--journal-marks account group (append read unread)))
              (gnus-request-set-mark
               group
               (delq nil (list (and read (list (gnus-compress-sequence (sort read #'<)) 'add '(read)))
                               (and unread (list (gnus-compress-sequence (sort unread #'<)) 'del '(read))))))
              (when-let* ((m (gnus-find-method-for-group group))
                          (account (and (eq (car m) 'nngmail) (gethash (cadr m) nngmail--accounts))))
                (when read (nngmail--announce-cross-group account group read 'read))
                (when unread (nngmail--announce-cross-group account group unread 'unread))))
          (error
           (nngmail--message 3 "failed to push marks for %s: %s" group (error-message-string err))))))))

(defun nngmail--nngmail-group-p (group)
  "Non-nil if GROUP (full name) is served by nngmail."
  (and (stringp group)
       (eq (car-safe (gnus-find-method-for-group group)) 'nngmail)))

(defun nngmail--mark-read-advice (&rest _)
  "After `gnus-summary-mark-article-as-read': queue the read for Gmail."
  (when (nngmail--nngmail-group-p gnus-newsgroup-name)
    (let ((article (gnus-summary-article-number)))
      (when (and article (> article 0))
        (nngmail--queue-mark gnus-newsgroup-name article 'read)))))

(defun nngmail--mark-article-advice (article &optional mark _no-expire)
  "After `gnus-mark-article-as-read' / `-unread' with explicit ARTICLE and MARK."
  (when (and (nngmail--nngmail-group-p gnus-newsgroup-name)
             (integerp article) (> article 0))
    (nngmail--queue-mark gnus-newsgroup-name article
                         (if (memq article gnus-newsgroup-unreads) 'unread 'read))
    (ignore mark)))

(defun nngmail--mark-unread-advice (&rest _)
  "After `gnus-summary-mark-article-as-unread': queue the unread for Gmail.
Only when the article really ended up unread (not ticked/dormant, which
Gnus treats as read for the backend's purposes)."
  (when (nngmail--nngmail-group-p gnus-newsgroup-name)
    (let ((article (gnus-summary-article-number)))
      (when (and article (> article 0) (memq article gnus-newsgroup-unreads))
        (nngmail--queue-mark gnus-newsgroup-name article 'unread)))))

;; `c' / `C' in *Group* (`gnus-group-catchup') never opens the summary; it
;; derives "unread" from the info's read range and diffs against the same
;; info.  That range drifts more "read" than Gmail (summary visits, M marks,
;; any earlier lost push), so the diff comes out empty and nothing is sent
;; even though Gmail still shows the articles unread.  nngmail knows the
;; truth -- its label table -- so catch up from that instead, then let
;; Gnus do its bookkeeping.

(defun nngmail--group-catchup-advice (orig group &optional all)
  "Around `gnus-group-catchup' on GROUP: mark everything Gmail has UNREAD as read.
For nngmail groups, push read for every member still carrying UNREAD
\(and, with ALL, unstar the starred ones) before Gnus updates the info,
so the result matches what a user expects of a catch-up regardless of
how stale the info is.  Other backends pass straight through.
ORIG is the advised function."
  (if (not (nngmail--nngmail-group-p group))
      (funcall orig group all)
    (let* ((method (gnus-find-method-for-group group))
           (account (nngmail--account (cadr method)))
           (short (gnus-group-real-name group))
           (labels (nngmail-account-labels account))
           (unread nil) (starred nil))
      ;; Local numbers: that is what `nngmail-request-set-mark' takes.
      (when-let* ((g (nngmail--group account short)))
        (maphash (lambda (local n)
                   (let ((l (gethash n labels)))
                     (when (member "UNREAD" l) (push local unread))
                     (when (and all (member "STARRED" l)) (push local starred))))
                 (nngmail-group-local->num g)))
      (when (or unread starred)
        (nngmail--log "group catch-up %s: %d unread%s at Gmail"
                      short (length unread)
                      (if starred (format ", %d starred" (length starred)) ""))
        (condition-case err
            (progn
              (nngmail-request-set-mark
               short
               (delq nil (list (and unread (list (gnus-compress-sequence (sort unread #'<)) 'add '(read)))
                               (and starred (list (gnus-compress-sequence (sort starred #'<)) 'del '(tick)))))
               (cadr method))
              (nngmail--announce-cross-group account group unread 'read))
          (nngmail-error
           (nngmail--message 3 "catch-up push for %s failed: %s" short (error-message-string err)))))
      ;; Now let Gnus recompute the info; with labels updated, our
      ;; request-group/update-info will agree with it from here on.
      (prog1 (funcall orig group all)
        ;; Make the info reflect the labels immediately rather than at
        ;; the next sync, so the *Group* line is right.
        (when-let* ((info (gnus-get-info group)))
          (nngmail--update-info account short info)
          (gnus-get-unread-articles-in-group info (gnus-active group)))))))

(advice-add 'gnus-group-catchup :around #'nngmail--group-catchup-advice)

(advice-add 'gnus-summary-mark-article-as-read   :after #'nngmail--mark-read-advice)
(advice-add 'gnus-summary-mark-article-as-unread :after #'nngmail--mark-unread-advice)
(advice-add 'gnus-mark-article-as-read           :after #'nngmail--mark-article-advice)
(advice-add 'gnus-mark-article-as-unread         :after #'nngmail--mark-article-advice)

;; On exit: flush (q) or revert (Q) before the summary's own
;; bookkeeping, so the order is deterministic; flush on Emacs exit.
(add-hook 'gnus-summary-prepare-exit-hook #'nngmail--summary-exit)
(add-hook 'kill-emacs-hook #'nngmail-flush-marks)

;;;; Info (read/tick) synchronisation

(defun nngmail--update-info (account group info)
  "Set read and tick marks in INFO for GROUP from ACCOUNT's labels.
Works in GROUP's local numbering: unread and starred articles are the
only ones enumerated, so this is O(unread), and the read range Gnus
stores is a handful of spans however big the group is.

Does nothing while a summary for GROUP is open: the info is then the
base Gnus diffs the summary's marks against on exit, and rewriting it
from Gmail mid-session would make that diff miss what the user did
\(see `nngmail-mark-propagation').  The *Group* line keeps its
entry-time counts until the summary is left, as with nnimap."
  (unless (nngmail--summary-open-p (gnus-info-group info))
    (nngmail--update-info-1 account group info)))

(defun nngmail--update-info-1 (account group info)
  "The body of `nngmail--update-info' for GROUP, without the open-summary guard.
ACCOUNT is the nngmail account."
  (let* ((g (nngmail--group account group))
         (labels (nngmail-account-labels account))
         (high (nngmail--group-high account group))
         unread starred)
    (when g
      (maphash (lambda (local num)
                 (let ((l (gethash num labels)))
                   (when (member "UNREAD" l) (push local unread))
                   (when (member "STARRED" l) (push local starred))))
               (nngmail-group-local->num g)))
    (if (or (null g) (zerop (hash-table-count (nngmail-group-local->num g))))
        ;; Empty group: nothing unread, no ticks.  HIGH keeps retired
        ;; numbers retired.
        (let ((marks (gnus-info-marks info)))
          (gnus-set-active (gnus-info-group info) (cons (1+ high) high))
          (setf (gnus-info-read info) (and (> high 0) (list (cons 1 high))))
          (setq marks (assq-delete-all 'tick marks))
          (setq marks (assq-delete-all 'unexist marks))
          (setf (gnus-info-marks info) marks))
      (let* ((low (car (nngmail--group-locals account group)))
             ;; Read = everything up to HIGH that is not unread.  Retired
             ;; local numbers (messages that left the group) fall in here
             ;; too, which is right: Gnus must not show them.
             (read (range-difference (cons 1 high)
                                     (gnus-compress-sequence (sort unread #'<)))))
        (gnus-set-active (gnus-info-group info) (cons low high))
        (setf (gnus-info-read info) read)
        (let ((marks (gnus-info-marks info)))
          (setq marks (assq-delete-all 'tick marks))
          (setq marks (assq-delete-all 'unexist marks))
          (when starred
            (push (cons 'tick (gnus-compress-sequence (sort starred #'<))) marks))
          (setf (gnus-info-marks info) marks))))))

;;;; Bulk import over IMAP
;;
;; The REST API meters every messages.get at 5 quota units against a
;; per-user budget of 15,000 units a minute, so a header-only index can
;; never run faster than ~3,000 messages a minute however the requests
;; are arranged.  Gmail's IMAP endpoint has no such meter: one
;; `UID FETCH 1:50000 (...)' streams headers for tens of thousands of
;; messages at network speed.  Gmail's IMAP extensions (X-GM-EXT-1)
;; return, per message, the same 64-bit id the REST API uses
;; (X-GM-MSGID, decimal; the REST id is that number in hex), the thread
;; id and the label *names*.  That is everything the index needs, so a
;; bulk import can be done over IMAP and the result plugs straight into
;; the REST-based index: the same article numbers, the same label
;; table, history.list picks up from there.
;;
;; This reuses nnimap's connection, login (XOAUTH2 with the token nngmail
;; already holds), command and response machinery.  It does not create
;; an nnimap Gnus server or group; the connection lives in a private
;; process buffer and is closed at the end.

(require 'nnimap)

(defcustom nngmail-imap-address "imap.gmail.com"
  "IMAP host used for bulk imports."
  :type 'string)

(defvar nngmail--imap-service "imaps"
  "Port or service name for the IMAP connection.  Tests rebind this.")
(defvar nngmail--imap-stream-type 'tls
  "`open-network-stream' :type for the IMAP connection.  Tests rebind this.")

(defcustom nngmail-imap-chunk 2000
  "Messages per IMAP FETCH during a bulk import.
Bigger chunks mean fewer round trips but longer gaps between
checkpoints and progress updates."
  :type 'integer)

(defcustom nngmail-imap-all-mail "[Gmail]/All Mail"
  "Gmail's all-mail mailbox in IMAP, in the account's language.
Spam and Trash are not in it; they are imported from their own
mailboxes (`nngmail-imap-spam' and `nngmail-imap-trash') when asked."
  :type 'string)

(defcustom nngmail-imap-spam "[Gmail]/Spam" "Gmail's spam mailbox in IMAP." :type 'string)
(defcustom nngmail-imap-trash "[Gmail]/Trash" "Gmail's trash mailbox in IMAP." :type 'string)

(defvar nngmail--imap-buffer nil
  "Process buffer of the primary IMAP bulk-import connection, or nil.
Extra connections for a parallel import are kept in
`nngmail--imap-extra-buffers'.")

(defvar nngmail--imap-extra-buffers nil
  "Process buffers of additional IMAP connections opened for a parallel import.")

(defcustom nngmail-imap-connections 8
  "IMAP connections used side by side for a bulk import.
Gmail meters bandwidth per connection (about 400 KB/s, some 750
headers a second, was measured) and allows up to 15 connections per
account, so N connections import close to N times faster.  Each
connection walks its own stripe of the mailbox; results merge into the
one index.  1 disables the parallelism.  Leave a few of the 15 for
other IMAP clients (a phone, a mail app) or Gmail will refuse them
\"Too many simultaneous connections\" while an import runs."
  :type 'integer)

(define-error 'nngmail-imap-unavailable
  "nngmail: IMAP is not usable for this account" 'nngmail-error)

(defvar nngmail--imap-unavailable-reason nil
  "Why IMAP was found unusable this session, or nil.
Once set, backfills go straight to REST without trying IMAP again.
`nngmail-imap-retry' clears it.")

(defun nngmail--imap-open (account &optional extra)
  "Open an IMAP connection to Gmail for ACCOUNT, log in with its OAuth token.
Return the process buffer.  Reuses the primary connection if it is
open; with EXTRA non-nil, always opens a new one and records it in
`nngmail--imap-extra-buffers'."
  (if (and (not extra)
           nngmail--imap-buffer (buffer-live-p nngmail--imap-buffer)
           (process-live-p (get-buffer-process nngmail--imap-buffer)))
      nngmail--imap-buffer
    (let* ((token (nngmail--obtain-token account))
           (user (nngmail-account-user account))
           (buf (generate-new-buffer (format " *nngmail-imap %s*" (nngmail-account-name account)))))
      (with-current-buffer buf
        (mm-disable-multibyte)
        (buffer-disable-undo)
        (setq-local nnimap-object (make-nnimap :server (nngmail-account-name account)
                                               :initial-resync 0))
        (setq-local nnimap-address nngmail-imap-address)
        (setq-local nnimap-inhibit-logging t)
        (setq-local nnimap-streaming t)
        (nngmail--progress account "connecting to IMAP" nil nil t)
        (nngmail--log "%s: IMAP connecting to %s:%s as %s (XOAUTH2)"
                      (nngmail-account-name account) nngmail-imap-address nngmail--imap-service user)
        (let* ((coding-system-for-read 'binary)
               (coding-system-for-write 'binary)
               (stream-list (open-network-stream
                             "*nngmail-imap*" buf nngmail-imap-address nngmail--imap-service
                             :type nngmail--imap-stream-type :return-list t
                             :capability-command "1 CAPABILITY\r\n"
                             :always-query-capabilities t
                             :end-of-command "\r\n" :success " OK "))
               (stream (car stream-list))
               (props (cdr stream-list)))
          (unless (and stream (memq (process-status stream) '(open run)))
            (kill-buffer buf)
            (signal 'nngmail-imap-unavailable
                    (list (format "cannot connect to %s:993 (firewall, or IMAP blocked?)"
                                  nngmail-imap-address))))
          (set-process-query-on-exit-flag stream nil)
          (setf (nnimap-process nnimap-object) stream
                (nnimap-stream-type nnimap-object) 'tls
                (nnimap-greeting nnimap-object) (plist-get props :greeting)
                (nnimap-capabilities nnimap-object)
                (mapcar #'upcase (split-string (or (plist-get props :capabilities) ""))))
          (let ((result (nnimap-command
                         "AUTHENTICATE XOAUTH2 %s"
                         (base64-encode-string
                          (format "user=%s\001auth=Bearer %s\001\001" user token) t))))
            (unless (car result)
              (let ((line (nngmail--imap-last-line)))
                (delete-process stream)
                (kill-buffer buf)
                (signal 'nngmail-imap-unavailable
                        (list (nngmail--imap-explain-login-failure user line))))))
          (unless (nnimap-capability "X-GM-EXT-1")
            (nnimap-command "CAPABILITY")
            (unless (nnimap-capability "X-GM-EXT-1")
              (delete-process stream)
              (kill-buffer buf)
              (signal 'nngmail-imap-unavailable
                      (list (format "%s does not offer Gmail's IMAP extensions (X-GM-EXT-1)"
                                    nngmail-imap-address)))))
          (nngmail--log "%s: IMAP logged in; capabilities: %s"
                        (nngmail-account-name account)
                        (mapconcat #'identity
                                   (seq-filter (lambda (c) (string-match-p "\\`X-GM\\|IDLE\\|COMPRESS\\|UIDPLUS" c))
                                               (nnimap-capabilities nnimap-object))
                                   " "))))
      (if extra
          (progn (push buf nngmail--imap-extra-buffers) buf)
        (setq nngmail--imap-buffer buf)))))

(defun nngmail--imap-explain-login-failure (user line)
  "Turn Gmail's IMAP login rejection LINE for USER into advice."
  (cond
   ((string-match-p "IMAP access is disabled\\|not enabled" line)
    (format "IMAP access is disabled for %s (Gmail settings > Forwarding and POP/IMAP)" user))
   ((string-match-p "Invalid credentials\\|AUTHENTICATIONFAILED\\|Invalid SASL" line)
    (format "Gmail rejected the OAuth token for IMAP (%s); the token's scope must include https://mail.google.com/"
            (string-trim line)))
   (t (format "IMAP login failed for %s: %s" user (string-trim line)))))

(defun nngmail--imap-last-line ()
  "Last non-empty line of the IMAP process buffer, for error messages."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "\r\n")
    (buffer-substring-no-properties (line-beginning-position) (point))))

(defun nngmail--imap-close-buffer (buf)
  "LOGOUT and kill the IMAP connection in BUF."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (when-let* ((proc (get-buffer-process (current-buffer))))
        (when (process-live-p proc)
          (ignore-errors
            (let ((nnimap-streaming t)) (nnimap-send-command "LOGOUT")))
          (delete-process proc))))
    (kill-buffer buf)))

(defun nngmail--imap-close ()
  "Close every bulk-import IMAP connection."
  (let ((n (length (seq-filter #'buffer-live-p
                               (cons nngmail--imap-buffer nngmail--imap-extra-buffers)))))
    (nngmail--imap-close-buffer nngmail--imap-buffer)
    (mapc #'nngmail--imap-close-buffer nngmail--imap-extra-buffers)
    (setq nngmail--imap-buffer nil
          nngmail--imap-extra-buffers nil)
    (when (> n 0)
      (nngmail--log "IMAP connection%s closed" (if (= n 1) "" (format "s (%d)" n))))))

(defun nngmail--imap-examine (mailbox)
  "EXAMINE MAILBOX read-only in the current IMAP buffer; return (EXISTS . UIDNEXT)."
  (let ((result (nnimap-command "EXAMINE %S" (utf7-encode mailbox t))))
    (unless (car result)
      (signal 'nngmail-error (list (format "IMAP EXAMINE %s failed: %s" mailbox
                                           (nngmail--imap-last-line)))))
    (let (exists uidnext)
      (dolist (line (cdr result))
        (pcase line
          (`(,n "EXISTS") (setq exists (string-to-number n)))
          (`("OK" ("UIDNEXT" ,n) . ,_) (setq uidnext (string-to-number n)))))
      (cons (or exists 0) (or uidnext 1)))))

(defconst nngmail--imap-fetch-items
  (format "(UID X-GM-MSGID X-GM-THRID X-GM-LABELS RFC822.SIZE BODY.PEEK[HEADER.FIELDS (%s)])"
          (mapconcat #'identity nngmail--metadata-headers " "))
  "What one bulk-import FETCH asks for per message.")

(defvar nngmail--imap-chunk-stats nil
  "Timing of the last IMAP chunk, for the log.
A list (WAIT-SECONDS BYTES MESSAGES PARSE-SECONDS).")

(defun nngmail--imap-send-fetch (lo hi)
  "Send UID FETCH LO:HI on the current IMAP connection; return its tag regexp.
Does not wait.  The response accumulates in the process buffer until
`nngmail--imap-await-fetch' collects it."
  (nngmail--trace "IMAP UID FETCH %d:%d" lo hi)
  (let ((sequence (let ((nnimap-streaming t))
                    (nnimap-send-command "UID FETCH %d:%d %s" lo hi nngmail--imap-fetch-items))))
    (format "^%d " sequence)))

(defun nngmail--imap-await-fetch (tag lo hi &optional account phase scanned total)
  "Wait for the FETCH of LO:HI tagged TAG (from `nngmail--imap-send-fetch').
Return the parsed records and leave the buffer empty for the next one.
While the response streams in, the status line shows the bytes and
messages received so far (ACCOUNT, PHASE, SCANNED and TOTAL feed the
progress display).  \\[keyboard-quit] quits at once.

Not `nnimap-wait-for-response': that one turns \\[keyboard-quit] into a silent nil
and gives no feedback for the minutes a big chunk can take on a slow
link.  Counting messages received is incremental (from where the last
count stopped), not a rescan of the growing buffer.
LO is the lowest UID."
  (let* ((proc (get-buffer-process (current-buffer)))
         (started (float-time))
         (last-shown 0)
         (fetched 0)
         (counted-to (point-min)))
    (while (and (process-live-p proc)
                (not (save-excursion
                       (goto-char (point-max))
                       (forward-line -1)
                       (looking-at tag))))
      (accept-process-output proc 0.5)
      (when (> (- (float-time) last-shown) 0.5)
        (setq last-shown (float-time))
        ;; Count new FETCH lines since the last look; a line may still be
        ;; arriving, so stop at the last complete one.
        (save-excursion
          (goto-char counted-to)
          (let ((limit (progn (goto-char (point-max)) (line-beginning-position))))
            (goto-char counted-to)
            (while (re-search-forward "^\\* [0-9]+ FETCH" limit t)
              (cl-incf fetched))
            (setq counted-to limit)))
        (when account
          (nngmail--set-status
           (if (zerop (buffer-size))
               (format "waiting for chunk %s" (nngmail--fmt-duration (- (float-time) started)))
             (format "%s msgs, %s in %s"
                     (nngmail--fmt-num fetched)
                     (file-size-human-readable (buffer-size))
                     (nngmail--fmt-duration (- (float-time) started)))))
          (let ((nngmail--progress-extra (list (+ (or scanned 0) fetched) nil total)))
            (nngmail--progress account phase (+ (or scanned 0) fetched) total t)))))
    (when account (nngmail--set-status nil))
    (unless (process-live-p proc)
      (signal 'nngmail-error (list "IMAP connection closed during FETCH")))
    (save-excursion
      (goto-char (point-max))
      (forward-line -1)
      (unless (looking-at (concat tag "OK"))
        (signal 'nngmail-error
                (list (format "IMAP FETCH %d:%d failed: %s" lo hi
                              (buffer-substring-no-properties (point) (line-end-position)))))))
    (let* ((wait (- (float-time) started))
           (bytes (buffer-size))
           (t0 (float-time))
           (records (nngmail--imap-parse-fetch)))
      (setq nngmail--imap-chunk-stats (list wait bytes (length records) (- (float-time) t0)))
      (erase-buffer)
      records)))

(defun nngmail--imap-response-complete-p (tag)
  "Non-nil if the response tagged TAG has fully arrived in the current buffer."
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (looking-at tag)))

(defun nngmail--imap-take-response (tag lo hi)
  "Parse the complete FETCH response TAG (LO:HI) in the current buffer.
Return its records and empty the buffer.  Signals if the response is
an error or the connection died."
  (unless (process-live-p (get-buffer-process (current-buffer)))
    (signal 'nngmail-error (list "IMAP connection closed during FETCH")))
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (unless (looking-at (concat tag "OK"))
      (signal 'nngmail-error
              (list (format "IMAP FETCH %d:%d failed: %s" lo hi
                            (buffer-substring-no-properties (point) (line-end-position)))))))
  (prog1 (nngmail--imap-parse-fetch)
    (erase-buffer)))

(defun nngmail--imap-fetch-range (lo hi &optional account phase scanned total)
  "UID FETCH LO:HI and wait for it; return parsed records.
PHASE names the step for the status line.
Convenience wrapper over `nngmail--imap-send-fetch' and
`nngmail--imap-await-fetch' for one-off fetches.
ACCOUNT is the nngmail account."
  (erase-buffer)
  (nngmail--imap-await-fetch (nngmail--imap-send-fetch lo hi) lo hi account phase scanned total))

(defun nngmail--imap-parse-fetch ()
  "Parse the FETCH responses in the current buffer into records.
See `nngmail--imap-fetch-range' for the record shape."
  (goto-char (point-min))
  (let (records)
    (while (re-search-forward "^\\* [0-9]+ FETCH (" nil t)
      (let ((start (point)) msgid thrid labels size headers)
        ;; Attributes come in any order; the literal header block is
        ;; announced by {N} and followed by exactly N octets.
        (catch 'done
          (while t
            (skip-chars-forward " ")
            (cond
             ((looking-at "UID [0-9]+") (goto-char (match-end 0)))
             ((looking-at "X-GM-MSGID \\([0-9]+\\)")
              (setq msgid (match-string 1)) (goto-char (match-end 0)))
             ((looking-at "X-GM-THRID \\([0-9]+\\)")
              (setq thrid (match-string 1)) (goto-char (match-end 0)))
             ((looking-at "RFC822.SIZE \\([0-9]+\\)")
              (setq size (string-to-number (match-string 1))) (goto-char (match-end 0)))
             ((looking-at "X-GM-LABELS (")
              (goto-char (1- (match-end 0)))
              (setq labels (nngmail--imap-read-labels)))
             ((looking-at "BODY\\[HEADER\\.FIELDS ([^)]*)\\] {\\([0-9]+\\)}\r?\n")
              (let* ((n (string-to-number (match-string 1)))
                     (beg (match-end 0))
                     (end (+ beg n)))
                (setq headers (nngmail--imap-parse-headers beg (min end (point-max))))
                (goto-char (min end (point-max)))))
             ((looking-at ")") (forward-char 1) (throw 'done nil))
             ((eobp) (throw 'done nil))
             (t
              ;; Unknown attribute: skip a token (or a parenthesised group).
              (if (looking-at "(")
                  (forward-sexp 1)
                (skip-chars-forward "^ )"))))))
        (ignore start)
        (when msgid
          (push (list (format "%x" (string-to-number msgid))
                      (and thrid (format "%x" (string-to-number thrid)))
                      labels (or size 0) headers)
                records))))
    (nreverse records)))

(defun nngmail--imap-read-quoted ()
  "Read an IMAP quoted string at point (opening quote under point).
Handles the two escapes IMAP allows, \\\" and \\\\, and leaves point
after the closing quote.  Returns the unescaped text."
  (forward-char 1)
  (let ((parts nil) (beg (point)))
    (while (not (eq (char-after) ?\"))
      (when (eobp) (error "nngmail: Unterminated IMAP quoted string"))
      (if (eq (char-after) ?\\)
          (progn (push (buffer-substring beg (point)) parts)
                 (forward-char 1)
                 (setq beg (point))
                 (forward-char 1))
        (forward-char 1)))
    (push (buffer-substring beg (point)) parts)
    (forward-char 1)
    (apply #'concat (nreverse parts))))

(defun nngmail--imap-read-labels ()
  "Read an X-GM-LABELS list at point: (\\Inbox \"My Label\" \"\\\\Sent\").
Gmail sends system labels as backslash atoms (\\Inbox) or, more often,
as quoted strings with the backslash escaped (\"\\\\Inbox\"); user
labels as quoted strings, modified UTF-7 encoded.  Return REST label
ids for system labels and decoded display names for the rest."
  (forward-char 1)                      ; past (
  (let (names)
    (while (progn (skip-chars-forward " ") (not (eq (char-after) ?\))))
      (push (utf7-decode
             (if (eq (char-after) ?\")
                 (nngmail--imap-read-quoted)
               (let ((beg (point)))
                 (skip-chars-forward "^ )")
                 (buffer-substring beg (point))))
             t)
            names))
    (forward-char 1)                    ; past )
    (mapcar #'nngmail--imap-label-id (nreverse names))))

(defconst nngmail--imap-system-labels
  '(("\\Inbox" . "INBOX") ("\\Sent" . "SENT") ("\\Draft" . "DRAFT") ("\\Drafts" . "DRAFT")
    ("\\Spam" . "SPAM") ("\\Trash" . "TRASH") ("\\Starred" . "STARRED")
    ("\\Important" . "IMPORTANT") ("\\Muted" . "MUTED")
    ("\\Chat" . "CHAT") ("\\Scheduled" . "SCHEDULED"))
  "IMAP X-GM-LABELS system names -> REST label ids.")

(defun nngmail--imap-label-id (name)
  "Map an IMAP label NAME to the REST label id used by the index.
System labels come as backslash names; user labels come by display name
and are resolved through the account's label table."
  (or (cdr (assoc-string name nngmail--imap-system-labels t))
      ;; An unlisted backslash label: strip the slash, upper-case, and
      ;; that is Gmail's REST id for every system label seen so far.
      (and (string-prefix-p "\\" name) (upcase (substring name 1)))
      name))

(defun nngmail--imap-parse-headers (beg end)
  "Parse RFC 822 header lines between BEG and END into an alist of (NAME . VALUE)."
  (save-restriction
    (narrow-to-region beg end)
    (goto-char (point-min))
    (let (result)
      (while (re-search-forward "^\\([!-9;-~]+\\):[ \t]*" nil t)
        (let ((name (match-string 1))
              (vbeg (point))
              vend)
          ;; The value runs to the end of this line and any continuation
          ;; lines (starting with whitespace).
          (end-of-line)
          (setq vend (point))
          (while (and (not (eobp))
                      (progn (forward-line 1) (looking-at "[ \t]")))
            (end-of-line)
            (setq vend (point)))
          (push (cons name (string-trim
                            (replace-regexp-in-string
                             "\r?\n[ \t]+" " "
                             (buffer-substring-no-properties vbeg vend))))
                result)))
      (nreverse result))))

(defun nngmail--imap-header-value (alist name)
  "Value of header NAME in ALIST, compared case-insensitively."
  (cdr (assoc-string name alist t)))

(defun nngmail--imap-record->header (account num record seen-labels)
  "Store RECORD (see `nngmail--imap-fetch-range') as article NUM of ACCOUNT.
User label names are resolved to ids through ACCOUNT's label table; names
not yet known are collected in SEEN-LABELS (a hash) for a later refresh."
  (pcase-let* ((`(,id ,thrid ,labels ,size ,hdrs) record)
               (ids (mapcar (lambda (l)
                              (or (and (string-match-p "\\`[A-Z_]+\\'" l) l)
                                  (nngmail--group-label-or-nil account l)
                                  (progn (puthash l t seen-labels) l)))
                            labels))
               (flags-unread (not (member "\\Seen" labels))))
    (ignore flags-unread)
    (let* ((message-id (or (nngmail--imap-header-value hdrs "Message-ID")
                           (format "<%s@nngmail.gmail>" id)))
           (refs (or (nngmail--imap-header-value hdrs "References")
                     (nngmail--imap-header-value hdrs "In-Reply-To") ""))
           (extra `((To . ,(nngmail--decode-header (nngmail--imap-header-value hdrs "To")))
                    (Cc . ,(nngmail--decode-header (nngmail--imap-header-value hdrs "Cc")))
                    (X-Gmail-Labels . ,(mapconcat #'identity ids ","))
                    (X-Gmail-Thread-Id . ,(or thrid "")))))
      (nngmail--store-header account num
                             (make-full-mail-header
                              num
                              (nngmail--decode-header (nngmail--imap-header-value hdrs "Subject"))
                              (nngmail--decode-header (nngmail--imap-header-value hdrs "From"))
                              (or (nngmail--imap-header-value hdrs "Date") "")
                              message-id refs size 0 nil extra)
                             ids))))

(defun nngmail--group-label-or-nil (account group)
  "Label id for GROUP in ACCOUNT without refetching labels, or nil."
  (gethash group (nngmail-account-name->label account)))

(defcustom nngmail-import-slice 0.15
  "Seconds of work per step of a background IMAP import.
The import runs from a timer.  Each step parses and stores at most this
long, then returns so redisplay and keystrokes get through; the next
step follows after `nngmail-import-slice-gap'.  Smaller keeps Emacs
snappier; larger finishes sooner."
  :type 'number)

(defcustom nngmail-import-slice-gap 0.05
  "Seconds between steps of a background IMAP import."
  :type 'number)

(defvar nngmail--import nil
  "The running background IMAP import (an `nngmail-import' struct), or nil.")

(cl-defstruct nngmail-import
  account mailbox count phase newest-first
  known spec seen-labels
  added added-before scanned total
  chunk workers pending-records
  done finished timer started last-shown
  on-done                                 ; function called with the import when it ends
  error)                                  ; error data if it stopped abnormally

(defun nngmail--imap-stripes (uidnext n)
  "Split UIDs 1..UIDNEXT-1 into N contiguous stripes: list of (LO . HI).
UIDNEXT is the mailbox's UIDNEXT."
  (let* ((last (1- uidnext))
         (n (max 1 (min n last)))
         (size (ceiling (/ (float last) n)))
         (out nil))
    (dotimes (i n)
      (let ((lo (1+ (* i size)))
            (hi (min last (* (1+ i) size))))
        (when (<= lo hi) (push (cons lo hi) out))))
    (nreverse out)))

;; The import is a small state machine driven by a timer:
;;
;;   start   open connections, EXAMINE, arm every worker with its first
;;           FETCH.
;;   step    (every `nngmail-import-slice-gap' s) for up to
;;           `nngmail-import-slice' s: take any complete response, re-arm
;;           that worker, and store the records a few hundred at a time;
;;           records not stored within the slice wait in
;;           `pending-records' for the next step.  Then return, so Emacs
;;           redisplays and reads keys.
;;   finish  when every worker is spent (or the count is reached):
;;           rebuild groups, checkpoint, close connections, call on-done.
;;
;; `nngmail-import-wait' turns this back into a blocking call for
;; callers that need the result (tests, the first-open offer): it just
;; pumps `accept-process-output' until the import ends, running timers.

(defun nngmail--import-worker-range (imp st)
  "Next chunk of stripe ST for import IMP, or nil when spent."
  (let ((c (plist-get st :cursor)) (lo (plist-get st :lo)) (hi (plist-get st :hi))
        (chunk (nngmail-import-chunk imp)))
    (cond ((and (nngmail-import-newest-first imp) (>= c lo)) (cons (max lo (- c chunk -1)) c))
          ((and (not (nngmail-import-newest-first imp)) (<= c hi)) (cons c (min hi (+ c chunk -1))))
          (t nil))))

(defun nngmail--import-arm (imp w)
  "Send worker W's next FETCH; return nil if its stripe is spent.
IMP is the `nngmail-import'."
  (let ((r (nngmail--import-worker-range imp (plist-get w :stripe))))
    (when r
      (with-current-buffer (plist-get w :buf)
        (erase-buffer)
        (plist-put w :tag (nngmail--imap-send-fetch (car r) (cdr r))))
      (plist-put w :lo (car r))
      (plist-put w :hi (cdr r))
      t)))

(defun nngmail--import-advance (imp w)
  "Move worker W's stripe cursor past the chunk just taken.
IMP is the `nngmail-import'."
  (let ((st (plist-get w :stripe)))
    (plist-put st :cursor (if (nngmail-import-newest-first imp)
                              (1- (plist-get w :lo))
                            (1+ (plist-get w :hi))))))

(defun nngmail--import-ready (w)
  "Non-nil when worker W has a FETCH out whose reply has fully arrived."
  (and (plist-get w :tag)
       (buffer-live-p (plist-get w :buf))
       (with-current-buffer (plist-get w :buf)
         (nngmail--imap-response-complete-p (plist-get w :tag)))))

(defun nngmail--import-start (account mailbox count phase newest-first on-done)
  "Begin a background IMAP import of MAILBOX; return the `nngmail-import'.
See `nngmail--imap-import-mailbox' for the arguments.
ACCOUNT is the nngmail account."
  (when nngmail--import
    (signal 'nngmail-busy (list "an IMAP import is already running")))
  (let* ((known (nngmail-account-id->num account))
         (spec (list 'imap mailbox newest-first))
         (resume (nngmail-account-backfill account))
         (resume (and resume (equal (plist-get resume :spec) spec) resume))
         (imp (make-nngmail-import
               :account account :mailbox mailbox :count count
               :phase (or phase "importing") :newest-first newest-first
               :known known :spec spec :seen-labels (make-hash-table :test #'equal)
               :added (or (and resume (plist-get resume :found)) 0)
               :added-before (or (and resume (plist-get resume :found)) 0)
               :scanned (or (and resume (plist-get resume :scanned)) 0)
               :chunk nngmail-imap-chunk :on-done on-done
               :started (float-time) :last-shown 0)))
    (with-current-buffer (nngmail--imap-open account)
      (pcase-let ((`(,exists . ,uidnext) (nngmail--imap-examine mailbox)))
        (setf (nngmail-import-total imp) exists)
        (let* ((stripes (or (plist-get resume :stripes)
                            (mapcar (lambda (r) (list :lo (car r) :hi (cdr r)
                                                      :cursor (if newest-first (cdr r) (car r))))
                                    (nngmail--imap-stripes uidnext nngmail-imap-connections))))
               (live (seq-filter (lambda (st) (nngmail--import-worker-range imp st)) stripes)))
          (nngmail--log "%s: IMAP import of %s: %s messages there, %s known locally, fetching %s%s: %d stripe%s, chunks of %d, in the background"
                        (nngmail-account-name account) mailbox
                        (nngmail--fmt-num exists) (nngmail--fmt-num (hash-table-count known))
                        (if count (nngmail--fmt-num count) "all")
                        (if newest-first " newest first" "")
                        (length live) (if (= 1 (length live)) "" "s") nngmail-imap-chunk)
          (when resume
            (nngmail--message 5 "%s: resuming IMAP import of %s (%s scanned, %s added so far)"
                              (nngmail-account-name account) mailbox
                              (nngmail--fmt-num (nngmail-import-scanned imp))
                              (nngmail--fmt-num (nngmail-import-added imp))))
          (let ((first t) workers)
            (dolist (st live)
              (let ((buf (if first (current-buffer) (nngmail--imap-open account t))))
                (setq first nil)
                (unless (eq buf (current-buffer))
                  (with-current-buffer buf (nngmail--imap-examine mailbox)))
                (push (list :buf buf :stripe st :tag nil :lo nil :hi nil) workers)))
            (setf (nngmail-import-workers imp)
                  (seq-filter (lambda (w) (nngmail--import-arm imp w)) (nreverse workers))))
          (setf (nngmail-import-done imp)
                (or (zerop exists) (null (nngmail-import-workers imp)))))))
    (setq nngmail--import imp)
    (if (nngmail-import-done imp)
        (nngmail--import-finish imp)
      (setf (nngmail-import-timer imp)
            (run-at-time 0 nil #'nngmail--import-tick)))
    imp))

(defun nngmail--import-tick ()
  "One timer step of the running import: take, re-arm, store for a slice."
  (let ((imp nngmail--import))
    (when imp
      (setf (nngmail-import-timer imp) nil)
      (condition-case err
          (let ((deadline (+ (float-time) nngmail-import-slice)))
            (nngmail--import-step imp deadline)
            (if (nngmail-import-done imp)
                (nngmail--import-finish imp)
              (setf (nngmail-import-timer imp)
                    (run-at-time nngmail-import-slice-gap nil #'nngmail--import-tick))))
        ((quit error)
         (setf (nngmail-import-error imp) err
               (nngmail-import-done imp) t)
         (nngmail--import-finish imp))))))

(defun nngmail--import-step (imp deadline)
  "Do import work on IMP until DEADLINE (a `float-time') or nothing is ready."
  (let ((account (nngmail-import-account imp))
        (nngmail--bulk-filing t))
    (cl-block step
      (while (< (float-time) deadline)
        ;; 0. Every stripe spent and nothing left to store: finished.
        (when (and (null (nngmail-import-workers imp))
                   (null (nngmail-import-pending-records imp)))
          (setf (nngmail-import-done imp) t)
          (cl-return-from step))
        ;; 1. Store queued records, a few hundred per check of the clock.
        (when (nngmail-import-pending-records imp)
          (let ((n 0))
            (while (and (nngmail-import-pending-records imp) (< n 200))
              (let ((r (pop (nngmail-import-pending-records imp))))
                (cl-incf (nngmail-import-scanned imp))
                (unless (gethash (car r) (nngmail-import-known imp))
                  (when (or (null (nngmail-import-count imp))
                            (< (nngmail-import-added imp) (nngmail-import-count imp)))
                    (nngmail--imap-record->header
                     account (nngmail--assign account (car r)) r (nngmail-import-seen-labels imp))
                    (cl-incf (nngmail-import-added imp)))))
              (cl-incf n))
            (when (and (nngmail-import-count imp)
                       (>= (nngmail-import-added imp) (nngmail-import-count imp)))
              (setf (nngmail-import-done imp) t)
              (cl-return-from step))
            (unless (nngmail-import-pending-records imp)
              (nngmail--import-checkpoint imp)
              (when (null (nngmail-import-workers imp))
                (setf (nngmail-import-done imp) t)))
            (cl-return-from step)))    ; give the display a turn between batches
        ;; 2. Nothing queued: take a complete response if there is one.
        (let ((w (seq-find #'nngmail--import-ready (nngmail-import-workers imp))))
          (if (null w)
              (progn
                ;; Nothing ready: pull bytes off the sockets briefly, then
                ;; let the timer come back.
                (accept-process-output nil 0.05)
                (nngmail--import-show imp)
                (cl-return-from step))
            (let* ((lo (plist-get w :lo)) (hi (plist-get w :hi))
                   (records (with-current-buffer (plist-get w :buf)
                              (nngmail--imap-take-response (plist-get w :tag) lo hi))))
              (nngmail--import-advance imp w)
              (plist-put w :tag nil)
              (unless (nngmail--import-arm imp w)
                (setf (nngmail-import-workers imp) (delq w (nngmail-import-workers imp))))
              (nngmail--trace "IMAP chunk %d:%d: %s msgs, %d conn busy"
                              lo hi (nngmail--fmt-num (length records))
                              (length (nngmail-import-workers imp)))
              (setf (nngmail-import-pending-records imp)
                    (if (nngmail-import-newest-first imp) (nreverse records) records))
              (when (and (null records) (null (nngmail-import-workers imp)))
                (setf (nngmail-import-done imp) t)
                (cl-return-from step)))))))))

(defun nngmail--import-show (imp)
  "Refresh the status line for IMP, at most twice a second."
  (when (> (- (float-time) (nngmail-import-last-shown imp)) 0.5)
    (setf (nngmail-import-last-shown imp) (float-time))
    (let ((in-flight (apply #'+ (mapcar (lambda (w) (if (buffer-live-p (plist-get w :buf))
                                                        (with-current-buffer (plist-get w :buf) (buffer-size))
                                                      0))
                                        (nngmail-import-workers imp)))))
      (nngmail--set-status
       (format "%s in flight, %s"
               (file-size-human-readable in-flight)
               (nngmail--fmt-duration (- (float-time) (nngmail-import-started imp))))))
    (let ((nngmail--progress-extra (list (nngmail-import-scanned imp) (nngmail-import-added imp)
                                         (nngmail-import-total imp))))
      (nngmail--progress (nngmail-import-account imp) (nngmail-import-phase imp)
                         (nngmail-import-scanned imp) (nngmail-import-total imp) t))))

(defun nngmail--import-checkpoint (imp)
  "Record IMP's stripe cursors and counters; save at most every few seconds."
  (let ((account (nngmail-import-account imp)))
    (setf (nngmail-account-backfill account)
          (unless (nngmail-import-done imp)
            (list :spec (nngmail-import-spec imp)
                  :found (nngmail-import-added imp)
                  :scanned (nngmail-import-scanned imp)
                  :stripes (mapcar (lambda (w) (plist-get w :stripe)) (nngmail-import-workers imp))))
          (nngmail-account-dirty account) t)
    (nngmail--checkpoint account)
    (nngmail--import-show imp)))

(defun nngmail--import-finish (imp)
  "End import IMP: file into groups, save, close connections, report."
  (let* ((account (nngmail-import-account imp))
         (mailbox (nngmail-import-mailbox imp))
         (name (nngmail-account-name account))
         (seen (nngmail-import-seen-labels imp)))
    (when (nngmail-import-timer imp)
      (cancel-timer (nngmail-import-timer imp))
      (setf (nngmail-import-timer imp) nil))
    (unwind-protect
        (progn
          ;; Records taken but not yet stored belong to the checkpointed
          ;; cursor already; drop them, the resume refetches that chunk.
          (setf (nngmail-import-pending-records imp) nil)
          (nngmail--set-status nil)
          (nngmail--progress account (concat (nngmail-import-phase imp) ", filing")
                             (nngmail-import-scanned imp) (nngmail-import-total imp) t)
          (nngmail--rebuild-groups account)
          (setf (nngmail-account-backfill account)
                (if (and (nngmail-import-done imp) (null (nngmail-import-error imp))
                         (null (nngmail-import-workers imp)))
                    nil
                  (list :spec (nngmail-import-spec imp)
                        :found (nngmail-import-added imp)
                        :scanned (nngmail-import-scanned imp)
                        :stripes (mapcar (lambda (w) (plist-get w :stripe)) (nngmail-import-workers imp))))
                (nngmail-account-dirty account) t)
          (nngmail--save account)
          (when (> (hash-table-count seen) 0)
            (nngmail--log "%s: %d label name(s) unknown to the index (%s); refreshing labels"
                          name (hash-table-count seen)
                          (mapconcat #'identity (seq-take (hash-table-keys seen) 5) ", "))
            (nngmail--relabel-groups account (nngmail--fetch-labels account))
            (nngmail--imap-remap-label-names account seen)
            (nngmail--rebuild-groups account))
          (nngmail--log "%s: IMAP import of %s %s: %s scanned, %s added%s"
                        name mailbox
                        (cond ((nngmail-import-error imp) "stopped")
                              ((nngmail-account-backfill account) "interrupted")
                              (t "finished"))
                        (nngmail--fmt-num (nngmail-import-scanned imp))
                        (nngmail--fmt-num (nngmail-import-added imp))
                        (if (nngmail-import-error imp)
                            (format " (%s)" (error-message-string (nngmail-import-error imp)))
                          "")))
      (setf (nngmail-import-finished imp) t)
      (when (eq nngmail--import imp) (setq nngmail--import nil))
      (when (nngmail-import-on-done imp)
        (funcall (nngmail-import-on-done imp) imp)))))

(defun nngmail-import-stop ()
  "Stop the running background IMAP import; what was imported is kept."
  (interactive)
  (if (null nngmail--import)
      (message "nngmail: no import running")
    (setf (nngmail-import-done nngmail--import) t)
    (nngmail--import-finish nngmail--import)
    (nngmail--imap-close)
    (message "nngmail: import stopped; M-x nngmail-backfill resumes it")))

(defun nngmail-import-wait (imp)
  "Block until background import IMP ends; return the number it added.
Timers keep running, so the import progresses.  \\[keyboard-quit] stops the import
and keeps what it imported."
  (condition-case nil
      (while (not (nngmail-import-finished imp))
        ;; `accept-process-output' runs timers only while it waits for
        ;; output; once everything has arrived nothing wakes it, so step
        ;; the import directly when its timer is due.
        (let ((timer (nngmail-import-timer imp)))
          ;; `timer-until' is negative once the timer's time has passed.
          (if (and timer (<= (timer-until timer (current-time)) 0))
              (progn (cancel-timer timer)
                     (setf (nngmail-import-timer imp) nil)
                     (nngmail--import-tick))
            (accept-process-output nil 0.05))))
    (quit
     (unless (nngmail-import-finished imp)
       (setf (nngmail-import-done imp) t)
       (nngmail--import-finish imp))
     (signal 'quit nil)))
  (when (nngmail-import-error imp)
    (signal (car (nngmail-import-error imp)) (cdr (nngmail-import-error imp))))
  (- (nngmail-import-added imp) (nngmail-import-added-before imp)))

(defun nngmail--imap-import-mailbox (account mailbox count &optional phase newest-first)
  "Import up to COUNT not-yet-indexed messages of MAILBOX into ACCOUNT over IMAP.
COUNT nil means all of them.  NEWEST-FIRST walks each stripe downwards
so a limited import gets the most recent mail.  Returns the number
added.

The mailbox's UID space is split into `nngmail-imap-connections'
stripes, each fetched on its own connection in chunks of
`nngmail-imap-chunk'.  The work runs from a timer in short slices
\(`nngmail-import-slice'), so Emacs stays responsive; this function
waits for it (see `nngmail-import-wait').  Checkpoints (one cursor per
stripe) go to the account's `backfill' slot, so an interrupted import
resumes exactly."
  (nngmail-import-wait (nngmail--import-start account mailbox count phase newest-first nil)))

(defun nngmail--imap-remap-label-names (account names)
  "Replace label NAMES (a hash of display names) by their ids in ACCOUNT's tables.
Records imported before the label table knew a label carry its display
name; once `nngmail--fetch-labels' has run the name resolves to an id."
  (let ((mapping (make-hash-table :test #'equal)))
    (maphash (lambda (name _)
               (when-let* ((id (nngmail--group-label-or-nil account name)))
                 (puthash name id mapping)))
             names)
    (when (> (hash-table-count mapping) 0)
      (nngmail--with-transaction account
        (maphash (lambda (num labels)
                   (when (seq-some (lambda (l) (gethash l mapping)) labels)
                     (let ((fixed (mapcar (lambda (l) (or (gethash l mapping) l)) labels)))
                       (puthash num fixed (nngmail-account-labels account))
                       (nngmail--db-put-labels account num fixed)
                       ;; The header row carries the labels too (X-Gmail-Labels
                       ;; in the NOV extra); rewrite it if we have it.
                       (when-let* ((h (nngmail--header account num)))
                         (setf (alist-get 'X-Gmail-Labels (mail-header-extra h))
                               (mapconcat #'identity fixed ","))
                         (nngmail--db-put-message account num (gethash num (nngmail-account-num->id account))
                                                  fixed h)))))
                 (nngmail-account-labels account)))
      (setf (nngmail-account-dirty account) t))))

(defun nngmail--imap-import (account count &optional phase newest-first with-spam-trash)
  "Import COUNT messages (nil = all) into ACCOUNT over IMAP.
All Mail first; Spam and Trash too when WITH-SPAM-TRASH."
  (let ((total 0))
    (unwind-protect
        (progn
          (cl-incf total (nngmail--imap-import-mailbox account nngmail-imap-all-mail count
                                                       phase newest-first))
          (when with-spam-trash
            (dolist (mb (list nngmail-imap-spam nngmail-imap-trash))
              (cl-incf total (nngmail--imap-import-mailbox account mb nil phase newest-first)))))
      (nngmail--imap-close))
    total))

;;;###autoload
(defun nngmail-imap-benchmark (server &optional chunk)
  "Measure raw IMAP fetch speed against SERVER's Gmail account.
Opens the bulk-import connection, examines All Mail, then times a few
`UID FETCH' commands of CHUNK messages (default `nngmail-imap-chunk')
with the plain nnimap-style wait and no parsing, storing or progress
display.  Reports messages/s and bytes/s per fetch to the log and the
echo area.  If this is fast and a backfill is slow, the loss is in
nngmail's processing; if this is slow, it is the link or Gmail."
  (interactive
   (list (nngmail--read-server)
         (and current-prefix-arg (read-number "Messages per fetch: " nngmail-imap-chunk))))
  (let* ((account (nngmail--account server))
         (chunk (or chunk nngmail-imap-chunk))
         (report nil))
    (unwind-protect
        (with-current-buffer (nngmail--imap-open account)
          (pcase-let* ((t0 (float-time))
                       (`(,exists . ,uidnext) (nngmail--imap-examine nngmail-imap-all-mail)))
            (push (format "EXAMINE %s: %s messages, UIDNEXT %s, %.2fs"
                          nngmail-imap-all-mail (nngmail--fmt-num exists) uidnext (- (float-time) t0))
                  report)
            ;; Three ranges: newest, middle, oldest.
            (dolist (start (list (max 1 (- uidnext chunk)) (max 1 (/ uidnext 2)) 1))
              (erase-buffer)
              (let* ((hi (min (1- uidnext) (+ start chunk -1)))
                     (t1 (float-time))
                     (tag (nngmail--imap-send-fetch start hi))
                     (proc (get-buffer-process (current-buffer)))
                     (first-byte nil))
                (while (and (process-live-p proc)
                            (not (save-excursion (goto-char (point-max)) (forward-line -1) (looking-at tag))))
                  (accept-process-output proc 0.5)
                  (when (and (null first-byte) (> (buffer-size) 0))
                    (setq first-byte (- (float-time) t1))))
                (let* ((secs (- (float-time) t1))
                       (n (count-matches "^\\* [0-9]+ FETCH" (point-min) (point-max)))
                       (bytes (buffer-size)))
                  (push (format "UID FETCH %d:%d: %s msgs, %s in %.1fs (first byte after %.1fs) = %s msg/s, %s/s"
                                start hi (nngmail--fmt-num n) (file-size-human-readable bytes) secs
                                (or first-byte 0.0)
                                (nngmail--fmt-num (round (/ n (max 0.01 secs))))
                                (file-size-human-readable (/ bytes (max 0.01 secs))))
                        report))))))
      (nngmail--imap-close))
    (setq report (nreverse report))
    (dolist (line report) (nngmail--log "benchmark: %s" line))
    (message "%s" (mapconcat #'identity report "\n"))))

;;;; nnoo interface

(defun nngmail--account-stale-p (account)
  "Non-nil if ACCOUNT was built by an older `nngmail-account' definition.
Happens when nngmail.el is reloaded in a running session after the
struct gained a slot: the object is shorter than the accessors expect
and setters signal `args-out-of-range'."
  (and account
       (recordp account)
       (/= (length account)
           (length (nngmail--make-account :name "" :user "")))))

(defun nngmail--account-upgrade (account)
  "Return a fresh `nngmail-account' carrying ACCOUNT's slots.
Slots are only ever appended to the struct, so the common index prefix
is copied and any new trailing slots keep their defaults.  Used
transparently when a stale object is found after reloading nngmail.el."
  (let* ((new (nngmail--make-account :name (aref account 1) :user (aref account 2)))
         (old-len (length account)))
    (cl-loop for i from 1 below (min old-len (length new))
             do (aset new i (aref account i)))
    (nngmail--log "upgraded stale account object for %s (%d -> %d slots)"
                  (aref account 1) (1- old-len) (1- (length new)))
    new))

(defun nngmail--upgrade-all-accounts ()
  "Upgrade any stale account objects in `nngmail--accounts' (after a reload)."
  (maphash (lambda (name acct)
             (when (nngmail--account-stale-p acct)
               (let ((new (nngmail--account-upgrade acct)))
                 (puthash name new nngmail--accounts)
                 (when (eq nngmail--current acct)
                   (setq nngmail--current new)))))
           nngmail--accounts))

(defun nngmail--account (&optional server)
  "Return the account for SERVER (or the current one), opening if needed.
A stale object left over from reloading nngmail.el is upgraded in place."
  (let* ((name (or server (and nngmail--current (aref nngmail--current 1))))
         (acct (and name (gethash name nngmail--accounts))))
    (when (nngmail--account-stale-p acct)
      (setq acct (nngmail--account-upgrade acct))
      (puthash name acct nngmail--accounts)
      (when (and nngmail--current (equal (aref nngmail--current 1) name))
        (setq nngmail--current acct)))
    (or acct
        (progn (nngmail-open-server name)
               (gethash name nngmail--accounts))
        (error "nngmail: No such server %S" name))))

(deffoo nngmail-open-server (server &optional defs)
  (nnoo-change-server 'nngmail server defs)
  (or (gethash server nngmail--accounts)
      (let ((account (nngmail--make-account :name server :user nngmail-user)))
        (condition-case err
            (progn
              (unless nngmail-user
                (error "nngmail: (nngmail-user \"you@example.com\") is required"))
              (nngmail--timed (format "open %s: load index" server)
                (nngmail--load account))
              (nngmail--timed (format "open %s: OAuth token" server)
                (nngmail--obtain-token account))
              (unless (nngmail-account-label-names account)
                (nngmail--timed (format "open %s: fetch labels" server)
                  (nngmail--fetch-labels account)))
              (puthash server account nngmail--accounts)
              (setq nngmail--current account
                    nngmail-state account)
              (nngmail--message 6 "opened server %s" server)
              t)
          (error
           (setq nngmail-status-string (error-message-string err))
           (nnheader-report 'nngmail "%s" nngmail-status-string)
           nil)))))

(deffoo nngmail-close-server (&optional server _defs)
  (when-let* ((account (and server (gethash server nngmail--accounts))))
    (nngmail--db-close account)
    (remhash server nngmail--accounts)
    (when (eq nngmail--current account) (setq nngmail--current nil)))
  (nnoo-close-server 'nngmail server)
  t)

(deffoo nngmail-server-opened (&optional server)
  (and (or server (and nngmail--current t))
       (gethash (or server (nngmail-account-name nngmail--current)) nngmail--accounts)
       t))

(deffoo nngmail-status-message (&optional _server)
  nngmail-status-string)

(deffoo nngmail-request-close ()
  (maphash (lambda (_k a) (nngmail--db-close a)) nngmail--accounts)
  (clrhash nngmail--accounts)
  (setq nngmail--current nil)
  t)

(defun nngmail-reset-limits ()
  "Forget the batch size and concurrency limits learned this session.
Use after Gmail has been generous for a while, or to retest the defaults."
  (interactive)
  (setq nngmail--batch-size-current nil
        nngmail--batch-size-ceiling nil
        nngmail--batch-parallel-current nil)
  (message "nngmail: batch limits reset to %d parts, %d parallel"
           nngmail-batch-size nngmail-batch-parallel))

(defun nngmail--sync-safely (account)
  "Run an incremental sync of ACCOUNT, reporting rather than signalling errors.
Return non-nil on success.  Whatever was indexed before the error is
kept, so Gnus can still show the groups from local state."
  (condition-case err
      (progn (nngmail--incremental-sync account) t)
    (nngmail-aborted
     (nngmail--save account)
     (nngmail--message 5 "%s: sync aborted; %d messages still to fetch, will resume on next sync"
                       (nngmail-account-name account)
                       (length (nth 2 (nngmail-account-pending account))))
     nil)
    (quit
     ;; C-g: keep what we have and let the quit propagate.
     (nngmail--save account)
     (signal 'quit nil))
    (nngmail-busy
     ;; A scan fired (demon, timer) while a sync was already waiting on
     ;; Gmail.  Nothing is lost: the running sync continues and this
     ;; one happens on the next scan.
     (nngmail--trace "%s: scan skipped, %s already in flight"
                     (nngmail-account-name account) (cadr err))
     nil)
    ((nngmail-error error)
     (let ((msg (error-message-string err)))
       (setq nngmail-status-string msg)
       (setf (nngmail-account-last-error account) msg)
       (nngmail--save account)
       (nnheader-report 'nngmail "sync of %s failed: %s"
                        (nngmail-account-name account) msg)
       nil))))

(deffoo nngmail-request-list (&optional server)
  (nngmail--timed "nngmail-request-list"
  (let ((account (nngmail--account server)))
    (nngmail--sync-safely account)
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (group (nngmail--all-group-names account))
        (insert (nngmail--active-line account group)))
      t))))

(defun nngmail--active-line (account group)
  "The active-file line for GROUP: \"GROUP HIGH LOW y\" in local numbers.
ACCOUNT is the nngmail account."
  (let ((locals (nngmail--group-locals account group))
        (high (nngmail--group-high account group)))
    (format "%S %d %d y\n" group high (if locals (car locals) (1+ high)))))

(defun nngmail--all-group-names (account)
  "All group names exposed by ACCOUNT."
  (cons nngmail-all-mail-group
        (mapcar #'cdr (nngmail-account-label-names account))))

(deffoo nngmail-request-newgroups (_date &optional server)
  (nngmail-request-list server))

;; On `g', Gnus does not call `request-group' with the group's info for
;; backends that merely have `request-list'; it reads the active line and
;; recomputes unread counts from the read ranges already in .newsrc.eld.
;; That is how nnml works, but for us the read state lives at Gmail, so
;; the *Group* buffer would only reflect what Gnus last saw.  Like
;; nnimap, we implement the -early/-finish pair, which is the one path
;; where Gnus hands us the infos for all subscribed groups at once.

(deffoo nngmail-retrieve-group-data-early (server infos)
  "Sync SERVER once; return INFOS as the token for `finish-retrieve-group-infos'."
  (let ((account (nngmail--account server)))
    (nngmail--sync-safely account)
    ;; Anything non-nil works as the token; Gnus passes it straight back.
    (or infos t)))

(deffoo nngmail-finish-retrieve-group-infos (server infos _data &optional dont-insert)
  "Update every info in INFOS from the local index and emit active lines."
  (nngmail--timed "nngmail-finish-retrieve-group-infos"
  (let ((account (nngmail--account server)))
    (dolist (info infos)
      (let ((group (gnus-group-real-name (gnus-info-group info))))
        (nngmail--update-info account group info)))
    (unless dont-insert
      (with-current-buffer nntp-server-buffer
        (erase-buffer)
        (dolist (info infos)
          (insert (nngmail--active-line account (gnus-group-real-name (gnus-info-group info)))))))
    t)))

(deffoo nngmail-request-scan (&optional _group server)
  (nngmail--timed "nngmail-request-scan"
  (nngmail--sync-safely (nngmail--account server))))

(deffoo nngmail-request-group (group &optional server dont-check info)
  (nngmail--timed "nngmail-request-group"
  (let ((account (nngmail--account server)))
    (unless dont-check
      (nngmail--sync-safely account))
    (let ((locals (nngmail--group-locals account group))
          (high (nngmail--group-high account group)))
      ;; Always push our read/tick state into the info when Gnus hands us
      ;; one, even with DONT-CHECK: `gnus-get-unread-articles' calls us
      ;; that way for every group on `g', and it is the only chance to
      ;; get UNREAD counts right in the *Group* buffer.  If Gnus didn't
      ;; pass an info, look it up ourselves for the same reason.
      (let ((info (or info
                      (gnus-get-info
                       (gnus-group-full-name
                        group (list 'nngmail (nngmail-account-name account)))))))
        (when info
          (nngmail--update-info account group info)))
      (with-current-buffer nntp-server-buffer
        (erase-buffer)
        (insert (format "211 %d %d %d %S\n"
                        (length locals)
                        (if locals (car locals) (1+ high))
                        high
                        group))
        t)))))

(deffoo nngmail-request-group-scan (group &optional server info)
  (nngmail-request-group group server nil info))

(deffoo nngmail-close-group (_group &optional _server)
  t)

(deffoo nngmail-retrieve-headers (articles &optional group server _fetch-old)
  (nngmail--timed "nngmail-retrieve-headers"
  (let* ((account (nngmail--account server))
         ;; ARTICLES are GROUP's local numbers; work in server numbers,
         ;; and remember the way back for the NOV lines.
         (pairs (and articles (numberp (car articles)) group
                     (delq nil (mapcar (lambda (l)
                                         (when-let* ((n (nngmail--local->num account group l)))
                                           (cons n l)))
                                       articles))))
         (nums (mapcar #'car pairs))
         ;; One query for everything the summary shows.
         (have (nngmail--headers account nums))
         missing)
    ;; Fetch any headers the database does not have (lazy arrivals, see
    ;; `nngmail-sync-headers'): one batch for exactly these articles.
    (dolist (n nums)
      (unless (assq n have)
        (when-let* ((id (gethash n (nngmail-account-num->id account))))
          (push id missing))))
    (when missing
      (nngmail--fetch-metadata account missing "fetching headers")
      (nngmail--progress-clear)
      (setq have (nngmail--headers account nums)))
    (with-current-buffer nntp-server-buffer
      (erase-buffer)
      (dolist (pair (sort (copy-sequence pairs) (lambda (a b) (< (cdr a) (cdr b)))))
        (when-let* ((h (cdr (assq (car pair) have))))
          ;; The stored header carries the server number; Gnus must see
          ;; the local one.  Copy so the cache keeps the server form.
          (let ((h2 (copy-sequence h)))
            (setf (mail-header-number h2) (cdr pair))
            (nnheader-insert-nov h2))))
      'nov))))

(defun nngmail--raw-message (account num)
  "Return the raw RFC 822 text of article NUM in ACCOUNT (unibyte), or nil.
Served from the database's article cache when present, else fetched
from Gmail and cached (see `nngmail-cache-articles')."
  (or (and nngmail-cache-articles
           (nngmail--timed (format "article %d: cache lookup" num)
             (nngmail--db-get-article account num)))
      (when-let* ((id (gethash num (nngmail-account-num->id account)))
                  (msg (nngmail--timed (format "article %d: messages.get raw" num)
                         (nngmail--get account (concat "messages/" id) '(("format" . "raw")))))
                  (raw (alist-get 'raw msg)))
        ;; Gmail returns base64url (RFC 4648 §5), usually without padding;
        ;; BASE64URL=t makes `base64-decode-string' accept both.
        (let ((text (base64-decode-string raw t)))
          (when nngmail-cache-articles
            (nngmail--db-put-article account num text))
          text))))

(deffoo nngmail-request-article (article &optional group server to-buffer)
  (nngmail--timed "nngmail-request-article"
  (let* ((account (nngmail--account server))
         (num (cond ((stringp article) (nngmail--find-by-message-id account article))
                    (group (nngmail--local->num account group article))
                    (t article)))
         (text (and num (nngmail--raw-message account num))))
    (when text
      (with-current-buffer (or to-buffer nntp-server-buffer)
        (erase-buffer)
        ;; TEXT is unibyte.  Do not make the buffer unibyte to match: Gnus
        ;; passes its *Article* buffer here, which is multibyte, and
        ;; `mm-decode-body' only decodes charsets in a multibyte buffer.
        ;; Inserting bytes into a multibyte buffer stores them as raw-byte
        ;; characters, which the charset decoder handles (what nnimap does).
        (insert text)
        (goto-char (point-min))
        ;; Normalise CRLF.
        (while (search-forward "\r\n" nil t) (replace-match "\n" t t))
        (nnheader-ms-strip-cr)
        (nngmail--insert-labels-header account num)
        (let ((g (or group (nngmail--group-for-number account num))))
          (cons g (or (nngmail--num->local account g num) num))))))))

(deffoo nngmail-request-head (article &optional group server to-buffer)
  (when (nngmail-request-article article group server to-buffer)
    (with-current-buffer (or to-buffer nntp-server-buffer)
      (goto-char (point-min))
      (when (search-forward "\n\n" nil t)
        (delete-region (1- (point)) (point-max))))
    (cons group article)))

(defun nngmail--group-for-number (account num)
  "Some group containing article NUM in ACCOUNT."
  (car (nngmail--message-groups account (gethash num (nngmail-account-labels account)))))

(defun nngmail--find-by-message-id (account message-id)
  "Return the article number in ACCOUNT whose Message-ID is MESSAGE-ID.
The cached headers are checked first; otherwise Gmail is asked (one
search), which also indexes the message.  The database is not scanned:
reading every stored header takes minutes on a large account."
  (or (catch 'found
        (maphash (lambda (num h)
                   (when (equal (mail-header-id h) message-id) (throw 'found num)))
                 (nngmail-account-headers account))
        nil)
      (let ((ids (nngmail--list-ids account (format "rfc822msgid:%s" message-id) nil 1)))
        (when ids
          (car (nngmail--fetch-metadata account ids))))))

(deffoo nngmail-request-thread (header &optional group server)
  "Fetch the whole Gmail thread of HEADER into the current group listing."
  (let* ((account (nngmail--account server))
         (thread-id (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra header)))))
    (when (and thread-id (not (string-empty-p thread-id)))
      (let* ((thread (nngmail--get account (concat "threads/" thread-id)
                                   '(("format" . "minimal"))))
             (ids (mapcar (lambda (m) (alist-get 'id m)) (alist-get 'messages thread)))
             (nums (nngmail--fetch-metadata account ids))
             ;; Only the thread's members that are in this group can be
             ;; shown under its numbering (the rest have no number here).
             (locals (delq nil (mapcar (lambda (n) (nngmail--num->local account group n)) nums))))
        (nngmail-retrieve-headers locals group server)))))

;;;; Marks

(defun nngmail--modify (account nums add remove)
  "Add labels ADD and remove labels REMOVE on articles NUMS in ACCOUNT."
  (let ((ids (delq nil (mapcar (lambda (n) (gethash n (nngmail-account-num->id account)))
                               nums))))
    (when (and ids (or add remove))
      (dolist (chunk (seq-partition ids 1000))
        (nngmail--post account "messages/batchModify"
                       `((ids . ,(vconcat chunk))
                         ,@(when add `((addLabelIds . ,(vconcat add))))
                         ,@(when remove `((removeLabelIds . ,(vconcat remove))))))
        (nngmail--log "batchModify %d id(s): +%S -%S" (length chunk) add remove))
      (dolist (n nums)
        (let ((labels (gethash n (nngmail-account-labels account))))
          (nngmail--set-labels account n
                               (seq-uniq (append add (seq-difference labels remove)))))))))

(deffoo nngmail-request-set-mark (group actions &optional server)
  "Push Gnus mark changes for GROUP to Gmail as label edits.
ACTIONS use GROUP's local numbering.  Only articles whose labels the
change would actually alter are sent, so an idempotent catch-up costs
nothing."
  (nngmail--timed "nngmail-request-set-mark"
  (let* ((account (nngmail--account server))
         (labels (nngmail-account-labels account)))
    (nngmail--log "set-mark request for %s: %S (group has %d members)"
                  group
                  (mapcar (lambda (a) (list (let ((r (car a)))
                                              (if (> (length (range-uncompress r)) 20)
                                                  (format "<%d articles>" (length (range-uncompress r)))
                                                r))
                                            (nth 1 a) (nth 2 a)))
                          actions)
                  (nngmail--group-size account group))
    (dolist (action actions)
      (cl-destructuring-bind (range act marks) action
        (let* ((requested (range-uncompress range))
               ;; Local numbers; retired ones (messages that left the
               ;; group) have no server number and are skipped.
               (nums (nngmail--locals->nums account group requested))
               add remove)
          (when (< (length nums) (length requested))
            (nngmail--trace "set-mark %s: %d of %d requested numbers are retired"
                            group (- (length requested) (length nums)) (length requested)))
          (pcase act
            ('add
             (when (memq 'read marks) (push "UNREAD" remove))
             (when (memq 'tick marks)
               (push "STARRED" add)
               ;; Gnus can't have ticked-but-unread; mirror nnimap.
               (push "UNREAD" remove)))
            ('del
             (when (memq 'read marks) (push "UNREAD" add))
             (when (memq 'tick marks) (push "STARRED" remove)))
            ('set
             ;; Exactly these marks and no others.
             (if (memq 'read marks) (push "UNREAD" remove) (push "UNREAD" add))
             (if (memq 'tick marks) (push "STARRED" add) (push "STARRED" remove))))
          ;; Skip articles the change would not affect (already read when
          ;; marking read, etc.), so idempotent catch-ups cost nothing.
          (setq nums (seq-filter
                      (lambda (n)
                        (let ((l (gethash n labels)))
                          (or (seq-some (lambda (a) (not (member a l))) add)
                              (seq-some (lambda (r) (member r l)) remove))))
                      nums))
          (if (null nums)
              (nngmail--log "set-mark %s: %s %S -> nothing to do (labels already reflect it)"
                            group act marks)
            (nngmail--log "set-mark %s: %s %S on %d article(s)" group act marks (length nums))
            (condition-case err
                (nngmail--modify account nums add remove)
              (nngmail-error
               (nngmail--message 3 "failed to set marks: %s" (error-message-string err))))))))
    nil)))

(deffoo nngmail-request-update-mark (_group _article mark)
  mark)

;;;; Moving, copying, expiring

(deffoo nngmail-request-move-article (article group server accept-form
                                              &optional _last internal-move-group)
  (let* ((account (nngmail--account server))
         (num (nngmail--local->num account group article)))
    (cond
     ((null num) nil)
     (internal-move-group
      ;; Label change within the same account.
      (let ((from (nngmail--group-label account group))
            (to (nngmail--group-label account internal-move-group)))
        (nngmail--modify account (list num)
                         (and to (list to))
                         (delq nil (list from
                                         ;; Leaving trash/spam restores.
                                         (and to (member from '("TRASH" "SPAM")) from))))
        ;; The message now has a number in the destination group.
        (cons internal-move-group
              (or (nngmail--num->local account internal-move-group num) num))))
     (t
      ;; Moving to another backend: hand over the article, then remove here.
      (with-temp-buffer
        (mm-disable-multibyte)
        (when (nngmail-request-article article group server (current-buffer))
          (when-let* ((result (eval accept-form t)))
            (nngmail--expire-one account num group)
            result)))))))

(deffoo nngmail-request-accept-article (group &optional server _last)
  "Import the message in the current buffer into GROUP (labels only)."
  (let* ((account (nngmail--account server))
         (label (and group (nngmail--group-label account group)))
         (raw (progn (nnmail-check-syntax)
                     (buffer-substring-no-properties (point-min) (point-max))))
         (resp (nngmail--request account "POST" (concat nngmail--upload-api "messages")
                                 '(("uploadType" . "media") ("internalDateSource" . "dateHeader"))
                                 raw "message/rfc822")))
    (when-let* ((id (alist-get 'id resp)))
      (when label
        (nngmail--post account (concat "messages/" id "/modify")
                       `((addLabelIds . ,(vector label)))))
      (let ((nums (nngmail--fetch-metadata account (list id))))
        (nngmail--save account)
        (cons group (or (and group (nngmail--num->local account group (car nums)))
                        (car nums)))))))

(defun nngmail--expire-one (account num group)
  "Apply `nngmail-expire-action' to article NUM in GROUP of ACCOUNT."
  (let ((id (gethash num (nngmail-account-num->id account)))
        (label (nngmail--group-label account group)))
    (when id
      (pcase nngmail-expire-action
        ('delete
         (nngmail--request account "DELETE" (concat "messages/" id))
         (nngmail--forget account num))
        ('archive
         (if label
             (nngmail--modify account (list num) nil (list label))
           (nngmail--modify account (list num) nil '("INBOX"))))
        (_
         (if (member "TRASH" (gethash num (nngmail-account-labels account)))
             ;; Already in trash: expire = delete for good.
             (progn (nngmail--request account "DELETE" (concat "messages/" id))
                    (nngmail--forget account num))
           (nngmail--post account (concat "messages/" id "/trash"))
           (nngmail--set-labels account num
                                (cons "TRASH" (seq-difference
                                               (gethash num (nngmail-account-labels account))
                                               '("INBOX" "UNREAD"))))))))))

(deffoo nngmail-request-expire-articles (articles group &optional server force)
  (let ((account (nngmail--account server))
        not-expired)
    (dolist (local articles)
      (let* ((num (nngmail--local->num account group local))
             (h (and num (nngmail--header account num)))
             (date (and h (mail-header-date h)))
             (time (and date (ignore-errors (date-to-time date)))))
        (cond
         ((null num))                    ; retired number: nothing to expire
         ((or force (and time (nnmail-expired-article-p group time force)))
          (condition-case err
              (nngmail--expire-one account num group)
            (nngmail-error
             (nngmail--message 3 "expire failed for %d: %s" local (error-message-string err))
             (push local not-expired))))
         (t (push local not-expired)))))
    (nngmail--save account)
    (nreverse not-expired)))

(deffoo nngmail-request-create-group (group &optional server _args)
  (let* ((account (nngmail--account server))
         (resp (nngmail--post account "labels"
                              `((name . ,group)
                                (labelListVisibility . "labelShow")
                                (messageListVisibility . "show")))))
    (when (alist-get 'id resp)
      ;; A new label has nothing under it: the name map is all that changes.
      (nngmail--relabel-groups account (nngmail--fetch-labels account))
      (nngmail--save account)
      t)))

(deffoo nngmail-request-delete-group (group &optional _force server)
  (let* ((account (nngmail--account server))
         (label (nngmail--group-label account group)))
    (when (and label (not (member label '("INBOX" "SENT" "DRAFT" "SPAM" "TRASH"))))
      (nngmail--request account "DELETE" (concat "labels/" label))
      ;; Gmail drops the label from every message but history does not
      ;; report that per message; `nngmail--relabel-groups' strips it
      ;; locally (only this group is walked).
      (nngmail--relabel-groups account (nngmail--fetch-labels account))
      (nngmail--save account)
      t)))

(deffoo nngmail-request-rename-group (group new-name &optional server)
  (let* ((account (nngmail--account server))
         (label (nngmail--group-label account group)))
    (when label
      (nngmail--request account "PATCH" (concat "labels/" label) nil `((name . ,new-name)))
      ;; The group's rows move to the new name with their local numbers;
      ;; `gnus-group-rename-group' moves the Gnus info the same way.
      ;; Nested labels (a/b under a) come back renamed in the same list.
      (nngmail--relabel-groups account (nngmail--fetch-labels account))
      (nngmail--save account)
      t)))

(deffoo nngmail-request-expunge-group (group &optional server)
  "Permanently delete everything in Trash when GROUP is the trash group."
  (let* ((account (nngmail--account server))
         (label (nngmail--group-label account group)))
    (when (equal label "TRASH")
      (dolist (num (nngmail--group-numbers account group))
        (let ((nngmail-expire-action 'delete))
          (nngmail--expire-one account num group)))
      (nngmail--save account)
      t)))

(deffoo nngmail-request-post (&optional _server)
  (nnheader-report 'nngmail "nngmail groups are mail groups; use nngmail-message-send-mail"))

;;;; Sending

(defun nngmail--base64url (string)
  "Encode unibyte STRING as base64url without padding."
  (let ((b64 (base64-encode-string string t)))
    (replace-regexp-in-string
     "=+\\'" ""
     (replace-regexp-in-string "/" "_" (replace-regexp-in-string "+" "-" b64)))))

(defun nngmail--reply-thread-id (account buffer)
  "Gmail thread id the message in BUFFER replies to, or nil.
Gmail files a sent message into an existing thread only when the send
names the thread (`threadId') in addition to the References/In-Reply-To
headers and a matching subject.  The thread comes from
`message-reply-headers' when the reply was started from an nngmail
article (its extra headers carry X-Gmail-Thread-Id); otherwise the
In-Reply-To message is looked up, one request at most.
ACCOUNT is the nngmail account."
  (with-current-buffer buffer
    (or (and (boundp 'message-reply-headers)
             (vectorp message-reply-headers)
             (let ((tid (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra message-reply-headers)))))
               (and tid (not (string-empty-p tid)) tid)))
        (when-let* ((parent (save-restriction
                              (message-narrow-to-headers)
                              (message-fetch-field "In-Reply-To")))
                    (id (and (string-match "<[^>]+>" parent) (match-string 0 parent)))
                    (num (nngmail--find-by-message-id account id))
                    (h (nngmail--header account num))
                    (tid (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra h)))))
          (and (not (string-empty-p tid)) tid)))))

(defun nngmail--server-for-sending ()
  "Pick the nngmail server to send through, based on the From header."
  (require 'message)
  (let* ((from (save-restriction
                 (message-narrow-to-headers)
                 (or (message-fetch-field "from") "")))
         (addr (car (mail-header-parse-address from)))
         match)
    (maphash (lambda (name a)
               (when (and (not match) addr
                          (string-equal-ignore-case addr (nngmail-account-user a)))
                 (setq match name)))
             nngmail--accounts)
    (or match
        (car (nngmail-configured-servers)))))

(defvar nngmail-last-sent-id nil
  "Gmail id of the last message `nngmail-message-send-mail' sent.
A sent message lands in the sender's own mailbox (Sent, and INBOX when
addressed to self); this lets a caller find that copy without a search.")

;;;###autoload
(defun nngmail-message-send-mail (&optional _arg)
  "Send the current message buffer through the Gmail API.
Usable as the option `message-send-mail-function'.  The account is chosen by
matching the From address against open nngmail servers (falling back to
the first configured one)."
  (require 'message)
  (let* ((server (or (nngmail--server-for-sending)
                     (error "nngmail: No nngmail server to send through")))
         (account (nngmail--account server))
         (source (current-buffer))
         (raw (with-temp-buffer
                (mm-disable-multibyte)
                (insert-buffer-substring source)
                ;; `message-send-mail' hands us the buffer with Gnus's
                ;; header separator still in it (every send function
                ;; removes it itself: see `message-send-mail-with-sendmail').
                ;; Gmail sees a message whose headers end at the
                ;; separator line and answers "Recipient address required".
                (goto-char (point-min))
                (when (re-search-forward
                       (concat "^" (regexp-quote mail-header-separator) "\n") nil t)
                  (replace-match "\n" t t))
                ;; Bcc must not travel; Gmail delivers to it but keeps it
                ;; in the stored copy, and some clients then show it.
                (save-restriction
                  (message-narrow-to-headers)
                  (message-remove-header "Bcc"))
                (run-hooks 'message-send-mail-hook)
                (buffer-string)))
         (raw (if (multibyte-string-p raw) (encode-coding-string raw 'utf-8) raw))
         (thread (nngmail--reply-thread-id account source))
         (what (nngmail--send-describe raw))
         (resp
          ;; The wait is synchronous (message.el expects the answer) but
          ;; not a freeze: progress state makes the spinner run and the
          ;; status line redraw inside `nngmail--plz-wait'.
          (nngmail--with-progress account (format "sending %s" what)
            (nngmail--post account "messages/send"
                           `((raw . ,(nngmail--base64url raw))
                             ,@(and thread `((threadId . ,thread))))))))
    (unless (alist-get 'id resp)
      (error "nngmail: Sending failed: %S" resp))
    (setq nngmail-last-sent-id (alist-get 'id resp))
    (nngmail--message 5 "sent %s (id %s)" what nngmail-last-sent-id)
    t))

(defun nngmail--send-describe (raw)
  "Subject and first recipient of the RFC 822 text RAW, for messages."
  (with-temp-buffer
    (insert raw)
    (goto-char (point-min))
    (let ((subject (mail-fetch-field "subject"))
          (to (mail-fetch-field "to")))
      (format "%S to %s" (or subject "(no subject)") (or to "?")))))


;;;; Search

(defun nngmail--search (account query &optional limit)
  "Run Gmail QUERY in ACCOUNT; return article numbers (newest first)."
  (let ((ids (nngmail--list-ids account query nil (or limit 500))))
    (when ids
      (nngmail--fetch-metadata account ids)
      (nngmail--save account)
      ;; Preserve Gmail's relevance/recency order.
      (delq nil (mapcar (lambda (id) (gethash id (nngmail-account-id->num account))) ids)))))

(defclass gnus-search-gmail (gnus-search-engine)
  ((raw-queries-p :initform nil)
   (limit :initarg :limit :initform 500 :type integer
          :documentation "Maximum number of results per search."))
  :documentation "Gnus search engine using the Gmail API's search.")

(cl-defmethod gnus-search-run-search ((engine gnus-search-gmail) srv query groups)
  "Run QUERY against Gmail's search for server SRV with ENGINE; GROUPS is ignored."
  (let* ((method (gnus-server-to-method srv))
         (server (cadr method))
         (account (nngmail--account server))
         (q-string (gnus-search-make-query-string engine query))
         (labels (delq nil (mapcar (lambda (g)
                                     (nngmail--group-label account (gnus-group-short-name g)))
                                   groups)))
         (q (if (and labels (not (equal q-string "")))
                (format "(%s) {%s}" q-string
                        (mapconcat (lambda (l)
                                     (concat "label:" (nngmail--gmail-label-query account l)))
                                   labels " "))
              q-string))
         ;; Results are shown in All Mail, which never contains spam or
         ;; trash; keep the search consistent unless the query itself asks
         ;; for them (in:spam, in:trash, in:anywhere, label:spam...).
         (q (if (string-match-p "\\b\\(in\\|label\\):\\(spam\\|trash\\|anywhere\\)\\b" q)
                q
              (concat q " -in:spam -in:trash")))
         (nums (nngmail--search account q (slot-value engine 'limit)))
         (target (gnus-group-full-name nngmail-all-mail-group method)))
    (gnus-message 7 "nngmail search: %s" q)
    ;; Results are addressed in All Mail's local numbering.
    (vconcat (delq nil (mapcar (lambda (n)
                                 (when-let* ((l (nngmail--num->local account nngmail-all-mail-group n)))
                                   (vector target l 100)))
                               nums)))))

(cl-defmethod gnus-search-transform ((_ gnus-search-gmail) (_query null))
  "An empty query is the empty Gmail search."
  "")

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-gmail) (expr (head or)))
  "Transform an `or' EXPR into Gmail's OR syntax with ENGINE."
  (let ((l (gnus-search-transform-expression engine (nth 1 expr)))
        (r (gnus-search-transform-expression engine (nth 2 expr))))
    (if (and l r) (format "{%s %s}" l r) (or l r))))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-gmail) (expr (head not)))
  "Transform a `not' EXPR into Gmail's minus prefix with ENGINE."
  (when-let* ((n (gnus-search-transform-expression engine (cadr expr))))
    (format "-%s" n)))

(cl-defmethod gnus-search-transform-expression ((engine gnus-search-gmail) (expr list))
  "Transform a (KEY . VALUE) EXPR into a Gmail search operator with ENGINE."
  (cl-flet ((gdate (d) (if (stringp d) d
                         (pcase d
                           (`(,day ,m ,y) (format "%d/%02d/%02d" (or y (nth 5 (decode-time))) (or m 1) (or day 1)))
                           (_ nil))))
            (q (s) (if (string-match-p "[[:space:]]" s) (format "\"%s\"" s) s)))
    (pcase (car expr)
      ((pred consp) (format "(%s)" (gnus-search-transform engine expr)))
      ((or 'from 'sender) (format "from:%s" (q (cdr expr))))
      ((or 'to 'recipient) (format "to:%s" (q (cdr expr))))
      ('cc (format "cc:%s" (q (cdr expr))))
      ('bcc (format "bcc:%s" (q (cdr expr))))
      ('address (format "{from:%s to:%s}" (q (cdr expr)) (q (cdr expr))))
      ('subject (format "subject:%s" (q (cdr expr))))
      ('body (q (cdr expr)))
      ('text (q (cdr expr)))
      ('attachment (format "filename:%s" (q (cdr expr))))
      ('id (format "rfc822msgid:%s" (cdr expr)))
      ('thread nil)
      ('since (when-let* ((d (gdate (cdr expr)))) (format "after:%s" d)))
      ('before (when-let* ((d (gdate (cdr expr)))) (format "before:%s" d)))
      ('on (when-let* ((d (gdate (cdr expr)))) (format "after:%s before:%s" d d)))
      ('larger (format "larger:%s" (cdr expr)))
      ('smaller (format "smaller:%s" (cdr expr)))
      ('mark (pcase (cdr expr)
               ((or "flag" "flagged" "tick") "is:starred")
               ((or "read" "seen") "is:read")
               ((or "unread" "unseen") "is:unread")
               ("replied" "in:sent")
               (m (format "label:%s" m))))
      ('label (format "label:%s" (q (cdr expr))))
      ('has (format "has:%s" (cdr expr)))
      ('in (format "in:%s" (cdr expr)))
      ('is (format "is:%s" (cdr expr)))
      (_ (ignore-errors (cl-call-next-method))))))

(add-to-list 'gnus-search-default-engines '(nngmail . gnus-search-gmail))

(defun nngmail--gmail-label-query (account label-id)
  "Turn LABEL-ID of ACCOUNT into the token Gmail's label: operator expects."
  (pcase label-id
    ("INBOX" "inbox") ("SENT" "sent") ("DRAFT" "draft") ("SPAM" "spam")
    ("TRASH" "trash") ("STARRED" "starred") ("UNREAD" "unread")
    (_
     ;; User labels: Gmail's search wants the display name with spaces and
     ;; slashes turned into dashes.
     (replace-regexp-in-string
      "[ /]" "-"
      (or (cdr (assoc label-id (nngmail-account-label-names account))) label-id)))))

;;;; Showing a message's other labels
;;
;; A Gmail message can sit in several nngmail groups at once, and the
;; group view hides that.  Three ways to surface it, usable together:
;;
;;  1. `%u&gmail-labels;' in `gnus-summary-line-format': the message's
;;     *other* labels (everything except the group you are in and system
;;     noise), as a compact string.  `%u&gmail-label-count;' gives just
;;     the count.  Empty when the message lives only here, so it costs
;;     nothing visually in the common case.
;;     Labels in `nngmail-label-display-alist' show as an icon with an
;;     optional face (Gmail's superstars are yellow, red... stars by
;;     default); `nngmail-summary-explain-labels' decodes the icons.
;;  2. `%u&gmail-label-mark;': a fixed-width cell that is empty, or
;;     `nngmail-label-mark', or the icons of the other labels drawn as
;;     one SVG image (vector shapes; one icon, or up to four in a grid).
;;
;; These use Gnus's named user-format syntax (`%u&name;') rather than
;; single letters, so they cannot collide with any
;; `gnus-user-format-function-X' you define yourself.
;;  3. In the article buffer, an `X-Gmail-Labels' header listing all
;;     labels by display name (`nngmail-article-labels-header').
;;
;; Labels are read from the NOV extra header, so none of this touches
;; the network.

(defcustom nngmail-summary-hidden-labels
  '("UNREAD" "STARRED" "IMPORTANT" "SENT" "CHAT" "DRAFT"
    "CATEGORY_PERSONAL" "CATEGORY_SOCIAL" "CATEGORY_PROMOTIONS"
    "CATEGORY_UPDATES" "CATEGORY_FORUMS")
  "Label ids never shown as other labels in the summary.
These are state or Gmail-internal categories rather than places the
user filed the message.  INBOX is deliberately not here: a message in a
label group that is also still in INBOX is worth knowing about."
  :type '(repeat string))

(defcustom nngmail-label-mark ?&
  "Mark shown by `%u&gmail-label-mark;' for a message with other labels.
Used when none of the other labels has an icon in
`nngmail-label-display-alist'; otherwise the icon itself is shown."
  :type 'character)

(defcustom nngmail-label-mark-width 2
  "Width in character cells of `%u&gmail-label-mark;'.
On graphical displays the cell is one image of exactly this many cells,
so the summary line stays aligned whatever the cell shows."
  :type 'integer)

(defcustom nngmail-summary-label-separator ","
  "Separator between label names in `%u&gmail-labels;'.
Labels shown as icons (see `nngmail-label-display-alist') are not
separated; they come first, followed by the remaining names."
  :type 'string)

(defcustom nngmail-label-shapes
  '((star     "\u2605" . nngmail--shape-star)
    (circle   "\u25cf" . nngmail--shape-circle)
    (square   "\u25a0" . nngmail--shape-square)
    (diamond  "\u25c6" . nngmail--shape-diamond)
    (triangle "\u25b2" . nngmail--shape-triangle)
    (heart    "\u2665" . nngmail--shape-heart)
    (flag     "\u2691" . nngmail--shape-flag)
    (check    "\u2713" . nngmail--shape-check)
    (bang     "!"      . nngmail--shape-bang)
    (question "?"      . nngmail--shape-question)
    (info     "\u2139" . nngmail--shape-info)
    (chevron  "\u00bb" . nngmail--shape-chevron)
    (person   "\u263a" . nngmail--shape-person))
  "Vector shapes usable as label icons: (NAME FALLBACK . DRAW-FUNCTION).
FALLBACK is the character shown where images are not available (text
terminals, `%u&gmail-labels;', the legend).  DRAW-FUNCTION is called
with (SVG X Y SIZE COLOR) and draws the shape inside the SIZE x SIZE
box whose top-left corner is X,Y."
  :type '(alist :key-type symbol
                :value-type (cons (string :tag "Fallback") function)))

(defcustom nngmail-label-display-alist
  '(("YELLOW_STAR" star :foreground "gold")
    ("ORANGE_STAR" star :foreground "dark orange")
    ("RED_STAR" star :foreground "red")
    ("PURPLE_STAR" star :foreground "medium purple")
    ("BLUE_STAR" star :foreground "dodger blue")
    ("GREEN_STAR" star :foreground "lime green")
    ("RED_BANG" bang :foreground "red")
    ("YELLOW_BANG" bang :foreground "gold")
    ("ORANGE_GUILLEMET" chevron :foreground "dark orange")
    ("GREEN_CHECK" check :foreground "lime green")
    ("BLUE_INFO" info :foreground "dodger blue")
    ("PURPLE_QUESTION" question :foreground "medium purple"))
  "How to show particular labels in the summary line.
Each entry is (LABEL ICON . FACE).  LABEL is a Gmail label id or a
label display name, matched exactly.  ICON is either a shape name from
`nngmail-label-shapes', drawn as vector graphics in the mark cell, or a
string such as an emoji, drawn as text.  Shapes are preferred: they
are exactly the same size on every line and need no emoji font.  FACE
is optional: a face name, or attributes such as (:foreground \"gold\");
its foreground is the icon colour.

The defaults cover the Gmail superstar labels.  Add your own, for
example:

  (push \='(\"Team Lead\" person :foreground \"sky blue\")
        nngmail-label-display-alist)

For anything more dynamic, set `nngmail-label-display-function'."
  :type '(alist :key-type (string :tag "Label id or name")
                :value-type (cons (choice (symbol :tag "Shape") (string :tag "Text"))
                                  (choice (const :tag "No face" nil)
                                          face
                                          (plist :tag "Face attributes")))))

(defcustom nngmail-label-display-function nil
  "Function that decides how a label appears in the summary line, or nil.
Called with LABEL-ID and NAME (the group name).  Return nil to fall
back to `nngmail-label-display-alist', a shape symbol or string to show
that, or \(ICON . FACE) to show ICON in FACE.  Consulted before the alist."
  :type '(choice (const nil) function))

(defun nngmail--label-display (id name)
  "Return (ICON . FACE) for label ID / NAME if customized, else nil.
ICON is a shape symbol or a string."
  (let ((d (or (and nngmail-label-display-function
                    (funcall nngmail-label-display-function id name))
               (cdr (or (assoc id nngmail-label-display-alist)
                        (assoc name nngmail-label-display-alist))))))
    (cond ((null d) nil)
          ((or (stringp d) (symbolp d)) (cons d nil))
          ((consp d) (cons (car d) (cdr d))))))

(defun nngmail--icon-text (icon)
  "Text form of ICON: the string itself, or a shape's fallback character."
  (if (stringp icon)
      icon
    (or (cadr (assq icon nngmail-label-shapes)) "?")))

(defun nngmail--label-icon-string (display)
  "Text for DISPLAY, an (ICON . FACE) pair, keeping FACE under Gnus highlighting.
`gnus-summary-highlight-line' overwrites `face' on the whole line, except
where `gnus-face' is set; there it only replaces the second element of
the face list.  A fresh list per call is required because Gnus mutates
it in place, so this string must never be cached."
  (let ((string (nngmail--icon-text (car display)))
        (face (cdr display)))
    (if (null face)
        string
      (propertize string 'face (list face 'default) 'gnus-face t))))

(defun nngmail--header-label-ids (header)
  "Return the label ids recorded in HEADER's NOV extra, or nil."
  (when-let* ((s (cdr (assq 'X-Gmail-Labels (mail-header-extra header)))))
    (and (not (string-empty-p s)) (split-string s ","))))

(defun nngmail--current-account-for-summary ()
  "Return the nngmail account behind the current summary buffer, or nil."
  (when (and (boundp 'gnus-newsgroup-name) gnus-newsgroup-name)
    (let ((m (gnus-find-method-for-group gnus-newsgroup-name)))
      (when (eq (car-safe m) 'nngmail)
        (gethash (cadr m) nngmail--accounts)))))

(defun nngmail--other-label-ids (header)
  "Ids of HEADER's labels other than the current group's, in NOV order.
System noise (`nngmail-summary-hidden-labels'), quarantine labels and
hidden labels are dropped.  Returns nil outside nngmail groups."
  (when-let* ((account (nngmail--current-account-for-summary))
              (ids (nngmail--header-label-ids header)))
    (let ((here (nngmail--group-label account (gnus-group-real-name gnus-newsgroup-name)))
          (result nil))
      (dolist (id ids)
        (unless (or (equal id here)
                    (member id nngmail-summary-hidden-labels)
                    (member id nngmail-quarantine-labels)
                    (null (nngmail--label-group account id)))
          (push id result)))
      (nreverse result))))

(defun nngmail--other-labels (header)
  "Display names of HEADER's labels other than the current group's."
  (when-let* ((account (nngmail--current-account-for-summary)))
    (mapcar (lambda (id) (nngmail--label-group account id))
            (nngmail--other-label-ids header))))

(defun nngmail--format-other-labels (header)
  "Other labels of HEADER as one string: icons first, then names.
On graphical displays the icons are one image, a text cell per icon, so
the column keeps its width: a star or emoji glyph is drawn wider than
the one column `string-width' counts for it, which shifted everything
after it on lines that had icons."
  (when-let* ((account (nngmail--current-account-for-summary)))
    (let (displays names)
      (dolist (id (nngmail--other-label-ids header))
        (let* ((name (nngmail--label-group account id))
               (display (nngmail--label-display id name)))
          (if display
              (push display displays)
            (push name names))))
      (setq displays (nreverse displays))
      (concat (cond ((null displays) "")
                    ((nngmail--svg-p)
                     (nngmail--label-mark-cell displays (length displays) 'row))
                    (t (mapconcat #'nngmail--label-icon-string displays "")))
              (and displays names " ")
              (mapconcat #'identity (nreverse names) nngmail-summary-label-separator)))))

;;;###autoload
(defun gnus-user-format-function-gmail-labels (header)
  "Other labels of HEADER, for `%u&gmail-labels;' in `gnus-summary-line-format'.
Labels listed in `nngmail-label-display-alist' appear as their icon."
  (or (nngmail--format-other-labels header) ""))

;;;###autoload
(defun gnus-user-format-function-gmail-label-count (header)
  "Number of other labels of HEADER, for `%u&gmail-label-count;'; empty when none."
  (let ((n (length (nngmail--other-labels header))))
    (if (zerop n) "" (number-to-string n))))

;; The mark cell as one image.
;;
;; Fonts cannot give aligned icons: `string-width' says a star is one
;; column, yet many fonts draw it wider, and emoji glyphs vary in size
;; and height between fonts.  So on graphical displays the cell is a
;; single SVG image, exactly `nngmail-label-mark-width' cells wide and
;; one line high, and the icons are drawn into it as vector shapes (or
;; as text, for string icons).  One icon fills the cell; several are
;; placed in a 2x2 grid, so lines with two, three or four icons look
;; alike.  Images are cached by content and size.

(defun nngmail--face-foreground (face)
  "Foreground colour of FACE (a face, attribute plist or nil) as a colour name."
  (cl-flet ((usable (c) (and (stringp c) (not (string-prefix-p "unspecified" c)) c)))
    (or (usable (cond ((null face) nil)
                      ((keywordp (car-safe face)) (plist-get face :foreground))
                      ((facep face) (face-attribute face :foreground nil 'default))))
        (usable (face-attribute 'default :foreground))
        (usable (frame-parameter nil 'foreground-color))
        "currentColor")))

(defun nngmail--svg-color (name)
  "Turn Emacs colour NAME into a value librsvg understands."
  (if-let* ((rgb (and (not (equal name "currentColor")) (color-values name))))
      (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (ash v -8)) rgb))
    name))

;; Shape drawers.  Each gets the SVG, the box origin X Y, its SIZE and
;; the fill COLOR.  Shapes are declared on a 16x16 grid and placed with
;; a transform, so the path data stays readable.

(defun nngmail--shape-in-box (svg x y size children)
  "Add CHILDREN (dom nodes on a 16x16 grid) to SVG, scaled into the box X Y SIZE."
  (svg--append svg (apply #'dom-node 'g
                          `((transform . ,(format "translate(%s %s) scale(%s)"
                                                  x y (/ size 16.0))))
                          children)))

(defun nngmail--shape-path (d color &rest attrs)
  "A path node with data D filled with COLOR, plus ATTRS alist."
  (dom-node 'path `((d . ,d) (fill . ,color) ,@attrs)))

(defun nngmail--shape-stroke (d color width)
  "An unfilled path node with data D, stroked in COLOR at WIDTH."
  (dom-node 'path `((d . ,d) (fill . "none") (stroke . ,color)
                    (stroke-width . ,width)
                    (stroke-linecap . "round") (stroke-linejoin . "round"))))

(defun nngmail--star-points ()
  "Path data for a five-pointed star centred at 8,8 on the 16 grid."
  (let ((points nil))
    (dotimes (i 10)
      (let ((r (if (cl-evenp i) 7.5 3.0))
            (a (- (* i (/ float-pi 5)) (/ float-pi 2))))
        (push (format "%.2f,%.2f" (+ 8 (* r (cos a))) (+ 8 (* r (sin a)))) points)))
    (concat "M" (mapconcat #'identity (nreverse points) " L") " Z")))

(defun nngmail--shape-star (svg x y size color)
  "Draw the star inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box svg x y size
                         (list (nngmail--shape-path (nngmail--star-points) color))))

(defun nngmail--shape-circle (svg x y size color)
  "Draw the circle inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box svg x y size
                         (list (dom-node 'circle `((cx . 8) (cy . 8) (r . 6.5) (fill . ,color))))))

(defun nngmail--shape-square (svg x y size color)
  "Draw the square inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box svg x y size
                         (list (dom-node 'rect `((x . 2) (y . 2) (width . 12) (height . 12)
                                                 (rx . 1.5) (fill . ,color))))))

(defun nngmail--shape-diamond (svg x y size color)
  "Draw the diamond inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box svg x y size
                         (list (nngmail--shape-path "M8,1 L15,8 L8,15 L1,8 Z" color))))

(defun nngmail--shape-triangle (svg x y size color)
  "Draw the triangle inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box svg x y size
                         (list (nngmail--shape-path "M8,1.5 L15,14.5 L1,14.5 Z" color))))

(defun nngmail--shape-heart (svg x y size color)
  "Draw the heart inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box
   svg x y size
   (list (nngmail--shape-path
          "M8,14 C1,9 1,3 4.5,3 C6.5,3 8,5 8,5 C8,5 9.5,3 11.5,3 C15,3 15,9 8,14 Z"
          color))))

(defun nngmail--shape-flag (svg x y size color)
  "Draw the flag inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box
   svg x y size
   (list (dom-node 'rect `((x . 3) (y . 2) (width . 1.5) (height . 12) (fill . ,color)))
         (nngmail--shape-path "M4.5,2 L13,5 L4.5,8 Z" color))))

(defun nngmail--shape-check (svg x y size color)
  "Draw the check inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box svg x y size
                         (list (nngmail--shape-stroke "M3,8.5 L6.5,12 L13,4.5" color 2.5))))

(defun nngmail--shape-bang (svg x y size color)
  "Draw the bang inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box
   svg x y size
   (list (dom-node 'rect `((x . 6.5) (y . 2) (width . 3) (height . 8) (rx . 1.5) (fill . ,color)))
         (dom-node 'circle `((cx . 8) (cy . 13) (r . 1.7) (fill . ,color))))))

(defun nngmail--shape-question (svg x y size color)
  "Draw the question inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box
   svg x y size
   (list (nngmail--shape-stroke "M4,5.5 C4,1 12.5,1 12.5,5.5 C12.5,8.5 8,8 8,10.5" color 2.4)
         (dom-node 'circle `((cx . 8) (cy . 13.5) (r . 1.6) (fill . ,color))))))

(defun nngmail--shape-info (svg x y size color)
  "Draw the info inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box
   svg x y size
   (list (dom-node 'circle `((cx . 8) (cy . 8) (r . 7) (fill . ,color)))
         (dom-node 'circle '((cx . 8) (cy . 4.5) (r . 1.4) (fill . "white")))
         (dom-node 'rect '((x . 6.8) (y . 7) (width . 2.4) (height . 5.5) (fill . "white"))))))

(defun nngmail--shape-chevron (svg x y size color)
  "Draw the chevron inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box
   svg x y size
   (list (nngmail--shape-stroke "M1.5,3 L6.5,8 L1.5,13" color 2.2)
         (nngmail--shape-stroke "M7,3 L12,8 L7,13" color 2.2))))

(defun nngmail--shape-person (svg x y size color)
  "Draw the person inside the SIZE box at X Y on SVG in COLOR."
  (nngmail--shape-in-box
   svg x y size
   (list (dom-node 'circle `((cx . 8) (cy . 5) (r . 3.2) (fill . ,color)))
         (nngmail--shape-path "M1.5,15 C1.5,8.5 14.5,8.5 14.5,15 Z" color))))

(defun nngmail--svg-text-icon (svg text x y size color)
  "Draw TEXT centred in the SIZE box at X,Y on SVG in COLOR."
  (svg-text svg text
            :x (+ x (/ size 2.0))
            ;; Baseline about a third of the em below the middle.
            :y (+ y (/ size 2.0) (* size 0.35))
            :font-size (* size 0.9) :text-anchor "middle" :fill color
            :font-family "sans-serif, 'Apple Color Emoji', 'Noto Color Emoji', 'Segoe UI Emoji'"))

(defun nngmail--draw-icon (svg icon color x y size)
  "Draw ICON (shape symbol or string) into SVG at X,Y with SIZE and COLOR."
  (if-let* ((drawer (and (symbolp icon) (cddr (assq icon nngmail-label-shapes)))))
      (funcall drawer svg x y size color)
    (nngmail--svg-text-icon svg (nngmail--icon-text icon) x y size color)))

(defvar nngmail--label-mark-images (make-hash-table :test #'equal)
  "Cache: (ICON-COLOR-LIST WIDTH CHAR-WIDTH LINE-HEIGHT) -> image spec.")

(defun nngmail--cell-pixel-size (width)
  "Return (PIXELS . LINE-HEIGHT) of WIDTH space characters in the current buffer.
Measured in the buffer rather than taken from `frame-char-width', so a
summary buffer whose default face is not the frame font (text-scale,
a buffer-local font, HiDPI rounding) still gets an image exactly as
wide as the columns it replaces."
  (cons (string-pixel-width (make-string width ?\s) (current-buffer))
        (frame-char-height)))

(defun nngmail--label-mark-image (icons width &optional layout)
  "Image spec WIDTH cells wide showing ICONS, a list of (ICON . COLOR).
LAYOUT `grid' (the default): one icon fills the cell, two to four share
a 2x2 grid, further ones are dropped.  LAYOUT `row': each icon gets one
text cell of the WIDTH, left to right, so a run of icons inside a text
column occupies exactly as many columns as it counts for."
  (let* ((size (nngmail--cell-pixel-size width))
         (px (car size))
         (lh (cdr size))
         (key (list icons width px lh layout)))
    (or (gethash key nngmail--label-mark-images)
        (let* ((svg (progn (require 'svg) (nngmail--svg px lh)))
               (i 0))
          (if (eq layout 'row)
              (let* ((cell (/ (float px) (max 1 width)))
                     (size (min cell lh))
                     (oy (/ (- lh size) 2.0)))
                (dolist (icon (seq-take icons width))
                  (nngmail--draw-icon svg (car icon) (nngmail--svg-color (cdr icon))
                                      (+ (* i cell) (/ (- cell size) 2.0)) oy size)
                  (cl-incf i)))
            (let* ((icons (seq-take icons 4))
                   (grid (cdr icons))
                   (size (if grid (/ (min px lh) 2.0) (min px lh)))
                   (ox (/ (- px (* size (if grid 2 1))) 2.0))
                   (oy (/ (- lh (* size (if grid 2 1))) 2.0)))
              (dolist (icon icons)
                (nngmail--draw-icon svg (car icon) (nngmail--svg-color (cdr icon))
                                    (+ ox (* size (% i 2))) (+ oy (* size (/ i 2))) size)
                (cl-incf i))))
          ;; :scale 1 -- Emacs otherwise multiplies image sizes by
          ;; `image-scaling-factor', which is not 1 on HiDPI displays and
          ;; would widen the cell past the columns it stands for.
          (puthash key (svg-image svg :ascent 'center :scale 1)
                   nngmail--label-mark-images)))))

(defun nngmail--pad-to-width (string width)
  "Pad STRING with spaces on the right up to WIDTH columns."
  (let ((pad (- width (string-width string))))
    (if (> pad 0) (concat string (make-string pad ?\s)) string)))

(defun nngmail--label-mark-cell (displays width &optional layout)
  "The mark cell for DISPLAYS, a list of (ICON . FACE), WIDTH columns wide.
An image on graphical displays (LAYOUT as in `nngmail--label-mark-image'),
else the first icon's text, padded."
  (if (nngmail--svg-p)
      (propertize (make-string width ?\s)
                  'display (nngmail--label-mark-image
                            (mapcar (lambda (d) (cons (car d) (nngmail--face-foreground (cdr d))))
                                    displays)
                            width layout))
    (nngmail--pad-to-width (nngmail--label-icon-string (car displays)) width)))

(defun nngmail--label-mark-legend (account ids)
  "Tooltip text naming the labels IDS of ACCOUNT, icon first where one exists."
  (mapconcat (lambda (id)
               (let* ((name (nngmail--label-group account id))
                      (display (nngmail--label-display id name)))
                 (if display (format "%s %s" (nngmail--icon-text (car display)) name) name)))
             ids "\n"))

;;;###autoload
(defun gnus-user-format-function-gmail-label-mark (header)
  "Compact indicator of HEADER's other labels, `nngmail-label-mark-width' wide.
Empty when the message lives only here.  `nngmail-label-mark' when the
other labels have no icons.  Otherwise the icons from
`nngmail-label-display-alist': one fills the cell, up to four share a
2x2 grid.  Hovering shows the label names.
For `%u&gmail-label-mark;' in `gnus-summary-line-format'."
  (let* ((account (nngmail--current-account-for-summary))
         (ids (and account (nngmail--other-label-ids header)))
         (displays (delq nil (mapcar (lambda (id)
                                       (nngmail--label-display id (nngmail--label-group account id)))
                                     ids)))
         (width nngmail-label-mark-width))
    (if (null ids)
        (make-string width ?\s)
      (propertize (nngmail--label-mark-cell
                   (or displays (list (cons (string nngmail-label-mark) nil)))
                   width)
                  'help-echo (nngmail--label-mark-legend account ids)))))

(defcustom nngmail-article-labels-header t
  "If non-nil, show an `X-Gmail-Labels' header in the article buffer.
Lists all of the message's labels by display name.  Add it to
`gnus-visible-headers' (or remove it from `gnus-ignored-headers') if
your header filtering hides unknown headers."
  :type 'boolean)

(defun nngmail--insert-labels-header (account num)
  "Insert an X-Gmail-Labels header for article NUM at point (after headers).
ACCOUNT is the nngmail account."
  (when nngmail-article-labels-header
    (when-let* ((labels (gethash num (nngmail-account-labels account))))
      (let ((names (delq nil (mapcar (lambda (id)
                                       (or (nngmail--label-group account id)
                                           (and (not (member id nngmail-summary-hidden-labels)) id)))
                                     labels))))
        (when names
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward "^$" nil t)
              (goto-char (line-beginning-position))
              ;; The buffer holds the message's bytes (raw-byte
              ;; characters when it is multibyte).  Keep the header
              ;; ASCII so it is right either way: RFC 2047 for
              ;; non-ASCII names, as in any header on the wire.
              (insert (rfc2047-encode-string
                       (concat "X-Gmail-Labels: " (mapconcat #'identity names ", ") "\n"))))))))))

;;;###autoload
(defun nngmail-summary-show-labels ()
  "Show all labels of the article at point in the echo area."
  (interactive)
  (let* ((header (gnus-summary-article-header))
         (account (nngmail--current-account-for-summary)))
    (unless (and header account)
      (user-error "Not on an nngmail article"))
    (let ((names (mapcar (lambda (id) (or (nngmail--label-group account id) id))
                         (nngmail--header-label-ids header))))
      (message "Labels: %s" (mapconcat #'identity names ", ")))))

;;;###autoload
(defun nngmail-summary-explain-labels ()
  "Show the icon legend for the article at point: icon -> label name."
  (interactive)
  (let* ((header (gnus-summary-article-header))
         (account (nngmail--current-account-for-summary)))
    (unless (and header account)
      (user-error "Not on an nngmail article"))
    (let (parts)
      (dolist (id (nngmail--header-label-ids header))
        (let* ((name (or (nngmail--label-group account id) id))
               (display (nngmail--label-display id name)))
          (push (if display
                    (format "%s = %s" (nngmail--label-icon-string display) name)
                  name)
                parts)))
      (message "Labels: %s" (mapconcat #'identity (nreverse parts) "  ")))))

;;;; Editing labels from the summary
;;
;; Gmail messages carry a set of labels; Gnus groups show one label at
;; a time.  `B m' and `B c' edit one label relative to the group you are
;; in (see the README's Model section).  These commands edit the whole
;; set, for the article at point or the process-marked articles, and go
;; through `nngmail--modify' so Gmail, the local index and the other
;; groups all agree afterwards.  They live on the `L' prefix in nngmail
;; summary buffers (`nngmail-summary-label-prefix-key').

(defcustom nngmail-summary-label-prefix-key "L"
  "Prefix key in nngmail summary buffers for the label commands.
Set before nngmail loads.  nil installs no bindings."
  :type '(choice (const nil) key-sequence))

(defun nngmail--summary-context (&optional articles)
  "Return (ACCOUNT GROUP NUMS) for ARTICLES (local numbers) in this summary.
ARTICLES defaults to the process-marked articles, else the one at point."
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "nngmail: Not in a summary buffer"))
  (let ((method (gnus-find-method-for-group gnus-newsgroup-name)))
    (unless (eq (car-safe method) 'nngmail)
      (user-error "nngmail: %s is not an nngmail group" gnus-newsgroup-name))
    (let* ((account (nngmail--account (cadr method)))
           (group (gnus-group-real-name gnus-newsgroup-name))
           (locals (or articles (gnus-summary-work-articles nil)))
           (nums (nngmail--locals->nums account group locals)))
      (when (null nums) (user-error "nngmail: No article here"))
      (list account group nums))))

(defun nngmail--editable-labels (account)
  "Alist (DISPLAY-NAME . LABEL-ID) of labels a user may put on a message.
System state labels (UNREAD, STARRED, the categories) are left out;
those are marks, not labels.  INBOX, SPAM and TRASH are offered.
ACCOUNT is the nngmail account."
  (let (out)
    (pcase-dolist (`(,id . ,name) (nngmail-account-label-names account))
      (unless (or (member id nngmail-summary-hidden-labels)
                  (member id '("DRAFT" "SENT" "CHAT")))
        (push (cons name id) out)))
    (sort out (lambda (a b) (string< (car a) (car b))))))

(defun nngmail--read-labels (account prompt &optional initial)
  "Read label names with completion, asking with PROMPT; return their ids.
INITIAL, a list of ids, is offered as the default set.
ACCOUNT is the nngmail account."
  (let* ((choices (nngmail--editable-labels account))
         (initial-names (delq nil (mapcar (lambda (id) (car (rassoc id choices))) initial)))
         (names (completing-read-multiple
                 prompt choices nil t
                 (and initial-names (concat (mapconcat #'identity initial-names ",") ","))
                 'nngmail-label-history)))
    (delq nil (mapcar (lambda (n) (cdr (assoc (string-trim n) choices))) names))))

(defun nngmail--apply-label-edit (account group nums add remove)
  "Do ADD/REMOVE on NUMS of GROUP, refresh the summary lines, announce the effects.
ACCOUNT is the nngmail account."
  (when (or add remove)
    (nngmail--modify account nums add remove)
    (nngmail--refresh-summary-lines account group nums)
    (nngmail--announce-label-edit account group nums add remove)))

(defun nngmail--refresh-summary-lines (account group nums)
  "Redraw the summary lines of server articles NUMS of GROUP.
Their label columns then update.
ACCOUNT is the nngmail account."
  (dolist (num nums)
    (when-let* ((local (nngmail--num->local account group num))
                (h (nngmail--header account num)))
      ;; The stored header's X-Gmail-Labels changed; give Gnus a copy in
      ;; local numbering and redraw the line.
      (let ((h2 (copy-sequence h)))
        (setf (mail-header-number h2) local)
        (setq gnus-newsgroup-headers
              (cons h2 (cl-remove local gnus-newsgroup-headers :key #'mail-header-number)))
        (save-excursion
          (when (gnus-summary-goto-subject local t)
            (gnus-summary-update-line)))))))

(defun nngmail--announce-label-edit (account group nums add remove)
  "Say what a label edit on NUMS of GROUP did, naming the groups it touched.
ACCOUNT is the nngmail account."
  (let ((name (lambda (id) (or (nngmail--label-group account id) id)))
        (n (length nums)))
    (message "nngmail: %s%s%s on %d message%s"
             (if add (format "added %s" (mapconcat name add ", ")) "")
             (if (and add remove) ", " "")
             (if remove (format "removed %s" (mapconcat name remove ", ")) "")
             n (if (= n 1) "" "s"))
    (ignore group)))

;;;###autoload
(defun nngmail-summary-edit-labels ()
  "Edit the full label set of the article at point (or the process-marked ones).
Offers every label with completion, preselecting the current ones; the
difference is sent to Gmail as one request per label change.  With
several articles the current set shown is their intersection."
  (interactive)
  (pcase-let* ((`(,account ,group ,nums) (nngmail--summary-context))
               (labels (nngmail-account-labels account))
               (current (cl-reduce (lambda (a b) (seq-intersection a b))
                                   (mapcar (lambda (n) (gethash n labels)) nums)))
               (editable (mapcar #'cdr (nngmail--editable-labels account)))
               (current (seq-filter (lambda (id) (member id editable)) current))
               (wanted (nngmail--read-labels account
                                             (format "Labels for %d message%s: " (length nums)
                                                     (if (= 1 (length nums)) "" "s"))
                                             current))
               (add (seq-difference wanted current))
               (remove (seq-difference current wanted)))
    (if (and (null add) (null remove))
        (message "nngmail: labels unchanged")
      (nngmail--apply-label-edit account group nums add remove))))

;;;###autoload
(defun nngmail-summary-add-label (label)
  "Add LABEL to the article at point or the process-marked articles."
  (interactive
   (pcase-let ((`(,account ,_ ,_) (nngmail--summary-context)))
     (list (car (nngmail--read-labels account "Add label: ")))))
  (pcase-let ((`(,account ,group ,nums) (nngmail--summary-context)))
    (unless label (user-error "nngmail: No label chosen"))
    (nngmail--apply-label-edit account group nums (list label) nil)))

;;;###autoload
(defun nngmail-summary-remove-label (label)
  "Remove LABEL from the article at point or the process-marked articles.
Offers the labels those articles actually carry."
  (interactive
   (pcase-let* ((`(,account ,_ ,nums) (nngmail--summary-context))
                (labels (nngmail-account-labels account))
                (present (seq-uniq (apply #'append (mapcar (lambda (n) (gethash n labels)) nums))))
                (choices (seq-filter (lambda (c) (member (cdr c) present))
                                     (nngmail--editable-labels account)))
                (name (completing-read "Remove label: " choices nil t)))
     (list (cdr (assoc name choices)))))
  (pcase-let ((`(,account ,group ,nums) (nngmail--summary-context)))
    (unless label (user-error "nngmail: No label chosen"))
    (nngmail--apply-label-edit account group nums nil (list label))))

;;;###autoload
(defun nngmail-summary-archive ()
  "Archive the article at point or the process-marked articles.
Gmail's archive: remove INBOX and nothing else.  The message keeps its
other labels and stays in All Mail."
  (interactive)
  (pcase-let* ((`(,account ,group ,nums) (nngmail--summary-context))
               (labels (nngmail-account-labels account))
               (inboxed (seq-filter (lambda (n) (member "INBOX" (gethash n labels))) nums)))
    (if (null inboxed)
        (message "nngmail: not in INBOX")
      (nngmail--apply-label-edit account group inboxed nil '("INBOX")))))

(defvar nngmail-summary-label-map
  (let ((map (make-sparse-keymap)))
    (define-key map "e" #'nngmail-summary-edit-labels)
    (define-key map "+" #'nngmail-summary-add-label)
    (define-key map "-" #'nngmail-summary-remove-label)
    (define-key map "a" #'nngmail-summary-archive)
    (define-key map "l" #'nngmail-summary-show-labels)
    (define-key map "?" #'nngmail-summary-explain-labels)
    map)
  "Keymap for nngmail label commands in summary buffers.")

(defun nngmail--install-summary-keys ()
  "Bind `nngmail-summary-label-map' under the prefix in nngmail summaries only."
  (when (and nngmail-summary-label-prefix-key
             (eq (car-safe (gnus-find-method-for-group gnus-newsgroup-name)) 'nngmail))
    (local-set-key nngmail-summary-label-prefix-key nngmail-summary-label-map)))

(add-hook 'gnus-summary-mode-hook #'nngmail--install-summary-keys)

;;;; Diagnostics

;;;###autoload
(defun nngmail-repair-labels (server)
  "Fix messages of SERVER whose labels were stored as IMAP names.
An early IMAP import kept Gmail's quoted system labels with their
backslash, so messages were filed under \"\\INBOX\" instead of INBOX
and did not show in the INBOX group.  This maps every such label to its
REST id, rewrites the rows, and refiles the groups."
  (interactive (list (nngmail--read-server)))
  (let* ((account (nngmail--account server))
         (labels (nngmail-account-labels account))
         (fixed 0))
    (nngmail--with-transaction account
      (maphash (lambda (num ls)
                 (when (seq-some (lambda (l) (string-prefix-p "\\" l)) ls)
                   (let ((new (seq-uniq (mapcar #'nngmail--imap-label-id ls))))
                     (puthash num new labels)
                     (nngmail--db-put-labels account num new)
                     (when-let* ((h (nngmail--header account num)))
                       (setf (alist-get 'X-Gmail-Labels (mail-header-extra h))
                             (mapconcat #'identity new ","))
                       (nngmail--db-put-message account num (gethash num (nngmail-account-num->id account)) new h))
                     (cl-incf fixed))))
               labels))
    (when (> fixed 0)
      (nngmail--rebuild-groups account)
      (setf (nngmail-account-dirty account) t)
      (nngmail--save account))
    (message "nngmail: repaired labels on %s message%s%s" (nngmail--fmt-num fixed)
             (if (= fixed 1) "" "s") (if (> fixed 0) "; press g in *Group*" ""))))

;;;###autoload
(defun nngmail-check-newsrc (&optional fix)
  "Report nngmail groups whose Gnus data uses an old numbering.
Before per-group numbering, Gnus stored read ranges and `unexist' marks
in server-wide article numbers, which run into the millions on a big
account.  A group whose stored ranges reach above its active high, or
that still carries `unexist' marks, is stale.  With FIX (prefix
argument, or when you answer yes) `gnus-group-clear-data' is run on
each stale group; read and starred state comes back from Gmail on the
next sync, since Gmail holds it."
  (interactive "P")
  (let (stale)
    (dolist (info (cdr gnus-newsrc-alist))
      (let ((group (gnus-info-group info)))
        ;; The info's own method: works before servers are opened and
        ;; for groups whose method is only recorded in .newsrc.eld.
        (when (or (eq (car-safe (gnus-info-method info)) 'nngmail)
                  (string-prefix-p "nngmail+" group))
          (let* ((active (gnus-active group))
                 (high (or (cdr active) 0))
                 (read (gnus-info-read info))
                 (top (let ((last (car (last read))))
                        (cond ((consp last) (cdr last)) (last last) (t 0))))
                 (unexist (length (cdr (assq 'unexist (gnus-info-marks info)))))
                 (tick (cdr (assq 'tick (gnus-info-marks info))))
                 (tick-top (let ((last (car (last tick))))
                             (cond ((consp last) (cdr last)) (last last) (t 0)))))
            (when (or (> top (max high 1)) (> unexist 0) (> tick-top (max high 1)))
              (push (list group high top unexist (length read)) stale))))))
    (if (null stale)
        (message "nngmail: every nngmail group's Gnus data matches its numbering")
      (with-current-buffer (get-buffer-create "*nngmail-newsrc*")
        (erase-buffer)
        (insert "nngmail groups whose .newsrc.eld data predates per-group numbering:\n\n")
        (insert (format "%-40s %10s %12s %9s %7s\n" "group" "active-hi" "read-top" "unexist" "ranges"))
        (dolist (row (reverse stale))
          (insert (format "%-40s %10d %12d %9d %7d\n" (nth 0 row) (nth 1 row) (nth 2 row) (nth 3 row) (nth 4 row))))
        (insert "\nread-top above active-hi, or any unexist marks, means the old numbering.\n")
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      (when (or fix
                (y-or-n-p (format "nngmail: %d group%s with stale data.  Clear their Gnus data (marks come back from Gmail)? "
                                  (length stale) (if (= 1 (length stale)) "" "s"))))
        (dolist (row stale)
          (gnus-group-clear-data (car row)))
        (message "nngmail: cleared %d group%s; press g in *Group*" (length stale)
                 (if (= 1 (length stale)) "" "s"))))))


(defun nngmail--current-article-account ()
  "Return (ACCOUNT GROUP ARTICLE) for the article at point in a summary buffer."
  (unless (derived-mode-p 'gnus-summary-mode)
    (error "nngmail: Not in a Gnus summary buffer"))
  (let* ((method (gnus-find-method-for-group gnus-newsgroup-name)))
    (unless (eq (car method) 'nngmail)
      (error "nngmail: %s is not an nngmail group" gnus-newsgroup-name))
    (let* ((account (nngmail--account (cadr method)))
           (group (gnus-group-real-name gnus-newsgroup-name))
           (local (gnus-summary-article-number)))
      ;; Summary numbers are the group's local numbers; diagnostics and
      ;; label lookups want the server number.
      (list account group (or (nngmail--local->num account group local) local)))))

;;;###autoload
(defun nngmail-show-labels (&optional refresh)
  "Show the Gmail labels of the article at point, local and (with REFRESH) live.
Prints the article number, Gmail id, the labels nngmail has stored, and
with a prefix argument also asks Gmail for the current labels so the two
can be compared.  If they differ, the local copy is corrected."
  (interactive "P")
  (pcase-let* ((`(,account ,group ,num) (nngmail--current-article-account))
               (id (gethash num (nngmail-account-num->id account)))
               (local (gethash num (nngmail-account-labels account)))
               (names (lambda (ls)
                        (mapconcat (lambda (l)
                                     (or (cdr (assoc l (nngmail-account-label-names account))) l))
                                   ls ", "))))
    (unless id (error "nngmail: Article %d unknown in %s" num (nngmail-account-name account)))
    (if (not refresh)
        (message "#%d %s [%s]: %s" num id group (funcall names local))
      (let* ((msg (nngmail--get account (concat "messages/" id) '(("format" . "minimal"))))
             (live (alist-get 'labelIds msg)))
        (cond
         ((null msg)
          (message "#%d %s: local %s; Gmail: message no longer exists" num id (funcall names local)))
         ((and (null (seq-difference local live)) (null (seq-difference live local)))
          (message "#%d %s: %s (Gmail agrees)" num id (funcall names local)))
         (t
          (nngmail--set-labels account num live)
          (nngmail--save account)
          (message "#%d %s: local %s / Gmail %s -- corrected locally"
                   num id (funcall names local) (funcall names live))))))))

;;;###autoload
(defun nngmail-debug-group (&optional live-count)
  "Dump what nngmail believes about the current group, for bug reports.
Writes to the *nngmail-debug* buffer: the group's active range and the
info Gnus holds (read ranges, marks), a label histogram over the local
index, the newest LIVE-COUNT (default 20) messages Gmail lists for this
label side by side with the local labels, and the last few history ids.
No subjects, senders or bodies are included."
  (interactive "P")
  (pcase-let* ((`(,account ,group ,_) (nngmail--current-article-account))
               (label (nngmail--group-label account group))
               (full (gnus-group-full-name group (list 'nngmail (nngmail-account-name account))))
               (info (gnus-get-info full))
               (nums (nngmail--group-numbers account group))
               (live-count (or live-count 20))
               (buf (get-buffer-create "*nngmail-debug*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "nngmail debug for %s (label %s)\n" full label))
      (insert (format "history-id: %s   pending: %s   next-article: %d   indexed: %d\n\n"
                      (nngmail-account-history-id account)
                      (and (nngmail-account-pending account)
                           (length (nth 2 (nngmail-account-pending account))))
                      (nngmail-account-next-article account)
                      (hash-table-count (nngmail-account-num->id account))))
      (insert (format "active: %S\n" (gnus-active full)))
      (insert (format "info read: %S\n" (gnus-info-read info)))
      (insert (format "info marks: %S\n" (gnus-info-marks info)))
      (insert (format "local members: %d, newest %S, oldest %S\n"
                      (length nums) (car nums) (car (last nums))))
      (let ((unread 0) (starred 0))
        (dolist (n nums)
          (let ((l (gethash n (nngmail-account-labels account))))
            (when (member "UNREAD" l) (cl-incf unread))
            (when (member "STARRED" l) (cl-incf starred))))
        (insert (format "local UNREAD in group: %d   STARRED: %d\n\n" unread starred)))
      ;; Label histogram across the whole index.
      (let ((hist (make-hash-table :test #'equal)))
        (maphash (lambda (_n ls) (dolist (l ls) (cl-incf (gethash l hist 0))))
                 (nngmail-account-labels account))
        (insert "label histogram (whole index):\n")
        (let (rows)
          (maphash (lambda (l c) (push (cons l c) rows)) hist)
          (dolist (r (sort rows (lambda (a b) (> (cdr a) (cdr b)))))
            (insert (format "  %6d  %s  (%s)\n" (cdr r) (car r)
                            (or (cdr (assoc (car r) (nngmail-account-label-names account))) "?"))))))
      ;; Live comparison.
      (when label
        (insert (format "\nnewest %d at Gmail with label %s vs local:\n" live-count label))
        (dolist (id (nngmail--list-ids account nil (list label) live-count))
          (let* ((num (gethash id (nngmail-account-id->num account)))
                 (local (and num (gethash num (nngmail-account-labels account))))
                 (msg (nngmail--get account (concat "messages/" id) '(("format" . "minimal"))))
                 (live (alist-get 'labelIds msg)))
            (insert (format "  %s  #%s  local=%S  live=%S  in-group=%s\n"
                            id (or num "-") local live
                            (if (and num (memq num nums)) "yes" "NO"))))))
      ;; And a few local members that Gmail may not list.
      (insert (format "\nnewest %d local members vs Gmail:\n" (min 10 (length nums))))
      (dolist (num (seq-take nums 10))
        (let* ((id (gethash num (nngmail-account-num->id account)))
               (local (gethash num (nngmail-account-labels account)))
               (msg (and id (nngmail--get account (concat "messages/" id) '(("format" . "minimal")))))
               (live (alist-get 'labelIds msg)))
          (insert (format "  #%d %s  local=%S  live=%S  %s\n" num id local live
                          (if (equal (sort (copy-sequence local) #'string<)
                                     (sort (copy-sequence live) #'string<))
                              "" "MISMATCH")))))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(defun nngmail-verify-group (&optional limit)
  "Compare the current group's membership with Gmail and report drift.
Asks Gmail for the newest LIMIT (default 200) messages carrying this
group's label and checks each against the local index: messages Gmail
has that we file elsewhere, and messages we file here that Gmail says
have lost the label.  Corrects the local labels of anything it fetched.
Useful for confirming whether a wrong-group problem is in the local
state or in what Gmail actually reports."
  (interactive "P")
  (pcase-let* ((`(,account ,group ,_) (nngmail--current-article-account))
               (label (nngmail--group-label account group))
               (limit (or limit 200)))
    (unless label (error "nngmail: %s has no Gmail label (virtual group)" group))
    (let* ((live-ids (nngmail--list-ids account nil (list label) limit))
           (live-set (make-hash-table :test #'equal))
           (local-nums (nngmail--group-numbers account group))
           missing-here extra-here checked)
      (dolist (id live-ids) (puthash id t live-set))
      ;; Refresh labels for everything Gmail lists (bounded by LIMIT).
      (nngmail--fetch-in-chunks account live-ids "verifying" t)
      (dolist (id live-ids)
        (let ((num (gethash id (nngmail-account-id->num account))))
          (unless (nngmail--group-member-p account group num)
            (push (cons num id) missing-here))))
      ;; Anything local in the group that Gmail didn't list among its newest
      ;; LIMIT: refresh those too, but only up to LIMIT of them.
      (dolist (num (seq-take local-nums limit))
        (let ((id (gethash num (nngmail-account-num->id account))))
          (unless (gethash id live-set)
            (push id checked))))
      (when checked
        (nngmail--fetch-in-chunks account checked "verifying (local extras)" t)
        (dolist (id checked)
          (let ((num (gethash id (nngmail-account-id->num account))))
            (unless (member label (gethash num (nngmail-account-labels account)))
              (push (cons num id) extra-here)))))
      (nngmail--save account)
      (message "nngmail %s: checked %d live + %d local; %d were wrongly absent, %d wrongly present; local index corrected"
               group (length live-ids) (length checked)
               (length missing-here) (length extra-here))
      (list :missing missing-here :extra extra-here))))

;;;; Housekeeping

(defun nngmail--save-all ()
  "Write every open account's meta (for `kill-emacs-hook' and Gnus exit).
Rows are already committed; this is the history id and checkpoints."
  (maphash (lambda (_k a) (ignore-errors (nngmail--save a t))) nngmail--accounts))

(add-hook 'kill-emacs-hook #'nngmail--save-all)
(add-hook 'gnus-exit-gnus-hook #'nngmail--save-all)

;;;###autoload
(defun nngmail-reset (server)
  "Forget all local state for nngmail SERVER; next open does a full sync."
  (interactive
   (list (nngmail--read-server)))
  (when-let* ((account (gethash server nngmail--accounts)))
    (when-let* ((db (nngmail-account-db account)))
      (when (sqlitep db) (ignore-errors (sqlite-close db))))
    (remhash server nngmail--accounts))
  (dolist (f (list (expand-file-name (concat server ".sqlite") nngmail-directory)
                   (expand-file-name (concat server ".sqlite-wal") nngmail-directory)
                   (expand-file-name (concat server ".sqlite-shm") nngmail-directory)
                   (expand-file-name (concat server ".eld") nngmail-directory)))
    (when (file-exists-p f) (delete-file f)))
  (let ((dir (expand-file-name server nngmail-directory)))
    (when (file-directory-p dir) (delete-directory dir t)))
  (message "nngmail: state for %s removed" server))

;; `server-marks' is what makes Gnus push read/tick changes to the backend
;; (via `request-set-mark') instead of keeping them only in .newsrc.eld.
;; Without it nothing ever reaches Gmail and the next sync overwrites the
;; local marks with Gmail's, which looks like "catch-up does not stick".
(gnus-declare-backend "nngmail" 'mail 'address 'respool 'server-marks)

;; If this file is being reloaded into a running session, bring existing
;; account objects up to the current struct layout.
(when (and (boundp 'nngmail--accounts) (hash-table-p nngmail--accounts))
  (nngmail--upgrade-all-accounts))

;; Companion commands that live in their own files.  Autoloaded here so
;; they exist as soon as nngmail does, without a package autoload
;; cookie step for people who `load' this file by hand.
(autoload 'nngmail-reload "nngmail-reload"
  "Reload every loaded nngmail library from its source." t)
(autoload 'nngmail-efrit-ensure-tools "nngmail-efrit"
  "Register nngmail's Gmail tools with efrit." t)
(autoload 'nngmail-efrit-reload "nngmail-efrit"
  "Reload efrit and nngmail from source and re-register the tools." t)
(autoload 'nngmail-gdocs-show "nngmail-gdocs"
  "Show the Google Docs linked from the article at point." t)
(autoload 'nngmail-gdocs-fetch "nngmail-gdocs"
  "The title and text of a Google Doc by URL or id.")
(autoload 'nngmail-testdrive "nngmail-testdrive"
  "Walk through the nngmail live test plan interactively." t)
(autoload 'nngmail-background-status "nngmail-background"
  "Say what each account's background job is doing." t)
(autoload 'nngmail-background-stop "nngmail-background"
  "Stop a server's background sync job." t)
(autoload 'nngmail-background-start "nngmail-background"
  "Start or resume a server's background sync job." t)

(provide 'nngmail)

;; Background synchronisation: `g' reads the local index and a timer-
;; driven job brings it up to date (`nngmail-background').  Loaded after
;; `provide' since it requires nngmail; it installs the advice that
;; routes syncs to the job.
(require 'nngmail-background)

;; Google Docs linked from mail (`L D', the gdoc_fetch tool, efrit-gnus
;; link expansion) are part of nngmail; loaded after `provide' since
;; the file requires nngmail.
(require 'nngmail-gdocs)

;; The menu (menu-bar entry and the `L L' transient) is part of nngmail
;; and comes along with it.  Loaded after `provide': nngmail-menu
;; requires nngmail, so a require above would be a cycle.  It needs
;; transient; without one (Emacs 29 with no ELPA transient) nngmail
;; still works, without the menu, and says so once.
(if (locate-library "transient")
    (require 'nngmail-menu)
  (message "nngmail: transient not found; the nngmail menu (L L) is not available"))

;;; nngmail.el ends here
