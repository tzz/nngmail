;;; nngmail-background-tests.el --- Tests for the background job  -*- lexical-binding: t; -*-

;;; Commentary:

;; The job is driven by hand: `run-at-time' is replaced by a queue the
;; test pops, and `nngmail--job-send' by a fake transport that records
;; the request and delivers a canned response when the test says so.
;; No real timers, no network, no waiting.

;;; Code:

(require 'ert)
(require 'nngmail-tests)
(require 'nngmail-background)

(defvar nngmail-bgtest--timers nil "Scheduled (DELAY . FUNCTION) pairs, oldest first.")
(defvar nngmail-bgtest--sent nil "Requests sent and not yet answered: (METHOD URL BODY CALLBACK), oldest first.")
(defvar nngmail-bgtest--answers nil "Alist of (URL-REGEXP . (STATUS . BODY-OR-FUNCTION)).")

(defun nngmail-bgtest--run-at-time (delay _repeat fn &rest args)
  "Queue FN with ARGS; return a token the job can cancel."
  (let ((entry (list delay (lambda () (apply fn args)))))
    (setq nngmail-bgtest--timers (append nngmail-bgtest--timers (list entry)))
    entry))

(defun nngmail-bgtest--cancel (entry)
  "Remove ENTRY from the timer queue."
  (setq nngmail-bgtest--timers (delq entry nngmail-bgtest--timers)))

(defun nngmail-bgtest--send (job method url body _content-type callback)
  "Fake `nngmail--job-send': record the request, hold the callback."
  (setq nngmail--batch-in-flight 'nngmail-background)
  (setf (nngmail-job-proc job) 'fake)
  (setq nngmail-bgtest--sent
        (append nngmail-bgtest--sent
                (list (list method url body
                            (lambda (status headers text)
                              (setf (nngmail-job-proc job) nil)
                              (setq nngmail--batch-in-flight nil)
                              (nngmail--quota-charge method url body)
                              (funcall callback status headers text)))))))

(defun nngmail-bgtest--deliver ()
  "Answer the oldest unanswered request from `nngmail-bgtest--answers'."
  (let* ((req (pop nngmail-bgtest--sent))
         (url (nth 1 req))
         (answer (cl-loop for (re . a) in nngmail-bgtest--answers
                          when (string-match-p re url) return a
                          finally (error "nngmail-bgtest: no answer for %s" url))))
    (let ((body (cdr answer)))
      (funcall (nth 3 req) (car answer) nil
               (if (functionp body) (funcall body req) body)))))

(defun nngmail-bgtest--tick ()
  "Run the oldest scheduled step.  Return its delay, or nil when none."
  (when-let* ((entry (pop nngmail-bgtest--timers)))
    (funcall (cadr entry))
    (car entry)))

(defun nngmail-bgtest--drain (&optional max)
  "Alternate ticks and deliveries until the job is idle or MAX rounds pass.
Return the number of requests answered."
  (let ((rounds 0) (answered 0))
    (while (and (< rounds (or max 200))
                (or nngmail-bgtest--timers nngmail-bgtest--sent))
      (cl-incf rounds)
      (cond (nngmail-bgtest--sent (nngmail-bgtest--deliver) (cl-incf answered))
            (t (let ((delay (nngmail-bgtest--tick)))
                 ;; An idle-length delay means the job went to sleep.
                 (when (and delay (>= delay nngmail-background-interval))
                   (setq rounds (or max 200)))))))
    answered))

(defmacro nngmail-bgtest-with (answers &rest body)
  "Run BODY with a mocked account, timers and transport; ANSWERS feeds the transport."
  (declare (indent 1))
  `(nngmail-test-with-mock
       `(("GET labels" . ,nngmail-test--labels))
     (let ((nngmail-bgtest--timers nil)
           (nngmail-bgtest--sent nil)
           (nngmail-bgtest--answers ,answers)
           (nngmail--jobs (make-hash-table :test #'equal))
           (nngmail--mode-line-flash-timer nil)
           (nngmail--mode-line-flash nil)
           (nngmail--progress-quiet nil)
           (nngmail--batch-in-flight nil)
           (nngmail--batch-size-current nil)
           (nngmail-background t)
           (nngmail--background-in-batch t)
           (nngmail-background-interval 120)
           (nngmail-use-plz t))
       (cl-letf (((symbol-function 'run-at-time) #'nngmail-bgtest--run-at-time)
                 ((symbol-function 'cancel-timer) #'nngmail-bgtest--cancel)
                 ((symbol-function 'nngmail--job-send) #'nngmail-bgtest--send)
                 ;; No timers for the spinner in tests; the string is still computed.
                 ((symbol-function 'nngmail--spinner-start) #'ignore)
                 ((symbol-function 'force-mode-line-update) #'ignore)
                 ((symbol-function 'redisplay) #'ignore))
         ,@body))))

(defun nngmail-bgtest--json (obj)
  "OBJ (alists, with lists for arrays, as `nngmail-test--msg' builds) as JSON text."
  (require 'json)
  (let ((json-encoding-pretty-print nil))
    (json-encode obj)))

(defun nngmail-bgtest--page (ids-with-labels &optional next last-id)
  "A history.list page adding IDS-WITH-LABELS ((ID . LABELS)...), NEXT token, LAST-ID."
  (nngmail-bgtest--json
   `((historyId . "999")
     ,@(when next `((nextPageToken . ,next)))
     (history . (((id . ,(or last-id "100"))
                  (messagesAdded . ,(mapcar (lambda (il)
                                              `((message . ((id . ,(car il))
                                                            ,@(when (cdr il) `((labelIds . ,(cdr il))))))))
                                            ids-with-labels))))))))

(defun nngmail-bgtest--batch-response (req msgs)
  "A multipart batch response to REQ serving MSGS (alist id -> metadata alist)."
  (let* ((body (nth 2 req))
         (boundary (and (string-match "--\\(nngmail_[^\r\n]+\\)" body) (match-string 1 body)))
         (ids (let (out (start 0))
                (while (string-match "Content-ID: <\\([^>]+\\)>" body start)
                  (push (match-string 1 body) out)
                  (setq start (match-end 0)))
                (nreverse out))))
    (concat
     (mapconcat (lambda (id)
                  (let ((msg (cdr (assoc id msgs))))
                    (concat "--" boundary "\r\n"
                            "Content-Type: application/http\r\n"
                            "Content-ID: <response-" id ">\r\n\r\n"
                            (if msg
                                (concat "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                                        (nngmail-bgtest--json msg))
                              "HTTP/1.1 404 Not Found\r\nContent-Type: application/json\r\n\r\n{\"error\":{\"message\":\"gone\"}}")
                            "\r\n")))
                ids "")
     "--" boundary "--\r\n")))

;;;; Tests

(ert-deftest nngmail-bgtest-g-does-not-block ()
  "With background on, `nngmail--sync-safely' returns at once and schedules a step."
  (nngmail-bgtest-with nil
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-history-id a) "50")
      (should (nngmail--sync-safely a))
      (should-not nngmail-bgtest--sent)
      (should (= 1 (length nngmail-bgtest--timers)))
      (should (= 0 (car (car nngmail-bgtest--timers)))))))

(ert-deftest nngmail-bgtest-first-sync-is-inline ()
  "No history id: the first sync runs synchronously so Gnus has something to show."
  (let ((ran nil))
    (nngmail-bgtest-with nil
      (let ((a (nngmail-test--account)))
        (cl-letf (((symbol-function 'nngmail--incremental-sync)
                   (lambda (&rest _) (setq ran t))))
          (should (nngmail--sync-safely a))
          (should ran)
          (should-not nngmail-bgtest--timers))))))

(ert-deftest nngmail-bgtest-history-pages-applied-in-steps ()
  "Three history pages arrive one request at a time; each page is applied and
checkpointed before the next is requested; lazy arrivals need no fetch."
  (nngmail-bgtest-with
      `(("pageToken=p3" . (200 . ,(nngmail-bgtest--page '(("m3" "INBOX")) nil "301")))
        ("pageToken=p2" . (200 . ,(nngmail-bgtest--page '(("m2" "INBOX")) "p3" "201")))
        ("history" . (200 . ,(nngmail-bgtest--page '(("m1" "INBOX")) "p2" "101"))))
    (let ((a (nngmail-test--account))
          (nngmail-sync-headers 'lazy))
      (setf (nngmail-account-history-id a) "50")
      (nngmail--sync-safely a)
      ;; Step 1: request page 1.  Nothing applied yet.
      (nngmail-bgtest--tick)
      (should (= 1 (length nngmail-bgtest--sent)))
      (should (string-search "startHistoryId=50" (nth 1 (car nngmail-bgtest--sent))))
      (nngmail-bgtest--deliver)
      ;; Page 1 applied and checkpointed; next step queued, not yet sent.
      (should (equal "101" (nngmail-account-history-id a)))
      (should (gethash "m1" (nngmail-account-id->num a)))
      (should-not nngmail-bgtest--sent)
      (should (= 1 (length nngmail-bgtest--timers)))
      ;; Page 2 is requested from the checkpoint, not from 50.
      (nngmail-bgtest--tick)
      (should (string-search "startHistoryId=101" (nth 1 (car nngmail-bgtest--sent))))
      (should (string-search "pageToken=p2" (nth 1 (car nngmail-bgtest--sent))))
      (nngmail-bgtest--deliver)
      (should (equal "201" (nngmail-account-history-id a)))
      (nngmail-bgtest--tick)
      (nngmail-bgtest--deliver)
      ;; Last page: the mailbox's current id is adopted, job idle, no fetch.
      (should (equal "999" (nngmail-account-history-id a)))
      (should (= 3 (hash-table-count (nngmail-account-id->num a))))
      (should (eq 'idle (nngmail-job-state (nngmail-background-job a))))
      (should-not (nngmail-account-pending a))
      ;; The next poll is queued (a shorter mode-line flash timer may sit beside it).
      (should (seq-find (lambda (e) (>= (car e) nngmail-background-interval)) nngmail-bgtest--timers)))))

(ert-deftest nngmail-bgtest-fetch-batches-async ()
  "Arrivals without labels are fetched in background batches; a part that
bounces is retried in a later batch; the pending list tracks it on disk."
  (let* ((msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id))))
                       '("n1" "n2" "n3")))
         (batches 0))
    (nngmail-bgtest-with
        `(("batch" . (200 . ,(lambda (req)
                               (cl-incf batches)
                               (if (= batches 1)
                                   ;; First batch: n2 bounces with a rate limit.
                                   (let ((text (nngmail-bgtest--batch-response req msgs)))
                                     (replace-regexp-in-string
                                      "Content-ID: <response-n2>\r\n\r\nHTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n[^\r]*"
                                      "Content-ID: <response-n2>\r\n\r\nHTTP/1.1 429 Too Many\r\nContent-Type: application/json\r\n\r\n{\"error\":{\"message\":\"Rate Limit Exceeded\",\"errors\":[{\"reason\":\"rateLimitExceeded\"}]}}"
                                      text t t))
                                 (nngmail-bgtest--batch-response req msgs)))))
          ("history" . (200 . ,(nngmail-bgtest--page '(("n1") ("n2") ("n3")) nil "101"))))
      (let ((a (nngmail-test--account))
            (nngmail-batch-size 50))
        (setf (nngmail-account-history-id a) "50")
        (nngmail--sync-safely a)
        (nngmail-bgtest--tick) (nngmail-bgtest--deliver)   ; history page
        (let ((job (nngmail-background-job a)))
          (should (eq 'fetch (nngmail-job-state job)))
          (should (equal '("n1" "n2" "n3") (sort (copy-sequence (nngmail-job-to-fetch job)) #'string<)))
          (should (equal (nngmail-job-to-fetch job) (nth 2 (nngmail-account-pending a))))
          (nngmail-bgtest--tick)                               ; batch 1 sent
          (should (string-prefix-p "POST" (nth 0 (car nngmail-bgtest--sent))))
          (nngmail-bgtest--deliver)
          ;; n1 and n3 stored; n2 back in the queue; the retry is delayed.
          (should (= 2 (nngmail-test--header-count a)))
          (should (equal '("n2") (nngmail-job-to-fetch job)))
          (should (equal '("n2") (nth 2 (nngmail-account-pending a))))
          (should (> (car (car nngmail-bgtest--timers)) 0))
          (nngmail-bgtest--tick) (nngmail-bgtest--deliver)   ; batch 2: n2 alone
          (should (= 3 (nngmail-test--header-count a)))
          (nngmail-bgtest--tick)                               ; fetch-done -> idle
          (should (eq 'idle (nngmail-job-state job)))
          (should-not (nngmail-account-pending a))
          (should (= 2 batches)))))))

(ert-deftest nngmail-bgtest-interrupted-fetch-resumes-next-pass ()
  "A pending fetch list saved by one pass is carried into the next."
  (let ((msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id))))
                      '("q1" "q2"))))
    (nngmail-bgtest-with
        `(("batch" . (200 . ,(lambda (req) (nngmail-bgtest--batch-response req msgs))))
          ("history" . (200 . ,(nngmail-bgtest--page nil nil "101"))))
      (let ((a (nngmail-test--account)))
        (setf (nngmail-account-history-id a) "50"
              (nngmail-account-pending a) '("fetching changed messages" t ("q1" "q2")))
        (nngmail--sync-safely a)
        (should (= 2 (nngmail-bgtest--drain)))
        (should (= 2 (nngmail-test--header-count a)))
        (should-not (nngmail-account-pending a))))))

(ert-deftest nngmail-bgtest-errors-back-off-and-recover ()
  "A transport failure backs the job off with growing delays; a later
success resets the counter."
  (nngmail-bgtest-with
      '(("history" . (0 . "curl: timeout")))
    (progn
      (let ((a (nngmail-test--account)))
        (setf (nngmail-account-history-id a) "50")
        (nngmail--sync-safely a)
        (nngmail-bgtest--tick) (nngmail-bgtest--deliver)
        (let ((job (nngmail-background-job a)))
          (should (nngmail-job-backing-off job))
          (should (eq 'history (nngmail-job-state job)))
          (should (= 1 (nngmail-job-errors job)))
          (let ((d1 (car (car nngmail-bgtest--timers))))
            (nngmail-bgtest--tick) (nngmail-bgtest--deliver)
            (should (= 2 (nngmail-job-errors job)))
            (should (> (car (car nngmail-bgtest--timers)) d1)))
          ;; Now a good page: delivered as status 200 by the fake below.
          (setq nngmail-bgtest--answers
                `(("history" . (200 . ,(nngmail-bgtest--page nil nil "101")))))
          (nngmail-bgtest--tick) (nngmail-bgtest--deliver)
          (should (eq 'idle (nngmail-job-state job)))
          (should (= 0 (nngmail-job-errors job))))))))

(ert-deftest nngmail-bgtest-stop-and-start ()
  "Stopping cancels the timer and keeps state; starting resumes."
  (nngmail-bgtest-with
      `(("history" . (200 . ,(nngmail-bgtest--page nil nil "101"))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-history-id a) "50")
      (nngmail--sync-safely a)
      (cl-letf (((symbol-function 'message) #'ignore))
        (nngmail-background-stop "test"))
      (should-not nngmail-bgtest--timers)
      (should (nngmail-job-stopped (nngmail-background-job a)))
      ;; A plain g does not restart a stopped job...
      (nngmail--sync-safely a)
      (should-not nngmail-bgtest--timers)
      ;; ...an explicit start does.
      (cl-letf (((symbol-function 'message) #'ignore))
        (nngmail-background-start "test"))
      (should nngmail-bgtest--timers)
      (nngmail-bgtest--drain)
      (should (equal "999" (nngmail-account-history-id a))))))

(ert-deftest nngmail-bgtest-waits-for-synchronous-caller ()
  "While a synchronous batch holds the slot, the job waits instead of sending."
  (nngmail-bgtest-with
      `(("history" . (200 . ,(nngmail-bgtest--page nil nil "101"))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-history-id a) "50")
      (nngmail--sync-safely a)
      (setq nngmail--batch-in-flight "marks")
      (nngmail-bgtest--tick)
      (should-not nngmail-bgtest--sent)
      (should (= 2 (car (car nngmail-bgtest--timers))))
      (setq nngmail--batch-in-flight nil)
      (nngmail-bgtest--tick)
      (should (= 1 (length nngmail-bgtest--sent))))))

(ert-deftest nngmail-bgtest-pacing-is-a-delay-not-a-prompt ()
  "With the bucket empty, the job sleeps for the refill time; no prompt is shown."
  (nngmail-bgtest-with
      `(("history" . (200 . ,(nngmail-bgtest--page nil nil "101"))))
    (let ((a (nngmail-test--account))
          (nngmail-quota-pacing 'always)
          (nngmail-quota-policy 'ask)
          (nngmail-quota-per-minute 6000)
          (nngmail-quota-burst 250)
          (nngmail--quota-log nil)
          (asked nil))
      (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) (setq asked t) ?a))
                ((symbol-function 'nngmail--quota-ask) (lambda (&rest _) (setq asked t) 'abort)))
        (setf (nngmail-account-history-id a) "50")
        ;; Spend the whole bucket a moment ago.
        (push (cons (float-time) 250) nngmail--quota-log)
        (nngmail--sync-safely a)
        (nngmail-bgtest--tick)
        (should-not nngmail-bgtest--sent)
        (should-not asked)
        (let ((delay (car (car nngmail-bgtest--timers))))
          (should (> delay 0))
          (should (<= delay 60)))
        ;; Pretend the time passed: clear the spend, the request goes out.
        (setq nngmail--quota-log nil)
        (nngmail-bgtest--tick)
        (should (= 1 (length nngmail-bgtest--sent)))))))

(ert-deftest nngmail-bgtest-pass-refreshes-touched-group-lines ()
  "Groups whose membership a pass changed are redrawn in *Group* when idle."
  (let ((updated nil))
    (nngmail-bgtest-with
        `(("history" . (200 . ,(nngmail-bgtest--page '(("t1" "INBOX" "UNREAD")) nil "101"))))
      (let ((a (nngmail-test--account))
            (nngmail-sync-headers 'lazy))
        (setf (nngmail-account-history-id a) "50")
        (nngmail--relabel-groups a (nngmail--fetch-labels a))
        (cl-letf (((symbol-function 'gnus-alive-p) (lambda () t))
                  ((symbol-function 'get-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'gnus-get-info) (lambda (full) (list full)))
                  ((symbol-function 'gnus-group-full-name) (lambda (g m) (format "%s+%s:%s" (car m) (cadr m) g)))
                  ((symbol-function 'nngmail--summary-open-p) #'ignore)
                  ((symbol-function 'nngmail--update-info) #'ignore)
                  ((symbol-function 'gnus-group-update-group) (lambda (full &rest _) (push full updated))))
          (nngmail--sync-safely a)
          (nngmail-bgtest--drain)
          (should (equal '("nngmail+test:INBOX" "nngmail+test:[Gmail]/All Mail")
                         (sort updated #'string<)))
          (should-not (nngmail-job-touched (nngmail-background-job a))))))))

(ert-deftest nngmail-bgtest-mode-line-status-keeps-echo-area-clear ()
  "In `mode-line' mode the job writes nothing to the echo area: progress
goes beside the G, and the done message is a mode-line flash."
  (let ((messages nil))
    (nngmail-bgtest-with
        `(("history" . (200 . ,(nngmail-bgtest--page '(("m1" "INBOX")) nil "101"))))
      (let ((a (nngmail-test--account))
            (nngmail-background-status 'mode-line)
            (nngmail-sync-headers 'lazy)
            (nngmail--spinner-string nil)
            (global-mode-string nil))
        (setf (nngmail-account-history-id a) "50")
        (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (when fmt (push (apply #'format fmt args) messages))))
                  ((symbol-function 'nnheader-message) (lambda (_l fmt &rest args) (push (apply #'format fmt args) messages)))
                  ((symbol-function 'force-mode-line-update) #'ignore))
          (nngmail--sync-safely a)
          (nngmail-bgtest--tick)
          (should nngmail--progress-quiet)
          ;; While the request is out the tick would draw: it must not message.
          (nngmail--spinner-tick-1)
          (should (string-search "…" nngmail--spinner-string))
          (should-not (string-search "checking" nngmail--spinner-string))
          (nngmail-bgtest--deliver)
          (should-not (seq-find (lambda (m) (string-match-p "sync done\\|applying\\|checking" m)) messages))
          ;; Idle again: the G (text fallback in batch) with the pills; the
          ;; indexed total is one of them, so no separate done flash.
          (should (string-prefix-p " G" nngmail--spinner-string))
          (should (string-search "[1]" (substring-no-properties nngmail--spinner-string)))
          (should-not nngmail--mode-line-flash)
          (should-not nngmail--progress-quiet))))))

(ert-deftest nngmail-bgtest-echo-area-status-still-available ()
  "`echo-area' mode behaves as before: the done message is a message."
  (let ((messages nil))
    (nngmail-bgtest-with
        `(("history" . (200 . ,(nngmail-bgtest--page nil nil "101"))))
      (let ((a (nngmail-test--account))
            (nngmail-background-status 'echo-area))
        (setf (nngmail-account-history-id a) "50")
        (cl-letf (((symbol-function 'nnheader-message) (lambda (_l fmt &rest args) (push (apply #'format fmt args) messages))))
          (nngmail--sync-safely a)
          (nngmail-bgtest--drain)
          (should (seq-find (lambda (m) (string-search "sync done" m)) messages)))))))

(ert-deftest nngmail-bgtest-idle-indicator-carries-count-pills ()
  "The idle G is followed by unread (INBOX) and starred pills with the
counts from the index; zero counts show no pill."
  (nngmail-bgtest-with
      `(("history" . (200 . ,(nngmail-bgtest--page '(("u1" "INBOX" "UNREAD") ("u2" "INBOX" "UNREAD")
                                                      ("s1" "INBOX" "STARRED") ("r1" "INBOX"))
                                                    nil "101"))))
    (let ((a (nngmail-test--account))
          (nngmail-sync-headers 'lazy)
          (nngmail-mode-line-counts '((unread . "INBOX") (starred . t) (indexed . t))))
      (setf (nngmail-account-history-id a) "50")
      (nngmail--relabel-groups a (nngmail--fetch-labels a))
      (nngmail--sync-safely a)
      (nngmail-bgtest--drain)
      ;; Let the flash expire so the idle indicator is what shows.
      (setq nngmail--mode-line-flash nil)
      (nngmail--spinner-refresh-idle)
      (let ((text (substring-no-properties nngmail--spinner-string)))
        (should (string-search "2 unread" text))
        (should (string-search "1 ★" text))
        (should (string-search "[4]" text)))
      ;; Reading both: the unread pill goes away, starred stays.
      (nngmail--set-labels a (gethash "u1" (nngmail-account-id->num a)) '("INBOX"))
      (nngmail--set-labels a (gethash "u2" (nngmail-account-id->num a)) '("INBOX"))
      (nngmail--spinner-refresh-idle)
      (let ((text (substring-no-properties nngmail--spinner-string)))
        (should-not (string-search "unread" text))
        (should (string-search "1 ★" text)))
      ;; No counts wanted: the G alone.
      (let ((nngmail-mode-line-counts nil))
        (nngmail--spinner-refresh-idle)
        (should (equal " G " (substring-no-properties nngmail--spinner-string))))
      ;; While a pass runs the pills stay beside the spinner.
      (setq nngmail--progress (list "test" "applying changes" 7 nil nil)
            nngmail--progress-quiet t)
      (cl-letf (((symbol-function 'nngmail--draw-status) #'ignore))
        (nngmail--spinner-tick-1))
      (let ((text (substring-no-properties nngmail--spinner-string)))
        (should (string-search "↻ 7" text))
        (should (string-search "1 ★" text))
        (should (string-search "[4]" text)))
      (setq nngmail--progress nil nngmail--progress-quiet nil))))

(ert-deftest nngmail-bgtest-expired-history-does-full-sync ()
  "A 404 from history.list hands over to the synchronous full sync."
  (let ((full nil))
    (nngmail-bgtest-with
        `(("history" . (404 . "{\"error\":{\"message\":\"Start history ID is too old\"}}")))
      (let ((a (nngmail-test--account)))
        (setf (nngmail-account-history-id a) "50")
        (cl-letf (((symbol-function 'nngmail--full-sync)
                   (lambda (acct) (setq full t) (setf (nngmail-account-history-id acct) "777"))))
          (nngmail--sync-safely a)
          (nngmail-bgtest--tick) (nngmail-bgtest--deliver)
          (should full)
          (should (equal "777" (nngmail-account-history-id a)))
          (should (eq 'idle (nngmail-job-state (nngmail-background-job a)))))))))

(provide 'nngmail-background-tests)
;;; nngmail-background-tests.el ends here
