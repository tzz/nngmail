;;; nngmail-tests.el --- ERT tests for nngmail with a mocked Gmail API  -*- lexical-binding: t; -*-

;; Run:  emacs -batch -Q -L . -L /path/to/oauth2 -l nngmail-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'nngmail)

(defvar nngmail-test--calls nil "List of (METHOD PATH PARAMS BODY) seen by the mock.")
(defvar nngmail-test--responses nil "Alist of (REGEXP . RESPONSE-OR-FUNCTION).")

(defun nngmail-test--mock-request (_account method path &optional params body _ct raw)
  "Mock for `nngmail--request'."
  (push (list method path params body) nngmail-test--calls)
  (let ((key (concat method " " path)))
    (cl-loop for (re . resp) in nngmail-test--responses
             when (string-match-p re key)
             return (let ((v (if (functionp resp) (funcall resp method path params body) resp)))
                      (if raw v v))
             finally (error "nngmail-test: unmocked request %s" key))))

(defun nngmail-test--msg (id labels &rest headers)
  "Build a metadata-format message alist."
  `((id . ,id) (threadId . ,(concat "t" id)) (labelIds . ,labels)
    (sizeEstimate . 1234) (internalDate . "1789400000000")
    (payload . ((headers . ,(cl-loop for (n v) on headers by #'cddr
                                     collect `((name . ,n) (value . ,v))))))))

(defmacro nngmail-test-with-mock (responses &rest body)
  "Run BODY with `nngmail--request' mocked using RESPONSES and a temp dir."
  (declare (indent 1))
  `(let* ((nngmail-directory (make-temp-file "nngmail-test" t))
          (nngmail-test--calls nil)
          (nngmail-test--responses ,responses)
          (nngmail-initial-limit nil)
          (nngmail-label-limit nil)
          (nngmail-sync-debounce 0)
          (nngmail-quota-per-minute 1000000)
          (nngmail-batch-parallel 1)
          (nngmail--batch-size-current nil)
          (nngmail--quota-enforced nil)
          (nngmail--quota-log nil)
          (nngmail--last-sync (make-hash-table :test #'equal))
          (nngmail--accounts (make-hash-table :test #'equal))
          (nngmail--current nil))
     (cl-letf (((symbol-function 'nngmail--request) #'nngmail-test--mock-request)
               ((symbol-function 'nngmail--obtain-token) (lambda (_a) "fake-token")))
       (unwind-protect (progn ,@body)
         (delete-directory nngmail-directory t)))))

(defun nngmail-test--account ()
  "Create a fresh test account, loading any persisted state."
  (let ((a (nngmail--make-account :name "test" :user "me@example.com")))
    (nngmail--load a)
    (puthash "test" a nngmail--accounts)
    (setq nngmail--current a)
    a))

(defun nngmail-test--header (a num)
  "The stored NOV header of article NUM in account A (from the database)."
  (nngmail--header a num))

(defun nngmail-test--header-count (a)
  "How many articles in A have a stored header."
  (car (car (sqlite-select (nngmail--db a) "SELECT count(*) FROM messages WHERE header IS NOT NULL"))))

(defun nngmail-test--put-header (a num header)
  "Store HEADER for article NUM of A the way the backend does."
  (nngmail--store-header a num header (gethash num (nngmail-account-labels a))))

(defconst nngmail-test--labels
  '((labels . (((id . "INBOX") (name . "INBOX") (type . "system"))
               ((id . "SENT") (name . "SENT") (type . "system"))
               ((id . "TRASH") (name . "TRASH") (type . "system"))
               ((id . "UNREAD") (name . "UNREAD") (type . "system"))
               ((id . "STARRED") (name . "STARRED") (type . "system"))
               ((id . "Label_1") (name . "Work/Alpha") (type . "user"))))))

(defun nngmail-test--batch-handler (msgs)
  "Return a mock for `nngmail--batch' that serves MSGS (alist id -> msg)."
  (lambda (_account requests &optional _phase)
    (mapcar (lambda (r) (cons (car r) (cdr (assoc (car r) msgs)))) requests)))

(ert-deftest nngmail-test-full-sync-numbering-and-groups ()
  "First sync assigns ascending numbers oldest-first and files by label."
  (let ((msgs `(("a1" . ,(nngmail-test--msg "a1" '("INBOX" "UNREAD") "Subject" "one" "From" "x@y" "Message-ID" "<1@x>"))
                ("b2" . ,(nngmail-test--msg "b2" '("INBOX" "Label_1" "STARRED") "Subject" "two" "From" "x@y" "Message-ID" "<2@x>"))
                ("c3" . ,(nngmail-test--msg "c3" '("TRASH") "Subject" "three" "From" "x@y" "Message-ID" "<3@x>")))))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "100") (emailAddress . "me@example.com")))
          ("GET labels" . ,nngmail-test--labels)
          ;; messages.list returns newest first.
          ("GET messages$" . ((messages . (((id . "c3")) ((id . "b2")) ((id . "a1")))))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account)))
          (nngmail--incremental-sync a)
          ;; Oldest (a1) gets 1, newest (c3) gets 3.
          (should (= 1 (gethash "a1" (nngmail-account-id->num a))))
          (should (= 3 (gethash "c3" (nngmail-account-id->num a))))
          (should (equal '(2 1) (nngmail--group-numbers a "INBOX")))
          (should (equal '(2) (nngmail--group-numbers a "Work/Alpha")))
          (should (equal '(3) (nngmail--group-numbers a "[Gmail]/Trash")))
          ;; Trash is excluded from All Mail.
          (should (equal '(2 1) (nngmail--group-numbers a nngmail-all-mail-group)))
          (should (equal "100" (nngmail-account-history-id a)))
          ;; NOV header content.
          (let ((h (nngmail-test--header a 2)))
            (should (equal "two" (mail-header-subject h)))
            (should (equal "<2@x>" (mail-header-id h)))
            (should (equal "tb2" (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra h)))))))))))

(ert-deftest nngmail-test-persistence-roundtrip ()
  "State survives save/load with identical numbering and groups."
  (let ((msgs `(("a1" . ,(nngmail-test--msg "a1" '("INBOX") "Subject" "s" "From" "f" "Message-ID" "<1@x>")))))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "7")))
          ("GET labels" . ,nngmail-test--labels)
          ("GET messages$" . ((messages . (((id . "a1")))))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account)))
          (nngmail--incremental-sync a)
          (nngmail--save a t)
          (let ((b (nngmail--make-account :name "test" :user "me@example.com")))
            (should (nngmail--load b))
            (should (equal "7" (nngmail-account-history-id b)))
            (should (= 2 (nngmail-account-next-article b)))
            (should (= 1 (gethash "a1" (nngmail-account-id->num b))))
            (should (equal '(1) (nngmail--group-numbers b "INBOX")))
            (should (equal "s" (mail-header-subject (nngmail-test--header b 1))))))))))

(ert-deftest nngmail-test-history-apply ()
  "history.list adds, relabels and deletes are applied incrementally."
  (let ((msgs `(("a1" . ,(nngmail-test--msg "a1" '("INBOX" "UNREAD") "Subject" "s" "From" "f" "Message-ID" "<1@x>"))
                ("n9" . ,(nngmail-test--msg "n9" '("INBOX" "UNREAD") "Subject" "new" "From" "f" "Message-ID" "<9@x>")))))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "10")))
          ("GET labels" . ,nngmail-test--labels)
          ("GET messages$" . ((messages . (((id . "a1"))))))
          ("GET history" . ((historyId . "20")
                            (history . (((id . "11")
                                         (messagesAdded . (((message . ((id . "n9") (labelIds . ("INBOX" "UNREAD"))))))))
                                        ((id . "12")
                                         (labelsRemoved . (((message . ((id . "a1") (labelIds . ("INBOX"))))
                                                            (labelIds . ("UNREAD"))))))
                                        ((id . "13")
                                         (labelsAdded . (((message . ((id . "a1") (labelIds . ("INBOX" "Label_1" "STARRED"))))
                                                          (labelIds . ("Label_1" "STARRED"))))))
                                        ((id . "14")
                                         (messagesDeleted . (((message . ((id . "zz"))))))))))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account)))
          (nngmail--incremental-sync a)           ; full
          (nngmail--incremental-sync a)           ; delta
          (should (equal "20" (nngmail-account-history-id a)))
          (should (= 2 (gethash "n9" (nngmail-account-id->num a))))
          (should (equal '(2 1) (nngmail--group-numbers a "INBOX")))
          (should (equal '(1) (nngmail--group-numbers a "Work/Alpha")))
          (let ((l (gethash 1 (nngmail-account-labels a))))
            (should-not (member "UNREAD" l))
            (should (member "STARRED" l))))))))

(ert-deftest nngmail-test-history-uses-current-labels ()
  "History records carry current labelIds; those win over add/remove deltas.
This is the shape Gmail actually sends: a `messages' list with current
labels alongside labelsAdded/labelsRemoved."
  (let ((msgs `(("a1" . ,(nngmail-test--msg "a1" '("INBOX" "UNREAD") "Subject" "s" "From" "f" "Message-ID" "<1@x>")))))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "10")))
          ("GET labels" . ,nngmail-test--labels)
          ("GET messages$" . ((messages . (((id . "a1"))))))
          ("GET history" . ((historyId . "20")
                            (history . (((id . "11")
                                         ;; User archived it and labelled it in the web UI:
                                         ;; INBOX removed, Label_1 added.  The delta lists
                                         ;; only mention the change, `messages' has the truth.
                                         (messages . (((id . "a1") (threadId . "ta1")
                                                       (labelIds . ("Label_1")))))
                                         (labelsRemoved . (((message . ((id . "a1") (labelIds . ("Label_1"))))
                                                            (labelIds . ("INBOX" "UNREAD")))))
                                         (labelsAdded . (((message . ((id . "a1") (labelIds . ("Label_1"))))
                                                          (labelIds . ("Label_1")))))))))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account)))
          (nngmail--incremental-sync a)
          (nngmail--incremental-sync a)
          (should (equal '("Label_1") (gethash 1 (nngmail-account-labels a))))
          (should-not (nngmail--group-numbers a "INBOX"))
          (should (equal '(1) (nngmail--group-numbers a "Work/Alpha"))))))))

(ert-deftest nngmail-test-full-sync-chronological-numbering ()
  "Label top-ups of older mail still get lower numbers than recent mail."
  (let ((msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id))))
                      '("100" "200" "300" "050" "0f0"))))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "1")))
          ("GET labels" . ,nngmail-test--labels)
          ;; Overall listing (newest first) then the label top-up returns two older ones.
          ("GET messages$" . ,(let ((n 0))
                                (lambda (&rest _)
                                  (cl-incf n)
                                  (if (= n 1)
                                      '((messages . (((id . "300")) ((id . "200")) ((id . "100")))))
                                    '((messages . (((id . "0f0")) ((id . "050"))))))))))
      (let ((nngmail-label-limit 10))
        (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
          (let ((a (nngmail-test--account)))
            (nngmail--incremental-sync a)
            ;; Hex order: 050 < 0f0 < 100 < 200 < 300.
            (should (equal '(1 2 3 4 5)
                           (mapcar (lambda (id) (gethash id (nngmail-account-id->num a)))
                                   '("050" "0f0" "100" "200" "300"))))))))))

(ert-deftest nngmail-test-history-id-saved-before-fetch ()
  "An incremental sync interrupted during its fetch does not re-read the
history pages next time: the history id and the pending list are saved
as soon as the history is applied."
  (let* ((ids '("h1" "h2" "h3" "h4"))
         (msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id)))) ids))
         (history-calls 0)
         (batches 0))
    (nngmail-test-with-mock
        `(("GET labels" . ,nngmail-test--labels)
          ("GET history" . ,(lambda (&rest _)
                              (cl-incf history-calls)
                              ;; Records without labelIds: each needs a fetch.
                              `((historyId . "60")
                                (history . ,(mapcar (lambda (id) `((id . "55") (messagesAdded . (((message . ((id . ,id))))))))
                                                    ids))))))
      (let ((nngmail-batch-size 2) (nngmail-sync-chunk 2))
        (cl-letf (((symbol-function 'nngmail--batch)
                   (lambda (account requests &optional phase)
                     (cl-incf batches)
                     (when (= batches 2) (signal 'nngmail-aborted (list 1000 1000)))
                     (funcall (nngmail-test--batch-handler msgs) account requests phase))))
          (let ((a (nngmail-test--account)))
            (setf (nngmail-account-history-id a) "50")
            (should-error (nngmail--incremental-sync a t) :type 'nngmail-aborted)
            (should (= 1 history-calls))
            (should (= 2 (nngmail-test--header-count a)))
            ;; History id advanced already; the rest is pending, on disk.
            (should (equal "60" (nngmail-account-history-id a)))
            (should (= 2 (length (nth 2 (nngmail-account-pending a)))))
            (let ((b (nngmail--make-account :name "test" :user "me@example.com")))
              (should (nngmail--load b))
              (should (equal "60" (nngmail-account-history-id b)))
              (should (equal (nth 2 (nngmail-account-pending a)) (nth 2 (nngmail-account-pending b))))
              (puthash "test" b nngmail--accounts)
              ;; Next sync: finishes the two, then reads history from 60,
              ;; which yields the same (idempotent) records; no relisting.
              (setq nngmail-test--responses
                    `(("GET labels" . ,nngmail-test--labels)
                      ("GET history" . ,(lambda (_m _p params &rest _)
                                          (cl-incf history-calls)
                                          (should (equal "60" (cdr (assoc "startHistoryId" params))))
                                          '((historyId . "60") (history . nil))))))
              (nngmail--incremental-sync b t)
              (should (= 4 (nngmail-test--header-count b)))
              (should-not (nngmail-account-pending b))
              (should (= 2 history-calls)))))))))

(define-error 'nngmail-test-interrupt "test interrupt")

(ert-deftest nngmail-test-history-checkpoints-per-page ()
  "A multi-page history pass interrupted between pages resumes from the
last applied page, and keeps the messages that page left to fetch."
  (let* ((page-calls nil)
         (quit-on-page 3)
         (msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id))))
                       '("m1" "m2" "m3")))
         ;; Page N carries record id N01 and adds message mN without
         ;; labels (so it needs a fetch); page 3 ends the list.
         (page (lambda (n next)
                 (append `((historyId . "999"))
                         (and next `((nextPageToken . ,next)))
                         `((history . (((id . ,(format "%d01" n))
                                        (messagesAdded . (((message . ((id . ,(format "m%d" n))))))))))))))
         (responses (list (cons 1 (funcall page 1 "p2"))
                          (cons 2 (funcall page 2 "p3"))
                          (cons 3 (funcall page 3 nil))))
         (pages
          (lambda (_m _p params &rest _)
            (let* ((token (cdr (assoc "pageToken" params)))
                   (start (cdr (assoc "startHistoryId" params)))
                   (n (pcase token ("p2" 2) ("p3" 3) (_ 1))))
              (push (cons start token) page-calls)
              (when (eql n quit-on-page) (signal 'nngmail-test-interrupt nil))
              (cdr (assq n responses))))))
    (nngmail-test-with-mock
        `(("GET labels" . ,nngmail-test--labels)
          ("GET history" . ,pages))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account)))
          (setf (nngmail-account-history-id a) "50")
          ;; Interrupted while fetching page 3 (a C-g, a network error):
          ;; pages 1 and 2 are applied and saved.
          (should-error (nngmail--incremental-sync a t) :type 'nngmail-test-interrupt)
          (should (equal "201" (nngmail-account-history-id a)))
          (should (equal '("m1" "m2") (sort (copy-sequence (nth 2 (nngmail-account-pending a))) #'string<)))
          ;; What is on disk agrees.
          (let ((b (nngmail--make-account :name "test" :user "me@example.com")))
            (should (nngmail--load b))
            (should (equal "201" (nngmail-account-history-id b)))
            (should (equal '("m1" "m2") (sort (copy-sequence (nth 2 (nngmail-account-pending b))) #'string<)))
            (puthash "test" b nngmail--accounts)
            ;; Next pass starts at 201, not 50.  The mock keys pages on
            ;; the token, so the first request (no token) is page 1
            ;; again, then 2 and 3; the ids are idempotent.  It finishes,
            ;; adopts 999 and fetches all three.
            (setq page-calls nil quit-on-page nil)
            (nngmail--incremental-sync b t)
            (should (equal "201" (caar (last page-calls))))
            (should (equal "999" (nngmail-account-history-id b)))
            (should-not (nngmail-account-pending b))
            (should (= 3 (nngmail-test--header-count b)))))))))

(ert-deftest nngmail-test-history-expired-falls-back ()
  "A 404 from history.list triggers a full sync instead of an error."
  (let ((msgs `(("a1" . ,(nngmail-test--msg "a1" '("INBOX") "Subject" "s" "From" "f" "Message-ID" "<1@x>")))))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "50")))
          ("GET labels" . ,nngmail-test--labels)
          ("GET messages$" . ((messages . (((id . "a1"))))))
          ("GET history" . ,(lambda (&rest _) (signal 'nngmail-http-error (list 404 "expired" "u")))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account)))
          (setf (nngmail-account-history-id a) "1")
          (nngmail--incremental-sync a)
          (should (equal "50" (nngmail-account-history-id a)))
          (should (= 1 (gethash "a1" (nngmail-account-id->num a)))))))))

(ert-deftest nngmail-test-update-info-marks ()
  "UNREAD/STARRED become Gnus read ranges and tick marks."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (gnus-active-hashtb (gnus-make-hashtable 10)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (dolist (row '((1 "a" ("INBOX")) (2 "b" ("INBOX" "UNREAD")) (5 "e" ("INBOX" "STARRED"))
                     (7 "g" ("SENT"))))
        (pcase-let ((`(,num ,id ,labels) row))
          (puthash id num (nngmail-account-id->num a))
          (puthash num id (nngmail-account-num->id a))
          (puthash num labels (nngmail-account-labels a))))
      (nngmail--rebuild-groups a)
      ;; INBOX holds server articles 1, 2, 5 -> local 1, 2, 3.  Dense: no
      ;; gaps, no unexist marks, active is 1..3.
      (should (equal '(1 2 3) (nngmail--group-locals a "INBOX")))
      (should (equal 5 (nngmail--local->num a "INBOX" 3)))
      (should (equal 3 (nngmail--num->local a "INBOX" 5)))
      (let ((info (list "nngmail+test:INBOX" 3 nil nil)))
        (nngmail--update-info a "INBOX" info)
        ;; Only server 2 (local 2) is unread: read = 1, 3.
        (should (equal '(1 3) (gnus-info-read info)))
        ;; Server 5 (local 3) is starred.
        (should (equal '(3) (cdr (assq 'tick (gnus-info-marks info)))))
        (should-not (assq 'unexist (gnus-info-marks info)))
        (should (equal '(1 . 3) (gnus-active "nngmail+test:INBOX"))))
      ;; A message leaving the group retires its local number for good.
      (nngmail--set-labels-1 a 2 '("SENT"))
      (should (equal '(1 3) (nngmail--group-locals a "INBOX")))
      (let ((info (list "nngmail+test:INBOX" 3 nil nil)))
        (nngmail--update-info a "INBOX" info)
        (should (equal '((1 . 3)) (gnus-info-read info)))
        (should (equal '(1 . 3) (gnus-active "nngmail+test:INBOX"))))
      ;; Coming back gets a new number, never the retired 2.
      (nngmail--set-labels-1 a 2 '("INBOX" "UNREAD"))
      (should (equal '(1 3 4) (nngmail--group-locals a "INBOX")))
      (should (equal 2 (nngmail--local->num a "INBOX" 4))))))

(ert-deftest nngmail-test-set-mark-translation ()
  "Gnus mark actions map onto UNREAD/STARRED label edits."
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (dolist (row '((1 "a" ("INBOX" "UNREAD")) (2 "b" ("INBOX"))))
        (pcase-let ((`(,num ,id ,labels) row))
          (puthash id num (nngmail-account-id->num a))
          (puthash num id (nngmail-account-num->id a))
          (puthash num labels (nngmail-account-labels a))))
      (nngmail--rebuild-groups a)
      ;; Mark 1 as read.
      (nngmail-request-set-mark "INBOX" '(((1) add (read))) "test")
      (let ((body (nth 3 (car nngmail-test--calls))))
        (should (equal ["a"] (alist-get 'ids body)))
        (should (equal ["UNREAD"] (alist-get 'removeLabelIds body))))
      (should-not (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
      ;; Tick 2: adds STARRED and removes UNREAD.
      (nngmail-request-set-mark "INBOX" '(((2) add (tick))) "test")
      (let ((body (nth 3 (car nngmail-test--calls))))
        (should (equal ["STARRED"] (alist-get 'addLabelIds body)))
        (should (equal ["UNREAD"] (alist-get 'removeLabelIds body))))
      ;; Un-read 1 again.
      (nngmail-request-set-mark "INBOX" '(((1) del (read))) "test")
      (should (equal ["UNREAD"] (alist-get 'addLabelIds (nth 3 (car nngmail-test--calls)))))
      (should (member "UNREAD" (gethash 1 (nngmail-account-labels a)))))))

(ert-deftest nngmail-test-request-group-output ()
  "request-group writes a proper 211 line to nntp-server-buffer."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (setf (nngmail-account-history-id a) "1")
      (dolist (row '((3 "c" ("INBOX")) (8 "h" ("INBOX"))))
        (pcase-let ((`(,num ,id ,labels) row))
          (puthash id num (nngmail-account-id->num a))
          (puthash num id (nngmail-account-num->id a))
          (puthash num labels (nngmail-account-labels a))))
      (nngmail--rebuild-groups a)
      (nnheader-init-server-buffer)
      (let ((gnus-active-hashtb (gnus-make-hashtable 10))
            (gnus-newsrc-hashtb (gnus-make-hashtable 10)))
        (should (nngmail-request-group "INBOX" "test" t))
        ;; Two articles, local numbers 1 and 2 (server 3 and 8).
        (with-current-buffer nntp-server-buffer
          (should (equal "211 2 1 2 \"INBOX\"\n" (buffer-string))))))))

(ert-deftest nngmail-test-gnus-g-path-updates-infos ()
  "The `g' path (retrieve-group-data-early/finish) pushes read state into infos.
Mirrors `gnus-get-unread-articles' calling the -early/-finish pair with
stale read ranges from .newsrc.eld: UNREAD/STARRED at Gmail must win."
  (nngmail-test-with-mock nil
    (let* ((a (nngmail-test--account))
           (gnus-active-hashtb (gnus-make-hashtable 10))
           (info (list "nngmail+test:INBOX" 3 '((1 . 8)) nil))
           (info2 (list "nngmail+test:Work/Alpha" 3 nil nil)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "Work/Alpha")))
      (nngmail--rebuild-name-map a)
      (setf (nngmail-account-history-id a) "1")
      (dolist (row '((1 "a" ("Label_1" "UNREAD")) (3 "c" ("INBOX")) (5 "e" ("INBOX" "UNREAD"))
                     (7 "g" ("Label_1" "STARRED")) (8 "h" ("INBOX" "UNREAD"))))
        (pcase-let ((`(,num ,id ,labels) row))
          (puthash id num (nngmail-account-id->num a))
          (puthash num id (nngmail-account-num->id a))
          (nngmail--store-header a num (make-full-mail-header num "s" "f" "d" (format "<%d@x>" num) "" 1 0 nil nil)
                                 labels)))
      (nngmail--rebuild-groups a)
      (nnheader-init-server-buffer)
      (cl-letf (((symbol-function 'nngmail--sync-safely) (lambda (_a) t)))
        (let ((token (nngmail-retrieve-group-data-early "test" (list info info2))))
          (should token)
          (should (nngmail-finish-retrieve-group-infos "test" (list info info2) token))))
      ;; INBOX holds server 3, 5, 8 -> local 1, 2, 3.  Server 3 is read,
      ;; 5 and 8 unread, despite the stale (1 . 8) read range: read = 1.
      (should (equal '(1) (gnus-info-read info)))
      (should (equal '(1 . 3) (gnus-active "nngmail+test:INBOX")))
      (should-not (assq 'unexist (gnus-info-marks info)))
      ;; Work/Alpha holds server 1, 7 -> local 1, 2.  1 unread, 7 starred.
      (should (equal '(2) (gnus-info-read info2)))
      (should (equal '(2) (cdr (assq 'tick (gnus-info-marks info2)))))
      ;; Active lines were emitted for both, in local numbers.
      (with-current-buffer nntp-server-buffer
        (should (string-match-p "^\"INBOX\" 3 1 y$" (buffer-string)))
        (should (string-match-p "^\"Work/Alpha\" 2 1 y$" (buffer-string)))))))

(ert-deftest nngmail-test-batch-parser ()
  "Multipart batch responses are split and parsed per Content-ID."
  (let ((text (concat "--batch_x\r\nContent-Type: application/http\r\n"
                      "Content-ID: <response-a1>\r\n\r\n"
                      "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
                      "{\"id\":\"a1\",\"labelIds\":[\"INBOX\"]}\r\n"
                      "--batch_x\r\nContent-Type: application/http\r\n"
                      "Content-ID: <response-b2>\r\n\r\n"
                      "HTTP/1.1 404 Not Found\r\nContent-Type: application/json\r\n\r\n"
                      "{\"error\":{\"code\":404}}\r\n"
                      "--batch_x--\r\n")))
    (let ((r (nngmail--parse-batch text)))
      (should (equal '("INBOX") (alist-get 'labelIds (cdr (assoc "a1" r)))))
      (should (assoc "b2" r))
      (should-not (cdr (assoc "b2" r))))))

(ert-deftest nngmail-test-batch-request-body ()
  "Batch bodies are well-formed multipart/mixed with one part per request."
  (nngmail-test-with-mock
      `(("POST https://gmail.googleapis.com/batch" .
         ,(lambda (_m _p _params body)
            (should (string-match-p "Content-ID: <x1>" body))
            (should (string-search "GET /gmail/v1/users/me/messages/x1?format=metadata" body))
            "--b\r\nContent-ID: <response-x1>\r\n\r\nHTTP/1.1 200 OK\r\n\r\n{\"id\":\"x1\"}\r\n--b--\r\n")))
    (let ((a (nngmail-test--account)))
      (let ((res (nngmail--batch a (list (list "x1" "GET" (nngmail--metadata-path "x1"))))))
        (should (equal "x1" (alist-get 'id (cdr (assoc "x1" res)))))))))

(ert-deftest nngmail-test-search-transform ()
  "Parsed Gnus queries become Gmail search syntax."
  (let ((e (make-instance 'gnus-search-gmail)))
    (should (equal "from:alice subject:\"hello world\""
                   (gnus-search-transform e '((from . "alice") (subject . "hello world")))))
    (should (equal "{from:a from:b}"
                   (gnus-search-transform e '((or (from . "a") (from . "b"))))))
    (should (equal "-from:spam" (gnus-search-transform e '((not (from . "spam"))))))
    (should (equal "is:starred" (gnus-search-transform e '((mark . "flag")))))
    (should (equal "after:2026/01/15" (gnus-search-transform e '((since 15 1 2026)))))
    (should (equal "rfc822msgid:<x@y>" (gnus-search-transform e '((id . "<x@y>")))))))

(ert-deftest nngmail-test-quota-pacing ()
  "Requests that would exceed the per-minute budget wait for the window."
  (let ((nngmail-quota-per-minute 100)
        (nngmail-quota-burst 100)
        (nngmail-quota-policy 'wait)
        (nngmail-quota-pacing 'always)
        (nngmail--throttled nil)
        (nngmail--quota-log nil)
        (slept nil))
    (cl-letf (((symbol-function 'nngmail--countdown)
               (lambda (s _f &optional _r)
                 (push s slept)
                 ;; Simulate time passing: age the log out.
                 (setq nngmail--quota-log
                       (mapcar (lambda (e) (cons (- (car e) s) (cdr e))) nngmail--quota-log))))
              ((symbol-function 'nngmail--message) #'ignore))
      ;; 15 metadata gets = 75 units: fine, bucket now holds 25.
      (nngmail--quota-wait 75)
      (should-not slept)
      ;; Another 50 needs 25 more tokens; the bucket refills at 100/60 per
      ;; second, so the wait is ~15s (token bucket), not a full 60s window.
      (nngmail--quota-wait 50)
      (should slept)
      (should (< 14 (car slept) 16)))))

(ert-deftest nngmail-test-quota-keep-waiting-asks-once ()
  "[W] answers the prompt once for the session; [w] asks again next time."
  (let ((nngmail-quota-per-minute 100)
        (nngmail-quota-burst 100)
        (nngmail-quota-policy 'ask)
        (nngmail-quota-pacing 'always)
        (nngmail--throttled nil)
        (nngmail--session-quota-policy nil)
        (nngmail--quota-log nil)
        (asks 0)
        (answers nil))
    (cl-letf (((symbol-function 'nngmail--countdown)
               ;; Pretend time passed: age the log instead of sleeping.
               (lambda (s _f &optional _r)
                 (setq nngmail--quota-log
                       (mapcar (lambda (e) (cons (- (car e) s) (cdr e))) nngmail--quota-log))))
              ((symbol-function 'read-char-choice)
               (lambda (&rest _) (cl-incf asks) (pop answers)))
              ((symbol-function 'nngmail--message) #'ignore))
      (let ((noninteractive nil))
        ;; First overrun: user says w (wait once).  Second overrun: asked again.
        (setq answers '(?w ?W))
        (nngmail--quota-wait 75)
        (nngmail--quota-wait 50)                 ; overrun 1 -> w
        (should (= asks 1))
        (nngmail--quota-wait 75)                 ; overrun 2 -> W
        (should (= asks 2))
        (should (eq nngmail--session-quota-policy 'wait))
        ;; Third overrun: no prompt.
        (nngmail--quota-wait 75)
        (should (= asks 2))
        ;; Reset brings the prompt back.
        (nngmail-quota-reset-policy)
        (setq answers '(?t))
        (nngmail--quota-wait 75)
        (should (= asks 3))
        (should nngmail--throttled)
        (should (eq nngmail--session-quota-policy 'throttle))))))

(ert-deftest nngmail-test-quota-abort-and-resume ()
  "Aborting on quota keeps what was fetched and resumes on the next sync."
  (let* ((ids (mapcar (lambda (i) (format "m%02d" i)) (number-sequence 1 6)))
         (msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id)))) ids))
         (batches 0))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "99")))
          ("GET labels" . ,nngmail-test--labels)
          ("GET messages$" . ((messages . ,(mapcar (lambda (id) `((id . ,id))) (reverse ids)))))
          ("GET history" . ((historyId . "99") (history . nil))))
      (let ((nngmail-batch-size 2)
            (nngmail-sync-chunk 2)
            (nngmail-quota-policy 'abort))
        (cl-letf (((symbol-function 'nngmail--batch)
                   (lambda (account requests &optional phase)
                     (cl-incf batches)
                     ;; Third batch: pretend Gmail said quota exceeded and the
                     ;; user chose abort.
                     (when (= batches 3)
                       (signal 'nngmail-aborted (list 1000 1000)))
                     (funcall (nngmail-test--batch-handler msgs) account requests phase))))
          (let ((a (nngmail-test--account)))
            (should-error (nngmail--incremental-sync a) :type 'nngmail-aborted)
            ;; Four messages fetched, two still pending, no historyId yet.
            (should (= 4 (nngmail-test--header-count a)))
            (should (equal '("m05" "m06") (nth 2 (nngmail-account-pending a))))
            (should-not (nngmail-account-history-id a))
            (should (equal "99" (nngmail-account-pending-history-id a)))
            ;; State was checkpointed to disk with the pending list.
            (let ((b (nngmail--make-account :name "test" :user "me@example.com")))
              (should (nngmail--load b))
              (should (equal '("m05" "m06") (nth 2 (nngmail-account-pending b))))
              ;; Next sync resumes: fetches the remainder, adopts the historyId,
              ;; and does NOT relist or do another full sync.
              (puthash "test" b nngmail--accounts)
              (let ((lists-before (cl-count-if (lambda (c) (equal (nth 1 c) "messages"))
                                               nngmail-test--calls)))
                (nngmail--incremental-sync b t)
                (should (= 6 (nngmail-test--header-count b)))
                (should-not (nngmail-account-pending b))
                (should (equal "99" (nngmail-account-history-id b)))
                (should (= lists-before (cl-count-if (lambda (c) (equal (nth 1 c) "messages"))
                                                     nngmail-test--calls)))))))))))

(ert-deftest nngmail-test-quota-cost-estimates ()
  (should (= 5 (nngmail--quota-cost "GET" "messages/abc" nil)))
  (should (= 2 (nngmail--quota-cost "GET" "history" nil)))
  (should (= 100 (nngmail--quota-cost "POST" "messages/send" nil)))
  (should (= 50 (nngmail--quota-cost "POST" "messages/batchModify" nil)))
  ;; A batch body with three parts costs 15.
  (should (= 15 (nngmail--quota-cost "POST" nngmail--batch-api
                                     "--b\r\nContent-ID: <1>\r\n\r\nGET x\r\n--b\r\nContent-ID: <2>\r\n\r\nGET y\r\n--b\r\nContent-ID: <3>\r\n\r\nGET z\r\n--b--"))))

(ert-deftest nngmail-test-sync-debounce ()
  "Back-to-back syncs within the debounce window hit Gmail only once."
  (let ((msgs `(("a1" . ,(nngmail-test--msg "a1" '("INBOX") "Subject" "s" "From" "f" "Message-ID" "<1@x>")))))
    (nngmail-test-with-mock
        `(("GET profile" . ((historyId . "7")))
          ("GET labels" . ,nngmail-test--labels)
          ("GET messages$" . ((messages . (((id . "a1"))))))
          ("GET history" . ((historyId . "7") (history . nil))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account))
              (nngmail-sync-debounce 60))
          (nngmail--incremental-sync a)          ; full
          (let ((before (length nngmail-test--calls)))
            (nngmail--incremental-sync a)        ; debounced
            (nngmail--incremental-sync a)        ; debounced
            (should (= before (length nngmail-test--calls)))
            (nngmail--incremental-sync a t)      ; forced
            (should (> (length nngmail-test--calls) before))))))))

(defun nngmail-test--seed (a rows)
  "Populate account A with ROWS of (NUM ID LABELS) and rebuild groups."
  (nngmail--with-transaction a
    (dolist (row rows)
      (pcase-let ((`(,num ,id ,labels) row))
        (puthash id num (nngmail-account-id->num a))
        (puthash num id (nngmail-account-num->id a))
        (nngmail--store-header a num (make-full-mail-header num "s" "f" "d" (format "<%s@x>" id) "" 1 0 nil nil)
                               labels)
        (setf (nngmail-account-next-article a) (max (nngmail-account-next-article a) (1+ num))))))
  (nngmail--rebuild-groups a))

(ert-deftest nngmail-test-fetch-on-demand ()
  "C-u N RET indexes the newest N of the label that are missing locally."
  (let* ((live-ids '("i9" "i8" "i7" "i6" "i5" "i4" "i3" "i2" "i1"))   ; newest first at Gmail
         (msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id))))
                       live-ids))
         (list-calls 0))
    (nngmail-test-with-mock
        `(("GET messages$" . ,(lambda (_m _p params _b)
                                (cl-incf list-calls)
                                (let* ((max (string-to-number (format "%s" (cdr (assoc "maxResults" params)))))
                                       (page (cdr (assoc "pageToken" params)))
                                       (start (if page (string-to-number page) 0))
                                       (chunk (seq-take (nthcdr start live-ids) max))
                                       (next (+ start (length chunk))))
                                  `((messages . ,(mapcar (lambda (id) `((id . ,id))) chunk))
                                    ,@(when (< next (length live-ids))
                                        `((nextPageToken . ,(number-to-string next)))))))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account))
              (gnus-active-hashtb (gnus-make-hashtable 10)))
          (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
          (nngmail--rebuild-name-map a)
          (setf (nngmail-account-history-id a) "1")
          ;; Only the newest 3 are indexed locally.
          (nngmail-test--seed a '((7 "i7" ("INBOX")) (8 "i8" ("INBOX")) (9 "i9" ("INBOX"))))
          ;; Ask for 5: should fetch i6, i5 only.
          (should (= 2 (nngmail--ensure-newest a "INBOX" 5)))
          (should (= 5 (length (nngmail--group-numbers a "INBOX"))))
          (should (gethash "i5" (nngmail-account-id->num a)))
          (should-not (gethash "i4" (nngmail-account-id->num a)))
          ;; New numbers were allocated above the existing ones (append-only
          ;; numbering); older-first within the fetch.
          (should (< (gethash "i6" (nngmail-account-id->num a))
                     (gethash "i5" (nngmail-account-id->num a))))
          ;; Asking for 5 again does nothing but one cheap list call.
          (let ((before list-calls))
            (should (= 0 (nngmail--ensure-newest a "INBOX" 5)))
            (should (= 1 (- list-calls before))))
          ;; Unlimited backfill of the whole mailbox gets the rest.
          (should (= 4 (nngmail--backfill a nil nil)))
          (should (= 9 (length (nngmail--group-numbers a "INBOX")))))))))

(ert-deftest nngmail-test-select-newsgroup-advice-triggers ()
  "The advice fires only for nngmail groups with a numeric prefix."
  (let (calls)
    (cl-letf (((symbol-function 'nngmail--ensure-newest)
               (lambda (_a g n) (push (cons g n) calls) 0))
              ((symbol-function 'gnus-find-method-for-group)
               (lambda (g) (if (string-prefix-p "nngmail" g) '(nngmail "test") '(nnml ""))))
              ((symbol-function 'nngmail--account) (lambda (&rest _) nil))
              ((symbol-function 'nngmail--group-size) (lambda (&rest _) 3)))
      (let ((nngmail-fetch-on-demand t))
        (nngmail--maybe-fetch-on-demand "nngmail+test:INBOX" 1000)
        (should (equal '(("INBOX" . 1000)) calls))
        ;; Plain RET (nil) and C-u RET ((4)) don't fetch.
        (nngmail--maybe-fetch-on-demand "nngmail+test:INBOX" nil)
        (nngmail--maybe-fetch-on-demand "nngmail+test:INBOX" '(4))
        ;; Already have enough: no fetch.
        (nngmail--maybe-fetch-on-demand "nngmail+test:INBOX" 2)
        ;; Other backends untouched.
        (nngmail--maybe-fetch-on-demand "nnml:mail" 1000)
        (should (= 1 (length calls))))
      (let ((nngmail-fetch-on-demand nil))
        (nngmail--maybe-fetch-on-demand "nngmail+test:INBOX" 1000)
        (should (= 1 (length calls)))))))

(ert-deftest nngmail-test-transport-error-retries ()
  "A transport failure (no response) is retried with backoff, not fatal."
  (let ((attempts 0) (slept nil))
    (cl-letf (((symbol-function 'nngmail--http-url-el)
               (lambda (&rest _)
                 (cl-incf attempts)
                 (if (< attempts 3)
                     (signal 'nngmail-http-error (list "no response" "u"))
                   (list 200 nil "{\"ok\":true}"))))
              ((symbol-function 'nngmail--countdown) (lambda (s _f &optional _r) (push s slept)))
              ((symbol-function 'nngmail--obtain-token) (lambda (_a) "tok"))
              ((symbol-function 'nngmail--quota-wait) #'ignore))
      (let ((nngmail-use-plz nil)
            (a (nngmail--make-account :name "t" :user "u")))
        (should (equal '((ok . t)) (nngmail--request a "GET" "profile")))
        (should (= 3 attempts))
        (should (= 2 (length slept)))
        ;; Exponential, short: 2, 4.
        (should (equal '(4 2) slept))))))

(ert-deftest nngmail-test-transport-error-gives-up ()
  "Persistent transport failure surfaces as nngmail-http-error after retries."
  (let ((attempts 0))
    (cl-letf (((symbol-function 'nngmail--http-url-el)
               (lambda (&rest _) (cl-incf attempts) (signal 'nngmail-http-error (list "no response" "u"))))
              ((symbol-function 'nngmail--countdown) #'ignore)
              ((symbol-function 'nngmail--obtain-token) (lambda (_a) "tok"))
              ((symbol-function 'nngmail--quota-wait) #'ignore))
      (let ((nngmail-use-plz nil)
            (a (nngmail--make-account :name "t" :user "u")))
        (should-error (nngmail--request a "GET" "profile") :type 'nngmail-http-error)
        (should (= 6 attempts))))))

(ert-deftest nngmail-test-backfill-pages-and-resumes ()
  "Backfill fetches page by page, checkpoints the page token, and resumes."
  (let* ((live-ids (mapcar (lambda (i) (format "m%03d" i)) (number-sequence 30 1 -1)))  ; 30 newest-first
         (msgs (mapcar (lambda (id) (cons id (nngmail-test--msg id '("INBOX") "Subject" id "From" "f" "Message-ID" (format "<%s@x>" id))))
                       live-ids))
         (page-size 10)
         (fail-on-page nil)
         (list-calls nil))
    (nngmail-test-with-mock
        `(("GET messages$" . ,(lambda (_m _p params _b)
                                (let* ((page (cdr (assoc "pageToken" params)))
                                       (start (if page (string-to-number page) 0)))
                                  (push start list-calls)
                                  (when (and fail-on-page (= start fail-on-page))
                                    (setq fail-on-page nil)
                                    (signal 'nngmail-http-error (list "too many retries" "u")))
                                  (let* ((chunk (seq-take (nthcdr start live-ids) page-size))
                                         (next (+ start (length chunk))))
                                    `((messages . ,(mapcar (lambda (id) `((id . ,id))) chunk))
                                      ,@(when (< next (length live-ids))
                                          `((nextPageToken . ,(number-to-string next))))))))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs)))
        (let ((a (nngmail-test--account)))
          (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
          (nngmail--rebuild-name-map a)
          (setf (nngmail-account-history-id a) "1")
          ;; Fail when asking for the third page (start=20).
          (setq fail-on-page 20)
          (should-error (nngmail--backfill a nil nil nil "bf") :type 'nngmail-http-error)
          ;; Two pages' worth were fetched and saved before the failure.
          (should (= 20 (nngmail-test--header-count a)))
          (should (equal "20" (plist-get (nngmail-account-backfill a) :page)))
          ;; Reload from disk: checkpoint survives.
          (let ((b (nngmail--make-account :name "test" :user "me@example.com")))
            (should (nngmail--load b))
            (should (equal "20" (plist-get (nngmail-account-backfill b) :page)))
            (puthash "test" b nngmail--accounts)
            (setq list-calls nil)
            ;; Resume: must start at page 20, not 0.
            (should (= 10 (nngmail--backfill b nil nil nil "bf")))
            (should (equal '(20) list-calls))
            (should (= 30 (nngmail-test--header-count b)))
            (should-not (nngmail-account-backfill b))
            ;; A different query does not resume the old checkpoint.
            (setf (nngmail-account-backfill b) '(:spec ("label:x" nil) :page "20" :found 1 :scanned 20))
            (setq list-calls nil)
            (nngmail--backfill b nil nil nil "bf")
            (should (equal 0 (car (last list-calls))))))))))

(ert-deftest nngmail-test-batch-retries-rate-limited-parts ()
  "429 parts inside a 200 batch are retried with a smaller batch, not dropped."
  (let ((calls nil) (nngmail--batch-size-current nil) (nngmail-batch-size 10) (nngmail-batch-parallel 1))
    (cl-letf (((symbol-function 'nngmail--request)
               (lambda (_a _m _p _params body &rest _)
                 (let ((ids (let (r) (with-temp-buffer (insert body) (goto-char 1)
                                       (while (re-search-forward "Content-ID: <\\([^>]+\\)>" nil t)
                                         (push (match-string 1) r)))
                              (nreverse r))))
                   (push (length ids) calls)
                   ;; First call: the last 4 parts are rate-limited.
                   (concat
                    (mapconcat
                     (lambda (id)
                       (if (and (= (length calls) 1) (member id '("i7" "i8" "i9" "i10")))
                           (format "--b\r\nContent-ID: <response-%s>\r\n\r\nHTTP/1.1 429 Too Many Requests\r\n\r\n{\"error\":{\"code\":429}}\r\n" id)
                         (format "--b\r\nContent-ID: <response-%s>\r\n\r\nHTTP/1.1 200 OK\r\n\r\n{\"id\":\"%s\"}\r\n" id id)))
                     ids "")
                    "--b--\r\n"))))
              ((symbol-function 'nngmail--countdown) #'ignore)
              ((symbol-function 'nngmail--message) #'ignore))
      (let* ((reqs (mapcar (lambda (i) (list (format "i%d" i) "GET" (format "messages/i%d" i))) (number-sequence 1 10)))
             (res (nngmail--batch (nngmail--make-account :name "t" :user "u") reqs)))
        ;; All ten came back with JSON.
        (should (= 10 (length res)))
        (should (cl-every #'cdr res))
        (should (equal "i9" (alist-get 'id (cdr (assoc "i9" res)))))
        ;; Two calls: 10 then the 4 retried (the size was halved to 5 for
        ;; that round; after the clean retry it creeps back up).
        (should (equal '(4 10) calls))
        (should (<= nngmail--batch-size-current nngmail-batch-size))))))

(ert-deftest nngmail-test-rate-limit-vs-quota-distinction ()
  "A rateLimitExceeded 403/429 backs off briefly and does NOT charge the quota window;
a per-minute quota 403 does."
  (let ((nngmail--quota-log nil) (nngmail-quota-policy 'wait) (nngmail--throttled nil)
        (nngmail--session-quota-policy nil) (nngmail--batch-size-current nil)
        (nngmail--quota-enforced nil) (nngmail-quota-pacing 'adaptive)
        (responses nil) (waits nil) (quota-waits 0))
    (cl-letf (((symbol-function 'nngmail--http-plz)
               (lambda (&rest _) (pop responses)))
              ((symbol-function 'nngmail--countdown) (lambda (s _f &optional _r) (push s waits)))
              ((symbol-function 'nngmail--quota-wait) (lambda (_u) (cl-incf quota-waits)))
              ((symbol-function 'nngmail--obtain-token) (lambda (_a) "tok"))
              ((symbol-function 'nngmail--message) #'ignore))
      (let ((nngmail-use-plz t) (a (nngmail--make-account :name "t" :user "u")))
        ;; Burst limit with Retry-After: honoured, quota log untouched.
        (setq responses (list (list 429 '(("retry-after" . "7"))
                                    "{\"error\":{\"code\":429,\"message\":\"Rate Limit Exceeded\",\"errors\":[{\"reason\":\"rateLimitExceeded\"}]}}")
                              (list 200 nil "{\"ok\":1}")))
        (should (equal '((ok . 1)) (nngmail--request a "GET" "profile")))
        (should (equal '(7) waits))
        (should-not nngmail--quota-log)
        ;; Per-minute quota: goes through quota-wait, marks the window spent.
        (setq waits nil
              responses (list (list 403 nil
                                    "{\"error\":{\"code\":403,\"message\":\"Quota exceeded for quota metric 'Total Query Cost' and limit 'Units per minute per user'\",\"errors\":[{\"reason\":\"rateLimitExceeded\"}]}}")
                              (list 200 nil "{\"ok\":2}")))
        (should (equal '((ok . 2)) (nngmail--request a "GET" "profile")))
        (should-not waits)
        ;; Pacing is now enforced and the bucket has been drained.
        (should nngmail--quota-enforced)
        (should (< (nngmail--bucket-level nngmail-quota-burst (/ nngmail-quota-per-minute 60.0)) 1))))))


(ert-deftest nngmail-test-batch-parallel-plz ()
  "With plz, several batches go out concurrently and results merge in order."
  (let ((in-flight 0) (max-in-flight 0) (calls 0)
        (nngmail-batch-size 3) (nngmail-batch-parallel 4)
        (nngmail-use-plz t) (nngmail--batch-size-current nil)
        (nngmail-quota-pacing nil) (nngmail--quota-log nil))
    (cl-letf (((symbol-function 'require) (lambda (&rest _) t))
              ((symbol-function 'nngmail--obtain-token) (lambda (_a) "tok"))
              ((symbol-function 'nngmail--message) #'ignore)
              ;; Fake plz: "start" a request, complete it on the next
              ;; accept-process-output, tracking concurrency.
              ((symbol-function 'plz)
               (lambda (_m _u &rest kw)
                 (cl-incf calls) (cl-incf in-flight)
                 (setq max-in-flight (max max-in-flight in-flight))
                 (let* ((body (plist-get kw :body))
                        (then (plist-get kw :then))
                        (ids (let (r) (with-temp-buffer (insert body) (goto-char 1)
                                        (while (re-search-forward "Content-ID: <\\([^>]+\\)>" nil t)
                                          (push (match-string 1) r)))
                               (nreverse r))))
                   (push (lambda ()
                           (cl-decf in-flight)
                           (funcall then
                                    (record 'plz-response "1.1" 200 nil
                                            (concat (mapconcat (lambda (id) (format "--b\r\nContent-ID: <response-%s>\r\n\r\nHTTP/1.1 200 OK\r\n\r\n{\"id\":\"%s\"}\r\n" id id)) ids "")
                                                    "--b--\r\n"))))
                         nngmail-test--pending-completions)
                   'fake-proc)))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _)
                 (when nngmail-test--pending-completions
                   (funcall (pop nngmail-test--pending-completions)))
                 t))
              ((symbol-function 'plz-response-status) (lambda (r) (aref r 2)))
              ((symbol-function 'plz-response-body) (lambda (r) (aref r 4)))
              ((symbol-function 'plz-response-p) (lambda (r) (eq (type-of r) 'plz-response)))
              ((symbol-function 'plz-error-p) (lambda (_r) nil)))
      (let* ((nngmail-test--pending-completions nil)
             (reqs (mapcar (lambda (i) (list (format "i%d" i) "GET" (format "messages/i%d" i))) (number-sequence 1 10)))
             (res (nngmail--batch (nngmail--make-account :name "t" :user "u") reqs)))
        (should (= 10 (length res)))
        (should (equal (mapcar #'car reqs) (mapcar #'car res)))
        (should (cl-every #'cdr res))
        ;; 10 requests / batch 3 = 4 batches; all four started before any finished.
        (should (= 4 calls))
        (should (= 4 max-in-flight))))))

(defvar nngmail-test--pending-completions nil)


(ert-deftest nngmail-test-read-pace-parsing ()
  "The backfill pace prompt accepts labels, numbers in several spellings, and words."
  (cl-flet ((ask (answer)
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) answer)))
                (nngmail--read-pace))))
    (should (eq 'adaptive (ask "")))                                  ; default = first choice = adaptive
    (should (eq 'none (ask "no pacing (react to Gmail's rejections only)")))
    (should (= 15000 (ask "15,000 units/min from the start (Gmail's per-user limit)")))
    (should (eq 'adaptive (ask "adaptive: full speed until Gmail pushes back, then pace at the limit")))
    (should (= 9000 (ask "9000")))
    (should (= 9000 (ask "9,000")))
    (should (= 9000 (ask "9k")))
    (should (= 9000 (ask "9K")))
    (should (eq 'none (ask "no")))
    (should (eq 'adaptive (ask "auto")))
    (should-error (ask "fast") :type 'user-error)))

(ert-deftest nngmail-test-backfill-pace-is-scoped ()
  "Pacing chosen for a backfill does not leak into the session."
  (nngmail-test-with-mock
      `(("GET messages$" . ((messages . nil))))
    (nngmail-test--account)
    (let ((nngmail-quota-pacing nil) (nngmail-quota-per-minute 15000)
          seen-pacing seen-budget)
      (cl-letf (((symbol-function 'nngmail--backfill)
                 (lambda (&rest _) (setq seen-pacing nngmail-quota-pacing
                                         seen-budget nngmail-quota-per-minute)
                   0)))
        (nngmail-backfill "test" nil nil 9000 'rest)
        (should (eq seen-pacing 'always))
        (should (= seen-budget 9000))
        ;; Outside the call: untouched.
        (should (eq nngmail-quota-pacing nil))
        (should (= nngmail-quota-per-minute 15000))
        (nngmail-backfill "test" nil nil 'adaptive 'rest)
        (should (eq seen-pacing 'adaptive))
        ;; Omitted from Lisp = the default, adaptive.
        (nngmail-backfill "test" nil nil nil 'rest)
        (should (eq seen-pacing 'adaptive))
        ;; `none' is the explicit off switch.
        (nngmail-backfill "test" nil nil 'none 'rest)
        (should (eq seen-pacing nil))))))


(ert-deftest nngmail-test-backfill-how-much-parsing ()
  "The first backfill prompt accepts counts, durations, dates and \"all\"."
  (cl-flet ((ask (answer)
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) answer)))
                (nngmail--read-how-much))))
    ;; Menu labels map to their answers.
    (should (equal '(nil . "newer_than:6m") (ask "last 6 months")))
    (should (equal '(1000 . nil) (ask "1,000 more messages")))
    (should (equal '(nil . nil) (ask "everything (the whole mailbox)")))
    ;; Over IMAP only counts and "all" are offered; time answers are refused.
    (should (equal '("1,000 more messages" "10,000 more messages" "everything (the whole mailbox)")
                   (mapcar #'car (nngmail--backfill-choices-for 'imap))))
    (should (= (length nngmail-backfill-choices) (length (nngmail--backfill-choices-for 'rest))))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "30d")))
      (should-error (nngmail--read-how-much 'imap) :type 'user-error)
      (should (equal '(nil . "newer_than:30d") (nngmail--read-how-much 'rest))))
    ;; Free-form input.
    (should (equal '(nil . nil) (ask "")))
    (should (equal '(nil . nil) (ask "all")))
    (should (equal '(1000 . nil) (ask "1000")))
    (should (equal '(2500 . nil) (ask "2,500")))
    (should (equal '(nil . "newer_than:30d") (ask "30d")))
    (should (equal '(nil . "newer_than:21d") (ask "3 weeks")))
    (should (equal '(nil . "newer_than:6m") (ask "6m")))
    (should (equal '(nil . "newer_than:18m") (ask "18 months")))
    (should (equal '(nil . "newer_than:2y") (ask "2y")))
    (should (equal '(nil . "newer_than:1y") (ask "year")))
    (should (equal '(nil . "after:2024/01/15") (ask "2024-01-15")))
    (should (equal '(nil . "after:2024/06/01") (ask "2024-06")))
    ;; A bare number is always a count, never a year.
    (should (equal '(2023 . nil) (ask "2023")))
    (should (equal '(1000 . nil) (ask "1000")))
    (should-error (ask "0") :type 'user-error)
    (should-error (ask "soon") :type 'user-error)))

(ert-deftest nngmail-test-backfill-time-window-becomes-query ()
  "A time answer plus a free-form query are combined into one Gmail query."
  (let (seen)
    (cl-letf (((symbol-function 'nngmail--backfill)
               (lambda (_a count query &rest _) (setq seen (cons count query)) 0))
              ((symbol-function 'nngmail--account) (lambda (&rest _) (nngmail--make-account :name "t" :user "u")))
              ((symbol-function 'nngmail--message) #'ignore)
              ((symbol-function 'nngmail--progress) #'ignore))
      ;; Simulate the interactive spec's answers: the REST server, "6m",
      ;; "label:inbox".
      (let ((nngmail--accounts (make-hash-table :test #'equal))
            (nngmail--imap-unavailable-reason nil))
        (puthash "t" (nngmail--make-account :name "t" :user "u") nngmail--accounts)
        (let* ((rest-label (car (seq-find (lambda (c) (eq (cddr c) 'rest)) (nngmail--backfill-servers))))
               (cr-answers (list rest-label "6m")) (rs-answers (list "label:inbox")))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop cr-answers)))
                    ((symbol-function 'read-string) (lambda (&rest _) (pop rs-answers)))
                    ((symbol-function 'nngmail--read-pace) (lambda () nil)))
            (call-interactively #'nngmail-backfill)
            (should (equal '(nil . "newer_than:6m label:inbox") seen))))
        ;; The IMAP server: no query prompt at all, count goes through.
        (let* ((imap-label (car (seq-find (lambda (c) (eq (cddr c) 'imap)) (nngmail--backfill-servers))))
               (cr-answers (list imap-label "1000")) (asked-query nil) (imap-args nil))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop cr-answers)))
                    ((symbol-function 'read-string) (lambda (&rest _) (setq asked-query t) ""))
                    ((symbol-function 'nngmail--imap-import) (lambda (&rest a) (setq imap-args a) 0))
                    ((symbol-function 'nngmail--imap-close) #'ignore))
            (call-interactively #'nngmail-backfill)
            (should-not asked-query)
            (should (= 1000 (nth 1 imap-args)))))))))


(ert-deftest nngmail-test-pace-preselected-not-positional ()
  "The preselected pace is chosen by value, not by menu position or UI sorting."
  (let (seen-default seen-first)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt table &rest args)
                 ;; Record what the prompt offers as default and as first entry.
                 ;; args = (predicate require-match initial hist def ...)
                 (setq seen-default (nth 4 args)
                       seen-first (car (all-completions "" table)))
                 "")))
      ;; Even with the choices list reordered so adaptive is last, RET = adaptive.
      (let ((nngmail-backfill-pace-choices
             '(("no pacing (react to Gmail's rejections only)" . none)
               ("15,000 units/min from the start (Gmail's per-user limit)" . 15000)
               ("adaptive: full speed until Gmail pushes back, then pace at the limit" . adaptive)))
            (nngmail-backfill-pace-preselected 'adaptive))
        (should (eq 'adaptive (nngmail--read-pace)))
        (should (string-prefix-p "adaptive" seen-default))
        (should (string-prefix-p "adaptive" seen-first)))
      ;; And the knob can point elsewhere.
      (let ((nngmail-backfill-pace-preselected 15000))
        (should (= 15000 (nngmail--read-pace)))
        (should (string-prefix-p "15,000" seen-default))))))


(ert-deftest nngmail-test-server-marks-declared ()
  "Gnus only calls request-set-mark for backends declaring `server-marks'."
  (should (memq 'server-marks (assoc "nngmail" gnus-valid-select-methods))))

(ert-deftest nngmail-test-set-mark-restricted-to-group-members ()
  "A catch-up range spanning other labels' numbers only touches this group's
articles, and only those whose labels would change."
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "x")))
      (nngmail--rebuild-name-map a)
      ;; 1,3,5 in INBOX (3 already read); 2,4 belong to label x, 6 is trash.
      (nngmail-test--seed a '((1 "a" ("INBOX" "UNREAD")) (2 "b" ("Label_1" "UNREAD"))
                              (3 "c" ("INBOX")) (4 "d" ("Label_1" "UNREAD"))
                              (5 "e" ("INBOX" "UNREAD")) (6 "f" ("TRASH" "UNREAD"))))
      ;; Gnus catch-up: "everything 1..6 is read".
      (nngmail-request-set-mark "INBOX" '(((1 . 6) add (read))) "test")
      (let* ((call (car nngmail-test--calls))
             (body (nth 3 call)))
        ;; Exactly one batchModify, for the two INBOX articles that were unread.
        (should (equal "messages/batchModify" (nth 1 call)))
        (should (equal ["a" "e"] (alist-get 'ids body)))
        (should (equal ["UNREAD"] (alist-get 'removeLabelIds body))))
      ;; Other labels' messages untouched locally too.
      (should (member "UNREAD" (gethash 2 (nngmail-account-labels a))))
      (should (member "UNREAD" (gethash 4 (nngmail-account-labels a))))
      (should-not (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
      ;; Repeating the catch-up is a no-op: no further request.
      (let ((n (length nngmail-test--calls)))
        (nngmail-request-set-mark "INBOX" '(((1 . 6) add (read))) "test")
        (should (= n (length nngmail-test--calls)))))))


(ert-deftest nngmail-test-duplicate-suppression-reaches-gmail ()
  "M marks from `gnus-suppress-duplicates' are pushed to Gmail as read.
The same Gmail message appears in several nngmail groups under one number
and one Message-ID; Gnus's duplicate suppression marks the second sighting
read locally without telling the backend.  nngmail must propagate it, or
the next sync makes it unread again."
  (require 'gnus-dup)
  (let ((nngmail-mark-propagation 'immediate))
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let ((a (nngmail-test--account))
          (gnus-suppress-duplicates t) (gnus-save-duplicate-list nil)
          (gnus-dup-hashtb nil) (gnus-dup-list nil)
          (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "x")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((5 "e" ("INBOX" "Label_1" "UNREAD"))))
      ;; Register the group so gnus-find-method-for-group resolves it.
      (setq gnus-newsrc-alist (list (list "nngmail+test:x" 3 nil nil (list 'nngmail "test"))))
      (gnus-make-hashtable-from-newsrc-alist)
      (gnus-dup-open)
      (puthash "<e@x>" t gnus-dup-hashtb)          ; read elsewhere earlier
      ;; In group x, server article 5 is local article 1: that is what the
      ;; summary buffer holds.
      (should (equal 1 (nngmail--num->local a "x" 5)))
      (let* ((h (copy-sequence (nngmail-test--header a 5)))
             (gnus-newsgroup-name "nngmail+test:x")
             (gnus-newsgroup-unreads '(1))
             (gnus-newsgroup-reads nil)
             (gnus-newsgroup-auto-expire nil)
             (gnus-newsgroup-headers (progn (setf (mail-header-number h) 1) (list h))))
        (gnus-dup-suppress-articles)
        ;; Gnus marked it M ...
        (should (equal (list (cons 1 gnus-duplicate-mark)) gnus-newsgroup-reads))
        ;; ... and nngmail told Gmail.
        (let ((call (car nngmail-test--calls)))
          (should (equal "messages/batchModify" (nth 1 call)))
          (should (equal ["e"] (alist-get 'ids (nth 3 call))))
          (should (equal ["UNREAD"] (alist-get 'removeLabelIds (nth 3 call)))))
        (should-not (member "UNREAD" (gethash 5 (nngmail-account-labels a)))))))))


(ert-deftest nngmail-test-eager-mark-propagation ()
  "Reading an article is pushed to Gmail immediately, not only at exit.
Reproduces the bug where a sync between reading and exiting rebuilt the
info from Gmail's labels, so Gnus's exit-time diff found nothing to send."
  (let ((nngmail-mark-propagation 'immediate))
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let* ((a (nngmail-test--account))
           (gnus-active-hashtb (gnus-make-hashtable 10))
           (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil)
           (gnus-undo-mode nil) (gnus-use-cache nil)
           (g "nngmail+test:INBOX")
           (info (list g 3 '((1 . 1)) nil (list 'nngmail "test")))
           (nngmail--pending-marks nil) (nngmail--pending-marks-timer nil))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (setf (nngmail-account-history-id a) "1")
      (nngmail-test--seed a '((3 "c" ("INBOX")) (5 "e" ("INBOX" "UNREAD"))))
      (nnheader-init-server-buffer)
      (get-buffer-create "*Group*")
      (setq gnus-newsrc-alist (list info))
      (gnus-make-hashtable-from-newsrc-alist)
      (cl-letf (((symbol-function 'gnus-check-group) (lambda (&rest _) t))
                ((symbol-function 'run-with-idle-timer) (lambda (&rest _) (timer-create)))
                ;; `gnus-summary-article-number' is a defsubst reading this property.
                ((symbol-function 'get-text-property)
                 (lambda (_pos prop &rest _) (and (eq prop 'gnus-number) 2)))
                ((symbol-function 'gnus-summary-update-mark) #'ignore)
                ((symbol-function 'gnus-request-update-mark) (lambda (_g _a m) m)))
        (nngmail-request-group "INBOX" "test" t (gnus-get-info g))
        (let ((gnus-newsgroup-name g) (gnus-newsgroup-active (gnus-active g))
              (gnus-newsgroup-unreads '(2)) (gnus-newsgroup-marked nil)
              (gnus-newsgroup-spam-marked nil) (gnus-newsgroup-dormant nil)
              (gnus-newsgroup-reads nil) (gnus-newsgroup-expirable nil)
              (gnus-newsgroup-auto-expire nil))
          ;; User reads local article 2 (server 5).
          (gnus-summary-mark-article-as-read gnus-read-mark)
          (should (equal '(("nngmail+test:INBOX" (2 . read))) nngmail--pending-marks))
          (nngmail-flush-marks)
          ;; One batchModify went out and local labels agree.
          (should (= 1 (cl-count-if (lambda (c) (equal (nth 1 c) "messages/batchModify"))
                                    nngmail-test--calls)))
          (should-not (member "UNREAD" (gethash 5 (nngmail-account-labels a))))
          ;; A sync while the summary is open rebuilds the info: still read.
          (nngmail-finish-retrieve-group-infos "test" (list (gnus-get-info g)) t)
          (should (equal '((1 . 2)) (gnus-info-read (gnus-get-info g))))
          ;; Exit: nothing further to send.
          (let ((before (length nngmail-test--calls))
                (gnus-group-is-exiting-p t))
            (run-hooks 'gnus-summary-prepare-exit-hook)
            (gnus-update-read-articles g gnus-newsgroup-unreads)
            (should (= before (length nngmail-test--calls))))))
      ;; Next sync from labels keeps it read.
      (nngmail--update-info a "INBOX" (gnus-get-info g))
      (should-not (gnus-list-of-unread-articles g))))))

(ert-deftest nngmail-test-eager-marks-coalesce ()
  "Many quick mark changes (catch-up) become one request per group."
  (let ((nngmail-mark-propagation 'immediate))
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let ((a (nngmail-test--account))
          (nngmail--pending-marks nil) (nngmail--pending-marks-timer nil)
          (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "a" ("INBOX" "UNREAD")) (2 "b" ("INBOX" "UNREAD")) (3 "c" ("INBOX" "UNREAD"))))
      (setq gnus-newsrc-alist (list (list "nngmail+test:INBOX" 3 nil nil (list 'nngmail "test"))))
      (gnus-make-hashtable-from-newsrc-alist)
      (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) (timer-create)))
                ((symbol-function 'gnus-check-group) (lambda (&rest _) t)))
        (dolist (n '(1 2 3)) (nngmail--queue-mark "nngmail+test:INBOX" n 'read))
        (nngmail-flush-marks)
        (let ((mods (seq-filter (lambda (c) (equal (nth 1 c) "messages/batchModify")) nngmail-test--calls)))
          (should (= 1 (length mods)))
          (should (equal ["a" "b" "c"] (alist-get 'ids (nth 3 (car mods)))))))))))


(ert-deftest nngmail-test-group-buffer-catchup-uses-gmail-truth ()
  "`c' in *Group* marks read everything Gmail has UNREAD, not just what a
possibly-stale info thinks is unread."
  (require 'gnus-group)
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let* ((a (nngmail-test--account))
           (gnus-active-hashtb (gnus-make-hashtable 10))
           (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil)
           (gnus-undo-mode nil)
           (g "nngmail+test:INBOX")
           ;; Stale info: claims 5 is read; Gmail says 5 and 8 are UNREAD.
           (info (list g 3 '((1 . 7)) nil (list 'nngmail "test"))))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((3 "c" ("INBOX")) (5 "e" ("INBOX" "UNREAD")) (8 "h" ("INBOX" "UNREAD"))))
      (get-buffer-create "*Group*")
      (setq gnus-newsrc-alist (list info))
      (gnus-make-hashtable-from-newsrc-alist)
      (gnus-set-active g '(3 . 8))
      (gnus-get-unread-articles-in-group (gnus-get-info g) (gnus-active g))
      (should (equal '(8) (gnus-list-of-unread-articles g)))   ; Gnus's stale view
      (cl-letf (((symbol-function 'gnus-check-group) (lambda (&rest _) t))
                ((symbol-function 'gnus-group-update-group) #'ignore)
                ((symbol-function 'nnmail-purge-split-history) #'ignore))
        (gnus-group-catchup g))
      ;; Both were pushed, in one request.
      (let ((mods (seq-filter (lambda (c) (equal (nth 1 c) "messages/batchModify")) nngmail-test--calls)))
        (should (= 1 (length mods)))
        (should (equal ["e" "h"] (alist-get 'ids (nth 3 (car mods))))))
      (should-not (member "UNREAD" (gethash 5 (nngmail-account-labels a))))
      (should-not (member "UNREAD" (gethash 8 (nngmail-account-labels a))))
      ;; And the info now agrees with Gmail.
      (should-not (gnus-list-of-unread-articles g)))))


(ert-deftest nngmail-test-spam-and-trash-are-quarantined ()
  "A message carrying SPAM (or TRASH) appears only in that group, never in
All Mail, INBOX or a user label it also carries."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a)
            '(("INBOX" . "INBOX") ("SPAM" . "[Gmail]/Spam") ("TRASH" . "[Gmail]/Trash") ("Label_1" . "x")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "a" ("INBOX" "Label_1"))
                              (2 "b" ("INBOX" "Label_1" "SPAM"))      ; spam that still has INBOX+label
                              (3 "c" ("Label_1" "TRASH"))
                              (4 "d" ("SPAM" "TRASH"))))
      (should (equal '(1) (nngmail--group-numbers a "INBOX")))
      (should (equal '(1) (nngmail--group-numbers a "x")))
      (should (equal '(1) (nngmail--group-numbers a nngmail-all-mail-group)))
      (should (equal '(4 2) (nngmail--group-numbers a "[Gmail]/Spam")))
      (should (equal '(4 3) (nngmail--group-numbers a "[Gmail]/Trash")))
      ;; Unspamming (label change) moves it back into the normal groups.
      (nngmail--set-labels a 2 '("INBOX" "Label_1"))
      (should (equal '(2 1) (nngmail--group-numbers a "INBOX")))
      (should-not (memq 2 (nngmail--group-numbers a "[Gmail]/Spam"))))))


(ert-deftest nngmail-test-history-ignores-churn-on-unindexed-mail ()
  "A refresh fetches only new arrivals and label refreshes for mail we index.
Label changes on the millions of messages we never indexed must not
trigger metadata fetches: history maintains the index, it does not grow
it (backfill and C-u N RET do)."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (nngmail-sync-headers 'eager))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "x")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "have" ("INBOX" "UNREAD"))))
      (let ((refetch
             (append
              ;; New arrival: fetch (eager mode; lazy files it instead, see
              ;; `nngmail-test-lazy-headers-from-history').
              (nngmail--apply-history
               a '((messages . (((id . "new") (labelIds . ("INBOX" "UNREAD")))))
                   (messagesAdded . (((message . ((id . "new") (labelIds . ("INBOX" "UNREAD")))))))))
              ;; Read-state change on indexed mail with labels supplied: apply locally, no fetch.
              (nngmail--apply-history
               a '((messages . (((id . "have") (labelIds . ("INBOX")))))
                   (labelsRemoved . (((message . ((id . "have") (labelIds . ("INBOX")))) (labelIds . ("UNREAD")))))))
              ;; Label added to mail we never indexed: ignore.
              (nngmail--apply-history
               a '((messages . (((id . "old1") (labelIds . ("Label_1")))))
                   (labelsAdded . (((message . ((id . "old1") (labelIds . ("Label_1")))) (labelIds . ("Label_1")))))))
              ;; Same, with no labels supplied at all: still ignore.
              (nngmail--apply-history
               a '((messages . (((id . "old2"))))
                   (labelsRemoved . (((message . ((id . "old2"))) (labelIds . ("UNREAD"))))))))))
        (should (equal '("new") refetch))
        (should-not (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
        ;; Unindexed churn stayed unindexed in either mode.
        (should-not (gethash "old1" (nngmail-account-id->num a)))
        (should-not (gethash "old2" (nngmail-account-id->num a)))))))


(ert-deftest nngmail-test-summary-other-labels-format ()
  "%u&gmail-labels; etc. show a message's other labels, excluding the current
group, state labels and quarantine; X-Gmail-Labels header lists all."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil))
      (setf (nngmail-account-label-names a)
            '(("INBOX" . "INBOX") ("Label_1" . "dev/review") ("Label_2" . "wiki") ("SPAM" . "[Gmail]/Spam")))
      (nngmail--rebuild-name-map a)
      (setq gnus-newsrc-alist (list (list "nngmail+test:dev/review" 3 nil nil (list 'nngmail "test"))))
      (gnus-make-hashtable-from-newsrc-alist)
      (let* ((mk (lambda (labels)
                   (make-full-mail-header 1 "s" "f" "d" "<1@x>" "" 1 0 nil
                                          `((X-Gmail-Labels . ,(mapconcat #'identity labels ","))))))
             (gnus-newsgroup-name "nngmail+test:dev/review"))
        ;; In dev/review, also in INBOX and wiki, plus state noise.
        (let ((h (funcall mk '("INBOX" "Label_1" "Label_2" "UNREAD" "IMPORTANT" "CATEGORY_FORUMS"))))
          (should (equal "INBOX,wiki" (gnus-user-format-function-gmail-labels h)))
          (should (equal "2" (gnus-user-format-function-gmail-label-count h)))
          (should (equal "& " (gnus-user-format-function-gmail-label-mark h))))
        ;; Icons: alist entry by id with a face, by name without, and
        ;; the display function wins over the alist.
        (setf (nngmail-account-label-names a)
              (append (nngmail-account-label-names a)
                      '(("Label_9" . "YELLOW_STAR") ("Label_8" . "Team Lead"))))
        (nngmail--rebuild-name-map a)
        (let* ((nngmail-label-display-alist
                '(("YELLOW_STAR" star :foreground "gold") ("Team Lead" "H")))
               (h (funcall mk '("Label_1" "Label_9" "Label_8" "Label_2")))
               (out (gnus-user-format-function-gmail-labels h)))
          (should (equal "\u2605H wiki" (substring-no-properties out)))
          (should (equal '((:foreground "gold") default) (get-text-property 0 'face out)))
          (should (get-text-property 0 'gnus-face out))
          (should-not (get-text-property 1 'face out))
          (let ((nngmail-label-display-function
                 (lambda (id _name) (and (equal id "Label_2") "W"))))
            (should (equal "\u2605HW" (substring-no-properties
                                        (gnus-user-format-function-gmail-labels h)))))
          ;; The mark cell on a text terminal: first icon's fallback text,
          ;; column padded; tooltip names every label.
          (let* ((one (funcall mk '("Label_1" "Label_9" "Label_2")))
                 (m1 (gnus-user-format-function-gmail-label-mark one))
                 (m2 (gnus-user-format-function-gmail-label-mark h)))
            (should (equal "\u2605 " (substring-no-properties m1)))
            (should (equal '((:foreground "gold") default) (get-text-property 0 'face m1)))
            (should (equal "\u2605 " (substring-no-properties m2)))
            (should (equal "\u2605 YELLOW_STAR\nH Team Lead\nwiki"
                           (get-text-property 0 'help-echo m2))))
          ;; Graphical: one image exactly the cell size; a lone icon fills
          ;; it, several go into a 2x2 grid; the cache is keyed by content.
          (clrhash nngmail--label-mark-images)
          (cl-letf (((symbol-function 'nngmail--svg-p) (lambda () t))
                    ;; Two spaces measure 20 px in this buffer: 10 px cells.
                    ((symbol-function 'string-pixel-width)
                     (lambda (str &rest _) (* 10 (length str))))
                    ((symbol-function 'frame-char-height) (lambda (&rest _) 20))
                    ((symbol-function 'color-values)
                     (lambda (c &rest _) (and (equal c "gold") '(65535 55255 0))))
                    ((symbol-function 'svg-image)
                     (lambda (svg &rest props) `(image :type svg :data ,svg ,@props))))
            (let* ((m (gnus-user-format-function-gmail-label-mark
                       (funcall mk '("Label_1" "Label_9"))))
                   (img (get-text-property 0 'display m))
                   (svg (plist-get (cdr img) :data)))
              (should (equal "  " m))
              (should (equal "20" (format "%s" (dom-attr svg 'width))))
              (should (equal "20" (format "%s" (dom-attr svg 'height))))
              (should (eql 1 (plist-get (cdr img) :scale)))
              (should (= 1 (length (dom-by-tag svg 'path))))
              (should (equal "#ffd700" (dom-attr (car (dom-by-tag svg 'path)) 'fill)))
              ;; The lone icon fills the cell: a 16-grid scaled to 20 px.
              (should (equal "translate(0.0 0.0) scale(1.25)"
                             (dom-attr (car (dom-by-tag svg 'g)) 'transform)))
              (should (eq img (get-text-property
                               0 'display (gnus-user-format-function-gmail-label-mark
                                           (funcall mk '("Label_1" "Label_9")))))))
            (let* ((m (gnus-user-format-function-gmail-label-mark h))
                   (svg (plist-get (cdr (get-text-property 0 'display m)) :data))
                   (star (car (dom-by-tag svg 'g)))
                   (text (car (dom-by-tag svg 'text))))
              ;; Star top-left, "H" top-right, each in a 10 px box.
              (should (equal "translate(0.0 0.0) scale(0.625)" (dom-attr star 'transform)))
              (should (equal "H" (dom-text text)))
              (should (< 10 (string-to-number (format "%s" (dom-attr text 'x))) 20))
              (should (< (string-to-number (format "%s" (dom-attr text 'font-size))) 10)))
            ;; No icons: the plain mark, drawn as text in the image.
            (let* ((m (gnus-user-format-function-gmail-label-mark (funcall mk '("Label_1" "INBOX"))))
                   (svg (plist-get (cdr (get-text-property 0 'display m)) :data)))
              (should (equal "&amp;" (dom-text (car (dom-by-tag svg 'text))))))
            ;; %u&gmail-labels; on a graphical display: the icons are one
            ;; image, one text cell per icon, so the column keeps its
            ;; width (a glyph star is drawn wider than its one column).
            (let* ((out (gnus-user-format-function-gmail-labels h))
                   (img (get-text-property 0 'display out))
                   (svg (plist-get (cdr img) :data)))
              (should (equal "   wiki" (substring-no-properties out)))
              (should (= 2 (next-single-property-change 0 'display out)))
              (should (equal "20" (format "%s" (dom-attr svg 'width))))
              ;; Star in the first 10 px cell, H in the second, side by side.
              (should (equal "translate(0.0 5.0) scale(0.625)"
                             (dom-attr (car (dom-by-tag svg 'g)) 'transform)))
              (should (< 10 (string-to-number (format "%s" (dom-attr (car (dom-by-tag svg 'text)) 'x))) 20)))
            (clrhash nngmail--label-mark-images)))
        ;; Only here: nothing shown.
        (let ((h (funcall mk '("Label_1" "UNREAD"))))
          (should (equal "" (gnus-user-format-function-gmail-labels h)))
          (should (equal "" (gnus-user-format-function-gmail-label-count h)))
          (should (equal "  " (gnus-user-format-function-gmail-label-mark h)))))
      ;; Article header insertion.
      (nngmail-test--seed a '((7 "g" ("INBOX" "Label_1" "UNREAD"))))
      (with-temp-buffer
        (insert "From: x\nSubject: y\n\nbody\n")
        (nngmail--insert-labels-header a 7)
        (goto-char (point-min))
        (should (re-search-forward "^X-Gmail-Labels: INBOX, dev/review$" nil t))
        ;; header went before the blank line, not into the body
        (should (< (point) (progn (goto-char (point-min)) (re-search-forward "^$") (point))))))))

(ert-deftest nngmail-test-cross-group-mark-announcement ()
  "Reading in one group announces the other groups it affected."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account)) (said nil))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "dev/review") ("Label_2" . "wiki")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "a" ("INBOX" "Label_1")) (2 "b" ("INBOX" "Label_1" "Label_2")) (3 "c" ("Label_1"))))
      (cl-letf (((symbol-function 'nngmail--message) (lambda (_l f &rest args) (push (apply #'format f args) said))))
        (nngmail--announce-cross-group a "nngmail+test:dev/review" '(1 2 3) 'read)
        (should (equal '("also marked read in INBOX (2), wiki") said))
        (setq said nil)
        ;; Article only in this group: nothing to announce.
        (nngmail--announce-cross-group a "nngmail+test:dev/review" '(3) 'read)
        (should-not said)))))

(ert-deftest nngmail-test-base64url ()
  (should (equal "SGVsbG8_Pz4-" (nngmail--base64url "Hello??>>")))
  (should-not (string-match-p "[+/=]" (nngmail--base64url "\xff\xfe\xfd\xfc"))))

(ert-deftest nngmail-test-expire-trash ()
  "Expiring with `trash' calls messages/ID/trash and relabels locally."
  (nngmail-test-with-mock
      `(("POST messages/a/trash" . ((id . "a")))
        ("DELETE messages/a" . nil))
    (let ((a (nngmail-test--account))
          (nngmail-expire-action 'trash))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("TRASH" . "[Gmail]/Trash")))
      (nngmail--rebuild-name-map a)
      (puthash "a" 1 (nngmail-account-id->num a))
      (puthash 1 "a" (nngmail-account-num->id a))
      (puthash 1 '("INBOX" "UNREAD") (nngmail-account-labels a))
      (nngmail--rebuild-groups a)
      (should (null (nngmail-request-expire-articles '(1) "INBOX" "test" t)))
      (should (equal "POST" (car (car nngmail-test--calls))))
      (should (equal '(1) (nngmail--group-numbers a "[Gmail]/Trash")))
      (should-not (nngmail--group-numbers a "INBOX"))
      (should-not (nngmail--group-numbers a nngmail-all-mail-group))
      ;; Expiring from Trash deletes for good.
      (should (null (nngmail-request-expire-articles '(1) "[Gmail]/Trash" "test" t)))
      (should (equal "DELETE" (car (car nngmail-test--calls))))
      (should-not (gethash 1 (nngmail-account-num->id a))))))

(provide 'nngmail-tests)
;;; nngmail-tests.el ends here


(ert-deftest nngmail-test-batch-single-flight ()
  "Only one batch is in flight; the idle mark flush defers while one is out."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (nngmail--batch-in-flight nil)
          (nngmail--pending-marks '(("nngmail+test:INBOX" (1 . read))))
          (nngmail--pending-marks-timer nil)
          (flushed nil)
          (rescheduled nil))
      ;; While a batch is out, a timer-driven flush reschedules itself
      ;; instead of sending or blocking.
      (cl-letf (((symbol-function 'nngmail--flush-marks-now) (lambda () (setq flushed t)))
                ((symbol-function 'run-with-idle-timer)
                 (lambda (&rest args) (setq rescheduled args) 'timer)))
        (let ((nngmail--batch-in-flight "indexing"))
          (nngmail-flush-marks))
        (should-not flushed)
        (should rescheduled)
        (should (eq 'timer nngmail--pending-marks-timer))
        ;; With no batch out it flushes at once.
        (nngmail-flush-marks)
        (should flushed))
      ;; A nested call (timer inside the wait) is refused, not blocked.
      (let ((nngmail--batch-in-flight "indexing"))
        (should (eq 'nngmail-busy
                    (car (should-error (nngmail--batch a '(("x" "GET" "messages/x")) "scan")))))
        ;; and the sync wrapper treats that as "skip", not as a failure.
        (cl-letf (((symbol-function 'nngmail--incremental-sync)
                   (lambda (&rest _) (signal 'nngmail-busy '("indexing")))))
          (should-not (nngmail--sync-safely a))
          (should-not (nngmail-account-last-error a))))
      ;; The slot is held for the duration of a batch and released after,
      ;; even when the batch fails.
      (let ((seen nil))
        (cl-letf (((symbol-function 'nngmail--batch-1)
                   (lambda (&rest _) (setq seen nngmail--batch-in-flight) nil)))
          (nngmail--batch a '(("x" "GET" "messages/x")) "probe")
          (should (equal "probe" seen))
          (should-not nngmail--batch-in-flight))
        (cl-letf (((symbol-function 'nngmail--batch-1)
                   (lambda (&rest _) (error "boom"))))
          (should-error (nngmail--batch a '(("x" "GET" "messages/x")) "probe"))
          (should-not nngmail--batch-in-flight))))))


(ert-deftest nngmail-test-batch-size-ceiling ()
  "After a concurrency rejection the batch size never grows past what worked."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (nngmail--batch-size-current nil)
          (nngmail--batch-size-ceiling nil)
          (nngmail--batch-parallel-current nil)
          (nngmail-batch-size 50)
          (nngmail-use-plz nil)
          (sizes nil)
          (rejected-once nil))
      ;; The first 50-part batch gets one part bounced for concurrency;
      ;; every later batch is clean.
      (cl-letf (((symbol-function 'nngmail--batch-one)
                 (lambda (_account chunk)
                   (push (length chunk) sizes)
                   (setq nngmail--batch-part-failures
                         (when (and (not rejected-once) (= (length chunk) 50))
                           (setq rejected-once t)
                           (list (list (car (car chunk)) 429 "rateLimitExceeded"
                                       "Too many concurrent requests for user"))))
                   (mapcar (lambda (r) (cons (car r) (and (not (member (car r) (mapcar #'car nngmail--batch-part-failures))) '((id . "x")))))
                           chunk)))
                ((symbol-function 'nngmail--countdown) (lambda (&rest _) nil)))
        (let ((reqs (cl-loop for i from 1 to 300 collect (list (format "m%d" i) "GET" "x"))))
          (dotimes (_ 6) (nngmail--batch a reqs "t")))
        (should (= 49 nngmail--batch-size-ceiling))
        ;; 50 was tried once; nothing larger than the ceiling afterwards.
        (should (= 1 (cl-count 50 sizes)))
        (should (<= (apply #'max (cdr (reverse sizes))) 49))
        (should (<= nngmail--batch-size-current 49))
        (nngmail-reset-limits)
        (should-not nngmail--batch-size-ceiling)))))


(ert-deftest nngmail-test-plz-wait-runs-callback-timer ()
  "The plz wait lets plz's zero-delay callback timer run after curl exits,
and gives up instead of spinning when curl is gone with nothing queued."
  (skip-unless (and (executable-find "true") (locate-library "plz")))
  (require 'plz)
  ;; A process that exits at once stands in for curl; deliver the result
  ;; the way plz does, from a timer scheduled by the sentinel.
  (let* ((delivered nil)
         (deliver (lambda (p &rest _)
                    (setq delivered t)
                    (process-put p :nngmail-result 'the-response)
                    (process-put p :nngmail-state 'done)))
         (proc (make-process :name "nngmail-test-plz" :command '("true")
                             :noquery t
                             :sentinel (lambda (p _)
                                         (run-at-time 0 nil deliver p)))))
    (process-put proc :nngmail-state 'pending)
    ;; The wait recognises plz's queued callback by function name; for
    ;; this stand-in, recognise ours by its process argument instead.
    (cl-letf (((symbol-function 'nngmail--plz-callback-timer)
               (lambda (p) (seq-find (lambda (tm) (eq (car (timer--args tm)) p)) timer-list))))
      (let ((t0 (float-time)))
        (should (eq 'the-response (nngmail--plz-wait proc)))
        (should delivered)
        (should (< (- (float-time) t0) 5)))))
  ;; Exited, no timer queued, no state: must return an error, not hang.
  (let ((proc (make-process :name "nngmail-test-plz2" :command '("true") :noquery t)))
    (process-put proc :nngmail-state 'pending)
    (while (process-live-p proc) (sleep-for 0.05))
    (let ((t0 (float-time)))
      (should (stringp (nngmail--plz-wait proc)))
      (should (eq 'error (process-get proc :nngmail-state)))
      (should (< (- (float-time) t0) 2)))))

(ert-deftest nngmail-test-plz-wait-exited-before-sentinel ()
  "curl exited but Emacs never ran plz's sentinel (debbugs#50166): the
wait must call it, as plz's own synchronous path does, and deliver.
This showed as bursts of \"curl exited without a response\" on a healthy
link."
  (skip-unless (and (executable-find "sh") (locate-library "plz")))
  (require 'plz)
  (let* ((delivered nil)
         (proc (make-process :name "nngmail-test-plz3" :command '("sh" "-c" "sleep 0.2")
                             :noquery t :sentinel #'ignore)))
    (process-put proc :nngmail-state 'pending)
    (process-put proc :plz-result :plz-result)
    ;; A wait on the specific process drains its output and returns
    ;; without running status_notify; the sentinel is installed only
    ;; now, so Emacs will not call it on its own.
    (while (process-live-p proc) (accept-process-output proc 0.05))
    (should (eq (process-status proc) 'exit))
    (cl-letf (((symbol-function 'plz--sentinel)
               (lambda (p status)
                 (should (equal status "workaround"))
                 (run-at-time 0 nil (lambda (p)
                                      (setq delivered t)
                                      (process-put p :nngmail-result 'the-response)
                                      (process-put p :nngmail-state 'done))
                              p)))
              ((symbol-function 'nngmail--plz-callback-timer)
               (lambda (p) (seq-find (lambda (tm) (eq (car (timer--args tm)) p)) timer-list))))
      (should-not delivered)
      (let ((t0 (float-time)))
        (should (eq 'the-response (nngmail--plz-wait proc)))
        (should delivered)
        (should (< (- (float-time) t0) 2))))))

(ert-deftest nngmail-test-lazy-headers-from-history ()
  "In `lazy' mode a new arrival is filed from its history record alone and
its headers are fetched when a summary asks for them; `eager' fetches at once."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (history '((id . "9")
                     (messagesAdded . (((message . ((id . "new1") (labelIds . ("INBOX" "UNREAD"))))))))))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (let ((nngmail-sync-headers 'lazy))
        (should-not (nngmail--apply-history a history)))
      (let ((num (gethash "new1" (nngmail-account-id->num a))))
        (should num)
        (should (equal '("INBOX" "UNREAD") (gethash num (nngmail-account-labels a))))
        (should (memq num (nngmail--group-numbers a "INBOX")))
        (should-not (nngmail--has-header-p a num))
        ;; A later label change on that article needs no fetch either.
        (should-not (nngmail--apply-history
                     a `((id . "10")
                         (labelsRemoved . (((message . ((id . "new1") (labelIds . ("INBOX"))))
                                            (labelIds . ("UNREAD"))))))))
        (should (equal '("INBOX") (gethash num (nngmail-account-labels a))))
        ;; retrieve-headers fetches exactly the missing one.
        (let ((fetched nil))
          (cl-letf (((symbol-function 'nngmail--fetch-metadata)
                     (lambda (_a ids &rest _)
                       (setq fetched ids)
                       (dolist (id ids)
                         (let ((n (gethash id (nngmail-account-id->num a))))
                           (nngmail-test--put-header a n (make-full-mail-header n "s" "f" "d" "<1@x>" "" 1 0 nil
                                                                                 '((X-Gmail-Labels . "INBOX")))))))))
            (let ((nntp-server-buffer (get-buffer-create " *nngmail-test-nov*")))
              (nngmail-retrieve-headers (list num) "INBOX" "test")
              (should (equal '("new1") fetched))
              (with-current-buffer nntp-server-buffer
                (should (string-match-p "^[0-9]+\ts\t" (buffer-string))))))))
      ;; Eager: the arrival is returned for fetching, not filed yet.
      (let ((nngmail-sync-headers 'eager))
        (should (equal '("new2")
                       (nngmail--apply-history
                        a '((id . "11")
                            (messagesAdded . (((message . ((id . "new2") (labelIds . ("INBOX")))))))))))
        (should-not (gethash "new2" (nngmail-account-id->num a)))))))


(ert-deftest nngmail-test-eta-and-bar-label ()
  "The ETA smooths a rate over samples, formats durations, and the bar
label drops fields from the right to fit the lozenge."
  ;; Durations.
  (should (equal "250ms" (nngmail--fmt-duration 0.25)))
  (should (equal "42s" (nngmail--fmt-duration 42)))
  (should (equal "3m10s" (nngmail--fmt-duration 190)))
  (should (equal "3m" (nngmail--fmt-duration 180)))
  (should (equal "2h5m" (nngmail--fmt-duration (+ (* 2 3600) 300))))
  (should (equal "1d3h" (nngmail--fmt-duration (+ 86400 (* 3 3600)))))
  ;; Rate tracking: 3 samples at 10 items/s -> ~10 items/s, ETA follows.
  (let ((nngmail--rate nil) (nngmail-eta-after 0) (now 1000.0))
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) now)))
      (should (equal "0ms so far" (nngmail--fmt-eta "p" 0 1000))) ; no rate yet
      (setq now 1001.0)
      (should (equal "~1m39s left" (nngmail--fmt-eta "p" 10 1000)))
      (setq now 1002.0)
      (should (equal "~1m38s left" (nngmail--fmt-eta "p" 20 1000)))
      ;; A stall halves the smoothed rate rather than zeroing it.
      (setq now 1004.0)
      (let ((eta (nngmail--fmt-eta "p" 20 1000)))
        (should (string-match-p "left" eta)))
      ;; Different phase resets; no total -> elapsed shown.
      (setq now 1010.0)
      (should (equal "0ms so far" (nngmail--fmt-eta "q" 5 nil)))
      (setq now 1013.0)
      (should (equal "3s so far" (nngmail--fmt-eta "q" 9 nil)))))
  ;; Labels.
  (let ((nngmail-status-bar-width 30))
    (should (equal "450/500  90%  ~12s left" (nngmail--bar-label 450 500 "~12s left")))
    (should (equal "1,234  3m so far" (nngmail--bar-label 1234 nil "3m so far"))))
  (should (equal "450/500 90%" (nngmail--fit-label "450/500 90%  ~12s left" 14)))
  (should (equal "204,077 scanned  ~10h left"
                 (nngmail--fit-label "204,077 scanned  ~10h left  33,505 new" 28)))
  ;; Text bar reserves room for the label after the blocks.
  (let ((nngmail-status-bar 'text) (nngmail-status-bar-width 20))
    (let ((bar (nngmail--status-bar 0.5 nil "5/10")))
      (should (= 20 (string-width bar)))
      (should (string-suffix-p " 5/10" bar)))))


(ert-deftest nngmail-test-imap-label-forms ()
  "X-GM-LABELS arrive as backslash atoms, as quoted strings with the
backslash escaped (Gmail's usual form), and as UTF-7 user labels."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "(\"\\\\Inbox\" \"\\\\Sent\" \\Starred \"work/project\" \"&AOk-t&AOk-\" \"\\\\Important\" \"a\\\"b\")")
    (goto-char (point-min))
    (should (equal '("INBOX" "SENT" "STARRED" "work/project" "\u00e9t\u00e9" "IMPORTANT" "a\"b")
                   (nngmail--imap-read-labels)))
    (should (eobp))))

(ert-deftest nngmail-test-imap-parse-fetch ()
  "Gmail FETCH responses with X-GM-* attributes parse into index records."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((hdr1 "Subject: =?UTF-8?Q?T=C3=ABst?=\r\nFrom: A <a@example.com>\r\nDate: Wed, 17 Sep 2026 10:00:00 -0400\r\nMessage-ID: <1@x>\r\nReferences: <0@x>\r\n <00@x>\r\n\r\n")
          (hdr2 "Subject: plain\r\nFrom: b@example.com\r\nMessage-ID: <2@x>\r\n\r\n"))
      (insert (format "* 1 FETCH (X-GM-THRID 1234567890123456789 X-GM-MSGID 1234567890123456789 X-GM-LABELS (\\Inbox \"dev/review\" \"&AOk-t&AOk-\") UID 10 RFC822.SIZE 4321 BODY[HEADER.FIELDS (Subject From Date Message-ID References In-Reply-To To Cc Newsgroups)] {%d}\r\n%s)\r\n"
                      (length hdr1) hdr1))
      (insert (format "* 2 FETCH (UID 11 X-GM-MSGID 255 X-GM-THRID 255 X-GM-LABELS (\\Sent \\Starred) RFC822.SIZE 10 BODY[HEADER.FIELDS (Subject)] {%d}\r\n%s)\r\n"
                      (length hdr2) hdr2))
      (insert "3 OK Success\r\n"))
    (let ((records (nngmail--imap-parse-fetch)))
      (should (= 2 (length records)))
      (pcase-let ((`(,id ,thrid ,labels ,size ,hdrs) (car records)))
        (should (equal (format "%x" 1234567890123456789) id))
        (should (equal id thrid))
        (should (equal '("INBOX" "dev/review" "\u00e9t\u00e9") labels))
        (should (= 4321 size))
        (should (equal "<1@x>" (nngmail--imap-header-value hdrs "Message-ID")))
        ;; Folded header unfolded.
        (should (equal "<0@x> <00@x>" (nngmail--imap-header-value hdrs "References"))))
      (pcase-let ((`(,id ,_ ,labels ,size ,_) (cadr records)))
        (should (equal "ff" id))
        (should (equal '("SENT" "STARRED") labels))
        (should (= 10 size))))))

(ert-deftest nngmail-test-imap-record-to-index ()
  "An IMAP record becomes a numbered, labelled, headered article; unknown
user labels are noted for a label refresh."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (seen (make-hash-table :test #'equal)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "dev/review")))
      (nngmail--rebuild-name-map a)
      (let ((num (nngmail--assign a "abc123")))
        (nngmail--imap-record->header
         a num (list "abc123" "abc123" '("INBOX" "dev/review" "brand-new") 99
                     '(("Subject" . "=?UTF-8?Q?T=C3=ABst?=") ("From" . "a@example.com")
                       ("Date" . "Wed, 17 Sep 2026 10:00:00 -0400") ("Message-ID" . "<1@x>")))
         seen)
        (let ((h (nngmail-test--header a num)))
          (should (equal "T\u00ebst" (mail-header-subject h)))
          (should (equal "<1@x>" (mail-header-id h)))
          (should (= 99 (mail-header-chars h)))
          (should (equal "INBOX,Label_1,brand-new"
                         (cdr (assq 'X-Gmail-Labels (mail-header-extra h))))))
        (should (equal '("INBOX" "Label_1" "brand-new") (gethash num (nngmail-account-labels a))))
        (should (memq num (nngmail--group-numbers a "INBOX")))
        (should (memq num (nngmail--group-numbers a "dev/review")))
        (should (gethash "brand-new" seen))))))


(ert-deftest nngmail-test-backfill-transport-dispatch ()
  "The transport is what the caller says; the default picks IMAP only for a
query-less run with IMAP usable; IMAP refuses a Gmail query."
  (nngmail-test-with-mock nil
    (nngmail-test--account)
    (let (calls (nngmail--imap-unavailable-reason nil))
      (cl-letf (((symbol-function 'nngmail--imap-import)
                 (lambda (&rest _) (push 'imap calls) 0))
                ((symbol-function 'nngmail--backfill)
                 (lambda (&rest _) (push 'rest calls) 0))
                ((symbol-function 'nngmail--message) #'ignore))
        (nngmail-backfill "test" nil nil 'none 'imap)
        (nngmail-backfill "test" 100 nil 'none 'rest)
        (nngmail-backfill "test" nil nil 'none)                 ; default -> imap
        (nngmail-backfill "test" nil "newer_than:2y" 'none)     ; default with query -> rest
        (should-error (nngmail-backfill "test" nil "from:x" 'none 'imap) :type 'user-error)
        (let ((nngmail--imap-unavailable-reason "off"))
          (nngmail-backfill "test" nil nil 'none))              ; default, imap unusable -> rest
        (should (equal '(imap rest imap rest rest) (reverse calls))))
      ;; The server prompt offers both transports per account, IMAP first
      ;; while usable, REST first once it is not.
      (let ((labels (mapcar #'car (nngmail--backfill-servers))))
        (should (= 2 (length labels)))
        (should (string-match-p "\\`test (IMAP" (car labels)))
        (should (string-match-p "\\`test (REST" (cadr labels))))
      (let* ((nngmail--imap-unavailable-reason "off")
             (labels (mapcar #'car (nngmail--backfill-servers))))
        (should (string-match-p "\\`test (REST" (car labels)))
        (should (string-match-p "unavailable" (cadr labels)))))))

(ert-deftest nngmail-test-mode-line-element-is-a-literal-string ()
  "The element in `global-mode-string' is a string object at the front,
replaced in place on every change, carrying the text and its properties;
an indicator an earlier version left in a Gnus `mode-name' is removed."
  (let ((global-mode-string (list "" 'display-time-string))
        (nngmail--mode-line-element (nngmail--mode-line-empty))
        (nngmail--mode-name-element (nngmail--mode-line-empty))
        (nngmail--spinner-string nil))
    (cl-letf (((symbol-function 'force-mode-line-update) #'ignore))
      ;; Gnus buffers carry the G-only element after the mode name; a
      ;; stale one from an earlier version is reused, not duplicated.
      (with-temp-buffer
        (setq major-mode 'gnus-group-mode mode-name "Group")
        (nngmail--spinner-install)
        (should (equal "Group" (car mode-name)))
        (should (eq nngmail--mode-name-element (cadr mode-name)))
        (should (= 2 (length mode-name)))
        (nngmail--spinner-install)
        (should (= 2 (length mode-name)))
        (nngmail--mode-name-show (propertize "G" 'face 'bold))
        (should (equal " G" (substring-no-properties (cadr mode-name))))
        (should (eq nngmail--mode-name-element (cadr mode-name)))
        (nngmail--mode-name-show nil)
        (should (get-text-property 0 'invisible (cadr mode-name))))
      (should (equal "" (nth 0 global-mode-string)))
      (should (eq nngmail--mode-line-element (nth 1 global-mode-string)))
      (should (eq 'display-time-string (nth 2 global-mode-string)))
      (should (= 3 (length global-mode-string)))
      (let ((nngmail-mode-line-position 'end))
        (nngmail--spinner-install)
        (should (= 3 (length global-mode-string)))
        (should (eq nngmail--mode-line-element (nth 2 global-mode-string))))
      (nngmail--spinner-install)
      (nngmail--spinner-show (concat " " (propertize "x" 'display '(space :width 2)) "a✓"))
      (should (= 3 (length global-mode-string)))
      (should (stringp (nth 1 global-mode-string)))
      (should (equal '(space :width 2) (get-text-property 1 'display (nth 1 global-mode-string))))
      (should (equal " xa✓" (substring-no-properties (nth 1 global-mode-string))))
      (nngmail--spinner-show nil)
      (should (get-text-property 0 'invisible (nth 1 global-mode-string)))
      (should (equal "" (car global-mode-string))))))

(ert-deftest nngmail-test-status-visuals ()
  "Bar labels fit their lozenge, gauges reflect state, the spinner runs
while progress is shown and stops when it is cleared."
  ;; Narrow bar: compact figures, fields dropped from the right.
  (let ((nngmail-status-bar-width 16))
    (should (equal "282.5k/2.50M  11%  ~10h left"
                   (nngmail--bar-label 282500 2500000 "~10h left"))))
  (should (equal "9,999" (nngmail--fmt-compact 9999)))
  (should (equal "10.0k" (nngmail--fmt-compact 10000)))
  (should (equal "2.00M" (nngmail--fmt-compact 2000000)))
  (should (equal "2.53M" (nngmail--fmt-compact 2531573)))
  (let ((nngmail-status-bar-width 30))
    (should (equal "4,500/5,000  90%  ~12s left" (nngmail--bar-label 4500 5000 "~12s left"))))
  ;; In the SVG the text is pinned with textLength so it cannot overflow.
  (cl-letf (((symbol-function 'frame-char-width) (lambda (&rest _) 9))
            ((symbol-function 'frame-char-height) (lambda (&rest _) 18))
            ((symbol-function 'nngmail--svg-p) (lambda () t))
            ((symbol-function 'svg-image) (lambda (svg &rest _) (list :data svg))))
    (let* ((nngmail-status-bar 'svg) (nngmail-status-bar-width 16)
           (bar (nngmail--status-bar 0.1 nil (nngmail--bar-label 282500 2500000 "~10h40m left")))
           (svg (plist-get (get-text-property 0 'display bar) :data))
           (text (car (dom-by-tag svg 'text)))
           (px (* 16 9)) (h 16))
      (should (<= (string-to-number (format "%s" (dom-attr text 'textLength))) (- px h)))
      (should (equal "spacingAndGlyphs" (dom-attr text 'lengthAdjust))))
    ;; Gauges: a lozenge `nngmail-gauge-width' wide with the figures inside.
    (clrhash nngmail--gauge-cache)
    (let* ((nngmail-gauge-width 12)
           (g (nngmail--gauge 'stack (/ 2.0 3) "2/3 conns" "tip" 3))
           (svg (plist-get (get-text-property 0 'display g) :data))
           (rects (dom-by-tag svg 'rect))
           (text (car (dom-by-tag svg 'text))))
      (should (equal (make-string 12 ?\s) g))
      (should (equal "108" (format "%s" (dom-attr svg 'width))))   ; 12 cells x 9 px
      ;; the track is a pill path (`nngmail--pill-body'); the 3 segments
      ;; are rects, two lit (not the "off" colour)
      (should (>= (length (dom-by-tag svg 'path)) 1))
      (should (= 3 (length rects)))
      (should (= 2 (cl-count-if (lambda (r) (equal (dom-attr r 'fill) "#58a6ff")) rects)))
      (should (equal "2/3 conns" (dom-text text)))
      (should (equal "tip" (get-text-property 0 'help-echo g))))
    ;; Level meter: full is green, low amber, empty has no fill bar.
    (let ((fills (lambda (f) (mapcar (lambda (r) (dom-attr r 'fill))
                                     (dom-by-tag (plist-get (get-text-property 0 'display (nngmail--gauge 'level f "q" "t")) :data) 'rect)))))
      (should (member "#3fb950" (funcall fills 1.0)))
      (should (member "#d29922" (funcall fills 0.2)))
      (should-not (seq-intersection '("#3fb950" "#d29922" "#f85149") (funcall fills 0.0))))
    ;; The composed gauges carry the figures in text mode too, and route
    ;; each transient state to the right gauge.
    (let ((nngmail-status-bar nil) (nngmail-batch-parallel 8) (nngmail--requests-in-flight nil)
          (nngmail--quota-log nil) (nngmail--status nil) (nngmail--throttled nil)
          (nngmail--quota-alert nil))
      (should (string-match-p "0/8 conns +quota 100%" (nngmail--gauges)))
      (let ((nngmail--status "net error, retry 4s"))
        (should (string-match-p "0/8 conns  net error, retry 4s +quota 100%" (nngmail--gauges))))
      (let ((nngmail--status "pacing 43s  1.3k/1.5k"))
        (should (string-match-p "0/8 conns +pacing 43s  1.3k/1.5k" (nngmail--gauges))))
      (let ((nngmail--throttled t))
        (should (string-match-p "quota 100% (throttled)" (nngmail--gauges))))
      ;; The alert text is what `nngmail--quota-ask' sets; the gauge keeps
      ;; its fixed width (`nngmail-gauge-width'), so the text must fit it.
      (let ((nngmail--quota-alert "18.2k/15.0k per min"))
        (should (string-match-p "18.2k/15.0k per min" (nngmail--gauges)))))
    (should (eq 'alert (nngmail--status-tone "net error, retry 4s")))
    (should (eq 'warn (nngmail--status-tone "waiting 12s")))
    (should-not (nngmail--status-tone nil))
    ;; Spinner frames differ and are cached.
    (let ((f0 (nngmail--spinner-svg 0)) (f3 (nngmail--spinner-svg 3)))
      (should-not (equal (dom-attr (car (dom-by-tag (plist-get (get-text-property 0 'display f0) :data) 'g)) 'transform)
                         (dom-attr (car (dom-by-tag (plist-get (get-text-property 0 'display f3) :data) 'g)) 'transform)))
      (should (eq f0 (nngmail--spinner-svg 12)))))
  ;; Spinner lifecycle follows progress (simulated interactive).
  (let ((noninteractive nil)
        (global-mode-string nil)
        (nngmail--spinner-timer nil)
        (nngmail--spinner-string nil)
        (nngmail--progress nil))
    (cl-letf (((symbol-function 'run-at-time) (lambda (&rest _) 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'force-mode-line-update) #'ignore)
              ((symbol-function 'nngmail--draw-status) #'ignore))
      (nngmail--progress (nngmail--make-account :name "t" :user "u") "indexing" 1 10)
      (should nngmail--spinner-timer)
      (nngmail--spinner-tick)
      (should (stringp nngmail--spinner-string))
      (should (string-match-p "nngmail" nngmail--spinner-string))
      ;; The string object held for mode names is the spinner text.
      (should (equal (substring-no-properties nngmail--mode-line-element)
                     (substring-no-properties nngmail--spinner-string)))
      (nngmail--progress-clear)
      (should-not nngmail--spinner-timer)
      (should-not nngmail--spinner-string))))


(ert-deftest nngmail-test-backfill-imap-fallback ()
  "When IMAP is unusable the backfill warns once, runs over REST, and skips
IMAP for the rest of the session until `nngmail-imap-retry'."
  (nngmail-test-with-mock nil
    (nngmail-test--account)
    (let ((nngmail--imap-unavailable-reason nil)
          calls warnings)
      (cl-letf (((symbol-function 'nngmail--imap-import)
                 (lambda (&rest _)
                   (push 'imap calls)
                   (signal 'nngmail-imap-unavailable
                           '("IMAP access is disabled for u (Gmail settings > Forwarding and POP/IMAP)"))))
                ((symbol-function 'nngmail--backfill)
                 (lambda (&rest _) (push 'rest calls) 7))
                ((symbol-function 'nngmail--imap-close) #'ignore)
                ((symbol-function 'display-warning)
                 (lambda (_type msg &rest _) (push msg warnings)))
                ((symbol-function 'nngmail--message) #'ignore))
        (nngmail-backfill "test" nil nil 'none 'imap)
        (should (equal '(imap rest) (reverse calls)))
        (should (= 1 (length warnings)))
        (should (string-match-p "IMAP access is disabled" (car warnings)))
        (should (string-match-p "Falling back to the REST API" (car warnings)))
        (should (string-match-p "no pacing" (car warnings)))
        (should nngmail--imap-unavailable-reason)
        ;; Second backfill asking for IMAP: straight to REST, no new warning.
        (nngmail-backfill "test" 100 nil 'none 'imap)
        (should (equal '(imap rest rest) (reverse calls)))
        (should (= 1 (length warnings)))
        ;; After retry, IMAP is attempted again.
        (nngmail-imap-retry)
        (should-not nngmail--imap-unavailable-reason)
        (nngmail-backfill "test" nil nil 'none 'imap)
        (should (equal '(imap rest rest imap rest) (reverse calls)))))))

(ert-deftest nngmail-test-imap-login-failure-advice ()
  "Gmail's IMAP rejections are explained in terms the user can act on."
  (should (string-match-p "Forwarding and POP/IMAP"
                          (nngmail--imap-explain-login-failure
                           "u@example.com" "2 NO [ALERT] IMAP access is disabled for your domain.")))
  (should (string-match-p "scope"
                          (nngmail--imap-explain-login-failure
                           "u@example.com" "2 NO [AUTHENTICATIONFAILED] Invalid credentials (Failure)")))
  (should (string-match-p "IMAP login failed for u@example.com: 2 BAD"
                          (nngmail--imap-explain-login-failure "u@example.com" "2 BAD huh\r\n"))))


(ert-deftest nngmail-test-offer-import-on-empty-index ()
  "A first open with an empty index offers an IMAP import; declining or an
unavailable IMAP falls through to the normal first sync."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (noninteractive nil)
          (nngmail-offer-import 'ask)
          (nngmail--imap-unavailable-reason nil)
          imported warned)
      (cl-letf (((symbol-function 'nngmail--imap-import) (lambda (&rest _) (setq imported t) 5))
                ((symbol-function 'nngmail--imap-close) #'ignore)
                ((symbol-function 'display-warning) (lambda (&rest a) (setq warned (cadr a))))
                ((symbol-function 'nngmail--message) #'ignore))
        ;; Decline.
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
          (should-not (nngmail--maybe-offer-import a))
          (should-not imported))
        ;; Accept.
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (should (nngmail--maybe-offer-import a))
          (should imported))
        ;; Non-empty index: no offer at all.
        (nngmail-test--seed a '((1 "x" ("INBOX"))))
        (setq imported nil)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (error "must not ask"))))
          (should-not (nngmail--maybe-offer-import a)))
        (should-not imported))
      ;; IMAP unavailable: warn, mark, continue.
      (let ((b (progn (nngmail--new-tables a) a)))
        (cl-letf (((symbol-function 'nngmail--imap-import)
                   (lambda (&rest _) (signal 'nngmail-imap-unavailable '("IMAP access is disabled"))))
                  ((symbol-function 'nngmail--imap-close) #'ignore)
                  ((symbol-function 'y-or-n-p) (lambda (_) t))
                  ((symbol-function 'display-warning) (lambda (&rest a) (setq warned (cadr a))))
                  ((symbol-function 'nngmail--message) #'ignore))
          (should-not (nngmail--maybe-offer-import b))
          (should (string-match-p "IMAP access is disabled" warned))
          (should (string-match-p "normal first sync" warned))
          (should nngmail--imap-unavailable-reason))))))


(ert-deftest nngmail-test-reload ()
  "`nngmail-reload' reloads the nngmail features, upgrades stale account
objects and clears the image caches."
  (require 'nngmail-reload)
  (nngmail-test-with-mock nil
    (let* ((a (nngmail-test--account))
           ;; A "stale" object: one slot short of the current struct.
           (short (let ((v (make-record 'nngmail-account (- (length a) 2) nil)))
                    (dotimes (i (1- (length v))) (aset v (1+ i) (aref a (1+ i))))
                    v))
           (loaded nil))
      (puthash "test" short nngmail--accounts)
      (setq nngmail--current short)
      (puthash 'probe 'stale nngmail--gauge-cache)
      ;; `load' is mocked, so the keymaps the reload unbinds would stay
      ;; unbound; keep them and check that they were unbound.
      (let* ((maps (nngmail-reload-keymap-variables))
             (saved (mapcar (lambda (s) (cons s (symbol-value s))) maps))
             (unbound-during nil))
        (unwind-protect
            (cl-letf (((symbol-function 'load)
                       (lambda (file &rest _)
                         (push (file-name-nondirectory file) loaded)
                         (unless (boundp 'nngmail-summary-label-map)
                           (setq unbound-during t))
                         t))
                      ((symbol-function 'nngmail--log) #'ignore)
                      ((symbol-function 'message) #'ignore)
                      ;; Skip the self-reload indirection.
                      (nngmail-reload--self-reloaded t))
              (should (>= (nngmail-reload) 1))
              (should (member "nngmail" loaded))
              (should (member "nngmail-tests" loaded))
              (should (memq 'nngmail-summary-label-map maps))
              (should unbound-during))
          (pcase-dolist (`(,s . ,v) saved) (set s v))))
      (let ((acct (gethash "test" nngmail--accounts)))
        (should (= (length acct) (length a)))
        (should (eq acct nngmail--current))
        (should (equal "test" (nngmail-account-name acct))))
      (should (= 0 (hash-table-count nngmail--gauge-cache))))))


(ert-deftest nngmail-test-refresh-token-survives-persist-failure ()
  "A plstore write failure after the access token was refreshed is logged once
and the refreshed token used; a failure before any refresh is re-signalled."
  (let* ((token (make-oauth2-token :access-token "old" :refresh-token "r"))
         (logged nil)
         (nngmail--token-persist-warned nil))
    (cl-letf (((symbol-function 'nngmail--log)
               (lambda (fmt &rest args) (push (apply #'format fmt args) logged)))
              ((symbol-function 'oauth2-refresh-access)
               (lambda (tok _host)
                 (setf (oauth2-token-access-token tok) (concat (oauth2-token-access-token tok) "+"))
                 (signal 'error (list "GPG error: Decryption failed")))))
      (should (eq token (nngmail--refresh-token token "gmail")))
      (should (equal "old+" (oauth2-token-access-token token)))
      (should (= 1 (length logged)))
      (should (string-search "token refreshed but not saved" (car logged)))
      (should (string-search "Decryption failed" (car logged)))
      ;; Second time on the same host: quiet.
      (nngmail--refresh-token token "gmail")
      (should (= 1 (length logged))))
    ;; Nothing refreshed: the error is the caller's problem.
    (cl-letf (((symbol-function 'oauth2-refresh-access)
               (lambda (_tok _host) (error "invalid_grant"))))
      (should-error (nngmail--refresh-token token "gmail")))))


(defmacro nngmail-test--with-symmetric-store (passphrase &rest body)
  "Run BODY with a fresh symmetric plstore as `oauth2-token-file'.
The plstore passphrase prompt answers PASSPHRASE (a variable that BODY
can set to simulate a mistyped answer).  Needs gpg; skipped without it."
  (declare (indent 1))
  `(progn
     (skip-unless (executable-find "gpg"))
     (require 'plstore)
     (let* ((home (make-temp-file "nngmail-gnupg" t))
            (process-environment (cons (concat "GNUPGHOME=" home) process-environment))
            (oauth2-token-file (expand-file-name "oauth2.plstore" home))
            (plstore-cache-passphrase-for-symmetric-encryption t)
            (plstore-passphrase-alist nil)
            (plstore-encrypt-to nil)
            (plstore-select-keys 'silent)
            (nngmail--token-store-good nil)
            (prompts 0))
       (set-file-modes home #o700)
       ;; Let gpg take the passphrase from Emacs instead of a pinentry.
       (with-temp-file (expand-file-name "gpg-agent.conf" home) (insert "allow-loopback-pinentry\nmax-cache-ttl 0\ndefault-cache-ttl 0\n"))
       (with-temp-file (expand-file-name "gpg.conf" home) (insert "pinentry-mode loopback\nno-symkey-cache\n"))
       (cl-letf (((symbol-function 'read-passwd)
                  ;; epg clears the string it is handed: hand it a copy.
                  (lambda (&rest _) (cl-incf prompts) (copy-sequence ,passphrase)))
                 ((symbol-function 'nngmail--log) #'ignore)
                 ((symbol-function 'message) #'ignore)
                 ((symbol-function 'sit-for) #'ignore))
         (unwind-protect
             (progn ,@body)
           (ignore-errors (call-process "gpgconf" nil nil nil "--kill" "gpg-agent"))
           (delete-directory home t))))))

(defun nngmail-test--write-store (entries)
  "Write ENTRIES ((NAME PUBLIC SECRET)...) to `oauth2-token-file' and close it."
  (let ((store (plstore-open oauth2-token-file)))
    (pcase-dolist (`(,name ,public ,secret) entries)
      (plstore-put store name public secret))
    (plstore-save store)
    (plstore-close store)))

(defun nngmail-test--read-secret (name key)
  "KEY of the entry NAME in `oauth2-token-file', decrypting."
  (let ((store (plstore-open oauth2-token-file)))
    (unwind-protect
        (plist-get (cdr (plstore-get store name)) key)
      (plstore-close store))))

(ert-deftest nngmail-test-token-refresh-due-p ()
  "The store is verified only when oauth2 is about to refresh: no cache,
or the cached access token past expires_in."
  (let* ((now (time-convert nil 'integer))
         (fresh (make-oauth2-token
                 :access-response '((expires_in . 3600))
                 :request-cache (list 'gmail (list :access-token "a" :request-timestamp (- now 100)))))
         (stale (make-oauth2-token
                 :access-response '((expires_in . 3600))
                 :request-cache (list 'gmail (list :access-token "a" :request-timestamp (- now 4000)))))
         (empty (make-oauth2-token)))
    (should-not (nngmail--token-refresh-due-p fresh "gmail"))
    (should (nngmail--token-refresh-due-p fresh "other-host"))
    (should (nngmail--token-refresh-due-p stale "gmail"))
    (should (nngmail--token-refresh-due-p empty "gmail"))))

(ert-deftest nngmail-test-token-store-verify-keeps-good-copy ()
  "Verifying a store that decrypts keeps its contents in memory."
  (let ((pass "right"))
    (nngmail-test--with-symmetric-store pass
      (nngmail-test--write-store '(("id1" (:host "gmail") (:access-token "tok" :refresh-token "ref"))))
      (should (equal "tok" (nngmail-test--read-secret "id1" :access-token)))
      (nngmail--verify-token-store)
      (should nngmail--token-store-good)
      (should (equal '("id1") (mapcar #'car (car nngmail--token-store-good))))
      (should (equal "ref" (plist-get (cdr (assoc "id1" (cdr nngmail--token-store-good))) :refresh-token))))))

(ert-deftest nngmail-test-token-store-verify-reprompts-on-bad-cache ()
  "A wrong cached passphrase is dropped and the prompt asked again; a
second failure is a `nngmail-error' naming the recovery."
  (let ((pass "right"))
    (nngmail-test--with-symmetric-store pass
      (nngmail-test--write-store '(("id1" nil (:access-token "tok"))))
      ;; Poison the cache the way a mistyped prompt would.
      (setq plstore-passphrase-alist (list (cons (file-truename oauth2-token-file) (copy-sequence "wrong"))))
      (setq prompts 0)
      (nngmail--verify-token-store)
      (should (= 1 prompts))
      (should nngmail--token-store-good)
      ;; Both the cache and the new answer wrong: error, cache cleared.
      (setq nngmail--token-store-good nil
            plstore-passphrase-alist (list (cons (file-truename oauth2-token-file) (copy-sequence "wrong")))
            pass "still wrong")
      (let ((err (should-error (nngmail--verify-token-store) :type 'nngmail-error)))
        (should (string-search "cannot be decrypted" (cadr err)))
        (should (string-search "nngmail-reauthorize" (cadr err))))
      (should-not plstore-passphrase-alist))))

(ert-deftest nngmail-test-token-store-restore-rewrites-poisoned-file ()
  "After the file is re-encrypted under another passphrase, the in-memory
copy restores it under the passphrase the user types."
  (let ((pass "right"))
    (nngmail-test--with-symmetric-store pass
      (nngmail-test--write-store '(("id1" (:host "gmail") (:access-token "tok" :refresh-token "ref"))))
      (nngmail--verify-token-store)
      ;; Poison, the way it happens: the store is decrypted in memory with
      ;; the right passphrase, then the cache holds a wrong one when a
      ;; refresh saves.
      (let ((store (plstore-open oauth2-token-file)))
        (plstore-put store "id1" '(:host "gmail") '(:access-token "tok2" :refresh-token "ref"))
        (setq plstore-passphrase-alist (list (cons (file-truename oauth2-token-file) (copy-sequence "oops"))))
        (plstore-save store)
        (plstore-close store))
      (setq plstore-passphrase-alist nil pass "right")
      (should-error (nngmail-test--read-secret "id1" :access-token))
      ;; Restore from memory, encrypting with "right".
      (setq plstore-passphrase-alist nil)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (nngmail-restore-token-store))
      (setq plstore-passphrase-alist nil)
      (should (equal "tok" (nngmail-test--read-secret "id1" :access-token)))
      (should (equal "gmail" (plist-get (cdr (assoc "id1" (car nngmail--token-store-good))) :host))))))

(ert-deftest nngmail-test-plstore-empty-passphrase-refused ()
  "An empty answer at the plstore prompt is asked again, not cached."
  (require 'plstore)
  (let* ((answers (list "" "" "good"))
         (oauth2-token-file "/nonexistent/oauth2.plstore")
         (plstore-passphrase-alist nil)
         (plstore-cache-passphrase-for-symmetric-encryption nil))
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (pop answers)))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'sit-for) #'ignore))
      (should (equal "good" (nngmail--plstore-refuse-empty-passphrase
                             #'plstore-passphrase-callback-function
                             nil nil (plstore--make (get-buffer-create " test-plstore")))))
      (should-not answers))))


(ert-deftest nngmail-test-reload-adopts-changed-defaults ()
  "An option still at its old default takes a new default after a reload;
one the user set keeps the user's value."
  (require 'nngmail-reload)
  (defcustom nngmail-test--opt-a '(1 2) "A." :type 'sexp)
  (defcustom nngmail-test--opt-b '(1 2) "B." :type 'sexp)
  (defcustom nngmail-test--opt-c 'same "C." :type 'sexp)
  (unwind-protect
      (progn
        (setq nngmail-test--opt-b '(9))        ; the user changed B
        (let ((before (nngmail-reload--option-standards)))
          (should (assq 'nngmail-test--opt-a before))
          ;; "Reload": the defaults in the source changed for A and B.
          (put 'nngmail-test--opt-a 'standard-value '('(1 2 3)))
          (put 'nngmail-test--opt-b 'standard-value '('(1 2 3)))
          (let ((updated (nngmail-reload--adopt-new-defaults before)))
            (should (equal '(nngmail-test--opt-a) updated))
            (should (equal '(1 2 3) nngmail-test--opt-a))
            (should (equal '(9) nngmail-test--opt-b))
            (should (eq 'same nngmail-test--opt-c)))))
    (dolist (s '(nngmail-test--opt-a nngmail-test--opt-b nngmail-test--opt-c))
      (makunbound s) (put s 'standard-value nil))))

(ert-deftest nngmail-test-build-info ()
  "`nngmail-build-info' names the version, the source dir, the newest mtime,
and the files edited after they were loaded (stale)."
  (require 'nngmail-reload)
  (let* ((dir (make-temp-file "nngmail-build" t))
         (fresh (expand-file-name "nngmail.el" dir))
         (edited (expand-file-name "nngmail-menu.el" dir))
         (now (current-time))
         (nngmail-reload--load-times nil))
    (unwind-protect
        (progn
          (with-temp-file fresh (insert ";; fresh\n"))
          (with-temp-file edited (insert ";; edited\n"))
          ;; nngmail loaded a minute after its file was written; the menu
          ;; file was written after it was loaded.
          (set-file-times fresh (time-subtract now 120))
          (set-file-times edited now)
          (setf (alist-get 'nngmail nngmail-reload--load-times) (time-subtract now 60)
                (alist-get 'nngmail-menu nngmail-reload--load-times) (time-subtract now 60))
          (cl-letf (((symbol-function 'nngmail-reload-features) (lambda () '(nngmail nngmail-menu)))
                    ((symbol-function 'nngmail-reload--library-file)
                     (lambda (f) (expand-file-name (symbol-name f) dir))))
            (let ((info (nngmail-build-info)))
              (message "build info: %s" info)
              (should (string-prefix-p (format "nngmail %s from %s" nngmail-version (abbreviate-file-name (file-name-as-directory dir))) info))
              (should (string-search (format-time-string "%F %T" now) info))
              (should (string-search "STALE in this Emacs" info))
              (should (string-search "nngmail-menu" info))
              (should-not (string-match-p "STALE[^:]*: nngmail," info)))
            ;; No file newer than its load: no stale note.
            (set-file-times edited (time-subtract now 120))
            (should-not (string-search "STALE" (nngmail-build-info)))))
      (delete-directory dir t))))


(ert-deftest nngmail-test-imap-stripes ()
  "The UID space splits into contiguous, non-overlapping stripes that cover it."
  (should (equal '((1 . 2500) (2501 . 5000) (5001 . 7500) (7501 . 9999))
                 (nngmail--imap-stripes 10000 4)))
  (should (equal '((1 . 9999)) (nngmail--imap-stripes 10000 1)))
  ;; Fewer UIDs than connections: one stripe per UID, no empties.
  (should (equal '((1 . 1) (2 . 2)) (nngmail--imap-stripes 3 4)))
  ;; Coverage and disjointness on an odd split.
  (let* ((stripes (nngmail--imap-stripes 1001 3))
         (covered (apply #'+ (mapcar (lambda (s) (1+ (- (cdr s) (car s)))) stripes))))
    (should (= 1000 covered))
    (should (cl-every (lambda (pair) (= (1+ (cdr (car pair))) (car (cadr pair))))
                      (seq-mapn #'list stripes (cdr stripes))))))


(ert-deftest nngmail-test-formatters-survive-infinities ()
  "Infinite or NaN figures (a zero rate) format as 0 instead of overflowing."
  (should (equal "0" (nngmail--fmt-num 1.0e+INF)))
  (should (equal "0" (nngmail--fmt-num 0.0e+NaN)))
  (should (equal "1,234" (nngmail--fmt-num 1234)))
  (should (equal "0ms" (nngmail--fmt-duration 1.0e+INF)))
  (let ((nngmail--rate (list "r" (- (float-time) 100) (- (float-time) 10) 5 0.0))
        (nngmail-eta-after 0))
    ;; Zero rate: no "left" figure, elapsed instead.
    (should (string-match-p "so far" (nngmail--fmt-eta "r" 5 100))))
  (let ((nngmail--rate (list "r" (- (float-time) 100) (- (float-time) 10) 5 1e-6))
        (nngmail-eta-after 0))
    (should (equal "~weeks left" (nngmail--fmt-eta "r" 5 100000000)))))


(ert-deftest nngmail-test-sqlite-required ()
  "Without SQLite, opening an account fails with a message that says so."
  (cl-letf (((symbol-function 'sqlite-available-p) (lambda () nil)))
    (let ((err (should-error (nngmail--require-sqlite) :type 'nngmail-error)))
      (should (string-match-p "sqlite-available-p" (cadr err)))
      (should (string-match-p "--with-sqlite3" (cadr err))))))

(ert-deftest nngmail-test-legacy-eld-migration ()
  "A pre-SQLite NAME.eld (and cached article files) are imported once and renamed."
  (nngmail-test-with-mock nil
    (let* ((eld (expand-file-name "test.eld" nngmail-directory))
           (artdir (expand-file-name "test/articles/0" nngmail-directory))
           (h (make-full-mail-header 3 "old subject" "f@x" "d" "<3@x>" "" 10 0 nil
                                     '((X-Gmail-Labels . "INBOX,UNREAD") (X-Gmail-Thread-Id . "t3")))))
      (make-directory artdir t)
      (with-temp-file eld
        (insert ";; nngmail state -*- mode: lisp-data; coding: utf-8-emacs -*-\n")
        (prin1 `(:version 1 :history-id "42" :next-article 5 :backfill nil
                 :label-names (("INBOX" . "INBOX") ("Label_1" . "x"))
                 :messages ((3 "id3" ("INBOX" "UNREAD") ,h)
                            (4 "id4" ("Label_1") nil)))
                (current-buffer)))
      (with-temp-file (expand-file-name "3" artdir)
        (set-buffer-multibyte nil)
        (insert "From: f@x\r\nSubject: old subject\r\n\r\nbody\r\n"))
      (let ((a (nngmail-test--account)))
        (should (equal "42" (nngmail-account-history-id a)))
        (should (= 5 (nngmail-account-next-article a)))
        (should (= 3 (gethash "id3" (nngmail-account-id->num a))))
        (should (equal '("Label_1") (gethash 4 (nngmail-account-labels a))))
        (should (equal "old subject" (mail-header-subject (nngmail-test--header a 3))))
        (should-not (nngmail--has-header-p a 4))
        (should (memq 3 (nngmail--group-numbers a "INBOX")))
        (should (memq 4 (nngmail--group-numbers a "x")))
        ;; The cached article came along and the files are gone.
        (should (string-match-p "old subject" (nngmail--db-get-article a 3)))
        (should-not (file-exists-p eld))
        (should (file-exists-p (concat eld ".migrated")))
        (should-not (file-directory-p (expand-file-name "test" nngmail-directory)))
        ;; A second load does not migrate again and sees the same state.
        (let ((b (nngmail--make-account :name "test" :user "u")))
          (should (nngmail--load b))
          (should (= 3 (gethash "id3" (nngmail-account-id->num b)))))))))

(ert-deftest nngmail-test-article-cache-in-db-with-eviction ()
  "Raw articles round-trip through the database byte for byte, and the
least recently read are evicted once the cache passes its bound."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (nngmail-article-cache-max 1)          ; 1 MB
          (blob (apply #'unibyte-string (number-sequence 0 255))))
      (nngmail-test--seed a '((1 "i1" ("INBOX")) (2 "i2" ("INBOX")) (3 "i3" ("INBOX"))))
      ;; Binary-exact round trip.
      (nngmail--db-put-article a 1 blob)
      (let ((back (nngmail--db-get-article a 1)))
        (should (equal back blob))
        (should-not (multibyte-string-p back)))
      ;; Three 400 KB articles exceed 1 MB: the oldest goes.
      (let ((big (make-string 400000 ?x)))
        (nngmail--db-put-article a 1 big)
        (sleep-for 0.01)
        (nngmail--db-put-article a 2 big)
        (sleep-for 0.01)
        (nngmail--db-get-article a 1)          ; touch 1: now 2 is the oldest
        (sleep-for 0.01)
        (nngmail--db-put-article a 3 big)
        (should (nngmail--db-get-article a 1))
        (should-not (nngmail--db-get-article a 2))
        (should (nngmail--db-get-article a 3))
        (should (<= (nngmail--article-cache-size a) (* 1024 1024))))
      ;; `nngmail--raw-message' serves from the cache without a request.
      (let ((calls-before (length nngmail-test--calls)))
        (should (nngmail--raw-message a 3))
        (should (= calls-before (length nngmail-test--calls))))
      ;; Forgetting an article drops its cached text too.
      (nngmail--forget a 3)
      (should-not (nngmail--db-get-article a 3)))))

(ert-deftest nngmail-test-header-cache-is-bounded ()
  "Headers live in the database; the in-memory cache stays under its size."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (nngmail-header-cache-size 40))
      (nngmail-test--seed a (cl-loop for i from 1 to 100 collect (list i (format "i%d" i) '("INBOX"))))
      (should (<= (hash-table-count (nngmail-account-headers a)) 40))
      ;; Every header is still retrievable, from the database.
      (clrhash (nngmail-account-headers a))
      (setf (nngmail-account-header-lru a) nil)
      (should (equal "<i7@x>" (mail-header-id (nngmail-test--header a 7))))
      (should (= 100 (length (nngmail--headers a (number-sequence 1 100)))))
      (should (<= (hash-table-count (nngmail-account-headers a)) 40))
      ;; A fresh load keeps memory light: no headers resident until asked.
      (let ((b (nngmail--make-account :name "test" :user "u")))
        (nngmail--load b)
        (should (= 0 (hash-table-count (nngmail-account-headers b))))
        (should (= 100 (hash-table-count (nngmail-account-id->num b))))))))

(ert-deftest nngmail-test-transaction-rollback-on-error ()
  "An error inside a transaction leaves the database as it was."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account)))
      (nngmail-test--seed a '((1 "i1" ("INBOX"))))
      (should-error
       (nngmail--with-transaction a
         (nngmail--db-put-labels a 1 '("SENT"))
         (error "boom")))
      (should (equal "INBOX" (car (car (sqlite-select (nngmail--db a) "SELECT labels FROM messages WHERE num=1"))))))))


(ert-deftest nngmail-test-configured-servers ()
  "Servers are found in select methods, in `gnus-server-alist' (added with ^)
and among open accounts, without duplicates."
  (let ((gnus-select-method '(nnnil ""))
        (gnus-secondary-select-methods '((nngmail "second" (nngmail-user "b@x"))
                                         (nnml "")))
        (gnus-server-alist '(("gmail" nngmail "gmail" (nngmail-user "a@x"))
                             ("archive" nnfolder "archive")))
        (nngmail--accounts (make-hash-table :test #'equal)))
    (puthash "second" t nngmail--accounts)
    (should (equal '("gmail" "second") (nngmail-configured-servers)))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "gmail")))
      (should (equal "gmail" (nngmail--read-server))))
    (let ((gnus-secondary-select-methods nil) (gnus-server-alist nil))
      (clrhash nngmail--accounts)
      (should-error (nngmail--read-server) :type 'user-error))))


(ert-deftest nngmail-test-check-newsrc-detects-old-numbering ()
  "Groups with read ranges above their active high, or unexist marks, are flagged."
  (let ((gnus-newsrc-alist (list (list "dummy" 3 nil nil nil)
                                 (list "nngmail+t:INBOX" 3 '((1 . 4) (2354170 . 2355587)) nil '(nngmail "t"))
                                 (list "nngmail+t:ok" 3 '((1 . 9)) nil '(nngmail "t"))
                                 (list "nngmail+t:unexist" 3 '((1 . 2)) '((unexist (5 . 9))) '(nngmail "t"))
                                 (list "nnml:other" 3 '((1 . 99999)) nil '(nnml ""))))
        cleared)
    ;; `gnus-active' is a macro over `gnus-active-hashtb'; give it a table.
    (setq gnus-active-hashtb (gnus-make-hashtable 10))
    (gnus-set-active "nngmail+t:INBOX" '(1 . 1259))
    (gnus-set-active "nngmail+t:ok" '(1 . 10))
    (gnus-set-active "nngmail+t:unexist" '(1 . 3))
    (cl-letf (((symbol-function 'gnus-group-clear-data) (lambda (g) (push g cleared)))
              ((symbol-function 'display-buffer) #'ignore)
              ((symbol-function 'y-or-n-p) (lambda (_) t))
              ((symbol-function 'message) #'ignore))
      (nngmail-check-newsrc)
      (should (equal '("nngmail+t:INBOX" "nngmail+t:unexist") (sort cleared #'string<))))))


(ert-deftest nngmail-test-send-shows-progress-while-waiting ()
  "A send shows a phase (so the spinner runs and the status line draws)
while the request is out, and clears it afterwards; a sync's own display
is not disturbed."
  (require 'message)
  (let ((seen nil))
    (nngmail-test-with-mock
        `(("POST messages/send" . ,(lambda (&rest _)
                                     (setq seen (copy-sequence nngmail--progress))
                                     '((id . "sent1")))))
      (nngmail-test--account)
      (setq nngmail--progress nil)         ; whatever an earlier test left
      (cl-letf (((symbol-function 'nngmail--server-for-sending) (lambda () "test")))
        (with-temp-buffer
          (insert "From: me@example.com\nTo: you@example.com\nSubject: hello there\n" mail-header-separator "\nbody\n")
          (should (nngmail-message-send-mail)))
        (should seen)
        (should (equal "test" (car seen)))
        (should (string-match-p "sending \"hello there\" to you@example.com" (cadr seen)))
        (should-not nngmail--progress)
        ;; Inside a sync: the sync's phase stays.
        (setq nngmail--progress (list "test" "applying changes" 3 nil nil) seen nil)
        (with-temp-buffer
          (insert "From: me@example.com\nTo: you@example.com\nSubject: x\n" mail-header-separator "\nbody\n")
          (should (nngmail-message-send-mail)))
        (should (equal "applying changes" (cadr seen)))
        (should (equal "applying changes" (cadr nngmail--progress)))
        (setq nngmail--progress nil)))))

(ert-deftest nngmail-test-send-strips-separator-and-bcc ()
  "`nngmail-message-send-mail' posts RFC 822 text: no Gnus header separator,
no Bcc, body intact."
  (require 'message)
  (nngmail-test-with-mock
      `(("POST messages/send" . ((id . "sent1"))))
    (nngmail-test--account)
    (cl-letf (((symbol-function 'nngmail--server-for-sending) (lambda () "test")))
      (with-temp-buffer
        (insert "From: me@example.com\nTo: you@example.com\nBcc: secret@example.com\nSubject: hi\n"
                mail-header-separator "\nbody line\n")
        (should (nngmail-message-send-mail)))
      (let* ((call (car nngmail-test--calls))
             (raw (base64-decode-string (alist-get 'raw (nth 3 call)) t)))
        (should (equal "messages/send" (nth 1 call)))
        (should-not (string-match-p (regexp-quote mail-header-separator) raw))
        (should-not (string-match-p "Bcc" raw))
        (should (string-match-p "^To: you@example.com$" raw))
        (should (string-match-p "\n\nbody line\n\\'" raw))))))


(ert-deftest nngmail-test-summary-label-commands ()
  "Edit/add/remove/archive send exactly the label difference to Gmail, work
on process-marked articles, and leave other labels alone."
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let ((a (nngmail-test--account))
          (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil))
      (setf (nngmail-account-label-names a)
            '(("INBOX" . "INBOX") ("Label_1" . "work") ("Label_2" . "clock") ("UNREAD" . "UNREAD")
              ("SENT" . "[Gmail]/Sent Mail") ("TRASH" . "[Gmail]/Trash")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "a" ("INBOX" "Label_1" "UNREAD")) (2 "b" ("INBOX")) (3 "c" ("Label_2"))))
      (setq gnus-newsrc-alist (list (list "nngmail+test:INBOX" 3 nil nil (list 'nngmail "test"))))
      (gnus-make-hashtable-from-newsrc-alist)
      ;; Editable labels: no UNREAD, no SENT; sorted by name.
      (should (equal '("INBOX" "[Gmail]/Trash" "clock" "work")
                     (mapcar #'car (nngmail--editable-labels a))))
      (with-temp-buffer
        (gnus-summary-mode)
        (setq gnus-newsgroup-name "nngmail+test:INBOX")
        (cl-letf (((symbol-function 'gnus-summary-work-articles)
                   (lambda (&rest _) (list (nngmail--num->local a "INBOX" 1))))
                  ((symbol-function 'nngmail--refresh-summary-lines) #'ignore))
          ;; Edit: keep INBOX, drop work, add clock.
          (cl-letf (((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("INBOX" "clock"))))
            (nngmail-summary-edit-labels))
          (let ((body (nth 3 (car nngmail-test--calls))))
            (should (equal ["a"] (alist-get 'ids body)))
            (should (equal ["Label_2"] (alist-get 'addLabelIds body)))
            (should (equal ["Label_1"] (alist-get 'removeLabelIds body))))
          (should (equal '("Label_2" "INBOX" "UNREAD") (gethash 1 (nngmail-account-labels a))))
          ;; State labels survive an edit untouched.
          (should (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
          ;; Archive: INBOX only.
          (nngmail-summary-archive)
          (let ((body (nth 3 (car nngmail-test--calls))))
            (should (equal ["INBOX"] (alist-get 'removeLabelIds body)))
            (should-not (alist-get 'addLabelIds body)))
          (should-not (member "INBOX" (gethash 1 (nngmail-account-labels a))))
          (should (memq 1 (nngmail--group-numbers a "clock")))
          (should-not (memq 1 (nngmail--group-numbers a "INBOX")))
          ;; Article 1 has left INBOX: from this summary it is a retired
          ;; number, so the commands refuse rather than act on nothing.
          (should-error (nngmail-summary-archive) :type 'user-error))
        ;; Continue from the clock group, where article 1 still lives.
        (setq gnus-newsgroup-name "nngmail+test:clock")
        (setq gnus-newsrc-alist (list (list "nngmail+test:clock" 3 nil nil (list 'nngmail "test"))))
        (gnus-make-hashtable-from-newsrc-alist)
        (cl-letf (((symbol-function 'gnus-summary-work-articles)
                   (lambda (&rest _) (list (nngmail--num->local a "clock" 1))))
                  ((symbol-function 'nngmail--refresh-summary-lines) #'ignore))
          ;; Archive on a message not in INBOX: nothing to do, no request.
          (let ((n (length nngmail-test--calls)))
            (nngmail-summary-archive)
            (should (= n (length nngmail-test--calls))))
          ;; Add / remove single labels.
          (nngmail-summary-add-label "Label_1")
          (should (member "Label_1" (gethash 1 (nngmail-account-labels a))))
          (nngmail-summary-remove-label "Label_1")
          (should-not (member "Label_1" (gethash 1 (nngmail-account-labels a)))))
        ;; Process marks in INBOX: two articles, one request.
        (setq gnus-newsgroup-name "nngmail+test:INBOX")
        (setq gnus-newsrc-alist (list (list "nngmail+test:INBOX" 3 nil nil (list 'nngmail "test"))))
        (gnus-make-hashtable-from-newsrc-alist)
        (nngmail--set-labels-1 a 3 '("Label_2" "INBOX"))     ; put 3 into INBOX too
        (cl-letf (((symbol-function 'gnus-summary-work-articles)
                   (lambda (&rest _) (list (nngmail--num->local a "INBOX" 2)
                                           (nngmail--num->local a "INBOX" 3))))
                  ((symbol-function 'nngmail--refresh-summary-lines) #'ignore))
          (let ((n (length nngmail-test--calls)))
            (nngmail-summary-add-label "Label_1")
            (should (= (1+ n) (length nngmail-test--calls)))
            (should (equal ["b" "c"] (alist-get 'ids (nth 3 (car nngmail-test--calls)))))
            (should (member "Label_1" (gethash 2 (nngmail-account-labels a))))
            (should (member "Label_1" (gethash 3 (nngmail-account-labels a))))))))))


(ert-deftest nngmail-test-article-bytes-survive-label-header ()
  "The X-Gmail-Labels header added to a raw article does not turn the
buffer multibyte: a UTF-8 body decodes correctly afterwards, also when a
label name is non-ASCII."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account))
          (raw (encode-coding-string "From: a@x\r\nSubject: =?UTF-8?Q?T=C3=ABst?=\r\n\r\nKörper ✓ 日本\r\n" 'utf-8)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "été project")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "i1" ("INBOX" "Label_1"))))
      (nngmail--db-put-article a 1 raw)
      ;; Gnus passes its *Article* buffer, which is multibyte.  The bytes
      ;; must land there as raw bytes so `mm-decode-body' can decode
      ;; them, as it does for nnimap.
      (with-temp-buffer
        (should enable-multibyte-characters)
        (should (nngmail-request-article 1 "INBOX" "test" (current-buffer)))
        (should enable-multibyte-characters)
        (should (string-match-p "^X-Gmail-Labels: " (buffer-string)))
        (goto-char (point-min))
        (search-forward "\n\n")
        (decode-coding-region (point) (point-max) 'utf-8)
        (should (string-match-p "Körper ✓ 日本" (buffer-string))))
      ;; A unibyte caller (the move path) gets the bytes unchanged.
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (should (nngmail-request-article 1 "INBOX" "test" (current-buffer)))
        (should-not enable-multibyte-characters)
        (should (string-match-p "Körper ✓ 日本"
                                (decode-coding-string (buffer-string) 'utf-8)))
        ;; The label header itself is ASCII (RFC 2047 for the accented name).
        (goto-char (point-min))
        (re-search-forward "^X-Gmail-Labels: \\(.*\\)$")
        (should (string-match-p "\\`[[:ascii:]]*\\'" (match-string 1)))
        (should (equal "INBOX, été project" (rfc2047-decode-string (match-string 1))))))))


(ert-deftest nngmail-test-header-labels-follow-label-changes ()
  "After a label change the header's X-Gmail-Labels (what the summary
column shows) reflects it, from the cache and from the database."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "work")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "i1" ("INBOX"))))
      (nngmail--set-labels a 1 '("INBOX" "Label_1"))
      (should (equal "INBOX,Label_1"
                     (cdr (assq 'X-Gmail-Labels (mail-header-extra (nngmail--header a 1))))))
      ;; Drop the cache: the database copy agrees too.
      (clrhash (nngmail-account-headers a))
      (setf (nngmail-account-header-lru a) nil)
      (should (equal "INBOX,Label_1"
                     (cdr (assq 'X-Gmail-Labels (mail-header-extra (nngmail--header a 1))))))
      ;; And the summary column function sees the new label.
      (let ((gnus-newsrc-alist (list (list "nngmail+test:INBOX" 3 nil nil (list 'nngmail "test"))))
            (gnus-newsrc-hashtb nil))
        (gnus-make-hashtable-from-newsrc-alist)
        (let ((gnus-newsgroup-name "nngmail+test:INBOX"))
          (should (equal "work" (gnus-user-format-function-gmail-labels (nngmail--header a 1)))))))))


(ert-deftest nngmail-test-send-reply-carries-thread-id ()
  "A reply to an indexed message is sent with Gmail's threadId, so Gmail
files it in that thread; a fresh message is sent without one."
  (require 'message)
  (nngmail-test-with-mock
      `(("POST messages/send" . ((id . "sent1"))))
    (let ((a (nngmail-test--account)))
      (puthash "i1" 1 (nngmail-account-id->num a))
      (puthash 1 "i1" (nngmail-account-num->id a))
      (nngmail--store-header a 1 (make-full-mail-header 1 "s" "f" "d" "<orig@x>" "" 1 0 nil
                                                        '((X-Gmail-Thread-Id . "thr9")))
                             '("INBOX"))
      (cl-letf (((symbol-function 'nngmail--server-for-sending) (lambda () "test")))
        (with-temp-buffer
          (insert "From: me@example.com\nTo: you@example.com\nSubject: Re: s\n"
                  "In-Reply-To: <orig@x>\nReferences: <orig@x>\n"
                  mail-header-separator "\nreply\n")
          (should (nngmail-message-send-mail)))
        (should (equal "thr9" (alist-get 'threadId (nth 3 (car nngmail-test--calls)))))
        ;; Started from the article itself: `message-reply-headers' wins,
        ;; no lookup needed.
        (with-temp-buffer
          (setq-local message-reply-headers
                      (make-full-mail-header 1 "s" "f" "d" "<orig@x>" "" 1 0 nil
                                             '((X-Gmail-Thread-Id . "thr7"))))
          (insert "From: me@example.com\nTo: you@example.com\nSubject: Re: s\n"
                  mail-header-separator "\nreply\n")
          (should (nngmail-message-send-mail)))
        (should (equal "thr7" (alist-get 'threadId (nth 3 (car nngmail-test--calls)))))
        (with-temp-buffer
          (insert "From: me@example.com\nTo: you@example.com\nSubject: new\n"
                  mail-header-separator "\nhello\n")
          (should (nngmail-message-send-mail)))
        (should-not (assq 'threadId (nth 3 (car nngmail-test--calls))))))))

(ert-deftest nngmail-test-fetch-labels-reports-changes ()
  "`nngmail--fetch-labels' returns renamed and deleted labels as
\(ID OLD . NEW); new labels and unchanged ones are not reported."
  (nngmail-test-with-mock
      `(("GET labels" . ((labels . (((id . "INBOX") (name . "INBOX") (type . "system"))
                                    ((id . "Label_1") (name . "renamed") (type . "user"))
                                    ((id . "Label_3") (name . "new") (type . "user")))))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a)
            '(("INBOX" . "INBOX") ("Label_1" . "old") ("Label_2" . "gone")))
      (let ((changed (nngmail--fetch-labels a)))
        (should (equal '(("Label_1" "old" . "renamed") ("Label_2" "gone"))
                       (sort changed (lambda (x y) (string< (car x) (car y))))))
        (should (equal "new" (nngmail--label-group a "Label_3")))
        ;; Unchanged list: nothing to do.
        (should-not (nngmail--fetch-labels a))))))

(ert-deftest nngmail-test-rename-group-keeps-local-numbers ()
  "Renaming a label moves the group's rows: local numbers, and so Gnus's
read ranges, survive; the old group is gone; a full rebuild afterwards
changes nothing."
  (nngmail-test-with-mock
      `(("PATCH labels/Label_1" . ((id . "Label_1")))
        ("GET labels" . ((labels . (((id . "INBOX") (name . "INBOX") (type . "system"))
                                    ((id . "Label_1") (name . "after") (type . "user")))))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "before")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "i1" ("Label_1")) (2 "i2" ("INBOX" "Label_1")) (3 "i3" ("Label_1"))))
      ;; Retire local 2 so the counter matters.
      (nngmail--set-labels a 2 '("INBOX"))
      (should (equal '(1 3) (nngmail--group-locals a "before")))
      (should (= 4 (nngmail-group-next (nngmail--group a "before"))))
      (should (nngmail-request-rename-group "before" "after" "test"))
      (should-not (nngmail--group a "before"))
      (should (equal '(1 3) (nngmail--group-locals a "after")))
      (should (= 4 (nngmail-group-next (nngmail--group a "after"))))
      (should (= 1 (nngmail--local->num a "after" 1)))
      (should (= 3 (nngmail--local->num a "after" 3)))
      ;; Persisted: reload from the database gives the same picture.
      (nngmail--load-groups a)
      (should (equal '(1 3) (nngmail--group-locals a "after")))
      (should (= 4 (nngmail-group-next (nngmail--group a "after"))))
      (should-not (nngmail--group a "before"))
      ;; A full rebuild agrees (no drift introduced).
      (nngmail--rebuild-groups a)
      (should (equal '(1 3) (nngmail--group-locals a "after")))
      (should (= 4 (nngmail-group-next (nngmail--group a "after")))))))

(ert-deftest nngmail-test-relabel-groups-from-sync ()
  "A rename or deletion done outside Emacs is applied when the label list
is refreshed during a sync; nothing else is walked."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account)) walked)
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "x") ("Label_2" . "y")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "i1" ("Label_1")) (2 "i2" ("Label_2")) (3 "i3" ("INBOX"))))
      ;; Simulate the refreshed list: Label_1 renamed to z, Label_2 deleted.
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "z")))
      (nngmail--rebuild-name-map a)
      (cl-letf (((symbol-function 'nngmail--rebuild-groups)
                 (lambda (&rest _) (setq walked t))))
        (nngmail--relabel-groups a '(("Label_1" "x" . "z") ("Label_2" "y"))))
      (should-not walked)
      (should (equal '(1) (nngmail--group-locals a "z")))
      (should-not (nngmail--group a "x"))
      (should (= 0 (nngmail--group-size a "y")))
      (should (equal '() (gethash 2 (nngmail-account-labels a))))
      (should (= 1 (nngmail--group-size a "INBOX")))
      ;; Rename onto a name that exists: falls back to the full pass.
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (cl-letf (((symbol-function 'nngmail--rebuild-groups)
                 (lambda (&rest _) (setq walked t))))
        (nngmail--relabel-groups a '(("Label_1" "z" . "INBOX"))))
      (should walked))))

(ert-deftest nngmail-test-delete-group-strips-label ()
  "Deleting a label removes it from every message locally; the messages
stay in their other groups and the label's group is gone."
  (nngmail-test-with-mock
      `(("DELETE labels/Label_1" . ((ok . t)))
        ("GET labels" . ((labels . (((id . "INBOX") (name . "INBOX") (type . "system"))
                                    ((id . "Label_2") (name . "keep") (type . "user")))))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "gone") ("Label_2" . "keep")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "i1" ("INBOX" "Label_1")) (2 "i2" ("Label_1" "Label_2"))))
      (should (= 2 (nngmail--group-size a "gone")))
      (should (nngmail-request-delete-group "gone" nil "test"))
      (should (equal '("INBOX") (gethash 1 (nngmail-account-labels a))))
      (should (equal '("Label_2") (gethash 2 (nngmail-account-labels a))))
      (should (= 0 (nngmail--group-size a "gone")))
      (should (= 1 (nngmail--group-size a "keep")))
      (should (= 1 (nngmail--group-size a "INBOX")))
      ;; Stored rows agree with memory.
      (should (equal '("Label_2")
                     (split-string (car (car (sqlite-select (nngmail--db a)
                                                            "SELECT labels FROM messages WHERE num = 2")))
                                   "," t))))))

(ert-deftest nngmail-test-newest-locals-skip-retired ()
  "A numeric prefix selects the N newest existing articles; retired local
numbers (messages that left the group) are skipped, so N show."
  (nngmail-test-with-mock nil
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "i1" ("INBOX")) (2 "i2" ("INBOX")) (3 "i3" ("INBOX"))
                              (4 "i4" ("INBOX")) (5 "i5" ("INBOX"))))
      ;; Locals 1..5.  Archive 4 and 5 (they leave INBOX; 4,5 retired).
      (nngmail--set-labels a 4 '())
      (nngmail--set-labels a 5 '())
      (cl-letf (((symbol-function 'gnus-find-method-for-group) (lambda (_) '(nngmail "test"))))
        (should (equal '(2 3) (nngmail--newest-locals "nngmail+test:INBOX" 2)))
        (should (equal '(1 2 3) (nngmail--newest-locals "nngmail+test:INBOX" 100)))
        (should (equal '(1 2) (nngmail--newest-locals "nngmail+test:INBOX" -2)))
        ;; The advice hands Gnus that list as SELECT-ARTICLES.
        (let (seen)
          (nngmail--select-newsgroup-advice (lambda (_g _r sel) (setq seen sel))
                                            "nngmail+test:INBOX" 2)
          (should (equal '(2 3) seen))
          ;; Plain RET: Gnus chooses.
          (nngmail--select-newsgroup-advice (lambda (_g _r sel) (setq seen sel))
                                            "nngmail+test:INBOX" nil)
          (should-not seen))))))

;;;; The menu (nngmail-menu.el)

(require 'nngmail-menu)

(ert-deftest nngmail-test-menu-loads-and-binds ()
  "nngmail-menu defines the transient and, in an nngmail summary, binds it
under L L and adds the menu-bar entry; elsewhere it stays out of the way."
  (require 'nngmail-menu)
  (should (fboundp 'nngmail-menu))
  (should (get 'nngmail-menu 'transient--prefix))
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (cl-letf (((symbol-function 'nngmail-menu--in-summary-p) (lambda () t)))
      (nngmail-menu--install)
      (should (keymapp (lookup-key (current-local-map) [menu-bar nngmail])))))
  (should (eq 'nngmail-menu (lookup-key nngmail-summary-label-map "L")))
  (with-temp-buffer
    (use-local-map (make-sparse-keymap))
    (cl-letf (((symbol-function 'nngmail-menu--in-summary-p) (lambda () nil)))
      (nngmail-menu--install)
      (should-not (keymapp (lookup-key (current-local-map) [menu-bar nngmail]))))))


(ert-deftest nngmail-test-testdrive-look-restores-unread ()
  "Looking at a real article in the drive prefers a read one, and gives
UNREAD/STARRED back to an unread one it had to show."
  (require 'nngmail-testdrive)
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let ((a (nngmail-test--account)))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "i1" ("INBOX" "UNREAD" "STARRED")) (2 "i2" ("INBOX")) (3 "i3" ("INBOX" "UNREAD"))))
      (let ((nngmail-testdrive--account a)
            (nngmail-testdrive--own (make-hash-table :test #'eql))
            (nngmail-testdrive--restore-marks nil))
        ;; A read article exists: it is the pick.
        (should (= 2 (nngmail-testdrive--pick-real "INBOX")))
        ;; With a predicate only unread ones satisfy, an unread one is returned.
        (should (memq (nngmail-testdrive--pick-real "INBOX" (lambda (_n l) (member "STARRED" l))) '(1)))
        ;; Showing article 1 (unread, starred): remember its state; Gnus
        ;; then reads it (simulated); leaving restores both labels.
        (cl-letf (((symbol-function 'gnus-summary-read-group) #'ignore)
                  ((symbol-function 'gnus-summary-goto-article) #'ignore)
                  ((symbol-function 'gnus-summary-select-article)
                   (lambda (&rest _) (nngmail--set-labels a 1 '("INBOX"))))
                  ((symbol-function 'sit-for) #'ignore)
                  ((symbol-function 'nngmail-testdrive--group) (lambda (g) (concat "nngmail+test:" g))))
          (nngmail-testdrive--show 1 "INBOX")
          (should (equal '((1 "UNREAD" "STARRED")) nngmail-testdrive--restore-marks))
          (should-not (member "UNREAD" (gethash 1 (nngmail-account-labels a)))))
        (nngmail-testdrive--restore-looked-at)
        (should-not nngmail-testdrive--restore-marks)
        (let ((call (car nngmail-test--calls)))
          (should (equal "messages/batchModify" (nth 1 call)))
          (should (equal ["i1"] (alist-get 'ids (nth 3 call))))
          (should (equal '("UNREAD" "STARRED")
                         (sort (append (alist-get 'addLabelIds (nth 3 call)) nil) #'string>))))
        (should (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
        (should (member "STARRED" (gethash 1 (nngmail-account-labels a))))
        ;; A read article shown: nothing to restore, no request.
        (let ((n (length nngmail-test--calls)))
          (cl-letf (((symbol-function 'gnus-summary-read-group) #'ignore)
                    ((symbol-function 'gnus-summary-goto-article) #'ignore)
                    ((symbol-function 'gnus-summary-select-article) #'ignore)
                    ((symbol-function 'sit-for) #'ignore)
                    ((symbol-function 'nngmail-testdrive--group) (lambda (g) (concat "nngmail+test:" g))))
            (nngmail-testdrive--show 2 "INBOX"))
          (nngmail-testdrive--restore-looked-at)
          (should (= n (length nngmail-test--calls))))))))


(defmacro nngmail-test--with-open-summary (group &rest body)
  "Run BODY in a buffer that counts as an open, prepared summary of GROUP."
  (declare (indent 1))
  `(with-temp-buffer
     (gnus-summary-mode)
     (setq gnus-newsgroup-name ,group
           gnus-newsgroup-prepared t)
     (let ((gnus-summary-buffer (current-buffer)))
       ,@body)))

(ert-deftest nngmail-test-fast-quit-reverts-eager-marks ()
  "`Q' (exit without update) undoes the reads pushed while the summary was
open, drops anything still queued, and forgets the journal; `q' keeps
them.  Toggling an article twice restores its state at entry."
  (let ((nngmail-mark-propagation 'immediate))
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let ((a (nngmail-test--account))
          (nngmail--pending-marks nil) (nngmail--pending-marks-timer nil)
          (nngmail--mark-journal nil)
          (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil)
          (g "nngmail+test:INBOX"))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "a" ("INBOX" "UNREAD")) (2 "b" ("INBOX" "UNREAD")) (3 "c" ("INBOX"))))
      (setq gnus-newsrc-alist (list (list g 3 nil nil (list 'nngmail "test"))))
      (gnus-make-hashtable-from-newsrc-alist)
      (cl-letf (((symbol-function 'run-with-idle-timer) (lambda (&rest _) (timer-create)))
                ((symbol-function 'gnus-check-group) (lambda (&rest _) t)))
        (nngmail-test--with-open-summary g
          (progn
            ;; Read 1 and 2 (pushed), mark 3 unread (pushed), then 2 back to unread (pushed).
            (nngmail--queue-mark g 1 'read) (nngmail--queue-mark g 2 'read) (nngmail--queue-mark g 3 'unread)
            (nngmail-flush-marks)
            (nngmail--queue-mark g 2 'unread)
            (nngmail-flush-marks)
            (should-not (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
            (should (member "UNREAD" (gethash 2 (nngmail-account-labels a))))
            (should (member "UNREAD" (gethash 3 (nngmail-account-labels a))))
            ;; Journal holds the state at entry, first record wins.
            (should (equal '((1 . t) (2 . t) (3 . nil))
                           (sort (copy-sequence (cdr (assoc g nngmail--mark-journal)))
                                 (lambda (x y) (< (car x) (car y))))))
            ;; One more read queued but not yet flushed when Q arrives.
            (nngmail--queue-mark g 2 'read)
            (let ((gnus-group-is-exiting-p t)
                  (gnus-group-is-exiting-without-update-p t)
                  (before (length nngmail-test--calls)))
              (nngmail--summary-exit)
              ;; The queued read was dropped, not sent; the revert went out.
              (should (member "UNREAD" (gethash 2 (nngmail-account-labels a))))
              ;; Everything is back as at entry.
              (should (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
              (should (member "UNREAD" (gethash 2 (nngmail-account-labels a))))
              (should-not (member "UNREAD" (gethash 3 (nngmail-account-labels a))))
              (should-not (assoc g nngmail--mark-journal))
              (should-not nngmail--pending-marks)
              ;; Exactly the revert requests were made (one add, one remove).
              (should (= 2 (- (length nngmail-test--calls) before))))))
        ;; q: the journal is forgotten and nothing is reverted.
        (nngmail-test--with-open-summary g
          (progn
            (nngmail--queue-mark g 1 'read)
            (nngmail-flush-marks)
            (should (assoc g nngmail--mark-journal))
            (let ((gnus-group-is-exiting-p t)
                  (gnus-group-is-exiting-without-update-p nil)
                  (before (length nngmail-test--calls)))
              (nngmail--summary-exit)
              (should-not (assoc g nngmail--mark-journal))
              (should-not (member "UNREAD" (gethash 1 (nngmail-account-labels a))))
              (should (= before (length nngmail-test--calls))))))
        ;; No summary open (a catch-up from *Group*): nothing is journaled.
        (let ((gnus-summary-buffer nil))
          (nngmail--queue-mark g 2 'read)
          (nngmail-flush-marks)
          (should-not (assoc g nngmail--mark-journal))))))))

(ert-deftest nngmail-test-info-frozen-while-summary-open ()
  "While a summary for a group is open its info is not rebuilt from labels;
once it is closed (or exiting) it is."
  (nngmail-test-with-mock nil
    (let* ((a (nngmail-test--account))
           (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil)
           (g "nngmail+test:INBOX")
           (info (list g 3 '((1 . 1)) nil (list 'nngmail "test"))))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "a" ("INBOX" "UNREAD")) (2 "b" ("INBOX" "UNREAD"))))
      (setq gnus-newsrc-alist (list info))
      (gnus-make-hashtable-from-newsrc-alist)
      (cl-letf (((symbol-function 'gnus-check-group) (lambda (&rest _) t)))
        ;; No summary: the info follows the labels (both unread -> read range empty).
        (nngmail--update-info a "INBOX" info)
        (should (null (gnus-info-read info)))
        ;; Label changes at Gmail, summary open: info frozen.
        (nngmail--set-labels a 1 '("INBOX"))
        (nngmail-test--with-open-summary g
          (should (nngmail--summary-open-p g))
          (nngmail--update-info a "INBOX" info)
          (should (null (gnus-info-read info)))
          ;; Exiting counts as closed.
          (let ((gnus-group-is-exiting-p t))
            (should-not (nngmail--summary-open-p g))))
        ;; Closed: follows the labels again.
        (nngmail--update-info a "INBOX" info)
        (should (equal '(1) (range-uncompress (gnus-info-read info))))))))

(ert-deftest nngmail-test-on-exit-propagation ()
  "With `nngmail-mark-propagation' `on-exit': nothing is pushed while
reading; a sync during the summary does not lose the read; `q' pushes
the diff; `Q' pushes nothing; an article that arrived during the
session stays unread."
  (nngmail-test-with-mock
      `(("POST messages/batchModify" . ((ok . t))))
    (let* ((a (nngmail-test--account))
           (nngmail-mark-propagation 'on-exit)
           (gnus-active-hashtb (gnus-make-hashtable 10))
           (gnus-newsrc-alist nil) (gnus-newsrc-hashtb nil)
           (gnus-undo-mode nil) (gnus-use-cache nil)
           (g "nngmail+test:INBOX")
           (info (list g 3 nil nil (list 'nngmail "test")))
           (nngmail--pending-marks nil) (nngmail--pending-marks-timer nil)
           (nngmail--mark-journal nil))
      (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX")))
      (nngmail--rebuild-name-map a)
      (nngmail-test--seed a '((1 "a" ("INBOX" "UNREAD")) (2 "b" ("INBOX" "UNREAD")) (3 "c" ("INBOX" "UNREAD"))))
      (nnheader-init-server-buffer)
      (get-buffer-create "*Group*")
      (setq gnus-newsrc-alist (list info))
      (gnus-make-hashtable-from-newsrc-alist)
      (cl-letf (((symbol-function 'gnus-check-group) (lambda (&rest _) t))
                ((symbol-function 'run-with-idle-timer) (lambda (&rest _) (timer-create)))
                ((symbol-function 'gnus-summary-update-mark) #'ignore)
                ((symbol-function 'gnus-request-update-mark) (lambda (_g _a m) m)))
        ;; Enter: info from labels, all three unread.
        (nngmail-request-group "INBOX" "test" t (gnus-get-info g))
        (should (null (gnus-info-read (gnus-get-info g))))
        (nngmail-test--with-open-summary g
          (setq gnus-newsgroup-active (gnus-active g)
                gnus-newsgroup-unreads '(1 2 3) gnus-newsgroup-marked nil
                gnus-newsgroup-spam-marked nil gnus-newsgroup-dormant nil
                gnus-newsgroup-reads nil gnus-newsgroup-expirable nil
                gnus-newsgroup-unselected nil gnus-newsgroup-articles '(1 2 3)
                gnus-newsgroup-auto-expire nil)
          ;; Read article 2: nothing goes to Gmail yet.
          (gnus-mark-article-as-read 2 gnus-read-mark)
          (should-not nngmail--pending-marks)
          (should (member "UNREAD" (gethash 2 (nngmail-account-labels a))))
          ;; A sync during the session: a 4th message arrives, and Gnus
          ;; asks for the info again.  The info must stay frozen.
          (nngmail--set-labels a (nngmail--assign a "d") '("INBOX" "UNREAD"))
          (nngmail-finish-retrieve-group-infos "test" (list (gnus-get-info g)) t)
          (should (null (gnus-info-read (gnus-get-info g))))
          ;; q: the exit diff sends exactly the read of article 2.
          (let ((gnus-group-is-exiting-p t)
                (before (length nngmail-test--calls)))
            (run-hooks 'gnus-summary-prepare-exit-hook)
            (gnus-update-read-articles g '(1 3))
            (let ((mods (seq-filter (lambda (c) (equal (nth 1 c) "messages/batchModify"))
                                    (nthcdr 0 (seq-take nngmail-test--calls
                                                        (- (length nngmail-test--calls) before))))))
              (should (= 1 (length mods)))
              (should (equal ["b"] (alist-get 'ids (nth 3 (car mods)))))))
          (should-not (member "UNREAD" (gethash 2 (nngmail-account-labels a))))
          ;; The arrival (4) was never in the summary and stays unread.
          (should (member "UNREAD" (gethash 4 (nngmail-account-labels a)))))
        ;; Second session: read 1, then Q.  Nothing is sent.
        (nngmail--update-info a "INBOX" (gnus-get-info g))
        (nngmail-test--with-open-summary g
          (setq gnus-newsgroup-active (gnus-active g)
                gnus-newsgroup-unreads '(1 3 4) gnus-newsgroup-marked nil
                gnus-newsgroup-spam-marked nil gnus-newsgroup-dormant nil
                gnus-newsgroup-reads nil gnus-newsgroup-expirable nil
                gnus-newsgroup-unselected nil gnus-newsgroup-articles '(1 3 4)
                gnus-newsgroup-auto-expire nil)
          (gnus-mark-article-as-read 1 gnus-read-mark)
          (let ((gnus-group-is-exiting-p t)
                (gnus-group-is-exiting-without-update-p t)
                (before (length nngmail-test--calls)))
            (run-hooks 'gnus-summary-prepare-exit-hook)
            ;; Q does not call gnus-update-read-articles at all.
            (should (= before (length nngmail-test--calls)))))
        (should (member "UNREAD" (gethash 1 (nngmail-account-labels a))))))))

;;;; Google Docs (nngmail-gdocs.el)

(require 'nngmail-gdocs)

(ert-deftest nngmail-test-gdocs-doc-id ()
  "Docs URLs in their common shapes yield the id; sheets, slides and junk do not."
  (should (equal "1AbC_dEf-123" (nngmail-gdocs-doc-id "https://docs.google.com/document/d/1AbC_dEf-123/edit?usp=sharing")))
  (should (equal "1AbC_dEf-123" (nngmail-gdocs-doc-id "https://docs.google.com/document/u/0/d/1AbC_dEf-123/edit")))
  (should (equal "1AbC_dEf-123xxxxxxxxxxxx" (nngmail-gdocs-doc-id "1AbC_dEf-123xxxxxxxxxxxx")))
  (should-not (nngmail-gdocs-doc-id "https://docs.google.com/spreadsheets/d/1AbC/edit"))
  (should-not (nngmail-gdocs-doc-id "https://example.com/x"))
  (should-not (nngmail-gdocs-doc-id "short"))
  (should-not (nngmail-gdocs-doc-id nil)))

(defmacro nngmail-test--with-drive (responses &rest body)
  "Run BODY with an account and Drive RESPONSES mocked; clears the doc cache."
  (declare (indent 1))
  `(nngmail-test-with-mock ,responses
     (cl-letf (((symbol-function 'nngmail-configured-servers) (lambda () '("test"))))
       (nngmail-test--account)
       (clrhash nngmail-gdocs--cache)
       ,@body)))

(ert-deftest nngmail-test-gdocs-fetch-caches-by-modified-time ()
  "A doc is exported once per modifiedTime; a changed doc is fetched again;
long docs are cut with a marker."
  (let ((exports 0) (modified "2026-09-20T10:00:00Z"))
    (nngmail-test--with-drive
        `(("GET https://www.googleapis.com/drive/v3/files/DOCID12345678901234567890/export"
           . ,(lambda (&rest _) (cl-incf exports) (encode-coding-string "# Recap\n\nWe met. Körper ✓." 'utf-8)))
          ("GET https://www.googleapis.com/drive/v3/files/DOCID12345678901234567890$"
           . ,(lambda (&rest _) `((name . "Weekly recap") (modifiedTime . ,modified) (mimeType . "application/vnd.google-apps.document")))))
      (pcase-let ((`(,title . ,text) (nngmail-gdocs-fetch "https://docs.google.com/document/d/DOCID12345678901234567890/edit")))
        (should (equal "Weekly recap" title))
        (should (equal "# Recap\n\nWe met. Körper ✓." text)))
      (nngmail-gdocs-fetch "DOCID12345678901234567890")
      (should (= 1 exports))
      (setq modified "2026-09-21T10:00:00Z")
      (nngmail-gdocs-fetch "DOCID12345678901234567890")
      (should (= 2 exports))
      (let ((nngmail-gdocs-max-chars 10))
        (clrhash nngmail-gdocs--cache)
        (should (string-match-p "more characters of the document not shown"
                                (cdr (nngmail-gdocs-fetch "DOCID12345678901234567890"))))))))

(ert-deftest nngmail-test-gdocs-export-falls-back-when-too-large ()
  "Drive's 10 MB export cap: HTML is refused, Markdown is tried, then text;
the washer gets text marked as such; a doc refused in every format says so."
  (let ((asked nil))
    (nngmail-test--with-drive
        `(("GET https://www.googleapis.com/drive/v3/files/BIGDOC1234567890123456/export"
           . ,(lambda (_m _p params &rest _)
                (let ((mime (cdr (assoc "mimeType" params))))
                  (push mime asked)
                  (if (equal mime "text/plain")
                      (encode-coding-string "The text of the big doc." 'utf-8)
                    (signal 'nngmail-http-error (list 403 "This file is too large to be exported." "u"))))))
          ("GET https://www.googleapis.com/drive/v3/files/BIGDOC1234567890123456$"
           . ,(lambda (&rest _) '((name . "Big") (modifiedTime . "2026-09-21T10:00:00Z") (mimeType . "application/vnd.google-apps.document"))))
          ("GET https://www.googleapis.com/drive/v3/files/HUGEDOC123456789012345/export"
           . ,(lambda (&rest _) (signal 'nngmail-http-error (list 403 "This file is too large to be exported." "u"))))
          ("GET https://www.googleapis.com/drive/v3/files/HUGEDOC123456789012345$"
           . ,(lambda (&rest _) '((name . "Huge") (modifiedTime . "2026-09-21T10:00:00Z") (mimeType . "application/vnd.google-apps.document")))))
      (pcase-let ((`(,title . ,text) (nngmail-gdocs-fetch "BIGDOC1234567890123456" "text/html")))
        (should (equal "Big" title))
        (should (equal "The text of the big doc." (substring-no-properties text)))
        (should (equal "text/plain" (get-text-property 0 'nngmail-gdocs-mime text)))
        (should (equal '("text/html" "text/markdown" "text/plain") (reverse asked))))
      ;; The washer inserts it as text, not through shr.
      (with-temp-buffer
        (nngmail-gdocs--insert-doc "BIGDOC1234567890123456")
        (should (string-match-p "Google Doc: Big\nThe text of the big doc\." (buffer-string))))
      (let ((err (should-error (nngmail-gdocs-fetch "HUGEDOC123456789012345") :type 'user-error)))
        (should (string-match-p "any text format" (cadr err)))))))

(ert-deftest nngmail-test-gdocs-errors-are-specific ()
  "A 403 for a missing scope names the scope and the fix; a 404 says not
found; the expander turns either into a note rather than an error."
  (nngmail-test--with-drive
      `(("GET https://www.googleapis.com/drive/v3/files/NOSCOPE1234567890123456$"
         . ,(lambda (&rest _) (signal 'nngmail-http-error
                                      (list 403 "Request had insufficient authentication scopes." "u"))))
        ("GET https://www.googleapis.com/drive/v3/files/GONE123456789012345678$"
         . ,(lambda (&rest _) nil)))
    (let ((err (should-error (nngmail-gdocs-fetch "NOSCOPE1234567890123456") :type 'user-error)))
      (should (string-match-p "drive.readonly" (cadr err)))
      (should (string-match-p "nngmail-reauthorize" (cadr err))))
    (let ((err (should-error (nngmail-gdocs-fetch "GONE123456789012345678") :type 'user-error)))
      (should (string-match-p "not found" (cadr err))))
    (should (string-match-p "\\[Google Doc not fetched: .*drive.readonly"
                            (nngmail-gdocs-expand-link "https://docs.google.com/document/d/NOSCOPE1234567890123456/edit")))
    (should-not (nngmail-gdocs-expand-link "https://example.com/not-a-doc"))
    (should (string-match-p "not found" (nngmail-gdocs--tool-fetch '(("url" . "GONE123456789012345678")))))))

(ert-deftest nngmail-test-gdocs-links-in-buffer-and-tool ()
  "Links are found from shr's `shr-url' property and from plain text,
one per document id however many times and in whatever form it is
linked; the tool returns title, source and text."
  (nngmail-test--with-drive
      `(("GET https://www.googleapis.com/drive/v3/files/DOCA123456789012345678/export" . "Body A")
        ("GET https://www.googleapis.com/drive/v3/files/DOCA123456789012345678$"
         . ((name . "Doc A") (modifiedTime . "t1"))))
    (with-temp-buffer
      ;; An shr-rendered link whose visible text is not the URL, a plain
      ;; URL to the same doc, and a plain URL to another doc.
      (insert (propertize "Open" 'shr-url "https://docs.google.com/document/d/DOCA123456789012345678/edit?usp=sharing_eil"))
      (insert "\nNotes: https://docs.google.com/document/d/DOCA123456789012345678/edit?usp=x\n"
              "and https://docs.google.com/document/d/DOCB123456789012345678/view\n")
      (should (equal '("DOCA123456789012345678" "DOCB123456789012345678")
                     (mapcar #'cdr (nngmail-gdocs--doc-links-in-buffer)))))
    (let ((out (nngmail-gdocs--tool-fetch '(("url" . "https://docs.google.com/document/d/DOCA123456789012345678/edit")))))
      (should (string-match-p "^Title: Doc A$" out))
      (should (string-match-p "Body A" out)))
    (should (eq 'nngmail-gdocs-show (lookup-key nngmail-summary-label-map "D")))))

(ert-deftest nngmail-test-gdocs-washing-treatment ()
  "`gnus-article-show-gdocs' appends each linked doc once, rendered from
its HTML export; it is registered as a treatment after buttonizing and
on `W D'; a second run adds nothing."
  (skip-unless (fboundp 'libxml-parse-html-region))
  (nngmail-test--with-drive
      `(("GET https://www.googleapis.com/drive/v3/files/DOCA123456789012345678/export"
         . ,(lambda (_m _p params &rest _)
              (should (equal "text/html" (cdr (assoc "mimeType" params))))
              "<html><body><h1>Recap</h1><ul><li>decided X</li></ul></body></html>"))
        ("GET https://www.googleapis.com/drive/v3/files/DOCA123456789012345678$"
         . ((name . "Doc A") (modifiedTime . "t1"))))
    (should (assq 'gnus-treat-gdocs gnus-treatment-function-alist))
    (let ((alist gnus-treatment-function-alist))
      (should (< (cl-position 'gnus-treat-buttonize alist :key #'car)
                 (cl-position 'gnus-treat-gdocs alist :key #'car))))
    (should (eq 'gnus-article-show-gdocs (lookup-key gnus-summary-wash-map "D")))
    (with-temp-buffer
      (insert "From: a@x\n\nSee https://docs.google.com/document/d/DOCA123456789012345678/edit\n")
      (let ((gnus-article-buffer (current-buffer)))
        (gnus-article-show-gdocs)
        (should (string-match-p "^Google Doc: Doc A$" (buffer-string)))
        (should (string-match-p "Recap" (buffer-string)))
        (should (string-match-p "decided X" (buffer-string)))
        (let ((len (buffer-size)))
          (gnus-article-show-gdocs)
          (should (= len (buffer-size))))))))

(ert-deftest nngmail-test-gdocs-washing-without-libxml ()
  "Without libxml the treatment falls back to the Markdown export as text."
  (nngmail-test--with-drive
      `(("GET https://www.googleapis.com/drive/v3/files/DOCA123456789012345678/export" . "# Recap\n\n- decided X")
        ("GET https://www.googleapis.com/drive/v3/files/DOCA123456789012345678$"
         . ((name . "Doc A") (modifiedTime . "t1"))))
    (cl-letf (((symbol-function 'fboundp)
               (let ((orig (symbol-function 'fboundp)))
                 (lambda (s) (and (not (eq s 'libxml-parse-html-region)) (funcall orig s))))))
      (with-temp-buffer
        (insert "From: a@x\n\nSee https://docs.google.com/document/d/DOCA123456789012345678/edit\n")
        (let ((gnus-article-buffer (current-buffer)))
          (gnus-article-show-gdocs)
          (should (string-match-p "^Google Doc: Doc A\n# Recap" (buffer-string))))))))
