;;; nngmail-efrit-tests.el --- Tests for nngmail-efrit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Loaded by nngmail-tests.el; efrit itself is not needed, its
;; functions are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nngmail-efrit)
;; The mock API and fixtures (`nngmail-test-with-mock', `nngmail-test--msg',
;; `nngmail-test--batch-handler') come from nngmail-tests.el, which
;; loads this file at its end.

(defun nngmail-test--efrit-raw (subject body)
  "A raw RFC 822 message with SUBJECT and text BODY."
  (concat "From: Ann <ann@example.com>\r\nTo: me@example.com\r\nDate: Mon, 01 Sep 2026 10:00:00 +0000\r\n"
          "Subject: " subject "\r\nMessage-ID: <" subject "@x>\r\nMIME-Version: 1.0\r\n"
          "Content-Type: text/plain; charset=utf-8\r\n\r\n" body "\r\n"))

(ert-deftest nngmail-test-efrit-tools ()
  "The Gmail tools search and list labels from alist input, and report
efrit-gnus references alongside Gmail ids."
  (let ((msgs `(("s1" . ,(nngmail-test--msg "s1" '("INBOX") "Subject" "found" "From" "f" "Message-ID" "<s1@x>")))))
    (nngmail-test-with-mock
        `(("GET messages$" . ((messages . (((id . "s1")))))))
      (cl-letf (((symbol-function 'nngmail--batch) (nngmail-test--batch-handler msgs))
                ((symbol-function 'nngmail-configured-servers) (lambda () '("test")))
                ((symbol-function 'efrit-gnus--ref) (lambda (g n) (format "%s#%d" g n))))
        (let ((a (nngmail-test--account)))
          (setf (nngmail-account-label-names a) '(("INBOX" . "INBOX") ("Label_1" . "work")))
          (nngmail--rebuild-name-map a)
          (let ((listing (nngmail-efrit--tool-search '(("query" . "found")))))
            (should (string-match-p "1 message for \"found\"" listing))
            (should (string-match-p "- nngmail\\+test:INBOX#1 | gmail-id=s1 | .* | f | found" listing)))
          (should (equal '(("nngmail+test:INBOX" . 1)) (nngmail-efrit--refs a '(1))))
          (let ((labels (nngmail-efrit--tool-labels nil)))
            (should (string-match-p "- INBOX: 1" labels))
            (should (string-match-p "- work: 0" labels))))))))

(ert-deftest nngmail-test-efrit-tools-register ()
  "The three Gmail tools register with efrit as read-only nngmail tools,
after efrit-gnus's own."
  (let (registered ensured)
    (cl-letf (((symbol-function 'nngmail-efrit--require-efrit-gnus) (lambda () t))
              ((symbol-function 'efrit-gnus-ensure-tools) (lambda () (setq ensured t)))
              ((symbol-function 'efrit-register-tool)
               (lambda (name &rest keys) (push (cons name (plist-get keys :class)) registered))))
      (nngmail-efrit-ensure-tools)
      (should ensured)
      (should (equal '(("gmail_labels" . read) ("gmail_search" . read) ("gmail_thread" . read))
                     (sort registered (lambda (x y) (string< (car x) (car y)))))))))

(provide 'nngmail-efrit-tests)
;;; nngmail-efrit-tests.el ends here
