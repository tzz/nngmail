;;; nngmail-efrit.el --- Gmail-specific efrit tools for nngmail -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Maintainer: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mail, gnus, tools, ai
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

;; The analysis commands ("ask efrit about these messages") live in
;; efrit itself, in efrit-gnus.el, and work for every Gnus backend.
;; This file adds what only nngmail can offer on top:
;;
;;  - the `efrit-gnus-map' commands under nngmail's `L A' prefix, next
;;    to the label commands;
;;  - three Gmail-specific tools for the model: `gmail_search' (Gmail's
;;    own query syntax, over the whole account rather than one group),
;;    `gmail_thread' (a Gmail conversation, which spans labels) and
;;    `gmail_labels' (the label list with indexed counts).  The generic
;;    `gnus_search' and `gnus_articles' from efrit-gnus keep working on
;;    nngmail groups too; these are the shortcuts that know Gmail.
;;    Results carry both the Gmail id and the GROUP#NUMBER reference
;;    efrit-gnus uses, so the model can pass either on.
;;
;; Everything here is read-only.  Label editing stays with the `L'
;; commands.
;;
;; Part of the nngmail package.  Needs efrit 0.13 or later
;; (efrit-gnus.el, efrit-tool-registry.el and
;; `efrit-agent-header-extra-functions'), which is not a package
;; dependency: without efrit this file loads and does nothing until
;; `nngmail-efrit-ensure-tools' is called, which then says what is
;; missing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nngmail)

(declare-function efrit-register-tool "efrit-tool-registry")
(declare-function efrit-unregister-package-tools "efrit-tool-registry")
(declare-function efrit-gnus-ensure-tools "efrit-gnus")
(declare-function efrit-gnus--require "efrit-gnus")
(declare-function efrit-gnus--payload "efrit-gnus")
(declare-function efrit-gnus--ref "efrit-gnus")
(declare-function efrit-reload "efrit-reload")
(declare-function nngmail-reload "nngmail-reload")
(defvar efrit-gnus-map)
(defvar efrit-gnus-tool-max-articles)

(defun nngmail-efrit--require-efrit-gnus ()
  "Load efrit-gnus (and efrit before it), with a clear error when absent."
  (unless (featurep 'efrit)
    (unless (locate-library "efrit")
      (user-error "Nngmail-efrit: efrit is not on your load-path"))
    (require 'efrit))
  (unless (locate-library "efrit-gnus")
    (user-error "Nngmail-efrit: this efrit lacks efrit-gnus.el (efrit 0.13 or later)"))
  (require 'efrit-gnus))

;;;; Gmail-specific tools

(defun nngmail-efrit--tool-account (input)
  "The account a tool INPUT names (\"account\" key), else the first configured one."
  (let ((name (or (alist-get "account" input nil nil #'equal)
                  (car (nngmail-configured-servers)))))
    (unless name (error "No nngmail account configured"))
    (nngmail--account name)))

(defun nngmail-efrit--home-group (account num)
  "A group to refer to server article NUM of ACCOUNT by: a label before All Mail.
Every message is in All Mail; a label (INBOX, a user label) is the more
telling name and the group the user is likelier to have subscribed."
  (let ((groups (nngmail--message-groups account (gethash num (nngmail-account-labels account)))))
    (or (seq-find (lambda (g) (not (equal g nngmail-all-mail-group))) groups)
        (car groups))))

(defun nngmail-efrit--ref (account num)
  "The efrit-gnus reference (GROUP#NUMBER) of server article NUM of ACCOUNT."
  (when-let* ((group (nngmail-efrit--home-group account num))
              (local (nngmail--num->local account group num)))
    (efrit-gnus--ref (gnus-group-full-name group (list 'nngmail (nngmail-account-name account)))
                     local)))

(defun nngmail-efrit--header-line (account num)
  "One listing line for article NUM of ACCOUNT.
Reference, Gmail id, date, from, subject."
  (let ((h (nngmail--header account num))
        (id (gethash num (nngmail-account-num->id account))))
    (if h
        (format "- %s | gmail-id=%s | %s | %s | %s"
                (or (nngmail-efrit--ref account num) "?") id
                (mail-header-date h) (mail-header-from h) (mail-header-subject h))
      (format "- %s | gmail-id=%s | (headers not fetched)" (or (nngmail-efrit--ref account num) "?") id))))

(defun nngmail-efrit--refs (account nums)
  "The (GROUP . NUMBER) pairs efrit-gnus uses for server articles NUMS of ACCOUNT.
In order, skipping unfiled ones."
  (delq nil (mapcar (lambda (num)
                      (when-let* ((group (nngmail-efrit--home-group account num))
                                  (local (nngmail--num->local account group num)))
                        (cons (gnus-group-full-name group (list 'nngmail (nngmail-account-name account)))
                              local)))
                    nums)))

(defun nngmail-efrit--tool-search (input)
  "The gmail_search tool: messages matching the Gmail query in INPUT, newest first."
  (let* ((account (nngmail-efrit--tool-account input))
         (query (alist-get "query" input nil nil #'equal))
         (limit (min 100 (or (alist-get "limit" input nil nil #'equal) 20)))
         (nums (nngmail--search account query limit)))
    (if (null nums)
        (format "No messages match %S." query)
      (concat (format "%d message%s for %S (newest first). Use gnus_articles with the references for the text.\n"
                      (length nums) (if (= 1 (length nums)) "" "s") query)
              (mapconcat (lambda (num) (nngmail-efrit--header-line account num)) nums "\n")))))

(defun nngmail-efrit--thread-nums (account thread-id)
  "Server numbers of every message in THREAD-ID of ACCOUNT.
Messages not yet indexed are fetched first."
  (let* ((thread (nngmail--get account (concat "threads/" thread-id) '(("format" . "minimal"))))
         (ids (mapcar (lambda (m) (alist-get 'id m)) (alist-get 'messages thread))))
    (sort (nngmail--fetch-metadata account ids) #'<)))

(defun nngmail-efrit--tool-thread (input)
  "The gmail_thread tool: the conversation holding the id or reference in INPUT."
  (let* ((account (nngmail-efrit--tool-account input))
         (key (alist-get "id" input nil nil #'equal))
         (num (or (gethash key (nngmail-account-id->num account))
                  (when-let* ((ref (and (stringp key) (string-match "\\`\\(.+\\)#\\([0-9]+\\)\\'" key)
                                        (cons (match-string 1 key) (string-to-number (match-string 2 key))))))
                    (nngmail--local->num account (gnus-group-real-name (car ref)) (cdr ref)))
                  (car (nngmail--fetch-metadata account (list key)))))
         (h (and num (nngmail--header account num)))
         (tid (and h (cdr (assq 'X-Gmail-Thread-Id (mail-header-extra h))))))
    (if (or (null tid) (string-empty-p tid))
        (format "No thread found for %s." key)
      (car (efrit-gnus--payload (nngmail-efrit--refs account (nngmail-efrit--thread-nums account tid)))))))

(defun nngmail-efrit--tool-labels (input)
  "The gmail_labels tool: the labels of the account in INPUT with indexed counts."
  (let ((account (nngmail-efrit--tool-account input)))
    (concat "Gmail labels (name, indexed message count); each is also a Gnus group nngmail+SERVER:NAME:\n"
            (mapconcat (lambda (entry)
                         (format "- %s: %d" (cdr entry) (nngmail--group-size account (cdr entry))))
                       (nngmail-account-label-names account) "\n"))))

(defconst nngmail-efrit--tools
  `(("gmail_search"
     "Search the user's whole Gmail account with Gmail's query syntax (from:, to:, subject:, newer_than:7d, has:attachment, label:name, is:unread...). Returns a listing with article references (GROUP#NUMBER) and Gmail ids, newest first. Read-only. Follow up with gnus_articles for the text, or gmail_thread for a whole conversation."
     (("type" . "object")
      ("properties" . (("query" . (("type" . "string") ("description" . "Gmail search query")))
                       ("limit" . (("type" . "integer") ("description" . "Most results, default 20, max 100")))
                       ("account" . (("type" . "string") ("description" . "nngmail server name when there are several")))))
      ("required" . ["query"]))
     ,#'nngmail-efrit--tool-search)
    ("gmail_thread"
     "Every message in the Gmail conversation that contains the given message (a Gmail id or a GROUP#NUMBER reference), across all labels, oldest first, as text. Read-only."
     (("type" . "object")
      ("properties" . (("id" . (("type" . "string") ("description" . "A Gmail message id or article reference in the thread")))
                       ("account" . (("type" . "string")))))
      ("required" . ["id"]))
     ,#'nngmail-efrit--tool-thread)
    ("gmail_labels"
     "The user's Gmail labels with how many messages nngmail has indexed under each. Read-only."
     (("type" . "object") ("properties" . (("account" . (("type" . "string"))))))
     ,#'nngmail-efrit--tool-labels))
  "The Gmail tools offered to efrit: (NAME DESCRIPTION INPUT-SCHEMA FUNCTION).")

;;;###autoload
(defun nngmail-efrit-ensure-tools ()
  "Register the Gmail tools (and efrit-gnus's) with efrit.  Idempotent."
  (interactive)
  (nngmail-efrit--require-efrit-gnus)
  (efrit-gnus-ensure-tools)
  (pcase-dolist (`(,name ,description ,schema ,fn) nngmail-efrit--tools)
    (efrit-register-tool name :description description :input-schema schema
                         :function fn :class 'read :package 'nngmail))
  (when (called-interactively-p 'any)
    (message "nngmail: %d Gmail tools registered with efrit" (length nngmail-efrit--tools))))

(defun nngmail-efrit-remove-tools ()
  "Withdraw the Gmail tools from efrit."
  (interactive)
  (when (featurep 'efrit-tool-registry)
    (efrit-unregister-package-tools 'nngmail)))

;; Register as soon as efrit's registry exists, so the model has the
;; tools in every turn.  (Waits on an efrit feature, not an nngmail one,
;; so it is safe across `nngmail-reload'.)
(with-eval-after-load 'efrit-tool-registry
  (ignore-errors (nngmail-efrit-ensure-tools)))

;;;; Reloading both sides

;;;###autoload
(defun nngmail-efrit-reload (&optional verbose)
  "Reload efrit and nngmail from source, then re-register the tools.
efrit first, so the registry the tools go into is the new one.  With
VERBOSE (a prefix argument) each file is listed as it loads."
  (interactive "P")
  (let ((t0 (float-time)) (efrit-count 0) (nngmail-count 0))
    (if (featurep 'efrit)
        (progn (require 'efrit-reload)
               (setq efrit-count (or (efrit-reload verbose) 0)))
      (when (locate-library "efrit")
        (require 'efrit)
        (setq efrit-count 1)))
    ;; nngmail-reload.el is a developer tool in nngmail's dev/ directory,
    ;; not part of the installed package; without it nngmail stays as is.
    (when (require 'nngmail-reload nil t)
      (setq nngmail-count (or (nngmail-reload verbose) 0)))
    (funcall 'nngmail-efrit-ensure-tools)
    (message "nngmail-efrit: reloaded %d efrit and %d nngmail libraries in %.1fs; tools registered"
             efrit-count nngmail-count (- (float-time) t0))))

;;;; nngmail's indicator in efrit's agent header

(defcustom nngmail-efrit-header-indicator t
  "Whether nngmail's G and count pills also show in efrit agent headers.
efrit's agent buffers draw their own header line; with this on, the
same indicator the mode line carries (the G, unread/starred/indexed
pills, or the spinner while a sync runs) follows efrit's header there,
so it stays in sight while you watch agents.  Needs efrit 0.12.13."
  :type 'boolean
  :group 'nngmail)

(defun nngmail-efrit-header-indicator ()
  "For `efrit-agent-header-extra-functions': the mode-line indicator, or nil."
  (and nngmail-efrit-header-indicator
       (boundp 'nngmail--spinner-string)
       (stringp nngmail--spinner-string)
       (not (string-empty-p (substring-no-properties nngmail--spinner-string)))
       (string-trim-left nngmail--spinner-string)))

(defun nngmail-efrit--install-header-indicator ()
  "Register the indicator with efrit's header hook when efrit offers it."
  (when (boundp 'efrit-agent-header-extra-functions)
    (add-hook 'efrit-agent-header-extra-functions #'nngmail-efrit-header-indicator)))

(with-eval-after-load 'efrit-agent-svg-header
  (nngmail-efrit--install-header-indicator))
(nngmail-efrit--install-header-indicator)

;;;; Keys: efrit-gnus's commands under L A

(defun nngmail-efrit--install-keys ()
  "Put `efrit-gnus-map' under `L A' in nngmail's label prefix map.
Called at load and again by `nngmail-reload' after every file has
reloaded.  Does nothing when efrit-gnus is not available."
  (when (and (boundp 'nngmail-summary-label-map)
             (or (featurep 'efrit-gnus)
                 (and (locate-library "efrit-gnus")
                      (ignore-errors (nngmail-efrit--require-efrit-gnus)))))
    (define-key nngmail-summary-label-map "A" efrit-gnus-map)))

(nngmail-efrit--install-keys)

(provide 'nngmail-efrit)

;;; nngmail-efrit.el ends here
