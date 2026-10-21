;;; nngmail-menu.el --- Transient menu and menu-bar entry for nngmail -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1") (transient "0.7"))
;; Keywords: mail, gnus, tools

;;; Commentary:

;; nngmail's commands are spread over the `L' prefix in summaries
;; (labels, and `L A' for asking efrit), `M-x nngmail-...' for
;; backfill and diagnostics, and Gnus's own keys for moving mail.
;; This file puts them in two places where they can be found:
;;
;;  - `nngmail-menu', a transient.  In nngmail summary buffers it is
;;    on `L L' (the label prefix, twice) and shows the label commands,
;;    the efrit commands, group and account operations, and the
;;    diagnostics, with their keys.  Sections that do not apply where
;;    you are (no efrit installed, not in a summary) are hidden.
;;  - An "nngmail" entry in the menu bar of nngmail summary and group
;;    buffers, with the same commands.
;;
;; Needs transient 0.7 or later (Emacs 30 ships it; Emacs 29 gets it
;; from GNU ELPA).  The prefix is defined at top level, unguarded: a
;; `transient-define-prefix' inside a conditional is not expanded at
;; compile time and misbehaves on reload.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'nngmail)
(require 'gnus-sum)
(require 'gnus-group)
(declare-function efrit-gnus-analyze "efrit-gnus")
(declare-function efrit-gnus-analyze-thread "efrit-gnus")
(declare-function efrit-gnus-analyze-unread "efrit-gnus")
(declare-function efrit-gnus-analyze-group "efrit-gnus")
(declare-function efrit-gnus-analyze-search "efrit-gnus")
(declare-function efrit-gnus-cancel "efrit-gnus")
(declare-function efrit-prompts-manage "efrit-prompts")
(declare-function nngmail-efrit-reload "nngmail-efrit")
(declare-function nngmail-gdocs-show "nngmail-gdocs")
(declare-function nngmail-testdrive "nngmail-testdrive")
(declare-function nngmail-reload "nngmail-reload")
(declare-function nngmail-build-info "nngmail-reload")
(declare-function nngmail-restore-token-store "nngmail")
(declare-function nngmail-background-status "nngmail-background")
(declare-function nngmail-background-stop "nngmail-background")
(declare-function nngmail-background-start "nngmail-background")

(defun nngmail-menu--in-summary-p ()
  "Non-nil in an nngmail summary buffer."
  (and (derived-mode-p 'gnus-summary-mode)
       (eq (car-safe (gnus-find-method-for-group gnus-newsgroup-name)) 'nngmail)))

(defun nngmail-menu--efrit-p ()
  "Non-nil when efrit's Gnus commands can be used (efrit with efrit-gnus.el)."
  (and (locate-library "efrit") (locate-library "efrit-gnus") t))

;;;; The transient

(transient-define-prefix nngmail-menu ()
  "The nngmail menu: labels, asking efrit, groups and diagnostics."
  [:description
   (lambda () (if (nngmail-menu--in-summary-p)
                  (format "nngmail: %s" (gnus-group-real-name gnus-newsgroup-name))
                "nngmail"))
   [:if nngmail-menu--in-summary-p
    "Labels (L ...)"
    ("e" "edit the label set" nngmail-summary-edit-labels)
    ("+" "add a label" nngmail-summary-add-label)
    ("-" "remove a label" nngmail-summary-remove-label)
    ("a" "archive (drop INBOX)" nngmail-summary-archive)
    ("l" "show labels" nngmail-summary-show-labels)
    ("?" "explain icons" nngmail-summary-explain-labels)
    ("D" "linked Google Docs" nngmail-gdocs-show)]
   [:if nngmail-menu--efrit-p
    "Ask efrit (L A ...)"
    ("A a" "the selection" efrit-gnus-analyze
     :if nngmail-menu--in-summary-p)
    ("A t" "this thread" efrit-gnus-analyze-thread
     :if nngmail-menu--in-summary-p)
    ("A u" "unread in a group" efrit-gnus-analyze-unread)
    ("A g" "newest N of a group" efrit-gnus-analyze-group)
    ("A s" "a Gmail search" efrit-gnus-analyze-search)
    ("A e" "edit the prompts" efrit-prompts-manage)
    ("A k" "stop queued batches" efrit-gnus-cancel)]
   ["Account"
    ("B" "background sync: status" nngmail-background-status)
    ("S" "background sync: stop" nngmail-background-stop)
    ("G" "background sync: start / resume" nngmail-background-start)
    ("b" "backfill older mail" nngmail-backfill)
    ("v" "verify group vs Gmail" nngmail-verify-group
     :if nngmail-menu--in-summary-p)
    ("R" "rebuild groups from labels" nngmail-rebuild-groups)
    ("Q" "quota help" nngmail-quota-help)
    ("q" "forget quota answer" nngmail-quota-reset-policy)
    ("p" "pause report" nngmail-pause-report)]
   ["Diagnose"
    ("d" "debug group" nngmail-debug-group
     :if nngmail-menu--in-summary-p)
    ("n" "check .newsrc numbering" nngmail-check-newsrc)
    ("f" "repair IMAP-named labels" nngmail-repair-labels)
    ("T" "test drive" nngmail-testdrive)
    ("r" "reload nngmail" nngmail-reload)
    ("V" "which code runs (version, stale files)" nngmail-build-info)
    ("P" "restore oauth2 token store from memory" nngmail-restore-token-store
     :if (lambda () (bound-and-true-p nngmail--token-store-good)))
    ("E" "reload efrit + nngmail" nngmail-efrit-reload
     :if nngmail-menu--efrit-p)
    ("X" "reset local state" nngmail-reset)]])

;;;; Menu bar

(defvar nngmail-menu--bar
  '("nngmail"
    ["Edit labels..." nngmail-summary-edit-labels :active (nngmail-menu--in-summary-p)]
    ["Add label..." nngmail-summary-add-label :active (nngmail-menu--in-summary-p)]
    ["Remove label..." nngmail-summary-remove-label :active (nngmail-menu--in-summary-p)]
    ["Archive" nngmail-summary-archive :active (nngmail-menu--in-summary-p)]
    ["Show labels" nngmail-summary-show-labels :active (nngmail-menu--in-summary-p)]
    ["Linked Google Docs" nngmail-gdocs-show :active (nngmail-menu--in-summary-p)]
    "---"
    ("Ask efrit"
     :visible (nngmail-menu--efrit-p)
     ["About the selection..." efrit-gnus-analyze :active (nngmail-menu--in-summary-p)]
     ["About this thread..." efrit-gnus-analyze-thread :active (nngmail-menu--in-summary-p)]
     ["About unread in a group..." efrit-gnus-analyze-unread t]
     ["About the newest N of a group..." efrit-gnus-analyze-group t]
     ["About a Gmail search..." efrit-gnus-analyze-search t]
     "---"
     ["Edit the prompts..." efrit-prompts-manage t]
     ["Stop the batches still queued" efrit-gnus-cancel t])
    "---"
    ["Backfill older mail..." nngmail-backfill t]
    ["Verify group against Gmail" nngmail-verify-group :active (nngmail-menu--in-summary-p)]
    ["Rebuild groups from labels" nngmail-rebuild-groups t]
    ["Quota help" nngmail-quota-help t]
    ["Pause report" nngmail-pause-report t]
    "---"
    ["Debug group" nngmail-debug-group :active (nngmail-menu--in-summary-p)]
    ["Check .newsrc numbering" nngmail-check-newsrc t]
    ["Background sync status" nngmail-background-status t]
    ["Stop background sync..." nngmail-background-stop t]
    ["Start background sync..." nngmail-background-start t]
    ["Test drive..." nngmail-testdrive t]
    ["Reload nngmail" nngmail-reload t]
    ["Which code runs" nngmail-build-info t]
    ["Reload efrit and nngmail" nngmail-efrit-reload :visible (nngmail-menu--efrit-p)]
    ["Reset local state..." nngmail-reset t])
  "The menu-bar menu, in `easy-menu-define' form.")

(defvar nngmail-summary-label-map)

;; `L L' opens the menu.  Bound when this file loads, into the label
;; prefix map nngmail already installs in its summaries; the map is
;; shared, so every nngmail summary has it at once.  Guarded for the
;; reload order (see `nngmail-efrit--install-keys').
(defun nngmail-menu--install-keys ()
  "Bind `L L' to the menu in nngmail summaries."
  (when (boundp 'nngmail-summary-label-map)
    (define-key nngmail-summary-label-map "L" #'nngmail-menu)))

(nngmail-menu--install-keys)

(defun nngmail-menu--install ()
  "Add the menu-bar entry in nngmail summary and group buffers."
  (when (or (nngmail-menu--in-summary-p)
            (and (derived-mode-p 'gnus-group-mode) (nngmail-configured-servers)))
    ;; Under the local map's menu-bar prefix, named explicitly: with a
    ;; nil MAP easy-menu picks a map by guesswork and a nil PATH puts
    ;; the item at the top level of the keymap, not in the menu bar.
    (easy-menu-add-item (current-local-map) '("menu-bar") nngmail-menu--bar)))

(add-hook 'gnus-summary-mode-hook #'nngmail-menu--install)
(add-hook 'gnus-group-mode-hook #'nngmail-menu--install)

(provide 'nngmail-menu)

;;; nngmail-menu.el ends here
