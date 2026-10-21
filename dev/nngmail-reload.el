;;; nngmail-reload.el --- Reload every loaded nngmail library from source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mail, gnus, tools

;;; Commentary:

;; `M-x nngmail-reload' re-`load's every nngmail feature that is
;; currently loaded (nngmail, nngmail-efrit, nngmail-menu,
;; nngmail-gdocs, nngmail-testdrive, nngmail-tests, this file), in the order they were first loaded, so a checkout edited in
;; place takes effect without restarting Emacs or Gnus.
;;
;; What it keeps: your customizations and the live accounts.
;; `defvar'/`defcustom' do not reassign a bound variable, so settings,
;; `nngmail--accounts' (the in-memory index) and queued marks survive;
;; `defconst' and `defface' get their new values.  Hooks and advice are
;; idempotent and stay in place.
;;
;; What it fixes up afterwards, because a plain `load' would not:
;;
;;  - Account objects.  `nngmail-account' is a struct; when a reload
;;    adds a slot, existing objects are shorter than the new accessors
;;    expect.  `nngmail--upgrade-all-accounts' copies them into fresh
;;    objects.
;;  - Image caches.  Status bars, gauges, the spinner and label-mark
;;    images are cached by their inputs, not by the code that drew
;;    them, so a change to a drawing function would not show.  The
;;    caches are cleared.
;;  - Keymaps.  `nngmail-...-map' variables are unbound before the
;;    reload so a key added to a map's definition appears, and every
;;    open nngmail summary gets the new maps installed.
;;  - Timers.  The spinner timer and the idle mark flush point at
;;    function symbols, so they pick up new definitions by themselves;
;;    a running spinner is restarted anyway so a changed interval
;;    applies.
;;  - The backend registration (`gnus-declare-backend') and the nnoo
;;    server variables are re-run by the file itself; re-declaring is
;;    harmless.
;;
;; What it cannot do: a sync that is in progress keeps running the old
;; code until it returns, and an `eval-after-load' body runs again.
;; If Gnus behaves oddly after a reload, `M-x nngmail-reset' (local
;; state only, nothing at Gmail) or a Gnus restart puts things right.
;;
;; The source file is preferred over a stale .elc: `load' with the
;; extension elided picks the newer of the two, as `load-prefer-newer'
;; would, so an edited .el wins over the .elc compiled before it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst nngmail-reload--feature-prefix "nngmail"
  "Features whose names start with this are reloaded.")

(defconst nngmail-reload--never
  '(nngmail-reload)
  "Features not reloaded by the main pass.
This file reloads *itself* first, separately (see `nngmail-reload'),
so a fix to the reloader takes effect in the same invocation.")

(defvar nngmail-reload--self-reloaded nil
  "Non-nil while `nngmail-reload' runs its freshly loaded self.")

(defconst nngmail-reload--caches
  '(nngmail--gauge-cache
    nngmail--label-mark-images
    nngmail--icon-metrics)
  "Hash-table variables holding rendered images or metrics.
Cleared after a reload so changed drawing code shows.")

(defun nngmail-reload--clear-image-state ()
  "Drop every remembered rendering: nngmail's caches and Emacs's image cache.
The count-pill strings (`nngmail--count-pills-cache') carry their
images and are reused until a label changes, so a reload would keep
showing pills drawn by the old code; and Emacs caches rasterised
images by spec, so a redrawn SVG equal to an old one would show the old
pixels.  `clear-image-cache' on all frames covers the latter."
  (when (boundp 'nngmail--count-pills-cache)
    (set 'nngmail--count-pills-cache nil))
  (when (boundp 'nngmail--count-pills-generation)
    (set 'nngmail--count-pills-generation (1+ (symbol-value 'nngmail--count-pills-generation))))
  (when (fboundp 'clear-image-cache)
    (ignore-errors (funcall 'clear-image-cache t))))

(defvar nngmail-reload--load-times nil
  "Alist (FEATURE . TIME) of when each nngmail file was loaded into this Emacs.")

(defun nngmail-reload--note-load (file)
  "Record the load time of FILE when it is one of nngmail's.
On `after-load-functions'."
  (let ((base (file-name-base file)))
    (when (string-prefix-p nngmail-reload--feature-prefix base)
      (setf (alist-get (intern base) nngmail-reload--load-times) (current-time)))))

(add-hook 'after-load-functions #'nngmail-reload--note-load)

;;;###autoload
(defun nngmail-build-info ()
  "One line saying which nngmail code this Emacs runs.
The version, the source directory, the newest source modification
time, and the files edited on disk after they were loaded -- those run
old code until `nngmail-reload'.  For test-drive reports and bug
reports; interactively, shown in the echo area."
  (interactive)
  (let* ((loaded (delq nil (mapcar (lambda (f)
                                     (let ((path (nngmail-reload--library-file f)))
                                       (and path (cons f path))))
                                   (nngmail-reload-features))))
         (dir (and loaded (abbreviate-file-name (file-name-directory (cdar loaded)))))
         (newest nil) (stale nil))
    (dolist (lp loaded)
      (let* ((el (concat (cdr lp) ".el"))
             (mtime (and (file-exists-p el) (file-attribute-modification-time (file-attributes el))))
             (load-time (cdr (assq (car lp) nngmail-reload--load-times))))
        (when (and mtime (or (null newest) (time-less-p newest mtime))) (setq newest mtime))
        ;; Loaded before the hook existed counts as unknown, not stale.
        (when (and mtime load-time (time-less-p load-time mtime))
          (push (symbol-name (car lp)) stale))))
    (let ((info (format "nngmail %s from %s, sources last modified %s%s"
                        (if (boundp 'nngmail-version) (symbol-value 'nngmail-version) "?")
                        (or dir "?")
                        (if newest (format-time-string "%F %T" newest) "?")
                        (if stale
                            (format ".  STALE in this Emacs (edited on disk after loading): %s -- reload before trusting results"
                                    (mapconcat #'identity (nreverse stale) ", "))
                          ""))))
      (when (called-interactively-p 'interactive) (message "%s" info))
      info)))

(defun nngmail-reload-features ()
  "The loaded nngmail features, oldest first (the order to reload them in).
`features' lists the newest first."
  (reverse
   (seq-filter (lambda (f)
                 (and (string-prefix-p nngmail-reload--feature-prefix (symbol-name f))
                      (not (memq f nngmail-reload--never))))
               features)))

(defun nngmail-reload--library-file (feature)
  "The file FEATURE was loaded from, sans extension, or nil."
  (let ((file (or (locate-library (symbol-name feature))
                  (symbol-file feature 'provide))))
    (and file (file-name-sans-extension file))))

(defun nngmail-reload--clear-caches ()
  "Empty the image and metric caches; return how many were cleared."
  (let ((n 0))
    (dolist (sym nngmail-reload--caches)
      (when (and (boundp sym) (hash-table-p (symbol-value sym)))
        (clrhash (symbol-value sym))
        (cl-incf n)))
    n))

(declare-function nngmail-job-stopped "nngmail-background")
(declare-function nngmail-job-proc "nngmail-background")
(declare-function nngmail-job-timer "nngmail-background")
(declare-function nngmail-job-account "nngmail-background")
(declare-function nngmail--job-set-timer "nngmail-background")
(declare-function nngmail--job-set-state "nngmail-background")
(declare-function nngmail-background-kick "nngmail-background")

(defun nngmail-reload--option-standards ()
  "Alist (SYMBOL . STANDARD-VALUE) of every nngmail user option now defined.
Taken before a reload; `nngmail-reload--adopt-new-defaults' compares."
  (let (out)
    (mapatoms (lambda (sym)
                (when (and (string-prefix-p nngmail-reload--feature-prefix (symbol-name sym))
                           (get sym 'standard-value)
                           (boundp sym))
                  (push (cons sym (ignore-errors (eval (car (get sym 'standard-value)) t))) out))))
    out))

(defun nngmail-reload--adopt-new-defaults (before)
  "After a reload, give options whose default changed the new default.
BEFORE is `nngmail-reload--option-standards' from before the load.  A
`defcustom' re-evaluated by `load' leaves a bound variable alone, so a
changed default in the source would not reach a running session.  An
option is updated only when its value still equals the OLD default: the
user never set it, so the new default is what they would get on a
fresh start.  A customized or `setq'd value is left as it is.  Return
the symbols updated."
  (let (updated)
    (pcase-dolist (`(,sym . ,old) before)
      (when (and (boundp sym) (get sym 'standard-value))
        (let ((new (ignore-errors (eval (car (get sym 'standard-value)) t))))
          (when (and (not (equal old new))
                     (equal (symbol-value sym) old)
                     (not (get sym 'saved-value))
                     (not (get sym 'customized-value)))
            (set-default sym new)
            (push sym updated)))))
    (when updated
      (when (fboundp 'nngmail--log)
        (funcall 'nngmail--log "reload: new defaults adopted for %s"
                 (mapconcat #'symbol-name updated ", "))))
    updated))

(defun nngmail-reload--cancel-heartbeat ()
  "Cancel the idle heartbeat timer before its defvar is re-evaluated."
  (when (and (boundp 'nngmail--idle-heartbeat-timer)
             (timerp (symbol-value 'nngmail--idle-heartbeat-timer)))
    (cancel-timer (symbol-value 'nngmail--idle-heartbeat-timer))
    (makunbound 'nngmail--idle-heartbeat-timer)))

(defun nngmail-reload--rearm-background ()
  "Background jobs hold closures over old code: reschedule them on the new.
A job with a request out is left to finish it; the callback then
schedules the next step through the reloaded functions."
  (when (and (boundp 'nngmail--jobs) (fboundp 'nngmail-background-kick))
    (maphash (lambda (_name job)
               (unless (or (funcall 'nngmail-job-stopped job) (funcall 'nngmail-job-proc job))
                 ;; `nngmail-background-kick' cancels the pending timer itself
                 ;; when it schedules; put the job in a state it will schedule from.
                 (when-let* ((timer (funcall 'nngmail-job-timer job)))
                   (cancel-timer timer)
                   (funcall 'nngmail--job-set-timer job nil))
                 (funcall 'nngmail--job-set-state job 'idle)
                 (funcall 'nngmail-background-kick (funcall 'nngmail-job-account job))))
             (symbol-value 'nngmail--jobs))))

(defun nngmail-reload--restart-spinner ()
  "If the mode-line spinner is running, restart it on the new code."
  (when (and (boundp 'nngmail--spinner-timer) (symbol-value 'nngmail--spinner-timer)
             (fboundp 'nngmail--spinner-stop) (fboundp 'nngmail--spinner-start))
    (funcall 'nngmail--spinner-stop)
    (when (and (boundp 'nngmail--progress) (symbol-value 'nngmail--progress))
      (funcall 'nngmail--spinner-start))))

;;;###autoload
(defun nngmail-reload (&optional verbose)
  "Reload every loaded nngmail library from its source, dependencies first.
With VERBOSE (a prefix argument) list each file as it loads.  Files
that fail to load are reported at the end; the rest still load.
Afterwards, stale account objects are upgraded and image caches
cleared; see the Commentary for what survives and what does not."
  (interactive "P")
  ;; Reload this file first and run the new definition, so a fix to
  ;; the reloader applies right away instead of one restart later.
  ;; The re-entered call sees the flag and does the real work.
  (if (and (not nngmail-reload--self-reloaded)
           (nngmail-reload--library-file 'nngmail-reload))
      (let ((nngmail-reload--self-reloaded t)
            (load-prefer-newer t))
        (load (nngmail-reload--library-file 'nngmail-reload) nil t)
        (funcall 'nngmail-reload verbose))
    (nngmail-reload--run verbose)))

(defun nngmail-reload-keymap-variables ()
  "The bound `nngmail-...-map' variables that hold keymaps."
  (let (out)
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (string-prefix-p "nngmail-" (symbol-name sym))
                           (string-suffix-p "-map" (symbol-name sym))
                           (keymapp (symbol-value sym)))
                  (push sym out))))
    out))

(defun nngmail-reload--forget-after-load-forms ()
  "Remove `after-load-alist' entries waiting on nngmail features.
Those come from `with-eval-after-load' in nngmail's own files; the
files re-register whatever they still need when they load again."
  (setq after-load-alist
        (seq-remove (lambda (entry)
                      (let ((key (car entry)))
                        (and (symbolp key)
                             (string-prefix-p nngmail-reload--feature-prefix (symbol-name key)))))
                    after-load-alist)))

(defun nngmail-reload--refresh-summary-keys ()
  "Re-run nngmail's key installation in every live nngmail summary buffer.
After a reload the prefix maps are new objects; a summary opened before
still has its prefix key pointing at the old ones.  The sub-prefixes
other files hang off the label map (`L A', `L L') are re-attached
first: their files may have loaded before nngmail.el rebuilt the map."
  (dolist (fn '(nngmail-efrit--install-keys nngmail-menu--install-keys nngmail-gdocs--install-keys))
    (when (fboundp fn) (ignore-errors (funcall fn))))
  (when (fboundp 'nngmail--install-summary-keys)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'gnus-summary-mode)
                   (bound-and-true-p gnus-newsgroup-name))
          (ignore-errors (funcall 'nngmail--install-summary-keys))
          (when (fboundp 'nngmail-menu--install)
            (ignore-errors (funcall 'nngmail-menu--install))))))))

(defun nngmail-reload--run (verbose)
  "The body of `nngmail-reload', run from its freshly loaded definition."
  (let ((load-prefer-newer t)
        (loaded 0)
        (failed nil)
        (start (float-time)))
    ;; Keymaps are `defvar's: a reload leaves a bound one alone, so a
    ;; key added to a map's initial value would never appear.  Unbind
    ;; them first; the files rebuild them, and summaries get the new
    ;; objects below.
    (dolist (sym (nngmail-reload-keymap-variables))
      (makunbound sym))
    (nngmail-reload--cancel-heartbeat)
    ;; `with-eval-after-load' bodies from earlier loads of nngmail
    ;; files pile up in `after-load-alist' and run on every reload of
    ;; the feature they wait for, with whatever stale closures they
    ;; captured.  Drop the entries that nngmail files registered.
    (nngmail-reload--forget-after-load-forms)
    (let ((standards (nngmail-reload--option-standards)))
      (dolist (feature (nngmail-reload-features))
        (let ((file (nngmail-reload--library-file feature)))
          (if (null file)
              (push (cons feature "no file found") failed)
            (condition-case err
                (progn
                  (load file nil (not verbose))
                  (cl-incf loaded))
              (error
               (push (cons feature (error-message-string err)) failed))))))
      (nngmail-reload--adopt-new-defaults standards))
    ;; Fix-ups that a plain load leaves behind.
    (when (fboundp 'nngmail--upgrade-all-accounts)
      (funcall 'nngmail--upgrade-all-accounts))
    (nngmail-reload--refresh-summary-keys)
    (nngmail-reload--clear-image-state)
    (let ((caches (nngmail-reload--clear-caches)))
      (nngmail-reload--restart-spinner)
      (nngmail-reload--rearm-background)
      (when (and (boundp 'nngmail--status-last-line))
        ;; Force the next status draw even if the text is unchanged.
        (set 'nngmail--status-last-line nil))
      (let ((summary (format "nngmail %s: reloaded %d librar%s in %.1fs, %d cache%s cleared%s"
                             (if (boundp 'nngmail-version) (symbol-value 'nngmail-version) "?")
                             loaded (if (= loaded 1) "y" "ies")
                             (- (float-time) start)
                             caches (if (= caches 1) "" "s")
                             (if failed
                                 (format "; %d failed: %s" (length failed)
                                         (mapconcat (lambda (f) (format "%s (%s)" (car f) (cdr f)))
                                                    (nreverse failed) ", "))
                               ""))))
        (when (fboundp 'nngmail--log)
          (funcall 'nngmail--log "%s" summary)
          (funcall 'nngmail--log "%s" (nngmail-build-info)))
        (message "%s" summary)
        loaded))))

(provide 'nngmail-reload)

;;; nngmail-reload.el ends here
