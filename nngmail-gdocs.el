;;; nngmail-gdocs.el --- Google Docs linked from mail, as text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Teodor Zlatanov

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mail, gnus, tools

;;; Commentary:

;; Meeting recaps and the like arrive as a short email pointing at a
;; Google Doc.  The mail says nothing; the doc has the content.  This
;; file fetches the doc's text with the Google account nngmail already
;; holds a token for, and offers it in three places:
;;
;;  - in the article buffer, as a Gnus washing function: each linked
;;    doc's text appears under a "Google Doc: TITLE" heading below the
;;    part, when `gnus-treat-gdocs' says so (by default in groups
;;    matching "recaps"), or on demand with `W D'.
;;  - `L D' in an nngmail summary (`nngmail-gdocs-show'): the same docs
;;    in their own *Google Docs* buffer, for reading at length.
;;  - efrit-gnus's link expansion: when efrit is asked about an article
;;    (`L A a', or the model calling `gnus_articles'), the text of every
;;    Google Doc it links to follows the article.  Registered on
;;    `efrit-gnus-expand-link-functions' when efrit-gnus is around.
;;  - the `gdoc_fetch' tool for efrit's model: the text of a doc by URL
;;    or id, for a link it meets in conversation.
;;
;; The last two step aside when efrit has its own Google Drive document
;; source (`efrit-documents-gdrive', efrit 0.4.1+): that layer also
;; searches Drive, finds documents related to an article (the notes
;; Calendar took for a meeting), and serves Confluence and other
;; sources through the same `doc_fetch'/`doc_search' tools.  Its Drive
;; source can use the same auth-source entry as nngmail once the entry
;; carries the Drive scope.  The article washing and `L D' stay here:
;; they need no efrit.
;;
;; The fetch is Drive's export endpoint,
;; GET https://www.googleapis.com/drive/v3/files/ID/export?mimeType=text/markdown
;; which returns the document body as Markdown (headings, lists, tables,
;; links) -- a better fit for a model than the Docs API's JSON tree.
;;
;; The OAuth token must carry a Drive scope.  nngmail's default scope is
;; https://mail.google.com/ only; add https://www.googleapis.com/auth/drive.readonly
;; to the "scope" of the auth-source entry and re-consent once
;; (`nngmail-reauthorize').  A token without it gets 403 from Drive; the
;; error message says so and names the scope.  A doc the account cannot
;; open (not shared with you) is also a 403, with Google's own reason;
;; a wrong id is a 404.  In every failure case the URL is still shown,
;; so the model or you can follow it by hand.
;;
;; Docs are cached in memory for the Emacs session by id and the doc's
;; modifiedTime, so re-analysing the same recaps costs no requests.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'nngmail)
(require 'gnus-art)
(require 'gnus-sum)

(declare-function efrit-register-tool "efrit-tool-registry")
(declare-function shr-insert-document "shr")
(declare-function libxml-parse-html-region "xml.c")
(defvar shr-width) (defvar shr-inhibit-images)
(defvar efrit-gnus-expand-link-functions)
(defvar gnus-treat-condition)
(defvar gnus-summary-wash-map)

(defgroup nngmail-gdocs nil
  "Google Docs linked from mail, fetched as text."
  :group 'nngmail
  :prefix "nngmail-gdocs-")

(defcustom nngmail-gdocs-max-chars 40000
  "Characters kept of one document; the rest is cut and marked."
  :type 'integer)

(defcustom nngmail-gdocs-export-mime-type "text/markdown"
  "MIME type asked of Drive's export for text consumers (efrit, `L D').
text/markdown keeps headings, lists and tables in a form a model reads
well.  The article buffer asks for text/html separately and renders it
with shr, like HTML mail."
  :type '(choice (const "text/markdown") (const "text/plain")))

(defcustom nngmail-gdocs-link-regexp
  "https://docs\\.google\\.com/document/\\(?:u/[0-9]+/\\)?d/\\([-_a-zA-Z0-9]+\\)"
  "Matches a Google Docs URL; group 1 is the document id.
Spreadsheets and slides are not documents and are left alone."
  :type 'regexp)

(defconst nngmail-gdocs--drive-api "https://www.googleapis.com/drive/v3/files/"
  "Drive v3 files endpoint; the same OAuth token as Gmail is used.")

(defconst nngmail-gdocs--scope "https://www.googleapis.com/auth/drive.readonly"
  "The scope the token needs for the export.")

(defvar nngmail-gdocs--cache (make-hash-table :test #'equal)
  "(ID . MIME-TYPE) -> (MODIFIED-TIME TITLE . TEXT), for this Emacs session.")

;;;; Fetching

(defun nngmail-gdocs-doc-id (string)
  "The Google Docs id in STRING (a URL or a bare id), or nil."
  (cond
   ((null string) nil)
   ((string-match nngmail-gdocs-link-regexp string) (match-string 1 string))
   ((string-match-p "\\`[-_a-zA-Z0-9]\\{20,\\}\\'" string) string)
   (t nil)))

(defun nngmail-gdocs--account ()
  "The nngmail account whose token does the fetching: the first configured."
  (let ((name (car (nngmail-configured-servers))))
    (unless name (user-error "nngmail-gdocs: No nngmail server configured"))
    (nngmail--account name)))

(defun nngmail-gdocs--explain (err id)
  "A one-line reason for Drive failure ERR on doc ID, naming what to fix."
  (pcase err
    (`(nngmail-http-error 403 ,msg . ,_)
     (cond
      ((string-match-p "too large to be exported\\|exportSizeLimitExceeded" msg)
       (format "Drive refuses to export doc %s in any text format (over its 10 MB export limit)" id))
      ((string-match-p "insufficient\\|scope\\|ACCESS_TOKEN_SCOPE" msg)
       (format "the OAuth token has no Drive scope; add %s to the auth-source entry's scope and run M-x nngmail-reauthorize"
               nngmail-gdocs--scope))
      (t (format "Google refused access to doc %s: %s" id msg))))
    (`(nngmail-http-error 404 . ,_)
     (format "doc %s not found (deleted, or not a Google Doc)" id))
    (`(nngmail-http-error ,status ,msg . ,_)
     (format "Drive returned %s for doc %s: %s" status id msg))
    (_ (error-message-string err))))

(defun nngmail-gdocs--metadata (account id)
  "Name and modifiedTime of doc ID in ACCOUNT, as an alist, or nil on 404."
  (nngmail--request account "GET" (concat nngmail-gdocs--drive-api id)
                    '(("fields" . "name,modifiedTime,mimeType"))))

(defconst nngmail-gdocs--export-fallbacks
  '(("text/html" "text/markdown" "text/plain")
    ("text/markdown" "text/plain"))
  "Export types to try in turn when Drive says the doc is too large.
Drive caps exports at 10 MB; a doc with embedded images passes that as
HTML while its text is small.  The text formats drop the images.")

(defun nngmail-gdocs--too-large-p (err)
  "Whether ERR is Drive's export size refusal."
  (and (eq (car-safe err) 'nngmail-http-error)
       (string-match-p "too large to be exported\\|exportSizeLimitExceeded" (format "%s" (nth 2 err)))))

(defun nngmail-gdocs--export (account id mime-type)
  "The doc ID of ACCOUNT exported as MIME-TYPE, or smaller if Drive refuses.
Returns (MIME-TYPE . BYTES), or nil on 404.  A refusal in every format
signals the last `nngmail-http-error'."
  (let ((chain (or (assoc mime-type nngmail-gdocs--export-fallbacks) (list mime-type)))
        (result nil))
    (while (and chain (null result))
      (let ((mime (pop chain)))
        (condition-case err
            (when-let* ((bytes (nngmail--request account "GET" (concat nngmail-gdocs--drive-api id "/export")
                                                 `(("mimeType" . ,mime)) nil nil t)))
              (setq result (cons mime bytes)))
          (nngmail-http-error
           (if (and chain (nngmail-gdocs--too-large-p err))
               (nngmail--log "gdocs: %s too large as %s; trying %s" id mime (car chain))
             (signal (car err) (cdr err)))))))
    result))

(defun nngmail-gdocs-fetch (id-or-url &optional mime-type)
  "Return (TITLE . TEXT) of the Google Doc ID-OR-URL, from cache when unchanged.
MIME-TYPE is the export format, default `nngmail-gdocs-export-mime-type';
text/html for rendering with shr.  The size cap applies to text
formats; HTML is left whole for the renderer.  Signals `user-error'
with a specific reason when it cannot."
  (let* ((id (or (nngmail-gdocs-doc-id id-or-url)
                 (user-error "nngmail-gdocs: Not a Google Docs URL or id: %s" id-or-url)))
         (mime-type (or mime-type nngmail-gdocs-export-mime-type))
         (key (cons id mime-type))
         (account (nngmail-gdocs--account)))
    (condition-case err
        (let* ((meta (or (nngmail-gdocs--metadata account id)
                         (signal 'nngmail-http-error (list 404 "not found" id))))
               (modified (alist-get 'modifiedTime meta))
               (title (or (alist-get 'name meta) id))
               (cached (gethash key nngmail-gdocs--cache)))
          (if (and cached (equal (car cached) modified))
              (cdr cached)
            (nngmail--progress account (format "fetching doc %s" (truncate-string-to-width title 30)) nil nil t)
            (unwind-protect
                (let* ((got (or (nngmail-gdocs--export account id mime-type)
                                (signal 'nngmail-http-error (list 404 "no export" id))))
                       (mime-type (car got))
                       (text (decode-coding-string (cdr got) 'utf-8))
                       (text (replace-regexp-in-string "\r\n" "\n" text t t))
                       (text (string-trim text))
                       (text (if (or (equal mime-type "text/html")
                                     (<= (length text) nngmail-gdocs-max-chars))
                                 text
                               (concat (substring text 0 nngmail-gdocs-max-chars)
                                       (format "\n[... %d more characters of the document not shown ...]"
                                               (- (length text) nngmail-gdocs-max-chars))))))
                  ;; A fallback export is text, whatever was asked for;
                  ;; the property tells the renderer.
                  (unless (equal mime-type (cdr key))
                    (setq text (propertize text 'nngmail-gdocs-mime mime-type)))
                  (puthash key (cons modified (cons title text)) nngmail-gdocs--cache)
                  (cons title text))
              (nngmail--progress account nil))))
      (nngmail-http-error
       (user-error "nngmail-gdocs: %s" (nngmail-gdocs--explain err id))))))

;;;; efrit-gnus link expansion

(defun nngmail-gdocs-expand-link (url)
  "For `efrit-gnus-expand-link-functions': a Google Doc URL's text, else nil.
A fetch failure becomes a note (the hook shows it under the URL), not
an error, so the analysis of the mail itself still runs."
  (when (nngmail-gdocs-doc-id url)
    (condition-case err
        (pcase-let ((`(,title . ,text) (nngmail-gdocs-fetch url)))
          (format "Title: %s\n\n%s" title text))
      (user-error (format "[Google Doc not fetched: %s]" (cadr err))))))

(defun nngmail-gdocs-auth-hosts ()
  "For `efrit-auth-host-functions': the hosts nngmail's own credentials live under.
efrit's Drive and Calendar sources try these after their defaults, so
the entry nngmail already uses serves them too once its scope allows."
  (delete-dups
   (delq nil (append (list (bound-and-true-p nngmail-auth-host))
                     (nngmail-configured-servers)))))

(with-eval-after-load 'efrit-auth
  (add-hook 'efrit-auth-host-functions #'nngmail-gdocs-auth-hosts))
(when (featurep 'efrit-auth)
  (add-hook 'efrit-auth-host-functions #'nngmail-gdocs-auth-hosts))

(defun nngmail-gdocs--efrit-has-drive-p ()
  "Whether efrit's own document layer has a Google Drive source.
Then efrit-gnus fetches Docs through it (`efrit-documents-expand-url'
runs before the backend expanders) and the `doc_fetch' tool covers
Docs; registering ours too would put the same document in an analysis
twice."
  (and (featurep 'efrit-documents)
       (fboundp 'efrit-documents-source)
       (funcall 'efrit-documents-source "gdrive")
       t))

(defun nngmail-gdocs--install-expander ()
  "Offer `nngmail-gdocs-expand-link' to efrit-gnus unless efrit has Drive itself."
  (when (and (boundp 'efrit-gnus-expand-link-functions)
             (not (nngmail-gdocs--efrit-has-drive-p)))
    (add-hook 'efrit-gnus-expand-link-functions #'nngmail-gdocs-expand-link)))

(with-eval-after-load 'efrit-gnus (nngmail-gdocs--install-expander))
(nngmail-gdocs--install-expander)

;;;; The efrit tool

(defun nngmail-gdocs--tool-fetch (input)
  "The gdoc_fetch tool: the text of a Google Doc by URL or id, from INPUT."
  (let ((key (alist-get "url" input nil nil #'equal)))
    (condition-case err
        (pcase-let ((`(,title . ,text) (nngmail-gdocs-fetch key)))
          (format "Title: %s\nSource: %s\n\n%s" title key text))
      (user-error (cadr err)))))

(defconst nngmail-gdocs--tools
  `(("gdoc_fetch"
     "The text (Markdown) of a Google Doc the user can read, by URL or document id: meeting notes, recaps, specs linked from mail. Read-only. Long documents are cut with a marker."
     (("type" . "object")
      ("properties" . (("url" . (("type" . "string") ("description" . "A docs.google.com/document URL or the document id")))))
      ("required" . ["url"]))
     ,#'nngmail-gdocs--tool-fetch))
  "The tool offered to efrit: (NAME DESCRIPTION INPUT-SCHEMA FUNCTION).")

;;;###autoload
(defun nngmail-gdocs-ensure-tools ()
  "Register `gdoc_fetch' with efrit.  Idempotent.
Skipped when efrit's document layer has a Drive source: its `doc_fetch'
does the same through the same account."
  (interactive)
  (require 'efrit-tool-registry)
  (unless (nngmail-gdocs--efrit-has-drive-p)
    (pcase-dolist (`(,name ,description ,schema ,fn) nngmail-gdocs--tools)
      (efrit-register-tool name :description description :input-schema schema
                           :function fn :class 'read :package 'nngmail))))

(with-eval-after-load 'efrit-tool-registry
  (ignore-errors (nngmail-gdocs-ensure-tools)))

;;;; Summary command: L D

(defun nngmail-gdocs--doc-links-in (text)
  "Distinct Google Docs (URL . ID) pairs in TEXT, by first appearance of the id.
The same doc is linked several times in a share notification (title,
button, plain text) with different query strings; one entry per id."
  (let (out (start 0))
    (while (string-match nngmail-gdocs-link-regexp text start)
      (let ((url (match-string 0 text)) (id (match-string 1 text)))
        (unless (rassoc id out)
          (push (cons url id) out)))
      (setq start (match-end 0)))
    (nreverse out)))

(defun nngmail-gdocs--doc-links-in-buffer (&optional start end)
  "Distinct Google Docs (URL . ID) pairs between START and END of this buffer.
Links shr rendered from HTML carry their target in the `shr-url' text
property, and Gnus's own buttons in `gnus-data'; those are read first,
so a wrapped or elided link text does not matter.  Then the visible
text, for plain-text parts."
  (let ((start (or start (point-min))) (end (or end (point-max)))
        out)
    (cl-flet ((note (url)
                (when-let* ((id (and (stringp url) (nngmail-gdocs-doc-id url))))
                  (unless (rassoc id out) (push (cons url id) out)))))
      (save-excursion
        (goto-char start)
        (let ((pos start))
          (while (and pos (< pos end))
            (note (get-text-property pos 'shr-url))
            (let ((data (get-text-property pos 'gnus-data)))
              (when (stringp data) (note data)))
            (setq pos (next-single-property-change pos 'shr-url nil end))))
        (dolist (l (nngmail-gdocs--doc-links-in (buffer-substring-no-properties start end)))
          (note (car l)))))
    (nreverse out)))

(defun nngmail-gdocs--article-doc-urls ()
  "Google Docs URLs in the article at point, one per document.
Read from the article buffer as displayed (decoded, unwrapped), the
same text the washing function sees; the article is displayed first if
it is not yet."
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "nngmail-gdocs: Not in a summary buffer"))
  (unless (and (gnus-buffer-live-p gnus-article-buffer)
               (eq (gnus-summary-article-number) (cdr gnus-article-current)))
    (gnus-summary-select-article))
  (with-current-buffer gnus-article-buffer
    (mapcar #'car (nngmail-gdocs--doc-links-in-buffer))))

;;;###autoload
(defun nngmail-gdocs-show ()
  "Show the Google Docs linked from the article at point, as Markdown.
One buffer, *Google Docs*, with each doc under its title; `q' closes it.
Fails clearly when the article links no doc or the fetch is refused."
  (interactive)
  (let ((urls (nngmail-gdocs--article-doc-urls)))
    (when (null urls) (user-error "nngmail-gdocs: No Google Docs link in this article"))
    (let ((buf (get-buffer-create "*Google Docs*"))
          (i 0) (n (length urls)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (url urls)
            (cl-incf i)
            (condition-case err
                (pcase-let ((`(,title . ,text) (nngmail-gdocs-fetch url)))
                  (insert (format "# %s\n\n<%s>\n\n%s\n\n" title url text)))
              (user-error
               (insert (format "# (not fetched)\n\n<%s>\n\n%s\n\n" url (cadr err))))))
          (goto-char (point-min)))
        (if (fboundp 'markdown-mode) (funcall 'markdown-mode) (text-mode))
        (view-mode 1))
      (pop-to-buffer buf)
      (message "nngmail-gdocs: %d document%s" n (if (= n 1) "" "s")))))

;;;; Washing: the doc's text in the article buffer
;;
;; A Gnus article treatment, like buttonizing or emphasis: it runs over
;; each displayed text part under the control of `gnus-treat-gdocs',
;; which takes the usual treatment values (t, nil, a list of group
;; regexps, `(typep "text/plain")', an integer size limit, and/or/not),
;; and it is on `W' in the summary as `W D' to apply by hand.  Working
;; on the displayed text is what makes the URL scan trivial: the part
;; is decoded and unwrapped by the time a treatment sees it, so a
;; quoted-printable soft wrap never splits an id.

(defcustom gnus-treat-gdocs '("recaps")
  "Show the text of Google Docs linked from the article, below the part.
Valid values are nil, t, a list of regexps on the group name, an integer (only
parts smaller than that), a `typep' form, or and/or/not of those; see
Info node `(gnus)Customizing Articles'.  The default shows them in
groups whose name matches \"recaps\", where the mail is only a pointer.
`W D' in the summary applies it to the current article regardless.
Each doc costs a Drive request the first time; docs are cached for the
session by their modified time."
  :group 'gnus-article-treat
  :group 'nngmail-gdocs
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defface nngmail-gdocs-heading
  '((t :inherit gnus-header-subject :weight bold))
  "Face of the heading line above an inlined document.")

(defface nngmail-gdocs-body
  '((t :inherit default))
  "Face of an inlined document's text.")

(defun nngmail-gdocs--insert-doc (url)
  "Insert doc URL at point, rendered from its HTML export with shr.
On failure insert the reason under the heading.  Return non-nil on
success.  Without libxml the Markdown export is inserted as text."
  (condition-case err
      (let ((html-p (fboundp 'libxml-parse-html-region)))
        (pcase-let* ((`(,title . ,content) (nngmail-gdocs-fetch url (if html-p "text/html" nil)))
                     ;; Drive may have handed back text for a doc too big as HTML.
                     (html-p (and html-p
                                  (not (and (> (length content) 0)
                                            (get-text-property 0 'nngmail-gdocs-mime content))))))
          (insert (propertize (format "Google Doc: %s\n" title) 'face 'nngmail-gdocs-heading))
          (if html-p
              (let ((dom (with-temp-buffer
                           (insert content)
                           (libxml-parse-html-region (point-min) (point-max))))
                    (shr-width (and (boundp 'shr-width) shr-width))
                    (shr-inhibit-images t))
                (require 'shr)
                (shr-insert-document dom))
            (insert (propertize (concat content "\n") 'face 'nngmail-gdocs-body)))
          (unless (bolp) (insert "\n"))
          t))
    (user-error
     (insert (propertize (format "Google Doc: not shown\n%s\n" (cadr err))
                         'face 'nngmail-gdocs-heading))
     nil)))

(defun gnus-article-show-gdocs ()
  "Show the text of the Google Docs linked from the current part below it.
A Gnus washing function (`gnus-treat-gdocs'); \\<gnus-summary-mode-map>\\[gnus-article-show-gdocs] in the summary.
Idempotent: a doc already shown in this part is not added again."
  (interactive nil gnus-article-mode gnus-summary-mode)
  (gnus-with-article-buffer
    (save-restriction
      ;; When run as a treatment the buffer is narrowed to the part;
      ;; from the keyboard it is the whole article, so skip the headers.
      (unless (bound-and-true-p gnus-treat-condition)
        (article-goto-body)
        (narrow-to-region (point) (point-max)))
      (let* ((links (nngmail-gdocs--doc-links-in-buffer))
             (shown (save-excursion
                      (goto-char (point-min))
                      (let (ids)
                        (while (re-search-forward "^Google Doc: " nil t)
                          (when-let* ((id (get-text-property (point) 'nngmail-gdocs-id)))
                            (push id ids)))
                        ids))))
        (setq links (seq-remove (lambda (l) (member (cdr l) shown)) links))
        (when links
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n")
            (pcase-dolist (`(,url . ,id) links)
              (let ((start (point)))
                (nngmail-gdocs--insert-doc url)
                (put-text-property start (point) 'nngmail-gdocs-id id)))))))))

(defvar gnus-treatment-function-alist)
(unless (assq 'gnus-treat-gdocs gnus-treatment-function-alist)
  ;; After buttonizing: the doc text is appended below the part, and
  ;; the button pass has already run over the original text.
  (let ((cell (memq (assq 'gnus-treat-buttonize gnus-treatment-function-alist)
                    gnus-treatment-function-alist)))
    (if cell
        (setcdr cell (cons '(gnus-treat-gdocs gnus-article-show-gdocs) (cdr cell)))
      (setq gnus-treatment-function-alist
            (append gnus-treatment-function-alist '((gnus-treat-gdocs gnus-article-show-gdocs)))))))

(with-eval-after-load 'gnus-sum
  (define-key gnus-summary-wash-map "D" #'gnus-article-show-gdocs))

(defun nngmail-gdocs--install-keys ()
  "Bind `L D' to `nngmail-gdocs-show' in nngmail summaries."
  (when (boundp 'nngmail-summary-label-map)
    (define-key nngmail-summary-label-map "D" #'nngmail-gdocs-show)))

(nngmail-gdocs--install-keys)

(provide 'nngmail-gdocs)

;;; nngmail-gdocs.el ends here
