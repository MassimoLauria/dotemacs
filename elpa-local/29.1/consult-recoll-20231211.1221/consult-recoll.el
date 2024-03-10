;;; consult-recoll.el --- Recoll queries using consult  -*- lexical-binding: t; -*-

;; Author: Jose A Ortega Ruiz <jao@gnu.org>
;; Maintainer: Jose A Ortega Ruiz <jao@gnu.org>
;; Keywords: docs, convenience
;; License: GPL-3.0-or-later
;; Version: 0.8.1
;; Package-Requires: ((emacs "26.1") (consult "0.19"))
;; Homepage: https://codeberg.org/jao/consult-recoll

;; Copyright (C) 2021-2023  Free Software Foundation, Inc.

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

;; A `consult-recoll' command to perform interactive queries (including life
;; previews of documment snippets) over your Recoll
;; (https://www.lesbonscomptes.com/recoll/) index, using consult.
;;
;; Use
;;
;;     M-x consult-recoll
;;
;; to get started, and check the corresponding customization group for ways to
;; tweak its behaviour to your needs.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'files)

(eval-when-compile (require 'cl-lib))

(require 'consult)

(declare-function eww-open-file "eww")

(defgroup consult-recoll nil
  "Options for consult recoll."
  :group 'consult)

(defcustom consult-recoll-prompt "Recoll search: "
  "Prompt used by `consult-recoll'."
  :type 'string)

(defcustom consult-recoll-search-flags 'query
  "List of flags used to perform queries via recollq.

The special values query and terms will select for you flags
adequated to perform searches using recoll's query language or
simply search for all provided terms, respectively.

If you don't care about snippets and need speedier searches, you
can also set this variable to the special values no-snippets or
terms-no-snippets for query/all-terms searches that will return
no snippets.."
  :type '(choice (const :tag "Query language" query)
                 (const :tag "All terms" terms)
                 (const :tag "Query language sans snippets" no-snippets)
                 (const :tag "All terms sans snippets" terms-no-snippets)
                 (list string)))

(defcustom consult-recoll-open-fn nil
  "Default function used to open candidate URLs.
It receives the full path to the file to open and (if
`consult-recoll-inline-snippets' is set, a page number for the
snippet at hand. If set to nil, find-file is used.  See also
`consult-recoll-open-fns'"
  :type '(choice (const nil) function))

(defcustom consult-recoll-open-fns ()
  "Alist mapping mime types to functions to open a selected candidate.
If you are setting `consult-list-snippets' to t, these functions
will be called with two arguments (a file path and a page
number), otherwise just with one."
  :type '(alist :key-type string :value-type function))

(defcustom consult-recoll-inline-snippets nil
  "Show snippets as completion candidates in the minibuffer."
  :type 'boolean)

(defcustom consult-recoll-group-by-mime t
  "When set, list search results grouped by mime type."
  :type 'boolean)

(defcustom consult-recoll-format-candidate nil
  "A function taking title, path and mime type, and formatting them for display.
Set to nil to use the default `title (path)' format."
  :type '(choice (const nil) function))

(defface consult-recoll-url-face '((t :inherit link))
  "Face used to display URLs of candidates.")

(defface consult-recoll-title-face '((t :inherit italic))
  "Face used to display titles of candidates.")

(defface consult-recoll-mime-face '((t :inherit font-lock-comment-face))
  "Face used to display MIME type of candidates.")

(defvar consult-recoll-history nil "History for `consult-recoll'.")
(defvar consult-recoll--current nil)
(defvar consult-recoll--index 0)
(defvar consult-recoll--snippets nil)

(defun consult-recoll--search-flags ()
  "Compute search flags according to `consult-recoll-search-flags'."
  (cond ((listp consult-recoll-search-flags)
         consult-recoll-search-flags)
        ((symbolp consult-recoll-search-flags)
         (cl-case consult-recoll-search-flags
           (terms '("-A" "-p" "5" "-a"))
           (no-snippets '())
           (terms-no-snippets '("-a"))
           (t '("-A" "-p" "5"))))
        (t (user-error "Invalid value of `consult-recoll-search-flags'"))))

(defun consult-recoll--command (text)
  "Command used to perform queries for TEXT."
  (setq consult-recoll--current nil)
  (setq consult-recoll--index 0)
  (setq consult-recoll--snippets nil)
  `("recollq" ,@(consult-recoll--search-flags) ,text))

(defun consult-recoll--format (title urln mime)
  (if consult-recoll-format-candidate
      (funcall consult-recoll-format-candidate title urln mime)
    (format "%s (%s)"
            (propertize title 'face 'consult-recoll-title-face)
            (propertize urln 'face 'consult-recoll-url-face))))

(defsubst consult-recoll--candidate-title (candidate)
  (get-text-property 0 'title candidate))

(defsubst consult-recoll--candidate-mime (candidate)
  (get-text-property 0 'mime-type candidate))

(defsubst consult-recoll--candidate-url (candidate)
  (get-text-property 0 'url candidate))

(defsubst consult-recoll--candidate-size (candidate)
  (get-text-property 0 'size candidate))

(defsubst consult-recoll--candidate-page (candidate)
  (get-text-property 0 'page candidate))

(defsubst consult-recoll--candidate-index (candidate)
  (get-text-property 0 'index candidate))

(defsubst consult-recoll--snippets (candidate)
  (let* ((len (length consult-recoll--snippets))
         (idx (or (consult-recoll--candidate-index candidate) 0))
         (pos (- len idx)))
    (if (>= pos len)
        ""
      (mapconcat 'identity (reverse (elt consult-recoll--snippets pos)) "\n"))))

(defun consult-recoll--search-snippet (candidate _mime)
  "When CANDIDATE is the text of a snippet, search for it in current buffer."
  (when (string-match "^\s+0 : " candidate)
    (let ((txt (replace-match "" nil nil candidate)))
      (goto-char (point-min))
      (when (or (search-forward txt nil t)
                (and (derived-mode-p 'org-mode)
                     (let ((txt (replace-regexp-in-string "\\]\\].+" "" txt)))
                       (search-forward txt nil t)))
                (let ((mid (/ (length txt) 2)))
                  (or (search-forward (substring txt 0 mid) nil t)
                      (search-forward (substring txt mid) nil t))))
        (goto-char (match-beginning 0))
        (when (derived-mode-p 'org-mode) (org-reveal))))))

(declare-function doc-view-goto-page "doc-view")
(declare-function pdf-view-goto-page "ext:pdf-view")

(defun consult-recoll--open-file (filename &optional page)
  "Default function for opening result files."
  (find-file filename)
  (when page
    (cond ((derived-mode-p 'doc-view-mode) (doc-view-goto-page page))
          ((derived-mode-p 'pdf-view-mode) (pdf-view-goto-page page)))))

(defun consult-recoll--open (candidate)
  "Open file of corresponding completion CANDIDATE."
  (when candidate
    (let* ((url (consult-recoll--candidate-url candidate))
           (mime (consult-recoll--candidate-mime candidate))
           (open (cond ((cdr (assoc mime consult-recoll-open-fns)))
                       (consult-recoll-open-fn)
                       ((string= mime "text/html")
                        (lambda (f &optional _ignored) (eww-open-file f)))
                       (t #'consult-recoll--open-file))))
      (if (not consult-recoll-inline-snippets)
          (funcall open url)
        (funcall open url (consult-recoll--candidate-page candidate))
        (when (or (string-prefix-p "text/" mime)
                  (string-prefix-p "message/" mime))
          (consult-recoll--search-snippet candidate mime))))))

(defconst consult-recoll--line-rx
  "^\\(.*?\\)\t\\[\\(.*?\\)\\]\t\\[\\(.*\\)\\]\\(\t\\([0-9]+\\)\\)?"
  "Regular expression decomposing result lines returned by recollq")

(defun consult-recoll--transformer (str)
  "Decode STR, as returned by recollq."
  (cond ((string-match consult-recoll--line-rx str)
         (let* ((mime (match-string 1 str))
                (url (match-string 2 str))
                (title (match-string 3 str))
                (size (match-string 5 str))
                (urln (if (string-prefix-p "file://" url) (substring url 7) url))
                (idx (setq consult-recoll--index (1+ consult-recoll--index)))
                (cand (consult-recoll--format title url mime))
                (cand (propertize cand
                                  'mime-type mime
                                  'url urln
                                  'title title
                                  'index idx
                                  'size size)))
           (push () consult-recoll--snippets)
           (setq consult-recoll--current cand)))
        ((string-match-p "^/?SNIPPETS$" str) nil)
        ((and consult-recoll-inline-snippets consult-recoll--current)
         (when-let* ((page (and (string-match "^\\([0-9]+\\) :" str)
                                (match-string 1 str)))
                     (pageno (and page (string-to-number page)))
                     (props (text-properties-at 0 consult-recoll--current)))
           (apply #'propertize (concat "    " str) 'page pageno props)))
        (consult-recoll--current
         (push str (car consult-recoll--snippets))
         nil)))

(defvar consult-recoll--preview-buffer "*consult-recoll preview*")

(defun consult-recoll--preview (action candidate)
  "Preview search result CANDIDATE when ACTION is \\='preview."
  (cond ((or (eq action 'setup) (null candidate))
         (with-current-buffer (get-buffer-create consult-recoll--preview-buffer)
           (setq-local cursor-in-non-selected-windows nil)
           (delete-region (point-min) (point-max))))
        ((and (eq action 'preview) candidate)
         (when-let* ((url (consult-recoll--candidate-url candidate))
                     (buff (get-buffer consult-recoll--preview-buffer)))
           (with-current-buffer buff
             (delete-region (point-min) (point-max))
             (when-let (title (consult-recoll--candidate-title candidate))
               (insert (propertize title 'face 'consult-recoll-title-face) "\n"))
             (insert (propertize url 'face 'consult-recoll-url-face) "\n")
             (insert (propertize (consult-recoll--candidate-mime candidate)
                                 'face 'consult-recoll-mime-face))
             (when-let (s (consult-recoll--snippets candidate))
               (insert "\n\n" s))
             (goto-char (point-min)))
           (pop-to-buffer buff)))
        ((eq action 'exit)
         (when (get-buffer consult-recoll--preview-buffer)
           (kill-buffer consult-recoll--preview-buffer)))))

(defun consult-recoll--group (candidate transform)
  "If TRANSFORM return candidate, othewise extract mime-type."
  (if transform candidate (consult-recoll--candidate-mime candidate)))

(defun consult-recoll--format-size (bytes)
  "Format the given size with adaptive units."
  (file-size-human-readable (string-to-number bytes) nil " " "B"))

(defun consult-recoll--annotation (candidate)
  "Annotation for the given CANDIDATE (its size by default)"
  (and (not (consult-recoll--candidate-page candidate))
       (format " (%s)" (consult-recoll--format-size
                        (consult-recoll--candidate-size candidate)))))

(defun consult-recoll--search (&optional initial)
  "Perform an asynchronous recoll search via `consult--read'.
If given, use INITIAL as the starting point of the query."
  (consult--read (consult--async-command
                     #'consult-recoll--command
                   (consult--async-filter #'identity)
                   (consult--async-map #'consult-recoll--transformer))
                 :annotate #'consult-recoll--annotation
                 :prompt consult-recoll-prompt
                 :require-match t
                 :lookup #'consult--lookup-member
                 :sort nil
                 :state (and (not consult-recoll-inline-snippets)
                             #'consult-recoll--preview)
                 :group (and consult-recoll-group-by-mime
                             #'consult-recoll--group)
                 :initial (consult--async-split-initial initial)
                 :history '(:input consult-recoll-history)
                 :category 'recoll-result))

;;;###autoload
(defun consult-recoll (ask)
  "Consult recoll's local index.
With prefix argument ASK, the user is prompted for an initial query string."
  (interactive "P")
  (let ((initial (when ask
                   (if (stringp ask) ask (read-string "Initial query: ")))))
    (consult-recoll--open (consult-recoll--search initial))))

;;;###autoload
(defun consult-recoll-embark-setup ()
  "Set up integration with embark.
In particular, allow opening candidates from embark-collect
buffers."
  (add-to-list 'embark-default-action-overrides
               '(recoll-result . consult-recoll--open)))


(provide 'consult-recoll)
;;; consult-recoll.el ends here
