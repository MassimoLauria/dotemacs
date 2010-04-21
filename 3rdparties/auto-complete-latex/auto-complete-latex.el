;;; auto-complete-latex.el --- A LaTeX extention for auto-complete-mode

;; Copyright (C) 2010 tequilasunset

;; Author:   tequilasunset <tequilasunset.mac@gmail.com>
;; Keywords: LaTeX
;; Version:  0.2.3 dev

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Requirements:

;;   auto-complete-mode:
;;
;;     http://cx4a.org/software/auto-complete/
;;

;;; NOTICE:

;; This is development version. Current stable version is 0.2.2.
;;
;;   http://bitbucket.org/tequilasunset/auto-complete-latex/src/84383eb70aa5/
;;

;;; Installation:

;; Put files into your load-path, and add the following into your .emacs.
;;
;;   (require 'auto-complete-latex)
;;   (setq ac-l-dict-directory "/path/to/ac-l-dict/")
;;   (add-to-list 'ac-modes 'foo-mode)
;;   (add-hook 'foo-mode-hook 'ac-l-setup)
;;

;;; Commentary:

;;   When you get the error below
;;
;;     `variable binding depth exceeds max-specpdl-size',
;;
;;   add the following into your .emacs.
;;
;;     (setq max-specpdl-size (+ 500 max-specpdl-size))
;;

;;; History:

;;   http://bitbucket.org/tequilasunset/auto-complete-latex/history/auto-complete-latex.el

;;; Code:

(require 'cl)
(require 'auto-complete)

(defgroup auto-complete-latex nil
  "Auto completion of LaTeX keywords."
  :group 'auto-complete
  :group 'tex
  :prefix "ac-l-")


;;;; variables
(defcustom ac-l-update-delay 0.8
  "Delay to update candidates."
  :type 'float
  :group 'auto-complete-latex)

(defcustom ac-l-master-file nil
  "Specify LaTeX master file path as string.
If valid file path is specified, parse master file's \\input and
\\include, and create candidates from master file and parsed files."
  :type 'string
  :group 'auto-complete-latex)
(defvaralias 'ac-l-target 'ac-l-master-file)

(defcustom ac-l-sources nil
  "A list of user sources.
If you want to add sources defined in other elisp, use this list
instead of `ac-sources'."
  :type '(repeat symbol)
  :group 'auto-complete-latex)

(defcustom ac-l-package-files nil
  "A list of package files (valid suffixes are .sty and .cls).
If valid file path is specified, parse LaTeX command definitions
in it, and create candidates."
  :type '(repeat string)
  :group 'auto-complete-latex)

(defcustom ac-l-bib-files nil
  "A list of bib files (valid suffix is .bib).
If valid file path is specified, parse bibliography keys in it,
and create candidates."
  :type '(repeat string)
  :group 'auto-complete-latex)

(defcustom ac-l-use-word-completion nil
  "If non-nil, use sources for normal word completion."
  :type 'boolean
  :group 'auto-complete-latex)

;;; internal
(defconst ac-l-command-prefix "\\\\\\([a-zA-Z@]+\\)"
  "Prefix property of sources for LaTeX commands.")

(defvar ac-l-major-mode nil
  "Major mode which Auto Complete LaTeX is working.")

(defvar ac-l-update-timer nil
  "Timer for `ac-l-update-all'.")

(defconst ac-l-packages (make-hash-table :test 'equal)
  "Hash table. k -> package name in `ac-l-package-files', v -> [cmds args]")


;;;; functions

(defsubst ac-l-master-p ()
  "Return non-nil, if `ac-l-master-file' is valid. If not, return nil."
  (if (and (stringp ac-l-master-file)
           (file-exists-p ac-l-master-file))
      t))

(defsubst ac-l-completions (candidates)
  (all-completions ac-prefix `,candidates))

(defmacro ac-l-pushnew-1 (x place)
  "(pushnew x place :test 'eq)"
  `(if (memq ,x ,place) ,place (setq ,place (cons ,x ,place))))

(defmacro ac-l-pushnew-2 (x place)
  "(pushnew x place :test 'equal)"
  `(if (member ,x ,place) ,place (setq ,place (cons ,x ,place))))

;;; define internal array and accessors
(defconst ac-l--dummy (make-vector 16 nil))

(defsubst ac-l-get-package-commands      () (aref ac-l--dummy 0))
(defsubst ac-l-get-package-arguments     () (aref ac-l--dummy 1))
(defsubst ac-l-get-current-bib-tables    () (aref ac-l--dummy 2))
(defsubst ac-l-get-all-bib-tables        () (aref ac-l--dummy 3))
(defsubst ac-l-get-latex-commands        () (aref ac-l--dummy 4))
(defsubst ac-l-get-latex-arguments       () (aref ac-l--dummy 5))
(defsubst ac-l-get-package-sources       () (aref ac-l--dummy 6))
(defsubst ac-l-get-user-noprefix-sources () (aref ac-l--dummy 7))
(defsubst ac-l-get-user-prefix-sources   () (aref ac-l--dummy 8))
(defsubst ac-l-get-label-cands           () (aref ac-l--dummy 9))
(defsubst ac-l-get-bibitem-cands         () (aref ac-l--dummy 10))
(defsubst ac-l-get-bib-cands             () (aref ac-l--dummy 11))
(defsubst ac-l-get-buffer-sources        () (aref ac-l--dummy 12))
(defsubst ac-l-get-filenames             () (aref ac-l--dummy 13))
(defsubst ac-l-get-label-tables          () (aref ac-l--dummy 14))
(defsubst ac-l-get-bibitem-tables        () (aref ac-l--dummy 15))

(defsubst ac-l-set-package-commands      (newelt) (aset ac-l--dummy 0 newelt))
(defsubst ac-l-set-package-arguments     (newelt) (aset ac-l--dummy 1 newelt))
(defsubst ac-l-set-current-bib-tables    (newelt) (aset ac-l--dummy 2 newelt))
(defsubst ac-l-set-all-bib-tables        (newelt) (aset ac-l--dummy 3 newelt))
(defsubst ac-l-set-latex-commands        (newelt) (aset ac-l--dummy 4 newelt))
(defsubst ac-l-set-latex-arguments       (newelt) (aset ac-l--dummy 5 newelt))
(defsubst ac-l-set-package-sources       (newelt) (aset ac-l--dummy 6 newelt))
(defsubst ac-l-set-user-noprefix-sources (newelt) (aset ac-l--dummy 7 newelt))
(defsubst ac-l-set-user-prefix-sources   (newelt) (aset ac-l--dummy 8 newelt))
(defsubst ac-l-set-label-cands           (newelt) (aset ac-l--dummy 9 newelt))
(defsubst ac-l-set-bibitem-cands         (newelt) (aset ac-l--dummy 10 newelt))
(defsubst ac-l-set-bib-cands             (newelt) (aset ac-l--dummy 11 newelt))
(defsubst ac-l-set-buffer-sources        (newelt) (aset ac-l--dummy 12 newelt))
(defsubst ac-l-set-filenames             (newelt) (aset ac-l--dummy 13 newelt))
(defsubst ac-l-set-label-tables          (newelt) (aset ac-l--dummy 14 newelt))
(defsubst ac-l-set-bibitem-tables        (newelt) (aset ac-l--dummy 15 newelt))

;;; prefix properties
(defconst ac-l-argument-regexps
  '("usepackage"
    "RequirePackage"
    "documentclass"
    "begin"
    "end"
    "\\(?:this\\)?pagestyle"
    "bibliography\\(?:style\\)?"
    "pagenumbering"
    "\\(?:new\\|addto\\|set\\)counter"
    "[aA]lph"
    "arabic"
    "[rR]oman"
    "fnsymbol"
    "addcontentsline"
    "@addtoreset"
    "@startsection"
    "@namedef"
    "@definecounter"
    ;; amsmath
    "numberwithin"
    ;; color
    "\\(?:text\\|page\\|f\\|define\\)color"
    "colorbox"
    ;; colortbl
    "\\(?:column\\|row\\|cell\\|arrayrule\\|doublerulesep\\)color"
    ;; hyperref
    "hypersetup"
    ;; graphicx
    "includegraphics"
    ;; beamer
    "insert[a-z]+"
    "resetcount\\(?:er\\)?onoverlays"
    "frame"
    "tableofcontents"
    "movie"
    "hyperlinkmovie"
    "multiinclude"
    "sound"
    "hyperlinksound"
    "trans[a-z]+"
    "use[a-z]*theme"
    "ifbeamertemplateempty"
    "expandbeamertemplate"
    "defbeamertemplateparent"
    "\\(?:use\\|set\\)beamer\\(?:color\\|font\\)"
    "setbeamercovered"
    "note"
    "includeslide"
    ;; listings
    "lst[a-zDIMS]+")
  "A list of regexps to match commands involved with arguments.")

(defconst ac-l-file-regexps
  '("include\\(?:only\\)?"
    "input"
    ;; hyperref
    "hypersetup"
    ;; graphicx
    "includegraphics")
  "A list of regexps to match commands involved with file name argument.")

(defconst ac-l-label-regexps
  '("\\(?:page\\|auto\\|eq\\)?ref"
    "label")
  "A list of regexps to match commands involved with label name argument.")

(defconst ac-l-bib-regexps
  '("\\(?:no\\|short\\)?cite[a-zA-Z]*"
    "bibitem")
  "A list of regexps to match commands involved with bibliography argument.")

(defun ac-l-prefix-in-paren (regexps)
  (if (looking-back (concat "\\\\\\("
                            (mapconcat 'identity regexps "\\|")
                            "\\)\\*?\\(\\s([^]>}]*\\s)\\)*\\(\\s([^]>}]*\\)"))
      ac-point))

;;; read file data
(defun ac-l-convert-filename-to-file (filename)
  (let ((file-nodir (file-name-nondirectory filename)))
    (if (string-match "^[^.]+" file-nodir)
        (match-string 0 file-nodir)
      file-nodir)))

(defun ac-l-read-bibs (files)
  (let ((regexp "^@[^{@]+{\\([^ =,\t\n]*\\),\n[^@]+\\(^}\\)"))
    (dolist (filename files)
      (let* ((file (ac-l-convert-filename-to-file filename))
             (name (intern (format "ac-l-%s-table" file)))
             (table (make-hash-table :test 'equal)))
        (set name table)
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents filename)
            (while (re-search-forward regexp nil t)
              (puthash (match-string-no-properties 1)
                       (match-string-no-properties 0)
                       table))))
        (ac-l-set-all-bib-tables `(,@(ac-l-get-all-bib-tables) ,name))
        (ac-l-set-filenames `(,@(ac-l-get-filenames) ,file))))))

(defun ac-l-read-packages (files table)
  (let ((cmd-re "\\\\\\(?:[a-z@]*def\\|let\\|new[a-z]+\\|providecommand\\|Declare[a-zA-Z@]+\\)\\*?[ \t]*{?\\\\\\([a-zA-Z]+\\)}?[ =\\#[{]")
        (arg-re  "\\\\\\(?:DeclareOption[a-zA-Z]*\\|new[a-z]+\\|@definecounter\\)\\*?[ \t]*{\\([a-zA-Z]+\\)}"))
    (dolist (filename files)
      (let ((file (ac-l-convert-filename-to-file filename))
            candidate commands arguments)
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents filename)
            (while (re-search-forward cmd-re nil t)
              (setq candidate (match-string-no-properties 1))
              (ac-l-pushnew-2 candidate commands))
            (goto-char (point-min))
            (while (re-search-forward arg-re nil t)
              (setq candidate (match-string-no-properties 1))
              (ac-l-pushnew-2 candidate arguments))))
        (puthash file (vector commands arguments) table)
        (ac-l-set-filenames `(,@(ac-l-get-filenames) ,file))))))

(defun ac-l-write-package-files (dir)
  "Write files listed in `ac-l-package-files' into DIR."
  (interactive
   (list (read-directory-name "Directory: " ac-l-dict-directory nil t)))
  (maphash (lambda (k v)
             (dolist (pair `((c . ,(aref v 0)) (a . ,(aref v 1))))
               (when (cdr pair)
                 (with-temp-buffer
                   (insert
                    (mapconcat 'identity (sort (cdr pair) #'string<) "\n"))
                   (write-region (point-min) (point-max)
                                 (format "%s%s-%s-*-*" dir k (car pair)))))))
           ac-l-packages))

(defcustom ac-l-dict-directory "~/.emacs.d/ac-l-dict/"
  "Path of the ac-l-dict.
Make sources from files in this directory. If you want to add files,
see `ac-l-make-source-from-dir' and `ac-l-write-package-files'."
  :type 'string
  :group 'auto-complete-latex)

(defconst ac-l-package-dependences
  (let ((table (make-hash-table :test 'equal))
        (alist '(("hyperref" . "beamer")
                 ("color" . "colortbl\\|beamer")
                 ("array" . "tabularx\\|colortbl"))))
    (loop for (k . v) in alist do (puthash k v table))
    table)
  "Hash table. k -> required package name, v -> package names (regexp)

KEY package is required in VALUE packages. For example, array.sty is
required in tabularx.sty and colortbl.sty. So, KEY and VALUE must be
like below.

KEY    =>  \"array\"
VALUE  =>  \"tabularx\\\\|colortbl\"

Package and class files are treated equivalently. Below code indicates
that candidates of amsmath.sty will be shown when beamer class is loaded.

   \(puthash \"amsmath\" \"beamer\" ac-l-package-dependences)")

(defun ac-l-make-source-from-dir (dir)
  "Make source from files in DIR.

file name form:  NAME-TYPE-SYMBOL-REQUIRES

NAME      Package or class file name.
          For example, you set NAME to foo, the source is included in
          `ac-sources' while \\usepackage{foo}, \\RequirePackage{foo} or
          \\documentclass{foo} is written in documents. If you want to
          set package dependence, use `ac-l-package-dependences'.

TYPE      `c' (command) or `a' (argument).

SYMBOL    Symbol property. `*' => `p'.

REQUIRES  Requires property. `*' => not set."
  (let* ((files (directory-files dir nil "^[^.]"))
         (help-fn (cond
                   ((member "YATEXHLP.eng" files)
                    'ac-l-yatex-eng-documentation)
                   ((member "YATEXHLP.jp" files)
                    'ac-l-yatex-jp-documentation)
                   ((member "latex-help" files)
                    'ac-l-latex2e-documentation)
                   (t nil)))
         (help (if (and help-fn (require 'mule-util nil t)) t)))
    (dolist (file files)
      (let ((symbol "p")
            (prefix ac-l-command-prefix)
            source package req latex-c latex-a)
        ;; parse properties from file name
        (cond
         ((string-match "^\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)$" file)
          (let* ((p (match-string 1 file))
                 (T (match-string 2 file))
                 (s (match-string 3 file))
                 (r (match-string 4 file))
                 (v (gethash p ac-l-package-dependences))
                 (filenames (ac-l-get-filenames)))
            (unless (member p filenames)
              (ac-l-set-filenames (append filenames (list p))))
            (if v (setq package (concat p "\\|" v)) (setq package p))
            (unless (string= s "*") (setq symbol s))
            (unless (string= r "*") (setq req (string-to-number r)))
            (if (string= T "a")
                (setq prefix 'ac-l-argument
                      source (intern (format "ac-l-source-%s-arguments" p)))
              (setq source (intern (format "ac-l-source-%s-commands" p))))))
         ((or (string= "macro" file)
              (string= "latex-dot-ltx" file)
              (string-match "^\\(ptex-\\)?primitives$" file)
              (string-match "^\\(basic\\|platex\\)-commands$" file))
          (setq latex-c t))
         ((string-match "^\\(basic\\|platex\\)-arguments$" file)
          (setq latex-a t))
         ((cond
           ((string= "user-commands" file)
            (setq symbol "u"))
           ((string= "user-arguments" file)
            (setq symbol "u"
                  prefix 'ac-l-argument)))
          (setq source (intern (format "ac-l-source-%s" file)))))
        ;; read file contents, and define package and user sources
        (when (or source latex-c latex-a)
          (let ((candidates (with-temp-buffer
                              (insert-file-contents (concat dir file))
                              (split-string (buffer-string) "\n"))))
            (unless (equal candidates '(""))
              (cond
               (source
                (set source
                     (delq nil
                           (list (if package (cons 'ac-l-package package))
                                 (if (integerp req) (cons 'requires req))
                                 (cons 'symbol symbol)
                                 (cons 'prefix prefix)
                                 (cons 'candidates
                                       `(ac-l-completions ',candidates)))))
                (cond
                 (package
                  (ac-l-set-package-sources
                   `(,@(ac-l-get-package-sources) ,source)))
                 ((string= symbol "u")
                  (ac-l-set-user-prefix-sources
                   `(,@(ac-l-get-user-prefix-sources) ,source)))))
               (latex-c
                (ac-l-set-latex-commands
                 (append (ac-l-get-latex-commands) candidates)))
               (latex-a
                (ac-l-set-latex-arguments
                 (append (ac-l-get-latex-arguments) candidates)))))))))
    ;; define basic sources
    (when (ac-l-get-latex-commands)
      (defvar ac-l-source-latex-commands
        (delq nil
              `((symbol . "l")
                ,(if help (cons 'document help-fn))
                (prefix . ,ac-l-command-prefix)
                (candidates . (ac-l-completions (ac-l-get-latex-commands)))))))
    (when (ac-l-get-latex-arguments)
      (defvar ac-l-source-latex-arguments
        (delq nil
              `((symbol . "l")
                ,(if help (cons 'document help-fn))
                (prefix . ac-l-argument)
                (candidates . (ac-l-completions (ac-l-get-latex-arguments)))))))))

;;; structure
(defstruct ac-l-info
  "Information about each tex file."
  filename                              ; file path
  modification                          ; file modification
  words                                 ;
  commands                              ;
  packages                              ; package & class names
  labels                                ; ht: k -> label, v -> help
  bibitems                              ; ht: k -> bibitem, v -> help
  bibs)                                 ; bib file names

(defun ac-l-candidates-hash (regexp table beg end)
  (goto-char beg)
  (while (re-search-forward regexp end t)
    (puthash (match-string-no-properties 1)
             (match-string-no-properties 0)
             table)
    (goto-char (1+ (match-beginning 0)))))

(defun ac-l-make-info (file filename &optional master)
  (let* ((word-re "[^\\,]\\(\\<[-'a-zA-Z]+\\>\\)")
         (package-re "^[^%\n]*\\\\\\(?:\\(?:usep\\|RequireP\\)ackage\\|documentclass\\)\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)")
         (label-re "\\\\label{\\(\\(?:[^ }\t\n]\\)+\\)}")
         (label-re-1 (concat "^[^%\n]*" label-re ".*$"))
         (label-re-2 (concat "^.*\n.*\n.*\n[^%\n]*" label-re ".*\n.*\n.*\n.*$"))
         (bibitem-re "^[^%\n]*\\\\bibitem\\(?:\\[[^]]*\\]\\)?{\\(\\(?:[^ }\t\n]\\)+\\)}[^\\]*")
         (bib-re "^[^%\n]*\\\\bibliography{\\([^}]+\\)")
         (split-fn (lambda (str) (split-string str "[ %,\t\n]+" t)))
         (collect-p (and (ac-l-master-p)
                         (not master)
                         (not (buffer-file-name))))
         (beg (point-min))
         (label-beg (save-excursion (goto-char beg)
                                    (forward-line 3)
                                    (point)))
         (label-end (save-excursion (goto-char (point-max))
                                    (forward-line -3)
                                    (point)))
         (label-table (or (ignore-errors
                            (ac-l-info-labels (symbol-value file)))
                          (make-hash-table :test 'equal)))
         (bibitem-table (or (ignore-errors
                              (ac-l-info-bibitems (symbol-value file)))
                            (make-hash-table :test 'equal)))
         i candidate candidates)
    (clrhash label-table)
    (clrhash bibitem-table)
    (save-excursion
      (make-ac-l-info
       :filename filename
       :modification (ignore-errors (nth 5 (file-attributes filename)))
       :words (when (and ac-l-use-word-completion collect-p)
                (setq i 0 candidates nil)
                (goto-char beg)
                (while (and (re-search-forward word-re nil t) (<= i 100))
                  (setq candidate (match-string-no-properties 1))
                  (when (and (not (member candidate candidates))
                             (>= (length candidate) 3))
                    (push candidate candidates)
                    (incf i)))
                candidates)
       :commands (when collect-p
                   (let ((latex-commands (ac-l-get-latex-commands)))
                     (setq i 0 candidates nil)
                     (goto-char beg)
                     (while (and (re-search-forward ac-l-command-prefix nil t)
                                 (<= i 100))
                       (setq candidate (match-string-no-properties 1))
                       (unless (or (member candidate candidates)
                                   (member candidate latex-commands))
                         (push candidate candidates)
                         (incf i))))
                   candidates)
       :packages (when master
                   (setq candidates nil)
                   (goto-char beg)
                   (while (re-search-forward package-re nil t)
                     (dolist (name (funcall split-fn
                                            (match-string-no-properties 1)))
                       (ac-l-pushnew-2 name candidates)))
                   candidates)
       :labels (progn
                 (ac-l-candidates-hash label-re-1 label-table beg label-beg)
                 (ac-l-candidates-hash label-re-2 label-table label-beg nil)
                 (ac-l-candidates-hash label-re-1 label-table label-end nil)
                 label-table)
       :bibitems (progn
                   (ac-l-candidates-hash bibitem-re bibitem-table beg nil)
                   bibitem-table)
       :bibs (progn
               (setq candidates nil)
               (goto-char beg)
               (when (re-search-forward bib-re nil t)
                 (dolist (name (funcall split-fn (match-string-no-properties 1)))
                   (ac-l-pushnew-2 name candidates)))
               candidates)))))

;;; update file's info
(defconst ac-l-children (make-hash-table :test 'equal)
  "Hash table. k -> filename (full path), v -> file (cl struct)")

(defvar ac-l-master nil
  "Internal variable (cl struct).
Are you looking for `ac-l-master-file'?")

(defsubst ac-l-all-files ()
  (append (loop for v being the hash-values in ac-l-children collect v)
          '(ac-l-master)))

(defun ac-l-append-info (function)
  (loop for file in (ac-l-all-files)
        append (funcall function (symbol-value file)) into elts
        finally return (nreverse elts)))

(defun ac-l-collect-info (function)
  (loop for file in (ac-l-all-files)
        collect (funcall function (symbol-value file))))

(defun ac-l-update-children-names ()
  "Update `ac-l-children'."
  (let ((regexp "^[^%\n]*\\\\\\(?:input\\|include\\)[ {\t]+\\([^ }%\n]+\\)")
        (dir (if (string-match "^\\(.+/\\).+$" ac-l-master-file)
                 (match-string 1 ac-l-master-file)
               "/"))
        (i 0))
    (clrhash ac-l-children)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let* ((path (expand-file-name (match-string-no-properties 1) dir))
               (filename (concat (if (string-match "^\\(.+\\)\\.[^./]+$" path)
                                     (match-string 1 path)
                                   path)
                                 ".tex")))
          (when (and (file-exists-p filename)
                     (not (gethash filename ac-l-children)))
            (puthash filename
                     (intern (format "ac-l-child-%d" i))
                     ac-l-children)
            (incf i)))))))

(defsubst ac-l-file-mod-p (file filename)
  (if (not (equal (ac-l-info-modification (symbol-value file))
                  (nth 5 (file-attributes filename))))
      t))

(defsubst ac-l-same-file-p (file filename)
  (if (string= (ac-l-info-filename (symbol-value file)) filename) t))

(defun ac-l-update-info (&optional force)
  (if (ac-l-master-p)
      (let ((master-mod-p (or force
                              (ac-l-file-mod-p 'ac-l-master ac-l-master-file))))
        ;; master
        (or (loop with master = (expand-file-name ac-l-master-file)
                  for buf in (buffer-list)
                  if (string= master (buffer-file-name buf))
                  do
                  (when (or master-mod-p (buffer-modified-p buf))
                    (with-current-buffer buf
                      (ac-l-update-children-names)
                      (setq ac-l-master
                            (ac-l-make-info 'ac-l-master ac-l-master-file t))))
                  and return t
                  finally return nil)
            (when master-mod-p
              (with-temp-buffer
                (insert-file-contents ac-l-master-file)
                (ac-l-update-children-names)
                (setq ac-l-master
                      (ac-l-make-info 'ac-l-master ac-l-master-file t)))))
        ;; children
        (let ((table (copy-hash-table ac-l-children)))
          (dolist (buf (buffer-list))
            (let* ((filename (buffer-file-name buf))
                   (file (gethash filename table)))
              (when (and file
                         (or force
                             (buffer-modified-p buf)
                             (not (ac-l-same-file-p file filename))
                             (ac-l-file-mod-p file filename)))
                (with-current-buffer buf
                  (set file (ac-l-make-info file filename)))
                (remhash filename table))))
          (maphash (lambda (filename file)
                     (when (or force
                               (not (ac-l-same-file-p file filename))
                               (ac-l-file-mod-p file filename))
                       (with-temp-buffer
                         (insert-file-contents filename)
                         (set file (ac-l-make-info file filename)))))
                   table)))
    (when (or force (buffer-modified-p))
      (setq ac-l-master (ac-l-make-info 'ac-l-master (buffer-file-name) t)))))

;;; update
(defun ac-l-update ()
  "Update `ac-sources'."
  (let (p-c-sources p-a-sources)
    (ac-l-set-package-commands nil)
    (ac-l-set-package-arguments nil)
    ;; parse
    (dolist (name (ac-l-info-packages ac-l-master))
      (dolist (source (ac-l-get-package-sources))
        (let* ((alist (symbol-value source))
               (package (cdr (assq 'ac-l-package alist)))
               (prefix (cdr (assq 'prefix alist))))
          (when (string-match package name)
            (cond
             ((string= prefix ac-l-command-prefix)
              (ac-l-pushnew-1 source p-c-sources))
             ((eq prefix 'ac-l-argument)
              (ac-l-pushnew-1 source p-a-sources))))))
      ;; package candidates
      (let* ((v (gethash name ac-l-packages))
             (cmds (elt v 0))
             (args (elt v 1)))
        (when cmds
          (ac-l-set-package-commands (append (ac-l-get-package-commands) cmds)))
        (when args
          (ac-l-set-package-arguments (append (ac-l-get-package-arguments) args)))))
    ;; set
    (setq ac-sources
          (delq nil
                (append (ac-l-get-user-prefix-sources)
                        `(ac-source-filename
                          ac-l-source-labels
                          ac-l-source-bibitems
                          ,(when ac-l-bib-files
                             'ac-l-source-bibliographies)
                          ,(when (boundp 'ac-l-source-latex-commands)
                             'ac-l-source-latex-commands))
                        (nreverse p-c-sources)
                        `(,(when ac-l-package-files
                             'ac-l-source-package-commands)
                          ,(cdr (ac-l-get-buffer-sources))
                          ,(when (ac-l-master-p)
                             'ac-l-source-commands-in-files)
                          ,(when (boundp 'ac-l-source-latex-arguments)
                             'ac-l-source-latex-arguments))
                        (nreverse p-a-sources)
                        `(,(when ac-l-package-files
                             'ac-l-source-package-arguments)
                          ac-l-source-filenames
                          ac-source-files-in-current-dir)
                        ;; without prefix
                        (ac-l-get-user-noprefix-sources)
                        `(,(when ac-l-use-word-completion
                             (car (ac-l-get-buffer-sources)))
                          ,(when (and (ac-l-master-p)
                                      ac-l-use-word-completion)
                             'ac-l-source-words-in-files)
                          ac-source-dictionary))))))

(defun ac-l-update-bib ()
  "Update bib tables."
  (ac-l-set-current-bib-tables nil)
  (dolist (name (ac-l-append-info 'ac-l-info-bibs))
    (dolist (table (ac-l-get-all-bib-tables))
      (when (and (string= (format "ac-l-%s-table" name)
                          (symbol-name table))
                 (not (memq table (ac-l-get-current-bib-tables))))
        (ac-l-set-current-bib-tables
         `(,@(ac-l-get-current-bib-tables) ,table))))))

;;; candidate
;;; copied from auto-complete.el and add arguments
(defun ac-l-candidate (beg-regexp end-regexp)
  (let ((i 0)
        (regexp (concat beg-regexp (regexp-quote ac-prefix) end-regexp))
        candidates)
    (save-excursion
      ;; Search backward
      (goto-char ac-point)
      (while (and (or (not (integerp ac-limit)) (< i ac-limit))
                  (re-search-backward regexp nil t))
        (let ((candidate (match-string-no-properties 1)))
          (unless (member candidate candidates)
            (push candidate candidates)
            (incf i))))
      ;; Search backward
      (goto-char (+ ac-point (length ac-prefix)))
      (while (and (or (not (integerp ac-limit)) (< i ac-limit))
                  (re-search-forward regexp nil t))
        (let ((candidate (match-string-no-properties 1)))
          (unless (member candidate candidates)
            (push candidate candidates)
            (incf i))))
      (nreverse candidates))))

(defun ac-l-incremental-update-index (index function)
  (let ((pair (symbol-value index))
        (ac-limit (or (and (integerp ac-limit) ac-limit) 10)))
    (when (null pair)
      (set index (cons nil nil)))
    ;; Mark incomplete
    (when (car pair)
      (setcar pair nil))
    (let ((list (cdr pair))
          (words (funcall function)))
      (dolist (word words)
        (unless (member word list)
          (push word list)
          (setcdr pair list))))))

(defun ac-l-update-index (index function &optional same-mode)
  (dolist (buf (buffer-list))
    (when (and (if same-mode
                   (eq ac-l-major-mode (buffer-local-value 'major-mode buf))
                 t)
               (or ac-fuzzy-enable
                   (not (eq buf (current-buffer)))))
      (with-current-buffer buf
        (when (and (not (car (symbol-value index)))
                   (< (buffer-size) 1048576))
          ;; Complete index
          (set index (cons t (let ((ac-point (point-min))
                                   (ac-prefix "")
                                   ac-limit)
                               (funcall function)))))))))

(defun ac-l-candidates (index function &optional same-mode)
  (loop initially (unless ac-fuzzy-enable
                    (ac-l-incremental-update-index index function))
        for buf in (buffer-list)
        if (and (or (not (integerp ac-limit))
                    (< (length candidates) ac-limit))
                (if same-mode
                    (derived-mode-p (buffer-local-value 'major-mode buf))
                  t))
        append (funcall ac-match-function
                        ac-prefix
                        (cdr (buffer-local-value index buf)))
        into candidates
        finally return candidates))


;;;; sources

;;; words
(defvar ac-l-word-index nil)
(make-variable-buffer-local 'ac-l-word-index)

(defsubst ac-l-candidate-words-in-buffer ()
  (ac-l-candidate "[^\\,]\\(\\<" "[-'a-zA-Z]+\\>\\)"))

;; Meadow/Emacs memo: http://www.bookshelf.jp/soft/meadow_34.html#SEC495
(defun ac-l-smart-capitalize ()
  (when (and (looking-back "[[:space:][:cntrl:]]+[a-z']+")
             (= (point) (save-excursion
                          (backward-sentence)
                          (forward-word)
                          (point))))
    (capitalize-word -1)))

(defvar ac-l-source-words-in-buffer
  '((candidates . (ac-l-candidates 'ac-l-word-index
                                   'ac-l-candidate-words-in-buffer))
    (action . ac-l-smart-capitalize)
    (requires . 3))
  "Source for words in current buffer.")

(defvar ac-l-source-words-in-all-buffer
  '((init . (ac-l-update-index 'ac-l-word-index
                               'ac-l-candidate-words-in-buffer))
    (candidates . (ac-l-candidates 'ac-l-word-index
                                   'ac-l-candidate-words-in-buffer))
    (action . ac-l-smart-capitalize)
    (requires . 3))
  "Source for words in all buffers.")

(defvar ac-l-source-words-in-same-mode-buffers
  '((init . (ac-l-update-index 'ac-l-word-index
                               'ac-l-candidate-words-in-buffer
                               t))
    (candidates . (ac-l-candidates 'ac-l-word-index
                                   'ac-l-candidate-words-in-buffer
                                   t))
    (action . ac-l-smart-capitalize)
    (requires . 3))
  "Source for words in same mode buffers.")

(defvar ac-l-source-words-in-files
  '((candidates . (ac-l-completions (ac-l-append-info 'ac-l-info-words)))
    (action . ac-l-smart-capitalize)
    (requires . 3))
  "Source for words in tex files.")

;;; commands
(defvar ac-l-command-index nil)
(make-variable-buffer-local 'ac-l-command-index)

(defsubst ac-l-candidate-commands-in-buffer ()
  (ac-l-candidate "\\\\\\(" "[a-zA-Z@]+\\)"))

(defvar ac-l-source-commands-in-buffer
  `((candidates . (ac-l-candidates 'ac-l-command-index
                                   'ac-l-candidate-commands-in-buffer))
    (prefix . ,ac-l-command-prefix))
  "Source for commands in current buffer.")

(defvar ac-l-source-commands-in-all-buffer
  `((init . (ac-l-update-index 'ac-l-command-index
                               'ac-l-candidate-commands-in-buffer))
    (candidates . (ac-l-candidates 'ac-l-command-index
                                   'ac-l-candidate-commands-in-buffer))
    (prefix . ,ac-l-command-prefix))
  "Source for commands in all buffers.")

(defvar ac-l-source-commands-in-same-mode-buffers
  `((init . (ac-l-update-index 'ac-l-command-index
                               'ac-l-candidate-commands-in-buffer
                               t))
    (candidates . (ac-l-candidates 'ac-l-command-index
                                   'ac-l-candidate-commands-in-buffer
                                   t))
    (prefix . ,ac-l-command-prefix))
  "Source for commands in same mode buffers.")

(defvar ac-l-source-commands-in-files
  `((candidates . (ac-l-completions (ac-l-append-info 'ac-l-info-commands)))
    (prefix . ,ac-l-command-prefix))
  "Source for commands in tex files.")

;;; etc
(defvar ac-l-source-filenames
  '((candidates . (ac-l-completions (ac-l-get-filenames)))
    (prefix . ac-l-argument))
  "Source for names of packages and bib files.")

(defvar ac-l-source-package-commands
  `((candidates . (ac-l-completions (ac-l-get-package-commands)))
    (prefix . ,ac-l-command-prefix)
    (symbol . "p"))
  "Source for commands in `ac-l-package-files'.")

(defvar ac-l-source-package-arguments
  '((candidates . (ac-l-completions (ac-l-get-package-arguments)))
    (prefix . ac-l-argument)
    (symbol . "p"))
  "Source for arguments in `ac-l-package-files'.")

(defsubst ac-l-thereis-hash-value (key tables)
  (loop for table in tables
        thereis (gethash key (if (hash-table-p table) table (symbol-value table)))))

(defvar ac-l-source-labels
  '((init . (ac-l-set-label-cands
             (loop for table in (ac-l-set-label-tables
                                 (ac-l-collect-info 'ac-l-info-labels))
                   append (loop for k being the hash-keys in table collect k)
                   into candidates
                   finally return
                   (if (looking-back "\\\\label{\\([^}]+\\)")
                       (let ((m (match-beginning 1)))
                         (delete (buffer-substring-no-properties m (1+ m))
                                 candidates))
                     candidates))))
    (candidates . (ac-l-completions (ac-l-get-label-cands)))
    (prefix . ac-l-label)
    (document . (lambda (k) (ac-l-thereis-hash-value k (ac-l-get-label-tables))))
    (symbol . "L"))
  "Source for labels.")

(defun ac-l-complete-labels ()
  "Start label name completion at point."
  (interactive)
  (auto-complete
   (list (remove '(prefix . ac-l-label) ac-l-source-labels))))

(defvar ac-l-source-bibitems
  '((init . (ac-l-set-bibitem-cands
             (loop for table in (ac-l-set-bibitem-tables
                                 (ac-l-collect-info 'ac-l-info-bibitems))
                   append (loop for k being the hash-keys in table collect k)
                   into candidates
                   finally return
                   (if (looking-back "\\\\bibitem\\(\\[[^]]*\\]\\)?{\\([^}]+\\)")
                       (let ((m (match-beginning 2)))
                         (delete (buffer-substring-no-properties m (1+ m))
                                 candidates))
                     candidates))))
    (candidates . (ac-l-completions (ac-l-get-bibitem-cands)))
    (prefix . ac-l-bib)
    (document . (lambda (k) (ac-l-thereis-hash-value k (ac-l-get-bibitem-tables))))
    (symbol . "B"))
  "Source for bibitems.")

(defvar ac-l-source-bibliographies
  '((init . (ac-l-set-bib-cands
             (loop for table in (ac-l-get-current-bib-tables)
                   append (loop for k being the hash-keys in (symbol-value table)
                                collect k)
                   into candidates
                   finally return candidates)))
    (candidates . (ac-l-completions (ac-l-get-bib-cands)))
    (prefix . ac-l-bib)
    (document . (lambda (k) (ac-l-thereis-hash-value k (ac-l-get-current-bib-tables))))
    (symbol . "B"))
  "Source for bibliographies in `ac-l-bib-files'.")

(defun ac-l-complete-bibs ()
  "Start bibliography completion at point."
  (interactive)
  (auto-complete
   (list (remove '(prefix . ac-l-bib) ac-l-source-bibitems)
         (remove '(prefix . ac-l-bib) ac-l-source-bibliographies))))

;;; help
(defconst ac-l-help (make-hash-table :test 'equal)
  "Hash table. k -> keyword, v -> quick help contents")

(defmacro ac-l-define-help-doc (name file beg-regexp end-regexp)
  (declare (indent 1))
  `(defun ,(intern (format "ac-l-%s-documentation" name)) (str)
     (or (gethash str ac-l-help)
         (unless (string-match "@" str)
           (with-temp-buffer
             (insert-file-contents (concat ac-l-dict-directory ,file))
             (if (re-search-forward (concat ,beg-regexp str ,end-regexp) nil t)
                 (puthash str (match-string-no-properties 1) ac-l-help)
               (puthash str t ac-l-help)))))))

(ac-l-define-help-doc latex2e
  "latex-help"
  "\\(?:\n\\)\\([^]*\\(?:^[`\\]"
  "\\(?:\\s(\\|[ '\n]\\)[^]+\\)\\)")

(ac-l-define-help-doc yatex-jp
  "YATEXHLP.jp"
  "^\\(\\\\?"
  "\n[^]+\\)")

(ac-l-define-help-doc yatex-eng
  "YATEXHLP.eng"
  "^\\(\\\\?"
  "\n[^]+\\)")


;;;; clear

(defvar ac-l-clear-timer nil
  "Timer for `ac-l-clear'.")

(defun ac-l-clear ()
  (clrhash ac-l-help))

(defun ac-l-clear-all ()
  (interactive)
  (ac-l-clear)
  (ac-l-cancel-timer)
  (clrhash ac-l-packages)
  (dolist (file (append (ac-l-all-files)
                        (ac-l-get-all-bib-tables)
                        (ac-l-get-package-sources)))
    (let ((v (symbol-value file)))
      (if (hash-table-p v) (clrhash v))
      (if (arrayp v) (fillarray v nil))
      (set file nil)))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq ac-l-word-index nil
            ac-l-command-index nil)))
  (clrhash ac-l-children)
  (fillarray ac-l--dummy nil)
  (when (boundp 'ac-l-source-user-commands)
    (setq ac-l-source-user-commands nil))
  (when (boundp 'ac-l-source-user-arguments)
    (setq ac-l-source-user-arguments nil))
  (setq ac-l-major-mode nil))

(defun ac-l-cancel-timer ()
  (interactive)
  (dolist (timer '(ac-l-update-timer
                   ac-l-clear-timer))
    (let ((T (symbol-value timer)))
      (when (timerp T)
        (cancel-timer T)
        (set timer nil)))))


;;;; setup

(defun ac-l-update-all (&optional force)
  (when (eq ac-l-major-mode major-mode)
    (if force (ac-l-update-info t) (ac-l-update-info))
    (ac-l-update)
    (ac-l-update-bib)))

(defun ac-l-setup ()
  "Set up Auto Complete LaTeX."
  (let ((msg "Loading auto-complete-latex..."))
    (message "%s" msg)
    (setq ac-l-major-mode major-mode)
    ;; set buffer source
    (unless (ac-l-get-buffer-sources)
      (ac-l-set-buffer-sources
       (cond
        ((memq 'ac-source-words-in-buffer ac-sources)
         (cons 'ac-l-source-words-in-buffer
               'ac-l-source-commands-in-buffer))
        ((memq 'ac-source-words-in-all-buffer ac-sources)
         (cons 'ac-l-source-words-in-all-buffer
               'ac-l-source-commands-in-all-buffer))
        (t
         (cons 'ac-l-source-words-in-same-mode-buffers
               'ac-l-source-commands-in-same-mode-buffers)))))
    ;; ac-l-sources
    (when (and ac-l-sources
               (null (ac-l-get-user-noprefix-sources))
               (null (ac-l-get-user-prefix-sources)))
      (dolist (source ac-l-sources)
        (if (assq 'prefix (symbol-value source))
            (ac-l-set-user-prefix-sources
             (append (ac-l-get-user-prefix-sources) (list source)))
          (ac-l-set-user-noprefix-sources
           (append (ac-l-get-user-noprefix-sources) (list source))))))
    ;; read files
    (when (and ac-l-package-files
               (= (hash-table-count ac-l-packages) 0))
      (ac-l-read-packages ac-l-package-files ac-l-packages))
    (when (and ac-l-bib-files
               (null (ac-l-get-all-bib-tables)))
      (ac-l-read-bibs ac-l-bib-files))
    ;; make sources from ac-l-dict
    (unless (or (ac-l-get-latex-commands)
                (ac-l-get-latex-arguments)
                (ac-l-get-package-sources))
      (ac-l-make-source-from-dir ac-l-dict-directory))
    ;; add prefix properties
    (unless (local-variable-p 'ac-prefix-definitions)
      (make-local-variable 'ac-prefix-definitions)
      (setq ac-prefix-definitions
            `((ac-l-argument . (ac-l-prefix-in-paren ac-l-argument-regexps))
              (ac-l-file . (ac-l-prefix-in-paren ac-l-file-regexps))
              (ac-l-label . (ac-l-prefix-in-paren ac-l-label-regexps))
              (ac-l-bib . (ac-l-prefix-in-paren ac-l-bib-regexps))
              ,@ac-prefix-definitions)))
    ;; modify ac-source-*
    (unless (local-variable-p 'ac-source-files-in-current-dir)
      (make-local-variable 'ac-source-files-in-current-dir)
      (setq ac-source-files-in-current-dir
            `((prefix . ac-l-file)
              (symbol . "F")
              ,@ac-source-files-in-current-dir)))
    ;; set timer
    (ac-l-cancel-timer)
    (setq ac-l-update-timer
          (run-with-idle-timer ac-l-update-delay t 'ac-l-update-all)
          ac-l-clear-timer
          (run-with-timer 600 600 'ac-l-clear))
    ;; initial update
    (unless ac-l-master
      (ac-l-update-all t))
    (message "%sdone" msg)))

(provide 'auto-complete-latex)

;;; auto-complete-latex.el ends here