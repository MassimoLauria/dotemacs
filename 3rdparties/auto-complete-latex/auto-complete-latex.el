;;; auto-complete-latex.el --- A LaTeX extention for auto-complete-mode

;; Copyright (C) 2010 tequilasunset

;; Author:   tequilasunset <tequilasunset.mac@gmail.com>
;; Keywords: LaTeX AUCTeX YaTeX
;; Version:  0.2.1 dev

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

;;   auto-complete-mode v1.2:
;;
;;     http://cx4a.org/software/auto-complete/
;;

;;; NOTICE:

;; This is development version. If you want to use stable version,
;; use version 0.2.0.
;;
;;   http://bitbucket.org/tequilasunset/auto-complete-latex/src/a9e07114ea45/
;;

;;; Installation:

;; Put files into your load-path, and add the following into your .emacs.
;;
;;   (require 'auto-complete-latex)
;;   (setq ac-l-dict-directory "/path/to/ac-l-dict/")
;;
;; If necessary, add the following into your .emacs.
;;
;;   (setq ac-modes (append ac-modes '(foo-mode)))
;;   (add-hook 'foo-mode-hook 'ac-l-setup)
;;

;;; Commentary:

;; How to deal with the error of max-specpdl-size:
;;
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

(eval-when-compile (require 'cl))

(require 'auto-complete)

(defgroup auto-complete-latex nil
  "Auto completion of LaTeX keywords."
  :group 'auto-complete
  :group 'tex
  :prefix "ac-l-")

;;;; variables

(defcustom ac-l-master-file nil
  "Specify LaTeX master file path as string.
If valid file path is specified, parse master file's \\input and
\\include, and create candidates from master file and parsed files.
Moreover, parse \\usepackage, \\bibliography, \\label, etc. Limit
number of parsed files is specified by `ac-l-tex-files-max'.

NOTICE:
   While specified a master file, Auto Complete LaTeX will parse
   information after save a current buffer."
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
in it, and create candidates.
You can write files into `ac-l-dict-directory' with using the
function `ac-l-write-package-files'."
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
(defvar ac-l-update-delay 0.5
  "Delay to update `ac-sources'.")

(defvar ac-l-buffer-source-type 1
  "Number related to buffer source.

   0: ac-source-words-in-buffer
   1: ac-source-words-in-same-mode-buffers
   2: ac-source-words-in-all-buffer")

(defconst ac-l-command-prefix "\\\\\\([a-zA-Z@]+\\)"
  "Prefix property of sources for LaTeX commands.")

(defvar ac-l-major-mode nil
  "Major mode which Auto Complete LaTeX is working.")

(defvar ac-l-update-timer nil
  "Timer for `ac-l-update-for-timer'.")

(defconst ac-l-packages (make-hash-table :test 'equal)
  "Hash table.
KEY    =>  package name listed in `ac-l-package-files'
VALUE  =>  (COMMANDS . ARGUMENTS)")

(defvar ac-l-package-candidates nil
  "A list of candidates of packages now parsed.
`car' is a list of commands, `cdr' is a list of arguments.")

(defvar ac-l-filenames nil
  "A list of package and bib file names.")

;;;; functions

(defsubst ac-l-master-p ()
  "Return non-nil, if `ac-l-master-file' is valid. If not, return nil."
  (if (and (stringp ac-l-master-file)
           (file-exists-p ac-l-master-file))
      t))

(defsubst ac-l-buffers ()
  "Return a list of names of files buffer is visiting."
  (delq nil (mapcar 'buffer-file-name (buffer-list))))

(defsubst ac-l-split-string (str)
  (split-string str "[ %,\t\n]+" t))

(defsubst ac-l-completions (candidates)
  (all-completions ac-prefix `,candidates))

;;; prefix properties
(defconst ac-l-argument-regexps
  (list "usepackage"
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
  "A list of regexps for commands involved with arguments.
Argument completion will work only when you type arguments of these
commands.")

(defconst ac-l-file-regexps
  (list "include\\(?:only\\)?"
        "input"
        ;; hyperref
        "hypersetup"
        ;; graphicx
        "includegraphics")
  "A list of regexps for commands involved with file name argument.
File name completion will work only when you type arguments of
these commands.")

(defconst ac-l-label-regexps
  (list "\\(?:page\\|auto\\|eq\\)?ref"
        "label")
  "A list of regexps for commands involved with label name argument.
Label name completion will work only when you type arguments of
these commands.")

(defconst ac-l-bib-regexps
  (list "\\(?:no\\|short\\)?cite[a-zA-Z]*"
        "bibitem")
  "A list of regexps for commands involved with bibliography argument.
Bibliography completion will work only when you type arguments of
these commands.")

(defun ac-l-prefix-in-paren (regexps)
  "Prefix for argument."
  (when (looking-back (concat "\\\\\\("
                              (mapconcat 'identity regexps "\\|")
                              "\\)\\*?[ \t]*\\(\\s([^]>}]*\\s)\\)*\\(\\s([^]>}]*\\)"))
    ac-point))

(defconst ac-l-prefix-definitions
  '((ac-l-argument . (ac-l-prefix-in-paren ac-l-argument-regexps))
    (ac-l-file . (ac-l-prefix-in-paren ac-l-file-regexps))
    (ac-l-label . (ac-l-prefix-in-paren ac-l-label-regexps))
    (ac-l-bib . (ac-l-prefix-in-paren ac-l-bib-regexps)))
  "Prefix properties defined by Auto Complete LaTeX.

ac-l-argument: for argument completion (see `ac-l-argument-regexps').
ac-l-file:     for file name completion (see `ac-l-file-regexps').
ac-l-label:    for label name completion (see `ac-l-label-regexps').
ac-l-bib:      for bib key completion (see `ac-l-bib-regexps').

`ac-l-command-prefix': for command completion.")

;;; read file data
(defvar ac-l-bib-tables nil
  "A list of hash tables for bib files.")

(defun ac-l-read-files (files regexp &optional bib)
  (dolist (filename files)
    (let* ((file-nodir (file-name-nondirectory filename))
           (file (if (string-match "^[^.]+" file-nodir)
                     (match-string 0 file-nodir)
                   file-nodir))
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
      (when bib
        (push name ac-l-bib-tables)
        (push file ac-l-filenames)))))

(defun ac-l-read-packages (files c-regexp a-regexp table)
  (dolist (filename files)
    (let* ((file-nodir (file-name-nondirectory filename))
           (file (if (string-match "^[^.]+" file-nodir)
                     (match-string 0 file-nodir)
                   file-nodir))
           (contents (cons nil nil))
           candidate commands arguments)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents filename)
          ;; commands
          (while (re-search-forward c-regexp nil t)
            (setq candidate (match-string-no-properties 1))
            (unless (member candidate commands)
              (push candidate commands)))
          (setcar contents commands)
          ;; arguments
          (goto-char (point-min))
          (while (re-search-forward a-regexp nil t)
            (setq candidate (match-string-no-properties 1))
            (unless (member candidate arguments)
              (push candidate arguments)))
          (setcdr contents arguments)))
      (puthash file contents table)
      (push file ac-l-filenames))))

(defun ac-l-write-package-files (dir)
  "Write files listed in `ac-l-package-files' into DIR."
  (interactive
   (list (read-directory-name "Directory: " ac-l-dict-directory nil t)))
  (maphash (lambda (k v)
             (dolist (pair (list (cons 'c (car v)) (cons 'a (cdr v))))
               (when (cdr pair)
                 (with-temp-buffer
                   (insert
                    (mapconcat 'identity (sort (cdr pair) #'string<) "\n"))
                   (write-region (point-min) (point-max)
                                 (format "%s%s-%s-*-*" dir k (car pair)))))))
           ac-l-packages))

(defcustom ac-l-dict-directory "~/.emacs.d/ac-l-dict/"
  "A path of the ac-l-dict.
Auto Complete LaTeX will make sources from files in this directory.
If you want to add files, see `ac-l-make-source-from-dir' and
`ac-l-write-package-files'."
  :type 'string
  :group 'auto-complete-latex)

(defvar ac-l-dict-sources nil
  "A list of sources defined by `ac-l-make-source-from-dir'.")

(defconst ac-l-package-dependences
  (let ((table (make-hash-table :test 'equal))
        (alist (list (cons "hyperref" "beamer")
                     (cons "color" "colortbl\\|beamer")
                     (cons "array" "tabularx\\|colortbl"))))
    (loop for (k . v) in alist
          do (puthash k v table))
    table)
  "Hash table.
KEY    =>  called package name
VALUE  =>  package names (regexp)

KEY package is called in the definition of VALUE packages.
For example, array.sty is called by tabularx.sty and colortbl.sty.
So, KEY and VALUE must be like below.

KEY    =>  \"array\"
VALUE  =>  \"tabularx\\\\|colortbl\"

Second example:
   Package and class files are treated equivalently.
   Below code indicates that candidates of amsmath.sty will be shown
   when beamer class is loaded.

   \(puthash \"amsmath\" \"beamer\" ac-l-package-dependences)

KEY    =>  \"amsmath\"
VALUE  =>  \"beamer\"")

(defun ac-l-make-source-from-dir (dir)
  "Make source from files in DIR.

file name form:  NAME-TYPE-SYMBOL-REQUIRES

NAME      Package or class file name.
          For example, you set NAME to foo, the source is included in
          `ac-sources' while \\usepackage{foo}, \\RequirePackage{foo} or
          \\documentclass{foo} is written in documents. If you want to
          set package dependence, use `ac-l-package-dependences'.

TYPE      `c (command)' or `a (argument)'.

SYMBOL    Symbol property. `*' => `p'.

REQUIRES  Requires property. `*' => not set."
  (let* ((files (directory-files dir nil "^[^.]"))
         (help (and (member "latex-help" files)
                    (require 'mule-util nil t))))
    (dolist (file files)
      (let ((symbol "l")
            (prefix ac-l-command-prefix)
            source package req doc)
        ;; parse properties from file name
        (cond
         ((string-match "^\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)-\\([^-]+\\)$" file)
          (let* ((p (match-string 1 file))
                 (T (match-string 2 file))
                 (s (match-string 3 file))
                 (r (match-string 4 file))
                 (v (gethash p ac-l-package-dependences)))
            (push p ac-l-filenames)
            (if v (setq package (concat p "\\|" v)) (setq package p))
            (if (string= s "*") (setq symbol "p") (setq symbol s))
            (unless (string= r "*") (setq req (string-to-number r)))
            (if (string= T "a")
                (setq prefix 'ac-l-argument
                      source (intern (format "ac-l-source-%s-arguments" p)))
              (setq source (intern (format "ac-l-source-%s-commands" p))))))
         ((string-match "^\\(basic\\|platex\\)-\\(commands\\|arguments\\)$" file)
          (when (string= "arguments" (match-string 2 file))
            (setq prefix 'ac-l-argument))
          (setq doc t
                source (intern (format "ac-l-source-%s" file))))
         ((cond
           ((or (string= "macro" file)
                (string= "primitives" file)
                (string= "ptex-primitives" file)
                (string= "latex-dot-ltx" file))
            (setq doc t))
           ((string= "user-commands" file)
            (setq symbol "u"))
           ((string= "user-arguments" file)
            (setq symbol "u"
                  prefix 'ac-l-argument)))
          (setq source (intern (format "ac-l-source-%s" file)))))
        ;; define source
        (when source
          (let ((candidates (with-temp-buffer
                              (insert-file-contents (concat dir file))
                              (split-string (buffer-string) "\n"))))
            (unless (and (= (length candidates) 1)
                         (string= (car candidates) ""))
              (set source
                   (delq nil
                         (list (when package
                                 (cons 'ac-l-package package))
                               (when (integerp req)
                                 (cons 'requires req))
                               (when (and doc help)
                                 (cons 'document
                                       'ac-l-latex2e-documentation))
                               (cons 'symbol symbol)
                               (cons 'prefix prefix)
                               (cons 'candidates
                                     `(ac-l-completions ',candidates)))))
              (push source ac-l-dict-sources))))))))

;;; update and make cache
(defvar ac-l-master-cache nil)
(defvar ac-l-children-cache nil)

(defvar ac-l-tex-files-max 10
  "Max number of \\input and \\include commands parsed.")

(defsubst ac-l-set-children-cache (str)
  (setq ac-l-children-cache
        (concat ac-l-children-cache "\n\n\n\n" str)))

(defun ac-l-make-short-children (size)
  (let* ((c ac-l-children-cache)
         (l (length c))
         (s size))
    (when (> l s)
      (setq ac-l-children-cache
            (apply 'concat
                   (loop with d = (if (> ac-l-tex-files-max 20)
                                      20
                                    ac-l-tex-files-max)
                         with l1 = (/ l d)
                         with l2 = (/ s (* d 2))
                         for i from 1 to (- d 1)
                         for m = (* l1 i)
                         for m- = (- m l2)
                         for m+ = (+ m l2)
                         collect (substring-no-properties c m- m+)))))))

(defun ac-l-make-cache (&optional only-master)
  (when (ac-l-master-p)
    (let ((master (expand-file-name ac-l-master-file)))
      (if (member master (ac-l-buffers))
          ;; master file is a buffer
          (loop for buf in (buffer-list)
                if (string= master (buffer-file-name buf))
                do (with-current-buffer buf
                     (unless only-master (ac-l-make-children-cache))
                     (setq ac-l-master-cache (buffer-string)))
                and return nil)
        ;; master file is a file
        (ignore-errors
          (with-temp-buffer
            (insert-file-contents master)
            (unless only-master (ac-l-make-children-cache))
            (setq ac-l-master-cache (buffer-string))))))))

(defun ac-l-make-children-cache ()
  (let ((regexp "^[^%\n]*\\\\\\(?:input\\|include\\)[ {\t]+\\([^ }%\n]+\\)")
        (dir (if (string-match "^\\(.+/\\).+$" ac-l-master-file)
                 (match-string 1 ac-l-master-file)
               "/"))
        (i 0)
        (limit (if (> ac-l-tex-files-max 20)
                   20
                 ac-l-tex-files-max)))
    (save-excursion
      (goto-char (point-min))
      (while (and (re-search-forward regexp 10000 t)
                  (< i limit))
        (let* ((path (expand-file-name (match-string-no-properties 1) dir))
               (file (concat (if (string-match "^\\(.+\\)\\.[^./]+$" path)
                                 (match-string 1 path)
                               path)
                             ".tex")))
          (if (member file (ac-l-buffers))
              ;; tex file is a buffer
              (loop for buf in (buffer-list)
                    if (string= file (buffer-file-name buf))
                    do (with-current-buffer buf
                         (ac-l-set-children-cache (buffer-string)))
                    and return nil)
            ;; tex file is a file
            (ignore-errors
              (with-temp-buffer
                (insert-file-contents file)
                (ac-l-set-children-cache (buffer-string))))))
        (incf i)))))

(defvar ac-l-global-sources nil
  "Dummy sources of `ac-sources'. Not local.")

(defsubst ac-l-whole-cache ()
  "Return the cache of all tex files."
  (concat ac-l-master-cache "\n" ac-l-children-cache))

(defsubst ac-l-valid-sources (sources)
  (loop for source in sources
        if (boundp source)
        collect source))

(defun ac-l-update ()
  "Update `ac-sources'."
  (if (ac-l-master-p)
      (with-temp-buffer
        (insert ac-l-master-cache)
        (ac-l-update-inner))
    (ac-l-update-inner))
  (setq ac-sources ac-l-global-sources))

(defun ac-l-update-inner ()
  (let ((regexp "^[^%\n]*\\\\\\(\\(usep\\|RequireP\\)ackage\\|documentclass\\)\\(\\[[^]]*\\]\\)?{\\([^}]+\\)")
        (commands (ac-l-valid-sources (list 'ac-l-source-user-commands
                                            'ac-l-source-basic-commands
                                            'ac-l-source-platex-commands
                                            'ac-l-source-primitives
                                            'ac-l-source-ptex-primitives
                                            'ac-l-source-macro
                                            'ac-l-source-latex-dot-ltx)))
        (arguments (ac-l-valid-sources (list 'ac-l-source-user-arguments
                                             'ac-l-source-basic-arguments
                                             'ac-l-source-platex-arguments)))
        p-c-sources p-a-sources u-sources u-p-sources)
    (setq ac-l-package-candidates (cons nil nil))
    ;; parse cache or buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp 10000 t)
        (dolist (name (ac-l-split-string (match-string-no-properties 4)))
          (dolist (source (append ac-l-sources ac-l-dict-sources))
            (let* ((alist (eval source))
                   (package (cdr (assq 'ac-l-package alist)))
                   (prefix (cdr (assq 'prefix alist))))
              (cond
               ;; package sources
               (package
                (when (string-match package name)
                  (cond
                   ((string= prefix ac-l-command-prefix)
                    (unless (memq source p-c-sources)
                      (push source p-c-sources)))
                   ((eq prefix 'ac-l-argument)
                    (unless (memq source p-a-sources)
                      (push source p-a-sources))))))
               ;; user sources
               ((not (or (memq source commands)
                         (memq source arguments)))
                (if prefix
                    (unless (memq source u-p-sources)
                      (push source u-p-sources))
                  (unless (memq source u-sources)
                    (push source u-sources)))))))
          ;; package candidates
          (let* ((v (gethash name ac-l-packages))
                 (cmds (car v))
                 (args (cdr v)))
            (when cmds
              (setcar ac-l-package-candidates
                      (append (car ac-l-package-candidates) cmds)))
            (when args
              (setcdr ac-l-package-candidates
                      (append (cdr ac-l-package-candidates) args)))))))
    ;; set sources
    (setq ac-l-global-sources
          (delq nil
                (append u-p-sources
                        (list 'ac-source-filename
                              'ac-l-source-labels
                              'ac-l-source-bibitems
                              (when ac-l-bib-files
                                'ac-l-source-bibliographies))
                        commands
                        (nreverse p-c-sources)
                        (list (when ac-l-package-files
                                'ac-l-source-package-commands)
                              (cond
                               ((= ac-l-buffer-source-type 0)
                                'ac-l-source-commands-in-buffer)
                               ((= ac-l-buffer-source-type 2)
                                'ac-l-source-commands-in-all-buffer)
                               (t
                                'ac-l-source-commands-in-same-mode-buffers))
                              (when (ac-l-master-p)
                                'ac-l-source-commands-in-tex-files))
                        arguments
                        (nreverse p-a-sources)
                        (list (when ac-l-package-files
                                'ac-l-source-package-arguments)
                              'ac-l-source-filenames
                              'ac-source-files-in-current-dir)
                        ;; without prefix
                        u-sources
                        (list (when ac-l-use-word-completion
                                (cond
                                 ((= ac-l-buffer-source-type 0)
                                  'ac-l-source-words-in-buffer)
                                 ((= ac-l-buffer-source-type 2)
                                  'ac-l-source-words-in-all-buffer)
                                 (t
                                  'ac-l-source-words-in-same-mode-buffers)))
                              (when (and (ac-l-master-p)
                                         ac-l-use-word-completion)
                                'ac-l-source-words-in-tex-files)
                              'ac-source-dictionary))))))

(defvar ac-l-current-bib-tables nil
  "A list of hash tables for bib files now parsed.")

(defun ac-l-update-bib ()
  "Update `ac-l-current-bib-tables'."
  (setq ac-l-current-bib-tables nil)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^[^%\n]*\\\\bibliography{\\([^}]+\\)" nil t)
      (dolist (name (ac-l-split-string (match-string-no-properties 1)))
        (dolist (table ac-l-bib-tables)
          (when (and (string= (format "ac-l-%s-table" name)
                              (symbol-name table))
                     (not (memq table ac-l-current-bib-tables)))
            (push table ac-l-current-bib-tables)))))))

;;; clear
(defvar ac-l-clear-timer nil
  "Timer for `ac-l-clear'.")

(defun ac-l-clear ()
  "Clear variables and hash tables."
  (setq ac-l-master-cache nil
        ac-l-children-cache nil)
  (clrhash ac-l-latex2e-help)
  (when ac-l-major-mode
    (when (loop for buf in (buffer-list)
                never (eq ac-l-major-mode
                          (buffer-local-value 'major-mode buf)))
      (clrhash ac-l-packages)
      (clrhash ac-l-labels)
      (clrhash ac-l-bibitems)
      (dolist (source ac-l-dict-sources)
        (set source nil))
      (dolist (table ac-l-bib-tables)
        (clrhash (eval table))
        (set table nil))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (setq ac-l-word-index nil
                ac-l-command-index nil)))
      (setq ac-l-major-mode nil
            ac-l-package-candidates nil
            ac-l-filenames nil
            ac-l-bib-tables nil
            ac-l-dict-sources nil
            ac-l-global-sources nil
            ac-l-current-bib-tables nil
            ac-l-words-in-tex-files nil
            ac-l-commands-in-tex-files nil))))

(defun ac-l-cancel-timer ()
  "Clear timers."
  (interactive)
  (when (timerp ac-l-update-timer)
    (cancel-timer ac-l-update-timer)
    (setq ac-l-update-timer nil))
  (when (timerp ac-l-clear-timer)
    (cancel-timer ac-l-clear-timer)
    (setq ac-l-clear-timer nil)))

;;; candidate
(defmacro ac-l-without-limit (&rest body)
  (declare (indent 0))
  `(let ((ac-point (point-min))
         (ac-prefix "")
         ac-limit)
     ,@body))

(defun ac-l-candidate (beg-regexp end-regexp &optional table)
  (let ((i 0)
        (regexp (concat beg-regexp (regexp-quote ac-prefix) end-regexp))
        candidate candidates)
    (save-excursion
      (if table
          (clrhash table)
        ;; Search backward
        (goto-char ac-point)
        (while (and (or (not (integerp ac-limit))
                        (< i ac-limit))
                    (re-search-backward regexp nil t))
          (setq candidate (match-string-no-properties 1))
          (unless (member candidate candidates)
            (push candidate candidates)
            (incf i))))
      ;; Search backward
      (goto-char (+ ac-point (length ac-prefix)))
      (while (and (or (not (integerp ac-limit))
                      (< i ac-limit))
                  (re-search-forward regexp nil t))
        (if table
            (progn
              (puthash (match-string-no-properties 1)
                       (match-string-no-properties 0)
                       table)
              (goto-char (1+ (match-beginning 0)))
              (incf i))
          (setq candidate (match-string-no-properties 1))
          (unless (member candidate candidates)
            (push candidate candidates)
            (incf i))))
      (nreverse candidates))))

(defun ac-l-incremental-update-index (index function)
  (let ((pair (eval index))
        (ac-limit (or (and (integerp ac-limit)
                           ac-limit)
                      10)))
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
        (when (and (not (car (eval index)))
                   (< (buffer-size) 1048576))
          ;; Complete index
          (ac-l-without-limit
            (set index (cons t (funcall function)))))
        ;; (when same-mode (setq ac-word-index nil))
        ))))

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

(defconst ac-l-capitalize-regexps
  (append ac-l-argument-regexps
          ac-l-file-regexps
          (list "part"
                "chapter"
                "\\(?:sub\\)*section"
                "\\(?:sub\\)*paragraph"))
  "Regexps for `ac-l-smart-capitalize'.")

(defun ac-l-smart-capitalize ()
  (when (looking-back
         (concat
          "\\([.!?]\\|\\(\\("
          (mapconcat 'identity ac-l-capitalize-regexps "\\|")
          "\\)\\*?[ \t]*\\(\\s([^]>}]*\\s)\\)+\\)\\)[[:space:][:cntrl:]]+[a-z']+"))
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

(defvar ac-l-words-in-tex-files nil
  "A list of words in tex files.")

(defvar ac-l-source-words-in-tex-files
  '((candidates . (ac-l-completions ac-l-words-in-tex-files))
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

(defvar ac-l-commands-in-tex-files nil
  "A list of commands in tex files.")

(defvar ac-l-source-commands-in-tex-files
  `((candidates . (ac-l-completions ac-l-commands-in-tex-files))
    (prefix . ,ac-l-command-prefix))
  "Source for commands in tex files.")

;; file names
(defvar ac-l-source-filenames
  '((candidates . (ac-l-completions ac-l-filenames))
    (prefix . ac-l-argument))
  "Source for names of packages and bib files.")

;; packages
(defvar ac-l-source-package-commands
  `((candidates . (ac-l-completions (car ac-l-package-candidates)))
    (prefix . ,ac-l-command-prefix)
    (symbol . "p"))
  "Source for commands in `ac-l-package-files'.")

(defvar ac-l-source-package-arguments
  '((candidates . (ac-l-completions (cdr ac-l-package-candidates)))
    (prefix . ac-l-argument)
    (symbol . "p"))
  "Source for arguments in `ac-l-package-files'.")

;; bib files
(defvar ac-l-source-bibliographies
  '((candidates . (ac-l-completions
                   (loop for table in ac-l-current-bib-tables
                         append (loop for k being the hash-keys in (eval table)
                                      collect k)
                         into candidates
                         finally return candidates)))
    (prefix . ac-l-bib)
    (document . (lambda (str) (loop for table in ac-l-current-bib-tables
                                    thereis (gethash str (eval table)))))
    (symbol . "B"))
  "Source for bibliographies in `ac-l-bib-files'.")

;; labels
(defconst ac-l-labels (make-hash-table :test 'equal)
  "Hash table.
KEY    =>  label name
VALUE  =>  quick help contents")

(defsubst ac-l-candidate-labels ()
  (ac-l-without-limit
    (ac-l-candidate "^.*\n.*\n.*\n[^%\n]*\\\\label{\\("
                    "\\(?:[^ }\t\n]\\)+\\)}.*\n.*\n.*\n.*$"
                    ac-l-labels)))

(defvar ac-l-source-labels
  '((init . (lambda ()
              (unless (ac-l-master-p) (ac-l-candidate-labels))
              (when (looking-back "\\\\label[ \t]*{\\([^}]*\\)")
                (remhash (match-string-no-properties 1) ac-l-labels))))
    (candidates . (ac-l-completions
                   (loop for k being the hash-keys in ac-l-labels
                         collect k)))
    (prefix . ac-l-label)
    (document . (lambda (str) (gethash str ac-l-labels)))
    (symbol . "L"))
  "Source for labels.")

(defun ac-l-complete-labels ()
  "Start label name completion at point."
  (interactive)
  (auto-complete
   (list (delete (cons 'prefix 'ac-l-label) ac-l-source-labels))))

;; bibitems
(defconst ac-l-bibitems (make-hash-table :test 'equal)
  "Hash table.
KEY    =>  bibitem key
VALUE  =>  quick help contents")

(defsubst ac-l-candidate-bibitems ()
  (ac-l-without-limit
    (ac-l-candidate "^[^%\n]*\\\\bibitem\\(?:\\[[^]]*\\]\\)?{\\("
                    "\\(?:[^ }\t\n]\\)+\\)}[^\\]*"
                    ac-l-bibitems)))

(defvar ac-l-source-bibitems
  '((init . (lambda ()
              (unless (ac-l-master-p) (ac-l-candidate-bibitems))
              (when (looking-back
                     "\\\\bibitem[ \t]*\\(\\[\\([^]]*\\)\\]\\)?{\\([^}]*\\)")
                (remhash (match-string-no-properties 3) ac-l-bibitems))))
    (candidates . (ac-l-completions
                   (loop for k being the hash-keys in ac-l-bibitems
                         collect k)))
    (prefix . ac-l-bib)
    (document . (lambda (str) (gethash str ac-l-bibitems)))
    (symbol . "B"))
  "Source for bibitems.")

(defun ac-l-complete-bibs ()
  "Start bibliography completion at point."
  (interactive)
  (auto-complete
   (list (delete (cons 'prefix 'ac-l-bib) ac-l-source-bibitems)
         (delete (cons 'prefix 'ac-l-bib) ac-l-source-bibliographies))))

;; help
(defconst ac-l-latex2e-help (make-hash-table :test 'equal)
  "Hash table.
KEY    =>  LaTeX2e keyword
VALUE  =>  quick help contents")

(defun ac-l-latex2e-documentation (str)
  (or (gethash str ac-l-latex2e-help)
      (unless (string-match "@" str)
        (with-temp-buffer
          (insert-file-contents (concat ac-l-dict-directory "latex-help"))
          (if (re-search-forward (concat "\\(?:\n\\)\\([^]*\\(?:^[`\\]"
                                         str
                                         "\\(?:\\s(\\|[ '\n]\\)[^]+\\)\\)")
                                 nil t)
              (let ((contents (match-string-no-properties 1)))
                (puthash str contents ac-l-latex2e-help)
                contents)
            (puthash str t ac-l-latex2e-help))))))

;;;; setup

(defun ac-l-update-all ()
  "Update all information about tex files.
This function works only when `ac-l-master-file' is specified."
  (ac-l-make-cache)
  (ac-l-update)
  (when (ac-l-master-p)
    (with-temp-buffer
      (insert (ac-l-whole-cache))
      (ac-l-candidate-bibitems)
      (ac-l-candidate-labels)
      (ac-l-update-bib)
      (ac-l-make-short-children 100000)
      (erase-buffer)
      (insert ac-l-children-cache)
      (ac-l-without-limit
        (when ac-l-use-word-completion
          (setq ac-l-words-in-tex-files
                (ac-l-candidate-words-in-buffer)))
        (setq ac-l-commands-in-tex-files
              (ac-l-candidate-commands-in-buffer))))
    (setq ac-l-master-cache nil
          ac-l-children-cache nil)))

(defun ac-l-update-for-timer ()
  "Update `ac-sources'. Delay is specified by `ac-l-update-delay'.
In addition, parse \\bibliography (only when `ac-l-master-file'
isn't specified)."
  (when (eq major-mode ac-l-major-mode)
    (ac-l-make-cache t)
    (ac-l-update)
    (unless (ac-l-master-p)
      (ac-l-update-bib))
    (setq ac-l-master-cache nil)))

(defun ac-l-after-init-setup ()
  (push 'latex-mode ac-modes)
  (add-hook 'latex-mode-hook 'ac-l-setup)
  ;; for AUCTeX users
  (when (require 'tex-site nil t)
    (add-hook 'LaTeX-mode-hook 'ac-l-setup))
  ;; for YaTeX users
  (when (fboundp 'yatex-mode)
    (push 'yatex-mode ac-modes)
    (add-hook 'yatex-mode-hook 'ac-l-setup)))

(add-hook 'after-init-hook 'ac-l-after-init-setup)

(defvar ac-l-after-setup-hook nil
  "Run hook after `ac-l-setup'.")

(defun ac-l-setup ()
  "Set up Auto Complete LaTeX."
  (let ((msg "Loading auto-complete-latex..."))
    (message "%s" msg)
    ;; clear
    (let ((ac-l-major-mode 'none))
      (ac-l-clear))
    (ac-l-cancel-timer)
    ;; set ac-l-major-mode
    (setq ac-l-major-mode major-mode)
    ;; read information about buffer source
    (cond
     ((memq 'ac-source-words-in-buffer ac-sources)
      (setq ac-l-buffer-source-type 0))
     ((memq 'ac-source-words-in-all-buffer ac-sources)
      (setq ac-l-buffer-source-type 2))
     (t
      (setq ac-l-buffer-source-type 1)))
    ;; read files
    (ac-l-read-packages
     ac-l-package-files
     "\\\\\\(?:[a-z@]*def\\|let\\|new[a-z]+\\|providecommand\\|Declare[a-zA-Z@]+\\)\\*?[ \t]*{?\\\\\\([a-zA-Z]+\\)}?[ =\\#[{]"
     "\\\\\\(?:newenvironment\\|DeclareOption[a-zA-Z]*\\|new[a-z]+\\|@definecounter\\)\\*?[ \t]*{\\([a-zA-Z]+\\)}"
     ac-l-packages)
    (ac-l-read-files
     ac-l-bib-files "^@[^{@]+{\\([^ =,\t\n]*\\),\n[^@]+\\(^}\\)" t)
    ;; make sources
    (ac-l-make-source-from-dir ac-l-dict-directory)
    ;; add prefix properties
    (unless (local-variable-p 'ac-prefix-definitions)
      (make-local-variable 'ac-prefix-definitions)
      (setq ac-prefix-definitions
            (append ac-l-prefix-definitions ac-prefix-definitions)))
    ;; modify ac-source-*
    (unless (local-variable-p 'ac-source-files-in-current-dir)
      (make-local-variable 'ac-source-files-in-current-dir)
      (setq ac-source-files-in-current-dir
            `((prefix . ac-l-file)
              (symbol . "F")
              ,@ac-source-files-in-current-dir)))
    ;; set after-save-hook
    (add-hook 'after-save-hook 'ac-l-update-all nil t)
    ;; set timer
    (setq ac-l-update-timer
          (run-with-idle-timer ac-l-update-delay t 'ac-l-update-for-timer)
          ac-l-clear-timer
          (run-with-timer 600 600 'ac-l-clear))
    ;; run hook
    (run-hooks 'ac-l-after-setup-hook)
    ;; initial update
    (ac-l-update-all)
    (message "%sdone" msg)))

(provide 'auto-complete-latex)

;;; auto-complete-latex.el ends here