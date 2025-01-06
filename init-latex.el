;;;
;;; LaTex configuration (AucTeX) ***
;;;
;;;-----------------------------------------------------------------

;; Multifile support, completition, style, reverse search support, ...
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-master t)  ;; Do not query for master file.
(setq TeX-save-query nil)
(setq TeX-display-help t)
(setq TeX-electric-sub-and-superscript t)
(setq TeX-electric-math nil)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-use-external-file-finders t)  ;; make RefTeX uses `kpsewhich'
(setq bib-cite-use-reftex-view-crossref t)
(setq TeX-view-evince-keep-focus t)
(setq TeX-file-line-error t)
(setq TeX-command-extra-options "-shell-escape")
(setq LaTeX-item-indent 0)

;; Setup latex settings after loading AUCTeX
(defun mxl/setup-AUCTeX ()

  ;; environments for code/monospaced text
  (add-to-list 'LaTeX-verbatim-environments "comment")
  (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  (add-to-list 'LaTeX-verbatim-environments "minted")

  ;; no dot remap next/previous-error
  (define-key TeX-mode-map [remap next-error] nil)
  (define-key TeX-mode-map [remap previous-error] nil)

  ;; F10 to view the document
  (define-key TeX-mode-map (kbd "<f10>") 'TeX-view)

  ;; Label creation/selection via reftex
  (define-key TeX-mode-map (kbd "C-c l") 'reftex-label)
  (define-key TeX-mode-map (kbd "C-c r") 'reftex-reference)


  ;; View documents with no questions
  (add-to-list 'TeX-command-list '("View" "%V"
                                   TeX-run-discard-or-function nil t
                                   :help "Run Viewer"))

  ;; View PDFs within emacs using `pdf-tools' package
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))

  t)

(eval-after-load "latex" '(mxl/setup-AUCTeX))


(defun mxl/setup-LaTeX-mode ()
  ;; To help collaboration, in LaTeX file sometimes I need to use soft
  ;; word wrapping. I keep a low value of fill-column so that I can do
  ;; hard wrapping regardless but auto-fill must be off.
  (setq fill-column 70)
  (setq default-justification 'left)
  (auto-fill-mode -1))

;; Basic LaTeX-mode-hook setup
(add-hook 'TeX-mode-hook 'TeX-PDF-mode)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'TeX-mode-hook 'turn-on-flyspell)
(add-hook 'TeX-mode-hook 'TeX-source-specials-mode)
(add-hook 'LaTeX-mode-hook   'mxl/setup-LaTeX-mode)
(add-hook 'LaTeX-mode-hook 'mxl/guess-TeX-master-file)

;; Hints for automatic reference creation
(setq reftex-label-alist
      '(
        ("definition" ?d "def:"  "~\\ref{%s}" nil ("definition" "def.") -3)

        ("theorem"    ?h "thm:"  "~\\ref{%s}" t   ("th." "theorem") -3)
        ("lemma"      ?l "lmm:"  "~\\ref{%s}" t   ("lemma") -3)
        ("corollary"  ?c "cor:"  "~\\ref{%s}" nil ("corollary"  "cor.") -3)

        ("fact"        ?F "fact:"  "~\\ref{%s}" nil ("fact") -3)
        ("claim"       ?C "clm:"   "~\\ref{%s}" nil ("claim") -3)
        ("proposition" ?S "stm:"   "~\\ref{%s}" nil ("proposition" "prop.") -3)

        ("remark"      ?S "stm:"  "~\\ref{%s}" nil ("remark") -3)
        ("property"    ?S "stm:"  "~\\ref{%s}" nil ("property") -3)

        ("example"    ?g "eg:"   "~\\ref{%s}" nil ("example"  "ex." "e.g.") -3)
        ("exercise"   ?x "ex:"   "~\\ref{%s}" nil ("exercise") -3)

        ("open.problem" ?o "open:"   "~\\ref{%s}" nil ("problem") -3)
        ("problem"      ?p "prob:"   "~\\ref{%s}" nil ("problem") -3)

        ("equation" ?e "eq:"   "~\\eqref{%s}" nil ("equation" "Eqn." "Eq.") -3)

        ))


;; TeX asks for Flyspell and American dictionary.
(add-hook 'TeX-language-en-hook
	  (lambda () (ispell-change-dictionary "american")))
(add-hook 'TeX-language-it-hook
	  (lambda () (ispell-change-dictionary "italiano")))



(defun mxl/guess-TeX-master-file ()
  "Try to guess and set the TeX master file"
  (setq TeX-master (or (mxl/TeX-master-from-latexmkrc)
                       (mxl/TeX-master-is-maintex)
                       (mxl/TeX-master-from-docroot)
                       (mxl/TeX-master-from-open-buffers)
                       t))
  (if (not (eq TeX-master t))
      (message "TeX master document: %s" TeX-master)))

(defun mxl/current-TeX-docroot ()
  "Try to guess the base directory of a TeX project

First tries the project root, if defined by `project',
otherwise use the directory containing the current file."
  (or (and (project-current nil)
           (project-root (project-current nil)))
      (file-name-directory buffer-file-name)))

(defun mxl/TeX-master-is-maintex ()
  "Use main.tex as TeX-master document"
  (let ((candidate (concat (mxl/current-TeX-docroot) "main.tex")))
    (and (file-exists-p candidate)
         candidate)))

(defun mxl/TeX-master-from-latexmkrc ()
  "Try to get the TeX-master from latexmkrc"
  (when (file-exists-p (concat (mxl/current-TeX-docroot) ".latexmkrc"))
    (with-temp-buffer
      (insert-file-contents (concat (mxl/current-TeX-docroot) ".latexmkrc"))
      (re-search-forward
       "@default_files\\ *=\\ *(\\ *'\\(.*tex\\)'\\ *)" nil t)
      (concat (mxl/current-TeX-docroot) (match-string 1)))))

(defun mxl/TeX-master-from-docroot ()
  "Guess the master file for tex files in the document root."
  (let* ((candidate nil)
         (basedir (mxl/current-TeX-docroot))
         (filename (file-relative-name buffer-file-name basedir))
         (file-list (directory-files basedir t "\\.tex$")))
    (dolist (file file-list)
      (with-temp-buffer
        (insert-file-contents file nil nil nil t)
        (goto-char (point-min))
        (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
            (setq candidate file))
        (if (re-search-forward (concat "\\\\include{"
                                       (file-name-sans-extension filename) "}")
                               nil t)
            (setq candidate file))))
    candidate))

(defun mxl/TeX-master-from-open-buffers ()
  "Guess the master file for FILENAME from currently open .tex files.

I copied this function from somewhere on the web.
"
  (let ((candidate nil)
        (filename (file-name-nondirectory buffer-file-name)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                      (setq candidate file))))))))
    candidate))


(add-to-list 'auto-mode-alist '("latexmkrc" . conf-unix-mode))

(provide 'init-latex)
;; Local Variables:
;; mode: emacs-lisp
;; End:
