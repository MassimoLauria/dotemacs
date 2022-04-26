;;;
;;; LaTex support (AucTeX) ***
;;;
;;;-----------------------------------------------------------------

;;
;; Many great features require AucTeX 11.88.
;;
;; previous version of auctex would not support, e.g., forward/inverse
;; search with Evince, do not have a proper support of compilation
;; error management, ...
;;
;; Older versions of this file had workarounds. Now I decided to clean
;; up the LaTeX since updating auctex with elpa is very easy.
;;

;; Multifile support, completition, style, reverse search support, ...
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-master t)  ;; Do not query for master file.
(setq TeX-save-query nil)
(setq TeX-display-help t)
(setq TeX-electric-sub-and-superscript t)
(setq TeX-electric-math nil)
(setq reftex-plug-into-AUCTeX t)
(setq bib-cite-use-reftex-view-crossref t)
(setq TeX-view-evince-keep-focus t)
(setq TeX-file-line-error t)
(setq TeX-command-extra-options "-shell-escape")
(setq LaTeX-item-indent 0)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; View command fixes
;;
;; 1 - use `displayline' command under MacOSX (from Skim.app)
;; 2 - do not ask for confirmation with the View command.
;;
(defun init-latex--viewer-setup ()
  "Set the \"View\" function saner defaults"

  ;; No question asked when viewing documents
  (add-to-list 'TeX-command-list '("View" "%V"
                                   TeX-run-discard-or-function nil t
                                   :help "Run Viewer"))

  ;; View PDFs within emacs using `pdf-tools' package
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))

  ;; On MacOSX we could use Skim PDF viewer. Actually I don't because
  ;; lately the rendering quality is horrible, and I'd rather use
  ;; PDF-tools as in Linux.
  (when (eq system-type 'darwin)
    (add-to-list 'TeX-view-program-list
                 '("displayline"
                   "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))))




;; Load PDF tools if AucTeX wants to open a PDF
(use-package pdf-tools
  :mode  ("\\.pdf" . pdf-view-mode)
  :init
  (setq pdf-view-use-unicode-ligther nil)  ;; make loading faster
  :config
  (pdf-loader-install t)
  (setq pdf-view-use-scaling t)
  (setq revert-without-query '(".pdf"))
  (setq-default pdf-view-display-size 'fit-page)
  ;; Highlights do not open the annotation window when created
  (setq pdf-annot-activate-created-annotations nil)
  (defun mxl-pdf-annot-add-text-annotation ()
    "Add text annotation but forces activation which is off
by default."
    (interactive)
    (let ((pdf-annot-activate-created-annotations t))
      (call-interactively 'pdf-annot-add-text-annotation)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; use isearch instead of swiper
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; keyboard shortcut for zooming
  (define-key pdf-view-mode-map (kbd "+") 'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "=") 'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "-") 'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "0") 'pdf-view-scale-reset)
  (define-key pdf-view-mode-map (kbd "W") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "H") 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map (kbd "P") 'pdf-view-fit-page-to-window)
  ;; Open in apps
  (define-key pdf-view-mode-map (kbd "O") 'open-in-external-app)
  (defun mxl-pdf-open-in-xournal()
    "Open the current PDF in Xournal, for editing"
    (interactive)
    (start-process "open-pdf-in-xournal" nil "xournalpp" (buffer-file-name)))
  (define-key pdf-view-mode-map (kbd "X") 'mxl-pdf-open-in-xournal)
  ;; Margin removal
  (defun mxl-pdf-view-toggle-crop()
    "Crop/Uncrop according to the bounding box"
    (interactive)
    (if (pdf-view-current-slice)
        (pdf-view-reset-slice)
      (pdf-view-set-slice-from-bounding-box)))
  (define-key pdf-view-mode-map (kbd "c") 'mxl-pdf-view-toggle-crop)
  ;; keyboard shortcuts for annotations
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "u") 'pdf-annot-add-underline-markup-annotation)
  (define-key pdf-view-mode-map (kbd "s") 'pdf-annot-add-strikeout-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'mxl-pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))

(eval-after-load "tex" '(init-latex--viewer-setup))


;; Basic LaTeX-mode-hook setup
(add-hook 'TeX-mode-hook 'TeX-PDF-mode)
(add-hook 'TeX-mode-hook 'flycheck-mode)
(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'TeX-mode-hook 'turn-on-flyspell)
(add-hook 'TeX-mode-hook 'TeX-source-specials-mode)

;; All TeX made with a single keystroke (BibTeX must run at least once).
(require-maybe 'TeX-texify)

;; Various improvements to the mode
(add-hook 'LaTeX-mode-hook
          (lambda () (progn
                       (make-local-variable 'compilation-exit-message-function)
                       (setq compilation-exit-message-function 'nil)
                       (add-to-list 'LaTeX-verbatim-environments "comment")
                       (add-to-list 'LaTeX-verbatim-environments "lstlisting")
                       )))


;; Keyboard shortcut
(defun mxl-toggle-tex-errors ()
  "Hide/Show the latex error window"
  (interactive)
  (let* ((oldwin (get-buffer-window (current-buffer))))
    (if (get-buffer TeX-error-overview-buffer-name)
        (mxl-toggle-error-window TeX-error-overview-buffer-name)
      (TeX-error-overview)
      (message (concat "Opening error list: " TeX-error-overview-buffer-name))
      (select-window oldwin))))

(add-hook 'TeX-mode-hook
          '(lambda ()
             (define-key TeX-mode-map (kbd "<f9>")  'init-latex--make)
             (define-key TeX-mode-map (kbd "<f10>") 'TeX-pin-region)
             (define-key TeX-mode-map (kbd "<f11>") 'TeX-previous-error)
             (define-key TeX-mode-map (kbd "<f12>") 'TeX-next-error)
             (define-key TeX-mode-map (kbd "M-<f11>") 'mxl-toggle-tex-errors)
             (define-key TeX-mode-map (kbd "M-<f12>") 'mxl-toggle-tex-errors)
             ))


(defun init-latex--make ()
  "Produce the document, by trying several build commands"
  (interactive)
  (cond

   (TeX-command-region-begin            ; region pinned
    (TeX-command-region nil))

   ((fboundp 'TeX-texify)          ; TeX-texify loaded
    (call-interactively 'TeX-texify))

   (t                                   ; default
    (call-interactively 'TeX-command-master))))




(defun LaTeX-up-list ()
  "A function similar to standard Emacs `up-list', but if we are
outside of a syntax block, it attempts to escape math from
delimiters. It substitues `up-list' the first time AucTeX is
started."
  (interactive)
  (condition-case X
      ;; Try to jump to an outer syntax block.
      (up-list)
    ('error
     ;; If inside math mode of LaTeX-mode, escape from it.
     (if (or
               (eq (get-text-property (point) 'face) 'font-latex-math-face)
               (member 'font-latex-math-face (get-text-property (point) 'face)))
         (save-match-data (search-forward-regexp "\\$?\\$"))))))

;; Install LaTeX improved `up-list' command
(add-hook 'LaTeX-mode-hook
          (lambda()
            (if (and
                 (boundp 'massimo-keyboard-mode-map)
                 (fboundp 'sp-up-sexp))
                (define-key massimo-keyboard-mode-map (kbd "M-p") 'sp-up-sexp))))





;; To help collaboration, in LaTeX file sometimes I need to use soft
;; word wrapping. I keep a low value of fill-column so that I can do
;; hard wrapping regardless but auto-fill must be off.
(defun mxl-latex-wrapping()
  "Setup of text editing in LaTeX."
  (interactive)
  (setq fill-column 70)
  (setq default-justification 'left)
  (auto-fill-mode -1))

(add-hook 'LaTeX-mode-hook 'mxl-latex-wrapping)


;; RefTeX setup has to use `kpsewhich' to find stuff, so that it
;; respects TEXINPUTS, BIBINPUTS and BSTINPUTS settings.
(setq reftex-use-external-file-finders t)


(add-hook 'reftex-mode-hook
          (lambda ()
            (diminish 'reftex-mode)
            (local-set-key (kbd "C-c l") 'reftex-label)       ;; Label creation
            (local-set-key (kbd "C-c r") 'reftex-reference)   ;; Label selection
            (local-set-key (kbd "M-,") 'reftex-view-crossref) ;; View crossref
            (local-set-key (kbd "M-.") 'delete-other-windows-vertically)))


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



(defun init-latex--flymake-setup ()
  "Setup flymake for latex using one of the checker available on the system.
It either tries \"lacheck\" or \"chktex\"."
  (interactive)
  (cond ((executable-find "lacheck")
         (defun flymake-get-tex-args (file-name)
           (list "lacheck" (list file-name))))
        ((executable-find "chktex")
         (defun flymake-get-tex-args (file-name)
           (list "chktex" (list "-q" "-v0" file-name))))
        (t nil)))

(eval-after-load "flymake" '(init-latex--flymake-setup))


(defun my-flymake-show-help ()
   (when (get-char-property (point) 'flymake-overlay)
     (let ((help (get-char-property (point) 'help-echo)))
       (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)



;; Guess master file
(add-hook
 'LaTeX-mode-hook
 (lambda () (setq TeX-master (or
                              (guess-TeX-master (buffer-file-name))
                              t))))

(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
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
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
    candidate))


;; Smart parenthesis

(add-hook  ; Workaround for `TeX-insert-quote' conflict with `smartparens'
 'LaTeX-mode-hook
 (lambda () (local-unset-key "\"")))

(require 'smartparens-latex nil t)
(eval-after-load "smartparens-latex"
  '(sp-with-modes '(
                 tex-mode
                 plain-tex-mode
                 latex-mode
                 )
     ;;(sp-local-pair "$" "$" :post-handlers '(sp-latex-insert-spaces-inside-pair))
     (sp-local-pair "\\lceil" "\\rceil"
                    :post-handlers '(sp-latex-insert-spaces-inside-pair))
     (sp-local-pair "\\lfloor" "\\rfloor"
                    :post-handlers '(sp-latex-insert-spaces-inside-pair))
     ;; disable the default wrapping by `bi' and `be'
     (sp-local-tag "bi" nil nil :actions nil)
     (sp-local-tag "be" nil nil :actions nil)
     ))

;; Yasnippet in LaTeX
(add-hook
   'LaTeX-mode-hook
   (lambda () (yas-minor-mode-on)))


(provide 'init-latex)
;; Local Variables:
;; mode: emacs-lisp
;; End:
