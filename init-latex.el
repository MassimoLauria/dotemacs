;;;
;;; LaTex support (AucTeX) ***
;;;
(provide 'init-latex)
;;;-----------------------------------------------------------------

;; AucTex system
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)



;; Multifile support, completition, style, reverse search support 
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-specials-mode t)
(setq TeX-source-specials-view-start-server t)
(setq reftex-plug-into-AUCTeX t)

;; Macro are folded.
(defun TeX-fold-this-paragraph-toggle ()
  "If TeX-fold-mode is active then alternate between folded and not folded text in a paragraph"
  (interactive)
  (unless (TeX-fold-clearout-paragraph)
    (TeX-fold-paragraph)
    )
  )
(defun TeX-fold-this-buffer-toggle ()
  "If TeX-fold-mode is active then alternate between folded and not folded text in a paragraph"
  (interactive)
  (unless (TeX-fold-clearout-buffer)
    (TeX-fold-buffer)
    )
  )
(add-hook 'LaTeX-mode-hook 
          (lambda () (progn 
                       ;;(TeX-fold-mode 1) 
                       ;;(local-set-key (kbd "M-<SPC>") 'TeX-fold-this-paragraph-toggle) ;;Folding on/off
                       (local-set-key (kbd "<f9>")    'TeX-command-master) ;; Compile
                       (make-local-variable compilation-exit-message-function)
                       (setq compilation-exit-message-function 'nil)
                       ;;(TeX-fold-buffer)
                       )
            ))
  

;; Math writing facilities
;; (setq TeX-electric-escape t)
(setq TeX-electric-sub-and-superscript t)

	
;; XDvi launch customization
(add-hook 'LaTeX-mode-hook (lambda ()
							 (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard nil t))
							 ))

(add-hook 'LaTeX-mode-hook (lambda ()
							 ;; Use xdvi for dvi files
							 (add-to-list 'TeX-output-view-style 
										  '("^dvi$" "." 
											"%(o?)xdvi -watchfile 1 %dS %d"))
							 ;; Use Xpdf or Evince for PDF files
							 (add-to-list 'TeX-output-view-style 
											'("^pdf$" "." 
											  "xpdf -remote %s -raise %o %(outpage)"))
							 (add-to-list 'TeX-output-view-style 
											'("^pdf$" "." 
											  "evince %o"))
							 ))


;; Auto fill for LaTex
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  
 
;; TeX asks for Flyspell and American dictionary.
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'TeX-language-en-hook
	  (lambda () (ispell-change-dictionary "english")))
(add-hook 'TeX-language-it-hook
	  (lambda () (ispell-change-dictionary "italian")))


;; Flymake mode for LaTeX
; it is not enabled by default, but it is ready to go.
(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-q" "-v0" file-name)))

(defun my-flymake-show-help ()
   (when (get-char-property (point) 'flymake-overlay)
     (let ((help (get-char-property (point) 'help-echo)))
       (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)



;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
