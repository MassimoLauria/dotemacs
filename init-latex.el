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

;; XDvi launch customization
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard nil t))
                             ))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'TeX-output-view-style 
                                          '("^dvi$" "." 
                                            "%(o?)xdvi -watchfile 1 %dS %d"))
                             ))


;; Auto fill for LaTex
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  
 
;; TeX asks for Flyspell and American dictionary.
; (add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'TeX-language-en-hook
	  (lambda () (ispell-change-dictionary "american")))
(add-hook 'TeX-language-it-hook
	  (lambda () (ispell-change-dictionary "italian")))


;; Flymake mode for LaTeX
; it is not enabled by default, but it is ready to go.
(defun flymake-get-tex-args (file-name)
  (list "latex" (list "-file-line-error-style" 
                      "-draftmode" 
                      "-interaction=nonstopmode" file-name)))


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
