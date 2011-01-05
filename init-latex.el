;;;
;;; LaTex support (AucTeX) ***
;;;
;;;-----------------------------------------------------------------

;; AucTex system
(load "auctex.el" t t t)          ;; Fail quietly
(load "preview-latex.el" t t t)   ;; Fail quietly



;; Multifile support, completition, style, reverse search support
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-specials-mode t)
(setq TeX-source-specials-view-start-server t)
(setq reftex-plug-into-AUCTeX t)


;; These are the files that are produced by LaTeX processes.
(setq TeX-byproduct-files '(".aux" ".log" ".brf" ".bbl" ".dvi" ".ps" ".pdf" "spl" "out" ".ps.gz"))

;; Ignore them while opening files
(setq ido-ignore-files (if (boundp 'ido-ignore-files)
                           (append ido-ignore-files TeX-byproduct-files)
                         TeX-byproduct-files
                         ))

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
                       (add-to-list 'LaTeX-verbatim-environments "comment")
                       ;;(TeX-fold-buffer)
                       )
            ))




;; Math writing facilities
;; (setq TeX-electric-escape t)
(setq TeX-electric-sub-and-superscript t)


;; Viewer customization
(add-hook 'LaTeX-mode-hook (lambda ()
							 (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard nil t))
							 ))

(when-running-X11-process
 (add-hook 'LaTeX-mode-hook
           (lambda ()
             ;; Use xdvi for dvi files
             (add-to-list 'TeX-output-view-style '("^dvi$" "." "%(o?)xdvi -watchfile 1 %dS %d"))
             ;; Use Xpdf or Evince for PDF files
             (add-to-list 'TeX-output-view-style '("^pdf$" "." "xpdf -remote %s -raise %o %(outpage)"))
                                                       )))


;; To help collaboration, in LaTeX file I will only use soft word
;; wrapping.  Furthermore the filling is made to an arbitrary large
;; value, so that fill-paragraph won't do hard-wrapping by error.
;;
;; From Emacs 23, the visual-line-mode helps to visualize the file
;; properly.
(add-hook 'LaTeX-mode-hook (lambda ()
                             (turn-off-auto-fill)
							 (setq  default-justification 'left)
                             (setq  fill-column 99999)
							 ))
(if (fboundp 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  )



;; Additional facilities
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

;; Auto pairs
(add-hook 'LaTeX-mode-hook 'autopair-latex-setup)
(add-hook 'TeX-mode-hook   'autopair-latex-setup)

;; RefTeX hint for automatic ref creations
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

        ))

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


;; Drag and Drop on Mac
(when-running-Aquamacs
 (add-hook
  'LaTeX-mode-hook
  (lambda ()
    (smart-dnd-setup
     '(
       ("\\.tex\\'" . "\\input{%r}\n")
       ("\\.cls\\'" . "\\documentclass{%f}\n")
       ("\\.sty\\'" . "\\usepackage{%f}\n")
       ("\\.eps\\'" . "\\includegraphics[]{%r}\n")
       ("\\.ps\\'"  . "\\includegraphics[]{%r}\n")
       ("\\.pdf\\'" . "\\includegraphics[]{%r}\n")
       ("\\.jpg\\'" . "\\includegraphics[]{%r}\n")
       ("\\.png\\'" . "\\includegraphics[]{%n}\n")
       ("\\.mov\\'" .
        "\\includemovie[\n\tposter,\n\trepeat=1,\n\ttext=(%r)\n\t]{}{}{%r}\n")
       ("\\.avi\\'" .
        "\\includemovie[\n\tposter,\n\trepeat=1,\n\ttext=(%r)\n\t]{}{}{%r}\n")))))
 )


(provide 'init-latex)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:
