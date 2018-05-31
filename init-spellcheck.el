;;;
;;; Spell checking --- on the fly and with language guessing.
;;; 
;;;-----------------------------------------------------------------


;; Spellcheck the whole text
(global-set-key [f2]  'ispell-buffer)  

;; Switch among the chosen languages
(global-set-key (kbd "M-<f2>") 'spellcheck-language-cycle)


;; Flyspell -- spell checking on the fly.
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (setq ispell-program-name (executable-find "hunspell"))
  (if (not ispell-program-name)
      (message "Spell checking disabled: impossible to find correctly installed 'Hunspell'."))
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :diminish t)

; Automatically detect language for Flyspell
(use-package guess-language         
  :commands (guess-language-mode guess-language spellcheck-cycle-language)
  :init (add-hook 'flyspell-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("american" "American"))
                                   (it . ("italiano" "Italian")))
        guess-language-languages '(en it)
        guess-language-min-paragraph-length 45)
  :diminish t)


(defun spellcheck-cycle-language ()
  "Switch between spell checking languages, in the current buffer."
  (interactive)
  (let (new-pos
        old-pos)
    (setq old-pos (position guess-language-current-language guess-language-languages))
    (setq new-pos (cond
                   ((eq old-pos nil)                                    0)               ;; not found
                   ((= old-pos (- (length guess-language-langcodes) 1)) 0)               ;; last position
                   (t                                                   (+ old-pos 1)))) ;; any other pos

    (ispell-change-dictionary (car (cdr (nth new-pos guess-language-langcodes))))
    (setq guess-language-current-language (car (nth new-pos guess-language-langcodes)))
    ))


(provide 'init-spellcheck)
;; Local Variables:
;; mode: emacs-lisp
;; End:
