;;;
;;; Spell checking --- on the fly and with language guessing.
;;; 
;;;-----------------------------------------------------------------

(global-set-key [f2]  'ispell-buffer)     ;; spellcheck document
(global-set-key (kbd "M-s") 'ispell-word) ;; spellcheck word
(global-set-key (kbd "M-<f2>") 'spellcheck-cycle-language) ;; cycle languages

;; The main languages I switch between (the default is the last)"
(require 'ring)
(setq my-preferred-languages
      (ring-convert-sequence-to-ring '("british" "italiano" "english")))

;; Flyspell -- spell checking on the fly.
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (setq flyspell-duplicate-distance 0) ;; signal as repetitions only adjacent pairs
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-dictionary (ring-ref my-preferred-languages -1))  ;; first in list is default
  (if (not ispell-program-name)
      (message "Spell checking disabled: impossible to find correctly installed 'Hunspell'."))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; Puts the current language in the modeline.
(defadvice ispell-init-process (after ispell-init-process-after activate)
  (setq flyspell-mode-line-string
        (let ((lang (or ispell-local-dictionary ispell-dictionary nil)))
          (cond
           ((string-equal lang "italiano") " [IT]")
           ((string-equal lang "british")  " [GB]")
           ((string-equal lang "english") " [EN]")
           (t "")))))

        
(defun spellcheck-cycle-language ()
  "Switch between spell checking languages, in the current buffer."
  (interactive)
  (let* ((lang-ring my-preferred-languages)
         (lang (ring-ref lang-ring -1)))
        (ring-insert lang-ring lang)
        (ispell-change-dictionary lang)))

;; A nicer Helm based interface for flyspell
(use-package flyspell-correct-helm
  :after flyspell
  :bind (:map flyspell-mode-map
              ("M-s" . flyspell-correct-wrapper)))


;; Grammar check
(use-package langtool
  :if (file-directory-p "/usr/local/share/languagetool/")
  :init
  (setq langtool-language-tool-jar "/usr/local/share/languagetool/languagetool-commandline.jar")
  :config
  (setq langtool-mother-tongue "it")'
  (setq langtool-disabled-rules "WHITESPACE_RULE"))


(provide 'init-spellcheck)
;; Local Variables:
;; mode: emacs-lisp
;; End:
