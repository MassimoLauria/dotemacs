;;;
;;; Spell checking --- on the fly and with language guessing.
;;; 
;;;-----------------------------------------------------------------

(global-set-key [f2]  'language-check-dwim)  ;; spellcheck document
(global-set-key (kbd "M-s") 'ispell-word)    ;; spellcheck word
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
           ((string-equal lang "english")  " [EN]")
           ((string-equal lang "british")  " [GB]")
           (t "")))))

        
(defun spellcheck-cycle-language ()
  "Switch between spell checking languages, in the current buffer."
  (interactive)
  (let* ((lang-ring my-preferred-languages)
         (lang (ring-ref lang-ring -1)))
        (ring-insert lang-ring lang)
        (ispell-change-dictionary lang)
        (setq langtool-default-language 
              (cond
               ((string-equal lang "italiano") "it")
               ((string-equal lang "english")  "en")
               ((string-equal lang "british")  "en-GB")
               (t "")))))


;; Spell/Grammar check command
(defun language-check-dwim () 
  "Launch either spell check or grammar check

Offer a choice between spell checking the buffer, or grammar
checking it. It a region is active the spell check will be
performed on that region. If some grammar checking session is
open, the command will just close it.
"
  (interactive)
   ;; If grammar check is active, close it
  (if langtool-mode-line-message
      (langtool-check-done)
    ;; otherwise offer a choice
    (let* ((choices '("spelling" "grammar" "none"))
           (selection (ido-completing-read "Check for " choices )))
      (pcase selection
        ("spelling"
         (if (region-active-p)
             (call-interactively 'ispell-region)
           (ispell-buffer)))
        ("grammar" (langtool-check-buffer))
        (otherwise nil))
      )))



;; A nicer Helm based interface for flyspell
(use-package flyspell-correct-helm
  :after flyspell
  :bind (:map flyspell-mode-map
              ("M-s" . flyspell-correct-wrapper)))


;; Grammar check
(when (file-directory-p "/usr/local/share/languagetool/")
  (use-package langtool
    :init
    (setq langtool-language-tool-jar "/usr/local/share/languagetool/languagetool-commandline.jar")
    :config
    (setq langtool-mother-tongue "it")'
    (setq langtool-disabled-rules "WHITESPACE_RULE")
    :commands (langtool-check langtool-check-buffer langtool-switch-default-language)))

(provide 'init-spellcheck)
;; Local Variables:
;; mode: emacs-lisp
;; End:
