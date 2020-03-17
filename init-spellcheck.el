;;;
;;; Spell checking --- on the fly and with language guessing.
;;; 
;;;-----------------------------------------------------------------

(global-set-key [f2]  'ispell-buffer)     ;; spellcheck document
(global-set-key (kbd "M-s") 'ispell-word) ;; spellcheck word
(global-set-key (kbd "M-<f2>") 'spellcheck-cycle-language) ;; cycle languages


(defvar my-preferred-languages
  (list "american" "italiano")
  "The two main languages i switch between (default first)")

;; Flyspell -- spell checking on the fly.
(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-dictionary (car my-preferred-languages))  ;; first in list is default
  (if (not ispell-program-name)
      (message "Spell checking disabled: impossible to find correctly installed 'Hunspell'."))
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :diminish nil)

(defun spellcheck-cycle-language ()
  "Switch between spell checking languages, in the current buffer."
  (interactive)
  (let* ((old-pos (position ispell-current-dictionary
                            my-preferred-languages
                            :test 'equal))
         (new-pos (cond
                   ((eq old-pos nil)                                  0)               ;; not found
                   ((= old-pos (- (length my-preferred-languages) 1)) 0)               ;; last position
                   (t                                                 (+ old-pos 1))))) ;; any other pos

    (ispell-change-dictionary (nth new-pos my-preferred-languages))))

;; A nicer Helm based interface for flyspell
(use-package flyspell-correct-helm
  :after flyspell
  :bind (:map flyspell-mode-map
              ("M-s" . flyspell-correct-wrapper)))

(provide 'init-spellcheck)
;; Local Variables:
;; mode: emacs-lisp
;; End:
