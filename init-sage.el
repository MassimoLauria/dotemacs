;;;
;;;  Init file for SAGE math environment in Emacs
;;;
;;;------------------------------------------------------------------


(defvar sagemath-root-directory nil
"The position in which SageMath software is installed.")

;; Find Sagemath on the system
(if (executable-find "sage")
    (let ((tmp nil))
      (setq tmp (shell-command-to-string "sage -root"))
      (when (string-match "[ \t\n]*$" tmp)
        (setq sagemath-root-directory (replace-match "" nil nil tmp)))))

;; Find the emacs code 
(when sagemath-root-directory
  (add-to-list 'load-path (concat sagemath-root-directory "/data/emacs"))
  (setq sage-command (concat sagemath-root-directory "/sage"))
)

;; ;; Load of the package (disabled unless sage-mode 0.8 is installed)
;; (when sagemath-root-directory
;;   (require 'help-mode)
;;   (require 'dired-aux)
;;   (require 'sage "sage" t)
;; )

;; If you want sage-view to typeset all your output and have plot()
;; commands inline, uncomment the following line and configure sage-view:
;; (when (require 'sage-view "sage-view" t)
;;   (add-hook 'sage-startup-after-prompt-hook
;;             'sage-view
;;             'sage-view-disable-inline-output
;;             'sage-view-enable-inline-plots))

(provide 'init-sage)
;; Local Variables:
;; mode: emacs-lisp
;; End:
