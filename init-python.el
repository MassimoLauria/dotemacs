;;;
;;; Python Mode configuration.
;;;
(provide 'init-python)

;;;-------------------------------------------------------------------


;;; Try to set up IPython Shell. -------------------------------------------------------------------

;; We force the ipython path to avoid troubles with SAGE math version.
(setq ipython-command "/usr/bin/ipython2.6")
(when (require-maybe 'ipython)
  (setq py-python-command-args '("-pylab" "-colors" "Linux"))
  (setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")
)


;; Python Hook(s) ----------------------------------------------------------------------------------
(add-hook 
 'python-mode-hook (lambda ()
                     (set-variable 'py-indent-offset 4)
                     ;;(set-variable 'py-smart-indentation nil)
                     ;;(set-variable 'indent-tabs-mode nil)
                     (define-key py-mode-map (kbd "RET") 'newline-and-indent)
                     )
 )

;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))

(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)))


;; Pymacs -----------------------------------------------------------------------------------------
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
;;(eval-after-load "pymacs"
;; '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)
 


;; Auto Syntax Error Hightlight (very preliminary and with poor support) --------------------------
(when (load "flymake" t)
  (defun flymake-pylint-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
	  (list "~/config/emacs/scripts/epylint.py" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)





(provide 'init-python)
;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 

