;;;
;;; Python Mode configuration.
;;;
;;; If Emacs version >= 24.3 then `python.el' supports ipython out of
;;; the box. For older emacs we stick with canonical `python'
;;; interpreter.
;;;
;;; This setup avoids downloading external python modes like
;;; `python-mode.el', and should gracefully suck on older installation
;;; (that I do not use very often).
;;; -------------------------------------------------------------------


;; Emacs >= 24.3 supports ipython inferior shell out of the box but
;; IPython >=5.0.0 has a new prompt technology that screws with Emacs.
;; We fix it temporarily using its simple interface.
(when (and (executable-find "ipython") 
           (version<= "24.3" emacs-version))
  
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt --pprint"))


;; Code analysis
(use-package anaconda-mode
  :ensure t
  :diminish t
  :commands anaconda-mode
  :init '(add-hook 'python-mode-hook 'anaconda-mode))

;; Auto completion
(use-package company-anaconda
  :ensure t
  :commands company-anaconda)

;; Syntax checker
(use-package pylint
  :commands pylint
  :init (autoload 'tramp-tramp-file-p "tramp")) ;; needs `tramp-tramp-file-p'

;; Keys
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "<f10>") 'python-shell-switch-to-shell)
  (define-key inferior-python-mode-map (kbd "<f10>") 'delete-window))

;; Minor modes
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'flycheck-mode)


;;; Virtual Environments -----------------------------------------------

(use-package virtualenvwrapper
  :ensure t
  :init (setq venv-location (concat (getenv "HOME") "/.virtualenvs/"))
        (put 'python-project-venv-name 'safe-local-variable #'stringp)
  :commands (venv-workon venv-mkvirtualenv)
  :config (venv-initialize-eshell))
       
;; if buffer or dir local variable `python-project-venv-name' is set
;; to a string, the corresponding virtual environment is activated.
(defun ml/automatic-virtualenv-activation ()
  (hack-local-variables)
  (when (boundp 'python-project-venv-name)
    (venv-workon python-project-venv-name)))

(add-hook 'python-mode-hook 'ml/automatic-virtualenv-activation 'append)

(provide 'init-python)
;; Local Variables:
;; mode: emacs-lisp
;; End:
