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


;; Emacs >= 24.3 supports ipython inferior shell
(when (and (executable-find "ipython") 
           (version<= "24.3" emacs-version))

  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "))


;; Code analysis
(use-package anaconda-mode
  :diminish t
  :commands anaconda-mode
  :init '(add-hook 'python-mode-hook 'anaconda-mode))

;; Auto completion
(use-package company-anaconda
  :defer t
  :commands company-anaconda)

;; Syntax checker
(use-package pylint
  :commands pylint
  :init (autoload 'tramp-tramp-file-p "tramp")) ;; needs `tramp-tramp-file-p'

;; Keys
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "<f9>") 'pylint)
  (define-key python-mode-map (kbd "<f10>") 'python-shell-send-buffer))

;; Minor modes
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(add-hook 'python-mode-hook 'flycheck-mode)



;; use `rst-mode' in the docstrings 
(use-package mmm-mode
  :ensure t
  :config (progn
              (setq mmm-global-mode 'maybe)
              (mmm-add-classes
               '((python-rst
                  :submode rst-mode
                  :front "^ *[ru]?\"\"\"[^\"]*$"
                  :back "^ *\"\"\""
                  :include-front t
                  :include-back t
                  :end-not-begin t)))
              (mmm-add-mode-ext-class 'python-mode nil 'python-rst)))





;;; Virtual Environments -----------------------------------------------

(use-package virtualenvwrapper
  :ensure t
  :init (setq venv-location (concat (getenv "HOME") "/.virtualenvs/"))
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
