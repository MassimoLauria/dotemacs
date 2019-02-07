;;;
;;; Python Mode configuration.
;;;
;;; - uses default `python.el'
;;; - be sure anaconda-mode, pythonic, company-anaconda are in sync
;;; - makes use of pyenv 
;;; - avoid using ipython as a shell. Too many problem if it is not
;;;   well configured
;;; 
;;; -------------------------------------------------------------------


;; Use pyenv binaries if available
(add-to-list 'exec-path (concat (getenv "HOME") "/.pyenv/shims"))
(environment-add-path (concat (getenv "HOME") "/.pyenv/shims"))  ;; Pyenv 

(setq python-indent-offset 4)

;; Code analysis
(use-package anaconda-mode
  :diminish nil
  :commands anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config
  (define-key anaconda-mode-map  (kbd "M-/") 'anaconda-mode-show-doc)
  (define-key anaconda-mode-map  (kbd "M-.") 'anaconda-mode-find-definitions)
  (define-key anaconda-mode-map  (kbd "M-,") 'pop-tag-mark)
  (define-key anaconda-mode-map  (kbd "M-r") nil))

;; Auto completion
(use-package company-anaconda
  :after (anaconda-mode company)
  :commands company-anaconda
  :config (add-to-list 'company-backends 'company-anaconda))

;; Syntax checker
(use-package pylint
  :commands pylint
  :init (autoload 'tramp-tramp-file-p "tramp")) ;; needs `tramp-tramp-file-p'

;; Keys
(with-eval-after-load "python"
  (define-key python-mode-map (kbd "<f10>") 'python-shell-switch-to-shell)
  (define-key inferior-python-mode-map (kbd "<f10>") 'delete-window))

;; Minor modes
(add-hook 'python-mode-hook 'flycheck-mode)


;;; Virtual Environments -----------------------------------------------

(use-package pyenv-mode
  :commands (pyenv-mode-set pyenv-mode-unset pyenv-mode))

(use-package pyenv-mode-auto
  :commands pyenv-mode-auto-hook
  :init (add-hook 'find-file-hook #'pyenv-mode-auto-hook)
  :config (pyenv-mode))

(provide 'init-python)
;; Local Variables:
;; mode: emacs-lisp
;; End:
