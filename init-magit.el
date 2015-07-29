;; init-magit.el -- initialize the magit package


(defalias 'git 'magit-status)

(use-package magit
  :ensure t
  :commands (magit-status magit-blame magit-mode)
  
  :config
  ;; show full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  ;; restore windows when quit magit-status
  (defun magit-quit-session ()
    "Restore the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen))
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
  (add-hook 'magit-mode-hook 'magit-svn-mode))

(use-package magit-svn
  :ensure t
  :commands (magit-svn-mode))


(provide 'init-magit)


