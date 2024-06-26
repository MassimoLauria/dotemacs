;; init-magit.el -- initialize the magit package


(defalias 'git 'magit-status)


(use-package magit
  :commands (magit-status magit-blame magit-mode)
  :config
  (setq dired-vc-rename-file t)  ;; git aware dired rename
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq magit-push-always-verify nil)
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
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))

;; (use-package magit-svn
;;   :diminish magit-svn-mode
;;   :commands magit-svn-mode)

(use-package forge
   :after magit)

;; Edit commit messages with emacs
(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG\\'" . diff-mode))

(provide 'init-magit)
