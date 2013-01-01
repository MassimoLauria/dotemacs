;;;
;;; Unified autosave and Backup dir
;;;
;;;-----------------------------------------------------------------


;; (defvar autosave-dir (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
;; (defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))

(defvar autosave-dir "~/.emacs.d/autosaves/")
(defvar   backup-dir "~/.emacs.d/backups/")


(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))

;; Set backup dirs
(setq backup-directory-alist (list (cons "." backup-dir)))

;; No backups and autosaves for tramp files
(add-to-list 'backup-directory-alist
                (cons tramp-file-name-regexp nil))
(setq tramp-auto-save-directory nil)

;; Backup setting in Customized.
;; ;; Enable backup files.
;; (setq make-backup-files t)
;; ;; Enable versioning with default values (keep five last versions, I think!)
;; (setq version-control nil)
;; ;; Control backup
;; (setq delete-old-version t) ;; Do not ask
;; (setq kept-old-versions 5)
;; (setq kept-new-versions 5)



(provide 'init-backup)
;; Local Variables:
;; mode: emacs-lisp
;; End:
