;;;
;;; Local preferences based on host's env variables and personal data setting.
;;;
;;;-----------------------------------------------------------------


;;;
;;;  Default values
;;;


;; Load all Emacs specific personal preferences
;; and information
(setq prefs-local-file-name "~/personal/conf/emacs.el")

(when (file-readable-p prefs-local-file-name)
  (load-file prefs-local-file-name)
)

(provide 'init-local-preferences)
;; Local Variables:
;; mode: emacs-lisp
;; End:
