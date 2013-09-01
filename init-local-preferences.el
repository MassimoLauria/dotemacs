;;;
;;; Local preferences based on host's env variables and personal data setting.
;;;
;;;-----------------------------------------------------------------


;;;
;;;  Default values
;;;

;; Display preferences
(set-default 'prefs-activate-bigfont     nil) ; Big fonts for smallscreens
(set-default 'prefs-activate-smallscreen nil) ; Monitor is small here
(set-default 'prefs-activate-widescreen  t)   ; The screen aspect ration is wide


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
