;;;
;;; Local preferences based on host's env variables and personal data setting.
;;;
;;;-----------------------------------------------------------------


;; Load all Emacs specific personal preferences
;; and information
(setq prefs-local-file-name "~/personal/conf/emacs.el")

(when (file-readable-p prefs-local-file-name)
  (load-file prefs-local-file-name)
)


;;;
;;;  Default values
;;;

;; Display preferences
(setq prefs-activate-bigfont     nil) ; Big fonts for smallscreens
(setq prefs-activate-smallscreen nil) ; Monitor is small here
(setq prefs-activate-widescreen  nil) ; The screen aspect ration is wide


;; Applications
(setq prefs-activate-org-mode   t) ; The famous ORG-Mode! Yaiii!!


;;;
;;;  Host and env based preferences
;;;
(when (string= (getenv "LAPTOP") "yes")
  (setq prefs-activate-smallscreen t)
  (setq prefs-activate-bigfont     t)
  (setq prefs-activate-widescreen  t)
)

(provide 'init-local-preferences)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:
