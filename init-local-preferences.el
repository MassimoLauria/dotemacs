;;;
;;; Local preferences based on host's env variables and personal data setting.
;;;
(provide 'init-local-preferences)
;;;-----------------------------------------------------------------


;; First of all, load all Emacs specific personal preferences
;; and information
(setq prefs-local-file-name "~/personal/conf/emacs.el")

(when (file-readable-p prefs-local-file-name)
  (load-file prefs-local-file-name)
)

;;;
;;;  Default values
;;;


;; Math applications
(setq prefs-activate-latex      t) ; Enable auctex
(setq prefs-activate-maxima   nil) ; Imaxima and Imath
(setq prefs-activate-sage     nil) ; Sagemath 
(setq prefs-activate-singular nil) ; Singular


;; Display preferences
(setq prefs-activate-bigfont     nil) ; Big fonts for smallscreens
(setq prefs-activate-smallscreen nil) ; Monitor is small here
(setq prefs-activate-widescreen  nil) ; The screen aspect ration is wide


;; Applications
(setq prefs-activate-mail     t) ; Mail support
(setq prefs-activate-org-mode t) ; The famous ORG-Mode! Yaiii!!
(setq prefs-activate-twitter  t) ; Explicit activation of Twitter


;;;
;;;  Host and env based preferences
;;;
(when (string= (getenv "SAGEMATH") "yes")
  (setq prefs-activate-sage t)
)

(when (string= (getenv "LAPTOP") "yes")
  (setq prefs-activate-smallscreen t)
  (setq prefs-activate-bigfont     t)
  (setq prefs-activate-widescreen  t)
)


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
