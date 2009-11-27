;;;
;;; Mail: Wanderlust + BBDB
;;;
(provide 'init-mail-wl)
;;;------------------------------------------------------------------


;; autoload configuration
;; (Not required if you have installed Wanderlust as XEmacs package)
(require-maybe 'elscreen-wl)
(setq 
 wl-init-file    "~/config/mail/wl"
 wl-folders-file "~/config/mail/folders"
 wl-address-file "~/config/mail/addresses"
 bbdb-file       "~/personal/contacts.bbdb"
 diary-file      "~/personal/diary"
)



(setq wl-message-id-domaim "gmail.com")
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)


;; Use wl-draft to compose messages.
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; (Insidious) Big Brother DataBase, collects mail addresses.
(require 'bbdb)
(bbdb-initialize)
(setq bbdb-north-american-phone-numbers-p nil)


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 