;;;
;;; Mail: Wanderlust + BBDB
;;;
(provide 'init-mail-wl)
;;;------------------------------------------------------------------


;; autoload configuration
;; (Not required if you have installed Wanderlust as XEmacs package)
(require-maybe 'elscreen-wl)
(setq 
 wl-init-file    "~/config/emacs/wanderlust.el"
 wl-folders-file "~/personal/conf/wanderlust-folders"
 wl-address-file "~/personal/agenda/contacts.wl"
 bbdb-file       "~/personal/agenda/contacts.bbdb"
)


;; Define a dummy default value if no one is defined.
(if (boundp 'private-smtp-domain)
    (setq wl-message-id-domaim private-smtp-domain)
    (setq wl-message-id-domain "anydomain.com")
)

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)


;; Mail Composing - Use wl-draft to compose messages.
;; notice that this should be autoloaded before wl file.
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


(add-hook
 'wl-draft-mode-hook
 '(lambda ()
    ;; Features in draft-mode
    (auto-complete-mode)
    (set-fill-column 75)
    (wl-draft-highlight-and-recenter)
    )
 )


(setq wl-draft-elide-ellipsis "[...]" )
(define-key wl-draft-mode-map (kbd "<C-tab>") 'bbdb-complete-name)


(defun mail()
" 
Overload the original 'mail' command with 'wl-draft'. 
Both are intended to draft and send emails
"
  (interactive)
  (wl-draft '(
              (To . "") 
              (Subject . "")
              ) 
            )
  (mail-position-on-field "To")
)

(define-key global-map "\C-cm" 'mail)


;; (Insidious) Big Brother DataBase, collects mail addresses.
(require 'bbdb)
(bbdb-initialize)

(bbdb-insinuate-sendmail)
(define-key mail-mode-map (kbd "<C-tab>") 'bbdb-complete-name)


;; Say NO! to auto collection
(setq bbdb/mail-auto-create-p nil)
(setq bbdb-north-american-phone-numbers-p nil)

;; automatically add mailing list fields
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))))


(setq
    
    bbdb-offer-save t                        ;; 1 means save-without-asking
     
    bbdb-use-pop-up nil                      ;; allow popups for addresses
    bbdb-electric-p t                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small
    
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches t       ;; show name-mismatches 2 secs

    bbdb-always-add-address t                ;; add new addresses to existing...
                                             ;; ...contacts automatically
    bbdb-canonicalize-redundant-nets-p nil   ;; x@foo.bar.cx => x@bar.cx

    bbdb-completion-type nil                 ;; complete on anything

    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially

    bbbd-message-caching-enabled t           ;; be fast
    bbdb-use-alternate-names t               ;; use AKA

)

;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 