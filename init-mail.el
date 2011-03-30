;;;
;;; Mail: MailMode + BBDB + vCard
;;;
;;; These setting only involves writing mail. Mail reading is not done
;;; in Emacs (old wanderlust settings are commented out or deleted).
;;;------------------------------------------------------------------

(define-key global-map "\C-cm" 'compose-mail)


;; Setup MAIL EDITOR
(setq compose-mail-user-agent-warnings nil)
(setq mail-user-agent 'message-user-agent)
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq message-auto-save-directory "~/personal/mail/drafts")
(add-hook 'message-mode-hook 'turn-on-auto-fill)
;; Click urls/mails in Mime-View
(add-hook 'mime-view-mode-hook 'goto-address-mode)


;; Setup SEND MAIL
(setq smtpmail-smtp-server private-smtp-server)
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server private-smtp-server
      smtpmail-smtp-server private-smtp-server
      smtpmail-smtp-service private-smtp-port
      smtpmail-debug-info t)

;; Setup SEND MAIL Authentication
(setq starttls-use-gnutls t)
(setq smtpmail-starttls-credentials `((,private-smtp-server
                                       ,private-smtp-port nil nil)))

(setq auth-file-name (expand-file-name "~/personal/keys/authinfo.gpg"))

(if (file-exists-p auth-file-name)
    (setq smtpmail-auth-credentials auth-file-name) ; Load password
                                                    ; from encrypted
                                                    ; file
  (setq smtpmail-auth-credentials
        `((,private-smtp-server
           ,private-smtp-port ,private-smtp-login nil))) ; Password must be inserted
)

(require 'smtpmail)


;; (Insidious) Big Brother DataBase, collects mail addresses.

;; MacOSX alternative path
(when running-MacOSX
  (setq MacUser-bbdb-path (concat MacUser-site-lisp "/bbdb"))
  (if (file-directory-p MacUser-bbdb-path)
      (add-to-list 'load-path MacUser-bbdb-path))
)
;; Load bbdb
(and
 (require-maybe 'bbdb)
 (bbdb-initialize)
)

;; Say NO! to auto collection
(setq bbdb/mail-auto-create-p nil)
(setq bbdb-north-american-phone-numbers-p nil)

;; automatically add mailing list fields
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))))


(setq

    bbdb-file "~/personal/agenda/contacts.bbdb"

    bbdb-offer-save 1                        ;; 1 means save-without-asking

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

    bbdb-elided-display t                    ;; single-line addresses

)

;; vCard + BBDB-vCard
(setq load-path (append load-path (list (concat default-elisp-3rdparties "/bbdb-vcard") )))

(autoload 'bbdb-vcard-import-region       "bbdb-vcard" "Import vCard entries in the selected region into BBDB database." t)
(autoload 'bbdb-vcard-import-buffer       "bbdb-vcard" "Import vCard entries in the buffer into BBDB database." t)
(autoload 'bbdb-vcard-import-file         "bbdb-vcard" "Import vCard entries from a file into BBDB database." t)
(autoload 'bbdb-vcard-export              "bbdb-vcard" "Export BBDB entries to a vCard file." t)
(autoload 'bbdb-vcard-export-to-kill-ring "bbdb-vcard" "Export BBDB entries to vCard, and put the text in the killring." t)


(provide 'init-mail)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:
