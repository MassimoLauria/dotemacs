;;;
;;; Mail: MailMode + BBDB + vCard
;;;
;;; These setting only involves writing mail. Mail reading is not done
;;; in Emacs (old wanderlust settings are commented out or deleted).
;;;------------------------------------------------------------------

(define-key global-map "\C-cm" 'compose-mail)

;; Setup SEND MAIL
(setq sendmail-program (executable-find "msmtp"))
(if sendmail-program
    (progn ;; send mail using msmtp
      (setq send-mail-function 'sendmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail))
  (progn ;; Use emacs smtp client smtpmail
    (setq send-mail-function 'smtpmail-send-it)
    (setq message-send-mail-function 'message-smtpmail-send-it)))

(setq smtpmail-smtp-server private-smtp-server)
(setq smtpmail-smtp-service  private-smtp-port)



;; Setup MAIL EDITOR
(setq compose-mail-user-agent-warnings nil)
(setq mail-user-agent 'message-user-agent)
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq message-signature private-email-signature)
(setq message-auto-save-directory "~/personal/mail/drafts")
(setq message-kill-buffer-on-exit t)

(add-to-list 'auto-mode-alist '("mutt*" . message-mode))

;; Mu4e view in browser
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))



;; Try to load mu4e client if installed.
(setq mu4e-headers-toggle-full-search nil)
(setq mu4e-use-fancy-chars nil)

(when (require 'mu4e nil t)
  (defalias 'mail 'mu4e)
  (setq mail-user-agent 'mu4e-user-agent)
  (define-key mu4e-main-mode-map "q" 'bury-buffer)
  (if (not (boundp 'mu4e-view-actions)) 
      (setq mu4e-view-actions nil))
  (add-to-list 'mu4e-view-actions
               '("View in browser" . mu4e-msgv-action-view-in-browser) t)
  (defun mu4e-message (frm &rest args)) ;; hack to avoid the annoying massages 
  )

;; Click urls/mails in Mime-View
(add-hook 'mime-view-mode-hook 'goto-address-mode)
(add-hook 'message-mode-hook
          (lambda () (define-key message-mode-map [f2] 'ispell-message)))

;; (Insidious) Big Brother DataBase, collects mail addresses.

;; Load bbdb
(and
 (require 'bbdb nil t)
 (bbdb-initialize)
)

;; Say NO! to auto collection
(setq bbdb/mail-auto-create-p nil)
(setq bbdb-north-american-phone-numbers-p nil)

;; automatically add mailing list fields
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))))


;; Save address from outgoing mails
;; (require-maybe 'moy-bbdb-modified)
;; (when-available 'bbdb/send-hook
;;                 (add-hook 'message-send-hook 'bbdb/send-hook)
;;                 )

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

    bbdb-send-mail-style 'message
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
;; End:
