;;;
;;; These setting only involves writing/sending mail. Mail reading is not done
;;; in Emacs.
;;;------------------------------------------------------------------


;;
;; Sending emails
;;

(setq smtpmail-smtp-server private-smtp-server)
(setq smtpmail-smtp-service  private-smtp-port)
(setq sendmail-program (executable-find "msmtp"))

(if sendmail-program
    (progn ;; msmtp
      (setq send-mail-function 'sendmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail))
  (progn ;; emacs smtpmail
    (setq send-mail-function 'smtpmail-send-it)
    (setq message-send-mail-function 'message-smtpmail-send-it)))



;;
;; Composing emails
;;

(define-key global-map "\C-cm" 'compose-mail)

(setq compose-mail-user-agent-warnings nil)
(setq mail-user-agent 'message-user-agent)
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq message-signature private-email-signature)
(setq message-auto-save-directory "~/personal/mail/drafts")
(setq message-kill-buffer-on-exit t)

(defun setup-message-mode ()
  "Setup editor for emails"
  (interactive)
  (auto-fill-mode 1)
  (set-fill-column 70)
  (define-key message-mode-map [f2] 'ispell-message))

(add-hook 'message-mode-hook 'setup-message-mode)

(require 'post) ; setup emacs to edit for mutt.

(defun setup-post-mode ()
  "Setup editor for post-mode"
  (interactive)
  (set (make-local-variable 'mail-header-separator) "")
  (set-fill-column 70)
  (define-key post-mode-map [f2] 'ispell-message))

(add-hook 'post-mode-hook 'setup-post-mode)

;; BBDB auto-completion
(eval-after-load "bbdb" 
  '(progn
     (add-hook 'message-mode-hook  'turn-on-ac-bbdb)
     (add-hook 'post-mode-hook     'turn-on-ac-bbdb)
     (add-hook 'mml-mode-hook      'turn-on-ac-bbdb)
     (add-hook 'mail-mode-hook     'turn-on-ac-bbdb)))

;;
;; Email contacts
;;

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
