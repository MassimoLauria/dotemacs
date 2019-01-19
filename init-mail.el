;;;
;;; Sending:  msmtp (if installed)
;;; Fetching: mbsync
;;; Contacts: bbdb
;;; Reading/Writing:  mu4e
;;;------------------------------------------------------------------

;; Personal email infos

;; Email addresses
(setq
 email-address-personal "lauria.massimo@gmail.com"
 email-address-academic "massimo.lauria@uniroma1.it"
 signature-file-personal "~/personal/mail/personal.sign"
 signature-file-academic "~/personal/mail/work.sign")

;;
;; Sending emails (settings for gmail)
;;
(setq smtpmail-smtp-server  "smtp.gmail.com")
(setq smtpmail-smtp-service 587)
(setq sendmail-program (executable-find "msmtp"))

(if sendmail-program
    (progn ;; msmtp
      (setq send-mail-function 'sendmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail))
  (progn ;; emacs smtpmail
    (setq send-mail-function 'smtpmail-send-it)
    (setq message-send-mail-function 'message-smtpmail-send-it)))


;;
;; Composing emails with mu4e (and message-mode)
;;
(define-key global-map "\C-cm" 'compose-mail)
(setq mu4e-compose-complete-only-personal t)
(setq mu4e-compose-dont-reply-to-self t)

(setq message-auto-save-directory "~/personal/mail/drafts")
(setq message-kill-buffer-on-exit t)
(setq mu4e-sent-messages-behavior 'delete)  ;; for gmail


;; Pick the identity
;; 1) messages to work email should be replied from work email and signature
;; 2) messages to personal email should be replied from personal email and signature
;; 3) default is work email
(add-hook 'mu4e-compose-pre-hook
  (defun my-set-from-address ()
    "Set the From address based on the To address of the original."
    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
      (when msg
        (setq user-mail-address
          (cond
            ((mu4e-message-contact-field-matches msg :to email-address-academic) email-address-academic)
            ((mu4e-message-contact-field-matches msg :to email-address-personal) email-address-personal)
            (t email-address-academic)))
        (setq message-signature-file
          (cond
            ((mu4e-message-contact-field-matches msg :to email-address-academic) signature-file-academic)
            ((mu4e-message-contact-field-matches msg :to email-address-personal) signature-file-personal)
            (t signature-file-academic)))
        ))))

(defun setup-message-mode ()
  "Setup editor for emails"
  (interactive)
  (require 'bbdb-com nil t)
  (auto-fill-mode 1)
  (set-fill-column 70)
  (define-key message-mode-map [f2] 'ispell-message))

(add-hook 'message-mode-hook 'setup-message-mode)

;;; Reading email with mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")

(use-package mu4e
  :commands (mu4e)
  :init
  ;; addresses
  (setq mu4e-user-mail-address-list (list email-address-academic
                                          email-address-personal))
  ;; maildirs
  (setq mu4e-maildir (expand-file-name "~/personal/mail/gmail-mirror")
        mu4e-drafts-folder "/drafts"
        mu4e-sent-folder  "/sent"
        mu4e-trash-folder "/trash"
        mu4e-refile-folder "/archive"
        ;; attachments go here
        mu4e-attachment-dir  "~/Downloads")

  ;; shotcuts
  (setq mu4e-maildir-shortcuts
      '( ("/inbox"    . ?i)
         ("/sent"     . ?s)
         ("/archive"  . ?a)
         ("/special"  . ?t)))
  
  ;; Tuning
  (setq mu4e-show-images t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-get-mail-command "mbsync gmail")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-headers-show-threads t)
  (setq mu4e-headers-include-related t)  
  :config
  (setq mail-user-agent 'mu4e-user-agent))


(use-package mu4e-maildirs-extension
  :after mu4e
  :init
  (setq mu4e-maildirs-extension-use-bookmarks t)
  (setq mu4e-maildirs-extension-use-maildirs  nil)
  :config
  (mu4e-maildirs-extension))


;;
;; BBDB
;;

(use-package bbdb
  :commands (bbdb bbdb-complete-mail bbdb-complete-name)
  :config
  (bbdb-initialize))

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

       
(provide 'init-mail)
;; Local Variables:
;; mode: emacs-lisp
;; End:
