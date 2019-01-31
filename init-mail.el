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

 signature-personal (with-temp-buffer
                      (insert-file-contents "~/personal/mail/personal.sign")
                      (buffer-string))

 signature-academic (with-temp-buffer
                      (insert-file-contents "~/personal/mail/work.sign")
                      (buffer-string)))


;;
;; Sending emails (settings for gmail)
;;
(setq smtp-password (string-trim
                     (shell-command-to-string "python -c \"import keyring; print(keyring.get_password('smtp.gmail.com','lauria.massimo@gmail.com'))\"")))

(setq smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-user    "lauria.massimo@gmail.com"
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t 
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))

(setq sendmail-program (executable-find "msmtp"))

(if sendmail-program
    (progn ;; msmtp
      (setq send-mail-function 'sendmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail))
  (progn ;; emacs smtpmail (and queueing)
    (setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'message-smtpmail-send-it
          smtpmail-queue-dir "~/personal/mail/queue/cur/"
          smtpmail-queue-mail nil)))


;;
;; Composing emails with mu4e (and message-mode)
;;
(define-key global-map "\C-cm" 'compose-mail)
(setq mu4e-compose-complete-only-personal t)
(setq mu4e-compose-dont-reply-to-self t)

(setq message-auto-save-directory "~/personal/mail/drafts")
(setq message-kill-buffer-on-exit t)
(setq mu4e-sent-messages-behavior 'delete)  ;; for gmail


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


(defun icon-on-linux (icon name)
  "Apply the icon only on linux."
  (concat
   (or (and (eq system-type 'gnu/linux) icon) "")
   name))

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

  (setq mu4e-other-folders
        (cl-set-difference (directory-files mu4e-maildir)
                           '("." ".." "inbox" "archive" "drafts" "sent" "spam" "trash" "special") :test 'equal))

  ;; Update in the background, silently
  (setq mu4e-hide-index-messages t)
  (setq mu4e-update-interval 100)


  
  ;; no shotcuts (uses bookmarks instead)
  (setq mu4e-maildir-shortcuts nil)

  ;; Tuning
  (setq mu4e-show-images t)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-get-mail-command "mbsync gmail")
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-headers-show-threads t)
  (setq mu4e-headers-include-related t)

  (setq mu4e-compose-complete-addresses t)
  (setq mu4e-compose-complete-only-personal nil)
  (setq mu4e-compose-complete-only-after "2012-01-01")

  (setq mu4e-confirm-quit nil)

  :config
  (setq mail-user-agent 'mu4e-user-agent)

  ;; ---------------------------
  ;; Setup the list of maildirs
  ;; ---------------------------
  (setq mu4e-bookmarks (list
                        (make-mu4e-bookmark
                         :name "———————————————————————————"
                         :query "date:2999"
                         :key ?{)
                        (make-mu4e-bookmark
                         :name (icon-on-linux "📬 " "Posta in arrivo")
                         :query "maildir:/inbox"
                         :key ?i)
                        (make-mu4e-bookmark
                         :name (icon-on-linux "→ " "Inviati (ultimo mese)")
                         :query "maildir:/sent AND date:1m..now"
                         :key ?s)
                        (make-mu4e-bookmark
                         :name (icon-on-linux "📧 " "Tutti i messaggi (ultimo mese)")
                         :query "maildir:/archive AND date:1m..now"
                         :key ?a)
                        (make-mu4e-bookmark
                         :name (icon-on-linux "☆ " "Speciali")
                         :query "maildir:/special"
                         :key ?t)
                        (make-mu4e-bookmark
                         :name (icon-on-linux "🗋 " "Bozze")
                         :query "maildir:/drafts"
                         :key ?d)
                        (make-mu4e-bookmark
                         :name (icon-on-linux "📎" "Con allegato")
                         :query "flag:attach"
                         :key ?A)
                        (make-mu4e-bookmark
                         :name "———————————————————————————"
                         :query "date:2999"
                         :key ?-)))

  ;; Put in the bookmarks all the user folders with increasing
  ;; bookmark shortcut (won't work with more than 9 bookmarks)
  (loop for label in mu4e-other-folders
        with i = 1
        do
        (add-to-list 'mu4e-bookmarks
                        (make-mu4e-bookmark
                         :name  (icon-on-linux "🏷 " label)
                         :query (concat "maildir:/" label)
                         :key (string-to-char (number-to-string i)))
                        'append)
        (setq i (+ i 1)))

  ;; 
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "———————————————————————————"
                :query "date:2999"
                :key ?})
               'append)


  ;; ----------------------------------
  ;; Actions in view and headers panes
  ;; ----------------------------------
  (add-to-list 'mu4e-view-actions
  '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  (add-to-list 'mu4e-headers-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
               '("org-contact-add" . mu4e-action-add-org-contact) t)


  ;; -----------------
  ;; Setup identities
  ;; -----------------
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Academic"
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to email-address-academic)))
             :vars `( ( user-mail-address      . ,email-address-academic  )
                      ( mu4e-compose-signature . ,signature-academic)))

           ,(make-mu4e-context
             :name "Personal"
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 :to email-address-personal)))
             :vars `( ( user-mail-address      . ,email-address-personal  )
                      ( mu4e-compose-signature . ,signature-personal))))))

;;
;; Contacts
;;

(use-package org-contacts
  :commands (org-contacts org-contacts-anniversaries)
  :init
  ;; addresses
  (setq org-contacts-files '("~/personal/agenda/contacts.org"))
  (setq mu4e-org-contacts-file  (car org-contacts-files)))
  


(provide 'init-mail)
;; Local Variables:
;; mode: emacs-lisp
;; End:
