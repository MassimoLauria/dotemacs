;;;
;;; Sending:  msmtp (if installed)
;;; Fetching: mbsync
;;; Contacts: org-contacts / helm-org-contacts
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

(define-key global-map "\C-cm" 'mu4e-compose-new)



;; Sending emails (settings for gmail)
;; smtpmail-smtp-user set by mu4e-context
(setq smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))

(setq sendmail-program (executable-find "msmtp"))

(if sendmail-program
    (progn ;; msmtp
      (setq mail-specify-envelope-from t)  ;; otherwise MSMTP sends
                                           ;; with the wrong account
                                           ;; and it gets marked
                                           ;; as SPAM.
      (setq send-mail-function 'sendmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail))
  (progn ;; emacs smtpmail (and queueing)
    (setq send-mail-function 'smtpmail-send-it
          message-send-mail-function 'message-smtpmail-send-it
          smtpmail-queue-dir "~/personal/mail/queue/cur/"
          smtpmail-queue-mail nil)))
;;; Reading email with mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")


(defun icon-on-linux (icon name)
  "Apply the icon only on linux."
  (concat
   (or (and (eq system-type 'gnu/linux) icon) "")
   name))

(use-package mu4e
  :commands (mu4e mu4e-compose-new)
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
        mu4e-attachment-dir  "~/queue")

  (setq mu4e-other-folders
        (cl-set-difference (if (file-accessible-directory-p mu4e-maildir)
                               (directory-files mu4e-maildir)
                             nil)
                           '("." ".." "inbox" "archive" "drafts" "sent" "spam" "trash" "special") :test 'equal))

  ;; Update in the background, silently
  (setq mu4e-hide-index-messages t)
  (setq mu4e-update-interval 100)



  ;; no shotcuts (uses bookmarks instead)
  (setq mu4e-maildir-shortcuts nil)

  ;; Tuning

  (setq mu4e-split-view nil)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-show-images t)
  (setq mu4e-view-image-max-height 300)
  (setq mu4e-view-image-max-width 400)
  (setq mu4e-completing-read-function 'helm-comp-read)

  (setq mu4e-get-mail-command "mbsync gmail")
  (setq mu4e-change-filenames-when-moving t)

  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-headers-show-threads t)
  (setq mu4e-headers-include-related t)

  (setq mu4e-headers-date-format "%e %b '%y")

  (setq mu4e-compose-complete-addresses t)
  (setq mu4e-compose-complete-only-personal nil)
  (setq mu4e-compose-complete-only-after "2012-01-01")
  (setq mu4e-compose-dont-reply-to-self t)

  ;; gmail profile should use 'delete
  ;; uniroma1 profile should use 'sent Useful only if I use my main reading address also to send emails.
  (setq mu4e-sent-messages-behavior 'delete)

  (setq message-kill-buffer-on-exit t)
  (setq mu4e-confirm-quit nil)

  :config
  (define-key mu4e-compose-mode-map [f2] 'ispell-message)
  (setq mail-user-agent 'mu4e-user-agent)

  ;; ---------------------------
  ;; Setup the list of maildirs
  ;; ---------------------------
  (setq mu4e-bookmarks (list
                        (make-mu4e-bookmark
                         :name "———————————————————————————"
                         :query "date:2099"
                         :key ?{)
                        (make-mu4e-bookmark
                         :name " Posta in arrivo"
                         :query "maildir:/inbox"
                         :key ?i)
                        (make-mu4e-bookmark
                         :name "Inviati (ultimo mese)"
                         :query "maildir:/sent AND date:1m..now"
                         :key ?s)
                        (make-mu4e-bookmark
                         :name "Tutti i messaggi (ultimo mese)"
                         :query "maildir:/archive AND date:1m..now"
                         :key ?a)
                        (make-mu4e-bookmark
                         :name "Speciali"
                         :query "maildir:/special"
                         :key ?t)
                        (make-mu4e-bookmark
                         :name "Bozze"
                         :query "maildir:/drafts"
                         :key ?d)
                        (make-mu4e-bookmark
                         :name " Con allegato"
                         :query "flag:attach"
                         :key ?A)
                        (make-mu4e-bookmark
                         :name "———————————————————————————"
                         :query "date:2099"
                         :key ?-)))

  ;; Put in the bookmarks all the user folders with increasing
  ;; bookmark shortcut (won't work with more than 9 bookmarks)
  (cl-loop for label in mu4e-other-folders
        with i = 1
        do
        (add-to-list 'mu4e-bookmarks
                        (make-mu4e-bookmark
                         :name  (icon-on-linux " " label)
                         :query (concat "maildir:/" label)
                         :key (string-to-char (number-to-string i)))
                        'append)
        (setq i (+ i 1)))

  ;;
  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "———————————————————————————"
                :query "date:2099"
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


  ;; This hook correctly modifies the \Inbox and \Starred flags on email when they are marked.
  ;; Without it refiling (archiving) and flagging (starring) email won't properly result in
  ;; the corresponding gmail action.
  (add-hook 'mu4e-mark-execute-pre-hook
	    (lambda (mark msg)
	      (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
		    ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
		    ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

  ;; -----------------
  ;; Setup identities
  ;; -----------------
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "uniroma1"
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 '(:to :from :cc :bcc)
                                                                 email-address-academic)))
             :vars `( ( user-mail-address      . ,email-address-academic  )
                      ( smtpmail-smtp-user     . ,email-address-academic  )
                      ( mu4e-sent-messages-behavior . sent) ;; to have them in my personal 'sent' mailbox
                      ( mu4e-compose-signature . ,signature-academic)))

           ,(make-mu4e-context
             :name "gmail"
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg
                                                                 '(:to :from :cc :bcc)
                                                                 email-address-personal)))
             :vars `( ( user-mail-address      . ,email-address-personal  )
                      ( smtpmail-smtp-user     . ,email-address-personal  )
                      ( mu4e-sent-messages-behavior . delete)
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




;; Helm client for org-contacts allows to tag addresses, phone numbers
;; and email address with indicators like (WORK) (HOME) (CELL) (FAX)
;; that qualify the entries. These tags are represented with icons
;; and help choosing the right variant of an entry.

;; See https://github.com/tmalsburg/helm-org-contacts
(use-package helm-org-contacts
  :after helm
  :bind ([f7] . helm-org-contacts)
  :config
  ;; Reorder actions
  (helm-delete-action-from-source  "Insert address" helm-source-org-contacts)
  (helm-delete-action-from-source  "Insert plain email address" helm-source-org-contacts)
  (helm-delete-action-from-source  "Insert email address with name" helm-source-org-contacts)
  (helm-delete-action-from-source  "Insert phone number" helm-source-org-contacts)
  (helm-delete-action-from-source  "Show entry" helm-source-org-contacts)
  ;; Add actions in the right order (first one on top)
  (helm-add-action-to-source       "Insert email address (with name)" 'helm-org-contacts-insert-email-with-name helm-source-org-contacts)
  (helm-add-action-to-source       "Insert email address (no name)"   'helm-org-contacts-insert-plain-email     helm-source-org-contacts)
  (helm-add-action-to-source       "Insert phone"                     'helm-org-contacts-insert-phone-number    helm-source-org-contacts)
  (helm-add-action-to-source       "Insert posta address"             'helm-org-contacts-insert-address         helm-source-org-contacts)
  (helm-add-action-to-source       "Edit entry"                       'helm-org-contacts-edit-entry             helm-source-org-contacts))


(provide 'init-mail)
;; Local Variables:
;; mode: emacs-lisp
;; End:
