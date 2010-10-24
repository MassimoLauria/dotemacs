;;;
;;; Mail: Wanderlust + BBDB + vCard
;;;
;;;------------------------------------------------------------------



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
    (goto-address-mode)
    (wl-draft-highlight-and-recenter)
    )
 )


(setq wl-draft-elide-ellipsis "[...]" )


(defun mail()
"This 'mail' command calls 'wl-draft' with the appropriate
headers by default. If a region is selected, then the text in the
region is included in the mail body.
"
  (interactive)
  (wl-draft '(
              (To . "") 
              (Cc . "")
              (Bcc . "")
              (Subject . "")
              )
            nil
            nil
            (when (use-region-p) 
              (buffer-substring (region-beginning) 
                                (region-end))
              )
            )
  (mail-position-on-field "To")
)

(define-key global-map "\C-cm" 'mail)


;; (Insidious) Big Brother DataBase, collects mail addresses.
;; MacOSX alternative path
(when running-MacOSX
  (setq MacUser-bbdb-path (concat MacUser-site-lisp "/bbdb-2.3.5"))
  (if (file-directory-p MacUser-bbdb-path)
      (add-to-list 'load-path MacUser-bbdb-path))
)
;; Load bbdb
(and 
 (require-maybe 'bbdb)
 (bbdb-initialize)
)

;; message-tab uses tab to call BBDB in header, but not elsewhere.
;(define-key mail-mode-map (kbd "<tab>") 'message-tab)
;(define-key wl-draft-mode-map (kbd "<tab>") 'message-tab)


;; Say NO! to auto collection
(setq bbdb/mail-auto-create-p nil)
(setq bbdb-north-american-phone-numbers-p nil)

;; automatically add mailing list fields
(add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
(setq bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0))))


(setq
    
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



;; This is in the case we edit mail for Mutt client
(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))                                                                   
(setq mail-header-separator "")                                                                                               
(add-hook 'message-mode-hook
          'turn-on-auto-fill
          (function
           (lambda ()
             (progn
               (local-unset-key "\C-c\C-c")
               (define-key message-mode-map "\C-c\C-c" '(lambda ()
                                                          "save and exit quickly"
                                                          (interactive)
                                                          (save-buffer)
                                                          (server-edit)))))))

;; Click urls/mails in Mime-View
(add-hook 'mime-view-mode-hook 'goto-address-mode)


(provide 'init-mail-wl)
;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
