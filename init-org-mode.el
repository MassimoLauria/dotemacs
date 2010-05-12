;;;
;;; Utilities for robust and safe emacs usage.
;;;
(provide 'init-org-mode)
;;;------------------------------------------------------------------

(setq org-directory "~/personal/organizer/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-default-journal-file (concat org-directory "journal.org"))
(setq org-agenda-files (list org-directory))

;;;-------------------------------------------------------------------

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
(define-key global-map "\C-cj" 'org-add-journal-entry)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq 
 org-log-done t
 ;;org-CUA-compatible t
 ;;org-support-shift-select t
 org-cycle-emulate-tab nil
 org-cycle-global-at-bob t
 org-highlight-latex-fragments-and-specials t
)


;; Org-mode Keys
(setq org-replace-disputed-keys t)
(setq org-disputed-keys '(
                         ([(tab)]      . [(meta tab)]) 
                         ([(shift up)]      . [(control è)]) 
                         ([(shift down)]    . [(control à)]) 
                         ([(shift left)]    . [(control ò) ])
                         ([(shift right)]   . [(control ù) ]) 
                         ([(meta  up)]      . [(meta è) ]) 
                         ([(meta  down)]    . [(meta à) ]) 
                         ([(meta  left)]    . [(meta ò) ])
                         ([(meta  right)]   . [(meta ù) ])
                         ([(meta  shift up)]      . [(control meta è) ]) 
                         ([(meta  shift down)]    . [(control meta à) ]) 
                         ([(meta  shift left)]    . [(control meta ò) ])
                         ([(meta  shift right)]   . [(control meta ù) ])
                         ([(control shift right)] . [(meta shift +)]) 
                         ([(control shift left)] . [(meta shift -)])
                         ))


(defun org-mode-setup-local-keys()
  "Define/>Undefine of org-mode keys"
  (interactive)
  (local-unset-key "\t")                     ; Use Tab for more decent things (auto-complete or yasnippet)
  (local-unset-key [tab])
  (local-unset-key [(tab)])

  (local-set-key (kbd "M-<tab>")    'org-cycle)  ; Use M-Tab for
                                                 ; org-cycle, which
                                                 ; avoid to use the
                                                 ; overloaded TAB key
  
  (local-set-key (kbd "M-<space> ") 'org-cycle)  ; Use M-Space for
                                                 ; org-cycle, which is
                                                 ; similar to what I
                                                 ; use for
                                                 ; folding-toggle-show-hide

  (local-set-key (kbd "C-è") 'org-shiftup    ) 
  (local-set-key (kbd "C-à") 'org-shiftdown  ) 
  (local-set-key (kbd "C-ò") 'org-shiftleft  ) 
  (local-set-key (kbd "C-ù") 'org-shiftright ) 

  (local-set-key (kbd "M-è") 'org-metaup    ) 
  (local-set-key (kbd "M-à") 'org-metadown  ) 
  (local-set-key (kbd "M-ò") 'org-metaleft  ) 
  (local-set-key (kbd "M-ù") 'org-metaright ) 

  (local-set-key (kbd "C-M-è") 'org-shiftmetaup    ) 
  (local-set-key (kbd "C-M-à") 'org-shiftmetadown  ) 
  (local-set-key (kbd "C-M-ò") 'org-shiftmetaleft  ) 
  (local-set-key (kbd "C-M-ù") 'org-shiftmetaright ) 

  )

(add-hook 'org-mode-hook 'org-mode-setup-local-keys)


(org-remember-insinuate)
(setq org-remember-templates
      '(("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org" "X")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "notes.org" "New Ideas")))


(defun org-add-journal-entry ()
  "Start a new journal entry."
  (interactive)
  (find-file org-default-journal-file)
  (goto-char (point-min))
  (org-insert-heading)
  (org-insert-time-stamp (current-time) t)
  (open-line 2)
  (insert " "))


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 

