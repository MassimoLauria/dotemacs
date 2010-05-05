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
                         ([(shift up)]      . [(meta è)]) 
                         ([(shift down)]    . [(meta à)]) 
                         ([(shift left)]    . [(meta ò) ])
                         ([(shift right)]   . [(meta ù) ]) 
                         ([(meta  up)]      . [(meta i) ]) 
                         ([(meta  down)]    . [(meta k) ]) 
                         ([(meta  left)]    . [(meta j) ])
                         ([(meta  right)]   . [(meta l) ])
                         ([(meta  shift up)]      . [(control meta i) ]) 
                         ([(meta  shift down)]    . [(control meta k) ]) 
                         ([(meta  shift left)]    . [(control meta j) ])
                         ([(meta  shift right)]   . [(control meta l) ])
                         ([(control shift right)] . [(meta shift +)]) 
                         ([(control shift left)] . [(meta shift -)])
                         ))


(defun org-mode-setup-local-keys()
  "Define/>Undefine of org-mode keys"
  (interactive)
  (local-unset-key "\t")                     ; Use Tab for more decent things (auto-complete or yasnippet)
  (local-unset-key [tab])
  (local-unset-key [(tab)])

  (local-set-key   "\M-\ " 'org-cycle)       ; Use M-SPC for org-cycle, which is similar to what I use for folding-toggle-show-hide

  ;;(local-set-key   "M-è" 'org-shiftup)   -- BROKEN  
  ;;(local-set-key   "M-à" 'org-shiftdown) -- BROKEN
  ;;(local-set-key   "M-ò" 'org-shiftleft) -- BROKEN 
  ;;(local-set-key   "M-ù" 'org-shiftright)-- BROKEN

  (local-set-key (quote [134217960]) (quote org-shiftup))    ; M-è
  (local-set-key (quote [134217952]) (quote org-shiftdown))  ; M-à
  (local-set-key (quote [134217970]) (quote org-shiftleft))  ; M-ò
  (local-set-key (quote [134217977]) (quote org-shiftright)) ; M-ù
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

