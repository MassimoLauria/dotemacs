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
                          ([(control shift left)] . [(meta shift _)])
                          ))


(defun org-mode-setup-local-keys ()
  "Define/>Undefine of org-mode keys"
  (local-unset-key "\t")                     ; Use Tab for more decent things (auto-complete or yasnippet)
  (local-set-key   "\M-\ " 'org-cycle)       ; Use M-SPC for org-cycle, which is similar to what I use for folding-toggle-show-hide
  (local-set-key   "\M-+" 'org-priority-up)  
  (local-set-key   "\M--" 'org-priority-down)
  ;;(local-set-key   "\M-è" 'org-priority-up)  
  ;;(local-set-key   "\M-à" 'org-priority-down)
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

