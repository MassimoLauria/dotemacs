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

(setq org-log-done t)
(setq org-CUA-compatible t)
(setq org-support-shift-select t)

(org-remember-insinuate)

(setq org-remember-templates
      '(("Journal" ?j "* %U %?\n\n  %i\n  %a" "journal.org" "X")
        ("Idea" ?i "* %^{Title}\n  %i\n  %a" "notes.org" "New Ideas")))


(add-hook 'org-mode-hook
          	  (lambda ()
          	    (org-set-local 'yas/trigger-key [tab])
          	    (define-key yas/keymap [tab] 'yas/next-field-group)))

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

