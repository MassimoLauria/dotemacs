;;;
;;; Utilities for robust and safe emacs usage.
;;;
(provide 'init-org-mode)
;;;------------------------------------------------------------------

(setq org-directory "~/personal/organizer/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-agenda-files (list "~/personal/organizer/"))


;;;-------------------------------------------------------------------

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-log-done t)
(setq org-CUA-compatible t)

(org-remember-insinuate)

(add-hook 'org-mode-hook
          	  (lambda ()
          	    (org-set-local 'yas/trigger-key [tab])
          	    (define-key yas/keymap [tab] 'yas/next-field-group)))


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 

