;;;
;;; Utilities for robust and safe emacs usage.
;;;
(provide 'init-org-mode)
;;;------------------------------------------------------------------

;;;-------------------------------------------------------------------


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-agenda-files (list "~/personal/organizer/remember.org"
							 "~/personal/organizer/research.org" 
                             "~/personal/organizer/diary.org"
                             "~/personal/organizer/scrapbook.org"))

(setq org-log-done t)
(setq org-CUA-compatible t)

(add-hook 'org-mode-hook
          	  (lambda ()
          	    (org-set-local 'yas/trigger-key [tab])
          	    (define-key yas/keymap [tab] 'yas/next-field-group)))


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 

