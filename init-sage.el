;;;
;;;  Init file for SAGE math environment in Emacs
;;;
(provide 'init-sage)
;;;------------------------------------------------------------------

(setq sage-elisp-path "/usr/local/lib/sage-4.1.1/data/emacs")


;;;-------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name sage-elisp-path))
(require 'sage "sage")
(setq sage-command "/usr/local/lib/sage-4.1.1/sage")

;; If you want sage-view to typeset all your output and have plot()
;; commands inline, uncomment the following line and configure sage-view:
(require 'sage-view "sage-view")
(add-hook 'sage-startup-hook 'sage-view)
;; You can use commands like
;; (add-hook 'sage-startup-hook 'sage-view
;; 'sage-view-disable-inline-output 'sage-view-disable-inline-plots)
;; to have some combination of features.  In future, the customize interface
;; will make this simpler... hint, hint!
;;}}}


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 