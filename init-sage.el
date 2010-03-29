;;;
;;;  Init file for SAGE math environment in Emacs
;;;
(provide 'init-sage)
;;;------------------------------------------------------------------

(setq sage-version "4.3.3")
(setq sage-basedir "/usr/local/lib/sage-4.3.3/")
(setq sage-elisp-path (concat sage-basedir "data/emacs"))

;;;-------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name sage-elisp-path))
(require-maybe 'sage "sage")
(setq sage-command (concat sage-basedir "sage"))

;; If you want sage-view to typeset all your output and have plot()
;; commands inline, uncomment the following line and configure sage-view:
(require-maybe 'sage-view "sage-view")
(add-hook 'sage-startup-after-prompt-hook 'sage-view)
;; You can use commands like 
;; (add-hook 'sage-startup-after-prompt-hook 'sage-view
;; 'sage-view-disable-inline-output 'sage-view-disable-inline-plots)
;; to have xsome combination of features.  In future, the customize interface
;; will make this simpler... hint, hint!


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 