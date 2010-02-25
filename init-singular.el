;;;
;;; Singular math package for Emacs.
;;;
(provide 'init-singular)
;;;------------------------------------------------------------------



(setq load-path (append load-path 
                        (list "/usr/share/Singular/emacs")))

(require 'singular)


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 

