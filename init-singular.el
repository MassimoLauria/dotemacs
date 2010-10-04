;;;
;;; Singular math package for Emacs.
;;;
;;;------------------------------------------------------------------



(setq load-path (append load-path 
                        (list "/usr/share/Singular/emacs")))

(require 'singular)


(provide 'init-singular)
;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 

