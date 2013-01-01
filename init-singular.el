;;;
;;; Singular math package for Emacs.
;;;
;;;------------------------------------------------------------------

(defvar singular-emacs-path nil
"Path where to find singular Emacs support files")

(setq singular-emacs-path "/usr/share/Singular/emacs")

(when (file-directory-p singular-emacs-path)
  (add-to-list 'load-path singular-emacs-path t)
  (require 'singular)
)

(provide 'init-singular)
;; Local Variables:
;; mode: emacs-lisp
;; End:
