;;;
;;; Setup for editing google-chrome text areas with emacs
;;;
(provide 'init-editserver-chrome)
;;;-----------------------------------------------------------------


(if (and (fboundp 'daemon) (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start))
  )


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
