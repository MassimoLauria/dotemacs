;;;
;;; Matlab support in Emacs.
;;;
;;;------------------------------------------------------------------


(use-package matlab
  :init
  (setq matlab-shell-command
        (or
         (executable-find "matlab")
         (executable-find "/usr/local/bin/matlab")
         (executable-find "/Applications/Matlab.app/bin/matlab")))
  (setq matlab-indent-function-body t)
  :commands (matlab-mode matlab-shell))

(defalias 'run-matlab 'matlab-shell)

(provide 'init-matlab)
;; Local Variables:
;; mode: emacs-lisp
;; End:
