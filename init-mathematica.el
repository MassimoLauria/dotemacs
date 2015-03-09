;;;
;;; Mathematica support in Emacs.
;;;
;;;------------------------------------------------------------------


(use-package wolfram-mode
  :commands (run-wolfram wolfram-mode)
  :init (setq wolfram-program "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
  :mode ("\\.m$" . wolfram-mode))


(provide 'init-mathematica)
;; Local Variables:
;; mode: emacs-lisp
;; End:
