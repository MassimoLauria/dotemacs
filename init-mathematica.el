;;;
;;; Mathematica support in Emacs.
;;;
;;;------------------------------------------------------------------


(setq wolfram-program
      (or
       (executable-find "math")
       (executable-find "/usr/local/bin/math")
       (executable-find "/Applications/Mathematica.app/Contents/MacOS/MathKernel")
       (executable-find "/pkg/mathematica/10.0.2/os/bin/MathKernel")))


(use-package wolfram-mode
  :commands (run-wolfram wolfram-mode)
  :mode ("\\.m$" . wolfram-mode))

(defalias 'run-mathematica 'run-wolfram)

(provide 'init-mathematica)
;; Local Variables:
;; mode: emacs-lisp
;; End:
