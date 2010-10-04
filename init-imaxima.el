;;;
;;; Imath and Imaxima configuration.
;;;
;;;------------------------------------------------------------------

(setq imaxima-elisp-path "/usr/local/share/maxima/5.19.2/emacs")

;;;-------------------------------------------------------------------


(require 'cl)
(pushnew  imaxima-elisp-path load-path)
(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Frontend for maxima" t)
;; Make the line effective if you want to use maxima mode with imaxima.
(setq imaxima-use-maxima-mode-flag t)


(provide 'init-imath)
(provide 'init-imaxima)
;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 