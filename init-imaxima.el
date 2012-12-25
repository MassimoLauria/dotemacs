;;;
;;; Imath and Imaxima configuration.
;;;
;;;------------------------------------------------------------------

(defvar imaxima-base-path nil
"Path of Maxima (use the SageMath one by default)")


(defvar imaxima-elisp-path nil
"Path of the Emacs packages to be used with Maxima (use the SageMath one by default)")

(defvar imaxima-maxima-version nil
"Version number of installed Maxima (use SageMath one by default)")



;; Find Maxima inside SageMath folder
(when sagemath-root-directory
  (setq imaxima-base-path (concat sagemath-root-directory "/local/share/maxima/"))
  (let ((tmp nil))
    (setq tmp (split-string (shell-command-to-string (concat "ls " imaxima-base-path)) "\n" t))
    (setq imaxima-maxima-version (first (last tmp))))
  (setq imaxima-elisp-path (concat imaxima-base-path imaxima-maxima-version "/emacs/"))
  (setq maxima-command     (concat sagemath-root-directory "/local/bin/maxima")))
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
;; End:
