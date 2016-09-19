;;; init.el --- emacs bootstrap   -*- coding: utf-8 -*-
;;
;; 
;;  Full Emacs configuration requires version >= 24, but in case of
;;  older emacs, we revert to a bare minimal configuration that still
;;  does not make me cringe.
;;

;; -------------------------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(setq load-path (cons "~/config/emacs/" load-path))

(if (>= emacs-major-version 24)
    (require 'init-start)
  (require 'init-minimal))
