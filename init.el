;;; init.el --- emacs bootstrap   -*- coding: utf-8 -*-
;;
;; 
;;  Full Emacs configuration requires version >= 24, but in case of
;;  older emacs, a bare minimal configuration is used. 
;;

;; -------------------------------------------------------------------
(setq load-path (cons "~/config/emacs/" load-path))

(if (>= emacs-major-version 24)
    (require 'init-start)
  (require 'init-minimal))
