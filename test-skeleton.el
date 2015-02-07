;;; test-skeleton.el --- Test skeleton file -*- coding: utf-8 -*-
;;
;; This configuration file could a starting point to test features and
;; packages. It is not supposed to be use as init file. For a small
;; init file see `init-minimal.el'
;;
;; - compatible with older Emacs
;; - self contained
;; 
;; Usage:
;;
;; $ emacs --script ~/config/emacs/test-skeleton.el
;; 


;; ----------------- Packages active ---------------------------------
(setq package-user-dir "~/emacs-temp/")
(setq package-archives  '(("gnu"   . "http://elpa.gnu.org/packages/")
                          ("elpa"  . "http://tromey.com/elpa/")
                          ("melpa" . "http://melpa.org/packages/")
			  ))
(when (require 'package nil t)
  (package-initialize))

;; ----------------- Test code starts here ---------------------------
