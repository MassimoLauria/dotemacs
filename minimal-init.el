;;; init-minimal.el --- Miminal configuration file -*- coding: utf-8 -*-
;;
;; This configuration file could a starting point to test features and
;; packages.  Unfortunately it seems that my usual setup is so complex
;; and full of workarounds that when I do some serious upgrade or
;; install new packages, something breaks because of some lisp code
;; that was a good idea a some time but not anymore.
;;
;; Usage:
;;
;; $ emacs -q -l ~/config/emacs/minimal-init.el
;; 


;; -----------------------------------------------------------------------------------------
;; Basic
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq debug-on-error t)                 ; stop on errors
(setq load-path (cons   "~/config/emacs" load-path))

;; Emacs packages
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa-minimal/")
(setq package-archives  '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; Usability setup
(cua-mode)
(show-paren-mode)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(line-number-mode)
(column-number-mode)
(when (require 'massimo-keyboard nil t)	; some shortcuts are broken in this minimal setup.
  (massimo-keyboard-global-mode))
(condition-case msg
    (load-theme 'zenburn t)
    (error (format "%s" msg) ))

;; ADD TEST CODE DOWN HERE -----------------------------------------------------------------
