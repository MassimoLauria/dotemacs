;;; init-start.el --- Main configuration file -*- coding: utf-8 -*-

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015, 2016  Massimo Lauria
;; Time-stamp: "2016-06-09, 14:36 (CEST) Massimo Lauria"

;; Author: Massimo Lauria
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;;  This is the entry point of my main configuration. It requires
;;  Emacs 24.  In case Emacs is older, this file should not be loaded
;;  and the fallback `init-minimal.el' should.



;; Fallback configuration
(when (< emacs-major-version 24)
  (error "Emacs <24 not supported by default configuration. Use fallback one"))

;; Stop on errors.
(setq debug-on-error t)
(add-hook 'after-init-hook '(lambda () (setq debug-on-error nil)))


;;; Setup Emacs environment --------------------------------------------
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(setq default-elisp-path        "~/config/emacs")             ;; configuration files
(setq default-elisp-3rdparties  "~/config/emacs/3rdparties")  ;; 3rd parties conf. packages
(setq load-path (cons 	default-elisp-path        load-path))
(setq load-path (cons 	default-elisp-3rdparties  load-path))

(require 'pallet)
(pallet-mode t)



(add-to-list 'Info-directory-list (concat default-elisp-path "/info"))

;;; Module(s) initialization -------------------------------------------

;; Bootstrap
(require 'init-discover-runtime) ; Discover emacs version and runtime
(require 'init-environment)      ; setup running environment
(require 'init-functions)        ; Utility functions for configuration

;; Work environment customization
(require 'init-coding)
(require 'init-italian-l10n)
(require 'init-local-preferences) ; Host based and personal configuration
(require 'init-preferences)       ; Basic editor preferences -- check for speed
(require 'init-modeline)          ; modeline preferences -- check for speed
(require 'init-backup)            ; Autosaves and backups behaviour

;; Writing
(require 'init-textmode)          ; Preferences for text editing
(require 'init-spellcheck)        ; Spellchecking -- check for speed

;; Editor behaviour customization
(require 'init-terminal-fix)      ; Fix some keys combinations in terminals
(require 'init-eshell)            ; Emacs shell
(require 'init-clipboard)         ; Clipboard managing

;; Coding
(require 'init-autotype)          ; Automatic file filling
(require 'init-highlight)         ; Preferences for code highlighting
(require 'init-templates)         ; Templates
(require 'init-auto-complete)     ; Completion configuration -- check for speed

;; Keyboard settings
(require 'massimo-keyboard)       ; basic keyboard settings
(require 'init-global-keys)       ; global keys
(require 'init-hyperlink)         ; keys for opening links
(require 'init-windows)           ; windows layout

;; Programming
(require 'init-magit)
(require 'init-cc-mode)
(require 'init-python)
(require 'init-scheme)
(require 'init-java-mode)

;; Math packages
(require 'init-latex)        ; AucTeX
(require 'init-sage)         ; SageMath
(require 'init-imaxima)      ; Imaxima and Imath (from SageMath)
(require 'init-singular)     ; Singular (not from SageMath!)
(require 'init-mathematica)  ; Wolfram Mathematica
(require 'init-matlab)       ; Mathworks MATLAB

;; Applications
(require 'init-mail)       ; Mail + Contacts
(require 'init-org-mode)   ; Organizer
(require 'init-websearch)  ; Search the Web

;; Other stuff
(require 'init-unsorted-elisp)  ; various setups -- check for speed


;;; Customized settings -------------------------------------------------
(setq custom-file "~/config/emacs/custom.el")
(load custom-file 'noerror)
;;; Enabled/Disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'iconify-or-deiconify-frame 'disabled t)

;;; Start server --------------------------------------------------------
(require 'init-server)

;;; Start editing -------------------------------------------------------

(init-scratch-buffer)

(provide 'init-start)
;; Local Variables:
;; mode: emacs-lisp
;; End:
