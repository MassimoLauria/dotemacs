;;; init.el --- Main configuration file -*- coding: utf-8 -*-

;; Copyright (C) 2010, 2011, 2012  Massimo Lauria
;; Time-stamp: "2012-06-14, 23:09 (CEST) Massimo Lauria"

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

;; We define several function and environment variables useful for
;; discovering the running OS type (Linux, MacOSX, Windows...) and the
;; particular type of emacs running (GNU Emacs, XEmacs , Aquamacs,
;; ...)


;;; Setup Emacs environment --------------------------------------------
(setq default-elisp-path        "~/config/emacs")
(setq default-elisp-3rdparties  "~/config/emacs/3rdparties")
(setq default-elisp-macosx      "~/Library/site-lisp")


(setq load-path (cons 	default-elisp-path load-path       ))
(setq load-path (cons 	default-elisp-3rdparties load-path ))
(setq load-path (cons 	default-elisp-macosx     load-path))

(setq load-path (cons 	"~/.emacs.d" load-path))

(require 'init-discover-runtime) ; Discover emacs version and runtime
(require 'init-functions)        ; Utility functions for configuration


;;; Module(s) initialization -------------------------------------------

;; Work environment customization
(require 'init-coding)
(require 'init-italian-l10n)
(require 'init-local-preferences) ; Host based and personal configuration
(require 'init-preferences)       ; Basic editor preferences
(require 'init-backup)            ; Autosaves and backups behaviour

;; Editor behaviour customization
(require 'init-textmode)          ; Preferences for text editing
(require 'init-terminal-fix)      ; Fix some keys combinations in terminals
(require 'init-clipboard)         ; Clipboard managing
(require 'init-autotype)          ; Automatic file filling
(require 'init-auto-complete)     ; Completion configuration
(require 'init-spellcheck)        ; Spellchecking

;; Keyboard settings
(require 'massimo-keyboard)       ; basic keyboard settings
(require 'init-global-keys)       ; global keys
(require 'init-open-link)         ; keys for opening links

;; Programming Languages
(require 'init-cc-mode)
(require 'init-python)
;;(require 'init-java-mode)

;; Math packages
(require 'init-latex)        ;; AucTeX
(require 'init-sage)         ;; SageMath
(require 'init-imaxima)      ;; Imaxima and Imath (from SageMath)
(require 'init-singular)     ;; Singular (not from SageMath!)

;; Applications
(require 'init-mail)       ;; Mail + Contacts
(require 'init-org-mode)   ;; Organizer

;; Other stuff
(require 'init-unsorted-elisp)


;;; Customized settings -------------------------------------------------
(setq custom-file "~/config/emacs/custom.el")
(load custom-file 'noerror)


;;; Start server in Aquamacs --------------------------------------------
(if (and (boundp 'aquamacs-version) (not (server-running-p))) (server-start))


(provide 'init)
;; Local Variables:
;; mode: emacs-lisp
;; End:
