;;; init.el --- Main configuration file -*- coding: utf-8 -*-

;; Copyright (C) 2010, 2011, 2012  Massimo Lauria
;; Time-stamp: "2012-11-03, 02:33 (CET) Massimo Lauria"

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
(setq default-elisp-path        "~/config/emacs")             ;; configuration files
(setq default-elisp-3rdparties  "~/config/emacs/3rdparties")  ;; 3rd parties conf. packages
(setq load-path (cons 	default-elisp-path        load-path))
(setq load-path (cons 	default-elisp-3rdparties  load-path))

;; Set paths, since sometime Mac OSX has weird paths and Emacs.app
;; doesn't pick them up.
(add-to-list 'exec-path "/usr/local/bin/") ; local
(add-to-list 'exec-path "/opt/local/bin/") ; macports
(add-to-list 'exec-path "~/.local/bin")    ; home



;; Sometimes the system does not contains important packages as Org or
;; BBDB, which cannot be installed using ELPA (either because Emacs <
;; 24 or because BBDB is not on ELPA).
;;
;; If there is an user installation of those packages, those will be
;; found first. I guess ELPA will overload them. Anyway everything
;; lives in the user local installation which can be tuned for the
;; specific system.
;;
(setq additional-elisp-paths (list
                              "~/Library/site-lisp/" ;; macosx user site-lisp
                              "~/.emacs.d/site-lisp/"     ;; standard user path
                              ))

(setq additional-elisp-packages (list
                              ""
                              "bbdb/lisp"        ;; main bbdb
                              "bbdb/bits"        ;; bbdb contributed utilities
                              "org/lisp"         ;; main org-mode
                              "org/contrib/lisp" ;; org-mode contributed utilities
                              "color-theme"      ;; for older emacs
                              ))

(dolist (path additional-elisp-paths)
  (dolist (package additional-elisp-packages)
    (let ((pp (concat path package)))
      (if (file-directory-p pp)
              (setq load-path (cons pp load-path))))))


(require 'init-discover-runtime) ; Discover emacs version and runtime
(require 'init-functions)        ; Utility functions for configuration


;;; Module(s) initialization -------------------------------------------

;; Work environment customization
(require 'init-coding)
(require 'init-italian-l10n)
(require 'init-local-preferences) ; Host based and personal configuration
(require 'init-preferences)       ; Basic editor preferences
(require 'init-backup)            ; Autosaves and backups behaviour

;; Load packages which can be setup later
(if (fboundp 'package-initialize)
    (package-initialize))

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
;;; Enabled commands
(put 'narrow-to-region 'disabled nil)


;;; Start server in Mac OSX --------------------------------------------
(require 'server nil t)
(when (fboundp 'server-running-p) ; not defined in emacs 22
  (if (and running-MacOSX (not (server-running-p))) (server-start)))




(provide 'init)
;; Local Variables:
;; mode: emacs-lisp
;; End:
