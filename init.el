;;; init.el --- Main configuration file -*- coding: utf-8 -*-

;; Copyright (C) 2010, 2011, 2012  Massimo Lauria
;; Time-stamp: "2012-03-12, 00:45 (CET) Massimo Lauria"

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

;;; Code:

(setq emacs-load-start-time (current-time)) ;; save the clock at start-time


;;; Setup load-path --------------------------------------------------------------------------------------------
(setq default-elisp-path "~/config/emacs")
(setq default-elisp-3rdparties "~/config/emacs/3rdparties")

(setq load-path (cons 	default-elisp-path load-path       ))
(setq load-path (cons 	default-elisp-3rdparties load-path ))
(setq load-path (cons 	"~/.emacs.d" load-path))


;;; Recognize and setup the running environment ----------------------------------------------------------------

(require 'init-discover-runtime)
(require 'init-functions)         ; Utility functions for configuration

(when-running-MacOSX
 (setq MacUser-site-lisp "~/Library/site-lisp")
 (setq load-path (cons 	MacUser-site-lisp load-path))
)


;;; Module(s) initialization -----------------------------------------------------------------------------------

;;{{{ *** Key binding rules ***

;; FIXME Some of them do not work in xterm
;; FIXME Many of them do not work in console
;;
;;
;;  One modifier for  intra-buffer operations (i.e. selection)
;;  Two modifiers for inter-buffer operations (i.e. navigation)
;;  Function keys for buffer processing (compile,check,...)
;;
;;  M-C-<arrow> for moving between windows
;;  M-S-<arrow> for moving between buffers/screens
;;  CUA-selection on (C-<SPC) mark, C-<RET> rect.,C-z C-x C-c C-v)
;;  F2   for local  spell check
;;  S-F2 for global spell check
;;  M-Space for folding
;;  Tab for indent/auto-complete
;;  M-Tab for correct w.r.t. spellcheck (on Flyspell)
;;

;; Moving in text
(global-set-key [C-left]  'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [C-up]    'backward-paragraph)
(global-set-key [C-down]  'forward-paragraph)

;; Moving in structes
(global-set-key [M-left] 'backward-sentence)
(global-set-key [M-right] 'forward-sentence)
(global-set-key [M-up] 'backward-sexp)
(global-set-key [M-down] 'forward-sexp)

;; Text movements keybindings
(require 'massimo-keyboard)

;; Managing windows [C-M]
; Moving
(global-set-key [M-C-right] 'windmove-right)
(global-set-key [M-C-left] 'windmove-left)
(global-set-key [M-C-up] 'windmove-up)
(global-set-key [M-C-down] 'windmove-down)
; Scrolling "other window"
(global-set-key [M-C-prior] 'scroll-other-window-down)
(global-set-key [M-C-next] 'scroll-other-window)
; Create and destroy windows
(global-set-key (kbd "M-C--") 'split-window-vertically)
(global-set-key (kbd "M-C-.") 'split-window-horizontally)
;; Make a window to be sticky.
(global-set-key [pause] 'toggle-current-window-sticky)



;; Broken on Xterm
(global-set-key (kbd "M-C-<backspace>") 'delete-window)
(global-set-key (kbd "<C-M-backspace>") 'delete-window)
(global-set-key (kbd "M-C-<return>") 'delete-other-windows)
;; Cheap Xterm substitutions
(global-set-key (kbd "ESC C-h") 'delete-window)
(global-set-key (kbd "ESC <C-return>") 'delete-other-windows)


;; Next/Prev item after Compiling
(global-set-key (kbd "<f9>") 'recompile)
(global-set-key (kbd "<f10>") 'gdb)
(global-set-key (kbd "<f11>") 'previous-error) ; Does not work with LaTeX!
(global-set-key (kbd "<f12>") 'next-error)
(global-set-key [M-prior] 'previous-error) ; Does not work with LaTeX!
(global-set-key [M-next] 'next-error)

;; (global-set-key (kbd "#") 'comment-region-maybe) `comment-dwim' is already bound to M-;

;; Spellcheck
(global-set-key (kbd "M-s") 'my-spell-correct-word)
(global-set-key [f2] 'ispell-buffer)


;; Tab is actually a "Smart tab"
;; (global-set-key [(tab)] 'smart-tab)




;; Agenda.
(global-set-key [f5] 'org-remember)  ;; Taking notes
(global-set-key [f6] 'org-agenda)    ;; View agenda/Todo
(global-set-key [f7] 'bbdb)          ;; Query Contacts
;; Switch language
(global-set-key [f8] 'spellcheck-language-cycle)

;;}}}


;; Work environment customization
(require 'init-coding)
(require 'init-italian-l10n)
(require 'init-local-preferences) ; Host based and personal configuration
(require 'init-preferences)       ; Basic editor preferences
(require 'init-backup)            ; Autosaves and backups behaviour


;; Editor behaviour customization
(require 'init-textmode)          ; Preferences for text editing
(require 'init-clipboard)         ; Clipboard managing
(require 'init-open-link)         ; Keys for opening/jumping links
(require 'init-autotype)          ; Automatic file filling
(require 'init-auto-complete)     ; Completion configuration
(require 'init-spellcheck)        ; Spellchecking


;; Programming Languages
(require 'init-python)

;; Math packages
(require 'init-latex)        ;; AucTeX
(require 'init-sage)         ;; SageMath
(require 'init-imaxima)      ;; Imaxima and Imath (from SageMath)
(require 'init-singular)     ;; Singular (not from SageMath!)

;; Applications
(require 'init-mail)    ;; Mail + Contacts
(when prefs-activate-org-mode   (require 'init-org-mode))   ;; Organizer


;; External packages
(when (require-maybe 'package) (package-initialize))

;; Other stuff
(require 'init-unsorted-elisp)

;;; Customization variables (in a separate file)----------------------------------------------------------------
(setq custom-file "~/config/emacs/custom.el")
(load custom-file 'noerror)

;; Aquamacs requires explicit server-start call.
(if (and (boundp 'aquamacs-version)
         (not (server-running-p)))
    (server-start))


(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds." (time-to-seconds (time-since emacs-load-start-time)))) ;; compute load-time
(provide 'init)
;; Local Variables:
;; mode: emacs-lisp
;; End:
