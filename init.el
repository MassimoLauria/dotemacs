;;; init.el --- Main configuration file

;; Copyright (C) 2010  Massimo Lauria
;; Time-stamp: "2010-09-27, lunedì 23:52:26 (CEST) Massimo Lauria"

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

;;  Save start time.
(setq emacs-load-start-time (current-time))


;;; Setup load-path and runtime  -------------------------------------------------------------------------------
(setq default-elisp-path "~/config/emacs")
(setq default-elisp-3rdparties "~/config/emacs/3rdparties")

(setq load-path (cons 	default-elisp-path load-path ))
(setq load-path (cons 	default-elisp-3rdparties load-path ))

;; First thing first, discover the running environment before anything
;; else.  In this way even the basic setup can be system dependent.
(require 'init-discover-runtime)

 
                       
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

;;; Possible keybinding for moving in text. Notice that 
;;; such kybindings are made for not leaving the homerow. 
;;; For me is quite premature to apply such bindings.

;; Character based.
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-l")  'forward-char)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k")  'next-line)

(global-set-key (kbd "M-u")  'beginning-of-line)
(global-set-key (kbd "M-o")  'end-of-line)

;; Logical unit based.
(global-set-key (kbd "C-M-j")  'backward-word)
(global-set-key (kbd "C-M-l")  'forward-word)
(global-set-key (kbd "C-M-i")  'backward-paragraph) 
(global-set-key (kbd "C-M-k")  'forward-paragraph)
; 
(global-set-key (kbd "C-M-u")  'backward-sentence) 
(global-set-key (kbd "C-M-o")  'forward-sentence)


;; Deletion keys
(global-set-key (kbd "M-w")  'backward-kill-word) 
(global-set-key (kbd "M-d")  'backward-delete-char)
(global-set-key (kbd "M-f")  'delete-char) 
; 
(global-set-key (kbd "C-M-d")  'backward-kill-word)
(global-set-key (kbd "C-M-f")  'kill-word)




;; Moving between buffers (M-S)
(global-set-key [M-S-up] 'previous-user-buffer) 
(global-set-key [M-S-down] 'next-user-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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


;; Spellcheck
(global-set-key (kbd "M-s") 'my-spell-correct-word)
(global-set-key [f2] 'ispell-buffer) 

;; Folding on/off (M-Space) 
(global-set-key (kbd "M-<SPC>") 'folding-toggle-show-hide) 

;; Tab is actually a "Smart tab"
;; (global-set-key [(tab)] 'smart-tab)

;; Register at finger tips from 1 to ... 0!
(global-unset-key (kbd "M-r"))
(global-set-key (kbd "M-1") '(lambda () (interactive) (register-to-point 1) )  )
(global-set-key (kbd "M-2") '(lambda () (interactive) (register-to-point 2) )  )
(global-set-key (kbd "M-3") '(lambda () (interactive) (register-to-point 3) )  )
(global-set-key (kbd "M-4") '(lambda () (interactive) (register-to-point 4) )  )
(global-set-key (kbd "M-5") '(lambda () (interactive) (register-to-point 5) )  )
(global-set-key (kbd "M-6") '(lambda () (interactive) (register-to-point 6) )  )
(global-set-key (kbd "M-7") '(lambda () (interactive) (register-to-point 7) )  )
(global-set-key (kbd "M-9") '(lambda () (interactive) (register-to-point 9) )  )
(global-set-key (kbd "M-r 0") '(lambda () (interactive) (point-to-register 0) )  )
(global-set-key (kbd "M-r 1") '(lambda () (interactive) (point-to-register 1) )  )
(global-set-key (kbd "M-r 2") '(lambda () (interactive) (point-to-register 2) )  )
(global-set-key (kbd "M-r 3") '(lambda () (interactive) (point-to-register 3) )  )
(global-set-key (kbd "M-r 4") '(lambda () (interactive) (point-to-register 4) )  )
(global-set-key (kbd "M-r 5") '(lambda () (interactive) (point-to-register 5) )  )
(global-set-key (kbd "M-r 6") '(lambda () (interactive) (point-to-register 6) )  )
(global-set-key (kbd "M-r 7") '(lambda () (interactive) (point-to-register 7) )  )
(global-set-key (kbd "M-r 9") '(lambda () (interactive) (point-to-register 9) )  )
(global-set-key (kbd "M-r 0") '(lambda () (interactive) (point-to-register 0) )  )



;; Agenda.
(global-set-key [f5] 'org-remember)  ;; Taking notes
(global-set-key [f6] 'org-agenda)    ;; View agenda/Todo
(global-set-key [f7] 'bbdb)          ;; Query Contacts
;; Switch language
(global-set-key [f8] 'toggle-it-en)


;;}}}

;; Folding - load early to avoid warnings.
(load "folding" 'nomessage)
(folding-mode-add-find-file-hook)



; Editor customization 
(require 'init-functions)         ; Utility functions for configuration
(require 'init-local-preferences) ; Host based and personal configuration
(require 'init-preferences)       ; Basic editor preferences
(require 'init-elscreen)          ; ElScreen preferences
(require 'init-backup)            ; Autosaves and backups behaviour
;; (require 'init-unstable)       ; Features that are not yet stable

; Editor Utilities.
(require 'init-autotype)          ; Automatic file filling
(require 'init-auto-complete)     ; Completion configuration
(require 'init-spellcheck)        ; Spellchecking

; Programming Languages
(require 'init-python)

; Math packages
(when prefs-activate-latex    (require 'init-latex))        ;; AucTeX
(when prefs-activate-maxima   (require 'init-imaxima))      ;; Imaxima and Imath
(when prefs-activate-sage     (require 'init-sage))         ;; Sagemath 
(when prefs-activate-singular (require 'init-singular))     ;; Singular 

; Applications
(when prefs-activate-mail       (require 'init-mail-wl))    ;; Wanderlust MUA + bbdb
(when prefs-activate-org-mode   (require 'init-org-mode))   ;; The famous ORG-Mode! Yaiii!!

(require 'init-editserver-chrome) ;; Edit text area on Google Chrome

(autoload 'twit-post "twit" "Frontend for twitter" t)   ;; Twitter Support
(when prefs-activate-twitter    (require 'twit))            ;; Explicit


;;; Things below here are still a little mess---------------------------------------------------------------------

;; Auto-mode for renamed config files
(setq auto-mode-alist (cons '("bashrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("zshrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zsh" . sh-mode) auto-mode-alist))



;;{{{ *** Primary-Clipboard selection Panic! ***
;; Cut (C-x)  Copy(C-c) Paste(C-v) Undo(C-z)
;; S-<arrow> select, C-<Ret> rectangular mark, C-<SPC> mark
(transient-mark-mode t)
(delete-selection-mode t)
(cua-mode t)
(setq cua-keep-region-after-copy t)
(setq mouse-drag-copy-region nil)   ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)	; stops killing/yanking interacting with primary X11 selection 
(setq x-select-enable-clipboard t)	; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; (when (string= (window-system) "x") 
(setq select-active-regions t)                 ; active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary) ; middle-click only pastes from primary X11 selection.
(setq yank-pop-change-selection t)             ; makes rotating the kill ring change the X11 clipboard.	
;;)                                 
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)
;; XTerm support
(xterm-mouse-mode t)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)
;;}}}


;;{{{ *** Advanced editing customization ***

;; Undo-Tree, much better than default.
(require 'undo-tree)
(global-undo-tree-mode)



;; Text mode by default, with auto-fill
(require 'typopunct)
(setq-default typopunct-buffer-language 'english)
(setq default-major-mode 'text-mode)
;;(setq initial-major-mode 'text-mode) ;; Better to stick with Lisp-Interaction.
(setq text-mode-hook
      '(lambda nil
         (if prefs-activate-smallscreen
             (setq fill-column 70)
           (setq fill-column 80)
           )
         (auto-fill-mode 1)
         ;;(orgtbl-mode 1)
         (flyspell-mode 1)  ; annoying spell checking 
         ;;(typopunct-mode)
         )
      )


;; Prepare *scratch buffer*
;; FROM: Morten Welind
;;http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (if (boundp 'initial-major-mode)
      (eval (cons initial-major-mode ()))
      (lisp-interaction-mode)
    )
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))


;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
)

;; Ediff customization 
; (no external control frame) 
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
; (use vertical split if there is enough room)
(setq ediff-split-window-function 
      (lambda (&optional arg)
        (if (> (frame-width) 150)
            (split-window-horizontally arg)
          (split-window-vertically arg)
          )))



;; Save histories across sessions. Not buffers
(savehist-mode 1)

;; Use full featured gdb GUI.
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(add-hook 'gud-mode-hook '(lambda ()
                            (local-set-key (kbd "<f10>") 'gud-nexti)
                            (local-set-key (kbd "<f11>") 'gud-next )
                            (local-set-key (kbd "<f12>") 'gud-cont)
                            
                            (local-set-key (kbd "M-<f10>") 'gud-stepi)
                            (local-set-key (kbd "M-<f11>") 'gud-step )
                            (local-set-key (kbd "M-<f12>") 'gud-until)

                            (local-set-key (kbd "<f9>"  ) 'gud-break)
                            (local-set-key (kbd "M-<f9>"  ) 'gud-tbreak)
                            )
          )


;; fixme highlight
(require 'fixme)

;; IDO mode for selection of file and buffers. VERY GOOD
(require 'ido)

(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map (kbd "<tab>")   'ido-complete)
            (define-key ido-completion-map (kbd "M-<tab>") 'ido-next-match)
            (define-key ido-completion-map (kbd "M-j") 'ido-prev-match)
            (define-key ido-completion-map (kbd "M-l") 'ido-next-match)
            (define-key ido-completion-map (kbd "M-i") 'ido-prev-match)
            (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
            ))

(ido-mode t) 

(setq ido-enable-flex-matching t ; fuzzy matching is a must have
      ido-max-prospects 5        ; minibuffer is not saturated
      ido-ignore-buffers ;; ignore these guys
       '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido") 
      ido-everywhere t            ; use for many file dialogs
      ido-case-fold  t            ; be case-insensitive
      ido-auto-merge-werk-directories-length nil) ; all failed, no more digging

;; Moving between windows with (M-C-<arrow>)
(require 'windmove)               ; to load the package
(setq windmove-wrap-around t)

;; Overwrite flymake-display-warning so that no annoying dialog box is
;; used.
(defun flymake-display-warning (warning) 
  "Display a warning to the user, using lwarn"
  (message warning))



;; Settings for cc-mode
(add-hook 'cc-mode-hook 
          (lambda () 
            (setq c-block-comment-prefix "*")
            )
          )

;; Eldoc for lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; Eldoc for C
(add-hook 'c-mode 'c-turn-on-eldoc-mode)

;;}}}


;;{{{ *** ELPA Managing: Emacs Lisp Package Archive ***
(when (require-maybe 'package)
  (package-initialize))
;;}}}


;;{{{ *** Auto completion with SMART TAB (OBSOLETE) *** 
;;
;; If a region is selected, indent.
;; If at the end of a symbol, complete
;; 

(defvar smart-tab-using-hippie-expand t
  "turn this on if you want to use hippie-expand completion.")

(setq hippie-expand-try-functions-list '(
                                         ;;yas/hippie-try-expand
                                         try-expand-dabbrev 
                                         try-complete-file-name-partially 
                                         try-expand-dabbrev-all-buffers 
                                         try-expand-dabbrev-from-kill 
                                         try-complete-file-name
                                         ;;try-expand-all-abbrevs
                                         try-expand-list 
                                         ;;try-expand-line 
                                         try-complete-lisp-symbol-partially 
                                         try-complete-lisp-symbol)
      )
 

(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (if (smart-tab-must-expand prefix)
        (if smart-tab-using-hippie-expand
            (hippie-expand nil)
          (dabbrev-expand nil))
      (smart-indent))))


(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
   (looking-at "\\_>")))
;;}}}    


;;{{{ *** Color Schemes (ZenBurn or tty-dark) ***
;;
(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)   ;; High color theme (xterm-256color and X11)

;; Multi-TTY support
;(add-hook 'after-make-frame-functions
;          (lambda (frame)
;            (set-variable 'color-theme-is-global nil)
;            (select-frame frame)
;            (if (> (display-color-cells) 255)
;                (color-theme-zenburn) ;; High color theme (xterm-256color and X11)
;              (color-theme-tty-dark)) ;; Low color theme (xterm or linux console)
;))

;;}}}



;;; Customization variables (in a separate file)----------------------------------------------------------------
(setq custom-file "~/config/emacs/custom.el")
(load custom-file 'noerror)

;; Loading time 
(when (require 'time-date nil t)
  (message "Emacs startup time: %d seconds." (time-to-seconds (time-since emacs-load-start-time))))

(provide 'init)
;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
