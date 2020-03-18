;;; -*- coding: utf-8 -*-
;;;
;;; Basic Editor customization
;;;
;;;-----------------------------------------------------------------

;;; Themes


;; Set theme to zenburn
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Fix zenburn theme for flyspell/flymake/flycheck
(custom-theme-set-faces
 'zenburn
 '(flycheck-error-face ((t (:underline "DodgerBlue1"))))
 '(flycheck-warning-face ((t (:underline "green"))))
 '(flymake-errline ((t (:underline "DodgerBlue1"))))
 '(flymake-warnline ((t (:underline "green"))))
 '(flyspell-duplicate ((t (:strike-through "red"))))
 '(flyspell-incorrect ((t (:underline "red"))))
 )


;; Meta usage in MacOSX requires some thought
(if (boundp 'ns-right-alternate-modifier)
    (progn
      (setq ns-alternate-modifier 'meta)
      (setq ns-right-alternate-modifier 'nil))
  (setq ns-alternate-modifier 'nil))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(blink-cursor-mode nil)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq mouse-highlight nil)
(setq use-file-dialog nil)
(setq use-dialog-box  nil)

(setq search-highlight t)
(setq query-replace-highlight t)

(line-number-mode 1)
(column-number-mode 1)

(setq x-stretch-cursor t)
; (show-paren-mode 1)                     ; We use sp-show-paren-mode from smartparen

;; Speedbar
(setq speedbar-mode-hook '(lambda () (text-scale-decrease 1)))


;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))

(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(setq confirm-kill-emacs nil)     ; exit without questions


(setq echo-keystrokes 0.1)


;;  Tab expansion
(setq-default indent-tabs-mode nil) ;; Expand tabs as spaces
(setq default-tab-width 4)


;; VC warning
(setq vc-follow-symlinks nil)

;; Go to the first error
(setq compilation-auto-jump-to-first-error nil)

;; Scroll preferences
(setq scroll-preserve-screen-position 1)
(setq scroll-margin 0)
(setq scroll-conservatively 1000)

;; Recenter sequence
(setq recenter-positions '(top middle bottom))


;; Cursor position helpers
(global-hl-line-mode nil)
  
(use-package beacon ; flashes the cursor's line when you scroll
  :diminish ""
  :config
  (beacon-mode 1))
  
(use-package which-key
  :diminish ""      
  :config
  (which-key-mode))


;; Canonical behaviour of modern interfaces. Not the default in Emacs22
;; Cut (C-x)  Copy(C-c) Paste(C-v) Undo(C-z)
;; S-<arrow> select, C-<Ret> rectangular mark, C-<SPC> mark
(transient-mark-mode t)
(delete-selection-mode t)
(cua-mode t)


(setq cua-keep-region-after-copy t)


;;(setq warning-minimum-level :error)
(setq frame-title-format '( " " "%[%b%]" " [%*] %p" "  <" invocation-name "@" (:eval (system-name)) ">%@"))

;; Tramp connection caching causes pain.  When characteristics on the
;; system change, it may cause double password request.
(setq tramp-persistency-file-name nil)

(provide 'init-preferences)
;; Local Variables:
;; mode: emacs-lisp
;; End:
