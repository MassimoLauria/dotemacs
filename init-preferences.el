;;; -*- coding: utf-8 -*-
;;;
;;; Basic Editor customization
;;;
;;;-----------------------------------------------------------------


;; Move to home directory if started as a server
(when (daemonp)
  (setq default-directory "~/")
  (setq command-line-default-directory "~/"))

;; Meta usage in MacOSX requires some thought
(if (boundp 'ns-right-alternate-modifier)
    (progn
      (setq ns-alternate-modifier 'meta)
      (setq ns-right-alternate-modifier 'nil))
  (setq ns-alternate-modifier 'nil))


(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Cursor
(setq x-stretch-cursor t)
(blink-cursor-mode nil)
(setq cua-normal-cursor-color 'bar
      cua-overwrite-cursor-color 'hollow
      cua-read-only-cursor-color 'hbar
      cua-enable-cursor-indications t
      cua-enable-modeline-indications t)


(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq mouse-highlight nil)
(setq use-file-dialog nil)
(setq use-dialog-box  nil)
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.2)
(help-at-pt-set-timer)

(setq search-highlight t)
(setq query-replace-highlight t)

(setq history-delete-duplicates t)

(setq global-auto-rever-non-file-buffers t)

(line-number-mode 1)
(column-number-mode 1)

;; Speedbar
(setq speedbar-mode-hook '(lambda () (text-scale-decrease 1)))


;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(setq confirm-kill-emacs nil)     ; exit without questions


(setq echo-keystrokes 0.1)


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

(use-package beacon ; flashes the cursor's line when you scroll
  :diminish ""
  :custom
  (beacon-size 70)
  (beacon-blink-duration 0.1)  ;; fade duration
  (beacon-blink-delay 0.2)     ;; time before fading
  (beacon-blink-when-point-moves-horizontally nil)
  (beacon-blink-when-point-moves-vertically 5)
  (beacon-blink-when-buffer-changes t)
  (beacon-blink-when-window-scrolls t)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-focused t)
  (beacon-dont-blink-commands '(next-line
                                previous-line
                                forward-line
                                next-error
                                previous-error))

  :config
  ;; Avoid beacon during isearch
  (add-hook 'beacon-dont-blink-predicates #'(lambda () (bound-and-true-p isearch-mode)))
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
(setq cua-enable-cua-keys t)
(setq cua-keep-region-after-copy t)
(cua-mode t)



;;(setq warning-minimum-level :error)

;; Careful!! The option "%p" makes the emacs to hang when folding
;; headers in org-mode
(setq frame-title-format '( " "
			    "%[%b%]"
			    " [%*]"
			    "  <" invocation-name "@" (:eval (system-name)) ">%@"))

;; Tramp connection caching causes pain.  When characteristics on the
;; system change, it may cause double password request.
(setq tramp-persistency-file-name nil)

(provide 'init-preferences)
;; Local Variables:
;; mode: emacs-lisp
;; End:
