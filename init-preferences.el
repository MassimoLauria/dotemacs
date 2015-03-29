;;; -*- coding: utf-8 -*-
;;;
;;; Basic Editor customization
;;;
;;;-----------------------------------------------------------------

;;; Themes and Fonts

;; Color theme
(setq
 default-color-theme  'zenburn
 default-color-number 256)

;; Regular fonts
(setq
 font-X11   "DejaVu Sans Mono 12"
 font-Mac   "DejaVu Sans Mono 12"
 font-Win   "Consolas 12" )

;; Bigger fonts
(when (and (boundp 'prefs-activate-bigfont) prefs-activate-bigfont)
  (setq
   font-X11   "DejaVu Sans Mono 14"
   font-Mac   "DejaVu Sans Mono 16"
   font-Win   "Consolas 14" ))

(defun set-myfont-preference () 
  "Setup the font preferences"
  (let ((fontspec (cond
                   (running-MacOSX   font-Mac)
                   (running-GNULinux font-X11)
                   (running-Windows  font-Win)
                   )))
    (set-default-font fontspec)
    (add-to-list 'default-frame-alist `(font . ,fontspec))))

(set-myfont-preference)

;; (add-hook 'after-make-frame-functions
;;     (lambda (frame) 
;;       (set-font-preference)))

;; Load color theme
(when (or  ;; enough colors? (meaningless in server-mode)
       (>= (display-color-cells) default-color-number)
       (if (fboundp 'daemonp) (daemonp)))
  (condition-case msg
      (load-theme default-color-theme t)
    (error (format "%s" msg) )))

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
(when running-NSCocoa-process
  (if (boundp 'ns-right-alternate-modifier)
      (progn
        (setq ns-alternate-modifier 'meta)
        (setq ns-right-alternate-modifier 'nil))
    (setq ns-alternate-modifier 'nil)))


;; GUI elements
(when-available 'scroll-bar-mode (scroll-bar-mode -1)) ;; scroll-bar-mode undefined in terminal emacs!
(when-available 'tool-bar-mode   (tool-bar-mode -1)  ) ;;   tool-bar-mode undefined in terminal emacs!
(menu-bar-mode -1)
(when-available 'tabbar-mode     (tabbar-mode -1)    ) ;;     tabbar-mode defined only in Aquamacs


(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(blink-cursor-mode nil)
(setq visible-bell nil)
(setq mouse-highlight nil)

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
(if running-Aquamacs
    (setq confirm-kill-emacs nil)     ; Aquamacs won't interrupt logging out
  (setq confirm-kill-emacs 'y-or-n-p) ; In other cases daemon is just killed.
  )

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
(defadvice recenter-top-bottom (after ad-recenter-show)
    "Highlight point after recentering."
    (pulse-momentary-highlight-one-line (point)))

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
