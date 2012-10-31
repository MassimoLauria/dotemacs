;;; -*- coding: utf-8 -*-
;;;
;;; Basic Editor customization
;;;
;;;-----------------------------------------------------------------

;; Regular fonts
(setq
 font-X11-no-antialias "-misc-fixed-medium-r-normal--18-*-*-*-*-*-iso10646-1"
 font-X11-antialias    "Inconsolata-14"
 ;; font-Mac-antialias    "Monaco-12"
 font-Mac-antialias    "-apple-dejavu sans mono-medium-r-normal--0-0-0-0-m-0-mac-roman"
 font-Win-antialias    "Consolas-14"
 )

;; Bigger fonts
(when (and (boundp 'prefs-activate-bigfont) prefs-activate-bigfont)
  (setq
   font-X11-no-antialias "-misc-fixed-medium-r-normal--20-*-*-*-*-*-iso10646-1"
   font-X11-antialias    "Inconsolata-14"
   ;; font-Mac-antialias    "Monaco-14"
   font-Mac-antialias    "-apple-dejavu sans mono-medium-r-normal--20-0-0-0-m-0-mac-roman"
   font-Win-antialias    "Consolas-14"
   ))

;; Meta usage in MacOSX requires some thought
(when running-NSCocoa-process
  (if (boundp 'ns-right-alternate-modifier)
      (progn
        (setq ns-alternate-modifier 'meta)
        (setq ns-right-alternate-modifier 'nil))
    (setq ns-alternate-modifier 'nil)
    )
)


;; Decide whether load the color-theme
(defun my-theme-loadable-p (theme-name required-colors)
  "It is possible or desiderable to load the color-theme?  If
Emacs is running as a server, or it has enough color, then tries
to load the theme.
"
  (and
   (or  ;; enough colors? (meaningless in server-mode)
    (>= (display-color-cells) required-colors)
    (if (fboundp 'daemonp) (daemonp))
    )
   (fboundp theme-name)
   t ))


;;; Font setup
;; Linux (GNUEmacs >=23)
(when (and running-GNULinux running-GNUEmacs23+ )
  (set-default-font font-X11-antialias)
  (add-to-list 'default-frame-alist `(font . ,font-X11-antialias))
  )
;; Linux (GNUEmacs 22)
(when (and running-GNULinux running-GNUEmacs22)
  (set-default-font font-X11-no-antialias)
  (add-to-list 'default-frame-alist `(font . ,font-X11-no-antialias))
  )
;; MacOSX
(when (and running-MacOSX running-GNUEmacs23+)
  (set-default-font font-Mac-antialias)
  (add-to-list 'default-frame-alist `(font . ,font-Mac-antialias))
  )
;; Windows
(when (and running-Windows running-GNUEmacs23+)
 (set-default-font font-Win-antialias)
 (add-to-list 'default-frame-alist `(font . ,font-Win-antialias))
 )


;; Color theme (not available on default Emacs22 for MacOSX)
(autoload 'color-theme-zenburn "zenburn.el"
  "Just some alien fruit salad to keep you in the zone." t nil)

(when (my-theme-loadable-p 'color-theme-zenburn 256)
  (when-available 'color-theme-zenburn (color-theme-zenburn)))

(when-available 'scroll-bar-mode (scroll-bar-mode -1)) ;; scroll-bar-mode undefined in terminal emacs!
(when-available 'tool-bar-mode   (tool-bar-mode -1)  ) ;;   tool-bar-mode undefined in terminal emacs!
(menu-bar-mode -1)
(when-available 'tabbar-mode     (tabbar-mode -1)    ) ;;     tabbar-mode defined only in Aquamacs


(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(blink-cursor-mode nil)
(setq visible-bell nil)

(setq search-highlight t)
(setq query-replace-highlight t)

(line-number-mode 1)
(column-number-mode 1)

(setq x-stretch-cursor t)
(show-paren-mode 1)

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
(setq compilation-auto-jump-to-first-error t)

;; Scroll preferences
(setq scroll-preserve-screen-position 1)
(setq scroll-margin 0)
(setq scroll-conservatively 1000)

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
;; folded-file: t
;; End:
