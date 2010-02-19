;;;
;;; Basic Editor customization
;;;
(provide 'init-preferences)
;;;-----------------------------------------------------------------


(set-default-font "Monospace-10")
(add-to-list 'default-frame-alist '(font . "Monospace-10"))

(when (boundp 'ask-bigfont) 
  (set-default-font "Monospace-14")
  (add-to-list 'default-frame-alist '(font . "Monospace-14"))
)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

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
(setq confirm-kill-emacs 'y-or-n-p)


;;  Tab expansion
(setq-default indent-tabs-mode nil) ;; Expand tabs as spaces
(setq default-tab-width 4)


;; Scroll preferences
(setq scroll-preserve-screen-position 1)
(setq scroll-margin 0)
(setq scroll-conservatively 1000)

;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
