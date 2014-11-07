;;; init-minimal.el --- Miminal conf file -*- coding: utf-8 -*-
;;
;; This configuration file could a starting point to test features and
;; packages.  Unfortunately it seems that my usual setup is so complex
;; and full of workarounds that when I do some serious upgrade or
;; install new packages, something breaks because of some lisp code
;; that was a good idea a some time but not anymore.
;;
;; - compatible with older Emacs
;; - self contained
;;
;; Usage:
;;
;; $ emacs -q -l ~/config/emacs/minimal-init.el
;; 


;; -------------------------------------------------------------------
;; Basic
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq debug-on-error t)                 ; stop on errors
(setq load-path (cons "~/config/emacs/" load-path))

;; Emacs packages
(setq package-user-dir "~/.emacs.d/elpa-minimal/")
(setq package-archives  '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("melpa-stable" . "http://stable.melpa.org/packages/")))
(when (require 'package nil t)
  (package-initialize))


;; Usability setup
(when (fboundp 'cua-mode)           (cua-mode))
(when (fboundp 'show-paren-mode)    (show-paren-mode))
(when (fboundp 'scroll-bar-mode)    (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)      (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)      (tool-bar-mode -1))
(when (fboundp 'line-number-mode)   (line-number-mode))
(when (fboundp 'column-number-mode) (column-number-mode))

;; Keyboard
(defvar minimo-keyboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-j") 'backward-char)
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "M-i") 'previous-line)
    (define-key map (kbd "M-k") 'next-line)
    (define-key map (kbd "M-u") 'backward-word)
    (define-key map (kbd "M-o") 'forward-word)
    (define-key map (kbd "M-g") 'move-beginning-of-line)
    (define-key map (kbd "M-h") 'move-end-of-line)
    (define-key map (kbd "M-b") 'backward-paragraph)
    (define-key map (kbd "M-n") 'forward-paragraph)
    (define-key map (kbd "M-p") 'up-list)
    ;; Deletion keys
    (define-key map (kbd "M-e")  'backward-kill-word)
    (define-key map (kbd "M-r")  'kill-word)
    (define-key map (kbd "M-d")  'backward-delete-char)
    (define-key map (kbd "M-f")  'delete-char)
    (define-key map (kbd "M-w")  'kill-whole-line)
    ;; Moving between buffers (M-S)
    (define-key map [M-S-up] 'previous-buffer)
    (define-key map [M-S-down] 'next-buffer)
    map)
  "Keymap for minimo-keyboard-mode.")

(define-minor-mode minimo-keyboard-mode
  :init-value nil
  :lighter " Minimo" ; Modeline string
  :group   'minimo-keyboard)
(defun turn-on-minimo-keyboard () (minimo-keyboard-mode 1))
(define-globalized-minor-mode  minimo-keyboard-global-mode  
  minimo-keyboard-mode turn-on-minimo-keyboard)
(minimo-keyboard-global-mode)

;; Color theme
(condition-case msg
    (load-theme 'zenburn t)
    (error (format "%s" msg) ))

;; add TEST CODE DOWN HERE -------------------------------------------
