;;; init-minimal.el --- Minimal conf file -*- coding: utf-8 -*-
;;
;; This configuration file could a starting point to test features and
;; packages.  Unfortunately my usual setup is so complex and full of
;; workarounds that something breaks when I do some serious upgrade or
;; install new packages. Most of the times this is because of some
;; code that was a good idea at some point but it is not anymore.
;;
;; - compatible with older Emacs
;; - self contained
;; - still quite usable
;;
;; Usage:
;;
;; $ emacs -q -l ~/config/emacs/init-minimal.el
;;


;; -------------------------------------------------------------------
;; Basic
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(setq confirm-kill-emacs nil)     ; exit without questions

;; Usability setup
(when (fboundp 'cua-mode)           (cua-mode            t))
(when (fboundp 'show-paren-mode)    (show-paren-mode     t))
(when (fboundp 'scroll-bar-mode)    (scroll-bar-mode    -1))
(when (fboundp 'menu-bar-mode)      (menu-bar-mode      -1))
(when (fboundp 'tool-bar-mode)      (tool-bar-mode      -1))
(when (fboundp 'line-number-mode)   (line-number-mode    t))
(when (fboundp 'column-number-mode) (column-number-mode  t))

;; Keyboard
(require 'iso-transl)
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
    ;; Deletion keys
    (define-key map (kbd "M-e")  'backward-kill-word)
    (define-key map (kbd "M-r")  'kill-word)
    (define-key map (kbd "M-d")  'backward-delete-char)
    (define-key map (kbd "M-f")  'delete-char)
    (define-key map (kbd "M-w")  'kill-whole-line)
    ;; Moving between buffers
    (define-key map (kbd "M-m")   'other-window)
    ;; Quicker actions
    (define-key map (kbd "C-x k") 'kill-this-buffer)
    ;; Compilation
    (define-key map (kbd "<f9>") 'compile)
    (define-key map (kbd "<f11>")  'previous-error)
    (define-key map (kbd "<f12>")  'next-error)
    map)
  "Keymap for minimo-keyboard-mode.")

(define-minor-mode minimo-keyboard-mode
  "Minimal keyboard setup"
  nil
  " Minimo" ; Modeline string
  minimo-keyboard-mode-map)


(defun turn-on-minimo-keyboard ()
  (minimo-keyboard-mode 1))
(define-globalized-minor-mode
  minimo-keyboard-global-mode
  minimo-keyboard-mode
  turn-on-minimo-keyboard)

(minimo-keyboard-global-mode)

;; Fix XTerm issues
(when (and (>= emacs-major-version 23)
           (member (tty-type) '("xterm"
				"xterm-256color"
				"screen"
				"screen-256-color")))
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2C" [S-right])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;4A" [M-S-up])
  (define-key input-decode-map "\e[1;4B" [M-S-down])
  (define-key input-decode-map "\e[1;4C" [M-S-right])
  (define-key input-decode-map "\e[1;4D" [M-S-left])
  (define-key input-decode-map "\e[1;7A" [C-M-up])
  (define-key input-decode-map "\e[1;7B" [C-M-down])
  (define-key input-decode-map "\e[1;7C" [C-M-right])
  (define-key input-decode-map "\e[1;7D" [C-M-left]))


;;; Enabled/Disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'iconify-or-deiconify-frame 'disabled t)

;; -------------------------------------------------------------------
(provide 'init-minimal)
