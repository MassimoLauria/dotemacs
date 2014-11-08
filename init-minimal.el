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
(setq load-path (cons "~/config/emacs/" load-path))

;; Emacs packages
(setq package-user-dir "~/.emacs.d/elpa-minimal/")
(setq package-archives  '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("melpa-stable" . "http://stable.melpa.org/packages/")))
(when (require 'package nil t)
  (package-initialize))


;; Usability setup
(when (fboundp 'cua-mode)           (cua-mode            t))
(when (fboundp 'show-paren-mode)    (show-paren-mode     t))
(when (fboundp 'scroll-bar-mode)    (scroll-bar-mode    -1))
(when (fboundp 'menu-bar-mode)      (menu-bar-mode      -1))
(when (fboundp 'tool-bar-mode)      (tool-bar-mode      -1))
(when (fboundp 'line-number-mode)   (line-number-mode    t))
(when (fboundp 'column-number-mode) (column-number-mode  t))

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
  (define-key input-decode-map "\e[1;7C" [C-M-S-right])
  (define-key input-decode-map "\e[1;7D" [C-M-left]))

;; Color theme
(condition-case msg
    (load-theme 'zenburn t)
    (error (format "%s" msg) ))

;; -------------------------------------------------------------------
(provide 'init-minimal)
