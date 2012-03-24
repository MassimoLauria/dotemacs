;;; init-terminal-fix.el --- Fix various terminal issues with keyboad. -*- coding: utf-8 -*-

;; Copyright (C) 2012  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>

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

;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;




;;; Terminal setup function for 'screen' terminal.  There is a similar
;;; function for each terminal, like xterm and rvxt. The one for
;;; screen is missing, but it is called by multi-tty emacs if present.
(defun terminal-init-screen ()
  "Terminal initialization function for screen-256color."
  (load "term/xterm")
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))


;; tty-type and input-decode-map are not defined in Emacs < 23.
(when running-GNUEmacs23+

  (defun myfix-xterm  ()
    "Some key combinations would not work on `xterm' (e.g. shift-up mapped to <select>). This will fix it."
    (interactive)
    (define-key input-decode-map "\e[1;2A" [S-up]))


  (defun myfix-screen ()
    "Some key combinations would not work in `screen'. This will fix it."
    (interactive)
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
    (define-key input-decode-map "\e[1;7D" [C-M-left])
    )


;;; Run the appropriate fix if emacs is run directly from terminal.
  (cond
   ((equal "xterm" (tty-type)) (myfix-xterm))
   ((equal "xterm-256color" (tty-type)) (myfix-xterm))
   ((equal "screen" (tty-type)) (myfix-screen))
   ((equal "screen-256color" (tty-type)) (myfix-screen))
   ((equal "screen-256color-bce" (tty-type)) (myfix-screen))
   )

;;; Run the appropriate fix if emacs is a multi-tty instance. The fix
;;; is run by emacs itself after tty initialization.

  ;; xterm
  (defadvice terminal-init-xterm (after myfix-xterm activate)
    "Map some terminal escape sequences to the correct keys"
    (myfix-xterm))
  ;; screen
  (defadvice terminal-init-screen (after myfix-screen activate)
    "Map some terminal escape sequences to the correct keys"
    (myfix-screen))


  ) ;; (when-running-GNUEmacs23+ ...

(provide 'init-terminal-fix)
;;; init-terminal-fix.el ends here
