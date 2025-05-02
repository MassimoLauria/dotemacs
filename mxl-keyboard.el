;;; mxl-keyboard.el --- Keybindings specific for the author habits -*- coding: utf-8 -*-

;; Copyright (C) 2010-2021, 2024, 2025  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
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

;; When `mxl-keyboard-mode' mode is enabled, several keybindings are
;; modified according to the usage pattern of the author."


;;; Code:

(defgroup mxl-keyboard nil
  "Minor mode for which setup a bunch of useful keybindings.
At least they are considered useful for the author.
")

(defvar mxl-keyboard-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movements in Text without leaving homerow.
    (define-key map (kbd "M-j") 'backward-char)
    (define-key map (kbd "M-l") 'forward-char)
    (define-key map (kbd "M-i") 'previous-line)
    (define-key map (kbd "M-k") 'next-line)
    ;; Word movements
    (define-key map (kbd "M-u") 'backward-word)
    (define-key map (kbd "M-o") 'forward-word)
    ;; Line
    (define-key map (kbd "M-g") 'move-beginning-of-line)
    (define-key map (kbd "M-h") 'move-end-of-line)
    ;; Paragraphs
    (define-key map (kbd "M-b") 'backward-paragraph)     ;; Fight with canonical binding
    (define-key map (kbd "M-n") 'forward-paragraph)

    ;; Relatively standard movements on arrow keys I don't use them
    ;; a lot but they work in other systems and I sometime find myself
    ;; using them
    (define-key map [C-left]  'backward-word)
    (define-key map [C-right] 'forward-word)
    (define-key map [C-up]    'backward-paragraph)
    (define-key map [C-down]  'forward-paragraph)

    ;; Deletion keys
    (define-key map (kbd "M-e")  'backward-kill-word)
    (define-key map (kbd "M-r")  'kill-word)
    (define-key map (kbd "M-d")  'backward-delete-char)
    (define-key map (kbd "M-f")  'delete-char)
    (define-key map (kbd "M-w")  'kill-whole-line)


    (define-key map (kbd "C-x k") 'kill-this-buffer)
    ;; Default text navigation (usually shadowed by other modes)
    (define-key map (kbd "M-.") 'xref-find-definitions)
    (define-key map (kbd "M-,") 'xref-pop-marker-stack)

    ;; Edit power features
    (define-key map (kbd "C-'") 'narrow-or-widen-dwim)
    (define-key map (kbd "C-;") 'iedit-mode)

    ;; Zoom in/out buffer fonts
    (define-key map (kbd "C--") 'global-text-scale-adjust)
    (define-key map (kbd "C-=") 'global-text-scale-adjust)
    (define-key map (kbd "C-+") 'global-text-scale-adjust)
    (define-key map (kbd "C-0") 'global-text-scale-adjust)

    map)
  "Keymap for mxl-keyboard-mode.")


;; Define the mode as global
(define-globalized-minor-mode
  mxl-keyboard-global-mode
  mxl-keyboard-mode
  mxl-keyboard-activate
  :group 'mxl-keyboard)

;; Utility functions
(defun mxl-keyboard-activate ()
  "Activate `mxlkeyboard-mode'."
  (mxl-keyboard-mode 1))

(define-minor-mode mxl-keyboard-mode
  "Toggle mxl-keyboard mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  :init-value nil
  :lighter " mxl" ; Modeline string
  :group   'mxl-keyboard

  ;; suppress auxiliary keymaps
  (setq mxl-keyboard-comint
        (and mxl-keyboard-mode
             (member major-mode
                     '(shell-mode
                       comint-mode
                       inferior-octave-mode
                       inferior-emacs-lisp-mode)))))



;; ---- `comint-mode'  keybidings ----------------------------------------------------------------

(make-variable-buffer-local 'mxl-keyboard-comint)
(set-default 'mxl-keyboard-comint     nil)

(defvar mxl-keyboard-comint-mode-map
  (let ((map (make-sparse-keymap)))
    ;; History searching
    (define-key map (kbd "M-i") 'comint-previous-input)
    (define-key map (kbd "M-k") 'comint-next-input)
    (define-key map (kbd "<up>") 'comint-previous-input)
    (define-key map (kbd "<down>") 'comint-next-input)
    (define-key map (kbd "<prior>") 'comint-previous-matching-input-from-input)
    (define-key map (kbd "<next>")  'comint-next-matching-input-from-input)
    ;; Line ends
    (define-key map (kbd "M-g") 'comint-bol-or-process-mark)
    (define-key map (kbd "M-h") 'move-end-of-line)
    (define-key map (kbd "<home>") 'comint-bol-or-process-mark)
    (define-key map (kbd "<end>")  'move-end-of-line)

    ;; Paragraphs
    (define-key map (kbd "M-b") 'comint-previous-prompt)     ;; Fight with canonical binding
    (define-key map (kbd "M-n") 'comint-next-prompt)

    ;; Deletion keys
    (define-key map (kbd "M-w")  'comint-kill-whole-line)

    ;; hide buffer
    (define-key map (kbd "<M-return>")  'bury-buffer)

    map)
  "Keymap for mxl-keyboard-mode (for Comint mode).")

(add-to-list 'minor-mode-map-alist
             (cons 'mxl-keyboard-comint mxl-keyboard-comint-mode-map))




(provide 'mxl-keyboard)
;;; mxl-keyboard.el ends here
