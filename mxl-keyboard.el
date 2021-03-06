;;; mxl-keyboard.el --- Keybindings specific for the author habits -*- coding: utf-8 -*-

;; Copyright (C) 2010-2021  Massimo Lauria

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

(defcustom mxl-keyboard-comint-modes '(shell-mode comint-mode inferior-octave-mode inferior-emacs-lisp-mode)
  "`comint' keybidnings from mxl activated in these modes."
  :tag "Comint derived modes where mxl keymap is active"
  :type 'sexp
  :group 'mxl-keyboard
  )

(defcustom mxl-keyboard-eshell-active t
  "Activate mxl keybindings for Eshell."
  :tag "Activate mxl keybidings for Eshell"
  :type 'boolean
  :group 'mxl-keyboard
  )

(defvar mxl-keyboard-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movements in Text without leaving basic keyboard.
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

    ;; Out of expression
    (define-key map (kbd "M-p") 'up-list)

    ;; Ripgrep
    (define-key map (kbd "C-M-s") 'helm-rg)

    ;; Deletion keys
    (define-key map (kbd "M-e")  'backward-kill-word)
    (define-key map (kbd "M-r")  'kill-word)
    (define-key map (kbd "M-d")  'backward-delete-char)
    (define-key map (kbd "M-f")  'delete-char)
    (define-key map (kbd "M-w")  'kill-whole-line)


    (define-key map (kbd "C-x k")   'kill-this-buffer)
    ;; Default text navigation (usually shadowed by other modes)
    (define-key map (kbd "M-.") 'xref-find-definitions)
    (define-key map (kbd "M-,") 'xref-pop-marker-stack)

    ;; Edit power features
    (define-key map (kbd "C-'") 'narrow-or-widen-dwim)
    (define-key map (kbd "C-;") 'iedit-mode)


    ;; Expand region configuration
    (define-key map (kbd "C-SPC") 'er/expand-region)

    ;; Zoom in/out
    (define-key map (kbd "C--") 'text-scale-decrease)
    (define-key map (kbd "C-=") 'text-scale-increase)
    (define-key map (kbd "C-0") 'text-scale-adjust)

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
Null prefix argument turns off the mode.

When Massimo-Keyboard mode is enabled, several keybindings are
modified according to the useage pattern of the author."
  :init-value nil
  :lighter " mxl" ; Modeline string
  :group   'mxl-keyboard

  ;; suppress auxiliary keymaps
  (setq
   mxl-keyboard-comint   nil
   mxl-keyboard-eshell   nil
   )
  (when mxl-keyboard-mode
    (setq
     ;; comint
     mxl-keyboard-comint (member major-mode mxl-keyboard-comint-modes)
     ;; eshell
     mxl-keyboard-eshell (and mxl-keyboard-eshell-active
                                  (eq major-mode 'eshell-mode))
     )))


;;; Auxiliary keymaps which extend `mxl-keyboard-mode-map', in
;;; order to specify keybindings for specific modes

;; ---- `comint-mode'  ----------------------------------------------------

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


;; ---- `eshell-mode'  ----------------------------------------------------

(make-variable-buffer-local 'mxl-keyboard-eshell)
(set-default 'mxl-keyboard-eshell     nil)

(defvar mxl-keyboard-eshell-mode-map
  (let ((map (make-sparse-keymap)))
    ;; History searching
    (define-key map (kbd "M-i") 'eshell-previous-input)
    (define-key map (kbd "M-k") 'eshell-next-input)
    (define-key map (kbd "<up>") 'eshell-previous-input)
    (define-key map (kbd "<down>") 'eshell-next-input)
    (define-key map (kbd "<prior>") 'eshell-previous-matching-input-from-input)
    (define-key map (kbd "<next>")  'eshell-next-matching-input-from-input)
    ;; Line ends
    (define-key map (kbd "M-g") 'eshell-bol)
    (define-key map (kbd "M-h") 'move-end-of-line)
    (define-key map (kbd "<home>") 'eshell-bol)
    (define-key map (kbd "<end>")  'move-end-of-line)

    ;; Paragraphs
    (define-key map (kbd "M-b") 'eshell-previous-prompt)     ;; Fight with canonical binding
    (define-key map (kbd "M-n") 'eshell-next-prompt)

    ;; Deletion keys
    (define-key map (kbd "M-w")  'eshell-kill-input)

    (define-key map (kbd "<M-return>")  'bury-buffer)

    map)
  "Keymap for mxl-keyboard-mode (for Eshell mode).")

(add-to-list 'minor-mode-map-alist
             (cons 'mxl-keyboard-eshell mxl-keyboard-eshell-mode-map))



(provide 'mxl-keyboard)
;;; mxl-keyboard.el ends here
