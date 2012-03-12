;;; massimo-keyboard.el --- Keybindings specific for the author habits

;; Copyright (C) 2010, 2011, 2012  Massimo Lauria

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

;; When Massimo-Keyboard mode is enabled, several keybindings are
;; modified according to the usage pattern of the author."


;;; Code:

(defgroup massimo-keyboard nil
  "Minor mode for which setup a bunch of useful keybindings.
At least they are considered useful for the author.
")


(defcustom massimo-keyboard-comint-modes nil
  "A list of major modes for which the keymap variant for
`comint-mode' is activated."
  :tag "Comint derived modes with special keymap"
  :type 'sexp
  :group 'massimo-keyboard
  )

(defcustom massimo-keyboard-folding-meta-g-override-p t
  "*If non-nil, override folding mode M-g for `folding-goto-line' with `move-beginning-of-line'."
  :tag "Massimo Keyboard Folding meta-g override"
  :type 'boolean
  :group 'massimo-keyboard)


(defvar massimo-keyboard-mode-map
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
    (define-key map (kbd "M-g") 'move-beginning-of-line) ;; Fights with folding-mode
    (define-key map (kbd "M-h") 'move-end-of-line)
    ;; Paragraphs
    (define-key map (kbd "M-b") 'backward-paragraph)     ;; Fight with canonical binding
    (define-key map (kbd "M-n") 'forward-paragraph)

    ;; Out of expression
    (define-key map (kbd "M-p") 'up-list)

    ;; Deletion keys
    (define-key map (kbd "M-w")  'backward-kill-word)
    (define-key map (kbd "M-d")  'backward-delete-char)
    (define-key map (kbd "M-f")  'delete-char)
    (define-key map (kbd "C-w")  'kill-whole-line)


    ;; Moving between buffers (M-S)
    (require 'init-buffer-navigation nil t)
    (if (boundp 'buffer-navigation-method)
        (progn
          (define-key map [M-S-up] 'previous-buffer-smart)
          (define-key map [M-S-down] 'next-buffer-smart)
          (define-key map [M-S-left] 'previous-buffer-navigation-method)
          (define-key map [M-S-right] 'next-buffer-navigation-method)
          )
      (define-key map [M-S-up] 'previous-buffer)
      (define-key map [M-S-down] 'next-buffer)
      )
    (define-key map (kbd "C-x C-b") 'ibuffer)


    ;; Register keys
    ;; Register at finger tips from 1 to ... 0!
    (define-key map (kbd "M-r M-r") 'pop-to-mark-command) ;; Pop mark-ring

    ;; Load registers position
    (define-key map (kbd "M-1") '(lambda () (interactive) (register-to-point 1) )  )
    (define-key map (kbd "M-2") '(lambda () (interactive) (register-to-point 2) )  )
    (define-key map (kbd "M-3") '(lambda () (interactive) (register-to-point 3) )  )
    (define-key map (kbd "M-4") '(lambda () (interactive) (register-to-point 4) )  )
    (define-key map (kbd "M-5") '(lambda () (interactive) (register-to-point 5) )  )
    (define-key map (kbd "M-6") '(lambda () (interactive) (register-to-point 6) )  )
    (define-key map (kbd "M-7") '(lambda () (interactive) (register-to-point 7) )  )
    (define-key map (kbd "M-9") '(lambda () (interactive) (register-to-point 9) )  )
    ;; Save register position
    (define-key map (kbd "M-r 0") '(lambda () (interactive) (point-to-register 0) )  )
    (define-key map (kbd "M-r 1") '(lambda () (interactive) (point-to-register 1) )  )
    (define-key map (kbd "M-r 2") '(lambda () (interactive) (point-to-register 2) )  )
    (define-key map (kbd "M-r 3") '(lambda () (interactive) (point-to-register 3) )  )
    (define-key map (kbd "M-r 4") '(lambda () (interactive) (point-to-register 4) )  )
    (define-key map (kbd "M-r 5") '(lambda () (interactive) (point-to-register 5) )  )
    (define-key map (kbd "M-r 6") '(lambda () (interactive) (point-to-register 6) )  )
    (define-key map (kbd "M-r 7") '(lambda () (interactive) (point-to-register 7) )  )
    (define-key map (kbd "M-r 9") '(lambda () (interactive) (point-to-register 9) )  )
    (define-key map (kbd "M-r 0") '(lambda () (interactive) (point-to-register 0) )  )

    map)
  "Keymap for massimo-keyboard-mode.")


;; Define the mode as global
(define-globalized-minor-mode
  massimo-keyboard-global-mode
  massimo-keyboard-mode
  massimo-keyboard-activate
  :group 'massimo-keyboard)

;; Utility functions
(defun massimo-keyboard-activate () (massimo-keyboard-mode 1))

(make-variable-buffer-local 'massimo-keyboard-folding-meta-g-overridden)



(defun massimo-keyboard-folding-meta-g-override ()
"If massimo-keyboard-mode is active then run backward-paragraph,
otherwise cua-repeat-replace-region. An ugly hack to solve key conflicts..."
  (interactive)
  (if massimo-keyboard-mode
      (call-interactively 'move-beginning-of-line)
    (call-interactively 'folding-goto-line))
  )


(define-minor-mode massimo-keyboard-mode
  "Toggle Massimo-Keyboard mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Massimo-Keyboard mode is enabled, several keybindings are
modified according to the useage pattern of the author."
  :init-value nil
  :lighter " Massimo" ; Modeline string
  :group   'massimo-keyboard

  (when (and
         (not (eq massimo-keyboard-folding-meta-g-overridden 'massimo-keyboard-folding-meta-g-override))
         (boundp 'folding-mode-map)
         massimo-keyboard-folding-meta-g-override-p
         )
    ;; Function to be overridded
    (setq massimo-keyboard-folding-meta-g-overridden (lookup-key folding-mode-map [(meta g)]))
    (if massimo-keyboard-folding-meta-g-overridden
         (define-key folding-mode-map [(meta g)] 'massimo-keyboard-folding-meta-g-override)))

  ;; suppress auxiliary keymaps
  (setq massimo-keyboard-comint nil)
  (when massimo-keyboard-mode
    (setq massimo-keyboard-comint (member major-mode massimo-keyboard-comint-modes))
    ))


;;; Auxiliary keymaps which extend `massimo-keyboard-mode-map', in
;;; order to specify keybindings for specific modes

;; ---- `comint-mode'  ----------------------------------------------------

(make-variable-buffer-local 'massimo-keyboard-comint)
(set-default 'massimo-keyboard-comint     nil)

(defvar massimo-keyboard-comint-mode-map
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
    (define-key map (kbd "C-w")  'comint-kill-whole-line)

    map)
  "Keymap for massimo-keyboard-mode (for Comint mode).")

(add-to-list 'minor-mode-map-alist
             (cons 'massimo-keyboard-comint massimo-keyboard-comint-mode-map))


(provide 'massimo-keyboard)
;;; massimo-keyboard.el ends here


