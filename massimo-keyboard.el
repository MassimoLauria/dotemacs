;;; massimo-keyboard.el --- Keybindings specific for the author habits -*- coding: utf-8 -*-

;; Copyright (C) 2010, 2011, 2012, 2013, 2015, 2016  Massimo Lauria

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

(defcustom massimo-keyboard-eshell-active t
  "Activate special keybinding for Eshell."
  :tag "Activate special keymap for Eshell"
  :type 'boolean
  :group 'massimo-keyboard
  )

(defcustom massimo-keyboard-org-active t
  "Activate special keybinding for org-mode."
  :tag "Activate special keymap for Org"
  :type 'boolean
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
    (define-key map (kbd "M-e")  'backward-kill-word)
    (define-key map (kbd "M-r")  'kill-word)
    (define-key map (kbd "M-d")  'backward-delete-char)
    (define-key map (kbd "M-f")  'delete-char)
    (define-key map (kbd "M-w")  'kill-whole-line)


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

    (define-key map (kbd "C-x C-b") 'ivy-switch-buffer)
    (define-key map (kbd "C-x b")   'ivy-switch-buffer)
    (define-key map (kbd "C-x C-r") 'ivy-switch-buffer)
    (define-key map (kbd "C-x C-f") 'counsel-find-file)
    (define-key map (kbd "M-x")     'counsel-M-x)
    (define-key map (kbd "C-s")     'swiper)

    (define-key map (kbd "C-x k")   'kill-this-buffer)
    ;; Default text navigation (usually shadowed by other modes)
    (define-key map (kbd "M-.") 'org-open-at-point-global) 
    (define-key map (kbd "M-,") 'pop-global-mark)  
 
    ;; Alternative  pop mark (usually not shadowed)
    (define-key map (kbd "C-M-,") 'pop-global-mark)
    (define-key map (kbd "M-*")   'pop-global-mark)
    

    ;; Edit power features
    (define-key map (kbd "C-'") 'narrow-or-widen-dwim)
    (define-key map (kbd "C-;") 'iedit-mode)

    ;; Expand region configuration
    (define-key map (kbd "C-SPC") 'er/expand-region)

    ;; Zoom in/out
    (define-key map (kbd "C--") 'zoom-frm-out)
    (define-key map (kbd "C-=") 'zoom-frm-in)
    (define-key map (kbd "C-0") 'zoom-frm-unzoom)
    

    
    ;; Register keys
    ;; Register at finger tips from 1 to ... 0!
    ;; (define-key map (kbd "M-r M-r") 'pop-to-mark-command) ;; Pop mark-ring

    ;; Load registers position
    ;; (define-key map (kbd "M-1") '(lambda () (interactive) (register-to-point 1) )  )
    ;; (define-key map (kbd "M-2") '(lambda () (interactive) (register-to-point 2) )  )
    ;; (define-key map (kbd "M-3") '(lambda () (interactive) (register-to-point 3) )  )
    ;; (define-key map (kbd "M-4") '(lambda () (interactive) (register-to-point 4) )  )
    ;; (define-key map (kbd "M-5") '(lambda () (interactive) (register-to-point 5) )  )
    ;; (define-key map (kbd "M-6") '(lambda () (interactive) (register-to-point 6) )  )
    ;; (define-key map (kbd "M-7") '(lambda () (interactive) (register-to-point 7) )  )
    ;; (define-key map (kbd "M-9") '(lambda () (interactive) (register-to-point 9) )  )
    ;; ;; Save register position
    ;; (define-key map (kbd "M-r 0") '(lambda () (interactive) (point-to-register 0) )  )
    ;; (define-key map (kbd "M-r 1") '(lambda () (interactive) (point-to-register 1) )  )
    ;; (define-key map (kbd "M-r 2") '(lambda () (interactive) (point-to-register 2) )  )
    ;; (define-key map (kbd "M-r 3") '(lambda () (interactive) (point-to-register 3) )  )
    ;; (define-key map (kbd "M-r 4") '(lambda () (interactive) (point-to-register 4) )  )
    ;; (define-key map (kbd "M-r 5") '(lambda () (interactive) (point-to-register 5) )  )
    ;; (define-key map (kbd "M-r 6") '(lambda () (interactive) (point-to-register 6) )  )
    ;; (define-key map (kbd "M-r 7") '(lambda () (interactive) (point-to-register 7) )  )
    ;; (define-key map (kbd "M-r 9") '(lambda () (interactive) (point-to-register 9) )  )
    ;; (define-key map (kbd "M-r 0") '(lambda () (interactive) (point-to-register 0) )  )

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

(defun massimo-keyboard/org-mode-active ()
  "Check if org-mode is active. Workaround to compatibility."
  (interactive)
  (let ((org-6  (and (fboundp 'org-mode-p) (org-mode-p)))           ; Org 6.xx
        (org-7  (and    (boundp 'org-mode) org-mode))               ; Org 7.xx
        (org-8  (and    (featurep 'org) (eq major-mode 'org-mode))) ; Org 8.xx
        ) 
    (or org-6 org-7 org-8)
    ))

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
  (setq
   massimo-keyboard-comint   nil
   massimo-keyboard-eshell   nil
   massimo-keyboard-org      nil
   )
  (when massimo-keyboard-mode
    (setq
     ;; comint
     massimo-keyboard-comint (member major-mode massimo-keyboard-comint-modes)
     ;; eshell
     massimo-keyboard-eshell (and massimo-keyboard-eshell-active
                                  (eq major-mode 'eshell-mode))
     ;; org-mode
     massimo-keyboard-org    (and massimo-keyboard-org-active (massimo-keyboard/org-mode-active))
     )))


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
    (define-key map (kbd "M-w")  'comint-kill-whole-line)
    
    ;; hide buffer
    (define-key map (kbd "<M-return>")  'bury-buffer)

    map)
  "Keymap for massimo-keyboard-mode (for Comint mode).")

(add-to-list 'minor-mode-map-alist
             (cons 'massimo-keyboard-comint massimo-keyboard-comint-mode-map))


;; ---- `eshell-mode'  ----------------------------------------------------

(make-variable-buffer-local 'massimo-keyboard-eshell)
(set-default 'massimo-keyboard-eshell     nil)

(defvar massimo-keyboard-eshell-mode-map
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
  "Keymap for massimo-keyboard-mode (for Eshell mode).")

(add-to-list 'minor-mode-map-alist
             (cons 'massimo-keyboard-eshell massimo-keyboard-eshell-mode-map))


;; ---- `org-mode'  ----------------------------------------------------

(make-variable-buffer-local 'massimo-keyboard-org)
(set-default 'massimo-keyboard-org     nil)

(defvar massimo-keyboard-org-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "M-SPC")   'org-cycle)  ; Use M-Space for
                                                 ; org-cycle, which is
                                                 ; similar to what I
                                                 ; use for
                                                 ; folding-toggle-show-hide

    ;; Seems to work in X window
    (define-key map (kbd "C-è") 'org-shiftup    )
    (define-key map (kbd "C-à") 'org-shiftdown  )
    (define-key map (kbd "C-ò") 'org-shiftleft  )
    (define-key map (kbd "C-ù") 'org-shiftright )

    ;; Seems to work in my Xterm
    (define-key map "\e[27;5;232~" 'org-shiftup)
    (define-key map "\e[27;5;224~" 'org-shiftdown)
    (define-key map "\e[27;5;242~" 'org-shiftleft)
    (define-key map "\e[27;5;249~" 'org-shiftright)

    (define-key map (kbd "M-è") 'org-metaup    )
    (define-key map (kbd "M-à") 'org-metadown  )
    (define-key map (kbd "M-ò") 'org-metaleft  )
    (define-key map (kbd "M-ù") 'org-metaright )

    ;; Xterm apparently does not generate such sequences.
    (define-key map (kbd "C-M-è") 'org-shiftmetaup    )
    (define-key map (kbd "C-M-à") 'org-shiftmetadown  )
    (define-key map (kbd "C-M-ò") 'org-shiftmetaleft  )
    (define-key map (kbd "C-M-ù") 'org-shiftmetaright )

    map)
  "Keymap for massimo-keyboard-mode (for Org mode).")

(add-to-list 'minor-mode-map-alist
             (cons 'massimo-keyboard-org massimo-keyboard-org-mode-map))




(provide 'massimo-keyboard)
;;; massimo-keyboard.el ends here

