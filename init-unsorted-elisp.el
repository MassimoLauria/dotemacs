;;; init-unsorted-elisp.el --- Contains small chunks of elisp code in no particular order

;; Copyright (C) 2010  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Keywords:

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

;; Several functionality of Emacs must be set up with small chunks of
;; codes.  To avoid an excessive amount of clutter in the main init.el
;; file, all such small pieces are collected in this file.

;;; Code:




;; Auto-mode for renamed config files
(setq auto-mode-alist (cons '("bashrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("zshrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.zsh" . sh-mode) auto-mode-alist))



;; Undo-Tree, much better than default.
(require 'undo-tree)
(global-undo-tree-mode)


;; Text mode by default, with auto-fill
(require 'typopunct)
(setq-default typopunct-buffer-language 'english)
(setq default-major-mode 'text-mode)
;;(setq initial-major-mode 'text-mode) ;; Better to stick with Lisp-Interaction.
(setq text-mode-hook
      '(lambda nil
         (if prefs-activate-smallscreen
             (setq fill-column 70)
           (setq fill-column 80)
           )
         (auto-fill-mode 1)
         ;;(orgtbl-mode 1)  ; conflicts with autopair mode.
         (flyspell-mode 1)  ; annoying spell checking
         (when-available 'goto-address-mode (goto-address-mode)) ; Find urls/emails in text and press (C-c RET) to click them.
         ;;(typopunct-mode)
         )
      )


;; Prepare *scratch buffer*
;; FROM: Morten Welind
;; http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (if (boundp 'initial-major-mode)
      (eval (cons initial-major-mode ()))
      (lisp-interaction-mode)
    )
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))


;; Edit text area on Google Chrome
(if (and (fboundp 'daemon) (daemonp) (locate-library "edit-server"))
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start))
  )


;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
)

;; Ediff customization
; (no external control frame)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
; (use vertical split if there is enough room)
(setq ediff-split-window-function
      (lambda (&optional arg)
        (if (> (frame-width) 150)
            (split-window-horizontally arg)
          (split-window-vertically arg)
          )))



;; Save histories across sessions. Not buffers
(savehist-mode 1)

;; Use full featured gdb GUI.
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(add-hook 'gud-mode-hook '(lambda ()
                            (local-set-key (kbd "<f10>") 'gud-nexti)
                            (local-set-key (kbd "<f11>") 'gud-next )
                            (local-set-key (kbd "<f12>") 'gud-cont)

                            (local-set-key (kbd "M-<f10>") 'gud-stepi)
                            (local-set-key (kbd "M-<f11>") 'gud-step )
                            (local-set-key (kbd "M-<f12>") 'gud-until)

                            (local-set-key (kbd "<f9>"  ) 'gud-break)
                            (local-set-key (kbd "M-<f9>"  ) 'gud-tbreak)
                            )
          )


;; fixme highlight
(require 'fixme)

;; IDO mode for selection of file and buffers. VERY GOOD
(require 'ido)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "<tab>")   'ido-complete)
            (define-key ido-completion-map (kbd "M-<tab>") 'ido-next-match)
            (define-key ido-completion-map (kbd "M-j") 'ido-prev-match)
            (define-key ido-completion-map (kbd "M-l") 'ido-next-match)
            (define-key ido-completion-map (kbd "M-i") 'ido-prev-match)
            (define-key ido-completion-map (kbd "M-k") 'ido-next-match)
            ))

(ido-mode t)

(setq ido-enable-flex-matching t ; fuzzy matching is a must have
      ido-max-prospects 5        ; minibuffer is not saturated
      ido-ignore-buffers ;; ignore these guys
       '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido")
      ido-everywhere t            ; use for many file dialogs
      ido-case-fold  t            ; be case-insensitive
      ido-auto-merge-werk-directories-length nil) ; all failed, no more digging

;; Moving between windows with (M-C-<arrow>)
(require 'windmove)               ; to load the package
(setq windmove-wrap-around t)

;; Overwrite flymake-display-warning so that no annoying dialog box is
;; used.
(defun flymake-display-warning (warning)
  "Display a warning to the user, using lwarn"
  (message warning))


;; All urls/mails are clickable in comments and strings (Not present in Emacs22)
(when-available 'goto-address-prog-mode
  (add-hook 'find-file-hooks 'goto-address-prog-mode)
  )

;; Remove trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Settings for cc-mode
(add-hook 'cc-mode-hook
          (lambda ()
            (setq c-block-comment-prefix "*")
            )
          )

;; Eldoc for lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; Eldoc for C
(add-hook 'c-mode 'c-turn-on-eldoc-mode)

;;}}}



(provide 'init-unsorted-elisp)
;;; init-unsorted-elisp.el ends here
