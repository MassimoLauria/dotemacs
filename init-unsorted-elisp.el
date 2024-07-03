;;; init-unsorted-elisp.el --- Contains small chunks of elisp code in no particular order

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024  Massimo Lauria

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

(use-package graphviz-dot-mode
  :mode ("\\.dot" . graphviz-dot-mode))

(autoload 'muttrc-mode "muttrc-mode"
  "Mode to edit mutt configuration files")

;; Editing
(use-package expand-region
  :commands er/expand-region)


;; Undo-Tree, much better than default.
(use-package undo-tree
  :diminish undo-tree-mode
  :commands (global-undo-tree-mode
             undo-tree-mode
             undo-tree-undo
             undo-tree-redo)
  :bind (( "C-S-z" . undo-tree-redo ))
  :config (global-undo-tree-mode))



;; Make buffer names unique
(use-package uniquify
  :defer 2
  :init
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-separator ":"))



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




;; Customize Tramp

; sometimes remote shells are very bad and unusable by TRAMP
; (e.g. some busybox found in NAS) problem is that even if you install
; better commands TRAMP will stick to default ones because of the
; $PATH variable.  With the following setup, the remote path setting
; are taken in consideration.  This allows to fix a remote system to
; be accessed by TRAMP.
(eval-after-load "tramp"
  '(add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Two settings to speed up tramp. tramp-verbose defaults to 3. Only
;; set it higher for debugging.

(setq tramp-verbose 3)

;; As far as I know, I don't want emacs doing anything with version
;; control. I certaily don't want tramp running extra commands to
;; check on the version control status of files.

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))


;; Save histories across sessions. Not buffers
(savehist-mode 1)

;; Use full featured gdb GUI.
(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)
(add-hook 'gud-mode-hook #'(lambda ()
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
;; Comint keys
(use-package comint
  :defer t
  :init
  (add-hook 'comint-mode-hook
            #'(lambda()
                (local-set-key (kbd "M-n") 'comint-next-input)
                (local-set-key (kbd "M-p") 'comint-previous-input)
                (local-set-key [down] 'comint-next-matching-input-from-input)
                (local-set-key [up] 'comint-previous-matching-input-from-input)
                )))


;; Kill buffers with running processes
(setq kill-buffer-query-functions
      (remove 'process-kill-buffer-query-function kill-buffer-query-functions))

;; This is just nice
(use-package which-func
  :commands (which-func-mode))

;; Powershell facilities
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script
(autoload 'powershell "powershell" "Start a interactive shell of PowerShell." t)


(defun kill-fascists ()
  "This elisp code kills fascists"
  (interactive)
  (message "This elisp code kills fascists."))


(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))


;; Indirect narrow to region
(defun edit-function ()
      (interactive)
      (clone-indirect-buffer-other-window (which-function) 'pop-to-buffer)
      (mark-defun) ; works not only in emacs-lisp, but C++, Python, ...
      (narrow-to-region (mark) (point))
      (pop-mark)
      (other-window 1))

(defun edit-region (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defadvice fundamental-mode (after add-massimo-keyboard-mode ())
  (when (fboundp 'massimo-keyboard-mode)
    (massimo-keyboard-mode t)))

(ad-activate 'fundamental-mode)



(setq dired-use-ls-dired (memq system-type '(gnu/linux cygwin)))

;; Helm always at bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))




;; Eshell prompt
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))


(defun mxl/eshell-exit-status ()
  (let (status-color)
    (setq status-color (if (= eshell-last-command-status 0) "light green" "red"))
    (with-face (format "%d" eshell-last-command-status)
               :foreground status-color :weight 'bold)
    ))


(defun mxl-eshell-prompt ()
  (concat
   "\n"
   (with-face "(" :foreground "white" :weight 'bold)
   (with-face (format-time-string "%H:%M" (current-time))
              :foreground "green" :weight 'bold)
   (with-face ")" :foreground "white" :weight 'bold)
   (with-face "──" :foreground "#79a8ff" :weight 'bold)
   (let ((branch (git-prompt-branch-name)))
     (if branch
         (concat
          (with-face "[±" :foreground "white" :weight 'bold)
          (with-face (git-prompt-branch-name)
                     :foreground "light green" :weight 'bold)
          (with-face "]" :foreground "white" :weight 'bold)
          )
       (with-face "" :foreground "white" :weight 'bold)
       ))
   (with-face "──" :foreground "#79a8ff" :weight 'bold)
   (with-face "(" :foreground "white" :weight 'bold)
   (with-face (concat (abbreviate-file-name (eshell/pwd)) "")
              :foreground "#44f"
              :weight 'bold)
   (with-face ")" :foreground "white" :weight 'bold)
   "\n\n"
   (with-face "[" :foreground "white" :weight 'bold)
   (mxl/eshell-exit-status)
   (with-face "]" :foreground "white" :weight 'bold)
   " "
   (if (= (user-uid) 0)
       (with-face "#" :foreground "red")
     "$")
   " "))
(setq eshell-prompt-function 'mxl-eshell-prompt)
(setq eshell-prompt-regexp    "^[^#$\n]*[#$] ")
(defun git-prompt-branch-name ()
    "Get current git branch name"
    (let ((args '("symbolic-ref" "HEAD" "--short")))
      (with-temp-buffer
        (apply #'process-file "git" nil (list t nil) nil args)
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position))))))
(setq comint-prompt-read-only t)

(provide 'init-unsorted-elisp)
;;; init-unsorted-elisp.el ends here
