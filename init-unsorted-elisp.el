;;; init-unsorted-elisp.el --- Contains small chunks of elisp code in no particular order

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018  Massimo Lauria

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




;; Auto-mode
(setq auto-mode-alist (cons '("\\.zsh" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.yasnippet" . snippet-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.snippet" . snippet-mode) auto-mode-alist))

;; Command line editing
(setq auto-mode-alist (cons '("zshec[0-9]*" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("bash-fc-[0-9]*" . sh-mode) auto-mode-alist))





(use-package graphviz-dot-mode
  :mode ("\\.dot" . graphviz-dot-mode))

(autoload 'muttrc-mode "muttrc-mode"
  "Mode to edit mutt configuration files")


(use-package semantic
  :commands semantic-mode)


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



;; Add a message to yank-pop
(defun kill-ring-counters ()
  "Output a cons (pos . len) whenre `len' is the number of the elements
in the kill-ring and `pos' is the position current-kill"
  (interactive)
  (let ((len (length kill-ring))
        (pos (length kill-ring-yank-pointer)))
    (cons pos len)))

(defadvice yank-pop (before print-kill-ring-counter ())
  "Show in the minibuffer what is the current element of the kill-ring"
  (let ((pl (kill-ring-counters)))
    (message (format "(Yank Pop) kill-ring position %d/%d" (car pl) (cdr pl)))))

(ad-activate 'yank-pop)

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


;;; Flymake setup --------

;; Overwrite flymake-display-warning to suppress annoying dialog boxs
(defun flymake-display-warning (warning)
  "Display a warning to the user, using minibuffer"
  (message warning))

;; All urls/mails are clickable in comments and strings (Not present in Emacs22)
(when-available 'goto-address-prog-mode
  (add-hook 'find-file-hooks 'goto-address-prog-mode)
  )


;; Eldoc for lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;;}}}


;; CCLookup documentation of C/C++

;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
(setq cclookup-dir (concat default-elisp-3rdparties "/cclookup"))
(add-to-list 'load-path cclookup-dir)
;; load pylookup when compile time
(eval-when-compile (require-maybe 'cclookup))

;; set executable file and db file
(setq cclookup-program (concat cclookup-dir "/cclookup.py"))
(setq cclookup-db-file "~/.emacs.d/cclookup.db")
(if (not (file-exists-p cclookup-db-file))
    (message "CClookup database not yet initialized")
    )

;; to speedup, just load it on demand
(autoload 'cclookup-lookup "cclookup"
  "Lookup SEARCH-TERM in the C++ reference indexes." t)
(autoload 'cclookup-update "cclookup"
  "Run cclookup-update and create the database at `cclookup-db-file'." t)


;; Comint keys
(use-package comint
  :defer t
  :init 
  (add-hook 'comint-mode-hook
            '(lambda()
               (local-set-key (kbd "M-n") 'comint-next-input)
               (local-set-key (kbd "M-p") 'comint-previous-input)
               (local-set-key [down] 'comint-next-matching-input-from-input)
               (local-set-key [up] 'comint-previous-matching-input-from-input)
               )))



;; Tags managements ----------------------------------------
;; Fix the keybindings for Semantic, Gtags and Etags

;; M-. is for finding the tag
;; M-, is for popping the tag stack
;; The usual binding for popping is M-* which is disabled here.


(defvar semantic-tags-location-ring (make-ring 20))

(defun semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump   
save the pointer marker if tag is found. 

Code from 
http://sourceforge.net/mailarchive/message.php?msg_id=27414242"
  (interactive "d")
  (condition-case err
      (progn                            
        (ring-insert semantic-tags-location-ring (point-marker))  
        (semantic-ia-fast-jump point))
    (error
     ;;if not found remove the tag saved in the ring  
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun semantic-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition

Code from 
http://sourceforge.net/mailarchive/message.php?msg_id=27414242"   
  (interactive)                                                    
  (if (ring-empty-p semantic-tags-location-ring)                   
      (message "%s" "No more tags available")                      
    (let* ((marker (ring-remove semantic-tags-location-ring 0))    
              (buff (marker-buffer marker))                        
                 (pos (marker-position marker)))                   
      (if (not buff)                                               
            (message "Buffer has been deleted")                    
        (switch-to-buffer buff)                                    
        (goto-char pos))                                           
      (set-marker marker nil nil))))

(eval-after-load "semantic"
  '(progn
     (define-key semantic-mode-map (kbd "M-.") 'semantic-goto-definition)
     (define-key semantic-mode-map (kbd "M-,") 'semantic-pop-tag-mark)
     ))


;; Gtags keys
(eval-after-load 'gtags-mode
          '(progn
             (define-key gtags-mode-map (kbd "M-.") 'gtags-find-tag)
             (define-key gtags-mode-map (kbd "M-,") 'gtags-pop-stack)
             (define-key gtags-mode-map (kbd "M-*") 'nil)
             ))

;; Elisp navigation
(eval-after-load "elisp-slime-nav-autoloads.el"
          '(progn
             (add-hook 'lisp-interaction-mode-hook 'elisp-slime-nav-mode)
             (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
             ))


;; Kill buffers with running processes
(setq kill-buffer-query-functions
      (remove 'process-kill-buffer-query-function kill-buffer-query-functions))

;; This is just nice
(use-package which-func
  :commands (which-func-mode))


;; Trailining whitespace removal
(defvar do-delete-whitespace t
  "Local variable to decide whether deleting trailing whitespaces
  when saving.")

(defun delete-trailing-whitespace--conditional ()
  "Call `delete-trailing-whitespace' unless `do-delete-whitespace' in non `nil'"
  (when do-delete-whitespace
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-trailing-whitespace--conditional)

;; Powershell facilities
(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script
(autoload 'powershell "powershell" "Start a interactive shell of PowerShell." t)


(defun kill-fascists ()
  "This elisp code kills fascists"
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


;; SDCV is a command line client for StarDict dictionaries.
;; it is handy to have support of OFFLINE dictionaries.
(autoload 'sdcv-search      "sdcv-mode" "Prompt for a word to search through sdcv." t)
(autoload 'sdcv-search-word "sdcv-mode" "Search WORD through the command-line tool sdcv." nil)
(defun sdcv-query-word (&optional word)
  "Lookup the WORD using the `sdcv' dictionary program."
  (interactive)
  (sdcv-search-word (or word (current-word))))


(use-package calibre-mode
  :commands calibre-find
  :config
  (setq calibre-root-dir (expand-file-name "~/Documents/Calibre"))
  (setq calibre-db (concat calibre-root-dir "/metadata.db"))
  (setq sql-sqlite-program "/usr/bin/sqlite3")
  )

(use-package ivy
  :diminish (ivy-mode)
  :config
  (ivy-mode)
  (setq counsel-find-file-ignore-regexp "\\`\\.")
  (setq ivy-use-virtual-buffers t))


(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map)
  :config
  (my-ivy-setup))


(defun my-ivy-setup ()
  (define-key ivy-minibuffer-map (kbd "<left>") 'counsel-up-directory)
  (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-i") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "M-k") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-j") 'counsel-up-directory)
  (define-key ivy-minibuffer-map (kbd "M-l") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "M-u") 'ivy-previous-history-element)
  (define-key ivy-minibuffer-map (kbd "M-o") 'ivy-next-history-element))


(provide 'init-unsorted-elisp)
;;; init-unsorted-elisp.el ends here
