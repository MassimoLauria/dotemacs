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
;; All urls/mails are clickable in comments and strings
(when-available 'goto-address-prog-mode
  (add-hook 'find-file-hooks 'goto-address-prog-mode))


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


(defvar helm-sdcv-word-cache nil
  "Caches all words from the wordfile.")

(defvar helm-sdcv-word-file "~/personal/dictionaries/wordlists/english_and_italian.txt"
  "File containing all words to be looked up.")

;; All words from the wordlist (using cache)
(defun helm-sdcv-list-of-words ()
  (or helm-sdcv-word-cache
      (setq helm-sdcv-word-cache
            (with-temp-buffer
              (insert-file helm-sdcv-word-file)
              (split-string (buffer-string) "\n")))))

(setq helm-sdcv-source
      '((name . "English and Italian expression")
        (candidates . helm-sdcv-list-of-words)
        (action . (lambda (candidate)
                    (sdcv-search-input candidate)))))

(defun helm-sdcv (&optional word)
  (interactive)
  (require 'sdcv)
  (helm
   :prompt "Lookup in dictionary: "
   :input (or word (word-at-point))
   :fuzzy-math t
   :buffer "*SDCV with helm"
   :sources '(helm-sdcv-source)))

(global-set-key (kbd "C-c d") 'helm-sdcv)


;; Helm always at bottom
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

;; The following setup is due to the answer to my question on Emacs
;; Stackexchange, by user d125q.
;;
;; https://emacs.stackexchange.com/questions/73414/how-to-toggle-helm-ff-skip-boring-files-during-file-navigation-with-helm-find/73418#73418

(defun helm-ff-toggle-skip-boring-files ()
  (with-helm-buffer
    (setq helm-ff-skip-boring-files (not helm-ff-skip-boring-files))
    (let* ((cand (helm-get-selection))
           (target (if helm-ff-transformer-show-only-basename
                       (helm-basename cand)
                     cand)))
      (if helm-ff-skip-boring-files
          (message "Hide hidden files")
        (message "Show hidden files"))
      (helm-force-update
       (format helm-ff-last-expanded-candidate-regexp (regexp-quote target))))))

(defun helm-ff-run-toggle-skip-boring-files ()
  (interactive)
  (with-helm-alive-p
    (unless (helm-empty-source-p)
      (helm-ff-toggle-skip-boring-files))))


(use-package helm
  :bind (( "C-x C-b" . helm-mini)
         ( "C-x b"   . helm-mini)
         ( "C-x C-r" . helm-mini)
         ( "C-x C-f" . helm-find-files)
         ( "M-x"     . helm-M-x)
         ( "M-y"     . helm-show-kill-ring)
         ( "M-`"     . helm-resume)
         :map helm-map
         ( "M-`"  . helm-keyboard-quit)
         ( "M-i"  . helm-previous-line)
         ( "M-k"  . helm-next-line)
         ( "M-j"  . backward-kill-word)
         ( "M-l"  . helm-maybe-exit-minibuffer)
         ( "M-u"  . helm-previous-source)
         ( "M-o"  . helm-next-source)
         ( "M-;"  . helm-select-action)
         :map helm-find-files-map
         ( "M-`"  . helm-keyboard-quit)
         ( "M-i"  . helm-previous-line)
         ( "M-k"  . helm-next-line)
         ( "M-l"  . helm-ff-RET)
         ( "M-j"  . helm-find-files-up-one-level)
         ( "M-u"  . helm-previous-source)
         ( "M-o"  . helm-next-source)
         ( "M-;"  . helm-select-action)
         ( "M-."  . helm-ff-run-toggle-skip-boring-files)
         :map helm-generic-files-map
         ( "M-`"  . helm-keyboard-quit)
         ( "M-i"  . helm-previous-line)
         ( "M-k"  . helm-next-line)
         ( "M-j"  . backward-kill-word)
         ( "M-l"  . helm-maybe-exit-minibuffer)
         ( "M-u"  . helm-previous-source)
         ( "M-o"  . helm-next-source)
         ( "M-;"  . helm-select-action)
         :map helm-read-file-map
         ( "M-`"  . helm-keyboard-quit)
         ( "M-i"  . helm-previous-line)
         ( "M-k"  . helm-next-line)
         ( "M-j"  . backward-kill-word)
         ( "M-l"  . helm-maybe-exit-minibuffer)
         ( "M-u"  . helm-previous-source)
         ( "M-o"  . helm-next-source)
         ( "M-;"  . helm-select-action)
         :map helm-buffer-map
         ( "M-`"  . helm-keyboard-quit)
         ( "M-i"  . helm-previous-line)
         ( "M-k"  . helm-next-line)
         ( "M-j"  . backward-kill-word)
         ( "M-l"  . helm-maybe-exit-minibuffer)
         ( "M-u"  . helm-previous-source)
         ( "M-o"  . helm-next-source))
         ( "M-;"  . helm-select-action)
  :diminish helm-mode
  :init
  (setq helm-display-header-line t)
  (setq helm-ff-skip-boring-files t)
  :config
  (setq helm-move-to-line-cycle-in-source nil)
  (helm-mode)
  (helm-ff-icon-mode)
  (put 'helm-ff-run-toggle-skip-boring-files 'helm-only t)
    ;; Hidden files are not shown in helm-find-file
  (customize-set-variable 'helm-boring-file-regexp-list
                          (cons "^\\..+" helm-boring-file-regexp-list)))

(use-package helm-rg                    ;
  :commands (helm-rg)
  :init
  (defalias 'rg 'helm-rg "RipGrep in the current folder"))



;; Fill/unfill paragraph
(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package projectile
  :init
  (setq compilation-read-command nil)
  (setq projectile-current-project-on-switch t)
  (setq projectile-per-project-compilation-buffer t)
  :bind (:map projectile-mode-map
              ("<f9>"  . projectile-compile-project)
              ("M-<f9>"  . projectile-test-project)
              ("<f10>"   . projectile-run-project)
              ("<f11>" . previous-error)
              ("<f12>" . next-error))
  :config
  (projectile-register-project-type 'latexmk '(".latexmkrc")
                                    :project-file ".latexmkrc"
                                    :compile "latexmk"
                                    :run "latexmk -pvc")

  (projectile-register-project-type 'go projectile-go-project-test-function
                                    :compile "go build"
                                    :run "go run ."
                                    :test "go test ./..."
                                    :test-suffix "_test"))


;; Setup PDF tools
(use-package pdf-tools
  :mode  ("\\.pdf" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :init
  (setq pdf-view-use-unicode-ligther nil)  ;; make loading faster
  :config
  (delete 'pdf-outline-minor-mode pdf-tools-enabled-modes)
  (pdf-tools-install :no-query)
  (pdf-loader-install t)
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (auto-revert-mode 1)
                                  (beacon-mode -1)
                                  ))
  (setq pdf-view-use-scaling t)
  (setq revert-without-query '(".pdf"))
  (setq-default pdf-view-display-size 'fit-page)
  ;; Highlights do not open the annotation window when created
  (setq pdf-annot-activate-created-annotations nil)
  (defun mxl-pdf-annot-add-text-annotation ()
    "Add text annotation but forces activation which is off
by default."
    (interactive)
    (let ((pdf-annot-activate-created-annotations t))
      (call-interactively 'pdf-annot-add-text-annotation)))
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)
  ;; use isearch instead of swiper
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; keyboard shortcut for zooming
  (define-key pdf-view-mode-map (kbd "+") 'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "=") 'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "-") 'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "0") 'pdf-view-scale-reset)
  (define-key pdf-view-mode-map (kbd "W") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "H") 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map (kbd "P") 'pdf-view-fit-page-to-window)
  ;; Open in apps
  (define-key pdf-view-mode-map (kbd "O") 'open-in-external-app)
  ;; Movements
  (define-key pdf-view-mode-map (kbd "o") 'pdf-history-backward)
  (define-key pdf-view-mode-map (kbd "u") 'pdf-history-forward)
  (define-key pdf-view-mode-map (kbd "i") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-previous-page)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "M-o") 'pdf-history-backward)
  (define-key pdf-view-mode-map (kbd "M-u") 'pdf-history-forward)
  (define-key pdf-view-mode-map (kbd "M-i") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "M-k") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "M-j") 'pdf-view-previous-page)
  (define-key pdf-view-mode-map (kbd "M-l") 'pdf-view-next-page)
  ;; Outline
  (define-key pdf-view-mode-map (kbd "g") 'pdf-outline)
  (define-key pdf-outline-buffer-mode-map (kbd "g") 'quit-window)
  (define-key pdf-outline-buffer-mode-map (kbd "RET") 'pdf-outline-display-link) ; stay in the outline buffer

  (defun mxl-pdf-open-in-xournal()
    "Open the current PDF in Xournal, for editing"
    (interactive)
    (start-process "open-pdf-in-xournal" nil "xournalpp" (buffer-file-name)))
  (define-key pdf-view-mode-map (kbd "X") 'mxl-pdf-open-in-xournal)
  ;; Margin removal
  (defun mxl-pdf-view-toggle-crop()
    "Crop/Uncrop according to the bounding box"
    (interactive)
    (if (pdf-view-current-slice)
        (pdf-view-reset-slice)
      (pdf-view-set-slice-from-bounding-box)))
  (define-key pdf-view-mode-map (kbd "c") 'mxl-pdf-view-toggle-crop)
  ;; keyboard shortcuts for annotations
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "u") 'pdf-annot-add-underline-markup-annotation)
  (define-key pdf-view-mode-map (kbd "s") 'pdf-annot-add-strikeout-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'mxl-pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "L") 'pdf-annot-list-annotations)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))


;; Eshell prompt
(defun with-face (str &rest face-plist)
  (propertize str 'face face-plist))

(defun mxl-eshell-prompt ()
  (concat
   "\n"
   (with-face "┌─" :foreground "#79a8ff" :weight 'bold)
   (with-face "─(" :foreground "white" :weight 'bold)
   (with-face (format-time-string "%H:%M" (current-time))
              :foreground "green" :weight 'bold)
   (with-face ")─" :foreground "white" :weight 'bold)
   (with-face "──" :foreground "#79a8ff" :weight 'bold)
   (let ((branch (git-prompt-branch-name)))
     (if branch
         (concat
          (with-face "─[±" :foreground "white" :weight 'bold)
          (with-face (git-prompt-branch-name)
                     :foreground "light green" :weight 'bold)
          (with-face "]─" :foreground "white" :weight 'bold)
          )
       (with-face "" :foreground "white" :weight 'bold)
       ))
   (with-face "──" :foreground "#79a8ff" :weight 'bold)
   (with-face "─(" :foreground "white" :weight 'bold)
   (with-face (concat (abbreviate-file-name (eshell/pwd)) "")
              :foreground "#44f"
              :weight 'bold)
   (with-face ")" :foreground "white" :weight 'bold)
   "\n"
   (with-face "│\n└─" :foreground "#79a8ff" :weight 'bold)
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
