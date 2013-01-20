;;; init-cc-mode.el --- Setup for C/C++ programming modes

;; Copyright (C) 2012, 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-01-19, 20:25 (CET) Massimo Lauria>

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

;; This is my setup for C/C++ code.

;; Dependency
(require 'c-eldoc nil t)
(require 'flymake-clang-c)
(require 'flymake-clang-c++)
(require 'auto-complete-clang-async "emacs-clang-complete-async/auto-complete-clang-async.el" t)
(require 'auto-complete-clang "auto-complete-clang/auto-complete-clang.el" t)

(defcustom clang-executable "clang-mp-3.2"
  "Executable compiler")
(defvar clang-include-path '("." "./src" "./include"))

(setq ac-clang-executable clang-executable)
(setq clang-completion-suppress-error 't)

(defun clang-include-discover (lang dialect)
  "Discover the include path for Clang"
  (interactive)
  (let ((pattern "echo ''| clang -v -x %s --std=%s -stdlib=libc++  -E - 2>&1 |grep '<...> search starts' -A 100|grep 'End of search list' -B 100 | grep '^ ' |grep -v Framework")
        cmdline)
    (setq cmdline (format pattern lang dialect ))
    (split-string (shell-command-to-string cmdline))))

;; Auxiliary libraries dedicated to C/C++ support
(defun setup-clang (lang dialect )
  "Setup clang variables."
  (interactive)
  ;; Load header paths
  (let ((tmp (clang-include-discover lang dialect)))
    ;; Initialize clang command line
    (setq ac-clang-flags
          (mapcar (lambda (item)(concat "-I" item))
                  (append tmp clang-include-path))))
  (add-to-list 'ac-clang-flags (format "--std=%s" dialect))
  )



(defun ac-clang-async-setup ()
  "Setup C/C++ completion using clang, async server"
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (when (executable-find ac-clang-complete-executable)
    (add-to-list 'ac-sources 'ac-source-clang-async)
    (ac-clang-launch-completion-process)))

(defun ac-clang-setup ()
  "Setup C/C++ completion using clang"
  (add-to-list 'ac-sources 'ac-source-clang))

(defun setup-c-common-completion ()
  "Add gtags/Clang/semantic sources for auto-completion."
  (interactive)
  (unless (boundp 'ac-sources) (setq 'ac-sources nil))
  ;; Yasnippet
  (when (boundp 'ac-source-yasnippet)
    (add-to-list 'ac-sources 'ac-source-yasnippet))
  ;; Clang
  (when (executable-find clang-executable)
    (ac-clang-setup))
  ;; Gtags
  (when (boundp 'ac-source-gtags)
    (add-to-list 'ac-sources 'ac-source-gtags))
  )



(defun setup-c-mode ()
  "Setup for C mode"
  (interactive)

  ;; editing facilities
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-SPC") 'ff-find-related-file)
  (setq c-block-comment-prefix "")
  (setq fill-column 70)

  ;; Minor modes
  (when (fboundp 'doxygen-mode)  (doxygen-mode  t))
  (when (fboundp 'c-turn-on-eldoc-mode) (c-turn-on-eldoc-mode))
  (when (fboundp 'flyspell-prog-mode) (flyspell-prog-mode))

  ;; Clang
  (setup-clang "c" "c99")
  (setup-c-common-completion))

(defun setup-c++-mode ()
  "My setup for C++ mode"
  (interactive)

  ;; editing facilities
  (local-set-key (kbd "RET")   'newline-and-indent)
  (local-set-key (kbd "M-SPC") 'ff-find-related-file)
  (setq fill-column 70)

  ;; Minor modes
  (when (fboundp 'doxygen-mode)  (doxygen-mode  t))
  (when (fboundp 'c-turn-on-eldoc-mode) (c-turn-on-eldoc-mode))
  (when (fboundp 'flyspell-prog-mode) (flyspell-prog-mode))

  ;; Clang
  (setup-clang "c++" "c++11")
  (setup-c-common-completion))


;; install the main hooks
(add-hook 'c-mode-hook   'setup-c-mode)
(add-hook 'c++-mode-hook 'setup-c++-mode)

;; clang is very good for syntax check
(when (executable-find "clang")
  (add-hook 'c-mode-hook 'flymake-clang-c-load)
  (add-hook 'c++-mode-hook 'flymake-clang-c++-load))


(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
