;;; init-cc-mode.el --- Setup for C/C++ programming modes

;; Copyright (C) 2012, 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-07-29, 17:11 (CEST) Massimo Lauria>

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

;; Optional dependency
(require 'gtags nil t)
(require 'c-eldoc nil t)

;; Load configuration for C/C++ compilers
(require 'init-cc-compiler)


;; Syntax checker 
(eval-after-load "flycheck"
  '(require 'init-cc-mode-syntax-check nil t))

;; test with cppunit
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist (list 'cppunit "\\(!!!FAILURES!!!\nTest Results:\nRun: [^\n]*\n\n\n\\)?\\([0-9]+\\)) test: \\([^(]+\\)(F) line: \\([0-9]+\\) \\([^ \n]+\\)" 5 4))


(require 'auto-complete-clang-async "emacs-clang-complete-async/auto-complete-clang-async.el" t)
(require 'auto-complete-clang "auto-complete-clang/auto-complete-clang.el" t)

(defvar clang-executable (executable-find "clang")
  "Executable compiler")
(defvar clang-include-path '("." "./src" "./include"))

(setq ac-clang-executable clang-executable)
(setq clang-completion-suppress-error 't)

(defun clang-include-discover (lang)
  "Discover the include path for Clang"
  (interactive)
  (let* (clang
         (pattern "echo ''| %s -v -x %s %s %s  -E - 2>&1 |grep '<...> search starts' -A 100|grep 'End of search list' -B 100 | grep '^ ' |grep -v Framework")
         (dialect "")
         (stdlib  "")
         cmdline)
    ;; find language dialect
    (when  (and (string= lang "c")
                init-cc-clang-dialect)
      (setq dialect (concat "--std=" init-cc-clang-dialect)))
    
    (when  (and (string= lang "c++")
                init-cc-clang++-dialect)
      (setq dialect (concat "--std=" init-cc-clang++-dialect)))
    
    ;; C++ standard library implementation
    (when  (and (string= lang "c++")
                init-cc-clang++-stdlib)
      (setq stdlib (concat "--stdlib=" init-cc-clang++-stdlib)))
    
    ;; Clang executable
    (cond ((string= lang "c") (setq clang (executable-find "clang")))
          ((string= lang "c++") (setq clang (executable-find "clang++")))
          )

    (setq cmdline (format pattern clang lang dialect stdlib))

    (split-string (shell-command-to-string cmdline))))

;; Auxiliary libraries dedicated to C/C++ support
(defun setup-clang (lang)
  "Setup clang variables."
  (interactive)

  ;; Header paths in command line
  (let ((tmp (clang-include-discover lang)))
    (setq ac-clang-flags
          (mapcar (lambda (item)(concat "-I" item))
                  (append tmp clang-include-path))))

  (setq dialect)
  (when  (and (string= lang "c")
              init-cc-clang-dialect)
    (add-to-list 'ac-clang-flags (concat "--std=" init-cc-clang-dialect)))
  
  (when  (and (string= lang "c++")
              init-cc-clang++-dialect)
    (add-to-list 'ac-clang-flags (concat "--std=" init-cc-clang++-dialect))))
 


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
  (when (executable-find "clang")
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
  (setup-clang "c")
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
  (setup-clang "c++")
  (setup-c-common-completion))


;; install the main hooks
(add-hook 'c-mode-hook   'setup-c-mode)
(add-hook 'c++-mode-hook 'setup-c++-mode)


(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
