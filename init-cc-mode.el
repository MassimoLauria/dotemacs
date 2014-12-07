;;; init-cc-mode.el --- Setup for C/C++ programming modes

;; Copyright (C) 2012, 2013, 2014  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2014-12-07, 11:54 (CET) Massimo Lauria>

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
;;
;; Syntax check
;;
;; Syntax is checked by `flycheck-mode'. Of course it is necessary to
;; setup the correct C/C++ dialect, and various project dependant
;; paramenters. In the end the best is to use either `.dir-locals.el'
;; files or file local variables.
;;
;; Auto completion
;; 
;; We use `irony-mode' and `company-irony', assuming clang compiler is
;; installed. Apparently `irony-mode' does its best autodetect include
;; and library paths. Nevertheless `.dir-locals.el' files or file
;; local variables are your friends.
;;
;; `irony-mode' needs the `irony-server' binary installed in the
;; proper path (by default that is ".emacs.d/irony/bin/irony/server").
;; Compile it on MacOSX with XCode is an horrible experience, so I
;; included a documented scripts.


;; Basic setup for C/C++
(defun setup-cc-mode ()
  "Setup for C mode"
  (interactive)

  ;; editing facilities
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-SPC") 'ff-find-related-file)
  (setq c-block-comment-prefix "")
  (setq fill-column 70)

  ;; Minor modes
  (when (fboundp 'doxygen-mode)  (doxygen-mode  t))
  (when (fboundp 'flyspell-prog-mode) (flyspell-prog-mode)))

(add-hook 'c-mode-hook   'setup-cc-mode)
(add-hook 'c++-mode-hook 'setup-cc-mode)


;; Syntax checker
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)

;; Auto completion
(when (fboundp 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode))

(with-eval-after-load 'irony
  (require 'company-irony nil t)
  (add-to-list 'company-backends 'company-irony)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))


;; Testing with CPPunit
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             (list 'cppunit "\\(!!!FAILURES!!!\nTest Results:\nRun: [^\n]*\n\n\n\\)?\\([0-9]+\\)) test: \\([^(]+\\)(F) line: \\([0-9]+\\) \\([^ \n]+\\)" 5 4))



(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
