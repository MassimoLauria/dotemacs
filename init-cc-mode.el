;;; init-cc-mode.el --- Setup for C/C++ programming modes

;; Copyright (C) 2012  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2012-06-04, 01:52 (CEST) Massimo Lauria>

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

;; CC-mode


;; Auxiliary libraries dedicated to C/C++ support
(require 'auto-complete-clang "auto-complete-clang/auto-complete-clang.el" t)
(require 'c-eldoc nil t)



(defun setup-c-mode-completion ()
  "Add gtags/Clang/semantic sources for auto-completion."
  (interactive)
  (setq ac-sources nil)

  ;; Yasnippet
  (when (boundp 'ac-source-yasnippet)
    (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

  ;; Semantic (CEDET)
  (when (boundp 'ac-source-semantic)
    (setq ac-sources (append '(ac-source-semantic) ac-sources)))

  ;; Clang
  (when (boundp 'ac-source-clang)
    (setq ac-sources (append '(ac-source-clang) ac-sources)))

  ;; Gtags
  (when (boundp 'ac-source-gtags)
    (setq ac-sources (append '(ac-source-gtags) ac-sources)))

  ;; File
  (when (boundp 'ac-source-filename)
    (setq ac-sources (append '(ac-source-filename) ac-sources)))
)



(defun setup-c-mode ()
  "Setup all for C/C++ modes"
  (interactive)

  ;; editing facilities
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "M-SPC") 'ff-find-related-file)
  (setq c-block-comment-prefix "")
  (setq fill-column 70)

  ;; Minor modes
  (when (fboundp 'semantic-mode) (semantic-mode t))
  (when (fboundp 'doxygen-mode)  (doxygen-mode  t))
  (when (fboundp 'c-turn-on-eldoc-mode) (c-turn-on-eldoc-mode))

  (setup-c-mode-completion)
  )



;; install the hook
(add-hook 'c-mode-hook 'setup-c-mode)


(provide 'init-cc-mode)
;;; init-cc-mode.el ends here
