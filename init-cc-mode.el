;;; init-cc-mode.el --- Setup for C/C++ programming modes

;; Copyright (C) 2012  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2012-05-08, 15:07 (CEST) Massimo Lauria>

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

(defun setup-c-mode-completion ()
  "Add gtags and semantic sources for auto-completion."

  ;; Semantic completion (from CEDET)
  (when (boundp 'ac-source-semantic)
    (setq ac-sources (append '(ac-source-semantic) ac-sources))
    )
  ;; gtags completion (from gtags-mode)
  (when (boundp 'ac-source-gtags)
    (setq ac-sources (append '(ac-source-gtags) ac-sources))
    )
)



(defun setup-c-mode ()
  "Setup all for C/C++ modes"
  (local-set-key (kbd "RET") 'newline-and-indent)
  (when (fboundp 'semantic-mode)
    (semantic-mode t))
  (setup-c-mode-completion)
  )



;; install the hook
(add-hook 'c-mode-hook 'setup-c-mode)


(provide 'init-cc-mode)
;;; init-autotype.el ends here
