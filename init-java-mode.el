;;; init-java-mode.el --- Setup for Java programming modes

;; Copyright (C) 2012  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2012-06-02, 13:56 (CEST) Massimo Lauria>

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

;; This is my setup for Java code.



;; Auxiliary libraries for Eclipse/Eclim support. Eclipse is a
;; powerful but very slow IDE for Java. It has a lot of functions in
;; order to support Java programming. It is possible to use such
;; features in Emacs using Eclim, a plugin for Eclipse which export
;; said functionality with a server.
(add-to-list 'load-path (concat default-elisp-3rdparties "/eclim/"))
(add-to-list 'load-path (concat default-elisp-3rdparties "/eclim/vendor"))


(defun local-eclim-executable-find ()
  (let ((file "~/.eclipse"))
    (and (file-exists-p
          (setq file (expand-file-name file)))
         (setq file (car (last (directory-files file t "^org.eclipse.platform_"))))
         (file-exists-p
          (setq file (expand-file-name "plugins" file)))
         (setq file (car (last (directory-files file t "^org.eclim_"))))
         (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
         file)))

;; Load Eclim
(eval-after-load "eclim"
  '(progn
     (setq eclim-auto-save t)
     (global-eclim-mode t)
     (setq eclim-executable
	   (or
	    (executable-find "eclim")
	    (eclim-executable-find)
	    (local-eclim-executable-find)))))

(defun setup-java-mode-completion ()
  "Add auto-completion support for Java."

  ;; Eclipse/Eclim completion
  (require 'eclim nil t)
  (when (require 'ac-emacs-eclim-source nil t)
    (add-hook 'eclim-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-emacs-eclim)))))



(defun setup-java-mode ()
  "Setup for Java modes"

  ;; Help at cursor
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  (setup-java-mode-completion)
  )



;; install the hook
(add-hook 'java-mode-hook 'setup-java-mode)


(provide 'init-java-mode)
;;; init-java-mode.el ends here
