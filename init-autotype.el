;;; init-autotype.el --- Automatic test insertion configuration

;; Copyright (C) 2010  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Keywords: convenience
;; Time-stamp: <2010-09-10, venerdì 12:26:59 (CEST) Massimo Lauria>

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

;; This is the part of my init files devoted to automatic text
;; insertion. It configures facilities like update of copyright notes
;; and time-stamps, Yasnippet and auto-insert.


;;; Auto Insert:

(require 'autoinsert)
(add-hook 'find-file-hook 'auto-insert)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/config/emacs/templates/") ;; Template's files folder, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

;;; Auto Insert rules:

;(define-auto-insert "\.sh" "sh-template.sh") ; Example of a template file based rule.
(define-auto-insert 'sh-mode "sh-template.sh") ; Example of a template file based rule.


;;; YaSnippet

(require 'yasnippet)
(setq yas/root-directory 
      (list "/usr/share/emacs/site-lisp/yasnippet/snippets/"
            (concat default-elisp-path "/snippets/")))   ;; Snippet's file folder. 
;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)
(setq yas/ignore-filenames-as-triggers t)


;;; Copyright update --- setup in custom.el

;;; Time-Stamp update --- setup in custom.el



(provide 'init-autotype)
;;; init-autotype.el ends here
