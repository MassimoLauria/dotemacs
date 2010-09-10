;;; init-autotype.el --- Automatic test insertion configuration

;; Copyright (C) 2010  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Keywords: convenience
;; Time-stamp: <2010-09-10, venerdì 21:03:00 (CEST) Massimo Lauria>

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


;;; Markers for template filling
(setq template-marker-for-name  ">>>NAME<<<" )
(setq template-marker-for-email ">>>EMAIL<<<")
(setq template-marker-for-point ">>>POINT<<<")
(setq template-marker-for-time  ">>>TIME<<<")
(setq template-time-format      "%Y-%02m-%02d, %A %02H:%02M:%02S (%Z)")  ;; Time format similar with time-stamp one.


;;; Auto Insert:

(require 'autoinsert)
(add-hook 'find-file-hook 'auto-insert)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory "~/config/emacs/templates/") ;; Template's files folder, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

;;; Auto Insert rules:

;(define-auto-insert "\.sh" "sh-template.sh") ; Example of a template file based rule.
(define-auto-insert 'sh-mode ["sh-template.sh" apply-template-marker]) ; Example of a template file based rule.


;;; YaSnippet

(require 'yasnippet)
(if (file-directory-p (concat default-elisp-path "/snippets/"))
    (add-to-list 'yas/root-directory (concat default-elisp-path "/snippets/")) ;; Snippet's file folder. 
  )
;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)
(setq yas/ignore-filenames-as-triggers t)


;;; Copyright update --- setup in custom.el

;;; Time-Stamp update --- setup in custom.el



;;; Utility functions for template filling.

(defun process-string-matches (mark-exp F) 
  "Find matches of a particular string, and process the matching
text with function F which return a string."
  (save-excursion
    (goto-char (point-min)) 
    ;; We search for matchings
    (while (search-forward mark-exp nil t) 
      ;; We save the restriction, because we are going to call narrow-to-region
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (let (
              (new-text (funcall F (buffer-string)))
              )
          (kill-region (point-min) (point-max))
          (insert new-text))))))


(defun apply-template-marker()
  "Fill template fields"
  (interactive)
  (process-string-matches ">>>NAME<<<"  '(lambda (x)  user-full-name))
  (process-string-matches ">>>EMAIL<<<" '(lambda (x) user-mail-address))
  (process-string-matches ">>>TIME<<<"  '(lambda (x) (format-time-string template-time-format (current-time))))
  ;; Move to point
  (goto-char (point-min))
  (if (search-forward ">>>POINT<<<" nil t) 
      (progn 
        (goto-char (match-beginning 0))
        (kill-region (match-beginning 0) (match-end 0))
        )
      )
)


(provide 'init-autotype)
;;; init-autotype.el ends here
