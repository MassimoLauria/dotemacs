;;; init-autotype.el --- Automatic test insertion configuration

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015, 2016  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2016-11-11, 01:03 (CET) Massimo Lauria>

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


(setq template-time-format      "%Y-%02m-%02d, %A %02H:%02M (%Z)")  ;; Time format similar with time-stamp one.


;;; Snippets -----------------------------------------------------------------

(use-package yasnippet
  :diminish yas-minor-mode
  :pin gnu
  :commands (yas-expand yas-minor-mode yas-minor-mode-on yas-global-mode)
  :config
  (progn
    (add-to-list 'yas-snippet-dirs (concat default-elisp-path "/snippets/"))
    (setq yas-snippet-dirs
          (delete  "~/.emacs.d/snippets" yas-snippet-dirs))
    (yas-reload-all)))



;; Avoid automatic insertion of newlines at the end of a snippet recipe.
(add-hook 'snippet-mode-hook (lambda ()
                               (whitespace-mode)
                               (make-local-variable 'require-final-newline)
                               (setq require-final-newline nil)
                               ))

;;; Parenthesis support ----------------------------------------------------------

(use-package autopair
  :commands (autopair-mode autopair-global-mode)
  :diminish " ♊")


(use-package smartparens
  :commands (smartparens-mode smartparens-global-mode)
  :diminish " ♊"
  :config (progn 
            (require 'smartparens-config nil t)
            (require 'smartparens-latex nil t)
            (define-key smartparens-mode-map (kbd "C-M-i") 'sp-up-sexp)
            (define-key smartparens-mode-map (kbd "C-M-k") 'sp-down-sexp)
            (define-key smartparens-mode-map (kbd "C-M-j") 'sp-backward-sexp)
            (define-key smartparens-mode-map (kbd "C-M-l") 'sp-forward-sexp)
            (define-key smartparens-mode-map (kbd "C-M-u") 'sp-forward-barf-sexp)
            (define-key smartparens-mode-map (kbd "C-M-o") 'sp-forward-slurp-sexp)))




;; work around for smartparens
(unless (fboundp 'cua-replace-region)
  (defun cua-replace-region ()
    "Replace the active region with the character you type."
    (interactive)
    (let ((not-empty (and cua-delete-selection (cua-delete-region))))
      (unless (eq this-original-command this-command)
        (let ((overwrite-mode
               (and overwrite-mode
                    not-empty
                    (not (eq this-original-command 'self-insert-command)))))
          (cua--fallback))))))


;;; File templates (using `auto-insert' and `yasnippet') -----------------------

(require 'autoinsert)
(setq auto-insert-query nil)
(setq auto-insert-alist nil)  ;; Reset auto-insert rules.


(defun template-file ()
    "Create a file according to the appropriate template.

If the newly created filetype has more that one templates, then a
choice is offered."
    (interactive)
    (call-interactively 'find-file)
    (yas-minor-mode 1)
    (auto-insert))


(defun define-template-rule (rule template)
  "Setup a rule for the template application

RULE is either a mode symbol as `sh-mode' and TEMPLATE is either
a string or a list of strings. Each string must be a valid
`yasnippet' template for the mode."
  (cond ((stringp template)
         (define-auto-insert rule
           `(lambda ()
              (goto-char (point-min))
              (insert ,template)
              (call-interactively 'yas-expand))))
        ((functionp template)
         (define-auto-insert rule template))))


;; Templates are nothing else that snippets for the respective mode,
(define-template-rule 'sh-mode               "empty-template")
(define-template-rule 'makefile-bsdmake-mode "empty-template")
(define-template-rule 'makefile-gmake-mode   "empty-template")
(define-template-rule 'emacs-lisp-mode       "empty-template")
(define-template-rule "\\.c\\'"              "empty-c-template")
(define-template-rule "\\.h\\'"              "empty-h-template")
(define-template-rule "\\.\\(C\\|cc\\|cpp\\)\\'" "empty-cc-template-snippet")
(define-template-rule "\\.\\(H\\|hh\\|hpp\\)\\'" "empty-hh-template-snippet")
(define-template-rule 'rust-mode             "selfcontained")
(define-template-rule 'scheme-mode           "selfcontained")

(defun choose-latex-template ()
  "Query the user to choose a template for a new latex file."
  (interactive)
  (let ((type (ido-completing-read "Document type: "
                                   '("Letter"
                                     "Paper"
                                     "Note"
                                     "Slides"
                                     "Picture (Tikz)"
                                     "Empty"))))
    (cond ((string-equal "Letter" type)
           (insert "latex-letter-template"))
          ((string-equal "Paper" type)
           (insert "latex-paper-template"))
          ((string-equal "Note" type)
           (insert "latex-note-template"))
          ((string-equal "Slides" type)
           (insert "latex-slides-template"))
          ((string-equal "Picture (Tikz)" type)
           (insert "latex-pgfpic-template"))
          ))
  (yas-expand))

;; Function `choose-latex-template' must ber defined before calling
;; `define-template-rule'.
(define-template-rule "\\.tex\\'" 'choose-latex-template)


(provide 'init-autotype)
;;; init-autotype.el ends here
