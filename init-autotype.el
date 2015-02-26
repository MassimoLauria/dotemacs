;;; init-autotype.el --- Automatic test insertion configuration

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2015-02-26, 11:54 (CET) Massimo Lauria>

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


;;; Auto Insert: ----------------------------------------------------------------------

(require 'autoinsert)
(add-hook 'find-file-hook 'auto-insert)  ;;; Adds hook to find-files-hook
(setq auto-insert-query nil)
(setq auto-insert-alist nil)  ;; Reset auto-insert rules.
;; Auto Insert rules:

;; Use yasnippet to implement templates.
(defun apply-yasnippet-function (template)
  "It produces a function which insert a yasnipper template"
  `(lambda ()
     (goto-char (point-min))
     (insert ,template)
     (call-interactively 'yas-expand))
  )

(define-auto-insert 'sh-mode (apply-yasnippet-function "empty-template"))
(define-auto-insert 'makefile-bsdmake-mode (apply-yasnippet-function "empty-template"))
(define-auto-insert 'makefile-gmake-mode (apply-yasnippet-function "empty-template"))
(define-auto-insert 'emacs-lisp-mode (apply-yasnippet-function "empty-template"))
(define-auto-insert "\\.c\\'" (apply-yasnippet-function "empty-c-template"))
(define-auto-insert "\\.h\\'" (apply-yasnippet-function "empty-h-template"))
(define-auto-insert "\\.\\(C\\|cc\\|cpp\\)\\'" (apply-yasnippet-function "empty-cc-template-x"))
(define-auto-insert "\\.\\(H\\|hh\\|hpp\\)\\'" (apply-yasnippet-function "empty-hh-template-x"))

;;; Snippets -----------------------------------------------------------------

(use-package yasnippet
  :ensure t
  :defer t
  :diminish " ⓨ"
  :pin melpa-stable
  :idle (yas-global-mode)
  :config '(progn
             (add-to-list 'yas-snippet-dirs (concat default-elisp-path "/snippets/"))
             (delete  "~/.emacs.d/snippets" yas-snippet-dirs)))


;; Avoid automatic insertion of newlines at the end of a snippet recipe.
(add-hook 'snippet-mode-hook (lambda ()
                               (whitespace-mode)
                               (make-local-variable 'require-final-newline)
                               (setq require-final-newline nil)
                               ))

;;; Parenthesis support ----------------------------------------------------------

(use-package autopair
  :demand t               ;; FIXME: something breaks if package not loaded.
  :commands (autopair-mode autopair-global-mode)
  :diminish " ♊")


(use-package smartparens
  :commands (smartparents-mode smartparens-global-mode)
  :diminish " ♊"
  :config '(progn 
             (require 'smartparens-config nil t)
             (require 'smartparens-latex nil t)))


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

;; Project templates (using `skeletor' package)

(defalias 'template-project-create        'skeletor-create-project)
(defalias 'template-project-create-at-dir 'skeletor-create-project-at)

(use-package skeletor
  :init (setq skeletor-user-directory (concat (getenv "HOME") "/lavori/templates/")
              skeletor-project-directory (concat (getenv "HOME") "/lavori/hacks/"))
  :commands (skeletor-create-project skeletor-create-project-at))


(provide 'init-autotype)
;;; init-autotype.el ends here
