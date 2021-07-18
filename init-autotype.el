;;; init-autotype.el --- setup template and completion

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2021  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2021-07-18, 15:02 (CEST) Massimo Lauria>

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

;;; Parenthesis support ----------------------------------------------------------

(use-package autopair
  :commands (autopair-mode autopair-global-mode)
  :diminish "")


(use-package smartparens
  :commands (smartparens-mode smartparens-global-mode)
  :diminish ""
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

(provide 'init-autotype)
;;; init-autotype.el ends here
