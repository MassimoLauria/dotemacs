;;; init-textmode.el --- Preferences for Text editing

;; Copyright (C) 2010, 2011, 2013, 2015, 2018, 2020, 2023  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Keywords: files, wp

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

;; Defaults for text editing are here. Most of these are customization
;; for the text-mode, like word wrapping and auto-filling.

;;; Code:



;; Text mode is default for files with no definite mode
(setq default-major-mode 'text-mode)


;; Sentences. Identifying and breaking up.
(setq sentence-end-double-space nil)
(add-hook 'fill-nobreak-predicate 'fill-single-char-nobreak-p)
(add-hook 'fill-nobreak-predicate 'fill-single-word-nobreak-p)


(defun MassimoLauria/text-mode-setup()
  "Initial setup of Text mode (common to all children modes)"
  (when-available 'goto-address-mode (goto-address-mode 1)))


(defun MassimoLauria/text-mode-nontex()
  "Setup of text modes which are not for LaTeX."
  (when (cl-notany (lambda (m) (eq m major-mode))
                '(LaTeX-mode TeX-mode latex-mode tex-mode))
    ;; Text formatting
    (set-default 'fill-column 70)
    (auto-fill-mode 1)
    (setq default-justification 'full)
    ;; Citation
    (turn-on-reftex)
    (set (make-local-variable 'reftex-cite-punctuation) '(", " " & " " et al."))
    (set (make-local-variable 'reftex-cite-format) "(%2a, %y)")))


(defun MassimoLauria/rst-mode-setup()
  "Initial setup of Text mode (common to all children modes)"
  (turn-on-reftex)
  (set (make-local-variable 'reftex-cite-punctuation) '(", " " & " " et al."))
  (set (make-local-variable 'reftex-cite-format) ".. [%l]\n   %a\n   %t\n   %j%b (%y)")
  (when-available 'goto-address-mode (goto-address-mode 1)))



;; Nice quotes
(defun MassimoLauria/replace-straight-quotes (pair action context)
  (when (eq action 'insert)
    (delete-char 1)
    (delete-char -1)
    (insert "“")
    (insert "”")
    (backward-char 1)))

;; Nice quotes and smartparens
(when (require 'smartparens nil t)
  (sp-local-pair 'text-mode "“" "”")  ;; add so you can jump back and forth and out and in the pair!
  (sp-local-pair 'text-mode "\"" nil :post-handlers '(MassimoLauria/replace-straight-quotes))
  (sp-local-tag  'text-mode "\"" "“" "”" :actions '(wrap)))


;; Reset the text-mode hook
(add-hook 'text-mode-hook 'MassimoLauria/text-mode-setup)
(add-hook 'text-mode-hook 'MassimoLauria/text-mode-nontex)
(add-hook 'rst-mode-hook 'MassimoLauria/rst-mode-setup)


(provide 'init-textmode)
;;; init-textmode.el ends here
