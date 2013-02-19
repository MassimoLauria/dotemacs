;;; init-textmode.el --- Preferences for Text editing

;; Copyright (C) 2010, 2011  Massimo Lauria

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


;; Smart quotes setup. Like “this and that”.
(if (require 'typopunct "typopunct" t)
    (setq-default typopunct-buffer-language 'english)
  )


(defun my-setup-of-text-mode-common()
  "Initial setup of Text mode (common to all children modes)"
  (when-available 'flyspell-mode     (flyspell-mode     1))
  (when-available 'goto-address-mode (goto-address-mode 1))
  )


(defun my-setup-of-text-mode-nontex()
  "Setup of text modes which are not for LaTeX."
  (when (notany (lambda (m) (eq m major-mode))
                '(LaTeX-mode TeX-mode latex-mode tex-mode))
    ;; Text formatting
    (set-default 'fill-column 70)
    (auto-fill-mode 1)
    (set-default default-justification 'full)
    ;; Typography
    (when-available 'typopunct-mode (typopunct-mode 1))
    ))


;; Reset the text-mode hook
(setq text-mode-hook nil)
(add-hook 'text-mode-hook 'my-setup-of-text-mode-common)
(add-hook 'text-mode-hook 'my-setup-of-text-mode-nontex)


(provide 'init-textmode)
;;; init-textmode.el ends here
