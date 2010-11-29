;;; init-textmode.el --- Preferences for Text editing

;; Copyright (C) 2010  Massimo Lauria

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

;; To make Text mode default for *scratch* uncomment below
;;(setq initial-major-mode 'text-mode)

;; Smart quotes setup. Like “this and that”.
(if (require 'typopunct "typepunct" t)
    (setq-default typopunct-buffer-language 'english)
  )

;; Setup of text-mode
(setq text-mode-hook
      '(lambda nil
         (if prefs-activate-smallscreen
             (setq fill-column 70)
           (setq fill-column 80)
           )

         (auto-fill-mode)                   ; Hard word wrapping...
         (setq default-justification 'full) ; ... with full justification

         ;;(orgtbl-mode 1)  ; conflicts with autopair mode.
         (flyspell-mode 1)  ; annoying spell checking
         (when-available 'goto-address-mode (goto-address-mode)) ; Find urls/emails in text and press (C-c RET) to click them.
         ;;(typopunct-mode)
         )
      )



(provide 'init-textmode)
;;; init-textmode.el ends here
