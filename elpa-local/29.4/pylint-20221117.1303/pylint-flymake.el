;;; pylint-flymake.el --- Configures Flymake for Python

;; Copyright (C) 2008 Alexandre Fayolle

;; Author: Alexandre Fayolle <alexandre.fayolle@logilab.fr>
;; Keywords: languages python
;; SPDX-License-Identifier: GPL-2.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 2 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Loading this library configures Flymake for Python.

;;; Code:

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (expand-file-name "epylint.py"
			    (file-name-directory
			     (locate-library "pylint-flymake")))
          (list local-file))))

(when (require 'flymake nil t)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(add-hook 'python-mode-hook 'flymake-mode)

(provide 'pylint-flymake)

;;; pylint-flymake.el ends here
