;;; init-highlight.el --- Setup forms of syntax highlight

;; Copyright (C) 2015, 2016  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Keywords:

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

;; Many of these syntax highlight packages are described in
;; http://www.wilfred.me.uk/blog/2014/09/27/the-definitive-guide-to-syntax-highlighting/
;;


;; nested delimiters of matching colors
(use-package rainbow-delimiters
  :ensure t
  :init (setq rainbow-delimiters-max-face-count 4)
  :commands rainbow-delimiters-mode)

;; a color for each identifier
(use-package rainbow-identifiers
  :ensure t
  :commands rainbow-identifiers-mode)

;; a background color for each nested block
(use-package highlight-blocks
  :ensure t
  :commands highlight-blocks-mode)

;; highlight occurrences of identifiers
(use-package highlight-symbol
  :ensure t
  :commands highlight-symbol-mode)

;; Highlight quoted symbols
(use-package highlight-quoted
  :ensure t
  :commands highlight-quoted-mode)

;; Highlight defined names
(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode)

(provide 'init-highlight)
