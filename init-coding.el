;;; init-coding.el --- Configuration of the utf-8 encoding. -*- coding: utf-8 -*-

;; Copyright (C) 2011  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Keywords: i18n

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

;;

;;; Code:
;;;;;;;;;;;;;;;;;;;;

;; set up unicode and utf-8 coding for buffers.
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; Utf-8 encoding for clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(set-clipboard-coding-system 'utf-8)


;; Default input method is TeX, but it is not active by default
(setq default-input-method 'TeX)
(set-input-method nil)

(provide 'init-coding)
;;; init-coding.el ends here
