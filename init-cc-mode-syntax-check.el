;;; init-cc-mode-syntax-check --- Syntax checker configuration for C/C++

;; Copyright (C) 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-07-18, 23:47 (CEST) Massimo Lauria>

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

(require 'flycheck)

;; This package contains setup for C/C++ compilers
(require 'init-cc-compiler)

;;; Code:


(flycheck-declare-checker c-gcc
  "A checker for C syntax which uses vanilla GCC."
  :command '("gcc"
             (option "--std=" init-cc-gcc-dialect)
             "-Wall" "-Wextra" "-fsyntax-only" source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error: \\(?4:.+\\)$" error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning: \\(?4:.+\\)$" warning))
  :modes 'c-mode)


(flycheck-declare-checker cplusplus-g++
  "A checker for C++ syntax which uses vanilla G++."
  :command '("g++"
             (option "--std=" init-cc-g++-dialect)
             "-Wall" "-Wextra" "-fsyntax-only" source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error: \\(?4:.+\\)$" error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning: \\(?4:.+\\)$" warning))
  :modes 'c++-mode)


(flycheck-declare-checker c-clang
  "A checker for C syntax which uses clang compiler."
  :command '("clang" 
             (option "--std=" init-cc-clang-dialect) 
             "-Wall" "-Wextra" 
             "-fno-color-diagnostics"
             "-fsyntax-only" source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error: \\(?4:.+\\)$" error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning: \\(?4:.+\\)$" warning))
  :modes 'c-mode)


(flycheck-declare-checker cplusplus-clang
  "A checker for C++ syntax (C++11) which uses clang compiler."
  :command '("clang++" 
             (option "--std=" init-cc-clang++-dialect) 
             (option "-stdlib=" flycheck-clang-stdlib-c++)
             "-Wall" "-Wextra"
             "-fno-color-diagnostics"
             "-fsyntax-only" source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error: \\(?4:.+\\)$" error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning: \\(?4:.+\\)$" warning))
  :modes 'c++-mode)


;; Add checkers to flycheck
(add-to-list 'flycheck-checkers 'c-gcc)
(add-to-list 'flycheck-checkers 'cplusplus-g++)
(add-to-list 'flycheck-checkers 'c-clang)
(add-to-list 'flycheck-checkers 'cplusplus-clang)

;; Use flycheck in c/c++ source
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)


(provide 'init-cc-mode-syntax-check)
;;; init-flycheck-clanguage.el ends here
