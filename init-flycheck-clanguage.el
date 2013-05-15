;;; init-flycheck-clanguage --- Syntax checker configuration for C/C++

;; Copyright (C) 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-05-15, 20:44 (CEST) Massimo Lauria>

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


;;; Code:


(flycheck-def-option-var flycheck-gcc-standard nil c-gcc
  "The language standard of C used for checking with GCC.

When nil, use the default standard used by g++ compiler.  When
set to a string, pass the string to \"--std=\" option of g++."
  :type '(choice (const :tag "Compiler default" nil)
                 (const :tag "C'90 (ANSI)" "c90")
                 (const :tag "C'99" "c99")
                 (const :tag "C'1x" "c1x")
                 (const :tag "C'90 with GNU extensions"  "gnu90")
                 (const :tag "C'99 with GNU extensions"  "gnu99")
                 (const :tag "C'1x with GNU extensions"  "gnu1x"))
  :safe t)

(flycheck-declare-checker c-gcc
  "A checker for C syntax which uses vanilla GCC."
  :command '("gcc"
             (option "-std=" flycheck-gcc-standard)
             "-Wall" "-Wextra" "-fsyntax-only" source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error: \\(?4:.+\\)$" error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning: \\(?4:.+\\)$" warning))
  :modes 'c-mode)

(add-to-list 'flycheck-checkers 'c-gcc)


(flycheck-def-option-var flycheck-g++-standard nil cplusplus-g++
  "The language standard of C++ used for checking with G++.

When nil, use the default standard used by g++ compiler.  When
set to a string, pass the string to \"--std=\" option of g++."
  :type '(choice (const :tag "Compiler default" nil)
                 (const :tag "C++ '98 (ANSI)"  "c++98")
                 (const :tag "C++ '03" "c++03")
                 (const :tag "C++ '0X" "c++0x")
                 (const :tag "C++ '98 with GNU extensions" "gnu++98")
                 (const :tag "C++ '0X with GNU extensions" "gnu++0x"))
  :safe t)

(flycheck-declare-checker cplusplus-g++
  "A checker for C++ syntax which uses vanilla G++."
  :command '("g++"
             (option "-std=" flycheck-g++-standard)
             "-Wall" "-Wextra" "-fsyntax-only" source-inplace)
  :error-patterns
  '(("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): error: \\(?4:.+\\)$" error)
    ("^\\(?1:.*?\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): warning: \\(?4:.+\\)$" warning))
  :modes 'c++-mode)

(add-to-list 'flycheck-checkers 'cplusplus-g++)


(provide 'init-flycheck-clanguage)
;;; init-flycheck-clanguage.el ends here
