;;; init-cc-compiler --- Compilers configuration for C/C++

;; Copyright (C) 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-07-18, 23:51 (CEST) Massimo Lauria>

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

;; Setup for C/C++ dialects and library configurations are unified in
;; this file. In this way both syntax checker and auto completion can
;; make use of it.

;;; Code:

(defgroup init-cc nil "C/C++ compiler configuration")


(defcustom init-cc-gcc-dialect nil
  "The language standard of C used for GCC compiler.

When nil, use the default standard used by g++ compiler.  When
set to a string, pass the string to \"--std=\" option of g++."
  :tag "Default C language dialect for GNU C compiler"
  :type '(choice (const :tag "Compiler default" nil)
                 (const :tag "C'90 (ANSI)" "c90")
                 (const :tag "C'99" "c99")
                 (const :tag "C'1x" "c1x")
                 (const :tag "C'90 with GNU extensions"  "gnu90")
                 (const :tag "C'99 with GNU extensions"  "gnu99")
                 (const :tag "C'1x with GNU extensions"  "gnu1x"))
  :group 'init-cc
  :safe t)

(defcustom init-cc-g++-dialect nil
  "The language standard of C++ used for G++ compiler

When nil, use the default standard used by g++ compiler.  When
set to a string, pass the string to \"--std=\" option of g++."
  :tag "Default C++ language dialect for GNU C++ compiler"
  :type '(choice (const :tag "Compiler default" nil)
                 (const :tag "C++ '98 (ANSI)"  "c++98")
                 (const :tag "C++ '03" "c++03")
                 (const :tag "C++ '0X" "c++0x")
                 (const :tag "C++ '11" "c++11")
                 (const :tag "C++ '98 with GNU extensions" "gnu++98")
                 (const :tag "C++ '0X with GNU extensions" "gnu++0x"))
  :group 'init-cc
  :safe t)


(defcustom init-cc-clang-dialect nil
  "The language standard of C used for GCC compiler.

When nil, use the default standard used by g++ compiler.  When
set to a string, pass the string to \"--std=\" option of gcc."
  :tag "Default C language dialect for Clang compiler"
  :type '(choice (const :tag "Compiler default" nil)
                 (const :tag "C'90 (ANSI)" "c90")
                 (const :tag "C'99" "c99")
                 (const :tag "C'1x" "c1x"))
  :group 'init-cc
  :safe t)


(defcustom init-cc-clang++-dialect nil
  "The language standard of C++ used for clang++ compiler

When nil, use the default standard used by clang++ compiler.  When
set to a string, pass the string to \"--std=\" option of clang++."
  :tag "Default C++ language dialect for Clang++ compiler"
  :type '(choice (const :tag "Compiler default" nil)
                 (const :tag "C++ '98 (ANSI)"  "c++98")
                 (const :tag "C++ '03" "c++03")
                 (const :tag "C++ '0X" "c++0x")
                 (const :tag "C++ '11" "c++11"))
  :group 'init-cc
  :safe t)


(defcustom init-cc-clang++-stdlib nil
  "The standard C++ library used for with clang.

When nil, use the default standard used by clang compiler.  When
set to a value, the value is passed \"--stdlib=\" option of clang++."
  :tag "Standard Library for C++ to be used with Clang"
  :type '(choice (const :tag "Default" nil)
                 (const :tag "libc++ from clang project" "libc++")
                 (const :tag "libstdc++ from GNU project" "libstdc++")
                 )
  :group 'init-cc
  :safe t)






(provide 'init-cc-compiler)
