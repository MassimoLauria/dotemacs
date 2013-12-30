;;; init-cc-mode-syntax-check --- Syntax checker configuration for C/C++

;; Copyright (C) 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-12-30, 17:15 (CET) Massimo Lauria>

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

(when (fboundp 'flycheck-declare-checker) ;; older flychecker

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
                                       (option "--stdlib=" init-cc-clang++-stdlib)
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
  (add-to-list 'flycheck-checkers 'cplusplus-clang))

(when (fboundp 'flycheck-define-checker)

  (flycheck-define-checker my-c/c++-gcc
    "A C/C++ syntax checker using Gcc."
    :command ("gcc"
              "-fsyntax-only"
              "-Wall" "-Wextra"
              (eval
                    (cl-case major-mode
                      (c++-mode (concat "-std=" init-cc-g++-dialect))
                      (c-mode (concat "-std=" init-cc-gcc-dialect))))
              "-x" (eval
                    (cl-case major-mode
                      (c++-mode "c++")
                      (c-mode "c")))
              ;; We must stay in the same directory, to properly resolve #include
              ;; with quotes
              source-inplace)
    :error-patterns
    ((info line-start (file-name) ":" line ":" column
           ": note: " (message) line-end)
     (warning line-start (file-name) ":" line ":" column
              ": warning: " (message) line-end)
     (error line-start (file-name) ":" line ":" column
            ": " (or "fatal error" "error") ": " (message) line-end))
    :modes (c-mode c++-mode)
    :next-checkers ((warnings-only . c/c++-cppcheck)))


  (flycheck-define-checker my-c/c++-clang
    "A C/C++ syntax checker using Clang.

See URL `http://clang.llvm.org/'."
    :command ("clang"
              "-fsyntax-only"
              "-fno-color-diagnostics"    ; Do not include color codes in output
              "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
              "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
              (eval
               (cl-case major-mode
                 (c++-mode (concat "-std=" init-cc-clang++-dialect))
                 (c-mode (concat "-std=" init-cc-clang-dialect))))
              (option "-stdlib=" init-cc-clang++-stdlib)
              (option-flag "-fms-extensions" flycheck-clang-ms-extensions)
              (option-flag "-fno-rtti" flycheck-clang-no-rtti)
              (option-list "-include" flycheck-clang-includes)
              (option-list "-W" flycheck-clang-warnings s-prepend)
              (option-list "-D" flycheck-clang-definitions s-prepend)
              (option-list "-I" flycheck-clang-include-path)
              "-x" (eval
                    (cl-case major-mode
                      (c++-mode "c++")
                      (c-mode "c")))
              ;; We must stay in the same directory, to properly resolve #include
              ;; with quotes
              source-inplace)
    :error-patterns
    ((info line-start (file-name) ":" line ":" column
           ": note: " (message) line-end)
     (warning line-start (file-name) ":" line ":" column
              ": warning: " (message) line-end)
     (error line-start (file-name) ":" line ":" column
            ": " (or "fatal error" "error") ": " (message) line-end))
    :modes (c-mode c++-mode)
    :next-checkers ((warnings-only . c/c++-cppcheck)))
  
  )



;; Use flycheck in c/c++ source
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)


(provide 'init-cc-mode-syntax-check)
;;; init-flycheck-clanguage.el ends here
