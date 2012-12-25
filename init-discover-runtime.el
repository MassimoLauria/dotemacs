;;; init-discover-runtime.el --- Find out running OS, Emacsen, and setup accordingly

;; Copyright (C) 2010, 2011, 2012  Massimo Lauria
;; Time-stamp: "2012-12-25, 19:35 (CET) Massimo Lauria"

;; Author: Massimo Lauria
;; Keywords: convenience

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

;; I borrowed ideas and source code from
;; http://www.mygooglest.com/fni/dot-emacs.html

;; We define several function and environment variables useful for
;; discovering the running OS type (Linux, MacOSX, Windows...) and the
;; particular type of emacs running (GNU Emacs, XEmacs , Aquamacs,
;; ...)

;;; Code:


;; OS type --- are we running what?
(defvar running-GNULinux (string-match "linux" (prin1-to-string system-type)))
(defvar running-MacOSX   (string-match "darwin" (prin1-to-string system-type)))
(defvar running-Windows  (string-match "windows" (prin1-to-string system-type)))


;; Emacs type --- are we running XEmacs or GNU Emacs)?
(defvar running-GNUEmacs (string-match "GNU Emacs" (version)))
(defvar running-XEmacs   (string-match "XEmacs" (version)))


;; GNU Emacs version, try from 22 to 24.
(defvar running-GNUEmacs22   (= emacs-major-version 22))
(defvar running-GNUEmacs23   (= emacs-major-version 23))
(defvar running-GNUEmacs24   (= emacs-major-version 24))
(defvar running-GNUEmacs22+  (>= emacs-major-version 22))
(defvar running-GNUEmacs23+  (>= emacs-major-version 23))
(defvar running-GNUEmacs24+  (>= emacs-major-version 24))
(defvar running-Aquamacs     (boundp 'aquamacs-version))

;; Graphical capabilities (MacOS interface, or X11)

;; window system of the frame
(defvar running-NSCocoa-frame (eq window-system 'ns))  ;; MacOS
(defvar running-X11-frame     (eq window-system 'x))  ;; X11
(defvar running-Windows-frame (eq window-system 'w32)) ;; Windows

;; window system of the initial process
(if(boundp 'initial-window-system) ;; Does not exist on GNU Emacs 22.
    (progn
      (defvar running-NSCocoa-process  (eq initial-window-system 'ns))  ;; MacOS
      (defvar running-X11-process      (eq initial-window-system 'x))   ;; X11
      (defvar running-Windows-process  (eq initial-window-system 'w32)) ;; Windows
      )
  (progn
    (defvar running-NSCocoa-process  running-NSCocoa-frame)  ;; MacOS
    (defvar running-X11-process      running-X11-frame)      ;; X11
    (defvar running-Windows-process  running-Windows-frame) ;; Windows
    )
  )


;; Conditional running of code, based on OS
(defmacro when-running-GNULinux (&rest body) (list 'if (string-match "linux" (prin1-to-string system-type))  (cons 'progn body)))
(defmacro when-running-MacOSX   (&rest body) (list 'if (string-match "darwin" (prin1-to-string system-type)) (cons 'progn body)))
(defmacro when-running-Windows  (&rest body) (list 'if (string-match "windows" (prin1-to-string system-type))(cons 'progn body)))


;; Conditional running of code, based on Emacs type
(defmacro when-running-GNUEmacs (&rest body) (list 'if (string-match "GNU Emacs" (version))  (cons 'progn body)))
(defmacro when-running-XEmacs (&rest body)   (list 'if (string-match "XEmacs" (version))     (cons 'progn body)))

;; Conditional running of code, based on Emacs version
(defmacro when-running-GNUEmacs22 (&rest body) (list 'if (string-match "GNUEmacs 22" (version)) (cons 'progn body)))
(defmacro when-running-GNUEmacs23 (&rest body) (list 'if (string-match "GNUEmacs 23" (version)) (cons 'progn body)))
(defmacro when-running-GNUEmacs24 (&rest body) (list 'if (string-match "GNUEmacs 24" (version)) (cons 'progn body)))
(defmacro when-running-Aquamacs   (&rest body) (list 'if (boundp 'aquamacs-version) (cons 'progn body)))

;; Conditional running of code, based on graphical system of the selected frame
(defmacro when-running-NSCocoa-frame (&rest body) (list 'if (eq window-system 'ns)     (cons 'progn body)))
(defmacro when-running-X11-frame     (&rest body) (list 'if (eq window-system 'x )     (cons 'progn body)))
(defmacro when-running-Windows-frame (&rest body) (list 'if (eq window-system 'w32)     (cons 'progn body)))

;; Conditional running of code, based on graphical system of the process initial frame
(defmacro when-running-NSCocoa-process (&rest body) (list 'if running-NSCocoa-process    (cons 'progn body)))
(defmacro when-running-X11-process     (&rest body) (list 'if running-X11-process        (cons 'progn body)))
(defmacro when-running-Windows-process (&rest body) (list 'if running-Windows-frame      (cons 'progn body)))


(message "0 Discovering runtime environment...Done")


(provide 'init-discover-runtime)
;;; init-discover-runtime.el ends here
