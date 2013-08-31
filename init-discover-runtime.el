;;; init-discover-runtime.el --- Find out running OS, Emacsen, and setup accordingly

;; Copyright (C) 2010, 2011, 2012, 2013  Massimo Lauria
;; Time-stamp: "2013-08-31, 12:35 (CEST) Massimo Lauria"

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


;; GNU Emacs version, try from 22 to 26.
(defvar running-GNUEmacs22   (= emacs-major-version 22))
(defvar running-GNUEmacs23   (= emacs-major-version 23))
(defvar running-GNUEmacs24   (= emacs-major-version 24))
(defvar running-GNUEmacs25   (= emacs-major-version 25))
(defvar running-GNUEmacs26   (= emacs-major-version 26))
(defvar running-GNUEmacs22+  (>= emacs-major-version 22))
(defvar running-GNUEmacs23+  (>= emacs-major-version 23))
(defvar running-GNUEmacs24+  (>= emacs-major-version 24))
(defvar running-GNUEmacs25+  (>= emacs-major-version 25))
(defvar running-GNUEmacs26+  (>= emacs-major-version 26))
(defvar running-Aquamacs     (boundp 'aquamacs-version))


;;; Graphical capabilities (MacOS interface, or X11)

;; window system of the initial process (maybe different from the frame)
(defvar running-NSCocoa-process  (eq initial-window-system 'ns))
(defvar running-X11-process      (eq initial-window-system 'x))
(defvar running-Windows-process  (eq initial-window-system 'w32)) 


;; window system of the frame
(defmacro this-is-NSCocoa-frame () '(eq window-system 'ns))  ;; MacOS
(defmacro this-is-X11-frame     () '(eq window-system 'x))  ;; X11
(defmacro this-is-Windows-frame () '(eq window-system 'w32)) ;; Windows

(provide 'init-discover-runtime)
;;; init-discover-runtime.el ends here
