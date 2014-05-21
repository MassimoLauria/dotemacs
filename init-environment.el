;;; init-environment.el --- fix and setup the running environment

;; Copyright (C) 2013, 2014 Massimo Lauria <lauria.massimo@gmail.com>

;; Created : "2013-12-17, Tuesday 10:43 (CET) Massimo Lauria"
;; Time-stamp: "2014-05-21, 15:57 (CEST) Massimo Lauria"


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

;; When Emacs is lauched as an app (on MacOSX) or from a gui command
;; (in linux) there is often the chance that the running environment
;; does not contain some environment variables or does not set them up
;; appropriately. In this file I take care of such issues as long as
;; they arise in my setup. I don't claim any generality here.

;; Notice that the right way to set system-wide exec-paths on Mac is
;; to use `/etc/paths.d' files. But this solution requires root
;; privileges.

;;; Code:
;;

;; Apparently OSX Maverick starts emacs in root. This has been fixed,
;; but not yet in the released version 24.3.
(if running-MacOSX 
    (setq default-directory (getenv "HOME"))) 


;; Set paths, since sometime Mac OSX has weird paths and Emacs.app
;; doesn't pick them up.
(add-to-list 'exec-path "/usr/local/bin/") ; local
(add-to-list 'exec-path "/opt/local/bin/") ; macports
(add-to-list 'exec-path "~/.local/bin")    ; home


;; read paths from files in "/etc/paths.d/" if exist.
(with-temp-buffer 
  (condition-case nil
      (dolist (file (directory-files "/etc/paths.d/" t))
        (if (not (file-directory-p file))
            (insert-file-contents file)))
    (error nil))
      
  (dolist (path (split-string (buffer-string) "\n" t))
    (if (file-directory-p path)
        (add-to-list 'exec-path path))))


;; Path for LaTeX distrubution TeX-live, on Mac.
(when (file-directory-p "/usr/texbin/")
  (add-to-list 'exec-path "/usr/texbin/" 'append))



(defun environment-variable-add-to-list (varname element)
  "Add ELEMENT from the list in the enviroment variable VARNAME.

VARNAME is considered as a list of elements like \"aa:bb:cc\". If
VARNAME is undefined of empty, it defines it. If ELEMENT is
already in the list, the function won't do anything.

There is no guarantee on the actual order of the elements in the
list."  
  (let ((separator (if (eq system-type 'windows-nt) ";" ":"))
        tmplist)
    (if (getenv varname)
        (setq tmplist (split-string 
                       (getenv varname) 
                       separator)))
    (add-to-list 'tmplist element 'append 'string-equal)
    (setenv varname (mapconcat 'identity tmplist ":"))))

(defun environment-variable-rm-from-list (varname element)
  "Remove ELEMENT from the list in the enviroment variable VARNAME.

VARNAME is considered as a list of elements like \"aa:bb:cc\". If
ELEMENT is not in the list, the function won't do anything.

There is no guarantee on the actual order of the elements in the
list."  
  (let ((separator (if (eq system-type 'windows-nt) ";" ":"))
        tmplist)
    (if (getenv varname)
        (setq tmplist (split-string 
                       (getenv varname)
                       separator)))
    (setenv varname (mapconcat 'identity (remove element tmplist) ":"))))


(provide 'init-environment)
