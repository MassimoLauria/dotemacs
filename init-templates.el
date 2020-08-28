;;; init-templates.el --- Setup template projects

;; Copyright (C) 2015, 2020  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2020-08-28, 15:58 (CEST) Massimo Lauria>

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


;;; Project templates (using `skeletor' package) -------------------------------

(defalias 'template-project        'skeletor-create-project)
(defalias 'template-project-at-dir 'skeletor-create-project-at)

(use-package skeletor
  :ensure t
  :init (setq skeletor-user-directory    (concat (getenv "HOME") "/lavori/templates/")
              skeletor-project-directory (concat (getenv "HOME") "/lavori/hacks/"))
  :commands (skeletor-project skeletor-project-at skeletor-define-template))


;; Defaults substitutions are
;;
;; __PROJECT-NAME__
;; __YEAR__
;; __USER-NAME__
;; __USER-MAIL-ADDRESS__
;; __ORGANISATION__
;; __LICENSE-FILE-NAME__
;;
;; Custom useful subtitutions
;; __DESCRIPTION__
;;
;;

;; Project dependant substitution
;; __PYTHON-BIN__
(skeletor-define-template "python-project"
  :title "Python Library"
  :requires-executables '(("make" . "http://www.gnu.org/software/make/")
                          ("easy_install"."https://pypi.python.org/pypi/setuptools"))
  :substitutions '(("__PYTHON-BIN__" . skeletor-py--read-python-bin)
                   ("__DESCRIPTION__"
                    . (lambda ()
                        (read-string "Description: "))))
  :after-creation
  (lambda (dir)
    (skeletor-async-shell-command "make editor-tools")))






;; Project dependant substitution
;; __TITLE__
(skeletor-define-template "kth-paper"
  :title "KTH Paper"
  :no-license? t
  :substitutions '(("__TITLE__"
                    . (lambda ()
                        (read-string "Title: ")))))


(provide 'init-templates)
;;; init-autotype.el ends here
