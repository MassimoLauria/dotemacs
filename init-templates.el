;;; init-templates.el --- Setup template projects

;; Copyright (C) 2015  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2015-02-28, 02:12 (CET) Massimo Lauria>

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

(defalias 'template-project-create        'skeletor-create-project)
(defalias 'template-project-create-at-dir 'skeletor-create-project-at)

(use-package skeletor
  :init (setq skeletor-user-directory (concat default-elisp-path "/templates/")
              skeletor-project-directory (concat (getenv "HOME") "/lavori/hacks/"))
  :commands (skeletor-create-project skeletor-create-project-at))


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
  :title "Python Project"
  :requires-executables '(("make" . "http://www.gnu.org/software/make/")
                          ("pip".   "https://pypi.python.org/pypi/pip")
                          ("virtualenv" . "http://www.virtualenv.org"))
  :substitutions '(("__PYTHON-BIN__" . skeletor-py--read-python-bin)
                   ("__DESCRIPTION__"
                    . (lambda ()
                        (read-string "Description: "))))
  :after-creation
  (lambda (dir)
    (skeletor-async-shell-command "make editor-tools")))























(provide 'init-templates)
;;; init-autotype.el ends here
