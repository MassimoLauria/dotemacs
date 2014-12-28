;;; init-server.el --- setup Emacs as a server.

;; Copyright (C) 2012, 2013, 2014  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Keywords:

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

;; Contains the code for starting server. Both for emacs client and
;; for Google Chrome "Edit with emacs"

;;; Code:



;; ;; Edit text area on Google Chrome
;; (autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
;; (autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
;; (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
;; (add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
;; (add-hook 'edit-server-start-hook 'edit-server-textarea-setup)

(defun edit-server-textarea-setup ()
  "Setup the modes for editing textareas in webpages, using edit
server."
  (auto-fill-mode -1)
  (visual-line-mode))



;; Launch server in MacOSX 
(and (require 'server nil t)
     (eq system-type 'darwin)
     (not (server-running-p))
     (server-start))

;; Launch edit server
(and (server-running-p)
     (require 'edit-server nil t)
     (edit-server-start))



(provide 'init-server)
;;; init-server.el ends here










