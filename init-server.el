;;; init-server.el --- setup Emacs as a server.

;; Copyright (C) 2012, 2013, 2014, 2015, 2018, 2019, 2024  Massimo Lauria

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

;; Contains the code for starting server.

;;; Code:

;; Launch server in MacOSX (defer the loading to improve startup time)
(use-package server
  :defer 3
  :config (or (daemonp) (server-running-p) (server-start)))



;; ;; Autoload org-protocol in case org-mode is not already loaded
;; ;; code from https://github.com/alezost/emacs-config/

;; (defun autoload-org-protocol (fun files &rest args)
;;   "Load `org-protocol' if needed.
;; `org' is huge and loading it during emacs start is wasteful, but
;; it is needed to use `org-protocol', isn't it?  Not necessarily:
;; this function makes it possible to avoid requiring `org-protocol'
;; \(thus the whole `org') in the emacs config file.
;; Making this function an 'after' advice for `server-visit-files',
;; will do the right thing."
;;   (if (and (null (featurep 'org-protocol))
;;            (cl-find-if (lambda (spec)
;;                          ;; SPEC is (FILENAME . FILEPOS).
;;                          (string-match "org-protocol:/" (car spec)))
;;                        files))
;;       (if (require 'org-protocol nil t)
;;           ;; `server-visit-files' can't be called as is here, because
;;           ;; `org-protocol' has just been loaded and the protocol advice
;;           ;; is not active yet, so call `server-visit-files' outside
;;           ;; this body.
;;           (apply #'run-with-idle-timer .1 nil
;;                  #'server-visit-files files args)
;;         (message "`org-protocol' has not been loaded!"))
;;     (apply fun files args)))
;; (advice-add 'server-visit-files :around #'autoload-org-protocol)




(provide 'init-server)
;;; init-server.el ends here
