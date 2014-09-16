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

;; Launch server in MacOSX (on Linux I use Xsession)
(require 'server nil t)
(when (fboundp 'server-running-p) ; not defined in emacs 22
  (if (and running-MacOSX
           (not (server-running-p)))
      (server-start)))


;; ;; Edit text area on Google Chrome
(autoload 'edit-server-maybe-dehtmlize-buffer "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer   "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
(add-hook 'edit-server-start-hook 'edit-server-textarea-setup)

(defun edit-server-textarea-setup ()
  "Setup the modes for editing textareas in webpages, using edit
server."
  (auto-fill-mode -1)
  (visual-line-mode))

(when (and (fboundp 'server-running-p)
           (server-running-p)
           (locate-library "edit-server"))
  (require 'edit-server)
  (edit-server-start))


;;
;; This is a convoluted way to go back to the previous window after
;; emacsclient session ends. The window id of the callee can be saved
;; to `init-server--cache-win-id' using `save-window-id'
;; function. Then the frame can be restored using
;; `jumpback-window-id'.
;;
;; an example of usage is the following:
;; 
;; $ emacsclient -e '(save-window-id)'; emacsclient <file_to_edit>; emacsclient -e '(jumpback-window-id)'

(defvar init-server--cache-win-id nil
  "The window ID saved by `save-window-id'.")
  
(defun save-window-id ()
  "Save the current active window ID. Useful to be called using
emacsclient."
  (setq init-server--cache-win-id
        (with-temp-buffer 
          (call-process "xprop" nil (buffer-name) nil "-root")
          (goto-char (point-min))
          (search-forward "_NET_ACTIVE_WINDOW") ; find the active window line
          (search-forward "0x")
          (backward-char 2)
          (current-word))))

(defun jumpback-window-id ()
    (call-process
     "wmctrl" nil nil nil "-i" "-a" init-server--cache-win-id))

(provide 'init-server)
;;; init-clipboard.el ends here










