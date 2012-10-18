;;; init-open-link.el --- Keybindings for opening links in various modes

;; Copyright (C) 2011, 2012  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
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

;; There are several modes with different key sequence for opening
;; links in text.  This files organize the setup of such various sets
;; of functions.


;;; Code:

(defvar massimo-keyboard-open-link-key1 (kbd "C-c o")
"Primary key sequence used to open links in text files."
)

(defvar massimo-keyboard-open-link-key2 (kbd "C-c C-o")
"Secondary key sequence used to open links in text files."
)

(defvar massimo-keyboard-fallback-browser "google-chrome"
"What browser to use if none is configured."
)


;; Setup default browser
(when running-GNULinux
  (let ((browser
         (or (getenv "BROWSER")
             massimo-keyboard-fallback-browser)))
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program browser)))

;; Xah Lee function for opening links in dired-mode
;; http://xahlee.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun open-in-external-app ()
  "Open the current file or dired marked files in external app.
Works in Microsoft Windows, Mac OS X, Linux."
  (interactive)

  (let ( doIt
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           (t (list (buffer-file-name))) ) ) )

    (setq doIt (if (<= (length myFileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files?") ) )

    (when doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) myFileList)
        )
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\" &" fPath)) )  myFileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (shell-command (format "xdg-open \"%s\" &" fPath)) ) myFileList) ) ) ) ) )



;; Setup for `goto-address-mode' and `goto-address-prog-mode'
(add-hook 'goto-address-mode-hook
          (lambda ()
            ;; Remove old binding
            (define-key goto-address-highlight-keymap (kbd "C-c RET") nil)
            (define-key goto-address-highlight-keymap
              massimo-keyboard-open-link-key1 'goto-address-at-point)
            (define-key goto-address-highlight-keymap
              massimo-keyboard-open-link-key2 'goto-address-at-point)
            ))


;; Setup for `org-mode'
(add-hook 'org-mode-hook
          (lambda ()
            ;; Remove old binding
            (define-key org-mode-map (kbd "C-c C-o") nil)
            (define-key org-mode-map
              massimo-keyboard-open-link-key1 'org-open-at-point)
            (define-key org-mode-map
              massimo-keyboard-open-link-key2 'org-open-at-point)
            ))

;; Setup for `dired-mode'
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map
              massimo-keyboard-open-link-key1 'open-in-external-app)
            (define-key dired-mode-map
              massimo-keyboard-open-link-key2 'open-in-external-app)
            ))


;; Fall back keybinding. Mode maps will override this choice.
(global-set-key massimo-keyboard-open-link-key1 'browse-url-at-point)
(global-set-key massimo-keyboard-open-link-key2 'browse-url-at-point)


(provide 'init-open-link)
;;; init-open-link.el ends here
