;;; init-hyperlink.el --- Keybindings for inserting/opening/saving links in various modes

;; Copyright (C) 2011, 2012, 2013, 2018, 2019, 2024  Massimo Lauria

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
;;
;; To insert URL and links I will always use `org-insert-link' which
;; can be customized to insert the link in the appropriate format.
;;
;; To save URL and links I will always use `org-store-link'

;;; Code:

(defvar massimo-keyboard-store-link-key (kbd "C-c s")
  "Key sequence used to insert links in text files.")

(defvar massimo-keyboard-insert-link-key (kbd "C-c i")
  "Key sequence used to insert links in text files.")

(defvar massimo-keyboard-open-link-key1 (kbd "C-c o")
  "Primary key sequence used to open links in text files.")

(defvar massimo-keyboard-open-link-key2 (kbd "C-c C-o")
  "Secondary key sequence used to open links in text files.")


(defalias 'open-in-external-app 'browse-url-of-dired-file
  "Open the current file or dired marked files in external app.
Works in Microsoft Windows, Mac OS X, Linux.")

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

;; All urls/mails are clickable in comments and strings
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

;; Setup for `org-mode'
(add-hook 'org-mode-hook
          (lambda ()
            ;; Remove old binding
            (define-key org-mode-map (kbd "C-c C-o") nil)
            (define-key org-mode-map
              massimo-keyboard-open-link-key1 'org-open-at-point)
            (define-key org-mode-map
              massimo-keyboard-open-link-key2 'org-open-at-point)
            (define-key org-mode-map
              massimo-keyboard-insert-link-key 'org-insert-link)
            (define-key org-mode-map
              massimo-keyboard-store-link-key 'org-store-link)
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
(global-set-key massimo-keyboard-insert-link-key 'org-insert-link-global)
(global-set-key massimo-keyboard-store-link-key  'org-store-link)


(provide 'init-hyperlink)
;;; init-open-link.el ends here
