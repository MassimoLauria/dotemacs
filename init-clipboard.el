;;; init-clipboard.el --- Setup for the clipboard, less easy than it sounds.

;; Copyright (C) 2010  Massimo Lauria

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

;; Contains the code for clipboard managing.  In Unix this is far less
;; than trivial.

;;; Code:



(setq mouse-drag-copy-region nil)   ; stops selection with a mouse being immediately injected to the kill ring

(setq x-select-enable-primary nil)	; stops killing/yanking interacting with primary X11 selection 

(setq x-select-enable-clipboard t)	; makes killing/yanking interact with clipboard X11 selection

(if running-X11-process
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
)

;; (when (string= (window-system) "x") 
(setq select-active-regions t)                 ; active region sets primary X11 selection
(global-set-key [mouse-2] 'mouse-yank-primary) ; middle-click only pastes from primary X11 selection.
(setq yank-pop-change-selection t)             ; makes rotating the kill ring change the X11 clipboard.	
;;)                                 

;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)


;; XTerm support
(xterm-mouse-mode t)

;; Mouse wheel scrool the text.
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)



(provide 'init-clipboard)
;;; init-clipboard.el ends here
