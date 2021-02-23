;;; init-modeline.el --- Setup the modeline

;; Copyright (C) 2013, 2014, 2015, 2016, 2018, 2020, 2021  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2021-02-23, 01:02 (CET) Massimo Lauria>

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

;; Modeline carries a lot of information, and can carry more.  Here we
;; get rid of the info we do not care and add the one we care.  We
;; also deal with the appearance of the mode-line itself.

;;; Code:




(defun flycheck-mode-line-status-text-slim (&optional status)
  "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
  (let ((text (pcase (or status flycheck-last-status-change)
                (`not-checked "")
                (`no-checker "")
                (`running "")
                (`errored "✗")
                (`finished
                 (if flycheck-current-errors
                     (let ((error-counts (flycheck-count-errors
                                          flycheck-current-errors)))
                       (format "✗:%s/%s"
                               (or (cdr (assq 'error error-counts)) 0)
                               (or (cdr (assq 'warning error-counts)) 0)))
                   "✓"))
                (`interrupted "")
                (`suspicious "?"))))
    (concat " " text)))

(eval-after-load "flycheck"
  '(defalias
     'flycheck-mode-line-status-text
     'flycheck-mode-line-status-text-slim
     ))


(defalias 'flymake-report-status 'flymake-report-status-slim)
(defun flymake-report-status-slim (e-w &optional status)
  "Show \"slim\" flymake status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Φ"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

;; ;; powerline makes mode-line cool
(when (require 'powerline nil t)
  (powerline-raw mode-line-mule-info nil 'l)
  (powerline-default-theme))


(provide 'init-modeline)
;;; init-modeline.el ends here
