;;; init-modeline.el --- Setup the modeline

;; Copyright (C) 2013, 2014  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2014-12-08, 00:36 (CET) Massimo Lauria>

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


;; abbreviations of major modes
(defvar rename-major-mode-alist
  `(;; lisp
    (lisp-interaction-mode . "λ")
    (emacs-lisp-mode . "eλ")
    (inferior-emacs-lisp-mode . "ieλ")
    ;; other programming languages
    (python-mode . "Py"))
  "Alist for `rename-major-mode-alist'.")

(defun rename-major-mode-line ()
  "Change the major mode name in the mode-line.

The renaming table is `rename-major-mode-alist'."
  (interactive)
  (loop for cleaner in rename-major-mode-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner)))
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'rename-major-mode-line)


;; abbreviation of minor modes
(require 'diminish nil t)
(when (fboundp 'diminish)
  (eval-after-load 'undo-tree '(diminish 'undo-tree-mode))
  (eval-after-load 'hideshow '(diminish 'hs-minor-mode))
  (eval-after-load 'reftex-mode '(diminish 'reftex-mode))
  (eval-after-load 'eldoc '(diminish 'eldoc-mode))
  (eval-after-load 'massimo-keyboard '(diminish 'massimo-keyboard-mode " ⊤"))
  (eval-after-load 'company '(diminish 'company-mode " α"))
  (eval-after-load 'autopair '(diminish 'autopair-mode " ♊"))
  (eval-after-load 'smartparens '(diminish 'smartparens-mode " ♊"))
  (eval-after-load 'yasnippet '(diminish 'yas-minor-mode " ⓨ"))
  (eval-after-load 'simple '(diminish 'auto-fill-function " ⓕ"))
  (eval-after-load 'projectile '(diminish 'projectile-mode))
  (eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))
  (eval-after-load 'fixme-mode '(diminish 'fixme-mode))

  ;; Spell checkers
  (eval-after-load 'flyspell  '(diminish 'flyspell-mode))
  (eval-after-load 'writegood-mode '(diminish 'writegood-mode))
  )


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


;; ;; powerline makes mode-line cool
(when (require 'powerline nil t)
  (powerline-raw mode-line-mule-info nil 'l)
  (powerline-default-theme))


(provide 'init-modeline)
;;; init-modeline.el ends here
