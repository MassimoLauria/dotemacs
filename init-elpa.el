;;; init.el --- Main configuration file -*- coding: utf-8 -*-

;; Time-stamp: "2013-02-22, 23:17 (CET) Massimo Lauria"

;; Author: Massimo Lauria
;; Keywords: convenience

;;; Commentary:

;; Manage ELPA packages also in Emacs < 24. A lot of code has been
;; borrowed by
;; https://github.com/purcell/emacs.d/blob/master/init-elpa.el


;;; Code
(require 'package)

(setq package-archives  '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")))




;; Color theme manager depends on the version
(when (< emacs-major-version 24)
  (add-to-list 'package-load-list '(zenburn-theme nil) 'append))
(when (>= emacs-major-version 24)
  (add-to-list 'package-load-list '(color-theme nil) 'append)
  (add-to-list 'package-load-list '(zenburn nil) 'append))


;; Initialiaze packages
(package-initialize)


;;------------------------------------------------------------------------------
;; Patch up annoying package.el quirks
;;------------------------------------------------------------------------------

(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat name "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))


;;------------------------------------------------------------------------------
;; Add support to package.el for pre-filtering available packages
;;------------------------------------------------------------------------------

(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.

The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
            (funcall package-filter-function
                     (car package)
                     (package-desc-vers (cdr package))
                     archive))
    ad-do-it))


;;------------------------------------------------------------------------------
;; On-demand installation of packages
;;------------------------------------------------------------------------------

(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))




;;------------------------------------------------------------------------------
;; Install missing packages
;;------------------------------------------------------------------------------


(require-package 'auto-complete) ;; auto completion
(require-package 'diminish)      ;; remove names from modeline
(require-package 'deferred)      ;; 
(require-package 'epc)           ;; process used for python auto-completion

(require-package 'expand-region)
(require-package 'multiple-cursors)

;; Color theme 
(when (< emacs-major-version 24)
  (require-package 'color-theme)
  (require-package 'zenburn))
(when (>= emacs-major-version 24)
  (require-package 'zenburn-theme))


(provide 'init-elpa)
