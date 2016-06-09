;;; init-elpa.el --- Bootstrap package system -*- coding: utf-8 -*-

;; Time-stamp: "2016-06-09, 13:19 (CEST) Massimo Lauria"

;; Author: Massimo Lauria
;; Keywords: convenience

;;; Commentary:

;; This setup requires Emacs 24

;;; Initialize package system
(setq package-enable-at-startup t)
(require 'package)
(setq package-archives  '(
                          ("gnu"          . "http://elpa.gnu.org/packages/")
                          ("elpa"         . "http://tromey.com/elpa/")
                          ("melpa"        . "http://melpa.org/packages/")
                          )) ;; end of package list
(package-initialize)


;; Bootstrap `use-package' to configure packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


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

;; various
(require-package 'auctex)
;; (require-package 'org)

(use-package semantic
  :ensure t
  :commands semantic-mode)

(require-package 'company)          ;; auto completion...

(require-package 'irony)            
(require-package 'company-irony)    ;; ...for C/C++

(require-package 'diminish)      ;; remove names from modeline
(require-package 'deferred)      ;; 
(require-package 'elisp-slime-nav)
(require-package 'zenburn-theme)
(require-package 'flycheck)

;; from Melpa-unstable
(require-package 'smartparens)
(require-package 'edit-server-htmlize)

;; Editing
(use-package expand-region
  :defer t
  :ensure t
  :pin melpa-stable
  :commands er/expand-region)

(use-package multiple-cursors
  :defer t
  :ensure t
  :pin melpa-stable
  :commands mc/mark-all-like-this-dwim)

(provide 'init-elpa)
