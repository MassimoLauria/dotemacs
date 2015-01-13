;;; init-elpa.el --- Bootstrap package system -*- coding: utf-8 -*-

;; Time-stamp: "2015-01-13, 17:42 (CET) Massimo Lauria"

;; Author: Massimo Lauria
;; Keywords: convenience

;;; Commentary:

;; This setup requires Emacs 24

;;; Initialize package system
(require 'package)
(setq package-archives  '(
                          ("gnu"          . "http://elpa.gnu.org/packages/")
                          ("elpa"         . "http://tromey.com/elpa/")
                          ("melpa-stable" . "http://stable.melpa.org/packages/")
                          ("melpa"        . "http://melpa.org/packages/")
                          ;; ("marmalade"    . "http://marmalade-repo.org/packages/")
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
(require-package 'org)


;; (require-package 'yasnippet)
(use-package yasnippet
  :ensure t
  :defer t
  :pin melpa-stable
  :idle (yas-global-mode))


(require-package 'company)          ;; auto completion...

(require-package 'irony)            
(require-package 'company-irony)    ;; ...for C/C++

(require-package 'anaconda-mode)
(require-package 'company-anaconda) ;; ...for Python


(require-package 'diminish)      ;; remove names from modeline
(require-package 'deferred)      ;; 
(require-package 'magit)
(require-package 'magit-svn)
(require-package 'elisp-slime-nav)
(require-package 'zenburn-theme)
(require-package 'flycheck)

;; from Melpa-unstable
(require-package 'smartparens)
(require-package 'edit-server-htmlize)
(require-package 'bbdb)

;; Editing
(require-package 'expand-region)
(require-package 'multiple-cursors)


(provide 'init-elpa)
