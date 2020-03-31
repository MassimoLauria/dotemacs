;;; bootstrap.el --- package/profile utils -*- coding: utf-8 -*-
;;
;; 
;;  Usage:
;;
;;    emacs -batch -l bootstrap.el -f install-pkgs
;;
;;    emacs -batch -l bootstrap.el -f upgrade-pkgs

;;  This is a list of packages that can be installed in batch
;;
(setq requested-packages '(ace-window
                           anaconda-mode 
                           async 
                           auctex
                           beacon 
                           bind-key 
                           company 
                           company-anaconda 
                           company-irony 
                           company-irony-c-headers 
                           dash
                           deferred 
                           diminish 
                           elisp-slime-nav 
                           epl 
                           esup 
                           expand-region 
                           f
                           flycheck
                           flycheck-correct-helm
                           flycheck-irony 
                           forge
                           git-commit 
                           gnus-alias 
                           graphviz-dot-mode 
                           helm
                           helm-bibtex
                           helm-rg
                           helm-recoll
                           helm-swoop
                           helm-org-rifle
                           highlight-blocks 
                           highlight-defined 
                           highlight-quoted 
                           highlight-symbol 
                           htmlize 
                           iedit 
                           irony 
                           irony-eldoc 
                           latex-preview-pane 
                           let-alist 
                           logito 
                           magic-latex-buffer 
                           magit 
                           magit-popup 
                           magit-svn 
                           metaweblog
                           ob-ipython 
                           olivetti
                           org
                           org-bullets 
                           org-pdfview 
                           org-plus-contrib 
                           package-build 
                           pallet 
                           pcache 
                           pdf-tools 
                           pkg-info
                           poet-theme
                           pyenv-mode 
                           pyenv-mode-auto 
                           pythonic 
                           rainbow-delimiters 
                           rainbow-identifiers 
                           rainbow-mode
                           rg
                           s 
                           sdcv 
                           seq 
                           shut-up 
                           skeletor 
                           smartparens
                           solarized-theme
                           ssh-config-mode 
                           try 
                           undo-tree
                           unfill
                           unicode-fonts
                           use-package 
                           virtualenvwrapper 
                           visual-fill-column 
                           which-key 
                           with-editor 
                           writeroom-mode
                           writegood-mode
                           xml-rpc
                           xscheme
                           yapfify
                           yasnippet 
                           yasnippet-snippets 
                           zenburn-theme))
;; -------------------------------------------------------------------

;; Emacs packages
(setq package-user-dir (concat "~/.emacs.d/elpa/" emacs-version))
(setq package-archives  '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("melpa"        . "http://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ))

(when (require 'package nil t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun install-pkgs ()
  "Install all packages in `requested-packages'"
  (package-refresh-contents)
  (dolist (package requested-packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun upgrade-pkgs ()
  "Upgrade installed packages"
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))



(provide 'bootstrap)
