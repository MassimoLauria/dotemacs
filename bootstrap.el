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
(setq requested-packages '(;; very important packages first
                           use-package
                           org-plus-contrib
                           helm
                           magit
                           magit-svn 
                           auctex
                           pdf-tools
                           ;; others
                           ace-window
                           anaconda-mode 
                           async 
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
                           flycheck-irony 
                           flyspell
                           flyspell-correct-helm
                           forge
                           git-commit 
                           gnus-alias 
                           graphviz-dot-mode 
                           helm-bibtex
                           helm-c-yasnippet
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
                           langtool
                           latex-preview-pane 
                           let-alist 
                           logito 
                           magic-latex-buffer 
                           magit-popup 
                           metaweblog
                           ob-ipython 
                           olivetti
                           org-bullets 
                           org-pdftools 
                           org-static-blog
                           package-build 
                           pcache 
                           pinboard
                           pkg-info
                           poet-theme
                           pyenv-mode 
                           pyenv-mode-auto 
                           python-pytest
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

;; Security setting for connection
;; https://github.com/antifuchs/safe-tls-defaults-mode/blob/master/safe-tls-defaults.el
(setq tls-checktrust t)
(setq gnutls-verify-error t)
(setq tls-checktrust t
      tls-program
      '("gnutls-cli -p %p --dh-bits=2048 --ocsp --x509cafile=%t --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:%%PROFILE_MEDIUM' %h"))
(setq network-security-level 'high)

;; Emacs packages
(setq package-user-dir (concat "~/.emacs.d/elpa/" emacs-version))
(setq package-archives  '(("gnu"   . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")
                          ("org"   . "https://orgmode.org/elpa/")
                          ))

(when (require 'package nil t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun install-pkgs ()
  "Install all packages in `requested-packages'."
  (package-refresh-contents)
  (dolist (package requested-packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun upgrade-pkgs ()
  "Upgrade installed packages."
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
        (package-menu-execute t)
      (error
       (package-menu-execute)))))



(provide 'bootstrap)
;;; bootstrap.el ends here
