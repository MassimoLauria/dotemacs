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
                           eglot
                           helm
                           magit
                           magit-svn 
                           auctex
                           pdf-tools
                           ;; others
                           ace-window
                           all-the-icons
                           async 
                           beacon 
                           bind-key 
                           cmake-mode
                           company 
                           dash
                           deferred 
                           diminish 
                           elisp-slime-nav 
                           epl 
                           esup 
                           expand-region 
                           f
                           flycheck
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


;; Load `org-mode' config file.
;; Load README.org only if newer than README.el. This code avoid
;; loading `org-mode' for `org-babel-load-file'
(defun mxl-maybe-load-org-config (fName)
  "Load `org-mode' config file `fName'

Load the org-mode file only if it is newer than the corresponding
elisp file obtained with `org-babel', otherwise load the elisp
file directly. This code avoid loading `org-mode' for
`org-babel-load-file' function, when unnecessary"
  (let* ((org-file fName)
         (el-file (concat (file-name-sans-extension org-file) ".el"))
         (age (lambda (file)
                (float-time
                 (time-since
                  (file-attribute-modification-time
                   (or (file-attributes (file-truename file))
                       (file-attributes file))))))))
    (if (and
         (file-exists-p el-file)
         (< (funcall age el-file) (funcall age org-file)))
        (load-file el-file)
      (org-babel-load-file org-file))))



(provide 'bootstrap)
;;; bootstrap.el ends here
