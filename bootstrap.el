;;; bootstrap.el --- package/profile utils -*- coding: utf-8 -*-


;;; Commentary:
;;
;;  Setup my Emacs installation, installing/upgrading packages.
;;  It provides useful function which can be called even by
;;  the Makefile.

;;; Code:

;;  This is a list of packages that can be installed in batch
;;
(defvar requested-packages nil "Packages I want to install via makefile.")

(setq requested-packages '(;; very important packages first
                           use-package
                           org
                           org-contrib
                           org-contacts
                           eglot
                           compile-multi
                           magit
                           auctex
                           biblio
                           pdf-tools
                           doom-modeline
                           ;; minibuffer completion
                           vertico
                           embark
                           marginalia
                           orderless
                           consult
                           consult-project-extra
                           consult-yasnippet
                           consult-recoll
                           ;; completion
                           corfu
                           cape
                           ;; others
                           ace-window
                           async
                           bind-key
                           calibredb
                           cmake-mode
                           citar
                           citar-embark
                           dash
                           deferred
                           diminish
                           epl
                           esup
                           f
                           flyspell
                           flyspell-correct
                           forge
                           git-commit
                           gnus-alias
                           go-mode
                           graphviz-dot-mode
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
                           magit-popup
                           metaweblog
                           nerd-icons
                           nerd-icons-dired
                           nerd-icons-completion
                           ob-ipython
                           olivetti
                           org-bullets
                           org-pdftools
                           org-static-blog
                           package-build
                           pcache
                           pkg-info
                           pip-requirements
                           pythonic
                           rainbow-delimiters
                           rainbow-identifiers
                           rainbow-mode
                           rg
                           s
                           sdcv
                           seq
                           shut-up
                           smartparens
                           ssh-config-mode
                           try
                           undo-tree
                           unfill
                           unicode-fonts
                           virtualenvwrapper
                           visual-fill-column
                           visual-regexp
                           which-key
                           with-editor
                           writeroom-mode
                           writegood-mode
                           xml-rpc
                           xscheme
                           yasnippet
                           yapfify
                           ;; themes
                           poet-theme
                           modus-themes))
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
(setopt package-install-upgrade-built-in t)
(setq package-user-dir (concat "~/config/emacs/elpa-local/" emacs-version))
(setq package-archives  '(("gnu"    . "https://elpa.gnu.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                          ("melpa"  . "https://melpa.org/packages/")
                          ))


(when (require 'package nil t)
  (package-initialize)
  ;; https://github.com/jwiegley/use-package/issues/319#issuecomment-845214233
  (assq-delete-all 'org package--builtins)
  (assq-delete-all 'org package--builtin-versions)
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
(defun mxl/maybe-load-org-config (fname)
  "Config file `FNAME' is loaded, via `org-babel-load-file'.

Load the `org-mode' file only if it is newer than the corresponding
elisp file obtained with `org-babel', otherwise load the elisp
file directly. This code avoid loading `org-mode' for
`org-babel-load-file' function, when unnecessary"
  (let* ((org-file fname)
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
