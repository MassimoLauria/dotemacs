;;; custom.el --- Customization file

;;; Commentary:

;; File containing M-x customize data

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   [solarized-bg red green yellow blue magenta cyan solarized-fg])
 '(apropos-do-all t)
 '(auth-source-save-behavior nil)
 '(before-save-hook '(copyright-update time-stamp))
 '(colon-double-space t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output 'this)
 '(comint-prompt-read-only nil)
 '(comint-scroll-to-bottom-on-input 'this)
 '(company-idle-delay 0.15)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-quickhelp-delay 0.1)
 '(company-require-match nil)
 '(compilation-scroll-output 'first-error)
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "f8fb7488faa7a70aee20b63560c36b3773bd0e4c56230a97297ad54ff8263930" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2b4a4d1155e279aadd8ebcabf4b0eb8f9bb64ebbd9141fcf61c7655276b587b8" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "de538b2d1282b23ca41ac5d8b69c033b911521fe27b5e1f59783d2eb20384e1f" "e5377626af4d9c413b309267384647f42a8cfd047e0a0b57c3b703a3c170d86b" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default))
 '(default-input-method "TeX")
 '(default-justification 'left)
 '(display-hourglass t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-world-list
   '(("Europe/Rome" "Rome")
     ("Asia/Tokyo" "Tokyo")
     ("Europe/Stockholm" "Stockholm")))
 '(eldoc-idle-delay 0.3)
 '(epg-pinentry-mode 'loopback)
 '(fci-rule-color "#073642")
 '(font-latex-fontify-sectioning 'color)
 '(font-latex-match-bold-command-keywords '(("stressterm" "{")))
 '(font-latex-match-italic-command-keywords '(("introduceterm" "{")))
 '(font-latex-match-reference-keywords
   '(("refdef" "{")
     ("refcor" "{")
     ("Cref" "{")
     ("cref" "{")
     ("reflem" "{")
     ("refth" "{")))
 '(global-company-mode t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(highlight-symbol-idle-delay 0.5)
 '(large-file-warning-threshold nil)
 '(menu-bar-mode nil)
 '(no-easy-keys t)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(org-beamer-environments-extra
   '(("onlyenv" "O" "\\\\begin{onlyenv}%a" "\\\\end{onlyenv}")))
 '(org-format-latex-options
   '(:foreground "White" :background default :scale 1.7279999999999998 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-preview-latex-default-process 'dvisvgm)
 '(package-selected-packages
   '(helm-projectile helm-xref projectile-ripgrep fira-code-mode pdfgrep visual-regexp pdf-tools company-jedi ninja-mode emoji-cheat-sheet-plus eglot cmake-mode neotree all-the-icons nov latex-math-preview lsp-mode org-pdftools python-pytest helm-c-yasnippet langtool org-static-blog yapfify writegood-mode flyspell-correct-helm powerline helm-swoop gnu-elpa-keyring-update deadgrep helm-recoll helm-rg rg unicode-fonts forge helm-ls-git helm-bibtex helm org unfill ivy-hydra leuven-theme moe-theme solarized-theme poet-theme sdcv gscholar-bibtex org-bullets company-box ivy-rich which-key yasnippet-snippets magit pythonic esup pyenv-mode-auto ivy-bibtex guess-language wc-mode atomic-chrome zoom-frm latex-preview-pane ob-ipython ssh-config-mode py-autopep8 visual-fill-column wgrep wgrep-ag ag iedit try smex counsel zenburn-theme yasnippet virtualenvwrapper use-package undo-tree swiper smartparens skeletor rainbow-mode rainbow-identifiers rainbow-delimiters pallet org-plus-contrib magit-svn magit-gh-pulls magic-latex-buffer highlight-symbol highlight-quoted highlight-defined highlight-blocks graphviz-dot-mode gnus-alias flycheck expand-region elisp-slime-nav elfeed deferred auctex ace-window))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(powerline-default-separator 'arrow)
 '(powerline-text-scale-factor 0.5)
 '(processing-location "/usr/bin/processing-java")
 '(project-switch-commands
   '((project-find-file "Find file" nil)
     (project-find-regexp "Find regexp" nil)
     (project-dired "Dired" nil)
     (project-vc-dir "VC-Dir" nil)
     (project-eshell "Eshell" nil)
     (magit-project-status "Magit" nil)))
 '(project-vc-merge-submodules nil)
 '(pulse-flag nil)
 '(pulse-iterations 4)
 '(quack-pretty-lambda-p t)
 '(quack-programs
   '("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi"))
 '(reb-re-syntax 'string)
 '(reftex-ref-macro-prompt nil)
 '(require-final-newline 't)
 '(safe-local-variable-values
   '((org-latex-packages-alist
      ("" "minted"))
     (org-latex-listings quote minted)
     (flycheck-python-pycompile-executable . "python3")
     (org-babel-default-header-args:python
      (:results . "replace output code"))
     (org-latex-listings-options
      ("numbers" "right"))
     (org-confirm-babel-evaluate)
     (org-latex-listings quote listing)
     (org-html-toplevel-hlevel . 3)
     (org-html-postamble)
     (org-html-preamble)
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (do-delete-whitespace)
     (eval quote
           (auto-fill-mode 1))
     (eval quote
           (visual-line-mode -1))
     (eval quote
           (auto-fill-mode -1))
     (collaborations-turn-on-auto-fill . t)
     (collaborations-delete-whitespace)
     (default-justification . full)
     (TeX-source-correlate-method-active . source-specials)
     (ispell-default-dictionary . american)
     (TeX-PDF-mode . t)
     (org-babel-python-command . "python3")))
 '(save-interprogram-paste-before-kill t)
 '(save-place-file "~/.emacs.d/places")
 '(save-place-mode t nil (saveplace))
 '(semantic-decoration-styles
   '(("semantic-decoration-on-includes")
     ("semantic-decoration-on-protected-members" . t)
     ("semantic-decoration-on-private-members" . t)
     ("semantic-tag-boundary" . t)))
 '(semantic-default-submodes
   '(global-semantic-decoration-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode))
 '(semantic-idle-scheduler-idle-time 0.4)
 '(semantic-idle-summary-function 'semantic-format-tag-summarize-with-file)
 '(semantic-mode nil)
 '(semantic-stickyfunc-show-only-functions-p t)
 '(show-paren-mode t)
 '(show-smartparens-global-mode nil)
 '(smartparens-global-mode t)
 '(sp-autoinsert-if-followed-by-same 3)
 '(sp-highlight-pair-overlay nil)
 '(sp-navigate-consider-stringlike-sexp '(latex-mode))
 '(sp-wrap-repeat-last 2)
 '(time-stamp-format "%:y-%02m-%02d, %02H:%02M (%Z) %U")
 '(toe-max-length 10)
 '(toe-starting-time-per-word 5)
 '(toe-treat-words 'downcase)
 '(toe-words-per-level 8)
 '(typopunct-buffer-language 'english)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visual-line-mode nil t)
 '(writeroom-bottom-divider-width 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "gold1"))))
 '(flymake-error ((t (:underline "Red1"))))
 '(flymake-note ((t (:underline "deep sky blue"))))
 '(flymake-warning ((t (:underline "yellow"))))
 '(font-lock-fixme-face ((((class color) (background dark)) (:background "Yellow" :foreground "Red" :underline nil))) t)
 '(fringe ((t (:background "#3F3F3F" :foreground "#DCDCCC"))))
 '(linum ((t (:inherit (shadow default) :background "#303030" :foreground "dark gray"))))
 '(linum-highlight-face ((t (:foreground "Yellow"))))
 '(mmm-default-submode-face ((t (:background "gray15"))))
 '(mode-line ((t (:background "#2B2B2B" :foreground "#8FB28F" :box nil))))
 '(mode-line-inactive ((t (:background "#383838" :foreground "#5F7F5F" :box nil))))
 '(pulse-highlight-start-face ((t (:background "yellow4"))))
 '(vertical-border ((t (:foreground "#2b2b2b")))))

(provide 'custom)
;;; custom.el ends here
