
;; File containing M-x customize data

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-item-indent 0)
 '(TeX-master t t)
 '(ansi-color-names-vector
   [solarized-bg red green yellow blue magenta cyan solarized-fg])
 '(apropos-do-all t)
 '(atomic-chrome-buffer-open-style (quote full))
 '(before-save-hook (quote (copyright-update time-stamp)))
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(colon-double-space t)
 '(comint-input-ignoredups t)
 '(comint-move-point-for-output (quote this))
 '(comint-prompt-read-only nil)
 '(comint-scroll-to-bottom-on-input (quote this))
 '(company-bbdb-modes (quote (message-mode post-mode mail-mode)))
 '(company-idle-delay 0.15)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-quickhelp-delay 0.1)
 '(company-require-match nil)
 '(compilation-scroll-output (quote first-error))
 '(copyright-query nil)
 '(counsel-prompt-function (quote counsel-prompt-function-dir))
 '(cua-enable-cua-keys t)
 '(cua-enable-cursor-indications t)
 '(cua-enable-modeline-indications t)
 '(cua-normal-cursor-color (quote bar))
 '(cua-overwrite-cursor-color (quote hollow))
 '(cua-read-only-cursor-color (quote hbar))
 '(custom-safe-themes
   (quote
    ("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "f8fb7488faa7a70aee20b63560c36b3773bd0e4c56230a97297ad54ff8263930" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2b4a4d1155e279aadd8ebcabf4b0eb8f9bb64ebbd9141fcf61c7655276b587b8" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "de538b2d1282b23ca41ac5d8b69c033b911521fe27b5e1f59783d2eb20384e1f" "e5377626af4d9c413b309267384647f42a8cfd047e0a0b57c3b703a3c170d86b" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(default-input-method "TeX")
 '(default-justification (quote left))
 '(display-hourglass t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-world-list
   (quote
    (("Europe/Rome" "Rome")
     ("Asia/Tokyo" "Tokyo")
     ("Europe/Stockholm" "Stockholm"))))
 '(ebib-index-display-fields (quote (year author title)))
 '(ebib-print-multiline t)
 '(ebib-sort-order (quote ((year) (author editor) (title))))
 '(eldoc-idle-delay 0.3)
 '(epa-pinentry-mode (quote loopback))
 '(fci-rule-color "#073642")
 '(fixme-highlighted-words (quote ("FIXME" "TODO" "BUG" "HACK" "REVIEW" "OPTIMIZE")))
 '(fixme-modes
   (quote
    (erlang-mode java-mode c-mode emacs-lisp-mode jde-mode scheme-mode python-mode ruby-mode cperl-mode slime-mode common-lisp-mode c++-mode d-mode js2-mode haskell-mode tuareg-mode lua-mode pascal-mode fortran-mode prolog-mode asm-mode csharp-mode sml-mode latex-mode LaTeX-mode)))
 '(flycheck-disabled-checkers (quote (tex-chktex)))
 '(flycheck-display-errors-delay 0.5)
 '(flycheck-g++-standard "c++0x")
 '(flycheck-gcc-standard "c99")
 '(flycheck-highlighting-mode (quote columns))
 '(flycheck-idle-change-delay 1.0)
 '(flymake-gui-warnings-enabled t)
 '(flymake-max-parallel-syntax-checks 1)
 '(flymake-number-of-errors-to-display nil)
 '(flymake-run-in-place t)
 '(flyspell-auto-correct-binding "\363")
 '(flyspell-highlight-flag t)
 '(flyspell-issue-message-flag nil)
 '(flyspell-mode-line-string " Fly")
 '(flyspell-persistent-highlight nil)
 '(flyspell-use-meta-tab nil)
 '(font-latex-fontify-sectioning (quote color))
 '(font-latex-match-bold-command-keywords (quote (("stressterm" "{"))))
 '(font-latex-match-italic-command-keywords (quote (("introduceterm" "{"))))
 '(font-latex-match-reference-keywords
   (quote
    (("refdef" "{")
     ("refcor" "{")
     ("Cref" "{")
     ("cref" "{")
     ("reflem" "{")
     ("refth" "{"))))
 '(fringe-mode 0 nil (fringe))
 '(global-company-mode t)
 '(global-hl-line-mode t)
 '(global-nlinum-mode t)
 '(global-semantic-decoration-mode t)
 '(global-semantic-idle-scheduler-mode t)
 '(global-semantic-idle-summary-mode t)
 '(global-semanticdb-minor-mode t)
 '(google-translate-default-source-language "auto")
 '(google-translate-default-target-language "en")
 '(google-translate-enable-ido-completion t)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(haskell-mode-hook
   (quote
    (turn-on-haskell-indentation turn-on-haskell-doc-mode turn-on-haskell-decl-scan
                                 (lambda nil
                                   (add-to-list
                                    (quote ac-sources)
                                    (quote ac-source-haskell))))))
 '(haskell-program-name "ghci")
 '(highlight-symbol-idle-delay 0.5)
 '(init-cc-clang++-dialect "c++11")
 '(init-cc-clang-dialect "c99")
 '(init-cc-g++-dialect "c++11")
 '(init-cc-gcc-dialect "c99")
 '(large-file-warning-threshold nil)
 '(linum-format " %2d│")
 '(magit-auto-revert-mode-lighter "")
 '(make-backup-files t)
 '(massimo-keyboard-comint-modes
   (quote
    (shell-mode comint-mode inferior-octave-mode inferior-emacs-lisp-mode)))
 '(massimo-keyboard-eshell-active t)
 '(massimo-keyboard-folding-meta-g-override-p t)
 '(massimo-keyboard-global-mode t)
 '(menu-bar-mode nil)
 '(mu4e-headers-include-related t t)
 '(mu4e-headers-skip-duplicates t t)
 '(mu4e-split-view nil)
 '(mu4e-view-image-max-height 300)
 '(mu4e-view-image-max-width 400)
 '(mu4e-view-show-addresses t t)
 '(mu4e-view-show-images t)
 '(nlinum-format "%2d│")
 '(no-easy-keys t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(ns-right-alternate-modifier nil)
 '(ns-tool-bar-display-mode (quote both) t)
 '(ns-tool-bar-size-mode (quote regular) t)
 '(org-agenda-current-time-string #("——————————————⌚⌚⌚—————————————" 0 30 (org-heading t)))
 '(org-agenda-search-headline-for-time nil)
 '(org-beamer-environments-extra
   (quote
    (("onlyenv" "O" "\\\\begin{onlyenv}%a" "\\\\end{onlyenv}"))))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . default)
     ("jpg" . "open %s"))))
 '(org-format-latex-options
   (quote
    (:foreground "White" :background default :scale 1.7279999999999998 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
 '(org-preview-latex-default-process (quote dvisvgm))
 '(package-selected-packages
   (quote
    (writegood-mode flyspell-correct-helm anaconda-mode powerline py-yapf helm-swoop gnu-elpa-keyring-update deadgrep helm-recoll helm-rg rg unicode-fonts forge helm-ls-git helm-bibtex helm org unfill ivy-hydra leuven-theme moe-theme solarized-theme poet-theme sdcv gscholar-bibtex org-bullets company-box ivy-rich which-key yasnippet-snippets magit pythonic esup pyenv-mode-auto ivy-bibtex guess-language org-static-blog pdf-tools wc-mode atomic-chrome zoom-frm latex-preview-pane ob-ipython htmlize ssh-config-mode py-autopep8 company-irony-c-headers flycheck-irony visual-fill-column writeroom-mode wgrep wgrep-ag ag iedit try smex counsel zenburn-theme yasnippet virtualenvwrapper use-package undo-tree swiper smartparens skeletor rainbow-mode rainbow-identifiers rainbow-delimiters pallet org-plus-contrib magit-svn magit-gh-pulls magic-latex-buffer irony-eldoc highlight-symbol highlight-quoted highlight-defined highlight-blocks graphviz-dot-mode gnus-alias flycheck expand-region elisp-slime-nav elfeed deferred company-irony auctex ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(post-attachment-regexp "\\(attach\\|alleg\\)")
 '(post-rename-buffer nil)
 '(powerline-default-separator (quote arrow))
 '(powerline-text-scale-factor 0.5)
 '(processing-location "/usr/bin/processing-java")
 '(pulse-flag nil)
 '(pulse-iterations 4)
 '(quack-pretty-lambda-p t)
 '(quack-programs
   (quote
    ("mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gracket" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "racket" "racket -il typed/racket" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(reb-re-syntax (quote string))
 '(reftex-ref-macro-prompt nil)
 '(require-final-newline (quote t))
 '(safe-local-variable-values
   (quote
    ((org-latex-packages-alist
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
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
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
     (org-babel-python-command . "python3"))))
 '(save-interprogram-paste-before-kill t)
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/places")
 '(semantic-decoration-styles
   (quote
    (("semantic-decoration-on-includes")
     ("semantic-decoration-on-protected-members" . t)
     ("semantic-decoration-on-private-members" . t)
     ("semantic-tag-boundary" . t))))
 '(semantic-default-submodes
   (quote
    (global-semantic-decoration-mode global-semantic-idle-scheduler-mode global-semanticdb-minor-mode global-semantic-idle-summary-mode)))
 '(semantic-idle-scheduler-idle-time 0.4)
 '(semantic-idle-summary-function (quote semantic-format-tag-summarize-with-file))
 '(semantic-mode nil)
 '(semantic-stickyfunc-show-only-functions-p t)
 '(show-paren-mode t)
 '(show-smartparens-global-mode nil)
 '(skeletor-project-directory "~/lavori/hacks")
 '(smartparens-global-mode t)
 '(sp-autoinsert-if-followed-by-same 3)
 '(sp-highlight-pair-overlay nil)
 '(sp-navigate-consider-stringlike-sexp (quote (latex-mode)))
 '(sp-wrap-repeat-last 2)
 '(time-stamp-format "%:y-%02m-%02d, %02H:%02M (%Z) %U")
 '(toe-max-length 10)
 '(toe-starting-time-per-word 5)
 '(toe-treat-words (quote downcase))
 '(toe-words-per-level 8)
 '(typopunct-buffer-language (quote english))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
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
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visual-line-mode nil t)
 '(winner-boring-buffers (quote ("*Completions*")))
 '(winner-dont-bind-my-keys t)
 '(winner-mode t nil (winner)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "gold1"))))
 '(font-lock-fixme-face ((((class color) (background dark)) (:background "Yellow" :foreground "Red" :underline nil))) t)
 '(linum ((t (:inherit (shadow default) :background "#303030" :foreground "dark gray"))))
 '(linum-highlight-face ((t (:foreground "Yellow"))))
 '(mmm-default-submode-face ((t (:background "gray15"))))
 '(pulse-highlight-start-face ((t (:background "yellow4")))))
