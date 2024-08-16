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
   ["#f4eedb" "#cc1f24" "#778c00" "#a67c00" "#007ec4" "#c42475" "#11948b" "#88999b"])
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
   '("0af489efe6c0d33b6e9b02c6690eb66ab12998e2649ea85ab7cfedfb39dd4ac9" "21e3d55141186651571241c2ba3c665979d1e886f53b2e52411e9e96659132d4" "88cb0f9c0c11dbb4c26a628d35eb9239d1cf580cfd28e332e654e7f58b4e721b" "69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8" "91db2df9490180a006964179f3aa4fcbc6bbf63cdcba189b41ea1ff5a606df33" "f700bc979515153bef7a52ca46a62c0aa519950cc06d539df4f3d38828944a2c" "7c646f886861603c46ff9bf0e923bdadc199732fd7dfcd60c3081ee25febe334" "a3e99dbdaa138996bb0c9c806bc3c3c6b4fd61d6973b946d750b555af8b7555b" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "7a121b56eb7622d75e8323f9311b9797f4d06b8ba2f0b39c125ce33a5648d0a2" "e01db763cd9daa56f75df8ebd057f84017ae8b5f351ec90c96c928ad50f3eb25" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "97965ccdac20cae22c5658c282544892959dc541af3e9ef8857dbf22eb70e82b" "f8fb7488faa7a70aee20b63560c36b3773bd0e4c56230a97297ad54ff8263930" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2b4a4d1155e279aadd8ebcabf4b0eb8f9bb64ebbd9141fcf61c7655276b587b8" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "de538b2d1282b23ca41ac5d8b69c033b911521fe27b5e1f59783d2eb20384e1f" "e5377626af4d9c413b309267384647f42a8cfd047e0a0b57c3b703a3c170d86b" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "5e1d1564b6a2435a2054aa345e81c89539a72c4cad8536cfe02583e0b7d5e2fa" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default))
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
 '(embark-prompter 'embark-keymap-prompter nil nil "Customized with use-package embark")
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
   '(:foreground "White" :background default :scale 1.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-preview-latex-default-process 'dvisvgm)
 '(package-selected-packages
   '(consult-project-extra compile-multi cape chatgpt-shell flyspell-correct citar citar-embark consult-yasnippet consult consult-recoll consult-spotify embark embark-consult marginalia orderless vertico corfu page-break-lines monkeytype helpful bibtex-completion flymake-go flymake-go-staticcheck go-mode rust-mode eat nerd-icons-completion nerd-icons-dired modus-themes base16-theme emacsql emacsql-sqlite green-phosphor-theme green-screen-theme pip-requirements company-emoji company-emojify org-contacts calibredb org-contrib helm-company projectile-ripgrep fira-code-mode pdfgrep visual-regexp pdf-tools company-jedi ninja-mode emoji-cheat-sheet-plus eglot cmake-mode neotree nov latex-math-preview lsp-mode org-pdftools langtool org-static-blog yapfify writegood-mode powerline helm-swoop gnu-elpa-keyring-update deadgrep helm-recoll rg unicode-fonts forge helm-ls-git helm-bibtex helm org unfill ivy-hydra leuven-theme moe-theme poet-theme sdcv gscholar-bibtex org-bullets company-box ivy-rich which-key magit pythonic esup pyenv-mode-auto ivy-bibtex guess-language wc-mode atomic-chrome zoom-frm latex-preview-pane ob-ipython ssh-config-mode py-autopep8 visual-fill-column wgrep wgrep-ag ag iedit try smex counsel yasnippet virtualenvwrapper use-package undo-tree swiper smartparens rainbow-mode rainbow-identifiers rainbow-delimiters pallet magit-svn magit-gh-pulls highlight-symbol highlight-quoted highlight-defined highlight-blocks graphviz-dot-mode gnus-alias elisp-slime-nav elfeed deferred auctex ace-window))
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
   '((TeX-master . "main")
     (org-latex-packages-alist
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
 '(show-paren-mode t)
 '(show-smartparens-global-mode nil)
 '(smartparens-global-mode t)
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
 '(world-clock-list
   '(("Europe/Rome" "Rome")
     ("Asia/Tokyo" "Tokyo")
     ("Europe/Stockholm" "Stockholm")))
 '(writeroom-bottom-divider-width 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "gold1"))))
 '(flycheck-error ((t (:underline "Red"))))
 '(flycheck-info ((t nil)))
 '(flycheck-warning ((t (:underline "Yellow"))))
 '(flymake-error ((t (:underline "Red1"))))
 '(flymake-note ((t (:underline "deep sky blue"))))
 '(flymake-warning ((t (:underline "yellow"))))
 '(flyspell-duplicate ((t (:underline (:color "magenta" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "magenta" :style wave)))))
 '(linum ((t (:inherit (shadow default) :background "#303030" :foreground "dark gray"))))
 '(linum-highlight-face ((t (:foreground "Yellow"))))
 '(mmm-default-submode-face ((t (:background "gray15"))))
 '(org-block-begin-line ((t (:slant italic :background "#303030" :foreground "#535353"))))
 '(vertical-border ((t (:foreground "#2b2b2b"))))
 '(writegood-duplicates-face ((t nil)))
 '(writegood-passive-voice-face ((t (:underline (:color "magenta" :style wave)))))
 '(writegood-weasels-face ((t (:underline (:color "magenta" :style wave))))))

(provide 'custom)
;;; custom.el ends here
