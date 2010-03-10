;;; init-auto-complete.el --- Configuration for auto-complete mode

;; Filename: init-auto-complete.el

;;; Require

(setq load-path (append load-path 
                        (list (concat default-elisp-3rdparties "/auto-complete") 
                              (concat default-elisp-3rdparties "/auto-complete-latex")
                              )))


;; Load auto-complete 
(require 'auto-complete)
(require 'auto-complete-config) ; utilities

;; Load auto-complete for latex.
(require 'auto-complete-latex)
(require 'auto-complete-latex-lib)

(add-to-list 'ac-dictionary-directories (concat default-elisp-3rdparties "/auto-complete/dict"))

;;; Code:

;; Generic setup.
(global-auto-complete-mode t)           ;enable global-mode
(setq ac-auto-start 3)                  ;automatically start
(setq ac-dwim t)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
 
 
;; The mode that automatically startup.
(setq ac-modes
      '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode
                        c-mode cc-mode c++-mode java-mode
                        perl-mode cperl-mode python-mode ruby-mode
                        ecmascript-mode javascript-mode php-mode css-mode
                        makefile-mode sh-mode fortran-mode f90-mode ada-mode
                        xml-mode sgml-mode 
                        latex-mode
                        haskell-mode literate-haskell-mode
                        emms-tag-editor-mode
                        asm-mode
                        org-mode
                        text-mode))
;; (add-to-list 'ac-trigger-commands 'org-self-insert-command) ; if you want enable auto-complete at org-mode, uncomment this line
 
;; The sources for common all mode.
(custom-set-variables
 '(ac-sources
   '(
     ac-source-yasnippet ;this source need file `auto-complete-yasnippet.el'
     ;; ac-source-semantic    ;this source need file `auto-complete-semantic.el'
     ac-source-imenu
     ac-source-abbrev
     ac-source-words-in-buffer
     ac-source-files-in-current-dir
     ac-source-filename
     )))


 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook (list
               'emacs-lisp-mode-hook
               'lisp-interaction-mode
               ))
  (add-hook hook '(lambda ()
                    (add-to-list 'ac-sources 'ac-source-symbols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C-common-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enables omnicompletion with `c-mode-common'.
(add-hook 'c-mode-common-hook
          '(lambda ()
             (add-to-list 'ac-omni-completion-sources
                          (cons "\\." '(ac-source-semantic)))
             (add-to-list 'ac-omni-completion-sources
                          (cons "->" '(ac-source-semantic)))
             (add-to-list 'ac-sources 'ac-source-gtags)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C++-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords.
(add-hook 'c++-mode-hook '(lambda ()
                            (add-to-list 'ac-sources 'ac-c++-sources)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Haskell mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keywords.
(add-hook 'haskell-mode-hook '(lambda ()
                                (add-to-list 'ac-sources 'ac-source-haskell)))
 

(provide 'init-auto-complete)

;;; init-auto-complete.el ends here

