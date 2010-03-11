;;; init-auto-complete.el --- Configuration for auto-complete mode

;; Filename: init-auto-complete.el

;;; Require

;; Choose either v1.0 or v1.2 versions (1.2 gives me problems with LaTeX)
(defvar default-ac-version 1.0)


;; Load v1.0 of autocomplete
(when (= default-ac-version 1.0)
  (setq default-ac-dir   (concat default-elisp-3rdparties "/auto-complete-v1.0"))
  (setq load-path (append load-path (list default-ac-dir)))
  (byte-recompile-directory default-ac-dir   0)
  )


;; Load v1.2 of autocomplete
(when (>= default-ac-version 1.2)
  (setq default-ac-dir   (concat default-elisp-3rdparties "/auto-complete-v1.2"))
  (setq default-ac-l-dir (concat default-elisp-3rdparties "/auto-complete-v1.2-latex"))
  (setq load-path (append load-path (list default-ac-dir default-ac-l-dir)))
  (byte-recompile-directory default-ac-dir   0)
  (byte-recompile-directory default-ac-l-dir 0)
  )


;; Finally load chosen auto-complete library.
(require 'auto-complete)
(require 'auto-complete-config)

(when (>= default-ac-version 1.2)
  ;; v1.2 comes with dictionary facilities
  (add-to-list 'ac-dictionary-directories (concat default-ac-dir "/dict"))
  )


;; Load LaTeX facilities only if set up
(when (boundp 'default-ac-l-dir)
  (require 'auto-complete-latex)
  (require 'auto-complete-latex-lib)
  )


;; Generic setup.
(global-auto-complete-mode t)           ;enable global-mode
(setq ac-auto-start 3)                  ;automatically start
(setq ac-dwim t)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
 
 
;; The mode that automatically startup.
(setq ac-modes '(emacs-lisp-mode lisp-interaction-mode lisp-mode
      scheme-mode c-mode cc-mode c++-mode java-mode perl-mode
      cperl-mode python-mode ruby-mode ecmascript-mode
      javascript-mode php-mode css-mode makefile-mode sh-mode
      fortran-mode f90-mode ada-mode xml-mode sgml-mode
      haskell-mode literate-haskell-mode latex-mode LaTeX-mode
      emms-tag-editor-mode asm-mode org-mode text-mode))

(add-to-list 'ac-trigger-commands 'org-self-insert-command) ; if you want enable auto-complete at org-mode, uncomment this line

 
;; The sources for common all mode.
(custom-set-variables
 '(ac-sources
   '(
     ac-source-yasnippet
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LaTeX mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (fboundp 'ac-l-setup)
  (add-hook 'latex-mode-hook 'ac-l-setup)
  (add-hook 'LaTeX-mode-hook 'ac-l-setup)
  )


(provide 'init-auto-complete)

;;; init-auto-complete.el ends here

