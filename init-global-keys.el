;;;
;;; Some global key bindings (they should be in a minor-mode)
;;;
;;;-----------------------------------------------------------------



;;; Global keys .........-------------------------------------------

;;{{{ *** Key binding rules ***

;; FIXME Some of them do not work in xterm
;; FIXME Many of them do not work in console
;;
;;
;;  One modifier for  intra-buffer operations (i.e. selection)
;;  Two modifiers for inter-buffer operations (i.e. navigation)
;;  Function keys for buffer processing (compile,check,...)
;;
;;  M-C-<arrow> for moving between windows
;;  M-S-<arrow> for moving between buffers/screens
;;  CUA-selection on (C-<SPC) mark, C-<RET> rect.,C-z C-x C-c C-v)
;;  F2   for local  spell check
;;  S-F2 for global spell check
;;  M-Space for folding
;;  Tab for indent/auto-complete
;;  M-Tab for correct w.r.t. spellcheck (on Flyspell)
;;

;; Moving in text
(global-set-key [C-left]  'backward-word)
(global-set-key [C-right] 'forward-word)
(global-set-key [C-up]    'backward-paragraph)
(global-set-key [C-down]  'forward-paragraph)

;; Moving in structes
(global-set-key [M-left] 'backward-sentence)
(global-set-key [M-right] 'forward-sentence)
(global-set-key [M-up] 'backward-sexp)
(global-set-key [M-down] 'forward-sexp)

;; Managing windows [C-M]
; Moving
(global-set-key [M-C-right] 'windmove-right)
(global-set-key [M-C-left] 'windmove-left)
(global-set-key [M-C-up] 'windmove-up)
(global-set-key [M-C-down] 'windmove-down)
; Scrolling "other window"
(global-set-key [M-C-prior] 'scroll-other-window-down)
(global-set-key [M-C-next] 'scroll-other-window)
; Create and destroy windows
(global-set-key (kbd "M-C--") 'split-window-vertically)
(global-set-key (kbd "M-C-.") 'split-window-horizontally)
;; Make a window to be sticky.
(global-set-key [pause] 'toggle-current-window-sticky)



;; Broken on Xterm
(global-set-key (kbd "M-C-<backspace>") 'delete-window)
(global-set-key (kbd "<C-M-backspace>") 'delete-window)
(global-set-key (kbd "M-C-<return>") 'delete-other-windows)
;; Cheap Xterm substitutions
(global-set-key (kbd "ESC C-h") 'delete-window)
(global-set-key (kbd "ESC <C-return>") 'delete-other-windows)


;; Next/Prev item after Compiling
(global-set-key [M-prior] 'previous-error) ; Does not work with LaTeX!
(global-set-key [M-next] 'next-error)

;; Ispell
(global-set-key (kbd "M-s") 'ispell-word) ; usually overridden by flyspell

;;; ---- Function keys ----

;; emacs commands
(global-set-key [f1]  'help)
(global-set-key [f2]  'ispell-buffer)
(global-set-key [f3]  'kmacro-start-macro-or-insert-counter)
(global-set-key [f4]  'kmacro-end-or-call-macro)
;; daily life
(global-set-key [f5]  'org-capture)  ;; Taking notes
(global-set-key [f6]  'org-agenda)    ;; View agenda/Todo
(global-set-key [f7]  'bbdb)          ;; Query Contacts
(global-set-key [f8]  'spellcheck-language-cycle)
;; devel (adapted to each mode)
(global-set-key [f9]  'recompile)
(global-set-key [f10] 'gdb)
(global-set-key [f11] 'previous-error) ; Does not work with LaTeX!
(global-set-key [f12] 'next-error)

;; Eval S-EXP
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(global-set-key [remap eval-expression] 'pp-eval-expression)


(provide 'init-global-keys)
;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; End:
