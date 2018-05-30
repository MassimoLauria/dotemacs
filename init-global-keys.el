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



;; Next/Prev item after Compiling
(global-set-key [M-prior] 'previous-error) ; Does not work with LaTeX!
(global-set-key [M-next] 'next-error)

;; Ispell
(global-set-key (kbd "M-s") 'ispell-word) ; usually overridden by flyspell

;; Font size in Emacs 24
(when (fboundp 'text-scale-adjust)
  (global-set-key (kbd "C--") 'text-scale-adjust)
  (global-set-key (kbd "C-=") 'text-scale-adjust)
  (global-set-key (kbd "C-+") 'text-scale-adjust)
  (global-set-key (kbd "C-0") 'text-scale-adjust))


;;; ---- Function keys ----

;; emacs commands
(global-set-key [f1]  'help)
(global-set-key [f2]  'ispell-buffer)
(global-set-key (kbd "M-<f2>") 'spellcheck-language-cycle)

(global-set-key [f3]  'kmacro-start-macro-or-insert-counter)
(global-set-key [f4]  'kmacro-end-or-call-macro)
;; daily life
(global-set-key [f5]  'org-capture)  ;; Taking notes
;; (global-set-key [f6]  'org-agenda)    ;; Set in init-org-mode
(global-set-key [f7]  'bbdb)          ;; Query Contacts
;; devel (adapted to each mode)
(global-set-key [f9]  'recompile)
(global-set-key (kbd "M-<f9>")  'compile)
(global-set-key [f10] 'gdb)
(global-set-key [f11] 'previous-error) ; Does not work with LaTeX!
(global-set-key [f12] 'next-error)

;; Other fallback command
(when (fboundp 'text-citation-from-reftex)
  (global-set-key (kbd "C-c b") 'text-citation-from-reftex))



(provide 'init-global-keys)
;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; End:
