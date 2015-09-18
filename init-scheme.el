;;;
;;; Mit Scheme support for Emacs.
;;;
;;;------------------------------------------------------------------


;; (use-package xscheme
;;   :config (progn
(setq scheme-program-name
      (or
       (executable-find "mit-scheme")
       (executable-find "/usr/local/bin/mit-scheme")
       (executable-find "/Applications/MITScheme.app/Contents/Resources/mit-scheme")
       "scheme"
       ))
(if (file-name-directory scheme-program-name)
    (add-to-list 'exec-path (file-name-directory scheme-program-name) 'append)
  )
;;)
  ;; :commands (run-scheme scheme-mode))

(provide 'init-scheme)
;; Local Variables:
;; mode: emacs-lisp
;; End:
