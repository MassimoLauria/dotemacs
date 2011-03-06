;;;
;;; Python Mode configuration.
;;;
;;;-------------------------------------------------------------------


;;; Try to set up IPython Shell. -------------------------------------------------------------------

;; We force the ipython path to avoid troubles with SAGE math version.
(setq ipython-command "/usr/bin/ipython")
(when (require-maybe 'ipython)
  (setq py-python-command-args '("-pylab" "-colors" "Linux"))
  (setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")
)


;; Python Hook(s) ----------------------------------------------------------------------------------
(add-hook
 'python-mode-hook (lambda ()
                     (set-variable 'py-indent-offset 4)
                     ;;(set-variable 'py-smart-indentation nil)
                     ;;(set-variable 'indent-tabs-mode nil)
                     (define-key py-mode-map (kbd "RET") 'newline-and-indent)
                     )
 )

;; Use a graphical lambda
(require-maybe 'lambda-mode)
(when-available 'lambda-mode
                (add-hook 'python-mode-hook #'lambda-mode 1)
                )

;; Autofill inside of comments

(defsubst python-in-string/comment ()
     "Return non-nil if point is in a Python literal (a comment or string)."
     ;; We don't need to save the match data.
     (nth 8 (syntax-ppss)))


(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))

(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)))



;; Code checkers

;; PyChecker and PyFlakes
;; Use pyflakes instead of pychecker
(if (executable-find "pyflakes")
    (progn
         (setq py-pychecker-command "pyflakes")
         (setq py-pychecker-command-args "")
      )
)

;; PyLint
;; already included but use a more recent version if present.
(require-maybe 'python-pylint)

;; Pep8
;; python style checker
(require-maybe 'python-pep8)


;; PyLookup documentation

;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
(setq pylookup-dir (concat default-elisp-3rdparties "/pylookup"))
(add-to-list 'load-path pylookup-dir)
;; load pylookup when compile time
(eval-when-compile (require-maybe 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file "~/.emacs.d/pylookup.db")
(if (not (file-exists-p pylookup-db-file))
    (warn "Pylookup database not yet initialized")
    )

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c h") 'pylookup-lookup)))


;; Auto Syntax Error Hightlight (very preliminary and with poor support) -------

;; Choose a file checker
(setq flymake-python-syntax-checker "pep8")

(if (and (executable-find "pep8") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "pep8")
)
(if (and (executable-find "epylint") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "epylint")
)
(if (and (executable-find "pyflakes") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "pyflakes")
)
(if (and (executable-find "pychecker") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "pychecker")
)

(when (load "flymake" t)
  (defun flymake-python-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
	  (list flymake-python-syntax-checker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)
;; (add-hook 'python-mode-hook
;;       (lambda ()
;;         (unless (eq buffer-file-name nil) (flymake-mode 1)) ;dont invoke flymake on temporary buffers for the interpreter
;;         ))



(provide 'init-python)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:

