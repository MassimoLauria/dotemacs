;;;
;;; Python Mode configuration.
;;;
;;;-------------------------------------------------------------------


;; Uses Virtual Python Environment if present ----------------------------------------------------
(defvar python-virtualenv-path (concat (getenv "HOME")
                                       "/.emacs.d/local-python")
"Sometimes it is useful to prepare a well defined python environment
for Emacs (e.g. using the virtualenv utility). This is its default
path."
)

;; Put the virtual env path in front.
(if (file-executable-p (concat python-virtualenv-path "/bin/python"))
    (setq exec-path (cons (concat python-virtualenv-path "/bin/") exec-path))
    (setenv "PATH" (concat python-virtualenv-path "/bin/:" (getenv "PATH")))
  (message "Unable to setup Python vitual environment. I'll rely on the system.")
  )





;; Load python-mode.el ---------------------------------------------------------------------------
(require 'python-mode)







;; Try to set up IPython Shell. -------------------------------------------------------------------
(when (executable-find "ipython")
  (require-maybe 'ipython)
)

(defun setup-ipython-010-completion ()
  "Ipython.el does not support completion for Ipython 0.10. This
is a workaround."

  (interactive)
  ;; Initialize the python shell with an appropriate completer to be used with
  ;; ipython.el
  (py-execute-string
   "import rlcompleter\nemacs_completer=rlcompleter.Completer(globals())")
  ;; Setup of a completion command for ipython.el
  (set (make-local-variable 'ipython-completion-command-string)
   "print(';'.join(filter(bool,[emacs_completer.complete(('%s','%s')[1],i) for i in range(200)]))) #PYTHON-MODE SILENT\n")
  (end-of-buffer))





;; Python Hook(s) ----------------------------------------------------------------------------------

(add-hook
 'python-mode-hook (lambda ()
                     (set-variable 'py-indent-offset 4)
                     (set-variable 'py-smart-indentation t)
                     (set-variable 'indent-tabs-mode nil)
                     (define-key py-mode-map (kbd "RET") 'newline-and-indent)
                     (define-key py-mode-map (kbd "M-q") 'py-fill-paragraph)
                     )
 )





;; Code checker(s) -----------------------------------------------------------------

;; PyChecker and/or PyFlakes for checking on demand.
(if (executable-find "pyflakes")
    (progn
         (setq py-pychecker-command "pyflakes")
         (setq py-pychecker-command-args "")))

(require 'compile)

(autoload 'python-pylint "python-pylint" "Run pylint checker on the current buffer." t nil)
(autoload 'pylint "python-pylint" "Run pylint checker on the current buffer." t nil)
(autoload 'python-pep8 "python-pep8" "Run PEP8 checker on the current buffer." t nil)
(autoload 'pep8 "python-pep8" "Run PEP8 checker on the current buffer." t nil)


(add-hook 'python-mode-hook
          (lambda ()
            ;; Compile command
            (local-set-key (kbd "<f9>") 'pylint)
            (local-set-key (kbd "<f10>") 'py-execute-import-or-reload)))



;; Flymake code checker(s) ------------------------------------------python-synta---------------------------
(setq flymake-python-syntax-checker nil)

(if (and (executable-find "pyflakes") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "pyflakes"))
(if (and (executable-find "pychecker") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "pychecker"))
(if (and (executable-find "pep8") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "pep8"))
(if (and (executable-find "epylint") (not flymake-python-syntax-checker))
    (setq flymake-python-syntax-checker "epylint"))


(when (and flymake-python-syntax-checker (load "flymake" t))
  (defun flymake-python-init ()
	(let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
	  (list flymake-python-syntax-checker (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init)))



;; Ropemacs completion (not on Aquamacs, because it is too slow!) ------------------------------
(unless running-Aquamacs
  (ac-ropemacs-initialize)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq ac-sources (append '(ac-source-ropemacs) ac-sources))
              (local-set-key (kbd "M-TAB") 'auto-complete)
              )))





;; PyLookup documentation --------------------------------------------------------------------

;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
(setq pylookup-dir (concat default-elisp-3rdparties "/pylookup"))
(add-to-list 'load-path pylookup-dir)
;; load pylookup when compile time
(eval-when-compile (require-maybe 'pylookup))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file "~/.emacs.d/pylookup.db")
;; (if (not (file-exists-p pylookup-db-file))
;;     (warn "Pylookup database not yet initialized")
;;     )

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)


(provide 'init-python)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:

