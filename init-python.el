;;;
;;; Python Mode configuration.
;;;
;;; If Emacs version >= 24.3 then `python.el' supports ipython (>=
;;; 0.11) out of the box. For older emacs we stick with canonical
;;; `python' interpreter.  
;;;
;;; This setup avoids downloading external python modes like
;;; `python-mode.el', and should gracefully suck on older installation
;;; (that I do not use very often).
;;; -------------------------------------------------------------------


;;; IPython setup

(defun setup-ipython-inferior-shell (&optional oldversion)
  "Setup IPython as inferior python shell.

If OLDVERSION is non-nil, it will setup completion for ipython
0.10 or less (which is currently used in Sagemath)."
  (interactive)
  ;; common values
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion")
  ;; completion setup is different for old IPython
  (if oldversion
      (setq python-shell-completion-string-code
            "';'.join(__IP.complete('''%s'''))\n"
            python-shell-completion-module-string-code "")
    (setq python-shell-completion-module-string-code
          "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")))


;; Only for Emacs >= 24.3
(when (and (executable-find "ipython") 
           (or (> emacs-major-version 24)
               (and (>= emacs-major-version 24)
                    (>= emacs-minor-version 3))))
           (setup-ipython-inferior-shell))



;;; Jedi library for completion

(setq jedi-python-dir (concat default-elisp-3rdparties "/emacs-jedi"))
(add-to-list 'load-path jedi-python-dir)

(autoload 'jedi:setup "jedi" nil t)
(autoload 'jedi:ac-setup "jedi" nil t)
(setq jedi:setup-keys nil)
(add-hook 'python-mode-hook 'jedi:setup)


;;; Code checker(s)
(autoload 'flycheck-mode "flycheck" nil t)

(add-hook 'python-mode-hook
          (lambda ()
            ;; Compile command
            (flycheck-mode 1)
            (autoload 'tramp-tramp-file-p "tramp") ; needed for pylint
            (local-set-key (kbd "<f9>") 'pylint)
            (local-set-key (kbd "<f10>") 'python-shell-send-buffer)))


;; additional checkers
(require 'compile)
(autoload 'python-pylint "python-pylint" "Run pylint checker on the current buffer." t nil)
(autoload 'pylint "python-pylint" "Run pylint checker on the current buffer." t nil)
(autoload 'python-pep8 "python-pep8" "Run PEP8 checker on the current buffer." t nil)
(autoload 'pep8 "python-pep8" "Run PEP8 checker on the current buffer." t nil)



;;; Documentation lookup

(setq pylookup-dir (concat default-elisp-3rdparties "/pylookup"))
(add-to-list 'load-path pylookup-dir)
(eval-when-compile (require 'pylookup nil t))

;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file "~/.emacs.d/pylookup.db")

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)


(provide 'init-python)
;; Local Variables:
;; mode: emacs-lisp
;; End:
