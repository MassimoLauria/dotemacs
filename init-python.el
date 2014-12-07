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


;;; IPython as inferior shell

(defun setup-ipython-inferior-shell (&optional oldversion)
  "Setup IPython as inferior python shell."
  (interactive)
  
  ;; common values
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args ""
        python-shell-prompt-regexp "In \\[[0-9]+\\]: "
        python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
        python-shell-completion-setup-code
        "from IPython.core.completerlib import module_completion")

  ;; auto completion under ipython
  (setq python-shell-completion-module-string-code
        "';'.join(module_completion('''%s'''))\n"
        python-shell-completion-string-code
        "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))


;; Emacs >= 24.3 supports IPython out of the box. 
(when (and (executable-find "ipython") 
           (or (> emacs-major-version 24)
               (and (>= emacs-major-version 24)
                    (>= emacs-minor-version 3))))
  (setup-ipython-inferior-shell))


;; Code editing support
(when (boundp 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode))


;; Syntax checker
(with-eval-after-load 'flycheck
  (add-hook 'python-mode-hook 'flycheck-mode))


;; Code checkers
(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)


;; Basic keybidings
(add-hook 'python-mode-hook
          (lambda ()
            ;; Compile command
            (autoload 'tramp-tramp-file-p "tramp") ; needed for pylint
            (local-set-key (kbd "<f9>") 'pylint)
            (local-set-key (kbd "<f10>") 'python-shell-send-buffer)))


(provide 'init-python)
;; Local Variables:
;; mode: emacs-lisp
;; End:
