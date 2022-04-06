;;; latex-preview-pane-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "latex-preview-pane" "latex-preview-pane.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from latex-preview-pane.el

(autoload 'latex-preview-pane-enable "latex-preview-pane" "\
Enable `latex-preview-pane-mode' in `latex-mode'." nil nil)

(autoload 'init-latex-preview-pane "latex-preview-pane" nil nil nil)

(autoload 'latex-preview-update "latex-preview-pane" nil t nil)

(autoload 'latex-preview-pane-update "latex-preview-pane" nil t nil)

(autoload 'latex-preview-pane-update-p "latex-preview-pane" nil nil nil)

(autoload 'latex-preview-pane-mode "latex-preview-pane" "\
Toggle Latex Preview Pane Mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state.
     
     When Latex Preview Pane mode is enabled, saving a latex file will cause 
     a PDF preview pane of your document to appear.

This is a minor mode.  If called interactively, toggle the
`Latex-Preview-Pane mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `latex-preview-pane-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "latex-preview-pane" '("latex-p" "lpp/" "pdf-latex-command" "preview-orientation" "shell-escape-mode"))

;;;***

;;;### (autoloads nil nil ("latex-preview-pane-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; latex-preview-pane-autoloads.el ends here
