;;; magic-latex-buffer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magic-latex-buffer" "magic-latex-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from magic-latex-buffer.el

(autoload 'magic-latex-buffer "magic-latex-buffer" "\
Minor mode that highlights latex document magically.

If called interactively, toggle `Magic-Latex-Buffer mode'.  If
the prefix argument is positive, enable the mode, and if it is
zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "magic-latex-buffer" '("jit-lock-fontify-now" "magic-latex-" "ml/"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magic-latex-buffer-autoloads.el ends here
