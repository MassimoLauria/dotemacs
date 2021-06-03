;;; magic-latex-buffer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magic-latex-buffer" "magic-latex-buffer.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from magic-latex-buffer.el

(autoload 'magic-latex-buffer "magic-latex-buffer" "\
Minor mode that highlights latex document magically.

If called interactively, enable Magic-Latex-Buffer mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magic-latex-buffer" '("jit-lock-fontify-now" "magic-latex-" "ml/")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magic-latex-buffer-autoloads.el ends here
