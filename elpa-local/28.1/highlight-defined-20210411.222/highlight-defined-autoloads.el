;;; highlight-defined-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "highlight-defined" "highlight-defined.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from highlight-defined.el

(autoload 'highlight-defined-mode "highlight-defined" "\
Minor mode for highlighting known Emacs Lisp functions and variables.

Toggle highlight defined mode on or off.

With a prefix argument ARG, enable highlight defined mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "highlight-defined" '("highlight-defined-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highlight-defined-autoloads.el ends here
