;;; writegood-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "writegood-mode" "writegood-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from writegood-mode.el

(autoload 'writegood-reading-ease "writegood-mode" "\
Flesch-Kincaid reading ease test in the region bounded by START and END.

Scores roughly between 0 and 100.

\(fn &optional START END)" t nil)

(autoload 'writegood-grade-level "writegood-mode" "\
Flesch-Kincaid grade level test. Converts reading ease score to a grade level (Score ~ years of school needed to read passage).

\(fn &optional START END)" t nil)

(autoload 'writegood-mode "writegood-mode" "\
Colorize issues with the writing in the buffer.

This is a minor mode.  If called interactively, toggle the
`Writegood mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `writegood-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "writegood-mode" '("writegood-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; writegood-mode-autoloads.el ends here
