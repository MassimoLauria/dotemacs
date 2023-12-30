;;; nerd-icons-completion-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nerd-icons-completion" "nerd-icons-completion.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from nerd-icons-completion.el

(autoload 'nerd-icons-completion-marginalia-setup "nerd-icons-completion" "\
Hook to `marginalia-mode-hook' to bind `nerd-icons-completion-mode' to it." nil nil)

(defvar nerd-icons-completion-mode nil "\
Non-nil if Nerd-Icons-Completion mode is enabled.
See the `nerd-icons-completion-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `nerd-icons-completion-mode'.")

(custom-autoload 'nerd-icons-completion-mode "nerd-icons-completion" nil)

(autoload 'nerd-icons-completion-mode "nerd-icons-completion" "\
Add icons to completion candidates.

This is a minor mode.  If called interactively, toggle the
`Nerd-Icons-Completion mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='nerd-icons-completion-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "nerd-icons-completion" '("nerd-icons-completion-completion-metadata-get"))

;;;***

;;;### (autoloads nil nil ("nerd-icons-completion-pkg.el") (0 0 0
;;;;;;  0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nerd-icons-completion-autoloads.el ends here
