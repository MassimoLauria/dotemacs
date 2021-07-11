;;; fira-code-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fira-code-mode" "fira-code-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from fira-code-mode.el

(autoload 'fira-code-mode "fira-code-mode" "\
Fira Code ligatures minor mode

If called interactively, toggle `Fira-Code mode'.  If the prefix
argument is positive, enable the mode, and if it is zero or
negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-fira-code-mode 'globalized-minor-mode t)

(defvar global-fira-code-mode nil "\
Non-nil if Global Fira-Code mode is enabled.
See the `global-fira-code-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-fira-code-mode'.")

(custom-autoload 'global-fira-code-mode "fira-code-mode" nil)

(autoload 'global-fira-code-mode "fira-code-mode" "\
Toggle Fira-Code mode in all buffers.
With prefix ARG, enable Global Fira-Code mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if ARG is
omitted or nil.

Fira-Code mode is enabled in all buffers where `fira-code-mode'
would do it.

See `fira-code-mode' for more information on Fira-Code mode.

\(fn &optional ARG)" t nil)

(autoload 'fira-code-mode-set-font "fira-code-mode" "\
Setup Fira Code Symbols font.
This function isn't normally required, but if the range #Xe100 to #Xe16f is
being rendered by some other font besides Fira Code Symbol, then this function
will ensure that this range is resolved using the Fira Code Symbol font
instead." nil nil)

(autoload 'fira-code-mode-install-fonts "fira-code-mode" "\
Helper function to download and install the latests fonts based on OS.
When PFX is non-nil, ignore the prompt and just install

\(fn &optional PFX)" t nil)

(register-definition-prefixes "fira-code-mode" '("fira-code-mode-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fira-code-mode-autoloads.el ends here
