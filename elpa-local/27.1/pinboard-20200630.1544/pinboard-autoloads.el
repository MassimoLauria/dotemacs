;;; pinboard-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pinboard" "pinboard.el" (0 0 0 0))
;;; Generated autoloads from pinboard.el

(autoload 'pinboard-add "pinboard" "\
Add a new pin to Pinboard." t nil)

(autoload 'pinboard-visit-pinboard "pinboard" "\
Visit pinboard.in itself." t nil)

(autoload 'pinboard-add-for-later "pinboard" "\
Quickly add URL for later review and reading.

This command simply prompts for a URL and adds it to Pinboard as
private and unread, so you can come back to it and look at it
later.

\(fn URL)" t nil)

(autoload 'pinboard "pinboard" "\
Browse your Pinboard pins.

Key bindings that are active in the pin list include:

\\{pinboard-mode-map}" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "pinboard" '("pinboard-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pinboard-autoloads.el ends here
