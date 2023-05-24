;;; org-contacts-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-contacts" "org-contacts.el" (0 0 0 0))
;;; Generated autoloads from org-contacts.el

(autoload 'org-contacts-org-complete-function "org-contacts" "\
completion-at-point function to complete @name in `org-mode'.
Usage: (add-hook \\='completion-at-point-functions
                 #\\='org-contacts-org-complete-function nil \\='local)" nil nil)

(autoload 'org-contacts "org-contacts" "\
Create agenda view for contacts matching NAME.

\(fn NAME)" t nil)

(autoload 'org-contacts-setup-completion-at-point "org-contacts" "\
Add `org-contacts-message-complete-function' as a new function
to complete the thing at point." nil nil)

(if (fboundp 'org-link-set-parameters) (org-link-set-parameters "org-contact" :follow #'org-contacts-link-open :complete #'org-contacts-link-complete :store #'org-contacts-link-store :face 'org-contacts-link-face) (if (fboundp 'org-add-link-type) (org-add-link-type "org-contact" 'org-contacts-link-open)))

(autoload 'org-contacts-link-store "org-contacts" "\
Store the contact in `org-contacts-files' with a link." nil nil)

(autoload 'org-contacts-link-open "org-contacts" "\
Open contacts: link type with jumping or searching.

\(fn QUERY)" nil nil)

(autoload 'org-contacts-link-complete "org-contacts" "\
Create a org-contacts link using completion.

\(fn &optional ARG)" nil nil)

(register-definition-prefixes "org-contacts" '("org-contacts-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-contacts-autoloads.el ends here
