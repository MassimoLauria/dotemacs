;;;
;;; Utilities for robust and safe emacs usage.
;;;
;;; In some terminals the keybindings may not work properly.
;;;
;;; No support for Org-Mode <= 6.36
;;;------------------------------------------------------------------


;;;------------ File locations ---------------------------------------
(setq org-directory "~/personal/agenda/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-default-journal-file (concat org-directory "journal.org"))
(when (not (boundp 'org-agenda-files))
  (setq org-agenda-files (list org-directory))) ;May be already
                                                ;defined in personal
                                                ;conf file.


;;;---------------- TODO states --------------------------------------
(setq org-todo-keywords
            '((sequence "TODO" "FEEDBACK" "WAIT" "|" "DONE" "CANCELED" "DELEGATED")))


;;;---------------- Basic setup --------------------------------------
(setq
 org-agenda-include-diary nil
 org-log-done t
 org-CUA-compatible t
 org-support-shift-select t
 org-cycle-emulate-tab nil
 org-cycle-global-at-bob t
 org-popup-calendar-for-date-prompt t
 org-read-date-display-live t
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-confirm-babel-evaluate t
 ;; org-babel-no-eval-on-ctrl-c-ctrl-c t
 )


(defun init-org-mode--setup ()
  "Setup for org-mode"
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

  ;; org-babel-setup
  (init-org-mode--babel-setup)
  ;; org-bibtex patch
  (init-org-mode--patch-org-bibtex)

  ;; Org-mode communicating with external applications.
  (require 'org-protocol nil t)

  ;; Wordpress blogging in Org-mode! (with Math!)
  (add-to-list 'load-path (concat default-elisp-3rdparties "/org2blog"))
  (require 'org2blog-autoloads nil t)

  ;; Setup keyboard
  (add-hook 'org-mode-hook    'org-mode/setup-keys/gb)
  (add-hook 'orgtbl-mode-hook 'orgtbl-mode/setup-keys/gb)
  (add-hook 'org-agenda-mode-hook  'org-agenda-mode-setup-local-keys)
  (define-key calendar-mode-map (kbd "RET") 'th-calendar-open-agenda)

  ;; org-capture
  (add-hook 'org-capture-mode-hook
            (lambda ()(select-frame-set-input-focus (selected-frame))))

  ;; org-agenda and calendar in sync
  ;; (add-hook 'calendar-mode-hook 'th-org-agenda-follow-calendar-mode)

  (message "Setup of org-mode"))


;; Setup of different org-mode auxiliary layout.
(defun org-mode/setup-keys/clean-default ()
  "Remove the auxiliary keys which fight with other modes."

  ;; Free tab for completion
  (local-unset-key "\t")
  (local-unset-key [tab])
  (local-unset-key [(tab)])

  ;; Disable the defaults to support CUA-mode.
  (local-unset-key (kbd "<S-up>")    )
  (local-unset-key (kbd "<S-down>")  )
  (local-unset-key (kbd "<S-left>")  )
  (local-unset-key (kbd "<S-right>") )
  (local-unset-key (kbd "<C-S-up>")    )
  (local-unset-key (kbd "<C-S-down>")  )
  (local-unset-key (kbd "<C-S-left>")  )
  (local-unset-key (kbd "<C-S-right>") )
  )


(defun org-mode/setup-keys/gb ()
  "Setup auxiliary org-mode keys for GB keyboard layout.  They are
structured as a reverse L centered on charachter `#' on the right
part of the keyboard.

         ]
     ; ' #

I could not use a reverse T layout because C-[ is low level
translated to an escape sequence.
"

  (interactive)

  (org-mode/setup-keys/clean-default)

  ;; Alternative `org-cycle' keys
  (local-set-key (kbd "M-<tab>")    'org-cycle)
  (local-set-key (kbd "M-SPC")      'org-cycle)

  ;; X window
  (local-set-key (kbd "C-]") 'org-shiftup    )
  (local-set-key (kbd "C-#") 'org-shiftdown  )
  (local-set-key (kbd "C-;") 'org-shiftleft  )
  (local-set-key (kbd "C-'") 'org-shiftright )

  (local-set-key (kbd "M-]") 'org-metaup    )
  (local-set-key (kbd "M-#") 'org-metadown  )
  (local-set-key (kbd "M-;") 'org-metaleft  )
  (local-set-key (kbd "M-'") 'org-metaright )

  (local-set-key (kbd "C-M-]") 'org-shiftmetaup    )
  (local-set-key (kbd "C-M-#") 'org-shiftmetadown  )
  (local-set-key (kbd "C-M-;") 'org-shiftmetaleft  )
  (local-set-key (kbd "C-M-'") 'org-shiftmetaright )

  ;; FIXME: org-shiftdown does not work in XTerm
  )



(defun org-mode/setup-keys/it()
  "Setup auxiliary org-mode keys for IT keyboard layout. They are
structured as a reverse T centered on charachter `à' on the right
part of the keyboard.

       è
     ò à ù
"
  (interactive)

  (org-mode/setup-keys/clean-default)

  ;; Alternative `org-cycle' keys
  (local-set-key (kbd "M-<tab>")    'org-cycle)
  (local-set-key (kbd "M-SPC")      'org-cycle)

  ;; X window
  (local-set-key (kbd "C-è") 'org-shiftup    )
  (local-set-key (kbd "C-à") 'org-shiftdown  )
  (local-set-key (kbd "C-ò") 'org-shiftleft  )
  (local-set-key (kbd "C-ù") 'org-shiftright )

  (local-set-key (kbd "M-è") 'org-metaup    )
  (local-set-key (kbd "M-à") 'org-metadown  )
  (local-set-key (kbd "M-ò") 'org-metaleft  )
  (local-set-key (kbd "M-ù") 'org-metaright )

  (local-set-key (kbd "C-M-]") 'org-shiftmetaup    )
  (local-set-key (kbd "C-M-#") 'org-shiftmetadown  )
  (local-set-key (kbd "C-M-;") 'org-shiftmetaleft  )
  (local-set-key (kbd "C-M-'") 'org-shiftmetaright )

  ;; Xterm fix
  (local-set-key "\e[27;5;232~" 'org-shiftup)
  (local-set-key "\e[27;5;224~" 'org-shiftdown)
  (local-set-key "\e[27;5;242~" 'org-shiftleft)
  (local-set-key "\e[27;5;249~" 'org-shiftright)
  )


(defun orgtbl-mode/setup-keys/it ()
  "Define orgtbl-mode keys for IT keyboard layout"
  (interactive)
  ;; Org Table movements
  (define-key orgtbl-mode-map (kbd "M-ò") 'org-table-move-column-left)
  (define-key orgtbl-mode-map (kbd "M-ù") 'org-table-move-column-right)
  (define-key orgtbl-mode-map (kbd "M-è") 'org-table-move-row-up)
  (define-key orgtbl-mode-map (kbd "M-à") 'org-table-move-row-down)

  (define-key orgtbl-mode-map (kbd "C-M-ò") 'org-table-delete-column)
  (define-key orgtbl-mode-map (kbd "C-M-ù") 'org-table-insert-column)
  (define-key orgtbl-mode-map (kbd "C-M-è") 'org-table-kill-row)
  (define-key orgtbl-mode-map (kbd "C-M-à") 'org-table-insert-row)
  )


(defun orgtbl-mode/setup-keys/gb ()
  "Define orgtbl-mode keys for GB keyboard layout"
  (interactive)
  ;; Org Table movements
  (define-key orgtbl-mode-map (kbd "M-;") 'org-table-move-column-left)
  (define-key orgtbl-mode-map (kbd "M-'") 'org-table-move-column-right)
  (define-key orgtbl-mode-map (kbd "M-]") 'org-table-move-row-up)
  (define-key orgtbl-mode-map (kbd "M-#") 'org-table-move-row-down)

  (define-key orgtbl-mode-map (kbd "C-M-;") 'org-table-delete-column)
  (define-key orgtbl-mode-map (kbd "C-M-'") 'org-table-insert-column)
  (define-key orgtbl-mode-map (kbd "C-M-]") 'org-table-kill-row)
  (define-key orgtbl-mode-map (kbd "C-M-#") 'org-table-insert-row)
  )

;; Normally my private (and translated) configuration is used.
(when (not (boundp 'org-capture-templates))
  (setq org-capture-templates
        '(
          ;; task
          ("t" "task" entry (file "agenda.org")
           "* TODO ⌚ %?\n  %U\n\n  %i\n\n")
          ;; meeting and events
          ("e" "event" entry (file "agenda.org")
           "* %?\n  WHEN %^t\n  %i\n\n")
          ;; notebook
          ("n" "note" entry (file "notebook.org")
           "* %?\n  %U\n  %i\n  %a\n\n")
          ;; org-capture from the web
          ("w" "webpage" entry (file "notebook.org")
         "* “%:description” :webpage:\n  %u\n\n  %?\n\n  ----- Source: %c\n\n  %i")
          ("g" "gmail" entry (file "agenda.org")
         "* “%:description” :mail:\n\n  --- [[gmail:%(org-gmail-extract-msgid (current-kill 0))][link to message]]\n\n  %?\n\n  %i")
        )))

(defun org-gmail-extract-msgid (orgurl)
  "Extract the message ID from a GMail urls"
  (save-match-data
    (string-match "\/\\([0-9a-f]*\\)\]" orgurl)
    (match-string 1 orgurl)))

(setq org-default-capture-template "w")


;; Url abbreviation (and search engine)
(setq org-link-abbrev-alist
      '(("gmail"   . "https://mail.google.com/mail/u/0/#all/%s")
        ("google"  . "http://www.google.com/search?q=%s")
        ("map"     . "http://maps.google.com/maps?q=%s")
        ))


;; Aquamacs misses some org-agenda keybindings!
(defun org-agenda-mode-setup-local-keys()
  "Define/>Undefine of orgtbl-mode keys"
  ;; Org Agenda Left/Right movements
  (define-key org-agenda-mode-map (kbd "<left>") 'org-agenda-earlier)
  (define-key org-agenda-mode-map (kbd "<right>") 'org-agenda-later)
)

(defun th-calendar-open-agenda () ;; by tassilo horn
  (interactive)
  (let* ((calendar-date (or
                         ;; the date at point in the calendar buffer
                         (calendar-cursor-to-date)
                         ;; if there's none, use the curren date
                         (calendar-current-date)))
         (day (time-to-days (encode-time 1 1 1
                                         (second calendar-date)
                                         (first calendar-date)
                                         (third calendar-date))))
         (calendar-buffer (current-buffer)))
    (org-agenda-list nil day 1)
    (select-window (get-buffer-window calendar-buffer))
    (th-org-agenda-follow-calendar-mode t)
    ))


(define-minor-mode th-org-agenda-follow-calendar-mode
  "If enabled, each calendar movement will refresh the org agenda
buffer."
  :lighter " OrgAgendaFollow"
  (if (not (eq major-mode 'calendar-mode))
      (message "Cannot activate th-org-agenda-follow-calendar-mode in %s." major-mode)
    (if th-org-agenda-follow-calendar-mode
        (add-hook 'calendar-move-hook 'th-calendar-open-agenda)
      (remove-hook 'calendar-move-hook 'th-calendar-open-agenda))))



;; Patch org-bibtex-store-link to manage Capitalized Fields.
(defun init-org-mode--patch-org-bibtex ()
  "The original `org-bibtex-store-link' does not accept capitalized fields."
  (if (member 'org-bibtex-store-link org-store-link-functions)
      (progn
        (remove-hook 'org-store-link-functions 'org-bibtex-store-link)
        (add-hook 'org-store-link-functions 'org-bibtex-store-link-patched))))

(defun org-bibtex-store-link-patched ()
  "Store a link to a BibTeX entry."
  (when (eq major-mode 'bibtex-mode)
    (let* ((search (org-create-file-search-in-bibtex))
	   (link (concat "file:" (abbreviate-file-name buffer-file-name)
			 "::" search))
	   (entry (mapcar ; repair strings enclosed in "..." or {...}
		   (lambda(c)
		     (if (string-match
			  "^\\(?:{\\|\"\\)\\(.*\\)\\(?:}\\|\"\\)$" (cdr c))
			 (cons (car c) (match-string 1 (cdr c))) c))
		   (save-excursion
		     (bibtex-beginning-of-entry)
		     (mapcar '(lambda (pair) (cons (downcase (car pair)) (cdr pair)))
               (bibtex-parse-entry))
             ))))
      (org-store-link-props
       :key (cdr (assoc "=key=" entry))
       :author (or (cdr (assoc "author" entry)) "[no author]")
       :editor (or (cdr (assoc "editor" entry)) "[no editor]")
       :title (or (cdr (assoc "title" entry)) "[no title]")
       :booktitle (or (cdr (assoc "booktitle" entry)) "[no booktitle]")
       :journal (or (cdr (assoc "journal" entry)) "[no journal]")
       :publisher (or (cdr (assoc "publisher" entry)) "[no publisher]")
       :pages (or (cdr (assoc "pages" entry)) "[no pages]")
       :url (or (cdr (assoc "url" entry)) "[no url]")
       :year (or (cdr (assoc "year" entry)) "[no year]")
       :month (or (cdr (assoc "month" entry)) "[no month]")
       :address (or (cdr (assoc "address" entry)) "[no address]")
       :volume (or (cdr (assoc "volume" entry)) "[no volume]")
       :number (or (cdr (assoc "number" entry)) "[no number]")
       :annote (or (cdr (assoc "annote" entry)) "[no annotation]")
       :series (or (cdr (assoc "series" entry)) "[no series]")
       :abstract (or (cdr (assoc "abstract" entry)) "[no abstract]")
       :btype (or (cdr (assoc "=type=" entry)) "[no type]")
       :type "bibtex"
       :link link
       :description description))))



(defun init-org-mode--babel-setup ()
  "Org-babel configuration. Code in org-mode files!
Requires Org-mode 7.0"
  (when (>= (string-to-number org-version) 7)

    ;; Activate languages (it could be a security RISK!!)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (emacs-lisp . t)
       (sh .t)
       (python . t)
       (C . t)
       (latex . t)
       (dot . t)
       (gnuplot . t)
       (ditaa . t)
       ))))

;;;------------------------- Load -----------------------------------
(if (and running-GNUEmacs23+
         (require 'org-install nil t))
    (eval-after-load "org" '(init-org-mode--setup)))


(provide 'init-org-mode)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:
