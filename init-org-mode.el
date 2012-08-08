;;;
;;; Utilities for robust and safe emacs usage.
;;;
;;; In Emacs 22 or in some terminals the keybindings may not work properly.
;;;
;;;------------------------------------------------------------------


;;;------------------------- Load -----------------------------------

;; Aquamacs 2.3 (Emacs 23.3) has an older version of org-mode
(when (and running-MacOSX (not running-GNUEmacs24+))
  (setq MacUser-org-path (concat default-elisp-macosx "/org-7.4/lisp/"))
  (setq MacUser-org-contrib-path (concat default-elisp-macosx "/org-7.4/contrib/lisp"))
  (if (file-directory-p MacUser-org-path)
      (setq load-path (cons MacUser-org-path load-path)))
  (if (file-directory-p MacUser-org-contrib-path)
      (setq load-path (cons MacUser-org-contrib-path load-path)))
)

(if (require 'org nil t)
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

;;;------------ File locations ---------------------------------------
(setq org-directory "~/personal/agenda/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-default-journal-file (concat org-directory "journal.org"))
(when (not (boundp 'org-agenda-files))
  (setq org-agenda-files (list org-directory))) ;May be already
                                                ;defined in personal
                                                ;conf file.

;;;---------------- TODO states --------------------------------------
(when (boundp 'org-version)
  (if (> (string-to-number org-version) 5)
      (setq org-todo-keywords
            '((sequence "TODO" "FEEDBACK" "WAIT" "|" "DONE" "CANCELED" "DELEGATED")))
    (setq org-todo-keywords '("TODO" "FEEDBACK" "WAIT" "DONE")
          org-todo-interpretation 'type)))


;;;---------------- Basic setup --------------------------------------
(setq
 org-log-done t
 org-CUA-compatible t
 org-support-shift-select t
 org-cycle-emulate-tab nil
 org-cycle-global-at-bob t
)



;;;---------------- Global keys --------------------------------------
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)




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


(add-hook 'org-mode-hook    'org-mode/setup-keys/gb)
(add-hook 'orgtbl-mode-hook 'orgtbl-mode/setup-keys/gb)

;; Link org-mode to remember-mode (which is not present on Emacs <22)
(when (fboundp 'org-remember-insinuate)
  (org-remember-insinuate)
)

;; Org-protocol to make org-mode to interact with other applications
(require 'org-protocol nil t)          ;; Does not exists on Emacs22

;; Autofocus and raise the Emacs frame which should get the input.
(when (boundp 'org-remember-mode-hook)
  (add-hook 'org-remember-mode-hook
            (lambda ()(select-frame-set-input-focus (selected-frame)))))


;; Normally my private (and translated) configuration is used.
(when (not (boundp 'org-remember-templates))
  (setq org-remember-templates
        '(
          ("journal"        ?j "* %? %t\n\n  %i\n  %a\n\n" "journal.org")
          ("note"           ?n "* %? %T\n\n  %i\n  %a\n\n" "notes.org")
          ("meeting"        ?m "* TODO ⌚ %? %T\n\n  %i\n\n\n" "notes.org")
          ("deadline"       ?d "* TODO ⌚ %? %U\n  DEADLINE: %t\n\n  %i\n\n" "notes.org")
          ("event"          ?e "* %? %t--%t\n\n  %i\n  %a\n\n" "notes.org")
          ("webpage"        ?w "* %^{Title}\n\n  Source: %u, %c\n\n  %i" nil "notes.org")
          )
        )
  )


;; Aquamacs miss some org-agenda keybindings!
(defun org-agenda-mode-setup-local-keys()
  "Define/>Undefine of orgtbl-mode keys"
  ;; Org Agenda Left/Right movements
  (define-key org-agenda-mode-map (kbd "<left>") 'org-agenda-earlier)
  (define-key org-agenda-mode-map (kbd "<right>") 'org-agenda-later)
)
(add-hook 'org-agenda-mode-hook 'org-agenda-mode-setup-local-keys)



;; Wordpress blogging in Org-mode! (with Math!)
(when (fboundp 'org-mode)
  (add-to-list 'load-path (concat default-elisp-3rdparties "/org2blog"))
  (require 'org2blog-autoloads nil t))


;; Calendar navigation plus agenda
(setq org-popup-calendar-for-date-prompt t)
(setq org-read-date-display-live t)

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
    (org-agenda-list nil day)
    (select-window (get-buffer-window calendar-buffer))
    (th-org-agenda-follow-calendar-mode t)
    ))

(add-hook 'calendar-mode-hook
          (lambda ()
            (define-key calendar-mode-map (kbd "RET") 'th-calendar-open-agenda)))


(define-minor-mode th-org-agenda-follow-calendar-mode
  "If enabled, each calendar movement will refresh the org agenda
buffer."
  :lighter " OrgAgendaFollow"
  (if (not (eq major-mode 'calendar-mode))
      (message "Cannot activate th-org-agenda-follow-calendar-mode in %s." major-mode)
    (if th-org-agenda-follow-calendar-mode
        (add-hook 'calendar-move-hook 'th-calendar-open-agenda)
      (remove-hook 'calendar-move-hook 'th-calendar-open-agenda))))

;; (add-hook 'calendar-mode-hook 'th-org-agenda-follow-calendar-mode)




;; Patch org-bibtex-store-link to manage Capitalized Fields.
(require 'org-bibtex)
(defun org-bibtex-store-link ()
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


;; Org-babel configuration. Code in org-mode files!
;; Requires Org-mode 7.0
(when (>= (string-to-number org-version) 7)

  ;; Configuration
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  ;; (setq org-confirm-babel-evaluate t)
  ;; (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

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
     ))

  ) ;; Org babel for Org-mode > 7.0


(provide 'init-org-mode)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:
