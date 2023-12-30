;;;
;;; The Org-Mode setup. One of the most important pieces of
;;; configuration of my emacs setup.
;;;
;;; No support for Org-Mode < 9
;;;
;;; Some of the ideas in this configuration come from the following
;;; post:  http://cachestocaches.com/2020/3/my-organized-life/
;;; ------------------------------------------------------------------


;;;------------ File locations ---------------------------------------
(setq org-directory "~/personal/agenda/")
(setq org-default-notes-file (concat org-directory "agenda.org"))      ;; default capture file
(setq org-default-journal-file (concat org-directory "journal.org"))

(setq org-archive-location (concat org-directory
                                   "ZZ_archived.org"                   ;; archive file
                                   "::"
                                   "* Archived from original file %s"  ;; archive header
                                   ))

(when (not (boundp 'org-agenda-files))
  (setq org-agenda-files (list
                        "~/personal/agenda/agenda.org"    ;; deadlines / appointments /events
                        "~/personal/agenda/notebook.org"  ;; notebook / ideas / projects
                        "~/personal/agenda/journal.org"   ;; daily events and notes
                        )))

;; Refile targets
(setq org-refile-targets
      '((nil :maxlevel . 2)
        (org-agenda-files :maxlevel . 2)))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;;---------------- TAGS states --------------------------------------
(setq org-tag-persistent-alist '(;; Work related (english)
                                 ("project"  . ?p)
                                 ("meeting"  . ?m)
                                 ("class"    . ?c)
                                 (:newline)
                                 ;; Contents
                                 ("@idea"      . ?i)
                                 ("@reference" . ?r)
                                 ("@question"  . ??)
                                 ("@quote"     . ?q)
                                 (:newline)
                                 ;; Personal (italian)
                                 ("fun"       . ?f)
                                 ("viaggi"    . ?v)
                                 ("salute"    . ?s)
                                 ("money"     . ?$)
                                 (:newline)
                                 (:newline)
                                 (:newline)
                                 ))

;;;---------------- TODO states --------------------------------------
(setq org-todo-keyword-faces
   (quote
    (;; Basic
     ("NEXT" :foreground "white" :background "blue" :weight bold)
     ("TODO" :foreground "red" :background "black" :weight bold)
     ("WAIT" :foreground "yellow" :weight bold)
     ("SOMEDAY" :foreground "blue" :background "white" :weight bold)
     ;; Review the entry
     ("REVIEW" . (:foreground "blue" :background "lightgreen" :weight bold))
     ;; Done tags
     ("DONE" :foreground "lightgreen" :weight bold)
     ("CANCELED" :foreground "lightgreen" :strike-through t :weight bold)
     ("DELEGATED" :foreground "cyan" :weight bold)
     )))


(setq org-todo-keywords
      '((sequence "REVIEW" "TODO" "NEXT" "WAIT" "SOMEDAY" "|" "DONE" "CANCELED" "DELEGATED")
        ))

;;;---------------- Basic setup --------------------------------------
(setq
 org-agenda-include-diary nil
 org-agenda-window-setup 'only-window
 org-agenda-restore-windows-after-quit t
 org-agenda-show-future-repeats 'next
 org-list-allow-alphabetical t
 org-log-done t
 org-log-reschedule t
 org-CUA-compatible t
 org-support-shift-select t
 org-cycle-emulate-tab t
 org-cycle-global-at-bob t
 org-popup-calendar-for-date-prompt t
 org-read-date-display-live t
 org-src-fontify-natively t
 org-src-tab-acts-natively t
 org-src-preserve-indentation t
 org-edit-src-content-indentation 0
 org-confirm-babel-evaluate nil
 org-babel-no-eval-on-ctrl-c-ctrl-c nil
 org-use-sub-superscripts (quote {})
 org-M-RET-may-split-line nil
 org-hide-leading-stars t
 org-highlight-latex-and-related '(latex script entities)
 org-highlight-latex-fragments-and-specials t
 )

;;;---------------- Agenda setup --------------------------------------
(setq org-agenda-time-grid
      '((daily today)
        (600 800 1000 1200 1400 1600 1800 2000 2200)
        "    "
        "----------------"))
(setq org-agenda-current-time-string "——————————————⌚⌚⌚—————————————")
(setq org-agenda-search-headline-for-time nil)


;; These tags categorizes a whole big thing, and I don't want all
;; subitems to pop up in the corresponding agenda view.
(setq org-tags-exclude-from-inheritance '("project" "@question"))
(setq org-stuck-projects '("project|@question/-SOMEDAY-DONE"
                           ("CALL" "NEXT" "REVIEW") ()))



(setq org-agenda-custom-commands
      '(("n" "My agenda setting"
         ((agenda "" ((org-agenda-overriding-header "           AGENDA OF THE DAY / DEADLINES in 3 months\n")
                      ;; limits the agenda display to a single day)
                      (org-agenda-span 1)
                      (org-deadline-warning-days 90)        ;; [1]
                      ))
          (agenda "" ;; Birthdays in this week
                  ((org-agenda-overriding-header "           ANNIVERSARIES IN 7 DAYS\n")
                   (org-agenda-span 7)
                   (org-agenda-show-all-dates nil)
                   (org-agenda-start-on-weekday nil)
                   (org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:sexp))))
          (todo "NEXT|CALL|REVIEW"
                   ((org-agenda-overriding-header "           NEXT THING TO DO\n")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                'scheduled))
                    ))
          (tags    "project"
                   ((org-agenda-overriding-header "           PROJECTS STATUS\n")
                    (org-agenda-sorting-strategy '(priority-down))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                'todo '("DONE" "CANCELED" "DELEGATED")))
                    ))
          (tags    "@question"
                   ((org-agenda-overriding-header "           OPEN PROBLEMS\n")
                    (org-agenda-sorting-strategy '(priority-down))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                'todo '("DONE" "CANCELED" "DELEGATED")))
                    ))
          (tags-todo "-project-@question"
                   ((org-agenda-overriding-header "           TODO LIST\n")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                'scheduled
                                                'deadline
                                                'todo '("NEXT" "CALL" "REVIEW")))
                    (org-agenda-sorting-strategy '(priority-down))
                    ))
          ))))



;; Set a key for the agenda view
(defun my-org-agenda-show (&optional arg)
  "Show my custom agenda.

It shows the full view of my custom agenda."
  (interactive "P")
  (org-agenda arg "n"))

(defun init-org-mode--setup ()
  "Setup for org-mode"
  (interactive)
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


  ;; org-babel and export
  (init-org-mode--babel-setup)
  (init-org-mode--latex-export-setup)

  ;; Fix color theme
  (init-org-mode-faces)

  ;; Org-mode communicating with external applications.
  (require 'org-protocol nil t)

  ;; Setup keyboard
  (add-hook 'org-mode-hook    'org-mode/setup-keys/gb)
  (add-hook 'orgtbl-mode-hook 'orgtbl-mode/setup-keys/gb)
  (add-hook 'org-agenda-mode-hook  'org-agenda-mode-setup-local-keys)
  (define-key calendar-mode-map (kbd "RET") 'th-calendar-open-agenda)
  (define-key org-mode-map (kbd "M-<f9>") 'org-export-dispatch)
  (define-key org-mode-map (kbd "<f9>")   '(lambda () (interactive) (org-export-dispatch t)))

  ;; org-capture
  (add-hook 'org-capture-mode-hook
            (lambda ()(select-frame-set-input-focus (selected-frame))))

  ;; citation
  (add-hook 'org-mode-hook 'org-mode/setup-citations)

  ;; Latex fragments scaling
  (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

  ;; Getting out of org-src editing
  (define-key org-src-mode-map (kbd "C-'") 'org-edit-src-exit)
  )


;; Setup of different org-mode auxiliary layout.
(defun org-mode/setup-keys/clean-default ()
  "Remove the auxiliary keys which fight with other modes."

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Capture template configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; support functions

(defun org-capture-URL-data ()
  "Define the structure of a capture note for an external link"
  (let ((title (plist-get org-store-link-plist :description))
        (link  (plist-get org-store-link-plist :link))
        (time  (format-time-string "[%Y-%m-%d %a]" (current-time)))
        (text  (plist-get org-store-link-plist :initial))
        output)
    (with-temp-buffer
      (insert (concat
               "* REVIEW “" title "”\n"
               "  "  time "\n\n"
               "  %?\n\n"))
      (insert
       (if (= (length text) 0)
           (concat "  --- Link: [[" link "][" title "]]"
                   "\n\n  ")
         (concat       "#+BEGIN_QUOTE\n"
                       text
                       "\n#+END_QUOTE\n\n"
                       "--- Source: [[" link "][" title "]]"
                       )))
      (set-fill-column 70)
      (fill-paragraph 'full)
      (setq output (buffer-string)))
    output))


(setq my/org-contacts-template "* REVIEW %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(org-contacts-template-email)
  :END:

  %?
  ")

(setq org-capture-templates
      `(
        ("n" "Notebook note"     entry (file "notebook.org")    "* REVIEW %?\n  %T\n  %i\n  %a\n\n")
        ("t" "Task"              entry (file "agenda.org"  )    "* REVIEW ⌚ %?\n  %T\n\n  %i\n\n")
        ("d" "Deadline"          entry (file "agenda.org")      "* REVIEW ⌚ %?\n  DEADLINE: %^t\n  %U\n\n  %i\n\n")
        ("e" "Event/Appointment" entry (file "agenda.org")      "* %?\n  %^t\n  %i\n\n")
        ("c" "New contact"       entry (file+olp "contacts.org" "CONTATTI") ,my/org-contacts-template :empty-lines 1)
        ("j" "Journal Entry"     entry (file+olp+datetree "journal.org") "* inserito il %U\n\n  %?\n\n%i\n\n")

        ;; ("Q" "File a notebook" entry (file+olp "notebook.org" "PHYSICAL NOTEBOOKS")
        ;;  "* New notebook %? :notebook:%^g\n  %U\n\n  %^{ID}p%^{Formato}p%^{Fogli}p%^{Nome}p\n\n")
        ("w" "External URL" entry (file+olp "notebook.org" "CAPTURED URLS")  "%(org-capture-URL-data)")))

(setq org-default-capture-template "w")


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
    (select-window (get-buffer-window calendar-buffer))))


(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list))
    (delete-other-windows))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )


;; Nice quotes and smartparens
(when (require 'smartparens nil t)
  (sp-local-pair 'org-mode "“" "”")  ;; add so you can jump back and forth and out and in the pair!
  (sp-local-pair 'org-mode "\"" nil :post-handlers '(my-replace-straight-quotes))
  (sp-local-tag  'org-mode "\"" "“" "”" :actions '(wrap))
  (sp-local-pair 'org-mode "$" "$" )
  (sp-local-tag  'org-mode "$" "$" "$" :actions '(wrap))
  (sp-local-pair 'org-mode "/" "/")
  (sp-local-tag  'org-mode "/" "/" "/" :actions '(wrap))
  (sp-local-tag  'org-mode "*" "*" "*" :actions '(wrap))
  (sp-local-tag  'org-mode "=" "=" "=" :actions '(wrap)))

;; Remove annoying auto-completion sources
(defun org-mode/setup-auto-complete ()
  "Set the `ac-sources', in particular remove some annoying ones."
  (interactive)
  (dolist (badSource
           '(ac-source-files-in-current-dir ac-source-filename)
           ac-sources)
    (setq ac-sources (remove badSource ac-sources))))

;; Patch up org-mode support for bibtex

(defun my-org-bibtex-open (path)
  "Visit the bibliography entry on PATH.

If the bibtex entry does not specify a bibtex file in its path,
then the first file in `reftex-default-bibliography' is used."
  (let* ((search (when (string-match "::\\(.+\\)\\'" path)
                   (match-string 1 path)))
         (path (substring path 0 (match-beginning 0))))
    (message (concat "This is path: " path))
    (org-open-file
     (cond ((not path) (car reftex-default-bibliography))
           ((= (length path) 0) (car reftex-default-bibliography))
           (t path))
     t nil search)))

(defun my-org-bibtex-export-handler (path desc format)
  "Converts a bibtex org-mode link into a full LaTeX citation.

Apapted from the blog \"WebLog Pro Olivier Berger\""
  (message "my-org-bibtex-export-handler is called : path = %s, desc = %s, format = %s" path desc format)
  (let* ((search (when (string-match "::#?\\(.+\\)\\'" path)
                   (match-string 1 path)))
         (path (substring path 0 (match-beginning 0))))
    (cond ((eq format 'latex)
           (if (or (not desc)
                   (equal 0 (search "bibtex:" desc)))
               (format "\\cite{%s}" search)
             (format "\\cite[%s]{%s}" desc search)))
          (t
           (if (or (not desc)
                   (equal 0 (search "bibtex:" desc)))
               (format "[%s]" search)
             (format "(%s)" desc search)))
          )))

(defvar my-org-mode-cite-format "[[bibtex:::%l][%2a, %y]]"
  "This is the format of BibTeX entries in org-mode files.")

(defun org-mode/setup-citations ()
  "Setup bibtex links with support for LaTeX export and
for `reftex-default-bibliography'."
  (interactive)
  (turn-on-reftex)
  (set (make-local-variable 'reftex-cite-punctuation) '(", " " & " " et al."))
  (set (make-local-variable 'reftex-cite-format) my-org-mode-cite-format)
  (org-add-link-type "bibtex" 'my-org-bibtex-open 'my-org-bibtex-export-handler))


(defun update-org-latex-fragments ()
  (org-toggle-latex-fragment '(16))
  (let ((text-scale-factor (expt text-scale-mode-step text-scale-mode-amount)))
    (plist-put org-format-latex-options :scale text-scale-factor))
  (org-toggle-latex-fragment '(16)))


(defun init-org-mode-faces ()
  "Setup preferred colors for org mode"
  ;; Set the color
  ;;(require 'color)

  ;; Fontify Quote and Verse
  ;; do not use org-block for verse
  ;; use italic for verse
  (setq org-fontify-quote-and-verse-block t)
  ;; (set-face-attribute 'org-verse nil
  ;;                     :slant 'italic
  ;;                     :inherit t)

  ;; Darken the background color for code blocks
  ;; Not very useful for a black background theme
  ;; (let ((darkbg (color-darken-name
  ;;                (face-attribute 'default :background) 15)))
  ;;   (set-face-attribute 'org-block nil :background darkbg :extend t))

  ;; Code blocks delimiters are oblique and with different color
  (setq org-fontify-whole-block-delimiter-line t)
  ;; (set-face-attribute 'org-block-begin-line nil
  ;;                     :slant 'italic
  ;;                     :foreground "dim gray"
  ;;                     :background "#333333")
  ;; (set-face-attribute 'org-block-end-line nil
  ;;                     :slant 'italic
  ;;                     :foreground "#7F9F7F"
  ;;                     :background "#4F4F4F")
  )


;; Setup for PDF/Latex exports
(defun init-org-mode--latex-export-setup ()
  "Setup minted package for code listing

   Better than default because it manages UTF-8 characters but
   requires pygments installed and -shell-escape option in the
   pdflatex call"
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%bib %b"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))

;; Org babel setup
(defun org-babel-python-strip-session-chars ()
  "Remove >>> and ... from a Python session output."
  (when (and (string=
              "python"
              (org-element-property :language (org-element-at-point)))
             (string-match
              ":session"
              (org-element-property :parameters (org-element-at-point))))

    (save-excursion
      (when (org-babel-where-is-src-block-result)
        (goto-char (org-babel-where-is-src-block-result))
        (end-of-line 1)
        ;(while (looking-at "[\n\r\t\f ]") (forward-char 1))
        (while (re-search-forward
                "\\(>>> \\|\\.\\.\\. \\|: $\\|: >>>$\\)"
                (org-element-property :end (org-element-at-point))
                t)
          (replace-match "")
          ;; this enables us to get rid of blank lines and blank : >>>
          (beginning-of-line)
          (when (looking-at "^$")
            (kill-line)))))))



(defun init-org-mode--babel-setup ()
  "Org-babel configuration. Code in org-mode files!"

  ;; Activate languages (it could be a security RISK!!)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     (shell .t)
     (python . t)
     (sqlite . t)
     (C . t)
     (latex . t)
     (dot . t)
     (gnuplot . t)
     (ditaa . t)
     ))

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (add-hook 'org-babel-after-execute-hook 'org-babel-python-strip-session-chars))



(defun company-org-keywords (command &optional arg &rest ignored)
  "Company-mode backend for #+KEYWORDS

See https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'org-keyword-backend))
    (prefix (and (eq major-mode 'org-mode)
                 (cons (company-grab-line "^#\\+\\(\\w*\\)" 1)
                       t)))
    (candidates (mapcar #'upcase
                        (cl-remove-if-not
                         (lambda (c) (string-prefix-p arg c))
                         (pcomplete-completions))))
    (ignore-case t)
    (duplicates t)))


;;;------------------------- Load -----------------------------------
(use-package org-contrib :pin nongnu)

(use-package org
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :bind (([f5] . org-capture)
         ([f6] . my-org-agenda-show))
  :config
  (init-org-mode--setup))


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package helm-org-rifle
  :bind ([f8] . helm-org-rifle))



(provide 'init-org-mode)
;; Local Variables:
;; mode: emacs-lisp
;; End:
