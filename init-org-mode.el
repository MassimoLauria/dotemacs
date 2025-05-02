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
(setq org-agenda-files
              '("~/personal/agenda/agenda.org"       ;; deadlines / appointments /events
                "~/personal/agenda/notebook.org"     ;; notebook / ideas personal
                "~/personal/agenda/compscience.org"  ;; research /workflow
                "~/personal/agenda/ricorrenze.org"   ;; anniversaries / holidays
                ))
(setq org-default-notes-file "~/personal/agenda/agenda.org")
(setq org-default-journal-file nil)  ;; No journal

(setq org-archive-location (concat "~/personal/agenda/ZZ_archived.org"    ;; file
                                   "::* Archived from original file %s")) ;; header

(add-to-list 'org-agenda-files "~/lavori/latex/bibnotes.org" t) ;; papers in bibtex


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
 org-fontify-quote-and-verse-block t
 org-fontify-whole-block-delimiter-line t)

;;;---------------- Agenda setup --------------------------------------
(setq org-agenda-time-grid
      '((daily today)
        (800 1200 1600 2000 2400)
        "    "
        "·································"))
(setq org-agenda-current-time-string "——————————————⌚⌚⌚—————————————")
(setq org-agenda-search-headline-for-time nil)

;; These tags categorizes a whole big thing, and I don't want all
;; subitems to pop up in the corresponding agenda view.
(setq org-tags-exclude-from-inheritance '("project" "@question"))
(setq org-stuck-projects '("project|@question/-SOMEDAY-DONE"
                           ("CALL" "NEXT" "REVIEW") ()))



(setq org-agenda-custom-commands
      '(("n" "My agenda setting"
         ((agenda "" ((org-agenda-overriding-header "           NEXT FEW DAYS / DEADLINES in 3 months\n")
                      ;; limits the agenda display to a single day)
                      ;;(org-agenda-span 1)
                      (org-agenda-span 10)
                      (org-agenda-start-on-weekday nil)
                      (org-agenda-show-all-dates nil)
                      (org-agenda-start-day nil)
                      (org-deadline-warning-days 90)        ;; [1]
                      ))
          (todo "NEXT|CALL|REVIEW"
                   ((org-agenda-overriding-header "           NEXT THING TO DO\n")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                'scheduled))
                    ))
          (tags    "project"
                   ((org-agenda-overriding-header "           PROJECTS\n")
                    (org-agenda-sorting-strategy '(priority-down))
                    (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                'todo '("DONE" "CANCELED" "DELEGATED")))
                    ))
          (tags-todo "-project"
                   ((org-agenda-overriding-header "           TODO LIST\n")
                    (org-agenda-skip-function '(org-agenda-skip-entry-if
                                                'scheduled
                                                'deadline
                                                'todo '("NEXT" "CALL" "REVIEW")))
                    (org-agenda-sorting-strategy '(priority-down))
                    ))
          ))))

(defun init-org-mode--setup ()
  "Setup for org-mode"
  (interactive)

  ;; org-babel and export
  (init-org-mode--babel-setup)

  ;; Setup keyboard
  (define-key calendar-mode-map (kbd "RET") 'th-calendar-open-agenda)

  ;; citation
  (add-hook 'org-mode-hook 'org-mode/setup-citations)

  ;; Latex fragments scaling
  (add-hook 'text-scale-mode-hook 'update-org-latex-fragments)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Capture template configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-capture-templates
      `(
        ("t" "Task      (agenda.org)"  entry (file "agenda.org"  )    "* REVIEW ⌚ %?\n  %T\n\n  %i\n\n")
        ("d" "Deadline  (agenda.org)"  entry (file "agenda.org")      "* REVIEW ⌚ %?\n  DEADLINE: %^t\n  %U\n\n  %i\n\n")
        ("m" "Email       (new file)"  plain (function mxl/create-mail-draft) ""    ; Just create an email draft and stop capture
         :immediate-finish t
         :jump-to-captured t
         :empty-lines-after 3
         )
        ("n" "Note    (notebook.org)"  entry (file "notebook.org")    "* REVIEW %?\n  %T\n  %i\n  %a\n\n")
        ))




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




;; Setup minted package for code listing in latex exports
(use-package ox-latex
  :config
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process
        '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%bib %b"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))

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

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))



;;;------------------------- Load -----------------------------------
(use-package org-contrib :pin nongnu)

(use-package org
  :pin gnu
  :mode ("\\.org\\'" . org-mode)
  :bind (([f5] . org-capture)
         ([f6] . (lambda() (interactive) (org-agenda nil "n")))
         ([f7] . mxl/contacts)
         ([f8] . mxl/search-agenda)
         :map org-mode-map
         ;; Avoid fights with cua-mode
         ("<S-up>". nil)
         ("<S-down>". nil)
         ("<S-left>". nil)
         ("<S-right>". nil)
         ("<C-S-up>". nil)
         ("<C-S-down>". nil)
         ("<C-S-left>". nil)
         ("<C-S-right>". nil)
         ;;
         ("C-c   o" . org-open-at-point)
         ("C-c C-o" . org-open-at-point)
         ("C-c s" . org-store-link)
         ("C-c i" . org-insert-link)
         ;;
         :map org-src-mode-map
         ("C-'" . org-edit-src-exit)
         :map org-agenda-mode-map
         ("<left>"  . org-agenda-earlier)
         ("<right>" . org-agenda-later)
         )

  :custom
  (org-bookmark-names-plist . nil)
  :config
  (init-org-mode--setup)
  )

(use-package org-bullets
  :hook org-mode)


(use-package org-contacts
  :commands (org-contacts org-contacts-anniversaries)
  :init
  ;; addresses
  (setq org-contacts-files '("~/personal/agenda/contacts.org")))

(defun mxl/contacts ()
  "Search and select a personal contact"
  (interactive)
  (let ((starting-point (current-buffer)))
  (condition-case
      err
      (progn (find-file "~/personal/agenda/contacts.org")
             (consult-org-heading))
    (t (switch-to-buffer starting-point)))))

(defun mxl/search-agenda ()
  "Search (i.e. ripgrep) stuff in my agenda files"
  (interactive)
  (consult-ripgrep "~/personal/agenda/"))


(provide 'init-org-mode)
;; Local Variables:
;; mode: emacs-lisp
;; End:
