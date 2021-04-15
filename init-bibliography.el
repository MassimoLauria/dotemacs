;;; -*- coding: utf-8 -*-
;;;
;;; Setting up facilities for managing the bibliography
;;;
;;;-----------------------------------------------------------------


;; Main bibliography file
(defconst mxl-main-bibtex-file "~/lavori/latex/bibliographies/theoryofcomputing.bib"
  "My main bibliography file.")

;; For reftex
(setq reftex-default-bibliography (list mxl-main-bibtex-file))

(setq bibtex-completion-bibliography mxl-main-bibtex-file
      bibtex-completion-library-path "~/cloud/Papers/"
      bibtex-completion-notes-path "~/lavori/latex/bibliographies/papernotes.org"
      bibtex-completion-pdf-field "file")


;; Bib entries are imported via biblio.el
;;
;; which nees a bit of setup, especially for ArXiv
(use-package biblio
  :defer t
  :init
  (setq biblio-arxiv-bibtex-header "misc") ;; default "@Online" is non standard
  (setq biblio-cleanup-bibtex-function 'bibtex-clean-entry)
  (setq biblio-download-directory "~/Downloads/"))

;;
;; Setup the key generation for bibtex files
;;
;; Examples:
;;   SurnameSurname2014TitleWord  -- up to two authors
;;   SurnameEtAl2014TitleWord     -- more than three authors
;;
(setq bibtex-autokey-edit-before-use nil
      ;; Name part
      ;;
      bibtex-autokey-names 1
      bibtex-autokey-names-stretch 1
      bibtex-autokey-name-separator ""
      bibtex-autokey-name-case-convert 'capitalize
      bibtex-autokey-additional-names "EtAl"

      ;; Year part
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-length 4

      ;; Title part
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 0
      bibtex-autokey-titleword-separator ""
      bibtex-autokey-titleword-case-convert 'capitalize
      bibtex-autokey-titleword-length nil)

(defun bibtex-generate-filename ()
  "Generate the PDF file name for a bibtex entry.

Cleans up colons in the filename

Examples:
  Surname, Surname (2014) - This is a title.pdf         -- up to two authors
  Surname, Surname, et al (2014) - This is a title.pdf  -- more than two authors"
  (interactive "P")
  (let (( bibtex-autokey-names  2 )
        ( bibtex-autokey-names-stretch 1)
        ( bibtex-autokey-name-separator ", ")
        ( bibtex-autokey-name-case-convert 'capitalize)
        ( bibtex-autokey-additional-names " et al")

        ;; Year part
        (bibtex-autokey-name-year-separator " (")
        (bibtex-autokey-year-length 4)
        (bibtex-autokey-year-title-separator "")

        ;; Title part
        (bibtex-autokey-titlewords 0)
        (forbidden ":;")  ;; char that we are going to substitute
        (fulltitle (bibtex-autokey-get-field "title"))
        )

    (unless (string-empty-p fulltitle)
      (concat (bibtex-generate-autokey) ") - "
              (replace-regexp-in-string (concat "[" forbidden "]")
                                        "_" fulltitle)))
    ))


;; Bibtex Entry format
(setq bibtex-align-at-equal-sign t
      bibtex-field-indentation   2
      bibtex-text-indentation    20
      bibtex-comma-after-last-field nil
      bibtex-entry-format '(opts-or-alts
                            required-fields
                            numerical-fields
                            whitespace
                            inherit-booktitle
                            last-comma
                            realign
                            unify-case
                            sort-fields)
      )



;;
;; Code stolen and adapted from Org-Ref
;; https://github.com/jkitchin/org-ref

(defun bibtex-set-field (field value &optional nodelim)
  "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'."
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
        ;; we found a field
        (progn
          (goto-char (car (cdr found)))
          (when value
            (bibtex-kill-field)
            (bibtex-make-field field nil nil nodelim)
            (backward-char)
            (insert value)))
      ;; make a new field
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      (bibtex-next-field nil)
      (forward-char)
      (bibtex-make-field field nil nil nodelim)
      (backward-char)
      (insert value))))


(defun orcb-key-comma ()
  "Make sure there is a comma at the end of the first line."
  (bibtex-beginning-of-entry)
  (end-of-line)
  ;; some entries do not have a key or comma in first line. We check and add it,
  ;; if needed.
  (unless (string-match ", *$" (thing-at-point 'line))
    (end-of-line)
    (insert ",")))


(defun orcb-key ()
  "Replace the key in the entry."
  (let ((key (bibtex-generate-autokey)))
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (save-excursion
	    (bibtex-search-entry key))
      (save-excursion
	(bibtex-search-entry key)
	(bibtex-copy-entry-as-kill)
	(switch-to-buffer-other-window "*duplicate entry*")
	(bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))

    (insert key)
    (kill-new key)))

;;
;; End of code stolen and adapted from Org-Ref
;; https://github.com/jkitchin/org-ref


(defun mybibtex-clean-pages-dashes ()
  "Normalize the dash in the page number to '\\nobreakdash--'."
  (let ((pages (bibtex-autokey-get-field "pages")))
    (bibtex-set-field "pages"
                      (replace-regexp-in-string "\\(\\\\nobreakdash\\)?--*"
                                                "\\\\nobreakdash--" pages))))

(defun mybibtex-add-file-to-library (&optional file)
  "Associate a document to the bibtex entry currently under cursor.

  ISSUES:
  1. Documents  can only be added to the file field.
  2. There are not alternative actions when the target filename already exists.
"
  (interactive "f")
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry t))
         (newname (bibtex-generate-filename))
         (path (-flatten (list bibtex-completion-library-path)))
         (path (if (cdr path)
                   (completing-read "Add pdf to: " path nil t)
                 (car path)))
         (ext (file-name-extension file))
         (pdf (expand-file-name (concat newname "." ext)
                                path))
         (filefieldcontent (bibtex-autokey-get-field bibtex-completion-pdf-field))
         (filefieldcontent (if (string-empty-p filefieldcontent)
                               ""
                             (concat filefieldcontent ";"))))
    ;; Copy the file in the right location
    (condition-case nil
        (when (yes-or-no-p (concat "Attach paper " pdf))
          (copy-file file pdf 1)
          (bibtex-set-field bibtex-completion-pdf-field (concat filefieldcontent
                                                                ":"
                                                                (file-name-nondirectory pdf)
                                                                ":"
                                                                (upcase ext))))
      (error (message ("Error copying " file " to " pdf))))))

(defun mybibtex-dnd-add-file-mac (event)
  "Attach a the file to a Bibtex entry it is dragged on"
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (arg (car (cdr (cdr event))))
         (type (car arg))
         (data (car (cdr arg)))
         (f (cond ((eq type 'file) data)
                  (t (dnd-get-local-file-name data t))
                  )))
    (if (and f (file-readable-p f))
	(mybibtex-add-file-to-library f)
      (error "Can not read %s" uri))))


(defun mybibtex-dnd-add-file-linux (uri _action)
  "Attach a the file to a Bibtex entry it is dragged on"
  (let* ((f (dnd-get-local-file-name uri t)))
    (if (and f (file-readable-p f))
	(mybibtex-add-file-to-library f)
      (error "Can not read %s" uri))))

(defun mybibtex-dnd-setup ()
  (setq-local dnd-protocol-alist '(("^file:" . mybibtex-dnd-add-file-linux))))

(defun mybibtex-open-pdf ()
  "Opens the PDF files mentioned in the bibtex entry"
  (interactive)
  (bibtex-completion-open-pdf (list (bibtex-completion-key-at-point))))

(defun mybibtex-open-url-or-doi ()
  "Opens the PDF, the URL or the DOI mentioned in the bibtex entry"
  (interactive)
  (bibtex-completion-open-url-or-doi (list (bibtex-completion-key-at-point))))

(defun mybibtex-open-any ()
  "Opens the PDF, the URL or the DOI mentioned in the bibtex entry"
  (interactive)
  (bibtex-completion-open-any (list (bibtex-completion-key-at-point))))

(use-package bibtex
  :bind (:map bibtex-mode-map
              ("M-q" . bibtex-fill-entry)
              ("C-c C-o" . mybibtex-open-any)
              ("<drag-n-drop>" . mybibtex-dnd-add-file-mac))

  :config
  (setq bibtex-autokey-name-case-convert 'capitalize
        bibtex-autokey-titleword-case-convert 'capitalize)
  (add-hook 'bibtex-clean-entry-hook 'orcb-key)
  (add-hook 'bibtex-clean-entry-hook 'orcb-key-comma)
  (add-hook 'bibtex-clean-entry-hook 'mybibtex-clean-pages-dashes)
  (add-hook 'bibtex-clean-entry-hook 'bibtex-fill-entry 'append)
  (add-hook 'bibtex-mode-hook
            (lambda () (setq fill-column 999999)))
  (add-hook 'bibtex-mode-hook 'mybibtex-dnd-setup))


(use-package helm-bibtex
  :commands (helm-bibtex bibtex-completion-open-pdf)
  :bind ("C-c b" . helm-bibtex)
  :config

  ;; Use latex citation in org files
  (setf (cdr (assoc 'org-mode
                    bibtex-completion-format-citation-functions))
        'bibtex-completion-format-citation-cite)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil)

  ;; Make 'Insert citation' the first (and thus the default) action
  (helm-delete-action-from-source  "Insert citation" helm-source-bibtex)
  (helm-add-action-to-source       "Insert citation" 'helm-bibtex-insert-citation helm-source-bibtex 0)
  ;; Default height for helm-bibtex window
  (setq helm-bibtex-full-frame nil)

  ;; Workaround because
  (defalias 'bibtex-completion-find-pdf-in-field 'mxl-find-pdf-in-field-workaround
    "Massimo's workaround")
  )


(defun mxl-find-pdf-in-field-workaround (key-or-entry)
  "Workaround by Masssimo.

I had to put here the old implementation because it was broken. See Bug #370 in
https://github.com/tmalsburg/helm-bibtex

Here KEY-OR-ENTRY is either a bibtex entry or a bibtex key."
  (when bibtex-completion-pdf-field
    (let* ((entry (if (stringp key-or-entry)
                      (bibtex-completion-get-entry1 key-or-entry t)
                    key-or-entry))
           (value (bibtex-completion-get-value bibtex-completion-pdf-field entry)))
      (cond
       ((not value) nil)         ; Field not defined.
       ((f-file? value) (list value))   ; A bare full path was found.
       ((-any 'f-file? (--map (f-join it (f-filename value)) (-flatten bibtex-completion-library-path))) (-filter 'f-file? (--map (f-join it (f-filename value)) (-flatten bibtex-completion-library-path))))
       (t                               ; Zotero/Mendeley/JabRef/Calibre format:
        (let ((value (replace-regexp-in-string "\\([^\\]\\)[;]" "\\1\^^" value)))
          (cl-loop  ; Looping over the files:
           for record in (s-split "\^^" value)
                                        ; Replace unescaped colons by field separator:
           for record = (replace-regexp-in-string "\\([^\\]\\|^\\):" "\\1\^_" record)
                                        ; Unescape stuff:
           for record = (replace-regexp-in-string "\\\\\\(.\\)" "\\1" record)
                                        ; Now we can safely split:
           for record = (s-split "\^_" record)
           for file-name = (nth 0 record)
           for path = (or (nth 1 record) "")
           for paths = (if (s-match "^[A-Z]:" path)
                           (list path)                 ; Absolute Windows path
                                        ; Something else:
                         (append
                          (list
                           path
                           file-name
                           (f-join (f-root) path) ; Mendeley #105
                           (f-join (f-root) path file-name)) ; Mendeley #105
                          (--map (f-join it path)
                                 (-flatten bibtex-completion-library-path)) ; Jabref #100
                          (--map (f-join it path file-name)
                                 (-flatten bibtex-completion-library-path)))) ; Jabref #100
           for result = (-first (lambda (path)
                                  (if (and (not (s-blank-str? path))
                                           (f-exists? path))
                                      path nil)) paths)
           if result collect result)))))))


(provide 'init-bibliography)
