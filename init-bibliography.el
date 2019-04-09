;;; -*- coding: utf-8 -*-
;;;
;;; Setting up facilities for managing the bibliography
;;;
;;;-----------------------------------------------------------------


;; Bibfiles and folder

;; For reftex
(setq reftex-default-bibliography '("~/lavori/latex/bibliographies/theoryofcomputing.bib"))

(setq bibtex-completion-bibliography "~/lavori/latex/bibliographies/theoryofcomputing.bib"
      bibtex-completion-library-path "~/cloud/Papers/"
      bibtex-completion-notes-path "~/lavori/latex/bibliographies/notes.org"
      bibtex-completion-pdf-field "file"
      )

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


;;
;; Setup the filename generation for bibtex entries
;;
;; Examples:
;;   Surname, Surname (2014) - This is a title.pdf         -- up to two authors
;;   Surname, Surname, et al (2014) - This is a title.pdf  -- more than two authors
;;
(defun bibtex-generate-filename ()
  "docstring"
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
        (fulltitle (bibtex-autokey-get-field "title"))
        )

    (unless (string-empty-p fulltitle) 
      (concat (bibtex-generate-autokey) ") - " fulltitle))
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
         (pdf (expand-file-name (completing-read "Rename pdf to: "
                                                 (list (concat newname "." ext)))
                                path))
         (filefieldcontent (bibtex-autokey-get-field bibtex-completion-pdf-field))
         (filefieldcontent (if (string-empty-p filefieldcontent)
                               ""
                             (concat filefieldcontent ";"))))
    ;; Copy the file in the right location
    (condition-case nil
        (copy-file file pdf 1)
      (error (message ("Error copying " file " to " pdf))))
    (bibtex-set-field bibtex-completion-pdf-field (concat filefieldcontent
                                                          ":"
                                                          (file-name-nondirectory pdf)
                                                          ":"
                                                          (upcase ext)))))

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


(use-package bibtex
  :bind (:map bibtex-mode-map
              ("M-q" . bibtex-fill-entry)
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


(use-package ivy-bibtex
  :commands (ivy-bibtex)
  :bind ("C-c b" . ivy-bibtex)
  :config
  (setf (cdr (assoc 'org-mode
                    bibtex-completion-format-citation-functions))
        'bibtex-completion-format-citation-cite)
  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)
  (setq bibtex-completion-cite-prompt-for-optional-arguments nil))

(provide 'init-bibliography)
