;;; -*- coding: utf-8 -*-
;;;
;;; Setting up facilities for managing the bibliography
;;;
;;;-----------------------------------------------------------------


;; Bibfiles and folder

;; For reftex
(setq reftex-default-bibliography '("~/lavori/latex/bibliographies/theoryofcomputing.bib"))

;; For org-ref
(setq org-ref-bibliography-notes "~/lavori/latex/bibliographies/notes.org"
      org-ref-default-bibliography '("~/lavori/latex/bibliographies/theoryofcomputing.bib")
      org-ref-pdf-directory "~/Dropbox/Library/articles/")

(setq bibtex-completion-bibliography "~/lavori/latex/bibliographies/theoryofcomputing.bib"
      bibtex-completion-library-path "~/Dropbox/Library/articles/"
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
(defun bibtex-generate-filename (args)
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

    (message (concat (bibtex-generate-autokey) ") - " fulltitle ".pdf"))
    ))


;; Open PDF files
(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)




(setq doi-utils-open-pdf-after-download t)
(use-package org-ref
  :commands (org-ref doi-utils)
  :init
  (setq org-ref-get-pdf-filename-function 'bibtex-generate-filename)
  )

(provide 'init-bibliography)
