;;; -*- coding: utf-8 -*-
;;;
;;; Setting up facilities for managing the bibliography
;;;
;;;-----------------------------------------------------------------



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


;; 


(provide 'init-bibliography)
