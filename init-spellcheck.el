;;;
;;; Spellcheck facilities, functions and settings.
;;;
;;;-----------------------------------------------------------------


(setq ispell-process-directory (expand-file-name "~/"))

;; Names for aspell dictionary
(setq italian-dict-name "italiano")
(setq english-dict-name "english")

;; Names for Aquamacs dictionary
(when running-Aquamacs
  (setq italian-dict-name "it")
  (setq english-dict-name "en")
  )

(defvar guess-language-rules                                            
  `(                    
    (,english-dict-name . "\\<\\(of\\|the\\|and\\|or\\|how\\)\\>")
    (,italian-dict-name . "\\<\\(un\\|uno\\|una\\|il\\|lo\\|la\\|gli\\|le|\\|per\\|in\\)\\>")
    )                   
  "Alist of rules to determine the language of some text.  Each
rule has the form (CODE . REGEXP) where CODE is a string to
identify the language (probably according to ISO 639), and REGEXP
is a regexp that matches some very common words particular to
that language.  The default language should be listed first.
That will be the language returned when no REGEXP matches, as
would happen for an empty document.")

(defun guess-buffer-language ()                                         
  "Guess language in the current buffer."                               
  (save-excursion 
    (goto-char (point-min))
    (let ((count (map 'list (lambda (x)
                              (cons (count-matches (cdr x)) (car x)))
                      guess-language-rules)))
      (cdr (assoc (car (sort (map 'list 'car count) '>)) 
                  count)))))

(defun guess-language ()
  "Guess language in the current buffer."                               
  (interactive)                                                         
  (message (guess-buffer-language)))

    
(defun set-guessed-dictionary ()
   "It tries to guess the language and to set it as dictionary for ispell"
   (interactive)
   (let ( (language (guess-buffer-language)) )
     (if (null language)
         (message "Language unknown, ispell dictionary unchanged")
       (message "Guessing language: %s" language)
       (ispell-change-dictionary language))))


(defun toggle-it-en ()
  "Swtich between ENGLISH and ITALIAN for spell checking, in the current buffer."                               
  (interactive)   
  (when (boundp 'ispell-local-dictionary)
    (let ( 
          (old-local-language ispell-local-dictionary) 
          new-local-language 
          )
      (setq new-local-language (if (string= old-local-language italian-dict-name)
                                   english-dict-name
                                 italian-dict-name))
      (ispell-change-dictionary  new-local-language)
      (message (concat "Buffer language is " (upcase new-local-language )))
      )
    )
  )


(add-hook 'find-file-hook  'set-guessed-dictionary)


(provide 'init-spellcheck)
;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 



