;;;
;;; Spellcheck facilities, functions and settings.
;;;
;;;-----------------------------------------------------------------


;; User variables initialised  -------------------------------------------------------------


;; English
(defvar spellcheck-english-regexp "\\<\\(of\\|the\\|and\\|or\\|how\\)\\>"
  "If a buffer match this REGEXP it is supposed to be in english.")
(defvar spellcheck-english-names '("british" "american" "english" "en_GB" "en_US" "en" )
  "Possible names for an ENGLISH dictionary.")


;; Italian
(defvar spellcheck-italian-regexp "\\<\\(un\\|uno\\|una\\|il\\|lo\\|la\\|gli\\|le|\\|per\\|in\\)\\>"
  "If a buffer match this REGEXP it is supposed to be in italian.")
(defvar spellcheck-italian-names '("italiano" "italian" "it" "it_IT")
  "Possible names for an ITALIAN dictionary.")


;; Variables automatically initialised  -------------------------------------------------------------

(defvar spellcheck-languages nil
  "All languages which are available to the spellchecking facilities like:

   -  language guessing
   -  dictionary cycling

   By default the list is empty and is filled at init time, by a
   setup function."
)

(defvar spellcheck-guessing-rules nil
  "Alist of rules to determine the language of some text.  Each
rule has the form (CODE . REGEXP) where CODE is a string to
identify the language (probably according to ISO 639), and REGEXP
is a regexp that matches some very common words particular to
that language.  The default language should be listed first.
That will be the language returned when no REGEXP matches, as
would happen for an empty document.")

;; Setup ----------------------------------------------------------------------------------------

(when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    (setq ispell-list-command "list"))


(setq ispell-process-directory (expand-file-name "~/"))

(defun spellcheck-first-valid-dictionary (dict-seq)
  "Given a list of dictionary names, try them until one is valid. If none is valid, return `nil'"
  (catch 'break-on-good-dict
    (loop for dict in dict-seq do
          (ignore-errors
            (ispell-change-dictionary dict)
            (throw 'break-on-good-dict dict)))))


;; Setup of guessing rules
(let (
      (en-dict (spellcheck-first-valid-dictionary spellcheck-english-names))
      (it-dict (spellcheck-first-valid-dictionary spellcheck-italian-names))
      )
  (when en-dict
    (add-to-list 'spellcheck-guessing-rules `(,en-dict . ,spellcheck-english-regexp))
    (add-to-list 'spellcheck-languages      en-dict))
  (when it-dict
    (add-to-list 'spellcheck-guessing-rules `(,it-dict . ,spellcheck-italian-regexp))
    (add-to-list 'spellcheck-languages      it-dict))
  )


(add-hook 'find-file-hook  'spellcheck-set-guessed-dictionary)

;; Internal utility functions --------------------------------------------------------------------

(defun spellcheck-set-dictionary (dict)
  "Set the dictionary by calling `ispell-dictionary'. Catch errors in case of failure"
  (condition-case dict-error
      (ispell-change-dictionary dict)
    (error (message (nth 1 dict-error)))
    ))


(defun spellcheck-try-to-guess()
  "Guess language in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((count (map 'list (lambda (x)
                              (cons (count-matches (cdr x)) (car x)))
                      spellcheck-guessing-rules)))
      (cdr (assoc (car (sort (map 'list 'car count) '>))
                  count)))))





;; Interactive commands -------------------------------------------------------------

(defun spellcheck-set-guessed-dictionary ()
   "It tries to guess the language and to set it as dictionary for ispell"
   (interactive)
   (let ( (language (spellcheck-try-to-guess)) )
     (if (null language)
         (message "Language unknown, ispell dictionary unchanged")
       (message "Guessing language: %s" language)
       (spellcheck-set-dictionary language))))



(defun spellcheck-guess-language ()
  "Guess the language in the current buffer."
  (interactive)
  (message (concat "I think that the buffer language is [" (upcase (spellcheck-try-to-guess)) "]")))


(defun spellcheck-language-cycle ()
  "Switch between ENGLISH and ITALIAN for spell checking, in the current buffer."
  (interactive)
  (unless spellcheck-languages
    (message "No spellchecking dictionaries available")
    )
  (when (and (boundp 'ispell-local-dictionary) spellcheck-languages)
    (let (
          (old-local-language ispell-local-dictionary)
          new-pos
          old-pos
          )
      (setq old-pos (loop for lang in spellcheck-languages
            until (string= lang old-local-language)
            count lang
            ))
      (cond
       ((= old-pos (length spellcheck-languages))       (setq new-pos 0))
       ((= old-pos (- (length spellcheck-languages) 1)) (setq new-pos 0))
       (t                                               (setq new-pos (+ old-pos 1)))
        )
      (spellcheck-set-dictionary (nth new-pos spellcheck-languages))
      )
    )
  )


(provide 'init-spellcheck)
;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:
