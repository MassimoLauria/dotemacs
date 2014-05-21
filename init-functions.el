;;;
;;; Utilities for robust and safe emacs usage.
;;;
;;;------------------------------------------------------------------


;;; Some functions maybe absent in Emacs 22.

;; Define `string-match-p' if it does not exists
(unless (fboundp 'string-match-p)
(defun string-match-p (pattern text)
  "It is a substitution of the one present only in Emacs 23 and above.
This function is equivalent to `string-match' but preserves match
data"
  (save-match-data
    (string-match pattern text) ) )
)

;; Define `region-active-p' if it does not exists
(unless (fboundp 'region-active-p)
(defun region-active-p ()
  "It is a substitution of the one present only in Emacs 23 and above."
  (and transient-mark-mode mark-active))
)


;;; Older versions of `called-interactively-p' do not take the
;;; optional argument.

(condition-case nil (called-interactively-p 'interactive)
  (error
   ; Save reference to called-interactively-p in
   ; inglorion-system-called-interactively-p
   (fset 'inglorion-system-called-interactively-p
         (symbol-function 'called-interactively-p))
   ; Define called-interactively-p so that it discards
   ; its arguments and calls inglorion-system-called-interactively-p
   (fset 'called-interactively-p
         (lambda (&rest args)
           (inglorion-system-called-interactively-p)))))

;;;-------------------------------------------------------------------

(defun get-buffer-major-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
     (set-buffer buffer-or-string)
     major-mode))

(defmacro require-maybe (feature &optional file)
  "Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))

(defmacro when-available (func foo)
  "Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(defun root-file-reopen ()
  "Visit the file corresponding to the active buffer using root privileges."
  (interactive)
  (let (
        (file (buffer-file-name))
        (pos  (point))
        )
    (set-buffer (find-file (concat "/sudo::" file)))
    (goto-char pos)
    )
  )


(defun comment-region-maybe ()
  "Comment a region or uncomment it, if it is already
commented. If the command is called several times in a row the
commentation is toggled. If there is no active region, then just
insert the character used to activate the command.
It will not work properly if it is not bound to a key.
"
  (interactive)
  (if (or (region-active-p) (eq last-command 'comment-region-maybe))
      (call-interactively 'comment-or-uncomment-region)
    (progn
      (setq this-command 'self-insert-command)
      (call-interactively 'self-insert-command) ) ) )

;;------ Useful functions --------------------------------------------------------------------


(defun decorate-formula (F sexp-arg)
  "Given an S-exp, it descend in all subtrees to apply function F
to the leaves of a formula (i.e.\ tokens).

The resulting formula is a similar S-exp in which each tokens t is substituted by
the result of (F t)
"
  (cond
   ( (listp sexp-arg)
     (cons (car sexp-arg) (mapcar '(lambda (l) (decorate-formula F l)) (cdr sexp-arg)))
     )
   ( 't (funcall F sexp-arg))
   )
  )


(defun wc (&optional start end)
  "Prints number of lines, words and characters in region or whole buffer."
  (interactive)
  (let ((n 0)
        (start (if mark-active (region-beginning) (point-min)))
        (end (if mark-active (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (< (point) end) (if (forward-word 1) (setq n (1+ n)))))
    (message "%3d lines, %3d words, %3d chars" (count-lines start end) n (- end start))))


;;;- Previous/Next user/emacs buffer ----- and extension alike ErgoEmacs ----------------------

; Those are DNFs, lists of lists. External list is an OR of rules, internal is an AND.
(setq user-buffer-whitelist '(or "^*scratch*" "^*Remember*" "^*shell*" "^*Python*" "^*mail*" "^*draft*" "^*info" rcirc-mode))
(setq user-buffer-blacklist '(or "^*" "\.pdfsync" (and "\\.log" latex-mode) "contacts.bbdb"))

;; string-match-p does not exists before Emacs 23
(unless (fboundp 'string-match-p)
(defun string-match-p (pattern text)
  "It is a substitution of the noe in Emacs 23 and above.
This function is equivalent to `string-match' but preserves match
data"
  (save-match-data
    (string-match pattern text) ) )
)

(defun user-buffer-match-p (rule buffer-name-arg)
  "Decide if the buffer matches the rule. If the rule is a string
then it has to match the name, if it is a mode, it has to match
the mode, if it is a function which accept one arg, then the
function is evaluated"
  (cond
   ( (stringp   rule)  (string-match-p rule buffer-name-arg))   ; Test the buffer name.
   ( (and (symbolp   rule)
          (string-match-p "-mode$" (symbol-name rule))
          ) (eq (get-buffer-major-mode buffer-name-arg) rule)) ; Test if mode matches.
   ( (functionp rule)  (funcall rule buffer-name-arg))          ; Eval an arbitrary test function
   )
  )


(defun user-buffer-p (buffer-name-arg)
  "Test whether the buffer is actually a \"user buffer\", meaning
it not one of the ugly buffers generated by AucTeX or Emacs.

variable \'user-buffer-blacklist\' and \'user-buffer-whitelist\' define a series of rules to decide.
"
  (or
   (eval (decorate-formula '(lambda (x) (list 'user-buffer-match-p  `(quote ,x) buffer-name-arg)) user-buffer-whitelist ))
   (not
    (eval (decorate-formula '(lambda (x) (list 'user-buffer-match-p `(quote ,x) buffer-name-arg)) user-buffer-blacklist ))
    )
   )
  )

(defun next-user-buffer ()
  "Switch to the next user buffer. User buffers are decided with 'user-buffer-p' predicate."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (user-buffer-p (buffer-name))) (< i 50) )
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.  User buffers are decided with 'user-buffer-p' predicate."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (user-buffer-p (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))


(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (user-buffer-p (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))


(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (user-buffer-p (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))


;;;------ Text processing -------------------------------------------

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun join-all-lines (start end)
  "Produce a single line, joining all lines in the selected region."
  (interactive "r")
  (save-restriction
    (when mark-active
      (narrow-to-region start end))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match " " nil t))
    )
  )

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

(defun format-author-string (start end)
  "Replace all “and”s but last one, to commas. This is very
  useful for formatting author lists in LaTeX."
  (interactive "r")
  (save-restriction
    (when mark-active
      (narrow-to-region start end))
    (goto-char (point-min))
    (setq last-match nil)
    ;; Change all the "and" in commas
    (while (word-search-forward "and" nil t)
      (setq last-match (match-beginning 0))
      (replace-match "," nil t)
      )
    ;; Last "and" should have been left alone
    (if (last-match)
        (progn
          (goto-char last-match)
          (delete-char 1)
          (insert "and")
          )
      )
    (normalize-space-punctuation (point-min) (point-max))
    )
  )


(defun normalize-space-punctuation (start end)
  "Remove un-necessary spaces in text.
   In particular (1) multiple spaces
                 (2) spaces before punctuation
  "
  (interactive "r")
  (save-restriction
    (when mark-active
      (narrow-to-region start end))
    (goto-char (point-min))
    (while (re-search-forward "[ ]+" nil t) (replace-match " " nil t))
    ;; It trims spaces in front of punctuation [,.:;]
    (goto-char (point-min))
    (while (re-search-forward "\\( +\\)[,.;:]" nil t) (replace-match "" nil t nil 1))
    )
  )

;; Lisp specific defuns by Magnar Sveen
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;;; Create passwords using 'pwgen command'
(defun create-password ()
  (interactive)
  (let ((password-gen "pwgen")
        (password-length 12)
        (password-options "-cys"))
    (shell-command (format "%s %s %d " password-gen password-options password-length) t)))
 


;;;---- Recreate *scratch* buffer as soon as it's killed ------------------

;; If the *scratch* buffer is killed, recreate it automatically
(defun reinit-scratch-buffer ()
  "Kill scratch buffer and recreate it according to may preferences."
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'reinit-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (init-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

;; Prepare *scratch buffer*
;; FROM: Morten Welind
;; http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(defun init-scratch-buffer ()
  "Setup the *scratch* buffer according to may preferences.

In particular every time the buffer is killed, it is recreated
according to this default."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode)
    (make-local-variable 'kill-buffer-query-functions)
    (add-hook 'kill-buffer-query-functions 'reinit-scratch-buffer)))




;; Fake fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


;;----- Fix compile modes ------------------------------------------------

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    (let ((cmpl-buffername (buffer-name)))
      ;; then bury the *compilation* buffer, so that C-x b doesn't go there
      (bury-buffer cmpl-buffername)
      ;; and delete the *compilation* window
      (delete-window (get-buffer-window (get-buffer cmpl-buffername)))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))

;; Specify my function (maybe I should have done a lambda function)
;; (setq compilation-exit-message-function 'compilation-exit-autoclose)

;; Keeps windows in order by dedicating them (make them sticky)
(defun toggle-current-window-sticky ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to buffer \"%s\""
            (if dedicated "no longer " "")
            (buffer-name))))

;;----- Additional modes -—-------------------------------------------------
(define-generic-mode 'vimrc-mode
    '("\"")
    '("set" "syntax" "noremap" "inoremap" "map")
    '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
       (0 font-lock-warning-face))
      ("\\(^\\|[\t ]\\)\\(\".*\\)$"
      (2 font-lock-comment-face))
      ("\"\\([^\n\r\"\\]\\|\\.\\)*\""
       (0 font-lock-string-face)))
    '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
    '((lambda ()
        (modify-syntax-entry ?\" ".")))
    "Generic mode for Vim configuration files.")


(provide 'init-functions)
;; Local Variables:
;; mode: emacs-lisp
;; End:
