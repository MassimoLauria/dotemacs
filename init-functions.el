;;;
;;; Utilities for robust and safe emacs usage.
;;;
(provide 'init-functions)
;;;------------------------------------------------------------------

;;;-------------------------------------------------------------------

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
    (rename-buffer (concat "sudo::" (buffer-name)))
    (goto-char pos)
    )
  )


;;------ Useful functions --------------------------------------------------------------------

(defun list-or (s)
  "Return the logical OR of a list"
  (reduce '(lambda (x y) (or x y)) s :initial-value nil)
)

(defun list-and (s)
  "Return the logical AND of a list"
  (reduce '(lambda (x y) (and x y)) s :initial-value t)
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
    (message "%3dL %3dW %3dC" (count-lines start end) n (- end start))))


;;;- Previous/Next user/emacs buffer ----- and extension from ErgoEmacs ----------------------
(setq user-buffer-whitelist '("^*scratch*" "^*eshell*"))
(setq user-buffer-blacklist '("^*"))

(defun user-buffer-p (name)
  "Decide if a buffer name correspond to a user's buffer or not.
This is good for functions like 'next-user-buffer' which skip some 
emacs annoying buffers."
  (or
   (list-or (mapcar (lambda (p) (string-match-p p name)) user-buffer-whitelist))
   (not 
    (list-or (mapcar (lambda (p) (string-match-p p name)) user-buffer-blacklist))
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

(defun join-all-lines (start end)
  "Produce a single line, joining all lines in the text."
  (interactive "r")
  (save-restriction 
    (when mark-active 
      (narrow-to-region start end))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match " " nil t))
    )
  )


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


;;;---- Spelling functions ------------------------------------------------

(defun my-spell-correct-word ()
  "Correct the spelling of a word at point by using
either ispell or flyspell, if the latter is active"
  (interactive)
  (if (not (flyspell-auto-correct-word))
    (ispell-word)
    )
  )


;;;---- Recreate *scratch* buffer as soon as it's killed ------------------

;; If the *scratch* buffer is killed, recreate it automatically
;; FROM: Morten Welind
;;http://www.geocrawler.com/archives/3/338/1994/6/0/1877802/
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)


;;----- Fix compile modes ------------------------------------------------

;; Helper for compilation. Close the compilation window if
;; there was no error at all.
(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (and (eq status 'exit) (zerop code))
    ;; then bury the *compilation* buffer, so that C-x b doesn't go there
    (bury-buffer "*compilation*")
    ;; and delete the *compilation* window
    (delete-window (get-buffer-window (get-buffer "*compilation*"))))
  ;; Always return the anticipated result of compilation-exit-message-function
  (cons msg code))
;; Specify my function (maybe I should have done a lambda function)
(setq compilation-exit-message-function 'compilation-exit-autoclose)

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
(define-generic-mode vimrc-mode
  '("\"")
  '("set" "syntax" "noremap" "inoremap" "map")
  nil
  '("\\.vim\\'")
  nil)

;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 