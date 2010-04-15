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
(setq user-buffer-whitelist '("^*scratch*"))
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


;;;-------------------------------------------------------------------



;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 