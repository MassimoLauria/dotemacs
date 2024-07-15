;;;
;;; Utilities for robust and safe emacs usage.
;;;
;;;------------------------------------------------------------------




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


;;;------ Text processing -------------------------------------------

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
(require 'subr-x)
(defun create-password ()
  (interactive)
  (let ((password-gen "pwgen")
        (password-length 12)
        (password-options "-cs"))
    (insert
     (string-remove-suffix "\n"
                           (shell-command-to-string
                            (format "%s %s %d "
                                    password-gen
                                    password-options
                                    password-length))))))


;; Fake fullscreen
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


;;----- RefTeX -----------------------------------------------------------
(defun text-citation-from-reftex ()
  "Insert RefTeX citation as explicit text form."
  (interactive)
  (let ((reftex-cite-format "%a\n%t\n%j%b (%y)")
        (reftex-cite-punctuation '(", " " and " " et al.")))
    (reftex-citation)))


(defun uskey ()
  "Refresh the keyboard settings.

Unfortunately on for some reason the keyboard settings get lost. This command will refresh them from inside EMACS, so that I don't have to open a terminal for that."
  (interactive)
  (shell-command "setxkbmap -options" nil nil)
  (shell-command "python ~/config/xsession/xkb_capsunlock.py" nil nil)
  (shell-command "setxkbmap -config ~/config/xsession/setxkbmap.pcus" nil nil))

(defalias 'USKEY 'uskey)

;;----- Fix compile modes ------------------------------------------------

;; Helper for compilation. Close the compilation window if
;; there was no error at all. Unless we lock it open.

(defvar mxl/auto-close-compilation-window 'noerror
  "Whether the compilation windows should close after compilation.

Set it to nil to forbid closing. Set to t to force it. Set to
`noerror' to close it only if there are no error.
")

(defun compilation-exit-autoclose (status code msg)
  ;; If M-x compile exists with a 0
  (when (or (eq mxl/auto-close-compilation-window t)
            (and (eq status 'exit) (zerop code)
                 (eq mxl/auto-close-compilation-window 'noerror)
                 ) ;; check errors
         )
    (let ((cmpl-buffername (buffer-name)))
      ;; then bury the *compilation* buffer, so that C-x b doesn't go there
      (bury-buffer cmpl-buffername)
      ;; and delete the *compilation* window
      (delete-window (get-buffer-window (get-buffer cmpl-buffername)))))
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
(define-generic-mode 'vimrc-mode
    '(("\"" . nil))
    '("set" "syntax" "noremap" "inoremap" "map")
    '(("^[\t ]*:?\\(!\\|ab\\|map\\|unmap\\)[^\r\n\"]*\"[^\r\n\"]*\\(\"[^\r\n\"]*\"[^\r\n\"]*\\)*$"
       (0 font-lock-warning-face))
      ;; Line comment
      ("^[\t ]*\\(\"\\)\\(.*\\)$"
       (1 font-lock-comment-delimiter-face)
       (2 font-lock-comment-face))
      ;; String start:
      ("\\(\"[^\n\r\"]*\"\\)\\|\\('[^\n\r]*'\\)"
       (0 font-lock-string-face))) ;; String end;
    '("/vimrc\\'" "\\.vim\\(rc\\)?\\'")
    '((lambda ()
        (modify-syntax-entry ?\" ".")))
    "Generic mode for Vim configuration files.")

(defun url-unhex-region (start end)
  "Remove the URL encoding from a region of text."
  (interactive "r")
  (when mark-active
    (insert (url-unhex-string (delete-and-extract-region start end)))))

(defun url-hexify-region (start end)
  "Remove the URL encoding from a region of text."
  (interactive "r")
  (when mark-active
    (insert (url-hexify-string (delete-and-extract-region start end)))))

(provide 'init-functions)
;; Local Variables:
;; mode: emacs-lisp
;; End:
