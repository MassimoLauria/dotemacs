;;;
;;; Utilities for robust and safe emacs usage.
;;;
;;;------------------------------------------------------------------

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
        (password-options "-csy"))
    (insert
     (string-remove-suffix "\n"
                           (shell-command-to-string
                            (format "%s %s %d "
                                    password-gen
                                    password-options
                                    password-length))))))

;;----- RefTeX -----------------------------------------------------------
(defun text-citation-from-reftex ()
  "Insert RefTeX citation as explicit text form."
  (interactive)
  (let ((reftex-cite-format "%a\n%t\n%j%b (%y)")
        (reftex-cite-punctuation '(", " " and " " et al.")))
    (reftex-citation)))


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

(provide 'init-functions)
;; Local Variables:
;; mode: emacs-lisp
;; End:
