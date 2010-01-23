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
  (let ((file (buffer-file-name)))
    (set-buffer (find-file (concat "/sudo::" file)))
    (rename-buffer (concat "sudo::" (buffer-name)))
    )
  )

;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 