;;;
;;; Python Mode configuration.
;;;
(provide 'init-python)
;;;-------------------------------------------------------------------

(setq python-command "/usr/bin/python2.6")
(setq python3-command "/usr/bin/python3.1")
(setq ipython-command "/usr/bin/ipython2.6")

;;;-------------------------------------------------------------------


;; Try to set up IPython Shell.
;; We force the ipython path to avoid troubles with SAGE math version.
(setq ipython-command "/usr/bin/ipython")
(when (require-maybe 'ipython)
  (setq py-python-command-args '("-pylab" "-colors" "Linux"))
)

;; Load ropemacs
(pymacs-load "ropemacs" "rope-")


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 

