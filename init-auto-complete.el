;;; init-auto-complete.el --- Configuration for auto-completion



;; We use GNU project `company' for auto completion.
(add-hook 'after-init-hook 'global-company-mode)


;; Company keyboard setup
;;
;; `company-complete' is the aggressive completion: complete and
;; execute the action.
;;
;; `company-complete-common expand' just expand the common part or go
;; to the next candidate.
;;
;; By default RET = `company-complete'
;;            TAB = nil
;;
(with-eval-after-load "company"
  ;; Menu movement
  (define-key company-active-map (kbd "M-j") #'company-select-previous)
  (define-key company-active-map (kbd "M-l") #'company-select-next)
  (define-key company-active-map (kbd "M-i") #'company-select-previous)
  (define-key company-active-map (kbd "M-k") #'company-select-next)
  (define-key company-active-map (kbd "M-u") #'company-abort)
  (define-key company-active-map (kbd "M-o") #'company-complete-common)
  (define-key company-active-map "\r"        nil)
  (define-key company-active-map [return]    nil)
  (define-key company-active-map "\t"  'company-complete-selection)
  (define-key company-active-map [tab] 'company-complete-selection))



;; Setup various auto-completion backends
;;
(when (fboundp #'company-auctex-init)
  (add-hook 'LaTeX-mode-hook #'company-auctex-init))

(with-eval-after-load 'company
  (when (fboundp 'company-anaconda)
    (add-to-list 'company-backends 'company-anaconda))  ; python

  (when (fboundp 'company-irony)
    (add-to-list 'company-backends 'company-irony))     ; c/c++
  )


;; UNICODE annotations slow down on MacOSX
(with-eval-after-load 'company-auctex
  (if (eq system-type 'darwin)
      (defun company-auctex-symbol-annotation (candidate) "" "")))


(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
