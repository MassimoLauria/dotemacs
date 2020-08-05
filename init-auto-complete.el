;;; init-auto-complete.el --- Configuration for auto-completion



;; We use GNU project `company' for auto completion.
(add-hook 'after-init-hook 'global-company-mode)


;; Set backends macro
(defmacro set-company-backends (mode list)
  `(add-hook ',mode
             (lambda ()
               (set (make-local-variable 'company-backends) ,list))))


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
(with-eval-after-load 'company
  (when (fboundp 'company-anaconda)
    (add-to-list 'company-backends 'company-anaconda))  ; python

  (when (fboundp 'company-irony)
    (add-to-list 'company-backends 'company-irony)))     ; c/c++

;; Yasnippet
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :config
  (setq company-dabbrev-downcase nil)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

;; (use-package company-box
;;   :after company
;;   :diminish
;;   :hook (company-mode . company-box-mode))





(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
