;;; init-auto-complete.el --- Configuration for auto-complete mode

;; Filename: init-auto-complete.el


;; Company keyboard setup
;;
;; `company-complete' is the aggressive completion: complete and execute the action.
;; `ac-expand' just expand the common part or go to the next candidate.
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

;;; Require
(setq default-ac-dir   (concat default-elisp-3rdparties "/auto-complete"))
(add-to-list 'load-path default-ac-dir)
(require 'auto-complete)
(require 'auto-complete-config)

;; auto-complete dictionaries
(add-to-list 'ac-dictionary-directories (concat default-ac-dir "/dict/"))

;; Load `auto-complete-auctex'
(require 'auto-complete-auctex nil t)

;; Generic setup.
(global-auto-complete-mode)             ;enable global-mode
(setq ac-dwim nil)                      ;do not change keys semantic
(setq ac-override-local-map nil)        ;don't override local map
(setq popup-use-optimized-column-computation t)  ; slower but precise
                                                 ; menu positioning
(ac-config-default)

;; Workaround for flyspell-mode
(ac-flyspell-workaround)

;; Menu movement
(define-key ac-completing-map (kbd "M-j") 'ac-quick-help-scroll-up)
(define-key ac-completing-map (kbd "M-l") 'ac-quick-help-scroll-down)
(define-key ac-completing-map (kbd "M-i") 'ac-previous)
(define-key ac-completing-map (kbd "M-k") 'ac-next)
(define-key ac-completing-map (kbd "M-u") 'ac-stop)
(define-key ac-completing-map (kbd "M-o") 'ac-complete)


;; `ac-complete' is the aggressive completion: complete and execute the action.
;; `ac-expand' just expand the common part or go to the next candidate.
;; By default RET = `ac-complete'
;;            TAB = `ac-expand'
(define-key ac-completing-map   "\r"     'nil)         ; conflicts with autopair-mode
(define-key ac-completing-map   [return] 'nil)
(define-key ac-completing-map   "\t"     'ac-complete) ; aggressive expansion
(define-key ac-completing-map   [tab]    'ac-complete)


;; The mode that automatically startup.
(setq ac-modes '(emacs-lisp-mode lisp-interaction-mode lisp-mode
                 inferior-emacs-lisp-mode scheme-mode c-mode
                 cc-mode c++-mode java-mode perl-mode cperl-mode
                 python-mode ruby-mode ecmascript-mode
                 javascript-mode php-mode css-mode makefile-mode
                 sh-mode fortran-mode f90-mode ada-mode xml-mode
                 sgml-mode haskell-mode literate-haskell-mode
                 latex-mode LaTeX-mode emms-tag-editor-mode
                 asm-mode org-mode text-mode
                 wl-draft-mode message-mode
                 mml-mode mail-mode))

; if you want enable auto-complete at org-mode, uncomment this line
(add-to-list 'ac-trigger-commands 'org-self-insert-command)


;; The sources for common all mode.
(custom-set-variables
 '(ac-sources
   '(
     ac-source-yasnippet
     ac-source-words-in-buffer
     ac-source-files-in-current-dir
     ac-source-filename
     )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Lisp mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(dolist (hook (list
               'lisp-interaction-mode-hook
               'inferior-emacs-lisp-mode-hook
               ))
  (add-hook hook 'ac-emacs-lisp-mode-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Mail-mode + BBDB ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ac-bbdb-header-list '("to" "from" "cc" "bcc"))

(defun ac-bbdb-candidate ()
  (delete-dups
   (apply
    'append
    (mapcar (lambda (rec)
              (mapcar (lambda (n) (bbdb-dwim-net-address rec n))
                      (bbdb-record-net rec)))
            (bbdb-records)))))

(defun ac-bbdb-prefix ()
  (interactive)
  (let ((case-fold-search t))
    (when (and
           (< (point)
              (save-excursion
                (goto-char (point-min))
                (search-forward (concat "\n" mail-header-separator "\n") nil t)
                (point)))
           (save-excursion
             (beginning-of-line)
             (while (and (looking-at "^[ \t]")
                         (not (= (point) (point-min))))
               (forward-line -1))
             (looking-at (concat (regexp-opt ac-bbdb-header-list t) ":"))))
      (ac-prefix-symbol))))

(defvar ac-source-bbdb
  '((candidates . ac-bbdb-candidate)
    (match . substring)
    (prefix . ac-bbdb-prefix)))

(defun turn-on-ac-bbdb ()
  "Activate auto-complete in message draft's (Wanderlust mail client)"
  (interactive)
  (add-to-list 'ac-sources 'ac-source-bbdb)
  (auto-complete-mode 1))


(provide 'init-auto-complete)

;;; init-auto-complete.el ends here
