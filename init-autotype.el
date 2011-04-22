;;; init-autotype.el --- Automatic test insertion configuration

;; Copyright (C) 2010, 2011  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2011-04-22, Friday 12:27 (CEST) Massimo Lauria>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the part of my init files devoted to automatic text
;; insertion. It configures facilities like update of copyright notes
;; and time-stamps, Yasnippet and auto-insert.


(setq template-time-format      "%Y-%02m-%02d, %A %02H:%02M (%Z)")  ;; Time format similar with time-stamp one.


;;; Auto Insert: ----------------------------------------------------------------------

(require 'autoinsert)
(add-hook 'find-file-hook 'auto-insert)  ;;; Adds hook to find-files-hook
(setq auto-insert-query nil)
(setq auto-insert-alist nil)  ;; Reset auto-insert rules.
;; Auto Insert rules:

;; Use yasnippet to implement templates.
(defun apply-yasnippet-function (template)
  "It produces a function which insert a yasnipper template"
  `(lambda ()
     (goto-char (point-min))
     (insert ,template)
     (call-interactively 'yas/expand))
  )

(define-auto-insert 'sh-mode (apply-yasnippet-function "empty-template"))
(define-auto-insert 'makefile-bsdmake-mode (apply-yasnippet-function "empty-template"))
(define-auto-insert 'makefile-gmake-mode (apply-yasnippet-function "empty-template"))
(define-auto-insert "\\.\\([Cc]\\|cc\\|cpp\\)\\'" (apply-yasnippet-function "empty-c-template"))
(define-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)\\'" (apply-yasnippet-function "empty-h-template"))

;;; YaSnippet -------------------------------------------------------------------------

(require-maybe 'yasnippet-bundle)
(when-available
 'yas/about
 (progn
   (setq yas/root-directory nil)
   (if (file-directory-p "/usr/share/emacs/site-lisp/yasnippet/snippets")
       (add-to-list 'yas/root-directory "/usr/share/emacs/site-lisp/yasnippet/snippets") ;; Snippet's file folder.
     )
   (if (file-directory-p (concat default-elisp-path "/snippets/"))
       (add-to-list 'yas/root-directory (concat default-elisp-path "/snippets/")) ;; Snippet's file folder.
     )
   ;; Map `yas/load-directory' to every element
   (mapc 'yas/load-directory yas/root-directory)
   (setq yas/ignore-filenames-as-triggers t)
   ))

;;; Copyright update --- setup in custom.el

;;; Time-Stamp update --- setup in custom.el

;;; Auto pair configuration -----------------------------------------------------------
(setq autopair-blink nil)

;; Broken on Emacs22 since it uses `region-active-p' which is not
;; present here
(setq autopair-autowrap (fboundp 'region-active-p))

(require 'autopair)

;; AutoPair seems to mess with Emacs in several Major-modes, so I will
;; activate on single mode basis... Furthermore several modes require
;; a workaround.
;;
;; (autopair-global-mode t)


;; Auto pair work-around for term-mode
(defun autopair-term-mode-handle-action (action pair pos-before)
  "Trick autopair to working with the `term-mode' nonsense.

`insert' and friends don't really do anything in a term-mode
buffer, we need to send actual strings to the subprocess. So
override these emacs primitives to do so, then call the usual
default handler."
  (let ((proc (get-buffer-process (current-buffer))))
    (flet ((insert (&rest args)
                   (mapc #'(lambda (arg)
                             (if (stringp arg)
                                 (term-send-raw-string arg)
                               (term-send-raw-string (char-to-string arg))))
                         args))
           (delete-char (howmany)
                        (dotimes (i howmany)
                          (term-send-del)))
           (backward-char (howmany)
                          (dotimes (i howmany)
                            (term-send-left))))
      (autopair-default-handle-action action pair pos-before)
      (goto-char (process-mark proc)))))

;; Term mode is quirky, this will fix it.
(add-hook 'term-mode-hook
          #'(lambda ()
              (set (make-local-variable 'autopair-handle-action-fns) '(autopair-term-mode-handle-action))
              (autopair-mode 1)
              ))


;; LaTeX version of `autopair-latex-mode-paired-delimiter-action'. The
;; original does not work for AucTeX (at least for me).
(defun autopair-LaTeX-mode-paired-delimiter-action (action pair pos-before)
  "Pair or skip latex's \"paired delimiter\" syntax in math mode. FIXED by Massimo Lauria"
  (when (eq action 'paired-delimiter)
    (when (eq (char-before) pair)
      (if (and (or
                (eq (get-text-property (- pos-before 1) 'face) 'font-latex-math-face)
                (member 'font-latex-math-face (get-text-property (- pos-before 1) 'face)))
               (eq (char-after) pair))
          (cond ((and (eq (char-after) pair)
                      (eq (char-after (1+ (point))) pair))
                 ;; double skip
                 (delete-char 1)
                 (forward-char))
                ((eq (char-before pos-before) pair)
                 ;; doube insert
                 (insert pair)
                 (backward-char))
                (t
                 ;; simple skip
                 (delete-char 1)))
        (insert pair)
        (backward-char)))))

;; Manage `` typed as "
(defun autopair-latex-setup ()
  "Install AutoPair in LaTex, with all the needed workarounds"
  (interactive)
  (set (make-local-variable 'autopair-handle-action-fns)
       (list #'autopair-default-handle-action
             #'autopair-LaTeX-mode-paired-delimiter-action))
  (autopair-mode t)
  )


;; Activate autopair and also Pair triple quotes in python
(add-hook 'python-mode-hook
           #'(lambda ()
               (set (make-local-variable 'autopair-handle-action-fns)
                     (list #'autopair-default-handle-action
                           #'autopair-python-triple-quote-action))
               (autopair-mode t)
               ))

;; Activate `autopair-mode' and pair ` ' in comments and strings
(add-hook 'emacs-lisp-mode-hook
           #'(lambda ()
               (push '(?` . ?')
                     (getf autopair-extra-pairs :comment))
               (push '(?` . ?')
                     (getf autopair-extra-pairs :string))
               (autopair-mode t)
               ))

;; Activate `autopair-mode' in sh-mode
(add-hook 'sh-mode-hook  #'(lambda () (autopair-mode t) ))
(add-hook 'c-mode-hook   #'(lambda () (autopair-mode t) ))
(add-hook 'org-mode-hook #'(lambda () (autopair-mode t) ))


;; In these modes, autopair seems completely broken
(add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'xrdb-mode-hook #'(lambda () (setq autopair-dont-activate t)))
(add-hook 'orgtbl-mode-hook
          (lambda ()
            (set (make-local-variable 'autopair-autowrap) nil)  ;; Autowrap fights with Orgtbl-mode
            (message "Autopair-Autowrap disactivated because it conflicts with OrgTbl-Mode... ")
            ))

(provide 'init-autotype)
;;; init-autotype.el ends here
