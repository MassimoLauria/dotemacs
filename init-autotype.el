;;; init-autotype.el --- Automatic test insertion configuration

;; Copyright (C) 2010, 2011  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2011-03-12, sabato 15:34 (CET) Massimo Lauria>

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


;;; Markers for template filling
(setq template-marker-for-name            ">>>NAME<<<" )
(setq template-marker-for-email           ">>>EMAIL<<<")
(setq template-marker-for-point           ">>>POINT<<<")
(setq template-marker-for-time            ">>>TIME<<<")
(setq template-marker-for-header          ">>>HEADERNAME<<<" )
(setq template-marker-for-ucfname         ">>>UCFILENAME<<<" )
(setq template-marker-for-lcfname         ">>>LCFILENAME<<<" )
(setq template-marker-for-fname           ">>>FILENAME<<<" )
(setq template-marker-for-fname-sans-ext  ">>>FNAMENOEXT<<<" )

(setq template-time-format      "%Y-%02m-%02d, %A %02H:%02M (%Z)")  ;; Time format similar with time-stamp one.


;;; Auto Insert: ----------------------------------------------------------------------

(require 'autoinsert)
(add-hook 'find-file-hook 'auto-insert)  ;;; Adds hook to find-files-hook
(setq auto-insert-directory (concat default-elisp-path "/templates/")) ;; Template's files folder, *NOTE* Trailing slash important
(setq auto-insert-query nil) ;;; If you don't want to be prompted before insertion

;; Auto Insert rules:

(define-auto-insert 'sh-mode ["sh-template.sh" apply-template-marker]) ; Example of a template file based rule.
(define-auto-insert 'makefile-mode ["make-template.mak" apply-template-marker]) ; Example of a template file based rule.
(define-auto-insert 'makefile-gmake-mode ["make-template.mak" apply-template-marker]) ; Example of a template file based rule.
(define-auto-insert "\\.\\([Cc]\\|cc\\|cpp\\)\\'" ["c-template.c" apply-template-marker]) ; Example of a template file based rule.
(define-auto-insert "\\.\\([Hh]\\|hh\\|hpp\\)\\'" ["h-template.h" apply-template-marker]) ; Example of a template file based rule.

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



;;; Utility functions for template filling ---------------------------------------------

(defun process-string-matches (mark-exp F)
  "Find matches of a particular string, and process the matching
text with function F which return a string."
  (save-excursion
    (goto-char (point-min))
    ;; We search for matchings
    (while (search-forward mark-exp nil t)
      ;; We save the restriction, because we are going to call narrow-to-region
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (let (
              (new-text (funcall F (buffer-string)))
              )
          (delete-region (point-min) (point-max))
          (insert new-text))))))


(defun apply-template-marker()
  "Fill template fields"
  (interactive)
  (process-string-matches template-marker-for-name  '(lambda (x)  user-full-name))
  (process-string-matches template-marker-for-email '(lambda (x) user-mail-address))
  (process-string-matches template-marker-for-time  '(lambda (x) (format-time-string template-time-format (current-time))))

  (process-string-matches template-marker-for-header
                          '(lambda (x)   (upcase (concat (file-name-nondirectory
                                                          (file-name-sans-extension buffer-file-name))
                                                         "_"
                                                         (file-name-extension buffer-file-name)))))
  (process-string-matches template-marker-for-ucfname
                          '(lambda (x) (upcase (file-name-nondirectory buffer-file-name))))
  (process-string-matches template-marker-for-lcfname
                          '(lambda (x) (downcase (file-name-nondirectory buffer-file-name))))
  (process-string-matches template-marker-for-fname
                          '(lambda (x) (file-name-nondirectory buffer-file-name)))
  (process-string-matches template-marker-for-fname-sans-ext
                          '(lambda (x)(file-name-nondirectory (file-name-sans-extension buffer-file-name))))
  ;; Move to point
  (goto-char (point-min))
  (if (search-forward template-marker-for-point  nil t)
      (progn
        (goto-char (match-beginning 0))
        (delete-region (match-beginning 0) (match-end 0))
        )
      )
)

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
