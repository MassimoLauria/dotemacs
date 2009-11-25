; Highlight fixme comments in source code.
;
;; Time-stamp: <010501 mostert 19:31 fixme.el>
;
;; FIXME COMMENTS
; This is no substitute for defect-tracking or change-management, of
; course, but putting the note right next to the code it relates to has
; good side effects: Next time somebody works on the code, they can keep
; the work that needs to be done in mind If somebody tracks down a
; bug/misbehaviour, the comment may help them understand or fix it After
; writing the comment, it may seem easier to just fix it than to leave
; the comment in. So, writing them is a way to briefly pop up one
; DesignLevel and think about what problem needs to be solved.  Typing
; right into the code is easier than using ProdSup, so it gives a balance
; between going off on tangents, and forgetting about things that need
; attention.
; see http://c2.com/cgi/wiki?FixmeComments
;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You can find the GNU General Public License at
;; http://www.gnu.org/copyleft/gpl.html
;; or you can write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307, USA.
;
;; Author: Jeroen M. Mostert  <mostert@users.sourceforge.net>

(defun fixme-set-regexp (my-symbol my-value)
  ""
  (set-default my-symbol my-value)
  (mapcar (lambda (mode)
        (font-lock-add-keywords
         mode
         (list (list fixme-regexp
                     1 ''font-lock-fixme-face t)
               )
         )
        )
      fixme-modes)
  )  


(defgroup fixme '() "highlite fixme comments"
  :group 'tools
  :prefix "fixme"
  )

(defcustom fixme-modes 
  '(erlang-mode java-mode c-mode latex-mode python-mode
                emacs-lisp-mode scheme-mode
                jde-mode perl-mode rul-generic-mode
                rexx-mode visual-basic-mode gml)
  "modes to activate fixme comments in"
  :type '(repeat symbol)
  :group 'fixme
;;  :set 'fixme-set-regexp
  )


;; so now, every FIXME string on the screen stands out.
(defcustom fixme-regexp
  "\\<\\(FIXME\\|TO ?DO\\|2DO\\|REFACTOR\\|DRY\\|BUG\\|HACK\\)\\>"
  "regular expression to match FixmeComments
on order of priority:
BUG = this is a known bug
FIXME = needs to be fixed
HACK = quick and dirty implementation should be rechecked
DRY = Don't Repeat Yourself (tags the results of rape&paste programming)
REFACTOR = this piece of code smells bad so we should refactor it
TODO = stuff not yet done or not implemented but can be postponed
"
  :type 'regexp
  :group 'fixme
;;  :set 'fixme-set-regexp
  )

;; angy red on yellow background. It *really* gets your attention
(defface font-lock-fixme-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "Red"
                                                     :background "Yellow"
                                                     :underline t))
    (((class color) (background dark)) (:foreground "Red"
                                                    :background "Yellow"
                                                    :underline t))
    (t (:underline t)))
  "face for fixme comments. It should *really* get your attention"
  :group 'fixme
  )

(defvar font-lock-fixme-face
  'font-lock-fixme-face)



(mapcar (lambda (mode)
        (font-lock-add-keywords
         mode
         (list (list fixme-regexp
                     1 ''font-lock-fixme-face t)
               )
         )
        )
      fixme-modes)


(defun fixme-jump-to-fixme ( &optional arg)
  "jump to the next fixme comment"
  (interactive "P")
  (let ((case-fold-search nil))
    (if (and (null arg) (> (prefix-numeric-value arg) 0))
        (re-search-forward fixme-regexp)
      (re-search-backward fixme-regexp)
      )
    )
  )
;; global key for next fixme comment C-` (to feel like A-` next-error)
(global-set-key [?\C-`] 'fixme-jump-to-fixme )

(defun fixme-for-category ()
  (interactive)
  (grep (concat "grep -n -e \"" fixme-regexp "\" *.c *.h *.java"))
  )

(global-set-key [?\A-\C-`] 'fixme-for-category )

(provide 'fixme)

