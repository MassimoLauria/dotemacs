;;;
;;; Sandbox for Experimental features
;;;
;;; Time-stamp: <2010-01-23 00:24:58 (Massimo Lauria)>
;;;
(provide 'init-unstable)
;;;-----------------------------------------------------------------



;; Change cursor color according to mode; inspired by
;; http://www.emacswiki.org/emacs/ChangingCursorDynamically
;; http://emacs-fu.blogspot.com/2009/12/changing-cursor-color-and-shape.html

;; valid values are t, nil, box, hollow, bar, (bar . WIDTH), hbar,
;; (hbar. HEIGHT); see the docs for set-cursor-type

(setq massimo-read-only-color       "gray")
(setq massimo-read-only-cursor-type 'box)
(setq massimo-overwrite-color       "red")
(setq massimo-overwrite-cursor-type 'box)
(setq massimo-normal-color          "gray")
(setq massimo-normal-cursor-type    'bar)

(defun massimo-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."

  (cond
    (buffer-read-only
      (set-cursor-color massimo-read-only-color)
      (setq cursor-type massimo-read-only-cursor-type))
    (overwrite-mode
      (set-cursor-color massimo-overwrite-color)
      (setq cursor-type massimo-overwrite-cursor-type))
    (t 
      (set-cursor-color massimo-normal-color)
      (setq cursor-type massimo-normal-cursor-type))))

(add-hook 'post-command-hook 'massimo-set-cursor-according-to-mode)

(defun massimo-fullscreen-toggle ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (if (frame-parameter nil 'fullscreen) 
                           nil 
                         'fullboth
                         )
                       )
  )

(setq 
  time-stamp-active t          ; do enable time-stamps
  time-stamp-line-limit 8      ; check first 10 buffer lines for Time-stamp: 
  time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S (%U)")   ; date format

(add-hook 'write-file-hooks 'time-stamp) ; update when saving

;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:
