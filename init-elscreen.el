;;;
;;; Elscreen setup
;;;
(provide 'init-elscreen)
;;;-----------------------------------------------------------------


;; Keybindings
; create/destroy screens
(global-set-key "\C-t" 'elscreen-create)
(global-set-key "\C-w" 'elscreen-kill)
; Moving between Elscreens (M-S)
(global-set-key [M-S-right] 'elscreen-next) 
(global-set-key [M-S-left] 'elscreen-previous)
(global-set-key [C-tab] 'elscreen-toggle)



;; No screen tabbar (it is annoying when there are vertical splits)
(setq elscreen-display-tab nil)



;; Screen numbers on the title bar
(defun elscreen-frame-title-update ()
  (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
	   (screen-to-name-alist (elscreen-get-screen-to-name-alist))
	   (title (mapconcat
		   (lambda (screen)
		     (format "%d%s"
			     screen (elscreen-status-label screen)
			     (get-alist screen screen-to-name-alist)))
		   screen-list " ")))
      (if (fboundp 'set-frame-name)
	  (set-frame-name title)
	(setq frame-title-format title)))))

(eval-after-load "elscreen"
  '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))



;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
