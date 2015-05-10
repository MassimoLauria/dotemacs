;;; init-windows.el --- Windows and popup positioning and movements

;; Jump among windows
(use-package ace-window
  :bind ("M-m" . ace-window)
  :config (setq
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-scope 'frame
           aw-background t))


;; Force the some buffers to be displayed in "side" windows, i.e.
;; windows that are locked and reusable.
(defun force-window-at-bottom (regexp)
  (add-to-list 'display-buffer-alist
               `(,regexp
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.3))))

(defun force-window-at-right (regexp)
  (add-to-list 'display-buffer-alist
               `(,regexp
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (window-height   . 0.4))))
 
(defun side-windows-clear ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))


(force-window-at-bottom (rx bos "*Flycheck errors*" eos))
(force-window-at-bottom (rx bos "*Help*" eos))
(force-window-at-bottom (rx bos "*Apropos*" eos))
(force-window-at-bottom (rx bos "*Metahelp*" eos))


(eval-after-load 'winner-mode-hook
  '(progn 
     (define-key winner-mode-map (kbd "C-c C-j") 'winner-undo)
     (define-key winner-mode-map (kbd "C-c C-l") 'winner-redo)
     ))


(provide 'init-windows)
