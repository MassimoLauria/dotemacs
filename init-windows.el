;;; init-windows.el --- Windows and popup positioning and movements

;; Jump among windows
(use-package ace-window
  :bind ("M-m" . ace-window)
  :config (setq
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-scope 'frame
           aw-background t
           )
  :custom-face
  ;; I don't like the annoying large size of the font. Ok it is more
  ;; readable but it moves around all the content.
  (aw-leading-char-face ((t (:height 1.0)))))


;; Force the some buffers to be displayed at the "bottom" of the frame.
;; The windows that is reusable (but not locked).
(defun force-window-at-bottom-of-frame (regexp)
  (add-to-list 'display-buffer-alist
               `(,regexp
                 (display-buffer-reuse-window
                  display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.4))))

;; Force the some buffers to be displayed at the "bottom" of the
;; current window. The windows that is reusable (but not locked).
(defun force-window-at-bottom-of-window (regexp)
  (add-to-list 'display-buffer-alist
               `(,regexp
                 (display-buffer-reuse-window
                  display-buffer-below-selected)
                 (reusable-frames . visible)
                 (side            . bottom)
                 (window-height   . 0.4))))

;; Force the some buffers to be displayed in a "side" window, i.e.
;; a window that is locked and reusable.
(defun force-window-at-right (regexp)
  (add-to-list 'display-buffer-alist
               `(,regexp
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (reusable-frames . visible)
                 (side            . right)
                 (window-height   . 0.5))))

(defun side-windows-clear ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))


(force-window-at-bottom-of-window (rx bos "*Flycheck errors*" eos))
(force-window-at-bottom-of-window (rx bos "*Flycheck error messages*" eos))
(force-window-at-bottom-of-frame (rx bos "*Help*" eos))
(force-window-at-bottom-of-frame (rx bos "*helpful"))
(force-window-at-bottom-of-frame (rx bos "*Apropos*" eos))
(force-window-at-bottom-of-frame (rx bos "*Metahelp*" eos))
(force-window-at-bottom-of-window (rx bos "*Tex errors*" eos))
(force-window-at-bottom-of-window (rx bos "*Tex Help*" eos))
(force-window-at-bottom-of-window (rx bos "*compilation*"))

(provide 'init-windows)
