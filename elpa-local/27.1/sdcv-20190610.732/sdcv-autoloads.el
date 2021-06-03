;;; sdcv-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "sdcv" "sdcv.el" (0 0 0 0))
;;; Generated autoloads from sdcv.el

(autoload 'sdcv-mode "sdcv" "\
Major mode to look up word through sdcv.
\\{sdcv-mode-map}
Turning on Text mode runs the normal hook `sdcv-mode-hook'.

\(fn)" t nil)

(autoload 'sdcv-search-pointer "sdcv" "\
Get current word.
And display complete translations in other buffer.

\(fn &optional WORD)" t nil)

(autoload 'sdcv-search-pointer+ "sdcv" "\
Translate current point word.
And show information use tooltip.
But this function use a simple dictionary list." t nil)

(autoload 'sdcv-search-input "sdcv" "\
Translate current input WORD.
And show information in other buffer.

\(fn &optional WORD)" t nil)

(autoload 'sdcv-search-input+ "sdcv" "\
Translate current point WORD.
And show information use tooltip.

\(fn &optional WORD)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "sdcv" '("sdcv-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; sdcv-autoloads.el ends here
