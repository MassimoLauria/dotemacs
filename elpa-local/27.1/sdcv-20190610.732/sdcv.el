;;; sdcv.el --- Interface for sdcv (StartDict console version).

;; Description: Interface for sdcv (StartDict console version).
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>, stardiviner(numbchild@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-05 22:04:02
;; Version: 1.5.2
;; Package-Version: 20190610.732
;; Package-Commit: 943ae3e90cc9a0a88a37cc710acd7424fd4defc4
;; Last-Updated: 2009-04-04 09:11:00
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/sdcv.el
;; Keywords: startdict, sdcv
;; Package-Requires: ((emacs "24.3") (popup "0.5.3") (showtip "0.01") (pos-tip "0.4.6") (cl-lib "0.3"))

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Interface for sdcv (StartDict console version).
;;
;; Translate word by sdcv (console version of Stardict), and display
;; translation use popup tooltip or buffer.
;;
;; Below are commands you can use:
;;
;; `sdcv-search-pointer'
;; Search around word and display with buffer.
;; `sdcv-search-pointer+'
;; Search around word and display with `popup tooltip'.
;; `sdcv-search-input'
;; Search input word and display with buffer.
;; `sdcv-search-input+'
;; Search input word and display with `popup tooltip'.
;;
;; Tips:
;;
;; If current mark is active, sdcv commands will translate
;; region string, otherwise translate word around point.
;;

;;; Installation:
;;
;; To use this extension, you have to install Stardict and sdcv
;; If you use Debian, it's simply, just:
;;
;;      sudo aptitude install stardict sdcv -y
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'sdcv)
;;
;; And then you need set two options.
;;
;;  sdcv-dictionary-simple-list         (a simple dictionary list for popup tooltip display)
;;  sdcv-dictionary-complete-list       (a complete dictionary list for buffer display)
;;
;; Example, setup like this:
;;
;; (setq sdcv-dictionary-simple-list        ;; a simple dictionary list
;;       '(
;;         "懒虫简明英汉词典"
;;         "懒虫简明汉英词典"
;;         "KDic11万英汉词典"
;;         ))
;; (setq sdcv-dictionary-complete-list      ;; a complete dictionary list
;;       '("KDic11万英汉词典"
;;         "懒虫简明英汉词典"
;;         "朗道英汉字典5.0"
;;         "XDICT英汉辞典"
;;         "朗道汉英字典5.0"
;;         "XDICT汉英辞典"
;;         "懒虫简明汉英词典"
;;         "牛津英汉双解美化版"
;;         "stardict1.3英汉辞典"
;;         "英汉汉英专业词典"
;;         "CDICT5英汉辞典"
;;         "Jargon"
;;         "FOLDOC"
;;         "WordNet"
;;         ))
;;

;;; Customize:
;;
;; `sdcv-buffer-name'
;; The name of sdcv buffer.
;;
;; `sdcv-dictionary-simple-list'
;; The dictionary list for simple describe.
;;
;; `sdcv-dictionary-complete-list'
;; The dictionary list for complete describe.
;;
;; All of the above can customize by:
;;      M-x customize-group RET sdcv RET
;;

;;; Change log:
;;
;; 2009/04/04
;;      * Fix the bug of `sdcv-search-pointer'.
;;      * Fix doc.
;;        Thanks "Santiago Mejia" report those problems.
;;
;; 2009/04/02
;;      * Remove unnecessary information from transform result.
;;
;; 2009/03/04
;;      * Refactory code.
;;      * Search region or word around point.
;;      * Fix doc.
;;
;; 2009/02/05
;;      * Fix doc.
;;
;; 2008/06/01
;;      * First released.
;;

;;; Acknowledgements:
;;
;; - pluskid@gmail.com   (Zhang ChiYuan) for sdcv-mode.el
;; - numbchild@gmail.com (stardiviner) for popup, interactive input, and pronounce.
;;

;;; TODO
;;
;; - [ ] add defcustom for popup function.
;;

;;; Require
(require 'outline)
(eval-when-compile
  (require 'cl))

(require 'showtip)
(require 'popup)
(require 'pos-tip)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup sdcv nil
  "Interface for sdcv (StartDict console version)."
  :group 'editing)

(defcustom sdcv-buffer-name "*SDCV*"
  "The name of the buffer of sdcv."
  :type 'string
  :group 'sdcv)

(defcustom sdcv-dictionary-complete-list nil
  "The complete dictionary list for translate."
  :type 'list
  :group 'sdcv)

(defcustom sdcv-dictionary-simple-list nil
  "The simply dictionary list for translate."
  :type 'list
  :group 'sdcv)

(defcustom sdcv-popup-function 'pos-tip-show
  "The popup function for showing result."
  :type '(choice
          :tag "An option to set use which popup function."
          (const :tag "pos-tip" pos-tip-show)
          (const :tag "popup" popup-tip)
          (const :tag "showtip" showtip)
          (const :tag "tooltip" tooltip-show)
          (function :tag "user custom popup function"))
  :group 'sdcv)

(defcustom sdcv-word-pronounce t
  "Auto pronounce word."
  :type 'boolean
  :group 'sdcv)

(defcustom sdcv-word-pronounce-command "espeak"
  "Specify word pronounce command."
  :type 'string
  :group 'sdcv)

(defcustom sdcv-word-pronounce-command-args "-v en"
  "Specify word pronounce command arguments."
  :type 'string
  :group 'sdcv)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sdcv-previous-window-configuration nil
  "Window configuration before switching to sdcv buffer.")

(defvar sdcv-current-translate-object nil
  "The search object.")

(defvar sdcv-filter-string "^对不起，没有发现和.*\n"
  "The filter string that sdcv output.")

(defvar sdcv-fail-notify-string "没有发现解释也... \n用更多的词典查询一下吧! ^_^"
  "This string is for notify user when search fail.")

(defvar sdcv-mode-font-lock-keywords    ;keyword for buffer display
  '(
    ;; Dictionary name
    ("^-->\\(.*\\)\n-" . (1 font-lock-type-face))
    ;; Search word
    ("^-->\\(.*\\)[ \t\n]*" . (1 font-lock-function-name-face))
    ;; Serial number
    ("\\(^[0-9] \\|[0-9]+:\\|[0-9]+\\.\\)" . (1 font-lock-constant-face))
    ;; Type name
    ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
    ;; Phonetic symbol
    ("^\\/\\([^>]*\\)\\/$" . (1 font-lock-string-face))
    ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face))
    )
  "Expressions to highlight in `sdcv-mode'.")

(defvar sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    ;; sdcv result buffer commands.
    (define-key map "q" 'sdcv-quit)
    (define-key map "j" 'sdcv-next-line)
    (define-key map "k" 'sdcv-prev-line)
    (define-key map "l" 'forward-char)
    (define-key map "h" 'backward-char)
    (define-key map "J" 'sdcv-scroll-up-one-line)
    (define-key map "K" 'sdcv-scroll-down-one-line)
    (define-key map "M-g M-n" 'sdcv-next-dictionary)
    (define-key map "M-g M-p" 'sdcv-previous-dictionary)
    (define-key map "i" 'sdcv-search-input)
    (define-key map "I" 'sdcv-search-input+)
    (define-key map "p" 'sdcv-search-pointer)
    (define-key map "P" 'sdcv-search-pointer+)
    ;; Isearch.
    (define-key map "S" 'isearch-forward-regexp)
    (define-key map "R" 'isearch-backward-regexp)
    (define-key map "s" 'isearch-forward)
    (define-key map "r" 'isearch-backward)
    ;; Hideshow.
    (define-key map "a" 'show-all)
    (define-key map "A" 'hide-body)
    (define-key map "v" 'show-entry)
    (define-key map "V" 'hide-entry)
    ;; Misc.
    (define-key map "e" 'scroll-down)
    (define-key map " " 'scroll-up)
    (define-key map "?" 'describe-mode)
    map)
  "Keymap for `sdcv-mode'.")

;;;###autoload
(define-derived-mode sdcv-mode nil "sdcv"
  "Major mode to look up word through sdcv.
\\{sdcv-mode-map}
Turning on Text mode runs the normal hook `sdcv-mode-hook'."
  (setq font-lock-defaults '(sdcv-mode-font-lock-keywords))
  (setq buffer-read-only t)
  (set (make-local-variable 'outline-regexp) "^-->.*\n-->"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun sdcv-search-pointer (&optional word)
  "Get current word.
And display complete translations in other buffer."
  (interactive)
  ;; Display details translate result.
  (sdcv-search-detail (or word (sdcv-region-or-word))))

;;;###autoload
(defun sdcv-search-pointer+ ()
  "Translate current point word.
And show information use tooltip.
But this function use a simple dictionary list."
  (interactive)
  (let ((word (sdcv-region-or-word)))
    ;; Display simple translate result.
    (sdcv-search-simple word)))

;;;###autoload
(defun sdcv-search-input (&optional word)
  "Translate current input WORD.
And show information in other buffer."
  (interactive)
  ;; Display details translate result.
  (sdcv-search-detail (or word (sdcv-prompt-input))))

;;;###autoload
(defun sdcv-search-input+ (&optional word)
  "Translate current point WORD.
And show information use tooltip."
  (interactive)
  ;; Display simple translate result.
  (sdcv-search-simple (or word (sdcv-prompt-input)))
  ;; I set this delay for fast finger. ;)
  (sit-for 0.5))

(defun sdcv-quit ()
  "Bury sdcv buffer and restore the previous window configuration."
  (interactive)
  (if (window-configuration-p sdcv-previous-window-configuration)
      (progn
        (set-window-configuration sdcv-previous-window-configuration)
        (setq sdcv-previous-window-configuration nil)
        (bury-buffer (sdcv-get-buffer)))
    (bury-buffer)))

(defun sdcv-next-dictionary ()
  "Jump to next dictionary."
  (interactive)
  (show-all)
  (if (search-forward-regexp "^-->.*\n-" nil t) ;don't show error when search failed
      (progn
        (call-interactively 'previous-line)
        (recenter 0))
    (message "Have reach last dictionary.")))

(defun sdcv-previous-dictionary ()
  "Jump to previous dictionary."
  (interactive)
  (show-all)
  (if (search-backward-regexp "^-->.*\n-" nil t) ;don't show error when search failed
      (progn
        (forward-char 1)
        (recenter 0))                   ;adjust position
    (message "Have reach first dictionary.")))

(defun sdcv-scroll-up-one-line ()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun sdcv-scroll-down-one-line ()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(defun sdcv-next-line (arg)
  "Next ARG line and show item."
  (interactive "P")
  (ignore-errors
    (call-interactively 'next-line arg)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))

(defun sdcv-prev-line (arg)
  "Previous ARG line."
  (interactive "P")
  (ignore-errors
    (call-interactively 'previous-line arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sdcv-search-detail (&optional word)
  "Search WORD through the `command-line' tool sdcv.
The result will be displayed in buffer named with
`sdcv-buffer-name' with `sdcv-mode'."
  (message "Search...")
  (with-current-buffer (get-buffer-create sdcv-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let ((process
           (apply #'start-process
                  "sdcv" sdcv-buffer-name "sdcv"
                  (sdcv-search-with-dictionary-args
                   (or word (sdcv-region-or-word))
                   sdcv-dictionary-complete-list))))
      (set-process-sentinel
       process
       (lambda (process signal)
         (when (memq (process-status process) '(exit signal))
           (unless (eq (current-buffer) (sdcv-get-buffer))
             (sdcv-goto-sdcv))
           (let ((str (sdcv-filter (buffer-string))))
             (let ((inhibit-read-only t))
               (erase-buffer)
               (insert str)))
           (sdcv-mode-reinit))))))

  ;; pronounce the word (Add by me)
  (sdcv-pronounce-word word)
  )

(defun sdcv-search-simple (&optional word)
  "Search WORD simple translate result."
  (funcall
   sdcv-popup-function
   (sdcv-filter
    (shell-command-to-string
     (mapconcat #'identity
                (cons "sdcv" (sdcv-search-with-dictionary-args
                              (or word (sdcv-region-or-word))
                              sdcv-dictionary-simple-list))
                " "))))

  ;; pronounce the word (Add by me)
  (when sdcv-word-pronounce
    ;; `sleep-for', `sit-for'.
    (sleep-for 1)
    (sdcv-pronounce-word word)
    )
  )

(defun sdcv-search-with-dictionary-args (word dictionary-list)
  "Construct list of arguments to search for WORD.

Specify dictionaries to search in DICTIONARY-LIST."
  (setq sdcv-current-translate-object word)
  (let ((args `("-n" ,(substring-no-properties word))))
    (dolist (dict dictionary-list)
      (push dict args)
      (push "-u" args))
    args))

(defun sdcv-filter (sdcv-string)
  "This function is for filter sdcv output string,.
Argument SDCV-STRING the search string from sdcv."
  (setq sdcv-string (replace-regexp-in-string sdcv-filter-string "" sdcv-string))
  (if (equal sdcv-string "")
      sdcv-fail-notify-string
    (with-temp-buffer
      (insert sdcv-string)
      (goto-char (point-min))
      (kill-line 1)                     ;remove unnecessary information.
      (buffer-string))))

(defun sdcv-goto-sdcv ()
  "Switch to sdcv buffer in other window."
  (setq sdcv-previous-window-configuration (current-window-configuration))
  (let* ((buffer (sdcv-get-buffer))
         (window (get-buffer-window buffer)))
    (if (null window)
        (switch-to-buffer-other-window buffer)
      (select-window window))))

(defun sdcv-get-buffer ()
  "Get the sdcv buffer.  Create one if there's none."
  (let ((buffer (get-buffer-create sdcv-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'sdcv-mode)
        (sdcv-mode)))
    buffer))

(defun sdcv-mode-reinit ()
  "Re-initialize buffer.
Hide all entry but the first one and goto
the beginning of the buffer."
  (ignore-errors
    (setq buffer-read-only t)
    (goto-char (point-min))
    (sdcv-next-dictionary)
    (show-all)
    (message "Have search finished with `%s'." sdcv-current-translate-object)))

(defun sdcv-prompt-input ()
  "Prompt input object for translate."
  (read-string (format "Word (%s): " (or (sdcv-region-or-word) ""))
               nil nil
               (sdcv-region-or-word)))

(defun sdcv-region-or-word ()
  "Return region or word around point.
If `mark-active' on, return region string.
Otherwise return word around point."
  (if mark-active
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (thing-at-point 'word)))


;;; I add this function to pronounce
(defun sdcv-pronounce-word (&optional word)
  "Pronounce `WORD' after querying."
  (call-process-shell-command
   sdcv-word-pronounce-command
   nil nil nil
   sdcv-word-pronounce-command-args
   (shell-quote-argument word)
   ))

(provide 'sdcv)

;;; sdcv.el ends here

;;; LocalWords:  sdcv StartDict startdict showtip stardict KDic XDICT CDICT
;;; LocalWords:  FOLDOC WordNet ChiYuan Hideshow reinit
