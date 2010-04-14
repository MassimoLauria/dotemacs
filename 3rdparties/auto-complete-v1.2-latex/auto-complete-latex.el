;;; auto-complete-latex.el --- A LaTeX extention for auto-complete-mode

;; Copyright (C) 2010 tequilasunset

;; Author:   tequilasunset <tequilasunset.mac@gmail.com>
;; Keywords: LaTeX AUCTeX YaTeX
;; Version:  0.2.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;;; Requirements:

;;   auto-complete-mode v1.2:
;;
;;     http://cx4a.org/software/auto-complete/index.html
;;
;;   auto-complete-latex-lib.el:
;;
;;     http://bitbucket.org/tequilasunset/auto-complete-latex/src/
;;

;;; NOTICE:

;; This version requires auto-complete.el v1.2. If you are using old
;; version of it, upgrade now!
;;

;;; Commentary:

;; Put files into your load-path, and add the following into your .emacs.
;;
;;   (require 'auto-complete-latex)
;;   (require 'auto-complete-latex-lib)
;;
;; If necessary, add the following into your .emacs.
;;
;;   (setq ac-modes (append ac-modes '(foo-mode)))
;;   (add-hook 'foo-mode-hook 'ac-l-setup)
;;
;; You can access to the configuration with key C-c C-a or M-x ac-l-status.
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'auto-complete)
(require 'ispell)

(defgroup auto-complete-latex nil
  "Auto completion of LaTeX keywords."
  :group 'auto-complete
  :group 'tex
  :prefix "ac-l-")


;;;; variables

(defvar ac-l-status-key "C-c C-a"
  "*Key for `ac-l-status'. This variable has to be a string
understood by the `kbd' macro. You have to set it in your .emacs.")

(defcustom ac-l-level 1
  "*An amount of dictionary sources.

1: LaTeX basic commands and package commands
2: 1, primitives and macro commands
3: 1, 2 and commands defined in latex.ltx"
  :type 'integer
  :group 'auto-complete-latex)

(defcustom ac-l-target nil
  "*Specify LaTeX target file name as string. `nil' means current buffer.
Target file needs to include the informations about external packages
\(e.g. \\usepackage, \\RequirePackage). `auto-complete-latex' searches
those informations and creates proper candidates automatically
\(see `ac-l-update'). You can change target file at any time with key
\\[ac-l-status]."
  :type '(choice (string :tag "buffer name")
                 (const :tag "current buffer" nil))
  :group 'auto-complete-latex)

(defcustom ac-l-use-ispell nil
  "*If non-nil, use ispell for completion."
  :type 'boolean
  :group 'auto-complete-latex)

(defcustom ac-l-do-nothing nil
  "*If non-nil, `auto-complete-latex' won't set up anything."
  :type 'boolean
  :group 'auto-complete-latex)

(defcustom ac-l-user-commands nil
  "*A list of user commands.
If you set user variables, use `ac-l-user-variables'."
  :type '(repeat string)
  :group 'auto-complete-latex)

(defcustom ac-l-user-variables nil
  "*A list of user variables and keywords except for commands.
If you set user commands, use `ac-l-user-commands'."
  :type '(repeat string)
  :group 'auto-complete-latex)

(defcustom ac-l-package-sources nil
  "*Alist of external package sources. Each list must be the form
\(PACKAGE NAME REGEXP . SOURCE NAME). The sources in this alist
will be included in `ac-sources' automatically by `ac-l-update'."
  :type '(repeat (cons regexp symbol))
  :group 'auto-complete-latex)

(defcustom ac-l-sources nil
  "*A list of command sources. The sources in this list is always
included in `ac-sources'."
  :type '(repeat symbol)
  :group 'auto-complete-latex)

(defvar ac-l-variable-sources nil
  "Internal variable.")
(defvar ac-l-command-sources nil
  "Internal variable.")
(defvar ac-l-variable-sources* nil
  "Internal variable.")
(defvar ac-l-command-sources* nil
  "Internal variable.")
(defvar ac-l-major-mode nil
  "Internal variable.")
(defvar ac-l-basic-commands nil
  "Internal variable.")


;;;; setup

(defun ac-l-after-init-setup ()
  (unless ac-l-do-nothing
    (setq ac-modes (append ac-modes '(latex-mode)))
    (add-hook 'latex-mode-hook 'ac-l-setup)
    ;; for AUCTeX users
    (and (require 'tex-site nil t)
         (add-hook 'LaTeX-mode-hook 'ac-l-setup))
    ;; for YaTeX users
    (when (fboundp 'yatex-mode)
      (setq ac-modes (append ac-modes '(yatex-mode)))
      (add-hook 'yatex-mode-hook 'ac-l-setup))))

(add-hook 'after-init-hook 'ac-l-after-init-setup)

(defun ac-l-setup ()
  (setq ac-l-major-mode major-mode)
  (local-set-key (read-kbd-macro ac-l-status-key) 'ac-l-status)
  (defadvice switch-to-buffer (after ac-l-switch-&-update activate)
    (and (eq major-mode ac-l-major-mode) (ac-l-update)))
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'ac-l-update)
  ;; source
  (cond ((memq 'ac-source-words-in-buffer ac-sources)
         (setq ac-l-variable-sources* '(ac-l-source-words-in-buffer
                                        ac-source-dictionary))
         (setq ac-l-command-sources* '(ac-l-source-commands-in-buffer)))
        ((memq 'ac-source-words-in-same-mode-buffers ac-sources)
         (setq ac-l-variable-sources* '(ac-l-source-words-in-same-mode-buffers
                                        ac-source-dictionary))
         (setq ac-l-command-sources* '(ac-l-source-commands-in-same-mode-buffers)))
        ((memq 'ac-source-words-in-all-buffer ac-sources)
         (setq ac-l-variable-sources* '(ac-l-source-words-in-all-buffer
                                        ac-source-dictionary))
         (setq ac-l-command-sources* '(ac-l-source-commands-in-all-buffer))))
  (loop for source in (reverse ac-l-sources)
        if (string-match "-commands$" (symbol-name source)) do
        (push source ac-l-command-sources)
        if (string-match "-variables$" (symbol-name source)) do
        (push source ac-l-variable-sources))
  (ac-l-update))


;;;; functions

(defun ac-l-real-target ()
  "Return real target buffer name."
  (cond
   ((stringp ac-l-target) (or (get-buffer ac-l-target) (current-buffer)))
   ((buffer-live-p ac-l-target) ac-l-target)
   (t (current-buffer))))

(defun ac-l-update ()
  "Set `ac-sources' according to the informations about target buffer
\(see `ac-l-target') and user configurations. This function is
executed automatically in the following cases. Moreover, you can
call it interactively.

1: After the function `save-buffer'.
2: After the function `switch-to-buffer'.
3: At the first of the function `ac-l-status' and at the end of it.

You can add sources and change the order of it. For users, the
following variables are available.

1: `ac-l-package-sources' (If package loaded, included in candidates.)
2: `ac-l-sources'         (Always included in candidates.)

If you want to simply add some keywords, use the following variables.
Keywords in those lists are always included in candidates.

1: `ac-l-user-variables'
2: `ac-l-user-commands'"
  (interactive)
  (let* ((reg "^[^%\n]*\\\\\\(usep\\|RequireP\\)ackage\\(\\[[^]]*\\]\\)?{\\([ a-zA-Z%,\t\n]+\\)")
         (list (reverse ac-l-package-sources))
         (regexp-list (mapcar #'car list))
         (source-list (mapcar #'cdr list))
         packages
         command-sources variable-sources
         package-command-sources package-variable-sources)
    ;; check packages
    (save-excursion
      (set-buffer (ac-l-real-target))
      (goto-char (point-min))
      (while (and (re-search-forward reg nil t) (<= (point) 60000))
        (push (match-string-no-properties 3) packages)))
    (dolist (package packages)
      (setq package (split-string package "[ %,\t\n]+" t))
      (dolist (p package)
        (loop for regexp in regexp-list
              for source in source-list
              if (and (string-match regexp p)
                      (string-match "-commands$" (symbol-name source))) do
              (push source package-command-sources)
              if (and (string-match regexp p)
                      (string-match "-variables$" (symbol-name source))) do
              (push source package-variable-sources))))
    ;; reset ac-sources
    (setq ac-sources ac-l-variable-sources*)
    ;; set up ac-sources
    ;; --variable--
    (and ac-l-use-ispell (push 'ac-l-source-ispell ac-sources))
    (setq ac-sources (append package-variable-sources ac-sources))
    (setq ac-sources (append ac-l-variable-sources ac-sources))
    ;; --command--
    (setq ac-sources (append ac-l-command-sources* ac-sources))
    ;; etags
    ;; (and (or tags-file-name tags-table-list)
    ;;      (push 'ac-l-source-etags ac-sources))
    (and (integerp ac-l-level)
         (>= ac-l-level 3)
         (push 'ac-l-source-latex-dot-ltx ac-sources))
    (and (integerp ac-l-level)
         (>= ac-l-level 2)
         (push 'ac-l-source-macro-commands ac-sources)
         (push 'ac-l-source-primitives ac-sources))
    (setq ac-sources (append package-command-sources ac-sources))
    (setq ac-sources (append ac-l-command-sources ac-sources))
    (push 'ac-source-filename ac-sources)))


;;;; sources

;;; I don't recommend to use it.
;; (defvar ac-l-source-etags
;;   '((candidates . (lambda ()
;;                     (all-completions ac-prefix (tags-completion-table))))
;;     (symbol . "e")
;;     (prefix . "\\\\\\([a-zA-Z@]+\\)")
;;     (requires . 3))
;;   "Source for etags.")

(defvar ac-l-source-ispell
  '((candidates . (lookup-words
                   (concat ac-prefix "*") ispell-complete-word-dict))
    (symbol . "i")
    (requires . 3))
  "Source for ispell.")


;;;; Standard sources

;;; normal words
(defun ac-l-candidate-words-in-buffer (limit)
  (let ((i 0)
        (regexp (concat
                 "[^\\]\\(\\_<" (regexp-quote ac-prefix) "[a-zA-Z---]+\\)"))
        candidate candidates)
    (save-excursion
      ;; Search backward
      (goto-char ac-point)
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-backward regexp nil t))
        (setq candidate (match-string-no-properties 1))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      ;; Search backward
      (goto-char (+ ac-point (length ac-prefix)))
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-forward regexp nil t))
        (setq candidate (match-string-no-properties 1))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      (nreverse candidates))))

(defvar ac-l-source-words-in-buffer
  '((candidates . (ac-l-candidate-words-in-buffer ac-limit)))
  "Source for normal words in current buffer.")

(defvar ac-l-word-index nil)

(ac-clear-variable-after-save 'ac-l-word-index)

(defun ac-l-build-word-index ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (local-variable-p 'ac-l-word-index)
        (make-local-variable 'ac-l-word-index))
      (when (and (null ac-l-word-index)
                 (< (buffer-size) 102400))
        (let ((ac-prefix "")
              (ac-point (point-min)))
          (setq ac-l-word-index (ac-l-candidate-words-in-buffer nil)))))))

(defun ac-l-word-candidates (&optional buffer-pred)
  (loop initially (setq candidates (ac-l-candidate-words-in-buffer nil))
        for buffer in (buffer-list)
        if (and (or (not (integerp ac-limit)) (< (length candidates) ac-limit))
                (if buffer-pred (funcall buffer-pred buffer) t))
        append (buffer-local-value 'ac-l-word-index buffer) into candidates
        finally return candidates))

(defvar ac-l-source-words-in-all-buffer
  '((init . ac-l-build-word-index)
    (candidates . ac-l-word-candidates))
  "Source for normal words in all buffers.")

(defvar ac-l-source-words-in-same-mode-buffers
  '((init . ac-l-build-word-index)
    (candidates . (ac-l-word-candidates
                   (lambda (buffer)
                     (derived-mode-p (buffer-local-value 'major-mode buffer))))))
  "Source for normal words in same mode buffers.")

;;; LaTeX commands
(defun ac-l-candidate-commands-in-buffer (limit)
  (let ((i 0)
        (regexp (concat "\\\\\\(" (regexp-quote ac-prefix) "[a-zA-Z@]+\\)"))
        candidate candidates)
    (save-excursion
      (goto-char ac-point)
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-backward regexp nil t))
        (setq candidate (match-string-no-properties 1))
        (unless (and (member candidate candidates)
                     (member candidate ac-l-basic-commands)
                     (member candidate ac-l-user-commands))
          (push candidate candidates)
          (incf i)))
      (goto-char (+ ac-point (length ac-prefix)))
      (while (and (or (not (integerp limit)) (< i limit))
                  (re-search-forward regexp nil t))
        (setq candidate (match-string-no-properties 1))
        (unless (member candidate candidates)
          (push candidate candidates)
          (incf i)))
      (nreverse candidates))))

(defvar ac-l-source-commands-in-buffer
  '((candidates . (ac-l-candidate-commands-in-buffer ac-limit))
    (prefix . "\\\\\\([a-zA-Z@]+\\)"))
  "Source for LaTeX commands in current buffer.")

(defvar ac-l-command-index nil)

(ac-clear-variable-after-save 'ac-l-command-index)

(defun ac-l-build-command-index ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless (local-variable-p 'ac-l-command-index)
        (make-local-variable 'ac-l-command-index))
      (when (and (null ac-l-command-index)
                 (< (buffer-size) 102400))
        (let ((ac-prefix "")
              (ac-point (point-min)))
          (setq ac-l-command-index (ac-l-candidate-commands-in-buffer nil)))))))

(defun ac-l-command-candidates (&optional buffer-pred)
  (loop initially (setq candidates (ac-l-candidate-commands-in-buffer nil))
        for buffer in (buffer-list)
        if (and (or (not (integerp ac-limit)) (< (length candidates) ac-limit))
                (if buffer-pred (funcall buffer-pred buffer) t))
        append (buffer-local-value 'ac-l-command-index buffer) into candidates
        finally return candidates))

(defvar ac-l-source-commands-in-all-buffer
  '((init . ac-l-build-command-index)
    (candidates . ac-l-command-candidates)
    (prefix . "\\\\\\([a-zA-Z@]+\\)"))
  "Source for LaTeX commands in all buffers.")

(defvar ac-l-source-commands-in-same-mode-buffers
  '((init . ac-l-build-command-index)
    (candidates . (ac-l-command-candidates
                   (lambda (buffer)
                     (derived-mode-p (buffer-local-value 'major-mode buffer)))))
    (prefix . "\\\\\\([a-zA-Z@]+\\)"))
  "Source for LaTeX commands in same mode buffers.")


;;;; utility

(defun ac-l-status ()
  "Show status. You can modify it interactively."
  (interactive)
  (ac-l-update)
  (let ((num (read-number (format "\
Status: 1.Level:%d       2.Ispell:%s     3.Target:%s\n\
        4.Find Target   5.Clear Target   6.AC Sources\n\
Number: "
                                  ac-l-level
                                  (if ac-l-use-ispell 'on 'off)
                                  (or ac-l-target 'current-buffer)))))
    (cond
     ((= num 1)
      (let ((level (read-number "Set level (1, 2 or 3): ")))
        (cond ((= level 99999999)
               (setq ac-l-level 99999999)
               (message "Okay, I set your level 99999999. Enjoy! :p"))
              ((and (>= level 1) (<= level 3))
               (setq ac-l-level level)
               (message "Set level %d." level))
              (t (error "Wrong number :(")))))
     ((= num 2)
      (if ac-l-use-ispell
          (progn
            (setq ac-l-use-ispell nil)
            (message "Turn ispell off."))
        (setq ac-l-use-ispell t)
        (message "Turn ispell on.")))
     ((= num 3)
      (let ((buf (read-buffer "Target on: " (current-buffer) t)))
        (setq ac-l-target buf)
        (message "Target on %s." buf)))
     ((= num 4)
      (switch-to-buffer
       (find-file-noselect
        (let ((find-file-default (and buffer-file-name
                                      (abbreviate-file-name buffer-file-name))))
          (minibuffer-with-setup-hook
              (lambda ()
                (setq minibuffer-default find-file-default))
            (read-file-name "Find target: " nil default-directory t)))))
      (setq ac-l-target (buffer-name))
      (message "Target on %s." (buffer-name)))
     ((= num 5)
      (setq ac-l-target nil)
      (message "Target on current-buffer."))
     ((= num 6) (describe-variable 'ac-sources))
     (t (error "Wrong number :("))))
  (ac-l-update))


(provide 'auto-complete-latex)

;;; auto-complete-latex.el ends here