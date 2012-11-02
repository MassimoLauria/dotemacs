;;;
;;; LaTex support (AucTeX) ***
;;;
;;;-----------------------------------------------------------------

;; On Mac OSX LaTeX may be installed in some non canonical path.
(when (file-directory-p "/usr/texbin/")
  (add-to-list 'exec-path "/usr/texbin/" 'append))

;; Multifile support, completition, style, reverse search support, ...
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
(setq TeX-display-help 'expert)
(setq TeX-electric-sub-and-superscript t)
(setq reftex-plug-into-AUCTeX t)
(setq bib-cite-use-reftex-view-crossref t)
(setq TeX-complete-word '(lambda () ))
(setq-default TeX-master t)  ;; Do not query for master file, and applies auto-insertion.


;; Since version 11.86 of AUCTeX the inverse/forward search is
;; implemented using source correlation.  Source correlation can be
;; realized either with source-specials, as has always been; or using
;; SyncTeX. The latter methods also works with PDF reader as Skin and
;; Evince.  The method is *guessed wrongly* in Auctex, so I switch it
;; according to TeX-PDF-mode.
;;
;; If AUCTeX implementation is below 11.86 source specials are used.
(defun init-latex--forward-search-setup ()
  "Setup the source special mode, depending on the AucTeX version loaded."
  (interactive)
  (if (not (boundp 'AUCTeX-version))
      (error "AUCTeX not installed or not loaded"))
  (let ((version (string-to-number AUCTeX-version)))
    (cond
     ((>= version 11.86)
      (setq TeX-source-correlate-mode t)
      (setq TeX-source-correlate-method 'source-specials) ; Default
      (setq TeX-source-correlate-start-server t))
     (t
      (setq TeX-source-specials-mode t)
      (setq TeX-source-specials-view-start-server t))
     )))


(defun init-latex--pdfmode-toggle ()
  "Setup the proper environment for TeX PDF mode. Mainly the
source-specials/synctex toggle."
  (interactive)
  (when (and (boundp 'AUCTeX-version)
             (>= (string-to-number AUCTeX-version) 11.86))
    (setq TeX-source-correlate-method-active
          (if TeX-PDF-mode 'synctex 'source-specials))
    ))

(add-hook 'TeX-PDF-mode-hook 'init-latex--pdfmode-toggle)

(defun init-latex--fix-viewer-invocation ()
  "Fix viewer invocation by not asking for an annoying confirmation.
      - avoid confirmation or editing of view command
      - allows for function to be used as viewer (AucTeX >= 11.86)"
  (interactive)
  (when (not running-Aquamacs)
    (let ((version (string-to-number AUCTeX-version)))
      (cond
       ((>= version 11.86)
        (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard-or-function nil t)))
       (t
        (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard nil t)))
       ))))

(eval-after-load "tex-site" '(init-latex--forward-search-setup))





;; These are the files that are produced by LaTeX processes.  It is annoying
;; that they show up while I'm trying to open a proper TeX file (or any other
;; text file).  IDO-mode can be instructed how to ignore such files.
(setq TeX-byproduct-files
'(".aux" ".log" ".brf" ".bbl" ".dvi" ".ps" ".pdf" "spl" "out" ".ps.gz" ".synctex.gz"))
(setq ido-ignore-files
      (if (boundp 'ido-ignore-files)
          (append ido-ignore-files TeX-byproduct-files)
        TeX-byproduct-files ))


;; Basic LaTeX-mode-hook setup  -- start by resetting the hook list
;; (setq LaTeX-mode-hook nil)
;; (setq TeX-mode-hook nil)

(add-hook 'TeX-mode-hook 'turn-on-reftex)
(add-hook 'TeX-mode-hook 'turn-on-flyspell)
(add-hook 'TeX-mode-hook 'autopair-latex-setup)
(add-hook 'LaTeX-mode-hook 'turn-on-ac-latex 'append)
(add-hook 'latex-mode-hook 'turn-on-ac-latex 'append)
(when-available 'aquamacs-latex-viewer-support
      (add-hook 'TeX-mode-hook 'aquamacs-latex-viewer-support 'append)) ;; load reftex first

;; All TeX made with a single keystroke (BibTeX must run at least once).
(require-maybe 'TeX-texify)

;; Various improvements to the mode
(add-hook 'LaTeX-mode-hook
          (lambda () (progn
                       (make-local-variable 'compilation-exit-message-function)
                       (setq compilation-exit-message-function 'nil)
                       (add-to-list 'LaTeX-verbatim-environments "comment")
                       (init-latex--fix-viewer-invocation)
                       )))


;; Keyboard shortcut
(add-hook 'TeX-mode-hook
          '(lambda ()
             (define-key TeX-mode-map (kbd "<f9>") 'init-latex--make)
             (define-key TeX-mode-map (kbd "<f10>") 'TeX-view)))

(defun init-latex--error-keys (style)
  "There ar edifferent ways to navigate through compilatiol
errors, depending on what system has been used to compile.

Var `style' can be either one of the symbols `compile' and `auctex'.
"
  (cond
   ((equal style 'compile)
      (local-set-key (kbd "<f11>") 'previous-error)
      (local-set-key (kbd "<f12>") 'next-error)
      (local-set-key [M-prior] 'previous-error)
      (local-set-key [M-next]  'next-error))
   ((equal style 'auctex)
      (local-set-key (kbd "<f11>")  'TeX-previous-error)
      (local-set-key (kbd "<f12>") 'TeX-next-error)
      (local-set-key [M-prior] 'TeX-previous-error)
      (local-set-key [M-next]  'TeX-next-error))))


(defun init-latex--make ()
  "Produce the document, by trying several bould commands"
  (interactive)
  (cond

   ;; if region is pinned, call master command on region
   (TeX-command-region-begin
        (init-latex--error-keys 'auctex)
        (TeX-command-region t))
   ;; is there a 'Makefile' in the folder? use that
   ((file-exists-p "Makefile")
        (set (make-local-variable 'compile-command)
             "make -k")
        (set (make-local-variable 'compilation-read-command)
             nil)
        (init-latex--error-keys 'compile)
        (call-interactively 'compile)
        (TeX-view))
   ;; or use the TeX-texify function
   ((fboundp 'TeX-texify)
        (init-latex--error-keys 'auctex)
        (call-interactively 'TeX-texify))
   ;; otherwise build the document with rubber. I would prefer this to
   ;; TeX-texify, but it does not support source correlation. Also it
   ;; is broken for paths with spaces in it.
   ((executable-find "rubber")
        (set (make-local-variable 'compile-command)
             (concat "rubber "
                     (if TeX-PDF-mode "--pdf " "")
                     (file-name-nondirectory (buffer-file-name))))
        (set (make-local-variable 'compilation-read-command)
             nil)
        (init-latex--error-keys 'compile)
        (call-interactively 'compile)
        (TeX-view))
   ;; auctex default
   (t
        (init-latex--error-keys 'auctex)
        (call-interactively 'TeX-command-master))))




(defun LaTeX-up-list ()
  "A function similar to standard Emacs `up-list', but if we are
outside of a syntax block, it attempts to escape math from
delimiters. It substitues `up-list' the first time AucTeX is
started."
  (interactive)
  (condition-case X
      ;; Try to jump to an outer syntax block.
      (up-list)
    ('error
     ;; If inside math mode of LaTeX-mode, escape from it.
     (if (or
               (eq (get-text-property (point) 'face) 'font-latex-math-face)
               (member 'font-latex-math-face (get-text-property (point) 'face)))
         (save-match-data (search-forward-regexp "\\$?\\$"))))))

;; Install LaTeX improved `up-list' command
(add-hook 'LaTeX-mode-hook
          (lambda()
            (if (boundp 'massimo-keyboard-mode-map)
                (define-key massimo-keyboard-mode-map (kbd "M-p") 'LaTeX-up-list))))


;; Setup DBUS communication between Evince and AUCTeX using SyncTeX
;; Forward/inverse search with evince using D-bus.
;; Forward search does not work with C-c C-v
(if (and (require 'dbus "dbus" t) (executable-find "evince"))
    (progn

      ;;; Inverse search setup (C-leftclick in Evince --> Open in Emacs).
      ;; Adapted from: http://www.mail-archive.com/auctex@gnu.org/msg04175.html
      ;; Changed for Gnome3 according to
      ;; http://ubuntuforums.org/showthread.php?p=11010827#post11010827
      ;;
      ;; Between Evince 2.32 and 3.2 Evince started to give url
      ;; instead of filenames.

      ;; `dbus-un-urlify' and `dbus-urlify-escape-only' should be improved to handle
      ;; all special characters, not only spaces.
      ;; The fix for spaces is based on the first comment on
      ;; http://emacswiki.org/emacs/AUCTeX#toc20

      (defun dbus-un-urlify (fname-or-url)
        "A trivial function that replaces a prefix of file:/// with just /."
        (if (string= (substring fname-or-url 0 8) "file:///")
            (url-unhex-string (substring fname-or-url 7))
          fname-or-url))

      (defun dbus-urlify-escape-only (path)
        "Handle special characters for urlify. Charachter '/' is added temporarily to the non escaped characters, so that `url-hexify-string' won't remove path separator."
        (let ((url-unreserved-chars (cons ?/ url-unreserved-chars )))
          (url-hexify-string path)))

      (defun dbus-urlify (absolute-path)
        "Transform /absolute/path to file:///absolute/path for Gnome with very limited support for special characters"
        (if (string= (substring absolute-path 0 1) "/")
            (concat "file://" (dbus-urlify-escape-only absolute-path))
          absolute-path))

      (defun auctex-evince-inverse-sync (url linecol &rest timestamp)
        (message "Try to open %s is not opened..." url)
        (let (
          (buf (get-buffer
                (file-name-nondirectory
                  (replace-regexp-in-string "%20" " " (dbus-un-urlify url)))))
          (line (car linecol))
          (col (cadr linecol)))
          (if (null buf)
              (message "Sorry, %s is not opened..." url)
        (switch-to-buffer buf)
        (goto-line (car linecol))
        (unless (= col -1)
          (move-to-column col)))))

      ;;; Forward search (View in Emacs --> Open in Evince ).
      ;; Adapted from http://dud.inf.tu-dresden.de/~ben/evince_synctex.tar.gz
      ;; Changed for Gnome3 according to
      ;; http://ubuntuforums.org/showthread.php?p=11010827#post11010827
      (defun auctex-evince-forward-sync (pdffile texfile line)
        (let ((dbus-name
           (dbus-call-method :session
                     "org.gnome.evince.Daemon"  ; service
                     "/org/gnome/evince/Daemon" ; path
                     "org.gnome.evince.Daemon"  ; interface
                     "FindDocument"
                     (dbus-urlify pdffile)
                     t     ; Open a new window if the file is not opened.
                     ))
              (time (current-time)))
          (dbus-call-method :session
                dbus-name
                "/org/gnome/evince/Window/0"
                "org.gnome.evince.Window"
                "SyncView"
                texfile
                (list :struct :int32 line :int32 1)
                (+ (* (car time) (expt 2 16)) (cadr time)))))

      (defun auctex-evince-view ()
        (interactive)
        (let ((pdf (file-truename (concat default-directory
                          (TeX-master-file (TeX-output-extension)))))
          (tex (buffer-file-name))
          (line (line-number-at-pos)))
          (auctex-evince-forward-sync pdf tex line)))


      (defun init-latex--evince--register ()
        "Evince is connected to Emacs Auctex using DBUS"
        (setq TeX-evince-dbus-registered t)
        (condition-case nil
            (dbus-register-signal
             :session nil "/org/gnome/evince/Window/0"
             "org.gnome.evince.Window" "SyncSource"
             'auctex-evince-inverse-sync)
          (error (setq TeX-evince-dbus-registered nil)))

        (when (and TeX-evince-dbus-registered
                   (boundp 'TeX-source-correlate-method))
          ;; View program and selection rule
          ;; for this to work `TeX-run-discard-or-function' must be in the "View" command.
          (add-to-list 'TeX-view-program-list '("EvinceDbus" auctex-evince-view))
          (add-to-list 'TeX-view-program-selection '(output-pdf "EvinceDbus"))
          ))

      ;; Register inverse and forward search
      (eval-after-load "tex" '(init-latex--evince--register))

      )) ;; D-Bus + Evince + SyncTeX

;; To help collaboration, in LaTeX file I will only use soft word
;; wrapping.  Furthermore the filling is made to an arbitrary large
;; value, so that fill-paragraph won't do hard-wrapping by error.
;; (that is not what my recent collaborators want! Back to 70 columns)
;;
;; From Emacs 23, the visual-line-mode helps to visualize the file
;; properly.
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (setq  default-justification 'left)
            (setq  fill-column 70)))


(if (fboundp 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode))


;; RefTeX setup
(add-hook 'reftex-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c l") 'reftex-label)       ;; Label creation
            (local-set-key (kbd "C-c r") 'reftex-reference)   ;; Label selection
            (local-set-key (kbd "C-c b") 'reftex-citation)  ;; Citation creation
            (local-set-key (kbd "M-,") 'reftex-view-crossref) ;; View crossref
            (local-set-key (kbd "M-.") 'delete-other-windows-vertically)))


;; Hints for automatic reference creation
(setq reftex-label-alist
      '(
        ("definition" ?d "def:"  "~\\ref{%s}" nil ("definition" "def.") -3)

        ("theorem"    ?h "thm:"  "~\\ref{%s}" t   ("th." "theorem") -3)
        ("lemma"      ?l "lmm:"  "~\\ref{%s}" t   ("lemma") -3)
        ("corollary"  ?c "cor:"  "~\\ref{%s}" nil ("corollary"  "cor.") -3)

        ("fact"        ?F "fact:"  "~\\ref{%s}" nil ("fact") -3)
        ("claim"       ?C "clm:"   "~\\ref{%s}" nil ("claim") -3)
        ("proposition" ?S "stm:"   "~\\ref{%s}" nil ("proposition" "prop.") -3)

        ("remark"      ?S "stm:"  "~\\ref{%s}" nil ("remark") -3)
        ("property"    ?S "stm:"  "~\\ref{%s}" nil ("property") -3)

        ("example"    ?g "eg:"   "~\\ref{%s}" nil ("example"  "ex." "e.g.") -3)
        ("exercise"   ?x "ex:"   "~\\ref{%s}" nil ("exercise") -3)

        ("open.problem" ?o "open:"   "~\\ref{%s}" nil ("problem") -3)
        ("problem"      ?p "prob:"   "~\\ref{%s}" nil ("problem") -3)

        ))


;; TeX asks for Flyspell and American dictionary.
(add-hook 'TeX-language-en-hook
	  (lambda () (ispell-change-dictionary "english")))
(add-hook 'TeX-language-it-hook
	  (lambda () (ispell-change-dictionary "italian")))



(defun init-latex--flymake-setup ()
  "Setup flymake for latex using one of the checker available on the system.
It either tries \"lacheck\" or \"chktex\"."
  (interactive)
  (cond ((executable-find "lacheck")
         (defun flymake-get-tex-args (file-name)
           (list "lacheck" (list file-name))))
        ((executable-find "chktex")
         (defun flymake-get-tex-args (file-name)
           (list "chktex" (list "-q" "-v0" file-name))))
        (t nil)))

(eval-after-load "flymake" '(init-latex--flymake-setup))


(defun my-flymake-show-help ()
   (when (get-char-property (point) 'flymake-overlay)
     (let ((help (get-char-property (point) 'help-echo)))
       (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)


;; Drag and Drop on Mac
(when-running-Aquamacs
 (add-hook
  'LaTeX-mode-hook
  (lambda ()
    (smart-dnd-setup
     '(
       ("\\.tex\\'" . "\\input{%r}\n")
       ("\\.cls\\'" . "\\documentclass{%f}\n")
       ("\\.sty\\'" . "\\usepackage{%f}\n")
       ("\\.eps\\'" . "\\includegraphics[]{%r}\n")
       ("\\.ps\\'"  . "\\includegraphics[]{%r}\n")
       ("\\.pdf\\'" . "\\includegraphics[]{%r}\n")
       ("\\.jpg\\'" . "\\includegraphics[]{%r}\n")
       ("\\.png\\'" . "\\includegraphics[]{%n}\n")
       ("\\.mov\\'" .
        "\\includemovie[\n\tposter,\n\trepeat=1,\n\ttext=(%r)\n\t]{}{}{%r}\n")
       ("\\.avi\\'" .
        "\\includemovie[\n\tposter,\n\trepeat=1,\n\ttext=(%r)\n\t]{}{}{%r}\n")))))
 )

;; Guess master file
(add-hook
 'LaTeX-mode-hook
 (lambda () (setq TeX-master (guess-TeX-master (buffer-file-name)))))

(defun guess-TeX-master (filename)
  "Guess the master file for FILENAME from currently open .tex files."
  (let ((candidate nil)
        (filename (file-name-nondirectory filename)))
    (save-excursion
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (let ((name (buffer-name))
                (file buffer-file-name))
            (if (and file (string-match "\\.tex$" file))
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward (concat "\\\\input{" filename "}") nil t)
                      (setq candidate file))
                  (if (re-search-forward (concat "\\\\include{" (file-name-sans-extension filename) "}") nil t)
                      (setq candidate file))))))))
    (if candidate
        (message "TeX master document: %s" (file-name-nondirectory candidate)))
        candidate))

;; Rubber is a nice tool which is basically a make-like system for
;; LaTeX documents
(eval-after-load "tex"
  '(progn
     (add-to-list 'TeX-expand-list
                  '("%(RubberPDF)"
                    (lambda ()
                      (if
                          (not TeX-PDF-mode)
                          ""
                        "--pdf"))))
     (add-to-list 'TeX-command-list
                  '("Rubber" "rubber %(RubberPDF) %t" TeX-run-shell t t) t)))



;; Latex autoinsertion
(defun choose-initial-latex-template ()
  "Query the user to choose a template for a new latex file"
  (interactive)
  (if (or (not (string-equal (buffer-name) "_region_.tex"))
          (called-interactively-p))
      (let ((input-char ?0))
        (loop until (member input-char '(?n ?p ?s ?l ?d ?e)) do
              (setq input-char (read-char "Template: [n]ote, [p]aper, [s]lide, [l]etter, [d]rawing, [e]mpty:")))
        (case input-char
          ((?n) (insert "latex-note-template"  ) (yas/expand))
          ((?p) (insert "latex-paper-template" ) (yas/expand))
          ((?s) (insert "latex-slides-template" ) (yas/expand))
          ((?l) (insert "latex-letter-template") (yas/expand))
          ((?d) (insert "latex-pgfpic-template") (yas/expand))
          ((?e) (insert ""))))))


;; (define-auto-insert 'latex-mode 'choose-initial-latex-template)


(provide 'init-latex)
;; Local Variables:
;; mode: emacs-lisp
;; End:
