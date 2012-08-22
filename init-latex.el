;;;
;;; LaTex support (AucTeX) ***
;;;
;;;-----------------------------------------------------------------

;; On Mac OSX LaTeX may be installed in some non canonical path.
(when running-MacOSX
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

(add-hook 'LaTeX-mode-hook
          (lambda () (progn
                       (if (fboundp 'TeX-texify)
                           (local-set-key (kbd "<f9>")    'TeX-texify)
                         (local-set-key (kbd "<f9>")    'TeX-command-master)
                         )
                       (make-local-variable 'compilation-exit-message-function)
                       (setq compilation-exit-message-function 'nil)
                       (add-to-list 'LaTeX-verbatim-environments "comment")

                       ;; The following viewer command:
                       ;;   - avoid confirmation or editing of view command
                       ;;   - allows for function to be used as viewer
                       (when (not running-Aquamacs)
                         (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard nil t))
                         )
                       )
            ))




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
(if (require 'dbus "dbus" t)
    (progn

      ;;; Inverse search setup (C-leftclick in Evince --> Open in Emacs).
      ;; Adapted from: http://www.mail-archive.com/auctex@gnu.org/msg04175.html
      ;; Changed for Gnome3 according to
      ;; http://ubuntuforums.org/showthread.php?p=11010827#post11010827
      ;;
      ;; Between Evince 2.32 and 3.2 Evince started to give url
      ;; instead of filenames.
      (defun un-urlify (fname-or-url)
        "A trivial function that replaces a prefix of file:/// with just /."
        (if (string= (substring fname-or-url 0 8) "file:///")
            (substring fname-or-url 7)
          fname-or-url))

      (defun auctex-evince-inverse-sync (url linecol &rest timestamp)
        (message "Try to open %s is not opened..." url)
        (let (
          (buf (get-buffer
                (file-name-nondirectory
                  (replace-regexp-in-string "%20" " " (un-urlify url)))))
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
                     (concat "file://" pdffile)
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
;;
;; From Emacs 23, the visual-line-mode helps to visualize the file
;; properly.
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq  default-justification 'left)
                             (setq  fill-column 99999)
							 ))
(if (fboundp 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  )


;; RefTeX setup
(add-hook 'reftex-mode-hook (lambda ()
                              (local-set-key (kbd "C-c l") 'reftex-label)       ;; Label creation
                              (local-set-key (kbd "C-c r") 'reftex-reference)   ;; Label selection
                              (local-set-key (kbd "C-c b") 'reftex-citation)  ;; Citation creation
                              (local-set-key (kbd "M-,") 'reftex-view-crossref) ;; View crossref
                              (local-set-key (kbd "M-.") 'delete-other-windows-vertically)
                              ))


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
        (t nil)
    ))

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


;; Latex autoinsertion
(defun choose-initial-latex-template ()
  "Query the user to choose a template for a new latex file"
  (interactive)
  (let ((input-char ?0))
    (loop until (member input-char '(?n ?p ?s ?l ?d ?e))
          do
          (setq input-char (read-char "Template: [n]ote, [p]aper, [s]lide, [l]etter, [d]rawing, [e]mpty:"))
          )
    (case input-char
      ((?n) (insert "latex-note-template"  ) (yas/expand))
      ((?p) (insert "latex-paper-template" ) (yas/expand))
      ((?s) (insert "latex-slides-template" ) (yas/expand))
      ((?l) (insert "latex-letter-template") (yas/expand))
      ((?d) (insert "latex-pgfpic-template") (yas/expand))
      ((?e) (insert ""))
      )
    )
  )

(define-auto-insert 'latex-mode 'choose-initial-latex-template)


(provide 'init-latex)
;; Local Variables:
;; mode: emacs-lisp
;; End:
