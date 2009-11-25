
;;; Load emacs code and personal data. -------------------------------------------------------------------------
(defvar massimo-elisp-path '("~/config/emacs" "~/config/emacs/3rdparties"))
(setq load-path (append load-path massimo-elisp-path))  
(load-file "~/personal/emacs-data.el")
;; ------------------------------------------------------------------------------------------------------------

;;{{{ *** Basic editor customization ***
(set-default-font "Monospace-10")
(add-to-list 'default-frame-alist '(font . "Monospace-10"))
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(blink-cursor-mode nil)
(setq search-highlight t)
(setq query-replace-highlight t)
(setq visible-bell nil)
(line-number-mode 1)
(setq x-stretch-cursor t)
(column-number-mode 1)
(show-paren-mode 1)
(scroll-bar-mode nil)
(tool-bar-mode nil)
(menu-bar-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(setq confirm-kill-emacs 'y-or-n-p)

;; Xterm setting
(when (or (string= (getenv "TERM") "xterm")) (xterm-mouse-mode))

;; Auto-mode for renamed config files
(setq auto-mode-alist (cons '("bashrc" . sh-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("zshrc" . sh-mode) auto-mode-alist))


;; Unified autosave and Backup dir
(defvar autosave-dir
 (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  (concat autosave-dir
   (if buffer-file-name
      (concat "#" (file-name-nondirectory buffer-file-name) "#")
    (expand-file-name
     (concat "#%" (buffer-name) "#")))))
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))
;; No backups and autosaves for tramp files
(add-to-list 'backup-directory-alist
                (cons tramp-file-name-regexp nil))
(setq tramp-auto-save-directory nil)

;;  Tab expansion
(setq-default indent-tabs-mode nil) ;; Expand tabs as spaces
(setq default-tab-width 4)

;;}}}


;;{{{ *** Key binding rules ***
;;
;;  One modifier for  intra-buffer operations (i.e. selection)
;;  Two modifiers for inter-buffer operations (i.e. navigation)
;;  Function keys for buffer processing (compile,check,...)
;;
;;  M-C-<arrow> for moving between windows 
;;  M-S-<arrow> for moving between buffers/screens
;;  CUA-selection on (C-<SPC) mark, C-<RET> rect.,C-z C-x C-c C-v)
;;  F2   for local  spell check  
;;  S-F2 for global spell check             
;;  M-Space for folding
;;  Tab for indent/complete
;;  M-Tab for correct w.r.t. spellcheck (on Flyspell)
;;


;; Management of ElScreens (M-S)
(global-set-key "\C-t" 'elscreen-create)
(global-set-key "\C-w" 'elscreen-kill)
(global-set-key "\C-b" 'elscreen-toggle-display-tab)
;; Moving between Elscreens
(global-set-key [M-S-right] 'elscreen-next) 
(global-set-key [M-S-left] 'elscreen-previous)
(global-set-key [C-tab] 'elscreen-toogle)
;; Moving between buffers (M-S)
(global-set-key [M-S-up] 'previous-buffer) 
(global-set-key [M-S-down] 'next-buffer)

;; Moving between windows
(global-set-key [M-C-right] 'windmove-right) ;; Windows
(global-set-key [M-C-left] 'windmove-left)
(global-set-key [M-C-up] 'windmove-up)
(global-set-key [M-C-down] 'windmove-down)
;; Scrolling other window
(global-set-key [M-C-prior] 'scroll-other-window-down)
(global-set-key [M-C-next] 'scroll-other-window)

;; Next/Prev error for Quickfix
(global-set-key [M-up] 'previous-error) ; Does not work with LaTeX!
(global-set-key [M-down] 'next-error)

;; Spellcheck
(global-set-key [f2] 'ispell-word)
(global-set-key [S-f2] 'ispell-buffer) 

;; Folding on/off (M-Space)
(global-set-key "\240" 'folding-toggle-show-hide) 

;; Tab is actually a "Smart tab"
(global-set-key [(tab)] 'smart-tab)

;; Remember notes.
(global-set-key [f5] 'org-remember)

;; Tweet your tweets
(global-set-key [f6] 'twit-post)
;;}}}


;;{{{ *** Primary-Clipboard selection Panic! ***
;; Cut (C-x)  Copy(C-c) Paste(C-v) Undo(C-z)
;; S-<arrow> select, C-<Ret> rectangular mark, C-<SPC> mark
(transient-mark-mode t)
(cua-mode t)
(setq cua-keep-region-after-copy t)
(setq mouse-drag-copy-region nil)   ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)	; stops killing/yanking interacting with primary X11 selection 
(setq x-select-enable-clipboard t)	; makes killing/yanking interact with clipboard X11 selection
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(when (string= (window-system) "x") 
  (setq select-active-regions t)                 ; active region sets primary X11 selection
  (global-set-key [mouse-2] 'mouse-yank-primary) ; middle-click only pastes from primary X11 selection.
  (setq yank-pop-change-selection t))            ; makes rotating the kill ring change the X11 clipboard.	
;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)
;; XTerm support
(xterm-mouse-mode t)
(global-set-key [mouse-4] 'scroll-down)
(global-set-key [mouse-5] 'scroll-up)
;;}}}


;;{{{ *** Advanced editing customization ***
;;
;; Text mode by default, with auto-fill
(setq default-major-mode 'text-mode)
(setq text-mode-hook
      '(lambda nil
	 (setq fill-column 110)
	 (auto-fill-mode 1)
     (orgtbl-mode 1)
     ;;(flyspell-mode 1)  ; annoying spell checking 
))

;; Save histories across sessions. Not buffers
(savehist-mode 1)


;; fixme highlight
(require 'fixme)

;; IDO mode for selection of file and buffers. VERY GOOD
(require 'ido)
(ido-mode t) 
(setq ido-enable-flex-matching t ; fuzzy matching is a must have
      ido-max-prospects 5        ; minibuffer is not saturated
      ido-ignore-buffers ;; ignore these guys
       '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido") 
      ido-everywhere t            ; use for many file dialogs
      ido-case-fold  t            ; be case-insensitive
      ido-auto-merge-werk-directories-length nil) ; all failed, no more digging


;; Moving between windows with (M-C-<arrow>)
(require 'windmove)               ; to load the package
(setq windmove-wrap-around t)

;; Folding
(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)
;;}}}


;;{{{ *** ELPA Managing: Emacs Lisp Package Archive ***
(require 'package)
(package-initialize)
;;}}}


;;{{{ *** Spell Checking + Language Guessing ***
(setq ispell-process-directory (expand-file-name "~/"))

(defvar guess-language-rules                                            
  '(                    
    ("italiano" . "\\<\\(un\\|uno\\|una\\|il\\|lo\\|la\\|gli\\|le|\\|per\\|in\\)\\>")           
    ("en" . "\\<\\(of\\|the\\|and\\|or\\|how\\)\\>"))                   
  "Alist of rules to determine the language of some text.               
Each rule has the form (CODE . REGEXP) where CODE is a string to        
identify the language (probably according to ISO 639), and REGEXP is a  
regexp that matches some very common words particular to that language. 
The default language should be listed first.  That will be the language 
returned when no REGEXP matches, as would happen for an empty           
document.")                                                             
                                                                         
(defun guess-buffer-language ()                                         
  "Guess language in the current buffer."                               
  (save-excursion 
        (goto-char (point-min))
        (let ((count (map 'list (lambda (x)
                                  (cons (count-matches (cdr x)) (car x)))
                          guess-language-rules)))
          (cdr (assoc (car (sort (map 'list 'car count) '>)) 
                      count)))))

(defun guess-language 
  "Guess language in the current buffer."                               
  (interactive)                                                         
  (message (guess-buffer-language)))


    
(defun set-guessed-dictionary ()
   "It tries to guess the language and to set it as dictionary for ispell"
   (let ( (language (guess-buffer-language)) )
     (if (null language)
         (message "Language unknown, ispell dictionary unchanged")
       (message "Guessing language: %s" language)
       (ispell-change-dictionary language))))

(add-hook 'find-file-hook  'set-guessed-dictionary)

;;}}}


;;{{{ *** Auto completion with SMART TAB *** 
;;
;; If a region is selected, indent.
;; If at the end of a symbol, complete
;; 

(defvar smart-tab-using-hippie-expand t
  "turn this on if you want to use hippie-expand completion.")

(setq hippie-expand-try-functions-list '(
                                         try-expand-dabbrev 
                                         try-complete-file-name-partially 
                                          try-expand-dabbrev-all-buffers 
                                         try-expand-dabbrev-from-kill 
                                         try-complete-file-name
                                         ;;try-expand-all-abbrevs
                                         try-expand-list 
                                         ;;try-expand-line 
                                         try-complete-lisp-symbol-partially 
                                         try-complete-lisp-symbol)
      )


(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
minibuffer compliant: it acts as usual in the minibuffer.

In all other buffers: if PREFIX is \\[universal-argument], calls
`smart-indent'. Else if point is at the end of a symbol,
expands it. Else calls `smart-indent'."
  (interactive "P")
  (if (minibufferp)
      (minibuffer-complete)
    (if (smart-tab-must-expand prefix)
        (if smart-tab-using-hippie-expand
            (hippie-expand nil)
          (dabbrev-expand nil))
      (smart-indent))))


(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
      (indent-region (region-beginning)
                     (region-end))
    (indent-for-tab-command)))

(defun smart-tab-must-expand (&optional prefix)
  "If PREFIX is \\[universal-argument], answers no.
Otherwise, analyses point position and answers."
  (unless (or (consp prefix)
              mark-active)
   (looking-at "\\_>")))
;;}}}    



;;{{{ *** Color Schemes (ZenBurn or tty-dark) ***
;;
(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)   ;; High color theme (xterm-256color and X11)

;; Multi-TTY support
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-variable 'color-theme-is-global nil)
            (select-frame frame)
            (if (> (display-color-cells) 255)
                (color-theme-zenburn) ;; High color theme (xterm-256color and X11)
              (color-theme-tty-dark)) ;; Low color theme (xterm or linux console)
))

;;}}}



;;{{{ *** LaTex support (AucTeX) ***
;;

;; AucTex system
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

	
;; Multifile support, completition, style, reverse search support 
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-specials-mode t)
(setq TeX-source-specials-view-start-server t)
(setq reftex-plug-into-AUCTeX t)

;; XDvi launch customization
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'TeX-command-list '("View" "%V" TeX-run-discard nil t))
                             ))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (add-to-list 'TeX-output-view-style 
                                          '("^dvi$" "." 
                                            "%(o?)xdvi -watchfile 1 %dS %d"))
                             ))


;; Auto fill for LaTex
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;; TeX asks for Flyspell and American dictionary.
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'TeX-language-en-hook
	  (lambda () (ispell-change-dictionary "american")))
(add-hook 'TeX-language-it-hook
	  (lambda () (ispell-change-dictionary "italian")))
;;}}}


;;{{{ *** Mail: Wanderlust+BBDB, IM: Twitter ***
;; autoload configuration
;; (Not required if you have installed Wanderlust as XEmacs package)
(require 'elscreen-wl)
(setq 
 wl-init-file    "~/config/mail/wl"
 wl-folders-file "~/config/mail/folders"
 wl-address-file "~/config/mail/addresses"
 bbdb-file       "~/personal/contacts.bbdb"
 diary-file      "~/personal/diary"
)



(setq wl-message-id-domaim "gmail.com")
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)


;; Use wl-draft to compose messages.
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))


(require 'bbdb)
(bbdb-initialize)
(setq bbdb-north-american-phone-numbers-p nil)


;;(require 'twit)
;;(setq twit-user "PinguinoRosso")
;;}}}


;;{{{ *** IDE facilities with ECB ***
;;(load-file "~/config/emacs/cedet/common/cedet.el")
;;(semantic-load-enable-excessive-code-helpers)
;;;; gcc setup
;;(require 'semantic-gcc)
;;;; smart complitions
;;(require 'semantic-ia)
;;(setq semantic-load-turn-useful-things-on t)
;;setq global-semantic-show-tag-boundaries-mode t)
;;setq global-semantic-show-parser-state-mode t)
;;setq semanticdb-default-save-directory "/tmp")
;;(setq semanticdb-global-mode t)
;;}}}


;;{{{ *** Imaxima and Imath support.
(require 'cl)
(pushnew "/usr/local/share/maxima/5.19.2/emacs" load-path)
(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)
(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
(autoload 'maxima "maxima" "Frontend for maxima" t)
;; Make the line effective if you want to use maxima mode with imaxima.
;(setq imaxima-use-maxima-mode-flag t)
;;}}}


;;{{{ *** FIXME CUA mode for console *** 
;; Make shifted direction keys work on the Linux console
 (when (member (getenv "TERM") '("linux"))
   (dolist (prefix '("\eO" "\eO1;" "\e[1;"))
     (dolist (m '(("2" . "S-") ("3" . "M-") ("4" . "S-M-") ("5" . "C-")
                  ("6" . "S-C-") ("7" . "C-M-") ("8" . "S-C-M-")))
       (dolist (k '(("A" . "<up>") ("B" . "<down>") ("C" . "<right>")
                    ("D" . "<left>") ("H" . "<home>") ("F" . "<end>")))
         (define-key function-key-map
                     (concat prefix (car m) (car k))
                     (read-kbd-macro (concat (cdr m) (cdr k))))))))
;;}}}



;;{{{ *** Root editing command ***
(defun root-file-reopen () 
  "Reopen this file with root privileges."
  (interactive)
  (let ((file (buffer-file-name)))
    (set-buffer (find-file (concat "/sudo::" file)))
    (rename-buffer (concat "sudo::" (buffer-name)))
    )
  )
;;}}}


;;{{{ *** Custom Safe variables ***
(setq custom-file "~/config/emacs/custom.el")
(load custom-file 'noerror)
;;}}}


;; Local Variables:
;; mode: emacs-lisp 
;; folded-file: t
;; End: 
