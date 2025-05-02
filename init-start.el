;;; init-start.el --- Main configuration file -*- coding: utf-8 -*-

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021, 2023, 2024, 2025  Massimo Lauria
;; Time-stamp: "2025-05-03, 00:06 (CEST) Massimo Lauria"

;; Author: Massimo Lauria
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;;  This is the entry point of my main configuration. For older Emacs
;;  the fallback `init-minimal.el' is loaded.

;;; Code:

;; Fallback configuration
(when (< emacs-major-version 29)
  (error "Emacs <29 not supported by default configuration. Use fallback one"))

;; Native compilation
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-query-on-exit t)
  (setq native-comp-async-jobs-number 0) ;; half the CPUs
  (setq native-comp-async-report-warnings-errors nil))

;; Init file loaded with less aggressive garbage collector and with
;; debug support on errors
(setq debug-on-error t)
(add-hook 'after-init-hook (lambda ()
                             (setq debug-on-error nil)))

;; Performance tuning
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(setq bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)



;;; Setup Emacs environment --------------------------------------------
(defvar base-config-path "~/config/emacs/"
  "The path of the whole Emacs setup.")

(add-to-list 'load-path base-config-path)
(require 'bootstrap)    ;; package system


;; Load README.org or, if newer, README.el.
(mxl/maybe-load-org-config (concat base-config-path "README.org"))

;;; Module(s) initialization -------------------------------------------
(require 'iso-transl)

;; Bootstrap
(require 'init-functions)        ; Utility functions for configuration

;; Work environment customization
(require 'init-preferences)       ; Basic editor preferences

;; Editor behaviour customization
(require 'init-clipboard)         ; Clipboard managing

;; Writing science
(require 'init-latex)        ; AucTeX
(require 'init-bibliography) ; Bibliography


;; Applications
(require 'init-magit)
(require 'init-org-mode)   ; Organizer

;; Other stuff
(require 'init-unsorted-elisp)  ; various setups -- check for speed


;;; Customized settings -------------------------------------------------
(setq custom-file "~/config/emacs/custom.el")
(load custom-file 'noerror)

;;; Enabled/Disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'iconify-or-deiconify-frame 'disabled t)

;;; Start server --------------------------------------------------------
(require 'init-server)

(message "*** Emacs loaded in %s with %d garbage collections."
     (format "%.2f seconds"
             (float-time
              (time-subtract after-init-time before-init-time))) gcs-done)

(provide 'init-start)
;; Local Variables:
;; mode: emacs-lisp
;; End:
