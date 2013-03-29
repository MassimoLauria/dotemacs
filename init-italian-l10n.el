;;; init-italian-l10n.el --- Italian localization stuff

;; Copyright (C) 2012, 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-03-29, 18:59 (CET) Massimo Lauria>

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

;; This is the part of my init files devoted to italian localization
;; of some of the emacs variables.


;; Calendar localization
(setq calendar-week-start-day 1
      calendar-day-name-array ["Domenica" "Lunedì" "Martedì" "Mercoledì"
                               "Giovedì" "Venerdì" "Sabato"]
      calendar-month-name-array ["Gennaio" "Febbraio" "Marzo" "Aprile" "Maggio"
                                 "Giugno" "Luglio" "Agosto" "Settembre"
                                 "Ottobre" "Novembre" "Dicembre"])





;;; variables from `parse-time.el'
(require 'parse-time)

(defvar parse-time-weekdays-orig parse-time-weekdays
  "The original Emacs value of `parse-time-weekdays', which has
  been modified to include italian weekday names.")

(defvar parse-time-months-orig parse-time-months
  "The original Emacs value of `parse-time-months', which has
  been modified to include italian month names.")

(defconst parse-time-weekdays-ita '(
      ("dom" . 0)
      ("lun" . 1)
      ("mar" . 2)
      ("mer" . 3)
      ("gio" . 4)
      ("ven" . 5)
      ("sab" . 6)
      ("domenica" . 0)
      ("lunenì" . 1)
      ("martedì" . 2)
      ("mercoledì" . 3)
      ("giovedì" . 4)
      ("venerdì" . 5)
      ("sabato" . 6))
  "Italian weekday names added to `parse-time-weekdays'.")


(defconst parse-time-months-ita '(("gen" . 1) ("feb" . 2) ("mar" . 3)
			    ("apr" . 4) ("mag" . 5) ("giu" . 6)
			    ("lug" . 7) ("ago" . 8) ("set" . 9)
			    ("ott" . 10) ("nov" . 11) ("dic" . 12)
			    ("gennaio" . 1) ("febbraio" . 2)
			    ("marzo" . 3) ("aprile" . 4) ("maggio" . 5) ("giugno" . 6)
			    ("luglio" . 7) ("agosto" . 8)
			    ("settembre" . 9) ("ottobre" . 10)
			    ("novembre" . 11) ("dicembre" . 12)))



;; Add the italian weekday names to `parse-time-weekdays'
(setq parse-time-weekdays (append parse-time-weekdays-orig parse-time-weekdays-ita))
;; Add the italian month names to `parse-time-months'
(setq parse-time-months (append parse-time-months-orig parse-time-months-ita))

;; Add important european timezones
(add-to-list 'parse-time-zoneinfo  '("cet" 3600 t) t)  ;; Central European Time
(add-to-list 'parse-time-zoneinfo  '("cest" 7200)  t)  ;; Central European Summer Time


(provide 'init-italian-l10n)
