;;; org-google-weather.el --- Show Google Weather forecasts in Org agenda.

;; Copyright (C) 2010 Julien Danjou

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This module allows to display the weather forecast fetched from Google in
;; your Org agenda.
;;
;;     Wednesday   8 September 2010
;;       Weather:    Pluie, 12/18 ℃
;;     Thursday    9 September 2010
;;       Weather:    Couverture nuageuse partielle, 11/21 ℃
;;
;; Just add the following in an Org buffer:
;; %%(org-google-weather)
;;
;;; Code:

(require 'google-weather)
(require 'image)
(require 'format-spec)

(defgroup org-google-weather nil
  "Google Weather for Org mode."
  :group 'comm)

(defcustom org-google-weather-location "Paris"
  "Default location for org-google-weather."
  :group 'org-google-weather)

(defcustom org-google-weather-format "%i %c, %l-%h %s"
  "String to return to describe the weather.
Valid %-sequences are:
  - %i the icon
  - %c means the weather condition
  - %L the supplied location
  - %C the city the weather is for
  - %l the lower temperature
  - %h the higher temperature
  - %s the temperature unit symbol")

(defcustom org-google-weather-cache-time 43200
  "Define for how many seconds we should cache the weather."
  :group 'org-google-weather)

(defcustom org-google-weather-display-icon-p t
  "Display icons."
  :group 'org-google-weather)

(defcustom org-google-weather-icon-directory "/usr/share/icons/gnome/16x16/status"
  "Directory where to find icon listed in `org-google-weather-icon-alist'."
  :group 'org-google-weather)

(defcustom org-google-weather-icon-alist
  '((chance_of_rain . "weather-showers-scattered.png")
    (chance_of_snow . "weather-snow.png")
    (chance_of_storm . "weather-storm.png")
    (cn_cloudy . "weather-overcast.png")
    (cn_heavyrun . "weather-showers.png")
    (cn_sunny . "weather-clear.png")
    (cloudy . "weather-overcast.png")
    (dust . "weather-fog.png")
    (flurries . "weather-storm.png")
    (fog . "weather-fog.png")
    (haze . "weather-fog.png")
    (icy . "weather-snow.png")
    (jp_sunny . "weather-clear.png")
    (jp_cloudy . "weather-overcast.png")
    (mist . "weather-storm.png")
    (mostly_cloudy . "weather-overcast.png")
    (mostly_sunny . "weather-clear.png")
    (partly_cloudy . "weather-few-clouds.png")
    (rain . "weather-showers.png")
    (sleet . "weather-snow.png")
    (smoke . "weather-fog.png")
    (snow . "weather-snow.png")
    (storm . "weather-storm.png")
    (thunderstorm . "weather-storm.png")
    (sunny . "weather-clear.png"))
  "Icons to use to illustrate the weather.")

;;;###autoload
(defun org-google-weather (&optional location language)
  "Return Org entry with the weather for LOCATION in LANGUAGE.
If LOCATION is not set, use org-google-weather-location."
  (let* ((data (google-weather-get-data (or location
                                            org-google-weather-location)
                                        language
                                        org-google-weather-cache-time))
         (forecast (google-weather-data->forecast-for-date data date)))
    (when forecast
      (let ((condition (cadr (assoc 'condition forecast)))
            (low (cadr (assoc 'low forecast)))
            (high (cadr (assoc 'high forecast)))
            (city (google-weather-data->city data))
            ;; But *they* told me it's just about calling functions!
            (icon (when (window-system)
                    (cdr
                     (assoc
                      (intern
                       (file-name-sans-extension
                        (file-name-nondirectory
                         (cadr (assoc 'icon forecast)))))
                      org-google-weather-icon-alist))))
            (temp-symbol (google-weather-data->temperature-symbol data)))
        (format-spec org-google-weather-format
                     `((?i . ,(if (and icon org-google-weather-display-icon-p)
                                  (concat (propertize "icon"
                                                      'display
                                                      (create-image
                                                       (concat
                                                        org-google-weather-icon-directory
                                                        "/"
                                                        icon))
                                                      'rear-nonsticky '(display))
                                          " ")
                                ""))
                       (?c . ,condition)
                       (?L . ,location)
                       (?C . ,city)
                       (?l . ,low)
                       (?h . ,high)
                       (?s . ,temp-symbol)))))))

(provide 'org-google-weather)
