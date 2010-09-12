;;; google-weather.el --- Fetch Google Weather forecasts.

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
;; This module allows you to fetch Google Weather forecast from the
;; Internet.
;;
;;; Code:

(require 'url)
(require 'url-cache)
(require 'xml)
(require 'time-date)

(eval-when-compile
  (require 'cl))

(defgroup google-weather nil
  "Google Weather."
  :group 'comm)

(defconst google-weather-url
  "http://www.google.com/ig/api"
  "URL of the Google Weather API.")

(defconst google-weather-image-url
  "http://www.google.com"
  "URL prefix for images.")

(defcustom google-weather-unit-system-temperature-assoc
  '(("SI" . "℃")
    ("US" . "℉"))
  "Find temperature symbol from unit system."
  :group 'google-weather)

(defun google-weather-cache-expired (url expire-time)
  "Check if URL is cached for more than EXPIRE-TIME."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   (url-is-cached url)
                   (seconds-to-time expire-time))
                  (current-time))
               t)))))

(defun google-weather-cache-fetch (url)
  "Fetch URL from the cache."
  (with-current-buffer (generate-new-buffer " *temp*")
    (url-cache-extract (url-cache-create-filename url))
    (current-buffer)))

(defun google-weather-retrieve-data (url &optional expire-time)
  "Retrieve URL and return its data as string.
If EXPIRE-TIME is set, the data will be fetched from the cache if
their are not older than EXPIRE-TIME seconds. Otherwise, they
will be fetched and then cached. Therefore, setting EXPIRE-TIME
to 0 force a cache renewal."
  (let* ((expired (if expire-time
                      (google-weather-cache-expired url expire-time)
                    t))
         (buffer (if expired
                     (url-retrieve-synchronously url)
                   (google-weather-cache-fetch url)))
         data)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (decode-coding-region
       (point) (point-max)
       (detect-coding-region (point) (point-max) t))
      (set-buffer-multibyte t)
      (setq data (xml-parse-region (point) (point-max)))
      (when (and expired expire-time)
        (url-store-in-cache (current-buffer)))
      (kill-buffer (current-buffer))
      data)))

(defun google-weather-build-url (location &optional language)
  "Build URL to retrieve weather for LOCATION in LANGUAGE."
  (concat google-weather-url "?weather=" (url-hexify-string location)
          (when language
            (concat "&hl=" language))))

(defun google-weather-get-data (location &optional language expire-time)
  "Get weather data for LOCATION in LANGUAGE.
See `google-weather-retrieve-data' for the use of EXPIRE-TIME."
  (google-weather-retrieve-data
   (google-weather-build-url location language) expire-time))

(defun google-weather-data->weather (data)
  "Return all weather information from DATA."
  (cddr (assoc 'weather (cdr (assoc 'xml_api_reply data)))))

(defun google-weather-data->forecast-information (data)
  "Return the forecast information of DATA."
  (cddr (assoc 'forecast_information (google-weather-data->weather data))))

(defun google-weather-assoc (key data)
  "Extract value of field KEY from DATA."
  (cdr (assoc 'data (cadr (assoc key data)))))

(defun google-weather-data->city (data)
  "Return the city where the DATA come from."
  (google-weather-assoc
   'city
   (google-weather-data->forecast-information data)))

(defun google-weather-data->postal-code (data)
  "Return the postal code where the DATA come from."
  (google-weather-assoc
   'postal_code
   (google-weather-data->forecast-information data)))

(defun google-weather-data->unit-system (data)
  "Return the unit system used for DATA."
  (google-weather-assoc
   'unit_system
   (google-weather-data->forecast-information data)))

(defun google-weather-data->forecast-date (data)
  "Return the unit system used for DATA."
  (google-weather-assoc
   'forecast_date
   (google-weather-data->forecast-information data)))

(defun google-weather-data->forecast (data)
  "Get forecast list from DATA."
  ;; Compute date of the forecast in the same format as `current-time'
  (let ((date (apply 'encode-time
                     (parse-time-string
                      (concat (google-weather-data->forecast-date data) " 00:00:00")))))
    (mapcar
     (lambda (forecast)
       (let* ((forecast-date (decode-time date))
              (forecast-encoded-date (list (nth 4 forecast-date)
                                           (nth 3 forecast-date)
                                           (nth 5 forecast-date))))
         ;; Add one day to `date'
         (setq date (time-add date (days-to-time 1)))
         `(,forecast-encoded-date
           (low ,(google-weather-assoc 'low forecast))
           (high ,(google-weather-assoc 'high forecast))
           (icon ,(concat google-weather-image-url
                          (google-weather-assoc 'icon forecast)))
           (condition ,(google-weather-assoc 'condition forecast)))))
     (loop for entry in (google-weather-data->weather data)
           when (eq (car entry) 'forecast_conditions)
           collect entry))))

(defun google-weather-data->forecast-for-date (data date)
  "Return forecast for DATE from DATA.
DATE should be in the same format used by calendar,
i.e. (MONTH DAY YEAR)."
  (cdr (assoc date (google-weather-data->forecast data))))

(defun google-weather-data->temperature-symbol (data)
  "Return the temperature to be used according in DATA.
It uses `google-weather-unit-system-temperature-assoc' to find a
match."
  (cdr (assoc (google-weather-data->unit-system data) google-weather-unit-system-temperature-assoc)))

(provide 'google-weather)
