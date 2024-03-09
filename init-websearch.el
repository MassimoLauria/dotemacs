;;; init-websearch.el --- Search Engines

;; Copyright (C) 2010, 2011, 2012, 2013, 2014, 2019, 2024  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2024-03-09, 19:05 (CET) Massimo Lauria>

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

;; This is the part of my init files devoted queries to web services.
;; It allows to query search engines and dictionaries for words and
;; text in the buffer.

;;; Code:
(require 'url-util)

(defmacro def-search-engine (name docstring func query)
  "Define a search engine lookup function.

NAME is a human readable search engine name.  DOCSTRING is the
documentation string for the search engine.  FUNC is the name of
the lookup function, QUERY is the url of the query string: the
search string is appended to the query string."
  `(defun ,func (&optional searchstring)
     ,docstring
     (interactive)
     (browse-url
      (concat
       ,query
       (url-hexify-string (or searchstring
                              (if mark-active
                                  (buffer-substring (region-beginning)
                                                    (region-end)))
                              (read-string (concat ,name ": ")
                                           (current-word t nil))))))))

;;
;; Google engines
;;
(def-search-engine "Google"
  "Search with Google"
  google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=")
(def-search-engine "Google Scholar"
  "Search with Google Scholar"
  scholar
  "http://scholar.google.com/scholar?hl=it&q=")
(def-search-engine "Translate to English"
  "Translate to English using Google Translate"
  english
  "http://translate.google.com/#auto/en/")
(def-search-engine "Translate to Italian"
  "Translate to Italian using Google Translate"
  italian
  "http://translate.google.com/#auto/it/")
(def-search-engine "Youtube"
  "Search on YouTube"
  yt
  "http://www.youtube.com/results?search_query=")
(def-search-engine "Google Maps"
  "Find on map"
  maps
  "https://maps.google.it/maps?q=")


;;
;; Wikipedia
;;
(def-search-engine "Wikipedia"
  "Search in the free enciclopedia Wikipedia."
  wiki
  "http://en.wikipedia.org/w/index.php?search=")
(def-search-engine "Wikipedia Italia"
  "Search in the Italian version of free enciclopedia Wikipedia."
  iwiki
  "http://it.wikipedia.org/w/index.php?search=")



;;
;; Emacs
;;
(def-search-engine "Emacs Wiki"
  "Search in Emacs Wiki"
  ew
  "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&sa=Search&siteurl=emacswiki.org%2F&q=")


;;
;; Math search engines
;;
(def-search-engine "Wolfram Alpha"
  "Search on Wolfram Alpha"
  al
  "http://www.wolframalpha.com/input/?i=")
(def-search-engine "OEIS"
  "Search in OnLine Encyclopedia of Integer Sequences"
  oeis
  "http://oeis.org/search?q=")


;;
;; Dictionary and thesaurus lookup
;;
(def-search-engine "The Free Dictionary"
  "Look up for a word in The Free Dictionary."
  freedict
  "http://www.thefreedictionary.com/_/search.aspx?pid=osearch&word=")

(def-search-engine "Oxford Dictionary"
  "Look up for a word in Oxford Dictionary Online"
  dict
  "https://en.oxforddictionaries.com/search?filter=dictionary&query=")

(def-search-engine "Oxford Dictionary (Thesaurus)"
  "Look up for a word in Thesaurus.com."
  thes
  "https://en.oxforddictionaries.com/search?filter=thesaurus&query=")


;; Programming languages references
(def-search-engine "Search in C++ Reference"
  "Search in cppreference.com."
  cpphelp
  "http://en.cppreference.com/mwiki/index.php?search=")

(def-search-engine "Search in Python Doc"
  "Search in Python documentation."
  pyhelp
"https://docs.python.org/3/search.html?q=")


(provide 'init-websearch)
;;; init-websearch.el ends here
