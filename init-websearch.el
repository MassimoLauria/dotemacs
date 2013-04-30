;;; init-websearch.el --- Search Engines

;; Copyright (C) 2010, 2011, 2012, 2013  Massimo Lauria

;; Author: Massimo Lauria <lauria.massimo@gmail.com>
;; Time-stamp: <2013-04-30, 13:45 (CEST) Massimo Lauria>

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

;; This is the part of my init files devoted Google services.  Right
;; now it is just google search on region, but it may support Google
;; Contacts, Google Maps, and Google Translation soon.

;;; Code:
(require 'url-util)


(defmacro def-search-engine (name docstring func query)
  "Define a search engine lookup function.

NAME is a human readable search engine name.  DOCSTRING is the
documentation string for the search engine.  FUNC is the name of
the lookup function, QUERY is the url of the query string: the
search string is appended to the query string."
  `(defun ,func ()
     ,docstring
     (interactive)
     (browse-url
      (concat
       ,query
       (url-hexify-string (if mark-active
                              (buffer-substring (region-beginning) (region-end))
                            (read-string (concat ,name ": "))))))))

(def-search-engine "Google"
  "Search with Google search engine."
  google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=")
(def-search-engine "Google Scholar"
  "Search with Google Scholar, and academic search engine."
  scholar
  "http://scholar.google.com/scholar?hl=it&q=")
(def-search-engine "Wikipedia"
  "Search in the free enciclopedia Wikipedia."
  wiki
  "http://en.wikipedia.org/w/index.php?search=")
(def-search-engine "Wikipedia Italia"
  "Search in the Italian version of free enciclopedia Wikipedia."
  iwiki
  "http://it.wikipedia.org/w/index.php?search=")
(def-search-engine "Emacs Wiki"
  "Search in Emacs Wiki"
  ew
  "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&sa=Search&siteurl=emacswiki.org%2F&q=")
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



;; Math search engines
(def-search-engine "Wolfram Alpha"
  "Search on Wolfram Alpha"
  al
  "http://www.wolframalpha.com/input/?i=")
(def-search-engine "OEIS" 
  "Search in OnLine Encyclopedia of Integer Sequences"
  oeis
  "http://oeis.org/search?q=")

;; Dictionary look-up do not follow the general scheme. It must focus
;; on a single word.
(defun dict (&optional word)
     "Look-up the WORD under cursor in an internet dictionary."
     (interactive)
     (browse-url
      (concat
       "http://www.thefreedictionary.com/_/search.aspx?pid=osearch&word="
       (url-hexify-string (if word
                              word
                            (current-word))))))


(provide 'init-websearch)
;;; init-websearch.el ends here
