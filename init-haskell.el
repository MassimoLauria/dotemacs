;;; init-haskell.el --- Setup for Haskell programming

;; Copyright (C) 2013 Massimo Lauria <lauria.massimo@gmail.com>

;; Created : "2013-12-10, Tuesday 01:41 (CET) Massimo Lauria"
;; Time-stamp: "2013-12-30, 16:37 (CET) Massimo Lauria"


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

;; Setup the environment and the auto-completion.


;;; Code:
;;

 
(add-to-list 'exec-path "~/.cabal/bin")    ; Cabal install binaries here




(provide 'init-haskell)
