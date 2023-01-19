;;; Lepton EDA library
;;; Scheme API
;;; Copyright (C) 2025 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.


(define-module (lepton autonumber)
  #:use-module (lepton ffi)

  #:export (autonumber-string->template))


;;; If BASE is a prefix of STR, the suffix of STR consisting of
;;; digits and question marks is dropped to make an additional
;;; template for renumbering.  If the resulting template length
;;; is less than the length of the base, return the base string,
;;; otherwise return the template.  If BASE is not a prefix of
;;; STR, return #f.
(define (autonumber-string->template str base)
  (define cs (string->char-set "0123456789?"))

  (and (string-prefix? base str)
       (let ((trimmed-str (string-trim-right str cs)))
         (if (> (string-length trimmed-str)
                (string-length base))
             trimmed-str
             base))))
