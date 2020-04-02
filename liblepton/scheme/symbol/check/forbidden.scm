;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2020 Lepton EDA Contributors
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
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA

(define-module (symbol check forbidden)
  #:use-module (lepton core gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)

  #:export (check-forbidden))

;;; Checks if current object is forbidden in symbols.
(define (forbidden? object)
  (or (bus? object)
      (net? object)
      (component? object)))

(define (check-forbidden object)
  "Checks if OBJECT is forbidden in symbols."
  (when (forbidden? object)
    (blame-object object
                  'error
                  (format #f
                          (_ "Object forbidden inside symbols: ~A")
                          (object-type object)))))
