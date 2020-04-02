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

(define-module (symbol check circle)
  #:use-module (lepton core gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)

  #:export (check-circle-radius
            check-circle))

(define (check-circle-radius object)
  "Checks circle OBJECT's radius."
  (when (= 0 (circle-radius object))
    (blame-object object
                  'error
                  (format #f
                          (_ "Zero radius circle at ~A")
                          (circle-center object)))))

(define (check-circle object)
  "Checks circle OBJECT:
  * Checks that it has non-zero radius."
  (check-circle-radius object))
