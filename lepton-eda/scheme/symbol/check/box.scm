;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2022 Lepton EDA Contributors
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

(define-module (symbol check box)
  #:use-module (ice-9 match)

  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)

  #:export (check-box-size
            check-box))

(define (check-box-size object)
  "Checks box OBJECT size."
  (define (blame-zero-box object)
    (blame-object object
                     'error
                     (format #f
                             (G_ "Zero sized box at ~A")
                             (box-top-left object))))

  (match `(,(box-top-left object) . ,(box-bottom-right object))
    (((x . y0) . (x . y1)) (blame-zero-box object))
    (((x0 . y) . (x1 . y)) (blame-zero-box object))
    (_ #f)))


(define (check-box object)
  "Checks box OBJECT:
  * Checks that it has non-zero size."
  (check-box-size object))
