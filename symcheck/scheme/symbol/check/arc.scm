;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017-2019 Lepton EDA Contributors
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

(define-module (symbol check arc)
  #:use-module (geda object)
  #:use-module (symbol gettext)
  #:use-module (symbol blame)

  #:export (check-arc-radius
            check-arc-angle
            check-arc))

(define (check-arc-radius object)
"Checks that arc OBJECT has non-zero radius."
  (and (= 0 (arc-radius object))
       (blame-object object
                     'error
                     (format #f
                             (_ "Zero radius arc at ~A")
                             (arc-center object)))))

(define (check-arc-angle object)
  "Checks that arc OBJECT has non-zero angle."
  (and (= 0 (euclidean-remainder
             (- (arc-sweep-angle object)
                (arc-start-angle object))
             360))
       (blame-object object
                     'error
                     (format #f
                             (_ "Zero angle arc at ~A")
                             (arc-center object)))))

(define (check-arc object)
  "Checks arc OBJECT:
  * Checks that it has non-zero radius.
  * Checks that it has non-zero angle."
  (check-arc-radius object)
  (check-arc-angle object))
