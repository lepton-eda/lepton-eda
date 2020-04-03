;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017 Lepton EDA Contributors
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

(define-module (symbol check duplicate)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:export (list->duplicate-list))

(define (list->duplicate-list ls f-less? f-equal?)
  "Sorts list LS using function F-LESS? for comparison and
transforms it into a list where duplicated members which values
are equal if compared using function F-EQUAL? are gathered
together into sublists."
  (fold-right
   (lambda (elem ret)
     (match ret
       (((x . xrest) . rest)
        (if (f-equal? elem x)
            `((,elem . (,x . ,xrest)) . ,rest)
            `(,elem . ,ret)))
       ((x . rest)
        (if (f-equal? elem x)
            `((,elem ,x) . ,rest)
            `(,elem . ,ret)))
       (_ `(,elem . ,ret))))
   '()
   (sort ls f-less?)))
