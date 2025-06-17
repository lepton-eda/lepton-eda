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

(define-module (symbol check alignment)
  #:use-module (lepton gettext)
  #:use-module (lepton object)
  #:use-module (symbol blame)

  #:export (check-pin-alignment))

(define %grid-step 100)

(define (check-pin-alignment pin)
  "Checks if both PIN ends of given object are aligned by grid.
Returns PIN."
  (define x car)
  (define y cdr)
  (define (on-grid? num)
    (= 0 (euclidean-remainder num %grid-step)))

  (define (is-on-grid? coord)
    (and (on-grid? (x coord))
         (on-grid? (y coord))))

  (define (check-on-grid pin msg-type msg coord)
    ;; Consider line-start to be the first pin point
    (let ((num (if (eq? msg-type 'error) 1 2)))
      (or (is-on-grid? coord)
          (blame-object pin
                        msg-type
                        (format #f
                                msg
                                num
                                (x coord)
                                num
                                (y coord))))))

  ;; line-start is the connectible point (whichend)
  (check-on-grid pin
                 'error
                 (G_ "Connectible end of pin is off grid (x~A=~A,y~A=~A)")
                 (line-start pin))
  (check-on-grid pin
                 'warning
                 (G_ "Non-connectible end of pin is off grid (x~A=~A,y~A=~A)")
                 (line-end pin)))
