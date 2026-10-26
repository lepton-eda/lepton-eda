;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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


(define-module (schematic stroke)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton gettext)
  #:use-module (lepton log)

  #:use-module (schematic ffi)

  #:export (init-libstroke
            record-stroke
            translate-stroke
            stroke-size))


(define libstroke (false-if-exception (dynamic-link "libstroke")))
(define stroke_init
  (and libstroke
       (pointer->procedure void (dynamic-func "stroke_init" libstroke) '())))
(define stroke_trans
  (and libstroke
       (pointer->procedure int (dynamic-func "stroke_trans" libstroke) '(*))))
(define stroke_record
  (and libstroke
       (pointer->procedure void (dynamic-func "stroke_record" libstroke) (list int int))))

;;; Largest number of points allowed to be sampled.
(define STROKE_MAX_POINTS 10000)


(define %points '())


(define (init-libstroke)
  (if libstroke
      (stroke_init)
      (log! 'message (G_ "No libstroke found."))))


;;; Records a new point with the coords (X . Y) for the stroke.
(define (record-stroke x y)
  (stroke_record x y)
  (when (< (length %points) STROKE_MAX_POINTS)
    (set! %points (cons (cons x y) %points))))


(define (stroke-size)
  (and (> (length %points) 0)
       (let ((x (map car %points))
             (y (map cdr %points)))
         (cons (cons (apply min x) (apply min y))
               (cons (apply max x) (apply max y))))))


;;; Translates the stroke input into a string and returns a string
;;; consisting of numbers if the stroke has been successfully
;;; translated, or #f if libstroke failed to transform the stroke.
;;; The stroke sequence is to be evaluated in Scheme code into a
;;; possible action by the Guile procedure eval-stroke().
(define (translate-stroke)
  (define *bv (bytevector->pointer (make-bytevector (sizeof '*) 0) 0))

  ;; Return translated stroke.
  (let ((result (stroke_trans *bv)))
    (set! %points '())
    (and (true? result)
         (pointer->string *bv))))
