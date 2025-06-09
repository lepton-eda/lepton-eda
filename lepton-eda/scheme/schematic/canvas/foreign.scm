;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2024 Lepton EDA Contributors
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


(define-module (schematic canvas foreign)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi)

  #:export (is-canvas?
            check-canvas
            canvas->pointer
            pointer->canvas))


;;; Define a wrapped pointer type.
(define-wrapped-pointer-type <canvas>
  is-canvas?
  wrap-canvas
  unwrap-canvas
  ;; Printer.
  (lambda (canvas port)
    (format port "#<canvas-0x~x>"
            (pointer-address (unwrap-canvas canvas)))))


;;; Helper transformers between the <canvas> type and C canvas
;;; pointers.
(define (canvas->pointer canvas)
  "Transforms CANVAS which should be an instance of the <canvas>
type into a foreign C pointer.  If CANVAS has another type, raises
a 'wrong-type-arg error."
  (if (is-canvas? canvas)
      (unwrap-canvas canvas)
      (error-wrong-type-arg 1 '<canvas> canvas)))


(define (pointer->canvas pointer)
  "Transforms POINTER to a <canvas> type instance. Raises a
'wrong-type-arg error if POINTER is not a foreign C pointer.
Raises a 'misc-error error if the pointer is a NULL pointer."
  (if (pointer? pointer)
      (if (null-pointer? pointer)
          (error "Cannot convert NULL pointer to <canvas>.")
          (wrap-canvas pointer))
      (error-wrong-type-arg 1 'pointer pointer)))


;;; Syntax rules to check <canvas> instances.  The same as for
;;; <object> in the module (lepton object foreign).
(define-syntax check-canvas
  (syntax-rules ()
    ((_ canvas pos)
     (let ((pointer (and (is-canvas? canvas)
                         (unwrap-canvas canvas))))
       (if (or (not pointer)
               (null-pointer? pointer))
           (error-wrong-type-arg pos '<canvas> canvas)
           pointer)))))
