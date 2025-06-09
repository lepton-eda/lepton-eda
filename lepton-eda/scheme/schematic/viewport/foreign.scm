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


(define-module (schematic viewport foreign)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi check-args)
  #:use-module (lepton ffi)

  #:export (is-viewport?
            check-viewport
            viewport->pointer
            pointer->viewport))


;;; Define a wrapped pointer type.
(define-wrapped-pointer-type <viewport>
  is-viewport?
  wrap-viewport
  unwrap-viewport
  ;; Printer.
  (lambda (viewport port)
    (format port "#<viewport-0x~x>"
            (pointer-address (unwrap-viewport viewport)))))


;;; Helper transformers between the <viewport> type and C viewport
;;; pointers.
(define (viewport->pointer viewport)
  "Transforms VIEWPORT which should be an instance of the <viewport>
type into a foreign C pointer.  If VIEWPORT has another type, raises
a 'wrong-type-arg error."
  (if (is-viewport? viewport)
      (unwrap-viewport viewport)
      (error-wrong-type-arg 1 '<viewport> viewport)))


(define (pointer->viewport pointer)
  "Transforms POINTER to a <viewport> type instance. Raises a
'wrong-type-arg error if POINTER is not a foreign C pointer.
Raises a 'misc-error error if the pointer is a NULL pointer."
  (if (pointer? pointer)
      (if (null-pointer? pointer)
          (error "Cannot convert NULL pointer to <viewport>.")
          (wrap-viewport pointer))
      (error-wrong-type-arg 1 'pointer pointer)))


;;; Syntax rules to check <viewport> instances.  The same as for
;;; <object> in the module (lepton object foreign).
(define-syntax check-viewport
  (syntax-rules ()
    ((_ viewport pos)
     (let ((pointer (and (is-viewport? viewport)
                         (unwrap-viewport viewport))))
       (if (or (not pointer)
               (null-pointer? pointer))
           (error-wrong-type-arg pos '<viewport> viewport)
           pointer)))))
