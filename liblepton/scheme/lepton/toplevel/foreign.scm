;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2022 Lepton EDA Contributors
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


(define-module (lepton toplevel foreign)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (is-toplevel?
            check-toplevel
            toplevel->pointer
            pointer->toplevel))


(define-wrapped-pointer-type <toplevel>
  is-toplevel?
  wrap-toplevel
  unwrap-toplevel
  (lambda (toplevel port)
    (format port "#<toplevel-0x~x>"
            (pointer-address (unwrap-toplevel toplevel)))))


;;; Helper transformers between the <toplevel> type and C toplevel
;;; pointers.
(define (toplevel->pointer toplevel)
  "Transforms TOPLEVEL which should be an instance of the
<toplevel> type into a foreign C pointer.  If TOPLEVEL has another
type, raises a 'wrong-type-arg error."
  (if (is-toplevel? toplevel)
      (unwrap-toplevel toplevel)
      (error-wrong-type-arg 1 '<toplevel> toplevel)))


(define (pointer->toplevel pointer)
  "Transforms POINTER to a <toplevel> type instance. Raises a
'wrong-type-arg error if POINTER is not a foreign C pointer.
Raises a 'misc-error error if the pointer is a NULL pointer."
  (if (pointer? pointer)
      (if (null-pointer? pointer)
          (error "Cannot convert NULL pointer to <toplevel>.")
          (wrap-toplevel pointer))
      (error-wrong-type-arg 1 'pointer pointer)))


;;; Syntax rules to check <toplevel> instances.  The same as for
;;; <object> in the module (lepton object foreign).
(define-syntax check-toplevel
  (syntax-rules ()
    ((_ toplevel pos)
     (let ((pointer (and (is-toplevel? toplevel)
                         (unwrap-toplevel toplevel))))
       (if (or (not pointer)
               (null-pointer? pointer))
           (error-wrong-type-arg pos '<toplevel> toplevel)
           pointer)))))
