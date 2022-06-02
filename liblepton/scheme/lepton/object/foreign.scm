;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2021-2022 Lepton EDA Contributors
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


(define-module (lepton object foreign)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (is-object?
            object->pointer
            pointer->object
            check-object))

(define-wrapped-pointer-type <object>
  is-object?
  wrap-object
  unwrap-object
  (lambda (object port)
    (format port "#<object-0x~x>"
            (pointer-address (unwrap-object object)))))


;;; Helper transformers between the <object> type and C object
;;; pointers.
(define (object->pointer object)
  "Transforms OBJECT which should be an instance of the <object>
type into a foreign C pointer.  If OBJECT has another type, raises
a 'wrong-type-arg error."
  (if (is-object? object)
      (unwrap-object object)
      (error-wrong-type-arg 1 '<object> object)))


(define (pointer->object pointer)
  "Transforms POINTER to an <object> type instance. Raises a
'wrong-type-arg error if POINTER is not a foreign C pointer.
Raises a 'misc-error error if the pointer is a NULL pointer."
  (if (pointer? pointer)
      (if (null-pointer? pointer)
          (error "Cannot convert NULL pointer to <object>.")
          (wrap-object pointer))
      (error-wrong-type-arg 1 'pointer pointer)))


;;; This syntax is reused in the below check-object syntax.
;;; Please see comments for the latter syntax.
(define-syntax check-object*
  (syntax-rules ()
    ((_ object pos)
     (let ((pointer (and (is-object? object)
                         (unwrap-object object))))
       (if (or (not pointer)
               (null-pointer? pointer))
           (error-wrong-type-arg pos '<object> object)
           pointer)))))

;;; This syntax rule is intended for use in top level 'define' or
;;; 'let' forms in the functions where the check for wrong type of
;;; OBJECT is necessary.  The rule checks if the object instance
;;; is of the type <object> and, if it is not, throws an error
;;; with the 'wrong-type-arg key reporting the function name and
;;; position POS of the OBJECT argument.  In short, the usage is
;;; as follows:
;;;   (define (myfunc object)
;;;     (define pointer (check-object object ...))
;;;     (function-body))
(define-syntax check-object
  (syntax-rules ()
    ((_ object pos)
     (check-object* object pos))
    ((_ object pos object-check-func type)
     (if (object-check-func object)
         (check-object* object pos)
         (error-wrong-type-arg pos type object)))))
