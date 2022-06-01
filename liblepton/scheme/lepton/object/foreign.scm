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
      (scm-error 'wrong-type-arg
                 'object->pointer
                 "Wrong type argument in position 1 (expecting <object>): ~A"
                 (list object)
                 #f)))

(define (pointer->object pointer)
  (and (pointer? pointer)
       (wrap-object pointer)))


;;; This syntax rule is intended for use in toplevel 'define' or
;;; 'let' forms in the functions where the check for wrong type of
;;; OBJECT is necessary.  The rule checks the object and, if it is
;;; not #<geda-object>, throws an error with the 'wrong-type-arg
;;; key reporting the function name and position POS of the
;;; OBJECT argument.  In short, the usage is as follows:
;;;   (define (myfunc object)
;;;     (define pointer (check-object object 1))
;;;     (function-body))
(define-syntax check-object
  (syntax-rules ()
    ((_ object pos)
     (let ((proc-name (frame-procedure-name (stack-ref (make-stack #t) 1)))
           (pointer (and (is-object? object)
                         (unwrap-object object))))
       (if (null-pointer? pointer)
           (scm-error 'wrong-type-arg
                      proc-name
                      "Wrong type argument in position ~A: ~A"
                      (list pos object)
                      #f)
           pointer)))
    ((_ object pos object-check-func type)
     (let ((proc-name (frame-procedure-name (stack-ref (make-stack #t) 1)))
           (pointer (and (is-object? object)
                         (unwrap-object object))))
       (if (null-pointer? pointer)
           (scm-error 'wrong-type-arg
                      proc-name
                      "Wrong type argument in position ~A: ~A"
                      (list pos object)
                      #f)
           (if (object-check-func object)
               pointer
               (scm-error 'wrong-type-arg
                          proc-name
                          "Wrong type argument in position ~A (expecting ~A object): ~A"
                          (list pos type object)
                          #f)))))))
