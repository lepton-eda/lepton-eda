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


(define-module (lepton page foreign)
  #:use-module (ice-9 format)
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (is-page?
            check-page
            page->pointer
            pointer->page))


(define-wrapped-pointer-type <page>
  is-page?
  wrap-page
  unwrap-page
  (lambda (page port)
    (format port "#<page-0x~x>"
            (pointer-address (unwrap-page page)))))


;;; Helper transformers between the <page> type and C page
;;; pointers.
(define (page->pointer page)
  "Transforms PAGE which should be an instance of the <page> type
into a foreign C pointer.  If PAGE has another type, raises a
'wrong-type-arg error."
  (if (is-page? page)
      (unwrap-page page)
      (error-wrong-type-arg 1 '<page> page)))


(define (pointer->page pointer)
  "Transforms POINTER to a <page> type instance. Raises a
'wrong-type-arg error if POINTER is not a foreign C pointer.
Raises a 'misc-error error if the pointer is a NULL pointer."
  (if (pointer? pointer)
      (if (null-pointer? pointer)
          (error "Cannot convert NULL pointer to <page>.")
          (wrap-page pointer))
      (error-wrong-type-arg 1 'pointer pointer)))


;;; Syntax rules to check <page> instances.  The same as for
;;; <object> in the module (lepton object foreign).
(define-syntax check-page
  (syntax-rules ()
    ((_ page pos)
     (let ((pointer (and (is-page? page)
                         (unwrap-page page))))
       (if (or (not pointer)
               (null-pointer? pointer))
           (error-wrong-type-arg pos '<page> page)
           pointer)))))
