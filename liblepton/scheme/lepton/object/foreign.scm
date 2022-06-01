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
            pointer->object))

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
  (or (false-if-exception (unwrap-object object))
      ;; Return NULL if the OBJECT is not <object>.
      %null-pointer))

(define (pointer->object pointer)
  (and (pointer? pointer)
       (wrap-object pointer)))
