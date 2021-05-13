;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2021 Lepton EDA Contributors
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
  #:use-module (system foreign)

  #:use-module (lepton ffi)

  #:export (geda-object->pointer
            pointer->geda-object))


;;; Helper transformers between #<geda-object> smobs and C object
;;; pointers.
(define (geda-object->pointer smob)
  (or (false-if-exception (edascm_to_object (scm->pointer smob)))
      ;; Return NULL if the SMOB is not the #<geda-object> smob.
      %null-pointer))

(define (pointer->geda-object pointer)
  ;; Return #f if the pointer is wrong.
  (false-if-exception (pointer->scm (edascm_from_object pointer))))
