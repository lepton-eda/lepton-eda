;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2024 Lepton EDA Contributors
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


(define-module (lepton bounds)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi)
  #:use-module (lepton ffi boolean)

  #:export (object-list-bounds))


(define (object-list-bounds *objects with-hidden?)
  (define (get-int bv)
    (bytevector-sint-ref bv 0 (native-endianness) (sizeof int)))
  (define x1 (make-bytevector (sizeof int)))
  (define y1 (make-bytevector (sizeof int)))
  (define x2 (make-bytevector (sizeof int)))
  (define y2 (make-bytevector (sizeof int)))
  (define result
    (true? (world_get_object_glist_bounds *objects
                                          with-hidden?
                                          (bytevector->pointer x1)
                                          (bytevector->pointer y1)
                                          (bytevector->pointer x2)
                                          (bytevector->pointer y2))))
  (if result
      (values (get-int x1) (get-int y1) (get-int x2) (get-int y2))
      (values #f #f #f #f)))
