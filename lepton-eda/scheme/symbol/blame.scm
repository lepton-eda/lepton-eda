;;; Lepton EDA Symbol Checker
;;; Scheme API
;;; Copyright (C) 2017 Lepton EDA Contributors
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

(define-module (symbol blame)
  #:export (object-blames
            blame-object
            acquit-object))

;;; Object property for storing blaming info.
(define object-blames (make-object-property))

(define (blame-object object severity message)
  "Adds blaming MESSAGE with given SEVERITY for schematic
OBJECT. SEVERITY may be one of 'info, 'warning, or 'error."
  (let ((blames (object-blames object)))
    (set! (object-blames object)
          `((,severity . ,message) . ,(or blames '())))))

(define (acquit-object object)
  "Remove any blaming info for schematic OBJECT."
  (set! (object-blames object) '()))
