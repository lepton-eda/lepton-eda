;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
;;; Copyright (C) 2017-2024 Lepton EDA Contributors
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

(define-module (schematic mouse-pointer)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton log)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (mouse-pointer-position
            pointer-position))


(define (mouse-pointer-position)
  "Returns the current mouse pointer position, expressed in world
coordinates in the form (X . Y).  If the mouse pointer is outside
the schematic drawing area, returns #f."
  (define *window
    (or (and=> (current-window) window->pointer)
        (error "~S: Current window is unavailable." 'mouse-pointer-position)))

  (define x (make-bytevector (sizeof int)))
  (define y (make-bytevector (sizeof int)))

  (let ((result (true? (x_event_get_pointer_position
                        *window
                        FALSE
                        (bytevector->pointer x)
                        (bytevector->pointer y)))))
    (and result
         (cons (bytevector-sint-ref x 0 (native-endianness) (sizeof int))
               (bytevector-sint-ref y 0 (native-endianness) (sizeof int))))))


(define (pointer-position)
  "The function is deprecated and is left for backward compatibility
only.  Use the mouse-pointer() function instead."
  (log! 'warning "The function name \"pointer-position()\" is deprecated.")
  (log! 'warning "Use \"mouse-pointer-position()\" instead.")
  (mouse-pointer-position))
