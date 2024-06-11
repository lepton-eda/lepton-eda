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


(define-module (schematic event)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (schematic ffi gtk)

  #:export (event-coords
            event-state))


(define (event-state *event)
  (define state-bv (make-bytevector (sizeof GdkModifierType) 0))

  (gdk_event_get_state *event (bytevector->pointer state-bv))
  (bytevector-u32-native-ref state-bv 0))


(define (event-coords *event)
  (define window-x-bv (make-bytevector (sizeof double) 0))
  (define window-y-bv (make-bytevector (sizeof double) 0))

  (gdk_event_get_coords *event
                        (bytevector->pointer window-x-bv)
                        (bytevector->pointer window-y-bv))

  (let ((window-x (bytevector-ieee-double-native-ref window-x-bv 0))
        (window-y (bytevector-ieee-double-native-ref window-y-bv 0)))
    (cons window-x window-y)))
