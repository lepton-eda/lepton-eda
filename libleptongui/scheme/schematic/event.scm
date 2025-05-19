;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2025 Lepton EDA Contributors
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

;;; Process GTK events.

(define-module (schematic event)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton m4)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (event-coords
            event-direction
            event-state
            event-time
            event-scroll-direction->symbol
            symbol->event-scroll-direction))


(define (event-state *event)
  "Get the 'state' field of C *EVENT containing the data of the type
GdkModifierType."
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


(define (event-scroll-direction->symbol direction)
  "Returns a Scheme symbol corresponding to integer DIRECTION value."
  (string->symbol
   (pointer->string
    (gdk_event_scroll_direction_to_string direction))))


(define (symbol->event-scroll-direction sym)
  "Returns an integer scroll direction value corresponding to the
symbol SYM."
  (gdk_event_scroll_direction_from_string (string->pointer
                                           (symbol->string sym))))


;;; The getter gdk_event_get_scroll_direction() is defined in GTK3
;;; only.
(define (event-direction-gtk3 *event)
  (define direction (make-bytevector (sizeof int) 0))

  (define has-direction?
    (true? (gdk_event_get_scroll_direction
            *event
            (bytevector->pointer direction))))

  (define result
    (bytevector-uint-ref direction
                         0
                         (native-endianness)
                         (sizeof int)))

  (if has-direction?
      result
      (let ((delta-x (make-bytevector (sizeof double) 0))
            (delta-y (make-bytevector (sizeof double) 0)))
        (and (true? (gdk_event_get_scroll_deltas *event
                                                 (bytevector->pointer delta-x)
                                                 (bytevector->pointer delta-y)))
             (cons (bytevector-ieee-double-native-ref delta-x 0)
                   (bytevector-ieee-double-native-ref delta-y 0))))))

(define (event-direction *event)
  ((if %m4-use-gtk3
       event-direction-gtk3
       schematic_event_get_scroll_direction) *event))

(define (event-time *event)
  (gdk_event_get_time *event))
