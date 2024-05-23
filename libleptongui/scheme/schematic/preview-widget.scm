;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2022-2024 Lepton EDA Contributors
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


(define-module (schematic preview-widget)
  #:use-module (system foreign)

  #:use-module (lepton m4)

  #:use-module (schematic ffi)

  #:export (init-preview-widget-signals))


(define (update-preview *preview *user-data)
  (schematic_preview_update *preview %null-pointer))


(define *update-preview
  (procedure->pointer void update-preview '(* *)))


;;; The list of pairs (NAME . CALLBACK) for initialization of
;;; preview widgets.
(define %signal-callback-list
  (list
   (if %m4-use-gtk3
       `("draw" . ,*x_event_draw)
       `("expose-event" . ,*x_event_expose))
   `("realize" . ,*schematic_preview_callback_realize)
   `("button-press-event" . ,*schematic_preview_callback_button_press)
   `("configure-event" . ,*x_event_configure)
   `("scroll-event" . ,*schematic_preview_callback_scroll_event)
   `("update-preview" . ,*update-preview)))


(define (init-preview-widget-signals *preview)
  "Initialize the preview widget *PREVIEW by connecting to it
appropriate signals.  Return *PREVIEW."
  (for-each
   (lambda (element)
     (schematic_signal_connect *preview
                               (string->pointer (car element))
                               (cdr element)
                               (schematic_preview_get_window *preview)))
   %signal-callback-list)
  *preview)
