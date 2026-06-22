;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2026 Lepton EDA Contributors
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

(define-module (schematic gtk helper)
  #:use-module (system foreign)

  #:use-module (schematic ffi)

  #:export (gtk-buttons-type->symbol
            symbol->gtk-buttons-type
            gtk-file-chooser-action->symbol
            symbol->gtk-file-chooser-action
            gtk-justification->symbol
            symbol->gtk-justification
            gtk-message-type->symbol
            symbol->gtk-message-type
            gtk-position-type->symbol
            symbol->gtk-position-type
            gtk-response->symbol
            symbol->gtk-response))

(define (gtk-buttons-type->symbol type)
  "Transforms GtkButtonsType value TYPE to Scheme symbol."
  (string->symbol (pointer->string (gtk_buttons_type_to_string type))))

(define (symbol->gtk-buttons-type sym)
  "Transforms symbol SYM to corresponding GtkButtonsType value."
  (gtk_string_to_buttons_type (string->pointer (symbol->string sym))))


(define (gtk-file-chooser-action->symbol action)
  "Transforms GtkFileChooserAction value ACTION to Scheme symbol."
  (string->symbol
   (pointer->string (gtk_file_chooser_action_to_string action))))

(define (symbol->gtk-file-chooser-action sym)
  "Transforms symbol SYM to corresponding GtkFileChooserAction value."
  (gtk_string_to_file_chooser_action
   (string->pointer (symbol->string sym))))


(define (gtk-justification->symbol justify)
  "Transforms GtkJustification value JUSTIFY to Scheme symbol."
  (string->symbol
   (pointer->string (schematic_gtk_justification_to_string justify))))

(define (symbol->gtk-justification sym)
  "Transforms symbol SYM to corresponding GtkJustification value."
  (schematic_gtk_justification_from_string
   (string->pointer (symbol->string sym))))


(define (gtk-message-type->symbol type)
  "Transforms GtkMessageType value TYPE to Scheme symbol."
  (string->symbol (pointer->string (gtk_message_type_to_string type))))

(define (symbol->gtk-message-type sym)
  "Transforms symbol SYM to corresponding GtkMessageType value."
  (gtk_string_to_message_type (string->pointer (symbol->string sym))))


(define (gtk-position-type->symbol type)
  "Transforms GtkPositionType value TYPE to Scheme symbol."
  (string->symbol
   (pointer->string (schematic_gtk_position_type_to_string type))))

(define (symbol->gtk-position-type sym)
  "Transforms symbol SYM to corresponding GtkPositionType value."
  (schematic_gtk_position_type_from_string
   (string->pointer (symbol->string sym))))


(define (gtk-response->symbol response)
  "Transforms GtkResponse integer RESPONSE to Scheme symbol."
  (string->symbol (pointer->string (gtk_response_to_string response))))

(define (symbol->gtk-response sym)
  "Transforms symbol SYM to corresponding GtkResponse integer value."
  (gtk_string_to_response (string->pointer (symbol->string sym))))
