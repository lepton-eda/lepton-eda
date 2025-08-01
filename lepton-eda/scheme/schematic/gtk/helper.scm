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

(define-module (schematic gtk helper)
  #:use-module (system foreign)

  #:use-module (schematic ffi)

  #:export (gtk-response->symbol
            symbol->gtk-response))

(define (gtk-response->symbol response)
  "Transforms GtkResponse integer RESPONSE to Scheme symbol."
  (string->symbol (pointer->string (gtk_response_to_string response))))

(define (symbol->gtk-response sym)
  "Transforms symbol SYM to corresponding GtkResponse integer value."
  (gtk_string_to_response (string->pointer (symbol->string sym))))
