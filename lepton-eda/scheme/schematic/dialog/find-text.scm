;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2024-2025 Lepton EDA Contributors
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


(define-module (schematic dialog find-text)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi)

  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (find-text-dialog))


(define (find-text-dialog window)
  "Open the Find text dialog in WINDOW."
  (define *window (check-window window 1))
  (define *object (o_select_return_first_object *window))

  (find_text_dialog *window
                    (if (true? (lepton_object_is_text *object))
                        (lepton_text_object_get_string *object)
                        %null-pointer)))
