;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2024 Lepton EDA Contributors
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


(define-module (schematic dialog attrib-edit)
  #:use-module (system foreign)

  #:use-module (schematic ffi)

  #:export (attrib-edit-dialog))


(define (attrib-edit-dialog *window *attrib invocation-flag)
  "Launch the single attrib edit dialog in *WINDOW for *ATTRIB.  The
INVOCATION-FLAG must be an integer set to a value defined in
libleptongui/include/schematic_defines.h, namely FROM_MENU (0) or
FROM_KEY (1)."
  ;; Return if the widget has already been created.
  (when (null-pointer? (schematic_window_get_attrib_edit_widget *window))
    (attrib_edit_dialog *window *attrib invocation-flag)))
