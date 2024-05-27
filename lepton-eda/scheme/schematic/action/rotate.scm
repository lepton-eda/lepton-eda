;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023-2025 Lepton EDA Contributors
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not write to the Free Software
;;; Foundation Inc. 51 Franklin Street Fifth Floor Boston MA 02110-1301 USA.


(define-module (schematic action rotate)
  #:use-module (system foreign)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)

  #:export (rotate-objects))

(define (rotate-objects *window center-x center-y angle *objects)
  "Rotate the list *OBJECTS in *WINDOW around the coords CENTER-X and
CENTER-Y by ANGLE."
  ;; This is okay if you just hit rotate and have nothing
  ;; selected.
  (if (null-pointer? *objects)
      (begin
        (i_action_stop *window)
        (i_set_state *window (symbol->action-mode 'select-mode)))
      (begin
        (o_invalidate_glist *window *objects)
        (o_rotate_world_update *window center-x center-y angle *objects))))
