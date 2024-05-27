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
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)

  #:export (rotate-objects))

(define (rotate-objects *window center-x center-y angle *objects)
  "Rotate the list *OBJECTS in *WINDOW around the coords CENTER-X and
CENTER-Y by ANGLE."
  (define *object-ls (glist->list *objects identity))

  ;; This is okay if you just hit rotate and have nothing
  ;; selected.
  (if (null? *object-ls)
      (begin
        (i_action_stop *window)
        (i_set_state *window (symbol->action-mode 'select-mode)))
      (begin
        (o_invalidate_glist *window *objects)
        ;; Find connected objects, removing each object in turn
        ;; from the connection list.  We only *really* want those
        ;; objects connected to the selection, not those within in
        ;; it.
        (for-each s_conn_remove_object_connections *object-ls)
        (lepton_object_list_rotate *objects center-x center-y angle)
        (o_rotate_world_update *window center-x center-y angle *objects))))
