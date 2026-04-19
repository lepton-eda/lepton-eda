;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2025-2026 Lepton EDA Contributors
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


(define-module (schematic action mirror)
  #:use-module (system foreign)

  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic window foreign)

  #:export (mirror-objects))


(define (mirror-objects *window x y *objects)
  "Mirror *OBJECTS in *WINDOW around the center point (X . Y).  The
coords are in the world units."
  (if (null-pointer? *objects)
      (begin
        (i_action_stop *window)
        (set-action-mode! 'select-mode #:window (pointer->window *window)))

      (begin
        (schematic_draw_invalidate_object_list *window *objects)

        ;; Find connected objects, removing each object in turn
        ;; from the connection list. We only _really_ want those
        ;; objects connected to the selection, not those within in
        ;; it.
        (for-each s_conn_remove_object_connections
                  (glist->list *objects identity))

        (lepton_object_list_mirror *objects x y)

        ;; Find connected objects, adding each object in turn back
        ;; to the connection list. We only _really_ want those
        ;; objects connected to the selection, not those within in
        ;; it.
        (for-each
         (lambda (*object)
           (let ((*page (lepton_object_get_page *object)))
             (s_conn_update_object *page *object)))
         (glist->list *objects identity))

        (schematic_draw_invalidate_object_list *window *objects)

        (o_mirror_world_update *window x y *objects))))
