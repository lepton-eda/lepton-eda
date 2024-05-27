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
  #:use-module (lepton object foreign)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic hook)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (rotate-objects))

(define* (rotate-objects center-x center-y angle *objects
                         #:key (window (current-window)))
  "Rotate the list *OBJECTS in WINDOW around the coords CENTER-X and
CENTER-Y by ANGLE.  The function runs the hook
rotate-objects-hook() after rotation of the objects."
  (define *window (window->pointer window))
  (define *object-ls (glist->list *objects identity))
  (define objects (glist->list *objects pointer->object))

  ;; This is okay if you just hit rotate and have nothing
  ;; selected.
  (if (null? *object-ls)
      (begin
        (i_action_stop *window)
        (set-action-mode! 'select-mode #:window window))
      (begin
        (o_invalidate_glist *window *objects)
        ;; Find connected objects, removing each object in turn
        ;; from the connection list.  We only *really* want those
        ;; objects connected to the selection, not those within in
        ;; it.
        (for-each s_conn_remove_object_connections *object-ls)
        (lepton_object_list_rotate *objects center-x center-y angle)

        ;; Find connected objects, adding each object in turn back
        ;; to the connection list.  We only *really* want those
        ;; objects connected to the selection, not those within in
        ;; it.
        (for-each
         (lambda (*object)
           (s_conn_update_object (lepton_object_get_page *object)
                                 *object))
         *object-ls)
        (o_invalidate_glist *window *objects)
        ;; Run hook.
        (with-window *window (run-hook rotate-objects-hook objects))
        (schematic_window_active_page_changed *window)
        ;; Don't save the undo state if we are inside an action.
        ;; This is useful when rotating the selection while
        ;; moving, for example.
        (unless (in-action? window)
          (o_undo_savestate_old *window))
        (when (eq? (action-mode window) 'rotate-mode)
          (set-action-mode! 'select-mode #:window window)))))
