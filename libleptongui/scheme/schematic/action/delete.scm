;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2023 Lepton EDA Contributors
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


(define-module (schematic action delete)
  #:use-module (srfi srfi-1)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton object foreign)
  #:use-module (lepton object)

  #:use-module (schematic ffi)
  #:use-module (schematic action-mode)
  #:use-module (schematic gtk helper)
  #:use-module (schematic hook)
  #:use-module (schematic window foreign)
  #:use-module (schematic window global)

  #:export (delete-selection))

;;; Temp definition.
(define UNDO_ALL 0)

(define* (delete-selection *window)
  "Delete selected objects on the active page in *WINDOW."
  (define *active-page (schematic_window_get_active_page *window))
  (define *selection (schematic_window_get_selection_list *window))
  (define selected-objects
    (glist->list (lepton_list_get_glist *selection) pointer->object))
  (define locked? (negate object-selectable?))
  (define locked-objects (filter locked? selected-objects))
  (define non-locked-objects (filter object-selectable? selected-objects))

  (let ((objects-to-remove
         (if (null? locked-objects)
             ;; If there are no locked objects, remove them all.
             selected-objects
             ;; Otherwise ask the user what objects to remove.
             (case (gtk-response->symbol (schematic_delete_dialog))
               ;; Remove all objects.
               ((yes) selected-objects)
               ;; Remove non-locked objects.
               ((no) non-locked-objects)
               ;; Cancel the action.
               (else '())))))

    (unless (null? objects-to-remove)
      ;; First remove objects from selection and from the active
      ;; page.
      (for-each
       (lambda (*object)
         (o_selection_remove *selection *object)
         (lepton_page_remove *active-page *object))
       (map object->pointer objects-to-remove))

      ;; Run hook on the objects before removing them.
      (with-window *window
                   (run-hook remove-objects-hook objects-to-remove))

      (when (and (in-action? (pointer->window *window))
                 (eq? (action-mode->symbol
                       (schematic_window_get_action_mode *window))
                      'move-mode))
        ;; In MOVE mode selection is equal to the place list and
        ;; we have to remove the place list as well.
        ;; o_move_cancel() will do it for us.
        (o_move_cancel *window)
        ;; Now change the current mode to SELECT since we have
        ;; nothing to move any more.
        (i_set_state *window (symbol->action-mode 'select-mode)))

      ;; Actually remove the objects.
      (for-each lepton_object_delete
                (map object->pointer objects-to-remove))

      (schematic_window_active_page_changed *window)
      (o_undo_savestate_old *window UNDO_ALL)
      (i_update_menus *window))))
