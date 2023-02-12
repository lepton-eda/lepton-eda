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


(define-module (schematic action copy)
  #:use-module (system foreign)

  #:use-module (lepton ffi boolean)
  #:use-module (lepton ffi glib)
  #:use-module (lepton ffi)
  #:use-module (lepton object foreign)

  #:use-module (schematic action-mode)
  #:use-module (schematic ffi)
  #:use-module (schematic hook)
  #:use-module (schematic window global)

  #:export (finish-copy
            start-copy))

(define* (finish-copy *window #:optional keep-on?)
  "Finish copy action in *WINDOW."
  (define continue-placement? (if keep-on? TRUE FALSE))

  (o_place_end *window
               (schematic_window_get_second_wx *window)
               (schematic_window_get_second_wy *window)
               continue-placement?
               (string->pointer "paste-objects-hook")))


(define (start-copy *window x y)
  "Copy objects in *WINDOW into the buffer at their current
position, with future motion relative to the mouse origin, (X
. Y)."

  (schematic_window_set_first_wx *window x)
  (schematic_window_set_first_wy *window y)

  (when (true? (o_select_selected *window))
    (let ((*selection
           (lepton_list_get_glist
            (schematic_window_get_selection_list *window))))
      (schematic_window_delete_place_list *window)

      (schematic_window_set_place_list
       *window
       (o_glist_copy_all *selection
                         (schematic_window_get_place_list *window)))

      (let ((objects
             (glist->list (schematic_window_get_place_list *window)
                          pointer->object)))
        (with-window *window (run-hook copy-objects-hook objects)))

      (o_place_start *window x y))))
