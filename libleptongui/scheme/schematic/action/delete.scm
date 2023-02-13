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

  #:export (delete-selection))

(define* (delete-selection *window)
  "Delete selected objects in *WINDOW."
  (define *active-page (schematic_window_get_active_page *window))
  (define *selection (schematic_window_get_selection_list *window))
  (define objects-to-remove
    (glist->list (lepton_list_get_glist *selection) pointer->object))
  (define locked? (negate object-selectable?))
  (define locked-exist? (any locked? objects-to-remove))

  (o_delete_selected *window
                     *active-page
                     *selection
                     (if locked-exist? TRUE FALSE)))
