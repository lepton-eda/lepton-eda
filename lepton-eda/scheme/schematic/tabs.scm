;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2026 Lepton EDA Contributors
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


(define-module (schematic tabs)
  #:use-module (schematic ffi)

  #:export (add-tab-canvas!
            close-page-tab!
            set-tab-header!))


(define (add-tab-canvas! *tab *canvas)
  "Add *CANVAS to the *TAB widget container and focus it."
  (schematic_tabs_add_canvas *canvas *tab))


(define (close-page-tab! *window *page)
  "Closes a tab associated with *PAGE in *WINDOW."
  (x_tabs_nbook_page_close *window *page))


(define (set-tab-header! *notebook *tab-info)
  "Creates a header widget for a *NOTEBOOK tab defined by its *TAB-INFO
and sets it as the label for the tab."
  (x_tabs_hdr_set *notebook *tab-info))
