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
  #:use-module (system foreign)

  #:use-module (schematic ffi gtk)
  #:use-module (schematic ffi)

  #:export (add-tab-canvas!
            append-tab!
            close-page-tab!
            make-tabs-notebook
            set-tab-header!))


(define (make-tabs-notebook *window *work-box)
  "Creates a new tabs notebook in *WINDOW and adds it to the container
*WORK-BOX.  Returns the new notebook widget."
  (x_tabs_nbook_create *window *work-box))


(define (add-tab-canvas! *tab *canvas)
  "Adds *CANVAS to the *TAB widget container and focuses it."
  (schematic_tabs_add_canvas *canvas *tab))


(define (append-tab! *window *tab)
  "Appends new *TAB to the tabs notebook in *WINDOW.  Returns the index
of the new tab."
  (gtk_notebook_append_page (schematic_window_get_tab_notebook *window)
                            *tab
                            %null-pointer))


(define (close-page-tab! *window *page)
  "Closes a tab associated with *PAGE in *WINDOW."

  (define *info-list (schematic_window_get_tab_info_list *window))
  (define *tab-info (x_tabs_info_find_by_page *info-list *page))

  (unless (null-pointer? *tab-info)
    (let* ((*notebook (schematic_window_get_tab_notebook *window))
           (index (gtk_notebook_page_num
                   *notebook
                   (schematic_tab_info_get_tab_widget *tab-info))))

      (gtk_notebook_remove_page *notebook index))))


(define (set-tab-header! *notebook *tab-info)
  "Creates a header widget for a *NOTEBOOK tab defined by its *TAB-INFO
and sets it as the label for the tab."
  (x_tabs_hdr_set *notebook *tab-info))
