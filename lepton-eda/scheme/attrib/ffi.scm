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

(define-module (attrib ffi)
  #:use-module (system foreign)

  #:use-module (lepton ffi lff)
  #:use-module (lepton ffi lib)
  #:use-module (lepton m4)

  #:export (gtk_sheet_set_active_cell

            attrib_get_notebook
            attrib_get_sheet
            attrib_set_sheet_data
            attrib_get_sheet_data
            attrib_get_sheets_number
            attrib_get_toplevel
            attrib_set_toplevel
            attrib_get_window
            attrib_set_window

            set_verbose_mode

            x_fileselect_open

            x_dialog_about_dialog
            x_dialog_export_file
            x_dialog_missing_sym
            x_dialog_newattrib
            x_dialog_unimplemented_feature
            x_dialog_unsaved_data

            attrib_sheet_data_get_component_attrib_count
            attrib_sheet_data_get_component_count
            attrib_sheet_data_set_component_table
            attrib_sheet_data_get_net_attrib_count
            attrib_sheet_data_get_net_count
            attrib_sheet_data_set_net_table
            attrib_sheet_data_get_pin_attrib_count
            attrib_sheet_data_get_pin_count
            attrib_sheet_data_set_pin_table
            s_sheet_data_new
            s_sheet_data_add_master_comp_list_items
            s_sheet_data_add_master_comp_attrib_list_items
            s_sheet_data_add_master_net_list_items
            s_sheet_data_add_master_net_attrib_list_items
            s_sheet_data_add_master_pin_list_items
            s_sheet_data_add_master_pin_attrib_list_items
            s_sheet_data_changed
            s_sheet_data_set_changed
            s_sheet_data_gtksheet_to_sheetdata

            s_string_list_sort_master_comp_list
            s_string_list_sort_master_comp_attrib_list
            s_string_list_sort_master_net_list
            s_string_list_sort_master_net_attrib_list
            s_string_list_sort_master_pin_list
            s_string_list_sort_master_pin_attrib_list

            s_table_new
            s_table_add_toplevel_comp_items_to_comp_table
            s_table_add_toplevel_net_items_to_net_table
            s_table_add_toplevel_pin_items_to_pin_table

            s_toplevel_sheetdata_to_toplevel

            s_visibility_set_invisible
            s_visibility_set_name_and_value
            s_visibility_set_name_only
            s_visibility_set_value_only

            attrib_run
            attrib_window_new
            attrib_window_sheets_new
            attrib_window_set_menu_callback
            menu_edit_delattrib
            x_window_add_items
            x_window_init
            ))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define libleptonattrib
  (dynamic-link (or (getenv "LIBLEPTONATTRIB") %libleptonattrib)))

(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... libleptonattrib))

(define-syntax-rule (define-lfc arg ...)
  (define-lfc-lib arg ... libleptonattrib))


(define %libgtksheet
  (if %m4-use-gtk3 "libgtksheet-4.0" "libgtkextra-x11-3.0"))

(define libgtksheet (dynamic-link %libgtksheet))

(define-lff-lib gtk_sheet_set_active_cell int (list '* int int) libgtksheet)


;;; attrib.c
(define-lff attrib_get_notebook '* '())
(define-lff attrib_get_sheet '* (list int))
(define-lff attrib_set_sheet_data void '(*))
(define-lff attrib_get_sheet_data '* '())
(define-lff attrib_get_sheets_number int '())
(define-lff attrib_get_toplevel '* '())
(define-lff attrib_set_toplevel void '(*))
(define-lff attrib_get_window '* '())
(define-lff attrib_set_window void '(*))

;;; s_misc.c
(define-lff set_verbose_mode void '())

;;; x_fileselect.c
(define-lff x_fileselect_open '* '())

;;; x_dialog.c
(define-lff x_dialog_about_dialog void '(* * *))
(define-lff x_dialog_export_file void '())
(define-lff x_dialog_missing_sym void '())
(define-lff x_dialog_newattrib void '())
(define-lff x_dialog_unimplemented_feature void '())
(define-lff x_dialog_unsaved_data '* '())

;;; s_sheet_data.c
(define-lff attrib_sheet_data_get_component_attrib_count int '(*))
(define-lff attrib_sheet_data_get_component_count int '(*))
(define-lff attrib_sheet_data_set_component_table void '(* *))
(define-lff attrib_sheet_data_get_net_attrib_count int '(*))
(define-lff attrib_sheet_data_get_net_count int '(*))
(define-lff attrib_sheet_data_set_net_table void '(* *))
(define-lff attrib_sheet_data_get_pin_attrib_count int '(*))
(define-lff attrib_sheet_data_get_pin_count int '(*))
(define-lff attrib_sheet_data_set_pin_table void '(* *))
(define-lff s_sheet_data_new '* '())
(define-lff s_sheet_data_add_master_comp_list_items void '(*))
(define-lff s_sheet_data_add_master_comp_attrib_list_items void '(*))
(define-lff s_sheet_data_add_master_net_list_items void '(*))
(define-lff s_sheet_data_add_master_net_attrib_list_items void '(*))
(define-lff s_sheet_data_add_master_pin_list_items void '(*))
(define-lff s_sheet_data_add_master_pin_attrib_list_items void '(*))
(define-lff s_sheet_data_changed int '(*))
(define-lff s_sheet_data_set_changed void (list '* int))
(define-lff s_sheet_data_gtksheet_to_sheetdata void '())

;;; s_string_list.c
(define-lff s_string_list_sort_master_comp_list void '())
(define-lff s_string_list_sort_master_comp_attrib_list void '())
(define-lff s_string_list_sort_master_net_list void '())
(define-lff s_string_list_sort_master_net_attrib_list void '())
(define-lff s_string_list_sort_master_pin_list void '())
(define-lff s_string_list_sort_master_pin_attrib_list void '())

;;; s_table.c
(define-lff s_table_new '* (list int int))
(define-lff s_table_add_toplevel_comp_items_to_comp_table void '(*))
;;; The following function is not really defined.
;; (define-lff s_table_add_toplevel_net_items_to_net_table void '(*))
(define s_table_add_toplevel_net_items_to_net_table #f)
(define-lff s_table_add_toplevel_pin_items_to_pin_table void '(*))

;;; s_toplevel.c
(define-lff s_toplevel_sheetdata_to_toplevel void '(* *))

;;; s_visibility.c
(define-lff s_visibility_set_invisible void '(* * *))
(define-lff s_visibility_set_name_and_value void '(* * *))
(define-lff s_visibility_set_name_only void '(* * *))
(define-lff s_visibility_set_value_only void '(* * *))

;;; x_window.c
(define-lff attrib_run int '(* *))
(define-lff attrib_window_new '* '(*))
(define-lff attrib_window_sheets_new void '())
(define-lff attrib_window_set_menu_callback void '(* *))
(define-lff menu_edit_delattrib void '(* * *))
(define-lff x_window_add_items void '())
(define-lff x_window_init void '())
