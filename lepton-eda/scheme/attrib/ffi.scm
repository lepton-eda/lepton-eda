;;; Lepton EDA attribute editor
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
            gtk_sheet_cell_get_text
            gtk_sheet_column_button_get_label
            gtk_sheet_delete_columns
            gtk_sheet_get_active_cell
            gtk_sheet_get_selection
            gtk_sheet_insert_columns
            gtk_sheet_unselect_range

            attrib_get_notebook
            attrib_set_notebook
            attrib_get_sheet
            attrib_set_sheet_data
            attrib_get_sheet_data
            attrib_get_sheets_number
            attrib_get_toplevel
            attrib_set_toplevel
            attrib_get_window
            attrib_set_window

            set_verbose_mode
            verbose_done
            verbose_print

            x_dialog_fatal_error
            x_dialog_unimplemented_feature

            x_gtksheet_add_cell_item
            x_gtksheet_add_col_labels
            x_gtksheet_add_row_labels
            x_gtksheet_set_cell_text_color
            x_gtksheet_get_max_col
            x_gtksheet_get_min_col
            x_gtksheet_init

            attrib_sheet_data_new
            attrib_sheet_data_get_changed
            attrib_sheet_data_set_changed
            attrib_sheet_data_get_component_attrib_count
            attrib_sheet_data_set_component_attrib_count
            attrib_sheet_data_get_component_attrib_counter_address
            attrib_sheet_data_get_component_attrib_list
            attrib_sheet_data_set_component_attrib_list
            attrib_sheet_data_get_component_attrib_list_address
            attrib_sheet_data_get_component_count
            attrib_sheet_data_set_component_count
            attrib_sheet_data_get_component_counter_address
            attrib_sheet_data_get_component_list
            attrib_sheet_data_set_component_list
            attrib_sheet_data_get_component_table
            attrib_sheet_data_set_component_table
            attrib_sheet_data_get_net_attrib_count
            attrib_sheet_data_set_net_attrib_count
            attrib_sheet_data_get_net_attrib_list
            attrib_sheet_data_set_net_attrib_list
            attrib_sheet_data_get_net_count
            attrib_sheet_data_set_net_count
            attrib_sheet_data_get_net_list
            attrib_sheet_data_set_net_list
            attrib_sheet_data_get_net_table
            attrib_sheet_data_set_net_table
            attrib_sheet_data_get_pin_attrib_count
            attrib_sheet_data_set_pin_attrib_count
            attrib_sheet_data_get_pin_attrib_counter_address
            attrib_sheet_data_get_pin_attrib_list
            attrib_sheet_data_set_pin_attrib_list
            attrib_sheet_data_get_pin_count
            attrib_sheet_data_set_pin_count
            attrib_sheet_data_get_pin_counter_address
            attrib_sheet_data_get_pin_list
            attrib_sheet_data_set_pin_list
            attrib_sheet_data_get_pin_table
            attrib_sheet_data_set_pin_table
            s_sheet_data_set_changed

            attrib_string_list_get_data
            attrib_string_list_get_next
            s_string_list_new
            s_string_list_add_item
            s_string_list_delete_item
            s_string_list_duplicate_string_list
            s_string_list_find_in_list
            s_string_list_get_data_at_index
            s_string_list_in_list
            s_string_list_sort_master_comp_list
            s_string_list_sort_master_comp_attrib_list
            s_string_list_sort_master_net_list
            s_string_list_sort_master_net_attrib_list
            s_string_list_sort_master_pin_list
            s_string_list_sort_master_pin_attrib_list

            attrib_table_init_attrib_value
            attrib_table_get_attrib_value
            attrib_table_set_attrib_value
            attrib_table_set_column
            attrib_table_init_column_name
            attrib_table_get_column_name
            attrib_table_set_column_name
            attrib_table_set_row
            attrib_table_get_row_contents
            attrib_table_set_row_contents
            attrib_table_init_row_name
            attrib_table_get_row_name
            attrib_table_set_row_name
            attrib_table_get_show_name_value
            attrib_table_set_show_name_value
            attrib_table_get_visibility
            attrib_table_set_visibility
            attrib_table_realloc_row
            attrib_table_new
            attrib_table_row_new

            attrib_run
            attrib_window_menubar_new
            attrib_window_new
            attrib_window_sheets_new
            attrib_window_set_menu_callback
            separator_new
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
(define-lff-lib gtk_sheet_cell_get_text '* (list '* int int) libgtksheet)
(define-lff-lib gtk_sheet_column_button_get_label '* (list '* int) libgtksheet)
(define-lff-lib gtk_sheet_delete_columns void (list '* unsigned-int unsigned-int) libgtksheet)
(define-lff-lib gtk_sheet_get_active_cell void '(* * *) libgtksheet)
(define-lff-lib gtk_sheet_get_selection int '(* * *) libgtksheet)
(define-lff-lib gtk_sheet_insert_columns void (list '* unsigned-int unsigned-int) libgtksheet)
(define-lff-lib gtk_sheet_unselect_range void '(*) libgtksheet)

;;; attrib.c
(define-lff attrib_get_notebook '* '())
(define-lff attrib_set_notebook void '(*))
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
(define-lff verbose_done void '())
(define-lff verbose_print void '(*))

;;; x_dialog.c
(define-lff x_dialog_fatal_error void (list '* int))
(define-lff x_dialog_unimplemented_feature void '())

;;; x_gtksheet.c
(define-lff x_gtksheet_add_cell_item void (list '* int int '* int int))
(define-lff x_gtksheet_add_col_labels void (list '* int '*))
(define-lff x_gtksheet_add_row_labels void (list '* int '*))
(define-lff x_gtksheet_set_cell_text_color void (list '* int int int))
(define-lff x_gtksheet_get_max_col int '(*))
(define-lff x_gtksheet_get_min_col int '(*))
(define-lff x_gtksheet_init void '())

;;; s_sheet_data.c
(define-lff attrib_sheet_data_new '* '())
(define-lff attrib_sheet_data_get_changed int '(*))
(define-lff attrib_sheet_data_set_changed void (list '* int))
(define-lff attrib_sheet_data_get_component_attrib_count int '(*))
(define-lff attrib_sheet_data_set_component_attrib_count void (list '* int))
(define-lff attrib_sheet_data_get_component_attrib_counter_address '* '(*))
(define-lff attrib_sheet_data_get_component_attrib_list '* '(*))
(define-lff attrib_sheet_data_set_component_attrib_list void '(* *))
(define-lff attrib_sheet_data_get_component_attrib_list_address '* '(*))
(define-lff attrib_sheet_data_get_component_count int '(*))
(define-lff attrib_sheet_data_set_component_count void (list '* int))
(define-lff attrib_sheet_data_get_component_counter_address '* '(*))
(define-lff attrib_sheet_data_get_component_list '* '(*))
(define-lff attrib_sheet_data_set_component_list void '(* *))
(define-lff attrib_sheet_data_get_component_table '* '(*))
(define-lff attrib_sheet_data_set_component_table void '(* *))
(define-lff attrib_sheet_data_get_net_attrib_count int '(*))
(define-lff attrib_sheet_data_set_net_attrib_count void (list '* int))
(define-lff attrib_sheet_data_get_net_attrib_list '* '(*))
(define-lff attrib_sheet_data_set_net_attrib_list void '(* *))
(define-lff attrib_sheet_data_get_net_count int '(*))
(define-lff attrib_sheet_data_set_net_count void (list '* int))
(define-lff attrib_sheet_data_get_net_list '* '(*))
(define-lff attrib_sheet_data_set_net_list void '(* *))
(define-lff attrib_sheet_data_get_net_table '* '(*))
(define-lff attrib_sheet_data_set_net_table void '(* *))
(define-lff attrib_sheet_data_get_pin_attrib_count int '(*))
(define-lff attrib_sheet_data_set_pin_attrib_count void (list '* int))
(define-lff attrib_sheet_data_get_pin_attrib_counter_address '* '(*))
(define-lff attrib_sheet_data_get_pin_attrib_list '* '(*))
(define-lff attrib_sheet_data_set_pin_attrib_list void '(* *))
(define-lff attrib_sheet_data_get_pin_count int '(*))
(define-lff attrib_sheet_data_set_pin_count void (list '* int))
(define-lff attrib_sheet_data_get_pin_counter_address '* '(*))
(define-lff attrib_sheet_data_get_pin_list '* '(*))
(define-lff attrib_sheet_data_set_pin_list void '(* *))
(define-lff attrib_sheet_data_get_pin_table '* '(*))
(define-lff attrib_sheet_data_set_pin_table void '(* *))
(define-lff s_sheet_data_set_changed void (list '* int))

;;; s_string_list.c
(define-lff attrib_string_list_get_data '* '(*))
(define-lff attrib_string_list_get_next '* '(*))
(define-lff s_string_list_new '* '())
(define-lff s_string_list_add_item void '(* * *))
(define-lff s_string_list_delete_item void '(* * *))
(define-lff s_string_list_duplicate_string_list '* '(*))
(define-lff s_string_list_find_in_list int '(* *))
(define-lff s_string_list_get_data_at_index '* (list '* int))
(define-lff s_string_list_in_list int '(* *))
(define-lff s_string_list_sort_master_comp_list void '())
(define-lff s_string_list_sort_master_comp_attrib_list void '())
(define-lff s_string_list_sort_master_net_list void '())
(define-lff s_string_list_sort_master_net_attrib_list void '())
(define-lff s_string_list_sort_master_pin_list void '())
(define-lff s_string_list_sort_master_pin_attrib_list void '())

;;; s_table.c
(define-lff attrib_table_init_attrib_value void (list '* int int))
(define-lff attrib_table_get_attrib_value '* (list '* int int))
(define-lff attrib_table_set_attrib_value void (list '* int int '*))
(define-lff attrib_table_set_column void (list '* int int int))
(define-lff attrib_table_init_column_name void (list '* int int))
(define-lff attrib_table_get_column_name '* (list '* int int))
(define-lff attrib_table_set_column_name void (list '* int int '*))
(define-lff attrib_table_set_row void (list '* int int int))
(define-lff attrib_table_get_row_contents '* (list '* int))
(define-lff attrib_table_set_row_contents void (list '* int '*))
(define-lff attrib_table_init_row_name void (list '* int int))
(define-lff attrib_table_get_row_name '* (list '* int int))
(define-lff attrib_table_set_row_name void (list '* int int '*))
(define-lff attrib_table_get_show_name_value int (list '* int int))
(define-lff attrib_table_set_show_name_value void (list '* int int int))
(define-lff attrib_table_get_visibility int (list '* int int))
(define-lff attrib_table_set_visibility void (list '* int int int))
(define-lff attrib_table_realloc_row '* (list '* int))
(define-lff attrib_table_new '* (list int))
(define-lff attrib_table_row_new '* (list int))

;;; x_window.c
(define-lff attrib_run int '(* *))
(define-lff attrib_window_menubar_new '* '(*))
(define-lff attrib_window_new '* '(*))
(define-lff attrib_window_sheets_new void '())
(define-lff attrib_window_set_menu_callback void '(* *))
(define-lff separator_new '* '())
