;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2020-2022 Lepton EDA Contributors
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

(define-module (schematic ffi)
  #:use-module (system foreign)
  #:use-module (lepton ffi lib)

  #:export (g_init_keys
            g_init_window
            generic_confirm_dialog
            generic_filesel_dialog
            generic_msg_dialog
            i_callback_add_arc
            i_callback_add_attribute
            i_callback_add_box
            i_callback_add_bus
            i_callback_add_circle
            i_callback_add_component
            i_callback_add_line
            i_callback_add_net
            i_callback_add_path
            i_callback_add_picture
            i_callback_add_pin
            i_callback_add_text
            i_callback_attributes_show_both
            i_callback_attributes_show_name
            i_callback_attributes_show_value
            i_callback_attributes_visibility_toggle
            i_callback_cancel
            i_callback_clipboard_copy
            i_callback_clipboard_cut
            i_callback_clipboard_paste
            i_callback_edit_autonumber_text
            i_callback_edit_copy
            i_callback_edit_delete
            i_callback_edit_deselect
            i_callback_edit_edit
            i_callback_edit_embed
            i_callback_edit_find
            i_callback_edit_hide_text
            i_callback_edit_invoke_macro
            i_callback_edit_lock
            i_callback_edit_mcopy
            i_callback_edit_mirror
            i_callback_edit_move
            i_callback_edit_object_properties
            i_callback_edit_redo
            i_callback_edit_rotate_90
            i_callback_edit_select
            i_callback_edit_select_all
            i_callback_edit_show_hidden
            i_callback_edit_show_text
            i_callback_edit_slot
            i_callback_edit_text
            i_callback_edit_translate
            i_callback_edit_undo
            i_callback_edit_unembed
            i_callback_edit_unlock
            i_callback_edit_update
            i_callback_file_close
            i_callback_file_new
            i_callback_file_new_window
            i_callback_file_open
            i_callback_file_print
            i_callback_file_quit
            i_callback_file_save
            i_callback_file_save_all
            i_callback_file_save_as
            i_callback_file_script
            i_callback_file_write_png
            i_callback_help_about
            i_callback_help_hotkeys
            i_callback_hierarchy_down_schematic
            i_callback_hierarchy_down_symbol
            i_callback_hierarchy_up
            i_callback_options_afeedback
            i_callback_options_draw_grips
            i_callback_options_grid
            i_callback_options_magneticnet
            i_callback_options_rubberband
            i_callback_options_scale_down_snap_size
            i_callback_options_scale_up_snap_size
            i_callback_options_select_font
            i_callback_options_show_coord_window
            i_callback_options_show_log_window
            i_callback_options_snap
            i_callback_options_snap_size
            i_callback_page_close
            i_callback_page_manager
            i_callback_page_next
            i_callback_page_next_tab
            i_callback_page_prev
            i_callback_page_prev_tab
            i_callback_page_print
            i_callback_page_revert
            i_callback_view_bw_colors
            i_callback_view_color_edit
            i_callback_view_dark_colors
            i_callback_view_find_text_state
            i_callback_view_light_colors
            i_callback_view_pan
            i_callback_view_pan_down
            i_callback_view_pan_left
            i_callback_view_pan_right
            i_callback_view_pan_up
            i_callback_view_redraw
            i_callback_view_sidebar
            i_callback_view_status
            i_callback_view_zoom_box
            i_callback_view_zoom_extents
            i_callback_view_zoom_full
            i_callback_view_zoom_in
            i_callback_view_zoom_out
            make_menu_action
            make_separator_menu_item
            o_attrib_add_attrib
            o_buffer_init
            o_undo_init
            set_quiet_mode
            set_verbose_mode
            x_color_init
            x_menu_attach_recent_files_submenu
            x_show_uri
            x_stroke_init
            x_widgets_show_log
            x_window_create_main
            x_window_close_page
            x_window_new
            x_window_open_page
            x_window_set_current_page
            x_window_setup

            gschem_page_view_get_page

            gschem_toplevel_get_current_page_view
            gschem_toplevel_get_toplevel
            schematic_window_get_active_page
            schematic_window_get_options

            gschem_options_get_snap_size

            o_undo_savestate

            x_event_get_pointer_position
            ))

(define libleptongui
  (dynamic-link (or (getenv "LIBLEPTONGUI") %libleptongui)))

;;; Brief syntax macro for defining lazy foreign functions.
(define-syntax define-lff
  (syntax-rules ()
    ((_ name type args)
     (define name
       (let ((proc (delay (pointer->procedure
                           type
                           (dynamic-func (symbol->string (quote name)) libleptongui)
                           args))))
         (force proc))))))


(define-lff g_init_keys void '())
(define-lff g_init_window void '())
(define-lff o_attrib_add_attrib '* (list '* '* int int '*))
(define-lff o_buffer_init void '())
(define-lff set_quiet_mode void '())
(define-lff set_verbose_mode void '())
(define-lff x_color_init void '())
(define-lff x_widgets_show_log void (list '*))

;;; gschem_page_view.c
(define-lff gschem_page_view_get_page '* '(*))

;;; gschem_toplevel.c
(define-lff gschem_toplevel_get_current_page_view '* '(*))
(define-lff gschem_toplevel_get_toplevel '* '(*))
(define-lff schematic_window_get_active_page '* '(*))
(define-lff schematic_window_get_options '* '(*))

;;; gschem_options.c
(define-lff gschem_options_get_snap_size int '(*))

;;; x_menus.c
(define-lff make_separator_menu_item '* '())
(define-lff make_menu_action '* '(* * * * *))
(define-lff x_menu_attach_recent_files_submenu void '(* *))
;;; x_window.c
(define-lff x_window_new '* '())
(define-lff x_window_open_page '* '(* *))
(define-lff x_window_set_current_page void '(* *))
(define-lff x_window_setup '* '(*))
(define-lff x_window_create_main '* '(* *))
(define-lff x_window_close_page void '(* *))

;;; x_dialog.c
(define-lff generic_confirm_dialog int '(*))
(define-lff generic_filesel_dialog '* (list '* '* int))
(define-lff generic_msg_dialog void '(*))
;;; i_callbacks.c
(define-lff i_callback_add_arc void '(* *))
(define-lff i_callback_add_attribute void '(* *))
(define-lff i_callback_add_box void '(* *))
(define-lff i_callback_add_bus void '(* *))
(define-lff i_callback_add_circle void '(* *))
(define-lff i_callback_add_component void '(* *))
(define-lff i_callback_add_line void '(* *))
(define-lff i_callback_add_net void '(* *))
(define-lff i_callback_add_path void '(* *))
(define-lff i_callback_add_picture void '(* *))
(define-lff i_callback_add_pin void '(* *))
(define-lff i_callback_add_text void '(* *))
(define-lff i_callback_attributes_show_both void '(* *))
(define-lff i_callback_attributes_show_name void '(* *))
(define-lff i_callback_attributes_show_value void '(* *))
(define-lff i_callback_attributes_visibility_toggle void '(* *))
(define-lff i_callback_cancel void '(* *))
(define-lff i_callback_clipboard_copy void '(* *))
(define-lff i_callback_clipboard_cut void '(* *))
(define-lff i_callback_clipboard_paste void '(* *))
(define-lff i_callback_edit_autonumber_text void '(* *))
(define-lff i_callback_edit_copy void '(* *))
(define-lff i_callback_edit_delete void '(* *))
(define-lff i_callback_edit_deselect void '(* *))
(define-lff i_callback_edit_edit void '(* *))
(define-lff i_callback_edit_embed void '(* *))
(define-lff i_callback_edit_find void '(* *))
(define-lff i_callback_edit_hide_text void '(* *))
(define-lff i_callback_edit_invoke_macro void '(* *))
(define-lff i_callback_edit_lock void '(* *))
(define-lff i_callback_edit_mcopy void '(* *))
(define-lff i_callback_edit_mirror void '(* *))
(define-lff i_callback_edit_move void '(* *))
(define-lff i_callback_edit_object_properties void '(* *))
(define-lff i_callback_edit_redo void '(* *))
(define-lff i_callback_edit_rotate_90 void '(* *))
(define-lff i_callback_edit_select void '(* *))
(define-lff i_callback_edit_select_all void '(* *))
(define-lff i_callback_edit_show_hidden void '(* *))
(define-lff i_callback_edit_show_text void '(* *))
(define-lff i_callback_edit_slot void '(* *))
(define-lff i_callback_edit_text void '(* *))
(define-lff i_callback_edit_translate void '(* *))
(define-lff i_callback_edit_undo void '(* *))
(define-lff i_callback_edit_unembed void '(* *))
(define-lff i_callback_edit_unlock void '(* *))
(define-lff i_callback_edit_update void '(* *))
(define-lff i_callback_file_close void '(* *))
(define-lff i_callback_file_new void '(* *))
(define-lff i_callback_file_new_window void '(* *))
(define-lff i_callback_file_open void '(* *))
(define-lff i_callback_file_print void '(* *))
(define-lff i_callback_file_quit void '(* *))
(define-lff i_callback_file_save void '(* *))
(define-lff i_callback_file_save_all void '(* *))
(define-lff i_callback_file_save_as void '(* *))
(define-lff i_callback_file_script void '(* *))
(define-lff i_callback_file_write_png void '(* *))
(define-lff i_callback_help_about void '(* *))
(define-lff i_callback_help_hotkeys void '(* *))
(define-lff i_callback_hierarchy_down_schematic void '(* *))
(define-lff i_callback_hierarchy_down_symbol void '(* *))
(define-lff i_callback_hierarchy_up void '(* *))
(define-lff i_callback_options_afeedback void '(* *))
(define-lff i_callback_options_draw_grips void '(* *))
(define-lff i_callback_options_grid void '(* *))
(define-lff i_callback_options_magneticnet void '(* *))
(define-lff i_callback_options_rubberband void '(* *))
(define-lff i_callback_options_scale_down_snap_size void '(* *))
(define-lff i_callback_options_scale_up_snap_size void '(* *))
(define-lff i_callback_options_select_font void '(* *))
(define-lff i_callback_options_show_coord_window void '(* *))
(define-lff i_callback_options_show_log_window void '(* *))
(define-lff i_callback_options_snap void '(* *))
(define-lff i_callback_options_snap_size void '(* *))
(define-lff i_callback_view_bw_colors void '(* *))
(define-lff i_callback_view_color_edit void '(* *))
(define-lff i_callback_view_dark_colors void '(* *))
(define-lff i_callback_view_find_text_state void '(* *))
(define-lff i_callback_view_light_colors void '(* *))
(define-lff i_callback_view_pan void '(* *))
(define-lff i_callback_view_pan_down void '(* *))
(define-lff i_callback_view_pan_left void '(* *))
(define-lff i_callback_view_pan_right void '(* *))
(define-lff i_callback_view_pan_up void '(* *))
(define-lff i_callback_view_redraw void '(* *))
(define-lff i_callback_view_sidebar void '(* *))
(define-lff i_callback_view_status void '(* *))
(define-lff i_callback_view_zoom_box void '(* *))
(define-lff i_callback_view_zoom_extents void '(* *))
(define-lff i_callback_view_zoom_full void '(* *))
(define-lff i_callback_view_zoom_in void '(* *))
(define-lff i_callback_view_zoom_out void '(* *))
(define-lff i_callback_page_close void '(* *))
(define-lff i_callback_page_manager void '(* *))
(define-lff i_callback_page_next void '(* *))
(define-lff i_callback_page_next_tab void '(* *))
(define-lff i_callback_page_prev void '(* *))
(define-lff i_callback_page_prev_tab void '(* *))
(define-lff i_callback_page_print void '(* *))
(define-lff i_callback_page_revert void '(* *))
;;; x_misc.c
(define-lff x_show_uri int '(* * *))
;;; x_event.c
(define-lff x_event_get_pointer_position int (list '* int '* '*))

;;; o_undo.c
(define-lff o_undo_init void '())
(define-lff o_undo_savestate void (list '* '* int))

;;; This is a special case: the function may be not defined in C
;;; if libstroke was not found on the configure stage.
(define (x_stroke_init)
  (let ((func (delay (false-if-exception (dynamic-func "x_stroke_init"
                                                       libleptongui)))))
    (and (force func)
         (let ((proc (delay (pointer->procedure void (force func) '()))))
           ((force proc))))))
