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
  #:use-module (srfi srfi-9)

  #:use-module (lepton ffi lff)
  #:use-module (lepton ffi lib)
  #:use-module (lepton ffi)

  #:export (lepton_schematic_run
            lepton_schematic_app
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
            i_callback_close_wm
            i_callback_edit_autonumber_text
            i_callback_edit_edit
            i_callback_edit_embed
            i_callback_edit_invoke_macro
            i_callback_edit_mirror
            i_callback_edit_rotate_90
            i_callback_edit_show_hidden
            i_callback_edit_translate
            i_callback_edit_unembed
            i_callback_edit_update
            i_callback_file_save
            *i_callback_file_save
            i_callback_file_script
            i_callback_hierarchy_down_schematic
            i_callback_hierarchy_down_symbol
            i_callback_hierarchy_up
            i_callback_page_next
            i_callback_page_prev
            i_callback_page_print
            i_callback_page_revert
            i_callback_toolbar_add_bus
            i_callback_toolbar_add_net
            i_callback_view_color_edit
            i_callback_view_pan
            i_callback_view_pan_down
            i_callback_view_pan_left
            i_callback_view_pan_right
            i_callback_view_pan_up
            i_callback_view_sidebar
            i_callback_view_status
            i_callback_view_zoom_box
            i_callback_view_zoom_extents
            i_callback_view_zoom_full
            i_callback_view_zoom_in
            i_callback_view_zoom_out

            i_action_stop
            i_set_state
            i_set_state_msg
            i_show_state
            i_update_grid_info
            i_update_menus

            make_menu_action
            make_separator_menu_item
            schematic_window_create_main_popup_menu

            o_attrib_add_attrib

            o_buffer_init
            o_undo_init

            o_redraw_cleanstates
            o_invalidate_rubber

            o_copy_start

            o_delete_selected

            o_lock
            o_unlock

            o_move_start

            o_place_invalidate_rubber

            page_select_widget_new
            page_select_widget_update

            set_quiet_mode
            set_verbose_mode
            x_color_init

            color_edit_widget_update

            x_colorcb_update_colors

            x_menu_attach_recent_files_submenu
            x_show_uri
            x_stroke_init

            find_text_dialog
            hide_text_dialog
            show_text_dialog

            slot_edit_dialog
            slot_edit_dialog_response
            slot_edit_dialog_get_text
            slot_edit_dialog_quit

            x_widgets_create
            x_widgets_init
            x_widgets_show_find_text_state
            x_widgets_show_font_select
            x_widgets_show_log
            x_widgets_show_object_properties
            x_widgets_show_options
            x_widgets_show_page_select

            x_window_close
            x_window_close_all
            x_window_close_page
            x_window_new
            x_window_open_page
            x_window_save_page
            x_window_set_current_page
            x_window_setup
            x_window_setup_draw_events_drawing_area
            x_window_setup_draw_events_main_wnd
            x_window_untitled_page
            schematic_window_create_app_window
            schematic_window_create_main_box
            schematic_window_create_work_box
            schematic_window_create_menubar
            schematic_toolbar_toggle_tool_button_get_active
            schematic_window_get_inside_action
            schematic_window_set_key_event_callback
            schematic_window_set_page_select_widget
            schematic_window_create_page_view
            schematic_window_create_find_text_widget
            schematic_window_create_hide_text_widget
            schematic_window_create_show_text_widget
            schematic_window_create_macro_widget
            schematic_window_create_translate_widget
            schematic_window_create_notebooks
            schematic_window_create_statusbar
            schematic_window_restore_geometry
            schematic_window_show_all
            schematic_window_set_main_window
            schematic_window_set_toolbar

            schematic_toolbar_new
            schematic_toolbar_activate_button
            schematic_toolbar_button_new
            schematic_toolbar_button_set_icon_widget
            schematic_toolbar_button_set_label
            schematic_toolbar_button_set_tooltip_text
            schematic_toolbar_radio_button_new
            schematic_toolbar_radio_button_get_group
            schematic_toolbar_radio_button_set_group
            schematic_toolbar_insert_button
            schematic_toolbar_insert_separator

            x_tabs_create
            x_tabs_enabled
            x_tabs_hdr_update

            schematic_action_mode_from_string
            schematic_action_mode_to_string

            schematic_grid_mode_from_string
            schematic_grid_mode_to_string

            schematic_snap_mode_from_string
            schematic_snap_mode_to_string

            about_dialog

            x_dialog_close_changed_page

            coord_dialog

            x_dialog_hotkeys

            schematic_keys_get_event_keyval
            schematic_keys_get_event_modifiers
            schematic_keys_verify_keyval

            lepton_action_create_menu_item
            lepton_menu_set_action_data

            gschem_page_view_get_page
            gschem_page_view_invalidate_all

            schematic_signal_connect

            gschem_toplevel_get_current_page_view
            gschem_toplevel_get_toplevel
            schematic_window_get_actionfeedback_mode
            schematic_window_set_actionfeedback_mode
            schematic_window_get_active_page
            schematic_window_get_draw_grips
            schematic_window_set_draw_grips
            schematic_window_get_gdk_display
            schematic_window_get_options
            schematic_window_get_place_list
            schematic_window_update_keyaccel_string
            schematic_window_update_keyaccel_timer

            gschem_options_cycle_grid_mode
            gschem_options_get_grid_mode
            gschem_options_cycle_magnetic_net_mode
            gschem_options_cycle_net_rubber_band_mode
            gschem_options_cycle_snap_mode
            gschem_options_get_snap_mode
            gschem_options_get_snap_size
            gschem_options_set_snap_size

            text_edit_dialog

            o_select_return_first_object
            o_select_unselect_all
            o_select_visible_unlocked

            o_slot_end

            o_undo_callback
            o_undo_savestate

            x_event_get_pointer_position
            x_event_key

            x_fileselect_open
            x_fileselect_save

            x_image_setup

            x_print

            x_tabs_next
            x_tabs_prev

            parse-gschemrc
            ))

(define libleptongui
  (dynamic-link (or (getenv "LIBLEPTONGUI") %libleptongui)))

;;; Simplify definition of functions by omitting the library
;;; argument.
(define-syntax-rule (define-lff arg ...)
  (define-lff-lib arg ... libleptongui))


;;; Brief syntax macro for defining lazy foreign callbacks.
;;; Unlike 'define-lff' above, it returns a pointer to a C
;;; function by name, not a Scheme procedure wrapping it.
;;;
;;; By convention, the first character of a callback function name
;;; should be '*'.  The first character is dropped here before
;;; dlopening the function in any case, so be careful when
;;; composing callback names.
(define-syntax define-lfc
  (syntax-rules ()
    ((_ name)
     (define name
       (let ((*callback
              (delay (dynamic-func (string-drop (symbol->string (quote name)) 1)
                                   libleptongui))))
         (force *callback))))))


;;; lepton_schematic.c
(define-lff lepton_schematic_run int '(*))
(define-lff lepton_schematic_app '* '())

;;; g_window.c
(define-lff g_init_window void '(*))

;;; o_attrib.c
(define-lff o_attrib_add_attrib '* (list '* '* int int '* int int int))

;;; page_select_widget.c
(define-lff page_select_widget_update void '(*))
(define-lff page_select_widget_new '* '(* * * * *))

(define-lff o_buffer_init void '())
(define-lff set_quiet_mode void '())
(define-lff set_verbose_mode void '())
(define-lff x_color_init void '())

;;; color_edit_widget.c
(define-lff color_edit_widget_update void '(*))

;;; x_colorcb.c
(define-lff x_colorcb_update_colors void '())

;;; x_widgets.c
(define-lff x_widgets_create void '(*))
(define-lff x_widgets_init void '())
(define-lff x_widgets_show_find_text_state void '(*))
(define-lff x_widgets_show_font_select void '(*))
(define-lff x_widgets_show_log void '(*))
(define-lff x_widgets_show_object_properties void '(*))
(define-lff x_widgets_show_options void '(*))
(define-lff x_widgets_show_page_select void '(*))

;;; action_mode.c
(define-lff schematic_action_mode_from_string int '(*))
(define-lff schematic_action_mode_to_string '* (list int))

;;; grid_mode.c
(define-lff schematic_grid_mode_from_string int '(*))
(define-lff schematic_grid_mode_to_string '* (list int))

;;; keys.c
(define-lff schematic_keys_get_event_keyval int '(*))
(define-lff schematic_keys_get_event_modifiers int '(*))
(define-lff schematic_keys_verify_keyval int (list int))

;;; gschem_about_dialog.c
(define-lff about_dialog void '(*))

;;; gschem_close_confirmation_dialog.c
(define-lff x_dialog_close_changed_page int '(* *))

;;; gschem_coord_dialog.c
(define-lff coord_dialog void (list '* int int))

;;; gschem_hotkey_dialog.c
(define-lff x_dialog_hotkeys void '(*))

;;; gschem_page_view.c
(define-lff gschem_page_view_get_page '* '(*))
(define-lff gschem_page_view_invalidate_all void '(*))

;;; slot_edit_dialog.c
(define-lff slot_edit_dialog '* '(* * *))
(define-lff slot_edit_dialog_response int (list int))
(define-lff slot_edit_dialog_get_text '* '(*))
(define-lff slot_edit_dialog_quit void '(*))

;;; snap_mode.c
(define-lff schematic_snap_mode_from_string int '(*))
(define-lff schematic_snap_mode_to_string '* (list int))

;;; gschem_toplevel.c
(define-lff gschem_toplevel_get_current_page_view '* '(*))
(define-lff gschem_toplevel_get_toplevel '* '(*))
(define-lff schematic_window_get_actionfeedback_mode int '(*))
(define-lff schematic_window_set_actionfeedback_mode void (list '* int))
(define-lff schematic_window_get_active_page '* '(*))
(define-lff schematic_window_get_draw_grips int '(*))
(define-lff schematic_window_set_draw_grips void (list '* int))
(define-lff schematic_window_get_gdk_display '* '(*))
(define-lff schematic_window_get_options '* '(*))
(define-lff schematic_window_get_place_list '* '(*))
(define-lff schematic_window_update_keyaccel_string void '(* *))
(define-lff schematic_window_update_keyaccel_timer void (list '* int))

;;; gschem_options.c
(define-lff gschem_options_cycle_grid_mode void '(*))
(define-lff gschem_options_get_grid_mode int '(*))
(define-lff gschem_options_cycle_magnetic_net_mode void '(*))
(define-lff gschem_options_cycle_net_rubber_band_mode void '(*))
(define-lff gschem_options_cycle_snap_mode void '(*))
(define-lff gschem_options_get_snap_mode int '(*))
(define-lff gschem_options_get_snap_size int '(*))
(define-lff gschem_options_set_snap_size void (list '* int))

;;; gschem_text_properties_widget.c
(define-lff text_edit_dialog void '(*))

;;; x_menus.c
(define-lff make_separator_menu_item '* '())
(define-lff make_menu_action '* '(* * * * *))
(define-lff x_menu_attach_recent_files_submenu void '(* *))
(define-lff lepton_action_create_menu_item '* '(* * *))
(define-lff lepton_menu_set_action_data void '(* * * *))
(define-lff schematic_window_create_main_popup_menu '* '(*))

;;; x_rc.c
(define-lff x_rc_parse_gschem void '(*))

;;; x_window.c
(define-lff x_window_new '* '(*))
(define-lff x_window_open_page '* '(* *))
(define-lff x_window_save_page int '(* * *))
(define-lff x_window_set_current_page void '(* *))
(define-lff x_window_setup '* '(*))
(define-lff x_window_setup_draw_events_drawing_area void '(* *))
(define-lff x_window_setup_draw_events_main_wnd void '(* *))
(define-lff x_window_untitled_page int '(*))
(define-lff x_window_close void '(*))
(define-lff x_window_close_all void '(*))
(define-lff x_window_close_page void '(* *))
(define-lff schematic_window_create_app_window '* '(*))
(define-lff schematic_window_create_main_box '* '(*))
(define-lff schematic_window_create_work_box '* '())
(define-lff schematic_window_create_menubar void '(* * *))
(define-lff schematic_toolbar_toggle_tool_button_get_active int '(*))
(define-lff schematic_window_get_inside_action int '(*))
(define-lff schematic_window_set_key_event_callback void '(*))
(define-lff schematic_window_set_page_select_widget void '(* *))
(define-lff schematic_window_create_page_view '* '(* *))
(define-lff schematic_window_create_find_text_widget void '(* *))
(define-lff schematic_window_create_hide_text_widget void '(* *))
(define-lff schematic_window_create_show_text_widget void '(* *))
(define-lff schematic_window_create_macro_widget void '(* *))
(define-lff schematic_window_create_translate_widget void '(* *))
(define-lff schematic_window_create_notebooks void '(* * *))
(define-lff schematic_window_create_statusbar void '(* *))
(define-lff schematic_window_restore_geometry void '(* *))
(define-lff schematic_window_show_all void '(* *))
(define-lff schematic_window_set_main_window '* '(* *))
(define-lff schematic_window_set_toolbar void '(* *))

;;; toolbar.c
(define-lff schematic_toolbar_new '* '(* *))
(define-lff schematic_toolbar_activate_button void '(*))
(define-lff schematic_toolbar_button_new '* '())
(define-lff schematic_toolbar_button_set_icon_widget void '(* *))
(define-lff schematic_toolbar_button_set_label void '(* *))
(define-lff schematic_toolbar_button_set_tooltip_text void '(* *))
(define-lff schematic_toolbar_radio_button_new '* '())
(define-lff schematic_toolbar_insert_button void (list '* '* int))
(define-lff schematic_toolbar_insert_separator void (list '* int))
(define-lff schematic_toolbar_radio_button_get_group '* '(*))
(define-lff schematic_toolbar_radio_button_set_group void '(* *))

;;; x_tabs.c
(define-lff x_tabs_create void '(* *))
(define-lff x_tabs_enabled int '())
(define-lff x_tabs_hdr_update void '(* *))

;;; gschem_find_text_widget.c
(define-lff find_text_dialog void '(*))

;;; gschem_show_hide_text_widget.c
(define-lff hide_text_dialog void '(*))
(define-lff show_text_dialog void '(*))

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
(define-lff i_callback_close_wm int '(* * *))
(define-lff i_callback_edit_autonumber_text void '(* *))
(define-lff i_callback_edit_edit void '(* *))
(define-lff i_callback_edit_embed void '(* *))
(define-lff i_callback_edit_invoke_macro void '(* *))
(define-lff i_callback_edit_mirror void '(* *))
(define-lff i_callback_edit_rotate_90 void '(* *))
(define-lff i_callback_edit_show_hidden void '(* *))
(define-lff i_callback_edit_translate void '(* *))
(define-lff i_callback_edit_unembed void '(* *))
(define-lff i_callback_edit_update void '(* *))
(define-lff i_callback_file_save void '(* *))
(define-lfc *i_callback_file_save)
(define-lff i_callback_file_script void '(* *))
(define-lff i_callback_hierarchy_down_schematic void '(* *))
(define-lff i_callback_hierarchy_down_symbol void '(* *))
(define-lff i_callback_hierarchy_up void '(* *))
(define-lff i_callback_view_color_edit void '(* *))
(define-lff i_callback_view_pan void '(* *))
(define-lff i_callback_view_pan_down void '(* *))
(define-lff i_callback_view_pan_left void '(* *))
(define-lff i_callback_view_pan_right void '(* *))
(define-lff i_callback_view_pan_up void '(* *))
(define-lff i_callback_view_sidebar void '(* *))
(define-lff i_callback_view_status void '(* *))
(define-lff i_callback_view_zoom_box void '(* *))
(define-lff i_callback_view_zoom_extents void '(* *))
(define-lff i_callback_view_zoom_full void '(* *))
(define-lff i_callback_view_zoom_in void '(* *))
(define-lff i_callback_view_zoom_out void '(* *))
(define-lff i_callback_page_next void '(* *))
(define-lff i_callback_page_prev void '(* *))
(define-lff i_callback_page_print void '(* *))
(define-lff i_callback_page_revert void '(* *))
(define-lff i_callback_toolbar_add_bus void '(* *))
(define-lff i_callback_toolbar_add_net void '(* *))

;;; i_basic.c
(define-lff i_action_stop void '(*))
(define-lff i_set_state void (list '* int))
(define-lff i_set_state_msg void (list '* int '*))
(define-lff i_show_state void '(* *))
(define-lff i_update_grid_info void '(*))
(define-lff i_update_menus void '(*))

;;; o_basic.c
(define-lff o_redraw_cleanstates int '(*))
(define-lff o_invalidate_rubber int '(*))

;;; o_copy.c
(define-lff o_copy_start void (list '* int int))

;;; o_delete.c
(define-lff o_delete_selected void '(*))

;;; o_misc.c
(define-lff o_lock void '(*))
(define-lff o_unlock void '(*))

;;; o_move.c
(define-lff o_move_start void (list '* int int))

;;; o_place.c
(define-lff o_place_invalidate_rubber void (list '* int))

;;; x_misc.c
(define-lff x_show_uri int '(* * *))
;;; x_event.c
(define-lff x_event_get_pointer_position int (list '* int '* '*))

;;; x_event.c
(define-lff x_event_key '* '(* * *))

;;; x_fileselect.c
(define-lff x_fileselect_open void '(*))
(define-lff x_fileselect_save int '(* * *))

;;; x_image.c
(define-lff x_image_setup void '(*))

;;; x_print.c
(define-lff x_print void '(*))

;;; o_select.c
(define-lff o_select_return_first_object '* '(*))
(define-lff o_select_unselect_all void '(*))
(define-lff o_select_visible_unlocked void '(*))

;;; o_slot.c
(define-lff o_slot_end void '(* * *))

;;; signals.c
(define-lff schematic_signal_connect void '(* * * *))

;;; x_tabs.c
(define-lff x_tabs_next void '(*))
(define-lff x_tabs_prev void '(*))

;;; o_undo.c
(define-lff o_undo_init void '())
(define-lff o_undo_callback void (list '* '* int))
(define-lff o_undo_savestate void (list '* '* int))

;;; This is a special case: the function may be not defined in C
;;; if libstroke was not found on the configure stage.
(define (x_stroke_init)
  (let ((func (delay (false-if-exception (dynamic-func "x_stroke_init"
                                                       libleptongui)))))
    (and (force func)
         (let ((proc (delay (pointer->procedure void (force func) '()))))
           ((force proc))))))


(define (parse-gschemrc toplevel)
  "Loads old (system, user, etc.) \"gschemrc\" files and new
configuration \".conf\" files in a newly created toplevel
environment.  Saves the values in the foreign LeptonToplevel
structure TOPLEVEL and returns it."
  (x_rc_parse_gschem toplevel)
  toplevel)
