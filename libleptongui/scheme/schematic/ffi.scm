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
  #:use-module (lepton log)

  #:export (lepton_schematic_run
            lepton_schematic_app

            g_init_window

            g_read_file

            generic_confirm_dialog
            generic_error_dialog
            generic_filesel_dialog
            generic_msg_dialog
            major_changed_dialog

            schematic_execute_script

            i_callback_cancel
            i_callback_file_save
            *i_callback_file_save

            i_action_stop
            i_set_state
            i_set_state_msg
            i_show_state
            i_update_grid_info
            *i_update_grid_info_callback
            i_update_menus

            i_vars_set

            snap_grid

            make_menu_action
            make_separator_menu_item
            schematic_window_create_main_popup_menu

            o_attrib_add_attrib

            o_buffer_init
            o_buffer_copy
            o_buffer_cut
            o_buffer_free
            o_buffer_paste_start

            o_redraw_cleanstates
            o_invalidate_rubber

            o_arc_start

            o_box_start

            o_bus_start

            o_circle_start

            o_component_place_changed_run_hook

            o_copy_end
            o_copy_start

            o_delete_selected

            o_edit
            o_edit_show_hidden

            o_grips_end

            o_line_start

            o_mirror_world_update
            o_rotate_world_update

            o_move_end
            o_move_invalidate_rubber
            o_move_start

            o_net_reset
            o_net_start

            o_path_end

            picture_selection_dialog

            o_pin_start

            o_place_invalidate_rubber
            o_place_mirror
            o_place_rotate

            page_select_widget_new
            page_select_widget_update
            schematic_page_select_widget_get_window
            schematic_page_select_widget_set_callback
            pagesel_callback_selection_changed

            s_attrib_free

            s_clib_refresh
            s_clib_free

            set_quiet_mode
            set_verbose_mode
            x_color_init

            color_edit_widget_new
            color_edit_widget_update

            x_colorcb_update_colors

            schematic_menu_recent_chooser_get_filename
            x_menu_attach_recent_files_submenu

            attrib_edit_dialog

            autonumber_text_dialog

            x_clipboard_finish
            x_clipboard_init

            x_show_uri

            x_stroke_init
            x_stroke_free
            x_stroke_translate_and_execute

            gschem_find_text_state_new

            find_text_dialog
            hide_text_dialog
            show_text_dialog

            schematic_hierarchy_get_page_control_counter
            schematic_hierarchy_increment_page_control_counter
            s_hierarchy_find_up_page
            s_hierarchy_down_schematic_single

            schematic_page_revert_dialog

            slot_edit_dialog
            slot_edit_dialog_response
            slot_edit_dialog_get_text
            slot_edit_dialog_quit

            *x_compselect_callback_response
            x_compselect_open
            schematic_compselect_new
            schematic_compselect_get_preview

            x_widgets_destroy_dialogs
            x_widgets_init
            x_widgets_show_color_edit
            x_widgets_show_find_text_state
            x_widgets_show_font_select
            x_widgets_show_log
            x_widgets_show_object_properties
            x_widgets_show_options
            x_widgets_show_page_select
            x_widgets_toggle_widget_visibility

            x_window_close_page
            x_window_new
            x_window_open_page
            x_window_save_page
            *x_window_select_object
            x_window_set_current_page
            x_window_setup_draw_events_drawing_area
            x_window_setup_draw_events_main_wnd
            x_window_setup_scrolling
            x_window_untitled_page
            schematic_window_create_app_window
            schematic_window_create_main_box
            schematic_window_create_work_box
            schematic_window_create_menubar
            schematic_toolbar_toggle_tool_button_get_active
            schematic_window_get_inside_action
            schematic_window_set_page_select_widget
            schematic_window_create_page_view
            schematic_window_create_find_text_widget
            schematic_window_create_hide_text_widget
            schematic_window_create_show_text_widget
            schematic_window_create_macro_widget
            schematic_window_create_translate_widget
            schematic_window_show_translate_widget
            schematic_window_create_notebooks
            schematic_window_create_statusbar
            schematic_window_restore_geometry
            schematic_window_save_geometry
            schematic_window_show_all
            schematic_window_get_main_window
            schematic_window_set_main_window
            schematic_window_get_tab_info_list
            schematic_window_get_tab_notebook
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

            x_tabs_cancel_all
            x_tabs_enabled
            x_tabs_hdr_set
            x_tabs_hdr_update
            x_tabs_info_add
            x_tabs_info_cur
            x_tabs_info_find_by_page
            x_tabs_info_rm
            x_tabs_nbook_create
            x_tabs_nbook_page_add
            x_tabs_nbook_page_close
            x_tabs_page_on_reordered
            *x_tabs_page_on_sel
            x_tabs_tl_page_find
            x_tabs_tl_pview_cur
            x_tabs_tl_pview_cur_set
            schematic_tab_info_get_page
            schematic_tab_info_set_page
            schematic_tab_info_get_page_view
            schematic_tab_info_get_tab_widget
            schematic_tab_info_get_window
            schematic_tabs_add_page_view
            schematic_tabs_set_callback

            schematic_action_mode_from_string
            schematic_action_mode_to_string

            schematic_grid_mode_from_string
            schematic_grid_mode_to_string

            schematic_snap_mode_from_string
            schematic_snap_mode_to_string

            about_dialog

            x_dialog_close_changed_page
            x_dialog_close_window

            coord_dialog

            x_dialog_hotkeys

            macro_widget_show

            schematic_keys_get_event_keyval
            schematic_keys_get_event_modifiers
            schematic_keys_verify_keyval

            lepton_action_create_menu_item
            lepton_menu_set_action_data

            gschem_page_view_get_page
            gschem_page_view_invalidate_all
            gschem_page_view_new_with_page
            gschem_page_view_pan
            gschem_page_view_pan_end
            gschem_page_view_pan_mouse
            gschem_page_view_SCREENtoWORLD
            gschem_page_view_zoom_extents
            schematic_page_view_grab_focus

            *preview_callback_realize
            *preview_callback_button_press
            *preview_event_scroll
            schematic_preview_get_preview_w_current

            schematic_signal_connect

            schematic_window_active_page_changed
            schematic_window_add_timer
            schematic_window_destroy_timer
            gschem_toplevel_free
            gschem_toplevel_get_current_page_view
            gschem_toplevel_get_show_hidden_text
            gschem_toplevel_get_toplevel
            *gschem_toplevel_notify_page_callback
            gschem_toplevel_page_changed
            gschem_toplevel_page_content_changed
            schematic_window_get_actionfeedback_mode
            schematic_window_set_actionfeedback_mode
            schematic_window_get_action_mode
            schematic_window_get_active_page
            schematic_window_get_bottom_notebook
            schematic_window_set_bottom_notebook
            schematic_window_set_color_edit_widget
            schematic_window_get_compselect
            schematic_window_set_compselect
            schematic_window_get_draw_grips
            schematic_window_set_draw_grips
            schematic_window_get_enforce_hierarchy
            schematic_window_get_find_text_state_widget
            schematic_window_set_find_text_state_widget
            schematic_window_set_font_select_widget
            schematic_window_get_gdk_display
            schematic_window_get_keyboardpan_gain
            schematic_window_get_macro_widget
            schematic_window_get_middle_button
            schematic_window_get_options
            schematic_window_get_place_list
            schematic_window_get_right_notebook
            schematic_window_set_right_notebook
            schematic_window_set_rubber_visible
            schematic_window_get_selection_list
            schematic_window_get_undo_panzoom
            schematic_window_get_keyaccel_string
            schematic_window_set_keyaccel_string
            schematic_window_get_keyaccel_string_source_id
            schematic_window_set_keyaccel_string_source_id
            schematic_window_get_shift_key_pressed
            schematic_window_get_arc_edit_widget
            schematic_window_get_attrib_edit_widget
            schematic_window_get_compselect_widget
            schematic_window_get_coord_widget
            schematic_window_get_hotkey_widget
            schematic_window_get_slot_edit_widget
            schematic_window_get_text_input_widget
            schematic_window_set_dont_invalidate
            schematic_window_set_log_widget
            schematic_window_set_object_properties_widget
            schematic_window_set_options_widget
            schematic_window_set_text_properties_widget
            schematic_window_set_alt_key_pressed
            schematic_window_set_control_key_pressed
            schematic_window_set_shift_key_pressed

            font_select_widget_new

            gschem_log_widget_new

            gschem_object_properties_widget_new

            gschem_options_cycle_grid_mode
            gschem_options_get_grid_mode
            gschem_options_cycle_magnetic_net_mode
            gschem_options_cycle_net_rubber_band_mode
            gschem_options_cycle_snap_mode
            gschem_options_get_snap_mode
            gschem_options_set_snap_mode
            gschem_options_get_snap_size
            gschem_options_set_snap_size

            gschem_options_widget_new

            gschem_text_properties_widget_new
            text_edit_dialog

            text_input_dialog

            o_select_box_end
            o_select_end
            o_select_unselect_all

            o_slot_end

            s_slot_update_object

            o_undo_callback
            o_undo_cleanup
            o_undo_init
            o_undo_savestate
            o_undo_savestate_viewport

            s_log_close

            x_event_get_pointer_position
            x_event_key
            x_event_button_pressed
            *x_event_configure
            *x_event_draw
            *x_event_expose
            *x_event_motion
            *x_event_scroll
            schematic_event_get_button
            schematic_event_is_double_button_press
            schematic_event_set_doing_stroke
            schematic_event_alt_mask
            schematic_event_control_mask
            schematic_event_shift_mask


            x_fileselect_open
            x_fileselect_save
            schematic_file_open

            x_image_setup

            x_multiattrib_close

            x_print

            x_tabs_next
            x_tabs_prev

            a_zoom
            a_zoom_box_end
            a_zoom_box_start

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

;;; g_basic.c
(define-lff g_read_file int '(* * *))

;;; g_window.c
(define-lff g_init_window void '(*))

;;; o_attrib.c
(define-lff o_attrib_add_attrib '* (list '* '* int int '* int int int))

;;; page_select_widget.c
(define-lff page_select_widget_update void '(*))
(define-lff page_select_widget_new '* '(*))
(define-lff schematic_page_select_widget_get_window '* '(*))
(define-lff schematic_page_select_widget_set_callback void '(* *))
(define-lff pagesel_callback_selection_changed '* '(* *))

;;; o_buffer.c
(define-lff o_buffer_init void '())
(define-lff o_buffer_copy void (list '* int))
(define-lff o_buffer_cut void (list '* int))
(define-lff o_buffer_free void '(*))
(define-lff o_buffer_paste_start int (list '* int int int))

(define-lff set_quiet_mode void '())
(define-lff set_verbose_mode void '())
(define-lff x_color_init void '())

;;; s_attrib.c
(define-lff s_attrib_free void '())

;;; s_clib.c
(define-lff s_clib_refresh void '())
(define-lff s_clib_free void '())

;;; color_edit_widget.c
(define-lff color_edit_widget_new '* '(*))
(define-lff color_edit_widget_update void '(*))

;;; x_colorcb.c
(define-lff x_colorcb_update_colors void '())

;;; x_compselect.c
(define-lfc *x_compselect_callback_response)
(define-lff x_compselect_open void '(*))
(define-lff schematic_compselect_new '* '(*))
(define-lff schematic_compselect_get_preview '* '(*))

;;; x_widgets.c
(define-lff x_widgets_destroy_dialogs void '(*))
(define-lff x_widgets_init void '())
(define-lff x_widgets_show_color_edit void '(*))
(define-lff x_widgets_show_find_text_state void '(*))
(define-lff x_widgets_show_font_select void '(*))
(define-lff x_widgets_show_log void '(*))
(define-lff x_widgets_show_object_properties void '(*))
(define-lff x_widgets_show_options void '(*))
(define-lff x_widgets_show_page_select void '(*))
(define-lff x_widgets_toggle_widget_visibility void '(*))

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
(define-lff x_dialog_close_window int '(*))

;;; gschem_coord_dialog.c
(define-lff coord_dialog void (list '* int int))

;;; gschem_hotkey_dialog.c
(define-lff x_dialog_hotkeys void '(*))

;;; gschem_macro_widget.c
(define-lff macro_widget_show void '(*))

;;; gschem_page_view.c
(define-lff gschem_page_view_get_page '* '(*))
(define-lff gschem_page_view_invalidate_all void '(*))
(define-lff gschem_page_view_new_with_page '* '(*))
(define-lff gschem_page_view_pan void (list '* int int))
(define-lff gschem_page_view_pan_end int '(*))
(define-lff gschem_page_view_pan_mouse void (list '* int int))
(define-lff gschem_page_view_SCREENtoWORLD void (list '* int int '* '*))
(define-lff gschem_page_view_zoom_extents void '(* *))
(define-lff schematic_page_view_grab_focus void '(*))

;;; gschem_preview.c
(define-lfc *preview_callback_realize)
(define-lfc *preview_callback_button_press)
(define-lfc *preview_event_scroll)
(define-lff schematic_preview_get_preview_w_current '* '(*))

;;; schematic_hierarchy.c
(define-lff schematic_hierarchy_get_page_control_counter int '())
(define-lff schematic_hierarchy_increment_page_control_counter void '())
(define-lff s_hierarchy_find_up_page '* '(*))
(define-lff s_hierarchy_down_schematic_single '* (list '* '* '* int '*))

;;; page_revert_dialog.c
(define-lff schematic_page_revert_dialog int '(* *))

;;; slot_edit_dialog.c
(define-lff slot_edit_dialog '* '(* * *))
(define-lff slot_edit_dialog_response int (list int))
(define-lff slot_edit_dialog_get_text '* '(*))
(define-lff slot_edit_dialog_quit void '(*))

;;; snap_mode.c
(define-lff schematic_snap_mode_from_string int '(*))
(define-lff schematic_snap_mode_to_string '* (list int))

;;; gschem_toplevel.c
(define-lff schematic_window_active_page_changed void '(*))
(define-lff schematic_window_add_timer int (list int '* '*))
(define-lff schematic_window_destroy_timer void (list int))
(define-lff gschem_toplevel_free void '(*))
(define-lff gschem_toplevel_get_current_page_view '* '(*))
(define-lff gschem_toplevel_get_show_hidden_text int '(*))
(define-lff gschem_toplevel_get_toplevel '* '(*))
(define-lfc *gschem_toplevel_notify_page_callback)
(define-lff gschem_toplevel_page_changed void '(*))
(define-lff gschem_toplevel_page_content_changed void '(* *))
(define-lff schematic_window_get_actionfeedback_mode int '(*))
(define-lff schematic_window_set_actionfeedback_mode void (list '* int))
(define-lff schematic_window_get_action_mode int '(*))
(define-lff schematic_window_get_active_page '* '(*))
(define-lff schematic_window_get_bottom_notebook '* '(*))
(define-lff schematic_window_set_bottom_notebook void '(* *))
(define-lff schematic_window_set_color_edit_widget void '(* *))
(define-lff schematic_window_get_compselect '* '(*))
(define-lff schematic_window_set_compselect void '(* *))
(define-lff schematic_window_get_draw_grips int '(*))
(define-lff schematic_window_set_draw_grips void (list '* int))
(define-lff schematic_window_get_enforce_hierarchy int '(*))
(define-lff schematic_window_get_find_text_state_widget '* '(*))
(define-lff schematic_window_set_find_text_state_widget void '(* *))
(define-lff schematic_window_set_font_select_widget void '(* *))
(define-lff schematic_window_get_gdk_display '* '(*))
(define-lff schematic_window_get_keyboardpan_gain int '(*))
(define-lff schematic_window_get_macro_widget '* '(*))
(define-lff schematic_window_get_middle_button int '(*))
(define-lff schematic_window_get_options '* '(*))
(define-lff schematic_window_get_place_list '* '(*))
(define-lff schematic_window_get_right_notebook '* '(*))
(define-lff schematic_window_set_right_notebook void '(* *))
(define-lff schematic_window_set_rubber_visible void (list '* int))
(define-lff schematic_window_get_selection_list '* '(*))
(define-lff schematic_window_get_undo_panzoom int '(*))
(define-lff schematic_window_get_keyaccel_string '* '(*))
(define-lff schematic_window_set_keyaccel_string void '(* *))
(define-lff schematic_window_get_keyaccel_string_source_id int '(*))
(define-lff schematic_window_set_keyaccel_string_source_id void (list '* int))
(define-lff schematic_window_get_shift_key_pressed int '(*))
(define-lff schematic_window_get_arc_edit_widget '* '(*))
(define-lff schematic_window_get_attrib_edit_widget '* '(*))
(define-lff schematic_window_get_compselect_widget '* '(*))
(define-lff schematic_window_get_coord_widget '* '(*))
(define-lff schematic_window_get_hotkey_widget '* '(*))
(define-lff schematic_window_get_slot_edit_widget '* '(*))
(define-lff schematic_window_get_text_input_widget '* '(*))
(define-lff schematic_window_set_dont_invalidate void (list '* int))
(define-lff schematic_window_set_log_widget void '(* *))
(define-lff schematic_window_set_object_properties_widget void '(* *))
(define-lff schematic_window_set_options_widget void '(* *))
(define-lff schematic_window_set_text_properties_widget void '(* *))
(define-lff schematic_window_set_alt_key_pressed void (list '* int))
(define-lff schematic_window_set_control_key_pressed void (list '* int))
(define-lff schematic_window_set_shift_key_pressed void (list '* int))

;;; font_select_widget.c
(define-lff font_select_widget_new '* '(*))

;;; gschem_log_widget.c
(define-lff gschem_log_widget_new '* '())

;;; gschem_object_properties_widget.c
(define-lff gschem_object_properties_widget_new '* '(*))

;;; gschem_options.c
(define-lff gschem_options_cycle_grid_mode void '(*))
(define-lff gschem_options_get_grid_mode int '(*))
(define-lff gschem_options_cycle_magnetic_net_mode void '(*))
(define-lff gschem_options_cycle_net_rubber_band_mode void '(*))
(define-lff gschem_options_cycle_snap_mode void '(*))
(define-lff gschem_options_get_snap_mode int '(*))
(define-lff gschem_options_set_snap_mode void (list '* int))
(define-lff gschem_options_get_snap_size int '(*))
(define-lff gschem_options_set_snap_size void (list '* int))

;;; gschem_options_widget.c
(define-lff gschem_options_widget_new '* '(*))

;;; gschem_text_properties_widget.c
(define-lff gschem_text_properties_widget_new '* '(*))
(define-lff text_edit_dialog void '(*))

;;; x_menus.c
(define-lff make_separator_menu_item '* '())
(define-lff make_menu_action '* '(* * * * *))
(define-lff schematic_menu_recent_chooser_get_filename '* '(* *))
(define-lff x_menu_attach_recent_files_submenu void (list '* '* '* int))
(define-lff lepton_action_create_menu_item '* '(* * *))
(define-lff lepton_menu_set_action_data void '(* * * *))
(define-lff schematic_window_create_main_popup_menu '* '(*))

;;; x_rc.c
(define-lff x_rc_parse_gschem void '(*))

;;; x_window.c
(define-lff x_window_new '* '(*))
(define-lff x_window_open_page '* '(* *))
(define-lff x_window_save_page int '(* * *))
(define-lfc *x_window_select_object)
(define-lff x_window_set_current_page void '(* *))
(define-lff x_window_setup_draw_events_drawing_area void '(* *))
(define-lff x_window_setup_draw_events_main_wnd void '(* *))
(define-lff x_window_setup_scrolling void '(* *))
(define-lff x_window_untitled_page int '(*))
(define-lff x_window_close_page '* '(* *))
(define-lff schematic_window_create_app_window '* '(*))
(define-lff schematic_window_create_main_box '* '(*))
(define-lff schematic_window_create_work_box '* '())
(define-lff schematic_window_create_menubar void '(* * *))
(define-lff schematic_toolbar_toggle_tool_button_get_active int '(*))
(define-lff schematic_window_get_inside_action int '(*))
(define-lff schematic_window_set_page_select_widget void '(* *))
(define-lff schematic_window_create_page_view '* '(* *))
(define-lff schematic_window_create_find_text_widget void '(* *))
(define-lff schematic_window_create_hide_text_widget void '(* *))
(define-lff schematic_window_create_show_text_widget void '(* *))
(define-lff schematic_window_create_macro_widget void '(* *))
(define-lff schematic_window_create_translate_widget void '(* *))
(define-lff schematic_window_show_translate_widget void '(*))
(define-lff schematic_window_create_notebooks void '(* * *))
(define-lff schematic_window_create_statusbar void '(* *))
(define-lff schematic_window_restore_geometry void '(* *))
(define-lff schematic_window_save_geometry void '(*))
(define-lff schematic_window_show_all void '(* *))
(define-lff schematic_window_get_main_window '* '(*))
(define-lff schematic_window_set_main_window '* '(* *))
(define-lff schematic_window_get_tab_info_list '* '(*))
(define-lff schematic_window_get_tab_notebook '* '(*))
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
(define-lff x_tabs_cancel_all void '(*))
(define-lff x_tabs_enabled int '())
(define-lff x_tabs_hdr_set void '(* *))
(define-lff x_tabs_hdr_update void '(* *))
(define-lff x_tabs_info_add '* (list '* int '* '* '*))
(define-lff x_tabs_info_cur '* '(*))
(define-lff x_tabs_info_find_by_page '* '(* *))
(define-lff x_tabs_info_rm void '(* *))
(define-lff x_tabs_nbook_create '* '(* *))
(define-lff x_tabs_nbook_page_add int '(* * * *))
(define-lff x_tabs_nbook_page_close void '(* *))
(define-lff x_tabs_page_on_reordered void (list '* '* int '*))
(define-lfc *x_tabs_page_on_sel)
(define-lff schematic_tabs_add_page_view void '(* *))
(define-lff x_tabs_tl_page_find int '(* *))
(define-lff x_tabs_tl_pview_cur '* '(*))
(define-lff x_tabs_tl_pview_cur_set void '(* *))
(define-lff schematic_tab_info_get_page '* '(*))
(define-lff schematic_tab_info_set_page void '(* *))
(define-lff schematic_tab_info_get_page_view '* '(*))
(define-lff schematic_tab_info_get_tab_widget '* '(*))
(define-lff schematic_tab_info_get_window '* '(*))
(define-lff schematic_tabs_set_callback void '(* *))

;;; gschem_find_text_state.c
(define-lff gschem_find_text_state_new '* '())

;;; gschem_find_text_widget.c
(define-lff find_text_dialog void '(*))

;;; gschem_show_hide_text_widget.c
(define-lff hide_text_dialog void '(*))
(define-lff show_text_dialog void '(*))

;;; x_dialog.c
(define-lff generic_confirm_dialog int '(*))
(define-lff generic_error_dialog void '(* * *))
(define-lff generic_filesel_dialog '* (list '* '* int))
(define-lff generic_msg_dialog void '(*))
(define-lff major_changed_dialog void '(*))

;;; execute_script.c
(define-lff schematic_execute_script '* '(*))

;;; i_callbacks.c
(define-lff i_callback_cancel void '(* *))
(define-lff i_callback_file_save void '(* *))
(define-lfc *i_callback_file_save)

;;; i_basic.c
(define-lff i_action_stop void '(*))
(define-lff i_set_state void (list '* int))
(define-lff i_set_state_msg void (list '* int '*))
(define-lff i_show_state void '(* *))
(define-lff i_update_grid_info void '(*))
(define-lfc *i_update_grid_info_callback)
(define-lff i_update_menus void '(*))

;;; i_vars.c
(define-lff i_vars_set void '(*))

;;; m_basic.c
(define-lff snap_grid int (list '* int))

;;; o_basic.c
(define-lff o_redraw_cleanstates int '(*))
(define-lff o_invalidate_rubber int '(*))

;;; o_arc.c
(define-lff o_arc_start void (list '* int int))

;;; o_box.c
(define-lff o_box_start void (list '* int int))

;;; o_bus.c
(define-lff o_bus_start void (list '* int int))

;;; o_circle.c
(define-lff o_circle_start void (list '* int int))

;;; o_component.c
(define-lff o_component_place_changed_run_hook void '(*))

;;; o_copy.c
(define-lff o_copy_end void '(*))
(define-lff o_copy_start void (list '* int int))

;;; o_delete.c
(define-lff o_delete_selected void '(*))

;;; o_grips.c
(define-lff o_grips_end void '(*))

;;; o_line.c
(define-lff o_line_start void (list '* int int))

;;; o_misc.c
(define-lff o_edit void '(* *))
(define-lff o_edit_show_hidden void '(* *))
(define-lff o_mirror_world_update void (list '* int int '*))
(define-lff o_rotate_world_update void (list '* int int int '*))

;;; o_move.c
(define-lff o_move_end void '(*))
(define-lff o_move_invalidate_rubber void (list '* int))
(define-lff o_move_start void (list '* int int))

;;; o_net.c
(define-lff o_net_reset void '(*))
(define-lff o_net_start void (list '* int int))

;;; o_path.c
(define-lff o_path_end void (list '* int int))

;;; o_picture.c
(define-lff picture_selection_dialog void '(*))

;;; o_pin.c
(define-lff o_pin_start void (list '* int int))

;;; o_place.c
(define-lff o_place_invalidate_rubber void (list '* int))
(define-lff o_place_mirror void '(*))
(define-lff o_place_rotate void '(*))

;;; x_attribedit.c
(define-lff attrib_edit_dialog void (list '* '* int))

;;; x_autonumber.c
(define-lff autonumber_text_dialog void '(*))

;;; x_clipboard.c
(define-lff x_clipboard_finish void '(*))
(define-lff x_clipboard_init void '(*))

;;; x_misc.c
(define-lff x_show_uri int '(* * *))

;;; x_event.c
(define-lff x_event_get_pointer_position int (list '* int '* '*))
(define-lff x_event_key '* '(* * *))
(define-lff x_event_button_pressed int (list '* '* '* int '* int uint32 double double int int int int))
(define-lfc *x_event_configure)
(define-lfc *x_event_draw)
(define-lfc *x_event_expose)
(define-lfc *x_event_motion)
(define-lfc *x_event_scroll)
(define-lff schematic_event_get_button int '(*))
(define-lff schematic_event_is_double_button_press int '(*))
(define-lff schematic_event_set_doing_stroke void (list int))
(define-lff schematic_event_alt_mask int '())
(define-lff schematic_event_control_mask int '())
(define-lff schematic_event_shift_mask int '())

;;; x_fileselect.c
(define-lff x_fileselect_open '* '(*))
(define-lff x_fileselect_save int '(* * *))
(define-lff schematic_file_open int '(* * * *))

;;; x_image.c
(define-lff x_image_setup void '(*))

;;; x_multiattrib.c
(define-lff x_multiattrib_close void '(*))

;;; x_newtext.c
(define-lff text_input_dialog void '(*))

;;; x_print.c
(define-lff x_print void '(*))

;;; o_select.c
(define-lff o_select_box_end void (list '* int int))
(define-lff o_select_end void (list '* int int))
(define-lff o_select_unselect_all void '(*))

;;; o_slot.c
(define-lff o_slot_end void '(* * *))

;;; s_slot.c
(define-lff s_slot_update_object void '(*))

;;; signals.c
(define-lff schematic_signal_connect void '(* * * *))

;;; x_tabs.c
(define-lff x_tabs_next void '(*))
(define-lff x_tabs_prev void '(*))

;;; o_undo.c
(define-lff o_undo_init void '())
(define-lff o_undo_callback void (list '* '* int))
(define-lff o_undo_cleanup void '())
(define-lff o_undo_savestate void (list '* '* int))
(define-lff o_undo_savestate_viewport void '(*))

;;; s_log.c
(define-lff s_log_close void '())

;;; a_zoom.c
(define-lff a_zoom void (list '* '* int int))
(define-lff a_zoom_box_end void (list '* int int))
(define-lff a_zoom_box_start void (list '* int int))

;;; This is a special case: the function may be not defined in C
;;; if libstroke was not found on the configure stage.
(define (x_stroke_init)
  (let ((func (delay (false-if-exception (dynamic-func "x_stroke_init"
                                                       libleptongui)))))
    (and (force func)
         (let ((proc (delay (pointer->procedure void (force func) '()))))
           ((force proc))))))

;;; The same as above.
(define (x_stroke_free)
  (let ((func (delay (false-if-exception (dynamic-func "x_stroke_free"
                                                       libleptongui)))))
    (and (force func)
         (let ((proc (delay (pointer->procedure void (force func) '()))))
           ((force proc))))))

(define (x_stroke_translate_and_execute *window)
  (let ((func (delay (false-if-exception (dynamic-func "x_stroke_translate_and_execute"
                                                       libleptongui)))))
    (and (force func)
         (let ((proc (delay (pointer->procedure int (force func) '(*)))))
           ((force proc) *window)))))


(define (parse-gschemrc toplevel)
  "Loads old (system, user, etc.) \"gschemrc\" files and new
configuration \".conf\" files in a newly created toplevel
environment.  Saves the values in the foreign LeptonToplevel
structure TOPLEVEL and returns it."
  (x_rc_parse_gschem toplevel)
  toplevel)
