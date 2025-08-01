;;; Lepton EDA Schematic Capture
;;; Scheme API
;;; Copyright (C) 2020-2025 Lepton EDA Contributors
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

            schematic_alignment_combo_get_align

            schematic_integer_combo_box_get_value

            schematic_rotation_combo_get_angle

            g_init_window

            g_read_file

            gtk_response_to_string
            gtk_string_to_response
            gtk_widget_get_gtk_window

            generic_confirm_dialog
            generic_error_dialog
            generic_filesel_dialog
            generic_msg_dialog
            major_changed_dialog

            schematic_execute_script

            i_callback_cancel
            i_callback_file_save
            *i_callback_file_save

            i_action_start
            i_action_stop
            i_set_state
            i_set_state_msg
            i_show_state
            i_update_grid_info
            *i_update_grid_info_callback
            i_update_menus

            i_vars_set

            snap_grid

            do_popup
            make_menu_action
            make_separator_menu_item
            schematic_window_create_main_popup_menu

            o_attrib_add_attrib

            o_redraw_cleanstates
            o_invalidate_rubber

            o_arc_end1
            o_arc_invalidate_rubber
            o_arc_motion
            o_arc_start

            o_box_end
            o_box_invalidate_rubber
            o_box_motion
            o_box_start

            o_bus_end
            o_bus_motion
            o_bus_reset
            o_bus_start

            o_circle_end
            o_circle_motion
            o_circle_invalidate_rubber
            o_circle_start

            o_component_place_changed_run_hook

            schematic_delete_dialog

            o_edit_show_hidden

            o_find_object

            o_grips_end
            o_grips_motion

            o_line_end
            o_line_invalidate_rubber
            o_line_motion
            o_line_start

            o_mirror_world_update
            o_rotate_world_update

            o_move_cancel
            o_move_end
            o_move_invalidate_rubber
            o_move_motion
            o_move_start

            o_net_end
            o_net_motion
            o_net_reset
            o_net_start
            o_net_start_magnetic

            o_path_continue
            o_path_end
            o_path_invalidate_rubber
            o_path_motion
            o_path_start

            picture_change_filename_dialog
            o_picture_end
            o_picture_invalidate_rubber
            o_picture_motion
            o_picture_start
            picture_selection_dialog

            o_pin_end
            o_pin_invalidate_rubber
            o_pin_motion
            o_pin_start

            o_place_end
            o_place_invalidate_rubber
            o_place_mirror
            o_place_motion
            o_place_rotate
            o_place_start

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

            x_colorcb_get_index
            x_colorcb_update_colors

            schematic_menu_recent_chooser_get_filename
            x_menu_attach_recent_files_submenu

            attrib_edit_dialog

            schematic_autonumber_new
            schematic_autonumber_get_autotext
            schematic_autonumber_set_autotext
            schematic_autonumber_get_autotext_dialog
            schematic_autonumber_set_autotext_dialog
            schematic_autonumber_get_autotext_removenum
            schematic_autonumber_get_autotext_scope_overwrite
            schematic_autonumber_set_autotext_scope_overwrite
            schematic_autonumber_set_autotext_window
            schematic_autonumber_dialog_lookup_widget
            schematic_autonumber_dialog_new
            schematic_autonumber_dialog_restore_state
            schematic_autonumber_dialog_save_state
            schematic_autonumber_run
            schematic_autonumber_sort_order_widget_init

            x_clipboard_finish
            x_clipboard_init
            x_clipboard_get
            x_clipboard_set

            x_show_uri

            x_stroke_init
            x_stroke_free
            x_stroke_record
            x_stroke_translate_and_execute

            schematic_find_text_state_new

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
            schematic_window_create_canvas
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
            schematic_tab_info_get_canvas
            schematic_tab_info_get_tab_widget
            schematic_tab_info_get_window
            schematic_tabs_add_canvas
            schematic_tabs_set_callback

            schematic_action_mode_from_string
            schematic_action_mode_to_string

            schematic_grid_mode_from_string
            schematic_grid_mode_to_string

            schematic_snap_mode_from_string
            schematic_snap_mode_to_string

            about_dialog

            arc_angle_dialog

            x_dialog_close_changed_page
            x_dialog_close_window

            coord_dialog
            coord_display_update

            x_dialog_hotkeys

            macro_widget_show

            schematic_keys_get_event_keyval
            schematic_keys_get_event_modifiers
            schematic_keys_verify_keyval

            lepton_action_create_menu_item
            lepton_menu_set_action_data

            schematic_canvas_get_page
            schematic_canvas_get_viewport
            schematic_canvas_get_hadjustment
            schematic_canvas_get_vadjustment
            schematic_canvas_invalidate_all
            schematic_canvas_invalidate_world_rect
            schematic_canvas_new_with_page
            schematic_canvas_pan
            schematic_canvas_pan_end
            schematic_canvas_pan_general
            schematic_canvas_pan_mouse
            schematic_canvas_pan_motion
            schematic_canvas_pan_start
            schematic_canvas_redraw
            schematic_canvas_SCREENtoWORLD
            schematic_canvas_zoom_extents
            schematic_canvas_grab_focus

            schematic_preview_new
            *schematic_preview_callback_realize
            *schematic_preview_callback_button_press
            schematic_preview_get_active
            schematic_preview_get_buffer
            schematic_preview_get_filename
            schematic_preview_get_window

            schematic_signal_connect

            schematic_window_active_page_changed
            schematic_window_add_timer
            schematic_window_destroy_timer
            schematic_window_free
            schematic_window_get_current_canvas
            schematic_window_get_show_hidden_text
            schematic_window_get_toplevel
            *schematic_window_notify_page_callback
            schematic_window_page_changed
            schematic_window_page_content_changed
            schematic_window_get_actionfeedback_mode
            schematic_window_set_actionfeedback_mode
            schematic_window_get_action_mode
            schematic_window_get_active_page
            schematic_window_get_bottom_notebook
            schematic_window_set_bottom_notebook
            schematic_window_set_color_edit_widget
            schematic_window_get_draw_grips
            schematic_window_set_draw_grips
            schematic_window_get_enforce_hierarchy
            schematic_window_get_file_preview
            schematic_window_set_file_preview
            schematic_window_get_first_wx
            schematic_window_set_first_wx
            schematic_window_get_first_wy
            schematic_window_set_first_wy
            schematic_window_get_second_wx
            schematic_window_set_second_wx
            schematic_window_get_second_wy
            schematic_window_set_second_wy
            schematic_window_get_find_text_state_widget
            schematic_window_set_find_text_state_widget
            schematic_window_set_font_select_widget
            schematic_window_get_gdk_display
            schematic_window_get_keyboardpan_gain
            schematic_window_get_macro_widget
            schematic_window_get_middle_button
            schematic_window_get_mousepan_gain
            schematic_window_set_multiattrib_widget
            schematic_window_get_options
            schematic_window_delete_place_list
            schematic_window_get_place_list
            schematic_window_set_place_list
            schematic_window_get_right_notebook
            schematic_window_set_right_notebook
            schematic_window_get_rubber_visible
            schematic_window_set_rubber_visible
            schematic_window_get_selection_list
            schematic_window_get_third_button
            schematic_window_get_third_button_cancel
            schematic_window_get_undo_panzoom
            schematic_window_get_keyaccel_string
            schematic_window_set_keyaccel_string
            schematic_window_get_keyaccel_string_source_id
            schematic_window_set_keyaccel_string_source_id
            schematic_window_get_arc_edit_widget
            schematic_window_get_attrib_edit_widget
            schematic_window_get_compselect_widget
            schematic_window_set_compselect_widget
            schematic_window_get_coord_widget
            schematic_window_get_hotkey_widget
            schematic_window_get_slot_edit_widget
            schematic_window_get_newtext_dialog
            schematic_window_set_newtext_dialog
            schematic_window_set_dont_invalidate
            schematic_window_set_log_widget
            schematic_window_get_multiattrib_widget
            schematic_window_set_object_properties_widget
            schematic_window_set_options_widget
            schematic_window_set_text_properties_widget
            schematic_window_get_alt_key_pressed
            schematic_window_set_alt_key_pressed
            schematic_window_get_control_key_pressed
            schematic_window_set_control_key_pressed
            schematic_window_get_shift_key_pressed
            schematic_window_set_shift_key_pressed
            schematic_window_get_scroll_wheel
            schematic_window_get_scrollbars_flag
            schematic_window_set_scrollbars_flag
            schematic_window_get_scrollpan_steps
            schematic_window_set_scrollpan_steps
            schematic_window_get_text_caps
            schematic_window_text_caps_to_string
            schematic_window_get_text_size

            font_select_widget_new

            schematic_log_widget_new

            schematic_object_properties_widget_new

            schematic_options_cycle_grid_mode
            schematic_options_get_grid_mode
            schematic_options_cycle_magnetic_net_mode
            schematic_options_cycle_net_rubber_band_mode
            schematic_options_cycle_snap_mode
            schematic_options_get_snap_mode
            schematic_options_set_snap_mode
            schematic_options_get_snap_size
            schematic_options_set_snap_size

            schematic_options_widget_new

            schematic_viewport_get_bottom
            schematic_viewport_get_left
            schematic_viewport_get_right
            schematic_viewport_get_top
            schematic_viewport_pan
            schematic_viewport_set_world_bottom
            schematic_viewport_set_world_left
            schematic_viewport_set_world_right
            schematic_viewport_set_world_top

            schematic_text_properties_widget_new
            text_edit_dialog

            schematic_newtext_dialog_get_aligncb
            schematic_newtext_dialog_get_colorcb
            schematic_newtext_dialog_get_rotatecb
            schematic_newtext_dialog_get_window
            schematic_newtext_dialog_new
            schematic_newtext_dialog_run
            schematic_newtext_dialog_get_text
            schematic_newtext_dialog_get_text_view
            schematic_newtext_dialog_get_textsizecb
            schematic_newtext_dialog_textview_select_all

            o_select_box_end
            o_select_box_motion
            o_select_end
            o_select_motion
            o_select_selected
            o_select_start
            o_select_unselect_all

            o_slot_end

            lepton_slot_update_object

            o_undo_savestate
            o_undo_savestate_old
            o_undo_savestate_viewport
            schematic_undo_get_file_index
            schematic_undo_index_to_filename
            schematic_undo_set_tmp_path

            lepton_log_get_logging_enabled
            lepton_log_set_logging_enabled
            s_log_close

            gdk_event_scroll_direction_from_string
            gdk_event_scroll_direction_to_string
            x_event_faked_motion
            x_event_get_pointer_position
            x_event_key
            *x_event_configure
            schematic_event_get_button
            schematic_event_is_double_button_press
            schematic_event_get_doing_stroke
            schematic_event_set_doing_stroke
            schematic_event_get_scroll_direction
            schematic_event_skip_motion_event
            schematic_event_alt_mask
            schematic_event_control_mask
            schematic_event_shift_mask

            schematic_file_select_dialog_new
            x_fileselect_add_preview
            x_fileselect_open
            x_fileselect_save
            *x_fileselect_callback_update_preview
            schematic_file_open

            x_image_setup

            schematic_multiattrib_widget_new
            schematic_multiattrib_widget_update

            x_print

            x_tabs_next
            x_tabs_prev

            a_zoom

            g_action_eval_by_name

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

;;; alignment_combo.c
(define-lff schematic_alignment_combo_get_align int '(*))

;;; integer_combo_box.c
(define-lff schematic_integer_combo_box_get_value int '(*))

;;; rotation_combo.c
(define-lff schematic_rotation_combo_get_angle int '(*))

;;; g_basic.c
(define-lff g_read_file int '(* * *))

;;; g_window.c
(define-lff g_init_window void '(*))

;;; gtk_helper.c
(define-lff gtk_response_to_string '* (list int))
(define-lff gtk_string_to_response int '(*))
(define-lff gtk_widget_get_gtk_window '* '(*))

;;; o_attrib.c
(define-lff o_attrib_add_attrib '* (list '* '* int int '* int int int))

;;; page_select_widget.c
(define-lff page_select_widget_update void '(*))
(define-lff page_select_widget_new '* '(*))
(define-lff schematic_page_select_widget_get_window '* '(*))
(define-lff schematic_page_select_widget_set_callback void '(* *))
(define-lff pagesel_callback_selection_changed '* '(* *))

;;; lepton-schematic.c
(define-lff set_quiet_mode void '())
(define-lff set_verbose_mode void '())

;;; x_color.c
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
(define-lff x_colorcb_get_index int '(*))
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

;;; about_dialog.c
(define-lff about_dialog void '(*))

;;; arc_dialog.c
(define-lff arc_angle_dialog void '(* *))

;;; close_confirmation_dialog.c
(define-lff x_dialog_close_changed_page int '(* *))
(define-lff x_dialog_close_window int '(*))

;;; coord_dialog.c
(define-lff coord_dialog void (list '* int int))
(define-lff coord_display_update void (list '* int int))

;;; hotkey_dialog.c
(define-lff x_dialog_hotkeys void '(*))

;;; macro_widget.c
(define-lff macro_widget_show void '(*))

;;; canvas.c
(define-lff schematic_canvas_get_page '* '(*))
(define-lff schematic_canvas_get_viewport '* '(*))
(define-lff schematic_canvas_get_hadjustment '* '(*))
(define-lff schematic_canvas_get_vadjustment '* '(*))
(define-lff schematic_canvas_invalidate_all void '(*))
(define-lff schematic_canvas_invalidate_world_rect void (list '* int int int int))
(define-lff schematic_canvas_new_with_page '* '(*))
(define-lff schematic_canvas_pan void (list '* int int))
(define-lff schematic_canvas_pan_end int '(*))
(define-lff schematic_canvas_pan_general void (list '* int int double))
(define-lff schematic_canvas_pan_mouse void (list '* int int))
(define-lff schematic_canvas_pan_motion void (list '* int int int))
(define-lff schematic_canvas_pan_start void (list '* int int))
(define-lff schematic_canvas_redraw void '(* * *))
(define-lff schematic_canvas_SCREENtoWORLD void (list '* int int '* '*))
(define-lff schematic_canvas_zoom_extents void '(* *))
(define-lff schematic_canvas_grab_focus void '(*))

;;; preview_widget.c
(define-lff schematic_preview_new '* '())
(define-lfc *schematic_preview_callback_realize)
(define-lfc *schematic_preview_callback_button_press)
(define-lff schematic_preview_get_active int '(*))
(define-lff schematic_preview_get_buffer '* '(*))
(define-lff schematic_preview_get_filename '* '(*))
(define-lff schematic_preview_get_window '* '(*))

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

;;; window.c
(define-lff schematic_window_active_page_changed void '(*))
(define-lff schematic_window_add_timer int (list int '* '*))
(define-lff schematic_window_destroy_timer void (list int))
(define-lff schematic_window_free void '(*))
(define-lff schematic_window_get_current_canvas '* '(*))
(define-lff schematic_window_get_show_hidden_text int '(*))
(define-lff schematic_window_get_toplevel '* '(*))
(define-lfc *schematic_window_notify_page_callback)
(define-lff schematic_window_page_changed void '(*))
(define-lff schematic_window_page_content_changed void '(* *))
(define-lff schematic_window_get_actionfeedback_mode int '(*))
(define-lff schematic_window_set_actionfeedback_mode void (list '* int))
(define-lff schematic_window_get_action_mode int '(*))
(define-lff schematic_window_get_active_page '* '(*))
(define-lff schematic_window_get_bottom_notebook '* '(*))
(define-lff schematic_window_set_bottom_notebook void '(* *))
(define-lff schematic_window_set_color_edit_widget void '(* *))
(define-lff schematic_window_get_draw_grips int '(*))
(define-lff schematic_window_set_draw_grips void (list '* int))
(define-lff schematic_window_get_enforce_hierarchy int '(*))
(define-lff schematic_window_get_file_preview int '(*))
(define-lff schematic_window_set_file_preview void (list int '*))
(define-lff schematic_window_get_first_wx int '(*))
(define-lff schematic_window_set_first_wx void (list '* int))
(define-lff schematic_window_get_first_wy int '(*))
(define-lff schematic_window_set_first_wy void (list '* int))
(define-lff schematic_window_get_second_wx int '(*))
(define-lff schematic_window_set_second_wx void (list '* int))
(define-lff schematic_window_get_second_wy int '(*))
(define-lff schematic_window_set_second_wy void (list '* int))
(define-lff schematic_window_get_find_text_state_widget '* '(*))
(define-lff schematic_window_set_find_text_state_widget void '(* *))
(define-lff schematic_window_set_font_select_widget void '(* *))
(define-lff schematic_window_get_gdk_display '* '(*))
(define-lff schematic_window_get_keyboardpan_gain int '(*))
(define-lff schematic_window_get_macro_widget '* '(*))
(define-lff schematic_window_get_middle_button int '(*))
(define-lff schematic_window_get_mousepan_gain int '(*))
(define-lff schematic_window_set_multiattrib_widget void '(* *))
(define-lff schematic_window_get_options '* '(*))
(define-lff schematic_window_delete_place_list void '(*))
(define-lff schematic_window_get_place_list '* '(*))
(define-lff schematic_window_set_place_list void '(* *))
(define-lff schematic_window_get_right_notebook '* '(*))
(define-lff schematic_window_set_right_notebook void '(* *))
(define-lff schematic_window_get_rubber_visible int '(*))
(define-lff schematic_window_set_rubber_visible void (list '* int))
(define-lff schematic_window_get_selection_list '* '(*))
(define-lff schematic_window_get_third_button int '(*))
(define-lff schematic_window_get_third_button_cancel int '(*))
(define-lff schematic_window_get_undo_panzoom int '(*))
(define-lff schematic_window_get_keyaccel_string '* '(*))
(define-lff schematic_window_set_keyaccel_string void '(* *))
(define-lff schematic_window_get_keyaccel_string_source_id int '(*))
(define-lff schematic_window_set_keyaccel_string_source_id void (list '* int))
(define-lff schematic_window_get_arc_edit_widget '* '(*))
(define-lff schematic_window_get_attrib_edit_widget '* '(*))
(define-lff schematic_window_get_compselect_widget '* '(*))
(define-lff schematic_window_set_compselect_widget void '(* *))
(define-lff schematic_window_get_coord_widget '* '(*))
(define-lff schematic_window_get_hotkey_widget '* '(*))
(define-lff schematic_window_get_slot_edit_widget '* '(*))
(define-lff schematic_window_get_newtext_dialog '* '(*))
(define-lff schematic_window_set_newtext_dialog void '(* *))
(define-lff schematic_window_set_dont_invalidate void (list '* int))
(define-lff schematic_window_set_log_widget void '(* *))
(define-lff schematic_window_get_multiattrib_widget '* '(*))
(define-lff schematic_window_set_object_properties_widget void '(* *))
(define-lff schematic_window_set_options_widget void '(* *))
(define-lff schematic_window_set_text_properties_widget void '(* *))
(define-lff schematic_window_get_alt_key_pressed int '(*))
(define-lff schematic_window_set_alt_key_pressed void (list '* int))
(define-lff schematic_window_get_control_key_pressed int '(*))
(define-lff schematic_window_set_control_key_pressed void (list '* int))
(define-lff schematic_window_get_shift_key_pressed int '(*))
(define-lff schematic_window_set_shift_key_pressed void (list '* int))
(define-lff schematic_window_get_scroll_wheel int '(*))
(define-lff schematic_window_get_scrollbars_flag int '(*))
(define-lff schematic_window_set_scrollbars_flag void (list '* int))
(define-lff schematic_window_get_scrollpan_steps int '(*))
(define-lff schematic_window_set_scrollpan_steps void (list '* int))
(define-lff schematic_window_get_text_caps int '(*))
(define-lff schematic_window_text_caps_to_string '* (list int))
(define-lff schematic_window_get_text_size int '(*))

;;; font_select_widget.c
(define-lff font_select_widget_new '* '(*))

;;; log_widget.c
(define-lff schematic_log_widget_new '* '())

;;; object_properties_widget.c
(define-lff schematic_object_properties_widget_new '* '(*))

;;; options.c
(define-lff schematic_options_cycle_grid_mode void '(*))
(define-lff schematic_options_get_grid_mode int '(*))
(define-lff schematic_options_cycle_magnetic_net_mode void '(*))
(define-lff schematic_options_cycle_net_rubber_band_mode void '(*))
(define-lff schematic_options_cycle_snap_mode void '(*))
(define-lff schematic_options_get_snap_mode int '(*))
(define-lff schematic_options_set_snap_mode void (list '* int))
(define-lff schematic_options_get_snap_size int '(*))
(define-lff schematic_options_set_snap_size void (list '* int))

;;; options_widget.c
(define-lff schematic_options_widget_new '* '(*))

;;; viewport.c
(define-lff schematic_viewport_get_bottom int '(*))
(define-lff schematic_viewport_get_left int '(*))
(define-lff schematic_viewport_get_right int '(*))
(define-lff schematic_viewport_get_top int '(*))
(define-lff schematic_viewport_pan void (list '* int int double))
(define-lff schematic_viewport_set_world_bottom void (list '* int))
(define-lff schematic_viewport_set_world_left void (list '* int))
(define-lff schematic_viewport_set_world_right void (list '* int))
(define-lff schematic_viewport_set_world_top void (list '* int))

;;; text_properties_widget.c
(define-lff schematic_text_properties_widget_new '* '(*))
(define-lff text_edit_dialog void '(*))

;;; x_menus.c
(define-lff do_popup int '(* *))
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
(define-lff schematic_window_create_canvas '* '(* *))
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
(define-lff schematic_tabs_add_canvas void '(* *))
(define-lff x_tabs_tl_page_find int '(* *))
(define-lff x_tabs_tl_pview_cur '* '(*))
(define-lff x_tabs_tl_pview_cur_set void '(* *))
(define-lff schematic_tab_info_get_page '* '(*))
(define-lff schematic_tab_info_set_page void '(* *))
(define-lff schematic_tab_info_get_canvas '* '(*))
(define-lff schematic_tab_info_get_tab_widget '* '(*))
(define-lff schematic_tab_info_get_window '* '(*))
(define-lff schematic_tabs_set_callback void '(* *))

;;; find_text_state.c
(define-lff schematic_find_text_state_new '* '())

;;; find_text_widget.c
(define-lff find_text_dialog void '(*))

;;; show_hide_text_widget.c
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
(define-lff i_action_start void '(*))
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
(define-lff o_arc_end1 void (list '* int int))
(define-lff o_arc_invalidate_rubber void '(*))
(define-lff o_arc_motion void (list '* int int int))
(define-lff o_arc_start void (list '* int int))

;;; o_box.c
(define-lff o_box_end void (list '* int int))
(define-lff o_box_invalidate_rubber void '(*))
(define-lff o_box_motion void (list '* int int))
(define-lff o_box_start void (list '* int int))

;;; o_bus.c
(define-lff o_bus_end void (list '* int int))
(define-lff o_bus_motion void (list '* int int))
(define-lff o_bus_reset void '(*))
(define-lff o_bus_start void (list '* int int))

;;; o_circle.c
(define-lff o_circle_end void (list '* int int))
(define-lff o_circle_invalidate_rubber void '(*))
(define-lff o_circle_motion void (list '* int int))
(define-lff o_circle_start void (list '* int int))

;;; o_component.c
(define-lff o_component_place_changed_run_hook void '(*))

;;; delete_dialog.c
(define-lff schematic_delete_dialog int '())

;;; o_find.c
(define-lff o_find_object int (list '* int int int))

;;; o_grips.c
(define-lff o_grips_end void '(*))
(define-lff o_grips_motion void (list '* int int))

;;; o_line.c
(define-lff o_line_end void (list '* int int))
(define-lff o_line_invalidate_rubber void '(*))
(define-lff o_line_motion void (list '* int int))
(define-lff o_line_start void (list '* int int))

;;; o_misc.c
(define-lff o_edit_show_hidden void '(* *))
(define-lff o_mirror_world_update void (list '* int int '*))
(define-lff o_rotate_world_update void (list '* int int int '*))

;;; o_move.c
(define-lff o_move_cancel void '(*))
(define-lff o_move_end void '(*))
(define-lff o_move_invalidate_rubber void (list '* int))
(define-lff o_move_motion void (list '* int int))
(define-lff o_move_start void (list '* int int))

;;; o_net.c
(define-lff o_net_end void (list '* int int))
(define-lff o_net_motion void (list '* int int))
(define-lff o_net_reset void '(*))
(define-lff o_net_start void (list '* int int))
(define-lff o_net_start_magnetic void (list '* int int))

;;; o_path.c
(define-lff o_path_continue void (list '* int int))
(define-lff o_path_end void (list '* int int))
(define-lff o_path_invalidate_rubber void '(*))
(define-lff o_path_motion void (list '* int int))
(define-lff o_path_start void (list '* int int))

;;; o_picture.c
(define-lff picture_change_filename_dialog void '(*))
(define-lff o_picture_end void (list '* int int))
(define-lff o_picture_invalidate_rubber void '(*))
(define-lff o_picture_motion void (list '* int int))
(define-lff o_picture_start void (list '* int int))
(define-lff picture_selection_dialog void '(*))

;;; o_pin.c
(define-lff o_pin_end void (list '* int int))
(define-lff o_pin_motion void (list '* int int))
(define-lff o_pin_invalidate_rubber void '(*))
(define-lff o_pin_start void (list '* int int))

;;; o_place.c
(define-lff o_place_end void (list '* int int int '*))
(define-lff o_place_invalidate_rubber void (list '* int))
(define-lff o_place_mirror void '(*))
(define-lff o_place_motion void (list '* int int))
(define-lff o_place_rotate void '(*))
(define-lff o_place_start void (list '* int int))

;;; x_attribedit.c
(define-lff attrib_edit_dialog void (list '* '* int))

;;; autonumber_dialog.c
(define-lff schematic_autonumber_new '* '())
(define-lff schematic_autonumber_get_autotext '* '())
(define-lff schematic_autonumber_set_autotext void '(*))
(define-lff schematic_autonumber_get_autotext_dialog '* '(*))
(define-lff schematic_autonumber_set_autotext_dialog void '(* *))
(define-lff schematic_autonumber_get_autotext_removenum int '(*))
(define-lff schematic_autonumber_get_autotext_scope_overwrite int '(*))
(define-lff schematic_autonumber_set_autotext_scope_overwrite void (list '* int))
(define-lff schematic_autonumber_set_autotext_window void '(* *))
(define-lff schematic_autonumber_dialog_lookup_widget '* '(* *))
(define-lff schematic_autonumber_dialog_new '* '(*))
(define-lff schematic_autonumber_dialog_restore_state void '(*))
(define-lff schematic_autonumber_dialog_save_state void '(*))
(define-lff schematic_autonumber_run void '(*))
(define-lff schematic_autonumber_sort_order_widget_init void '(*))

;;; x_clipboard.c
(define-lff x_clipboard_finish void '(*))
(define-lff x_clipboard_init void '(*))
(define-lff x_clipboard_get '* '(*))
(define-lff x_clipboard_set int '(* *))

;;; x_misc.c
(define-lff x_show_uri int '(* * *))

;;; x_event.c
(define-lff gdk_event_scroll_direction_from_string int '(*))
(define-lff gdk_event_scroll_direction_to_string '* (list int))
(define-lff x_event_faked_motion int '(* *))
(define-lff x_event_get_pointer_position int (list '* int '* '*))
(define-lff x_event_key '* '(* * *))
(define-lfc *x_event_configure)
(define-lff schematic_event_get_button int '(*))
(define-lff schematic_event_is_double_button_press int '(*))
(define-lff schematic_event_get_doing_stroke int '())
(define-lff schematic_event_set_doing_stroke void (list int))
(define-lff schematic_event_get_scroll_direction int '(*))
(define-lff schematic_event_skip_motion_event int '(*))
(define-lff schematic_event_alt_mask int '())
(define-lff schematic_event_control_mask int '())
(define-lff schematic_event_shift_mask int '())

;;; x_fileselect.c
(define-lff schematic_file_select_dialog_new '* '(*))
(define-lff x_fileselect_add_preview void '(* *))
(define-lff x_fileselect_open '* '(* *))
(define-lff x_fileselect_save int '(* * *))
(define-lfc *x_fileselect_callback_update_preview)
(define-lff schematic_file_open int '(* * * *))

;;; x_image.c
(define-lff x_image_setup void '(*))

;;; multiattrib_widget.c
(define-lff schematic_multiattrib_widget_new '* '(* *))
(define-lff schematic_multiattrib_widget_update void '(*))

;;; new_text_dialog.c
(define-lff schematic_newtext_dialog_get_aligncb '* '(*))
(define-lff schematic_newtext_dialog_get_colorcb '* '(*))
(define-lff schematic_newtext_dialog_get_rotatecb '* '(*))
(define-lff schematic_newtext_dialog_get_window '* '(*))
(define-lff schematic_newtext_dialog_new '* '(*))
(define-lff schematic_newtext_dialog_run void '(*))
(define-lff schematic_newtext_dialog_get_text '* '(*))
(define-lff schematic_newtext_dialog_get_text_view '* '(*))
(define-lff schematic_newtext_dialog_get_textsizecb '* '(*))
(define-lff schematic_newtext_dialog_textview_select_all void '(*))

;;; x_print.c
(define-lff x_print void '(*))

;;; o_select.c
(define-lff o_select_box_end void (list '* int int))
(define-lff o_select_box_motion void (list '* int int))
(define-lff o_select_end void (list '* int int))
(define-lff o_select_motion void (list '* int int))
(define-lff o_select_selected int '(*))
(define-lff o_select_start void (list '* int int))
(define-lff o_select_unselect_all void '(*))

;;; o_slot.c
(define-lff o_slot_end void '(* * *))

;;; slot.c
(define-lff lepton_slot_update_object void '(*))

;;; signals.c
(define-lff schematic_signal_connect void '(* * * *))

;;; x_tabs.c
(define-lff x_tabs_next void '(*))
(define-lff x_tabs_prev void '(*))

;;; o_undo.c
(define-lff o_undo_savestate void (list '* '* int))
(define-lff o_undo_savestate_old void '(*))
(define-lff o_undo_savestate_viewport void '(*))
(define-lff schematic_undo_get_file_index int '())
(define-lff schematic_undo_index_to_filename '* (list int))
(define-lff schematic_undo_set_tmp_path void '(*))

;;; s_log.c
(define-lff lepton_log_get_logging_enabled int '())
(define-lff lepton_log_set_logging_enabled void (list int))
(define-lff s_log_close void '())

;;; a_zoom.c
(define-lff a_zoom void (list '* '* int int))

;;; x_menus.c
(define-lff g_action_eval_by_name int (list '* '*))

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
         (let ((proc (delay (pointer->procedure '* (force func) '(*)))))
           ((force proc) *window)))))

(define (x_stroke_record *window x y)
  (let ((func (delay (false-if-exception (dynamic-func "x_stroke_record"
                                                       libleptongui)))))
    (and (force func)
         (let ((proc (delay (pointer->procedure void (force func) (list '* int int)))))
           ((force proc) *window x y)))))


(define (parse-gschemrc toplevel)
  "Loads old (system, user, etc.) \"gschemrc\" files and new
configuration \".conf\" files in a newly created toplevel
environment.  Saves the values in the foreign LeptonToplevel
structure TOPLEVEL and returns it."
  (x_rc_parse_gschem toplevel)
  toplevel)
