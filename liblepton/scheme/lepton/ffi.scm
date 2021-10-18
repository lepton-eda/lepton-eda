;;; Lepton EDA library - Scheme API
;;; Copyright (C) 2020-2021 Lepton EDA Contributors
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

(define-module (lepton ffi)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (lepton ffi lib)

  #:re-export (libgtk
               liblepton)

  #:export (liblepton_init
            c-string-array->list
            register-data-dirs
            edascm_init

            ;; Helpers.
            true?
            TRUE
            FALSE
            check-boolean
            check-coord
            check-integer
            check-string
            check-symbol
            check-vector

            ;; glib, gobject.
            g_clear_error
            g_free
            g_list_append
            g_list_free
            g_list_remove
            g_list_remove_all
            g_log
            g_object_unref
            ;; Mock glib functions.
            glist-data
            glist-next
            glist-prev

            ;; Foreign functions.
            edascm_c_current_toplevel
            edascm_is_config
            edascm_from_config
            edascm_to_config
            edascm_is_object
            edascm_from_object
            edascm_to_object
            edascm_is_page
            edascm_from_page
            edascm_to_page

            config_error_type
            config_error_code
            config_error_message
            config_get_legacy_mode
            config_set_legacy_mode
            eda_config_get_anyfile_context
            eda_config_remove_group
            eda_config_remove_key

            eda_get_system_config_dirs
            eda_get_system_data_dirs
            eda_get_user_cache_dir
            eda_get_user_config_dir
            eda_get_user_data_dir

            eda_renderer_new
            eda_renderer_set_color_map

            export_config
            lepton_export_eps
            lepton_export_pdf
            lepton_export_png
            lepton_export_ps
            lepton_export_svg
            lepton_export_list_paper_size_names
            lepton_export_parse_align
            lepton_export_parse_layout
            lepton_export_parse_margins
            lepton_export_parse_paper
            lepton_export_parse_scale
            lepton_export_parse_size
            lepton_export_settings_reset_paper_size
            lepton_export_settings_set_color
            lepton_export_settings_set_dpi
            lepton_export_settings_set_font
            lepton_export_settings_set_format
            lepton_export_settings_set_outfile

            lepton_object_get_attached_to
            lepton_object_set_attached_to
            lepton_object_get_attribs
            lepton_object_set_attribs
            lepton_object_get_color
            lepton_object_set_color
            lepton_object_get_fill_angle1
            lepton_object_get_fill_angle2
            lepton_object_get_fill_pitch1
            lepton_object_get_fill_pitch2
            lepton_object_get_fill_type
            lepton_object_get_fill_width
            lepton_object_set_fill_options
            lepton_object_get_id
            lepton_object_get_page
            lepton_object_get_parent
            lepton_object_set_parent
            lepton_object_get_selectable
            lepton_object_set_selectable
            lepton_object_get_stroke_cap_type
            lepton_object_set_stroke_cap_type
            lepton_object_get_stroke_type
            lepton_object_set_stroke_type
            lepton_object_get_stroke_width
            lepton_object_set_stroke_width
            lepton_object_get_stroke_dash_length
            lepton_object_set_stroke_dash_length
            lepton_object_get_stroke_space_length
            lepton_object_set_stroke_space_length
            lepton_object_get_type
            lepton_object_get_whichend

            lepton_object_is_arc
            lepton_object_is_attrib
            lepton_object_is_box
            lepton_object_is_bus
            lepton_object_is_circle
            lepton_object_is_component
            lepton_object_is_line
            lepton_object_is_net
            lepton_object_is_path
            lepton_object_is_picture
            lepton_object_is_pin
            lepton_object_is_text

            lepton_object_calculate_visible_bounds
            lepton_object_copy
            lepton_object_emit_change_notify
            lepton_object_emit_pre_change_notify
            lepton_object_mirror
            lepton_object_rotate
            lepton_object_translate
            lepton_object_visibility_from_string

            lepton_arc_object_get_center_x
            lepton_arc_object_set_center_x
            lepton_arc_object_get_center_y
            lepton_arc_object_set_center_y
            lepton_arc_object_get_radius
            lepton_arc_object_set_radius
            lepton_arc_object_get_start_angle
            lepton_arc_object_set_start_angle
            lepton_arc_object_get_sweep_angle
            lepton_arc_object_set_sweep_angle
            lepton_arc_object_new

            lepton_box_object_get_upper_x
            lepton_box_object_set_upper_x
            lepton_box_object_get_upper_y
            lepton_box_object_set_upper_y
            lepton_box_object_get_lower_x
            lepton_box_object_set_lower_x
            lepton_box_object_get_lower_y
            lepton_box_object_set_lower_y
            lepton_box_object_new

            lepton_bus_object_modify
            lepton_bus_object_new

            lepton_circle_object_get_center_x
            lepton_circle_object_set_center_x
            lepton_circle_object_get_center_y
            lepton_circle_object_set_center_y
            lepton_circle_object_get_radius
            lepton_circle_object_set_radius
            lepton_circle_object_new

            lepton_component_new
            lepton_component_new_embedded
            lepton_component_object_get_angle
            lepton_component_object_set_angle
            lepton_component_object_get_basename
            lepton_component_object_get_contents
            lepton_component_object_set_contents
            lepton_component_object_get_embedded
            lepton_component_object_embed
            lepton_component_object_unembed
            lepton_component_object_get_mirror
            lepton_component_object_set_mirror
            lepton_component_object_get_x
            lepton_component_object_get_y
            lepton_component_object_get_promotable

            lepton_line_object_get_x0
            lepton_line_object_get_y0
            lepton_line_object_get_x1
            lepton_line_object_get_y1
            lepton_line_object_modify
            lepton_line_object_new

            lepton_net_object_modify
            lepton_net_object_new

            lepton_path_object_get_num_sections
            lepton_path_object_get_section
            lepton_path_object_insert_section
            lepton_path_object_new
            lepton_path_object_remove_section

            lepton_path_section_code_from_string
            lepton_path_section_code_to_string
            lepton_path_section_get_code
            lepton_path_section_get_x1
            lepton_path_section_get_y1
            lepton_path_section_get_x2
            lepton_path_section_get_y2
            lepton_path_section_get_x3
            lepton_path_section_get_y3

            lepton_picture_get_fallback_pixbuf

            lepton_picture_object_get_angle
            lepton_picture_object_set_angle
            lepton_picture_object_get_embedded
            lepton_picture_object_get_filename
            lepton_picture_object_get_mirrored
            lepton_picture_object_set_mirrored
            lepton_picture_object_get_pixbuf
            lepton_picture_object_get_lower_x
            lepton_picture_object_set_lower_x
            lepton_picture_object_get_lower_y
            lepton_picture_object_set_lower_y
            lepton_picture_object_get_upper_x
            lepton_picture_object_set_upper_x
            lepton_picture_object_get_upper_y
            lepton_picture_object_set_upper_y
            lepton_picture_object_embed
            lepton_picture_object_unembed
            lepton_picture_object_new
            lepton_picture_object_set_from_buffer

            lepton_pin_object_is_bus_pin
            lepton_pin_object_is_net_pin
            lepton_pin_object_new_bus_pin
            lepton_pin_object_new_net_pin
            lepton_pin_object_modify

            lepton_text_object_get_alignment
            lepton_text_object_set_alignment
            lepton_text_object_get_angle
            lepton_text_object_set_angle
            lepton_text_object_get_name
            lepton_text_object_get_show
            lepton_text_object_set_show
            lepton_text_object_get_size
            lepton_text_object_set_size
            lepton_text_object_get_string
            lepton_text_object_set_string
            lepton_text_object_get_value
            lepton_text_object_set_visibility
            lepton_text_object_get_x
            lepton_text_object_set_x
            lepton_text_object_get_y
            lepton_text_object_set_y
            lepton_text_object_alignment_from_string
            lepton_text_object_alignment_to_string
            lepton_text_object_is_visible
            lepton_text_object_new
            lepton_text_object_recreate
            lepton_text_object_show_from_string
            lepton_text_object_show_to_string

            lepton_fill_type_from_string
            lepton_fill_type_to_string

            lepton_stroke_cap_type_from_string
            lepton_stroke_cap_type_to_string
            lepton_stroke_type_from_string
            lepton_stroke_type_to_string

            set_render_placeholders
            colors_count
            default_color_id
            g_rc_parse
            lepton_colormap_color_by_id
            lepton_colormap_disable_color
            lepton_colormap_set_color
            lepton_object_page_set_changed
            print_colors_array
            s_attrib_uniq
            s_attrib_add_entry
            s_clib_add_command
            s_clib_add_directory
            s_clib_add_scm
            s_clib_get_symbol_by_name
            s_clib_init
            s_clib_symbol_get_filename

            o_attrib_attach

            s_conn_remove_object
            s_conn_remove_object_connections
            s_conn_return_others
            s_conn_update_object

            lepton_toplevel_get_page_current
            lepton_toplevel_get_pages

            lepton_object_list_to_buffer

            lepton_page_get_changed
            lepton_page_set_changed
            lepton_page_get_selection_list
            lepton_page_append
            lepton_page_append_list
            lepton_page_delete
            lepton_page_get_filename
            lepton_page_set_filename
            lepton_page_new
            lepton_page_objects
            lepton_page_remove
            lepton_page_list_get_glist

            o_selection_remove

            o_read_buffer))

;;; Helper to check if result of C function is TRUE (non-zero).
(define true? (negate zero?))
;;; Helpers to set results of boolean functions.
(define TRUE 1)
(define FALSE 0)

;;; Brief syntax macro for defining lazy foreign functions.
(define-syntax define-lff
  (syntax-rules ()
    ((_ name type args)
     (define name
       (let ((proc (delay (pointer->procedure
                           type
                           (dynamic-func (symbol->string (quote name)) liblepton)
                           args))))
         (force proc))))
    ((_ name type args lib)
     (define name
       (let ((proc (delay (pointer->procedure
                           type
                           (dynamic-func (symbol->string (quote name)) lib)
                           args))))
         (force proc))))))

(define-lff g_clear_error void '(*) libglib)
(define-lff g_free void '(*) libglib)
(define-lff g_list_append '* '(* *) libglib)
(define-lff g_list_free void '(*) libglib)
(define-lff g_list_remove '* '(* *) libglib)
(define-lff g_list_remove_all '* '(* *) libglib)
(define-lff g_log void (list '* int '* '*) libglib)
(define-lff g_object_unref void '(*) libgobject)

;;; Glist struct is {data*, next*, prev*}.  We could use libglib
;;; functions to get data, but it's easier to parse the struct
;;; directly.
(define (parse-glist gls)
  (parse-c-struct gls '(* * *)))

(define (glist-next gls)
  (let ((pointer-ls (parse-glist gls)))
    (match pointer-ls
      ((data next prev) next)
      (_ (error "Wrong Glist in glist-next()")))))

(define (glist-prev gls)
  (let ((pointer-ls (parse-glist gls)))
    (match pointer-ls
      ((data next prev) prev)
      (_ (error "Wrong Glist in glist-prev()")))))

(define (glist-data gls)
  (let ((pointer-ls (parse-glist gls)))
    (match pointer-ls
      ((data next prev) data)
      (_ (error "Wrong Glist in glist-data()")))))

;;; Basic lepton initialisation function.
(define-lff liblepton_init void '())
(define-lff edascm_init void '())

(define-lff set_render_placeholders void '())
(define-lff colors_count size_t '())
(define-lff default_color_id int '())
(define-lff lepton_colormap_color_by_id '* (list '* size_t))
(define-lff lepton_colormap_disable_color void (list '* size_t))
(define-lff lepton_colormap_set_color void (list '* size_t uint8 uint8 uint8 uint8))
(define-lff print_colors_array '* '())
;;; s_clib.c
(define-lff s_clib_add_command '* '(* * *))
(define-lff s_clib_add_directory '* '(* *))
(define-lff s_clib_add_scm '* '(* * *))
(define-lff s_clib_get_symbol_by_name '* '(*))
(define-lff s_clib_init void '())
(define-lff s_clib_symbol_get_filename '* '(*))
;;; toplevel.c
(define-lff lepton_toplevel_get_page_current '* '(*))
(define-lff lepton_toplevel_get_pages '* '(*))
;;; g_rc.c
(define-lff g_rc_parse void '(* * *))
;;; s_attrib.c
(define-lff s_attrib_uniq int (list '*))
(define-lff s_attrib_add_entry int (list '*))

;;; edaconfig.c
(define-lff config_error_type '* '(*))
(define-lff config_error_code '* '(*))
(define-lff config_error_message '* '(*))
(define-lff config_get_legacy_mode int '())
(define-lff config_set_legacy_mode void (list int))
(define-lff eda_config_get_anyfile_context '* (list '* '* int))
(define-lff eda_config_remove_group int '(* * *))
(define-lff eda_config_remove_key int '(* * * *))

;;; edapaths.c
(define-lff eda_get_system_config_dirs '* '())
(define-lff eda_get_system_data_dirs '* '())
(define-lff eda_get_user_cache_dir '* '())
(define-lff eda_get_user_config_dir '* '())
(define-lff eda_get_user_data_dir '* '())

;;; edarenderer.c
(define-lff eda_renderer_new '* '(* *))
(define-lff eda_renderer_set_color_map void '(* *))

;;; scheme_smob.c
(define-lff edascm_c_current_toplevel '* '())
(define-lff edascm_is_config int '(*))
(define-lff edascm_from_config '* '(*))
(define-lff edascm_to_config '* '(*))
(define-lff edascm_is_object int '(*))
(define-lff edascm_is_page int '(*))
(define-lff edascm_from_object '* '(*))
(define-lff edascm_to_object '* '(*))
(define-lff edascm_from_page '* '(*))
(define-lff edascm_to_page '* '(*))

;;; object.c
(define-lff lepton_object_get_attached_to '* '(*))
(define-lff lepton_object_set_attached_to void '(* *))
(define-lff lepton_object_get_attribs '* '(*))
(define-lff lepton_object_set_attribs void '(* *))
(define-lff lepton_object_get_color int '(*))
(define-lff lepton_object_set_color void (list '* int))
(define-lff lepton_object_get_fill_angle1 int '(*))
(define-lff lepton_object_get_fill_angle2 int '(*))
(define-lff lepton_object_get_fill_pitch1 int '(*))
(define-lff lepton_object_get_fill_pitch2 int '(*))
(define-lff lepton_object_get_fill_type int '(*))
(define-lff lepton_object_get_fill_width int '(*))
(define-lff lepton_object_set_fill_options void (list '* int int int int int int))
(define-lff lepton_object_get_id int '(*))
(define-lff lepton_object_get_page '* '(*))
(define-lff lepton_object_get_parent '* '(*))
(define-lff lepton_object_set_parent void '(* *))
(define-lff lepton_object_get_selectable int '(*))
(define-lff lepton_object_set_selectable void (list '* int))
(define-lff lepton_object_get_stroke_cap_type int '(*))
(define-lff lepton_object_set_stroke_cap_type void (list '* int))
(define-lff lepton_object_get_stroke_type int '(*))
(define-lff lepton_object_set_stroke_type void (list '* int))
(define-lff lepton_object_get_stroke_width int '(*))
(define-lff lepton_object_set_stroke_width void (list '* int))
(define-lff lepton_object_get_stroke_dash_length int '(*))
(define-lff lepton_object_set_stroke_dash_length void (list '* int))
(define-lff lepton_object_get_stroke_space_length int '(*))
(define-lff lepton_object_set_stroke_space_length void (list '* int))
(define-lff lepton_object_get_type int '(*))
(define-lff lepton_object_get_whichend int '(*))

(define-lff lepton_object_is_arc int '(*))
(define-lff lepton_object_is_attrib int '(*))
(define-lff lepton_object_is_box int '(*))
(define-lff lepton_object_is_bus int '(*))
(define-lff lepton_object_is_circle int '(*))
(define-lff lepton_object_is_component int '(*))
(define-lff lepton_object_is_line int '(*))
(define-lff lepton_object_is_net int '(*))
(define-lff lepton_object_is_path int '(*))
(define-lff lepton_object_is_picture int '(*))
(define-lff lepton_object_is_pin int '(*))
(define-lff lepton_object_is_text int '(*))

(define-lff lepton_object_calculate_visible_bounds int (list '* int '* '* '* '*))
(define-lff lepton_object_copy '* '(*))
(define-lff lepton_object_emit_change_notify void '(*))
(define-lff lepton_object_emit_pre_change_notify void '(*))
(define-lff lepton_object_mirror void (list int int '*))
(define-lff lepton_object_rotate void (list int int int '*))
(define-lff lepton_object_translate void (list '* int int))
(define-lff lepton_object_visibility_from_string int '(*))

(define-lff lepton_object_page_set_changed void '(*))

(define-lff lepton_arc_object_get_center_x int '(*))
(define-lff lepton_arc_object_set_center_x void (list '* int))
(define-lff lepton_arc_object_get_center_y int '(*))
(define-lff lepton_arc_object_set_center_y void (list '* int))
(define-lff lepton_arc_object_get_radius int '(*))
(define-lff lepton_arc_object_set_radius void (list '* int))
(define-lff lepton_arc_object_get_start_angle int '(*))
(define-lff lepton_arc_object_set_start_angle void (list '* int))
(define-lff lepton_arc_object_get_sweep_angle int '(*))
(define-lff lepton_arc_object_set_sweep_angle void (list '* int))
(define-lff lepton_arc_object_new '* (list int int int int int int))

(define-lff lepton_box_object_get_upper_x int '(*))
(define-lff lepton_box_object_set_upper_x void (list '* int))
(define-lff lepton_box_object_get_upper_y int '(*))
(define-lff lepton_box_object_set_upper_y void (list '* int))
(define-lff lepton_box_object_get_lower_x int '(*))
(define-lff lepton_box_object_set_lower_x void (list '* int))
(define-lff lepton_box_object_get_lower_y int '(*))
(define-lff lepton_box_object_set_lower_y void (list '* int))
(define-lff lepton_box_object_new '* (list int int int int int))

(define-lff lepton_bus_object_modify void (list '* int int int))
(define-lff lepton_bus_object_new '* (list int int int int int int))

(define-lff lepton_circle_object_get_center_x int '(*))
(define-lff lepton_circle_object_set_center_x void (list '* int))
(define-lff lepton_circle_object_get_center_y int '(*))
(define-lff lepton_circle_object_set_center_y void (list '* int))
(define-lff lepton_circle_object_get_radius int '(*))
(define-lff lepton_circle_object_set_radius void (list '* int))
(define-lff lepton_circle_object_new '* (list int int int int))

(define-lff lepton_component_object_get_angle int '(*))
(define-lff lepton_component_object_set_angle void (list '* int))
(define-lff lepton_component_new '* (list '* int int int int int '* '* int))
(define-lff lepton_component_new_embedded '* (list int int int int int '* int))
(define-lff lepton_component_object_get_basename '* '(*))
(define-lff lepton_component_object_get_contents '* '(*))
(define-lff lepton_component_object_set_contents void '(* *))
(define-lff lepton_component_object_get_embedded int '(*))
(define-lff lepton_component_object_embed void '(*))
(define-lff lepton_component_object_unembed void '(*))
(define-lff lepton_component_object_get_mirror int '(*))
(define-lff lepton_component_object_set_mirror void (list '* int))
(define-lff lepton_component_object_get_x int '(*))
(define-lff lepton_component_object_get_y int '(*))
(define-lff lepton_component_object_get_promotable '* (list '* int))

(define-lff lepton_line_object_get_x0 int '(*))
(define-lff lepton_line_object_get_y0 int '(*))
(define-lff lepton_line_object_get_x1 int '(*))
(define-lff lepton_line_object_get_y1 int '(*))
(define-lff lepton_line_object_modify void (list '* int int int))
(define-lff lepton_line_object_new '* (list int int int int int))

(define-lff lepton_net_object_modify void (list '* int int int))
(define-lff lepton_net_object_new '* (list int int int int int))

(define-lff lepton_path_object_get_num_sections int '(*))
(define-lff lepton_path_object_get_section '* (list '* int))
(define-lff lepton_path_object_insert_section '* (list '* '* int))
(define-lff lepton_path_object_new '* (list int '*))
(define-lff lepton_path_object_remove_section '* (list '* int))
(define-lff lepton_path_section_code_from_string int '(*))
(define-lff lepton_path_section_code_to_string '* (list int))
(define-lff lepton_path_section_get_code int '(*))
(define-lff lepton_path_section_get_x1 int '(*))
(define-lff lepton_path_section_get_y1 int '(*))
(define-lff lepton_path_section_get_x2 int '(*))
(define-lff lepton_path_section_get_y2 int '(*))
(define-lff lepton_path_section_get_x3 int '(*))
(define-lff lepton_path_section_get_y3 int '(*))

(define-lff lepton_picture_get_fallback_pixbuf '* '())

(define-lff lepton_picture_object_get_angle int '(*))
(define-lff lepton_picture_object_set_angle void (list '* int))
(define-lff lepton_picture_object_get_embedded int '(*))
(define-lff lepton_picture_object_get_filename '* '(*))
(define-lff lepton_picture_object_get_mirrored int '(*))
(define-lff lepton_picture_object_set_mirrored void (list '* int))
(define-lff lepton_picture_object_get_pixbuf '* '(*))
(define-lff lepton_picture_object_get_lower_x int '(*))
(define-lff lepton_picture_object_set_lower_x void (list '* int))
(define-lff lepton_picture_object_get_lower_y int '(*))
(define-lff lepton_picture_object_set_lower_y void (list '* int))
(define-lff lepton_picture_object_get_upper_x int '(*))
(define-lff lepton_picture_object_set_upper_x void (list '* int))
(define-lff lepton_picture_object_get_upper_y int '(*))
(define-lff lepton_picture_object_set_upper_y void (list '* int))
(define-lff lepton_picture_object_embed void '(*))
(define-lff lepton_picture_object_unembed void '(*))
(define-lff lepton_picture_object_new '* (list '* int '* int int int int int int int))
(define-lff lepton_picture_object_set_from_buffer int (list '* '* '* size_t '*))

(define-lff lepton_pin_object_is_bus_pin int '(*))
(define-lff lepton_pin_object_is_net_pin int '(*))
(define-lff lepton_pin_object_new_bus_pin '* (list int int int int int int))
(define-lff lepton_pin_object_new_net_pin '* (list int int int int int int))
(define-lff lepton_pin_object_modify void (list '* int int int))

(define-lff lepton_text_object_get_alignment int '(*))
(define-lff lepton_text_object_set_alignment void (list '* int))
(define-lff lepton_text_object_get_angle int '(*))
(define-lff lepton_text_object_set_angle void (list '* int))
(define-lff lepton_text_object_get_name '* '(*))
(define-lff lepton_text_object_get_show int '(*))
(define-lff lepton_text_object_set_show void (list '* int))
(define-lff lepton_text_object_get_size int '(*))
(define-lff lepton_text_object_set_size void (list '* int))
(define-lff lepton_text_object_get_string '* '(*))
(define-lff lepton_text_object_set_string void '(* *))
(define-lff lepton_text_object_get_value '* '(*))
(define-lff lepton_text_object_set_visibility void (list '* int))
(define-lff lepton_text_object_get_x int '(*))
(define-lff lepton_text_object_set_x void (list '* int))
(define-lff lepton_text_object_get_y int '(*))
(define-lff lepton_text_object_set_y void (list '* int))
(define-lff lepton_text_object_alignment_from_string int '(*))
(define-lff lepton_text_object_alignment_to_string '* (list int))
(define-lff lepton_text_object_is_visible int '(*))
(define-lff lepton_text_object_show_from_string int '(*))
(define-lff lepton_text_object_show_to_string '* (list int))
(define-lff lepton_text_object_recreate void '(*))
(define-lff lepton_text_object_new '* (list int int int int int '* int int int))

(define-lff lepton_fill_type_from_string int '(*))
(define-lff lepton_fill_type_to_string '* (list int))

(define-lff lepton_stroke_cap_type_from_string int '(*))
(define-lff lepton_stroke_cap_type_to_string '* (list int))
(define-lff lepton_stroke_type_from_string int '(*))
(define-lff lepton_stroke_type_to_string '* (list int))

;;; o_attrib.c
(define-lff o_attrib_attach void (list '* '* int))

;; s_conn.c
(define-lff s_conn_remove_object void '(* *))
(define-lff s_conn_remove_object_connections void '(*))
(define-lff s_conn_return_others '* '(* *))
(define-lff s_conn_update_object void '(* *))

;;; object_list.c
(define-lff lepton_object_list_to_buffer '* '(*))

;;; page.c
(define-lff lepton_page_get_changed int '(*))
(define-lff lepton_page_set_changed void (list '* int))
(define-lff lepton_page_get_selection_list '* '(*))
(define-lff lepton_page_append void '(* *))
(define-lff lepton_page_append_list void '(* *))
(define-lff lepton_page_delete void '(* *))
(define-lff lepton_page_get_filename '* '(*))
(define-lff lepton_page_set_filename void '(* *))
(define-lff lepton_page_new '* '(* *))
(define-lff lepton_page_objects '* '(*))
(define-lff lepton_page_remove void '(* *))
(define-lff lepton_page_list_get_glist '* '(*))

;;; o_selection.c
(define-lff o_selection_remove void '(* *))

;;; a_basic.c
(define-lff o_read_buffer '* (list '* '* '* int '* '*))

;;; export.c
(define-lff export_config void '())
(define-lff lepton_export_eps void '())
(define-lff lepton_export_pdf void '())
(define-lff lepton_export_png void '())
(define-lff lepton_export_ps void '())
(define-lff lepton_export_svg void '())
(define-lff lepton_export_list_paper_size_names void '())
(define-lff lepton_export_parse_align int '(*))
(define-lff lepton_export_parse_layout int '(*))
(define-lff lepton_export_parse_margins int '(*))
(define-lff lepton_export_parse_paper int '(*))
(define-lff lepton_export_parse_scale int '(*))
(define-lff lepton_export_parse_size int '(*))
(define-lff lepton_export_settings_reset_paper_size void '())
(define-lff lepton_export_settings_set_color void (list int))
(define-lff lepton_export_settings_set_dpi void (list double))

(define-lff lepton_export_settings_set_font void '(*))
(define-lff lepton_export_settings_set_format void '(*))
(define-lff lepton_export_settings_set_outfile void '(*))

(define (c-string-array->list pointer)
  "Returns a list of search directories for system data."
  (let loop ((num 0)
             (ls '()))
    (let ((string-pointer
           (dereference-pointer
            (make-pointer (+ (pointer-address pointer)
                             (* num (sizeof '*)))))))
      (if (null-pointer? string-pointer)
          (reverse ls)
          (loop (1+ num)
                (cons (pointer->string string-pointer)
                      ls))))))


;;; Register liblepton directories with Scheme.
(define (register-data-dirs)
  ;; Enable scheme loading from a shared data directory.
  (define (register-data-dir dir)
    (add-to-load-path (string-append dir
                                     file-name-separator-string
                                     "scheme")))

  (let ((sys-dirs (c-string-array->list (eda_get_system_data_dirs))))
    (for-each register-data-dir sys-dirs)
    (register-data-dir (pointer->string (eda_get_user_data_dir)))))

(define (check-boolean val pos)
  ;; This function is defined just for consistency.  Someone may
  ;; get confused if we miss some argument checks. Since any value
  ;; in Scheme is a boolean in a sense, that is, it is considered
  ;; to be true if not #f, there is no point to check for real
  ;; boolean values, #t and #f, using Scheme boolean? function.
  ;; So we just return #t here.
  #t)

(define-syntax-rule (check-integer val pos)
  (unless (integer? val)
    (scm-error 'wrong-type-arg
               (frame-procedure-name (stack-ref (make-stack #t) 1))
               "Wrong type argument in position ~A (expecting integer): ~A"
               (list pos val)
               #f)))

(define-syntax-rule (check-coord val pos)
  (unless (and (pair? val)
               (integer? (car val))
               (integer? (cdr val)))
    (scm-error 'wrong-type-arg
               (frame-procedure-name (stack-ref (make-stack #t) 1))
               "Wrong type argument in position ~A (expecting a pair of integers): ~A"
               (list pos val)
               #f)))

(define-syntax-rule (check-string val pos)
  (unless (string? val)
    (scm-error 'wrong-type-arg
               (frame-procedure-name (stack-ref (make-stack #t) 1))
               "Wrong type argument in position ~A (expecting string): ~A"
               (list pos val)
               #f)))

(define-syntax-rule (check-symbol val pos)
  (unless (symbol? val)
    (scm-error 'wrong-type-arg
               (frame-procedure-name (stack-ref (make-stack #t) 1))
               "Wrong type argument in position ~A (expecting symbol): ~A"
               (list pos val)
               #f)))

(define-syntax-rule (check-vector val pos)
  (unless (and (list? val)
               (every integer? val))
    (scm-error 'wrong-type-arg
               (frame-procedure-name (stack-ref (make-stack #t) 1))
               "Wrong type argument in position ~A (expecting list of integers): ~A"
               (list pos val)
               #f)))
