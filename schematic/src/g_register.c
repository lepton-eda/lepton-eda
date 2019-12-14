/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <config.h>
#include "gschem.h"

/*! \brief */
struct gsubr_t {
  const char* name;
  int req;
  int opt;
  int rst;
  SCM (*fnc)();
};

/*! \brief */
static struct gsubr_t gschem_funcs[] = {
  /* rc file */
  { "gschem-version",               1, 0, 0, (SCM (*) ()) g_rc_gschem_version },

  { "display-color-map",            0, 1, 0, (SCM (*) ()) g_rc_display_color_map },
  { "display-outline-color-map",    0, 1, 0, (SCM (*) ()) g_rc_display_outline_color_map },

  { "attribute-name",               1, 0, 0, (SCM (*) ()) g_rc_attribute_name },

  { "log-window",                   1, 0, 0, (SCM (*) ()) g_rc_log_window },

  { "add-menu",                     2, 0, 0, (SCM (*) ()) g_rc_add_menu },

  /* general guile functions */
  { "gschem-exit",                  0, 0, 0, (SCM (*) ()) g_funcs_exit },
  { "gschem-log",                   1, 0, 0, (SCM (*) ()) g_funcs_log },
  { "gschem-msg",                   1, 0, 0, (SCM (*) ()) g_funcs_msg },
  { "gschem-confirm",               1, 0, 0, (SCM (*) ()) g_funcs_confirm },
  { "gschem-filesel",               2, 0, 1, (SCM (*) ()) g_funcs_filesel },

  /* keymapping callbacks */
  { "file-new-window",              0, 0, 0, (SCM (*) ()) g_keys_file_new_window },
  { "file-new",                     0, 0, 0, (SCM (*) ()) g_keys_file_new },
  { "file-open",                    0, 0, 0, (SCM (*) ()) g_keys_file_open },
  { "file-script",                  0, 0, 0, (SCM (*) ()) g_keys_file_script },
  { "file-save",                    0, 0, 0, (SCM (*) ()) g_keys_file_save },
  { "file-save-as",                 0, 0, 0, (SCM (*) ()) g_keys_file_save_as },
  { "file-save-all",                0, 0, 0, (SCM (*) ()) g_keys_file_save_all },
  { "file-print",                   0, 0, 0, (SCM (*) ()) g_keys_file_print },
  { "file-image",                   0, 0, 0, (SCM (*) ()) g_keys_file_write_png },
  { "file-close-window",            0, 0, 0, (SCM (*) ()) g_keys_file_close },
  { "file-quit",                    0, 0, 0, (SCM (*) ()) g_keys_file_quit },
  { "edit-undo",                    0, 0, 0, (SCM (*) ()) g_keys_edit_undo },
  { "edit-redo",                    0, 0, 0, (SCM (*) ()) g_keys_edit_redo },
  { "edit-select",                  0, 0, 0, (SCM (*) ()) g_keys_edit_select },
  { "edit-select-all",              0, 0, 0, (SCM (*) ()) g_keys_edit_select_all },
  { "edit-deselect",                0, 0, 0, (SCM (*) ()) g_keys_edit_deselect },
  { "edit-copy",                    0, 0, 0, (SCM (*) ()) g_keys_edit_copy },
  { "edit-mcopy",                   0, 0, 0, (SCM (*) ()) g_keys_edit_mcopy },
  { "edit-move",                    0, 0, 0, (SCM (*) ()) g_keys_edit_move },
  { "edit-delete",                  0, 0, 0, (SCM (*) ()) g_keys_edit_delete },
  { "edit-rotate-90",               0, 0, 0, (SCM (*) ()) g_keys_edit_rotate_90 },
  { "edit-mirror",                  0, 0, 0, (SCM (*) ()) g_keys_edit_mirror },
  { "edit-slot",                    0, 0, 0, (SCM (*) ()) g_keys_edit_slot },
  { "edit-object-properties",       0, 0, 0, (SCM (*) ()) g_keys_edit_object_properties },
  { "edit-edit",                    0, 0, 0, (SCM (*) ()) g_keys_edit_edit },
  { "edit-text",                    0, 0, 0, (SCM (*) ()) g_keys_edit_text },
  { "edit-lock",                    0, 0, 0, (SCM (*) ()) g_keys_edit_lock },
  { "edit-unlock",                  0, 0, 0, (SCM (*) ()) g_keys_edit_unlock },
  { "edit-translate",               0, 0, 0, (SCM (*) ()) g_keys_edit_translate },
  { "edit-invoke-macro",            0, 0, 0, (SCM (*) ()) g_keys_edit_invoke_macro },
  { "edit-embed",                   0, 0, 0, (SCM (*) ()) g_keys_edit_embed },
  { "edit-unembed",                 0, 0, 0, (SCM (*) ()) g_keys_edit_unembed },
  { "edit-update",                  0, 0, 0, (SCM (*) ()) g_keys_edit_update },
  { "edit-show-hidden",             0, 0, 0, (SCM (*) ()) g_keys_edit_show_hidden },
  { "edit-find-text",               0, 0, 0, (SCM (*) ()) g_keys_edit_find },
  { "edit-show-text",               0, 0, 0, (SCM (*) ()) g_keys_edit_show_text },
  { "edit-hide-text",               0, 0, 0, (SCM (*) ()) g_keys_edit_hide_text },
  { "edit-autonumber",              0, 0, 0, (SCM (*) ()) g_keys_edit_autonumber_text },

  { "clipboard-copy",               0, 0, 0, (SCM (*) ()) g_keys_clipboard_copy },
  { "clipboard-cut",                0, 0, 0, (SCM (*) ()) g_keys_clipboard_cut },
  { "clipboard-paste",              0, 0, 0, (SCM (*) ()) g_keys_clipboard_paste },

  { "buffer-copy1",                 0, 0, 0, (SCM (*) ()) g_keys_buffer_copy1 },
  { "buffer-copy2",                 0, 0, 0, (SCM (*) ()) g_keys_buffer_copy2 },
  { "buffer-copy3",                 0, 0, 0, (SCM (*) ()) g_keys_buffer_copy3 },
  { "buffer-copy4",                 0, 0, 0, (SCM (*) ()) g_keys_buffer_copy4 },
  { "buffer-copy5",                 0, 0, 0, (SCM (*) ()) g_keys_buffer_copy5 },
  { "buffer-cut1",                  0, 0, 0, (SCM (*) ()) g_keys_buffer_cut1 },
  { "buffer-cut2",                  0, 0, 0, (SCM (*) ()) g_keys_buffer_cut2 },
  { "buffer-cut3",                  0, 0, 0, (SCM (*) ()) g_keys_buffer_cut3 },
  { "buffer-cut4",                  0, 0, 0, (SCM (*) ()) g_keys_buffer_cut4 },
  { "buffer-cut5",                  0, 0, 0, (SCM (*) ()) g_keys_buffer_cut5 },
  { "buffer-paste1",                0, 0, 0, (SCM (*) ()) g_keys_buffer_paste1 },
  { "buffer-paste2",                0, 0, 0, (SCM (*) ()) g_keys_buffer_paste2 },
  { "buffer-paste3",                0, 0, 0, (SCM (*) ()) g_keys_buffer_paste3 },
  { "buffer-paste4",                0, 0, 0, (SCM (*) ()) g_keys_buffer_paste4 },
  { "buffer-paste5",                0, 0, 0, (SCM (*) ()) g_keys_buffer_paste5 },

  { "view-sidebar",                 0, 0, 0, (SCM (*) ()) g_keys_view_sidebar },
  { "view-status",                  0, 0, 0, (SCM (*) ()) g_keys_view_status },
  { "view-find-text-state",         0, 0, 0, (SCM (*) ()) g_keys_view_find_text_state },
  { "view-redraw",                  0, 0, 0, (SCM (*) ()) g_keys_view_redraw },
  { "view-zoom-full",               0, 0, 0, (SCM (*) ()) g_keys_view_zoom_full },
  { "view-zoom-extents",            0, 0, 0, (SCM (*) ()) g_keys_view_zoom_extents },
  { "view-zoom-in",                 0, 0, 0, (SCM (*) ()) g_keys_view_zoom_in },
  { "view-zoom-out",                0, 0, 0, (SCM (*) ()) g_keys_view_zoom_out },
  { "view-zoom-box",                0, 0, 0, (SCM (*) ()) g_keys_view_zoom_box },
  { "view-pan",                     0, 0, 0, (SCM (*) ()) g_keys_view_pan },
  { "view-pan-left",                0, 0, 0, (SCM (*) ()) g_keys_view_pan_left },
  { "view-pan-right",               0, 0, 0, (SCM (*) ()) g_keys_view_pan_right },
  { "view-pan-up",                  0, 0, 0, (SCM (*) ()) g_keys_view_pan_up },
  { "view-pan-down",                0, 0, 0, (SCM (*) ()) g_keys_view_pan_down },
  { "view-dark-colors",             0, 0, 0, (SCM (*) ()) g_keys_view_dark_colors },
  { "view-light-colors",            0, 0, 0, (SCM (*) ()) g_keys_view_light_colors },
  { "view-bw-colors",               0, 0, 0, (SCM (*) ()) g_keys_view_bw_colors },
  { "view-color-edit",              0, 0, 0, (SCM (*) ()) g_keys_view_color_edit },
  { "page-manager",                 0, 0, 0, (SCM (*) ()) g_keys_page_manager },
  { "page-next",                    0, 0, 0, (SCM (*) ()) g_keys_page_next },
  { "page-prev",                    0, 0, 0, (SCM (*) ()) g_keys_page_prev },
  { "page-close",                   0, 0, 0, (SCM (*) ()) g_keys_page_close },
  { "page-next-tab",                0, 0, 0, (SCM (*) ()) g_keys_page_next_tab },
  { "page-prev-tab",                0, 0, 0, (SCM (*) ()) g_keys_page_prev_tab },
  { "page-revert",                  0, 0, 0, (SCM (*) ()) g_keys_page_revert },
  { "page-print",                   0, 0, 0, (SCM (*) ()) g_keys_page_print },
  { "add-component",                0, 0, 0, (SCM (*) ()) g_keys_add_component },
  { "add-attribute",                0, 0, 0, (SCM (*) ()) g_keys_add_attribute },
  { "add-net",                      0, 0, 0, (SCM (*) ()) g_keys_add_net },
  { "add-bus",                      0, 0, 0, (SCM (*) ()) g_keys_add_bus },
  { "add-text",                     0, 0, 0, (SCM (*) ()) g_keys_add_text },
  { "add-path",                     0, 0, 0, (SCM (*) ()) g_keys_add_path },
  { "add-line",                     0, 0, 0, (SCM (*) ()) g_keys_add_line },
  { "add-box",                      0, 0, 0, (SCM (*) ()) g_keys_add_box },
  { "add-picture",                  0, 0, 0, (SCM (*) ()) g_keys_add_picture},
  { "add-circle",                   0, 0, 0, (SCM (*) ()) g_keys_add_circle },
  { "add-arc",                      0, 0, 0, (SCM (*) ()) g_keys_add_arc },
  { "add-pin",                      0, 0, 0, (SCM (*) ()) g_keys_add_pin },
  { "hierarchy-down-schematic",     0, 0, 0, (SCM (*) ()) g_keys_hierarchy_down_schematic },
  { "hierarchy-down-symbol",        0, 0, 0, (SCM (*) ()) g_keys_hierarchy_down_symbol },
  { "hierarchy-up",                 0, 0, 0, (SCM (*) ()) g_keys_hierarchy_up },
  { "attributes-show-name",         0, 0, 0, (SCM (*) ()) g_keys_attributes_show_name },
  { "attributes-show-value",        0, 0, 0, (SCM (*) ()) g_keys_attributes_show_value },
  { "attributes-show-both",         0, 0, 0, (SCM (*) ()) g_keys_attributes_show_both },
  { "attributes-visibility-toggle", 0, 0, 0, (SCM (*) ()) g_keys_attributes_visibility_toggle },
  { "options-snap-size",            0, 0, 0, (SCM (*) ()) g_keys_options_snap_size },
  { "options-scale-up-snap-size",   0, 0, 0, (SCM (*) ()) g_keys_options_scale_up_snap_size },
  { "options-scale-down-snap-size", 0, 0, 0, (SCM (*) ()) g_keys_options_scale_down_snap_size },
  { "options-action-feedback",      0, 0, 0, (SCM (*) ()) g_keys_options_afeedback },
  { "options-grid",                 0, 0, 0, (SCM (*) ()) g_keys_options_grid },
  { "options-snap",                 0, 0, 0, (SCM (*) ()) g_keys_options_snap },
  { "options-rubberband",           0, 0, 0, (SCM (*) ()) g_keys_options_rubberband },
  { "options-magneticnet",          0, 0, 0, (SCM (*) ()) g_keys_options_magneticnet },
  { "options-show-log-window",      0, 0, 0, (SCM (*) ()) g_keys_options_show_log_window },
  { "options-show-coord-window",    0, 0, 0, (SCM (*) ()) g_keys_options_show_coord_window },
  { "options-select-font",          0, 0, 0, (SCM (*) ()) g_keys_options_select_font },
  { "options-draw-grips",           0, 0, 0, (SCM (*) ()) g_keys_options_draw_grips },
  { "help-about",                   0, 0, 0, (SCM (*) ()) g_keys_help_about },
  { "help-hotkeys",                 0, 0, 0, (SCM (*) ()) g_keys_help_hotkeys },
  { "cancel",                       0, 0, 0, (SCM (*) ()) g_keys_cancel },

  { NULL,                           0, 0, 0, NULL } };

/*! \brief Define a hook.
 * \par Function Description
 * Creates a Guile new hook with \a n_args arguments, and binds it to
 * the variable \a name, returning the newly created hook.
 *
 * \param n_args Number of arguments the hook should take.
 * \param name   Name of variable to bind the hook to.
 *
 * \return the newly-created hook.
 */
static SCM
create_hook (const char *name, int n_args)
{
  SCM hook = scm_make_hook (scm_from_int (n_args));
  scm_c_define (name, hook);
  return scm_permanent_object (hook);
}

/*! \brief Register function with Scheme.
 *  \par Function Description
 *  Creates <B>subr</B> objects to make <B>g_rc_*</B> functions that are defined *  #g_rc.c, #g_keys.c and #g_funcs.c visible to Scheme.
 */
void g_register_funcs (void)
{
  struct gsubr_t *tmp = gschem_funcs;

  while (tmp->name != NULL) {
    scm_c_define_gsubr (tmp->name, tmp->req, tmp->opt, tmp->rst, (scm_t_subr) tmp->fnc);
    tmp++;
  }

  /* Hook stuff */
  complex_place_list_changed_hook = create_hook ("complex-place-list-changed-hook", 1);
}
