/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* for now this only supports single chars, not shift/alt/ctrl etc... */
int g_keys_execute(GSCHEM_TOPLEVEL *w_current, int state, int keyval)
{
  char *guile_string = NULL;
  char *modifier = NULL;
  char *key_name = NULL;
  SCM scm_retval;

  if (keyval == 0) {
    return 0;
  }

  key_name = gdk_keyval_name(keyval);
  if ( key_name == NULL ) {
    return 0;
  }

  /* don't pass the raw modifier key presses to the guile code */
  if (strstr(key_name, "Alt")    ||
      strstr(key_name, "Shift")  ||
      strstr(key_name, "Control") ) {
    return 0;
  }

  if (state & GDK_SHIFT_MASK) {
    modifier = g_strdup_printf("Shift ");
  } else if (state & GDK_CONTROL_MASK) {
    modifier = g_strdup_printf("Control ");
  } else if (state & GDK_MOD1_MASK) {
    modifier = g_strdup_printf("Alt ");
  } else
    modifier = g_strdup("");

  if(strcmp(key_name, "Escape") == 0) {
     g_free(w_current->keyaccel_string);
     w_current->keyaccel_string = NULL;
  } else if(w_current->keyaccel_string &&
        strlen(w_current->keyaccel_string) + strlen(key_name) > 10) {
     g_free(w_current->keyaccel_string);
     w_current->keyaccel_string = g_strconcat(modifier, key_name, NULL);
  } else {
     gchar *p, *r;

     p = w_current->keyaccel_string;
     w_current->keyaccel_string = g_strconcat(modifier, key_name, NULL);
     if(p) {
        r = g_strconcat(p, w_current->keyaccel_string, NULL);
        g_free(p);
        g_free(w_current->keyaccel_string);
        w_current->keyaccel_string = r;
     }
  }

  i_show_state(w_current, NULL);

  guile_string = g_strdup_printf("(press-key \"%s%s\")",
                                 modifier, key_name);

#if DEBUG 
  printf("_%s_\n", guile_string);
#endif
  scm_retval = scm_c_eval_string (guile_string);
  g_free(guile_string);
  g_free(modifier);

  return (SCM_FALSEP (scm_retval)) ? 0 : 1;
}

/*! \brief Exports the keymap in scheme to a GLib GArray.
 *  \par Function Description
 *  This function converts the list of key sequence/action pairs
 *  returned by the scheme function \c dump-current-keymap into an
 *  array of C structures.
 *
 *  The returned value must be freed by caller.
 *
 *  \return A GArray with keymap data.
  */
GArray*
g_keys_dump_keymap (void)
{
  SCM dump_proc = scm_c_lookup ("dump-current-keymap");
  SCM scm_ret;
  GArray *ret = NULL;
  struct keyseq_action_t {
    gchar *keyseq, *action;
  };

  dump_proc = scm_variable_ref (dump_proc);
  g_return_val_if_fail (SCM_NFALSEP (scm_procedure_p (dump_proc)), NULL);

  scm_ret = scm_call_0 (dump_proc);
  g_return_val_if_fail (SCM_CONSP (scm_ret), NULL);

  ret = g_array_sized_new (FALSE,
                           FALSE,
                           sizeof (struct keyseq_action_t),
                           (guint)scm_ilength (scm_ret));
  for (; scm_ret != SCM_EOL; scm_ret = SCM_CDR (scm_ret)) {
    SCM scm_keymap_entry = SCM_CAR (scm_ret);
    struct keyseq_action_t keymap_entry;

    g_return_val_if_fail (SCM_CONSP (scm_keymap_entry) &&
                          SCM_SYMBOLP (SCM_CAR (scm_keymap_entry)) &&
                          scm_is_string (SCM_CDR (scm_keymap_entry)), ret);
    keymap_entry.action = g_strdup (SCM_SYMBOL_CHARS (SCM_CAR (scm_keymap_entry)));
    keymap_entry.keyseq = g_strdup (SCM_STRING_CHARS (SCM_CDR (scm_keymap_entry)));
    ret = g_array_append_val (ret, keymap_entry);
  }

  return ret;
}

/*! \brief Clear the current key accelerator string
 *
 *  \par Function Description
 *  This function clears the current keyboard accelerator
 *  string in the status bar of the relevant toplevel.
 *  Called after the action specifed by the keyboard
 *  accelerator is executed and the associated timeout interval
 *  has passed.
 *
 *  \param [in] data a pointer to the GSCHEM_TOPLEVEL
 */
static gboolean clear_keyaccel_string(gpointer data)
{
  GSCHEM_TOPLEVEL *w_current = data;

  /* Find out if the GSCHEM_TOPLEVEL is present... */
  if (g_list_find(global_window_list, w_current) != NULL) {
    g_free(w_current->keyaccel_string);
    w_current->keyaccel_string = NULL;
    i_show_state(w_current, NULL);
  }

  return FALSE;
}

#define DEFINE_G_KEYS(name)				\
SCM g_keys_ ## name(SCM rest)				\
{							\
   g_timeout_add(400, clear_keyaccel_string, global_window_current); \
   i_callback_ ## name(global_window_current, 0, NULL); \
   return SCM_BOOL_T;				\
}

/*! \brief test-comment
 * test-comment
 */
DEFINE_G_KEYS(file_new)

DEFINE_G_KEYS(file_new_window)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
/* This should be renamed to page_open perhaps... */
DEFINE_G_KEYS(file_open)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
DEFINE_G_KEYS(file_script)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
DEFINE_G_KEYS(file_save)
DEFINE_G_KEYS(file_save_as)
DEFINE_G_KEYS(file_save_all)
DEFINE_G_KEYS(file_print)
DEFINE_G_KEYS(file_write_png)

/* don't use the widget parameter on this function, or do some checking... */
/* since there is a call: widget = NULL, data = 0 (will be w_current) */
/* this function closes a window */
DEFINE_G_KEYS(file_close)
DEFINE_G_KEYS(file_quit)

/* Select also does not update the middle button shortcut */
DEFINE_G_KEYS(edit_undo)
DEFINE_G_KEYS(edit_redo)
DEFINE_G_KEYS(edit_select)
DEFINE_G_KEYS(edit_copy)
DEFINE_G_KEYS(edit_copy_hotkey)
DEFINE_G_KEYS(edit_mcopy)
DEFINE_G_KEYS(edit_mcopy_hotkey)
DEFINE_G_KEYS(edit_move)
DEFINE_G_KEYS(edit_move_hotkey)
DEFINE_G_KEYS(edit_delete)
DEFINE_G_KEYS(edit_rotate_90)
DEFINE_G_KEYS(edit_rotate_90_hotkey)
DEFINE_G_KEYS(edit_mirror)
DEFINE_G_KEYS(edit_mirror_hotkey)
DEFINE_G_KEYS(edit_slot)
DEFINE_G_KEYS(edit_color)
DEFINE_G_KEYS(edit_edit)
DEFINE_G_KEYS(edit_text)
DEFINE_G_KEYS(edit_lock)
DEFINE_G_KEYS(edit_unlock)
DEFINE_G_KEYS(edit_linetype)
DEFINE_G_KEYS(edit_filltype)
DEFINE_G_KEYS(edit_translate)
DEFINE_G_KEYS(edit_invoke_macro)
DEFINE_G_KEYS(edit_embed)
DEFINE_G_KEYS(edit_unembed)
DEFINE_G_KEYS(edit_update)
DEFINE_G_KEYS(edit_show_hidden)
DEFINE_G_KEYS(edit_make_visible)
DEFINE_G_KEYS(edit_find)
DEFINE_G_KEYS(edit_show_text)
DEFINE_G_KEYS(edit_hide_text)
DEFINE_G_KEYS(edit_autonumber_text)

DEFINE_G_KEYS(buffer_copy1)
DEFINE_G_KEYS(buffer_copy2)
DEFINE_G_KEYS(buffer_copy3)
DEFINE_G_KEYS(buffer_copy4)
DEFINE_G_KEYS(buffer_copy5)
DEFINE_G_KEYS(buffer_cut1)
DEFINE_G_KEYS(buffer_cut2)
DEFINE_G_KEYS(buffer_cut3)
DEFINE_G_KEYS(buffer_cut4)
DEFINE_G_KEYS(buffer_cut5)
DEFINE_G_KEYS(buffer_paste1)
DEFINE_G_KEYS(buffer_paste2)
DEFINE_G_KEYS(buffer_paste3)
DEFINE_G_KEYS(buffer_paste4)
DEFINE_G_KEYS(buffer_paste5)
DEFINE_G_KEYS(buffer_paste1_hotkey)
DEFINE_G_KEYS(buffer_paste2_hotkey)
DEFINE_G_KEYS(buffer_paste3_hotkey)
DEFINE_G_KEYS(buffer_paste4_hotkey)
DEFINE_G_KEYS(buffer_paste5_hotkey)

/* repeat middle shortcut doesn't make sense on redraw, just hit right
 * button */
DEFINE_G_KEYS(view_redraw)

/* for these functions, repeat middle shortcut would get into the way
 * of what user is try to do */
DEFINE_G_KEYS(view_zoom_full)
DEFINE_G_KEYS(view_zoom_extents)
DEFINE_G_KEYS(view_zoom_in)
DEFINE_G_KEYS(view_zoom_out)
DEFINE_G_KEYS(view_zoom_in_hotkey)
DEFINE_G_KEYS(view_zoom_out_hotkey)

DEFINE_G_KEYS(view_zoom_box)
DEFINE_G_KEYS(view_zoom_box_hotkey)
DEFINE_G_KEYS(view_pan)
DEFINE_G_KEYS(view_pan_left)
DEFINE_G_KEYS(view_pan_right)
DEFINE_G_KEYS(view_pan_up)
DEFINE_G_KEYS(view_pan_down)
DEFINE_G_KEYS(view_pan_hotkey)
DEFINE_G_KEYS(view_update_cues)
DEFINE_G_KEYS(page_manager)
DEFINE_G_KEYS(page_next)
DEFINE_G_KEYS(page_prev)
DEFINE_G_KEYS(page_new)
DEFINE_G_KEYS(page_close)
DEFINE_G_KEYS(page_revert)
DEFINE_G_KEYS(page_discard)
DEFINE_G_KEYS(page_print)
DEFINE_G_KEYS(add_component)
DEFINE_G_KEYS(add_attribute)
DEFINE_G_KEYS(add_attribute_hotkey)
DEFINE_G_KEYS(add_net)
DEFINE_G_KEYS(add_net_hotkey)
DEFINE_G_KEYS(add_bus)
DEFINE_G_KEYS(add_bus_hotkey)
DEFINE_G_KEYS(add_text)
DEFINE_G_KEYS(add_line)
DEFINE_G_KEYS(add_line_hotkey)
DEFINE_G_KEYS(add_box)
DEFINE_G_KEYS(add_box_hotkey)
DEFINE_G_KEYS(add_picture)
DEFINE_G_KEYS(add_picture_hotkey)
DEFINE_G_KEYS(add_circle)
DEFINE_G_KEYS(add_circle_hotkey)
DEFINE_G_KEYS(add_arc)
DEFINE_G_KEYS(add_arc_hotkey)
DEFINE_G_KEYS(add_pin)
DEFINE_G_KEYS(add_pin_hotkey)
DEFINE_G_KEYS(hierarchy_down_schematic)
DEFINE_G_KEYS(hierarchy_down_symbol)
DEFINE_G_KEYS(hierarchy_up)
DEFINE_G_KEYS(hierarchy_documentation)
DEFINE_G_KEYS(attributes_attach)
DEFINE_G_KEYS(attributes_detach)
DEFINE_G_KEYS(attributes_show_name)
DEFINE_G_KEYS(attributes_show_value)
DEFINE_G_KEYS(attributes_show_both)
DEFINE_G_KEYS(attributes_visibility_toggle)

/* i_callback_script_console is not currently implemented */
DEFINE_G_KEYS(script_console)

/* repeat last command doesn't make sense on options either??? (does
 * it?) */
DEFINE_G_KEYS(options_text_size)

/* repeat last command doesn't make sense on options either??? (does
 * it?) */
DEFINE_G_KEYS(options_afeedback)
DEFINE_G_KEYS(options_grid)
DEFINE_G_KEYS(options_snap)
DEFINE_G_KEYS(options_snap_size)
DEFINE_G_KEYS(options_scale_up_snap_size)
DEFINE_G_KEYS(options_scale_down_snap_size)
DEFINE_G_KEYS(options_rubberband)
DEFINE_G_KEYS(options_magneticnet)
DEFINE_G_KEYS(options_show_log_window)
DEFINE_G_KEYS(options_show_coord_window)
DEFINE_G_KEYS(misc)
DEFINE_G_KEYS(misc2)
DEFINE_G_KEYS(misc3)

DEFINE_G_KEYS(help_about)
DEFINE_G_KEYS(help_hotkeys)

/* be sure that you don't use the widget parameter in this one, since it is
being called with a null, I suppose we should call it with the right param.
hack */
DEFINE_G_KEYS(cancel)


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/*help for generate-netlist hot key*/
SCM g_get_selected_filename(void)                     
{                                                     
	return (get_selected_filename(global_window_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_get_selected_component_attributes(void)                 
{
  return (get_selected_component_attributes(global_window_current));
}
