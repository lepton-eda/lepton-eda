/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#include <missing.h>

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

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include <gdk/gdkkeysyms.h>

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
  char *mod_end = NULL;
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

  /* Allocate space for concatenation of all strings below */
  modifier = mod_end = g_strnfill(3*10, '\0');

  /* The accels below must be in alphabetic order! */
  if (state & GDK_MOD1_MASK) {
    mod_end = g_stpcpy(mod_end, "Alt ");
  }
  if (state & GDK_CONTROL_MASK) {
    mod_end = g_stpcpy(mod_end, "Control ");
  }
  if (state & GDK_SHIFT_MASK) {
    mod_end = g_stpcpy(mod_end, "Shift ");
  }

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

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler (g_free, guile_string, SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (g_free, modifier, SCM_F_WIND_EXPLICITLY);
  g_dynwind_window (w_current);
  scm_retval = g_scm_c_eval_string_protected (guile_string);
  scm_dynwind_end ();

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
    char *str;

    g_return_val_if_fail (SCM_CONSP (scm_keymap_entry) &&
                          scm_is_symbol (SCM_CAR (scm_keymap_entry)) &&
                          scm_is_string (SCM_CDR (scm_keymap_entry)), ret);

    str = scm_to_utf8_string (scm_symbol_to_string (SCM_CAR (scm_keymap_entry)));
    keymap_entry.action = g_strdup (str);
    free(str);

    str = scm_to_utf8_string (SCM_CDR (scm_keymap_entry));
    keymap_entry.keyseq = g_strdup (str);
    free(str);

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
   GSCHEM_TOPLEVEL *w_current = g_current_window ();	\
   g_timeout_add(400, clear_keyaccel_string, w_current);       \
   i_callback_ ## name(w_current, 0, NULL);                   \
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
DEFINE_G_KEYS(edit_select_all)
DEFINE_G_KEYS(edit_deselect)
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
DEFINE_G_KEYS(edit_pin_type)
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
DEFINE_G_KEYS(edit_find)
DEFINE_G_KEYS(edit_show_text)
DEFINE_G_KEYS(edit_hide_text)
DEFINE_G_KEYS(edit_autonumber_text)

DEFINE_G_KEYS(clipboard_copy)
DEFINE_G_KEYS(clipboard_cut)
DEFINE_G_KEYS(clipboard_paste)
DEFINE_G_KEYS(clipboard_paste_hotkey)

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
DEFINE_G_KEYS(view_dark_colors)
DEFINE_G_KEYS(view_light_colors)
DEFINE_G_KEYS(view_bw_colors)
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

/*! Contains the smob tag for key smobs */
static scm_t_bits g_key_smob_tag;
#define G_SCM_IS_KEY(x) SCM_SMOB_PREDICATE (g_key_smob_tag, (x))

/*! Type for keybindings. Used internally by gschem key smobs. */
typedef struct {
  guint keyval;
  GdkModifierType modifiers;
  gchar *str; /* UTF-8. Free with g_free(). */
  gchar *disp_str; /* UTF-8. Free with g_free(). */
} GschemKey;

/*! \brief Test if a key is valid.
 * \par Function Description
 * Test if the key combination defined by \a keyval and \a modifiers
 * is valid for key binding.  This is a less restrictive version of
 * gtk_accelerator_valid() from GTK 2.
 *
 * \param keyval     The key that was pressed.
 * \param modifiers  The active modifiers when the key was pressed.
 *
 * \return TRUE if the key combination is valid for keybinding.
 */
static gboolean
g_key_is_valid (guint keyval, GdkModifierType modifiers)
{
  static const guint invalid_keyvals[] = {
    GDK_Shift_L, GDK_Shift_R, GDK_Shift_Lock, GDK_Caps_Lock, GDK_ISO_Lock,
    GDK_Control_L, GDK_Control_R, GDK_Meta_L, GDK_Meta_R,
    GDK_Alt_L, GDK_Alt_R, GDK_Super_L, GDK_Super_R, GDK_Hyper_L, GDK_Hyper_R,
    GDK_ISO_Level3_Shift, GDK_ISO_Next_Group, GDK_ISO_Prev_Group,
    GDK_ISO_First_Group, GDK_ISO_Last_Group,
    GDK_Mode_switch, GDK_Num_Lock, GDK_Multi_key,
    GDK_Scroll_Lock, GDK_Sys_Req,
    GDK_Tab, GDK_ISO_Left_Tab, GDK_KP_Tab,
    GDK_First_Virtual_Screen, GDK_Prev_Virtual_Screen,
    GDK_Next_Virtual_Screen, GDK_Last_Virtual_Screen,
    GDK_Terminate_Server, GDK_AudibleBell_Enable,
  };
  const guint *val;

  /* Exclude a bunch of control chars */
  if (keyval <= 0xFF) return keyval >= 0x20;

  /* Exclude special & modifier keys */
  val = invalid_keyvals;
  while (*val) {
    if (keyval == *val++) return FALSE;
  }

  return TRUE;
}

/*! \brief Create a new bindable key object.
 * \par Function Description
 * Create and return a new gschem key object from a \a keyval and a
 * set of \a modifiers.  If the key combination is invalid, return
 * SCM_BOOL_F.
 *
 * \param keyval     the pressed key.
 * \param modifiers  the active modifiers for the key.
 *
 * \return a new bindable key object, or SCM_BOOL_F.
 */
static SCM
g_make_key (guint keyval, GdkModifierType modifiers)
{
  SCM result = SCM_BOOL_F;
  if (g_key_is_valid (keyval, modifiers)) {
    GschemKey *k = g_new0 (GschemKey, 1);
    k->keyval = keyval;
    k->modifiers = modifiers & GDK_MODIFIER_MASK;
    SCM_NEWSMOB (result, g_key_smob_tag, k);
  }
  return result;
}

/*! \brief Test if a Scheme value is a bindable key object.
 * \par Function Description
 * Returns SCM_BOOL_T if \a key_s is a gschem key object.  Otherwise,
 * returns SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %key? procedure in the
 * (gschem core keymap) module.
 *
 * \param key_s          value to test
 * \return SCM_BOOL_T iff value is a key, otherwise SCM_BOOL_F.
 */
SCM_DEFINE (g_keyp, "%key?", 1, 0, 0, (SCM key_s),
            "Test if value is a gschem key.")
{
  if (G_SCM_IS_KEY (key_s)) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}

/*! \brief Create a bindable key object from a string.
 * \par Function Description
 * Parse the string key description \a str_s to create and return a
 * new gschem key object.  If \a str_s contains syntax errors, or does
 * not represent a valid bindable key combination, returns SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %string-key procedure in the
 * (gschem core keymap) module.
 *
 * \param str_s  string to parse.
 * \return a new gschem key object, or SCM_BOOL_F.
 */
SCM_DEFINE (g_string_to_key, "%string->key", 1, 0, 0, (SCM str_s),
            "Create a gschem key by parsing a string.")
{
  SCM_ASSERT (scm_is_string (str_s), str_s, SCM_ARG1, s_g_string_to_key);

  guint keyval;
  GdkModifierType modifiers;
  char *str = scm_to_utf8_string (str_s);
  gtk_accelerator_parse (str, &keyval, &modifiers);
  if ((keyval == 0) && (modifiers == 0)) return SCM_BOOL_F;
  return g_make_key (keyval, modifiers);
}

/*! \brief Convert a bindable key object to a string.
 * \par Function Description
 * Returns a string representation of the gschem key object \a key_s,
 * in a format suitable for parsing with %string->key.
 *
 * \note Scheme API: Implements the %key->string procedure in the
 * (gschem core keymap) module.
 *
 * \param key_s  Bindable key object to convert to string.
 * \return a string representation of the key combination.
 */
SCM_DEFINE (g_key_to_string, "%key->string", 1, 0, 0, (SCM key_s),
            "Create a string from a gschem key.")
{
  SCM_ASSERT (G_SCM_IS_KEY (key_s), key_s, SCM_ARG1, s_g_key_to_string);

  GschemKey *key = (GschemKey *) SCM_SMOB_DATA (key_s);
  if (key->str != NULL) return scm_from_utf8_string (key->str);

  key->str = gtk_accelerator_name (key->keyval, key->modifiers);
  return scm_from_utf8_string (key->str);
}

/*! \brief Convert a bindable key object to a displayable string.
 * \par Function Description
 * Returns a string representation of the gschem key object \a key_s,
 * in a format suitable for display to the user (e.g. as accelerator
 * text in a menu).
 *
 * \note Scheme API: Implements the %key->display-string procedure in
 * the (gschem core keymap) module.
 *
 * \param key_s  Bindable key object to convert to string.
 * \return a string representation of the key combination.
 */
SCM_DEFINE (g_key_to_display_string, "%key->display-string", 1, 0, 0,
            (SCM key_s), "Create a display string from a gschem key.")
{
  SCM_ASSERT (G_SCM_IS_KEY (key_s), key_s, SCM_ARG1,
              s_g_key_to_display_string);

  GschemKey *key = (GschemKey *) SCM_SMOB_DATA (key_s);
  if (key->disp_str != NULL) return scm_from_utf8_string (key->disp_str);

  key->disp_str = gtk_accelerator_get_label (key->keyval, key->modifiers);
  return scm_from_utf8_string (key->disp_str);
}

/*! \brief Print a representation of a key smob
 * \par Function Description
 * Outputs a string representing the \a smob to a Scheme output \a
 * port.  The format used is "#<gschem-key \"Ctrl+A\">".
 *
 * Used internally to Guile.
 */
static int
g_key_print (SCM smob, SCM port, scm_print_state *pstate)
{
  scm_puts ("#<gschem-key ", port);
  scm_write (g_key_to_display_string (smob), port);
  scm_puts (">", port);

  /* Non-zero means success */
  return 1;
}

/* \brief Test if two key combinations are equivalent.
 * \par Function Description
 * Tests if the two gschem key objects \a a and \a b represent the
 * same key event.
 *
 * Used internally to Guile.
 */
static SCM
g_key_equalp (SCM a, SCM b)
{
  GschemKey *akey = (GschemKey *) SCM_SMOB_DATA (a);
  GschemKey *bkey = (GschemKey *) SCM_SMOB_DATA (b);
  if (akey->keyval != bkey->keyval) return SCM_BOOL_F;
  if (akey->modifiers != bkey->modifiers) return SCM_BOOL_F;
  return SCM_BOOL_T;
}

/* \brief Destroy a bindable key object
 * \par Function Description
 * Destroys the contents of a gschem key object on garbage collection.
 *
 * Used internally to Guile.
 */
static size_t
g_key_free (SCM key) {
  GschemKey *k = (GschemKey *) SCM_SMOB_DATA (key);
  g_free (k->str);
  g_free (k->disp_str);
  g_free (k);
  return 0;
}

/*! \brief Create the (gschem core keymap) Scheme module
 * \par Function Description
 * Defines procedures in the (gschem core keymap) module.  The module
 * can be accessed using (use-modules (gschem core keymap)).
 */
static void
init_module_gschem_core_keymap ()
{
  /* Register the functions */
  #include "g_keys.x"

  /* Add them to the module's public definitions */
  scm_c_export (s_g_keyp, s_g_string_to_key, s_g_key_to_string,
                s_g_key_to_display_string, NULL);
}

/*! \brief Initialise the key combination procedures
 * \par Function Description
 * Registers some Scheme procedures for working with key combinations.
 * Should only be called by main_prog().
 */
void
g_init_keys ()
{
  /* Register key smob type */
  g_key_smob_tag = scm_make_smob_type ("gschem-key", 0);
  scm_set_smob_print (g_key_smob_tag, g_key_print);
  scm_set_smob_equalp (g_key_smob_tag, g_key_equalp);
  scm_set_smob_free (g_key_smob_tag, g_key_free);

  scm_c_define_module ("gschem core keymap",
                       init_module_gschem_core_keymap,
                       NULL);
}
