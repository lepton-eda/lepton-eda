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


#define DEFINE_G_KEYS(name)				\
SCM g_keys_ ## name(SCM rest)				\
{							\
   GSCHEM_TOPLEVEL *w_current = g_current_window ();	\
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

SCM_SYMBOL (reset_keys_sym, "reset-keys");
SCM_SYMBOL (press_key_sym, "press-key");
SCM_SYMBOL (prefix_sym, "prefix");

/*! \brief Clear the current key accelerator string.
 * \par Function Description
 * This function clears the current keyboard accelerator string in
 * the status bar of the relevant toplevel.  Called some time after a
 * keystroke is pressed.  If the current key sequence was a prefix,
 * let it persist.
 *
 * \param [in] data a pointer to the GSCHEM_TOPLEVEL to update.
 * \return FALSE (this is a one-shot timer).
 */
static gboolean clear_keyaccel_string(gpointer data)
{
  GSCHEM_TOPLEVEL *w_current = data;

  /* If the window context has disappeared, do nothing. */
  if (g_list_find(global_window_list, w_current) == NULL) {
    return FALSE;
  }

  g_free(w_current->keyaccel_string);
  w_current->keyaccel_string = NULL;
  w_current->keyaccel_string_source_id = 0;
  i_show_state(w_current, NULL);
  return FALSE;
}

/*! \brief Reset the current key sequence.
 * \par Function Description
 * If any prefix keys are stored in the current key sequence, clears
 * them.
 *
 * \param w_current  The active #GSCHEM_TOPLEVEL context.
 */
void
g_keys_reset (GSCHEM_TOPLEVEL *w_current)
{
  SCM s_expr = scm_list_1 (reset_keys_sym);

  /* Reset the status bar */
  g_free (w_current->keyaccel_string);
  w_current->keyaccel_string = NULL;
  i_show_state(w_current, NULL);

  /* Reset the Scheme keybinding state */
  scm_dynwind_begin (0);
  g_dynwind_window (w_current);
  g_scm_eval_protected (s_expr, scm_interaction_environment ());
  scm_dynwind_end ();
}

/*! \brief Evaluate a user keystroke.
 * \par Function Description
 * Evaluates the key combination specified by \a event using the
 * current keymap.  Updates the gschem status bar with the current key
 * sequence.
 *
 * \param w_current  The active #GSCHEM_TOPLEVEL context.
 * \param event      A GdkEventKey structure.
 *
 * \return 1 if a binding was found for the keystroke, 0 otherwise.
 */
int
g_keys_execute(GSCHEM_TOPLEVEL *w_current, GdkEventKey *event)
{
  SCM s_retval, s_key, s_expr;
  guint key, mods, upper, lower, caps;
  GdkDisplay *display;
  GdkKeymap *keymap;
  GdkModifierType consumed_modifiers;

  g_return_val_if_fail (w_current != NULL, 0);
  g_return_val_if_fail (event != NULL, 0);

  display = gtk_widget_get_display (w_current->main_window);
  keymap = gdk_keymap_get_for_display (display);

  /* Figure out what modifiers went into determining the key symbol */
  gdk_keymap_translate_keyboard_state (keymap,
                                       event->hardware_keycode,
                                       event->state, event->group,
                                       NULL, NULL, NULL, &consumed_modifiers);

  key = event->keyval;
  gdk_keyval_convert_case (event->keyval, &lower, &upper);
  mods = (event->state & gtk_accelerator_get_default_mod_mask ()
                & ~consumed_modifiers);

  /* Handle Caps Lock. The idea is to obtain the same keybindings
   * whether Caps Lock is enabled or not. */
  if (upper != lower) {
    caps = gdk_keymap_get_caps_lock_state (keymap);
    if ((caps && (key == lower)) || (!caps && (key == upper))) {
      mods |= GDK_SHIFT_MASK;
    }
  }

  /* Always process key as lower case */
  key = lower;

  /* Validate the key -- there are some keystrokes we mask out. */
  if (!g_key_is_valid (key, mods)) {
    return FALSE;
  }

  /* Create Scheme key value */
  /* FIXME Escape as cancel key shouldn't be hardcoded in. */
  if (key == GDK_Escape) {
    g_keys_reset (w_current);
    return FALSE;
  }

  s_key = g_make_key (key, mods);

  /* Update key hint string for status bar. */
  gchar *keystr = gtk_accelerator_get_label (key, mods);

  /* If no current hint string, or the hint string is going to be
   * cleared anyway, use key string directly */
  if ((w_current->keyaccel_string == NULL) ||
      w_current->keyaccel_string_source_id) {
    g_free (w_current->keyaccel_string);
    w_current->keyaccel_string = keystr;

  } else {
    gchar *p = w_current->keyaccel_string;
    w_current->keyaccel_string = g_strconcat (p, " ", keystr, NULL);
    g_free (p);
    g_free (keystr);
  }

  /* Update status bar */
  i_show_state(w_current, NULL);

  /* Build and evaluate Scheme expression. */
  scm_dynwind_begin (0);
  g_dynwind_window (w_current);
  s_expr = scm_list_2 (press_key_sym, s_key);
  s_retval = g_scm_eval_protected (s_expr, scm_interaction_environment ());
  scm_dynwind_end ();

  /* If the keystroke was not part of a prefix, start a timer to clear
   * the status bar display. */
  if (w_current->keyaccel_string_source_id) {
    /* Cancel any existing timers that haven't fired yet. */
    GSource *timer =
      g_main_context_find_source_by_id (NULL,
                                        w_current->keyaccel_string_source_id);
    g_source_destroy (timer);
    w_current->keyaccel_string_source_id = 0;
  }
  if (!scm_is_eq (s_retval, prefix_sym)) {
    w_current->keyaccel_string_source_id =
      g_timeout_add(400, clear_keyaccel_string, w_current);
  }

  return !scm_is_false (s_retval);
}

/*! \brief Exports the keymap in Scheme to a GtkListStore
 *  \par Function Description
 *  This function converts the list of key sequence/action pairs
 *  returned by the Scheme function \c dump-global-keymap into a
 *  GtkListStore with two columns.  The first column contains the name
 *  of the action executed by the keybinding as a string, and the
 *  second contains the keybinding itself as a string suitable for
 *  display.
 *
 *  The returned value must be freed by caller.
 *
 *  \return A GtkListStore containing keymap data.
  */
GtkListStore *
g_keys_to_list_store (void)
{
  SCM s_expr;
  SCM s_lst;
  SCM s_iter;
  GtkListStore *list_store;

  /* Call Scheme procedure to dump global keymap into list */
  s_expr = scm_list_1 (scm_from_utf8_symbol ("dump-global-keymap"));
  s_lst = g_scm_eval_protected (s_expr, scm_interaction_environment ());

  g_return_val_if_fail (scm_is_true (scm_list_p (s_lst)), NULL);

  /* Convert to  */
  scm_dynwind_begin (0);
  list_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  scm_dynwind_unwind_handler (g_object_unref, list_store, 0);

  for (s_iter = s_lst; !scm_is_null (s_iter); s_iter = scm_cdr (s_iter)) {
    SCM s_binding = scm_caar (s_iter);
    SCM s_keys = scm_cdar (s_iter);
    char *binding, *keys;
    GtkTreeIter iter;

    scm_dynwind_begin (0);

    binding = scm_to_utf8_string (s_binding);
    scm_dynwind_free (binding);

    keys = scm_to_utf8_string (s_keys);
    scm_dynwind_free (keys);

    gtk_list_store_insert_with_values (list_store, &iter, -1,
                                       0, binding,
                                       1, keys,
                                       -1);

    scm_dynwind_end ();
  }

  scm_dynwind_end ();
  return list_store;
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

