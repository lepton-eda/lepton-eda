/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

#include <gdk/gdkkeysyms.h>


/*! Contains the smob tag for key smobs */
static scm_t_bits g_key_smob_tag;

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
    GDK_KEY_Shift_L, GDK_KEY_Shift_R, GDK_KEY_Shift_Lock, GDK_KEY_Caps_Lock, GDK_KEY_ISO_Lock,
    GDK_KEY_Control_L, GDK_KEY_Control_R, GDK_KEY_Meta_L, GDK_KEY_Meta_R,
    GDK_KEY_Alt_L, GDK_KEY_Alt_R, GDK_KEY_Super_L, GDK_KEY_Super_R, GDK_KEY_Hyper_L, GDK_KEY_Hyper_R,
    GDK_KEY_ISO_Level3_Shift, GDK_KEY_ISO_Next_Group, GDK_KEY_ISO_Prev_Group,
    GDK_KEY_ISO_First_Group, GDK_KEY_ISO_Last_Group,
    GDK_KEY_Mode_switch, GDK_KEY_Num_Lock, GDK_KEY_Multi_key,
    GDK_KEY_Scroll_Lock, GDK_KEY_Sys_Req,
    GDK_KEY_Tab, GDK_KEY_ISO_Left_Tab, GDK_KEY_KP_Tab,
    GDK_KEY_First_Virtual_Screen, GDK_KEY_Prev_Virtual_Screen,
    GDK_KEY_Next_Virtual_Screen, GDK_KEY_Last_Virtual_Screen,
    GDK_KEY_Terminate_Server, GDK_KEY_AudibleBell_Enable,
    0
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
 * Create and return a new lepton-schematic key object from a \a
 * keyval and a set of \a modifiers.  If the key combination is
 * invalid, return SCM_BOOL_F.
 *
 * \param keyval     the pressed key.
 * \param modifiers  the active modifiers for the key.
 *
 * \return a new bindable key object, or SCM_BOOL_F.
 */
GschemKey*
g_make_key_struct (guint keyval,
                   GdkModifierType modifiers)
{
  GschemKey *k = NULL;
  if (g_key_is_valid (keyval, modifiers))
  {
    k = g_new0 (GschemKey, 1);
    k->keyval = keyval;
    k->modifiers = (GdkModifierType) (modifiers & GDK_MODIFIER_MASK);
  }
  return k;
}


SCM
g_make_key (GschemKey *k)
{
  SCM result = SCM_BOOL_F;
  if (k != NULL)
  {
    SCM_NEWSMOB (result, g_key_smob_tag, k);
  }
  return result;
}


/*! \brief Test if a Scheme value is a bindable key object.
 *
 * \param key_s the value to test
 * \return TRUE if the value is a key, otherwise FALSE.
 */
gboolean
schematic_key_is_key (SCM key_s)
{
  return SCM_SMOB_PREDICATE (g_key_smob_tag, key_s);
}


/*! \brief Return the \a str field value of a #GschemKey object.
 *
 * \param key The #GschemKey object.
 * \return The value of the \a str field.
 */
gchar*
schematic_key_get_str (GschemKey *key)
{
  g_return_val_if_fail (key != NULL, NULL);

  return key->str;
}


/*! \brief Sets the \a str field value of a #GschemKey object to a
 *  new value.
 *
 * \param key The #GschemKey object.
 * \param key The string to set.
 */
void
schematic_key_set_str (GschemKey *key, gchar* str)
{
  g_return_if_fail (key != NULL);

  if (key->str != NULL)
  {
    g_free (key->str);
  }

  key->str = g_strdup (str);
}


/*! \brief Return the \a disp_str field value of a #GschemKey object.
 *
 * \param key The #GschemKey object.
 * \return The value of the \a disp_str field.
 */
gchar*
schematic_key_get_disp_str (GschemKey *key)
{
  g_return_val_if_fail (key != NULL, NULL);

  return key->disp_str;
}


/*! \brief Sets the \a disp_str field value of a #GschemKey object to a
 *  new value.
 *
 * \param key The #GschemKey object.
 * \param key The string to set.
 */
void
schematic_key_set_disp_str (GschemKey *key, gchar* disp_str)
{
  g_return_if_fail (key != NULL);

  if (key->disp_str != NULL)
  {
    g_free (key->disp_str);
  }

  key->disp_str = g_strdup (disp_str);
}


/*! \brief Return the \a keyval field value of a #GschemKey object.
 *
 * \param key The #GschemKey object.
 * \return The value of the \a keyval field.
 */
guint
schematic_key_get_keyval (GschemKey *key)
{
  g_return_val_if_fail (key != NULL, 0);

  return key->keyval;
}


/*! \brief Return the \a modifiers field value of a #GschemKey object.
 *
 * \param key The #GschemKey object.
 * \return The value of the \a modifiers field.
 */
GdkModifierType
schematic_key_get_modifiers (GschemKey *key)
{
  g_return_val_if_fail (key != NULL, (GdkModifierType) 0);

  return key->modifiers;
}

/*! \brief Convert a bindable key object SMOB to #GschemKey.
 */
GschemKey*
schematic_key_unwrap_key (SCM key_s)
{
  return (GschemKey *) SCM_SMOB_DATA (key_s);
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
  SCM_ASSERT (schematic_key_is_key (smob),
              smob,
              SCM_ARG1,
              "g_key_print");

  GschemKey *key = schematic_key_unwrap_key (smob);

  if (schematic_key_get_disp_str (key) == NULL)
  {
    schematic_key_set_disp_str (key, gtk_accelerator_get_label (schematic_key_get_keyval (key),
                                                                schematic_key_get_modifiers (key)));
  }
  SCM s = scm_from_utf8_string (schematic_key_get_disp_str (key));

  scm_puts ("#<gschem-key ", port);
  scm_write (s, port);
  scm_puts (">", port);

  /* Non-zero means success */
  return 1;
}

/* \brief Test if two key combinations are equivalent.
 * \par Function Description
 * Tests if the two lepton-schematic key objects \a a and \a b
 * represent the same key event.
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
 * Destroys the contents of a lepton-schematic key object on
 * garbage collection.
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


/*! \brief Clear the current key accelerator string.
 * \par Function Description
 * This function clears the current keyboard accelerator string in
 * the status bar of the relevant toplevel.  Called some time after a
 * keystroke is pressed.  If the current key sequence was a prefix,
 * let it persist.
 *
 * \param [in] data a pointer to the GschemToplevel to update.
 * \return FALSE (this is a one-shot timer).
 */
static gboolean clear_keyaccel_string(gpointer data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (data);

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


/*! \brief Update timer for clearing the current key accelerator string.
 * \par Function Description
 * If a timer responsible for clearing key accelerator string in
 * the status bar has been started, the function stops it.  If \a
 * start_timer is TRUE, it runs a new timer for this.  It should
 * be FALSE if the current key sequence is a prefix which should
 * persist.
 *
 * \param [in] w_current The GschemToplevel to update.
 * \param [in] start_timer If a new timer should be started.
 */
void
schematic_keys_update_keyaccel_timer (GschemToplevel *w_current,
                                      gboolean start_timer)
{
  if (w_current->keyaccel_string_source_id)
  {
    /* Cancel any existing timers that haven't fired yet. */
    GSource *timer =
      g_main_context_find_source_by_id (NULL,
                                        w_current->keyaccel_string_source_id);
    if (timer != NULL)
    {
      g_source_destroy (timer);
    }
    w_current->keyaccel_string_source_id = 0;
  }
  if (start_timer)
  {
    w_current->keyaccel_string_source_id =
      g_timeout_add (400, clear_keyaccel_string, w_current);
  }
}


/*! \brief Reset the current key sequence.
 * \par Function Description
 * If any prefix keys are stored in the current key sequence, clears
 * them.
 *
 * \param w_current  The active #GschemToplevel context.
 */
void
g_keys_reset (GschemToplevel *w_current)
{
  SCM s_expr = scm_list_1 (scm_from_utf8_symbol ("reset-keys"));

  /* Reset the status bar */
  g_free (w_current->keyaccel_string);
  w_current->keyaccel_string = NULL;
  i_show_state(w_current, NULL);

  /* Reset the Scheme keybinding state */
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);
  g_scm_eval_protected (s_expr, scm_interaction_environment ());
  scm_dynwind_end ();
}

/*! \brief Evaluate a user keystroke.
 * \par Function Description
 * Evaluates the key combination specified by \a event using the
 * current keymap.  Updates the lepton-schematic status bar with
 * the current key sequence.
 *
 * \param w_current  The active #GschemToplevel context.
 * \param event      A GdkEventKey structure.
 *
 * \return 1 if a binding was found for the keystroke, 0 otherwise.
 */
int
g_keys_execute(GschemToplevel *w_current, GdkEventKey *event)
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
                                       (GdkModifierType) event->state,
                                       event->group,
                                       NULL,
                                       NULL,
                                       NULL,
                                       &consumed_modifiers);

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
  if (!g_key_is_valid (key, (GdkModifierType) mods)) {
    return FALSE;
  }

  /* Update key hint string for status bar. */
  gchar *keystr = gtk_accelerator_get_label (key, (GdkModifierType) mods);

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

  GschemKey *k = g_make_key_struct (key, (GdkModifierType) mods);

  /* Create Scheme key value */
  s_key = g_make_key (k);

  /* Build and evaluate Scheme expression. */
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);
  s_expr = scm_list_2 (scm_from_utf8_symbol ("press-key"), s_key);
  s_retval = g_scm_eval_protected (s_expr, scm_interaction_environment ());
  scm_dynwind_end ();

  /* If the keystroke was not part of a prefix, start a timer to
   * clear the status bar display. */
  gboolean is_prefix = scm_is_eq (s_retval, scm_from_utf8_symbol ("prefix"));
  schematic_keys_update_keyaccel_timer (w_current, !is_prefix);

  return !scm_is_false (s_retval);
}

/*! \brief Create the (schematic core keymap) Scheme module
 * \par Function Description
 * Defines procedures in the (schematic core keymap) module.  The module
 * can be accessed using (use-modules (schematic core keymap)).
 */
static void
init_module_schematic_core_keymap (void *unused)
{
  /* Register the functions */
  #include "g_keys.x"

  /* Add them to the module's public definitions */
  scm_c_export (NULL);
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

  scm_c_define_module ("schematic core keymap",
                       (void (*)(void*)) init_module_schematic_core_keymap,
                       NULL);
}
