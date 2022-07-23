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


/*! \brief Test if a key value is valid.
 * \par Function Description
 * Test if the event key value \a keyval is valid for key binding.
 * This is a less restrictive version of gtk_accelerator_valid()
 * from GTK 2.
 *
 * \param keyval     The key that was pressed.
 *
 * \return keyval if it is valid for keybinding, otherwise 0.
 */
guint
schematic_keys_verify_keyval (guint keyval)
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
  if (keyval <= 0xFF)
  {
    return (keyval >= 0x20) ? keyval : 0;
  }

  /* Exclude special & modifier keys */
  val = invalid_keyvals;
  while (*val)
  {
    if (keyval == *val++) return 0;
  }

  return keyval;
}


/*! \brief Reset the current key sequence.
 * \par Function Description
 * If any prefix keys are stored in the current key sequence, clears
 * them.
 *
 * \param w_current  The active #GschemToplevel context.
 */
void
schematic_keys_reset (GschemToplevel *w_current)
{
  SCM s_expr = scm_list_1 (scm_from_utf8_symbol ("reset-keys"));

  /* Reset the status bar */
  schematic_window_set_keyaccel_string (w_current, NULL);
  i_show_state(w_current, NULL);

  /* Reset the Scheme keybinding state */
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  g_dynwind_window (w_current);
  g_scm_eval_protected (s_expr, scm_interaction_environment ());
  scm_dynwind_end ();
}


/*! \brief Obtain key value of a #GdkEventKey event.
 * \par Function Description
 * Returns the key value specified by \a event always translated
 * into lower case.
 *
 * \param [in] event The #GdkEventKey structure.
 * \return The obtained key value.
 */
guint
schematic_keys_get_event_keyval (GdkEventKey *event)
{
  guint upper, lower;

  g_return_val_if_fail (event != NULL, 0);

  /* Always process key as lower case */
  gdk_keyval_convert_case (event->keyval, &lower, &upper);

  return lower;
}


/*! \brief Obtain key modifiers of a #GdkEventKey event.
 * \par Function Description
 * Evaluates the key modifiers specified by \a event using the
 * current keymap.  The function acts as follows:
 * - Gets the keyval and modifiers of the event.
 * - Modifies the modifiers taken into account the current event
 *   state along with the default modifier mask.
 * - Based on Caps Lock state and keyval (lower or upper case),
 *   gets the real state of the Shift modifier and changes the
 *   modifiers according to the result.
 * - Finally applies GDK_MODIFIER_MASK to the modifiers to filter
 *   out unwanted cruft and returns the resulting value.
 *
 * \param [in] event      The #GdkEventKey structure.
 * \return The obtained key modifiers.
 */
guint
schematic_keys_get_event_modifiers (GdkEventKey *event)
{
  GdkKeymap *keymap;
  GdkDisplay *display;
  guint key, mods, caps, upper, lower;
  GdkModifierType consumed_modifiers;

  g_return_val_if_fail (event != NULL, 0);

  display = gdk_window_get_display (event->window);
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
  mods = (event->state
          & gtk_accelerator_get_default_mod_mask ()
          & ~consumed_modifiers);

  /* Handle Caps Lock. The idea is to obtain the same keybindings
   * whether Caps Lock is enabled or not. */
  gdk_keyval_convert_case (key, &lower, &upper);
  if (upper != lower) {
    caps = gdk_keymap_get_caps_lock_state (keymap);
    if ((caps && (key == lower)) || (!caps && (key == upper))) {
      mods |= GDK_SHIFT_MASK;
    }
  }

  return mods & GDK_MODIFIER_MASK;
}
