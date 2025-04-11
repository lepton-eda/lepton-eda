/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2015 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include "schematic.h"


#define HELPER_FUNC_NAME "%gschem-hotkey-store/dump-global-keymap"


G_DEFINE_TYPE (SchematicHotkeyStore,
               schematic_hotkey_store,
               GTK_TYPE_LIST_STORE);


/*! Initialise SchematicHotkeyStore class */
static void
schematic_hotkey_store_class_init (SchematicHotkeyStoreClass *klass)
{
}



/*! Initialise SchematicHotkeyStore instance */
static void
schematic_hotkey_store_init (SchematicHotkeyStore *store)
{
  GType column_types[SCHEMATIC_HOTKEY_STORE_NUM_COLUMNS]
    = { G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, };

  /* This list store contains the hotkey data */
  gtk_list_store_set_column_types (GTK_LIST_STORE (store),
                                   SCHEMATIC_HOTKEY_STORE_NUM_COLUMNS,
                                   column_types);
}


/*! \brief Clear the hotkey list store.
 *  \par Function description
 *  Removes all rows from the hotkey list store.
 *
 *  \param [in,out] store The #SchematicHotkeyStore instance to
 *                        clear.
 */
void
schematic_hotkey_store_clear (SchematicHotkeyStore *store)
{
  gtk_list_store_clear (GTK_LIST_STORE (store));
}


/*! \brief Append a row with given data to a hotkey list store
 *  \par Function description
 *  Appends a new row with given values to \a store.  The data
 *  defines the name of a function to describe, the shortcut set
 *  for calling it, and the name of an icon which will be
 *  displayed in the list store row for that function.
 *
 *  \param [in,out] store The #SchematicHotkeyStore instance.
 *  \param [in] binding The function name.
 *  \param [in] keys The shortcut.
 *  \param [in] icon The icon name.
 */
void
schematic_hotkey_store_append_row (SchematicHotkeyStore *store,
                                   char *binding,
                                   char *keys,
                                   char *icon)
{
  GtkTreeIter iter;

  gtk_list_store_insert_with_values (GTK_LIST_STORE (store), &iter, -1,
                                     SCHEMATIC_HOTKEY_STORE_COLUMN_LABEL, binding,
                                     SCHEMATIC_HOTKEY_STORE_COLUMN_KEYS, keys,
                                     SCHEMATIC_HOTKEY_STORE_COLUMN_ICON, icon,
                                     -1);

}


/*! Rebuild the list view. Calls into Scheme to generate a list of
 * current keybindings, and uses it to update the GtkListStore that
 * backs the list of key bindings. */
gboolean
schematic_hotkey_store_rebuild (SchematicHotkeyStore *store)
{
  SCM s_expr = SCM_UNDEFINED;
  SCM s_lst, s_iter;

  g_assert (SCHEMATIC_IS_HOTKEY_STORE (store));

  /* First run the Scheme function to dump the global keymap */
  s_expr = scm_list_1 (scm_from_utf8_symbol (HELPER_FUNC_NAME));
  s_lst = g_scm_eval_protected (s_expr, SCM_UNDEFINED);

  g_return_val_if_fail (scm_is_true (scm_list_p (s_lst)), FALSE);

  /* If it worked, then rebuild the keymap */
  schematic_hotkey_store_clear (store);

  for (s_iter = s_lst; !scm_is_null (s_iter); s_iter = scm_cdr (s_iter)) {
    SCM s_info = scm_car (s_iter);
    SCM s_binding = scm_car (s_info);
    SCM s_keys = scm_cadr (s_info);
    SCM s_icon = scm_caddr (s_info);
    char *binding, *keys, *icon = NULL;

    scm_dynwind_begin ((scm_t_dynwind_flags) 0);

    binding = scm_to_utf8_string (s_binding);
    scm_dynwind_free (binding);

    keys = scm_to_utf8_string (s_keys);
    scm_dynwind_free (keys);

    if (scm_is_true (s_icon)) {
      icon = scm_to_utf8_string (s_icon);
      scm_dynwind_free (icon);
    }

    schematic_hotkey_store_append_row (store, binding, keys, icon);

    scm_dynwind_end ();
  }

  return FALSE;

} /* schematic_hotkey_store_rebuild() */



SchematicHotkeyStore*
schematic_hotkey_store_new ()
{
  return SCHEMATIC_HOTKEY_STORE (g_object_new (SCHEMATIC_TYPE_HOTKEY_STORE, NULL));
}
