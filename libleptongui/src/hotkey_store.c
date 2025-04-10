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
  gtk_list_store_clear (GTK_LIST_STORE (store));

  for (s_iter = s_lst; !scm_is_null (s_iter); s_iter = scm_cdr (s_iter)) {
    SCM s_info = scm_car (s_iter);
    SCM s_binding = scm_car (s_info);
    SCM s_keys = scm_cadr (s_info);
    SCM s_icon = scm_caddr (s_info);
    char *binding, *keys, *icon = NULL;
    GtkTreeIter iter;

    scm_dynwind_begin ((scm_t_dynwind_flags) 0);

    binding = scm_to_utf8_string (s_binding);
    scm_dynwind_free (binding);

    keys = scm_to_utf8_string (s_keys);
    scm_dynwind_free (keys);

    if (scm_is_true (s_icon)) {
      icon = scm_to_utf8_string (s_icon);
      scm_dynwind_free (icon);
    }

    gtk_list_store_insert_with_values (GTK_LIST_STORE (store), &iter, -1,
                                       SCHEMATIC_HOTKEY_STORE_COLUMN_LABEL, binding,
                                       SCHEMATIC_HOTKEY_STORE_COLUMN_KEYS, keys,
                                       SCHEMATIC_HOTKEY_STORE_COLUMN_ICON, icon,
                                       -1);

    scm_dynwind_end ();
  }

  return FALSE;

} /* schematic_hotkey_store_rebuild() */



SchematicHotkeyStore*
schematic_hotkey_store_new ()
{
  SchematicHotkeyStore *store =
    SCHEMATIC_HOTKEY_STORE (g_object_new (SCHEMATIC_TYPE_HOTKEY_STORE, NULL));

  schematic_hotkey_store_rebuild (store);

  return store;
}
