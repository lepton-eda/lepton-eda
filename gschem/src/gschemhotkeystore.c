/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#include <config.h>
#include "gschem.h"

enum {
  COLUMN_ICON = 0,
  COLUMN_LABEL,
  COLUMN_KEYS,
  NUM_COLUMNS,
};

#define HELPER_FUNC_NAME "%gschem-hotkey-store/dump-global-keymap"

static void gschem_hotkey_store_dispose (GObject *object);
static void gschem_hotkey_store_schedule_rebuild (GschemHotkeyStore *store);
static gboolean gschem_hotkey_store_rebuild (GschemHotkeyStore *store);
static void gschem_hotkey_store_action_property_handler (GschemHotkeyStore *store,
                                                          SCM s_args,
                                                          EdascmHookProxy *proxy);
static void gschem_hotkey_store_bind_keys_handler (GschemHotkeyStore *store,
                                                    SCM s_args,
                                                    EdascmHookProxy *proxy);

G_DEFINE_TYPE (GschemHotkeyStore, gschem_hotkey_store, GTK_TYPE_LIST_STORE);

/*! Initialise GschemHotkeyStore class */
static void
gschem_hotkey_store_class_init (GschemHotkeyStoreClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  /* Register functions with base class */
  gobject_class->dispose = gschem_hotkey_store_dispose;
}

/*! Initialise GschemHotkeyStore instance */
static void
gschem_hotkey_store_init (GschemHotkeyStore *store)
{
  GType column_types[GSCHEM_HOTKEY_STORE_NUM_COLUMNS]
    = { G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING, };

  /* This list store contains the hotkey data */
  gtk_list_store_set_column_types (GTK_LIST_STORE (store),
                                   GSCHEM_HOTKEY_STORE_NUM_COLUMNS,
                                   column_types);

  /* Make sure we can get notified when actions or keybindings
   * change */
  store->action_hook_proxy =
    g_hook_new_proxy_by_name ("%action-property-hook");
  g_signal_connect_swapped (store->action_hook_proxy, "run",
                            G_CALLBACK (gschem_hotkey_store_action_property_handler),
                            store);

  store->keymap_hook_proxy =
    g_hook_new_proxy_by_name ("%bind-keys-hook");
  g_signal_connect_swapped (store->keymap_hook_proxy, "run",
                            G_CALLBACK (gschem_hotkey_store_bind_keys_handler),
                            store);

  /* Finally, carry out initial rebuild of the treeview's backing
   * store */
  gschem_hotkey_store_schedule_rebuild (store);
}

/*! Dispose of a GschemHotkeyStore instance.  Drop all references to
 * other GObjects, but keep the instance otherwise intact. May be run
 * multiple times (due to reference loops).
 */
static void
gschem_hotkey_store_dispose (GObject *object)
{
  GschemHotkeyStore *store = GSCHEM_HOTKEY_STORE (object);

  if (store->action_hook_proxy) {
    g_object_unref (store->action_hook_proxy);
    store->action_hook_proxy = NULL;
  }

  if (store->keymap_hook_proxy) {
    g_object_unref (store->keymap_hook_proxy);
    store->keymap_hook_proxy = NULL;
  }

  /* Chain up to the parent class */
  G_OBJECT_CLASS (gschem_hotkey_store_parent_class)->dispose (object);
}

/*! Schedule a list view rebuild to occur next time the GLib main loop
 * is idle. */
static void
gschem_hotkey_store_schedule_rebuild (GschemHotkeyStore *store)
{
  if (store->rebuild_source_id) return;

  store->rebuild_source_id =
    g_idle_add ((GSourceFunc) gschem_hotkey_store_rebuild,
                store);
}

/*! Rebuild the list view. Calls into Scheme to generate a list of
 * current keybindings, and uses it to update the GtkListStore that
 * backs the list of key bindings. */
static gboolean
gschem_hotkey_store_rebuild (GschemHotkeyStore *store)
{
  static SCM s_expr = SCM_UNDEFINED;
  SCM s_lst, s_iter;

  g_assert (GSCHEM_IS_HOTKEY_STORE (store));

  /* First run the Scheme function to dump the global keymap */
  if (scm_is_eq (s_expr, SCM_UNDEFINED)) {
    s_expr = scm_permanent_object (scm_list_1 (scm_from_utf8_symbol (HELPER_FUNC_NAME)));
  }
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

    scm_dynwind_begin (0);

    binding = scm_to_utf8_string (s_binding);
    scm_dynwind_free (binding);

    keys = scm_to_utf8_string (s_keys);
    scm_dynwind_free (keys);

    if (scm_is_true (s_icon)) {
      icon = scm_to_utf8_string (s_icon);
      scm_dynwind_free (icon);
    }

    gtk_list_store_insert_with_values (GTK_LIST_STORE (store), &iter, -1,
                                       GSCHEM_HOTKEY_STORE_COLUMN_LABEL, binding,
                                       GSCHEM_HOTKEY_STORE_COLUMN_KEYS, keys,
                                       GSCHEM_HOTKEY_STORE_COLUMN_ICON, icon,
                                       -1);

    scm_dynwind_end ();
  }

  store->rebuild_source_id = 0;
  return FALSE;
}

/*! Scheme-level hook handler for %action-property-hook.  Triggers
 * rebuilding of the list of hotkeys, but only if the property that
 * was changed was the label or icon (since those are the properties
 * that are actually used in the hotkey display).
 */
static void
gschem_hotkey_store_action_property_handler (GschemHotkeyStore *store,
                                             SCM s_args,
                                             EdascmHookProxy *proxy)
{
  static SCM label_sym = SCM_UNDEFINED;
  static SCM icon_sym = SCM_UNDEFINED;
  SCM s_key;
  gboolean rebuild = FALSE;

  g_assert (GSCHEM_IS_HOTKEY_STORE (store));

  /* Unpack Scheme value before making any changes to the store.
   * args should be a list of the form (action key value).  We only
   * want to rebuild the store if the change is to a label or to an
   * icon. */
  if (scm_is_eq (label_sym, SCM_UNDEFINED)) {
    label_sym = scm_permanent_object (scm_from_utf8_symbol ("label"));
    icon_sym = scm_permanent_object (scm_from_utf8_symbol ("icon"));
  }
  /* Don't even bother checking that there's a well-formed argument
   * list; if there isn't, you've got bigger problems! */
  s_key = scm_cadr (s_args);
  rebuild = (scm_is_eq (s_key, label_sym) || scm_is_eq (s_key, icon_sym));

  if (rebuild) gschem_hotkey_store_schedule_rebuild (store);
}

/*! Scheme-level hook handler for %bind-keys-hook.  Triggers
 * rebuilding the list of hotkeys, but only if the %global-keymap was
 * modified.
 */
static void
gschem_hotkey_store_bind_keys_handler (GschemHotkeyStore *store,
                                       SCM s_args,
                                       EdascmHookProxy *proxy)
{
  static SCM global_keymap_sym = SCM_UNDEFINED;
  gboolean rebuild = FALSE;

  g_assert (GSCHEM_IS_HOTKEY_STORE (store));

  if (scm_is_eq (global_keymap_sym, SCM_UNDEFINED)) {
    global_keymap_sym =
      scm_permanent_object (scm_from_utf8_symbol ("%global-keymap"));
  }
  /* Rather brute-force approach to checking whether the affected
   * keymap is the global keymap */
  rebuild = (scm_is_eq (scm_car (s_args),
                        g_scm_eval_protected (global_keymap_sym,
                                              SCM_UNDEFINED)));

  if (rebuild) gschem_hotkey_store_schedule_rebuild (store);
}

GschemHotkeyStore *
gschem_hotkey_store_new (void)
{
  return g_object_new (GSCHEM_TYPE_HOTKEY_STORE, NULL);
}
