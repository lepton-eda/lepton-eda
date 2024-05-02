/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#include "config.h"

#ifdef HAVE_STRING_H
#include <string.h>
#endif


#include "gschem.h"

#include <glib/gstdio.h>

#define RECENT_MENU_ITEM_NAME "Open Recen_t"

struct PopupEntry
{
  const gchar* name;
  const gchar* action;
#ifdef ENABLE_GTK3
  const gchar* icon_name;
#else /* GTK2 */
  const gchar* stock_id;
#endif
};

static struct PopupEntry popup_items[] =
{
  { N_("Add Co_mponent..."), "&add-component", "insert-symbol" },
  { N_("Add Te_xt..."),      "&add-text",      "insert-text" },
  { N_("Add _Attribute..."), "&add-attribute", "insert-attribute" },
  { "SEPARATOR", NULL, NULL },
  { N_("Add _Net"), "&add-net", "insert-net" },
  { N_("Add _Bus"), "&add-bus", "insert-bus" },
  { "SEPARATOR", NULL, NULL },
  { N_("Cu_t"),    "&clipboard-cut",   "gtk-cut" },
  { N_("_Copy"),   "&clipboard-copy",  "gtk-copy" },
  { N_("_Paste"),  "&clipboard-paste", "gtk-paste" },
  { N_("_Delete"), "&edit-delete",     "gtk-delete" },
  { "SEPARATOR", NULL, NULL },
  { N_("_Edit..."),              "&edit-edit",              NULL },
  { N_("Ed_it Text..."),         "&edit-text",              "gtk-edit" },
  { N_("_Object Properties..."), "&edit-object-properties", "gtk-properties" },
  { "SEPARATOR", NULL, NULL },
  { N_("Hierarchy: Down _Schematic"), "&hierarchy-down-schematic", "gtk-go-down" },
  { N_("Hierarchy: Down S_ymbol"),    "&hierarchy-down-symbol",    "gtk-goto-bottom" },
  { N_("Hierarchy: _Up"),             "&hierarchy-up",             "gtk-go-up" },

  { NULL, NULL, NULL }, /* Guard */
};


/*! \brief Callback function for menu items. Execute action \a action.
 */
#ifdef ENABLE_GTK3
static void
g_menu_execute (GSimpleAction* action,
                GVariant *parameter,
                gpointer user_data)
{
  const gchar *action_name = g_action_get_name (G_ACTION (action));
  GschemToplevel *w_current = (GschemToplevel *) user_data;

  g_action_eval_by_name (w_current, action_name);
}

#else
static void g_menu_execute(GtkAction *action, gpointer user_data)
{
  const gchar *action_name = gtk_action_get_name (action);
  GschemToplevel *w_current = (GschemToplevel *) user_data;
  g_action_eval_by_name (w_current, action_name);
}
#endif


GtkWidget*
make_separator_menu_item ()
{
#ifdef ENABLE_GTK3
  return gtk_separator_menu_item_new ();
#else
  return gtk_menu_item_new();
#endif
}


#ifdef ENABLE_GTK3
GSimpleAction*
#else
GschemAction*
#endif
make_menu_action (const char *action_name,
                  const char *menu_item_name,
                  const char *menu_item_keys,
                  const char *menu_item_stock,
                  GschemToplevel *w_current)
{
#ifndef ENABLE_GTK3
  GtkStockItem stock_info;
#endif

#ifdef ENABLE_GTK3
  GSimpleAction* action = g_simple_action_new (action_name, NULL);

  /* Look up icon in the icon theme. */
//  gtk_action_set_icon_name (GTK_ACTION (action), menu_item_stock);
#else /* GTK2 */
  GschemAction *action =
    GSCHEM_ACTION (g_object_new (GSCHEM_TYPE_ACTION,
                                 "name", action_name,
                                 "label", menu_item_name,
                                 "tooltip", menu_item_name,
                                 "multikey-accel", menu_item_keys,
                                 NULL));
  /* If stock name corresponds to a GTK stock item, then use it.
   * Otherwise, look it up in the icon theme. */
  if (menu_item_stock != NULL &&
      gtk_stock_lookup (menu_item_stock, &stock_info))
  {
    gtk_action_set_stock_id (GTK_ACTION (action), menu_item_stock);
  }
  else
  {
    gtk_action_set_icon_name (GTK_ACTION (action), menu_item_stock);
  }
#endif

  g_signal_connect (G_OBJECT (action),
                    "activate",
                    G_CALLBACK (g_menu_execute),
                    w_current);

  return action;
}

#ifdef ENABLE_GTK3
static void
on_menu_item_activate (GtkMenuItem *item,
                       gpointer data)
{
  g_signal_emit_by_name (G_ACTION (data), "activate", NULL);
}


GtkWidget*
lepton_action_create_menu_item (GSimpleAction* action,
                                const gchar *label,
                                const gchar *shortcut)
{
  GtkWidget *item = gtk_menu_item_new_with_mnemonic (label);

  g_signal_connect (item,
                    "activate",
                    G_CALLBACK (&on_menu_item_activate),
                    action);

  GtkWidget *item_label = gtk_bin_get_child (GTK_BIN (item));

  /* make sure item_label is a GschemAccelLabel */
  if (item_label && !GSCHEM_IS_ACCEL_LABEL (item_label)) {
    gtk_container_remove (GTK_CONTAINER (item), item_label);
    item_label = NULL;
  }

  if (item_label == NULL) {
    /* char *label_string; */
    /* g_object_get (action, "label", &label_string, NULL); */
    g_object_new (GSCHEM_TYPE_ACCEL_LABEL,
                  "use-underline", TRUE,
                  "xalign", 0.0,
                  "visible", TRUE,
                  "parent", item,
                  "label", label,
                  "accel-string", shortcut,
                  NULL);
    /* g_free (label_string); */
  }

  return item;
}

#else
GtkWidget*
lepton_action_create_menu_item (GtkAction *action,
                                gpointer data1,
                                gpointer data2)
{
  return gtk_action_create_menu_item (GTK_ACTION (action));
}
#endif


void
lepton_menu_set_action_data (GtkWidget *menu,
                             const char *action_name,
                             GtkWidget *menu_item,
                             gpointer data)
{
#ifdef ENABLE_GTK3
      g_object_set_data (G_OBJECT (menu), action_name, menu_item);
#else
      g_object_set_data (G_OBJECT (menu), action_name, GTK_ACTION (data));
#endif
}


/*! \brief Create and return the popup menu widget.
 *
 *  \par Function Description
 *  The key/value data (see g_object_set_data()/g_object_get_data())
 *  associated with the menu widget created by this function:
 *  - key:   action name, string, e.g. "&edit-object-properties"
 *  - value: pointer to a corresponding GschemAction object
 */
GtkWidget*
schematic_window_create_main_popup_menu (GschemToplevel* w_current)
{
  GtkWidget *menu_item;
  GtkWidget *menu;
  int i;

  menu = gtk_menu_new ();

  for (i = 0; popup_items[i].name != NULL; i++) {
    struct PopupEntry e = popup_items[i];

    /* No action --> add a separator */
    if (e.action == NULL) {
      menu_item = make_separator_menu_item ();
      gtk_widget_show (menu_item);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), menu_item);
      continue;
    }

#ifdef ENABLE_GTK3
    GSimpleAction* action = g_simple_action_new (e.action, NULL);
    menu_item = lepton_action_create_menu_item (action, gettext (e.name), NULL);

    g_signal_connect (action,
                      "activate",
                      G_CALLBACK (g_menu_execute),
                      w_current);
#else /* GTK2 */
    GschemAction *action;
    /* Don't bother showing keybindings in the popup menu */
    action = make_menu_action (e.action,
                               gettext (e.name),
                               NULL,
                               e.stock_id,
                               w_current);
    menu_item = gtk_action_create_menu_item (GTK_ACTION (action));
#endif
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menu_item);

    lepton_menu_set_action_data (menu, e.action, menu_item, action);
  }

  gtk_widget_show_all (menu);

  w_current->popup_menu = menu;

  return menu;
}

/*! \brief Show the popup menu.
 *
 *  \note
 *  need to look at this... here and the setup
 */
gint do_popup (GschemToplevel *w_current, GdkEvent *event)
{
  GtkWidget *menu = (GtkWidget *) w_current->popup_menu;
  g_return_val_if_fail (menu != NULL, FALSE);

#ifdef ENABLE_GTK3
  gtk_menu_popup_at_pointer (GTK_MENU (menu), event);
#else
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  ((GdkEventButton*) event)->button,
                  ((GdkEventButton*) event)->time);
#endif

  return FALSE;
}

/*! \brief Enable/disable menu item linked to the action \a action_name.
 *
 *  \par Function Description
 *  Use the key/value data associated with the \a menu to find
 *  an action object, and if found, set its sensitivity (\a sensitive).
 *
 *  \param menu         Menu widget (menubar, popup_menu in st_gschem_toplevel)
 *  \param action_name  Action name (e.g. "&edit-object-properties")
 *  \param sensitive    Boolean, enable or disable the action
 */
void
x_menus_sensitivity (GtkWidget*   menu,
                     const gchar* action_name,
                     gboolean     sensitive)
{
#ifdef ENABLE_GTK3
  GtkWindow *win = gtk_application_get_active_window (GTK_APPLICATION (g_application_get_default ()));
  GAction *action = g_action_map_lookup_action (G_ACTION_MAP (win), action_name);

  GObject* obj = G_OBJECT (menu);
  gpointer data = g_object_get_data (obj, action_name);
  GtkWidget *menu_item = GTK_WIDGET (data);
  gtk_widget_set_sensitive (menu_item, sensitive);

  if (action != NULL)
  {
    g_simple_action_set_enabled (G_SIMPLE_ACTION (action), sensitive);
  }
  else
  {
    g_debug ("x_menus_sensitivity(): cannot find action [%s]", action_name);
  }

#else
  GObject* obj = G_OBJECT (menu);
  gpointer data = g_object_get_data (obj, action_name);

  GschemAction* action = (GschemAction*) data;
  if (action != NULL)
  {
    gtk_action_set_sensitive (GTK_ACTION (action), sensitive);
  }
  else
  {
    g_debug ("x_menus_sensitivity(): cannot find action [%s]", action_name);
  }
#endif
}

/*! \brief Return selected filename in recent-chooser.
 *
 *  \par Function Description
 *
 *  Will be called if element of recent-file-list is activated.
 *  Returns selected filename.  The caller must free the filename.
 *
 *  \param [in] chooser The recent-chooser instance.
 *  \param [in] w_current The current schematic window.
 *  \return The selected filename.
 */
char*
schematic_menu_recent_chooser_get_filename (GtkRecentChooser *chooser,
                                            GschemToplevel *w_current)
{
  gchar *uri;
  gchar *filename;

  uri = gtk_recent_chooser_get_current_uri (chooser);
  filename = g_filename_from_uri(uri, NULL, NULL);
  if (w_current->recent_manager != NULL) {
    gtk_recent_manager_add_item (w_current->recent_manager, uri);
  }

  g_free(uri);

  return filename;
}



/*! \brief Attach 'Open Recent' submenu to \a menuitem.
 */
void
x_menu_attach_recent_files_submenu (GschemToplevel* w_current,
                                    GtkWidget*      menuitem,
                                    GCallback callback_item_activated,
                                    gint max_items)
{
  GtkRecentFilter *recent_filter;
  GtkWidget *menuitem_file_recent_items;

  w_current->recent_manager = gtk_recent_manager_get_default ();

  menuitem_file_recent_items =
    gtk_recent_chooser_menu_new_for_manager (w_current->recent_manager);

  /* Show only schematic- and symbol-files (*.sch and *.sym) in list */
  recent_filter = gtk_recent_filter_new();
  gtk_recent_filter_add_mime_type(recent_filter, "application/x-lepton-schematic");
  gtk_recent_filter_add_mime_type(recent_filter, "application/x-lepton-symbol");
  gtk_recent_filter_add_pattern(recent_filter, "*.sch");
  gtk_recent_filter_add_pattern(recent_filter, "*.sym");
  gtk_recent_chooser_add_filter(GTK_RECENT_CHOOSER(menuitem_file_recent_items), recent_filter);

  gtk_recent_chooser_set_show_tips(GTK_RECENT_CHOOSER(menuitem_file_recent_items), TRUE);
  gtk_recent_chooser_set_sort_type(GTK_RECENT_CHOOSER(menuitem_file_recent_items),
                                   GTK_RECENT_SORT_MRU);

  gtk_recent_chooser_set_limit(GTK_RECENT_CHOOSER(menuitem_file_recent_items), max_items);

  gtk_recent_chooser_set_local_only(GTK_RECENT_CHOOSER(menuitem_file_recent_items), FALSE);
  gtk_recent_chooser_menu_set_show_numbers(GTK_RECENT_CHOOSER_MENU(menuitem_file_recent_items), TRUE);
  g_signal_connect (G_OBJECT (menuitem_file_recent_items),
                    "item-activated",
                    callback_item_activated,
                    w_current);

  if (menuitem == NULL)
    return;

  gtk_menu_item_set_submenu(GTK_MENU_ITEM(menuitem), menuitem_file_recent_items);
}
