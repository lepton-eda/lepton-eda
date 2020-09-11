/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
/*! \todo STILL NEED to clean up line lengths in aa and tr */

#include "config.h"

#include "gschem.h"



/***************** Start of help/keymapping dialog box **************/


/*! \brief Fix up displaying icons in list of hotkeys.
 * In gschem, we use both GTK's stock icons and also our own icons
 * that we add to the icon theme search path.  We identify each icon
 * by a single icon name, which might either name a GTK stock icon or
 * a theme icon.  To determine which icon to show, we first check if
 * there's a matching stock icon, and if one doesn't exist, we fall
 * back to looking in the theme.
 *
 * The GtkCellRendererPixbuf doesn't provide this capability.  If its
 * "icon-name" property is set, it doesn't look at stock items, but if
 * its "stock-id" property is set, it ignores the "icon-name" even if
 * no matching stock item exists.
 *
 * This handler hooks into the "notify::stock-id" signal in order to
 * implement the desired fallback behaviour.
 */
static void
x_dialog_hotkeys_cell_stock_id_notify (GObject *gobject,
                                       GParamSpec *pspec,
                                       gpointer user_data)
{
  gchar *stock_id = NULL;
  const gchar *new_icon_name = NULL;
  const gchar *new_stock_id = NULL;
  GtkStockItem stock_info;

  /* Decide whether the requested stock ID actually matches a stock
   * item */
  g_object_get (gobject,
                "stock-id", &stock_id,
                NULL);
  new_stock_id = stock_id;

  if (stock_id != NULL && !gtk_stock_lookup (stock_id, &stock_info)) {
    new_icon_name = stock_id;
    new_stock_id = NULL;
  }

  /* Fix up the cell renderer, making sure that this function doesn't
   * get called recursively. */
  g_signal_handlers_block_by_func (gobject,
                                   (gpointer) x_dialog_hotkeys_cell_stock_id_notify,
                                   NULL);
  g_object_set (gobject,
                "icon-name", new_icon_name,
                "stock-id", new_stock_id,
                NULL);
  g_signal_handlers_unblock_by_func (gobject,
                                     (gpointer) x_dialog_hotkeys_cell_stock_id_notify,
                                     NULL);

  g_free (stock_id);
}



static gboolean
filter (GtkTreeModel* model, GtkTreeIter* it, gpointer data)
{
  GtkEntry* ent = (GtkEntry*) data;
  const gchar* txt = gtk_entry_get_text (ent);

  if (g_strcmp0 (txt, "") == 0)
  {
    return TRUE;
  }

  gchar* cmd = NULL;
  gchar* key = NULL;
  gtk_tree_model_get (model, it, GSCHEM_HOTKEY_STORE_COLUMN_LABEL, &cmd, -1);
  gtk_tree_model_get (model, it, GSCHEM_HOTKEY_STORE_COLUMN_KEYS,  &key, -1);

  /* case-insensitive strings for comparison: */
  gchar* cmd_ci = g_utf8_casefold (cmd, strlen (cmd));
  gchar* key_ci = g_utf8_casefold (key, strlen (key));
  gchar* txt_ci = g_utf8_casefold (txt, strlen (txt));

  /* search for [txt_ci] substring: */
  gboolean found_cmd = strstr (cmd_ci, txt_ci) != NULL;
  gboolean found_key = strstr (key_ci, txt_ci) != NULL;

  g_free (cmd_ci);
  g_free (key_ci);
  g_free (txt_ci);

  g_free (cmd);
  g_free (key);

  return found_cmd || found_key;
}



static void
entry_changed (GtkEditable* editable, gpointer data)
{
  GtkTreeView* tree = GTK_TREE_VIEW (data);
  GtkTreeModel* model = gtk_tree_view_get_model (tree);

  gtk_tree_model_filter_refilter (GTK_TREE_MODEL_FILTER (model));
}



static void
filter_setup (GtkTreeView* tree, GtkTreeModel* model, GtkEntry* entry)
{
  GtkTreeModel* filter_model = gtk_tree_model_filter_new (model, NULL);

  gtk_tree_model_filter_set_visible_func(
    GTK_TREE_MODEL_FILTER (filter_model),
    &filter,
    entry,
    NULL);

  gtk_tree_view_set_model (tree, filter_model);

  g_signal_connect (entry,
                    "changed",
                    G_CALLBACK (&entry_changed),
                    tree);
}



/*! \brief "response" signal handler for hotkeys dialog
 */
static void
response (GtkWidget* widget, gint response, gpointer data)
{
  GschemToplevel* w_current = (GschemToplevel*) data;
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->hkwindow != NULL);

  gtk_widget_destroy (w_current->hkwindow);
  w_current->hkwindow = NULL;
}



/*! \brief Creates the hotkeys dialog
 *  \par Function Description
 *  This function creates the hotkey dialog and puts the list of hotkeys
 *  into it.
 */
void x_dialog_hotkeys (GschemToplevel *w_current)
{
  GtkWidget *vbox = NULL;
  GtkWidget *scrolled_win = NULL;
  GtkWidget *treeview = NULL;
  GtkTreeModel *store = NULL;
  GtkCellRenderer *renderer = NULL;
  GtkTreeViewColumn *column = NULL;

  if (w_current->hkwindow != NULL)
  {
    gtk_window_present (GTK_WINDOW (w_current->hkwindow));
    return;
  }


  w_current->hkwindow = gschem_dialog_new_with_buttons(
    _("Hotkeys"),
    GTK_WINDOW (w_current->main_window),
    (GtkDialogFlags) 0, /* not modal */
    "hotkeys", w_current,
    _("_Close"), GTK_RESPONSE_REJECT,
    NULL);

  g_signal_connect (G_OBJECT (w_current->hkwindow), "response",
                    G_CALLBACK (&response),
                    w_current);

  gtk_container_set_border_width (GTK_CONTAINER (w_current->hkwindow),
                                  DIALOG_BORDER_SPACING);
  gtk_widget_set_size_request (w_current->hkwindow, 300, 300);

  vbox = gtk_dialog_get_content_area (GTK_DIALOG (w_current->hkwindow));
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);


  /*
  *  filter text entry, label and tooltip:
  */
  GtkWidget* entry = gtk_entry_new();
  const gchar* txt = _("Start typing action name or keystroke to filter the list.\n"
                       "Type hotkeys as they are displayed in \"Keystroke(s)\" column.");
  gtk_widget_set_tooltip_text (entry, txt);

  GtkWidget* label = gtk_label_new_with_mnemonic (_("_Filter:"));
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry);

#ifdef ENABLE_GTK3
  GtkWidget* hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  GtkWidget* hbox = gtk_hbox_new (FALSE, 0);
#endif
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 5);
  gtk_box_pack_start (GTK_BOX (hbox), entry, TRUE,  TRUE,  5);
  gtk_box_pack_start (GTK_BOX (vbox), hbox,  FALSE, TRUE,  0);


  scrolled_win = gtk_scrolled_window_new (NULL, NULL);
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win, TRUE, TRUE, 0);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_win),
                                  GTK_POLICY_AUTOMATIC,
                                  GTK_POLICY_AUTOMATIC);

  /* the model */
  store = GTK_TREE_MODEL (gschem_hotkey_store_new ());

  /* the tree view */
  treeview = gtk_tree_view_new_with_model (store);
  gtk_container_add(GTK_CONTAINER(scrolled_win), treeview);

  /* the columns */
  /* The first column contains the action's icon (if one was set)
   * and its label. */
  renderer = gtk_cell_renderer_pixbuf_new ();
  column = gtk_tree_view_column_new_with_attributes (_("Action"),
                                                     renderer,
                                                     "stock-id",
                                                     GSCHEM_HOTKEY_STORE_COLUMN_ICON,
                                                     NULL);
  /* Fix things up to show stock icons *and* theme icons. */
  g_signal_connect (renderer, "notify::stock-id",
                    G_CALLBACK (x_dialog_hotkeys_cell_stock_id_notify),
                    NULL);

  renderer = gtk_cell_renderer_text_new ();
  gtk_tree_view_column_pack_start (column, renderer, FALSE);
  gtk_tree_view_column_set_attributes (column, renderer,
                                       "text", GSCHEM_HOTKEY_STORE_COLUMN_LABEL,
                                       NULL);

  /* The second column contains the action's keybinding */
  gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);
  column = gtk_tree_view_column_new_with_attributes (_("Keystroke(s)"),
                                                     renderer,
                                                     "text",
                                                     GSCHEM_HOTKEY_STORE_COLUMN_KEYS,
                                                     NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW(treeview), column);

  /* setup tree filtering: */
  filter_setup (GTK_TREE_VIEW (treeview), store, GTK_ENTRY (entry));

  /* show all recursively */
  gtk_widget_show_all(w_current->hkwindow);

} /* x_dialog_hotkeys() */
