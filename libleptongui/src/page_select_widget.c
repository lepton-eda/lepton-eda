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
#include <gdk/gdkkeysyms.h>
#include "gschem.h"


G_DEFINE_TYPE (PageSelectWidget, page_select_widget, GSCHEM_TYPE_BIN);


enum /* gobject property IDs */
{
  PROPID_TOPLEVEL = 1,
};


enum /* tree view column IDs */
{
  COLUMN_PAGE,
  COLUMN_NAME,
  COLUMN_CHANGED,
  NUM_COLUMNS
};



/* --------------------------------------------------------
 *
 * forward declarations:
 *
 */

static void
pagesel_update (PageSelectWidget* pagesel);

static void
pagesel_popup_menu (PageSelectWidget* pagesel, GdkEventButton* event);

static void
widget_create (PageSelectWidget *pagesel);

static GCallback callback_page_new = NULL;
static GCallback callback_page_open = NULL;

/*! \brief Create new PageSelectWidget object.
 *  \public
 */
GtkWidget*
page_select_widget_new (GschemToplevel* w_current,
                        GCallback page_new_callback,
                        GCallback page_open_callback)
{
  gpointer obj = g_object_new (PAGE_SELECT_WIDGET_TYPE,
                               "toplevel", w_current,
                               NULL);
  callback_page_new = G_CALLBACK (page_new_callback);
  callback_page_open = G_CALLBACK (page_open_callback);
  return GTK_WIDGET (obj);
}



/* --------------------------------------------------------
 *
 * gobject stuff:
 *
 */

static void
get_property (GObject* obj, guint id, GValue* val, GParamSpec* spec)
{
  PageSelectWidget* widget = PAGE_SELECT_WIDGET (obj);

  if (id == PROPID_TOPLEVEL)
  {
    g_value_set_pointer (val, widget->toplevel_);
  }
  else
  {
    G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, id, spec);
  }
}



static void
set_property (GObject* obj, guint id, const GValue* val, GParamSpec* spec)
{
  PageSelectWidget* widget = PAGE_SELECT_WIDGET (obj);

  if (id == PROPID_TOPLEVEL)
  {
    gpointer ptr = g_value_get_pointer (val);
    widget->toplevel_ = GSCHEM_TOPLEVEL (ptr);
  }
  else
  {
    G_OBJECT_WARN_INVALID_PROPERTY_ID (obj, id, spec);
  }
}



static void
page_select_widget_class_init (PageSelectWidgetClass* cls)
{
  GObjectClass* gcls = G_OBJECT_CLASS (cls);

  gcls->get_property = &get_property;
  gcls->set_property = &set_property;

  GParamFlags flags = (GParamFlags) (G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);
  GParamSpec* spec = g_param_spec_pointer ("toplevel", "", "", flags);
  g_object_class_install_property (gcls, PROPID_TOPLEVEL, spec);
}



static void
page_select_widget_init (PageSelectWidget* widget)
{
  widget_create (widget);
}



/* --------------------------------------------------------
 *
 * implementation:
 *
 */

/*! \brief Update the list and status of pages in the page manager.
 *  \public
 */
void
page_select_widget_update (GschemToplevel* w_current)
{
  GtkWidget* widget = w_current->page_select_widget;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (IS_PAGE_SELECT_WIDGET (widget));

  if (gtk_widget_get_visible (widget))
  {
    pagesel_update (PAGE_SELECT_WIDGET (widget));
  }

  GschemPageView* view = gschem_toplevel_get_current_page_view (w_current);
  LeptonPage* page = gschem_page_view_get_page (view);

  if (page == NULL)
  {
    return;
  }

  i_set_filename (w_current,
                  lepton_page_get_filename (page),
                  lepton_page_get_changed (page));

  if (x_tabs_enabled())
  {
    x_tabs_hdr_update (w_current, page);
  }
}



/*! \brief "changed" signal handler for GtkTreeSelection object.
 */
static void
pagesel_callback_selection_changed (GtkTreeSelection* selection,
                                    gpointer data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  PageSelectWidget *pagesel = (PageSelectWidget*) data;
  GschemToplevel *w_current;
  LeptonPage *page;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
  {
    return;
  }

  w_current = pagesel->toplevel_;

  gtk_tree_model_get (model, &iter,
                      COLUMN_PAGE, &page,
                      -1);

  /* Since setting the current page may call page_select_widget_update(), which
   * might change the current page selection, make sure we do nothing
   * if the newly-selected page is already the current page. */
  if (page == schematic_window_get_active_page (w_current))
  {
    return;
  }

  x_window_set_current_page (w_current, page);
}



/*! \brief "key-press-event" signal handler.
 *  \par Function Description
 *  Disable PgUp/PgDown keys.
 *  \todo Investiagte why the program crashes on PgUp/PgDown.
 */
static gboolean
pagesel_callback_key_pressed (GtkWidget*   widget,
                              GdkEventKey* event,
                              gpointer     data)
{
  gboolean blockKeys = FALSE;

  if (event->type == GDK_KEY_PRESS)
  {
    blockKeys = event->keyval == GDK_KEY_Page_Up    ||
                event->keyval == GDK_KEY_Page_Down  ||
                event->keyval == GDK_KEY_KP_Page_Up ||
                event->keyval == GDK_KEY_KP_Page_Down;
  }

  return blockKeys;
}



/*! \brief "button-press-event" signal handler.
 */
static gboolean
pagesel_callback_button_pressed (GtkWidget* widget,
                                 GdkEventButton* event,
                                 gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;
  gboolean ret = FALSE;

  if (event->type == GDK_BUTTON_PRESS && event->button == 3)
  {
    pagesel_popup_menu (pagesel, event);
    ret = TRUE;
  }

  return ret;
}



/*! \brief "popup-menu" signal handler.
 */
static gboolean
pagesel_callback_popup_menu (GtkWidget* widget,
                             gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;

  pagesel_popup_menu (pagesel, NULL);

  return TRUE;
}



/*! \brief Context menu item handler for "Refresh".
 */
static void
pagesel_callback_popup_refresh (GtkMenuItem* mitem, gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;

  pagesel_update (pagesel);
}



/*! \brief Context menu item handler for "New Page".
 */
static void
pagesel_callback_popup_new_page (GtkMenuItem* mitem, gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;
  GschemToplevel* toplevel = pagesel->toplevel_;
  ((void (*) (GtkWidget*, GschemToplevel*)) callback_page_new) (NULL, toplevel);
}



/*! \brief Context menu item handler for "Open Page".
 */
static void
pagesel_callback_popup_open_page (GtkMenuItem* mitem, gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;
  GschemToplevel* toplevel = pagesel->toplevel_;
  ((void (*) (GtkWidget*, GschemToplevel*)) callback_page_open) (NULL, toplevel);
}



/*! \brief Context menu item handler for "Save Page".
 */
static void
pagesel_callback_popup_save_page (GtkMenuItem* mitem, gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;
  GschemToplevel* toplevel = pagesel->toplevel_;

  i_callback_file_save (NULL, toplevel);
}



/*! \brief Context menu item handler for "Close Page".
 */
static void
pagesel_callback_popup_close_page (GtkMenuItem* mitem, gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;
  GschemToplevel* toplevel = pagesel->toplevel_;

  i_callback_page_close (NULL, toplevel);
}



/*! \brief "Show full paths" checkbox "toggled" signal handler
 */
static void
pagesel_callback_fullpaths_toggled (GtkToggleButton* btn, gpointer data)
{
  PageSelectWidget* pagesel = (PageSelectWidget*) data;
  g_return_if_fail (pagesel != NULL);

  pagesel->show_paths_ = gtk_toggle_button_get_active (btn);
  pagesel_update (pagesel);

  if (!pagesel->show_paths_)
  {
    gtk_tree_view_columns_autosize (pagesel->treeview_);
  }

  /* Save config: whether to show full paths in the pages list:
  */
  EdaConfig* cfg = eda_config_get_cache_context();
  if (cfg != NULL)
  {
    eda_config_set_boolean (cfg,
                           "schematic.page-manager",
                           "show-full-paths",
                           pagesel->show_paths_);
    GError* err = NULL;
    eda_config_save (cfg, &err);
    g_clear_error (&err);
  }

} /* pagesel_callback_fullpath_toggled() */



/*! \brief Popup context-sensitive menu.
 *
 *  \a event can be NULL if the popup is triggered by a key binding
 *  instead of a mouse click.
 *
 *  \param [in] pagesel  The Pagesel object.
 *  \param [in] event    Mouse click event info.
 */
static void
pagesel_popup_menu (PageSelectWidget* pagesel, GdkEventButton* event)
{
  GtkTreePath *path;
  GtkWidget *menu;
  struct menuitem_t {
    const gchar *label;
    GCallback callback;
  };
  struct menuitem_t menuitems[] = {
    { N_("Refresh"),      G_CALLBACK (pagesel_callback_popup_refresh)      },
    { "-",                NULL                                             },
    { N_("New Page"),     G_CALLBACK (pagesel_callback_popup_new_page)     },
    { N_("Open Page..."), G_CALLBACK (pagesel_callback_popup_open_page)    },
    { "-",                NULL                                             },
    { N_("Save Page"),    G_CALLBACK (pagesel_callback_popup_save_page)    },
    { N_("Close Page"),   G_CALLBACK (pagesel_callback_popup_close_page)   },
    { NULL,               NULL                                             } };
  struct menuitem_t *tmp;

  if (event != NULL &&
      gtk_tree_view_get_path_at_pos (pagesel->treeview_,
                                     (gint)event->x,
                                     (gint)event->y,
                                     &path, NULL, NULL, NULL)) {
    GtkTreeSelection *selection;
    selection = gtk_tree_view_get_selection (pagesel->treeview_);
    gtk_tree_selection_unselect_all (selection);
    gtk_tree_selection_select_path (selection, path);
    gtk_tree_path_free (path);
  }

  /* create the context menu */
  menu = gtk_menu_new();
  for (tmp = menuitems; tmp->label != NULL; tmp++) {
    GtkWidget *menuitem;
    if (strcmp (tmp->label, "-") == 0) {
      menuitem = gtk_separator_menu_item_new ();
    } else {
      menuitem = gtk_menu_item_new_with_label (_(tmp->label));
      g_signal_connect (menuitem,
                        "activate",
                        tmp->callback,
                        pagesel);
    }
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  }
  gtk_widget_show_all (menu);
  /* make menu a popup menu */
#ifdef ENABLE_GTK3
  gtk_menu_popup_at_pointer (GTK_MENU (menu), (GdkEvent*) event);
#else
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
#endif
} /* pagesel_popup_menu() */



/*! \brief Construct the widget.
 */
static void
widget_create (PageSelectWidget* pagesel)
{
#ifdef ENABLE_GTK3
  GtkWidget* vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);
#else
  GtkWidget* vbox = gtk_vbox_new (FALSE, 0);
#endif
  gtk_container_add (GTK_CONTAINER (pagesel), vbox);

  GtkWidget *scrolled_win, *treeview;
  GtkTreeModel *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  /* create the model for the treeview */
  store = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_POINTER,  /* page */
                                             G_TYPE_STRING,   /* name */
                                             G_TYPE_BOOLEAN); /* changed */

  /* create a scrolled window for the treeview */
  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width",      5,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));
  /* create the treeview */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "model",      store,
                                       "rules-hint", TRUE,
                                       NULL));
#ifdef ENABLE_GTK3
  g_object_unref (G_OBJECT (store));
#endif

  g_signal_connect (treeview,
                    "key-press-event",
                    G_CALLBACK (pagesel_callback_key_pressed),
                    NULL);

  g_signal_connect (treeview,
                    "button-press-event",
                    G_CALLBACK (pagesel_callback_button_pressed),
                    pagesel);
  g_signal_connect (treeview,
                    "popup-menu",
                    G_CALLBACK (pagesel_callback_popup_menu),
                    pagesel);
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection,
                               GTK_SELECTION_SINGLE);
  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (pagesel_callback_selection_changed),
                    pagesel);
  /*   - first column: page name */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                  /* GtkCellRendererText */
                  "editable", FALSE,
                  NULL));
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Filename"),
                  "min-width", 200,
                  "resizable", TRUE,
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text", COLUMN_NAME);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*   - second column: changed */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
                  /* GtkCellRendererToggle */
                  "activatable", FALSE,
                  NULL));
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Changed"),
                  "sizing", GTK_TREE_VIEW_COLUMN_FIXED,
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "active", COLUMN_CHANGED);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);
  /* set treeview of pagesel */
  pagesel->treeview_ = GTK_TREE_VIEW (treeview);

  /* add the scrolled window to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win,
                      TRUE, TRUE, 0);
#ifdef ENABLE_GTK3
  gtk_widget_set_vexpand (scrolled_win, TRUE);
#endif
  gtk_widget_show_all (scrolled_win);


  /* By default, show basenames in the pages list:
  */
  pagesel->show_paths_ = FALSE;


  /* Read config: whether to show full paths in the pages list:
  */
  EdaConfig* cfg = eda_config_get_cache_context();
  if (cfg != NULL)
  {
    GError* err = NULL;
    gboolean val = eda_config_get_boolean (cfg,
                                           "schematic.page-manager",
                                           "show-full-paths",
                                           &err);
    if (err == NULL)
    {
      pagesel->show_paths_ = val;
    }

    g_clear_error (&err);
  }


  /* "Show full paths" checkbox:
  */

#ifdef ENABLE_GTK3
  GtkWidget* hbox = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  GtkWidget* hbox = gtk_hbox_new (TRUE, 0);
#endif
  gtk_box_pack_start (GTK_BOX (vbox),
                      hbox, FALSE, TRUE, 0);
  gtk_widget_show (hbox);

  GtkWidget* chkbox = gtk_check_button_new_with_mnemonic (_("Show _full paths"));
  gtk_box_pack_start (GTK_BOX (hbox), chkbox, FALSE, TRUE, 5);

  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (chkbox),
                                pagesel->show_paths_);

  g_signal_connect(G_OBJECT (chkbox),
                   "toggled",
                   G_CALLBACK (&pagesel_callback_fullpaths_toggled),
                   pagesel);

  gtk_widget_show (chkbox);

} /* pagesel_init() */



/*! \brief Update tree model of <B>pagesel</B>'s treeview.
 *  \par Function Description
 *  Updates the tree model of <B>pagesel</B>\'s treeview.
 *
 *  Right now, each time it is called, it rebuilds all the model from the
 *  list of pages passed in.
 *  It is a recursive function to populate the tree store
 *
 *  \param [in] model   GtkTreeModel to update.
 *  \param [in] parent  GtkTreeIter pointer to tree root.
 *  \param [in] pages   LeptonPageList of pages for this toplevel.
 *  \param [in] page    The LeptonPage object to update tree model from.
 *  \param [in] pagesel The Pagesel object.
 */
static void
add_page (GtkTreeModel *model,
          GtkTreeIter *parent,
          LeptonPageList *pages,
          LeptonPage *page,
          PageSelectWidget *pagesel)
{
  GtkTreeIter iter;
  LeptonPage *p_current;
  GList *p_iter;

  /* add the page to the store */
  gtk_tree_store_append (GTK_TREE_STORE (model),
                         &iter,
                         parent);

  const gchar* page_filename = lepton_page_get_filename (page);
  gchar* display_filename = pagesel->show_paths_
                            ? g_strdup (page_filename)
                            : g_path_get_basename (page_filename);

  gtk_tree_store_set (GTK_TREE_STORE (model),
                      &iter,
                      COLUMN_PAGE, page,
                      COLUMN_NAME, display_filename,
                      COLUMN_CHANGED, lepton_page_get_changed (page),
                      -1);

  g_free (display_filename);

  /* search a page that has a up field == p_current->pid */
  for ( p_iter = lepton_list_get_glist( pages );
        p_iter != NULL;
        p_iter = g_list_next( p_iter ) ) {

    p_current = (LeptonPage *)p_iter->data;
    if (p_current->up == page->pid) {
      add_page (model, &iter, pages, p_current, pagesel);
    }
  }

} /* add_page() */



/*! \brief Recursive function to select the current page in the treeview.
 */
static void
select_page (GtkTreeView* treeview,
             GtkTreeIter* parent,
             LeptonPage* page)
{
  GtkTreeModel *treemodel = gtk_tree_view_get_model (treeview);
  GtkTreeIter iter;
  LeptonPage *p_current;

  if (!gtk_tree_model_iter_children (treemodel, &iter, parent)) {
    return;
  }

  do {
    gtk_tree_model_get (treemodel, &iter,
                        COLUMN_PAGE, &p_current,
                        -1);
    if (p_current == page) {
      gtk_tree_view_expand_all (treeview);
      gtk_tree_selection_select_iter (
        gtk_tree_view_get_selection (treeview),
        &iter);

      /* Ensure that currently selected item is visible:
      */
      GtkTreePath* path = gtk_tree_model_get_path (treemodel, &iter);
      if (path != NULL)
      {
        gtk_tree_view_scroll_to_cell (treeview, path, NULL, TRUE, 0.5, 0);
        gtk_tree_view_set_cursor (treeview, path, NULL, FALSE);
        gtk_tree_path_free (path);
      }

      return;
    }

    select_page (treeview, &iter, page);

  } while (gtk_tree_model_iter_next (treemodel, &iter));

} /* select_page() */



/*! \brief Rebuild the list of pages.
 */
static void
pagesel_update (PageSelectWidget* pagesel)
{
  GtkTreeModel *model;
  LeptonToplevel *toplevel;
  LeptonPage *p_current;
  GList *iter;

  g_assert (IS_PAGE_SELECT_WIDGET (pagesel));

  GschemToplevel *w_current = pagesel->toplevel_;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  model    = gtk_tree_view_get_model (pagesel->treeview_);

  /* wipe out every thing in the store */
#ifdef ENABLE_GTK3
  model = (GtkTreeModel*)gtk_tree_store_new (NUM_COLUMNS,
                                             G_TYPE_POINTER,  /* page */
                                             G_TYPE_STRING,   /* name */
                                             G_TYPE_BOOLEAN); /* changed */
  gtk_tree_view_set_model (pagesel->treeview_, model);
  g_object_unref (G_OBJECT (model));
#else
  gtk_tree_store_clear (GTK_TREE_STORE (model));
#endif
  /* now rebuild */
  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (LeptonPage *)iter->data;
    /* find every page that is not a hierarchy-down of another page */
    if (p_current->up < 0 ||
        lepton_toplevel_search_page_by_id (toplevel->pages,
                                           p_current->up) == NULL) {
      add_page (model, NULL, toplevel->pages, p_current, pagesel);
    }
  }

  /* select the current page in the treeview */
  select_page (pagesel->treeview_,
               NULL,
               schematic_window_get_active_page (w_current));

} /* pagesel_update() */
