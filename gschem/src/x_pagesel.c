/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "../include/gschem_dialog.h"
#include "../include/x_pagesel.h"


static void x_pagesel_callback_response (GtkDialog *dialog,
                                         gint arg1,
                                         gpointer user_data);



/*! \brief Open the page manager dialog.
 *  \par Function Description
 *  Opens the page manager dialog for <B>toplevel</B> if it is not already.
 *  In this last case, it raises the dialog.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object to open page manager for.
 */
void x_pagesel_open (GSCHEM_TOPLEVEL *w_current)
{
  if (w_current->pswindow == NULL) {
    w_current->pswindow = GTK_WIDGET (g_object_new (TYPE_PAGESEL,
                                                    /* GschemDialog */
                                                    "settings-name", "pagesel",
                                                    "gschem-toplevel", w_current,
                                                    NULL));

    g_signal_connect (w_current->pswindow,
                      "response",
                      G_CALLBACK (x_pagesel_callback_response),
                      w_current);

    gtk_widget_show (w_current->pswindow);
  } else {
    gdk_window_raise (w_current->pswindow->window);
  }

}

/*! \brief Close the page manager dialog.
 *  \par Function Description
 *  Closes the page manager dialog associated with <B>toplevel</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object to close page manager for.
 */
void x_pagesel_close (GSCHEM_TOPLEVEL *w_current)
{
  if (w_current->pswindow) {
    g_assert (IS_PAGESEL (w_current->pswindow));
    gtk_widget_destroy (w_current->pswindow);
    w_current->pswindow = NULL;
  }
  
}

/*! \brief Update the list and status of <B>toplevel</B>'s pages.
 *  \par Function Description
 *  Updates the list and status of <B>toplevel</B>\'s pages if the page
 *  manager dialog is opened.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object to update.
 */
void x_pagesel_update (GSCHEM_TOPLEVEL *w_current)
{
  if (w_current->pswindow) {
    g_assert (IS_PAGESEL (w_current->pswindow));
    pagesel_update (PAGESEL (w_current->pswindow));
  }
}

/*! \brief Callback for page manager response.
 *  \par Function Description
 *  Handles response <B>arg1</B> of the page manager dialog <B>dialog</B>.
 *
 *  \param [in] dialog     GtkDialog that issues callback.
 *  \param [in] arg1       Response argument of page manager dialog.
 *  \param [in] user_data  Pointer to relevant GSCHEM_TOPLEVEL structure.
 */
static void x_pagesel_callback_response (GtkDialog *dialog,
					 gint arg1,
					 gpointer user_data)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*)user_data;

  switch (arg1) {
      case PAGESEL_RESPONSE_UPDATE:
        pagesel_update (PAGESEL (dialog));
        break;
      case GTK_RESPONSE_DELETE_EVENT:
      case PAGESEL_RESPONSE_CLOSE:
        g_assert (GTK_WIDGET (dialog) == w_current->pswindow);
        gtk_widget_destroy (GTK_WIDGET (dialog));
        w_current->pswindow = NULL;
        break;
      default:
        g_assert_not_reached ();
  }
  
}

enum {
  COLUMN_PAGE,
  COLUMN_NAME,
  COLUMN_CHANGED,
  NUM_COLUMNS
};


static void pagesel_class_init (PageselClass *class);
static void pagesel_init       (Pagesel *pagesel);

static void pagesel_popup_menu (Pagesel *pagesel,
                                GdkEventButton *event);

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void pagesel_callback_selection_changed (GtkTreeSelection *selection,
						gpointer user_data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  Pagesel *pagesel = (Pagesel*)user_data;
  GSCHEM_TOPLEVEL *w_current;
  PAGE *page;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
    return;
  }

  w_current = GSCHEM_DIALOG (pagesel)->w_current;
  gtk_tree_model_get (model, &iter,
                      COLUMN_PAGE, &page,
                      -1);

  /* temp */
  s_page_goto (w_current->toplevel, page);
  i_set_filename (w_current, w_current->toplevel->page_current->page_filename);
  x_scrollbars_update (w_current);
  o_redraw_all (w_current);

  /* We would like to use the following call, but since it calls 
   * x_pagesel_update() it would cause an infinite loop.
   */
  /*  x_window_set_current_page (toplevel, page); */

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static gboolean pagesel_callback_button_pressed (GtkWidget *widget,
						 GdkEventButton *event,
						 gpointer user_data)
{
  Pagesel *pagesel = (Pagesel*)user_data;
  gboolean ret = FALSE;

  if (event->type == GDK_BUTTON_PRESS  &&  event->button == 3) {
    pagesel_popup_menu (pagesel, event);
    ret = TRUE;
  }

  return ret;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static gboolean pagesel_callback_popup_menu (GtkWidget *widget,
					     gpointer user_data)
{
  Pagesel *pagesel = (Pagesel*)user_data;

  pagesel_popup_menu (pagesel, NULL);
  
  return TRUE;
}

#define DEFINE_POPUP_CALLBACK(name, action)                       \
static void                                                       \
pagesel_callback_popup_ ## name (GtkMenuItem *menuitem,           \
                                 gpointer user_data)              \
{                                                                 \
  i_callback_ ## action (GSCHEM_DIALOG (user_data)->w_current, 0, NULL); \
}

DEFINE_POPUP_CALLBACK (new_page,     file_new)
DEFINE_POPUP_CALLBACK (open_page,    file_open)
DEFINE_POPUP_CALLBACK (save_page,    file_save)
DEFINE_POPUP_CALLBACK (close_page,   page_close)
DEFINE_POPUP_CALLBACK (discard_page, page_discard)


/*! \brief Popup context-sensitive menu.
 *  \par Function Description
 *  Pops up a context-sensitive menu.
 *
 *  <B>event</B> can be NULL if the popup is triggered by a key binding
 *  instead of a mouse click.
 *
 *  \param [in] pagesel  The Pagesel object.
 *  \param [in] event    Mouse click event info.
 */
static void pagesel_popup_menu (Pagesel *pagesel,
				GdkEventButton *event)
{
  GtkTreePath *path;
  GtkWidget *menu;
  struct menuitem_t {
    gchar *label;
    GCallback callback;
  };
  struct menuitem_t menuitems[] = {
    { N_("New Page"),     G_CALLBACK (pagesel_callback_popup_new_page)     },
    { N_("Open Page..."), G_CALLBACK (pagesel_callback_popup_open_page)    },
    { "-",                NULL                                             },
    { N_("Save Page"),    G_CALLBACK (pagesel_callback_popup_save_page)    },
    { N_("Close Page"),   G_CALLBACK (pagesel_callback_popup_close_page)   },
    { N_("Discard Page"), G_CALLBACK (pagesel_callback_popup_discard_page) },
    { NULL,               NULL                                             } };
  struct menuitem_t *tmp;
  
  if (event != NULL &&
      gtk_tree_view_get_path_at_pos (pagesel->treeview,
                                     (gint)event->x, 
                                     (gint)event->y,
                                     &path, NULL, NULL, NULL)) {
    GtkTreeSelection *selection;
    selection = gtk_tree_view_get_selection (pagesel->treeview);
    gtk_tree_selection_unselect_all (selection);
    gtk_tree_selection_select_path (selection, path);
    gtk_tree_path_free (path);
  }

  /* create the context menu */
  menu = gtk_menu_new();
  for (tmp = menuitems; tmp->label != NULL; tmp++) {
    GtkWidget *menuitem;
    if (g_strcasecmp (tmp->label, "-") == 0) {
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
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
  
}


/*! \brief Handler for the notify::gschem-toplevel signal of GschemDialog
 *
 *  \par Function Description
 *
 *  When the gschem-toplevel property is set on the parent GschemDialog,
 *  we should update the pagesel dialog.
 *
 *  \param [in] pspec      the GParamSpec of the property which changed
 *  \param [in] gobject    the object which received the signal.
 *  \param [in] user_data  user data set when the signal handler was connected.
 */
static void notify_gschem_toplevel_cb (GObject    *gobject,
                                       GParamSpec *arg1,
                                       gpointer    user_data)
{
  Pagesel *pagesel = PAGESEL( gobject );

  pagesel_update( pagesel );
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GType pagesel_get_type()
{
  static GType pagesel_type = 0;
  
  if (!pagesel_type) {
    static const GTypeInfo pagesel_info = {
      sizeof(PageselClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) pagesel_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Pagesel),
      0,    /* n_preallocs */
      (GInstanceInitFunc) pagesel_init,
    };
		
    pagesel_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                           "Pagesel",
                                           &pagesel_info, 0);
  }
  
  return pagesel_type;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void pagesel_class_init (PageselClass *klass)
{
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void pagesel_init (Pagesel *pagesel)
{
  GtkWidget *scrolled_win, *treeview, *label;
  GtkTreeModel *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

  /* dialog initialization */
  g_object_set (G_OBJECT (pagesel),
                /* GtkContainer */
                "border-width",    0,
                /* GtkWindow */
                "type",            GTK_WINDOW_TOPLEVEL,
                "title",           _("Page Manager"),
                "default-height",  180,
                "default-width",   515,
                "modal",           FALSE,
                "window-position", GTK_WIN_POS_NONE,
                "type-hint",       GDK_WINDOW_TYPE_HINT_NORMAL,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

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
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));
  /* create the treeview */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "model",      store,
                                       "rules-hint", TRUE,
                                       NULL));
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
                  "min-width", 400,
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
  pagesel->treeview = GTK_TREE_VIEW (treeview);

  /* add the scrolled window to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (pagesel)->vbox), scrolled_win,
                      TRUE, TRUE, 0);
  gtk_widget_show_all (scrolled_win);

  /* add a label below the scrolled window */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkLabel */
                                    "label", _("Right click on the filename for more options..."),
                                    NULL));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (pagesel)->vbox), label,
                      FALSE, TRUE, 5);
  gtk_widget_show (label);

  /* now add buttons in the action area */
  gtk_dialog_add_buttons (GTK_DIALOG (pagesel),
                          /*  - update button */
                          GTK_STOCK_REFRESH, PAGESEL_RESPONSE_UPDATE,
                          /*  - close button */
                          GTK_STOCK_CLOSE,   PAGESEL_RESPONSE_CLOSE,
                          NULL);

#if GTK_CHECK_VERSION (2,6,0)
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(pagesel),
					  PAGESEL_RESPONSE_UPDATE,
					  PAGESEL_RESPONSE_CLOSE,
					  -1);
#endif

  g_signal_connect( pagesel, "notify::gschem-toplevel",
                    G_CALLBACK( notify_gschem_toplevel_cb ), NULL );
}


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
 *  \param [in] pages   GedaPageList of pages for this toplevel.
 *  \param [in] page    The PAGE object to update tree model from.
 */
static void add_page (GtkTreeModel *model, GtkTreeIter *parent,
                      GedaPageList *pages, PAGE *page)
{
  GtkTreeIter iter;
  PAGE *p_current;
  GList *p_iter;

  /* add the page to the store */
  gtk_tree_store_append (GTK_TREE_STORE (model),
                         &iter,
                         parent);
  gtk_tree_store_set (GTK_TREE_STORE (model),
                      &iter,
                      COLUMN_PAGE, page,
                      COLUMN_NAME, page->page_filename,
                      COLUMN_CHANGED, page->CHANGED,
                      -1);

  /* search a page that has a up field == p_current->pid */
  for ( p_iter = geda_list_get_glist( pages );
        p_iter != NULL;
        p_iter = g_list_next( p_iter ) ) {

    p_current = (PAGE *)p_iter->data;
    if (p_current->up == page->pid) {
      add_page (model, &iter, pages, p_current);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Recursive function to select the current page in the treeview
 *
 */
static void select_page(GtkTreeView *treeview,
			GtkTreeIter *parent, PAGE *page)
{
  GtkTreeModel *treemodel = gtk_tree_view_get_model (treeview);
  GtkTreeIter iter;
  PAGE *p_current;

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
      return;
    }

    select_page (treeview, &iter, page);
    
  } while (gtk_tree_model_iter_next (treemodel, &iter));
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void pagesel_update (Pagesel *pagesel)
{
  GtkTreeModel *model;
  TOPLEVEL *toplevel;
  PAGE *p_current;
  GList *iter;

  g_assert (IS_PAGESEL (pagesel));

  g_return_if_fail (GSCHEM_DIALOG (pagesel)->w_current);

  toplevel = GSCHEM_DIALOG (pagesel)->w_current->toplevel;
  model    = gtk_tree_view_get_model (pagesel->treeview);

  /* wipe out every thing in the store */
  gtk_tree_store_clear (GTK_TREE_STORE (model));
  /* now rebuild */
  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;
    /* find every page that is not a hierarchy-down of another page */
    if (p_current->up < 0 ||
        s_hierarchy_find_page (toplevel->pages,
                               p_current->up) == NULL) {
      add_page (model, NULL, toplevel->pages, p_current);
    }
  }

  /* select the current page in the treeview */
  select_page (pagesel->treeview, NULL, toplevel->page_current);  
}

