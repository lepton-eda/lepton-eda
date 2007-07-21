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
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h> 

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "../include/gschem_dialog.h"
#include "../include/x_preview.h"
#include "../include/x_compselect.h"

/*! \def COMPSELECT_FILTER_INTERVAL
 *  \brief The time interval between request and actual filtering
 *
 *  This constant is the time-lag between user modifications in the
 *  filter entry and the actual evaluation of the filter which
 *  ultimately update the display. It helps reduce the frequency of
 *  evaluation of the filter as user types.
 *
 *  Unit is milliseconds.
 */
#define COMPSELECT_FILTER_INTERVAL 300


/*! \brief Process the response returned by the component selection dialog.
 *  \par Function Description
 *  This function handles the response <B>arg1</B> of the component
 *  selection dialog <B>dialog</B>.
 *
 *  Parameter <B>user_data</B> is a pointer on the relevant toplevel
 *  structure.
 *
 *  \param [in] dialog    The component selection dialog.
 *  \param [in] arg1      The response ID.
 *  \param [in] user_data A pointer on the toplevel environment.
 */
static void
x_compselect_callback_response (GtkDialog *dialog,
                                gint arg1,
                                gpointer user_data)
{
  Compselect *compselect = (Compselect*)dialog;
  TOPLEVEL *toplevel = (TOPLEVEL*)user_data;
  GValue value = { 0, };

  switch (arg1) {
      case COMPSELECT_RESPONSE_PLACE: {
	CLibSymbol *symbol = NULL;
        CompselectBehavior behavior;
        
        g_object_get (compselect,
                      "symbol", &symbol,
                      "behavior", &behavior,
                      NULL);

        toplevel->include_complex = toplevel->embed_complex = 0;
        switch (behavior) {
            case COMPSELECT_BEHAVIOR_REFERENCE:
              break;
            case COMPSELECT_BEHAVIOR_EMBED:
              toplevel->embed_complex   = 1;
              break;
            case COMPSELECT_BEHAVIOR_INCLUDE:
              toplevel->include_complex = 1;
              break;
            default:
              g_assert_not_reached();
        }

        if (symbol == NULL) {
          break;
        }
                
	if (toplevel->event_state == ENDCOMP) {
          gint diff_x, diff_y;

	  diff_x = toplevel->last_x - toplevel->start_x;
	  diff_y = toplevel->last_y - toplevel->start_y;
	  
	  o_complex_translate_display_object_glist(toplevel,
						   diff_x, diff_y,
						   toplevel->page_current->complex_place_list);
	}
	
	g_list_free(toplevel->page_current->complex_place_list);
	toplevel->page_current->complex_place_list = NULL;
	
	o_complex_set_filename(toplevel, s_clib_symbol_get_name (symbol));
        
	toplevel->event_state = DRAWCOMP;

        break;
      }
      case COMPSELECT_RESPONSE_HIDE:
	/* Response when clicking on the "hide" button */

	/* If there is no component in the complex place list, set the current one */
	if (toplevel->page_current->complex_place_list == NULL) {
	  gtk_dialog_response (GTK_DIALOG (compselect), 
                               COMPSELECT_RESPONSE_PLACE);
	}

	/* Hide the component selector */
	g_value_init (&value, G_TYPE_BOOLEAN);
	g_value_set_boolean(&value, TRUE);
	g_object_set_property (G_OBJECT(compselect), "hidden", &value);
	break;
      case GTK_RESPONSE_CLOSE:
      case GTK_RESPONSE_DELETE_EVENT:
        g_assert (GTK_WIDGET (dialog) == toplevel->cswindow);
        gtk_widget_destroy (GTK_WIDGET (dialog));
        toplevel->cswindow = NULL;

	/* Free the complex place list */
	g_list_free(toplevel->page_current->complex_place_list);
	toplevel->page_current->complex_place_list = NULL;
	
	/* return to the default state */
	i_set_state(toplevel, SELECT);
	i_update_toolbar(toplevel);
  
        break;
      default:
        /* Do nothing, in case there's another handler function which
           can handle the response ID received. */
        break;
  }
  
}

/*! \brief Opens a component selection dialog.
 *  \par Function Description
 *  This function opens the component chooser dialog for
 *  <B>toplevel</B> if it is not already. In this last case, it only
 *  raises the dialog.
 *
 *  \param [in] toplevel The toplevel environment.
 */
void
x_compselect_open (TOPLEVEL *toplevel)
{
  GtkWidget *current_tab, *entry_filter;
  GtkNotebook *compselect_notebook;

  if (toplevel->cswindow == NULL) {
    toplevel->cswindow = GTK_WIDGET (
      g_object_new (TYPE_COMPSELECT,
                    /* GschemDialog */
                    "settings-name", "compselect",
                    "toplevel",      toplevel,
                    NULL));

    g_signal_connect (toplevel->cswindow,
                      "response",
                      G_CALLBACK (x_compselect_callback_response),
                      toplevel);
    
    gtk_window_set_transient_for(GTK_WINDOW(toplevel->cswindow),
				 GTK_WINDOW(toplevel->main_window));

    gtk_widget_show (toplevel->cswindow);
    
  } else {
    gtk_window_present (GTK_WINDOW(toplevel->cswindow));
  }
  gtk_editable_select_region(GTK_EDITABLE(COMPSELECT(toplevel->cswindow)->entry_filter), 0, -1);

  /* Set the focus to the filter entry only if it is in the current 
     displayed tab */
  compselect_notebook = GTK_NOTEBOOK(COMPSELECT(toplevel->cswindow)->viewtabs);
  current_tab = gtk_notebook_get_nth_page(compselect_notebook,
                                          gtk_notebook_get_current_page(compselect_notebook));
  entry_filter = GTK_WIDGET(COMPSELECT(toplevel->cswindow)->entry_filter);
  if (gtk_widget_is_ancestor(entry_filter, current_tab)) {
    gtk_widget_grab_focus (entry_filter); 
  }
}

/*! \brief Closes the component selection dialog.
 *  \par Function Description
 *  This function closes the component chooser dialog associated with
 *  <B>toplevel</B>.
 *
 *  \param [in] toplevel The toplevel environment.
 */
void
x_compselect_close (TOPLEVEL *toplevel)
{
  if (toplevel->cswindow) {
    g_assert (IS_COMPSELECT (toplevel->cswindow));
    gtk_widget_destroy (toplevel->cswindow);
    toplevel->cswindow = NULL;
  }    
}



enum {
  PROP_SYMBOL=1,
  PROP_BEHAVIOR,
  PROP_HIDDEN
};

static GObjectClass *compselect_parent_class = NULL;


static void compselect_class_init (CompselectClass *class);
static void compselect_init       (Compselect *compselect);
static void compselect_finalize     (GObject *object);
static void compselect_set_property (GObject *object,
                                     guint property_id,
                                     const GValue *value,
                                     GParamSpec *pspec);
static void compselect_get_property (GObject *object,
                                     guint property_id,
                                     GValue *value,
                                     GParamSpec *pspec);



/*! \brief Sets data for a particular cell of the in use treeview.
 *  \par Function Description
 *  This function determines what data is to be displayed in the
 *  "in use" symbol selection view.
 *
 *  The model is a list of symbols. s_clib_symbol_get_name() is called
 *  to get the text to display.
 */
static void
inuse_treeview_set_cell_data (GtkTreeViewColumn *tree_column,
                            GtkCellRenderer   *cell,
                            GtkTreeModel      *tree_model,
                            GtkTreeIter       *iter,
                            gpointer           data)
{
  GValue value = { 0, };
  GValue strvalue = { 0, };
  CLibSymbol *symbol;

  gtk_tree_model_get_value (tree_model, iter, 0, &value);

  g_value_init (&strvalue, G_TYPE_STRING);

  symbol = (CLibSymbol *) g_value_get_pointer (&value);
  g_value_set_string (&strvalue, s_clib_symbol_get_name (symbol));

  g_object_set_property ((GObject*)cell, "text", &strvalue);

  g_value_unset (&value);
  g_value_unset (&strvalue);
}

/*! \brief Sets data for a particular cell of the library treeview.
 *  \par Function Description
 *  This function determines what data is to be displayed in the
 *  selection selection view.
 *
 *  The top level of the model contains sources, and the next symbols.
 *  s_clib_source_get_name() or s_clib_symbol_get_name() as
 *  appropriate is called to get the text to display.
 */
static void
lib_treeview_set_cell_data (GtkTreeViewColumn *tree_column,
                            GtkCellRenderer   *cell,
                            GtkTreeModel      *tree_model,
                            GtkTreeIter       *iter,
                            gpointer           data)
{
  GtkTreeIter parent;
  GValue value = { 0, };
  GValue strvalue = { 0, };
  CLibSource *source;
  CLibSymbol *symbol;
  
  gtk_tree_model_get_value (tree_model, iter, 0, &value);

  g_value_init (&strvalue, G_TYPE_STRING);

  if (!gtk_tree_model_iter_parent (tree_model, &parent, iter)) {
    /* If top level, must be a source. */
    source = (CLibSource *) g_value_get_pointer (&value);
    g_value_set_string (&strvalue, s_clib_source_get_name (source));
  } else {
    /* Otherwise, must be a symbol */
    symbol = (CLibSymbol *) g_value_get_pointer (&value);
    g_value_set_string (&strvalue, s_clib_symbol_get_name (symbol));
  }
  g_object_set_property ((GObject*)cell, "text", &strvalue);

  g_value_unset (&value);
  g_value_unset (&strvalue);
}

/*! \brief Determines visibility of items of the library treeview.
 *  \par Function Description
 *  This is the function used to filter entries of the component
 *  selection tree.
 *
 *  \param [in] model The current selection in the treeview.
 *  \param [in] iter  An iterator on a component or folder in the tree.
 *  \param [in] data  The component selection dialog.
 *  \returns TRUE if item should be visible, FALSE otherwise.
 */
static gboolean
lib_model_filter_visible_func (GtkTreeModel *model,
                                      GtkTreeIter  *iter,
                                      gpointer      data)
{
  Compselect *compselect = (Compselect*)data;
  CLibSymbol *sym;
  const gchar *compname;
  gchar *compname_upper=NULL, *text_upper=NULL;
  const gchar *text;
  gboolean ret;

  g_assert (IS_COMPSELECT (data));
  
  text = gtk_entry_get_text (compselect->entry_filter);
  if (g_ascii_strcasecmp (text, "") == 0) {
    return TRUE;
  }

  /* If this is a source, only display it if it has children that
   * match */
  if (gtk_tree_model_iter_has_child (model, iter)) {
    GtkTreeIter iter2;

    gtk_tree_model_iter_children (model, &iter2, iter);
    ret = FALSE;
    do {
      if (lib_model_filter_visible_func (model, &iter2, data)) {
        ret = TRUE;
        break;
      }
    } while (gtk_tree_model_iter_next (model, &iter2));
  } else {
    gtk_tree_model_get (model, iter,
                        0, &sym,
                        -1);
    compname = s_clib_symbol_get_name (sym);
    /* Do a case insensitive comparison, converting the strings 
       to uppercase */
    compname_upper = g_ascii_strup(compname, -1);
    text_upper = g_ascii_strup(text, -1);
    ret = (strstr (compname_upper, text_upper) != NULL);
    g_free(compname_upper);
    g_free(text_upper);
  }

  return ret;
}

/*! \brief Handles changes in the treeview selection.
 *  \par Function Description
 *  This is the callback function that is called every time the user
 *  select a row in either component treeview of the dialog.
 *
 *  If the selection is not a selection of a component (a directory
 *  name), it does nothing. Otherwise it retrieves the #CLibSymbol
 *  from the model.
 *
 *  It then emits the dialog's <B>apply</B> signal to let its parent
 *  know that a component has been selected.
 *
 *  \param [in] selection The current selection in the treeview.
 *  \param [in] user_data The component selection dialog.
 */
static void
compselect_callback_tree_selection_changed (GtkTreeSelection *selection,
                                            gpointer          user_data)
{
  GtkTreeView *view;
  GtkTreeModel *model;
  GtkTreeIter iter, parent;
  Compselect *compselect = (Compselect*)user_data;
  const CLibSymbol *sym = NULL;
  gchar *buffer = NULL;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
    return;
  }

  view = gtk_tree_selection_get_tree_view (selection);

  if (view == compselect->inusetreeview) {
    /* No special handling at the moment */
  } else if (view == compselect->libtreeview) {
    if (!gtk_tree_model_iter_parent (model, &parent, &iter)) {
      /* selected element is not a leaf -> not a component name */
      return;
    }
  } else {
    /* Something's gone wrong */
   g_assert_not_reached();
  }
  
  gtk_tree_model_get (model, &iter, 0, &sym, -1);

  buffer = s_clib_symbol_get_data (sym);

  /* update the preview with new symbol data */
  g_object_set (compselect->preview,
                "buffer", buffer,
                NULL);

  /* signal a component has been selected to parent of dialog */
  g_signal_emit_by_name (compselect,
                         "response",
                         COMPSELECT_RESPONSE_PLACE,
                         NULL);

  g_free (buffer);
}

/*! \brief Requests re-evaluation of the filter.
 *  \par Function Description
 *  This is the timeout function for the filtering of component in the
 *  tree of the dialog.
 *
 *  The timeout this callback is attached to is removed after the
 *  function.
 *
 *  \param [in] data The component selection dialog.
 *  \returns FALSE to remove the timeout.
 */
static gboolean
compselect_filter_timeout (gpointer data)
{
  Compselect *compselect = COMPSELECT (data);
  GtkTreeModel *model;

  /* resets the source id in compselect */
  compselect->filter_timeout = 0;
  
  model = gtk_tree_view_get_model (compselect->libtreeview);

  if (model != NULL) {
    gtk_tree_model_filter_refilter ((GtkTreeModelFilter*)model);
  }

  /* return FALSE to remove the source */
  return FALSE;
}

/*! \brief Callback function for the changed signal of the filter entry.
 *  \par Function Description
 *  This function monitors changes in the entry filter of the dialog.
 *
 *  It specifically manages the sensitivity of the clear button of the
 *  entry depending on its contents. It also requests an update of the
 *  component list by re-evaluating filter at every changes.
 *
 *  \param [in] editable  The filter text entry.
 *  \param [in] user_data The component selection dialog.
 */
static void
compselect_callback_filter_entry_changed (GtkEditable *editable,
                                          gpointer  user_data)
{
  Compselect *compselect = COMPSELECT (user_data);
  GtkWidget *button;
  gboolean sensitive;

  /* turns button off if filter entry is empty */
  /* turns it on otherwise */
  button    = GTK_WIDGET (compselect->button_clear);
  sensitive = 
    (g_ascii_strcasecmp (gtk_entry_get_text (compselect->entry_filter),
                         "") != 0);
  if (GTK_WIDGET_IS_SENSITIVE (button) != sensitive) {
    gtk_widget_set_sensitive (button, sensitive);
  }

  /* ask for an update of the component list */
  /* re-evaluation of filter will occur in _INTERVAL ms unless entry */
  /* has been modified again */
  compselect->filter_timeout =
    g_timeout_add (COMPSELECT_FILTER_INTERVAL,
                   compselect_filter_timeout,
                   compselect);
 
}

/*! \brief Handles a click on the clear button.
 *  \par Function Description
 *  This is the callback function called every time the user press the
 *  clear button associated with the filter.
 *
 *  It resets the filter entry, indirectly causing re-evaluation
 *  of the filter on the list of symbols to update the display.
 *
 *  \param [in] editable  The filter text entry.
 *  \param [in] user_data The component selection dialog.
 */
static void
compselect_callback_filter_button_clicked (GtkButton *button,
                                           gpointer   user_data)
{
  Compselect *compselect = COMPSELECT (user_data);

  /* clears text in text entry for filter */
  gtk_entry_set_text (compselect->entry_filter, "");
  
}

/*! \brief Handles changes of behavior.
 *  \par Function Description
 *  This function is called every time the value of the option menu
 *  for behaviors is modified.
 *
 *  It emits the dialog's <B>apply</B> signal to let the parent know
 *  that the requested behavior for the next adding of a component has
 *  been changed.
 *
 *  \param [in] optionmenu The behavior option menu.
 *  \param [in] user_data  The component selection dialog.
 */
static void
compselect_callback_behavior_changed (GtkOptionMenu *optionmenu,
                                      gpointer user_data)
{
  Compselect *compselect = (Compselect*)user_data;

  g_signal_emit_by_name (compselect,
                         "response",
                         COMPSELECT_RESPONSE_PLACE,
                         NULL);
}

/* \brief Create the tree model for the "In Use" view.
 * \par Function Description
 * Creates a straightforward list of symbols which are currently in
 * use, using s_toplevel_get_symbols().
 */
static GtkTreeModel*
create_inuse_tree_model (void)
{
  GtkListStore *store;
  GList *symhead, *symlist;
  GtkTreeIter iter;

  store = (GtkListStore *) gtk_list_store_new (1, G_TYPE_POINTER);

  symhead = s_toplevel_get_symbols (global_window_current);

  for (symlist = symhead;
       symlist != NULL;
       symlist = g_list_next (symlist)) {

    gtk_list_store_append (store, &iter);

    gtk_list_store_set (store, &iter,
                        0, symlist->data,
                        -1);
  }

  g_list_free (symlist);

  return (GtkTreeModel*)store;
}

/* \brief Create the tree model for the "Library" view.
 * \par Function Description
 * Creates a tree where the branches are the available component
 * sources and the leaves are the symbols.
 */
static GtkTreeModel*
create_lib_tree_model (void)
{
  GtkTreeStore *store;
  GList *srchead, *srclist;
  GList *symhead, *symlist; 

  store = (GtkTreeStore*)gtk_tree_store_new (1, G_TYPE_POINTER);
  
  /* populate component store */
  srchead = s_clib_get_sources (global_window_current->sort_component_library != 0);
  for (srclist = srchead; 
       srclist != NULL; 
       srclist = g_list_next (srclist)) {

    GtkTreeIter iter, iter2;

    gtk_tree_store_append (store, &iter, NULL);
    gtk_tree_store_set (store, &iter,
                        0, srclist->data,
                        -1);
    
    symhead = s_clib_source_get_symbols ((CLibSource *)srclist->data);
    for (symlist = symhead; 
	 symlist != NULL; 
	 symlist = g_list_next (symlist)) {

      gtk_tree_store_append (store, &iter2, &iter);
      gtk_tree_store_set (store, &iter2,
                          0, symlist->data,
                          -1);
    }

    g_list_free (symhead);
  }
  g_list_free (srchead);

  return (GtkTreeModel*)store;
}

/* \brief On-demand refresh of the component library.
 * \par Function Description
 * Requests a rescan of the component library in order to pick up any
 * new signals, and then updates the component selector.  Handler for
 * the #COMPSELECT_RESPONSE_REFRESH response ID.
 */
static void
compselect_callback_refresh_library (Compselect *compselect,
                                     gint response_id,
                                     gpointer user_data) {
  GtkTreeModel *model;

  if (response_id != COMPSELECT_RESPONSE_REFRESH) {
    return;
  }

  /* Rescan the libraries for symbols */
  s_clib_refresh ();

  /* Refresh the "Library" view */
  model = (GtkTreeModel *)
    g_object_new (GTK_TYPE_TREE_MODEL_FILTER,
                  "child-model", create_lib_tree_model (),
                  "virtual-root", NULL,
                  NULL);

  gtk_tree_view_set_model (compselect->libtreeview, model);

  /* Refresh the "In Use" view */
  model = create_inuse_tree_model ();
  gtk_tree_view_set_model (compselect->inusetreeview, model);
}

/*! \brief Creates the treeview for the "In Use" view. */
static GtkWidget*
create_inuse_treeview (Compselect *compselect)
{
  GtkWidget *scrolled_win, *treeview;
  GtkTreeModel *model;
  GtkTreeSelection *selection;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  model = create_inuse_tree_model ();

  /* Create a scrolled window to accomodate the treeview */
  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width", 5,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));

  /* Create the treeview */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "model",      model,
                                       "rules-hint", TRUE,
                                       "headers-visible", FALSE,
                                       NULL));

  /* Connect callback to selection */
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (compselect_callback_tree_selection_changed),
                    compselect);

  /* Insert a column for symbol name */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                  /* GtkCellRendererText */
                  "editable", FALSE,
                  NULL));
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Components"),
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           inuse_treeview_set_cell_data,
                                           NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);

  /* Add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);
  /* set the inuse treeview of compselect */
  compselect->inusetreeview = GTK_TREE_VIEW (treeview);

  return scrolled_win;
}

/*! \brief Creates the treeview for the "Library" view */
static GtkWidget *
create_lib_treeview (Compselect *compselect)
{
  GtkWidget *libtreeview, *vbox, *scrolled_win, *label, 
    *hbox, *entry, *button;
  GtkTreeModel *child_model, *model;
  GtkTreeSelection *selection;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  /* -- library selection view -- */

  /* vertical box for component selection and search entry */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkContainer */
                                   "border-width", 5,
                                   /* GtkBox */
                                   "homogeneous",  FALSE,
                                   "spacing",      5,
                                   NULL));

  child_model  = create_lib_tree_model ();
  model = (GtkTreeModel*)g_object_new (GTK_TYPE_TREE_MODEL_FILTER,
                                       "child-model",  child_model,
                                       "virtual-root", NULL,
                                       NULL);
  
  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));
  /* create the treeview */
  libtreeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                          /* GtkTreeView */
                                          "model",      model,
                                          "rules-hint", TRUE,
                                          "headers-visible", FALSE,
                                          NULL));
  /* connect callback to selection */
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (libtreeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (compselect_callback_tree_selection_changed),
                    compselect);

  /* insert a column to treeview for library/symbol name */
  renderer = GTK_CELL_RENDERER (
    g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                  /* GtkCellRendererText */
                  "editable", FALSE,
                  NULL));
  column = GTK_TREE_VIEW_COLUMN (
    g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                  /* GtkTreeViewColumn */
                  "title", _("Components"),
                  NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
                                           lib_treeview_set_cell_data,
                                           NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (libtreeview), column);
  
  /* add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), libtreeview);
  /* set directory/component treeview of compselect */
  compselect->libtreeview = GTK_TREE_VIEW (libtreeview);

  /* add the scrolled window for directories to the vertical box */
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win,
                      TRUE, TRUE, 0);

  
  /* -- filter area -- */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                          /* GtkBox */
                                          "homogeneous", FALSE,
                                          "spacing",     3,
                                          NULL));

  /* create the entry label */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
                                    /* GtkMisc */
                                    "xalign", 0.0,
                                    /* GtkLabel */
                                    "label",  _("Filter:"),
                                    NULL));
  /* add the search label to the filter area */
  gtk_box_pack_start (GTK_BOX (hbox), label,
                      FALSE, FALSE, 0);
  
  /* create the text entry for filter in components */
  entry = GTK_WIDGET (g_object_new (GTK_TYPE_ENTRY,
                                    /* GtkEntry */
                                    "text", "",
                                    NULL));
  g_signal_connect (entry,
                    "changed",
                    G_CALLBACK (compselect_callback_filter_entry_changed),
                    compselect);

  /* now that that we have an entry, set the filter func of model */
  gtk_tree_model_filter_set_visible_func ((GtkTreeModelFilter*)model,
                                          lib_model_filter_visible_func,
                                          compselect,
                                          NULL);

  /* add the filter entry to the filter area */
  gtk_box_pack_start (GTK_BOX (hbox), entry,
                      TRUE, TRUE, 0);
  /* set filter entry of compselect */
  compselect->entry_filter = GTK_ENTRY (entry);
  /* and init the event source for component filter */
  compselect->filter_timeout = 0;

  /* create the erase button for filter entry */
  button = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                     /* GtkWidget */
                                     "sensitive", FALSE,
                                     /* GtkButton */
                                     "relief",    GTK_RELIEF_NONE,
                                     NULL));
  gtk_container_add (GTK_CONTAINER (button),
                     gtk_image_new_from_stock (GTK_STOCK_CLEAR,
                                               GTK_ICON_SIZE_SMALL_TOOLBAR));
  g_signal_connect (button,
                    "clicked",
                    G_CALLBACK (compselect_callback_filter_button_clicked),
                    compselect);
  /* add the clear button to the filter area */
  gtk_box_pack_start (GTK_BOX (hbox), button,
                      FALSE, FALSE, 0);
  /* set clear button of compselect */
  compselect->button_clear = GTK_BUTTON (button);
                                     
  /* add the filter area to the vertical box */
  gtk_box_pack_start (GTK_BOX (vbox), hbox,
                      FALSE, FALSE, 0);

  compselect->libtreeview = GTK_TREE_VIEW (libtreeview);

  return vbox;
}

/*! \brief Create the combo box for behaviors.
 *  \par Function Description
 *  This function creates and returns a <B>GtkComboBox</B> for
 *  selecting the behavior when a component is added to the sheet.
 */
static GtkWidget*
create_behaviors_combo_box (void)
{
  GtkWidget *combobox;

  combobox = gtk_combo_box_new_text ();

  /* Note: order of items in menu is important */
  /* COMPSEL_BEHAVIOR_REFERENCE */
  gtk_combo_box_append_text (GTK_COMBO_BOX (combobox),
                             _("Default behavior - reference component"));
  /* COMPSEL_BEHAVIOR_EMBED */
  gtk_combo_box_append_text (GTK_COMBO_BOX (combobox),
                             _("Embed component in schematic"));
  /* COMPSEL_BEHAVIOR_INCLUDE */
  gtk_combo_box_append_text (GTK_COMBO_BOX (combobox),
                             _("Include component as individual objects"));

  gtk_combo_box_set_active (GTK_COMBO_BOX (combobox), 0);
  
  return combobox;
}

GType
compselect_get_type ()
{
  static GType compselect_type = 0;
  
  if (!compselect_type) {
    static const GTypeInfo compselect_info = {
      sizeof(CompselectClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) compselect_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Compselect),
      0,    /* n_preallocs */
      (GInstanceInitFunc) compselect_init,
    };
                
    compselect_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                              "Compselect",
                                              &compselect_info, 0);
  }
  
  return compselect_type;
}

#if GLIB_CHECK_VERSION(2,6,0)

/*! \brief GschemDialog "geometry_save" class method handler
 *
 *  \par Function Description
 *  Chain up to our parent's method to save the dialog's size and
 *  position, then save the dialog's current internal geometry.
 *
 *  \param [in] dialog     The GschemDialog to save the geometry of.
 *  \param [in] key_file   The GKeyFile to save the geometry data to.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void
compselect_geometry_save (GschemDialog *dialog, GKeyFile *key_file, gchar *group_name)
{
  int position;

  /* Call the parent's geometry_save method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
    geometry_save (dialog, key_file, group_name);

  position = gtk_paned_get_position (GTK_PANED (COMPSELECT (dialog)->hpaned));
  g_key_file_set_integer (key_file, group_name, "hpaned", position );
}


/*! \brief GschemDialog "geometry_restore" class method handler
 *
 *  \par Function Description
 *  Chain up to our parent's method to restore the dialog's size and
 *  position, then restore the dialog's current internal geometry.
 *
 *  \param [in] dialog     The GschemDialog to restore the geometry of.
 *  \param [in] key_file   The GKeyFile to save the geometry data to.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void
compselect_geometry_restore (GschemDialog *dialog, GKeyFile *key_file, gchar *group_name)
{
  int position;

  /* Call the parent's geometry_restore method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
    geometry_restore (dialog, key_file, group_name);

  position = g_key_file_get_integer (key_file, group_name, "hpaned", NULL);
  if (position != 0)
    gtk_paned_set_position (GTK_PANED (COMPSELECT (dialog)->hpaned), position);
}

#endif   /* !GLIB_CHECK_VERSION(2,6,0) */

static void
compselect_class_init (CompselectClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

#if GLIB_CHECK_VERSION(2,6,0)
  GschemDialogClass *gschem_dialog_class = GSCHEM_DIALOG_CLASS (klass);

  gschem_dialog_class->geometry_save    = compselect_geometry_save;
  gschem_dialog_class->geometry_restore = compselect_geometry_restore;
#endif

  gobject_class->finalize     = compselect_finalize;
  gobject_class->set_property = compselect_set_property;
  gobject_class->get_property = compselect_get_property;

  compselect_parent_class = g_type_class_peek_parent (klass);

  g_object_class_install_property (
    gobject_class, PROP_SYMBOL,
    g_param_spec_pointer ("symbol",
			  "",
			  "",
			  G_PARAM_READABLE));
  g_object_class_install_property (
    gobject_class, PROP_BEHAVIOR,
    g_param_spec_enum ("behavior",
                       "",
                       "",
                       COMPSELECT_TYPE_BEHAVIOR,
                       COMPSELECT_BEHAVIOR_REFERENCE,
                       G_PARAM_READWRITE));
  g_object_class_install_property (
    gobject_class, PROP_HIDDEN,
    g_param_spec_boolean ("hidden",
			  "",
			  "",
			  FALSE,
			  G_PARAM_READWRITE));
  
}

static void
compselect_init (Compselect *compselect)
{
  GtkWidget *hpaned, *notebook;
  GtkWidget *libview, *inuseview;
  GtkWidget *preview, *combobox;
  
  GtkWidget *alignment, *frame;
  
  /* dialog initialization */
  g_object_set (G_OBJECT (compselect),
                /* GtkWindow */
                "type",            GTK_WINDOW_TOPLEVEL,
                "title",           _("Select Component..."),
                "default-height",  300,
                "default-width",   400,
                "modal",           FALSE,
                "window-position", GTK_WIN_POS_NONE,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);
  g_object_set (GTK_DIALOG (compselect)->vbox,
                "homogeneous", FALSE,
                NULL);

  /* horizontal pane containing selection and preview */
  hpaned = GTK_WIDGET (g_object_new (GTK_TYPE_HPANED,
                                    /* GtkContainer */
                                    "border-width", 5,
                                     NULL));
  compselect->hpaned = hpaned;

  /* notebook for library and inuse views */
  notebook = GTK_WIDGET (g_object_new (GTK_TYPE_NOTEBOOK,
                                       NULL));
  compselect->viewtabs = GTK_NOTEBOOK (notebook);

  inuseview = create_inuse_treeview (compselect);
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), inuseview,
                            gtk_label_new (_("In Use")));  

  libview = create_lib_treeview (compselect);
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), libview,
                            gtk_label_new (_("Libraries")));

  /* include the vertical box in horizontal box */
  gtk_paned_pack1 (GTK_PANED (hpaned), notebook, TRUE, FALSE);

                     
  /* -- preview area -- */
  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                    /* GtkFrame */
                                    "label",        _("Preview"),
                                    NULL));
  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        /* GtkAlignment */
                                        "right-padding", 5,
                                        "left-padding",  5,
                                        "xscale",        0.0,
                                        "yscale",        0.0,
                                        "xalign",        0.5,
                                        "yalign",        0.5,
                                        NULL));
  preview = GTK_WIDGET (g_object_new (TYPE_PREVIEW,
                                      /* Preview */
                                      "active", TRUE,
                                      NULL));
  gtk_container_add (GTK_CONTAINER (alignment), preview);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  /* set preview of compselect */
  compselect->preview = PREVIEW (preview);

  gtk_paned_pack2 (GTK_PANED (hpaned), frame, FALSE, FALSE);

  /* add the hpaned to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (compselect)->vbox), hpaned,
                      TRUE, TRUE, 0);
  gtk_widget_show_all (hpaned);
  

  /* -- behavior combo box -- */
  combobox = create_behaviors_combo_box ();
  g_signal_connect (combobox,
                    "changed",
                    G_CALLBACK (compselect_callback_behavior_changed),
                    compselect);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (compselect)->vbox), combobox,
                      FALSE, FALSE, 10);
  gtk_widget_show_all (combobox);
  /* set behavior combo box of compselect */
  compselect->combobox_behaviors = GTK_COMBO_BOX (combobox);
  
  /* now add buttons in the action area */
  gtk_dialog_add_buttons (GTK_DIALOG (compselect),
                          /*  - close button */
                          GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
                          /*  - update button */
                          GTK_STOCK_APPLY, COMPSELECT_RESPONSE_PLACE,
			  GTK_STOCK_OK, COMPSELECT_RESPONSE_HIDE,
                          /*  - refresh button */
                          GTK_STOCK_REFRESH, COMPSELECT_RESPONSE_REFRESH,
                          NULL);

#if GTK_CHECK_VERSION (2,6,0)
  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order(GTK_DIALOG(compselect),
                                          COMPSELECT_RESPONSE_REFRESH,
					  COMPSELECT_RESPONSE_HIDE,
					  COMPSELECT_RESPONSE_PLACE,
					  GTK_RESPONSE_CLOSE,
					  -1);
#endif

  /* Add refresh handler */
  g_signal_connect (compselect, "response",
                    G_CALLBACK (compselect_callback_refresh_library),
                    NULL);

  /* Initialize the hidden property */
  compselect->hidden = FALSE;
}

static void
compselect_finalize (GObject *object)
{
  Compselect *compselect = COMPSELECT (object);

  if (compselect->filter_timeout != 0) {
    g_source_remove (compselect->filter_timeout);
    compselect->filter_timeout = 0;
  }
  
  G_OBJECT_CLASS (compselect_parent_class)->finalize (object);
}

static void
compselect_set_property (GObject *object,
                         guint property_id,
                         const GValue *value,
                         GParamSpec *pspec)
{
  Compselect *compselect = COMPSELECT (object);

  switch(property_id) {
    case PROP_BEHAVIOR:
      gtk_combo_box_set_active (compselect->combobox_behaviors,
                                g_value_get_enum (value));
      break;
    case PROP_HIDDEN:
      compselect->hidden = g_value_get_boolean(value);
      if (compselect->hidden) 
	gtk_widget_hide(GTK_WIDGET(compselect));
      else
	gtk_window_present (GTK_WINDOW(compselect));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}

static void
compselect_get_property (GObject *object,
                         guint property_id,
                         GValue *value,
                         GParamSpec *pspec)
{
  Compselect *compselect = COMPSELECT (object);

  switch(property_id) {
      case PROP_SYMBOL: 
	{
	  GtkTreeModel *model;
	  GtkTreeIter iter, parent;
	  CLibSymbol *symbol = NULL;

          /* FIXME assumes pages are in a specific order */
          switch (gtk_notebook_get_current_page(compselect->viewtabs)) {
          case 0: /* In Use page */
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->inusetreeview),
                  &model,
                  &iter)) {
              gtk_tree_model_get (model, &iter, 0, &symbol, -1);
            }
            break;
          case 1:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->libtreeview),
                  &model,
                  &iter)
                && gtk_tree_model_iter_parent (model, &parent, &iter)) {
              gtk_tree_model_get (model, &iter, 0, &symbol, -1);
            }
            break;
          default:
            g_assert_not_reached();
          }

          g_value_set_pointer (value, symbol);
          break;
        }
      case PROP_BEHAVIOR:
        g_value_set_enum (value,
                          gtk_combo_box_get_active (
                            compselect->combobox_behaviors));
        break;
      case PROP_HIDDEN:
	g_value_set_boolean(value, compselect->hidden);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}



GType
compselect_behavior_get_type (void)
{
  static GType etype = 0;
  
  if (etype == 0) {
    static const GEnumValue values[] = {
      { COMPSELECT_BEHAVIOR_REFERENCE, "COMPSELECT_BEHAVIOR_REFERENCE", "reference" },
      { COMPSELECT_BEHAVIOR_EMBED,     "COMPSELECT_BEHAVIOR_EMBED",     "embed" },
      { COMPSELECT_BEHAVIOR_INCLUDE,   "COMPSELECT_BEHAVIOR_INCLUDE",   "include" },
      { 0, NULL, NULL }
    };
    
    etype = g_enum_register_static ("CompselectBehavior", values);
  }
  
  return etype;
}
