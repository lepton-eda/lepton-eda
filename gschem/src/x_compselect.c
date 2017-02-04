/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"
#include <gdk/gdkkeysyms.h>

#include "../include/gschem_dialog.h"
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
#define COMPSELECT_FILTER_INTERVAL 200


enum compselect_view {
  VIEW_INUSE=0,
  VIEW_CLIB
};


/*! \brief Return currently active component-selector view
 *
 *  \par Function Description
 *  This function returns which one of the possible views is active
 *  in the component selector: VIEW_INUSE for the in-use view, or
 *  VIEW_CLIB for the component library view.
 *
 *  \todo FIXME: This function assumes the GtkNotebook pages displaying the
 *               views are in a specific order.
 *
 *  \param [in] compselect  The component selection dialog.
 *  \returns The currently active view (from the compselect_view enum).
 */
static enum compselect_view
compselect_get_view (Compselect *compselect)
{
  switch (gtk_notebook_get_current_page (compselect->viewtabs)) {
    case 0: return VIEW_INUSE;  /* In use page */
    case 1: return VIEW_CLIB;   /* Component library page */
    default:
      g_critical ("compselect_get_view: Unknown tab position\n");
      return 0;
  }
}


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
 *  \param [in] user_data A pointer on the GschemToplevel environment.
 */
static void
x_compselect_callback_response (GtkDialog *dialog,
                                gint arg1,
                                gpointer user_data)
{
  Compselect *compselect = (Compselect*)dialog;
  GschemToplevel *w_current = (GschemToplevel *)user_data;
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);

  switch (arg1) {
      case COMPSELECT_RESPONSE_PLACE: {
        CLibSymbol *symbol = NULL;
        CompselectBehavior behavior;

        g_object_get (compselect,
                      "symbol", &symbol,
                      "behavior", &behavior,
                      NULL);

        w_current->include_complex = w_current->embed_complex = 0;
        switch (behavior) {
            case COMPSELECT_BEHAVIOR_REFERENCE:
              break;
            case COMPSELECT_BEHAVIOR_EMBED:
              w_current->embed_complex   = 1;
              break;
            case COMPSELECT_BEHAVIOR_INCLUDE:
              w_current->include_complex = 1;
              break;
            default:
              g_assert_not_reached ();
        }

        if (w_current->event_state == COMPMODE) {
          /* Delete the component which was being placed */
          if (w_current->rubber_visible)
            o_place_invalidate_rubber (w_current, FALSE);
          w_current->rubber_visible = 0;
          geda_object_list_delete (toplevel,
                                   toplevel->page_current->place_list);
          toplevel->page_current->place_list = NULL;
        } else {
          /* Cancel whatever other action is currently in progress */
          o_redraw_cleanstates (w_current);
        }

        if (symbol == NULL) {
          /* If there is no symbol selected, switch to SELECT mode */
          i_set_state (w_current, SELECT);
          i_action_stop (w_current);
        } else {
          /* Otherwise set the new symbol to place */
          o_complex_prepare_place (w_current, symbol);
        }
        break;
      }

      case COMPSELECT_RESPONSE_HIDE:
        /* Response when clicking on the "hide" button */

        /* If there is no component in the complex place list, set the current one */
        if (toplevel->page_current->place_list == NULL) {
          gtk_dialog_response (GTK_DIALOG (compselect),
                               COMPSELECT_RESPONSE_PLACE);
        }

        /* Hide the component selector */
        g_object_set (G_OBJECT (compselect), "hidden", TRUE, NULL);
        break;

      case GTK_RESPONSE_CLOSE:
      case GTK_RESPONSE_DELETE_EVENT:
        g_assert (GTK_WIDGET (dialog) == w_current->cswindow);
        gtk_widget_destroy (GTK_WIDGET (dialog));
        w_current->cswindow = NULL;

        if (w_current->event_state == COMPMODE) {

          /* Cancel the place operation currently in progress */
          o_redraw_cleanstates (w_current);

          /* return to the default state */
          i_set_state (w_current, SELECT);
          i_action_stop (w_current);
        }
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
 *  \param [in] w_current  The GschemToplevel environment.
 */
void
x_compselect_open (GschemToplevel *w_current)
{
  GtkWidget *current_tab, *entry_filter;
  GtkNotebook *compselect_notebook;

  if (w_current->cswindow == NULL) {
    w_current->cswindow = GTK_WIDGET (
      g_object_new (TYPE_COMPSELECT,
                    /* GschemDialog */
                    "settings-name", "compselect",
                    "gschem-toplevel", w_current,
                    NULL));

    g_signal_connect (w_current->cswindow,
                      "response",
                      G_CALLBACK (x_compselect_callback_response),
                      w_current);

    gtk_window_set_transient_for (GTK_WINDOW (w_current->cswindow),
                                  GTK_WINDOW (w_current->main_window));

    gtk_widget_show (w_current->cswindow);

  } else {
    gtk_window_present (GTK_WINDOW (w_current->cswindow));
  }
  gtk_editable_select_region (GTK_EDITABLE (COMPSELECT (w_current->cswindow)->entry_filter), 0, -1);

  /* Set the focus to the filter entry only if it is in the current
     displayed tab */
  compselect_notebook = GTK_NOTEBOOK (COMPSELECT (w_current->cswindow)->viewtabs);
  current_tab = gtk_notebook_get_nth_page (compselect_notebook,
                                           gtk_notebook_get_current_page (compselect_notebook));
  entry_filter = GTK_WIDGET (COMPSELECT (w_current->cswindow)->entry_filter);
  if (gtk_widget_is_ancestor (entry_filter, current_tab)) {
    gtk_widget_grab_focus (entry_filter);
  }
}


void
x_compselect_deselect (GschemToplevel *w_current)
{
  Compselect *compselect = COMPSELECT (w_current->cswindow);

  if (compselect == NULL)
    return;

  switch (compselect_get_view (compselect)) {
  case VIEW_INUSE:
    gtk_tree_selection_unselect_all (
      gtk_tree_view_get_selection (compselect->inusetreeview));
    break;
  case VIEW_CLIB:
    gtk_tree_selection_unselect_all (
      gtk_tree_view_get_selection (compselect->libtreeview));
    break;
  default:
    g_assert_not_reached ();
  }
}


enum {
  PROP_SYMBOL=1,
  PROP_BEHAVIOR,
  PROP_HIDDEN
};

static GObjectClass *compselect_parent_class = NULL;


static void compselect_class_init      (CompselectClass *class);
static GObject *compselect_constructor (GType type,
                                        guint n_construct_properties,
                                        GObjectConstructParam *construct_params);
static void compselect_finalize        (GObject *object);
static void compselect_set_property    (GObject *object,
                                        guint property_id,
                                        const GValue *value,
                                        GParamSpec *pspec);
static void compselect_get_property    (GObject *object,
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
  CLibSymbol *symbol;

  gtk_tree_model_get (tree_model, iter, 0, &symbol, -1);
  g_object_set ((GObject*)cell, "text", s_clib_symbol_get_name (symbol), NULL);
}

/*! \brief Returns whether a library treeview node represents a symbol. */

static gboolean is_symbol(GtkTreeModel *tree_model, GtkTreeIter *iter)
{
  gboolean result;
  gtk_tree_model_get (tree_model, iter, 2, &result, -1);
  return result;
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
  gchar *compname_upper, *text_upper, *pattern;
  const gchar *text;
  gboolean ret;

  g_assert (IS_COMPSELECT (data));

  text = gtk_entry_get_text (compselect->entry_filter);
  if (g_ascii_strcasecmp (text, "") == 0) {
    return TRUE;
  }

  /* If this is a source, only display it if it has children that
   * match */
  if (!is_symbol (model, iter)) {
    GtkTreeIter iter2;

    ret = FALSE;
    if (gtk_tree_model_iter_children (model, &iter2, iter))
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
    compname_upper = g_ascii_strup (compname, -1);
    text_upper = g_ascii_strup (text, -1);
    pattern = g_strconcat ("*", text_upper, "*", NULL);
    ret = g_pattern_match_simple (pattern, compname_upper);
    g_free (compname_upper);
    g_free (text_upper);
    g_free (pattern);
  }

  return ret;
}


/*! \brief Handles activation (e.g. double-clicking) of a component row
 *  \par Function Description
 *  Component row activated handler:
 *  As a convenience to the user, expand / contract any node with children.
 *  Hide the component selector if a node without children is activated.
 *
 *  \param [in] tree_view The component treeview.
 *  \param [in] path      The GtkTreePath to the activated row.
 *  \param [in] column    The GtkTreeViewColumn in which the activation occurred.
 *  \param [in] user_data The component selection dialog.
 */
static void
tree_row_activated (GtkTreeView       *tree_view,
                    GtkTreePath       *path,
                    GtkTreeViewColumn *column,
                    gpointer           user_data)
{
  GtkTreeModel *model;
  GtkTreeIter iter;
  Compselect *compselect = (Compselect*)user_data;

  model = gtk_tree_view_get_model (tree_view);
  gtk_tree_model_get_iter (model, &iter, path);

  if (is_symbol (model, &iter)) {
    gtk_dialog_response (GTK_DIALOG (compselect),
                         COMPSELECT_RESPONSE_HIDE);
    return;
  }

  if (gtk_tree_view_row_expanded (tree_view, path))
    gtk_tree_view_collapse_row (tree_view, path);
  else
    gtk_tree_view_expand_row (tree_view, path, FALSE);
}

/*! \brief GCompareFunc to sort an text object list by the object strings
 */
static gint
sort_object_text (OBJECT *a, OBJECT *b)
{
  return strcmp (geda_text_object_get_string (a),
                 geda_text_object_get_string (b));
}

enum {
  ATTRIBUTE_COLUMN_NAME = 0,
  ATTRIBUTE_COLUMN_VALUE,
  NUM_ATTRIBUTE_COLUMNS
};

/*! \brief Update the model of the attributes treeview
 *  \par Function Description
 *  This function takes the toplevel attributes from the preview widget and
 *  puts them into the model of the <b>attrtreeview</b> widget.
 *  \param [in] compselect       The dialog compselect
 *  \param [in] preview_toplevel The toplevel of the preview widget
 */
void
update_attributes_model (Compselect *compselect, TOPLEVEL *preview_toplevel)
{
  GtkListStore *model;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GList *o_iter, *o_attrlist;
  gchar *name, *value;
  OBJECT *o_current;
  EdaConfig *cfg;
  gchar **filter_list;
  gint i;
  gsize n;

  model = (GtkListStore*) gtk_tree_view_get_model (compselect->attrtreeview);
  gtk_list_store_clear (model);

  /* Invalidate the column width for the attribute value column, so
   * the column is re-sized based on the new data being shown. Symbols
   * with long values are common, and we don't want having viewed those
   * forcing a h-scroll-bar on symbols with short valued attributes.
   *
   * We might also consider invalidating the attribute name columns,
   * however that gives an inconsistent column division when swithing
   * between symbols, which doesn't look nice. For now, assume that
   * the name column can keep the max width gained whilst previewing.
   */
  column = gtk_tree_view_get_column (compselect->attrtreeview,
                                     ATTRIBUTE_COLUMN_VALUE);
  gtk_tree_view_column_queue_resize (column);

  if (preview_toplevel->page_current == NULL) {
    return;
  }

  o_attrlist = o_attrib_find_floating_attribs (
                              s_page_objects (preview_toplevel->page_current));

  cfg = eda_config_get_context_for_path (preview_toplevel->page_current->page_filename);
  filter_list = eda_config_get_string_list (cfg, "gschem.library",
                                            "component-attributes", &n, NULL);

  if (filter_list == NULL || (n > 0 && strcmp (filter_list[0], "*") == 0)) {
    /* display all attributes in alphabetical order */
    o_attrlist = g_list_sort (o_attrlist, (GCompareFunc) sort_object_text);
    for (o_iter = o_attrlist; o_iter != NULL; o_iter = g_list_next (o_iter)) {
      o_current = o_iter->data;
      o_attrib_get_name_value (o_current, &name, &value);
      gtk_list_store_append (model, &iter);
      gtk_list_store_set (model, &iter, 0, name, 1, value, -1);
      g_free (name);
      g_free (value);
    }
  } else {
    /* display only attribute that are in the filter list */
    for (i = 0; i < n; i++) {
      for (o_iter = o_attrlist; o_iter != NULL; o_iter = g_list_next (o_iter)) {
        o_current = o_iter->data;
        if (o_attrib_get_name_value (o_current, &name, &value)) {
          if (strcmp (name, filter_list[i]) == 0) {
            gtk_list_store_append (model, &iter);
            gtk_list_store_set (model, &iter, 0, name, 1, value, -1);
          }
          g_free (name);
          g_free (value);
        }
      }
    }
  }

  /* Hide the attributes list if the list of attributes to show is
   * empty. */
  gtk_widget_set_visible (compselect->attrframe, n != 0);

  g_strfreev (filter_list);
  g_list_free (o_attrlist);
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
  GtkTreeIter iter;
  Compselect *compselect = (Compselect*)user_data;
  const CLibSymbol *sym = NULL;
  gchar *buffer = NULL;

  if (gtk_tree_selection_get_selected (selection, &model, &iter)) {

    view = gtk_tree_selection_get_tree_view (selection);
    if (view == compselect->inusetreeview ||
        /* No special handling required */
        (view == compselect->libtreeview && is_symbol (model, &iter))) {
         /* Tree view needs to check that we're at a symbol node */

      gtk_tree_model_get (model, &iter, 0, &sym, -1);
      buffer = s_clib_symbol_get_data (sym);
    }
  }

  /* update the preview with new symbol data */
  g_object_set (compselect->preview,
                "buffer", buffer,
                "active", (buffer != NULL),
                NULL);

  /* update the attributes with the toplevel of the preview widget*/
  update_attributes_model (compselect,
                           compselect->preview->preview_w_current->toplevel);

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
    const gchar *text = gtk_entry_get_text (compselect->entry_filter);
    gtk_tree_model_filter_refilter ((GtkTreeModelFilter*)model);
    if (strcmp (text, "") != 0) {
      /* filter text not-empty */
      gtk_tree_view_expand_all (compselect->libtreeview);
    } else {
      /* filter text is empty, collapse expanded tree */
      gtk_tree_view_collapse_all (compselect->libtreeview);
    }
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
  if (gtk_widget_is_sensitive (button) != sensitive) {
    gtk_widget_set_sensitive (button, sensitive);
  }

  /* Cancel any pending update of the component list filter */
  if (compselect->filter_timeout != 0)
    g_source_remove (compselect->filter_timeout);

  /* Schedule an update of the component list filter in
   * COMPSELECT_FILTER_INTERVAL milliseconds */
  compselect->filter_timeout = g_timeout_add (COMPSELECT_FILTER_INTERVAL,
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
 *  \param [in] button    The clear button
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
create_inuse_tree_model (Compselect *compselect)
{
  GtkListStore *store;
  GList *symhead, *symlist;
  GtkTreeIter iter;

  store = (GtkListStore *) gtk_list_store_new (1, G_TYPE_POINTER);

  symhead = s_toplevel_get_symbols (GSCHEM_DIALOG (compselect)->w_current->toplevel);

  for (symlist = symhead;
       symlist != NULL;
       symlist = g_list_next (symlist)) {

    gtk_list_store_append (store, &iter);

    gtk_list_store_set (store, &iter,
                        0, symlist->data,
                        -1);
  }

  g_list_free (symhead);

  return (GtkTreeModel*)store;
}

/* \brief Helper function for create_lib_tree_model. */

static void populate_component_store(GtkTreeStore *store, GList **srclist,
                                     GtkTreeIter *parent, const char *prefix)
{
  CLibSource *source = (CLibSource *)(*srclist)->data;
  const char *name = s_clib_source_get_name (source);

  char *text, *new_prefix;
  GList *new_srclist;

  if (*name == '\0') {
    text = NULL;
    new_prefix = NULL;
    new_srclist = NULL;
  } else if (*name != '/') {
    /* directory added by component-library */
    text = strdup(name);
    if (text == NULL) {
      fprintf(stderr, "Not enough memory\n");
      return;
    }
    new_prefix = NULL;
    new_srclist = NULL;
  } else {
    /* directory added by component-library-search */
    g_assert(strncmp(name, prefix, strlen(prefix)) == 0);
    char *p = strchr(name + strlen(prefix) + 1, '/');

    if (p != NULL) {
      /* There is a parent directory that was skipped
         because it doesn't contain symbols. */
      source = NULL;
      size_t prefix_len = strlen(prefix);
      text = malloc(p - name - prefix_len + 1);
      if (text == NULL) {
        fprintf(stderr, "Not enough memory\n");
        return;
      }
      memcpy(text, name + prefix_len, p - name - prefix_len);
      text[p - name - prefix_len] = '\0';
      new_prefix = malloc(p - name + 2);
      if (new_prefix == NULL) {
        fprintf(stderr, "Not enough memory\n");
        free(text);
        return;
      }
      memcpy(new_prefix, name, p - name + 1);
      new_prefix[p - name + 1] = '\0';
      new_srclist = *srclist;
    } else {
      size_t prefix_len = strlen(prefix);
      size_t name_len = strlen(name);
      text = malloc(name_len - prefix_len + 1);
      if (text == NULL) {
        fprintf(stderr, "Not enough memory\n");
        return;
      }
      memcpy(text, name + prefix_len, name_len - prefix_len);
      text[name_len - prefix_len] = '\0';
      new_prefix = malloc(name_len + 2);
      if (new_prefix == NULL) {
        fprintf(stderr, "Not enough memory\n");
        free(text);
        return;
      }
      memcpy(new_prefix, name, name_len);
      new_prefix[name_len] = '/';
      new_prefix[name_len + 1] = '\0';
      new_srclist = g_list_next (*srclist);
    }
  }

  GtkTreeIter iter;
  gtk_tree_store_append (store, &iter, parent);
  gtk_tree_store_set (store, &iter,
                      0, source,
                      1, text,
                      2, FALSE,
                      -1);
  free(text);

  /* Look ahead, adding subdirectories. */
  while (new_srclist != NULL &&
         strncmp(s_clib_source_get_name ((CLibSource *)new_srclist->data),
                 new_prefix, strlen(new_prefix)) == 0) {
    *srclist = new_srclist;
    populate_component_store(store, srclist, &iter, new_prefix);
    new_srclist = g_list_next (*srclist);
  }
  free(new_prefix);

  /* populate symbols */
  GList *symhead, *symlist;
  GtkTreeIter iter2;
  symhead = s_clib_source_get_symbols (source);
  for (symlist = symhead;
       symlist != NULL;
       symlist = g_list_next (symlist)) {

    gtk_tree_store_append (store, &iter2, &iter);
    gtk_tree_store_set (store, &iter2,
                        0, symlist->data,
                        1, s_clib_symbol_get_name ((CLibSymbol *)symlist->data),
                        2, TRUE,
                        -1);
  }
  g_list_free (symhead);
}

/* \brief Create the tree model for the "Library" view.
 * \par Function Description
 * Creates a tree where the branches are the available component
 * sources and the leaves are the symbols.
 */
static GtkTreeModel*
create_lib_tree_model (Compselect *compselect)
{
  GtkTreeStore *store;
  GList *srchead, *srclist;
  PAGE *page = GSCHEM_DIALOG(compselect)->w_current->toplevel->page_current;
  EdaConfig *cfg = eda_config_get_context_for_path (page->page_filename);
  gboolean sort = eda_config_get_boolean (cfg, "gschem.library", "sort", NULL);

  store = (GtkTreeStore*)gtk_tree_store_new (3, G_TYPE_POINTER,
                                                G_TYPE_STRING,
                                                G_TYPE_BOOLEAN);

  /* populate component store */
  srchead = s_clib_get_sources (sort);
  for (srclist = srchead;
       srclist != NULL;
       srclist = g_list_next (srclist)) {
    populate_component_store(store, &srclist, NULL, "/");
  }
  g_list_free (srchead);

  return (GtkTreeModel*)store;
}

/* \brief On-demand refresh of the component library.
 * \par Function Description
 * Requests a rescan of the component library in order to pick up any
 * new signals, and then updates the component selector.
 */
static void
compselect_callback_refresh_library (GtkButton *button, gpointer user_data)
{
  Compselect *compselect = COMPSELECT (user_data);
  GtkTreeModel *model;
  GtkTreeSelection *selection;

  /* Rescan the libraries for symbols */
  s_clib_refresh ();

  /* Refresh the "Library" view */
  g_object_unref (gtk_tree_view_get_model (compselect->libtreeview));
  model = (GtkTreeModel *)
    g_object_new (GTK_TYPE_TREE_MODEL_FILTER,
                  "child-model", create_lib_tree_model (compselect),
                  "virtual-root", NULL,
                  NULL);

  gtk_tree_model_filter_set_visible_func ((GtkTreeModelFilter*)model,
                                          lib_model_filter_visible_func,
                                          compselect,
                                          NULL);

  /* Block handling selection updated for duration of model changes */
  selection = gtk_tree_view_get_selection (compselect->libtreeview);
  g_signal_handlers_block_by_func (selection,
                                   compselect_callback_tree_selection_changed,
                                   compselect);

  /* Update the view model with signals blocked */
  gtk_tree_view_set_model (compselect->libtreeview, model);

  /* Refresh the "In Use" view */
  g_object_unref (gtk_tree_view_get_model (compselect->inusetreeview));
  model = create_inuse_tree_model (compselect);

  /* Here we can update the model without blocking signals
   * as this is the second (final) tree view we are updating */
  gtk_tree_view_set_model (compselect->inusetreeview, model);

  /* Unblock & fire handler for libtreeview selection */
  g_signal_handlers_unblock_by_func (selection,
                                     compselect_callback_tree_selection_changed,
                                     compselect);
}

/*! \brief Creates the treeview for the "In Use" view. */
static GtkWidget*
create_inuse_treeview (Compselect *compselect)
{
  GtkWidget *scrolled_win, *treeview, *vbox, *hbox, *button;
  GtkTreeModel *model;
  GtkTreeSelection *selection;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  model = create_inuse_tree_model (compselect);

  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkContainer */
                                   "border-width", 5,
                                   /* GtkBox */
                                   "homogeneous",  FALSE,
                                   "spacing",      5,
                                   NULL));

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

  g_signal_connect (treeview,
                    "row-activated",
                    G_CALLBACK (tree_row_activated),
                    compselect);

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

  /* add the scrolled window for directories to the vertical box */
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win,
                      TRUE, TRUE, 0);

  /* -- refresh button area -- */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                          /* GtkBox */
                                          "homogeneous", FALSE,
                                          "spacing",     3,
                                          NULL));
  /* create the refresh button */
  button = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                     /* GtkWidget */
                                     "sensitive", TRUE,
                                     /* GtkButton */
                                     "relief",    GTK_RELIEF_NONE,
                                     NULL));
  gtk_container_add (GTK_CONTAINER (button),
                     gtk_image_new_from_stock (GTK_STOCK_REFRESH,
                                            GTK_ICON_SIZE_SMALL_TOOLBAR));
  /* add the refresh button to the horizontal box at the end */
  gtk_box_pack_end (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect (button,
                    "clicked",
                    G_CALLBACK (compselect_callback_refresh_library),
                    compselect);

  /* add the refresh button area to the vertical box */
  gtk_box_pack_start (GTK_BOX (vbox), hbox,
                      FALSE, FALSE, 0);

  return vbox;
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

  child_model  = create_lib_tree_model (compselect);
  model = (GtkTreeModel*)g_object_new (GTK_TYPE_TREE_MODEL_FILTER,
                                       "child-model",  child_model,
                                       "virtual-root", NULL,
                                       NULL);

  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkContainer */
                  "border-width", 5,
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

  g_signal_connect (libtreeview,
                    "row-activated",
                    G_CALLBACK (tree_row_activated),
                    compselect);

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
  gtk_tree_view_column_add_attribute (column, renderer, "text", 1);
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

  /* create the refresh button */
  button = GTK_WIDGET (g_object_new (GTK_TYPE_BUTTON,
                                     /* GtkWidget */
                                     "sensitive", TRUE,
                                     /* GtkButton */
                                     "relief",    GTK_RELIEF_NONE,
                                     NULL));
  gtk_container_add (GTK_CONTAINER (button),
                     gtk_image_new_from_stock (GTK_STOCK_REFRESH,
                                            GTK_ICON_SIZE_SMALL_TOOLBAR));
  /* add the refresh button to the filter area */
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, FALSE, 0);
  g_signal_connect (button,
                    "clicked",
                    G_CALLBACK (compselect_callback_refresh_library),
                    compselect);

  /* add the filter area to the vertical box */
  gtk_box_pack_start (GTK_BOX (vbox), hbox,
                      FALSE, FALSE, 0);

  compselect->libtreeview = GTK_TREE_VIEW (libtreeview);

  return vbox;
}

/*! \brief Creates the treeview widget for the attributes
 */
static GtkWidget*
create_attributes_treeview (Compselect *compselect)
{
  GtkWidget *attrtreeview, *scrolled_win;
  GtkListStore *model;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;

  model = gtk_list_store_new (NUM_ATTRIBUTE_COLUMNS,
                              G_TYPE_STRING, G_TYPE_STRING);

  attrtreeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                           /* GtkTreeView */
                                           "model",      model,
                                           "headers-visible", FALSE,
                                           "rules-hint", TRUE,
                                           NULL));

  /* two columns for name and value of the attributes */
  renderer = GTK_CELL_RENDERER (g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
                                              "editable", FALSE,
                                              NULL));

  column = GTK_TREE_VIEW_COLUMN (g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                               "title", _("Name"),
                                               "resizable", TRUE,
                                               NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text",
                                      ATTRIBUTE_COLUMN_NAME);
  gtk_tree_view_append_column (GTK_TREE_VIEW (attrtreeview), column);

  column = GTK_TREE_VIEW_COLUMN (g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
                                               "title", _("Value"),
                                               "resizable", TRUE,
                                               NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_add_attribute (column, renderer, "text",
                                      ATTRIBUTE_COLUMN_VALUE);
  gtk_tree_view_append_column (GTK_TREE_VIEW (attrtreeview), column);

  scrolled_win = GTK_WIDGET (g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                                           /* GtkContainer */
                                           "border-width", 5,
                                           /* GtkScrolledWindow */
                                           "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                           "vscrollbar-policy", GTK_POLICY_AUTOMATIC,
                                           "shadow-type",       GTK_SHADOW_ETCHED_IN,
                                           NULL));

  gtk_container_add (GTK_CONTAINER (scrolled_win), attrtreeview);

  compselect->attrtreeview = GTK_TREE_VIEW (attrtreeview);

  return scrolled_win;
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
      sizeof (CompselectClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) compselect_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (Compselect),
      0,    /* n_preallocs */
      NULL  /* instance_init */
    };

    compselect_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                              "Compselect",
                                              &compselect_info, 0);
  }

  return compselect_type;
}


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
compselect_geometry_save (GschemDialog *dialog, EdaConfig *cfg, gchar *group_name)
{
  int position;

  /* Call the parent's geometry_save method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
    geometry_save (dialog, cfg, group_name);

  position = gtk_paned_get_position (GTK_PANED (COMPSELECT (dialog)->hpaned));
  eda_config_set_int (cfg, group_name, "hpaned", position);

  position = gtk_paned_get_position (GTK_PANED (COMPSELECT (dialog)->vpaned));
  eda_config_set_int (cfg, group_name, "vpaned", position);

  position = gtk_notebook_get_current_page (COMPSELECT (dialog)->viewtabs);
  eda_config_set_int (cfg, group_name, "source-tab", position);
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
compselect_geometry_restore (GschemDialog *dialog, EdaConfig *cfg, gchar *group_name)
{
  int position;

  /* Call the parent's geometry_restore method */
  GSCHEM_DIALOG_CLASS (compselect_parent_class)->
    geometry_restore (dialog, cfg, group_name);

  position = eda_config_get_int (cfg, group_name, "hpaned", NULL);
  if (position != 0)
    gtk_paned_set_position (GTK_PANED (COMPSELECT (dialog)->hpaned), position);

  position = eda_config_get_int (cfg, group_name, "vpaned", NULL);
  if (position != 0)
    gtk_paned_set_position (GTK_PANED (COMPSELECT (dialog)->vpaned), position);

  position = eda_config_get_int (cfg, group_name, "source-tab", NULL);
  gtk_notebook_set_current_page (COMPSELECT (dialog)->viewtabs, position);
}


static void
compselect_class_init (CompselectClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GschemDialogClass *gschem_dialog_class = GSCHEM_DIALOG_CLASS (klass);

  gschem_dialog_class->geometry_save    = compselect_geometry_save;
  gschem_dialog_class->geometry_restore = compselect_geometry_restore;

  gobject_class->constructor  = compselect_constructor;
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

static GObject*
compselect_constructor (GType type,
                        guint n_construct_properties,
                        GObjectConstructParam *construct_params)
{
  GObject *object;
  Compselect *compselect;

  GtkWidget *hpaned, *vpaned, *notebook, *attributes;
  GtkWidget *libview, *inuseview;
  GtkWidget *preview, *combobox;
  GtkWidget *alignment, *frame;

  /* chain up to constructor of parent class */
  object = G_OBJECT_CLASS (compselect_parent_class)->
    constructor (type, n_construct_properties, construct_params);
  compselect = COMPSELECT (object);

  /* dialog initialization */
  g_object_set (object,
                /* GtkWindow */
                "title",           _("Select Component..."),
                "default-height",  300,
                "default-width",   400,
                NULL);

  /* vertical pane containing preview and attributes */
  vpaned = GTK_WIDGET (g_object_new (GTK_TYPE_VPANED, NULL));
  compselect->vpaned = vpaned;

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
                                    "label", _("Preview"),
                                    NULL));
  alignment = GTK_WIDGET (g_object_new (GTK_TYPE_ALIGNMENT,
                                        /* GtkAlignment */
                                        "border-width", 5,
                                        "xscale",         1.0,
                                        "yscale",         1.0,
                                        "xalign",         0.5,
                                        "yalign",         0.5,
                                        NULL));

  preview = gschem_preview_new ();

  gtk_container_add (GTK_CONTAINER (alignment), preview);
  gtk_container_add (GTK_CONTAINER (frame), alignment);
  /* set preview of compselect */
  compselect->preview = GSCHEM_PREVIEW (preview);
  g_object_set (GTK_WIDGET (preview),
                "width-request",  160,
                "height-request", 120,
                NULL);

  gtk_paned_pack1 (GTK_PANED (vpaned), frame, FALSE, FALSE);

  compselect->attrframe = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                                    /* GtkFrame */
                                                    "label", _("Attributes"),
                                                    NULL));
  attributes = create_attributes_treeview (compselect);
  gtk_paned_pack2 (GTK_PANED (vpaned), compselect->attrframe, FALSE, FALSE);
  gtk_container_add (GTK_CONTAINER (compselect->attrframe), attributes);

  gtk_paned_pack2 (GTK_PANED (hpaned), vpaned, FALSE, FALSE);

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
                          GTK_STOCK_OK, COMPSELECT_RESPONSE_HIDE,
                          NULL);

  /* Set the alternative button order (ok, cancel, help) for other systems */
  gtk_dialog_set_alternative_button_order (GTK_DIALOG (compselect),
                                          COMPSELECT_RESPONSE_HIDE,
                                          GTK_RESPONSE_CLOSE,
                                          -1);

  /* Initialize the hidden property */
  compselect->hidden = FALSE;

  return object;
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

  switch (property_id) {
    case PROP_BEHAVIOR:
      gtk_combo_box_set_active (compselect->combobox_behaviors,
                                g_value_get_enum (value));
      break;
    case PROP_HIDDEN:
      compselect->hidden = g_value_get_boolean (value);
      if (compselect->hidden)
        gtk_widget_hide (GTK_WIDGET (compselect));
      else
        gtk_window_present (GTK_WINDOW (compselect));
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

  switch (property_id) {
      case PROP_SYMBOL:
        {
          GtkTreeModel *model;
          GtkTreeIter iter;
          CLibSymbol *symbol = NULL;

          switch (compselect_get_view (compselect)) {
          case VIEW_INUSE:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->inusetreeview),
                  &model,
                  &iter)) {
              gtk_tree_model_get (model, &iter, 0, &symbol, -1);
            }
            break;
          case VIEW_CLIB:
            if (gtk_tree_selection_get_selected (
                  gtk_tree_view_get_selection (compselect->libtreeview),
                  &model,
                  &iter)
                && is_symbol (model, &iter)) {
              gtk_tree_model_get (model, &iter, 0, &symbol, -1);
            }
            break;
          default:
            g_assert_not_reached ();
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
        g_value_set_boolean (value, compselect->hidden);
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
