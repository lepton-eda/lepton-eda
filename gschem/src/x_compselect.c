/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include "../include/x_preview.h"

#include "../include/x_compselect.h"


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
  Preview *preview = (Preview*)dialog;
  TOPLEVEL *toplevel = (TOPLEVEL*)user_data;

  switch (arg1) {
      case GTK_RESPONSE_APPLY: {
        gchar *filename, *directory, *component;
        CompselectBehavior behavior;
        
        g_object_get (preview,
                      "filename", &filename,
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

        if (filename == NULL) {
          break;
        }
        
        component = g_path_get_basename (filename);
        directory = g_path_get_dirname  (filename);
        g_free (filename);
        
        if (toplevel->current_clib == NULL ||
            g_ascii_strcasecmp (toplevel->current_clib, directory) != 0 ||
            toplevel->current_basename == NULL ||
            g_ascii_strcasecmp (toplevel->current_basename, component) != 0) {
          gint diff_x, diff_y;
          
          g_free (toplevel->current_clib);
          toplevel->current_clib = directory;
        
          strcpy (toplevel->current_basename, component);
          g_free (component);
        
          if (toplevel->event_state == ENDCOMP) {
            diff_x = toplevel->last_x - toplevel->start_x;
            diff_y = toplevel->last_y - toplevel->start_y;
            
            o_complex_translate_display(toplevel,
                                        diff_x, diff_y,
                                        toplevel->page_current->complex_place_head);
          }
        
          o_list_delete_rest(toplevel,
                             toplevel->page_current->complex_place_head);
          o_complex_set_filename(toplevel, toplevel->current_clib,
                                 toplevel->current_basename);
        
          toplevel->event_state = DRAWCOMP;
        } else {
          g_free (component);
          g_free (directory);
        }
        break;
      }
      case GTK_RESPONSE_CLOSE:
      case GTK_RESPONSE_DELETE_EVENT:
        g_assert (GTK_WIDGET (dialog) == toplevel->cswindow);
        gtk_widget_destroy (GTK_WIDGET (dialog));
        toplevel->cswindow = NULL;
        break;
      default:
        g_assert_not_reached ();
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
  if (toplevel->cswindow == NULL) {
    toplevel->cswindow = GTK_WIDGET (
      g_object_new (TYPE_COMPSELECT,
                    NULL));

    g_signal_connect (toplevel->cswindow,
                      "response",
                      G_CALLBACK (x_compselect_callback_response),
                      toplevel);
    
    gtk_widget_show (toplevel->cswindow);
    
  } else {
    gdk_window_raise (toplevel->cswindow->window);
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
  PROP_FILENAME=1,
  PROP_BEHAVIOR,
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


/*! \brief Sets data for a particular cell of the treeview.
 *  \par Function Description
 *  This function determines what data is to be displayed in the
 *  component column of the selection tree.
 *
 *  Any toplevel entry of the model (that is one without parent)
 *  represents an absolute path to a directory of the component
 *  library. Only the last part of the path is displayed. Otherwise it
 *  simply copies data from the model to the column.
 *
 *  \param [in] tree_column The GtkTreeColumn.
 *  \param [in] cell        The GtkCellRenderer that is being rendered
 *                          by tree_column.
 *  \param [in] tree_model  The tree model for components.
 *  \param [in] iter        An iterator on the current row to render.
 *  \param [in] data        Unused user data.
 */
static void
compselect_treeview_set_cell_data (GtkTreeViewColumn *tree_column,
                                   GtkCellRenderer   *cell,
                                   GtkTreeModel      *tree_model,
                                   GtkTreeIter       *iter,
                                   gpointer           data)
{
  GtkTreeIter parent;
  GValue value = { 0, };
  
  gtk_tree_model_get_value (tree_model, iter, 0, &value);
  if (!gtk_tree_model_iter_parent (tree_model, &parent, iter)) {
    g_value_set_string_take_ownership (
      &value, g_path_get_basename (g_value_get_string (&value)));
  }
  g_object_set_property ((GObject*)cell, "text", &value);

  g_value_unset (&value);
  
}

/*! \brief Determines visibility of items of the treeview.
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
compselect_model_filter_visible_func (GtkTreeModel *model,
                                      GtkTreeIter  *iter,
                                      gpointer      data)
{
  Compselect *compselect = (Compselect*)data;
  gchar *compname;
  const gchar *text;
  gboolean ret;

  g_assert (IS_COMPSELECT (data));
  
  text = gtk_entry_get_text (compselect->entry_filter);
  if (g_ascii_strcasecmp (text, "") == 0) {
    return TRUE;
  }

  if (gtk_tree_model_iter_has_child (model, iter)) {
    GtkTreeIter iter2;

    gtk_tree_model_iter_children (model, &iter2, iter);
    ret = FALSE;
    do {
      if (compselect_model_filter_visible_func (model, &iter2, data)) {
        ret = TRUE;
        break;
      }
    } while (gtk_tree_model_iter_next (model, &iter2));
  } else {
    gtk_tree_model_get (model, iter,
                        0, &compname,
                        -1);
    ret = (strstr (compname, text) != NULL);
    g_free (compname);
  }

  return ret;
}

/*! \brief Handles a key press on the dialog.
 *  \par Function Description
 *  This is the callback function that is connected to the key press
 *  event of the dialog.
 *
 *  If the user pressed the Escape key, the close response is emitted
 *  requesting the dialog to be deleted.
 *
 *  If any other key is pressed the event is further propagated.
 *
 *  \param [in] widget    The component selection dialog.
 *  \param [in] event     The event structure for key pressed.
 *  \param [in] user_data NULL.
 *  \returns TRUE to stop other handlers from being invoked, FALSE
 *           otherwise.
 */
static gboolean
compselect_callback_dialog_key_press_event (GtkWidget   *widget,
                                            GdkEventKey *event,
                                            gpointer     user_data)
{
  switch (event->keyval) {
      case GDK_Escape:
        /* user pressed escape key, request close of the dialog */
        gtk_dialog_response (GTK_DIALOG (widget), GTK_RESPONSE_CLOSE);
        return TRUE;
  }
  
  /* returns FALSE to propagate event further */
  return FALSE;
}

/*! \brief Handles changes in the filter entry.
 *  \par Function Description
 *  This is the callback function that is called every time the user
 *  select a row in the component treeview of the dialog.
 *
 *  If the selection is not a selection of a component (a directory
 *  name), it does nothing. Otherwise it retrieves the component and
 *  directory name from the model (the directory name is build from
 *  the ancestors of the row in the model).
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
  GtkTreeModel *model;
  GtkTreeIter iter, parent;
  Compselect *compselect = (Compselect*)user_data;
  GString *ret;
  gchar *value, *filename;

  if (!gtk_tree_selection_get_selected (selection, &model, &iter)) {
    return;
  }

  if (gtk_tree_model_iter_has_child (model, &iter)) {
    /* selected element is not a leaf -> not a component name */
    return;
  }
  
  /* build full path to component looking at parents */
  gtk_tree_model_get (model, &iter, 0, &value,-1);
  ret = g_string_new (value);
  g_free (value);
  while (gtk_tree_model_iter_parent (model, &parent, &iter)) {
    gtk_tree_model_get (model, &parent, 0, &value, -1);
    ret = g_string_prepend (ret, G_DIR_SEPARATOR_S);
    ret = g_string_prepend (ret, value);
    g_free (value);
    iter = parent;
  }
  filename = g_string_free (ret, FALSE);

  /* update the treeview with new filename */
  g_object_set (compselect->preview,
                "filename", filename,
                NULL);

  /* signal a component has been selected to parent of dialog */
  g_signal_emit_by_name (compselect,
                         "response",
                         GTK_RESPONSE_APPLY,
                         NULL);

  g_free (filename);

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
  
  model = gtk_tree_view_get_model (compselect->treeview);

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
  /* re-evaluation of filter will occur in 300 ms unless entry */
  /* has been modified again */
  compselect->filter_timeout =
    g_timeout_add (300,
                   compselect_filter_timeout,
                   compselect);
 
}

/*! \brief Handles a click on the clear button.
 *  \par Function Description
 *  This is the callback function called every time the user press the
 *  clear button associated with the filter.
 *
 *  It resets the filter entry and request a re-evaluation of the
 *  filter on the list of symbols to update display.
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
                         GTK_RESPONSE_APPLY,
                         NULL);
}

static GtkTreeModel*
compselect_create_child_model (void)
{
  GtkTreeStore *store;
  const GSList *directories, *dir; 

  store = (GtkTreeStore*)gtk_tree_store_new (1,
                                             G_TYPE_STRING);
  
  /* populate component store */
  directories = s_clib_get_directories ();
  for (dir = directories; dir != NULL; dir = g_slist_next (dir)) {
    GtkTreeIter iter, iter2;
    GSList *components, *comp;

    gtk_tree_store_append (store, &iter, NULL);
    gtk_tree_store_set (store, &iter,
                        0, dir->data,
                        -1);
    
    components = s_clib_get_files ((gchar*)dir->data, ".sym");
    components = g_slist_sort (components, (GCompareFunc)g_ascii_strcasecmp);
    for (comp = components; comp != NULL; comp = g_slist_next (comp)) {
      gtk_tree_store_append (store, &iter2, &iter);
      gtk_tree_store_set (store, &iter2,
                          0, comp->data,
                          -1);
    }

    g_slist_foreach (components, (GFunc)g_free, NULL);
    g_slist_free (components);
  }

  return (GtkTreeModel*)store;
}

/*! \brief Create the combo box for behaviors.
 *  \par Function Description
 *  This function creates and returns a <B>GtkComboBox</B> for
 *  selecting the behavior when a component is added to the sheet.
 */
static GtkWidget*
compselect_create_behaviors_combo_box (void)
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
                
    compselect_type = g_type_register_static (GTK_TYPE_DIALOG,
                                              "Compselect",
                                              &compselect_info, 0);
  }
  
  return compselect_type;
}

static void
compselect_class_init (CompselectClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  compselect_parent_class = g_type_class_peek_parent (klass);
  
  gobject_class->finalize     = compselect_finalize;
  gobject_class->set_property = compselect_set_property;
  gobject_class->get_property = compselect_get_property;

  g_object_class_install_property (
    gobject_class, PROP_FILENAME,
    g_param_spec_string ("filename",
                         "",
                         "",
                         NULL,
                         G_PARAM_READABLE));
  g_object_class_install_property (
    gobject_class, PROP_BEHAVIOR,
    g_param_spec_enum ("behavior",
                       "",
                       "",
                       COMPSELECT_TYPE_BEHAVIOR,
                       COMPSELECT_BEHAVIOR_REFERENCE,
                       G_PARAM_READWRITE));
  
}

static void
compselect_init (Compselect *compselect)
{
  GtkWidget *hbox, *vbox, *filter_hbox;
  GtkWidget *scrolled_win, *treeview, *label, *entry, *preview, *combobox;
  GtkWidget *button;
  GtkTreeModel *child_model, *model;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;

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
  /* connect dialog to key press event */
  g_signal_connect (compselect,
                    "key_press_event",
                    G_CALLBACK (compselect_callback_dialog_key_press_event),
                    NULL);

  
  /* horizontal box selection and preview */
  hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
                                   /* GtkBox */
                                   "homogeneous", FALSE,
                                   "spacing",     5,
                                   NULL));

  /* vertical box for component selection and search entry */
  vbox = GTK_WIDGET (g_object_new (GTK_TYPE_VBOX,
                                   /* GtkContainer */
                                   "border-width", 5,
                                   /* GtkBox */
                                   "homogeneous",  FALSE,
                                   "spacing",      5,
                                   NULL));
  
  /* -- directory/component selection -- */
  child_model  = compselect_create_child_model ();
  model = (GtkTreeModel*)g_object_new (GTK_TYPE_TREE_MODEL_FILTER,
                                       "child-model",  child_model,
                                       "virtual-root", NULL,
                                       NULL);
/*   gtk_tree_model_filter_set_visible_func ((GtkTreeModelFilter*)model, */
/*                                           compselect_model_filter_visible_func, */
/*                                           compselect, */
/*                                           NULL); */
  
  scrolled_win = GTK_WIDGET (
    g_object_new (GTK_TYPE_SCROLLED_WINDOW,
                  /* GtkScrolledWindow */
                  "hscrollbar-policy", GTK_POLICY_AUTOMATIC,
                  "vscrollbar-policy", GTK_POLICY_ALWAYS,
                  "shadow-type",       GTK_SHADOW_ETCHED_IN,
                  NULL));
  /* create the treeview */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
                                       /* GtkTreeView */
                                       "model",      model,
                                       "rules-hint", TRUE,
                                       NULL));
  /* connect callback to selection */
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);
  g_signal_connect (selection,
                    "changed",
                    G_CALLBACK (compselect_callback_tree_selection_changed),
                    compselect);

  /* insert a column to treeview for directory name */
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
                                           compselect_treeview_set_cell_data,
                                           NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  
  /* add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);
  /* set directory/component treeview of compselect */
  compselect->treeview = GTK_TREE_VIEW (treeview);

  /* add the scrolled window for directories to the vertical box */
  gtk_box_pack_start (GTK_BOX (vbox), scrolled_win,
                      TRUE, TRUE, 0);

  
  /* -- filter area -- */
  filter_hbox = GTK_WIDGET (g_object_new (GTK_TYPE_HBOX,
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
  gtk_box_pack_start (GTK_BOX (filter_hbox), label,
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
  /* add the filter entry to the filter area */
  gtk_box_pack_start (GTK_BOX (filter_hbox), entry,
                      FALSE, FALSE, 0);
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
  gtk_box_pack_start (GTK_BOX (filter_hbox), button,
                      FALSE, FALSE, 0);
  /* set clear button of compselect */
  compselect->button_clear = GTK_BUTTON (button);
                                     
  /* add the filter area to the vertical box */
  gtk_box_pack_start (GTK_BOX (vbox), filter_hbox,
                      FALSE, FALSE, 0);

  
  /* include the vertical box in horizontal box */
  gtk_box_pack_start (GTK_BOX (hbox), vbox,
                      TRUE, TRUE, 0);

  /* now that that we have an entry, set the filter func of model */
  gtk_tree_model_filter_set_visible_func ((GtkTreeModelFilter*)model,
                                          compselect_model_filter_visible_func,
                                          compselect,
                                          NULL);

                     
  /* -- preview area -- */
  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                    /* GtkContainer */
                                    "border-width", 5,
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
  
  gtk_box_pack_start (GTK_BOX (hbox), frame,
                      FALSE, TRUE, 0);

  /* add the hbox to the dialog vbox */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (compselect)->vbox), hbox,
                      TRUE, TRUE, 0);
  gtk_widget_show_all (hbox);
  

  /* -- behavior combo box -- */
  combobox = compselect_create_behaviors_combo_box ();
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
                          GTK_STOCK_APPLY, GTK_RESPONSE_APPLY,
                          NULL);
  
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
      case PROP_FILENAME: {
        GtkTreeModel *model;
        GtkTreeIter iter, parent;
        if (gtk_tree_selection_get_selected (
              gtk_tree_view_get_selection (compselect->treeview),
              &model,
              &iter)) {
          GString *str;
          gchar *tmp;
          
          gtk_tree_model_get (model, &iter, 0, &tmp, -1);
          str = g_string_new (tmp);
          g_free (tmp);
          while (gtk_tree_model_iter_parent (model, &parent, &iter)) {
            gtk_tree_model_get (model, &parent, 0, &tmp, -1);
            str = g_string_prepend (str, G_DIR_SEPARATOR_S);
            str = g_string_prepend (str, tmp);
            g_free (tmp);
            iter = parent;
          }
          g_value_take_string (value, g_string_free (str, FALSE));
        } else {
          g_value_set_string (value, NULL);
        }
      }
        break;
      case PROP_BEHAVIOR:
        g_value_set_enum (value,
                          gtk_combo_box_get_active (
                            compselect->combobox_behaviors));
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
