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

#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include <gdk/gdkkeysyms.h>
#include "../include/gschem_dialog.h"
#include "../include/x_multiattrib.h"

/*! \brief Process the response returned by the multi-attribte dialog.
 *  \par Function Description
 *  This function handles the response <B>arg1</B> of the multi-attribute
 *  editor dialog <B>dialog</B>.
 *
 *  \param [in] dialog    The multi-attribute editor dialog.
 *  \param [in] arg1      The response ID.
 *  \param [in] user_data A pointer on the toplevel environment.
 */
static void
multiattrib_callback_response (GtkDialog *dialog,
                               gint arg1,
                               gpointer user_data)
{
  TOPLEVEL *toplevel = (TOPLEVEL*)user_data;

  switch (arg1) {
      case GTK_RESPONSE_CLOSE:
      case GTK_RESPONSE_DELETE_EVENT:
        gtk_widget_destroy (GTK_WIDGET (dialog));
        toplevel->mawindow = NULL;
        break;
  }
}

/*! \brief Open multiple attribute editor dialog.
 *  \par Function Description
 *  Opens the multiple attribute editor dialog for objects in this <B>toplevel</B>.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 */
void x_multiattrib_open (TOPLEVEL *toplevel)
{
  if ( toplevel->mawindow == NULL ) {
    toplevel->mawindow = GTK_WIDGET (g_object_new (TYPE_MULTIATTRIB,
                                     "selection", toplevel->page_current->selection_list,
                                      /* GschemDialog */
                                      "settings-name", "multiattrib",
                                      "toplevel", toplevel,
                                      NULL));

      g_signal_connect (toplevel->mawindow,
                        "response",
                        G_CALLBACK (multiattrib_callback_response),
                        toplevel);

    gtk_window_set_transient_for (GTK_WINDOW(toplevel->mawindow),
                                  GTK_WINDOW(toplevel->main_window));

    gtk_widget_show (toplevel->mawindow);
  } else {
    gtk_window_present (GTK_WINDOW(toplevel->mawindow));
  }
}


/*! \brief Close the multiattrib dialog.
 *
 *  \par Function Description
 *
 *  Closes the multiattrib dialog associated with <B>toplevel</B>.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 */
void x_multiattrib_close (TOPLEVEL *toplevel)
{
  if (toplevel->mawindow != NULL) {
    gtk_widget_destroy (toplevel->mawindow);
    toplevel->mawindow = NULL;
  }
}


/*! \brief Update the multiattrib editor dialog for a TOPLEVEL.
 *
 *  \par Function Description
 *
 *  If the TOPLEVEL has an open multiattrib dialog, switch to
 *  watching the current page's SELECTION object for changes.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 */
void x_multiattrib_update( TOPLEVEL *toplevel )
{
  if (toplevel->mawindow != NULL) {
    g_object_set (G_OBJECT (toplevel->mawindow), "selection",
                  toplevel->page_current->selection_list, NULL);
  }
}


/*! \section celltextview-widget Cell TextView Widget Code.
 * This widget makes a 'GtkTextView' widget implements the 'GtkCellEditable'
 * interface. It can then be used to renderer multi-line texts inside
 * tree views ('GtkTreeView').
 */
static void celltextview_class_init (CellTextViewClass *klass);
static void celltextview_init       (CellTextView *self);
static void celltextview_cell_editable_init (GtkCellEditableIface *iface);

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean celltextview_key_press_event (GtkWidget   *widget,
                                              GdkEventKey *key_event,
                                              gpointer     data)
{
  CellTextView *celltextview = (CellTextView*)widget;

  /* If the Escape key is pressed, we flag the edit as canceled */
  if (key_event->keyval == GDK_Escape)
      celltextview->editing_canceled = TRUE;

  /* ends editing of cell if one of these keys are pressed or editing is canceled */
  if (celltextview->editing_canceled == TRUE ||
      /* the Enter key without the Control modifier */
      (!(key_event->state & GDK_CONTROL_MASK) &&
       (key_event->keyval == GDK_Return ||
        key_event->keyval == GDK_KP_Enter))) {
    gtk_cell_editable_editing_done  (GTK_CELL_EDITABLE (celltextview));
    gtk_cell_editable_remove_widget (GTK_CELL_EDITABLE (celltextview));
    return TRUE;
  }

  return FALSE;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void celltextview_start_editing (GtkCellEditable *cell_editable,
					GdkEvent        *event)
{
  g_signal_connect (cell_editable,
                    "key_press_event",
                    G_CALLBACK (celltextview_key_press_event),
                    NULL);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
GType celltextview_get_type()
{
  static GType celltextview_type = 0;
  
  if (!celltextview_type) {
    static const GTypeInfo celltextview_info = {
      sizeof(CellTextViewClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) celltextview_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(CellTextView),
      0,    /* n_preallocs */
      (GInstanceInitFunc) celltextview_init,
    };

    static const GInterfaceInfo cell_editable_info = {
      (GInterfaceInitFunc) celltextview_cell_editable_init,
      NULL, /* interface_finalize */
      NULL  /* interface_data */
    };
		
    celltextview_type = g_type_register_static(GTK_TYPE_TEXT_VIEW,
					       "CellTextView",
					       &celltextview_info, 0);
    g_type_add_interface_static(celltextview_type,
				GTK_TYPE_CELL_EDITABLE,
				&cell_editable_info);
  }
  
  return celltextview_type;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void celltextview_class_init(CellTextViewClass *klass)
{
/*   GObjectClass *gobject_class = G_OBJECT_CLASS (klass); */
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void celltextview_init(CellTextView *celltextview)
{
  celltextview->editing_canceled = FALSE;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void celltextview_cell_editable_init(GtkCellEditableIface *iface)
{
  iface->start_editing = celltextview_start_editing;
}

/*! \section multi-line-text-cell-renderer Multi-line Text Cell Renderer
 * GTK has no multi-line text cell renderer. This code adds one to be used
 * in gschem code. It is inspired by the 'GtkCellRendererCombo' renderer
 * of GTK 2.4 (LGPL).
 */
static void cellrenderermultilinetext_class_init(CellRendererMultiLineTextClass *klass);
static void cellrenderermultilinetext_init (CellRendererMultiLineText *self);

static void cellrenderermultilinetext_editing_done (GtkCellEditable *cell_editable,
                                                    gpointer         user_data);
static gboolean cellrenderermultilinetext_focus_out_event (GtkWidget *widget,
                                                           GdkEvent  *event,
                                                           gpointer   user_data);


#define CELL_RENDERER_MULTI_LINE_TEXT_PATH "cell-renderer-multi-line-text-path"


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static GtkCellEditable* cellrenderermultilinetext_start_editing(GtkCellRenderer      *cell,
								GdkEvent             *event,
								GtkWidget            *widget,
								const gchar          *path,
								GdkRectangle         *background_area,
								GdkRectangle         *cell_area,
								GtkCellRendererState  flags)
{
  GtkCellRendererText *cell_text;
  CellRendererMultiLineText *cell_mlt;
  GtkWidget *textview;
  GtkTextBuffer *textbuffer;
  
  cell_text = GTK_CELL_RENDERER_TEXT (cell);
  if (cell_text->editable == FALSE) {
    return NULL;
  }

  cell_mlt  = CELL_RENDERER_MULTI_LINE_TEXT (cell);

  textbuffer = GTK_TEXT_BUFFER (g_object_new (GTK_TYPE_TEXT_BUFFER,
                                              NULL));
  gtk_text_buffer_set_text (textbuffer,
                            cell_text->text,
                            strlen (cell_text->text));

  textview = GTK_WIDGET (g_object_new (TYPE_CELL_TEXT_VIEW,
                                       /* GtkTextView */
                                       "buffer",   textbuffer,
                                       "editable", TRUE,
                                       /* GtkWidget */
                                       "height-request", cell_area->height,
                                       NULL));
  g_object_set_data_full (G_OBJECT (textview),
                          CELL_RENDERER_MULTI_LINE_TEXT_PATH,
                          g_strdup (path), g_free);

  gtk_widget_show (textview);

  g_signal_connect (GTK_CELL_EDITABLE (textview),
                    "editing_done",
                    G_CALLBACK (cellrenderermultilinetext_editing_done),
                    cell_mlt);
  cell_mlt->focus_out_id =
  g_signal_connect (textview,
                    "focus_out_event",
                    G_CALLBACK (cellrenderermultilinetext_focus_out_event),
                    cell_mlt);

  return GTK_CELL_EDITABLE (textview);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void cellrenderermultilinetext_editing_done(GtkCellEditable *cell_editable,
						   gpointer         user_data)
{
  CellRendererMultiLineText *cell = CELL_RENDERER_MULTI_LINE_TEXT (user_data);
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  gchar *new_text;
  const gchar *path;

  if (cell->focus_out_id > 0) {
    g_signal_handler_disconnect (cell_editable,
                                 cell->focus_out_id);
    cell->focus_out_id = 0;
  }

  gtk_cell_renderer_stop_editing (GTK_CELL_RENDERER (cell),
                                  CELL_TEXT_VIEW (cell_editable)->editing_canceled);
  if (CELL_TEXT_VIEW (cell_editable)->editing_canceled)
    return;

  buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (cell_editable));
  gtk_text_buffer_get_start_iter (buffer, &start);
  gtk_text_buffer_get_end_iter   (buffer, &end);
  new_text = gtk_text_buffer_get_text (buffer, &start, &end, TRUE);

  path = g_object_get_data (G_OBJECT (cell_editable),
                            CELL_RENDERER_MULTI_LINE_TEXT_PATH);
  g_signal_emit_by_name (cell, "edited", path, new_text);

  g_free (new_text);

}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean cellrenderermultilinetext_focus_out_event(GtkWidget *widget,
							  GdkEvent *event,
							  gpointer user_data)
{
  cellrenderermultilinetext_editing_done (GTK_CELL_EDITABLE (widget),
                                          user_data);

  return FALSE;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
GType cellrenderermultilinetext_get_type()
{
  static GType cellrenderermultilinetext_type = 0;
  
  if (!cellrenderermultilinetext_type) {
    static const GTypeInfo cellrenderermultilinetext_info = {
      sizeof(CellRendererMultiLineTextClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) cellrenderermultilinetext_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(CellRendererMultiLineText),
      0,    /* n_preallocs */
      (GInstanceInitFunc) cellrenderermultilinetext_init,
    };
		
    cellrenderermultilinetext_type = g_type_register_static (
      GTK_TYPE_CELL_RENDERER_TEXT,
      "CellRendererMultiLineText",
      &cellrenderermultilinetext_info, 0);
  }
  
  return cellrenderermultilinetext_type;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void cellrenderermultilinetext_class_init(CellRendererMultiLineTextClass *klass)
{
/*   GObjectClass *gobject_class = G_OBJECT_CLASS (klass); */
  GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS (klass);

  cell_class->start_editing = cellrenderermultilinetext_start_editing;
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void cellrenderermultilinetext_init(CellRendererMultiLineText *self)
{
}


enum {
  PROP_SELECTION = 1
};

enum {
  COLUMN_ATTRIBUTE,
  NUM_COLUMNS
};

static GObjectClass *multiattrib_parent_class = NULL;

static void multiattrib_class_init (MultiattribClass *class);
static void multiattrib_init       (Multiattrib *multiattrib);
static void multiattrib_set_property (GObject *object,
                                      guint property_id,
                                      const GValue *value,
                                      GParamSpec *pspec);
static void multiattrib_get_property (GObject *object,
                                      guint property_id,
                                      GValue *value,
                                      GParamSpec *pspec);

static void multiattrib_popup_menu (Multiattrib *multiattrib,
                                    GdkEventButton *event);


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_action_add_attribute(TOPLEVEL *toplevel,
					     OBJECT *object,
					     const gchar *name,
					     const gchar *value,
					     gint visible,
					     gint show_name_value) 
{
  OBJECT *o_attrib;
  gchar *newtext;
  
  newtext = g_strdup_printf ("%s=%s", name, value);

  /* create a new attribute and link it */
  o_attrib = o_attrib_add_attrib (toplevel, newtext,
                                  visible, show_name_value, object);

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate (toplevel, UNDO_ALL);

  g_free (newtext);

}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_action_duplicate_attribute(TOPLEVEL *toplevel,
						   OBJECT *object,
						   OBJECT *o_attrib) 
{
  OBJECT *o_new;
  
  o_new = o_attrib_add_attrib (toplevel,
                               o_attrib->text->string,
                               o_attrib->visibility,
                               o_attrib->show_name_value,
                               object);
  toplevel->page_current->CHANGED = 1;
  o_undo_savestate (toplevel, UNDO_ALL);

}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_action_delete_attribute(TOPLEVEL *toplevel,
						OBJECT *o_attrib) 
{
  /* actually deletes the attribute */
  o_selection_remove ( toplevel->page_current->selection_list, o_attrib);
  o_delete_text (toplevel, o_attrib);
  toplevel->page_current->CHANGED=1;
  o_undo_savestate (toplevel, UNDO_ALL);

}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_column_set_data_name(GtkTreeViewColumn *tree_column,
					     GtkCellRenderer *cell,
					     GtkTreeModel *tree_model,
					     GtkTreeIter *iter,
					     gpointer data)
{
  OBJECT *o_attrib;
  gchar *name, *value;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  
  o_attrib_get_name_value (o_attrib->text->string, &name, &value);
  g_object_set (cell,
                "text", name,
                NULL);
  g_free (name);
  g_free (value);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_column_set_data_value(GtkTreeViewColumn *tree_column,
					      GtkCellRenderer *cell,
					      GtkTreeModel *tree_model,
					      GtkTreeIter *iter,
					      gpointer data)           
{
  OBJECT *o_attrib;
  gchar *name, *value;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  
  o_attrib_get_name_value (o_attrib->text->string, &name, &value);
  g_object_set (cell,
                "text", value,
                NULL);
  g_free (name);
  g_free (value);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_column_set_data_visible(GtkTreeViewColumn *tree_column,
						GtkCellRenderer *cell,
						GtkTreeModel *tree_model,
						GtkTreeIter *iter,
						gpointer data)
{
  OBJECT *o_attrib;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  
  g_object_set (cell,
                "active", (o_attrib->visibility == VISIBLE),
                NULL);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_column_set_data_show_name(GtkTreeViewColumn *tree_column,
						  GtkCellRenderer *cell,
						  GtkTreeModel *tree_model,
						  GtkTreeIter *iter,
						  gpointer data)
{
  OBJECT *o_attrib;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  
  g_object_set (cell,
                "active", (o_attrib->show_name_value == SHOW_NAME_VALUE ||
                           o_attrib->show_name_value == SHOW_NAME),
                NULL);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_column_set_data_show_value(GtkTreeViewColumn *tree_column,
						   GtkCellRenderer *cell,
						   GtkTreeModel *tree_model,
						   GtkTreeIter *iter,
						   gpointer data)
{
  OBJECT *o_attrib;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  
  g_object_set (cell,
                "active", (o_attrib->show_name_value == SHOW_NAME_VALUE ||
                           o_attrib->show_name_value == SHOW_VALUE),
                NULL);
  
}

/*! \brief Requests an update of the display of a row.
 *  \par Function Description
 *  This is an helper function to update the display of a row when
 *  data for this row have been modified in the model.
 *
 *  It emits the 'row_changed' signal on the pointed row.
 *
 *  \param [in] model A GtkTreeModel.
 *  \param [in] iter  A valid GtkTreeIter pointing to the changed row.
 */
static void
update_row_display (GtkTreeModel *model, GtkTreeIter *iter)
{
  GtkTreePath *path;
  
  path = gtk_tree_model_get_path (model, iter);
  gtk_tree_model_row_changed (model, path, iter);
  gtk_tree_path_free (path);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_edited_name(GtkCellRendererText *cellrenderertext,
					     gchar *arg1,
					     gchar *arg2,
					     gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  OBJECT *o_attrib;
  TOPLEVEL *toplevel;
  gchar *name, *value, *newtext;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, arg1)) {
    return;
  }

  if (g_ascii_strcasecmp (arg2, "") == 0) {
    GtkWidget *dialog = gtk_message_dialog_new (
      GTK_WINDOW (multiattrib),
      GTK_DIALOG_MODAL,
      GTK_MESSAGE_ERROR,
      GTK_BUTTONS_OK,
      _("Attributes with empty name are not allowed. Please set a name."));

    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
    return;
  }
  
  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);

  o_attrib_get_name_value (o_attrib->text->string, &name, &value);
  newtext = g_strdup_printf ("%s=%s", arg2, value);
  
  /* actually modifies the attribute */
  o_text_change (toplevel, o_attrib,
                 newtext, o_attrib->visibility, o_attrib->show_name_value);

  g_free (name);
  g_free (value);
  g_free (newtext);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_edited_value(GtkCellRendererText *cell_renderer,
					      gchar *arg1,
					      gchar *arg2,
					      gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  OBJECT *o_attrib;
  TOPLEVEL *toplevel;
  gchar *name, *value, *newtext;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, arg1)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);

  o_attrib_get_name_value (o_attrib->text->string, &name, &value);
  newtext = g_strdup_printf ("%s=%s", name, arg2);
  
  /* actually modifies the attribute */
  o_text_change (toplevel, o_attrib,
                 newtext, o_attrib->visibility, o_attrib->show_name_value);
  
  /* request an update of display for this row */
  update_row_display (model, &iter);
  
  g_free (name);
  g_free (value);
  g_free (newtext);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_toggled_visible(GtkCellRendererToggle *cell_renderer,
						 gchar *path,
						 gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  OBJECT *o_attrib;
  TOPLEVEL *toplevel;
  gint visibility;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  o_text_erase (toplevel, o_attrib);

  visibility = o_attrib->visibility == VISIBLE ? INVISIBLE : VISIBLE;

  /* actually modifies the attribute */
  o_attrib->visibility = visibility;
  o_text_recreate (toplevel, o_attrib);
  o_text_draw (toplevel, o_attrib);
  o_undo_savestate (toplevel, UNDO_ALL);

  /* request an update of display for this row */
  update_row_display (model, &iter);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_toggled_show_name(GtkCellRendererToggle *cell_renderer,
						   gchar *path,
						   gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  OBJECT *o_attrib;
  TOPLEVEL *toplevel;
  gint new_snv;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  o_text_erase (toplevel, o_attrib);

  switch (o_attrib->show_name_value) {
      case SHOW_NAME_VALUE: new_snv = SHOW_VALUE;      break;
      case SHOW_NAME:       new_snv = SHOW_VALUE;      break;
      case SHOW_VALUE:      new_snv = SHOW_NAME_VALUE; break;
      default:
        g_assert_not_reached ();
        new_snv = SHOW_NAME_VALUE;
  }

  /* actually modifies the attribute */
  o_attrib->show_name_value = new_snv;
  o_text_recreate (toplevel, o_attrib);
  o_text_draw (toplevel, o_attrib);
  o_undo_savestate (toplevel, UNDO_ALL);

  /* request an update of display for this row */
  update_row_display (model, &iter);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_toggled_show_value(GtkCellRendererToggle *cell_renderer,
						    gchar *path,
						    gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  OBJECT *o_attrib;
  TOPLEVEL *toplevel;
  gint new_snv;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);
  o_text_erase (toplevel, o_attrib);

  switch (o_attrib->show_name_value) {
      case SHOW_NAME_VALUE: new_snv = SHOW_NAME;       break;
      case SHOW_NAME:       new_snv = SHOW_NAME_VALUE; break;
      case SHOW_VALUE:      new_snv = SHOW_NAME;       break;
      default:
        g_assert_not_reached ();
        new_snv = SHOW_NAME_VALUE;
  }

  /* actually modifies the attribute */
  o_attrib->show_name_value = new_snv;
  o_text_recreate (toplevel, o_attrib);
  o_text_draw (toplevel, o_attrib);
  o_undo_savestate (toplevel, UNDO_ALL);
  
  /* request an update of display for this row */
  update_row_display (model, &iter);

}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean multiattrib_callback_key_pressed(GtkWidget *widget,
						 GdkEventKey *event,
						 gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;

  if (event->state == 0 &&
      (event->keyval == GDK_Delete || event->keyval == GDK_KP_Delete)) {
    GtkTreeModel *model;
    GtkTreeIter iter;
    OBJECT *o_attrib;
    /* delete the currently selected attribute */

    if (!gtk_tree_selection_get_selected (
          gtk_tree_view_get_selection (multiattrib->treeview),
          &model, &iter)) {
      /* nothing selected, nothing to do */
      return FALSE;
    }
    
    gtk_tree_model_get (model, &iter,
                        COLUMN_ATTRIBUTE, &o_attrib,
                        -1);
    g_assert (o_attrib->type == OBJ_TEXT);
    
    multiattrib_action_delete_attribute (GSCHEM_DIALOG (multiattrib)->toplevel,
                                         o_attrib);
    
    /* update the treeview contents */
    multiattrib_update (multiattrib);
  }

  return FALSE;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean multiattrib_callback_button_pressed(GtkWidget *widget,
						    GdkEventButton *event,
						    gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  gboolean ret = FALSE;

  if (event->type == GDK_BUTTON_PRESS  &&  event->button == 3) {
    multiattrib_popup_menu (multiattrib, event);
    ret = TRUE;
  }

  return ret;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean multiattrib_callback_popup_menu(GtkWidget *widget,
						gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;

  multiattrib_popup_menu (multiattrib, NULL);
  
  return TRUE;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_popup_duplicate(GtkMenuItem *menuitem,
						 gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  TOPLEVEL *toplevel;
  OBJECT *object, *o_attrib;
  
  if (!gtk_tree_selection_get_selected (
        gtk_tree_view_get_selection (multiattrib->treeview),
        &model, &iter)) {
    /* nothing selected, nothing to do */
    return;
  }

  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;
  object   = multiattrib->object;
  
  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);

  multiattrib_action_duplicate_attribute (toplevel, object, o_attrib);

  /* update the treeview contents */
  multiattrib_update (multiattrib);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_popup_delete(GtkMenuItem *menuitem,
					      gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  TOPLEVEL *toplevel;
  OBJECT *o_attrib;
  
  if (!gtk_tree_selection_get_selected (
        gtk_tree_view_get_selection (multiattrib->treeview),
        &model, &iter)) {
    /* nothing selected, nothing to do */
    return;
  }

  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE, &o_attrib,
                      -1);
  g_assert (o_attrib->type == OBJ_TEXT);

  multiattrib_action_delete_attribute (toplevel, o_attrib);

  /* update the treeview contents */
  multiattrib_update (multiattrib);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean multiattrib_callback_value_key_pressed(GtkWidget *widget,
						       GdkEventKey *event,
						       gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)widget;
  gboolean retval = FALSE;

  /* ends editing of cell if one of these keys are pressed: */
  /*  - the Return key without the Control modifier */
  /*  - the Tab key without the Control modifier */
  if ((event->keyval == GDK_Return || event->keyval == GDK_KP_Enter) ||
      (event->keyval == GDK_Tab    || event->keyval == GDK_KP_Tab)) {
    /* Control modifier activated? */
    if (event->state & GDK_CONTROL_MASK) {
      /* yes the modifier in event structure and let event propagate */
      event->state ^= GDK_CONTROL_MASK;
      retval = FALSE;
    } else {
      /* change focus and stop propagation */
      g_signal_emit_by_name (multiattrib,
                             "move_focus",
                             (event->state & GDK_SHIFT_MASK) ?
                             GTK_DIR_TAB_BACKWARD : GTK_DIR_TAB_FORWARD);
      retval = TRUE;
    }
  }

  return retval;
}


/*! \brief GtkWidget "grab-focus" signal handler
 *
 *  \par Function Description
 *  Select the text in the GtkTextView so it may be over-typed quickly
 */
static void multiattrib_callback_value_grab_focus (GtkWidget *widget,
                                                   gpointer user_data)
{
  GtkTextView *textview = GTK_TEXT_VIEW (widget);
  GtkTextBuffer *textbuffer;
  GtkTextIter startiter, enditer;

  textbuffer = gtk_text_view_get_buffer (textview);
  gtk_text_buffer_get_iter_at_offset (textbuffer, &startiter, 0);
  gtk_text_buffer_get_iter_at_offset (textbuffer, &enditer, -1);
  gtk_text_buffer_select_range (textbuffer, &enditer, &startiter);
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_callback_button_add(GtkButton *button,
					    gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  const gchar *name;
  gchar *value;
  TOPLEVEL *toplevel;
  OBJECT *object;
  gboolean visible;
  gint shownv;

  toplevel = GSCHEM_DIALOG (multiattrib)->toplevel;
  object   = multiattrib->object;
  buffer   = gtk_text_view_get_buffer (multiattrib->textview_value);
  
  /* retrieve information from the Add/Edit frame */
  /*   - attribute's name */
  name = gtk_entry_get_text (
    GTK_ENTRY (GTK_COMBO (multiattrib->combo_name)->entry));
  /*   - attribute's value */
  gtk_text_buffer_get_bounds (buffer, &start, &end);
  value = gtk_text_buffer_get_text (buffer, &start, &end, FALSE);
  /*   - attribute's visibility status */
  visible = gtk_toggle_button_get_active (
    (GtkToggleButton*)multiattrib->button_visible);
  /*   - visibility type */
  shownv = (gint)gtk_option_menu_get_history (multiattrib->optionmenu_shownv);

  if (name[0] == '\0' || name[0] == ' ') {
    /* name not allowed for an attribute */
    g_free (value);
    return;
  }

  multiattrib_action_add_attribute (toplevel, object,
                                    name, value,
                                    visible, shownv);
  g_free (value);

  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_init_attrib_names(GtkCombo *combo)
{
  GList *items = NULL;
  const gchar *string;
  gint i;
  
  for (i = 0, string = s_attrib_get (i);
       string != NULL;
       i++, string = s_attrib_get (i)) {
    items = g_list_append (items, (gpointer)string);
  }

  gtk_combo_set_popdown_strings (GTK_COMBO (combo), items);

  g_list_free (items);
  
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void multiattrib_init_visible_types(GtkOptionMenu *optionmenu)
{
  GtkWidget *menu, *item;

  menu = gtk_menu_new ();
  item = gtk_menu_item_new_with_label (_("Show Name & Value"));
  gtk_menu_append (menu, item);
  item = gtk_menu_item_new_with_label (_("Show Value only"));
  gtk_menu_append (menu, item);
  item = gtk_menu_item_new_with_label (_("Show Name only"));
  gtk_menu_append (menu, item);

  gtk_option_menu_set_menu (optionmenu, menu);
  
}


/*! \brief Popup a context-sensitive menu.
 *  \par Function Description
 *  Pops up a context-sensitive menu.
 *  <B>event</B> can be NULL if the popup is triggered by a key binding
 *  instead of a mouse click.
 *
 *  \param [in] multiattrib  The Multiattrib object.
 *  \param [in] event        Mouse event.
 */
static void multiattrib_popup_menu(Multiattrib *multiattrib,
				   GdkEventButton *event)
{
  GtkTreePath *path;
  GtkWidget *menu;
  struct menuitem_t {
    gchar *label;
    GCallback callback;
  };
  struct menuitem_t menuitems[] = {
    { N_("Duplicate"), G_CALLBACK (multiattrib_callback_popup_duplicate) },
    { N_("Delete"),    G_CALLBACK (multiattrib_callback_popup_delete)    },
    { NULL,            NULL                                              } };
  struct menuitem_t *tmp;
  
  if (event != NULL &&
      gtk_tree_view_get_path_at_pos (multiattrib->treeview,
                                     (gint)event->x, 
                                     (gint)event->y,
                                     &path, NULL, NULL, NULL)) {
    GtkTreeSelection *selection;
    selection = gtk_tree_view_get_selection (multiattrib->treeview);
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
                        multiattrib);
    }
    gtk_menu_shell_append (GTK_MENU_SHELL (menu), menuitem);
  }
  gtk_widget_show_all (menu);
  /* make menu a popup menu */
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL,
                  (event != NULL) ? event->button : 0,
                  gdk_event_get_time ((GdkEvent*)event));
  
}


/*! \brief Function to retrieve Multiattrib's GType identifier.
 *
 *  \par Function Description
 *
 *  Function to retrieve Multiattrib's GType identifier.
 *  Upon first call, this registers Multiattrib in the GType system.
 *  Subsequently it returns the saved value from its first execution.
 *
 *  \return the GType identifier associated with Multiattrib.
 */
GType multiattrib_get_type()
{
  static GType multiattrib_type = 0;
  
  if (!multiattrib_type) {
    static const GTypeInfo multiattrib_info = {
      sizeof(MultiattribClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) multiattrib_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof(Multiattrib),
      0,    /* n_preallocs */
      (GInstanceInitFunc) multiattrib_init,
    };
		
    multiattrib_type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                               "Multiattrib",
                                               &multiattrib_info, 0);
  }
  
  return multiattrib_type;
}


/*! \brief Update the multiattrib editor dialog when the page's selection changes.
 *
 *  \par Function Description
 *
 *  When the page's selection changes this function identifies how many objects
 *  which can have attributes are currently selected. If this number is 1, the
 *  dialog is set to edit its attributes.
 *
 *  \todo The dialog doesn't currently support editing multiple objects at once
 *
 *  \param [in] selection    The SELECTION object of page being edited.
 *  \param [in] multiattrib  The multi-attribute editor dialog.
 */
static void selection_changed_cb (SELECTION *selection, Multiattrib *multiattrib)
{
  int object_count = 0;
  GList *selection_glist;
  GList *iter;
  OBJECT *object;

  selection_glist = geda_list_get_glist (selection);

  for ( iter = selection_glist;
        iter != NULL;
        iter = g_list_next (iter) ) {
    object = (OBJECT *)iter->data;
    g_assert( object != NULL );

    if (object->type == OBJ_COMPLEX ||
        object->type == OBJ_PLACEHOLDER ||
        object->type == OBJ_NET ||
        object->type == OBJ_BUS ||
        object->type == OBJ_PIN) {
      object_count++;
    }
  }

  if (object_count == 0) {
    /* TODO: If the user selects a single attribute which is
     *       not floating, should we find its parent object and
     *       display the multi-attribute editor for that?
     *       Bonus marks for making it jump to the correct attrib.
     */
    multiattrib->object = NULL;
  } else if (object_count == 1) {
    multiattrib->object = (OBJECT *)selection_glist->data;
  } else {
    /* TODO: Something clever with multiple objects selected */
    multiattrib->object = NULL;
  }

  multiattrib_update (multiattrib);
}


/*! \brief Update the dialog when the current page's SELECTION object is destroyed
 *
 *  \par Function Description
 *
 *  This handler is called when the g_object_weak_ref() on the SELECTION object
 *  we're watching expires. We reset our multiattrib->selection pointer to NULL
 *  to avoid attempting to access the destroyed object. NB: Our signal handlers
 *  were automatically disconnected during the destruction process.
 *
 *  \param [in] data                  Pointer to the multi-attrib dialog
 *  \param [in] where_the_object_was  Pointer to where the object was just destroyed
 */
static void selection_weak_ref_cb (gpointer data, GObject *where_the_object_was)
{
  Multiattrib *multiattrib = (Multiattrib *)data;

  multiattrib->selection = NULL;
  multiattrib_update (multiattrib);
}


/*! \brief Connect signal handler and weak_ref on the SELECTION object
 *
 *  \par Function Description
 *
 *  Connect the "changed" signal and add a weak reference
 *  on the SELECTION object we are going to watch.
 *
 *  \param [in] multiattrib  The Multiattrib dialog.
 *  \param [in] selection    The SELECTION object to watch.
 */
static void connect_selection( Multiattrib *multiattrib, SELECTION *selection )
{
  multiattrib->selection = selection;
  if (multiattrib->selection != NULL) {
    g_object_weak_ref (G_OBJECT (multiattrib->selection),
                       selection_weak_ref_cb,
                       multiattrib);
    multiattrib->selection_changed_id =
      g_signal_connect (G_OBJECT (multiattrib->selection),
                        "changed",
                        G_CALLBACK (selection_changed_cb),
                        multiattrib);
    /* Synthesise a selection changed update to refresh the view */
    selection_changed_cb (multiattrib->selection, multiattrib);
  } else {
    /* Call an update to set the sensitivities */
    multiattrib_update (multiattrib);
  }
}


/*! \brief Disconnect signal handler and weak_ref on the SELECTION object
 *
 *  \par Function Description
 *
 *  If the dialog is watching a SELECTION object, disconnect the
 *  "changed" signal and remove our weak reference on the object.
 *
 *  \param [in] multiattrib  The Multiattrib dialog.
 */
static void disconnect_selection( Multiattrib *multiattrib )
{
  if (multiattrib->selection != NULL) {
    g_signal_handler_disconnect (multiattrib->selection,
                                 multiattrib->selection_changed_id);
    g_object_weak_unref(G_OBJECT( multiattrib->selection ),
                        selection_weak_ref_cb,
                        multiattrib );
  }
}


/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *
 *  Just before the Multiattrib GObject is finalized, disconnect from
 *  the SELECTION object being watched and then chain up to the parent
 *  class's finalize handler.
 *
 *  \param [in] object  The GObject being finalized.
 */
static void multiattrib_finalize (GObject *object)
{
  Multiattrib *multiattrib = MULTIATTRIB(object);

  disconnect_selection( multiattrib );
  G_OBJECT_CLASS (multiattrib_parent_class)->finalize (object);
}


/*! \brief GType class initialiser for Multiattrib
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in]  klass       The MultiattribClass we are initialising
 */
static void multiattrib_class_init(MultiattribClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  multiattrib_parent_class = g_type_class_peek_parent (klass);

  gobject_class->set_property = multiattrib_set_property;
  gobject_class->get_property = multiattrib_get_property;
  gobject_class->finalize     = multiattrib_finalize;

  g_object_class_install_property (
    gobject_class, PROP_SELECTION,
    g_param_spec_pointer ("selection",
                          "",
                          "",
                          G_PARAM_READWRITE));
}


/*! \brief GType instance initialiser for Multiattrib
 *
 *  \par Function Description
 *
 *  GType instance initialiser for Multiattrib. Create
 *  and setup the widgets which make up the dialog.
 *
 *  \param [in]  dialog       The Multiattrib we are initialising
 */
static void multiattrib_init(Multiattrib *multiattrib)
{
  GtkWidget *frame, *label, *scrolled_win, *treeview;
  GtkWidget *table, *textview, *combo, *optionm, *button;
  GtkTreeModel *store;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
  GtkTreeSelection *selection;
  GtkStyle *style;
  
  /* dialog initialization */
  g_object_set (G_OBJECT (multiattrib),
                /* GtkContainer */
                "border-width",    0,
                /* GtkWindow */
                "type",            GTK_WINDOW_TOPLEVEL,
                "title",           _("Edit Attributes"),
                "default-width",   320,
                "default-height",  350,
                "window-position", GTK_WIN_POS_MOUSE,
                "allow-grow",      TRUE,
                "allow-shrink",    FALSE,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

  multiattrib->object   = NULL;

  /* create the attribute list frame */
  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
				    /* GtkFrame */
				    "label", _("Attributes"),
				    NULL));
  multiattrib->frame_add = frame;
  /*   - create the model for the treeview */
  store = (GtkTreeModel*)gtk_list_store_new (NUM_COLUMNS,
					     G_TYPE_POINTER); /* attribute */
  /*   - create a scrolled window for the treeview */
  scrolled_win = GTK_WIDGET (
			     g_object_new (GTK_TYPE_SCROLLED_WINDOW,
					   /* GtkContainer */
					   "border-width",      3,
					   /* GtkScrolledWindow */
					   "hscrollbar-policy",
					   GTK_POLICY_AUTOMATIC,
					   "vscrollbar-policy",
					   GTK_POLICY_AUTOMATIC,
					   "shadow-type",
					   GTK_SHADOW_ETCHED_IN,
					   NULL));
  /*   - create the treeview */
  treeview = GTK_WIDGET (g_object_new (GTK_TYPE_TREE_VIEW,
				       /* GtkTreeView */
				       "model",      store,
				       "rules-hint", TRUE,
				       NULL));
  g_signal_connect (treeview,
		    "key-press-event",
		    G_CALLBACK (multiattrib_callback_key_pressed),
		    multiattrib);
  g_signal_connect (treeview,
		    "button-press-event",
		    G_CALLBACK (multiattrib_callback_button_pressed),
		    multiattrib);
  g_signal_connect (treeview,
		    "popup-menu",
		    G_CALLBACK (multiattrib_callback_popup_menu),
		    multiattrib);
  selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (treeview));
  gtk_tree_selection_set_mode (selection,
			       GTK_SELECTION_SINGLE);

  /*   - and now the columns of the treeview */
  /*       - column 1: attribute name */
  renderer = GTK_CELL_RENDERER (
				g_object_new (GTK_TYPE_CELL_RENDERER_TEXT,
					      /* GtkCellRendererText */
					      "editable",  TRUE,
					      /* unknown in GTK 2.4 */
					      /* "ellipsize",
					       * PANGO_ELLIPSIZE_END, */
					      NULL));
  g_signal_connect (renderer,
		    "edited",
		    G_CALLBACK (multiattrib_callback_edited_name),
		    multiattrib);
  column = GTK_TREE_VIEW_COLUMN (
				 g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
					       /* GtkTreeViewColumn */
					       "title", _("Name"),
					       "min-width", 100,
					       "resizable", TRUE,
					       NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
					   multiattrib_column_set_data_name,
					   NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*       - column 2: attribute value */
  renderer = GTK_CELL_RENDERER (
				g_object_new (TYPE_CELL_RENDERER_MULTI_LINE_TEXT,
					      /* GtkCellRendererText */
					      "editable",  TRUE,
					      /* unknown in GTK 2.4 */
					      /* "ellipsize",
						 PANGO_ELLIPSIZE_END, */
					      NULL));
  g_signal_connect (renderer,
		    "edited",
		    G_CALLBACK (multiattrib_callback_edited_value),
		    multiattrib);
  column = GTK_TREE_VIEW_COLUMN (
				 g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
					       /* GtkTreeViewColumn */
					       "title", _("Value"),
					       "min-width", 140,
					       "resizable", TRUE,
					       NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
					   multiattrib_column_set_data_value,
					   NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*       - column 3: visibility */
  renderer = GTK_CELL_RENDERER (
				g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
					      /* GtkCellRendererToggle */
					      "activatable", TRUE,
					      NULL));
  g_signal_connect (renderer,
		    "toggled",
		    G_CALLBACK (multiattrib_callback_toggled_visible),
		    multiattrib);
  column = GTK_TREE_VIEW_COLUMN (
				 g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
					       /* GtkTreeViewColumn */
					       "title", _("Vis?"),
					       NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
					   multiattrib_column_set_data_visible,
					   NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*       - column 4: show name */
  renderer = GTK_CELL_RENDERER (
				g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
					      /* GtkCellRendererToggle */
					      "activatable", TRUE,
					      NULL));
  g_signal_connect (renderer,
		    "toggled",
		    G_CALLBACK (multiattrib_callback_toggled_show_name),
		    multiattrib);
  column = GTK_TREE_VIEW_COLUMN (
				 g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
					       /* GtkTreeViewColumn */
					       "title", _("N"),
					       NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
					   multiattrib_column_set_data_show_name,
					   NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*       - column 5: show value */
  renderer = GTK_CELL_RENDERER (
				g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
					      /* GtkCellRendererToggle */
					      "activatable", TRUE,
					      NULL));
  g_signal_connect (renderer,
		    "toggled",
		    G_CALLBACK (multiattrib_callback_toggled_show_value),
		    multiattrib);
  column = GTK_TREE_VIEW_COLUMN (
				 g_object_new (GTK_TYPE_TREE_VIEW_COLUMN,
					       /* GtkTreeViewColumn */
					       "title", _("V"),
					       NULL));
  gtk_tree_view_column_pack_start (column, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (column, renderer,
					   multiattrib_column_set_data_show_value,
					   NULL, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  
  /* add the treeview to the scrolled window */
  gtk_container_add (GTK_CONTAINER (scrolled_win), treeview);
  /* set treeview of multiattrib */
  multiattrib->treeview = GTK_TREE_VIEW (treeview);
  /* add the scrolled window to frame */
  gtk_container_add (GTK_CONTAINER (frame), scrolled_win);
  /* pack the frame */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (multiattrib)->vbox), frame,
		      TRUE, TRUE, 1);
  gtk_widget_show_all (frame);
  
  /* create the add/edit frame */
  frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
				    "label", _("Add Attribute"),
				    NULL));
  multiattrib->frame_attributes = frame;
  table = GTK_WIDGET (g_object_new (GTK_TYPE_TABLE,
				    /* GtkTable */
				    "n-rows",      4,
				    "n-columns",   2,
				    "homogeneous", FALSE,
				    NULL));
  
  /*   - the name entry: a GtkComboBoxEntry */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
				    /* GtkMisc */
				    "xalign", 0.0,
				    "yalign", 0.5,
				    /* GtkLabel */
				    "label",  _("Name:"),
				    NULL));
  combo = GTK_WIDGET (g_object_new (GTK_TYPE_COMBO,
				    /* GtkCombo */
				    "value-in-list", FALSE,
				    NULL));
  multiattrib_init_attrib_names (GTK_COMBO (combo));
  multiattrib->combo_name = GTK_COMBO (combo);
  gtk_table_attach (GTK_TABLE (table), label,
		    0, 1, 0, 1, 0, 0, 0, 0);
  gtk_table_attach (GTK_TABLE (table), combo,
		    1, 2, 0, 1, GTK_EXPAND | GTK_FILL, 0, 6, 3);
  
  /*   - the value entry: a GtkEntry */
  label = GTK_WIDGET (g_object_new (GTK_TYPE_LABEL,
				    /* GtkMisc */
				    "xalign", 0.0,
				    "yalign", 0.5,
				    /* GtkLabel */
				    "label",  _("Value:"),
				    NULL));
  scrolled_win = GTK_WIDGET (
			     g_object_new (GTK_TYPE_SCROLLED_WINDOW,
					   /* GtkScrolledWindow */
					   "hscrollbar-policy",
					   GTK_POLICY_NEVER,
					   "vscrollbar-policy",
					   GTK_POLICY_AUTOMATIC,
					   "shadow-type",
					   GTK_SHADOW_IN,
					   NULL));
  textview = GTK_WIDGET (g_object_new (GTK_TYPE_TEXT_VIEW,
				       NULL));
  g_signal_connect (textview,
		    "key_press_event",
		    G_CALLBACK (multiattrib_callback_value_key_pressed),
		    multiattrib);
  g_signal_connect (textview,
                    "grab-focus",
                    G_CALLBACK (multiattrib_callback_value_grab_focus),
                    multiattrib);
  /* Save the GTK_STATE_NORMAL color so we can work around GtkTextView's
   * stubborn refusal to draw with GTK_STATE_INSENSITIVE later on */
  style = gtk_widget_get_style (textview);
  multiattrib->value_normal_text_color = style->text[ GTK_STATE_NORMAL ];

  gtk_container_add (GTK_CONTAINER (scrolled_win), textview);
  multiattrib->textview_value = GTK_TEXT_VIEW (textview);
  gtk_table_attach (GTK_TABLE (table), label,
		    0, 1, 1, 2, 0, 0, 0, 0);
  gtk_table_attach (GTK_TABLE (table), scrolled_win,
		    1, 2, 1, 2, GTK_EXPAND | GTK_FILL, 0, 6, 3);
  
  /*   - the visible status */
  button = GTK_WIDGET (g_object_new (GTK_TYPE_CHECK_BUTTON,
				     /* GtkButton */
				     "label", _("Visible"),
				     "active", TRUE,
				     NULL));
  multiattrib->button_visible = GTK_CHECK_BUTTON (button);
  gtk_table_attach (GTK_TABLE (table), button,
		    0, 1, 2, 3, GTK_FILL, 0, 3, 0);
  
  /*   - the visibility type */
  optionm = GTK_WIDGET (g_object_new (GTK_TYPE_OPTION_MENU,
				      NULL));
  multiattrib_init_visible_types (GTK_OPTION_MENU (optionm));
  multiattrib->optionmenu_shownv = GTK_OPTION_MENU (optionm);
  gtk_table_attach (GTK_TABLE (table), optionm,
		    1, 2, 2, 3, GTK_EXPAND | GTK_FILL, 0, 6, 3);
  gtk_widget_show_all (table);
  
  /* create the add button */
  button = gtk_button_new_from_stock (GTK_STOCK_ADD);
  g_signal_connect (button,
		    "clicked",
		    G_CALLBACK (multiattrib_callback_button_add),
		    multiattrib);
  gtk_table_attach (GTK_TABLE (table), button,
		    2, 3, 0, 3, 0, 0, 6, 3);
  
  /* add the table to the frame */
  gtk_container_add (GTK_CONTAINER (frame), table);
  /* pack the frame in the dialog */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (multiattrib)->vbox), frame,
		      FALSE, TRUE, 4);
  gtk_widget_show_all (frame);
  
  
  /* now add the close button to the action area */
  gtk_dialog_add_button (GTK_DIALOG (multiattrib),
                         GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE);
  
}


/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for Multiattrib's GObject property, "selection".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */

static void multiattrib_set_property (GObject *object,
                                      guint property_id,
                                      const GValue *value,
                                      GParamSpec *pspec)
{
  Multiattrib *multiattrib = MULTIATTRIB (object);

  switch(property_id) {
      case PROP_SELECTION:
        disconnect_selection (multiattrib);
        connect_selection (multiattrib, g_value_get_pointer (value));
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}


/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for Multiattrib's GObject property, "selection".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void multiattrib_get_property (GObject *object,
                                      guint property_id,
                                      GValue *value,
                                      GParamSpec *pspec)
{
  Multiattrib *multiattrib = MULTIATTRIB (object);

  switch(property_id) {
      case PROP_SELECTION:
        g_value_set_pointer (value, (gpointer)multiattrib->selection);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }

}


/*! \brief Update the multiattrib editor dialog's interface
 *
 *  \par Function Description
 *
 *  Update the dialog to reflect the attributes of the currently selected
 *  object. If no (or multiple) objects are selected, the dialog's controls
 *  are set insensitive.
 *
 *  \todo The dialog doesn't currently support editing multiple objects at once
 *
 *  \param [in] multiattrib  The multi-attribute editor dialog.
 */
void multiattrib_update (Multiattrib *multiattrib)
{
  GtkListStore *liststore;
  GtkTreeIter iter;
  OBJECT **object_attribs, *o_current;
  gint i;
  gboolean sensitive;
  GtkStyle *style;

  g_assert (GSCHEM_DIALOG (multiattrib)->toplevel != NULL);

  /* clear the list of attributes */
  liststore = (GtkListStore*)gtk_tree_view_get_model (multiattrib->treeview);
  gtk_list_store_clear (liststore);

  /* Update sensitivities */
  sensitive = (multiattrib->selection != NULL && multiattrib->object != NULL);
  gtk_widget_set_sensitive (GTK_WIDGET (multiattrib->frame_attributes), sensitive);
  gtk_widget_set_sensitive (GTK_WIDGET (multiattrib->frame_add), sensitive);

  /* Work around GtkTextView's stubborn indifference
   * to GTK_STATE_INSENSITIVE when rendering its text. */
  style = gtk_widget_get_style (GTK_WIDGET (multiattrib->textview_value));
  gtk_widget_modify_text (GTK_WIDGET (multiattrib->textview_value),
                          GTK_STATE_NORMAL,
                          sensitive ? &multiattrib->value_normal_text_color
                                    : &style->text[GTK_STATE_INSENSITIVE]);

  /* If we aren't sensitive, there is nothing more to do */
  if (!sensitive)
    return;

  /* get list of attributes */
  object_attribs = o_attrib_return_attribs (
    GSCHEM_DIALOG (multiattrib)->toplevel->page_current->object_head,
    multiattrib->object);
  /* populate the store with attributes */
  if (object_attribs) {
    for (i = 0, o_current = object_attribs[i];
         o_current != NULL;
         i++, o_current = object_attribs[i]) {
      gtk_list_store_append (liststore, &iter);
      gtk_list_store_set (liststore, &iter,
                          COLUMN_ATTRIBUTE, o_current,
                          -1);
    }
  }
  /* delete the list of attribute objects */
  o_attrib_free_returned (object_attribs);
}
