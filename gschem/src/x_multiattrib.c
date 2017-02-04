/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2013 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"
#include "x_multiattrib.h"
#include <gdk/gdkkeysyms.h>


static void multiattrib_update (Multiattrib *multiattrib);

static gboolean
snv_shows_name (int snv)
{
  return snv == SHOW_NAME_VALUE || snv == SHOW_NAME;
}

static gboolean
snv_shows_value (int snv)
{
  return snv == SHOW_NAME_VALUE || snv == SHOW_VALUE;
}

/*! \brief Process the response returned by the multi-attribte dialog.
 *  \par Function Description
 *  This function handles the response <B>arg1</B> of the multi-attribute
 *  editor dialog <B>dialog</B>.
 *
 *  \param [in] dialog    The multi-attribute editor dialog.
 *  \param [in] arg1      The response ID.
 *  \param [in] user_data A pointer on the GschemToplevel environment.
 */
static void
multiattrib_callback_response (GtkDialog *dialog,
                               gint arg1,
                               gpointer user_data)
{
  GschemToplevel *w_current = GSCHEM_TOPLEVEL (user_data);

  switch (arg1) {
      case GTK_RESPONSE_CLOSE:
      case GTK_RESPONSE_DELETE_EVENT:
        gtk_widget_destroy (GTK_WIDGET (dialog));
        w_current->mawindow = NULL;
        break;
  }
}

/*! \brief Open multiple attribute editor dialog.
 *  \par Function Description
 *  Opens the multiple attribute editor dialog for objects in this <B>toplevel</B>.
 *
 *  \param [in] w_current  The GschemToplevel object.
 */
void
x_multiattrib_open (GschemToplevel *w_current)
{
  if ( w_current->mawindow == NULL ) {
    w_current->mawindow =
      GTK_WIDGET (g_object_new (TYPE_MULTIATTRIB,
                                "object_list", w_current->toplevel->page_current->selection_list,
                                /* GschemDialog */
                                "settings-name", "multiattrib",
                                "gschem-toplevel", w_current,
                                NULL));

    gtk_window_set_transient_for (GTK_WINDOW(w_current->mawindow),
                                  GTK_WINDOW(w_current->main_window));

    g_signal_connect (w_current->mawindow,
                      "response",
                      G_CALLBACK (multiattrib_callback_response),
                      w_current);

    gtk_widget_show (w_current->mawindow);
  } else {
    gtk_window_present (GTK_WINDOW(w_current->mawindow));
  }
}


/*! \brief Close the multiattrib dialog.
 *
 *  \par Function Description
 *
 *  Closes the multiattrib dialog associated with <B>w_current</B>.
 *
 *  \param [in] w_current  The GschemToplevel object.
 */
void
x_multiattrib_close (GschemToplevel *w_current)
{
  if (w_current->mawindow != NULL) {
    gtk_widget_destroy (w_current->mawindow);
    w_current->mawindow = NULL;
  }
}

/*! \brief Update the multiattrib editor dialog for a GschemToplevel.
 *
 *  \par Function Description
 *
 *  If the GschemToplevel has an open multiattrib dialog, switch to
 *  watching the current page's SELECTION object for changes.
 *
 *  \param [in] w_current  The GschemToplevel object.
 */
void
x_multiattrib_update (GschemToplevel *w_current)
{
  if (w_current->mawindow != NULL) {
    g_object_set (G_OBJECT (w_current->mawindow), "object_list",
                  w_current->toplevel->page_current->selection_list, NULL);
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

enum {
    PROP_EDIT_CANCELED = 1
};

static void
celltextview_set_property (GObject *object,
                           guint property_id,
                           const GValue *value,
                           GParamSpec *pspec)
{
  CellTextView *celltextview = (CellTextView*) object;

  switch (property_id) {
      case PROP_EDIT_CANCELED:
        celltextview->editing_canceled = g_value_get_boolean (value);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
celltextview_get_property (GObject *object,
                           guint property_id,
                           GValue *value,
                           GParamSpec *pspec)
{
  CellTextView *celltextview = (CellTextView*) object;

  switch (property_id) {
      case PROP_EDIT_CANCELED:
        g_value_set_boolean (value, celltextview->editing_canceled);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean
celltextview_key_press_event (GtkWidget   *widget,
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
static void
celltextview_start_editing (GtkCellEditable *cell_editable,
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
GType
celltextview_get_type ()
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

    celltextview_type = g_type_register_static (GTK_TYPE_TEXT_VIEW,
                                                "CellTextView",
                                                &celltextview_info, 0);
    g_type_add_interface_static (celltextview_type,
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
static void
celltextview_class_init (CellTextViewClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);

  gobject_class->get_property = celltextview_get_property;
  gobject_class->set_property = celltextview_set_property;

  g_object_class_install_property (
    gobject_class,
    PROP_EDIT_CANCELED,
    g_param_spec_boolean ("editing-canceled",
                          "",
                          "",
                          FALSE,
                          G_PARAM_READWRITE));
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
celltextview_init (CellTextView *celltextview)
{
  celltextview->editing_canceled = FALSE;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
celltextview_cell_editable_init (GtkCellEditableIface *iface)
{
  iface->start_editing = celltextview_start_editing;
}

/*! \section multi-line-text-cell-renderer Multi-line Text Cell Renderer
 * GTK has no multi-line text cell renderer. This code adds one to be used
 * in gschem code. It is inspired by the 'GtkCellRendererCombo' renderer
 * of GTK 2.4 (LGPL).
 */
static void cellrenderermultilinetext_class_init (CellRendererMultiLineTextClass *klass);
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
static GtkCellEditable*
cellrenderermultilinetext_start_editing (GtkCellRenderer      *cell,
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
static void
cellrenderermultilinetext_editing_done (GtkCellEditable *cell_editable,
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

  if (CELL_TEXT_VIEW (cell_editable)->editing_canceled) {
    g_signal_emit_by_name (cell, "editing-canceled");
    return;
  }

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
static gboolean
cellrenderermultilinetext_focus_out_event (GtkWidget *widget,
                                           GdkEvent *event,
                                           gpointer user_data)
{
//  cellrenderermultilinetext_editing_done (GTK_CELL_EDITABLE (widget),
//                                          user_data);

  return FALSE;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
GType
cellrenderermultilinetext_get_type ()
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
      NULL, /* instance_init */
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
static void
cellrenderermultilinetext_class_init (CellRendererMultiLineTextClass *klass)
{
/*   GObjectClass *gobject_class = G_OBJECT_CLASS (klass); */
  GtkCellRendererClass *cell_class = GTK_CELL_RENDERER_CLASS (klass);

  cell_class->start_editing = cellrenderermultilinetext_start_editing;
}


enum {
  PROP_OBJECT_LIST = 1
};

enum {
  COLUMN_INHERITED,
  COLUMN_NAME,
  COLUMN_VALUE,
  COLUMN_VISIBILITY,
  COLUMN_SHOW_NAME_VALUE,
  COLUMN_PRESENT_IN_ALL,
  COLUMN_IDENTICAL_VALUE,
  COLUMN_IDENTICAL_VISIBILITY,
  COLUMN_IDENTICAL_SHOW_NAME,
  COLUMN_IDENTICAL_SHOW_VALUE,
  COLUMN_ATTRIBUTE_GEDALIST,
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


/*! \brief Returns TRUE/FALSE if the given object may have attributes attached.
 *
 *  \par Function Description
 *
 *  Returns TRUE/FALSE if the given object may have attributes attached.
 *
 *  \param [in] object  The OBJECT to test.
 *  \returns  TRUE/FALSE if the given object may have attributes attached.
 */
static gboolean is_multiattrib_object (OBJECT *object)
{
  if (object->type == OBJ_COMPLEX ||
      object->type == OBJ_PLACEHOLDER ||
      object->type == OBJ_NET ||
      object->type == OBJ_BUS ||
      object->type == OBJ_PIN) {
    return TRUE;
  }
  return FALSE;
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_action_add_attribute (Multiattrib *multiattrib,
                                  const gchar *name,
                                  const gchar *value,
                                  gint visible,
                                  gint show_name_value)
{
  OBJECT *object;
  gchar *newtext;
  GList *iter;
  GschemToplevel *w_current = GSCHEM_DIALOG (multiattrib)->w_current;

  newtext = g_strdup_printf ("%s=%s", name, value);

  if (!x_dialog_validate_attribute(GTK_WINDOW(multiattrib), newtext)) {
    g_free(newtext);
    return;
  }

  for (iter = geda_list_get_glist (multiattrib->object_list);
       iter != NULL;
       iter = g_list_next (iter)) {
    object = (OBJECT *)iter->data;

    if (is_multiattrib_object (object)) {

      /* create a new attribute and link it */
      o_attrib_add_attrib (w_current, newtext,
                           visible, show_name_value, object);
    }
  }

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);

  g_free (newtext);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_action_duplicate_attributes (Multiattrib *multiattrib,
                                         GList *attr_list)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (multiattrib)->w_current;
  GList *iter;

  for (iter = attr_list;
       iter != NULL;
       iter = g_list_next (iter)) {
    OBJECT *o_attrib = (OBJECT *)iter->data;

    /* create a new attribute and link it */
    o_attrib_add_attrib (w_current,
                         geda_text_object_get_string (o_attrib),
                         o_is_visible (w_current->toplevel, o_attrib),
                         o_attrib->show_name_value,
                         o_attrib->attached_to);
  }

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_action_promote_attributes (Multiattrib *multiattrib,
                                       GList *attr_list)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (multiattrib)->w_current;
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_new;
  GList *iter;

  for (iter = attr_list;
       iter != NULL;
       iter = g_list_next (iter)) {
    OBJECT *o_attrib = (OBJECT *)iter->data;

    if (o_is_visible (toplevel, o_attrib)) {
      /* If the attribute we're promoting is visible, don't clone its location */
      o_attrib_add_attrib (w_current,
                           geda_text_object_get_string (o_attrib),
                           VISIBLE,
                           o_attrib->show_name_value,
                           o_attrib->parent);
    } else {
        /* make a copy of the attribute object */
        o_new = o_object_copy (toplevel, o_attrib);
        s_page_append (toplevel, toplevel->page_current, o_new);
        /* add the attribute its parent */
        o_attrib_attach (toplevel, o_new, o_attrib->parent, TRUE);
        /* note: this object is unselected (not added to selection). */

        /* Call add-objects-hook */
        g_run_hook_object (w_current, "%add-objects-hook", o_new);
    }
  }

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_action_delete_attributes (Multiattrib *multiattrib,
                                      GList *attr_list)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (multiattrib)->w_current;
  GList *a_iter;
  OBJECT *o_attrib;

  for (a_iter = attr_list; a_iter != NULL; a_iter = g_list_next (a_iter)) {
    o_attrib = a_iter->data;
    /* actually deletes the attribute */
    o_delete (w_current, o_attrib);
  }

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_action_copy_attribute_to_all (Multiattrib *multiattrib,
                                          GList *attr_list)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (multiattrib)->w_current;
  GList *iter;
  GList *objects_needing_add;

  objects_needing_add = g_list_copy (geda_list_get_glist (multiattrib->object_list));

  /* Remove objects which already have this attribute from the list */
  for (iter = attr_list;
       iter != NULL;
       iter = g_list_next (iter)) {
    OBJECT *o_attrib = (OBJECT *)iter->data;

    objects_needing_add = g_list_remove (objects_needing_add, o_attrib->attached_to);
  }

  for (iter = objects_needing_add; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *object = iter->data;

    if (is_multiattrib_object (object)) {

      /* Pick the first instance to copy from */
      OBJECT *attrib_to_copy = attr_list->data;

      int visibility = o_is_visible (w_current->toplevel, attrib_to_copy)
          ? VISIBLE : INVISIBLE;

      /* create a new attribute and link it */
      o_attrib_add_attrib (w_current,
                           geda_text_object_get_string (attrib_to_copy),
                           visibility,
                           attrib_to_copy->show_name_value,
                           object);
    }
  }

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_column_set_data_name (GtkTreeViewColumn *tree_column,
                                  GtkCellRenderer *cell,
                                  GtkTreeModel *tree_model,
                                  GtkTreeIter *iter,
                                  gpointer data)
{
  Multiattrib *dialog = (Multiattrib *) data;
  gchar *name;
  gboolean present_in_all;
  int inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_NAME, &name,
                      COLUMN_PRESENT_IN_ALL, &present_in_all,
                      -1);

  g_object_set (cell,
                "text", name,
                "foreground-gdk", inherited ? &dialog->insensitive_text_color :
                                  (!present_in_all ? &dialog->not_present_in_all_text_color : NULL),
                "editable", !inherited,
                NULL);
  g_free (name);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_column_set_data_value (GtkTreeViewColumn *tree_column,
                                   GtkCellRenderer *cell,
                                   GtkTreeModel *tree_model,
                                   GtkTreeIter *iter,
                                   gpointer data)
{
  Multiattrib *dialog = (Multiattrib *) data;
  gchar *value;
  gboolean identical_value;
  int inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_VALUE, &value,
                      COLUMN_IDENTICAL_VALUE, &identical_value,
                      -1);

  g_object_set (cell,
                "text", identical_value ? value : _("<various>"),
                "foreground-gdk", inherited ? &dialog->insensitive_text_color :
                                  (!identical_value ? &dialog->not_identical_value_text_color : NULL),
                "editable", !inherited,
                NULL);
  g_free (value);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_column_set_data_visible (GtkTreeViewColumn *tree_column,
                                     GtkCellRenderer *cell,
                                     GtkTreeModel *tree_model,
                                     GtkTreeIter *iter,
                                     gpointer data)
{
  gboolean visibility;
  gboolean identical_visibility;
  int inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_VISIBILITY, &visibility,
                      COLUMN_IDENTICAL_VISIBILITY, &identical_visibility,
                      -1);

  g_object_set (cell,
                "active",       visibility,
                "sensitive",    !inherited,
                "activatable",  !inherited,
                "inconsistent", !identical_visibility,
                NULL);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_column_set_data_show_name (GtkTreeViewColumn *tree_column,
                                       GtkCellRenderer *cell,
                                       GtkTreeModel *tree_model,
                                       GtkTreeIter *iter,
                                       gpointer data)
{
  int show_name_value;
  gboolean identical_show_name;
  int inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_SHOW_NAME_VALUE, &show_name_value,
                      COLUMN_IDENTICAL_SHOW_NAME, &identical_show_name,
                      -1);

  g_object_set (cell,
                "active",       snv_shows_name (show_name_value),
                "sensitive",    !inherited,
                "activatable",  !inherited,
                "inconsistent", !identical_show_name,
                NULL);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_column_set_data_show_value (GtkTreeViewColumn *tree_column,
                                        GtkCellRenderer *cell,
                                        GtkTreeModel *tree_model,
                                        GtkTreeIter *iter,
                                        gpointer data)
{
  int show_name_value;
  gboolean identical_show_value;
  int inherited;

  gtk_tree_model_get (tree_model, iter,
                      COLUMN_INHERITED, &inherited,
                      COLUMN_SHOW_NAME_VALUE, &show_name_value,
                      COLUMN_IDENTICAL_SHOW_VALUE, &identical_show_value,
                      -1);

  g_object_set (cell,
                "active",       snv_shows_value (show_name_value),
                "sensitive",    !inherited,
                "activatable",  !inherited,
                "inconsistent", !identical_show_value,
                NULL);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_edited_name (GtkCellRendererText *cellrenderertext,
                                  gchar *arg1,
                                  gchar *new_name,
                                  gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GedaList *attr_list;
  GList *a_iter;
  OBJECT *o_attrib;
  GschemToplevel *w_current;
  gchar *value, *newtext;
  int visibility;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  w_current = GSCHEM_DIALOG (multiattrib)->w_current;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, arg1)) {
    return;
  }

  if (g_ascii_strcasecmp (new_name, "") == 0) {
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
                      COLUMN_VALUE, &value,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);

  newtext = g_strdup_printf ("%s=%s", new_name, value);

  if (!x_dialog_validate_attribute(GTK_WINDOW(multiattrib), newtext)) {
    g_free (value);
    g_free(newtext);
    return;
  }

  for (a_iter = geda_list_get_glist (attr_list);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    o_attrib = a_iter->data;

    visibility = o_is_visible (w_current->toplevel, o_attrib)
        ? VISIBLE : INVISIBLE;

    /* actually modifies the attribute */
    o_text_change (w_current, o_attrib,
                   newtext, visibility, o_attrib->show_name_value);
  }

  g_object_unref (attr_list);
  g_free (value);
  g_free (newtext);

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);

  /* NB: We don't fix up the model to reflect the edit, we're about to nuke it below... */

  /* Refresh the whole model.. some attribute names may consolidate into one row */
  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_edited_value (GtkCellRendererText *cell_renderer,
                                   gchar *arg1,
                                   gchar *new_value,
                                   gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GedaList *attr_list;
  GList *a_iter;
  OBJECT *o_attrib;
  GschemToplevel *w_current;
  char *name;
  char *old_value;
  char *newtext;
  int visibility;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  w_current = GSCHEM_DIALOG (multiattrib)->w_current;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, arg1)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_NAME, &name,
                      COLUMN_VALUE, &old_value,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);

  /* If the edit didn't change anything, don't adjust any attributes */
  if (strcmp (old_value, new_value) == 0)
    return;

  newtext = g_strdup_printf ("%s=%s", name, new_value);

  if (!x_dialog_validate_attribute(GTK_WINDOW(multiattrib), newtext)) {
    g_free (name);
    g_free(newtext);
    return;
  }

  for (a_iter = geda_list_get_glist (attr_list);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    o_attrib = (OBJECT *)a_iter->data;

    visibility = o_is_visible (w_current->toplevel, o_attrib)
        ? VISIBLE : INVISIBLE;

    /* actually modifies the attribute */
    o_text_change (w_current, o_attrib,
                   newtext, visibility, o_attrib->show_name_value);
  }

  g_object_unref (attr_list);

  g_free (name);
  g_free (newtext);

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);

  /* Fixup the model to reflect the edit */
  gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                      COLUMN_VALUE, new_value,
                      COLUMN_IDENTICAL_VALUE, TRUE,
                      -1);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_toggled_visible (GtkCellRendererToggle *cell_renderer,
                                      gchar *path,
                                      gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  OBJECT *o_attrib;
  GschemToplevel *w_current;
  gboolean new_visibility;
  GedaList *attr_list;
  GList *a_iter;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  w_current = GSCHEM_DIALOG (multiattrib)->w_current;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);

  new_visibility = !gtk_cell_renderer_toggle_get_active (cell_renderer);

  for (a_iter = geda_list_get_glist (attr_list);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    o_attrib = (OBJECT *)a_iter->data;

    /* actually modifies the attribute */
    o_invalidate (w_current, o_attrib);
    o_set_visibility (w_current->toplevel, o_attrib, new_visibility ? VISIBLE : INVISIBLE);
    o_text_recreate (w_current->toplevel, o_attrib);
  }

  g_object_unref (attr_list);

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);

  /* Fixup the model to reflect the edit */
  gtk_list_store_set (GTK_LIST_STORE (model), &iter,
                      COLUMN_VISIBILITY, new_visibility,
                      COLUMN_IDENTICAL_VISIBILITY, TRUE,
                      -1);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_toggled_show_name (GtkCellRendererToggle *cell_renderer,
                                        gchar *path,
                                        gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GschemToplevel *w_current;
  gboolean new_name_visible;
  GedaList *attr_list;
  GList *a_iter;
  gint new_snv;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  w_current = GSCHEM_DIALOG (multiattrib)->w_current;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);

  new_name_visible = !gtk_cell_renderer_toggle_get_active (cell_renderer);

  for (a_iter = geda_list_get_glist (attr_list);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    OBJECT *o_attrib = (OBJECT *)a_iter->data;

    gboolean value_visible = snv_shows_value (o_attrib->show_name_value);

    /* If we switch off the name visibility, but the value was not previously visible, make it so now */
    if (new_name_visible)
      new_snv = value_visible ? SHOW_NAME_VALUE : SHOW_NAME;
    else
      new_snv = SHOW_VALUE;

    o_invalidate (w_current, o_attrib);

    /* actually modifies the attribute */
    o_attrib->show_name_value = new_snv;
    o_text_recreate (w_current->toplevel, o_attrib);
  }

  g_object_unref (attr_list);

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);

  /* NB: We don't fix up the model to reflect the edit, we're about to nuke it below... */

  /* request an update of display for this row */
  /* Recompute the whole model as the consistency for the show value column may be affected above */
  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_toggled_show_value (GtkCellRendererToggle *cell_renderer,
                                         gchar *path,
                                         gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GschemToplevel *w_current;
  gboolean new_value_visible;
  GedaList *attr_list;
  GList *a_iter;
  gint new_snv;

  model = gtk_tree_view_get_model (multiattrib->treeview);
  w_current = GSCHEM_DIALOG (multiattrib)->w_current;

  if (!gtk_tree_model_get_iter_from_string (model, &iter, path)) {
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);

  new_value_visible = !gtk_cell_renderer_toggle_get_active (cell_renderer);

  for (a_iter = geda_list_get_glist (attr_list);
       a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    OBJECT *o_attrib = (OBJECT *)a_iter->data;

    gboolean name_visible = snv_shows_name (o_attrib->show_name_value);

    /* If we switch off the name visibility, but the value was not previously visible, make it so now */
    if (new_value_visible)
      new_snv = name_visible ? SHOW_NAME_VALUE : SHOW_VALUE;
    else
      new_snv = SHOW_NAME;

    o_invalidate (w_current, o_attrib);

    /* actually modifies the attribute */
    o_attrib->show_name_value = new_snv;
    o_text_recreate (w_current->toplevel, o_attrib);
  }

  g_object_unref (attr_list);

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);

  /* NB: We don't fix up the model to reflect the edit, we're about to nuke it below... */

  /* request an update of display for this row */
  /* Recompute the whole model as the consistency for the show name column may be affected above */
  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean
multiattrib_callback_key_pressed (GtkWidget *widget,
                                  GdkEventKey *event,
                                  gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;

  if (event->state == 0 &&
      (event->keyval == GDK_Delete || event->keyval == GDK_KP_Delete)) {
    GtkTreeModel *model;
    GtkTreeIter iter;
    GedaList *attr_list;
    int inherited;
    /* delete the currently selected attribute */

    if (!gtk_tree_selection_get_selected (
          gtk_tree_view_get_selection (multiattrib->treeview),
          &model, &iter)) {
      /* nothing selected, nothing to do */
      return FALSE;
    }

    gtk_tree_model_get (model, &iter,
                        COLUMN_INHERITED, &inherited,
                        COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                        -1);

    /* We can't delete inherited attribtes */
    if (inherited)
      return FALSE;

    multiattrib_action_delete_attributes (multiattrib,
                                          geda_list_get_glist (attr_list));

    g_object_unref (attr_list);

    /* update the treeview contents */
    multiattrib_update (multiattrib);
  }

  return FALSE;
}


/*! \brief Move edit focus to the cell pointed to by a mouse event.
 *  \par Function Description
 *  Uses the X and Y coordinates of a mouse event, to move edit focus
 *  to the cell at those coords.
 *
 * NB: The coordinates must be relative to the tree view's bin window, IE.. have
 *     come from en event where event->window == gtk_tree_view_get_bin_window ().
 *
 *  \param [in] multiattrib  The Multiattrib object.
 *  \param [in] x            The x coordinate of the mouse event.
 *  \param [in] y            The y coordinate of the mouse event.
 */
static void
multiattrib_edit_cell_at_pos (Multiattrib *multiattrib, gint x, gint y)
{
  GtkTreePath *path;
  GtkTreeViewColumn *column;

  if (gtk_tree_view_get_path_at_pos (multiattrib->treeview,
                                     x, y, &path, &column, NULL, NULL)) {

    gtk_tree_view_set_cursor_on_cell (multiattrib->treeview,
                                      path, column, NULL, TRUE);
  }
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean
multiattrib_callback_button_pressed (GtkWidget *widget,
                                     GdkEventButton *event,
                                     gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  gboolean ret = FALSE;

  /* popup menu on right click */
  if (event->type == GDK_BUTTON_PRESS && event->button == 3) {
    multiattrib_popup_menu (multiattrib, event);
    ret = TRUE;
  }

  /* edit cell on double (left) click */
  /* (Normally, edit focus by click is handled for us, but this function is useful
   * for overriding the default behavior of treating a double-click the same as a
   * single-click, with edit focus needing two consecutive double or single clicks
   * with a pause in between.  This can be unintuitive and time-wasting) */
  else
  if (event->type == GDK_2BUTTON_PRESS && event->button == 1) {
    multiattrib_edit_cell_at_pos (multiattrib, event->x, event->y);
    ret = TRUE;
  }

  return ret;
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean
multiattrib_callback_popup_menu (GtkWidget *widget,
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
static void
multiattrib_callback_popup_duplicate (GtkMenuItem *menuitem,
                                      gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GedaList *attr_list;

  if (!gtk_tree_selection_get_selected (
        gtk_tree_view_get_selection (multiattrib->treeview),
        &model, &iter)) {
    /* nothing selected, nothing to do */
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);
  multiattrib_action_duplicate_attributes (multiattrib, geda_list_get_glist (attr_list));
  g_object_unref (attr_list);

  /* update the treeview contents */
  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_popup_promote (GtkMenuItem *menuitem,
                                    gpointer user_data)
{
  Multiattrib *multiattrib = user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GedaList *attr_list;

  if (!gtk_tree_selection_get_selected (
         gtk_tree_view_get_selection (multiattrib->treeview),
         &model, &iter)) {
    /* nothing selected, nothing to do */
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);
  multiattrib_action_promote_attributes (multiattrib, geda_list_get_glist (attr_list));
  g_object_unref (attr_list);

  /* update the treeview contents */
  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_popup_delete (GtkMenuItem *menuitem,
                                   gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GedaList *attr_list;

  if (!gtk_tree_selection_get_selected (
        gtk_tree_view_get_selection (multiattrib->treeview),
        &model, &iter)) {
    /* nothing selected, nothing to do */
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);
  multiattrib_action_delete_attributes (multiattrib, geda_list_get_glist (attr_list));
  g_object_unref (attr_list);

  /* update the treeview contents */
  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static void
multiattrib_callback_popup_copy_to_all (GtkMenuItem *menuitem,
                                        gpointer user_data)
{
  Multiattrib *multiattrib = user_data;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GedaList *attr_list;

  if (!gtk_tree_selection_get_selected (
         gtk_tree_view_get_selection (multiattrib->treeview),
         &model, &iter)) {
    /* nothing selected, nothing to do */
    return;
  }

  gtk_tree_model_get (model, &iter,
                      COLUMN_ATTRIBUTE_GEDALIST, &attr_list,
                      -1);
  multiattrib_action_copy_attribute_to_all (multiattrib, geda_list_get_glist (attr_list));
  g_object_unref (attr_list);

  /* update the treeview contents */
  multiattrib_update (multiattrib);
}

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 */
static gboolean
multiattrib_callback_value_key_pressed (GtkWidget *widget,
                                        GdkEventKey *event,
                                        gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)widget;
  gboolean retval = FALSE;

  /* ends editing of cell if one of these keys are pressed: */
  /*  - the Return key without the Control modifier */
  /*  - the Tab key without the Control modifier */
  if ((event->keyval == GDK_Return || event->keyval == GDK_KP_Enter) ||
      (event->keyval == GDK_Tab    || event->keyval == GDK_KP_Tab ||
       event->keyval == GDK_ISO_Left_Tab)) {
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
static void
multiattrib_callback_value_grab_focus (GtkWidget *widget, gpointer user_data)
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
static void
multiattrib_callback_button_add (GtkButton *button, gpointer user_data)
{
  Multiattrib *multiattrib = (Multiattrib*)user_data;
  GtkTextBuffer *buffer;
  GtkTextIter start, end;
  const gchar *name;
  gchar *value;
  gboolean visible;
  gint shownv;

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

  multiattrib_action_add_attribute (multiattrib,
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
static void
multiattrib_init_attrib_names (GtkCombo *combo)
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
static void
multiattrib_init_visible_types (GtkOptionMenu *optionmenu)
{
  GtkWidget *menu, *item;

  menu = gtk_menu_new ();
  item = gtk_menu_item_new_with_label (_("Show Name & Value"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  item = gtk_menu_item_new_with_label (_("Show Value only"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
  item = gtk_menu_item_new_with_label (_("Show Name only"));
  gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);

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
static void
multiattrib_popup_menu (Multiattrib *multiattrib, GdkEventButton *event)
{
  GtkTreePath *path;
  GtkWidget *menu;
  struct menuitem_t {
    gchar *label;
    GCallback callback;
  };

  struct menuitem_t menuitems_inherited[] = {
    { N_("Promote"),     G_CALLBACK (multiattrib_callback_popup_promote)    },
    { NULL,              NULL                                               } };

  struct menuitem_t menuitems_noninherited[] = {
    { N_("Duplicate"),   G_CALLBACK (multiattrib_callback_popup_duplicate)  },
    { N_("Delete"),      G_CALLBACK (multiattrib_callback_popup_delete)     },
    { N_("Copy to all"), G_CALLBACK (multiattrib_callback_popup_copy_to_all)},
    { NULL,              NULL                                               } };

  struct menuitem_t *item_list;
  struct menuitem_t *tmp;
  int inherited;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GtkTreeSelection *selection;

  selection = gtk_tree_view_get_selection (multiattrib->treeview);

  if (event != NULL &&
      gtk_tree_view_get_path_at_pos (multiattrib->treeview,
                                     (gint)event->x,
                                     (gint)event->y,
                                     &path, NULL, NULL, NULL)) {
    gtk_tree_selection_unselect_all (selection);
    gtk_tree_selection_select_path (selection, path);
    gtk_tree_path_free (path);
  }

  /* if nothing is selected, nothing to do */
  if (!gtk_tree_selection_get_selected (selection, &model, &iter))
    return;

  gtk_tree_model_get (model, &iter,
                      COLUMN_INHERITED, &inherited,
                      -1);

  item_list = inherited ? menuitems_inherited : menuitems_noninherited;

  /* create the context menu */
  menu = gtk_menu_new();
  for (tmp = item_list; tmp->label != NULL; tmp++) {
    GtkWidget *menuitem;
    if (strcmp (tmp->label, "-") == 0) {
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
multiattrib_geometry_save (GschemDialog *dialog, EdaConfig *cfg, gchar *group_name)
{
  gboolean show_inherited;

  /* Call the parent's geometry_save method */
  GSCHEM_DIALOG_CLASS (multiattrib_parent_class)->
    geometry_save (dialog, cfg, group_name);

  show_inherited =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (MULTIATTRIB (dialog)->show_inherited));
  eda_config_set_boolean (cfg, group_name, "show_inherited", show_inherited);
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
multiattrib_geometry_restore (GschemDialog *dialog, EdaConfig *cfg, gchar *group_name)
{
  gboolean show_inherited;
  GError *error = NULL;

  /* Call the parent's geometry_restore method */
  GSCHEM_DIALOG_CLASS (multiattrib_parent_class)->
    geometry_restore (dialog, cfg, group_name);

  show_inherited = eda_config_get_boolean (cfg, group_name, "show_inherited", &error);
  if (error != NULL) {
    show_inherited = TRUE;
    g_error_free (error);
  }
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (MULTIATTRIB (dialog)->show_inherited), show_inherited);
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
GType
multiattrib_get_type ()
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


/*! \brief Update the multiattrib editor dialog when its object list changes.
 *
 *  \par Function Description
 *
 *  \param [in] selection    The GedaList object of we are watching/
 *  \param [in] multiattrib  The multi-attribute editor dialog.
 */
static void
object_list_changed_cb (GedaList *object_list, Multiattrib *multiattrib)
{
  multiattrib_update (multiattrib);
}


/*! \brief Update the dialog when the current object GedaList object is destroyed
 *
 *  \par Function Description
 *
 *  This handler is called when the g_object_weak_ref() on the GedaList object
 *  we're watching expires. We reset our multiattrib->object_list pointer to NULL
 *  to avoid attempting to access the destroyed object. NB: Our signal handlers
 *  were automatically disconnected during the destruction process.
 *
 *  \param [in] data                  Pointer to the multi-attrib dialog
 *  \param [in] where_the_object_was  Pointer to where the object was just destroyed
 */
static void
object_list_weak_ref_cb (gpointer data, GObject *where_the_object_was)
{
  Multiattrib *multiattrib = (Multiattrib *)data;

  multiattrib->object_list = NULL;
  multiattrib_update (multiattrib);
}


/*! \brief Connect signal handler and weak_ref on the GedaList object
 *
 *  \par Function Description
 *
 *  Connect the "changed" signal and add a weak reference
 *  on the GedaList object we are going to watch.
 *
 *  \param [in] multiattrib  The Multiattrib dialog.
 *  \param [in] object_list  The GedaList object to watch.
 */
static void
connect_object_list (Multiattrib *multiattrib, GedaList *object_list)
{
  multiattrib->object_list = object_list;
  if (multiattrib->object_list != NULL) {
    g_object_weak_ref (G_OBJECT (multiattrib->object_list),
                       object_list_weak_ref_cb,
                       multiattrib);
    multiattrib->object_list_changed_id =
      g_signal_connect (G_OBJECT (multiattrib->object_list),
                        "changed",
                        G_CALLBACK (object_list_changed_cb),
                        multiattrib);
    /* Synthesise a object_list changed update to refresh the view */
    object_list_changed_cb (multiattrib->object_list, multiattrib);
  } else {
    /* Call an update to set the sensitivities */
    multiattrib_update (multiattrib);
  }
}


/*! \brief Disconnect signal handler and weak_ref on the GedaList object
 *
 *  \par Function Description
 *
 *  If the dialog is watching a GedaList object, disconnect the
 *  "changed" signal and remove our weak reference on the object.
 *
 *  \param [in] multiattrib  The Multiattrib dialog.
 */
static void
disconnect_object_list (Multiattrib *multiattrib)
{
  if (multiattrib->object_list != NULL) {
    g_signal_handler_disconnect (multiattrib->object_list,
                                 multiattrib->object_list_changed_id);
    g_object_weak_unref (G_OBJECT (multiattrib->object_list),
                         object_list_weak_ref_cb,
                         multiattrib);
  }
}


/*! \brief GObject finalise handler
 *
 *  \par Function Description
 *
 *  Just before the Multiattrib GObject is finalized, disconnect from
 *  the GedaList object being watched and then chain up to the parent
 *  class's finalize handler.
 *
 *  \param [in] object  The GObject being finalized.
 */
static void
multiattrib_finalize (GObject *object)
{
  Multiattrib *multiattrib = MULTIATTRIB(object);

  disconnect_object_list (multiattrib);
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
static void
multiattrib_class_init (MultiattribClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GschemDialogClass *gschem_dialog_class = GSCHEM_DIALOG_CLASS (klass);

  gschem_dialog_class->geometry_save    = multiattrib_geometry_save;
  gschem_dialog_class->geometry_restore = multiattrib_geometry_restore;

  gobject_class->set_property = multiattrib_set_property;
  gobject_class->get_property = multiattrib_get_property;
  gobject_class->finalize     = multiattrib_finalize;

  multiattrib_parent_class = g_type_class_peek_parent (klass);

  g_object_class_install_property (
    gobject_class, PROP_OBJECT_LIST,
    g_param_spec_pointer ("object_list",
                          "",
                          "",
                          G_PARAM_READWRITE));
}

/*! \brief Regenerate the attribute list when the visibility
 *         setting for inherited attributes changes
 */
static void
multiattrib_show_inherited_toggled (GtkToggleButton *button,
                                    gpointer user_data)
{
  Multiattrib *multiattrib = user_data;

  /* update the treeview contents */
  multiattrib_update (multiattrib);
}


/*! \brief GType instance initialiser for Multiattrib
 *
 *  \par Function Description
 *
 *  GType instance initialiser for Multiattrib. Create
 *  and setup the widgets which make up the dialog.
 *
 *  \param [in] multiattrib The Multiattrib we are initialising
 */
static void
multiattrib_init (Multiattrib *multiattrib)
{
  GtkWidget *label, *scrolled_win, *treeview;
  GtkWidget *table, *textview, *combo, *optionm, *button;
  GtkWidget *attrib_vbox, *show_inherited;
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
                "title",           _("Edit Attributes"),
                "default-width",   320,
                "default-height",  350,
                "window-position", GTK_WIN_POS_MOUSE,
                "allow-grow",      TRUE,
                "allow-shrink",    FALSE,
                /* GtkDialog */
                "has-separator",   TRUE,
                NULL);

  gtk_box_set_spacing (GTK_BOX (GTK_DIALOG (multiattrib)->vbox), 5);

  /* create the attribute list frame */
  multiattrib->list_frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                                      /* GtkFrame */
                                                      "shadow", GTK_SHADOW_NONE,
                                                      NULL));
  /*   - create the model for the treeview */
  store = (GtkTreeModel*)gtk_list_store_new (NUM_COLUMNS,
                                             G_TYPE_BOOLEAN,  /* COLUMN_INHERITED */
                                             G_TYPE_STRING,   /* COLUMN_NAME */
                                             G_TYPE_STRING,   /* COLUMN_VALUE */
                                             G_TYPE_BOOLEAN,  /* COLUMN_VISIBILITY */
                                             G_TYPE_INT,      /* COLUMN_SHOW_NAME_VALUE */
                                             G_TYPE_BOOLEAN,  /* COLUMN_PRESENT_IN_ALL */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_VALUE */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_VISIBILITY */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_SHOW_NAME */
                                             G_TYPE_BOOLEAN,  /* COLUMN_IDENTICAL_SHOW_VALUE */
                                             G_TYPE_OBJECT);  /* COLUMN_ATTRIBUTE_GEDALIST */

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
                                           multiattrib, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*       - column 2: attribute value */
  renderer = GTK_CELL_RENDERER (
                                g_object_new (TYPE_CELL_RENDERER_MULTI_LINE_TEXT,
                                              /* GtkCellRendererText */
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
                                           multiattrib, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*       - column 3: visibility */
  renderer = GTK_CELL_RENDERER (
                                g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
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
                                           multiattrib, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (treeview), column);
  /*       - column 4: show name */
  renderer = GTK_CELL_RENDERER (
                                g_object_new (GTK_TYPE_CELL_RENDERER_TOGGLE,
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

  attrib_vbox = gtk_vbox_new (FALSE, 0);

  /* Pack the vbox into the frame */
  gtk_container_add (GTK_CONTAINER (multiattrib->list_frame), attrib_vbox);

  /* add the scrolled window to box */
  gtk_box_pack_start (GTK_BOX (attrib_vbox), scrolled_win, TRUE, TRUE, 0);

  /* create the show inherited button */
  show_inherited = gtk_check_button_new_with_label (_("Show inherited attributes"));
  multiattrib->show_inherited = show_inherited;
  gtk_box_pack_start (GTK_BOX (attrib_vbox), show_inherited, FALSE, FALSE, 0);

  g_signal_connect (show_inherited,
                    "toggled",
                    G_CALLBACK (multiattrib_show_inherited_toggled),
                    multiattrib);

  /* pack the frame */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (multiattrib)->vbox),
                      multiattrib->list_frame,
                      TRUE, TRUE, 1);
  gtk_widget_show_all (multiattrib->list_frame);

  /* create the add/edit frame */
  multiattrib->add_frame = GTK_WIDGET (g_object_new (GTK_TYPE_FRAME,
                                       "label", _("Add Attribute"),
                                       NULL));
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
                                           GTK_POLICY_AUTOMATIC,
                                           "vscrollbar-policy",
                                           GTK_POLICY_AUTOMATIC,
                                           "shadow-type",
                                           GTK_SHADOW_IN,
                                           NULL));
  /*! \todo Forcing the size request is a horrible band-aid and
   *  should be replaced by a better heuristic. */
  textview = GTK_WIDGET (g_object_new (GTK_TYPE_TEXT_VIEW, NULL));
  gtk_widget_set_tooltip_text (GTK_WIDGET (textview),
                  _("Ctrl+Enter inserts new line; Ctrl+Tab inserts Tab"));
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

  /* Save this one so we can pick it as a sensible colour to show the
   * inherited attributes dimmed.
   */
  style = gtk_widget_get_style (treeview);
  multiattrib->insensitive_text_color = style->text[ GTK_STATE_INSENSITIVE ];

  gdk_color_parse ("grey", &multiattrib->not_identical_value_text_color);
  gdk_color_parse ("red",  &multiattrib->not_present_in_all_text_color);

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
  gtk_container_add (GTK_CONTAINER (multiattrib->add_frame), table);
  /* pack the frame in the dialog */
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (multiattrib)->vbox),
                      multiattrib->add_frame,
                      FALSE, TRUE, 1);
  gtk_widget_show_all (multiattrib->add_frame);


  /* now add the close button to the action area */
  gtk_dialog_add_button (GTK_DIALOG (multiattrib),
                         GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE);

  multiattrib_update (multiattrib);
}


/*! \brief GObject property setter function
 *
 *  \par Function Description
 *  Setter function for Multiattrib's GObject property, "object_list".
 *
 *  \param [in]  object       The GObject whose properties we are setting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [in]  value        The GValue the property is being set from
 *  \param [in]  pspec        A GParamSpec describing the property being set
 */

static void
multiattrib_set_property (GObject *object,
                          guint property_id,
                          const GValue *value,
                          GParamSpec *pspec)
{
  Multiattrib *multiattrib = MULTIATTRIB (object);

  switch(property_id) {
      case PROP_OBJECT_LIST:
        disconnect_object_list (multiattrib);
        connect_object_list (multiattrib, g_value_get_pointer (value));
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}


/*! \brief GObject property getter function
 *
 *  \par Function Description
 *  Getter function for Multiattrib's GObject property, "object_list".
 *
 *  \param [in]  object       The GObject whose properties we are getting
 *  \param [in]  property_id  The numeric id. under which the property was
 *                            registered with g_object_class_install_property()
 *  \param [out] value        The GValue in which to return the value of the property
 *  \param [in]  pspec        A GParamSpec describing the property being got
 */
static void
multiattrib_get_property (GObject *object,
                          guint property_id,
                          GValue *value,
                          GParamSpec *pspec)
{
  Multiattrib *multiattrib = MULTIATTRIB (object);

  switch(property_id) {
      case PROP_OBJECT_LIST:
        g_value_set_pointer (value, (gpointer)multiattrib->object_list);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

typedef struct {
  gboolean inherited;
  char *name;
  int nth_with_name;
  char *value;
  gboolean visibility;
  int show_name_value;

  gboolean present_in_all;
  gboolean identical_value;
  gboolean identical_visibility;
  gboolean identical_show_name;
  gboolean identical_show_value;

  GedaList *attribute_gedalist;
} MODEL_ROW;

/*! \brief For a given OBJECT, produce a GList of MODEL_ROW records
 *
 *  \par Function Description
 *
 *  The main purpose of this function is to provide the "nth_with_name"
 *  count which we need to merge the attribute lists of various objects
 *  together.
 *
 *  \param [in] multiattrib  The multi-attribute editor dialog (For libgeda API which needs a TOPLEVEL)
 *  \param [in] object       The OBJECT * whos attributes we are processing
 *  \returns  A GList of MODEL_ROW records detailing object's attributes.
 */
static GList *
object_attributes_to_model_rows (Multiattrib *multiattrib, OBJECT *object)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (multiattrib)->w_current;
  GList *model_rows = NULL;
  GList *a_iter;
  GList *object_attribs = o_attrib_return_attribs (object);

  for (a_iter = object_attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {

    OBJECT *a_current = a_iter->data;
    MODEL_ROW *m_row = g_new0 (MODEL_ROW, 1);
    GList *m_iter;

    m_row->inherited = o_attrib_is_inherited (a_current);
    o_attrib_get_name_value (a_current, &m_row->name, &m_row->value);
    m_row->visibility = o_is_visible (w_current->toplevel, a_current);
    m_row->show_name_value = a_current->show_name_value;
    m_row->nth_with_name = 0; /* Provisional value until we check below */

    /* The following fields are always true for a single OBJECT */
    m_row->present_in_all = TRUE;
    m_row->identical_value = TRUE;
    m_row->identical_visibility = TRUE;
    m_row->identical_show_name = TRUE;
    m_row->identical_show_value = TRUE;

    m_row->attribute_gedalist = geda_list_new ();
    geda_list_add (m_row->attribute_gedalist, a_current);

    /* Search already processed attributes to see if we need to bump m_row->nth_with_name */
    for (m_iter = model_rows;
         m_iter != NULL;
         m_iter = g_list_next (m_iter)) {
      MODEL_ROW *m_compare = m_iter->data;
      if (strcmp (m_compare->name, m_row->name) == 0 &&
          m_compare->inherited == m_row->inherited) {
        m_row->nth_with_name = m_row->nth_with_name + 1;
      }
    }

    model_rows = g_list_append (model_rows, m_row);
  }

  g_list_free (object_attribs);

  return model_rows;
}

/*! \brief Produce a GList of MODEL_ROW records for all attribute objects in our GedaList
 *
 *  \par Function Description
 *
 *  This function produces a GList of MODEL_ROWs to the user can edit unattached
 *  attributes, or attributes which are selected separately from their owning
 *  object.
 *
 *  It is not expected this will be called when the GedaList the dialog is watching
 *  contains any higher level objects on which we could edit attributes.
 *
 *  \param [in] multiattrib  The multi-attribute editor dialog
 *  \returns  A GList of MODEL_ROW records detailing all lone selected attributes.
 */
static GList *
lone_attributes_to_model_rows (Multiattrib *multiattrib)
{
  GschemToplevel *w_current = GSCHEM_DIALOG (multiattrib)->w_current;
  GList *o_iter;
  GList *model_rows = NULL;

  /* populate the store with attributes */
  for (o_iter = multiattrib->object_list == NULL ? NULL : geda_list_get_glist (multiattrib->object_list);
       o_iter != NULL;
       o_iter = g_list_next (o_iter)) {
    OBJECT *object = o_iter->data;
    MODEL_ROW *m_row;

    /* Consider a selected text object might be an attribute */
    if (object->type != OBJ_TEXT || !o_attrib_is_attrib (object))
      continue;

    /* We have an OBJ_TEXT which libgeda can parse as an attribute */

    multiattrib->num_lone_attribs_in_list ++;

    m_row = g_new0 (MODEL_ROW, 1);
    m_row->inherited = o_attrib_is_inherited (object);
    o_attrib_get_name_value (object, &m_row->name, &m_row->value);
    m_row->visibility = o_is_visible (w_current->toplevel, object);
    m_row->show_name_value = object->show_name_value;
    m_row->nth_with_name = 0; /* All selected attributes are treated individually */

    /* The following fields are always true for a single attribute */
    m_row->present_in_all = TRUE;
    m_row->identical_value = TRUE;
    m_row->identical_visibility = TRUE;
    m_row->identical_show_name = TRUE;
    m_row->identical_show_value = TRUE;

    m_row->attribute_gedalist = geda_list_new ();
    geda_list_add (m_row->attribute_gedalist, object);

    model_rows = g_list_append (model_rows, m_row);
  }

  return model_rows;
}

/*! \brief Populate the multiattrib editor dialog's liststore
 *
 *  \par Function Description
 *
 *  Consumes the GList of MODEL_ROW data, populating the dialog's liststore.
 *  The function frees / consumes the GList and MODEL_ROW data.
 *
 *  \param [in] multiattrib  The multi-attribute editor dialog.
 *  \param [in] model_rows   A GList of MODEL_ROW data.
 */
static void
multiattrib_populate_liststore (Multiattrib *multiattrib,
                                GList *model_rows)
{
  GtkListStore *liststore;
  GtkTreeIter tree_iter;
  GList *m_iter;

  /* Clear the existing list of attributes */
  liststore = (GtkListStore*)gtk_tree_view_get_model (multiattrib->treeview);
  gtk_list_store_clear (liststore);

  for (m_iter = model_rows;
       m_iter != NULL;
       m_iter = g_list_next (m_iter)) {

    MODEL_ROW *model_row = m_iter->data;

    model_row->present_in_all =
      (g_list_length (geda_list_get_glist (model_row->attribute_gedalist))
       == multiattrib->total_num_in_list);

    gtk_list_store_append (liststore, &tree_iter);
    gtk_list_store_set (liststore,
                        &tree_iter,
                        COLUMN_INHERITED,            model_row->inherited,
                        COLUMN_NAME,                 model_row->name,
                        COLUMN_VALUE,                model_row->value,
                        COLUMN_VISIBILITY,           model_row->visibility,
                        COLUMN_SHOW_NAME_VALUE,      model_row->show_name_value,
                        COLUMN_PRESENT_IN_ALL,       model_row->present_in_all,
                        COLUMN_IDENTICAL_VALUE,      model_row->identical_value,
                        COLUMN_IDENTICAL_VISIBILITY, model_row->identical_visibility,
                        COLUMN_IDENTICAL_SHOW_NAME,  model_row->identical_show_name,
                        COLUMN_IDENTICAL_SHOW_VALUE, model_row->identical_show_value,
                        COLUMN_ATTRIBUTE_GEDALIST,   model_row->attribute_gedalist,
                        -1);

    /* Drop our ref on the GedaList so it is freed when the model has done with it */
    g_object_unref (model_row->attribute_gedalist);
  }

  g_list_foreach (model_rows, (GFunc) g_free, NULL);
  g_list_free (model_rows);
}

static void
append_dialog_title_extra (GString *title_string,
                           int *num_title_extras,
                           char *text,
                           ...)
{
  va_list args;

  va_start (args, text);
  g_string_append (title_string, ((*num_title_extras)++ == 0) ? " - " : ", ");
  g_string_append_vprintf (title_string, text, args);
  va_end (args);
}

static void
update_dialog_title (Multiattrib *multiattrib, char *complex_title_name)
{
  GString *title_string = g_string_new (_("Edit Attributes"));
  int num_title_extras = 0;

  if (multiattrib->num_complex_in_list > 0) {
    append_dialog_title_extra (title_string, &num_title_extras,
                               ngettext ("%i symbol (%s)", "%i symbols (%s)", multiattrib->num_complex_in_list),
                               multiattrib->num_complex_in_list, complex_title_name);
  }

  if (multiattrib->num_pins_in_list > 0) {
    append_dialog_title_extra (title_string, &num_title_extras,
                               ngettext ("%i pin", "%i pins", multiattrib->num_pins_in_list),
                               multiattrib->num_pins_in_list);
  }

  if (multiattrib->num_nets_in_list > 0) {
    append_dialog_title_extra (title_string, &num_title_extras,
                               ngettext ("%i net", "%i nets", multiattrib->num_nets_in_list),
                               multiattrib->num_nets_in_list);
  }

  if (multiattrib->num_buses_in_list > 0) {
    append_dialog_title_extra (title_string, &num_title_extras,
                               ngettext ("%i bus", "%i buses", multiattrib->num_buses_in_list),
                               multiattrib->num_buses_in_list);
  }

  if (multiattrib->num_lone_attribs_in_list > 0) {
    append_dialog_title_extra (title_string, &num_title_extras,
                               ngettext ("%i attribute", "%i attributes", multiattrib->num_lone_attribs_in_list),
                               multiattrib->num_lone_attribs_in_list);
  }

  char *title = g_string_free (title_string, FALSE);
  g_object_set (G_OBJECT (multiattrib), "title", title, NULL);
  g_free (title);
}

/*! \brief Update the multiattrib editor dialog's interface
 *
 *  \par Function Description
 *
 *  Update the dialog to reflect the attributes of the currently selected
 *  object. If no (or multiple) objects are selected, the dialog's controls
 *  are set insensitive.
 *
 *  \param [in] multiattrib  The multi-attribute editor dialog.
 */
static void
multiattrib_update (Multiattrib *multiattrib)
{
  GList *o_iter;
  GtkStyle *style;
  gboolean show_inherited;
  gboolean list_sensitive;
  gboolean add_sensitive;
  GList *model_rows = NULL;
  char *complex_title_name = NULL;

  show_inherited =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (multiattrib->show_inherited));

  multiattrib->total_num_in_list        = 0;
  multiattrib->num_complex_in_list      = 0;
  multiattrib->num_pins_in_list         = 0;
  multiattrib->num_nets_in_list         = 0;
  multiattrib->num_buses_in_list        = 0;
  multiattrib->num_lone_attribs_in_list = 0;

  /* populate the store with attributes */
  for (o_iter = multiattrib->object_list == NULL ? NULL : geda_list_get_glist (multiattrib->object_list);
       o_iter != NULL;
       o_iter = g_list_next (o_iter)) {
    OBJECT *object = o_iter->data;

    GList *object_rows;
    GList *or_iter;

    if (!is_multiattrib_object (object))
      continue;

    /* Count the different objects we are editing */
    multiattrib->total_num_in_list++;

    if (object->type == OBJ_COMPLEX ||
        object->type == OBJ_PLACEHOLDER) {
      multiattrib->num_complex_in_list++;

      if (complex_title_name == NULL)
        complex_title_name = object->complex_basename;
      else if (strcmp (complex_title_name, object->complex_basename) != 0)
        complex_title_name = _("<various>");
    }

    if (object->type == OBJ_PIN)
      multiattrib->num_pins_in_list++;

    if (object->type == OBJ_NET)
      multiattrib->num_nets_in_list++;

    if (object->type == OBJ_BUS)
      multiattrib->num_buses_in_list++;

    /* populate the store with any attributes from this object */
    object_rows = object_attributes_to_model_rows (multiattrib, object);

    for (or_iter = object_rows;
         or_iter != NULL;
         or_iter = g_list_next (or_iter)) {

      MODEL_ROW *object_row = or_iter->data;
      MODEL_ROW *model_row;
      GList *mr_iter;
      gboolean found = FALSE;

      /* Skip over inherited attributes if we don't want to show them */
      if (!show_inherited && object_row->inherited)
        continue;

      /* Search our list of attributes to see if we already encountered */
      for (mr_iter = model_rows;
           mr_iter != NULL && found == FALSE;
           mr_iter = g_list_next (mr_iter)) {
        model_row = mr_iter->data;
        if (strcmp (model_row->name, object_row->name) == 0 &&
            model_row->nth_with_name == object_row->nth_with_name &&
            model_row->inherited == object_row->inherited)
          found = TRUE;
      }

      if (found) {
        /* Name matches a previously found attribute */
        /* Check if the rest of its properties match the one we have stored... */

        if (strcmp (model_row->value, object_row->value) != 0)
          model_row->identical_value = FALSE;

        if (model_row->visibility != object_row->visibility)
          model_row->identical_visibility = FALSE;

        if (snv_shows_name (model_row->show_name_value) !=
            snv_shows_name (object_row->show_name_value))
          model_row->identical_show_name = FALSE;

        if (snv_shows_value (model_row->show_name_value) !=
            snv_shows_value (object_row->show_name_value))
          model_row->identical_show_value = FALSE;

        /* Add the underlying attribute to the row's GedaList of attributes */
        geda_list_add_glist (model_row->attribute_gedalist,
                             geda_list_get_glist (object_row->attribute_gedalist));

        g_object_unref (object_row->attribute_gedalist);
        g_free (object_row);

      } else {
        /*
         * The attribute name doesn't match any previously found
         * attribute, so add the model row entry describing it to the list.
         */
        model_rows = g_list_append (model_rows, object_row);
      }
    }

    /* delete the list of attribute objects */
    g_list_free (object_rows);
  }

  if (multiattrib->total_num_in_list == 0) {

    /* If the selection contains no high level objects we can edit,
     * take a look and see whether there are any lone attributes
     * selected we can edit directly.
     */
    model_rows = lone_attributes_to_model_rows (multiattrib);
    list_sensitive = (multiattrib->num_lone_attribs_in_list > 0);
    add_sensitive = FALSE;
  } else {
    list_sensitive = TRUE;
    add_sensitive = TRUE;
  }

  multiattrib_populate_liststore (multiattrib, model_rows);

  /* Update window title to describe the objects we are editing. */
  update_dialog_title (multiattrib, complex_title_name);

  /* Update sensitivities */
  gtk_widget_set_sensitive (GTK_WIDGET (multiattrib->list_frame), list_sensitive);
  gtk_widget_set_sensitive (GTK_WIDGET (multiattrib->add_frame),  add_sensitive);

  /* Work around GtkTextView's stubborn indifference
   * to GTK_STATE_INSENSITIVE when rendering its text. */
  style = gtk_widget_get_style (GTK_WIDGET (multiattrib->textview_value));
  gtk_widget_modify_text (GTK_WIDGET (multiattrib->textview_value),
                          GTK_STATE_NORMAL,
                          add_sensitive ? &multiattrib->value_normal_text_color
                                        : &style->text[GTK_STATE_INSENSITIVE]);
}
