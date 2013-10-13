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
/*!
 * \file x_editlprop.c
 *
 * \brief A dialog box for editing an object's line properties.
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
#include <gdk/gdkkeysyms.h>



static gint line_type_dialog_linetype_change (GtkWidget *widget, EditLProp *dialog);


static void
dispose (GObject *object);

static void
entry_activate (GtkWidget *widget, EditLProp *dialog);

static void
line_type_changed (GtkWidget *widget, EditLProp *dialog);

static void
line_width_changed (GtkWidget *widget, EditLProp *dialog);

static void
dash_length_changed (GtkWidget *widget, EditLProp *dialog);

static void
dash_space_changed (GtkWidget *widget, EditLProp *dialog);

static void
cap_style_changed (GtkWidget *widget, EditLProp *dialog);

static void
notify_cap_style (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog);

static void
notify_dash_length (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog);

static void
notify_dash_space (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog);

static void
notify_line_type (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog);

static void
notify_line_width (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog);

static void
update_cap_style (EditLProp *dialog);

static void
update_dash_length (EditLProp *dialog);

static void
update_dash_space (EditLProp *dialog);

static void
update_line_type (EditLProp *dialog);

static void
update_line_width (EditLProp *dialog);



/*! \brief Callback function for the linetype menu item in the line type dialog
 *  \par Function Description
 *  This Function is called when the user changes the line type selection.
 *  It sets the dash space/length entries either active or inactive.
 */
static gint line_type_dialog_linetype_change (GtkWidget *widget, EditLProp *dialog)
{
  gtk_widget_set_sensitive (dialog->space_entry,
                            x_linetypecb_get_use_space (widget));
  gtk_widget_set_sensitive (dialog->length_entry,
                            x_linetypecb_get_use_length (widget));

  return(0);
}



/*! \brief Handles user responses from the edit text dialog box

 *  \par Function Description
 *  Callback function for the edit text dialog.
 *
 *  \param [in,out] dialog The edit text dialog
 *  \param [na]     unused Unused parameter
 */
static void
dialog_response (EditLProp *dialog, gint response, gpointer unused)
{
  g_return_if_fail (dialog != NULL);

  switch(response) {
    case GTK_RESPONSE_CLOSE:
    case GTK_RESPONSE_DELETE_EVENT:
      i_set_state (dialog->parent.w_current, SELECT);
      i_update_toolbar (dialog->parent.w_current);
      gtk_widget_destroy (dialog->parent.w_current->lpwindow);
      dialog->parent.w_current->lpwindow = NULL;
      break;

    default:
      printf ("%s: dialog_response(): strange signal: %d\n", __FILE__, response);
  }
}



/*! \brief A signal handler for when the user presses enter in an entry
 *
 *  Pressing the enter key in an entry moves the focus to the next control
 *  in the column of values and applies the current value. This function
 *  moves the focus to the next control, and the focus-out-event applies the
 *  value.
 *
 *  This signal hander operates for multiple entry widgets.
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog This dialog
 */
static void
entry_activate (GtkWidget *widget, EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  gtk_widget_child_focus (GTK_WIDGET (dialog), GTK_DIR_DOWN);
}



/*! \brief Initialize EditLProp class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klasse
 */
static void
editlprop_class_init (EditLPropClass *klasse)
{
  GObjectClass *object_klasse;

  g_return_if_fail (klasse != NULL);

  object_klasse = G_OBJECT_CLASS (klasse);

  g_return_if_fail (object_klasse != NULL);

  object_klasse->dispose = dispose;
}



/*! \brief Initialize EditLProp instance
 *
 *  \param [in,out] dialog The edit text dialog
 */
static void editlprop_init(EditLProp *dialog)
{
  GtkWidget *alignment;
  GtkWidget *vbox;
  GtkWidget *label = NULL;
  GtkWidget *rlabel[5] = { NULL };
  GtkWidget *table;
  int index;

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CLOSE,
                         GTK_RESPONSE_CLOSE);

  gtk_window_position(GTK_WINDOW (dialog),
                      GTK_WIN_POS_NONE);

  g_signal_connect (G_OBJECT (dialog),
                    "response",
                    G_CALLBACK (dialog_response),
                    NULL);

  gtk_container_border_width(GTK_CONTAINER (dialog),
                             DIALOG_BORDER_SPACING);

  vbox = GTK_DIALOG(dialog)->vbox;
  gtk_box_set_spacing(GTK_BOX(vbox), DIALOG_V_SPACING);

  label = gtk_label_new (_("<b>Line Properties</b>"));
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment(GTK_MISC(label),0,0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);

  alignment = gtk_alignment_new(0,0,1,1);
  gtk_alignment_set_padding(GTK_ALIGNMENT(alignment), 0, 0,
                            DIALOG_INDENTATION, 0);
  gtk_box_pack_start(GTK_BOX(vbox), alignment, FALSE, FALSE, 0);

  table = gtk_table_new (5, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);
  gtk_container_add(GTK_CONTAINER(alignment), table);

  rlabel[0] = gtk_label_new (_("Type:"));
  rlabel[1] = gtk_label_new (_("Width:"));
  rlabel[2] = gtk_label_new (_("Dash Length:"));
  rlabel[3] = gtk_label_new (_("Dash Space:"));
  rlabel[4] = gtk_label_new (_("Cap style:"));

  for (index=0; index<5; index++) {
    gtk_misc_set_alignment (GTK_MISC(rlabel[index]), 0, 0);
    gtk_table_attach (GTK_TABLE(table),rlabel[index],0,1,index,index+1, GTK_FILL,0,0,0);
  }

  dialog->line_type = x_linetypecb_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->line_type,
                            1,2,0,1);

  dialog->width_entry = gschem_integer_combo_box_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->width_entry,
                            1,2,1,2);

  dialog->length_entry = gschem_integer_combo_box_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->length_entry,
                            1,2,2,3);

  dialog->space_entry = gschem_integer_combo_box_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->space_entry,
                            1,2,3,4);

  dialog->line_end = x_linecapcb_new ();
  gtk_table_attach_defaults(GTK_TABLE(table), dialog->line_end,
                            1,2,4,5);

  g_signal_connect(G_OBJECT (dialog->line_type), "changed",
                   G_CALLBACK (line_type_dialog_linetype_change),
                   dialog);

  g_signal_connect(G_OBJECT (dialog->line_type), "changed",
                   G_CALLBACK (line_type_changed),
                   dialog);

  g_signal_connect(G_OBJECT (dialog->width_entry),
                   "apply",
                   G_CALLBACK (line_width_changed),
                   dialog);

  g_signal_connect(G_OBJECT (gschem_integer_combo_box_get_entry (dialog->width_entry)), "activate",
                   G_CALLBACK (entry_activate),
                   dialog);

  g_signal_connect(G_OBJECT (dialog->length_entry),
                   "apply",
                   G_CALLBACK (dash_length_changed),
                   dialog);

  g_signal_connect(G_OBJECT (gschem_integer_combo_box_get_entry (dialog->length_entry)), "activate",
                   G_CALLBACK (entry_activate),
                   dialog);

  g_signal_connect(G_OBJECT (dialog->space_entry),
                   "apply",
                   G_CALLBACK (dash_space_changed),
                   dialog);

  g_signal_connect(G_OBJECT (gschem_integer_combo_box_get_entry (dialog->space_entry)), "activate",
                   G_CALLBACK (entry_activate),
                   dialog);

  g_signal_connect(G_OBJECT (dialog->line_end), "changed",
                   G_CALLBACK (cap_style_changed),
                   dialog);
}



/*! \brief Get/register EditLProp type.
 */
GType editlprop_get_type()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(EditLPropClass),
      NULL,                                   /* base_init */
      NULL,                                   /* base_finalize */
      (GClassInitFunc) editlprop_class_init,
      NULL,                                   /* class_finalize */
      NULL,                                   /* class_data */
      sizeof(EditLProp),
      0,                                      /* n_preallocs */
      (GInstanceInitFunc) editlprop_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG, "EditLProp", &info, 0);
  }

  return type;
}



/*! \brief Set the selection this dialog manipulates
 *
 *  \param [in,out] dialog
 *  \param [in]     selection
 */
void
x_editlprop_set_selection_adapter (EditLProp *dialog, GschemSelectionAdapter *adapter)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (notify_line_width),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (notify_line_type),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (notify_dash_space),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (notify_dash_length),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (notify_cap_style),
                                          dialog);

    g_object_unref (dialog->adapter);
  }

  dialog->adapter = adapter;

  if (dialog->adapter != NULL) {
    g_object_ref (dialog->adapter);

    g_signal_connect (dialog->adapter,
                      "notify::cap-style",
                      G_CALLBACK (notify_cap_style),
                      dialog);
    g_signal_connect (dialog->adapter,
                      "notify::dash-length",
                      G_CALLBACK (notify_dash_length),
                      dialog);

    g_signal_connect (dialog->adapter,
                      "notify::dash-space",
                      G_CALLBACK (notify_dash_space),
                      dialog);

    g_signal_connect (dialog->adapter,
                      "notify::line-type",
                      G_CALLBACK (notify_line_type),
                      dialog);

    g_signal_connect (dialog->adapter,
                      "notify::line-width",
                      G_CALLBACK (notify_line_width),
                      dialog);
  }

  update_cap_style (dialog);
  update_dash_length (dialog);
  update_dash_space (dialog);
  update_line_type (dialog);
  update_line_width (dialog);
}



/*! \brief Dispose
 *
 *  \param [in,out] object This object
 */
static void
dispose (GObject *object)
{
  EditLProp *dialog;
  EditLPropClass *klasse;
  GObjectClass *parent_klasse;

  g_return_if_fail (object != NULL);

  dialog = EDITLPROP (object);

  x_editlprop_set_selection_adapter (dialog, NULL);

  /* lastly, chain up to the parent dispose */

  klasse = EDITLPROP_GET_CLASS (object);
  g_return_if_fail (klasse != NULL);
  parent_klasse = g_type_class_peek_parent (klasse);
  g_return_if_fail (parent_klasse != NULL);
  parent_klasse->dispose (object);
}



/*! \brief Respond to change in the dialog box value of the line type
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
line_type_changed (GtkWidget *widget, EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->line_type != NULL)) {
    int line_type;

    g_return_if_fail (widget == dialog->line_type);

    line_type = x_linetypecb_get_index (dialog->line_type);

    if (line_type >= 0) {
      gschem_selection_adapter_set_line_type (dialog->adapter, line_type);

      toplevel->page_current->CHANGED = 1;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
}



/*! \brief Respond to change in the dialog box value of the line width
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
line_width_changed (GtkWidget *widget, EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->width_entry != NULL)) {
    g_return_if_fail (widget == dialog->width_entry);

    int line_width = gschem_integer_combo_box_get_value (dialog->width_entry);

    if (line_width >= 0) {
      gschem_selection_adapter_set_line_width (dialog->adapter, line_width);

      toplevel->page_current->CHANGED = 1;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
}



/*! \brief Respond to change in the dialog box value of the dash length
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
dash_length_changed (GtkWidget *widget, EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->length_entry != NULL)) {
    g_return_if_fail (widget == dialog->length_entry);

    int dash_length = gschem_integer_combo_box_get_value (dialog->length_entry);

    if (dash_length >= 0) {
      gschem_selection_adapter_set_dash_length (dialog->adapter, dash_length);

      toplevel->page_current->CHANGED = 1;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
}



/*! \brief Respond to change in the dialog box value of the dash space
 * *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
dash_space_changed (GtkWidget *widget, EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->space_entry != NULL)) {
    g_return_if_fail (widget == dialog->space_entry);

    int dash_space = gschem_integer_combo_box_get_value (dialog->space_entry);

    if (dash_space >= 0) {
      gschem_selection_adapter_set_dash_space (dialog->adapter, dash_space);

      toplevel->page_current->CHANGED = 1;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
}



/*! \brief Respond to change in the dialog box value of the cap style
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
cap_style_changed (GtkWidget *widget, EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->line_end != NULL)) {
    int cap_style;

    g_return_if_fail (widget == dialog->line_end);

    cap_style = x_linecapcb_get_index (dialog->line_end);

    if (cap_style >= 0) {
      gschem_selection_adapter_set_cap_style (dialog->adapter, cap_style);

      toplevel->page_current->CHANGED = 1;
      o_undo_savestate(w_current, UNDO_ALL);
    }
  }
}



/*! \brief Signal handler for when the selection cap style changes
 *
 *  \param [in]     adapter
 *  \param [in]     pspec
 *  \param [in,out] dialog
 */
static void
notify_cap_style (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (adapter != NULL);

  if (dialog->adapter != NULL) {
    g_return_if_fail (adapter == dialog->adapter);

    update_cap_style (dialog);
  }
}



/*! \brief Signal handler for when the selection dash length changes
 *
 *  \param [in]     adapter
 *  \param [in]     pspec
 *  \param [in,out] dialog
 */
static void
notify_dash_length (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (adapter != NULL);

  if (dialog->adapter != NULL) {
    g_return_if_fail (adapter == dialog->adapter);

    update_dash_length (dialog);
  }
}



/*! \brief Signal handler for when the selection dash space changes
 *
 *  \param [in]     adapter
 *  \param [in]     pspec
 *  \param [in,out] dialog
 */
static void
notify_dash_space (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (adapter != NULL);

  if (dialog->adapter != NULL) {
    g_return_if_fail (adapter == dialog->adapter);

    update_dash_space (dialog);
  }
}



/*! \brief Signal handler for when the selection line type changes
 *
 *  \param [in]     adapter
 *  \param [in]     pspec
 *  \param [in,out] dialog
 */
static void
notify_line_type (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (adapter != NULL);

  if (dialog->adapter != NULL) {
    g_return_if_fail (adapter == dialog->adapter);

    update_line_type (dialog);
  }
}



/*! \brief Signal handler for when the selection line width changes
 *
 *  \param [in]     adapter
 *  \param [in]     pspec
 *  \param [in,out] dialog
 */
static void
notify_line_width (GschemSelectionAdapter *adapter, GParamSpec *pspec, EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (adapter != NULL);

  if (dialog->adapter != NULL) {
    g_return_if_fail (adapter == dialog->adapter);

    update_line_width (dialog);
  }
}



/*! \brief Update the cap style in the dialog box
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_cap_style (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    OBJECT_END end = gschem_selection_adapter_get_cap_style (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->line_end),
                                    G_CALLBACK (cap_style_changed),
                                    dialog);

    x_linecapcb_set_index (dialog->line_end, end);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->line_end),
                                      G_CALLBACK (cap_style_changed),
                                      dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_end), (end != -1));
  }
}



/*! \brief Update the dash length in the dialog box widgets.
 *
 *  \param [in,out] dialog    This dialog
 */
static void
update_dash_length (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    gint length = gschem_selection_adapter_get_dash_length (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->length_entry),
                                    G_CALLBACK (dash_length_changed),
                                    dialog);

    gschem_integer_combo_box_set_value (dialog->length_entry, length);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->length_entry),
                                      G_CALLBACK (dash_length_changed),
                                      dialog);

    /* dash length and dash space are enabled/disabled by the value in line type */
  }
}



/*! \brief Update the dash space in the dialog box widgets.
 *
 *  \param [in,out] dialog    This dialog
 */
static void
update_dash_space (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    gint space = gschem_selection_adapter_get_dash_space (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->space_entry),
                                    G_CALLBACK (dash_space_changed),
                                    dialog);

    gschem_integer_combo_box_set_value (dialog->space_entry, space);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->space_entry),
                                      G_CALLBACK (dash_space_changed),
                                      dialog);

    /* dash length and dash space are enabled/disabled by the value in line type */
  }
}



/*! \brief Update the line type in the dialog box.
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_line_type (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    OBJECT_TYPE type = gschem_selection_adapter_get_line_type (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->line_type),
                                    G_CALLBACK (line_type_changed),
                                    dialog);

    x_linetypecb_set_index (dialog->line_type, type);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->line_type),
                                      G_CALLBACK (line_type_changed),
                                      dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_type), (type != -1));
  }
}



/*! \brief Update the line width in the dialog box.
 *
 *  \param [in,out] dialog    This dialog
 */
static void
update_line_width (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    gint width = gschem_selection_adapter_get_line_width (dialog->adapter);


    g_signal_handlers_block_by_func(G_OBJECT (dialog->width_entry),
                                    G_CALLBACK (line_width_changed),
                                    dialog);

    gschem_integer_combo_box_set_value (dialog->width_entry, width);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->width_entry),
                                      G_CALLBACK (line_width_changed),
                                      dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->width_entry), (width != -1));
  }
}



/*! \brief Open the dialog box to edit fill properties
 *
 *  \par Function Description
 *  This function creates or raises the modal text entry dialog
 *
 *  \param [in] w_current The gschem toplevel
 */
void
line_type_dialog (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  if (w_current->lpwindow == NULL) {
    /* dialog not created yet */
    w_current->lpwindow = g_object_new (TYPE_EDITLPROP,
                                        /* GtkContainer */
                                        "border-width",    DIALOG_BORDER_SPACING,
                                        /* GtkWindow */
                                        "title",           _("Edit Line Width & Type"),
                                        "default-width",   320,
                                        "default-height",  350,
                                        "window-position", GTK_WIN_POS_MOUSE,
                                        "allow-grow",      TRUE,
                                        "allow-shrink",    FALSE,
                                        "modal",           FALSE,
                                        /* GtkDialog */
                                        "has-separator",   TRUE,
                                        /* GschemDialog */
                                        "settings-name",    "line-type",
                                        "gschem-toplevel",  w_current,
                                        NULL);

    x_editlprop_set_selection_adapter (EDITLPROP (w_current->lpwindow),
                                       gschem_toplevel_get_selection_adapter (w_current));

    gtk_window_set_transient_for (GTK_WINDOW (w_current->lpwindow),
                                  GTK_WINDOW (w_current->main_window));

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->width_entry,
                           gschem_toplevel_get_line_width_list_store (w_current));

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->length_entry,
                           gschem_toplevel_get_dash_length_list_store (w_current));

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->space_entry,
                           gschem_toplevel_get_dash_space_list_store (w_current));

    gtk_widget_show_all (w_current->lpwindow);
  }
  else {
    /* dialog already created */
    x_editlprop_set_selection_adapter (EDITLPROP (w_current->lpwindow),
                                       gschem_toplevel_get_selection_adapter (w_current));

    gtk_window_present (GTK_WINDOW (w_current->lpwindow));
  }
}
