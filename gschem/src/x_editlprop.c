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



static GtkWidget*
create_fill_property_widget (EditLProp *dialog);

static GtkWidget*
create_line_property_widget (EditLProp *dialog);

static GtkWidget*
create_object_property_widget (EditLProp *dialog);

static GtkWidget*
create_property_label (const char *label);

static GtkWidget*
create_section_widget (const char *label, GtkWidget *child);

static void
dispose (GObject *object);

static void
entry_activate (GtkWidget *widget, EditLProp *dialog);

static void
handle_undo (EditLProp *dialog);

static void
update_cap_style_model (GtkWidget *widget, EditLProp *dialog);

static void
update_cap_style_widget (EditLProp *dialog);

static void
update_fill_type_model (GtkWidget *widget, EditLProp *dialog);

static void
update_fill_type_widget (EditLProp *dialog);

static void
update_line_type_model (GtkWidget *widget, EditLProp *dialog);

static void
update_line_type_widget (EditLProp *dialog);

static void
update_object_color_model (GtkWidget *widget, EditLProp *dialog);

static void
update_object_color_widget (EditLProp *dialog);



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



/*! \brief Initialize EditLProp class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klass
 */
static void
editlprop_class_init (EditLPropClass *klass)
{
  GObjectClass *object_class;

  g_return_if_fail (klass != NULL);

  object_class = G_OBJECT_CLASS (klass);

  g_return_if_fail (object_class != NULL);

  object_class->dispose = dispose;
}



/*! \brief Initialize EditLProp instance
 *
 *  \param [in,out] dialog The edit text dialog
 */
static void
editlprop_init (EditLProp *dialog)
{
  GtkWidget *vbox;
  GtkWidget *widget;

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

  widget = create_object_property_widget (dialog);
  gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, FALSE, 0);

  widget = create_line_property_widget (dialog);
  gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, FALSE, 0);

  widget = create_fill_property_widget (dialog);
  gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, FALSE, 0);
}



/*! \brief Get/register EditLProp type.
 */
GType
editlprop_get_type()
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
                                          G_CALLBACK (update_object_color_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (update_line_type_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (update_fill_type_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (update_cap_style_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (handle_undo),
                                          dialog);

    g_object_unref (dialog->adapter);
  }

  dialog->adapter = adapter;

  g_slist_foreach (dialog->bindings,
                   (GFunc) gschem_binding_set_model_object,
                   adapter);

  if (dialog->adapter != NULL) {
    g_object_ref (dialog->adapter);

    g_signal_connect_swapped (dialog->adapter,
                              "handle-undo",
                              G_CALLBACK (handle_undo),
                              dialog);

    g_signal_connect_swapped (dialog->adapter,
                              "notify::cap-style",
                              G_CALLBACK (update_cap_style_widget),
                              dialog);

    g_signal_connect_swapped (dialog->adapter,
                              "notify::fill-type",
                              G_CALLBACK (update_fill_type_widget),
                              dialog);

    g_signal_connect_swapped (dialog->adapter,
                              "notify::line-type",
                              G_CALLBACK (update_line_type_widget),
                              dialog);

    g_signal_connect_swapped (dialog->adapter,
                              "notify::object-color",
                              G_CALLBACK (update_object_color_widget),
                              dialog);
  }

  update_cap_style_widget (dialog);
  update_fill_type_widget (dialog);
  update_line_type_widget (dialog);
  update_object_color_widget (dialog);
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

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->widthe,
                           gschem_toplevel_get_fill_width_list_store (w_current));

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->angle1e,
                           gschem_toplevel_get_fill_angle_list_store (w_current));

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->pitch1e,
                           gschem_toplevel_get_fill_pitch_list_store (w_current));

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->angle2e,
                           gschem_toplevel_get_fill_angle_list_store (w_current));

    gschem_integer_combo_box_set_model (EDITLPROP (w_current->lpwindow)->pitch2e,
                           gschem_toplevel_get_fill_pitch_list_store (w_current));

    gtk_widget_show_all (w_current->lpwindow);
  }
  else {
    /* dialog already created */
    x_editlprop_set_selection_adapter (EDITLPROP (w_current->lpwindow),
                                       gschem_toplevel_get_selection_adapter (w_current));

    gtk_window_present (GTK_WINDOW (w_current->lpwindow));
  }
}



/*! \private
 *  \brief Create a fill property section widget
 *
 *  \param [in] dialog
 *  \return A new fill property section widget
 */
static GtkWidget*
create_fill_property_widget (EditLProp *dialog)
{
  int index;
  GtkWidget *label[6];
  GtkWidget *table;
  GtkWidget *widget[6];

  table = gtk_table_new (6, 2, FALSE);
  gtk_table_set_row_spacings(GTK_TABLE(table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings(GTK_TABLE(table), DIALOG_H_SPACING);

  label[0] = create_property_label (_("Fill Type:"));
  label[1] = create_property_label (_("Line Width:"));
  label[2] = create_property_label (_("Angle 1:"));
  label[3] = create_property_label (_("Pitch 1:"));
  label[4] = create_property_label (_("Angle 2:"));
  label[5] = create_property_label (_("Pitch 2:"));

  widget[0] = dialog->fstylecb = x_fstylecb_new ();
  widget[1] = dialog->widthe = gschem_integer_combo_box_new ();
  widget[2] = dialog->angle1e = gschem_integer_combo_box_new ();
  widget[3] = dialog->pitch1e = gschem_integer_combo_box_new ();
  widget[4] = dialog->angle2e = gschem_integer_combo_box_new ();
  widget[5] = dialog->pitch2e = gschem_integer_combo_box_new ();

  for (index=0; index<6; index++) {
    gtk_table_attach (GTK_TABLE (table),
                      label[index],      /* child         */
                      0,                 /* left_attach   */
                      1,                 /* right_attach  */
                      index,             /* top_attach    */
                      index+1,           /* bottom_attach */
                      GTK_FILL,          /* xoptions      */
                      0,                 /* yoptions      */
                      0,                 /* xpadding      */
                      0);                /* ypadding      */

    gtk_table_attach_defaults (GTK_TABLE (table),
                               widget[index],     /* child         */
                               1,                 /* left_attach   */
                               2,                 /* right_attach  */
                               index,             /* top_attach    */
                               index+1);          /* bottom_attach */
  }

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("fill-width",
                                                                 widget[1]));

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("fill-angle1",
                                                                 widget[2]));

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("fill-pitch1",
                                                                 widget[3]));

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("fill-angle2",
                                                                 widget[4]));

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("fill-pitch2",
                                                                 widget[5]));

  g_signal_connect (G_OBJECT (dialog->fstylecb), "changed",
                    G_CALLBACK (update_fill_type_model),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->widthe)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->angle1e)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->pitch1e)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->angle2e)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->pitch2e)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  return create_section_widget (_("<b>Fill Properties</b>"), table);
}



/*! \private
 *  \brief Create a line property section widget
 *
 *  \param [in] dialog
 *  \return A new line property section widget
 */
static GtkWidget*
create_line_property_widget (EditLProp *dialog)
{
  int index;
  GtkWidget *label[5];
  GtkWidget *table;
  GtkWidget *widget[5];

  table = gtk_table_new (5, 2, FALSE);
  gtk_table_set_row_spacings (GTK_TABLE (table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (table), DIALOG_H_SPACING);

  label[0] = create_property_label (_("Type:"));
  label[1] = create_property_label (_("Width:"));
  label[2] = create_property_label (_("Dash Length:"));
  label[3] = create_property_label (_("Dash Space:"));
  label[4] = create_property_label (_("Cap style:"));

  widget[0] = dialog->line_type = x_linetypecb_new ();
  widget[1] = dialog->width_entry = gschem_integer_combo_box_new ();
  widget[2] = dialog->length_entry = gschem_integer_combo_box_new ();
  widget[3] = dialog->space_entry = gschem_integer_combo_box_new ();
  widget[4] = dialog->line_end = x_linecapcb_new ();

  for (index=0; index<5; index++) {
    gtk_table_attach (GTK_TABLE (table),
                      label[index],      /* child         */
                      0,                 /* left_attach   */
                      1,                 /* right_attach  */
                      index,             /* top_attach    */
                      index+1,           /* bottom_attach */
                      GTK_FILL,          /* xoptions      */
                      0,                 /* yoptions      */
                      0,                 /* xpadding      */
                      0);                /* ypadding      */

    gtk_table_attach_defaults (GTK_TABLE (table),
                               widget[index],     /* child         */
                               1,                 /* left_attach   */
                               2,                 /* right_attach  */
                               index,             /* top_attach    */
                               index+1);          /* bottom_attach */
  }

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("line-width",
                                                                 widget[1]));

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("dash-length",
                                                                 widget[2]));

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("dash-space",
                                                                 widget[3]));

  g_signal_connect (G_OBJECT (dialog->line_type), "changed",
                    G_CALLBACK (update_line_type_model),
                    dialog);

  g_signal_connect (G_OBJECT (dialog->line_end), "changed",
                    G_CALLBACK (update_cap_style_model),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->width_entry)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->length_entry)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->space_entry)),
                    "activate",
                    G_CALLBACK (entry_activate),
                    dialog);

  return create_section_widget (_("<b>Line Properties</b>"), table);
}



/*! \private
 *  \brief Create a line property section widget
 *
 *  \param [in] dialog
 *  \return A new line property section widget
 */
static GtkWidget*
create_object_property_widget (EditLProp *dialog)
{
  GtkWidget *label;
  GtkWidget *table;

  table = gtk_table_new (1, 2, FALSE);
  gtk_table_set_row_spacings (GTK_TABLE (table), DIALOG_V_SPACING);
  gtk_table_set_col_spacings (GTK_TABLE (table), DIALOG_H_SPACING);

  label = create_property_label (_("Color:"));

  gtk_table_attach (GTK_TABLE (table),
                    label,             /* child         */
                    0,                 /* left_attach   */
                    1,                 /* right_attach  */
                    0,                 /* top_attach    */
                    1,                 /* bottom_attach */
                    GTK_FILL,          /* xoptions      */
                    0,                 /* yoptions      */
                    0,                 /* xpadding      */
                    0);                /* ypadding      */

  dialog->colorcb = x_colorcb_new ();

  gtk_table_attach_defaults (GTK_TABLE (table),
                             dialog->colorcb,    /* child         */
                             1,                  /* left_attach   */
                             2,                  /* right_attach  */
                             0,                  /* top_attach    */
                             1);                 /* bottom_attach */

  g_signal_connect(G_OBJECT (dialog->colorcb),
                   "changed",
                   G_CALLBACK (update_object_color_model),
                   dialog);

  return create_section_widget (_("<b>Object Properties</b>"), table);
}



/*! \private
 *  \brief Create a property label widget
 *
 *  \param [in] label The label text for this property
 *  \return A new property label widget
 */
static GtkWidget*
create_property_label (const char *label)
{
  GtkWidget *widget = gtk_label_new (label);

  gtk_misc_set_alignment (GTK_MISC (widget),
                          0.0,                  /* xalign */
                          0.0);                 /* yalign */

  return widget;
}



/*! \private
 *  \brief Create a section widget
 *
 *  Creates a widget to represent a section in the property editor. This
 *  function wraps the child widget with additional widgets to generate the
 *  proper layout.
 *
 *  \param [in] label The markup text for this section
 *  \param [in] child The child widget for this section
 *  \return A new section widget
 */
static GtkWidget*
create_section_widget (const char *label, GtkWidget *child)
{
  GtkWidget *alignment;
  GtkWidget *expander;

  alignment = gtk_alignment_new (0.0,     /* xalign */
                                 0.0,     /* yalign */
                                 1.0,     /* xscale */
                                 1.0);    /* yscale */

  gtk_alignment_set_padding (GTK_ALIGNMENT(alignment),
                            0,                     /* padding_top    */
                            0,                     /* padding_bottom */
                            DIALOG_INDENTATION,    /* padding_left   */
                            0);                    /* padding_right  */

  gtk_container_add (GTK_CONTAINER (alignment), child);

  expander = gtk_expander_new (label);

  gtk_expander_set_expanded (GTK_EXPANDER (expander), TRUE);
  gtk_expander_set_spacing (GTK_EXPANDER (expander), DIALOG_V_SPACING);
  gtk_expander_set_use_markup (GTK_EXPANDER (expander), TRUE);

  gtk_container_add (GTK_CONTAINER (expander), alignment);

  return expander;
}



/*! \brief Dispose
 *
 *  \param [in,out] object This object
 */
static void
dispose (GObject *object)
{
  EditLProp *dialog;
  EditLPropClass *klass;
  GObjectClass *parent_class;

  g_return_if_fail (object != NULL);

  dialog = EDITLPROP (object);

  x_editlprop_set_selection_adapter (dialog, NULL);

  g_slist_foreach (dialog->bindings, (GFunc) g_object_unref, NULL);
  g_slist_free (dialog->bindings);
  dialog->bindings = NULL;

  /* lastly, chain up to the parent dispose */

  klass = EDITLPROP_GET_CLASS (object);
  g_return_if_fail (klass != NULL);
  parent_class = g_type_class_peek_parent (klass);
  g_return_if_fail (parent_class != NULL);
  parent_class->dispose (object);
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



/*! \brief Allow the undo manager to process changes
 *
 *  This function needs to find a new home.
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
handle_undo (EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
  o_undo_savestate_old(w_current, UNDO_ALL);
}



/*! \brief Update the cap style value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_cap_style_model (GtkWidget *widget, EditLProp *dialog)
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
    }
  }
}



/*! \private
 *  \brief Update the value in the cap style widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_cap_style_widget (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    OBJECT_END end = gschem_selection_adapter_get_cap_style (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->line_end),
                                    G_CALLBACK (update_cap_style_model),
                                    dialog);

    x_linecapcb_set_index (dialog->line_end, end);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->line_end),
                                      G_CALLBACK (update_cap_style_model),
                                      dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_end), (end != -1));
  }
}



/*! \private
 *  \brief Update the cap style value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_fill_type_model (GtkWidget *widget, EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->fstylecb != NULL)) {
    int fill_type;

    g_return_if_fail (widget == dialog->fstylecb);

    fill_type = x_fstylecb_get_index (dialog->fstylecb);

    if (fill_type >= 0) {
      gschem_selection_adapter_set_fill_type (dialog->adapter, fill_type);
    }
  }
}



/*! \private
 *  \brief Update the value in the fill type widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_fill_type_widget (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if ((dialog->adapter != NULL) && (dialog->fstylecb != NULL)) {
    int index = gschem_selection_adapter_get_fill_type (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->fstylecb),
                                    G_CALLBACK (update_fill_type_model),
                                    dialog);

    x_fstylecb_set_index(dialog->fstylecb, index);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->fstylecb),
                                      G_CALLBACK (update_fill_type_model),
                                      dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->fstylecb), (index != -1));
  }
}



/*! \brief Update the line type value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_line_type_model (GtkWidget *widget, EditLProp *dialog)
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
    }
  }
}



/*! \private
 *  \brief Update the value in the line type widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_line_type_widget (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    OBJECT_TYPE type = gschem_selection_adapter_get_line_type (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->line_type),
                                    G_CALLBACK (update_line_type_model),
                                    dialog);

    x_linetypecb_set_index (dialog->line_type, type);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->line_type),
                                      G_CALLBACK (update_line_type_model),
                                      dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_type), (type != -1));
  }
}



/*! \brief Update the object color value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_object_color_model (GtkWidget *widget, EditLProp *dialog)
{
  TOPLEVEL *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->parent.w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->colorcb != NULL)) {
    int color;

    g_return_if_fail (widget == dialog->colorcb);

    color = x_colorcb_get_index (dialog->colorcb);

    if (color >= 0) {
      gschem_selection_adapter_set_object_color (dialog->adapter, color);
    }
  }
}



/*! \private
 *  \brief Update the value in the object color widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_object_color_widget (EditLProp *dialog)
{
  g_return_if_fail (dialog != NULL);

  if ((dialog->adapter != NULL) && (dialog->colorcb != NULL)) {
    int color = gschem_selection_adapter_get_object_color (dialog->adapter);

    g_signal_handlers_block_by_func(G_OBJECT (dialog->colorcb),
                                    G_CALLBACK (update_object_color_model),
                                    dialog);

    x_colorcb_set_index(dialog->colorcb, color);

    g_signal_handlers_unblock_by_func(G_OBJECT (dialog->colorcb),
                                      G_CALLBACK (update_object_color_model),
                                      dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->colorcb), (color != -1));
  }
}
