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
 * \brief A dialog box for editing object properties.
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


static void
class_init (EditLPropClass *klass);

static GtkWidget*
create_fill_property_widget (EditLProp *dialog);

static GtkWidget*
create_line_property_widget (EditLProp *dialog);

static GtkWidget*
create_object_property_widget (EditLProp *dialog);

static void
dispose (GObject *object);

static void
geometry_restore (EditLProp *dialog, EdaConfig *cfg, gchar* group_name);

static void
geometry_save (EditLProp *dialog, EdaConfig *cfg, gchar* group_name);

static void
instance_init (EditLProp *dialog);

static void
notify_gschem_toplevel (EditLProp *dialog);

static void
set_selection_adapter (EditLProp *dialog, GschemSelectionAdapter *adapter);

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
      (GClassInitFunc) class_init,
      NULL,                                   /* class_finalize */
      NULL,                                   /* class_data */
      sizeof(EditLProp),
      0,                                      /* n_preallocs */
      (GInstanceInitFunc) instance_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG, "EditLProp", &info, 0);
  }

  return type;
}



/*! \brief Create a new <to be named> dialog
 *
 *  \param [in] w_current The GschemToplevel structure
 */
GtkDialog*
editlprop_new (GschemToplevel *w_current)
{
  return g_object_new (TYPE_EDITLPROP,
                       /* GtkContainer */
                       "border-width",    DIALOG_BORDER_SPACING,
                       /* GtkWindow */
                       "title",           _("Edit Line Width & Type"),
                       "default-width",   320,
                       "default-height",  350,
                       "window-position", GTK_WIN_POS_NONE,
                       "allow-grow",      TRUE,
                       "allow-shrink",    FALSE,
                       "modal",           FALSE,
                       /* GtkDialog */
                       "has-separator",   TRUE,
                       /* GschemDialog */
                       "settings-name",    "line-type",
                       "gschem-toplevel",  w_current,
                       NULL);

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

  gschem_dialog_misc_show_non_modal (w_current,
                                     &(w_current->lpwindow),
                                     editlprop_new);
}



/*! \private
 *  \brief Initialize EditLProp class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klass
 */
static void
class_init (EditLPropClass *klass)
{
  GObjectClass *object_class;

  g_return_if_fail (klass != NULL);

  object_class = G_OBJECT_CLASS (klass);

  g_return_if_fail (object_class != NULL);

  object_class->dispose = dispose;
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
  GtkWidget *label[6];
  GtkWidget *table;
  GtkWidget *widget[6];

  label[0] = gschem_dialog_misc_create_property_label (_("Fill Type:"));
  label[1] = gschem_dialog_misc_create_property_label (_("Line Width:"));
  label[2] = gschem_dialog_misc_create_property_label (_("Angle 1:"));
  label[3] = gschem_dialog_misc_create_property_label (_("Pitch 1:"));
  label[4] = gschem_dialog_misc_create_property_label (_("Angle 2:"));
  label[5] = gschem_dialog_misc_create_property_label (_("Pitch 2:"));

  widget[0] = dialog->fstylecb = x_fstylecb_new ();
  widget[1] = dialog->widthe = gschem_integer_combo_box_new ();
  widget[2] = dialog->angle1e = gschem_integer_combo_box_new ();
  widget[3] = dialog->pitch1e = gschem_integer_combo_box_new ();
  widget[4] = dialog->angle2e = gschem_integer_combo_box_new ();
  widget[5] = dialog->pitch2e = gschem_integer_combo_box_new ();

  table = gschem_dialog_misc_create_property_table (label, widget, 6);

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
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->angle1e)),
                    "activate",
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->pitch1e)),
                    "activate",
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->angle2e)),
                    "activate",
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->pitch2e)),
                    "activate",
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>Fill Properties</b>"), table);
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
  GtkWidget *label[5];
  GtkWidget *table;
  GtkWidget *widget[5];

  label[0] = gschem_dialog_misc_create_property_label (_("Type:"));
  label[1] = gschem_dialog_misc_create_property_label (_("Width:"));
  label[2] = gschem_dialog_misc_create_property_label (_("Dash Length:"));
  label[3] = gschem_dialog_misc_create_property_label (_("Dash Space:"));
  label[4] = gschem_dialog_misc_create_property_label (_("Cap style:"));

  widget[0] = dialog->line_type = x_linetypecb_new ();
  widget[1] = dialog->width_entry = gschem_integer_combo_box_new ();
  widget[2] = dialog->length_entry = gschem_integer_combo_box_new ();
  widget[3] = dialog->space_entry = gschem_integer_combo_box_new ();
  widget[4] = dialog->line_end = x_linecapcb_new ();

  table = gschem_dialog_misc_create_property_table (label, widget, 5);

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
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->length_entry)),
                    "activate",
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->space_entry)),
                    "activate",
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>Line Properties</b>"), table);
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
  GtkWidget *label[1];
  GtkWidget *table;
  GtkWidget *widget[1];

  label[0] = gschem_dialog_misc_create_property_label (_("Color:"));

  widget[0] = dialog->colorcb = x_colorcb_new ();

  table = gschem_dialog_misc_create_property_table (label, widget, 1);

  g_signal_connect(G_OBJECT (dialog->colorcb),
                   "changed",
                   G_CALLBACK (update_object_color_model),
                   dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>Object Properties</b>"), table);
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

  set_selection_adapter (dialog, NULL);

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



/*! \private
 *  \brief Restore widget state
 *
 *  \param [in] dialog     The GschemDialog to restore the position and size of.
 *  \param [in] key_file   The GKeyFile to load the geometry data from.
 *  \param [in] group_name The group name in the key file to find the data under.
 */
static void
geometry_restore (EditLProp *dialog, EdaConfig *cfg, gchar* group_name)
{
  GError *error = NULL;
  gboolean value;

  g_return_if_fail (dialog != NULL);

  /* If any errors occur, use the value already set */

  value = eda_config_get_boolean (cfg,
                                  group_name,
                                  "fill-section-expanded",
                                  &error);

  if (error == NULL) {
    gtk_expander_set_expanded (GTK_EXPANDER (dialog->fill_section_widget), value);
  }

  g_clear_error (&error);

  value = eda_config_get_boolean (cfg,
                                  group_name,
                                  "line-section-expanded",
                                  &error);

  if (error == NULL) {
    gtk_expander_set_expanded (GTK_EXPANDER (dialog->line_section_widget), value);
  }

  g_clear_error (&error);

  value = eda_config_get_boolean (cfg,
                                  group_name,
                                  "object-section-expanded",
                                  &error);

  if (error == NULL) {
    gtk_expander_set_expanded (GTK_EXPANDER (dialog->object_section_widget), value);
  }

  g_clear_error (&error);
}



/*! \private
 *  \brief Save widget state
 *
 *  \param [in] dialog     The GschemDialog to save the position and size of.
 *  \param [in] key_file   The GKeyFile to save the geometry data to.
 *  \param [in] group_name The group name in the key file to store the data under.
 */
static void
geometry_save (EditLProp *dialog, EdaConfig *cfg, gchar* group_name)
{
  g_return_if_fail (dialog != NULL);

  eda_config_set_boolean (cfg,
                          group_name,
                          "fill-section-expanded",
                          gtk_expander_get_expanded (GTK_EXPANDER (dialog->fill_section_widget)));

  eda_config_set_boolean (cfg,
                          group_name,
                          "line-section-expanded",
                          gtk_expander_get_expanded (GTK_EXPANDER (dialog->line_section_widget)));

  eda_config_set_boolean (cfg,
                          group_name,
                          "object-section-expanded",
                          gtk_expander_get_expanded (GTK_EXPANDER (dialog->object_section_widget)));
}



/*! \private
 *  \brief Initialize EditLProp instance
 *
 *  \param [in,out] dialog The edit text dialog
 */
static void
instance_init (EditLProp *dialog)
{
  GtkWidget *vbox;

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CLOSE,
                         GTK_RESPONSE_CLOSE);

  g_signal_connect (G_OBJECT (dialog),
                    "notify::gschem-toplevel",
                    G_CALLBACK (notify_gschem_toplevel),
                    NULL);

  g_signal_connect (G_OBJECT (dialog),
                    "geometry-restore",
                    G_CALLBACK (geometry_restore),
                    NULL);

  g_signal_connect (G_OBJECT (dialog),
                    "geometry-save",
                    G_CALLBACK (geometry_save),
                    NULL);

  vbox = GTK_DIALOG (dialog)->vbox;
  gtk_box_set_spacing (GTK_BOX (vbox), DIALOG_V_SPACING);

  dialog->object_section_widget = create_object_property_widget (dialog);
  dialog->line_section_widget   = create_line_property_widget (dialog);
  dialog->fill_section_widget   = create_fill_property_widget (dialog);

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      dialog->object_section_widget,           /* child   */
                      FALSE,                                   /* expand  */
                      FALSE,                                   /* fill    */
                      0);                                      /* padding */

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      dialog->line_section_widget,             /* child   */
                      FALSE,                                   /* expand  */
                      FALSE,                                   /* fill    */
                      0);                                      /* padding */

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      dialog->fill_section_widget,             /* child   */
                      FALSE,                                   /* expand  */
                      FALSE,                                   /* fill    */
                      0);                                      /* padding */
}



/*! \private
 *  \brief Property change notification from the base class
 *
 *  Handles property change notification from the base class to update members
 *  and signal handlers in the this derived class.
 *
 *  \param [in,out] dialog    This dialog
 *  \param [in]     selection The selection to manipulate
 */
static void
notify_gschem_toplevel (EditLProp *dialog)
{
    GschemToplevel *w_current;

    g_return_if_fail (dialog != NULL);

    g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

    gschem_integer_combo_box_set_model (dialog->width_entry,
                                        gschem_toplevel_get_line_width_list_store (w_current));

    gschem_integer_combo_box_set_model (dialog->length_entry,
                                        gschem_toplevel_get_dash_length_list_store (w_current));

    gschem_integer_combo_box_set_model (dialog->space_entry,
                                        gschem_toplevel_get_dash_space_list_store (w_current));

    gschem_integer_combo_box_set_model (dialog->widthe,
                                        gschem_toplevel_get_fill_width_list_store (w_current));

    gschem_integer_combo_box_set_model (dialog->angle1e,
                                        gschem_toplevel_get_fill_angle_list_store (w_current));

    gschem_integer_combo_box_set_model (dialog->pitch1e,
                                        gschem_toplevel_get_fill_pitch_list_store (w_current));

    gschem_integer_combo_box_set_model (dialog->angle2e,
                                        gschem_toplevel_get_fill_angle_list_store (w_current));

    gschem_integer_combo_box_set_model (dialog->pitch2e,
                                        gschem_toplevel_get_fill_pitch_list_store (w_current));

    set_selection_adapter (dialog,
                           gschem_toplevel_get_selection_adapter (w_current));
}



/*! \private
 *  \brief Set the selection that this dialog manipulates
 *
 *  \param [in,out] dialog    This dialog
 *  \param [in]     selection The selection to manipulate
 */
static void
set_selection_adapter (EditLProp *dialog, GschemSelectionAdapter *adapter)
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

    g_object_unref (dialog->adapter);
  }

  dialog->adapter = adapter;

  g_slist_foreach (dialog->bindings,
                   (GFunc) gschem_binding_set_model_object,
                   adapter);

  if (dialog->adapter != NULL) {
    g_object_ref (dialog->adapter);

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
  g_return_if_fail (dialog->line_end != NULL);

  if (dialog->adapter != NULL) {
    OBJECT_END end = gschem_selection_adapter_get_cap_style (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->line_end),
                                     G_CALLBACK (update_cap_style_model),
                                     dialog);

    x_linecapcb_set_index (dialog->line_end, end);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->line_end),
                                       G_CALLBACK (update_cap_style_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_end), (end != NO_SELECTION));
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
  g_return_if_fail (dialog->fstylecb != NULL);

  if (dialog->adapter != NULL) {
    int index = gschem_selection_adapter_get_fill_type (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->fstylecb),
                                     G_CALLBACK (update_fill_type_model),
                                     dialog);

    x_fstylecb_set_index (dialog->fstylecb, index);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->fstylecb),
                                       G_CALLBACK (update_fill_type_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->fstylecb), (index != NO_SELECTION));
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
  g_return_if_fail (dialog->line_type != NULL);

  if (dialog->adapter != NULL) {
    OBJECT_TYPE type = gschem_selection_adapter_get_line_type (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->line_type),
                                     G_CALLBACK (update_line_type_model),
                                     dialog);

    x_linetypecb_set_index (dialog->line_type, type);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->line_type),
                                       G_CALLBACK (update_line_type_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_type), (type != NO_SELECTION));
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
  g_return_if_fail (dialog->colorcb != NULL);

  if (dialog->adapter != NULL) {
    int color = gschem_selection_adapter_get_object_color (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->colorcb),
                                     G_CALLBACK (update_object_color_model),
                                     dialog);

    x_colorcb_set_index (dialog->colorcb, color);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->colorcb),
                                       G_CALLBACK (update_object_color_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->colorcb), (color != NO_SELECTION));
  }
}
