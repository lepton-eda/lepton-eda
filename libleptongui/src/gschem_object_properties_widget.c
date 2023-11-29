/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
 * \file gschem_object_properties_widget.c
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


enum
{
  PROP_0,
  PROP_GSCHEM_TOPLEVEL
};

G_DEFINE_TYPE (GschemObjectPropertiesWidget, gschem_object_properties_widget, GSCHEM_TYPE_BIN);

static GtkWidget*
create_fill_property_widget (GschemObjectPropertiesWidget *dialog);

static GtkWidget*
create_general_property_widget (GschemObjectPropertiesWidget *dialog);

static GtkWidget*
create_line_property_widget (GschemObjectPropertiesWidget *dialog);

static GtkWidget*
create_pin_property_widget (GschemObjectPropertiesWidget *dialog);

static void
dispose (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
notify_gschem_toplevel (GschemObjectPropertiesWidget *dialog);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
set_selection_adapter (GschemObjectPropertiesWidget *dialog, GschemSelectionAdapter *adapter);

static void
update_cap_style_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog);

static void
update_cap_style_widget (GschemObjectPropertiesWidget *dialog);

static void
update_fill_type_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog);

static void
update_fill_type_widget (GschemObjectPropertiesWidget *dialog);

static void
update_line_type_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog);

static void
update_line_type_widget (GschemObjectPropertiesWidget *dialog);

static void
update_object_color_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog);

static void
update_object_color_widget (GschemObjectPropertiesWidget *dialog);

static void
update_pin_type_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog);

static void
update_pin_type_widget (GschemObjectPropertiesWidget *dialog);


/*! \brief Create a new Object properties dialog
 *
 *  \param [in] w_current The GschemToplevel structure
 */
GtkWidget*
gschem_object_properties_widget_new (GschemToplevel *w_current)
{
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_OBJECT_PROPERTIES_WIDGET,
                                   /* GschemObjectProperties */
                                   "gschem-toplevel",  w_current,
                                   NULL));
}



/*! \private
 *  \brief Initialize GschemObjectPropertiesWidget class
 *
 *  \par Function Description
 *
 *  GType class initialiser for Multiattrib. We override our parent
 *  virtual class methods as needed and register our GObject properties.
 *
 *  \param [in] klass
 */
static void
gschem_object_properties_widget_class_init (GschemObjectPropertiesWidgetClass *klass)
{
  GObjectClass *object_class;

  g_return_if_fail (klass != NULL);

  object_class = G_OBJECT_CLASS (klass);

  g_return_if_fail (object_class != NULL);

  object_class->dispose = dispose;

  object_class->get_property = get_property;
  object_class->set_property = set_property;

  g_object_class_install_property (
    object_class,
    PROP_GSCHEM_TOPLEVEL,
    g_param_spec_pointer ("gschem-toplevel",
                          "",
                          "",
                          (GParamFlags) (G_PARAM_CONSTRUCT_ONLY
                                         | G_PARAM_READWRITE)));
}



/*! \private
 *  \brief Create a fill property section widget
 *
 *  \param [in] dialog
 *  \return A new fill property section widget
 */
static GtkWidget*
create_fill_property_widget (GschemObjectPropertiesWidget *dialog)
{
  GtkWidget *label[6];
  GtkWidget *table;
  GtkWidget *widget[6];

  label[0] = gschem_dialog_misc_create_property_label (_("_Fill Type:"));
  label[1] = gschem_dialog_misc_create_property_label (_("Lin_e Width:"));
  label[2] = gschem_dialog_misc_create_property_label (_("Angle _1:"));
  label[3] = gschem_dialog_misc_create_property_label (_("P_itch 1:"));
  label[4] = gschem_dialog_misc_create_property_label (_("Angle _2:"));
  label[5] = gschem_dialog_misc_create_property_label (_("Pitc_h 2:"));

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
 *  \brief Create a general property section widget
 *
 *  \param [in] dialog
 *  \return A new general property section widget
 */
static GtkWidget*
create_general_property_widget (GschemObjectPropertiesWidget *dialog)
{
  GtkWidget *label[1];
  GtkWidget *table;
  GtkWidget *widget[1];

  label[0] = gschem_dialog_misc_create_property_label (_("Colo_r:"));

  widget[0] = dialog->colorcb = x_colorcb_new ();

  table = gschem_dialog_misc_create_property_table (label, widget, 1);

  g_signal_connect(G_OBJECT (dialog->colorcb),
                   "changed",
                   G_CALLBACK (update_object_color_model),
                   dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>General Properties</b>"), table);
}



/*! \private
 *  \brief Create a line property section widget
 *
 *  \param [in] dialog
 *  \return A new line property section widget
 */
static GtkWidget*
create_line_property_widget (GschemObjectPropertiesWidget *dialog)
{
  GtkWidget *label[5];
  GtkWidget *table;
  GtkWidget *widget[5];

  label[0] = gschem_dialog_misc_create_property_label (_("_Type:"));
  label[1] = gschem_dialog_misc_create_property_label (_("_Width:"));
  label[2] = gschem_dialog_misc_create_property_label (_("Dash _Length:"));
  label[3] = gschem_dialog_misc_create_property_label (_("Dash _Space:"));
  label[4] = gschem_dialog_misc_create_property_label (_("C_ap style:"));

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
 *  \brief Create a pin property section widget
 *
 *  \param [in] dialog
 *  \return A new pin property section widget
 */
static GtkWidget*
create_pin_property_widget (GschemObjectPropertiesWidget *dialog)
{
  GtkWidget *label[1];
  GtkWidget *table;
  GtkWidget *widget[1];

  label[0] = gschem_dialog_misc_create_property_label (_("_Pin Type:"));

  widget[0] = dialog->pin_type = gschem_pin_type_combo_new ();

  table = gschem_dialog_misc_create_property_table (label, widget, 1);

  g_signal_connect (G_OBJECT (dialog->pin_type),
                    "changed",
                    G_CALLBACK (update_pin_type_model),
                    dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>Pin Properties</b>"), table);
}



/*! \brief Dispose
 *
 *  \param [in,out] object This object
 */
static void
dispose (GObject *object)
{
  GschemObjectPropertiesWidget *dialog;
  GschemObjectPropertiesWidgetClass *klass;
  GObjectClass *parent_class;

  g_return_if_fail (object != NULL);

  dialog = GSCHEM_OBJECT_PROPERTIES_WIDGET (object);

  set_selection_adapter (dialog, NULL);

  g_slist_foreach (dialog->bindings, (GFunc) g_object_unref, NULL);
  g_slist_free (dialog->bindings);
  dialog->bindings = NULL;

  /* lastly, chain up to the parent dispose */

  klass = GSCHEM_OBJECT_PROPERTIES_WIDGET_GET_CLASS (object);
  g_return_if_fail (klass != NULL);
  parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));
  g_return_if_fail (parent_class != NULL);
  parent_class->dispose (object);
}



/*! \brief Get a property
 */
static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec)
{
  GschemObjectPropertiesWidget *widget = GSCHEM_OBJECT_PROPERTIES_WIDGET (object);

  switch (param_id) {
    case PROP_GSCHEM_TOPLEVEL:
      g_value_set_pointer (value, widget->w_current);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \private
 *  \brief Initialize GschemObjectPropertiesWidget instance
 *
 *  \param [in,out] dialog The edit text dialog
 */
static void
gschem_object_properties_widget_init (GschemObjectPropertiesWidget *dialog)
{
  GtkWidget *scrolled;
  GtkWidget *vbox;
  GtkWidget *viewport;

  g_signal_connect (G_OBJECT (dialog),
                    "notify::gschem-toplevel",
                    G_CALLBACK (notify_gschem_toplevel),
                    NULL);

  scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (dialog), scrolled);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                  GTK_POLICY_NEVER,
                                  GTK_POLICY_AUTOMATIC);

  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled),
                                       GTK_SHADOW_NONE);

  viewport = gtk_viewport_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (scrolled), viewport);

  gtk_viewport_set_shadow_type (GTK_VIEWPORT (viewport),
                                GTK_SHADOW_NONE);

#ifdef ENABLE_GTK3
  vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, DIALOG_V_SPACING);
#else
  vbox = gtk_vbox_new (FALSE, DIALOG_V_SPACING);
#endif
  gtk_container_add (GTK_CONTAINER (viewport), vbox);

  dialog->general_section_widget = create_general_property_widget (dialog);
  dialog->line_section_widget    = create_line_property_widget (dialog);
  dialog->fill_section_widget    = create_fill_property_widget (dialog);
  dialog->pin_section_widget     = create_pin_property_widget (dialog);

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      dialog->general_section_widget,          /* child   */
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

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      dialog->pin_section_widget,              /* child   */
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
notify_gschem_toplevel (GschemObjectPropertiesWidget *dialog)
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
set_selection_adapter (GschemObjectPropertiesWidget *dialog, GschemSelectionAdapter *adapter)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          (gpointer) update_pin_type_widget,
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          (gpointer) update_object_color_widget,
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          (gpointer) update_line_type_widget,
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          (gpointer) update_fill_type_widget,
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          (gpointer) update_cap_style_widget,
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

    g_signal_connect_swapped (dialog->adapter,
                              "notify::pin-type",
                              G_CALLBACK (update_pin_type_widget),
                              dialog);
  }

  update_cap_style_widget (dialog);
  update_fill_type_widget (dialog);
  update_line_type_widget (dialog);
  update_object_color_widget (dialog);
  update_pin_type_widget (dialog);
}



/*! \brief Update the cap style value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_cap_style_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog)
{
  LeptonToplevel *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->w_current;
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
update_cap_style_widget (GschemObjectPropertiesWidget *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->line_end != NULL);

  if (dialog->adapter != NULL) {
    int end = gschem_selection_adapter_get_cap_style (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->line_end),
                                     (gpointer) update_cap_style_model,
                                     dialog);

    x_linecapcb_set_index (dialog->line_end, end);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->line_end),
                                       (gpointer) update_cap_style_model,
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_end), end != NO_SELECTION);
  }
}



/*! \private
 *  \brief Update the cap style value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_fill_type_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog)
{
  LeptonToplevel *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->w_current;
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
update_fill_type_widget (GschemObjectPropertiesWidget *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->fstylecb != NULL);

  if (dialog->adapter != NULL) {
    int index = gschem_selection_adapter_get_fill_type (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->fstylecb),
                                     (gpointer) update_fill_type_model,
                                     dialog);

    x_fstylecb_set_index (dialog->fstylecb, index);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->fstylecb),
                                       (gpointer) update_fill_type_model,
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
update_line_type_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog)
{
  LeptonToplevel *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->w_current;
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
update_line_type_widget (GschemObjectPropertiesWidget *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->line_type != NULL);

  if (dialog->adapter != NULL) {
    int type = gschem_selection_adapter_get_line_type (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->line_type),
                                     (gpointer) update_line_type_model,
                                     dialog);

    x_linetypecb_set_index (dialog->line_type, type);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->line_type),
                                       (gpointer) update_line_type_model,
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->line_type), type != NO_SELECTION);
  }
}



/*! \brief Update the object color value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_object_color_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog)
{
  LeptonToplevel *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->w_current;
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
update_object_color_widget (GschemObjectPropertiesWidget *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->colorcb != NULL);

  if (dialog->adapter != NULL) {
    int color = gschem_selection_adapter_get_object_color (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->colorcb),
                                     (gpointer) update_object_color_model,
                                     dialog);

    x_colorcb_set_index (dialog->colorcb, color);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->colorcb),
                                       (gpointer) update_object_color_model,
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->colorcb), (color != NO_SELECTION));
  }
}


/*! \brief Update the pin type value in the model
 *
 *  \param [in] widget The widget emitting the event
 *  \param [in] dialog The line properties dialog box
 */
static void
update_pin_type_model (GtkWidget *widget, GschemObjectPropertiesWidget *dialog)
{
  LeptonToplevel *toplevel;
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);
  g_return_if_fail (widget != NULL);

  w_current = dialog->w_current;
  g_return_if_fail (w_current != NULL);

  toplevel = gschem_toplevel_get_toplevel (w_current);
  g_return_if_fail (toplevel != NULL);

  if ((dialog->adapter != NULL) && (dialog->pin_type != NULL)) {
    int pin_type;

    g_return_if_fail (widget == dialog->pin_type);

    pin_type = gschem_pin_type_combo_get_index (dialog->pin_type);

    if (pin_type >= 0) {
      gschem_selection_adapter_set_pin_type (dialog->adapter, pin_type);
    }
  }
}



/*! \private
 *  \brief Update the value in the pin type widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_pin_type_widget (GschemObjectPropertiesWidget *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->pin_type != NULL);

  if (dialog->adapter != NULL) {
    int type = gschem_selection_adapter_get_pin_type (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->pin_type),
                                     (gpointer) update_pin_type_model,
                                     dialog);

    gschem_pin_type_combo_set_index (dialog->pin_type, type);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->pin_type),
                                       (gpointer) update_pin_type_model,
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->pin_type), (type != NO_SELECTION));
  }
}


/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemObjectPropertiesWidget *widget = GSCHEM_OBJECT_PROPERTIES_WIDGET (object);

  switch (param_id) {
    case PROP_GSCHEM_TOPLEVEL:
      widget->w_current = GSCHEM_TOPLEVEL (g_value_get_pointer (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
