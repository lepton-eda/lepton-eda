/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
 * \file gschem_options_widget.c
 *
 * \brief A widget for editing options
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


enum
{
  PROP_0,
  PROP_GSCHEM_TOPLEVEL
};


G_DEFINE_TYPE (GschemOptionsWidget, gschem_options_widget, GSCHEM_TYPE_BIN);

GtkWidget*
create_grid_mode_widget (GschemOptionsWidget *widget);

GtkWidget*
create_snap_mode_widget (GschemOptionsWidget *widget);

static GtkWidget*
create_net_section (GschemOptionsWidget *widget);

static GtkWidget*
create_snap_section (GschemOptionsWidget *widget);

static void
dispose (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
notify_gschem_toplevel (GschemOptionsWidget *widget);

static void
set_options (GschemOptionsWidget *widget, GschemOptions *options);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
update_grid_mode_model (GschemOptionsWidget *widget, GtkWidget *button);

static void
update_grid_mode_widget (GschemOptionsWidget *widget);

static void
update_magnetic_net_mode_model (GschemOptionsWidget *widget);

static void
update_magnetic_net_mode_widget (GschemOptionsWidget *widget);

static void
update_net_rubber_band_mode_model (GschemOptionsWidget *widget);

static void
update_net_rubber_band_mode_widget (GschemOptionsWidget *widget);

static void
update_snap_mode_model (GschemOptionsWidget *widget, GtkWidget *button);

static void
update_snap_mode_widget (GschemOptionsWidget *widget);

static void
update_snap_size_model (GschemOptionsWidget *widget);

static void
update_snap_size_widget (GschemOptionsWidget *widget);



/*! \brief Adjust widget focus for the convienence of the user
 *
 *  \param [in] widget This options widget
 */
void
gschem_options_widget_adjust_focus (GschemOptionsWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->snap_size != NULL);

  /* Setting the focus to the snap size widget allows a subsequent keyboard
   * fumble to mess up the snap size. For now, it will not get focus.
   *
   * gtk_widget_grab_focus (widget->snap_size);
   */
}



/*! \brief Create a new options widget
 *
 *  \param [in] w_current The GschemToplevel structure
 */
GtkWidget*
gschem_options_widget_new (GschemToplevel *w_current)
{
  return GTK_WIDGET (g_object_new (GSCHEM_TYPE_OPTIONS_WIDGET,
                                   "gschem-toplevel",  w_current,
                                   NULL));
}



/*! \private
 *  \brief Initialize the options widget class structure
 *
 *  \param [in] klass
 */
static void
gschem_options_widget_class_init (GschemOptionsWidgetClass *klass)
{
  GObjectClass *object_class;

  g_return_if_fail (klass != NULL);

  object_class = G_OBJECT_CLASS (klass);

  g_return_if_fail (object_class != NULL);

  object_class->dispose = dispose;

  object_class->get_property = get_property;
  object_class->set_property = set_property;

  g_object_class_install_property (object_class,
                                   PROP_GSCHEM_TOPLEVEL,
                                   g_param_spec_pointer ("gschem-toplevel",
                                                         "",
                                                         "",
                                                         (GParamFlags) (G_PARAM_CONSTRUCT_ONLY
                                                                        | G_PARAM_READWRITE)));
}



/*! \private
 *  \brief Create the series of buttons that make the grid mode selection widget
 *
 *  \param [in] widget
 *  \return The grid mode widget
 */
GtkWidget*
create_grid_mode_widget (GschemOptionsWidget *widget)
{
  GtkWidget *box;
  int index;

  g_return_val_if_fail (widget != NULL, NULL);

#ifdef ENABLE_GTK3
  box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  box = gtk_hbox_new (FALSE, FALSE);
#endif

  for (index=0; index<GRID_MODE_COUNT; index++) {
    widget->grid_radio[index] = gtk_toggle_button_new_with_mnemonic (NULL);

    gtk_box_pack_start (GTK_BOX (box),                           /* box     */
                        widget->grid_radio[index],               /* child   */
                        FALSE,                                   /* expand  */
                        FALSE,                                   /* fill    */
                        0);                                      /* padding */

    gtk_size_group_add_widget (widget->size_group, widget->grid_radio[index]);

    g_signal_connect_swapped (G_OBJECT (widget->grid_radio[index]),
                              "clicked",
                              G_CALLBACK (update_grid_mode_model),
                              widget);
  }

  gtk_button_set_label (GTK_BUTTON (widget->grid_radio[GRID_MODE_NONE]),
                        _("_Off"));

  gtk_button_set_label (GTK_BUTTON (widget->grid_radio[GRID_MODE_DOTS]),
                        _("_Dots"));

  gtk_button_set_label (GTK_BUTTON (widget->grid_radio[GRID_MODE_MESH]),
                        _("M_esh"));

  return box;
}



/*! \private
 *  \brief Create section with net tool settings
 *
 *  \param [in] widget
 *  \return The net section widget
 */
static GtkWidget*
create_net_section (GschemOptionsWidget *widget)
{
  GtkWidget *label[2];
  GtkWidget *table;
  GtkWidget *editor[2];

  /* These widgets are shown in the same order as the options menu */

  label[0] = gschem_dialog_misc_create_property_label (_("Net R_ubber Band Mode:"));
  label[1] = gschem_dialog_misc_create_property_label (_("_Magnetic Net Mode:"));

  /*! \todo These should become a GtkSwitch when updating to GTK 3.0 */

  editor[0] = widget->net_rubber_band_widget = gtk_check_button_new_with_label (_("Enabled"));
  editor[1] = widget->magnetic_net_widget = gtk_check_button_new_with_label (_("Enabled"));

  table = gschem_dialog_misc_create_property_table (label, editor, 2);

  g_signal_connect_swapped (G_OBJECT (widget->magnetic_net_widget),
                            "toggled",
                            G_CALLBACK (update_magnetic_net_mode_model),
                            widget);

  g_signal_connect_swapped (G_OBJECT (widget->net_rubber_band_widget),
                            "toggled",
                            G_CALLBACK (update_net_rubber_band_mode_model),
                            widget);

  return gschem_dialog_misc_create_section_widget (_("<b>Net Options</b>"), table);
}


/*! \private
 *  \brief Create section with snap and grid settings
 *
 *  \param [in] widget
 *  \return The snap section widget
 */
static GtkWidget*
create_snap_section (GschemOptionsWidget *widget)
{
  GtkWidget *label[3];
  GtkWidget *table;
  GtkWidget *editor[3];

  label[0] = gschem_dialog_misc_create_property_label (_("Grid Mode:"));
  label[1] = gschem_dialog_misc_create_property_label (_("Snap Mode:"));
  label[2] = gschem_dialog_misc_create_property_label (_("_Snap Size:"));

  editor[0] = create_grid_mode_widget (widget);
  editor[1] = create_snap_mode_widget (widget);
  editor[2] = widget->snap_size = gtk_spin_button_new_with_range (MINIMUM_SNAP_SIZE,
                                                                  MAXIMUM_SNAP_SIZE,
                                                                  5);

  table = gschem_dialog_misc_create_property_table (label, editor, 3);

  // gtk_editable_select_region (GTK_EDITABLE(spin_size), 0, -1);

  g_signal_connect_swapped (G_OBJECT (widget->snap_size),
                            "value-changed",
                            G_CALLBACK (update_snap_size_model),
                            widget);

  return gschem_dialog_misc_create_section_widget (_("<b>Snap Options</b>"), table);
}



/*! \private
 *  \brief Create the series of buttons that make the snap mode selection widget
 *
 *  \param [in] widget
 *  \return The snap mode widget
 */
GtkWidget*
create_snap_mode_widget (GschemOptionsWidget *widget)
{
  GtkWidget *box;
  int index;

  g_return_val_if_fail (widget != NULL, NULL);

#ifdef ENABLE_GTK3
  box = gtk_box_new (GTK_ORIENTATION_HORIZONTAL, 0);
#else
  box = gtk_hbox_new (FALSE, FALSE);
#endif

  for (index = 0; index < SNAP_MODE_COUNT; index++)
  {
    widget->snap_radio[index] = gtk_toggle_button_new_with_mnemonic (NULL);

    gtk_box_pack_start (GTK_BOX (box),                           /* box     */
                        widget->snap_radio[index],               /* child   */
                        FALSE,                                   /* expand  */
                        FALSE,                                   /* fill    */
                        0);                                      /* padding */

    gtk_size_group_add_widget (widget->size_group, widget->snap_radio[index]);

    g_signal_connect_swapped (G_OBJECT (widget->snap_radio[index]),
                              "clicked",
                              G_CALLBACK (update_snap_mode_model),
                              widget);
  }

  gtk_button_set_label (GTK_BUTTON (widget->snap_radio[SNAP_OFF]),
                        _("O_ff"));

  gtk_button_set_label (GTK_BUTTON (widget->snap_radio[SNAP_GRID]),
                        _("_Grid"));

  gtk_button_set_label (GTK_BUTTON (widget->snap_radio[SNAP_RESNAP]),
                        _("_Resnap"));

  return box;
}


/*! \private
 *  \brief Dispose
 *
 *  \param [in] object The options widget to dispose
 */
static void
dispose (GObject *object)
{
  GschemOptionsWidget *widget;
  GschemOptionsWidgetClass *klass;
  GObjectClass *parent_class;

  g_return_if_fail (object != NULL);

  widget = GSCHEM_OPTIONS_WIDGET (object);

  set_options (widget, NULL);

  //g_slist_foreach (widget->bindings, (GFunc) g_object_unref, NULL);
  //g_slist_free (widget->bindings);
  //widget->bindings = NULL;

  /* lastly, chain up to the parent dispose */

  klass = GSCHEM_OPTIONS_WIDGET_GET_CLASS (object);
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
  GschemOptionsWidget *widget = GSCHEM_OPTIONS_WIDGET (object);

  switch (param_id) {
    case PROP_GSCHEM_TOPLEVEL:
      g_value_set_pointer (value, widget->w_current);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}

/*! \private
 *  \brief Initialize an options widget instance
 *
 *  \param [in,out] widget The text property widget
 */
static void
gschem_options_widget_init (GschemOptionsWidget *widget)
{
  GtkWidget *vbox;

  g_signal_connect (G_OBJECT (widget),
                    "notify::gschem-toplevel",
                    G_CALLBACK (notify_gschem_toplevel),
                    NULL);

  gtk_container_set_border_width (GTK_CONTAINER (widget),
                                  DIALOG_BORDER_SPACING);

  widget->size_group = gtk_size_group_new (GTK_SIZE_GROUP_BOTH);

#ifdef ENABLE_GTK3
  vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, DIALOG_V_SPACING);
#else
  vbox = gtk_vbox_new (FALSE, DIALOG_V_SPACING);
#endif
  gtk_container_add (GTK_CONTAINER (widget), vbox);

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_snap_section (widget),            /* child   */
                      FALSE,                                   /* expand  */
                      FALSE,                                   /* fill    */
                      0);                                      /* padding */

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_net_section (widget),             /* child   */
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
 *  \param [in,out] widget    This widget
 */
static void
notify_gschem_toplevel (GschemOptionsWidget *widget)
{
  GschemToplevel *w_current;

  g_return_if_fail (widget != NULL);

  g_object_get (widget, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  set_options (widget, w_current->options);
}



/*! \private
 *  \brief Set the options that this widget manipulates
 *
 *  \param [in,out] widget   This widget
 *  \param [in]     options  The options to manipulate
 */
static void
set_options (GschemOptionsWidget *widget, GschemOptions *options)
{
  if (widget->options != NULL) {
    g_signal_handlers_disconnect_by_func (widget->options,
                                          (gpointer) update_snap_size_widget,
                                          widget);

    g_signal_handlers_disconnect_by_func (widget->options,
                                          (gpointer) update_snap_mode_widget,
                                          widget);

    g_signal_handlers_disconnect_by_func (widget->options,
                                          (gpointer) update_net_rubber_band_mode_widget,
                                          widget);

    g_signal_handlers_disconnect_by_func (widget->options,
                                          (gpointer) update_magnetic_net_mode_widget,
                                          widget);

    g_signal_handlers_disconnect_by_func (widget->options,
                                          (gpointer) update_grid_mode_widget,
                                          widget);

    g_object_unref (widget->options);
  }

  widget->options = options;

  if (widget->options != NULL) {
    g_object_ref (widget->options);

    g_signal_connect_swapped (widget->options,
                              "notify::grid-mode",
                              G_CALLBACK (update_grid_mode_widget),
                              widget);

    g_signal_connect_swapped (widget->options,
                              "notify::magnetic-net-mode",
                              G_CALLBACK (update_magnetic_net_mode_widget),
                              widget);

    g_signal_connect_swapped (widget->options,
                              "notify::net-rubber-band-mode",
                              G_CALLBACK (update_net_rubber_band_mode_widget),
                              widget);

    g_signal_connect_swapped (widget->options,
                              "notify::snap-mode",
                              G_CALLBACK (update_snap_mode_widget),
                              widget);

    g_signal_connect_swapped (widget->options,
                              "notify::snap-size",
                              G_CALLBACK (update_snap_size_widget),
                              widget);
  }

  update_grid_mode_widget (widget);
  update_magnetic_net_mode_widget (widget);
  update_net_rubber_band_mode_widget (widget);
  update_snap_mode_widget (widget);
  update_snap_size_widget (widget);
}


/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemOptionsWidget *widget = GSCHEM_OPTIONS_WIDGET (object);

  switch (param_id) {
    case PROP_GSCHEM_TOPLEVEL:
      widget->w_current = GSCHEM_TOPLEVEL (g_value_get_pointer (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \private
 *  \brief Update the grid mode in the model
 *
 *  \param [in,out] widget This widget
 *  \param [in] button The radio button determining the grid mode.
 */
static void
update_grid_mode_model (GschemOptionsWidget *widget, GtkWidget *button)
{
  g_return_if_fail (widget != NULL);

  if (widget->options != NULL) {
    int index;

    for (index = 0; index < GRID_MODE_COUNT; index++) {
      if (widget->grid_radio[index] == button) {
        gschem_options_set_grid_mode (widget->options, (SchematicGridMode) index);
        break;
      }
    }
  }
}



/*! \private
 *  \brief Update the grid mode widget with the current value
 *
 *  \param [in,out] widget This widget
 */
static void
update_grid_mode_widget (GschemOptionsWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (widget->options != NULL) {
    SchematicGridMode grid_mode;
    int index;

    grid_mode = gschem_options_get_grid_mode (widget->options);

    for (index=0; index<GRID_MODE_COUNT; index++) {
      g_signal_handlers_block_by_func (G_OBJECT (widget->grid_radio[index]),
                                       (gpointer) update_grid_mode_model,
                                       widget);
    }

    for (index=0; index<GRID_MODE_COUNT; index++) {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget->grid_radio[index]),
                                    (grid_mode == index));
    }

    for (index=0; index<GRID_MODE_COUNT; index++) {
      g_signal_handlers_unblock_by_func (G_OBJECT (widget->grid_radio[index]),
                                         (gpointer) update_grid_mode_model,
                                         widget);
    }
  }
}



/*! \private
 *  \brief Update the magnetic net mode in the model
 *
 *  \param [in,out] widget This widget
 */
static void
update_magnetic_net_mode_model (GschemOptionsWidget *widget)
{
  GschemToplevel *w_current;

  g_return_if_fail (widget != NULL);

  g_object_get (widget, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  gschem_options_set_magnetic_net_mode (w_current->options,
                                        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget->magnetic_net_widget)));

  if (w_current->bottom_widget != NULL)
  {
    i_update_net_options_status (w_current);
  }
}



/*! \private
 *  \brief Update the net rubber band mode widget with the current value
 *
 *  \param [in,out] widget This widget
 */
static void
update_magnetic_net_mode_widget (GschemOptionsWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (widget->options != NULL) {
    GschemToplevel *w_current;

    g_object_get (widget, "gschem-toplevel", &w_current, NULL);

    g_return_if_fail (w_current != NULL);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget->magnetic_net_widget),
                                  gschem_options_get_magnetic_net_mode (w_current->options));
  }
}


/*! \private
 *  \brief Update the net rubber band mode in the model
 *
 *  \param [in,out] widget This widget
 */
static void
update_net_rubber_band_mode_model (GschemOptionsWidget *widget)
{
  GschemToplevel *w_current;

  g_return_if_fail (widget != NULL);

  g_object_get (widget, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  gschem_options_set_net_rubber_band_mode (w_current->options,
                                           gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (widget->net_rubber_band_widget)));

  if (w_current->bottom_widget != NULL)
  {
    i_update_net_options_status (w_current);
  }
}



/*! \private
 *  \brief Update the net rubber band mode widget with the current value
 *
 *  \param [in,out] widget This widget
 */
static void
update_net_rubber_band_mode_widget (GschemOptionsWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (widget->options != NULL) {
    GschemToplevel *w_current;

    g_object_get (widget, "gschem-toplevel", &w_current, NULL);

    g_return_if_fail (w_current != NULL);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget->net_rubber_band_widget),
                                  gschem_options_get_net_rubber_band_mode (w_current->options));
  }
}



/*! \private
 *  \brief Update the snap mode in the model
 *
 *  \param [in,out] widget This widget
 *  \param [in] button The radio button determining the snap mode.
 */
static void
update_snap_mode_model (GschemOptionsWidget *widget, GtkWidget *button)
{
  g_return_if_fail (widget != NULL);

  if (widget->options != NULL) {
    int index;

    for (index = 0; index < SNAP_MODE_COUNT; index++)
    {
      if (widget->snap_radio[index] == button) {
        gschem_options_set_snap_mode (widget->options, (SchematicSnapMode) index);
        break;
      }
    }
  }
}



/*! \private
 *  \brief Update the snap mode widget with the current value
 *
 *  \param [in,out] widget This widget
 */
static void
update_snap_mode_widget (GschemOptionsWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (widget->options != NULL) {
    int index;
    SchematicSnapMode snap_mode;

    snap_mode = gschem_options_get_snap_mode (widget->options);

    for (index = 0; index < SNAP_MODE_COUNT; index++)
    {
      g_signal_handlers_block_by_func (G_OBJECT (widget->snap_radio[index]),
                                       (gpointer) update_snap_mode_model,
                                       widget);
    }

    for (index = 0; index < SNAP_MODE_COUNT; index++)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (widget->snap_radio[index]),
                                    (snap_mode == index));
  }

    for (index = 0; index < SNAP_MODE_COUNT; index++)
    {
      g_signal_handlers_unblock_by_func (G_OBJECT (widget->snap_radio[index]),
                                         (gpointer) update_snap_mode_model,
                                         widget);
    }
  }
}



/*! \private
 *  \brief Update the snap size in the model
 *
 *  \param [in,out] widget This widget
 */
static void
update_snap_size_model (GschemOptionsWidget *widget)
{
  GschemToplevel *w_current;

  g_return_if_fail (widget != NULL);

  g_object_get (widget, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  gschem_options_set_snap_size (w_current->options,
                                gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (widget->snap_size)));
}



/*! \private
 *  \brief Update the snap size widget with the current value
 *
 *  \param [in,out] widget This widget
 */
static void
update_snap_size_widget (GschemOptionsWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (widget->options != NULL) {
    GschemToplevel *w_current;

    g_object_get (widget, "gschem-toplevel", &w_current, NULL);

    g_return_if_fail (w_current != NULL);

    gtk_spin_button_set_value (GTK_SPIN_BUTTON (widget->snap_size),
                               gschem_options_get_snap_size (w_current->options));
  }
}
