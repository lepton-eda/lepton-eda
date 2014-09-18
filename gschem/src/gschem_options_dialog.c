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
#include <version.h>
#include <missing.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"



static void
class_init (GschemOptionsDialogClass *klass);

GtkWidget*
create_grid_mode_widget (GschemOptionsDialog *dialog);

GtkWidget*
create_snap_mode_widget (GschemOptionsDialog *dialog);

static GtkWidget*
create_net_section (GschemOptionsDialog *dialog);

static GtkWidget*
create_snap_section (GschemOptionsDialog *dialog);

static void
dispose (GObject *object);

static void
instance_init (GschemOptionsDialog *dialog);

static void
notify_gschem_toplevel (GschemOptionsDialog *dialog);

static void
set_options (GschemOptionsDialog *dialog, GschemOptions *options);

static void
update_grid_mode_model (GschemOptionsDialog *dialog, GtkWidget *button);

static void
update_grid_mode_widget (GschemOptionsDialog *dialog);

static void
update_magnetic_net_mode_model (GschemOptionsDialog *dialog);

static void
update_magnetic_net_mode_widget (GschemOptionsDialog *dialog);

static void
update_net_rubber_band_mode_model (GschemOptionsDialog *dialog);

static void
update_net_rubber_band_mode_widget (GschemOptionsDialog *dialog);

static void
update_snap_mode_model (GschemOptionsDialog *dialog, GtkWidget *button);

static void
update_snap_mode_widget (GschemOptionsDialog *dialog);

static void
update_snap_size_model (GschemOptionsDialog *dialog);

static void
update_snap_size_widget (GschemOptionsDialog *dialog);



/*! \brief Adjust widget focus for the convienence of the user
 *
 *  \param [in] dialog This options dialog
 */
void
gschem_options_dialog_adjust_focus (GschemOptionsDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->snap_size != NULL);

  /* Setting the focus to the snap size widget allows a subsequent keyboard
   * fumble to mess up the snap size. For now, it will not get focus.
   *
   * gtk_widget_grab_focus (dialog->snap_size);
   */
}



/*! \brief Get/register options dialog type.
 */
GType
gschem_options_dialog_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemOptionsDialogClass),
      NULL,                                       /* base_init */
      NULL,                                       /* base_finalize */
      (GClassInitFunc) class_init,
      NULL,                                       /* class_finalize */
      NULL,                                       /* class_data */
      sizeof(GschemOptionsDialog),
      0,                                          /* n_preallocs */
      (GInstanceInitFunc) instance_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                   "GschemOptionsDialog",
                                   &info,
                                   0);
  }

  return type;
}



/*! \brief Create a new options dialog
 *
 *  \param [in] w_current The GschemToplevel structure
 */
GtkDialog*
gschem_options_dialog_new (GschemToplevel *w_current)
{
    return g_object_new (GSCHEM_TYPE_OPTIONS_DIALOG,
                         /* GtkContainer */
                         "border-width",     DIALOG_BORDER_SPACING,
                         /* GtkWindow */
                         "title",            _("Snap Size"),
                         "default-width",    320,
                         "default-height",   350,
                         "window-position",  GTK_WIN_POS_NONE,
                         "allow-grow",       TRUE,
                         "allow-shrink",     FALSE,
                         "modal",            FALSE,
                         /* GtkDialog */
                         "has-separator",    TRUE,
                         /* GschemDialog */
                         "settings-name",    "snap-size",
                         "gschem-toplevel",  w_current,
                         NULL);
}



/*! \brief Create the snap size dialog
 *  \par Function Description
 *  This function creates the snap size dialog.
 */
void
snap_size_dialog (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  gschem_dialog_misc_show_non_modal (w_current,
                                     &(w_current->sswindow),
                                     gschem_options_dialog_new);

  gschem_options_dialog_adjust_focus (GSCHEM_OPTIONS_DIALOG (w_current->sswindow));
}



/*! \private
 *  \brief Initialize the options dialog class structure
 *
 *  \param [in] klass
 */
static void
class_init (GschemOptionsDialogClass *klass)
{
    g_return_if_fail (klass != NULL);

    G_OBJECT_CLASS(klass)->dispose = dispose;
}



/*! \private
 *  \brief Create the series of buttons that make the grid mode selection widget
 *
 *  \param [in] dialog
 *  \return The grid mode widget
 */
GtkWidget*
create_grid_mode_widget (GschemOptionsDialog *dialog)
{
  GtkWidget *box;
  int index;

  g_return_if_fail (dialog != NULL);

  box = gtk_hbox_new (FALSE, FALSE);

  for (index=0; index<GRID_MODE_COUNT; index++) {
    dialog->grid_radio[index] = gtk_toggle_button_new ();

    gtk_box_pack_start (GTK_BOX (box),                           /* box     */
                        dialog->grid_radio[index],               /* child   */
                        FALSE,                                   /* expand  */
                        FALSE,                                   /* fill    */
                        0);                                      /* padding */

    gtk_size_group_add_widget (dialog->size_group, dialog->grid_radio[index]);

    g_signal_connect_swapped (G_OBJECT (dialog->grid_radio[index]),
                              "clicked",
                              G_CALLBACK (update_grid_mode_model),
                              dialog);
  }

  gtk_button_set_label (GTK_BUTTON (dialog->grid_radio[GRID_MODE_NONE]),
                        _("Off"));

  gtk_button_set_label (GTK_BUTTON (dialog->grid_radio[GRID_MODE_DOTS]),
                        _("Dots"));

  gtk_button_set_label (GTK_BUTTON (dialog->grid_radio[GRID_MODE_MESH]),
                        _("Mesh"));

  return box;
}



/*! \private
 *  \brief Create section with net tool settings
 *
 *  \param [in] dialog
 *  \return The net section widget
 */
static GtkWidget*
create_net_section (GschemOptionsDialog *dialog)
{
  GtkWidget *label[2];
  GtkWidget *table;
  GtkWidget *widget[2];

  /* These widgets are shown in the same order as the options menu */

  label[0] = gschem_dialog_misc_create_property_label (_("Net Rubber Band Mode:"));
  label[1] = gschem_dialog_misc_create_property_label (_("Magnetic Net Mode:"));

  /*! \todo These should become a GtkSwitch when updating to GTK 3.0 */

  widget[0] = dialog->net_rubber_band_widget = gtk_check_button_new_with_label (_("Enabled"));
  widget[1] = dialog->magnetic_net_widget = gtk_check_button_new_with_label (_("Enabled"));

  table = gschem_dialog_misc_create_property_table (label, widget, 2);

  g_signal_connect_swapped (G_OBJECT (dialog->magnetic_net_widget),
                            "toggled",
                            G_CALLBACK (update_magnetic_net_mode_model),
                            dialog);

  g_signal_connect_swapped (G_OBJECT (dialog->net_rubber_band_widget),
                            "toggled",
                            G_CALLBACK (update_net_rubber_band_mode_model),
                            dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>Net Options</b>"), table);
}


/*! \private
 *  \brief Create section with snap and grid settings
 *
 *  \param [in] dialog
 *  \return The snap section widget
 */
static GtkWidget*
create_snap_section (GschemOptionsDialog *dialog)
{
  GtkWidget *label[3];
  GtkWidget *table;
  GtkWidget *widget[3];

  label[0] = gschem_dialog_misc_create_property_label (_("Grid Mode:"));
  label[1] = gschem_dialog_misc_create_property_label (_("Snap Mode:"));
  label[2] = gschem_dialog_misc_create_property_label (_("Snap Size:"));

  widget[0] = create_grid_mode_widget (dialog);
  widget[1] = create_snap_mode_widget (dialog);
  widget[2] = dialog->snap_size = gtk_spin_button_new_with_range (MINIMUM_SNAP_SIZE,
                                                                  MAXIMUM_SNAP_SIZE,
                                                                  5);

  table = gschem_dialog_misc_create_property_table (label, widget, 3);

  // gtk_editable_select_region (GTK_EDITABLE(spin_size), 0, -1);

  g_signal_connect_swapped (G_OBJECT (dialog->snap_size),
                            "value-changed",
                            G_CALLBACK (update_snap_size_model),
                            dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>Snap Options</b>"), table);
}



/*! \private
 *  \brief Create the series of buttons that make the snap mode selection widget
 *
 *  \param [in] dialog
 *  \return The snap mode widget
 */
GtkWidget*
create_snap_mode_widget (GschemOptionsDialog *dialog)
{
  GtkWidget *box;
  int index;

  g_return_if_fail (dialog != NULL);

  box = gtk_hbox_new (FALSE, FALSE);

  for (index=0; index<SNAP_STATE_COUNT; index++) {
    dialog->snap_radio[index] = gtk_toggle_button_new ();

    gtk_box_pack_start (GTK_BOX (box),                           /* box     */
                        dialog->snap_radio[index],               /* child   */
                        FALSE,                                   /* expand  */
                        FALSE,                                   /* fill    */
                        0);                                      /* padding */

    gtk_size_group_add_widget (dialog->size_group, dialog->snap_radio[index]);

    g_signal_connect_swapped (G_OBJECT (dialog->snap_radio[index]),
                              "clicked",
                              G_CALLBACK (update_snap_mode_model),
                              dialog);
  }

  gtk_button_set_label (GTK_BUTTON (dialog->snap_radio[SNAP_OFF]),
                        _("Off"));

  gtk_button_set_label (GTK_BUTTON (dialog->snap_radio[SNAP_GRID]),
                        _("Grid"));

  gtk_button_set_label (GTK_BUTTON (dialog->snap_radio[SNAP_RESNAP]),
                        _("Resnap"));

  return box;
}


/*! \private
 *  \brief Dispose
 *
 *  \param [in] object The options dialog to dispose
 */
static void
dispose (GObject *object)
{
  GschemOptionsDialog *dialog;
  GschemOptionsDialogClass *klass;
  GObjectClass *parent_class;

  g_return_if_fail (object != NULL);

  dialog = GSCHEM_OPTIONS_DIALOG (object);

  set_options (dialog, NULL);

  //g_slist_foreach (dialog->bindings, (GFunc) g_object_unref, NULL);
  //g_slist_free (dialog->bindings);
  //dialog->bindings = NULL;

  /* lastly, chain up to the parent dispose */

  klass = GSCHEM_OPTIONS_DIALOG_GET_CLASS (object);
  g_return_if_fail (klass != NULL);
  parent_class = g_type_class_peek_parent (klass);
  g_return_if_fail (parent_class != NULL);
  parent_class->dispose (object);
}



/*! \private
 *  \brief Initialize an options dialog instance
 *
 *  \param [in,out] dialog The text property dialog
 */
static void
instance_init (GschemOptionsDialog *dialog)
{
  GtkWidget *vbox;

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CLOSE,
                         GTK_RESPONSE_CLOSE);

  g_signal_connect (G_OBJECT (dialog),
                    "notify::gschem-toplevel",
                    G_CALLBACK (notify_gschem_toplevel),
                    NULL);

  gtk_container_set_border_width(GTK_CONTAINER(dialog), DIALOG_BORDER_SPACING);

  dialog->size_group = gtk_size_group_new (GTK_SIZE_GROUP_BOTH);

  vbox = GTK_DIALOG (dialog)->vbox;
  gtk_box_set_spacing (GTK_BOX (vbox), DIALOG_V_SPACING);

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_snap_section (dialog),            /* child   */
                      FALSE,                                   /* expand  */
                      FALSE,                                   /* fill    */
                      0);                                      /* padding */

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_net_section (dialog),             /* child   */
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
notify_gschem_toplevel (GschemOptionsDialog *dialog)
{
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);

  g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  set_options (dialog, w_current->options);
}



/*! \private
 *  \brief Set the options that this dialog manipulates
 *
 *  \param [in,out] dialog   This dialog
 *  \param [in]     options  The options to manipulate
 */
static void
set_options (GschemOptionsDialog *dialog, GschemOptions *options)
{
  if (dialog->options != NULL) {
    g_signal_handlers_disconnect_by_func (dialog->options,
                                          G_CALLBACK (update_snap_size_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->options,
                                          G_CALLBACK (update_snap_mode_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->options,
                                          G_CALLBACK (update_net_rubber_band_mode_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->options,
                                          G_CALLBACK (update_magnetic_net_mode_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->options,
                                          G_CALLBACK (update_grid_mode_widget),
                                          dialog);

    g_object_unref (dialog->options);
  }

  dialog->options = options;

  if (dialog->options != NULL) {
    g_object_ref (dialog->options);

    g_signal_connect_swapped (dialog->options,
                              "notify::grid-mode",
                              G_CALLBACK (update_grid_mode_widget),
                              dialog);

    g_signal_connect_swapped (dialog->options,
                              "notify::magnetic-net-mode",
                              G_CALLBACK (update_magnetic_net_mode_widget),
                              dialog);

    g_signal_connect_swapped (dialog->options,
                              "notify::net-rubber-band-mode",
                              G_CALLBACK (update_net_rubber_band_mode_widget),
                              dialog);

    g_signal_connect_swapped (dialog->options,
                              "notify::snap-mode",
                              G_CALLBACK (update_snap_mode_widget),
                              dialog);

    g_signal_connect_swapped (dialog->options,
                              "notify::snap-size",
                              G_CALLBACK (update_snap_size_widget),
                              dialog);
  }

  update_grid_mode_widget (dialog);
  update_magnetic_net_mode_widget (dialog);
  update_net_rubber_band_mode_widget (dialog);
  update_snap_mode_widget (dialog);
  update_snap_size_widget (dialog);
}



/*! \private
 *  \brief Update the grid mode in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_grid_mode_model (GschemOptionsDialog *dialog, GtkWidget *button)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->options != NULL) {
    int index;

    for (index = 0; index < GRID_MODE_COUNT; index++) {
      if (dialog->grid_radio[index] == button) {
        gschem_options_set_grid_mode (dialog->options, index);
        break;
      }
    }
  }
}



/*! \private
 *  \brief Update the grid mode widget with the current value
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_grid_mode_widget (GschemOptionsDialog *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->options != NULL) {
    GRID_MODE grid_mode;
    int index;

    grid_mode = gschem_options_get_grid_mode (dialog->options);

    for (index=0; index<GRID_MODE_COUNT; index++) {
      g_signal_handlers_block_by_func (G_OBJECT (dialog->grid_radio[index]),
                                       G_CALLBACK (update_grid_mode_model),
                                       dialog);
    }

    for (index=0; index<GRID_MODE_COUNT; index++) {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->grid_radio[index]),
                                    (grid_mode == index));
    }

    for (index=0; index<GRID_MODE_COUNT; index++) {
      g_signal_handlers_unblock_by_func (G_OBJECT (dialog->grid_radio[index]),
                                         G_CALLBACK (update_grid_mode_model),
                                         dialog);
    }
  }
}



/*! \private
 *  \brief Update the magnetic net mode in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_magnetic_net_mode_model (GschemOptionsDialog *dialog)
{
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);

  g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  gschem_options_set_magnetic_net_mode (w_current->options,
                                        gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (dialog->magnetic_net_widget)));
}



/*! \private
 *  \brief Update the net rubber band mode widget with the current value
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_magnetic_net_mode_widget (GschemOptionsDialog *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->options != NULL) {
    GschemToplevel *w_current;

    g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

    g_return_if_fail (w_current != NULL);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->magnetic_net_widget),
                                  gschem_options_get_magnetic_net_mode (w_current->options));
  }
}


/*! \private
 *  \brief Update the net rubber band mode in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_net_rubber_band_mode_model (GschemOptionsDialog *dialog)
{
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);

  g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  gschem_options_set_net_rubber_band_mode (w_current->options,
                                           gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (dialog->net_rubber_band_widget)));
}



/*! \private
 *  \brief Update the net rubber band mode widget with the current value
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_net_rubber_band_mode_widget (GschemOptionsDialog *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->options != NULL) {
    GschemToplevel *w_current;

    g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

    g_return_if_fail (w_current != NULL);

    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->net_rubber_band_widget),
                                  gschem_options_get_net_rubber_band_mode (w_current->options));
  }
}



/*! \private
 *  \brief Update the snap mode in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_snap_mode_model (GschemOptionsDialog *dialog, GtkWidget *button)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->options != NULL) {
    int index;

    for (index = 0; index < SNAP_STATE_COUNT; index++) {
      if (dialog->snap_radio[index] == button) {
        gschem_options_set_snap_mode (dialog->options, index);
        break;
      }
    }
  }
}



/*! \private
 *  \brief Update the snap mode widget with the current value
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_snap_mode_widget (GschemOptionsDialog *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->options != NULL) {
    int index;
    SNAP_STATE snap_mode;

    snap_mode = gschem_options_get_snap_mode (dialog->options);

    for (index=0; index<SNAP_STATE_COUNT; index++) {
      g_signal_handlers_block_by_func (G_OBJECT (dialog->snap_radio[index]),
                                       G_CALLBACK (update_snap_mode_model),
                                       dialog);
    }

    for (index=0; index<SNAP_STATE_COUNT; index++) {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (dialog->snap_radio[index]),
                                    (snap_mode == index));
  }

    for (index=0; index<SNAP_STATE_COUNT; index++) {
      g_signal_handlers_unblock_by_func (G_OBJECT (dialog->snap_radio[index]),
                                         G_CALLBACK (update_snap_mode_model),
                                         dialog);
    }
  }
}



/*! \private
 *  \brief Update the snap size in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_snap_size_model (GschemOptionsDialog *dialog)
{
  GschemToplevel *w_current;

  g_return_if_fail (dialog != NULL);

  g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

  g_return_if_fail (w_current != NULL);

  gschem_options_set_snap_size (w_current->options,
                                gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (dialog->snap_size)));
}



/*! \private
 *  \brief Update the snap size widget with the current value
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_snap_size_widget (GschemOptionsDialog *dialog)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->options != NULL) {
    GschemToplevel *w_current;

    g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

    g_return_if_fail (w_current != NULL);

    gtk_spin_button_set_value (GTK_SPIN_BUTTON (dialog->snap_size),
                               gschem_options_get_snap_size (w_current->options));
  }
}
