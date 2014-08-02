/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013 gEDA Contributors (see ChangeLog for details)
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
 * \file gschem_text_properties_dialog.c
 *
 * \brief A dialog box for editing text properties
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
class_init (GschemTextPropertiesDialogClass *klass);

static GtkWidget*
create_text_content_section (GschemTextPropertiesDialog *dialog);

static GtkWidget*
create_text_property_section (GschemTextPropertiesDialog *dialog);

static void
dispose (GObject *object);

static void
instance_init (GschemTextPropertiesDialog *dialog);

static void
notify_gschem_toplevel (GschemTextPropertiesDialog *dialog);

static void
set_selection_adapter (GschemTextPropertiesDialog *dialog, GschemSelectionAdapter *adapter);

static void
update_text_alignment_model (GschemTextPropertiesDialog *dialog);

static void
update_text_alignment_widget (GschemTextPropertiesDialog *dialog);

static void
update_text_color_model (GschemTextPropertiesDialog *dialog);

static void
update_text_color_widget (GschemTextPropertiesDialog *dialog);

static void
update_text_content_model (GschemTextPropertiesDialog *dialog);

static void
update_text_content_widget (GschemTextPropertiesDialog *dialog);

static void
update_text_rotation_model (GschemTextPropertiesDialog *dialog);

static void
update_text_rotation_widget (GschemTextPropertiesDialog *dialog);



/*! \brief Adjust widget focus for the convienence of the user
 *
 *  If selecting one item, this function selects all the text content and gives
 *  focus to the text view. If multiple items are selected, this function gives
 *  focus to the color combo box.
 *
 *  \param [in] dialog This text properties dialog
 */
void
gschem_text_properties_dialog_adjust_focus (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->text_view != NULL);
  g_return_if_fail (dialog->colorcb != NULL);

  if (gtk_widget_is_sensitive (dialog->text_view)) {
    select_all_text_in_textview (GTK_TEXT_VIEW (dialog->text_view));
    gtk_widget_grab_focus (dialog->text_view);
  }
  else {
    gtk_widget_grab_focus (dialog->colorcb);
  }
}



/*! \brief Get/register text properties dialog type.
 */
GType
gschem_text_properties_dialog_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemTextPropertiesDialogClass),
      NULL,                                       /* base_init */
      NULL,                                       /* base_finalize */
      (GClassInitFunc) class_init,
      NULL,                                       /* class_finalize */
      NULL,                                       /* class_data */
      sizeof(GschemTextPropertiesDialog),
      0,                                          /* n_preallocs */
      (GInstanceInitFunc) instance_init,
    };

    type = g_type_register_static (GSCHEM_TYPE_DIALOG,
                                   "GschemTextPropertiesDialog",
                                   &info,
                                   0);
  }

  return type;
}



/*! \brief Create a new text properties dialog
 *
 *  \param [in] w_current The GschemToplevel structure
 */
GtkDialog*
gschem_text_properties_dialog_new (GschemToplevel *w_current)
{
    return g_object_new (GSCHEM_TYPE_TEXT_PROPERTIES_DIALOG,
                         /* GtkContainer */
                         "border-width",     DIALOG_BORDER_SPACING,
                         /* GtkWindow */
                         "title",            _("Edit Text Properties"),
                         "default-width",    320,
                         "default-height",   350,
                         "window-position",  GTK_WIN_POS_NONE,
                         "allow-grow",       TRUE,
                         "allow-shrink",     FALSE,
                         "modal",            FALSE,
                         /* GtkDialog */
                         "has-separator",    TRUE,
                         /* GschemDialog */
                         "settings-name",    "text-edit",
                         "gschem-toplevel",  w_current,
                         NULL);
}



/*! \brief Open the dialog box to edit text
 *
 *  \par Function Description
 *  This function creates or raises the modal text properties dialog
 *
 *  \param [in] w_current The gschem toplevel
 */
void
text_edit_dialog (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  gschem_dialog_misc_show_non_modal (w_current,
                                     &(w_current->tewindow),
                                     gschem_text_properties_dialog_new);

  gschem_text_properties_dialog_adjust_focus(GSCHEM_TEXT_PROPERTIES_DIALOG(w_current->tewindow));
}



/*! \private
 *  \brief Initialize the text properties dialog class structure
 *
 *  \param [in] klass
 */
static void
class_init (GschemTextPropertiesDialogClass *klass)
{
    g_return_if_fail (klass != NULL);

    G_OBJECT_CLASS(klass)->dispose = dispose;
}



/*! \private
 *  \brief Create the text content section widget
 *
 *  \param [in] dialog
 *  \return The new text content section widget
 */
static GtkWidget*
create_text_content_section (GschemTextPropertiesDialog *dialog)
{
  GtkWidget *bbox = gtk_hbutton_box_new ();
  GtkWidget *scrolled = gtk_scrolled_window_new (NULL, NULL);
  GtkWidget *vbox = gtk_vbox_new (FALSE, 0);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                  GTK_POLICY_AUTOMATIC,
                                  GTK_POLICY_AUTOMATIC);

  dialog->text_view = gtk_text_view_new ();

  gtk_text_view_set_editable (GTK_TEXT_VIEW (dialog->text_view), TRUE);

  /*! \bug FIXME: Set tab's width in the textview widget. */
  /* See first the code in text_input_dialog and get it working before adding
   * it here.
   */

  gtk_container_add (GTK_CONTAINER (scrolled), dialog->text_view);

  gtk_button_box_set_layout (GTK_BUTTON_BOX (bbox), GTK_BUTTONBOX_END);

  dialog->apply_button = gtk_button_new_from_stock (GTK_STOCK_APPLY);

  g_signal_connect_swapped (G_OBJECT (dialog->apply_button),
                            "clicked",
                            G_CALLBACK (update_text_content_model),
                            dialog);

  gtk_box_pack_start (GTK_BOX (bbox),                          /* box     */
                      dialog->apply_button,                    /* child   */
                      TRUE,                                    /* expand  */
                      TRUE,                                    /* fill    */
                      0);                                      /* padding */

  gtk_box_set_spacing (GTK_BOX (vbox), DIALOG_V_SPACING);

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      scrolled,                                /* child   */
                      TRUE,                                    /* expand  */
                      TRUE,                                    /* fill    */
                      0);                                      /* padding */

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      bbox,                                    /* child   */
                      FALSE,                                   /* expand  */
                      TRUE,                                    /* fill    */
                      0);                                      /* padding */

  return gschem_dialog_misc_create_section_widget (_("<b>Text Content</b>"), vbox);
}



/*! \private
 *  \brief Create the text property section widget
 *
 *  \param [in] dialog
 *  \return The new text property section widget
 */
static GtkWidget*
create_text_property_section (GschemTextPropertiesDialog *dialog)
{
  GtkWidget *label[4];
  GtkWidget *table;
  GtkWidget *widget[4];

  label[0] = gschem_dialog_misc_create_property_label(_("Color:"));
  label[1] = gschem_dialog_misc_create_property_label(_("Size:"));
  label[2] = gschem_dialog_misc_create_property_label(_("Alignment:"));
  label[3] = gschem_dialog_misc_create_property_label(_("Rotation:"));

  widget[0] = dialog->colorcb = x_colorcb_new ();
  widget[1] = dialog->textsizecb = gschem_integer_combo_box_new ();
  widget[2] = dialog->aligncb = x_aligncb_new();
  widget[3] = dialog->rotatecb = x_rotatecb_new();

  table = gschem_dialog_misc_create_property_table (label, widget, 4);

  dialog->bindings = g_slist_append (dialog->bindings,
                                     gschem_binding_integer_new ("text-size",
                                                                 widget[1]));

  g_signal_connect_swapped (G_OBJECT (dialog->colorcb),
                            "changed",
                            G_CALLBACK (update_text_color_model),
                            dialog);

  g_signal_connect_swapped (G_OBJECT (dialog->aligncb),
                            "changed",
                            G_CALLBACK (update_text_alignment_model),
                            dialog);

  g_signal_connect_swapped (G_OBJECT (dialog->rotatecb),
                            "changed",
                            G_CALLBACK (update_text_rotation_model),
                            dialog);

  g_signal_connect (G_OBJECT (gschem_integer_combo_box_get_entry (dialog->textsizecb)),
                    "activate",
                    G_CALLBACK (gschem_dialog_misc_entry_activate),
                    dialog);

  return gschem_dialog_misc_create_section_widget (_("<b>Text Properties</b>"), table);
}



/*! \private
 *  \brief Dispose
 *
 *  \param [in] object The text edit dialog to dispose
 */
static void
dispose (GObject *object)
{
  GschemTextPropertiesDialog *dialog;
  GschemTextPropertiesDialogClass *klass;
  GObjectClass *parent_class;

  g_return_if_fail (object != NULL);

  dialog = GSCHEM_TEXT_PROPERTIES_DIALOG (object);

  set_selection_adapter (dialog, NULL);

  g_slist_foreach (dialog->bindings, (GFunc) g_object_unref, NULL);
  g_slist_free (dialog->bindings);
  dialog->bindings = NULL;

  /* lastly, chain up to the parent dispose */

  klass = GSCHEM_TEXT_PROPERTIES_DIALOG_GET_CLASS (object);
  g_return_if_fail (klass != NULL);
  parent_class = g_type_class_peek_parent (klass);
  g_return_if_fail (parent_class != NULL);
  parent_class->dispose (object);
}



/*! \private
 *  \brief Initialize a text property dialog instance
 *
 *  \param [in,out] dialog The text property dialog
 */
static void
instance_init (GschemTextPropertiesDialog *dialog)
{
  GtkWidget *vbox;

  gtk_dialog_add_button (GTK_DIALOG (dialog),
                         GTK_STOCK_CLOSE,
                         GTK_RESPONSE_CLOSE);

  g_signal_connect (G_OBJECT (dialog),
                    "notify::gschem-toplevel",
                    G_CALLBACK (notify_gschem_toplevel),
                    NULL);

  vbox = GTK_DIALOG (dialog)->vbox;
  gtk_box_set_spacing (GTK_BOX (vbox), DIALOG_V_SPACING);

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_text_content_section (dialog),    /* child   */
                      TRUE,                                    /* expand  */
                      TRUE,                                    /* fill    */
                      0);                                      /* padding */

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_text_property_section (dialog),   /* child   */
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
notify_gschem_toplevel (GschemTextPropertiesDialog *dialog)
{
    GschemToplevel *w_current;

    g_return_if_fail (dialog != NULL);

    g_object_get (dialog, "gschem-toplevel", &w_current, NULL);

    gschem_integer_combo_box_set_model (dialog->textsizecb,
                                        gschem_toplevel_get_text_size_list_store (w_current));

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
set_selection_adapter (GschemTextPropertiesDialog *dialog, GschemSelectionAdapter *adapter)
{
  g_return_if_fail (dialog != NULL);

  if (dialog->adapter != NULL) {
    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (update_text_content_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (update_text_rotation_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (update_text_color_widget),
                                          dialog);

    g_signal_handlers_disconnect_by_func (dialog->adapter,
                                          G_CALLBACK (update_text_alignment_widget),
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
                              "notify::text-alignment",
                              G_CALLBACK (update_text_alignment_widget),
                              dialog);

    g_signal_connect_swapped (dialog->adapter,
                              "notify::text-color",
                              G_CALLBACK (update_text_color_widget),
                              dialog);

    g_signal_connect_swapped (dialog->adapter,
                              "notify::text-rotation",
                              G_CALLBACK (update_text_rotation_widget),
                              dialog);

    g_signal_connect_swapped (dialog->adapter,
                              "notify::text-string",
                              G_CALLBACK (update_text_content_widget),
                              dialog);
  }

  update_text_alignment_widget (dialog);
  update_text_color_widget (dialog);
  update_text_rotation_widget (dialog);
  update_text_content_widget (dialog);
}



/*! \private
 *  \brief Update the text alignment in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_alignment_model (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->aligncb != NULL);

  if (dialog->adapter != NULL) {
    int alignment = x_aligncb_get_align (dialog->aligncb);

    if (alignment >= 0) {
      gschem_selection_adapter_set_text_alignment (dialog->adapter, alignment);
    }
  }
}



/*! \private
 *  \brief Update the value in the text alignment widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_alignment_widget (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->aligncb != NULL);

  if (dialog->adapter != NULL) {
    int alignment = gschem_selection_adapter_get_text_alignment (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->aligncb),
                                     G_CALLBACK (update_text_alignment_model),
                                     dialog);

    x_aligncb_set_align (dialog->aligncb, alignment);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->aligncb),
                                       G_CALLBACK (update_text_alignment_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->aligncb), (alignment != NO_SELECTION));
  }
}



/*! \private
 *  \brief Update the text color value in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_color_model (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->colorcb != NULL);

  if (dialog->adapter != NULL) {
    int color = x_colorcb_get_index (dialog->colorcb);

    if (color >= 0) {
      gschem_selection_adapter_set_text_color (dialog->adapter, color);
    }
  }
}



/*! \private
 *  \brief Update the value in the object color widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_color_widget (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->colorcb != NULL);

  if (dialog->adapter != NULL) {
    int color = gschem_selection_adapter_get_text_color (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->colorcb),
                                     G_CALLBACK (update_text_color_model),
                                     dialog);

    x_colorcb_set_index(dialog->colorcb, color);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->colorcb),
                                       G_CALLBACK (update_text_color_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->colorcb), (color != NO_SELECTION));
  }
}



/*! \private
 *  \brief Update the text string in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_content_model (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->text_view != NULL);

  if (dialog->adapter != NULL) {
    char *string;
    GtkTextBuffer *buffer;
    GtkTextIter start;
    GtkTextIter end;

    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (dialog->text_view));
    gtk_text_buffer_get_bounds (buffer, &start, &end);
    string =  gtk_text_iter_get_text (&start, &end);

    if (string != NULL) {
      gschem_selection_adapter_set_text_string (dialog->adapter, string, dialog->parent.w_current);
    }
  }
}



/*! \private
 *  \brief Update the value in the object color widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_content_widget (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->text_view != NULL);

  if (dialog->adapter != NULL) {
    GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (dialog->text_view));
    const char *string = gschem_selection_adapter_get_text_string (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->text_view),
                                     G_CALLBACK (update_text_content_model),
                                     dialog);

    if (string != NULL) {
      gtk_text_buffer_set_text (buffer, string, -1);
    }
    else {
      GtkTextIter start;
      GtkTextIter end;

      gtk_text_buffer_get_bounds (buffer, &start, &end);
      gtk_text_buffer_delete (buffer, &start, &end);
    }

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->text_view),
                                       G_CALLBACK (update_text_content_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->text_view), (string != NULL));
    gtk_widget_set_sensitive (GTK_WIDGET (dialog->apply_button), (string != NULL));
  }
}



/*! \private
 *  \brief Update the text rotation value in the model
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_rotation_model (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->rotatecb != NULL);

  if (dialog->adapter != NULL) {
    int angle = x_rotatecb_get_angle (dialog->rotatecb);

    if (angle >= 0) {
      gschem_selection_adapter_set_text_rotation (dialog->adapter, angle);
    }
  }
}



/*! \private
 *  \brief Update the value in the text rotation widget
 *
 *  \param [in,out] dialog This dialog
 */
static void
update_text_rotation_widget (GschemTextPropertiesDialog *dialog)
{
  g_return_if_fail (dialog != NULL);
  g_return_if_fail (dialog->rotatecb != NULL);

  if (dialog->adapter != NULL) {
    int angle = gschem_selection_adapter_get_text_rotation (dialog->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (dialog->rotatecb),
                                     G_CALLBACK (update_text_rotation_model),
                                     dialog);

    x_rotatecb_set_angle (dialog->rotatecb, angle);

    g_signal_handlers_unblock_by_func (G_OBJECT (dialog->rotatecb),
                                       G_CALLBACK (update_text_rotation_model),
                                       dialog);

    gtk_widget_set_sensitive (GTK_WIDGET (dialog->rotatecb), (angle != NO_SELECTION));
  }
}
