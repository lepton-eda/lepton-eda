/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Ales Hvezda
 * Copyright (C) 2013-2015 gEDA Contributors
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
 * \file gschem_text_properties_widget.c
 *
 * \brief A widget for editing text properties
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
  PROP_SCHEMATIC_WINDOW
};

G_DEFINE_TYPE (SchematicTextPropertiesWidget,
               schematic_text_properties_widget,
               SCHEMATIC_TYPE_BIN);


static GtkWidget*
create_text_content_section (SchematicTextPropertiesWidget *widget);

static GtkWidget*
create_text_property_section (SchematicTextPropertiesWidget *widget);

static void
dispose (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
notify_schematic_window (SchematicTextPropertiesWidget *widget);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
set_selection_adapter (SchematicTextPropertiesWidget *widget,
                       SchematicSelectionAdapter *adapter);
static void
update_text_alignment_model (SchematicTextPropertiesWidget *widget);

static void
update_text_alignment_widget (SchematicTextPropertiesWidget *widget);

static void
update_text_color_model (SchematicTextPropertiesWidget *widget);

static void
update_text_color_widget (SchematicTextPropertiesWidget *widget);

static void
update_text_content_model (SchematicTextPropertiesWidget *widget);

static void
update_text_content_widget (SchematicTextPropertiesWidget *widget);

static void
update_text_rotation_model (SchematicTextPropertiesWidget *widget);

static void
update_text_rotation_widget (SchematicTextPropertiesWidget *widget);



/*! \brief Adjust widget focus for the convienence of the user
 *
 *  If selecting one item, this function selects all the text content and gives
 *  focus to the text view. If multiple items are selected, this function gives
 *  focus to the color combo box.
 *
 *  \param [in] widget This text properties widget
 */
void
gschem_text_properties_widget_adjust_focus (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->text_view != NULL);
  g_return_if_fail (widget->colorcb != NULL);

  if (gtk_widget_is_sensitive (widget->text_view)) {
    GtkTextBuffer *textbuffer =
      gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget->text_view));

    GtkTextIter start, end;
    gtk_text_buffer_get_bounds (textbuffer, &start, &end);
    gtk_text_buffer_select_range (textbuffer, &start, &end);

    gtk_widget_grab_focus (widget->text_view);
  }
  else {
    gtk_widget_grab_focus (widget->colorcb);
  }
}



/*! \brief Create a new text properties widget
 *
 *  \param [in] w_current The SchematicWindow structure
 */
GtkWidget*
gschem_text_properties_widget_new (SchematicWindow *w_current)
{
  return GTK_WIDGET (g_object_new (SCHEMATIC_TYPE_TEXT_PROPERTIES_WIDGET,
                                   "schematic-window", w_current,
                                   NULL));
}



/*! \brief Open the widget box to edit text
 *
 *  \par Function Description
 *  This function creates or raises the modal text properties widget
 *
 *  \param [in] w_current The gschem toplevel
 */
void
text_edit_dialog (SchematicWindow *w_current)
{
  x_widgets_show_text_properties (w_current);
}



/*! \private
 *  \brief Initialize the text properties widget class structure
 *
 *  \param [in] klass
 */
static void
schematic_text_properties_widget_class_init (SchematicTextPropertiesWidgetClass *klass)
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
    PROP_SCHEMATIC_WINDOW,
    g_param_spec_pointer ("schematic-window",
                          "",
                          "",
                          (GParamFlags) (G_PARAM_CONSTRUCT_ONLY
                                         | G_PARAM_READWRITE)));
}



/*! \private
 *  \brief Create the text content section widget
 *
 *  \param [in] widget
 *  \return The new text content section widget
 */
static GtkWidget*
create_text_content_section (SchematicTextPropertiesWidget *widget)
{
#ifdef ENABLE_GTK3
  GtkWidget *bbox = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
  GtkWidget *vbox = gtk_box_new (GTK_ORIENTATION_VERTICAL, 0);

  gtk_widget_set_hexpand (vbox, TRUE);
  gtk_widget_set_vexpand (vbox, TRUE);
  gtk_widget_set_halign (vbox, GTK_ALIGN_FILL);
  gtk_widget_set_valign (vbox, GTK_ALIGN_FILL);
#else
  GtkWidget *bbox = gtk_hbutton_box_new ();
  GtkWidget *vbox = gtk_vbox_new (FALSE, 0);
#endif
  GtkWidget *scrolled = gtk_scrolled_window_new (NULL, NULL);

  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled),
                                  GTK_POLICY_AUTOMATIC,
                                  GTK_POLICY_AUTOMATIC);

  widget->text_view = gtk_text_view_new ();

  gtk_text_view_set_editable (GTK_TEXT_VIEW (widget->text_view), TRUE);

  /*! \bug FIXME: Set tab's width in the textview widget. */
  /* See first the code in newtext_init() and get it
   * working before adding it here.
   */

  gtk_container_add (GTK_CONTAINER (scrolled), widget->text_view);

  gtk_button_box_set_layout (GTK_BUTTON_BOX (bbox), GTK_BUTTONBOX_END);

  widget->apply_button = gtk_button_new_with_mnemonic (_("_Apply"));

  g_signal_connect_swapped (G_OBJECT (widget->apply_button),
                            "clicked",
                            G_CALLBACK (update_text_content_model),
                            widget);

  gtk_box_pack_start (GTK_BOX (bbox),                          /* box     */
                      widget->apply_button,                    /* child   */
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

  return schematic_dialog_misc_create_section_widget (_("<b>Text Content</b>"), vbox);
}



/*! \private
 *  \brief Create the text property section widget
 *
 *  \param [in] widget
 *  \return The new text property section widget
 */
static GtkWidget*
create_text_property_section (SchematicTextPropertiesWidget *widget)
{
  GtkWidget *label[4];
  GtkWidget *table;
  GtkWidget *editor[4];

  label[0] = schematic_dialog_misc_create_property_label(_("Colo_r:"));
  label[1] = schematic_dialog_misc_create_property_label(_("_Size:"));
  label[2] = schematic_dialog_misc_create_property_label(_("Ali_gnment:"));
  label[3] = schematic_dialog_misc_create_property_label(_("Ro_tation:"));

  editor[0] = widget->colorcb = x_colorcb_new ();
  editor[1] = widget->textsizecb = schematic_integer_combo_box_new ();
  editor[2] = widget->aligncb = schematic_alignment_combo_new();
  editor[3] = widget->rotatecb = schematic_rotation_combo_new();

  table = schematic_dialog_misc_create_property_table (label, editor, 4);

  widget->bindings = g_slist_append (widget->bindings,
                                     schematic_binding_integer_new ("text-size",
                                                                    editor[1]));

  g_signal_connect_swapped (G_OBJECT (widget->colorcb),
                            "changed",
                            G_CALLBACK (update_text_color_model),
                            widget);

  g_signal_connect_swapped (G_OBJECT (widget->aligncb),
                            "changed",
                            G_CALLBACK (update_text_alignment_model),
                            widget);

  g_signal_connect_swapped (G_OBJECT (widget->rotatecb),
                            "changed",
                            G_CALLBACK (update_text_rotation_model),
                            widget);

  g_signal_connect (G_OBJECT (schematic_integer_combo_box_get_entry (widget->textsizecb)),
                    "activate",
                    G_CALLBACK (schematic_dialog_misc_entry_activate),
                    widget);

  return schematic_dialog_misc_create_section_widget (_("<b>Text Properties</b>"), table);
}



/*! \private
 *  \brief Dispose
 *
 *  \param [in] object The text edit widget to dispose
 */
static void
dispose (GObject *object)
{
  SchematicTextPropertiesWidget *widget;
  SchematicTextPropertiesWidgetClass *klass;
  GObjectClass *parent_class;

  g_return_if_fail (object != NULL);

  widget = SCHEMATIC_TEXT_PROPERTIES_WIDGET (object);

  set_selection_adapter (widget, NULL);

  g_slist_foreach (widget->bindings, (GFunc) g_object_unref, NULL);
  g_slist_free (widget->bindings);
  widget->bindings = NULL;

  /* lastly, chain up to the parent dispose */

  klass = SCHEMATIC_TEXT_PROPERTIES_WIDGET_GET_CLASS (object);
  g_return_if_fail (klass != NULL);
  parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));
  g_return_if_fail (parent_class != NULL);
  parent_class->dispose (object);
}



/*! \brief Get a property
 *
 *  \param [in]     object
 *  \param [in]     param_id
 *  \param [in,out] value
 *  \param [in]     pspec
 */
static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec)
{
  SchematicTextPropertiesWidget *widget = SCHEMATIC_TEXT_PROPERTIES_WIDGET (object);

  switch (param_id) {
    case PROP_SCHEMATIC_WINDOW:
      g_value_set_pointer (value, widget->w_current);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \private
 *  \brief Initialize a text property widget instance
 *
 *  \param [in,out] widget The text property widget
 */
static void
schematic_text_properties_widget_init (SchematicTextPropertiesWidget *widget)
{
  GtkWidget *scrolled;
  GtkWidget *vbox;
  GtkWidget *viewport;

  g_signal_connect (G_OBJECT (widget),
                    "notify::schematic-window",
                    G_CALLBACK (notify_schematic_window),
                    NULL);

  scrolled = gtk_scrolled_window_new (NULL, NULL);
  gtk_container_add (GTK_CONTAINER (widget), scrolled);

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
  //gtk_container_set_border_width (GTK_CONTAINER (vbox), DIALOG_V_SPACING);
  gtk_container_add (GTK_CONTAINER (viewport), vbox);

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_text_content_section (widget),    /* child   */
                      TRUE,                                    /* expand  */
                      TRUE,                                    /* fill    */
                      0);                                      /* padding */

  gtk_box_pack_start (GTK_BOX (vbox),                          /* box     */
                      create_text_property_section (widget),   /* child   */
                      FALSE,                                   /* expand  */
                      FALSE,                                   /* fill    */
                      0);                                      /* padding */
}



/*! \private
 *  \brief Property change notification from the base class
 *
 *  Handles property change notification from the base class to
 *  update members and signal handlers in the derived class.
 *
 *  \param [in,out] widget    This widget
 */
static void
notify_schematic_window (SchematicTextPropertiesWidget *widget)
{
    SchematicWindow *w_current;

    g_return_if_fail (widget != NULL);

    g_object_get (widget, "schematic-window", &w_current, NULL);

    schematic_integer_combo_box_set_model (widget->textsizecb,
                                           schematic_window_get_text_size_list_store (w_current));

    set_selection_adapter (widget,
                           schematic_window_get_selection_adapter (w_current));
}



/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  SchematicTextPropertiesWidget *widget = SCHEMATIC_TEXT_PROPERTIES_WIDGET (object);

  switch (param_id) {
    case PROP_SCHEMATIC_WINDOW:
      widget->w_current = SCHEMATIC_WINDOW (g_value_get_pointer (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \private
 *  \brief Set the selection that this widget manipulates
 *
 *  \param [in,out] widget  The #SchematicTextPropertiesWidget widget.
 *  \param [in]     adapter The #SchematicSelectionAdapter instance to manipulate.
 */
static void
set_selection_adapter (SchematicTextPropertiesWidget *widget,
                       SchematicSelectionAdapter *adapter)
{
  g_return_if_fail (widget != NULL);

  if (widget->adapter != NULL) {
    g_signal_handlers_disconnect_by_func (widget->adapter,
                                          (gpointer) update_text_content_widget,
                                          widget);

    g_signal_handlers_disconnect_by_func (widget->adapter,
                                          (gpointer) update_text_rotation_widget,
                                          widget);

    g_signal_handlers_disconnect_by_func (widget->adapter,
                                          (gpointer) update_text_color_widget,
                                          widget);

    g_signal_handlers_disconnect_by_func (widget->adapter,
                                          (gpointer) update_text_alignment_widget,
                                          widget);

    g_object_unref (widget->adapter);
  }

  widget->adapter = adapter;

  g_slist_foreach (widget->bindings,
                   (GFunc) schematic_binding_set_model_object,
                   adapter);

  if (widget->adapter != NULL) {
    g_object_ref (widget->adapter);

    g_signal_connect_swapped (widget->adapter,
                              "notify::text-alignment",
                              G_CALLBACK (update_text_alignment_widget),
                              widget);

    g_signal_connect_swapped (widget->adapter,
                              "notify::text-color",
                              G_CALLBACK (update_text_color_widget),
                              widget);

    g_signal_connect_swapped (widget->adapter,
                              "notify::text-rotation",
                              G_CALLBACK (update_text_rotation_widget),
                              widget);

    g_signal_connect_swapped (widget->adapter,
                              "notify::text-string",
                              G_CALLBACK (update_text_content_widget),
                              widget);
  }

  update_text_alignment_widget (widget);
  update_text_color_widget (widget);
  update_text_rotation_widget (widget);
  update_text_content_widget (widget);
}



/*! \private
 *  \brief Update the text alignment in the model
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_alignment_model (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->aligncb != NULL);

  if (widget->adapter != NULL) {
    int alignment = schematic_alignment_combo_get_align (widget->aligncb);

    if (alignment >= 0) {
      schematic_selection_adapter_set_text_alignment (widget->adapter, alignment);
    }
  }
}



/*! \private
 *  \brief Update the value in the text alignment widget
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_alignment_widget (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->aligncb != NULL);

  if (widget->adapter != NULL) {
    int alignment = schematic_selection_adapter_get_text_alignment (widget->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (widget->aligncb),
                                     (gpointer) update_text_alignment_model,
                                     widget);

    schematic_alignment_combo_set_align (widget->aligncb, alignment);

    g_signal_handlers_unblock_by_func (G_OBJECT (widget->aligncb),
                                       (gpointer) update_text_alignment_model,
                                       widget);

    gtk_widget_set_sensitive (GTK_WIDGET (widget->aligncb), (alignment != NO_SELECTION));
  }
}



/*! \private
 *  \brief Update the text color value in the model
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_color_model (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->colorcb != NULL);

  if (widget->adapter != NULL) {
    int color = x_colorcb_get_index (widget->colorcb);

    if (color >= 0) {
      schematic_selection_adapter_set_text_color (widget->adapter, color);
    }
  }
}



/*! \private
 *  \brief Update the value in the object color widget
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_color_widget (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->colorcb != NULL);

  if (widget->adapter != NULL) {
    int color = schematic_selection_adapter_get_text_color (widget->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (widget->colorcb),
                                     (gpointer) update_text_color_model,
                                     widget);

    x_colorcb_set_index(widget->colorcb, color);

    g_signal_handlers_unblock_by_func (G_OBJECT (widget->colorcb),
                                       (gpointer) update_text_color_model,
                                       widget);

    gtk_widget_set_sensitive (GTK_WIDGET (widget->colorcb), (color != NO_SELECTION));
  }
}



/*! \private
 *  \brief Update the text string in the model
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_content_model (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->text_view != NULL);

  if (widget->adapter != NULL) {
    char *string;
    GtkTextBuffer *buffer;
    GtkTextIter start;
    GtkTextIter end;

    buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget->text_view));
    gtk_text_buffer_get_bounds (buffer, &start, &end);
    string =  gtk_text_iter_get_text (&start, &end);

    if (string != NULL) {
      schematic_selection_adapter_set_text_string (widget->adapter, string, widget->w_current);
    }
  }
}



/*! \private
 *  \brief Update the value in the object color widget
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_content_widget (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->text_view != NULL);

  if (widget->adapter != NULL) {
    GtkTextBuffer *buffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (widget->text_view));
    const char *string = schematic_selection_adapter_get_text_string (widget->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (widget->text_view),
                                     (gpointer) update_text_content_model,
                                     widget);

    if (string != NULL) {
      gtk_text_buffer_set_text (buffer, string, -1);
    }
    else {
      GtkTextIter start;
      GtkTextIter end;

      gtk_text_buffer_get_bounds (buffer, &start, &end);
      gtk_text_buffer_delete (buffer, &start, &end);
    }

    g_signal_handlers_unblock_by_func (G_OBJECT (widget->text_view),
                                       (gpointer) update_text_content_model,
                                       widget);

    gtk_widget_set_sensitive (GTK_WIDGET (widget->text_view), (string != NULL));
    gtk_widget_set_sensitive (GTK_WIDGET (widget->apply_button), (string != NULL));
  }
}



/*! \private
 *  \brief Update the text rotation value in the model
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_rotation_model (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->rotatecb != NULL);

  if (widget->adapter != NULL) {
    int angle = schematic_rotation_combo_get_angle (widget->rotatecb);

    if (angle >= 0) {
      schematic_selection_adapter_set_text_rotation (widget->adapter, angle);
    }
  }
}



/*! \private
 *  \brief Update the value in the text rotation widget
 *
 *  \param [in,out] widget This widget
 */
static void
update_text_rotation_widget (SchematicTextPropertiesWidget *widget)
{
  g_return_if_fail (widget != NULL);
  g_return_if_fail (widget->rotatecb != NULL);

  if (widget->adapter != NULL) {
    int angle = schematic_selection_adapter_get_text_rotation (widget->adapter);

    g_signal_handlers_block_by_func (G_OBJECT (widget->rotatecb),
                                     (gpointer) update_text_rotation_model,
                                     widget);

    schematic_rotation_combo_set_angle (widget->rotatecb, angle);

    g_signal_handlers_unblock_by_func (G_OBJECT (widget->rotatecb),
                                       (gpointer) update_text_rotation_model,
                                       widget);

    gtk_widget_set_sensitive (GTK_WIDGET (widget->rotatecb), (angle != NO_SELECTION));
  }
}
