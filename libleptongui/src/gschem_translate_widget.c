/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * \file gschem_translate_widget.c
 *
 * \brief A widget for an offset for symbol translation
 */

#include <config.h>

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "gschem.h"


enum
{
  PROP_0,
  PROP_LABEL_TEXT,
  PROP_VALUE
};


static void
activate_entry (GtkWidget *entry, GschemTranslateWidget *widget);

static void
class_init (GschemTranslateWidgetClass *klass);

static void
click_cancel (GtkWidget *button, GschemTranslateWidget *widget);

static void
click_evaluate (GtkWidget *entry, GschemTranslateWidget *widget);

static gboolean
convert_value (const char *string, int *value);

static void
dispose (GObject *object);

static void
finalize (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
instance_init (GschemTranslateWidget *view);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
notify_entry_text (GtkWidget *entry, GParamSpec *pspec, GschemTranslateWidget *widget);


static GObjectClass *gschem_translate_widget_parent_class = NULL;


/*! \brief Get the entry
 *
 *  \param [in] widget This GschemTranslateWidget
 *  \return The entry
 */
GtkWidget*
gschem_translate_widget_get_entry (GschemTranslateWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return widget->entry;
}



/*! \brief Get the label text
 *
 *  \param [in] widget This GschemTranslateWidget
 *  \return The label text
 */
const char*
gschem_translate_widget_get_label_text (GschemTranslateWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_label_get_text (GTK_LABEL (widget->label));
}


/*! \brief Get/register GschemTranslateWidget type.
 */
GType
gschem_translate_widget_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemTranslateWidgetClass),
      NULL,                                  /* base_init */
      NULL,                                  /* base_finalize */
      (GClassInitFunc) class_init,
      NULL,                                  /* class_finalize */
      NULL,                                  /* class_data */
      sizeof(GschemTranslateWidget),
      0,                                     /* n_preallocs */
      (GInstanceInitFunc) instance_init,
    };

    type = g_type_register_static (GTK_TYPE_INFO_BAR,
                                   "GschemTranslateWidget",
                                   &info,
                                   (GTypeFlags) 0);
  }

  return type;
}



/*! \brief Get the value in the entry
 *
 *  If the value inside the entry is invalid, this function returns 0. However,
 *  this function will return a valid response in the handler for the response
 *  signal, since the response signal will not be emitted if the value is
 *  invalid.
 *
 *  \param [in] widget This GschemTranslateWidget
 *  \return The value in the entry
 */
int
gschem_translate_widget_get_value (GschemTranslateWidget *widget)
{
  int value = 0;

  g_return_val_if_fail (widget != NULL, value);

  convert_value (gtk_entry_get_text (GTK_ENTRY (widget->entry)), &value);

  return value;
}


/*! \brief Set the label text
 *
 *  \param [in,out] view This GschemTranslateWidget
 *  \param [in]     text The label text
 */
void
gschem_translate_widget_set_label_text (GschemTranslateWidget *widget, const char *text)
{
  g_return_if_fail (widget != NULL);

  gtk_label_set_text (GTK_LABEL (widget->label), text);

  g_object_notify (G_OBJECT (widget), "label-text");
}


/*! \brief Set the value in the entry
 *
 *  \param [in,out] view This GschemTranslateWidget
 *  \param [in]     value the value to put in the entry
 */
void
gschem_translate_widget_set_value (GschemTranslateWidget *widget, int value)
{
  GString *temp;

  g_return_if_fail (widget != NULL);

  temp = g_string_new (NULL);
  g_string_printf (temp, "%d", value);
  gtk_entry_set_text (GTK_ENTRY (widget->entry), temp->str);
  g_string_free (temp, TRUE);

  g_object_notify (G_OBJECT (widget), "value");
}


/* Callback for when the user presses enter in the entry widget
 */
static void
activate_entry (GtkWidget *entry, GschemTranslateWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (gtk_entry_get_text_length (GTK_ENTRY (widget->entry)) == 0) {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_CANCEL);
  }
  else if (convert_value (gtk_entry_get_text (GTK_ENTRY (widget->entry)), NULL)) {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_OK);
  }
}


/*! \brief Initialize GschemTranslateWidget class
 *
 *  \param [in] klass The class for the GschemTranslateWidget
 */
static void
class_init (GschemTranslateWidgetClass *klass)
{
  gschem_translate_widget_parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));

  G_OBJECT_CLASS (klass)->dispose  = dispose;
  G_OBJECT_CLASS (klass)->finalize = finalize;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LABEL_TEXT,
                                   g_param_spec_string ("label-text",
                                                        "Label Text",
                                                        "Label Text",
                                                        _("Coordinate:"),
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_VALUE,
                                   g_param_spec_int ("value",
                                                     "Value",
                                                     "Value",
                                                     0,
                                                     G_MAXINT,
                                                     0,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_CONSTRUCT)));
}


/* Callback for when the user clicks the cancel button
 */
static void
click_cancel (GtkWidget *button, GschemTranslateWidget *widget)
{
  gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_CANCEL);
}


/* Callback for when the user clicks the evaluate button
 */
static void
click_evaluate (GtkWidget *entry, GschemTranslateWidget *widget)
{
  g_return_if_fail (widget != NULL);

  if (convert_value (gtk_entry_get_text (GTK_ENTRY (widget->entry)), NULL)) {
    gtk_info_bar_response (GTK_INFO_BAR (widget), GTK_RESPONSE_OK);
  }
}


/*! \private
 *  \brief convert the value in the entry
 *
 *  In all cases where the value pointer is not NULL, the value will be set.
 *  When an error occurs and the function returns FALSE, the value will be set
 *  to 0. In cases of success, the value is set to the value of the text and
 *  the function returns TRUE.
 *
 *  \param [in] text the text from the entry
 *  \param [out] value the text converted to an int [allow none]
 *  \retval TRUE the value is valid
 *  \retval FALSE the value is invalid
 */
static gboolean
convert_value (const char *text, int *value)
{
  char *end = NULL;
  long lvalue;

  if (value != NULL) {
    *value = 0;
  }

  errno = 0;

  lvalue = strtol (text, &end, 10);

  if (errno != 0) {
    return FALSE;
  }

  if (text == end) {
    return FALSE;
  }

  if (lvalue > G_MAXINT) {
    return FALSE;
  }

  if (lvalue < 0) {
    return FALSE;
  }

  if (value != NULL) {
    *value = lvalue;
  }

  return TRUE;
}


/* Dispose of the object
 */
static void
dispose (GObject *object)
{
  /* lastly, chain up to the parent dispose */

  g_return_if_fail (gschem_translate_widget_parent_class != NULL);
  gschem_translate_widget_parent_class->dispose (object);
}



/* Finalize object
 */
static void
finalize (GObject *object)
{
  /* lastly, chain up to the parent finalize */

  g_return_if_fail (gschem_translate_widget_parent_class != NULL);
  gschem_translate_widget_parent_class->finalize (object);
}



/* Get a property
 */
static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec)
{
  GschemTranslateWidget *widget = GSCHEM_TRANSLATE_WIDGET (object);

  switch (param_id) {
    case PROP_LABEL_TEXT:
      g_value_set_string (value, gschem_translate_widget_get_label_text (widget));
      break;

    case PROP_VALUE:
      g_value_set_int (value, gschem_translate_widget_get_value (widget));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}


/*! \private
 *  \brief Initialize GschemTranslateWidget instance
 *
 *  \param [in,out] widget the GschemTranslateWidget
 */
static void
instance_init (GschemTranslateWidget *widget)
{
  GtkWidget *action = gtk_info_bar_get_action_area (GTK_INFO_BAR (widget));
  GtkWidget *button_box;
  GtkWidget *cancel_button;
  GtkWidget *content = gtk_info_bar_get_content_area (GTK_INFO_BAR (widget));

  g_return_if_fail (widget != NULL);

  gtk_widget_set_no_show_all (GTK_WIDGET (widget), TRUE);

  widget->label = gtk_label_new (NULL);
  gtk_widget_set_visible (widget->label, TRUE);
  gtk_box_pack_start (GTK_BOX (content), widget->label, FALSE, FALSE, 0);

  widget->entry = gtk_entry_new ();
  gtk_widget_set_visible (widget->entry, TRUE);
  gtk_box_pack_start (GTK_BOX (content), widget->entry, TRUE, TRUE, 0);

#ifdef ENABLE_GTK3
  button_box = gtk_button_box_new (GTK_ORIENTATION_HORIZONTAL);
#else
  button_box = gtk_hbutton_box_new ();
#endif
  gtk_widget_set_visible (button_box, TRUE);
  gtk_box_pack_start (GTK_BOX (content), button_box, FALSE, FALSE, 0);

  widget->evaluate_button = gtk_button_new_with_label (_("Translate"));
  gtk_widget_set_sensitive (widget->evaluate_button, FALSE);
  gtk_widget_set_visible (widget->evaluate_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), widget->evaluate_button, FALSE, FALSE, 0);

  cancel_button = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
  gtk_widget_set_visible (cancel_button, TRUE);
  gtk_box_pack_start (GTK_BOX (button_box), cancel_button, FALSE, FALSE, 0);

  gtk_widget_set_no_show_all (action, TRUE);
  gtk_widget_set_visible (action, FALSE);

  g_signal_connect (G_OBJECT (widget->entry),
                    "activate",
                    G_CALLBACK (activate_entry),
                    widget);

  g_signal_connect (G_OBJECT (cancel_button),
                    "clicked",
                    G_CALLBACK (click_cancel),
                    widget);

  g_signal_connect (G_OBJECT (widget->evaluate_button),
                    "clicked",
                    G_CALLBACK (click_evaluate),
                    widget);

  g_signal_connect (G_OBJECT (widget->entry),
                    "notify::text",
                    G_CALLBACK (notify_entry_text),
                    widget);
}


/* Update the sensitivity of the evaluate button
 */
static void
notify_entry_text (GtkWidget *entry, GParamSpec *pspec, GschemTranslateWidget *widget)
{
  g_return_if_fail (widget != NULL);

  gtk_widget_set_sensitive (widget->evaluate_button,
                            convert_value (gtk_entry_get_text (GTK_ENTRY (widget->entry)), NULL));
}


/* Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemTranslateWidget *widget = GSCHEM_TRANSLATE_WIDGET (object);

  switch (param_id) {
    case PROP_LABEL_TEXT:
      gschem_translate_widget_set_label_text (widget, g_value_get_string (value));
      break;

    case PROP_VALUE:
      gschem_translate_widget_set_value (widget, g_value_get_int (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}
