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
 * \file gschem_bottom_widget.c
 *
 * \brief A widget for the "status bar" at the bottom of the window
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_MATH_H
#include <math.h>
#endif

#include "gschem.h"


/*  The padding used around labels in the "status bar"
 */

#define LABEL_XPAD 10
#define LABEL_YPAD 5



enum
{
  PROP_0,
  PROP_GRID_MODE,
  PROP_GRID_SIZE,
  PROP_LEFT_BUTTON_TEXT,
  PROP_MIDDLE_BUTTON_TEXT,
  PROP_RIGHT_BUTTON_TEXT,
  PROP_SNAP_MODE,
  PROP_SNAP_SIZE,
  PROP_STATUS_TEXT,
  PROP_STATUS_TEXT_COLOR,
};



static void
dispose (GObject *object);

static void
finalize (GObject *object);

static void
get_property (GObject *object, guint param_id, GValue *value, GParamSpec *pspec);

static void
gschem_bottom_widget_class_init (GschemBottomWidgetClass *klass);

static void
gschem_bottom_widget_init (GschemBottomWidget *view);

static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec);

static void
update_grid_label (GschemBottomWidget *widget, GParamSpec *pspec, gpointer unused);


static GObjectClass *gschem_bottom_widget_parent_class = NULL;



/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  /* lastly, chain up to the parent dispose */

  g_return_if_fail (gschem_bottom_widget_parent_class != NULL);
  gschem_bottom_widget_parent_class->dispose (object);
}



/*! \brief Finalize object
 */
static void
finalize (GObject *object)
{
  /* lastly, chain up to the parent finalize */

  g_return_if_fail (gschem_bottom_widget_parent_class != NULL);
  gschem_bottom_widget_parent_class->finalize (object);
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
  GschemBottomWidget *widget = GSCHEM_BOTTOM_WIDGET (object);

  switch (param_id) {
    case PROP_GRID_MODE:
      g_value_set_int (value, gschem_bottom_widget_get_grid_mode (widget));
      break;

    case PROP_GRID_SIZE:
      g_value_set_int (value, gschem_bottom_widget_get_grid_size (widget));
      break;

    case PROP_LEFT_BUTTON_TEXT:
      g_value_set_string (value, gschem_bottom_widget_get_left_button_text (widget));
      break;

    case PROP_MIDDLE_BUTTON_TEXT:
      g_value_set_string (value, gschem_bottom_widget_get_middle_button_text (widget));
      break;

    case PROP_RIGHT_BUTTON_TEXT:
      g_value_set_string (value, gschem_bottom_widget_get_right_button_text (widget));
      break;

    case PROP_SNAP_MODE:
      g_value_set_int (value, gschem_bottom_widget_get_snap_mode (widget));
      break;

    case PROP_SNAP_SIZE:
      g_value_set_int (value, gschem_bottom_widget_get_snap_size (widget));
      break;

    case PROP_STATUS_TEXT:
      g_value_set_string (value, gschem_bottom_widget_get_status_text (widget));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Initialize GschemBottomWidget class
 *
 *  \param [in] klass The class for the GschemBottomWidget
 */
static void
gschem_bottom_widget_class_init (GschemBottomWidgetClass *klass)
{
  gschem_bottom_widget_parent_class = G_OBJECT_CLASS (g_type_class_peek_parent (klass));

  G_OBJECT_CLASS (klass)->dispose  = dispose;
  G_OBJECT_CLASS (klass)->finalize = finalize;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_GRID_MODE,
                                   g_param_spec_int ("grid-mode",
                                                     "Grid Mode",
                                                     "Grid Mode",
                                                     0,
                                                     (GRID_MODE_COUNT - 1),
                                                     GRID_MODE_NONE,
                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_GRID_SIZE,
                                   g_param_spec_int ("grid-size",
                                                     "Grid Size",
                                                     "Grid Size",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LEFT_BUTTON_TEXT,
                                   g_param_spec_string ("left-button-text",
                                                        "Left Button Text",
                                                        "Left Button Text",
                                                        "none",
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_MIDDLE_BUTTON_TEXT,
                                   g_param_spec_string ("middle-button-text",
                                                        "Middle Button Text",
                                                        "Middle Button Text",
                                                        "none",
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_RIGHT_BUTTON_TEXT,
                                   g_param_spec_string ("right-button-text",
                                                        "Right Button Text",
                                                        "Right Button Text",
                                                        "none",
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_MODE,
                                   g_param_spec_int ("snap-mode",
                                                     "Snap Mode",
                                                     "Snap Mode",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_SIZE,
                                   g_param_spec_int ("snap-size",
                                                     "Snap Size",
                                                     "Snap Size",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_STATUS_TEXT,
                                   g_param_spec_string ("status-text",
                                                        "Status Text",
                                                        "Status Text",
                                                        "none",
                                                        G_PARAM_READWRITE | G_PARAM_CONSTRUCT));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_STATUS_TEXT_COLOR,
                                   g_param_spec_boolean ("status-text-color",
                                                         "Status State",
                                                         "Status State",
                                                         FALSE,
                                                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT));
}



/*! \brief Get the grid mode
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The grid mode
 */
int
gschem_bottom_widget_get_grid_mode (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, 0);

  return widget->grid_mode;
}



/*! \brief Get the grid size
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The grid size
 */
int
gschem_bottom_widget_get_grid_size (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, 0);

  return widget->grid_size;
}



/*! \brief Get the left button text
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The left button text
 */
const char*
gschem_bottom_widget_get_left_button_text (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_label_get_text (GTK_LABEL (widget->left_button_label));
}



/*! \brief Get the middle button text
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The middle button text
 */
const char*
gschem_bottom_widget_get_middle_button_text (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_label_get_text (GTK_LABEL (widget->middle_button_label));
}



/*! \brief Get the right button text
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The right button text
 */
const char*
gschem_bottom_widget_get_right_button_text (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_label_get_text (GTK_LABEL (widget->right_button_label));
}



/*! \brief Get the snap mode
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The snap mode
 */
int
gschem_bottom_widget_get_snap_mode (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, 0);

  return widget->snap_mode;
}



/*! \brief Get the snap size
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The snap size
 */
int
gschem_bottom_widget_get_snap_size (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, 0);

  return widget->snap_size;
}



/*! \brief Get the status text
 *
 *  \param [in] widget This GschemBottomWidget
 *  \return The status text
 */
const char*
gschem_bottom_widget_get_status_text (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, NULL);

  return gtk_label_get_text (GTK_LABEL (widget->status_label));
}



/*! \brief Get/register GschemBottomWidget type.
 */
GType
gschem_bottom_widget_get_type ()
{
  static GType type = 0;

  if (type == 0) {
    static const GTypeInfo info = {
      sizeof(GschemBottomWidgetClass),
      NULL,                                                    /* base_init */
      NULL,                                                    /* base_finalize */
      (GClassInitFunc) gschem_bottom_widget_class_init,
      NULL,                                                    /* class_finalize */
      NULL,                                                    /* class_data */
      sizeof(GschemBottomWidget),
      0,                                                       /* n_preallocs */
      (GInstanceInitFunc) gschem_bottom_widget_init,
    };

    type = g_type_register_static (GTK_TYPE_HBOX, "GschemBottomWidget", &info, 0);
  }

  return type;
}



/*! \brief Initialize GschemBottomWidget instance
 *
 *  \param [in,out] view the gschem page view
 */
static void
gschem_bottom_widget_init (GschemBottomWidget *widget)
{
  GtkWidget *separator;

  g_return_if_fail (widget != NULL);

  widget->left_button_label = gtk_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->left_button_label), LABEL_XPAD, LABEL_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), widget->left_button_label, FALSE, FALSE, 0);

  separator = gtk_vseparator_new ();
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  widget->middle_button_label = gtk_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->middle_button_label), LABEL_XPAD, LABEL_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), widget->middle_button_label, FALSE, FALSE, 0);

  separator = gtk_vseparator_new ();
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  widget->right_button_label = gtk_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->right_button_label), LABEL_XPAD, LABEL_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), widget->right_button_label, FALSE, FALSE, 0);

  separator = gtk_vseparator_new ();
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  widget->grid_label = gtk_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->grid_label), LABEL_XPAD, LABEL_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), widget->grid_label, FALSE, FALSE, 0);

  separator = gtk_vseparator_new ();
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

  widget->status_label = gtk_label_new (NULL);
  gtk_misc_set_padding (GTK_MISC (widget->status_label), LABEL_XPAD, LABEL_YPAD);
  gtk_box_pack_end (GTK_BOX (widget), widget->status_label, FALSE, FALSE, 0);

  g_signal_connect (G_OBJECT (widget),
                    "notify::grid-mode",
                    G_CALLBACK (update_grid_label),
                    NULL);

  g_signal_connect (G_OBJECT (widget),
                    "notify::grid-size",
                    G_CALLBACK (update_grid_label),
                    NULL);

  g_signal_connect (G_OBJECT (widget),
                    "notify::snap-mode",
                    G_CALLBACK (update_grid_label),
                    NULL);

  g_signal_connect (G_OBJECT (widget),
                    "notify::snap-size",
                    G_CALLBACK (update_grid_label),
                    NULL);
}



/*! \brief Set the grid mode
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     size The grid mode
 */
void
gschem_bottom_widget_set_grid_mode (GschemBottomWidget *widget, int mode)
{
  g_return_if_fail (widget != NULL);

  widget->grid_mode = mode;

  g_object_notify (G_OBJECT (widget), "grid-mode");
}



/*! \brief Set the grid size
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     size The grid size
 */
void
gschem_bottom_widget_set_grid_size (GschemBottomWidget *widget, int size)
{
  g_return_if_fail (widget != NULL);

  widget->grid_size = size;

  g_object_notify (G_OBJECT (widget), "grid-size");
}



/*! \brief Set the left button text
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     text The text
 */
void
gschem_bottom_widget_set_left_button_text (GschemBottomWidget *widget, const char *text)
{
  g_return_if_fail (widget != NULL);

  gtk_label_set_text (GTK_LABEL (widget->left_button_label), text);

  g_object_notify (G_OBJECT (widget), "left-button-text");
}



/*! \brief Set the middle button text
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     text The text
 */
void
gschem_bottom_widget_set_middle_button_text (GschemBottomWidget *widget, const char *text)
{
  g_return_if_fail (widget != NULL);

  gtk_label_set_text (GTK_LABEL (widget->middle_button_label), text);

  g_object_notify (G_OBJECT (widget), "middle-button-text");
}



/*! \brief Set the right button text
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     text The text
 */
void
gschem_bottom_widget_set_right_button_text (GschemBottomWidget *widget, const char *text)
{
  g_return_if_fail (widget != NULL);

  gtk_label_set_text (GTK_LABEL (widget->right_button_label), text);

  g_object_notify (G_OBJECT (widget), "right-button-text");
}



/*! \brief Set the snap mode
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     size The snap mode
 */
void
gschem_bottom_widget_set_snap_mode (GschemBottomWidget *widget, int mode)
{
  g_return_if_fail (widget != NULL);

  widget->snap_mode = mode;

  g_object_notify (G_OBJECT (widget), "snap-mode");
}



/*! \brief Set the snap size
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     size The snap size
 */
void
gschem_bottom_widget_set_snap_size (GschemBottomWidget *widget, int size)
{
  g_return_if_fail (widget != NULL);

  widget->snap_size = size;

  g_object_notify (G_OBJECT (widget), "snap-size");
}


/*! \brief Set the status text color
 *
 *  \par Function Description
 *  Changes the status text color to show if the current editing
 *  action is active or not.
 *
 *  \param [in,out] view    This GschemBottomWidget
 *  \param [in]     active  The state to visualise
 */
void
gschem_bottom_widget_set_status_text_color (GschemBottomWidget *widget, gboolean active)
{
  g_return_if_fail (widget != NULL);

  GdkColor color;

  if (active) {
    gdk_color_parse ("green", &color);
  } else {
    gdk_color_parse ("black", &color);
  }

  gtk_widget_modify_fg (GTK_WIDGET (widget->status_label), GTK_STATE_NORMAL, &color);
}


/*! \brief Set the status text
 *
 *  \param [in,out] view This GschemBottomWidget
 *  \param [in]     text The status text
 */
void
gschem_bottom_widget_set_status_text (GschemBottomWidget *widget, const char *text)
{
  g_return_if_fail (widget != NULL);

  gtk_label_set_text (GTK_LABEL (widget->status_label), text);

  g_object_notify (G_OBJECT (widget), "status-text");
}



/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemBottomWidget *widget = GSCHEM_BOTTOM_WIDGET (object);

  switch (param_id) {
    case PROP_GRID_MODE:
      gschem_bottom_widget_set_grid_mode (widget, g_value_get_int (value));
      break;

    case PROP_GRID_SIZE:
      gschem_bottom_widget_set_grid_size (widget, g_value_get_int (value));
      break;

    case PROP_LEFT_BUTTON_TEXT:
      gschem_bottom_widget_set_left_button_text (widget, g_value_get_string (value));
      break;

    case PROP_MIDDLE_BUTTON_TEXT:
      gschem_bottom_widget_set_middle_button_text (widget, g_value_get_string (value));
      break;

    case PROP_RIGHT_BUTTON_TEXT:
      gschem_bottom_widget_set_right_button_text (widget, g_value_get_string (value));
      break;

    case PROP_SNAP_MODE:
      gschem_bottom_widget_set_snap_mode (widget, g_value_get_int (value));
      break;

    case PROP_SNAP_SIZE:
      gschem_bottom_widget_set_snap_size (widget, g_value_get_int (value));
      break;

    case PROP_STATUS_TEXT:
      gschem_bottom_widget_set_status_text (widget, g_value_get_string (value));
      break;

    case PROP_STATUS_TEXT_COLOR:
      gschem_bottom_widget_set_status_text_color (widget, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Write the grid settings to the gschem "status bar."
 *
 *  \param [in] widget This GschemBottomWidget
 *  \param [in] pspec  The parameter that changed
 *  \param [in] unused
 */
static void
update_grid_label (GschemBottomWidget *widget, GParamSpec *pspec, gpointer unused)
{
  if (widget->grid_label != NULL) {
    gchar *grid_text = NULL;
    gchar *label_text = NULL;
    gchar *snap_text = NULL;

    switch (widget->snap_mode) {
      case SNAP_OFF:
        snap_text = g_strdup (_("OFF"));
        break;

      case SNAP_GRID:
        snap_text = g_strdup_printf ("%d", widget->snap_size);
        break;

      case SNAP_RESNAP:
        snap_text = g_strdup_printf ("%dR", widget->snap_size);
        break;

      default:
        g_critical ("%s: update_grid_label(): widget->snap_mode out of range: %d\n", __FILE__, widget->snap_mode);
    }

    if (widget->grid_mode == GRID_MODE_NONE) {
      grid_text = g_strdup (_("OFF"));
    } else {
      if (widget->grid_size <= 0) {
        grid_text = g_strdup (_("NONE"));
      } else {
        grid_text = g_strdup_printf ("%d", widget->grid_size);
      }
    }

    label_text = g_strdup_printf (_("Grid(%1$s, %2$s)"), snap_text, grid_text);

    gtk_label_set_text (GTK_LABEL (widget->grid_label), label_text);

    g_free (grid_text);
    g_free (label_text);
    g_free (snap_text);
  }
}
