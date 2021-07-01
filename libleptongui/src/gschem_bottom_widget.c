/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
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
  PROP_RUBBER_BAND_MODE,
  PROP_MAGNETIC_NET_MODE,
  PROP_TOPLEVEL
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

static void
update_rubber_band_label (GschemBottomWidget *widget, GParamSpec *pspec, gpointer unused);

static void
update_magnetic_net_label (GschemBottomWidget *widget, GParamSpec *pspec, gpointer unused);



/* convenience macro - gobject type implementation:
*/
G_DEFINE_TYPE (GschemBottomWidget, gschem_bottom_widget, GTK_TYPE_HBOX);



static gboolean
on_click_rubber_band (GtkWidget* ebox, GdkEvent* e, gpointer data)
{
  GschemBottomWidget* widget = (GschemBottomWidget*) data;
  g_return_val_if_fail (widget != NULL, FALSE);

  GdkEventButton* ebtn = (GdkEventButton*) e;
  if (ebtn->type == GDK_BUTTON_PRESS && ebtn->button == 1)
  {
    gschem_options_cycle_net_rubber_band_mode (widget->toplevel->options);
    return TRUE;
  }

  return FALSE;
}



static gboolean
on_click_magnetic_net (GtkWidget* ebox, GdkEvent* e, gpointer data)
{
  GschemBottomWidget* widget = (GschemBottomWidget*) data;
  g_return_val_if_fail (widget != NULL, FALSE);

  GdkEventButton* ebtn = (GdkEventButton*) e;
  if (ebtn->type == GDK_BUTTON_PRESS && ebtn->button == 1)
  {
    gschem_options_cycle_magnetic_net_mode (widget->toplevel->options);
    return TRUE;
  }

  return FALSE;
}



static gboolean
on_click_grid_size (GtkWidget* ebox, GdkEvent* e, gpointer data)
{
  GschemBottomWidget* widget = (GschemBottomWidget*) data;
  g_return_val_if_fail (widget != NULL, FALSE);

  GdkEventButton* ebtn = (GdkEventButton*) e;
  if (ebtn->type == GDK_BUTTON_PRESS && ebtn->button == 1)
  {
    gschem_options_cycle_grid_mode (widget->toplevel->options);
    return TRUE;
  }

  return FALSE;
}



static gboolean
on_click_snap_info (GtkWidget* ebox, GdkEvent* e, gpointer data)
{
  GschemBottomWidget* widget = (GschemBottomWidget*) data;
  g_return_val_if_fail (widget != NULL, FALSE);

  GdkEventButton* ebtn = (GdkEventButton*) e;
  if (ebtn->type == GDK_BUTTON_PRESS && ebtn->button == 1)
  {
    gschem_options_cycle_snap_mode (widget->toplevel->options);
    return TRUE;
  }

  return FALSE;
}



/*! \brief Dispose of the object
 */
static void
dispose (GObject *object)
{
  /* lastly, chain up to the parent dispose */

  GschemBottomWidgetClass* cls = GSCHEM_BOTTOM_WIDGET_GET_CLASS (object);
  GObjectClass* parent_cls = (GObjectClass*) g_type_class_peek_parent (cls);

  parent_cls->dispose (object);
}



/*! \brief Finalize object
 */
static void
finalize (GObject *object)
{
  /* lastly, chain up to the parent finalize */

  GschemBottomWidgetClass* cls = GSCHEM_BOTTOM_WIDGET_GET_CLASS (object);
  GObjectClass* parent_cls = (GObjectClass*) g_type_class_peek_parent (cls);

  parent_cls->finalize (object);
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
    case PROP_TOPLEVEL:
          g_value_set_pointer (value, widget->toplevel);
          break;

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

    case PROP_RUBBER_BAND_MODE:
      g_value_set_boolean (value, gschem_bottom_widget_get_rubber_band_mode (widget));
      break;

    case PROP_MAGNETIC_NET_MODE:
      g_value_set_boolean (value, gschem_bottom_widget_get_magnetic_net_mode (widget));
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
  G_OBJECT_CLASS (klass)->dispose  = dispose;
  G_OBJECT_CLASS (klass)->finalize = finalize;

  G_OBJECT_CLASS (klass)->get_property = get_property;
  G_OBJECT_CLASS (klass)->set_property = set_property;

  GParamFlags flags = (GParamFlags) (G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);
  GParamSpec* spec  = g_param_spec_pointer ("toplevel", "", "", flags);
  g_object_class_install_property (G_OBJECT_CLASS (klass), PROP_TOPLEVEL, spec);

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_GRID_MODE,
                                   g_param_spec_int ("grid-mode",
                                                     "Grid Mode",
                                                     "Grid Mode",
                                                     0,
                                                     (GRID_MODE_COUNT - 1),
                                                     GRID_MODE_NONE,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_GRID_SIZE,
                                   g_param_spec_int ("grid-size",
                                                     "Grid Size",
                                                     "Grid Size",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_LEFT_BUTTON_TEXT,
                                   g_param_spec_string ("left-button-text",
                                                        "Left Button Text",
                                                        "Left Button Text",
                                                        "none",
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_MIDDLE_BUTTON_TEXT,
                                   g_param_spec_string ("middle-button-text",
                                                        "Middle Button Text",
                                                        "Middle Button Text",
                                                        "none",
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_RIGHT_BUTTON_TEXT,
                                   g_param_spec_string ("right-button-text",
                                                        "Right Button Text",
                                                        "Right Button Text",
                                                        "none",
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_MODE,
                                   g_param_spec_int ("snap-mode",
                                                     "Snap Mode",
                                                     "Snap Mode",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_SNAP_SIZE,
                                   g_param_spec_int ("snap-size",
                                                     "Snap Size",
                                                     "Snap Size",
                                                     G_MININT,
                                                     G_MAXINT,
                                                     0,
                                                     (GParamFlags) (G_PARAM_READWRITE
                                                                    | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_STATUS_TEXT,
                                   g_param_spec_string ("status-text",
                                                        "Status Text",
                                                        "Status Text",
                                                        "none",
                                                        (GParamFlags) (G_PARAM_READWRITE
                                                                       | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_STATUS_TEXT_COLOR,
                                   g_param_spec_boolean ("status-text-color",
                                                         "Status State",
                                                         "Status State",
                                                         FALSE,
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_RUBBER_BAND_MODE,
                                   g_param_spec_boolean ("net-rubber-band-mode",
                                                         "Rubber Band Mode",
                                                         "Rubber Band Mode",
                                                         TRUE,
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_CONSTRUCT)));

  g_object_class_install_property (G_OBJECT_CLASS (klass),
                                   PROP_MAGNETIC_NET_MODE,
                                   g_param_spec_boolean ("magnetic-net-mode",
                                                         "Magnetic Net Mode",
                                                         "Magnetic Net Mode",
                                                         TRUE,
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_CONSTRUCT)));

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



/*! \brief Get the rubber band mode
 *
 *  \param  widget This GschemBottomWidget
 *  \return        The rubber band mode: on (TRUE) or off (FALSE)
 */
gboolean
gschem_bottom_widget_get_rubber_band_mode (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, 0);

  return widget->rubber_band_mode;
}



/*! \brief Get the magnetic net mode
 *
 *  \param  widget This GschemBottomWidget
 *  \return        The magnetic net mode: on (TRUE) or off (FALSE)
 */
gboolean
gschem_bottom_widget_get_magnetic_net_mode (GschemBottomWidget *widget)
{
  g_return_val_if_fail (widget != NULL, 0);

  return widget->magnetic_net_mode;
}



/*! \brief Set grid_snap_widget text
 *
 *  \param  widget  This GschemBottomWidget
 *  \param  str     Text to set. Newly-allocated string. Will be g_free()'d
 */
static void
set_snap_info_widget_text (GschemBottomWidget* widget,
                           gchar* str)
{
  g_return_if_fail (widget != NULL);

  gtk_label_set_markup (GTK_LABEL(widget->grid_snap_widget), str);
  g_free (str);
}



/*! \brief Set grid_snap_widget color
 *
 *  \param  widget      This GschemBottomWidget
 *  \param  color_name  Color name, e.g. "red", "blue"
 */
static void
set_snap_info_widget_color (GschemBottomWidget* widget,
                            const gchar* color_name)
{
  g_return_if_fail (widget != NULL);

#ifdef ENABLE_GTK3
  GdkRGBA color;
  gdk_rgba_parse (&color, color_name);
  gtk_widget_override_color (GTK_WIDGET (widget->grid_snap_widget),
                             GTK_STATE_FLAG_NORMAL,
                             &color);
#else
  GdkColor color;
  gdk_color_parse (color_name, &color);
  gtk_widget_modify_fg (GTK_WIDGET (widget->grid_snap_widget),
                        GTK_STATE_NORMAL,
                        &color);
#endif
}



/*! \brief Set grid_snap_widget text and color
 *
 *  \param  widget      This GschemBottomWidget
 *  \param  str         Text to set. Newly-allocated string. Will be g_free()'d
 *  \param  color_name  Color name, e.g. "red", "blue"
 */
static void
set_snap_info_widget (GschemBottomWidget* widget,
                      gchar* str,
                      const gchar* color_name)
{
  g_return_if_fail (widget != NULL);

  set_snap_info_widget_text (widget, str);
  set_snap_info_widget_color (widget, color_name);
}



/*! \brief Reset grid_snap_widget visual state
 *
 *  \par Function Description
 *  Remove any text decorations, reset text color to black
 *
 *  \param  widget  This GschemBottomWidget
 */
static void
reset_snap_info_widget (GschemBottomWidget* widget)
{
  g_return_if_fail (widget != NULL);

  gchar* txt = g_strdup_printf ("Snap: %d", widget->snap_size);
  set_snap_info_widget (widget, txt, "black");
}



/*! \brief Update info in the grid_snap_widget depending on current snap settings
 *
 *  \param  widget  This GschemBottomWidget
 */
static void
update_snap_info_widget (GschemBottomWidget* widget)
{
  g_return_if_fail (widget != NULL);

  gchar* cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  gint default_snap_size = DEFAULT_SNAP_SIZE;

  if (cfg != NULL)
  {
    GError* err = NULL;
    gint val = eda_config_get_int (cfg,
                                   "schematic.gui",
                                   "snap-size",
                                   &err);
    if (err == NULL)
    {
      default_snap_size = val;
    }

    g_clear_error (&err);
  }


  reset_snap_info_widget (widget);

  gchar* txt = NULL;

  if (widget->snap_mode == SNAP_OFF)
  {
      txt = g_strdup_printf (_("<b>Snap: OFF</b>"));
      set_snap_info_widget (widget, txt, "blue");
  }
  else
  if (widget->snap_mode == SNAP_GRID)
  {
    if (widget->snap_size != default_snap_size)
    {
      txt = g_strdup_printf (_("Snap: <b>%d</b>"), widget->snap_size);
      set_snap_info_widget (widget, txt, "red");
    }
  }
  else
  if (widget->snap_mode == SNAP_RESNAP)
  {
    if (widget->snap_size != default_snap_size)
    {
      txt = g_strdup_printf (_("<u>Re</u>snap: <b>%d</b>"), widget->snap_size);
      set_snap_info_widget(widget, txt, "red");
    }
    else
    {
      txt = g_strdup_printf (_("<u>Re</u>snap: %d"), widget->snap_size);
      set_snap_info_widget_text(widget, txt);
    }
  }
  else
  {
    g_critical ("%s: update_snap_info_widget(): widget->snap_mode out of range: %d\n",
                __FILE__,
                widget->snap_mode);
  }


  if (widget->snap_mode != SNAP_OFF &&
      widget->snap_size != default_snap_size)
  {
    gchar* tooltip = g_strdup_printf(
      _("Snap size.\n"
        "Attention: current snap size (%d) differs\n"
        "from the value set in configuration: %d."),
        widget->snap_size,
        default_snap_size);

    gtk_widget_set_tooltip_text (widget->grid_snap_widget,
                                   tooltip);
    g_free (tooltip);
  }
  else
  {
    gtk_widget_set_tooltip_text (widget->grid_snap_widget,
                                 _("Snap size"));
  }

} /* update_snap_info_widget() */



/*! \brief Create grid_snap_widget
 *
 *  \param  widget  This GschemBottomWidget
 */
static void
create_snap_info_widget (GschemBottomWidget* widget)
{
  g_return_if_fail (widget != NULL);

  widget->grid_snap_widget = gtk_label_new (NULL);

  GtkWidget* ebox = gtk_event_box_new();
  gtk_container_add (GTK_CONTAINER (ebox), widget->grid_snap_widget);
  gtk_widget_show_all (ebox);

  gchar* str = g_strdup_printf ("Snap: <b>%d</b>", widget->snap_size);
  gtk_label_set_markup (GTK_LABEL(widget->grid_snap_widget), str);
  g_free (str);

  gtk_misc_set_padding (GTK_MISC (widget->grid_snap_widget),
                        LABEL_XPAD,
                        LABEL_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), ebox, FALSE, FALSE, 0);

  gtk_box_pack_start (GTK_BOX (widget), gtk_vseparator_new(), FALSE, FALSE, 0);

  g_signal_connect (G_OBJECT (ebox),
                    "button-press-event",
                    G_CALLBACK (&on_click_snap_info),
                    widget);
}



/*! \brief Update info in the grid_size_widget depending on current grid settings
 *
 *  \param  widget  This GschemBottomWidget
 */
static void
update_grid_size_widget (GschemBottomWidget* widget)
{
  g_return_if_fail (widget != NULL);

  gchar* str = NULL;

  if (widget->grid_mode == GRID_MODE_NONE)
  {
    str = g_strdup (_("Grid: OFF"));
  }
  else
  if (widget->grid_size <= 0)
  {
    str = g_strdup (_("Grid: NONE"));
  }
  else
  {
    str = g_strdup_printf ("Grid: %d", widget->grid_size);
  }

  gtk_label_set_label (GTK_LABEL(widget->grid_size_widget), str);
  gtk_widget_set_tooltip_text (widget->grid_size_widget,
                               _("Grid size"));
  g_free (str);
}



/*! \brief Create grid_size_widget
 *
 *  \param  widget  This GschemBottomWidget
 */
static void
create_grid_size_widget (GschemBottomWidget* widget)
{
  g_return_if_fail (widget != NULL);

  widget->grid_size_widget = gtk_label_new (NULL);

  GtkWidget* ebox = gtk_event_box_new();
  gtk_container_add (GTK_CONTAINER (ebox), widget->grid_size_widget);
  gtk_widget_show_all (ebox);

  gchar* str = g_strdup_printf ("Grid: %d", widget->grid_size);
  gtk_label_set_markup (GTK_LABEL(widget->grid_size_widget),
                        str);
  g_free (str);

  gtk_misc_set_padding (GTK_MISC (widget->grid_size_widget),
                        LABEL_XPAD,
                        LABEL_YPAD);
  gtk_box_pack_start (GTK_BOX (widget), ebox, FALSE, FALSE, 0);

  gtk_box_pack_start (GTK_BOX (widget), gtk_vseparator_new(), FALSE, FALSE, 0);

  g_signal_connect (G_OBJECT (ebox),
                    "button-press-event",
                    G_CALLBACK (&on_click_grid_size),
                    widget);
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
  gtk_widget_set_tooltip_text (widget->left_button_label, _("Left mouse button"));
  gtk_misc_set_padding (GTK_MISC (widget->left_button_label), LABEL_XPAD, LABEL_YPAD);

  widget->middle_button_label = gtk_label_new (NULL);
  gtk_widget_set_tooltip_text (widget->middle_button_label, _("Middle mouse button"));
  gtk_misc_set_padding (GTK_MISC (widget->middle_button_label), LABEL_XPAD, LABEL_YPAD);

  widget->right_button_label = gtk_label_new (NULL);
  gtk_widget_set_tooltip_text (widget->right_button_label, _("Right mouse button"));
  gtk_misc_set_padding (GTK_MISC (widget->right_button_label), LABEL_XPAD, LABEL_YPAD);


  /* default values for configuration settings: */
  gboolean show_mouse_indicators       = TRUE;
  gboolean show_rubber_band_indicator  = FALSE;
  gboolean show_magnetic_net_indicator = FALSE;
#ifdef ENABLE_GTK3
  gdk_rgba_parse (&widget->status_inactive_color, "black");
  gdk_rgba_parse (&widget->status_active_color, "green");
#else
  gdk_color_parse ("black", &widget->status_inactive_color);
  gdk_color_parse ("green", &widget->status_active_color);
#endif
  widget->status_bold_font = FALSE;

  gchar* cwd = g_get_current_dir();
  EdaConfig* cfg = eda_config_get_context_for_path (cwd);
  g_free (cwd);

  if (cfg != NULL)
  {
    GError*  err = NULL;
    gboolean val = FALSE;

    /* mouse indicators: */
    val = eda_config_get_boolean (cfg,
                                  "schematic.status-bar",
                                  "show-mouse-buttons",
                                  &err);
    if (err == NULL)
      show_mouse_indicators = val;
    g_clear_error (&err);

    /* net rubber band indicator: */
    val = eda_config_get_boolean (cfg,
                                  "schematic.status-bar",
                                  "show-rubber-band",
                                  &err);
    if (err == NULL)
      show_rubber_band_indicator = val;
    g_clear_error (&err);

    /* magnetic net indicator: */
    val = eda_config_get_boolean (cfg,
                                  "schematic.status-bar",
                                  "show-magnetic-net",
                                  &err);
    if (err == NULL)
      show_magnetic_net_indicator = val;
    g_clear_error (&err);

    /* status line active color: */
    gchar* color = eda_config_get_string (cfg,
                                          "schematic.status-bar",
                                          "status-active-color",
                                          &err);
    if (color != NULL)
    {
#ifdef ENABLE_GTK3
      gdk_rgba_parse (&widget->status_active_color, color);
#else
      gdk_color_parse (color, &widget->status_active_color);
#endif
      g_free (color);
    }
    g_clear_error (&err);

    /* status line bold font: */
    val = eda_config_get_boolean (cfg,
                                  "schematic.status-bar",
                                  "status-bold-font",
                                  &err);
    if (err == NULL)
      widget->status_bold_font = val;
    g_clear_error (&err);
  }


  if (show_mouse_indicators)
  {
    gtk_box_pack_start (GTK_BOX (widget), widget->left_button_label, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (widget), gtk_vseparator_new(), FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (widget), widget->middle_button_label, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (widget), gtk_vseparator_new(), FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (widget), widget->right_button_label, FALSE, FALSE, 0);
    gtk_box_pack_start (GTK_BOX (widget), gtk_vseparator_new(), FALSE, FALSE, 0);
  }


  create_snap_info_widget (widget);
  create_grid_size_widget (widget);


  separator = gtk_vseparator_new ();
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);


  widget->rubber_band_label = gtk_label_new (NULL);
  gtk_widget_set_tooltip_text (widget->rubber_band_label, _("Net rubber band mode"));
  gtk_misc_set_padding (GTK_MISC (widget->rubber_band_label), LABEL_XPAD, LABEL_YPAD);
  if (show_rubber_band_indicator)
  {
    GtkWidget* ebox_rubber_band = gtk_event_box_new();
    gtk_container_add (GTK_CONTAINER (ebox_rubber_band), widget->rubber_band_label);
    gtk_widget_show_all (ebox_rubber_band);
    gtk_box_pack_start (GTK_BOX (widget), ebox_rubber_band, FALSE, FALSE, 0);

    g_signal_connect (G_OBJECT (ebox_rubber_band),
                      "button-press-event",
                      G_CALLBACK (&on_click_rubber_band),
                      widget);
  }


  separator = gtk_vseparator_new ();
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);


  widget->magnetic_net_label = gtk_label_new (NULL);
  gtk_widget_set_tooltip_text (widget->magnetic_net_label, _("Magnetic net mode"));
  gtk_misc_set_padding (GTK_MISC (widget->magnetic_net_label), LABEL_XPAD, LABEL_YPAD);
  if (show_magnetic_net_indicator)
  {
    GtkWidget* ebox_magnetic_net = gtk_event_box_new();
    gtk_container_add (GTK_CONTAINER (ebox_magnetic_net), widget->magnetic_net_label);
    gtk_widget_show_all (ebox_magnetic_net);
    gtk_box_pack_start (GTK_BOX (widget), ebox_magnetic_net, FALSE, FALSE, 0);

    g_signal_connect (G_OBJECT (ebox_magnetic_net),
                      "button-press-event",
                      G_CALLBACK (&on_click_magnetic_net),
                      widget);
  }


  widget->status_label = gtk_label_new (NULL);
  gtk_widget_set_tooltip_text (widget->status_label, _("Current action mode"));
  gtk_misc_set_padding (GTK_MISC (widget->status_label), LABEL_XPAD, LABEL_YPAD);
  gtk_box_pack_end (GTK_BOX (widget), widget->status_label, FALSE, FALSE, 0);

  separator = gtk_vseparator_new ();
  gtk_box_pack_start (GTK_BOX (widget), separator, FALSE, FALSE, 0);

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

  g_signal_connect (G_OBJECT (widget),
                    "notify::net-rubber-band-mode",
                    G_CALLBACK (update_rubber_band_label),
                    NULL);

  g_signal_connect (G_OBJECT (widget),
                    "notify::magnetic-net-mode",
                    G_CALLBACK (update_magnetic_net_label),
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

#ifdef ENABLE_GTK3
  const GdkRGBA* color = NULL;
#else
  const GdkColor* color = NULL;
#endif

  if (active) {
    color = &widget->status_active_color;
  } else {
    color = &widget->status_inactive_color;
  }

#ifdef ENABLE_GTK3
  gtk_widget_override_color (GTK_WIDGET (widget->status_label),
                             GTK_STATE_FLAG_NORMAL,
                             color);
#else
  gtk_widget_modify_fg (GTK_WIDGET (widget->status_label),
                        GTK_STATE_NORMAL,
                        color);
#endif
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

  gchar* str = g_markup_printf_escaped (widget->status_bold_font
                                        ? "<b>%s</b>"
                                        : "%s",
                                        text);

  gtk_label_set_markup (GTK_LABEL (widget->status_label), str);
  g_free (str);

  g_object_notify (G_OBJECT (widget), "status-text");
}



/*! \brief Set the rubber band mode
 *
 *  \param [in] widget  This GschemBottomWidget
 *  \param [in] mode    The rubber band mode: on (TRUE) or off (FALSE)
 */
void
gschem_bottom_widget_set_rubber_band_mode (GschemBottomWidget *widget, gboolean mode)
{
  g_return_if_fail (widget != NULL);

  widget->rubber_band_mode = mode;

  g_object_notify (G_OBJECT (widget), "net-rubber-band-mode");
}



/*! \brief Set the magnetic net mode
 *
 *  \param [in] widget  This GschemBottomWidget
 *  \param [in] mode    The magnetic net mode: on (TRUE) or off (FALSE)
 */
void
gschem_bottom_widget_set_magnetic_net_mode (GschemBottomWidget *widget, gboolean mode)
{
  g_return_if_fail (widget != NULL);

  widget->magnetic_net_mode = mode;

  g_object_notify (G_OBJECT (widget), "magnetic-net-mode");
}



/*! \brief Set a gobject property
 */
static void
set_property (GObject *object, guint param_id, const GValue *value, GParamSpec *pspec)
{
  GschemBottomWidget *widget = GSCHEM_BOTTOM_WIDGET (object);

  switch (param_id) {
    case PROP_TOPLEVEL:
          widget->toplevel = GSCHEM_TOPLEVEL (g_value_get_pointer (value));
          break;

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

    case PROP_RUBBER_BAND_MODE:
      gschem_bottom_widget_set_rubber_band_mode (widget, g_value_get_boolean (value));
      break;

    case PROP_MAGNETIC_NET_MODE:
      gschem_bottom_widget_set_magnetic_net_mode (widget, g_value_get_boolean (value));
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, param_id, pspec);
  }
}



/*! \brief Write the snap and grid settings to the status bar.
 *
 *  \param [in] widget This GschemBottomWidget
 *  \param [in] pspec  The parameter that changed
 *  \param [in] unused
 */
static void
update_grid_label (GschemBottomWidget *widget, GParamSpec *pspec, gpointer unused)
{
  update_snap_info_widget (widget);
  update_grid_size_widget (widget);
}



/*! \brief Display the rubber band indicator in the status bar
 *
 *  \param [in] widget This GschemBottomWidget
 *  \param [in] pspec  The parameter that changed
 *  \param [in] unused
 */
static void
update_rubber_band_label (GschemBottomWidget *widget, GParamSpec *pspec, gpointer unused)
{
  g_return_if_fail (widget != NULL);

#ifdef ENABLE_GTK3
  GdkRGBA color;
  gdk_rgba_parse (&color, widget->rubber_band_mode ? "green" : "blue");
  gtk_widget_override_color (GTK_WIDGET (widget->rubber_band_label),
                             GTK_STATE_FLAG_NORMAL,
                             &color);
#else
  GdkColor color;
  gdk_color_parse (widget->rubber_band_mode ? "green" : "blue", &color);
  gtk_widget_modify_fg (GTK_WIDGET (widget->rubber_band_label),
                        GTK_STATE_NORMAL,
                        &color);
#endif

  gtk_label_set_markup (GTK_LABEL (widget->rubber_band_label),
                        widget->rubber_band_mode ?
                        "RB: <b>ON</b>" : "RB: off");
}



/*! \brief Display the magnetic net indicator in the status bar
 *
 *  \param [in] widget This GschemBottomWidget
 *  \param [in] pspec  The parameter that changed
 *  \param [in] unused
 */
static void
update_magnetic_net_label (GschemBottomWidget *widget, GParamSpec *pspec, gpointer unused)
{
  g_return_if_fail (widget != NULL);

#ifdef ENABLE_GTK3
  GdkRGBA color;
  gdk_rgba_parse (&color, widget->magnetic_net_mode ? "purple" : "darkgray");
  gtk_widget_override_color (GTK_WIDGET (widget->magnetic_net_label),
                             GTK_STATE_FLAG_NORMAL,
                             &color);
#else
  GdkColor color;
  gdk_color_parse (widget->magnetic_net_mode ? "purple" : "darkgray", &color);
  gtk_widget_modify_fg (GTK_WIDGET (widget->magnetic_net_label),
                        GTK_STATE_NORMAL,
                        &color);
#endif

  gtk_label_set_markup (GTK_LABEL (widget->magnetic_net_label),
                    widget->magnetic_net_mode ?
                    "MN: <b>ON</b>" : "MN: off");
}
