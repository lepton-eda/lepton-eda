/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
 *
 * Code based on GTK 2.14.5 gtk/gtkaccellabel.c (LGPL)
 *
 * GTK - The GIMP Toolkit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * SchematicAccelLabel: GtkLabel with accelerator monitoring facilities.
 * Copyright (C) 1998 Tim Janik
 *
 * Modified by the GTK+ Team and others 1997-2001.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 *
 *  Adapted for gEDA by Peter Clifton <pcjc2@cam.ac.uk>
 *
 *  THIS FILE IS LGPL LICENSED, gEDA AS A WHOLE IS GPL LICENSED
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */

#include "config.h"

#include "schematic.h"

#define P_(x) (x)

#ifdef ENABLE_GTK3

enum {
  PROP_0,
  PROP_ACCEL_WIDGET,
  PROP_ACCEL_STRING,
};

struct _SchematicAccelLabelPrivate
{
  GtkWidget     *accel_widget;       /* done */
  gchar         *accel_string;       /* has set function */
  guint          accel_padding;      /* should be style property? */
  guint16        accel_string_width; /* seems to be private */
};

G_DEFINE_TYPE_WITH_PRIVATE (SchematicAccelLabel, schematic_accel_label, GTK_TYPE_LABEL)

static void
schematic_accel_label_init (SchematicAccelLabel *accel_label)
{
  SchematicAccelLabelPrivate *priv;

  accel_label->priv = (SchematicAccelLabelPrivate*) schematic_accel_label_get_instance_private (accel_label);
  priv = accel_label->priv;
  priv->accel_padding = 3;
  priv->accel_widget = NULL;
  priv->accel_string = NULL;
}

gboolean
schematic_accel_label_refetch (SchematicAccelLabel *accel_label)
{
  gboolean enable_accels;

  g_return_val_if_fail (SCHEMATIC_IS_ACCEL_LABEL (accel_label), FALSE);

  g_object_get (gtk_widget_get_settings (GTK_WIDGET (accel_label)),
                "gtk-enable-accels", &enable_accels,
                NULL);

  if (!enable_accels || accel_label->priv->accel_string == NULL) {
    if (accel_label->priv->accel_string != NULL)
      g_free (accel_label->priv->accel_string);

    accel_label->priv->accel_string = g_strdup ("");
  }

  gtk_widget_queue_resize (GTK_WIDGET (accel_label));

  return FALSE;
}


static const gchar *
schematic_accel_label_get_string (SchematicAccelLabel *accel_label)
{
  if (!accel_label->priv->accel_string)
    schematic_accel_label_refetch (accel_label);

  return accel_label->priv->accel_string;
}


static void
schematic_accel_label_set_property (GObject      *object,
                                    guint         prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
  SchematicAccelLabel  *accel_label;

  accel_label = SCHEMATIC_ACCEL_LABEL (object);

  switch (prop_id) {
    /* Dummy properties from GtkAccelLabel */
    case PROP_ACCEL_WIDGET:
      break;

    case PROP_ACCEL_STRING:
      schematic_accel_label_set_accel_string (accel_label, g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
schematic_accel_label_get_property (GObject    *object,
                                    guint       prop_id,
                                    GValue     *value,
                                    GParamSpec *pspec)
{
  SchematicAccelLabel  *accel_label;

  accel_label = SCHEMATIC_ACCEL_LABEL (object);

  switch (prop_id) {
    /* Dummy property from GtkAccelLabel */
    case PROP_ACCEL_WIDGET:
      g_value_set_object (value, NULL);
      break;

    case PROP_ACCEL_STRING:
      g_value_set_string (value, accel_label->priv->accel_string);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
schematic_accel_label_finalize (GObject *object)
{
  SchematicAccelLabel *accel_label = SCHEMATIC_ACCEL_LABEL (object);

  g_free (accel_label->priv->accel_string);

  G_OBJECT_CLASS (schematic_accel_label_parent_class)->finalize (object);
}

guint
schematic_accel_label_get_accel_width (SchematicAccelLabel *accel_label)
{
  g_return_val_if_fail (SCHEMATIC_IS_ACCEL_LABEL (accel_label), 0);

  return (accel_label->priv->accel_string_width +
          (accel_label->priv->accel_string_width ? accel_label->priv->accel_padding : 0));
}

static PangoLayout*
schematic_accel_label_get_accel_layout (SchematicAccelLabel *accel_label)
{
  GtkWidget *widget = GTK_WIDGET (accel_label);
  PangoLayout *layout;

  layout = gtk_widget_create_pango_layout (widget, schematic_accel_label_get_string (accel_label));

  return layout;
}

static gint
get_first_baseline (PangoLayout *layout)
{
  PangoLayoutIter *iter;
  gint result;

  iter = pango_layout_get_iter (layout);
  result = pango_layout_iter_get_baseline (iter);
  pango_layout_iter_free (iter);

  return PANGO_PIXELS (result);
}

static gboolean
schematic_accel_label_draw (GtkWidget *widget,
                            cairo_t *cr)
{
  SchematicAccelLabel *accel_label = SCHEMATIC_ACCEL_LABEL (widget);
  guint ac_width;
  GtkAllocation allocation;
  GtkRequisition requisition;

  GTK_WIDGET_CLASS (schematic_accel_label_parent_class)->draw (widget, cr);

  ac_width = schematic_accel_label_get_accel_width (accel_label);

  gtk_widget_get_allocation (widget, &allocation);
  gtk_widget_get_preferred_size (widget, NULL, &requisition);

  if (allocation.width >= requisition.width + (gint) ac_width) {
    GtkStyleContext *context;
    PangoLayout *label_layout;
    PangoLayout *accel_layout;
    gint x;
    gint y;

    label_layout = gtk_label_get_layout (GTK_LABEL (accel_label));
    accel_layout = schematic_accel_label_get_accel_layout (accel_label);

    if (gtk_widget_get_direction (widget) == GTK_TEXT_DIR_RTL) {
      x = 0;
    } else {
      x = gtk_widget_get_allocated_width (widget) - ac_width;
    }

    gtk_label_get_layout_offsets (GTK_LABEL (accel_label), NULL, &y);

    y += get_first_baseline (label_layout) - get_first_baseline (accel_layout) - allocation.y;

    context = gtk_widget_get_style_context (widget);
    gtk_render_layout (context, cr, x, y, accel_layout);

    g_object_unref (accel_layout);
  }

  return FALSE;
}

/* Underscores in key names are better displayed as spaces
 * E.g., Page_Up should be "Page Up"
 */
static void
substitute_underscores (char *str)
{
  char *p;

  for (p = str; *p; p++)
    if (*p == '_')
      *p = ' ';
}


/**
 * schematic_accel_label_set_accel_string:
 * \param accel_label a #SchematicAccelLabel
 * \param accel_string the accelerator string.
 *
 * Sets the accelerator string for this accelerator label.
 **/
void
schematic_accel_label_set_accel_string (SchematicAccelLabel *accel_label,
                                        const gchar *accel_string)
{
  g_return_if_fail (SCHEMATIC_IS_ACCEL_LABEL (accel_label));

  if (accel_label->priv->accel_string)
    g_free (accel_label->priv->accel_string);

  if (accel_string) {
    accel_label->priv->accel_string = g_strdup (accel_string);
    substitute_underscores (accel_label->priv->accel_string);
  } else {
    accel_label->priv->accel_string = NULL;
  }

  g_object_notify (G_OBJECT (accel_label), "accel-string");
}

static void
schematic_accel_label_get_preferred_width (GtkWidget *widget,
                                           gint *minimal_width,
                                           gint *natural_width)
{
  SchematicAccelLabel *accel_label = SCHEMATIC_ACCEL_LABEL (widget);
  PangoLayout   *layout;
  gint           width;
  GTK_WIDGET_CLASS (schematic_accel_label_parent_class)->get_preferred_width (widget,
                                                                              minimal_width,
                                                                              natural_width);
  layout = schematic_accel_label_get_accel_layout (accel_label);
  pango_layout_get_pixel_size (layout, &width, NULL);
  accel_label->priv->accel_string_width = width;
  g_object_unref (layout);
}


static void
schematic_accel_label_class_init (SchematicAccelLabelClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gobject_class->finalize = schematic_accel_label_finalize;
  gobject_class->set_property = schematic_accel_label_set_property;
  gobject_class->get_property = schematic_accel_label_get_property;

  widget_class->get_preferred_width = schematic_accel_label_get_preferred_width;
  widget_class->draw = schematic_accel_label_draw;

  g_object_class_install_property (gobject_class,
                                   PROP_ACCEL_WIDGET,
                                   g_param_spec_object ("accel-widget",
                                                        P_("Accelerator Widget"),
                                                        P_("The widget to be monitored for accelerator changes"),
                                                        GTK_TYPE_WIDGET,
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
                                   PROP_ACCEL_STRING,
                                   g_param_spec_string ("accel-string",
                                                        P_("Accelerator String"),
                                                        P_("The accelerator string to be displayed"),
                                                        NULL,
                                                        G_PARAM_READWRITE));
}

#else /* GTK2 */

enum {
  PROP_0,
  PROP_ACCEL_CLOSURE,
  PROP_ACCEL_WIDGET,
  PROP_ACCEL_STRING,
};

G_DEFINE_TYPE (SchematicAccelLabel,
               schematic_accel_label,
               GTK_TYPE_ACCEL_LABEL)

gboolean
schematic_accel_label_refetch (SchematicAccelLabel *accel_label)
{
  gboolean enable_accels;

  g_return_val_if_fail (SCHEMATIC_IS_ACCEL_LABEL (accel_label), FALSE);

  g_object_get (gtk_widget_get_settings (GTK_WIDGET (accel_label)),
                "gtk-enable-accels", &enable_accels,
                NULL);

  if (!enable_accels || accel_label->accel_string == NULL) {
    if (accel_label->accel_string != NULL)
      g_free (accel_label->accel_string);

    accel_label->accel_string = g_strdup ("");
  }

  gtk_widget_queue_resize (GTK_WIDGET (accel_label));

  return FALSE;
}


static const gchar *
schematic_accel_label_get_string (SchematicAccelLabel *accel_label)
{
  if (!accel_label->accel_string)
    schematic_accel_label_refetch (accel_label);

  return accel_label->accel_string;
}


static void
schematic_accel_label_set_property (GObject      *object,
                                    guint         prop_id,
                                    const GValue *value,
                                    GParamSpec   *pspec)
{
  SchematicAccelLabel  *accel_label;

  accel_label = SCHEMATIC_ACCEL_LABEL (object);

  switch (prop_id) {
    /* Dummy properties from GtkAccelLabel */
    case PROP_ACCEL_CLOSURE:
    case PROP_ACCEL_WIDGET:
      break;

    case PROP_ACCEL_STRING:
      schematic_accel_label_set_accel_string (accel_label, g_value_get_string (value));
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
schematic_accel_label_get_property (GObject    *object,
                                    guint       prop_id,
                                    GValue     *value,
                                    GParamSpec *pspec)
{
  SchematicAccelLabel  *accel_label;

  accel_label = SCHEMATIC_ACCEL_LABEL (object);

  switch (prop_id) {
    /* Dummy property from GtkAccelLabel */
    case PROP_ACCEL_CLOSURE:
      g_value_set_boxed (value, NULL);
      break;

      /* Dummy property from GtkAccelLabel */
    case PROP_ACCEL_WIDGET:
      g_value_set_object (value, NULL);
      break;

    case PROP_ACCEL_STRING:
      g_value_set_string (value, accel_label->accel_string);
      break;

    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}

static void
schematic_accel_label_init (SchematicAccelLabel *accel_label)
{
  accel_label->accel_padding = 3;
  accel_label->accel_string = NULL;
}

static void
schematic_accel_label_finalize (GObject *object)
{
  SchematicAccelLabel *accel_label = SCHEMATIC_ACCEL_LABEL (object);

  g_free (accel_label->accel_string);

  G_OBJECT_CLASS (schematic_accel_label_parent_class)->finalize (object);
}

guint
schematic_accel_label_get_accel_width (SchematicAccelLabel *accel_label)
{
  g_return_val_if_fail (SCHEMATIC_IS_ACCEL_LABEL (accel_label), 0);

  return (accel_label->accel_string_width +
          (accel_label->accel_string_width ? accel_label->accel_padding : 0));
}

static void
schematic_accel_label_size_request (GtkWidget      *widget,
                                    GtkRequisition *requisition)
{
  SchematicAccelLabel *accel_label = SCHEMATIC_ACCEL_LABEL (widget);
  PangoLayout *layout;
  gint width;

  GTK_WIDGET_CLASS (schematic_accel_label_parent_class)->size_request (widget, requisition);

  layout = gtk_widget_create_pango_layout (widget, schematic_accel_label_get_string (accel_label));
  pango_layout_get_pixel_size (layout, &width, NULL);
  accel_label->accel_string_width = width;
  g_object_unref (layout);
}

static gint
get_first_baseline (PangoLayout *layout)
{
  PangoLayoutIter *iter;
  gint result;

  iter = pango_layout_get_iter (layout);
  result = pango_layout_iter_get_baseline (iter);
  pango_layout_iter_free (iter);

  return PANGO_PIXELS (result);
}

static gboolean
schematic_accel_label_expose_event (GtkWidget      *widget,
                                    GdkEventExpose *event)
{
  SchematicAccelLabel *accel_label = SCHEMATIC_ACCEL_LABEL (widget);
  GtkMisc *misc = GTK_MISC (accel_label);
  guint xpad = 0;
  GtkTextDirection direction;

  g_object_get (misc,
                "xpad", &xpad,
                NULL);

  direction = gtk_widget_get_direction (widget);

  if (gtk_widget_is_drawable (GTK_WIDGET (accel_label)))
    {
      guint ac_width;

      ac_width = schematic_accel_label_get_accel_width (accel_label);

      GtkAllocation *allocation = g_new (GtkAllocation, 1);
      GtkRequisition *requisition = g_new (GtkRequisition, 1);
      gtk_widget_get_allocation (widget, allocation);
      gtk_widget_get_requisition (widget, requisition);

      if (allocation->width >= requisition->width + (gint) ac_width)
        {
          PangoLayout *label_layout;
          PangoLayout *accel_layout;
          GtkLabel *label = GTK_LABEL (widget);

          gint x;
          gint y;

          label_layout = gtk_label_get_layout (GTK_LABEL (accel_label));

          if (direction == GTK_TEXT_DIR_RTL)
            allocation->x += ac_width;
          allocation->width -= ac_width;
          gtk_widget_set_allocation (widget, allocation);

          if (gtk_label_get_ellipsize (label))
            pango_layout_set_width (label_layout,
                                    pango_layout_get_width (label_layout)
                                    - ac_width * PANGO_SCALE);

          if (GTK_WIDGET_CLASS (schematic_accel_label_parent_class)->expose_event)
            GTK_WIDGET_CLASS (schematic_accel_label_parent_class)->expose_event (widget, event);

          gtk_widget_get_allocation (widget, allocation);
          if (direction == GTK_TEXT_DIR_RTL)
            allocation->x -= ac_width;
          allocation->width += ac_width;
          gtk_widget_set_allocation (widget, allocation);

          if (gtk_label_get_ellipsize (label))
            pango_layout_set_width (label_layout,
                                    pango_layout_get_width (label_layout)
                                    + ac_width * PANGO_SCALE);

          gtk_widget_get_allocation (widget, allocation);
          if (direction == GTK_TEXT_DIR_RTL)
            x = allocation->x + xpad;
          else
            x = allocation->x + allocation->width - xpad - ac_width;

          gtk_label_get_layout_offsets (GTK_LABEL (accel_label), NULL, &y);

          accel_layout = gtk_widget_create_pango_layout (widget, schematic_accel_label_get_string (accel_label));

          y += get_first_baseline (label_layout) - get_first_baseline (accel_layout);

          gtk_paint_layout (gtk_widget_get_style (widget),
                            gtk_widget_get_window (widget),
                            gtk_widget_get_state (widget),
                            FALSE,
                            &event->area,
                            widget,
                            "accellabel",
                            x, y,
                            accel_layout);

          g_object_unref (accel_layout);
        }
      else
        {
          if (GTK_WIDGET_CLASS (schematic_accel_label_parent_class)->expose_event)
            GTK_WIDGET_CLASS (schematic_accel_label_parent_class)->expose_event (widget, event);
        }
      g_free (allocation);
      g_free (requisition);
    }

  return FALSE;
}

/* Underscores in key names are better displayed as spaces
 * E.g., Page_Up should be "Page Up"
 */
static void
substitute_underscores (char *str)
{
  char *p;

  for (p = str; *p; p++)
    if (*p == '_')
      *p = ' ';
}


/**
 * schematic_accel_label_set_accel_string:
 * \param accel_label a #SchematicAccelLabel
 * \param accel_string the accelerator string.
 *
 * Sets the accelerator string for this accelerator label.
 **/
void
schematic_accel_label_set_accel_string (SchematicAccelLabel *accel_label,
                                        const gchar *accel_string)
{
  g_return_if_fail (SCHEMATIC_IS_ACCEL_LABEL (accel_label));

  if (accel_label->accel_string)
    g_free (accel_label->accel_string);

  if (accel_string) {
    accel_label->accel_string = g_strdup (accel_string);
    substitute_underscores (accel_label->accel_string);
  } else {
    accel_label->accel_string = NULL;
  }

  g_object_notify (G_OBJECT (accel_label), "accel-string");
}

static void
schematic_accel_label_class_init (SchematicAccelLabelClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GtkWidgetClass *widget_class = GTK_WIDGET_CLASS (klass);

  gobject_class->finalize = schematic_accel_label_finalize;
  gobject_class->set_property = schematic_accel_label_set_property;
  gobject_class->get_property = schematic_accel_label_get_property;

  widget_class->size_request = schematic_accel_label_size_request;
  widget_class->expose_event = schematic_accel_label_expose_event;

  g_object_class_install_property (gobject_class,
                                   PROP_ACCEL_CLOSURE,
                                   g_param_spec_boxed ("accel-closure",
                                                       P_("Accelerator Closure"),
                                                       P_("The closure to be monitored for accelerator changes"),
                                                       G_TYPE_CLOSURE,
                                                       G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
                                   PROP_ACCEL_WIDGET,
                                   g_param_spec_object ("accel-widget",
                                                        P_("Accelerator Widget"),
                                                        P_("The widget to be monitored for accelerator changes"),
                                                        GTK_TYPE_WIDGET,
                                                        G_PARAM_READWRITE));
  g_object_class_install_property (gobject_class,
                                   PROP_ACCEL_STRING,
                                   g_param_spec_string ("accel-string",
                                                        P_("Accelerator String"),
                                                        P_("The accelerator string to be displayed"),
                                                        NULL,
                                                        G_PARAM_READWRITE));
}
#endif
