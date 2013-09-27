/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#include <config.h>
#include <missing.h>
#include "gschem.h"

enum {
  PROP_ICON_ID = 1,
};

static void
gschem_cell_renderer_icon_set_property (GObject *object,
                                        guint property_id,
                                        const GValue *value,
                                        GParamSpec *pspec);
static void
gschem_cell_renderer_icon_get_property (GObject *object,
                                        guint property_id,
                                        GValue *value,
                                        GParamSpec *pspec);

G_DEFINE_TYPE (GschemCellRendererIcon, gschem_cell_renderer_icon,
               GTK_TYPE_CELL_RENDERER_PIXBUF);

static void
gschem_cell_renderer_icon_class_init (GschemCellRendererIconClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GParamFlags param_flags;

  /* Register functions with base class */
  gobject_class->set_property = gschem_cell_renderer_icon_set_property;
  gobject_class->get_property = gschem_cell_renderer_icon_get_property;

  /* Install properties */
  param_flags = (G_PARAM_READWRITE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
                 G_PARAM_STATIC_BLURB);

  g_object_class_install_property (gobject_class, PROP_ICON_ID,
                                   g_param_spec_string ("icon-id",
                                                        _("Icon ID"),
                                                        _("Icon stock ID or theme icon name"),
                                                        NULL,
                                                        param_flags));
}

static void
gschem_cell_renderer_icon_init (GschemCellRendererIcon *renderer)
{
}

static void
gschem_cell_renderer_icon_set_property (GObject *object,
                                        guint property_id,
                                        const GValue *value,
                                        GParamSpec *pspec)
{
  const gchar *icon_id = NULL;
  const gchar *stock_id = NULL, *icon_name = NULL;
  GtkStockItem stock_info;
  switch (property_id) {
  case PROP_ICON_ID:
    icon_id = g_value_get_string (value);
    if (icon_id != NULL && gtk_stock_lookup (icon_id, &stock_info)) {
      stock_id = icon_id;
    } else {
      icon_name = icon_id;
    }
    g_object_set (object,
                  "icon-name", icon_name,
                  "stock-id", stock_id,
                  NULL);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;
  }
}

static void
gschem_cell_renderer_icon_get_property (GObject *object,
                                        guint property_id,
                                        GValue *value,
                                        GParamSpec *pspec)
{
  gchar *stock_id = NULL, *icon_name = NULL;
  switch (property_id) {
  case PROP_ICON_ID:
    g_object_get (object,
                  "icon-name", &icon_name,
                  "stock-id", &stock_id,
                  NULL);
    if (stock_id != NULL) {
      g_value_set_string (value, stock_id);
    } else {
      g_value_set_string (value, icon_name);
    }
    g_free (stock_id);
    g_free (icon_name);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
    break;
  }
}

/* ---------------------------------------------------------------- */

/*! \public \memberof GschemCellRendererIcon
 * \brief Create a new gschem icon cell renderer.
 * \return a new #GschemCellRendererIcon instance.
 */
GtkCellRenderer *
gschem_cell_renderer_icon_new (void)
{
  return GTK_CELL_RENDERER (g_object_new (GSCHEM_TYPE_CELL_RENDERER_ICON, NULL));
}
