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

#ifndef __GSCHEM_CELL_RENDERER_ICON_H__
#define __GSCHEM_CELL_RENDERER_ICON_H__

G_BEGIN_DECLS

/* ---------------------------------------------------------------- */

/*! \class GschemCellRendererIcon gschemcellrenderericon.h "gschemcellrenderericon.h"
 * \brief GtkCellRenderer for icons that checks stock icons and theme.
 *
 * In gschem, we use both GTK's stock icons and also our own icons
 * that we add to the icon theme search path.  We identify each icon
 * by a single icon name, which might either name a GTK stock icon or
 * a theme icon.  To determine which icon to show, we first check if
 * there's a matching stock icon, and if one doesn't exist, we fall
 * back to looking in the theme.
 *
 * The GtkCellRendererPixbuf doesn't provide this capability.  If its
 * "icon-name" property is set, it doesn't look at stock items, but if
 * its "stock-id" property is set, it ignores the "icon-name" even if
 * no matching stock item exists.
 *
 * The GschemCellRendererIcon gets around this by subclassing
 * GtkCellRendererPixbuf to provide an additional "icon-name"
 * property, which implements the desired fallback behaviour.
 *
 * \b Properties: One property, \b "icon-id", string.  If the value
 * matches a stock item, the associated stock icon is displayed;
 * otherwise, the icon theme is searched for a matching icon name.
 */

#define GSCHEM_TYPE_CELL_RENDERER_ICON (gschem_cell_renderer_icon_get_type ())
#define GSCHEM_CELL_RENDERER_ICON(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), GSCHEM_TYPE_CELL_RENDERER_ICON, GschemCellRendererIcon))
#define GSCHEM_CELL_RENDERER_ICON_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), GSCHEM_TYPE_CELL_RENDERER_ICON, GschemCellRendererIconClass))
#define GSCHEM_IS_CELL_RENDERER_ICON(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), GSCHEM_TYPE_CELL_RENDERER_ICON))
#define GSCHEM_IS_CELL_RENDERER_ICON_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GSCHEM_TYPE_CELL_RENDERER_ICON))
#define GSCHEM_CELL_RENDERER_ICON_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_CELL_RENDERER_ICON, GschemCellRendererIconClass))

typedef struct _GschemCellRendererIconClass GschemCellRendererIconClass;
typedef struct _GschemCellRendererIcon GschemCellRendererIcon;

struct _GschemCellRendererIconClass
{
  GtkCellRendererPixbufClass parent_class;
};

struct _GschemCellRendererIcon
{
  GtkCellRendererPixbuf parent_instance;
};

GType gschem_cell_renderer_icon_get_type (void) G_GNUC_CONST;

GtkCellRenderer *gschem_cell_renderer_icon_new (void) G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS

#endif /* ! __GSCHEM_CELL_RENDERER_ICON_H__ */
