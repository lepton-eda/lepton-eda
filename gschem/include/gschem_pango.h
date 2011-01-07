/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2008-2010 gEDA Contributors (see ChangeLog for details)
 * Copyright (C) 2000 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 *
 * This file based on GDK's gdkpango.h (LGPL V2+)
 * Copyright (C) 2000 Red Hat, Inc.
 */

#ifndef __GSCHEM_PANGO_H__
#define __GSCHEM_PANGO_H__

#include <gdk/gdktypes.h>

G_BEGIN_DECLS

/* Pango interaction */

typedef struct _GschemPangoRenderer        GschemPangoRenderer;
typedef struct _GschemPangoRendererClass   GschemPangoRendererClass;
typedef struct _GschemPangoRendererPrivate GschemPangoRendererPrivate;

#define GSCHEM_TYPE_PANGO_RENDERER            (gschem_pango_renderer_get_type())
#define GSCHEM_PANGO_RENDERER(object)         (G_TYPE_CHECK_INSTANCE_CAST ((object), GSCHEM_TYPE_PANGO_RENDERER, GschemPangoRenderer))
#define GSCHEM_IS_PANGO_RENDERER(object)      (G_TYPE_CHECK_INSTANCE_TYPE ((object), GSCHEM_TYPE_PANGO_RENDERER))
#define GSCHEM_PANGO_RENDERER_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), GSCHEM_TYPE_PANGO_RENDERER, GschemPangoRendererClass))
#define GSCHEM_IS_PANGO_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), GSCHEM_TYPE_PANGO_RENDERER))
#define GSCHEM_PANGO_RENDERER_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), GSCHEM_TYPE_PANGO_RENDERER, GschemPangoRendererClass))

/**
 * GschemPangoRenderer:
 *
 * #GschemPangoRenderer is a subclass of #PangoRenderer used for rendering
 * text of gschem TEXT objects.
 *
 * Pango functions like pango_renderer_draw_layout() and
 * pango_renderer_draw_layout_line() are then used to draw objects with
 * the renderer.
 **/
struct _GschemPangoRenderer
{
  /*< private >*/
  PangoRenderer parent_instance;

  GschemPangoRendererPrivate *priv;
};


struct _GschemPangoRendererClass
{
  /*< private >*/
  PangoRendererClass parent_class;
};

GType gschem_pango_renderer_get_type (void) G_GNUC_CONST;

PangoRenderer *gschem_pango_renderer_new      (cairo_t *cr);

void gschem_pango_show_layout                 (cairo_t     *cr,
                                               PangoLayout *pl);

void gschem_pango_renderer_set_overbar        (GschemPangoRenderer *gdk_renderer,
                                               gboolean             overbar);

/************************************************************************/

/* Attributes use to render text with overbars */

typedef struct _GschemPangoAttrOverbar GschemPangoAttrOverbar;

struct _GschemPangoAttrOverbar
{
  PangoAttribute attr;
  gboolean overbar;
};

PangoAttribute *gschem_pango_attr_overbar_new (gboolean overbar);

G_END_DECLS

#endif /* __GSCHEM_PANGO_H__ */
