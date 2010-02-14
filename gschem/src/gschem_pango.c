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
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * This file based on GDK's gdkpango.c (LGPL V2+)
 * Copyright (C) 2000 Red Hat, Inc.
 */

#include <config.h>

#include "gschem.h"

#include <math.h>
#include <pango/pangocairo.h>
#include <gschem_pango.h>

#define MAGIC_OVERBAR_POS_CONSTANT 0.8

struct _GschemPangoRendererPrivate
{
  cairo_surface_t *surface;

  /* GschemPangoRenderer specific state */
  gboolean overbar;

  cairo_t *cr;
};

static PangoAttrType gschem_pango_attr_overbar_type;

enum {
  PROP_0,
  PROP_CR
};

G_DEFINE_TYPE (GschemPangoRenderer, gschem_pango_renderer, PANGO_TYPE_RENDERER)

static void
gschem_pango_renderer_finalize (GObject *object)
{
  G_OBJECT_CLASS (gschem_pango_renderer_parent_class)->finalize (object);
}

static GObject*
gschem_pango_renderer_constructor (GType                  type,
                                   guint                  n_construct_properties,
                                   GObjectConstructParam *construct_params)
{
  GObject *object;
  GschemPangoRenderer *gschem_renderer;

  object = G_OBJECT_CLASS (gschem_pango_renderer_parent_class)->constructor (type,
                                                                             n_construct_properties,
                                                                             construct_params);

  gschem_renderer = GSCHEM_PANGO_RENDERER (object);

  if (gschem_renderer->priv->cr == NULL) {
    g_warning ("Cairo context must be specified at construct time for GschemPangoRenderer");
  }

  return object;
}

static cairo_t *
get_cairo_context (GschemPangoRenderer *gschem_renderer,
                   PangoRenderPart   part)
{
  GschemPangoRendererPrivate *priv = gschem_renderer->priv;

  return priv->cr;
}

static void
gschem_pango_renderer_draw_glyphs (PangoRenderer    *renderer,
                                   PangoFont        *font,
                                   PangoGlyphString *glyphs,
                                   int               x,
                                   int               y)
{
  GschemPangoRenderer *gschem_renderer = GSCHEM_PANGO_RENDERER (renderer);
  GschemPangoRendererPrivate *priv = gschem_renderer->priv;
  cairo_t *cr;

  cr = get_cairo_context (gschem_renderer,
                          PANGO_RENDER_PART_FOREGROUND);


  if (priv->overbar) {
    double rx, ry, rwidth, rheight, cheight;
    PangoFontMetrics *metrics;
    PangoRectangle logical;
    int underline_thickness;

    /* Make the thickness the same as for the font's underline */
    metrics = pango_font_get_metrics (font, NULL);
    underline_thickness = pango_font_metrics_get_underline_thickness (metrics);
    pango_font_metrics_unref (metrics);

    pango_glyph_string_extents (glyphs, font, NULL, &logical);

    rx = x;
    ry = y - logical.height * MAGIC_OVERBAR_POS_CONSTANT;
    rwidth = logical.width;
    rheight = underline_thickness;

    cheight = rheight / PANGO_SCALE;

    /* Allow the overbar to fade out as it becomes < 1px high */
    if (cheight > 1.0)
      cheight = (int)(cheight);

    /* \note The +1 on width is a hack to ensure hinting doesn't
     *       sometimes cause the overbars to be broken by a 1px gap
     *       if the overbar spans multiple calls to this function.
     */
    cairo_rectangle (cr,
                     (int)(rx / PANGO_SCALE), (int)(ry / PANGO_SCALE),
                     (int)(rwidth / PANGO_SCALE) + 1, cheight);
    cairo_fill (cr);
  }

  cairo_move_to (cr, (double)x / PANGO_SCALE, (double)y / PANGO_SCALE);
  pango_cairo_show_glyph_string (cr, font, glyphs);
}

static void
gschem_pango_renderer_draw_rectangle (PangoRenderer    *renderer,
                                      PangoRenderPart   part,
                                      int               x,
                                      int               y,
                                      int               width,
                                      int               height)
{
  GschemPangoRenderer *gschem_renderer = GSCHEM_PANGO_RENDERER (renderer);
  cairo_t *cr;

  cr = get_cairo_context (gschem_renderer, part);

  cairo_rectangle (cr,
                   (double)x / PANGO_SCALE, (double)y / PANGO_SCALE,
                   (double)width / PANGO_SCALE, (double)height / PANGO_SCALE);
  cairo_fill (cr);
}

static void
gschem_pango_renderer_draw_error_underline (PangoRenderer    *renderer,
                                            int               x,
                                            int               y,
                                            int               width,
                                            int               height)
{
  GschemPangoRenderer *gschem_renderer = GSCHEM_PANGO_RENDERER (renderer);
  cairo_t *cr;

  cr = get_cairo_context (gschem_renderer, PANGO_RENDER_PART_UNDERLINE);

  pango_cairo_show_error_underline (cr,
        (double)x / PANGO_SCALE, (double)y / PANGO_SCALE,
        (double)width / PANGO_SCALE, (double)height / PANGO_SCALE);
}

static void
gschem_pango_renderer_part_changed (PangoRenderer   *renderer,
                                    PangoRenderPart  part)
{
}

static void
gschem_pango_renderer_begin (PangoRenderer *renderer)
{
}

static void
gschem_pango_renderer_end (PangoRenderer *renderer)
{
}

static void
gschem_pango_renderer_prepare_run (PangoRenderer  *renderer,
                                   PangoLayoutRun *run)
{
  GschemPangoRenderer *gschem_renderer = GSCHEM_PANGO_RENDERER (renderer);
  gboolean overbar = FALSE;
  gboolean changed = FALSE;
  GSList *l;

  for (l = run->item->analysis.extra_attrs; l; l = l->next) {
    PangoAttribute *attr = l->data;

    /* overbar_type isn't necessarily initialized, but it is 0,
     * which is an invalid type so won't occur.
     */
    if (attr->klass->type == gschem_pango_attr_overbar_type) {
      overbar = ((GschemPangoAttrOverbar*)attr)->overbar;
    }
  }

  if (overbar != gschem_renderer->priv->overbar) {
    gschem_renderer->priv->overbar = overbar;
    changed = TRUE;
  }

  if (changed)
    pango_renderer_part_changed (renderer, PANGO_RENDER_PART_FOREGROUND);

  PANGO_RENDERER_CLASS (gschem_pango_renderer_parent_class)->prepare_run (renderer, run);
}

static void
gschem_pango_renderer_set_property (GObject *object, guint prop_id,
                                    const GValue *value, GParamSpec *pspec)
{
  GschemPangoRenderer *gschem_renderer = GSCHEM_PANGO_RENDERER (object);

  switch (prop_id) {
    case PROP_CR:
      gschem_renderer->priv->cr = g_value_get_pointer (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
gschem_pango_renderer_get_property (GObject *object, guint prop_id,
                                    GValue *value, GParamSpec *pspec)
{
  GschemPangoRenderer *gschem_renderer = GSCHEM_PANGO_RENDERER (object);

  switch (prop_id) {
    case PROP_CR:
      g_value_set_pointer (value, gschem_renderer->priv->cr);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
  }
}

static void
gschem_pango_renderer_init (GschemPangoRenderer *renderer)
{
  renderer->priv = G_TYPE_INSTANCE_GET_PRIVATE (renderer,
                                                GSCHEM_TYPE_PANGO_RENDERER,
                                                GschemPangoRendererPrivate);
}

static void
gschem_pango_renderer_class_init (GschemPangoRendererClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);

  PangoRendererClass *renderer_class = PANGO_RENDERER_CLASS (klass);

  renderer_class->draw_glyphs = gschem_pango_renderer_draw_glyphs;
  renderer_class->draw_rectangle = gschem_pango_renderer_draw_rectangle;
  renderer_class->draw_error_underline = gschem_pango_renderer_draw_error_underline;
  renderer_class->part_changed = gschem_pango_renderer_part_changed;
  renderer_class->begin = gschem_pango_renderer_begin;
  renderer_class->end = gschem_pango_renderer_end;
  renderer_class->prepare_run = gschem_pango_renderer_prepare_run;

  object_class->finalize = gschem_pango_renderer_finalize;
  object_class->constructor = gschem_pango_renderer_constructor;
  object_class->set_property = gschem_pango_renderer_set_property;
  object_class->get_property = gschem_pango_renderer_get_property;

  g_object_class_install_property (object_class,
                                   PROP_CR,
                                   g_param_spec_pointer ("cr",
                                                         _("cairo context"),
                                                         _("the cairo context for the renderer"),
                                                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY |
                                                         G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
                                                         G_PARAM_STATIC_BLURB));

  g_type_class_add_private (object_class, sizeof (GschemPangoRendererPrivate));
}

/*!
 * gschem_pango_renderer_new:
 * @cr: a #cairo renderer
 *
 * Creates a new #PangoRenderer for @cr. Normally you can use the results
 * of gschem_pango_renderer_get_default() rather than creating a new renderer.
 *
 * Return value: a newly created #PangoRenderer. Free with g_object_unref().
 */
PangoRenderer *
gschem_pango_renderer_new (cairo_t *cr)
{
  return g_object_new (GSCHEM_TYPE_PANGO_RENDERER, "cr", cr, NULL);
}

void
gschem_pango_show_layout (cairo_t     *cr,
                          PangoLayout *pl)
{
  static PangoRenderer *renderer = NULL;
  static cairo_t *cr_cache = NULL;
  double x_off, y_off;

  if (cr_cache != cr && renderer != NULL) {
    g_object_unref (renderer);
    renderer = NULL;
  }

  if (renderer == NULL) {
    renderer = gschem_pango_renderer_new (cr);
    cr_cache = cr;
  }

  cairo_get_current_point (cr, &x_off, &y_off);

  pango_renderer_draw_layout (renderer, pl, x_off * PANGO_SCALE, y_off * PANGO_SCALE);
}


/* GschemPangoAttrOverbar */

static PangoAttribute *
gschem_pango_attr_overbar_copy (const PangoAttribute *attr)
{
  const GschemPangoAttrOverbar *e = (const GschemPangoAttrOverbar*) attr;

  return gschem_pango_attr_overbar_new (e->overbar);
}

static void
gschem_pango_attr_overbar_destroy (PangoAttribute *attr)
{
  g_free (attr);
}

static gboolean
gschem_pango_attr_overbar_compare (const PangoAttribute *attr1,
                                   const PangoAttribute *attr2)
{
  const GschemPangoAttrOverbar *e1 = (const GschemPangoAttrOverbar*) attr1;
  const GschemPangoAttrOverbar *e2 = (const GschemPangoAttrOverbar*) attr2;

  return e1->overbar == e2->overbar;
}

/*!
 * gschem_pango_attr_overbar_new:
 * @overbar: if the region should be drawn with an overbar
 *
 * Creates a new attribute flagging a region as being drawn with overbar or not
 *
 * Return value: new #PangoAttribute
 */
PangoAttribute *
gschem_pango_attr_overbar_new (gboolean overbar)
{
  GschemPangoAttrOverbar *result;

  static PangoAttrClass klass = { 0,
                                  gschem_pango_attr_overbar_copy,
                                  gschem_pango_attr_overbar_destroy,
                                  gschem_pango_attr_overbar_compare };

  if (!klass.type)
    klass.type = gschem_pango_attr_overbar_type =
      pango_attr_type_register ("GschemPangoAttrOverbar");

  result = g_new (GschemPangoAttrOverbar, 1);
  result->attr.klass = &klass;
  result->overbar = overbar;

  return (PangoAttribute *)result;
}
