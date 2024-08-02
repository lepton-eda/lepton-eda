/* Lepton EDA library
 * Copyright (C) 2010-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * This file based on GDK's gdkpango.c (LGPL V2+)
 * Copyright (C) 2000 Red Hat, Inc.
 */

/*!
 * \file edapangorenderer.c
 *
 * \brief Pango renderer
 */

#include <config.h>

#include <math.h>
#include <string.h>
#include <glib.h>
#include <glib-object.h>
#include <cairo.h>
#include <pango/pangocairo.h>
#include <liblepton/edapangorenderer.h>

/* We don't use gettext */
#define _(x) (x)

#define MAGIC_OVERBAR_POS_CONSTANT 0.8

enum {
  PROP_CAIRO_CONTEXT = 1
};

struct _EdaPangoRendererPrivate
{
  cairo_t *cr;
  gboolean overbar;
};

static GObject *eda_pango_renderer_constructor (GType type,
                                                guint n_construct_properties,
                                                GObjectConstructParam *construct_params);
static void eda_pango_renderer_set_property (GObject *object, guint prop_id,
                                             const GValue *value,
                                             GParamSpec *pspec);
static void eda_pango_renderer_get_property (GObject *object, guint prop_id,
                                             GValue *value, GParamSpec *pspec);
static void eda_pango_renderer_finalize (GObject *object);

static void eda_pango_renderer_draw_glyphs (PangoRenderer *renderer,
                                            PangoFont *font,
                                            PangoGlyphString *glyphs,
                                            int x, int y);
static void eda_pango_renderer_draw_rectangle (PangoRenderer *renderer,
                                               PangoRenderPart part,
                                               int x, int y,
                                               int width, int height);
static void eda_pango_renderer_draw_error_underline (PangoRenderer *renderer,
                                                     int x, int y,
                                                     int width, int height);
static void eda_pango_renderer_part_changed (PangoRenderer *renderer,
                                             PangoRenderPart part);
static void eda_pango_renderer_begin (PangoRenderer *renderer);
static void eda_pango_renderer_end (PangoRenderer *renderer);
static void eda_pango_renderer_prepare_run (PangoRenderer *renderer,
                                            PangoLayoutRun *run);

G_DEFINE_TYPE_WITH_PRIVATE (EdaPangoRenderer, eda_pango_renderer, PANGO_TYPE_RENDERER);

/* ---------------------------------------- */

static PangoAttribute *eda_pango_attr_overbar_copy (const PangoAttribute *attr);
static gboolean eda_pango_attr_overbar_compare (const PangoAttribute *attr1,
                                                const PangoAttribute *attr2);

/* ---------------------------------------- */

static void
eda_pango_renderer_class_init (EdaPangoRendererClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS (klass);
  PangoRendererClass *parent_class = PANGO_RENDERER_CLASS (klass);

  /* Register functions with base class */
  object_class->constructor = eda_pango_renderer_constructor;
  object_class->set_property = eda_pango_renderer_set_property;
  object_class->get_property = eda_pango_renderer_get_property;
  object_class->finalize = eda_pango_renderer_finalize;

  /* Register functions with parent class */
  parent_class->draw_glyphs = eda_pango_renderer_draw_glyphs;
  parent_class->draw_rectangle = eda_pango_renderer_draw_rectangle;
  parent_class->draw_error_underline = eda_pango_renderer_draw_error_underline;
  parent_class->part_changed = eda_pango_renderer_part_changed;
  parent_class->begin = eda_pango_renderer_begin;
  parent_class->end = eda_pango_renderer_end;
  parent_class->prepare_run = eda_pango_renderer_prepare_run;

  /* Install properties */
  g_object_class_install_property (object_class, PROP_CAIRO_CONTEXT,
                                   g_param_spec_pointer ("cairo-context",
                                                         _("Cairo context"),
                                                         _("The Cairo context for rendering"),
                                                         (GParamFlags) (G_PARAM_READWRITE
                                                                        | G_PARAM_CONSTRUCT_ONLY
                                                                        | G_PARAM_STATIC_NAME
                                                                        | G_PARAM_STATIC_NICK
                                                                        | G_PARAM_STATIC_BLURB)));
}

static void
eda_pango_renderer_init (EdaPangoRenderer *renderer)
{
  renderer->priv =
    (EdaPangoRendererPrivate*) eda_pango_renderer_get_instance_private (renderer);
}

static GObject *
eda_pango_renderer_constructor (GType type,
                                guint n_construct_properties,
                                GObjectConstructParam *construct_params)
{
  GObject *object;
  GObjectClass *parent_object_class;
  EdaPangoRenderer *renderer;

  parent_object_class = G_OBJECT_CLASS (eda_pango_renderer_parent_class);
  object = parent_object_class->constructor (type, n_construct_properties,
                                             construct_params);

  #ifndef G_DISABLE_ASSERT
  renderer = EDA_PANGO_RENDERER (object);
  if (renderer->priv->cr == NULL) {
    g_warning ("EdaPangoRenderer: Cairo context must be specified at construction.");
  }
  #endif

  return object;
}

static void
eda_pango_renderer_set_property (GObject *object, guint property_id,
                                 const GValue *value, GParamSpec *pspec)
{
  EdaPangoRenderer *renderer = EDA_PANGO_RENDERER (object);
  switch (property_id) {
  case PROP_CAIRO_CONTEXT:
    renderer->priv->cr = (cairo_t *) g_value_get_pointer (value);
    if (renderer->priv->cr != NULL) {
      cairo_reference (renderer->priv->cr);
    }
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_pango_renderer_get_property (GObject *object, guint property_id,
                                 GValue *value, GParamSpec *pspec)
{
  EdaPangoRenderer *renderer = EDA_PANGO_RENDERER (object);
  switch (property_id) {
  case PROP_CAIRO_CONTEXT:
    g_value_set_pointer (value, renderer->priv->cr);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_pango_renderer_finalize (GObject *object)
{
  EdaPangoRenderer *renderer = EDA_PANGO_RENDERER (object);
  G_OBJECT_CLASS (eda_pango_renderer_parent_class)->finalize (object);

  if (renderer->priv->cr != NULL) {
    cairo_destroy (renderer->priv->cr);
  }
}

static void
eda_pango_renderer_draw_glyphs (PangoRenderer *renderer,
                                PangoFont *font,
                                PangoGlyphString *glyphs,
                                int x, int y)
{
  EdaPangoRenderer *eda_renderer = EDA_PANGO_RENDERER (renderer);
  cairo_t *cr = eda_renderer->priv->cr;

  if (eda_renderer->priv->overbar) {
    PangoFontMetrics *metrics;
    double rx, ry;
    double rwidth;
    double rheight;
    PangoRectangle glyphs_extents;

    pango_glyph_string_extents (glyphs, font, NULL, &glyphs_extents);
    rx = x / PANGO_SCALE;
    ry = (y - glyphs_extents.height * MAGIC_OVERBAR_POS_CONSTANT) / PANGO_SCALE;
    rwidth = glyphs_extents.width / PANGO_SCALE;

    /* Make the thickness the same as for the font's underline */
    metrics = pango_font_get_metrics (font, NULL);
    rheight = pango_font_metrics_get_underline_thickness (metrics) / PANGO_SCALE;
    pango_font_metrics_unref (metrics);

    /* Allow the overbar to fade out as it becomes < 1px high */
    if (rheight > 1.0) {
      rheight = floor (rheight);
    }

    /* The +1 on width is a hack to ensure hinting doesn't sometimes
     * cause the overbars to be broken by a 1px gap if the overbar
     * spans multiple calls to this function. */
    rwidth += 1.0;

    cairo_rectangle (cr, floor(rx), floor(ry), floor(rwidth), rheight);
    cairo_fill (cr);
  }

  /* Now draw the actual characters */
  cairo_move_to (cr, (double) x / PANGO_SCALE, (double) y / PANGO_SCALE);
  pango_cairo_show_glyph_string (cr, font, glyphs);
}

static void
eda_pango_renderer_draw_rectangle (PangoRenderer *renderer,
                                   PangoRenderPart part,
                                   int x, int y, int width, int height)
{
  EdaPangoRenderer *eda_renderer = EDA_PANGO_RENDERER (renderer);
  cairo_t *cr = eda_renderer->priv->cr;

  cairo_rectangle (cr, (double) x / PANGO_SCALE, (double) y / PANGO_SCALE,
                   (double) width / PANGO_SCALE, (double) height / PANGO_SCALE);
  cairo_fill (cr);
}

static void
eda_pango_renderer_draw_error_underline (PangoRenderer *renderer,
                                   int x, int y, int width, int height)
{
  cairo_t *cr = EDA_PANGO_RENDERER (renderer)->priv->cr;
  pango_cairo_show_error_underline (cr, (double) x / PANGO_SCALE,
                                    (double) y / PANGO_SCALE,
                                    (double) width / PANGO_SCALE,
                                    (double) height / PANGO_SCALE);
}

static void
eda_pango_renderer_part_changed (PangoRenderer *renderer,
                                 PangoRenderPart part)
{
}

static void
eda_pango_renderer_begin (PangoRenderer *renderer)
{
}

static void
eda_pango_renderer_end (PangoRenderer *renderer)
{
}

static void
eda_pango_renderer_prepare_run (PangoRenderer *renderer,
                                PangoLayoutRun *run)
{
  EdaPangoRenderer *eda_renderer = EDA_PANGO_RENDERER (renderer);
  gboolean overbar = FALSE;
  GSList *l;

  for (l = run->item->analysis.extra_attrs; l != NULL; l = g_slist_next (l)) {
    if (eda_is_pango_attr_overbar ((PangoAttribute *) l->data)) {
      EdaPangoAttrOverbar *attr = (EdaPangoAttrOverbar *) l->data;
      overbar = attr->overbar;
    }
  }

  if (eda_renderer->priv->overbar != overbar) {
    pango_renderer_part_changed (renderer, PANGO_RENDER_PART_FOREGROUND);
    eda_renderer->priv->overbar = overbar;
  }

  PANGO_RENDERER_CLASS (eda_pango_renderer_parent_class)->prepare_run (renderer,
                                                                       run);
}

PangoRenderer *
eda_pango_renderer_new (cairo_t *cr)
{
  return PANGO_RENDERER (g_object_new (EDA_TYPE_PANGO_RENDERER,
                                       "cairo-context", cr,
                                       NULL));
}

void
eda_pango_renderer_show_layout (EdaPangoRenderer *renderer, PangoLayout *pl,
                                double x, double y)
{
  g_return_if_fail (EDA_IS_PANGO_RENDERER (renderer));
  g_return_if_fail (renderer->priv->cr != NULL);
  g_return_if_fail (PANGO_IS_LAYOUT (pl));

  pango_renderer_draw_layout (PANGO_RENDERER (renderer),
                              pl, x * PANGO_SCALE, y * PANGO_SCALE);
}


/* ---------------------------------------- */

static PangoAttribute *
eda_pango_attr_overbar_copy (const PangoAttribute *attr)
{
  const EdaPangoAttrOverbar *a = (const EdaPangoAttrOverbar *) attr;
  return eda_pango_attr_overbar_new (a->overbar);
}

static gboolean
eda_pango_attr_overbar_compare (const PangoAttribute *attr1,
                                const PangoAttribute *attr2)
{
  const EdaPangoAttrOverbar *a1 = (const EdaPangoAttrOverbar *) attr1;
  const EdaPangoAttrOverbar *a2 = (const EdaPangoAttrOverbar *) attr2;
  return (a1->overbar == a2->overbar);
}

PangoAttrClass *
eda_pango_attr_overbar_get_class ()
{
  static PangoAttrClass klass = { (PangoAttrType) 0,
                                  eda_pango_attr_overbar_copy,
                                  (void (*)(PangoAttribute *)) g_free,
                                  eda_pango_attr_overbar_compare };

  if (!klass.type) {
    klass.type = pango_attr_type_register ("EdaPangoAttrOverbar");
  }

  return &klass;
}

PangoAttribute *
eda_pango_attr_overbar_new (gboolean overbar)
{
  EdaPangoAttrOverbar *result = g_new (EdaPangoAttrOverbar, 1);
  result->attr.klass = eda_pango_attr_overbar_get_class ();
  result->overbar = overbar;
  return (PangoAttribute *) result;
}

gboolean
eda_is_pango_attr_overbar (PangoAttribute *attr)
{
  return attr->klass->type == eda_pango_attr_overbar_get_class()->type;
}

gboolean
eda_pango_parse_overbars (const gchar *overbar_text, int length,
                          PangoAttrList **attr_list, gchar **text)
{
  const char *in_ptr = NULL;
  char *out_ptr;
  char *overbar_start = NULL;
  char *overbar_end = NULL;
  const char *escape_start = NULL;

  g_return_val_if_fail ((overbar_text != NULL), FALSE);
  g_return_val_if_fail ((attr_list != NULL), FALSE);
  g_return_val_if_fail ((text != NULL), FALSE);

  /* Create the attribute list */
  *attr_list = pango_attr_list_new ();

  /* We know the length of the output will be <= the length of the
   * input text.  So we just allocate a string of the same length.  If
   * length was given as -1, the input should be null-terminated, so
   * just use strlen. */
  if (length == -1) {
    length = strlen (overbar_text);
  }
  *text = (gchar*) g_malloc0 (length + 1);
  out_ptr = *text;

  for (in_ptr = overbar_text;
       (in_ptr - overbar_text) <= length; /* Include \0 at end */
       in_ptr++) {

    /* If we find an escape character and we are not already in an
     * escaped state, enter escaped state and don't add the current
     * character to the output. */
    if ((*in_ptr == '\\') && !escape_start) {
      escape_start = in_ptr;
      continue;
    }

    /* If the escaped character is '_', this is an overbar delimiter.
     * Enter or exit overbar state if appropriate. Otherwise, simply
     * append the character (which may have been escaped) to the
     * output. */
    if ((*in_ptr == '_') && escape_start) {
      if (overbar_start) {
        overbar_end = out_ptr;
      } else {
        overbar_start = out_ptr;
      }
    } else {
      *out_ptr++ = *in_ptr;
    }
    escape_start = NULL;

    /* If we've previously found an overbar delimiter, and either we
     * find a null byte or another overbar delimiter, create an
     * overbar attribute for the intervening run of characters. */
    if (overbar_start && (overbar_end || (*in_ptr == '\0'))) {
      /* Create overbar attribute and add to attribute list */
      PangoAttribute *attr = eda_pango_attr_overbar_new (TRUE);
      attr->start_index = overbar_start - *text;
      attr->end_index = overbar_end - *text;
      pango_attr_list_insert (*attr_list, attr);
      /* Clear overbar start & end pointers */
      overbar_start = overbar_end = NULL;
    }

    /* If we encounter a null character before we were expecting it,
     * give up anyway. */
    if (*in_ptr == '\0') break;
  }

  return TRUE;
}
