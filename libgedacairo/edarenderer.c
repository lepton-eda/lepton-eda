/* gEDA - GPL Electronic Design Automation
 * libgedacairo - Rendering gEDA schematics with Cairo
 * Copyright (C) 2010-2012 gEDA Contributors (see ChangeLog for details)
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
 */

#include <config.h>

#include <math.h>
#include <glib.h>
#include <glib-object.h>
#include <gdk/gdk.h>
#include <cairo.h>
#include <pango/pangocairo.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include <libgeda/libgeda.h>

#include "edarenderer.h"

#include "edacairo.h"
#include "edapangorenderer.h"

/* We don't use gettext */
#define _(x) (x)

enum {
  PROP_CAIRO_CONTEXT = 1,
  PROP_PANGO_CONTEXT,
  PROP_FONT_NAME,
  PROP_COLOR_MAP,
  PROP_OVERRIDE_COLOR,
  PROP_GRIP_SIZE,
  PROP_RENDER_FLAGS,

  FLAG_HINTING = EDA_RENDERER_FLAG_HINTING,
  FLAG_PICTURE_OUTLINE = EDA_RENDERER_FLAG_PICTURE_OUTLINE,
  FLAG_TEXT_HIDDEN = EDA_RENDERER_FLAG_TEXT_HIDDEN,
  FLAG_TEXT_OUTLINE = EDA_RENDERER_FLAG_TEXT_OUTLINE,
  FLAG_TEXT_ORIGIN = EDA_RENDERER_FLAG_TEXT_ORIGIN,

  GRIP_SQUARE,
  GRIP_CIRCLE,
};

struct _EdaRendererPrivate
{
  cairo_t *cr;
  PangoContext *pc;
  PangoLayout *pl;
  EdaPangoRenderer *pr;
  int pc_from_cr;

  unsigned int flags;
  gchar *font_name;
  int override_color;
  double grip_size;

  GArray *color_map;

  /* Cache of font metrics for different font sizes. */
  GHashTable *metrics_cache;
};

static inline gboolean
EDA_RENDERER_CHECK_FLAG (EdaRenderer *r, int f) {
  return r->priv->flags & f;
}
static inline void
EDA_RENDERER_SET_FLAG (EdaRenderer *r, int f, gboolean e) {
  if (e) { r->priv->flags |= f; } else { r->priv->flags &= ~f; }
}
static inline unsigned int
EDA_RENDERER_CAIRO_FLAGS (EdaRenderer *r) {
  return EDA_RENDERER_CHECK_FLAG (r, FLAG_HINTING) ? EDA_CAIRO_ENABLE_HINTS : 0;
}
static inline double
EDA_RENDERER_STROKE_WIDTH (EdaRenderer *r, double width) {
  /* For now, the minimum line width possible is half the net width. */
  return fmax (width, NET_WIDTH / 2);
}

#define DEFAULT_FONT_NAME "Arial"
#define GRIP_STROKE_COLOR SELECT_COLOR
#define GRIP_FILL_COLOR BACKGROUND_COLOR
#define TEXT_MARKER_SIZE 10
#define TEXT_MARKER_COLOR LOCK_COLOR

static GObject *eda_renderer_constructor (GType type,
                                          guint n_construct_properties,
                                          GObjectConstructParam *construct_params);
static void eda_renderer_finalize (GObject *object);
static void eda_renderer_dispose (GObject *object);
static void eda_renderer_set_property (GObject *object, guint property_id,
                                       const GValue *value, GParamSpec *pspec);
static void eda_renderer_get_property (GObject *object, guint property_id,
                                       GValue *value, GParamSpec *pspec);
static void eda_renderer_update_contexts (EdaRenderer *renderer, cairo_t *new_cr,
                                          PangoContext *new_pc);

static void eda_renderer_set_color (EdaRenderer *renderer, int color);
static int eda_renderer_is_drawable (EdaRenderer *renderer, OBJECT *object);
static int eda_renderer_draw_hatch (EdaRenderer *renderer, OBJECT *object);

static void eda_renderer_default_draw (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_list (EdaRenderer *renderer, GList *objects);
static void eda_renderer_draw_line (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_pin (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_net (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_bus (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_box (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_arc (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_circle (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_path (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_text (EdaRenderer *renderer, OBJECT *object);
static int eda_renderer_get_font_descent (EdaRenderer *renderer,
                                          PangoFontDescription *desc);
static int eda_renderer_prepare_text (EdaRenderer *renderer, const GedaObject *object);
static void eda_renderer_calc_text_position (EdaRenderer *renderer, const GedaObject *object,
                                             int descent, double *x, double *y);
static void eda_renderer_draw_picture (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_complex (EdaRenderer *renderer, OBJECT *object);

static void eda_renderer_default_draw_grips (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_grips_list (EdaRenderer *renderer, GList *objects);
static void eda_renderer_draw_grips_impl (EdaRenderer *renderer, int type, int n_grips, ...);
static void eda_renderer_draw_arc_grips (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_path_grips (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_text_grips (EdaRenderer *renderer, OBJECT *object);

static void eda_renderer_default_draw_cues (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_cues_list (EdaRenderer *renderer, GList *objects);
static void eda_renderer_draw_end_cues (EdaRenderer *renderer, OBJECT *object,
                                        int end);
static void eda_renderer_draw_mid_cues (EdaRenderer *renderer, OBJECT *object);
static void eda_renderer_draw_junction_cue (EdaRenderer *renderer, int x, int y,
                                            int is_bus);

static gboolean
eda_renderer_default_get_user_bounds (EdaRenderer *renderer,
                                      const GedaObject *object,
                                      double *left,
                                      double *top,
                                      double *right,
                                      double *bottom);

static gboolean
eda_renderer_get_text_user_bounds (EdaRenderer *renderer,
                                   const GedaObject *object,
                                   double *left,
                                   double *top,
                                   double *right,
                                   double *bottom);

G_DEFINE_TYPE (EdaRenderer, eda_renderer, G_TYPE_OBJECT);

GType
eda_renderer_flags_get_type ()
{
  static const GFlagsValue values[] = {
    {FLAG_HINTING, "hinting", _("Enable hinting")},
    {FLAG_PICTURE_OUTLINE, "picture-outline", _("Picture outlines")},
    {FLAG_TEXT_HIDDEN, "text-hidden", _("Hidden text")},
    {FLAG_TEXT_OUTLINE, "text-outline", _("Text outlines")},
    {FLAG_TEXT_ORIGIN, "text-origin", _("Text origins")},
    {0, 0, 0},
  };
  static GType flags_type = 0;
  if (flags_type == 0) {
    flags_type = g_flags_register_static ("EdaRendererFlags",
                                          values);
  }
  return flags_type;
}

static void
eda_renderer_class_init (EdaRendererClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GParamFlags param_flags;

  g_type_class_add_private (gobject_class, sizeof (EdaRendererPrivate));

  /* Register functions with base class */
  gobject_class->constructor = eda_renderer_constructor;
  gobject_class->finalize = eda_renderer_finalize;
  gobject_class->dispose = eda_renderer_dispose;
  gobject_class->set_property = eda_renderer_set_property;
  gobject_class->get_property = eda_renderer_get_property;

  /* Install default implementations of virtual public methods */
  klass->draw = eda_renderer_default_draw;
  klass->draw_grips = eda_renderer_default_draw_grips;
  klass->draw_cues = eda_renderer_default_draw_cues;
  klass->user_bounds = eda_renderer_default_get_user_bounds;

  /* Install properties */
  param_flags = (G_PARAM_READWRITE | G_PARAM_STATIC_NAME | G_PARAM_STATIC_NICK |
                 G_PARAM_STATIC_BLURB);

  g_object_class_install_property (gobject_class, PROP_CAIRO_CONTEXT,
                                   g_param_spec_pointer ("cairo-context",
                                                         _("Cairo context"),
                                                         _("The Cairo context for rendering"),
                                                         param_flags));
  g_object_class_install_property (gobject_class, PROP_PANGO_CONTEXT,
                                   g_param_spec_pointer ("pango-context",
                                                         _("Pango context"),
                                                         _("The Pango context for text rendering"),
                                                         param_flags));
  g_object_class_install_property (gobject_class, PROP_FONT_NAME,
                                   g_param_spec_string ("font-name",
                                                        _("Font name"),
                                                        _("The name of the font to use for text rendering"),
                                                        DEFAULT_FONT_NAME,
                                                        param_flags));
  g_object_class_install_property (gobject_class, PROP_COLOR_MAP,
                                   g_param_spec_pointer ("color-map",
                                                         _("Color map"),
                                                         _("Map for determining colors from color indices"),
                                                         param_flags));
  g_object_class_install_property (gobject_class, PROP_OVERRIDE_COLOR,
                                   g_param_spec_int ("override-color",
                                                     _("Override color"),
                                                     _("Index of color to force used for all drawing."),
                                                     -1, G_MAXINT, -1,
                                                     param_flags));
  g_object_class_install_property (gobject_class, PROP_GRIP_SIZE,
                                   g_param_spec_double ("grip-size",
                                                        _("Grip size"),
                                                        _("Size in user coordinates to draw grips"),
                                                        0, G_MAXDOUBLE, 100,
                                                        param_flags));
  g_object_class_install_property (gobject_class, PROP_RENDER_FLAGS,
                                   g_param_spec_flags ("render-flags",
                                                       _("Rendering flags"),
                                                       _("Flags controlling rendering"),
                                                       EDA_TYPE_RENDERER_FLAGS,
                                                       FLAG_HINTING | FLAG_TEXT_ORIGIN,
                                                       param_flags));
}

static void
eda_renderer_init (EdaRenderer *renderer)
{
  renderer->priv = G_TYPE_INSTANCE_GET_PRIVATE (renderer,
                                                EDA_TYPE_RENDERER,
                                                EdaRendererPrivate);

  /* Set some sensible default options */
  renderer->priv->font_name = g_strdup (DEFAULT_FONT_NAME);
  renderer->priv->override_color = -1;
  renderer->priv->grip_size = 100;

  /* Font metrics are expensive to compute, so we need to cache them. */
  renderer->priv->metrics_cache =
    g_hash_table_new_full (g_int_hash, g_int_equal, g_free,
                           (GDestroyNotify) pango_font_metrics_unref);
}

static GObject *
eda_renderer_constructor (GType type,
                          guint n_construct_properties,
                          GObjectConstructParam *construct_params) {
  GObject *object;
  GObjectClass *parent_object_class;

  parent_object_class = G_OBJECT_CLASS (eda_renderer_parent_class);
  object = parent_object_class->constructor (type, n_construct_properties,
                                             construct_params);

  return object;
}

static void
eda_renderer_dispose (GObject *object)
{
  EdaRenderer *renderer = (EdaRenderer *) object;

  if (renderer->priv->pc != NULL) {
    g_object_unref (renderer->priv->pc);
    renderer->priv->pc = NULL;
  }

  if (renderer->priv->pl != NULL) {
    g_object_unref (renderer->priv->pl);
    renderer->priv->pl = NULL;
  }

  if (renderer->priv->pr != NULL) {
    g_object_unref (renderer->priv->pr);
    renderer->priv->pr = NULL;
  }

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_renderer_parent_class)->dispose (object);
}

static void
eda_renderer_finalize (GObject *object)
{
  EdaRenderer *renderer = (EdaRenderer *) object;

  g_hash_table_destroy (renderer->priv->metrics_cache);
  renderer->priv->metrics_cache = NULL;

  cairo_destroy (renderer->priv->cr);
  renderer->priv->cr = NULL;

  g_free (renderer->priv->font_name);
  renderer->priv->font_name = NULL;

  /* Chain up to the parent class */
  G_OBJECT_CLASS (eda_renderer_parent_class)->finalize (object);

}

static void
eda_renderer_set_property (GObject *object, guint property_id,
                           const GValue *value, GParamSpec *pspec)
{
  EdaRenderer *renderer = EDA_RENDERER (object);

  switch (property_id) {
  case PROP_CAIRO_CONTEXT:
    eda_renderer_update_contexts (renderer,
                                  (cairo_t *) g_value_get_pointer (value),
                                  NULL);
    break;
  case PROP_PANGO_CONTEXT:
    eda_renderer_update_contexts (renderer, NULL,
                                  PANGO_CONTEXT (g_value_get_pointer (value)));
    break;
  case PROP_FONT_NAME:
    if (renderer->priv->font_name != NULL)
      g_free (renderer->priv->font_name);
    renderer->priv->font_name = g_value_dup_string (value);
    /* Clear font metrics cache */
    g_hash_table_remove_all (renderer->priv->metrics_cache);
    break;
  case PROP_COLOR_MAP:
    renderer->priv->color_map = g_value_get_pointer (value);
    break;
  case PROP_OVERRIDE_COLOR:
    renderer->priv->override_color = g_value_get_int (value);
    break;
  case PROP_GRIP_SIZE:
    renderer->priv->grip_size = g_value_get_double (value);
    break;
  case PROP_RENDER_FLAGS:
    renderer->priv->flags = g_value_get_flags (value);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_renderer_get_property (GObject *object, guint property_id,
                           GValue *value, GParamSpec *pspec)
{
  EdaRenderer *renderer = EDA_RENDERER (object);

  switch (property_id) {
  case PROP_CAIRO_CONTEXT:
    g_value_set_pointer (value, renderer->priv->cr);
    break;
  case PROP_PANGO_CONTEXT:
    g_value_set_pointer (value, renderer->priv->pc);
    break;
  case PROP_FONT_NAME:
    g_value_set_string (value, renderer->priv->font_name);
    break;
  case PROP_COLOR_MAP:
    g_value_set_pointer (value, renderer->priv->color_map);
    break;
  case PROP_OVERRIDE_COLOR:
    g_value_set_int (value, renderer->priv->override_color);
    break;
  case PROP_GRIP_SIZE:
    g_value_set_double (value, renderer->priv->grip_size);
    break;
  case PROP_RENDER_FLAGS:
    g_value_set_flags (value, renderer->priv->flags);
    break;
  default:
    G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
  }
}

static void
eda_renderer_update_contexts (EdaRenderer *renderer, cairo_t *new_cr,
                              PangoContext *new_pc)
{
  /* First figure out what's invalidated */
  if (new_cr != NULL) {
    cairo_destroy (renderer->priv->cr);
    renderer->priv->cr = NULL;
    if (renderer->priv->pr != NULL) {
      g_object_unref (renderer->priv->pr);
      renderer->priv->pr = NULL;
    }

    /* If the PangoContext was created from the previous Cairo
     * context, it needs destroying too. */
    if (renderer->priv->pc_from_cr) {
      if (renderer->priv->pc != NULL) {
        g_object_unref (renderer->priv->pc);
        renderer->priv->pc = NULL;
      }
      if (renderer->priv->pl != NULL) {
        g_object_unref (renderer->priv->pl);
        renderer->priv->pl = NULL;
      }
    }

    renderer->priv->cr = cairo_reference (new_cr);
  }

  if (new_pc != NULL) {
    if (renderer->priv->pc != NULL) {
      g_object_unref (G_OBJECT (renderer->priv->pc));
      renderer->priv->pc = NULL;
    }
    if (renderer->priv->pl != NULL) {
      g_object_unref (G_OBJECT (renderer->priv->pl));
      renderer->priv->pl = NULL;
    }

    renderer->priv->pc = g_object_ref (G_OBJECT (new_pc));
    renderer->priv->pc_from_cr = 0;
  }

  /* Now recreate anything necessary */
  if ((renderer->priv->pc == NULL) && (renderer->priv->cr != NULL)) {
    renderer->priv->pc = pango_cairo_create_context (renderer->priv->cr);
    renderer->priv->pc_from_cr = 1;
  }

  if ((renderer->priv->pl == NULL) && (renderer->priv->pc != NULL)) {
    renderer->priv->pl = pango_layout_new (renderer->priv->pc);
  }

  if ((renderer->priv->pr == NULL) && (renderer->priv->cr != NULL)) {
    renderer->priv->pr =
      (EdaPangoRenderer *) eda_pango_renderer_new (renderer->priv->cr);
  }
}

/* ================================================================
 * OBJECT DRAWING
 * ================================================================ */

static void
eda_renderer_draw_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;

  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    eda_renderer_draw (renderer, (OBJECT *) iter->data);
  }
}

void
eda_renderer_draw (EdaRenderer *renderer, OBJECT *object)
{
  g_return_if_fail (EDA_IS_RENDERER(renderer));

  EDA_RENDERER_GET_CLASS (renderer)->draw (renderer, object);
}

static void
eda_renderer_default_draw (EdaRenderer *renderer, OBJECT *object)
{
  void (*draw_func)(EdaRenderer *, OBJECT *);

  g_return_if_fail (object != NULL);
  g_return_if_fail (renderer->priv->cr != NULL);
  g_return_if_fail (renderer->priv->pl != NULL);
  g_return_if_fail (renderer->priv->color_map != NULL);

  if (!eda_renderer_is_drawable (renderer, object)) return;

  switch (object->type) {
  case OBJ_LINE:        draw_func = eda_renderer_draw_line; break;
  case OBJ_NET:         draw_func = eda_renderer_draw_net; break;
  case OBJ_BUS:         draw_func = eda_renderer_draw_bus; break;
  case OBJ_PIN:         draw_func = eda_renderer_draw_pin; break;
  case OBJ_BOX:         draw_func = eda_renderer_draw_box; break;
  case OBJ_ARC:         draw_func = eda_renderer_draw_arc; break;
  case OBJ_CIRCLE:      draw_func = eda_renderer_draw_circle; break;
  case OBJ_PATH:        draw_func = eda_renderer_draw_path; break;
  case OBJ_TEXT:        draw_func = eda_renderer_draw_text; break;
  case OBJ_PICTURE:     draw_func = eda_renderer_draw_picture; break;

  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER: draw_func = eda_renderer_draw_complex; break;

  default:
    g_return_if_reached ();
  }

  eda_renderer_set_color (renderer, geda_object_get_drawing_color (object));
  draw_func (renderer, object);
}

static void
eda_renderer_set_color (EdaRenderer *renderer, int color)
{
  if (renderer->priv->override_color != -1) {
    color = renderer->priv->override_color;
  }
  eda_cairo_set_source_color (renderer->priv->cr, color,
                              renderer->priv->color_map);
}

static int
eda_renderer_is_drawable_color (EdaRenderer *renderer, int color,
                                int use_override)
{
  GArray *map = renderer->priv->color_map;
  /* Check for override color */
  if ((renderer->priv->override_color >= 0) && use_override) {
    color = renderer->priv->override_color;
  }
  /* If color index out of color map bounds, don't draw */
  g_return_val_if_fail ((map != NULL), FALSE);
  g_return_val_if_fail ((color >= 0) || (color < map->len), FALSE);

  /* Otherwise, return enabled flag of object's color */
  return (&g_array_index (map, GedaColor, color))->enabled;
}

static int
eda_renderer_is_drawable (EdaRenderer *renderer, OBJECT *object)
{
  int color = geda_object_get_drawing_color (object);

  /* Always attempt to draw complex objects */
  if ((object->type == OBJ_COMPLEX) || (object->type == OBJ_PLACEHOLDER)) {
    return TRUE;
  }
  return eda_renderer_is_drawable_color (renderer, color, TRUE);
}

static int
eda_renderer_draw_hatch (EdaRenderer *renderer, OBJECT *object)
{
  void (*hatch_func)(void *, gint, gint, GArray *);
  void *hatch_data;
  GArray *fill_lines;
  int i;

  /* Horrible horrible hacks! */
  switch (object->type) {
  case OBJ_BOX:
    hatch_func = (void *) m_hatch_box;
    hatch_data = (void (*)(void *, gint, gint, GArray *)) object->box;
    break;
  case OBJ_CIRCLE:
    hatch_func = (void *) m_hatch_circle;
    hatch_data = (void (*)(void *, gint, gint, GArray *)) object->circle;
    break;
  case OBJ_PATH:
    hatch_func = (void *) m_hatch_path;
    hatch_data = (void (*)(void *, gint, gint, GArray *)) object->path;
    break;
  default:
    g_return_val_if_reached (FALSE);
  }

  /* Handle solid and hollow fill types */
  switch (object->fill_type) {
  case FILLING_MESH:
  case FILLING_HATCH:
    break;
  case FILLING_FILL:
    return TRUE;
  case FILLING_HOLLOW:
    return FALSE;
  default:
    g_return_val_if_reached (FALSE);
  }

  /* Handle mesh and hatch fill types */
  fill_lines = g_array_new (FALSE, FALSE, sizeof (GedaLine));
  if (geda_fill_type_draw_first_hatch (object->fill_type)) {
    hatch_func (hatch_data, object->fill_angle1, object->fill_pitch1, fill_lines);
  }
  if (geda_fill_type_draw_second_hatch (object->fill_type)) {
    hatch_func (hatch_data, object->fill_angle2, object->fill_pitch2, fill_lines);
  }

  /* Draw fill pattern */
  for (i = 0; i < fill_lines->len; i++) {
    GedaLine *line = &g_array_index (fill_lines, GedaLine, i);
    eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    END_NONE, object->fill_width,
                    line->x[0], line->y[0], line->x[1], line->y[1]);
  }
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_NONE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->fill_width),
                    -1, -1);

  g_array_free (fill_lines, TRUE);
  return FALSE;
}

static void
eda_renderer_draw_complex (EdaRenderer *renderer, OBJECT *object)
{
  /* Recurse */
  eda_renderer_draw_list (renderer, object->complex->prim_objs);
}

static void
eda_renderer_draw_line (EdaRenderer *renderer, OBJECT *object)
{
  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  object->line_end,
                  object->line_width,
                  object->line->x[0], object->line->y[0],
                  object->line->x[1], object->line->y[1]);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_type,
                    object->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_width),
                    object->line_length,
                    object->line_space);
}

static void
eda_renderer_draw_net (EdaRenderer *renderer, OBJECT *object)
{
  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, NET_WIDTH,
                  object->line->x[0], object->line->y[0],
                  object->line->x[1], object->line->y[1]);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, NET_WIDTH),
                    -1, -1);
}

static void
eda_renderer_draw_bus (EdaRenderer *renderer, OBJECT *object)
{
  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, BUS_WIDTH,
                  object->line->x[0], object->line->y[0],
                  object->line->x[1], object->line->y[1]);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, BUS_WIDTH),
                    -1, -1);
}

static void
eda_renderer_draw_pin (EdaRenderer *renderer, OBJECT *object)
{
  int width = geda_pin_object_get_width (object);

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_SQUARE, width,
                  object->line->x[0], object->line->y[0],
                  object->line->x[1], object->line->y[1]);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_SQUARE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, width),
                    -1, -1);
}

static void
eda_renderer_draw_box (EdaRenderer *renderer, OBJECT *object)
{
  int fill_solid = FALSE;

  /* Hatch box */
  fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of box */
  eda_cairo_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 object->line_width,
                 object->box->lower_x, object->box->lower_y,
                 object->box->upper_x, object->box->upper_y);
  if (fill_solid) cairo_fill_preserve (renderer->priv->cr);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_type, object->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_width),
                    object->line_length, object->line_space);
}

static void
eda_renderer_draw_arc (EdaRenderer *renderer, OBJECT *object)
{
  eda_cairo_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 object->line_width,
                 geda_arc_object_get_center_x (object),
                 geda_arc_object_get_center_y (object),
                 geda_arc_object_get_radius (object),
                 geda_arc_object_get_start_angle (object),
                 geda_arc_object_get_sweep_angle (object));

  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_type,
                    object->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_width),
                    object->line_length,
                    object->line_space);
}

static void
eda_renderer_draw_circle (EdaRenderer *renderer, OBJECT *object)
{
  int fill_solid = FALSE;

  /* Hatch circle */
  fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of circle */
  eda_cairo_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                 object->line_width,
                 geda_circle_object_get_center_x (object),
                 geda_circle_object_get_center_y (object),
                 geda_circle_object_get_radius (object),
                 0, 360);
  if (fill_solid) cairo_fill_preserve (renderer->priv->cr);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_type, object->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_width),
                    object->line_length, object->line_space);
}

static void
eda_renderer_draw_path (EdaRenderer *renderer, OBJECT *object)
{
  int fill_solid = FALSE;

  /* Hatch path */
  fill_solid = eda_renderer_draw_hatch (renderer, object);

  /* Draw outline of path */
  eda_cairo_path (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  object->line_width, object->path->num_sections,
                  object->path->sections);

  if (fill_solid) cairo_fill_preserve (renderer->priv->cr);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    object->line_type, object->line_end,
                    EDA_RENDERER_STROKE_WIDTH (renderer, object->line_width),
                    object->line_length, object->line_space);
}

static void
eda_renderer_draw_text (EdaRenderer *renderer, OBJECT *object)
{
  double x, y;
  double dummy = 0, small_dist = TEXT_MARKER_SIZE;

  /* First check if this is hidden text. */
  if (object->visibility == INVISIBLE
      && !EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {
    return;
  }

  /* Also, check that we actually need to display a string */
  if (object->text->disp_string == NULL)
    return;

  /* If text outline mode is selected, draw an outline */
  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_OUTLINE)) {
    eda_cairo_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                   0, object->bounds.min_x, object->bounds.max_y,
                   object->bounds.max_x, object->bounds.min_y);
    eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_SQUARE,
                      EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                      -1, -1);
    return;
  }

  /* Otherwise, actually draw the text */
  cairo_save (renderer->priv->cr);
  if (eda_renderer_prepare_text (renderer, object)) {
    eda_pango_renderer_show_layout (renderer->priv->pr, renderer->priv->pl,
                                    0, 0);
    cairo_restore (renderer->priv->cr);
  } else {
    cairo_restore (renderer->priv->cr);
    return;
  }

  /* If the text is flagged invisible, and we're showing hidden text,
   * draw a little "I". */
  if (object->visibility != INVISIBLE)
    return;

  /* Check that color is enabled */
  if (!eda_renderer_is_drawable_color (renderer, TEXT_MARKER_COLOR, FALSE))
    return;

  /* If the text marker is too tiny, don't draw it. */
  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
    cairo_user_to_device_distance (renderer->priv->cr, &small_dist, &dummy);
    if (small_dist < 1) return;
  }

  eda_renderer_set_color (renderer, TEXT_MARKER_COLOR);

  /* Centre of marker is just below and to the right of the text
   * object's origin. */
  x = object->text->x + 2 * TEXT_MARKER_SIZE;
  y = object->text->y - 2 * TEXT_MARKER_SIZE;

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,  /* Top */
                  x - TEXT_MARKER_SIZE, y + TEXT_MARKER_SIZE,
                  x + TEXT_MARKER_SIZE, y + TEXT_MARKER_SIZE);
  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,  /* Vertical */
                  x, y + TEXT_MARKER_SIZE,
                  x, y - TEXT_MARKER_SIZE);
  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,  /* Bottom */
                  x - TEXT_MARKER_SIZE, y - TEXT_MARKER_SIZE,
                  x + TEXT_MARKER_SIZE, y - TEXT_MARKER_SIZE);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_NONE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                    -1, -1);
}

static int
eda_renderer_get_font_descent (EdaRenderer *renderer,
                               PangoFontDescription *desc)
{
  PangoFontMetrics *metrics;
  int size = pango_font_description_get_size (desc);
  int *key;

  /* Lookup the font size in the metrics cache, and get the metrics
   * from there if available. Otherwise, calculate the metrics and
   * cache them. */
  metrics = g_hash_table_lookup (renderer->priv->metrics_cache, &size);
  if (metrics == NULL) {
    metrics = pango_context_get_metrics (renderer->priv->pc, desc, NULL);
    key = g_new (int, 1);
    *key = size;
    g_hash_table_insert (renderer->priv->metrics_cache, key, metrics);
  }
  return pango_font_metrics_get_descent (metrics);
}

static int
eda_renderer_prepare_text (EdaRenderer *renderer, const GedaObject *object)
{
  gint angle;
  double points_size, dx, dy;
  int size, descent;
  char *draw_string;
  cairo_font_options_t *options;
  PangoFontDescription *desc;
  PangoAttrList *attrs;

  points_size = geda_text_object_get_size_in_points (object);
  size = lrint (points_size * PANGO_SCALE);

  /* Set hinting as appropriate */
  options = cairo_font_options_create ();
  cairo_font_options_set_hint_metrics (options, CAIRO_HINT_METRICS_OFF);
  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
    cairo_font_options_set_hint_style (options, CAIRO_HINT_STYLE_MEDIUM);
  } else {
    cairo_font_options_set_hint_style (options, CAIRO_HINT_STYLE_NONE);
  }
  pango_cairo_context_set_font_options (renderer->priv->pc, options);
  cairo_font_options_destroy (options);

  pango_cairo_context_set_resolution (renderer->priv->pc, 1000);

  /* Set font name and size, and obtain descent metric */
  desc = pango_font_description_from_string (renderer->priv->font_name);
  pango_font_description_set_size (desc, size);
  pango_layout_set_font_description (renderer->priv->pl, desc);
  descent = eda_renderer_get_font_descent (renderer, desc);
  pango_font_description_free (desc);

  /* Extract text to display and Pango text attributes, and then set
   * up layout. */
  if (!eda_pango_parse_overbars (object->text->disp_string, -1,
                                 &attrs, &draw_string)) {
    return FALSE;
  }
  pango_layout_set_text (renderer->priv->pl, draw_string, -1);
  pango_layout_set_attributes (renderer->priv->pl, attrs);
  g_free (draw_string);
  pango_attr_list_unref (attrs);

  /* Calculate text position. */
  eda_renderer_calc_text_position (renderer, object, descent, &dx, &dy);

  cairo_translate (renderer->priv->cr, object->text->x, object->text->y);

  /* Special case turns upside-down text back upright */
  angle = geda_text_object_get_angle (object);
  if (angle != 180) {
    cairo_rotate (renderer->priv->cr, M_PI * angle / 180.);
  }

  cairo_scale (renderer->priv->cr, 1, -1);
  cairo_translate (renderer->priv->cr, dx, dy);

  if (EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING)) {
  /* NB: Shift the position by 0.5px to match the hinting applied to single
   *     pixel wide lines. This means the text will sit correctly on top of
   *     the grid lines, and ensures consistency with other lines when the
   *     page view is zoomed out. */
    dx = 0.5; dy = 0.5;
    cairo_device_to_user_distance (renderer->priv->cr, &dx, &dy);
    cairo_translate (renderer->priv->cr, dx, dy);
  }

  pango_cairo_update_layout (renderer->priv->cr, renderer->priv->pl);
  return TRUE;
}

/* Calculate position to draw text relative to text origin marker, in
 * world coordinates. */
static void
eda_renderer_calc_text_position (EdaRenderer *renderer, const GedaObject *object,
                                 int descent, double *x, double *y)
{
  PangoRectangle inked_rect, logical_rect;
  double temp;
  double y_lower, y_middle, y_upper;
  double x_left, x_middle, x_right;

  pango_layout_get_extents (renderer->priv->pl,
                            &inked_rect, &logical_rect);

  x_left = 0;
  x_middle = -logical_rect.width / 2.0;
  x_right = -logical_rect.width;

  /*! \note Ideally, we would be using just font / logical metrics for vertical
   *        alignment, however this way seems to be more backward compatible
   *        with the old gschem rendering.
   *
   *        Lower alignment is at the baseline of the bottom text line, whereas
   *        middle and upper alignment is based upon the inked extents of the
   *        entire text block.
   */
  y_upper  = -inked_rect.y;                     /* Top of inked extents */
  y_middle = y_upper - inked_rect.height / 2.;  /* Middle of inked extents */
  y_lower  = descent - logical_rect.height;     /* Baseline of bottom line */

  /* Special case flips attachment point to opposite corner when
   * the text is rotated to 180 degrees, since the drawing code
   * does not rotate the text to be shown upside down.
   */
  if (geda_text_object_get_angle (object) == 180) {
    temp = y_lower; y_lower = y_upper; y_upper = temp;
    temp = x_left;  x_left  = x_right; x_right = temp;
  }

  switch (geda_text_object_get_alignment (object)) {
    default:
      /* Fall through to LOWER_left case */
    case LOWER_LEFT:    *y = y_lower;  *x = x_left;   break;
    case MIDDLE_LEFT:   *y = y_middle; *x = x_left;   break;
    case UPPER_LEFT:    *y = y_upper;  *x = x_left;   break;
    case LOWER_MIDDLE:  *y = y_lower;  *x = x_middle; break;
    case MIDDLE_MIDDLE: *y = y_middle; *x = x_middle; break;
    case UPPER_MIDDLE:  *y = y_upper;  *x = x_middle; break;
    case LOWER_RIGHT:   *y = y_lower;  *x = x_right;  break;
    case MIDDLE_RIGHT:  *y = y_middle; *x = x_right;  break;
    case UPPER_RIGHT:   *y = y_upper;  *x = x_right;  break;
  }

  *x /= PANGO_SCALE;
  *y /= PANGO_SCALE;
}

static void
eda_renderer_draw_picture (EdaRenderer *renderer, OBJECT *object)
{
  int swap_wh;
  double orig_width, orig_height;
  GdkPixbuf *pixbuf;

  /* Get a pixbuf. If image doesn't exist, libgeda should
   * provide a fallback image. */
  pixbuf = g_object_ref (object->picture->pixbuf);

  /* If no pixbuf was found, fall back to drawing an outline */
  if (pixbuf == NULL || EDA_RENDERER_CHECK_FLAG (renderer,
                                                 FLAG_PICTURE_OUTLINE)) {
    eda_cairo_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                   0,
                   object->picture->lower_x, object->picture->lower_y,
                   object->picture->upper_x, object->picture->upper_y);
    eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_SQUARE,
                      EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                      -1, -1);
    return;
  }

  g_return_if_fail (GDK_IS_PIXBUF (pixbuf));

  cairo_save (renderer->priv->cr);

  swap_wh = ((object->picture->angle == 90) || (object->picture->angle == 270));
  orig_width  = swap_wh ? gdk_pixbuf_get_height (object->picture->pixbuf)
                        : gdk_pixbuf_get_width (object->picture->pixbuf);
  orig_height = swap_wh ? gdk_pixbuf_get_width (object->picture->pixbuf)
                        : gdk_pixbuf_get_height (object->picture->pixbuf);

  cairo_translate (renderer->priv->cr,
                   object->picture->upper_x, object->picture->upper_y);
  cairo_scale (renderer->priv->cr,
               fabs (object->picture->upper_x - object->picture->lower_x) / orig_width,
               - fabs (object->picture->upper_y - object->picture->lower_y) / orig_height);

  /* Evil magic translates picture origin to the right position for a given rotation */
  switch (object->picture->angle) {
    case 0:                                                                    break;
    case 90:   cairo_translate (renderer->priv->cr, 0,          orig_height);  break;
    case 180:  cairo_translate (renderer->priv->cr, orig_width, orig_height);  break;
    case 270:  cairo_translate (renderer->priv->cr, orig_width, 0          );  break;
  }

  cairo_rotate (renderer->priv->cr, -object->picture->angle * M_PI / 180.);
  if (object->picture->mirrored) {
    cairo_translate (renderer->priv->cr, gdk_pixbuf_get_width (pixbuf), 0);
    cairo_scale (renderer->priv->cr, -1, 1);
  }

  gdk_cairo_set_source_pixbuf (renderer->priv->cr,
                               object->picture->pixbuf, 0,0);
  cairo_rectangle (renderer->priv->cr, 0, 0,
                   gdk_pixbuf_get_width (object->picture->pixbuf),
                   gdk_pixbuf_get_height (object->picture->pixbuf));

  cairo_clip (renderer->priv->cr);
  cairo_paint (renderer->priv->cr);

  cairo_restore (renderer->priv->cr);
  g_object_unref (pixbuf);
}

/* ================================================================
 * GRIP DRAWING
 * ================================================================ */

static void
eda_renderer_draw_grips_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;
  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    eda_renderer_draw_grips (renderer, (OBJECT *) iter->data);
  }
}

void
eda_renderer_draw_grips (EdaRenderer *renderer, OBJECT *object)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));

  EDA_RENDERER_GET_CLASS (renderer)->draw_grips (renderer, object);
}

static void
eda_renderer_default_draw_grips (EdaRenderer *renderer, OBJECT *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_return_if_fail (renderer->priv->cr != NULL);

  if (!eda_renderer_is_drawable (renderer, object))
    return;
  if (!eda_renderer_is_drawable_color (renderer, GRIP_STROKE_COLOR, FALSE))
    return;

  switch (object->type) {
  case OBJ_LINE:
  case OBJ_NET:
  case OBJ_BUS:
  case OBJ_PIN:
    eda_renderer_draw_grips_impl (renderer, GRIP_SQUARE, 2,
        object->line->x[0], object->line->y[0],
        object->line->x[1], object->line->y[1]);
    break;
  case OBJ_BOX:
    eda_renderer_draw_grips_impl (renderer, GRIP_SQUARE, 4,
        object->box->upper_x, object->box->upper_y,
        object->box->lower_x, object->box->upper_y,
        object->box->upper_x, object->box->lower_y,
        object->box->lower_x, object->box->lower_y);
    break;
  case OBJ_ARC:
    eda_renderer_draw_arc_grips (renderer, object);
    break;
  case OBJ_CIRCLE:
    /* Grip at bottom right of containing square */
    eda_renderer_draw_grips_impl (renderer, GRIP_SQUARE, 1,
        geda_circle_object_get_center_x (object) + geda_circle_object_get_radius (object),
        geda_circle_object_get_center_y (object) - geda_circle_object_get_radius (object));
    break;
  case OBJ_PATH:
    eda_renderer_draw_path_grips (renderer, object);
    break;
  case OBJ_TEXT:
    eda_renderer_draw_text_grips (renderer, object);
    break;
  case OBJ_PICTURE:
    eda_renderer_draw_grips_impl (renderer, GRIP_SQUARE, 4,
        object->picture->upper_x, object->picture->upper_y,
        object->picture->lower_x, object->picture->upper_y,
        object->picture->upper_x, object->picture->lower_y,
        object->picture->lower_x, object->picture->lower_y);
    break;
  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER:
    /* No grips */
    break;
  default:
    g_return_if_reached ();
  }
}

static void
eda_renderer_draw_grips_impl (EdaRenderer *renderer, int type, int n_grips, ...)
{
  va_list coordinates;
  int i;

  va_start (coordinates, n_grips);
  for (i = 0; i < n_grips; i++) {
    int x = va_arg (coordinates, int);
    int y = va_arg (coordinates, int);

    switch (type) {
    case GRIP_SQUARE:
      eda_cairo_center_box (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                            0, 0, x, y,
                            renderer->priv->grip_size,
                            renderer->priv->grip_size);
      break;
    case GRIP_CIRCLE:
      eda_cairo_center_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                            0, 0, x, y,
                            renderer->priv->grip_size,
                            0, 360);
      break;
    default:
      va_end (coordinates);
      g_return_if_reached ();
    }

    eda_cairo_set_source_color (renderer->priv->cr, GRIP_FILL_COLOR,
                                renderer->priv->color_map);
    cairo_fill_preserve (renderer->priv->cr);

    eda_cairo_set_source_color (renderer->priv->cr, GRIP_STROKE_COLOR,
                                renderer->priv->color_map);

    eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      TYPE_SOLID, END_NONE,
                      0, -1, -1);
  }
  va_end (coordinates);
}

static void
eda_renderer_draw_arc_grips (EdaRenderer *renderer, OBJECT *object)
{
  double radius, start_angle, sweep_angle;
  int x1, y1, x2, y2, x3, y3;

  /*
   * An arc has three grips:
   * <DL>
   *   <DT>*</DT><DD>one at the center that allows changes on the
   *                 radius - at (<B>x</B>,<B>y</B>).
   *   <DT>*</DT><DD>one at the start of the arc - at (<B>x1</B>,<B>y1</B>).
   *   <DT>*</DT><DD>one at the end of the arc - at (<B>x2</B>,<B>y2</B>).
   */

  x1 = geda_arc_object_get_center_x (object);
  y1 = geda_arc_object_get_center_y (object);

  radius      = geda_arc_object_get_radius (object);
  start_angle = geda_arc_object_get_start_angle (object);
  sweep_angle = geda_arc_object_get_sweep_angle (object);

  x2 = x1 + radius * cos ( start_angle                * M_PI / 180);
  y2 = y1 + radius * sin ( start_angle                * M_PI / 180);
  x3 = x1 + radius * cos ((start_angle + sweep_angle) * M_PI / 180);
  y3 = y1 + radius * sin ((start_angle + sweep_angle) * M_PI / 180);

  eda_renderer_draw_grips_impl (renderer, GRIP_SQUARE, 3,
                                x1, y1, /* center */
                                x2, y2, /* start_angle */
                                x3, y3); /* end_angle */
}

static void
eda_renderer_draw_path_grips (EdaRenderer *renderer, OBJECT *object)
{
  int i, last_x = 0, last_y = 0, next_x, next_y;
  for (i = 0; i < object->path->num_sections; i++) {
    PATH_SECTION *section = object->path->sections + i;

    if (section->code != PATH_END) {
      next_x = section->x3;
      next_y = section->y3;
    }

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point lines */
      eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      END_NONE, 0,
                      last_x, last_y, section->x1, section->y1);
      eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                      END_NONE, 0,
                      next_x, next_y, section->x2, section->y2);
      eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        TYPE_SOLID, END_NONE,
                        EDA_RENDERER_STROKE_WIDTH (renderer, 0), -1, -1);
      /* Two control point grips */
      eda_renderer_draw_grips_impl (renderer, GRIP_CIRCLE, 2,
                                    section->x1, section->y1,
                                    section->x2, section->y2);
      /* Deliberately fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      last_x = next_x;
      last_y = next_y;
      /* One control point grip */
      eda_renderer_draw_grips_impl (renderer, GRIP_SQUARE, 1,
                                    section->x3, section->y3);
      break;
    case PATH_END:
      break;
    }
  }
}

static void
eda_renderer_draw_text_grips (EdaRenderer *renderer, OBJECT *object)
{
  double dummy = 0, small_dist = TEXT_MARKER_SIZE;
  int x = object->text->x;
  int y = object->text->y;

  /* First check if this is hidden text. */
  if (object->visibility == INVISIBLE
      && !EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {
    return;
  }

  /* Check that color is enabled */
  if (!eda_renderer_is_drawable_color (renderer, TEXT_MARKER_COLOR, FALSE))
    return;

  /* If the text marker is too tiny, don't draw it. */
  cairo_user_to_device_distance (renderer->priv->cr, &small_dist, &dummy);
  if (small_dist < 1) return;

  eda_cairo_set_source_color (renderer->priv->cr, TEXT_MARKER_COLOR,
                              renderer->priv->color_map);

  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,
                  x - TEXT_MARKER_SIZE, y - TEXT_MARKER_SIZE,
                  x + TEXT_MARKER_SIZE, y + TEXT_MARKER_SIZE);
  eda_cairo_line (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                  END_NONE, 0,
                  x - TEXT_MARKER_SIZE, y + TEXT_MARKER_SIZE,
                  x + TEXT_MARKER_SIZE, y - TEXT_MARKER_SIZE);
  eda_cairo_stroke (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                    TYPE_SOLID, END_NONE,
                    EDA_RENDERER_STROKE_WIDTH (renderer, 0),
                    -1, -1);
}

/* ================================================================
 * CUE DRAWING
 * ================================================================ */

static void
eda_renderer_draw_cues_list (EdaRenderer *renderer, GList *objects)
{
  GList *iter;

  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    eda_renderer_draw_cues (renderer, (OBJECT *) iter->data);
  }
}

void
eda_renderer_draw_cues (EdaRenderer *renderer, OBJECT *object)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  EDA_RENDERER_GET_CLASS (renderer)->draw_cues (renderer, object);
}

static void
eda_renderer_default_draw_cues (EdaRenderer *renderer, OBJECT *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (renderer->priv->cr != NULL);

  switch (object->type) {
  case OBJ_LINE:
  case OBJ_BOX:
  case OBJ_ARC:
  case OBJ_CIRCLE:
  case OBJ_PATH:
  case OBJ_TEXT:
  case OBJ_PICTURE:
    break;
  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER:
    /* Recurse */
    eda_renderer_draw_cues_list (renderer, object->complex->prim_objs);
    break;
  case OBJ_NET:
  case OBJ_BUS:
    eda_renderer_draw_mid_cues (renderer, object);
    eda_renderer_draw_end_cues (renderer, object, 0);
    eda_renderer_draw_end_cues (renderer, object, 1);
    break;
  case OBJ_PIN:
    g_return_if_fail ((object->whichend == 1) || (object->whichend == 0));
    eda_renderer_draw_end_cues (renderer, object, object->whichend);
    break;
  default:
    g_return_if_reached ();
  }
}

static void
eda_renderer_draw_end_cues (EdaRenderer *renderer, OBJECT *object, int end)
{
  int x = object->line->x[end], y = object->line->y[end];
  int conn_count = 0;
  int conn_type = CONN_ENDPOINT;
  int is_bus = FALSE;
  GList *iter;

  /* We should never be at the unconnectable end of a pin */
  g_return_if_fail ((object->type != OBJ_PIN) || (object->whichend == end));

  /* Check whether the current object is a bus or bus pin */
  is_bus = ((object->type == OBJ_BUS)
            || ((object->type == OBJ_PIN)
                && (object->pin_type == PIN_TYPE_BUS)));

  for (iter = object->conn_list; iter != NULL; iter = g_list_next (iter)) {
    CONN *conn = (CONN *) iter->data;
    if ((conn->x != x) || (conn->y != y)) continue;

    /* Check whether the connected object is a bus or bus pin */
    is_bus |= ((conn->other_object->type == OBJ_BUS)
               || ((conn->other_object->type == OBJ_PIN)
                   && (conn->other_object->pin_type == PIN_TYPE_BUS)));

    if (conn->type == CONN_MIDPOINT) {
      /* If it's a mid-line connection, we can stop already. */
      conn_type = CONN_MIDPOINT;
      break;
    }

    conn_count++;
  }

  /* Draw a midpoint, if necessary */
  if ((conn_type == CONN_MIDPOINT)
      || ((object->type == OBJ_NET) && (conn_count > 1))) {
    eda_renderer_draw_junction_cue (renderer, x, y, is_bus);
    return;
  }

  /* Only things left to be drawn are end point cues */
  if (!eda_renderer_is_drawable_color (renderer, NET_ENDPOINT_COLOR, TRUE))
    return;
  eda_renderer_set_color (renderer, NET_ENDPOINT_COLOR);

  switch (object->type) {
  case OBJ_NET:
  case OBJ_PIN:
    /* If less than one thing was connected to this end of the net
     * segment or pin, draw box cue */
    if (conn_count > 0) break;

    eda_cairo_center_box (renderer->priv->cr,
                          EDA_RENDERER_CAIRO_FLAGS (renderer),
                          -1, -1, x, y, CUE_BOX_SIZE, CUE_BOX_SIZE);
    cairo_fill (renderer->priv->cr);
    break;

  case OBJ_BUS:
    break;
  default:
    g_return_if_reached ();
  }
}

static void
eda_renderer_draw_mid_cues (EdaRenderer *renderer, OBJECT *object)
{
  GList *iter;
  for (iter = object->conn_list; iter != NULL; iter = g_list_next (iter)) {
    CONN *conn = (CONN *) iter->data;

    if (conn->type == CONN_MIDPOINT) {
      int is_bus = ((object->type == OBJ_BUS)
                    || (conn->other_object->type == OBJ_BUS)
                    || ((conn->other_object->type == OBJ_PIN)
                        && (conn->other_object->pin_type == PIN_TYPE_BUS)));
      eda_renderer_draw_junction_cue (renderer, conn->x, conn->y, is_bus);
    }
  }
}

static void
eda_renderer_draw_junction_cue (EdaRenderer *renderer, int x, int y, int is_bus)
{
  double width = (is_bus ? BUS_WIDTH : NET_WIDTH);
  double radius = (is_bus ? JUNCTION_CUE_SIZE_BUS : JUNCTION_CUE_SIZE_NET) / 2.0;

  if (!eda_renderer_is_drawable_color (renderer, JUNCTION_COLOR, 1)) {
    return;
  }

  eda_cairo_center_arc (renderer->priv->cr, EDA_RENDERER_CAIRO_FLAGS (renderer),
                        width, -1, x, y, radius, 0, 360);
  eda_renderer_set_color (renderer, JUNCTION_COLOR);
  cairo_fill (renderer->priv->cr);
}

/* ================================================================
 * RENDERED BOUNDS
 * ================================================================ */

gboolean
eda_renderer_get_user_bounds (EdaRenderer *renderer,
                              const GedaObject *object,
                              double *left,
                              double *top,
                              double *right,
                              double *bottom)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), FALSE);

  return EDA_RENDERER_GET_CLASS (renderer)->user_bounds (renderer, object,
                                                         left, top,
                                                         right, bottom);
}

static gboolean
eda_renderer_default_get_user_bounds (EdaRenderer *renderer,
                                      const GedaObject *object,
                                      double *left,
                                      double *top,
                                      double *right,
                                      double *bottom)
{
  g_return_val_if_fail ((object != NULL), FALSE);
  g_return_val_if_fail ((renderer->priv->cr != NULL), FALSE);

  switch (object->type) {
  case OBJ_TEXT:
    return eda_renderer_get_text_user_bounds (renderer, object,
                                              left, top, right, bottom);
  case OBJ_LINE:
  case OBJ_BOX:
  case OBJ_ARC:
  case OBJ_CIRCLE:
  case OBJ_PATH:
  case OBJ_PICTURE:
  case OBJ_COMPLEX:
  case OBJ_PLACEHOLDER:
  case OBJ_NET:
  case OBJ_BUS:
  case OBJ_PIN:
    /* No rendered bounds available for most OBJECT types. */
    return FALSE;
  default:
    g_return_val_if_reached (FALSE);
  }
}

static gboolean
eda_renderer_get_text_user_bounds (EdaRenderer *renderer,
                                   const GedaObject *object,
                                   double *left,
                                   double *top,
                                   double *right,
                                   double *bottom)
{
  PangoRectangle inked_rect, logical_rect;

  /* First check if this is hidden text. */
  if (object->visibility == INVISIBLE
      && !EDA_RENDERER_CHECK_FLAG (renderer, FLAG_TEXT_HIDDEN)) {
    return FALSE;
  }

  /* Also, check that we actually need to display a string */
  if (object->text->disp_string == NULL)
    return FALSE;

  cairo_save (renderer->priv->cr);

  /* Set up the text and check it worked. */
  if (!eda_renderer_prepare_text (renderer, object))
    return FALSE;

  /* Figure out the bounds, send them back.  Note that Pango thinks in
   * device coordinates, but we need world coordinates. */
  pango_layout_get_pixel_extents (renderer->priv->pl,
                                  &inked_rect, &logical_rect);
  *left = (double) inked_rect.x;
  *top = (double) inked_rect.y;
  *right = (double) inked_rect.x + inked_rect.width;
  *bottom = (double) inked_rect.y + inked_rect.height;
  cairo_user_to_device (renderer->priv->cr, left, top);
  cairo_user_to_device (renderer->priv->cr, right, bottom);

  cairo_restore (renderer->priv->cr);

  cairo_device_to_user (renderer->priv->cr, left, top);
  cairo_device_to_user (renderer->priv->cr, right, bottom);

  return TRUE;
}


/* ================================================================
 * MISCELLANEOUS (CREATION, DESTRUCTION, ACCESSORS)
 * ================================================================ */

EdaRenderer *
eda_renderer_new (cairo_t *cr, PangoContext *pc)
{
  return g_object_new (EDA_TYPE_RENDERER,
                       "cairo-context", cr,
                       "pango-context", pc,
                       NULL);
}

void
eda_renderer_destroy (EdaRenderer *renderer)
{
  g_object_unref (G_OBJECT (renderer));
}

cairo_t *
eda_renderer_get_cairo_context (EdaRenderer *renderer)
{
  cairo_t *cr;
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  g_object_get (G_OBJECT (renderer), "cairo-context", &cr, NULL);
  return cr;
}

gboolean
eda_renderer_get_hinting_enabled (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), FALSE);
  return EDA_RENDERER_CHECK_FLAG (renderer, FLAG_HINTING);
}

GArray *
eda_renderer_get_color_map (EdaRenderer *renderer)
{
  GArray *map = NULL;
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), NULL);
  g_object_get (G_OBJECT (renderer), "color-map", &map, NULL);
  return map;
}

void
eda_renderer_set_color_map (EdaRenderer *renderer, GArray *map)
{
  g_return_if_fail (EDA_IS_RENDERER (renderer));
  g_object_set (G_OBJECT (renderer), "color-map", map, NULL);
}

int
eda_renderer_get_cairo_flags (EdaRenderer *renderer)
{
  g_return_val_if_fail (EDA_IS_RENDERER (renderer), 0);
  return EDA_RENDERER_CAIRO_FLAGS (renderer);
}
