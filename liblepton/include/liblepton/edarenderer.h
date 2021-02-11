/* libleptonrenderer - Rendering Lepton EDA schematics with Cairo
 * Copyright (C) 2010-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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

#ifndef __EDA_RENDERER_H__
#define __EDA_RENDERER_H__

#include <cairo.h>
#include <pango/pango.h>

G_BEGIN_DECLS

#define EDA_TYPE_RENDERER (eda_renderer_get_type ())
#define EDA_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_RENDERER, EdaRenderer))
#define EDA_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDA_TYPE_RENDERER, EdaRendererClass))
#define EDA_IS_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDA_TYPE_RENDERER))
#define EDA_IS_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((obj), EDA_TYPE_RENDERER))
#define EDA_RENDERER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_RENDERER, EdaRendererClass))

typedef struct _EdaRendererClass EdaRendererClass;
typedef struct _EdaRenderer EdaRenderer;
typedef struct _EdaRendererPrivate EdaRendererPrivate;

struct _EdaRendererClass
{
  GObjectClass parent_class;

  /* Virtual public methods */
  void (*draw)(EdaRenderer *renderer, OBJECT *object);
  void (*draw_grips)(EdaRenderer *renderer, OBJECT *object);
  void (*draw_cues)(EdaRenderer *renderer, OBJECT *object);
  gboolean (*user_bounds)(EdaRenderer *renderer, const LeptonObject *object,
                          double *left, double *top,
                          double *right, double *bottom);
};

struct _EdaRenderer
{
  GObject parent_instance;

  /* Private members */
  EdaRendererPrivate *priv;
};

#define EDA_TYPE_RENDERER_FLAGS (eda_renderer_flags_get_type ())

enum _EdaRendererFlags
{
  /* Should hinting be enabled? */
  EDA_RENDERER_FLAG_HINTING = 1 << 0,
  /* Should picture outlines be drawn instead of raster? */
  EDA_RENDERER_FLAG_PICTURE_OUTLINE = 1 << 1,
  /* Should hidden text be drawn? */
  EDA_RENDERER_FLAG_TEXT_HIDDEN = 1 << 2,
  /* Should text outlines be drawn instead of glyphs? */
  EDA_RENDERER_FLAG_TEXT_OUTLINE = 1 << 3,
  /* Should text origin markers be drawn? */
  EDA_RENDERER_FLAG_TEXT_ORIGIN = 1 << 4
};

typedef enum _EdaRendererFlags EdaRendererFlags;

GType eda_renderer_get_type (void) G_GNUC_CONST;
GType eda_renderer_flags_get_type (void) G_GNUC_CONST;

EdaRenderer *eda_renderer_new (cairo_t *cr, PangoContext *pc) G_GNUC_WARN_UNUSED_RESULT;
void eda_renderer_destroy (EdaRenderer *renderer);

void eda_renderer_draw (EdaRenderer *renderer, OBJECT *object);
void eda_renderer_draw_grips (EdaRenderer *renderer, OBJECT *object);
void eda_renderer_draw_cues (EdaRenderer *renderer, OBJECT *object);

GArray *eda_renderer_get_color_map (EdaRenderer *renderer);
void eda_renderer_set_color_map (EdaRenderer *renderer, GArray *map);

cairo_t *eda_renderer_get_cairo_context (EdaRenderer *renderer);
int eda_renderer_get_cairo_flags (EdaRenderer *renderer);

gboolean
eda_renderer_get_user_bounds (EdaRenderer *renderer,
                              const LeptonObject *object,
                              double *left,
                              double *top,
                              double *right,
                              double *bottom);

gboolean
eda_renderer_get_text_user_bounds (const LeptonObject *object,
                                   gboolean enable_hidden,
                                   double *left,
                                   double *top,
                                   double *right,
                                   double *bottom);

G_END_DECLS

#endif /* !__EDA_RENDERER_H__ */
