/* Lepton EDA library
 * Copyright (C) 2010-2012 gEDA Contributors
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
 * This file based on GDK's gdkpango.h (LGPL V2+)
 * Copyright (C) 2000 Red Hat, Inc.
 */

/*!
 * \file edapangorenderer.h
 *
 * \brief Pango renderer
 */

#ifndef __EDA_PANGO_RENDERER_H__
#define __EDA_PANGO_RENDERER_H__

G_BEGIN_DECLS

#define EDA_TYPE_PANGO_RENDERER (eda_pango_renderer_get_type ())
#define EDA_PANGO_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), EDA_TYPE_PANGO_RENDERER, EdaPangoRenderer))
#define EDA_IS_PANGO_RENDERER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), EDA_TYPE_PANGO_RENDERER))
#define EDA_PANGO_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), EDA_TYPE_PANGO_RENDERER), EdaPangoRendererClass)
#define EDA_IS_PANGO_RENDERER_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), EDA_TYPE_PANGO_RENDERER))
#define EDA_PANGO_RENDERER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), EDA_TYPE_PANGO_RENDERER, EdaPangoRendererClass))

typedef struct _EdaPangoRendererClass EdaPangoRendererClass;
typedef struct _EdaPangoRenderer EdaPangoRenderer;
typedef struct _EdaPangoRendererPrivate EdaPangoRendererPrivate;

struct _EdaPangoRendererClass
{
  PangoRendererClass parent_class;
};

struct _EdaPangoRenderer
{
  PangoRenderer parent_instance;

  /* Private members */
  EdaPangoRendererPrivate *priv;
};

GType eda_pango_renderer_get_type (void) G_GNUC_CONST;
PangoRenderer *eda_pango_renderer_new (cairo_t *cr) G_GNUC_WARN_UNUSED_RESULT;
void eda_pango_renderer_show_layout (EdaPangoRenderer *renderer,
                                     PangoLayout *pl, double x, double y);

/* ---------------------------------------- */

typedef struct _EdaPangoAttrOverbar EdaPangoAttrOverbar;
struct _EdaPangoAttrOverbar
{
  PangoAttribute attr;
  gboolean overbar;
};

PangoAttrClass *eda_pango_attr_overbar_get_class (void) G_GNUC_CONST;
PangoAttribute *eda_pango_attr_overbar_new (gboolean overbar) G_GNUC_WARN_UNUSED_RESULT;
gboolean eda_is_pango_attr_overbar (PangoAttribute *attr);
gboolean eda_pango_parse_overbars (const gchar *overbar_text,
                                   int length,
                                   PangoAttrList **attr_list,
                                   gchar **text);

G_END_DECLS

#endif /* !__EDA_PANGO_RENDERER_H__ */
