/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 2012 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#ifndef __EDA_RECTANGLE_H__
#define __EDA_RECTANGLE_H__

G_BEGIN_DECLS

#define EDA_TYPE_RECTANGLE (eda_rectangle_get_type ())

typedef struct _EdaRectangle EdaRectangle;

struct _EdaRectangle
{
  gint32 x, y;
  guint32 width, height;
};

/* ---------------------------------------------------------------- */

static guint64 eda_rectangle_area (const EdaRectangle *rect);
static guint64 eda_rectangle_circumference (const EdaRectangle *r);

static gboolean eda_rectangle_intersect (const EdaRectangle *a,
                                         const EdaRectangle *b,
                                         EdaRectangle *out);

static void eda_rectangle_union (const EdaRectangle *a,
                                 const EdaRectangle *b,
                                 EdaRectangle *out);

G_END_DECLS

#endif /* !__EDA_RECTANGLE_H__ */
