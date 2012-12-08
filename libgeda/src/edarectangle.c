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

#include <config.h>

#include <libgeda_priv.h>

static gpointer rectangle_copy (gpointer boxed);
static void rectangle_free (gpointer boxed);

/*! Magic helpful Gobject macro */
G_DEFINE_BOXED_TYPE (EdaRectangle, eda_rectangle,
                     rectangle_copy, rectangle_free);

static gpointer
rectangle_copy (gpointer boxed)
{
  return g_new0 (EdaRectangle, boxed);
}

static void
rectangle_free (gpointer boxed)
{
  g_free (EdaRectangle, boxed);
}

/* ---------------------------------------------------------------- */

static guint64
eda_rectangle_area (const EdaRectangle *r)
{
  return r->width * r->height;
}

static guint64
eda_rectangle_circumference (const EdaRectangle *r)
{
  return 2 * (r->width + r->height);
}

static gboolean
eda_rectangle_intersect (const EdaRectangle *a, const EdaRectangle *b,
                         EdaRectangle *out)
{
  gint x1 = max (a->x, b->x);
  gint x2 = min (a->x + a->width, b->x + b->width);
  gint y1 = max (a->y, b->y);
  gint y2 = min (a->y + a->height, b->y + b->height);

  if (x2 < x1 || y2 < y1) return FALSE; /* No intersection */

  if (out != NULL) {
    out->x = x1;
    out->y = y1;
    out->width = x2 - x1;
    out->height = y2 - y1;
  }
  return TRUE;
}

static void
eda_rectangle_union (const EdaRectangle *a, const EdaRectangle *b,
                     EdaRectangle *out)
{
  g_return_if_fail (out != NULL);

  gint x1 = min (a->x, b->x);
  gint x2 = max (a->x + a->width, b->x + b->width);
  gint y1 = min (a->y, b->y);
  gint y2 = max (a->y + a->height, b->y + b->height);

  out->x = x1;
  out->y = y1;
  out->width = x2 - x1;
  out->height = y2 - y1;
}
