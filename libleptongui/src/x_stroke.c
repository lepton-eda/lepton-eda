/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#include "gschem.h"

#ifdef HAVE_LIBSTROKE

/* libstroke seems to not have C++ support declarations so we add
   them explicitly here */
G_BEGIN_DECLS
#include <stroke.h>
G_END_DECLS

/*
 * <B>stroke_points</B> is an array of points for the stroke
 * footprints. The points of the stroke are displayed over the display
 * area of the main window. They have to be erased when the stroke is
 * translated and the sequence evaluated.
 *
 * Its size will never exceed <B>STROKE_MAX_POINTS</B> (the limit in
 * number of points of a stroke provided by libstroke).
 */
typedef struct {
  gint x, y;
} StrokePoint;

static GArray *stroke_points = NULL;


/*! \brief Initializes the stroke interface.
 *  \par Function Description
 *  This is the initialization function for the stroke interface. It
 *  initializes the libstroke library and prepare an array of points
 *  for the mouse footprints.
 *
 *  This function has to be called only once at application
 *  initialization before any use of the stroke interface.
 */
void
x_stroke_init (void)
{
  g_return_if_fail (stroke_points == NULL);

  stroke_init ();

  stroke_points = g_array_new (FALSE,
                               FALSE,
                               sizeof (StrokePoint));
}

/*! \brief Frees memory of the stroke interface.
 *  \par Function Description
 *  This function frees the memory used for the mouse footprint
 *  points. It terminates the use of the stroke interface.
 */
void
x_stroke_free (void)
{
  g_return_if_fail (stroke_points != NULL);

  g_array_free (stroke_points, TRUE);
  stroke_points = NULL;
}

/*! \brief Records a new point for the stroke.
 *  \par Function Description
 *  This function adds the point (<B>x</B>,<B>y</B>) as a new point in
 *  the stroke.
 *
 * The footprint is updated and the new point is drawn on the drawing area.
 *
 *  \param [in] w_current The GschemToplevel object.
 *  \param [in] x        The X coord of the new point.
 *  \param [in] Y        The X coord of the new point.
 */
void
x_stroke_record (GschemToplevel *w_current, gint x, gint y)
{
  cairo_matrix_t user_to_device_matrix;
  double x0, y0;
  GschemPageView *view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (view != NULL);
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (view);
  g_return_if_fail (geometry != NULL);

  g_assert (stroke_points != NULL);

  stroke_record (x, y);

  if (stroke_points->len < STROKE_MAX_POINTS) {
    StrokePoint point = { x, y };

    g_array_append_val (stroke_points, point);

    cairo_t *cr = gdk_cairo_create (gtk_widget_get_window (GTK_WIDGET(view)));
    LeptonColor *color = x_color_lookup (STROKE_COLOR);
    cairo_set_source_rgba (cr,
                           lepton_color_get_red_double (color),
                           lepton_color_get_green_double (color),
                           lepton_color_get_blue_double (color),
                           lepton_color_get_alpha_double (color));

    cairo_set_matrix (cr, gschem_page_geometry_get_world_to_screen_matrix (geometry));
    x0 = x;
    y0 = y;
    cairo_device_to_user (cr, &x0, &y0);
    cairo_get_matrix (cr, &user_to_device_matrix);
    cairo_save (cr);
    cairo_identity_matrix (cr);

    cairo_matrix_transform_point (&user_to_device_matrix, &x0, &y0);

    cairo_rectangle (cr, x0, y0, 1, 1);
    cairo_fill (cr);
    cairo_restore (cr);
    cairo_destroy (cr);
  }

}

/*! \brief Translates the stroke.
 *  \par Function Description
 *  This function transforms the stroke input so far into a string.
 *
 *  The stroke sequence is to be evaluated in Scheme code into a
 *  possible action by the Guile procedure <B>eval-stroke<B>. The
 *  mouse footprint is erased in this function.
 *
 *  It returns a string consisting of numbers if the stroke has
 *  been successfully translated, or NULL if libstroke failed to
 *  transform the stroke.  The user must g_free the returned
 *  string.
 *
 *  \param [in] w_current The GschemToplevel object.
 *  \returns A string representing the stroke, or NULL.
 */
char*
x_stroke_translate_and_execute (GschemToplevel *w_current)
{
  gchar sequence[STROKE_MAX_SEQUENCE];
  StrokePoint *point;
  int min_x, min_y, max_x, max_y;
  guint i;

  g_assert (stroke_points != NULL);

  if (stroke_points->len == 0)
    return NULL;

  point = &g_array_index (stroke_points, StrokePoint, 0);
  min_x = max_x = point->x;
  min_y = max_y = point->y;

  for (i = 1; i < stroke_points->len; i++) {
    point = &g_array_index (stroke_points, StrokePoint, i);
    min_x = MIN (min_x, point->x);
    min_y = MIN (min_y, point->y);
    max_x = MAX (max_x, point->x);
    max_y = MAX (max_y, point->y);
  }

  o_invalidate_rect (w_current, min_x, min_y, max_x + 1, max_y + 1);

  /* resets length of array */
  stroke_points->len = 0;

  /* Return translated stroke. */
  if (stroke_trans ((char*)&sequence)) {
    return g_strdup_printf ("%s", sequence);
  }
  return NULL;
}

#endif /* HAVE_LIBSTROKE */
