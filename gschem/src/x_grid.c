/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#define DOTS_POINTS_ARRAY_SIZE       5000
#define DOTS_VARIABLE_MODE_SPACING   30

#define MESH_COARSE_GRID_MULTIPLIER  5


/*! \brief Query the spacing in world coordinates at which the dots grid is drawn.
 *
 *  \par Function Description
 *  Returns the world spacing of the rendered grid, taking into account where
 *  the grid drawing code may drop elements which are too densely packed for a
 *  given zoom level.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \returns The grid spacing in world units of the grid as rendered, or -1
 *           if there are no items drawn.
 */
static int query_dots_grid_spacing (GschemToplevel *w_current)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  GschemPageGeometry *geometry = gschem_page_view_get_page_geometry (page_view);
  int incr, screen_incr;
  int snap_size = gschem_options_get_snap_size (w_current->options);

  g_return_val_if_fail (geometry != NULL, -1);
  g_return_val_if_fail (page_view != NULL, -1);

  if (w_current->dots_grid_mode == DOTS_GRID_VARIABLE_MODE) {
    /* In the variable mode around every (DOTS_VARIABLE_MODE_SPACING)'th
     * screenpixel will be grid-point. */
    /* adding 0.1 for correct cast*/
    incr = round_5_2_1 (geometry->to_world_x_constant * DOTS_VARIABLE_MODE_SPACING) + 0.1;

    /* limit minimum grid spacing to grid to snap_size */
    if (incr < snap_size) {
      incr = snap_size;
    }
  } else {
    /* Fixed size grid in world coorinates */
    incr = snap_size;
    screen_incr = gschem_page_view_SCREENabs (page_view, incr);
    if (screen_incr < w_current->dots_grid_fixed_threshold) {
      /* No grid drawn if the on-screen spacing is less than the threshold */
      incr = -1;
    }
  }
  return incr;
}


/*! \brief Draw an area of the screen with a dotted grid pattern
 *
 *  \par Function Description
 *  Draws the dotted grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
zz *  \param [in] height     The height of the region to draw.
 */
static void draw_dots_grid_region (GschemToplevel *w_current,
                                   int x, int y, int width, int height)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  int i, j;
  int dot_x, dot_y;
  int x_start, y_start, x_end, y_end;
  int count = 0;
  GdkPoint points[DOTS_POINTS_ARRAY_SIZE];

  int incr = query_dots_grid_spacing (w_current);

  if (incr == -1)
    return;

  gdk_gc_set_foreground (w_current->gc, x_get_color (DOTS_GRID_COLOR));

  gschem_page_view_SCREENtoWORLD (GSCHEM_PAGE_VIEW (w_current->drawing_area), x - 1, y + height + 1, &x_start, &y_start);
  gschem_page_view_SCREENtoWORLD (GSCHEM_PAGE_VIEW (w_current->drawing_area), x + width + 1, y - 1, &x_end, &y_end);

  /* figure starting grid coordinates, work by taking the start
   * and end coordinates and rounding down to the nearest
   * increment */
  x_start -= (x_start % incr);
  y_start -= (y_start % incr);

  for (i = x_start; i <= x_end; i = i + incr) {
    for(j = y_start; j <= y_end; j = j + incr) {
      gschem_page_view_WORLDtoSCREEN (GSCHEM_PAGE_VIEW (w_current->drawing_area), i,j, &dot_x, &dot_y);
      if (inside_region (toplevel->page_current->left,
                         toplevel->page_current->top,
                         toplevel->page_current->right,
                         toplevel->page_current->bottom, i, j)) {

        if (w_current->dots_grid_dot_size == 1) {
          points[count].x = dot_x;
          points[count].y = dot_y;
          count++;

          /* get out of loop if we're hit the end of the array */
          if (count == DOTS_POINTS_ARRAY_SIZE) {
            gdk_draw_points (w_current->drawable,
                             w_current->gc, points, count);
            count = 0;
          }
        } else {
          gdk_draw_arc (w_current->drawable, w_current->gc,
                        TRUE, dot_x, dot_y,
                        w_current->dots_grid_dot_size,
                        w_current->dots_grid_dot_size, 0, FULL_CIRCLE);
        }
      }
    }
  }

  /* now draw all the points in one step */
  if(count != 0) {
    gdk_draw_points (w_current->drawable, w_current->gc, points, count);
  }
}


/*! \brief Helper function for draw_mesh_grid_regin
 */
static void draw_mesh (GschemToplevel *w_current,
                       cairo_t *cr,
                       cairo_matrix_t *user_to_device_matrix,
                       int color,
                       int x_start, int y_start, int x_end, int y_end,
                       int incr, int coarse_mult)
{
  int i, j;
  double x1, y1, x2, y2;
  int next_coarse_x, next_coarse_y;
  int coarse_incr = incr * coarse_mult;
  COLOR *c;

  /* figure starting grid coordinates, work by taking the start
   * and end coordinates and rounding down to the nearest increment */
  x_start -= (x_start % incr);
  y_start -= (y_start % incr);

  if (coarse_incr == 0) {
    next_coarse_x = x_start - 1; /* Ensures we never hit this when looping */
    next_coarse_y = y_start - 1; /* Ensures we never hit this when looping */
  } else {
    next_coarse_x = x_start - (x_start % coarse_incr);
    next_coarse_y = y_start - (y_start % coarse_incr);
    if (next_coarse_x < x_start) next_coarse_x += coarse_incr;
    if (next_coarse_y < y_start) next_coarse_y += coarse_incr;
  }

  c = x_color_lookup (color);
  cairo_set_source_rgba (cr,
                         (double)c->r / 255.0,
                         (double)c->g / 255.0,
                         (double)c->b / 255.0,
                         (double)c->a / 255.0);

  cairo_set_line_width (cr, 1.);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_SQUARE);

  for (j = y_start; j < y_end; j = j + incr) {

    /* Skip lines which will be drawn in the coarser grid */
    if (j == next_coarse_y) {
      next_coarse_y += coarse_incr;
      continue;
    }

    x1 = x_start;
    y1 = j;
    x2 = x_end;
    y2 = j;

    cairo_matrix_transform_point (user_to_device_matrix, &x1, &y1);
    cairo_matrix_transform_point (user_to_device_matrix, &x2, &y2);

    cairo_move_to (cr, (int)(x1+0.5), (int)(y1+0.5));
    cairo_line_to (cr, (int)(x2+0.5), (int)(y2+0.5));

    cairo_stroke (cr);
  }

  for (i = x_start; i < x_end; i = i + incr) {

    /* Skip lines which will be drawn in the coarser grid */
    if (j == next_coarse_y) {
      next_coarse_y += coarse_incr;
      continue;
    }

    x1 = i;
    y1 = y_start;
    x2 = i;
    y2 = y_end;

    cairo_matrix_transform_point (user_to_device_matrix, &x1, &y1);
    cairo_matrix_transform_point (user_to_device_matrix, &x2, &y2);

    cairo_move_to (cr, (int)(x1+0.5), (int)(y1+0.5));
    cairo_line_to (cr, (int)(x2+0.5), (int)(y2+0.5));

    cairo_stroke (cr);
  }
}


/*! \brief Query the spacing in world coordinates at which the mesh grid is drawn.
 *
 *  \par Function Description
 *  Returns the world spacing of the rendered grid, taking into account where
 *  the grid drawing code may drop elements which are too densely packed for a
 *  given zoom level.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \returns The grid spacing in world units of the grid as rendered, or -1
 *           if there are no items drawn.
 */
static int query_mesh_grid_spacing (GschemToplevel *w_current)
{
  int incr, screen_incr;

  incr = gschem_options_get_snap_size (w_current->options);
  screen_incr = gschem_page_view_SCREENabs (GSCHEM_PAGE_VIEW (w_current->drawing_area), incr);

  /* We draw a fine grid if its on-screen spacing is large enough */
  if (screen_incr >= w_current->mesh_grid_display_threshold) {
    return incr;
  }

  incr *= MESH_COARSE_GRID_MULTIPLIER;
  screen_incr = gschem_page_view_SCREENabs (GSCHEM_PAGE_VIEW (w_current->drawing_area), incr);

  /* We draw a coarse grid if its on-screen spacing is large enough */
  if (screen_incr >= w_current->mesh_grid_display_threshold)
    return incr;

  return -1;
}

/*! \brief Draw an area of the screen with a mesh grid pattern
 *
 *  \par Function Description
 *  Draws the mesh grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \param [in] cr         The cairo context.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
static void
draw_mesh_grid_region (GschemToplevel *w_current, cairo_t *cr, int x, int y, int width, int height)
{
  int snap_size = gschem_options_get_snap_size (w_current->options);
  int coarse_increment = MESH_COARSE_GRID_MULTIPLIER * snap_size;
  double dummy = 0.0;
  double threshold = w_current->mesh_grid_display_threshold;

  cairo_device_to_user_distance (cr, &threshold, &dummy);

  if (coarse_increment >= threshold) {
    cairo_matrix_t user_to_device_matrix;
    double x_start = x - 1;
    double y_start = y + height + 1;
    double x_end = x + width + 1;
    double y_end = y - 1;

    cairo_device_to_user (cr, &x_start, &y_start);
    cairo_device_to_user (cr, &x_end, &y_end);

    cairo_get_matrix (cr, &user_to_device_matrix);
    cairo_save (cr);
    cairo_identity_matrix (cr);
    cairo_translate (cr, 0.5, 0.5);

    /* Draw the fine grid if its on-screen spacing is large enough */
    if (snap_size >= threshold) {
      draw_mesh (w_current,
                 cr,
                 &user_to_device_matrix,
                 MESH_GRID_MINOR_COLOR,
                 floor (x_start),
                 floor (y_start),
                 ceil (x_end),
                 ceil (y_end),
                 snap_size,
                 MESH_COARSE_GRID_MULTIPLIER);
    }

    draw_mesh (w_current,
               cr,
               &user_to_device_matrix,
               MESH_GRID_MAJOR_COLOR,
               floor (x_start),
               floor (y_start),
               ceil (x_end),
               ceil (y_end),
               coarse_increment,
               0);

    cairo_restore (cr);
  }
}


/*! \brief Draw an area of the screen with the current grid pattern.
 *
 *  \par Function Description
 *  Draws the desired grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \param [in] cr         The cairo context.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
void x_grid_draw_region (GschemToplevel *w_current,
                         cairo_t *cr,
                         int x,
                         int y,
                         int width,
                         int height)
{
  GRID_MODE grid_mode;

  g_return_if_fail (w_current != NULL);

  grid_mode = gschem_options_get_grid_mode (w_current->options);

  switch (grid_mode) {
    case GRID_MODE_NONE:
      return;

    case GRID_MODE_DOTS:
      draw_dots_grid_region (w_current, x, y, width, height);
      break;

    case GRID_MODE_MESH:
      draw_mesh_grid_region (w_current, cr, x, y, width, height);
      break;
  }

#if DEBUG
  /* highly temp, just for diag purposes */
  x_draw_tiles(w_current, cr);
#endif
}


/*! \brief Query the spacing in world coordinates at which the grid is drawn.
 *
 *  \par Function Description
 *  Returns the world spacing of the rendered grid, taking into account where
 *  the grid drawing code may drop elements which are too densely packed for a
 *  given zoom level.
 *
 *  \param [in] w_current  The GschemToplevel.
 *  \returns The grid spacing in world units of the grid as rendered, or -1
 *           if there are no items drawn.
 */
int x_grid_query_drawn_spacing (GschemToplevel *w_current)
{
  GRID_MODE grid_mode;

  g_return_if_fail (w_current != NULL);

  grid_mode = gschem_options_get_grid_mode (w_current->options);

  switch (grid_mode) {
    default:
    case GRID_MODE_NONE: return -1;
    case GRID_MODE_DOTS: return query_dots_grid_spacing (w_current);
    case GRID_MODE_MESH: return query_mesh_grid_spacing (w_current);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_draw_tiles(GschemToplevel *w_current, cairo_t *cr)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  TILE *t_current;
  int i,j;
  char *tempstring;
  GdkColor *color;
  cairo_text_extents_t extents;

  color = x_get_color (LOCK_COLOR);

  cairo_set_source_rgb (cr,
                        color->red   / 65535.0,
                        color->green / 65535.0,
                        color->blue  / 65535.0);

  cairo_select_font_face (cr,
                          "Sans",
                          CAIRO_FONT_SLANT_NORMAL,
                          CAIRO_FONT_WEIGHT_BOLD);

  cairo_set_font_size (cr, 2000);

  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      t_current = &toplevel->page_current->world_tiles[i][j];

      cairo_rectangle (cr,
                       t_current->left,
                       t_current->top,
                       t_current->right - t_current->left,
                       t_current->bottom - t_current->top);

      cairo_save (cr);

      cairo_translate (cr,
                      (t_current->left + t_current->right) / 2.0,
                      (t_current->top + t_current->bottom) / 2.0);

      tempstring = g_strdup_printf("(%d,%d)", i, j);

      cairo_scale (cr, 1.0, -1.0);

      cairo_text_extents (cr, tempstring, &extents);

      cairo_move_to (cr,
                     extents.width / -2.0,
                     extents.height / 2.0);

      cairo_show_text (cr, tempstring);

      g_free(tempstring);

      cairo_restore (cr);
    }
  }
}
