/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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

#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define DOTS_POINTS_ARRAY_SIZE       5000
#define DOTS_VARIABLE_MODE_SPACING   30

#define MESH_COARSE_GRID_MULTIPLIER  5


/*! \brief Draw an area of the screen with a dotted grid pattern
 *
 *  \par Function Description
 *  Draws the dotted grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
static void draw_dots_grid_region (GSCHEM_TOPLEVEL *w_current,
                                   int x, int y, int width, int height)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int i, j;
  int dot_x, dot_y;
  int x_start, y_start, x_end, y_end;
  int count = 0;
  GdkPoint points[DOTS_POINTS_ARRAY_SIZE];

  int incr;
  int screen_incr;

  if (w_current->dots_grid_mode == DOTS_GRID_VARIABLE_MODE) {
    /* In the variable mode around every (DOTS_VARIABLE_MODE_SPACING)'th
     * screenpixel will be grid-point. */
    /* adding 0.1 for correct cast*/
    incr = round_5_2_1 (toplevel->page_current->to_world_x_constant *
                        DOTS_VARIABLE_MODE_SPACING) + 0.1;

    /* limit minimum grid spacing to grid to snap_size */
    if (incr < toplevel->snap_size) {
      incr = toplevel->snap_size;
    }
  } else {
    /* Fixed size grid in world coorinates */
    incr = toplevel->snap_size;
    screen_incr = SCREENabs (toplevel, incr);
    if (screen_incr < w_current->dots_grid_fixed_threshold) {
      /* don't draw the grid if the on-screen spacing is less than the threshold */
      return;
    }
  }

  /* update status bar */
  i_set_grid (w_current, incr);

  gdk_gc_set_foreground (w_current->gc, x_get_color (DOTS_GRID_COLOR));

  SCREENtoWORLD (toplevel, x - 1, y + height + 1, &x_start, &y_start);
  SCREENtoWORLD (toplevel, x + width + 1, y - 1, &x_end, &y_end);

  /* figure starting grid coordinates, work by taking the start
   * and end coordinates and rounding down to the nearest
   * increment */
  x_start -= (x_start % incr);
  y_start -= (y_start % incr);

  for (i = x_start; i <= x_end; i = i + incr) {
    for(j = y_start; j <= y_end; j = j + incr) {
      WORLDtoSCREEN (toplevel, i,j, &dot_x, &dot_y);
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
static void draw_mesh (GSCHEM_TOPLEVEL *w_current, int color,
                       int x_start, int y_start, int x_end, int y_end,
                       int incr, int coarse_mult)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int i, j;
  int x1, y1, x2, y2;
  int next_coarse_x, next_coarse_y;
  int coarse_incr = incr * coarse_mult;

  gdk_gc_set_foreground (w_current->gc, x_get_color (color));

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

  for (j = y_start; j < y_end; j = j + incr) {

    /* Skip lines which will be drawn in the coarser grid */
    if (j == next_coarse_y) {
      next_coarse_y += coarse_incr;
      continue;
    }
    WORLDtoSCREEN (toplevel, x_start, j, &x1, &y1);
    WORLDtoSCREEN (toplevel, x_end,   j, &x2, &y2);
    gdk_draw_line (w_current->drawable, w_current->gc, x1, y1, x2, y2);
  }

  for (i = x_start; i < x_end; i = i + incr) {

    /* Skip lines which will be drawn in the coarser grid */
    if (j == next_coarse_y) {
      next_coarse_y += coarse_incr;
      continue;
    }
    WORLDtoSCREEN (toplevel, i, y_start, &x1, &y1);
    WORLDtoSCREEN (toplevel, i, y_end,   &x2, &y2);
    gdk_draw_line (w_current->drawable, w_current->gc, x1, y1, x2, y2);
  }
}


/*! \brief Draw an area of the screen with a mesh grid pattern
 *
 *  \par Function Description
 *  Draws the mesh grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
static void draw_mesh_grid_region (GSCHEM_TOPLEVEL *w_current,
                                   int x, int y, int width, int height)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x_start, y_start, x_end, y_end;
  int incr;
  int screen_incr;

  incr = toplevel->snap_size;
  screen_incr = SCREENabs (toplevel, incr);

  /* update status bar */
  i_set_grid (w_current, incr);

  SCREENtoWORLD (toplevel, x - 1, y + height + 1, &x_start, &y_start);
  SCREENtoWORLD (toplevel, x + width + 1, y - 1, &x_end, &y_end);

  /* Draw the fine grid if its on-screen spacing is large enough */
  if (screen_incr >= w_current->mesh_grid_display_threshold) {
    draw_mesh (w_current, MESH_GRID_MINOR_COLOR, x_start, y_start,
               x_end, y_end, incr, MESH_COARSE_GRID_MULTIPLIER);
  }

  incr *= MESH_COARSE_GRID_MULTIPLIER;
  screen_incr = SCREENabs (toplevel, incr);

  /* Draw the coarse grid if its on-screen spacing is large enough */
  if (screen_incr >= w_current->mesh_grid_display_threshold) {
    draw_mesh (w_current, MESH_GRID_MAJOR_COLOR,
               x_start, y_start, x_end, y_end, incr, 0);
  }
}


/*! \brief Draw an area of the screen with the current grid pattern.
 *
 *  \par Function Description
 *  Draws the desired grid pattern over a given region of the screen.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL.
 *  \param [in] x          The left screen coordinate for the drawing.
 *  \param [in] y          The top screen coordinate for the drawing.
 *  \param [in] width      The width of the region to draw.
 *  \param [in] height     The height of the region to draw.
 */
void x_grid_draw_region (GSCHEM_TOPLEVEL *w_current,
                         int x, int y, int width, int height)
{
  switch (w_current->grid) {
    case GRID_NONE:
      i_set_grid(w_current, -1);
      return;

    case GRID_DOTS:
      draw_dots_grid_region (w_current, x, y, width, height);
      break;

    case GRID_MESH:
      draw_mesh_grid_region (w_current, x, y, width, height);
      break;
  }

#if DEBUG
  /* highly temp, just for diag purposes */
  x_draw_tiles(w_current);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_draw_tiles(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  TILE *t_current;
  GdkFont *font;
  int i,j;
  int x1, y1, x2, y2;
  int screen_x, screen_y;
  int width, height;
  char *tempstring;

  gdk_gc_set_foreground (w_current->gc, x_get_color (LOCK_COLOR));

  font = gdk_fontset_load ("fixed");
  for (j = 0; j < MAX_TILES_Y; j++) {
    for (i = 0; i < MAX_TILES_X; i++) {
      t_current = &toplevel->page_current->world_tiles[i][j];
      WORLDtoSCREEN(toplevel, t_current->left,
                    t_current->top, &x1, &y1);
      WORLDtoSCREEN(toplevel, t_current->right,
                    t_current->bottom, &x2, &y2);

      screen_x = min(x1, x2);
      screen_y = min(y1, y2);

      width = abs(x1 - x2);
      height = abs(y1 - y2);

#if DEBUG
      printf("x, y: %d %d\n", screen_x, screen_y);
      printf("w x h: %d %d\n", width, height);
#endif
      gdk_draw_rectangle (w_current->drawable,
                          w_current->gc,
                          FALSE, screen_x, screen_y,
                          width, height);

      tempstring = g_strdup_printf("%d %d", i, j);

      gdk_draw_text (w_current->drawable,
                     font,
                     w_current->gc,
                     screen_x+10, screen_y+10,
                     tempstring,
                     strlen(tempstring));
      g_free(tempstring);
    }
  }

  gdk_font_unref(font);
}
