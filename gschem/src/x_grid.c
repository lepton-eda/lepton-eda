/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static GdkPoint points[5000];

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_grid_draw(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int i, j;
  int x, y;
  int x_start, y_start;
  int count = 0;

  int incr = 100;
  int screen_incr = 0;

  if (!w_current->grid) {
    i_set_grid(w_current, -1);
    return;
  }

  if (w_current->grid_mode == GRID_VARIABLE_MODE)
  {
    /* In the variable mode around every 30th screenpixel will be grid-point */
    /* adding 0.1 for correct cast*/
    incr = round_5_2_1(toplevel->page_current->to_world_x_constant *30)+0.1;

    /*limit grid to snap_size; only a idea of mine, hope you like it (hw) */
    if (incr < toplevel->snap_size) {
      incr = toplevel->snap_size;
    }
    /* usually this should never happen */
    if (incr < 1){
      incr = 1;
    }
  }
  else
  {
    incr = toplevel->snap_size;
    screen_incr = SCREENabs(toplevel, incr);
    if (screen_incr < w_current->grid_fixed_threshold)
    {
      /* don't draw the grid if the screen incr spacing is less than the */
      /* threshold */
      return;
    }
  }
  
  /* update status bar */
  i_set_grid(w_current, incr);

#if DEBUG 
  printf("---------x_grid_draw\n incr: %d\n",incr);

  printf("x1 %d\n", pix_x(w_current, 100));
  printf("x2 %d\n", pix_x(w_current, 200));
  printf("y1 %d\n", pix_y(w_current, 100));
  printf("y2 %d\n", pix_y(w_current, 200));
#endif

  gdk_gc_set_foreground(w_current->gc,
                        x_get_color(w_current->grid_color));

  /* figure starting grid coordinates, work by taking the start
   * and end coordinates and rounding down to the nearest
   * increment */
  x_start = (toplevel->page_current->left -
             (toplevel->page_current->left % incr));
  y_start = (toplevel->page_current->top -
             (toplevel->page_current->top  % incr));

  for (i = x_start; i < toplevel->page_current->right; i = i + incr) {
    for(j = y_start; j < toplevel->page_current->bottom; j = j + incr) {
      WORLDtoSCREEN(toplevel, i,j, &x, &y);
      if (inside_region(toplevel->page_current->left,
                        toplevel->page_current->top,
                        toplevel->page_current->right,
                        toplevel->page_current->bottom,
                        i, j)) {

	if (w_current->grid_dot_size == 1)
        {
          points[count].x = x;
          points[count].y = y;
          count++;

          /* get out of loop if more than 1000 points */
          if (count == 5000) {
            gdk_draw_points(
                            w_current->backingstore,
                            w_current->gc, points, count);
            count=0;
          }
        }
        else
        {
          gdk_draw_arc(w_current->backingstore, w_current->gc,
                       TRUE, x, y,
                       w_current->grid_dot_size,
                       w_current->grid_dot_size, 0, FULL_CIRCLE);
        }
      }
    }
  }

  /* now draw all the points in one step */
  if(count != 0) {
    gdk_draw_points(w_current->backingstore,
                    w_current->gc, points, count);
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

  gdk_gc_set_foreground(w_current->gc, x_get_color(w_current->lock_color));

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
      gdk_draw_rectangle(w_current->backingstore, 
                         w_current->gc, 
                         FALSE, screen_x, screen_y,
                         width, height);

      tempstring = g_strdup_printf("%d %d", i, j);

      gdk_draw_text (w_current->backingstore,
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
