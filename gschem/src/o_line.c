/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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

#include <stdio.h>
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 */
void o_line_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rect (w_current, x1, y1, x2, y2);
}

/*! \brief Start process to input a new line.
 *  \par Function Description
 *  This function starts the process of interactively adding a line to
 *  the current sheet.
 *
 *  During all the process, the line is internally represented by the two
 *  ends of the line as (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  A temporary line is drawn during the process with the selection color
 *  and changed according to the position of the mouse pointer.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe line */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  o_line_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of a line.
 *  \par Function Description
 *  This function ends the process of interactively adding a line to the
 *  current sheet.
 *
 *  It first erases the last temporary line displayed, calculates the
 *  corresponding world coordinates of the two ends of the line and finally
 *  adds a new initialized line object to the list of object of the current
 *  sheet.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_line_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;

  g_assert( w_current->inside_action != 0 );

  /* Don't bother.. the real object is invalidated, its in the same place */
  /* o_line_invalidate_rubber (w_current); */
  w_current->rubber_visible = 0;

  /* don't allow zero length lines */
  if ( (w_current->first_wx == w_current->second_wx) &&
       (w_current->first_wy == w_current->second_wy) ) {
    return;
  }

  /* create the line object and draw it */
  new_obj = o_line_new (toplevel, OBJ_LINE, GRAPHIC_COLOR,
                        w_current->first_wx, w_current->first_wy,
                        w_current->second_wx, w_current->second_wy);
  s_page_append (toplevel, toplevel->page_current, new_obj);

  /* Call add-objects-hook */
  g_run_hook_object (w_current, "%add-objects-hook", new_obj);

  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw temporary line while dragging end.
 *  \par Function Description
 *  This function manages the erase/update/draw process of temporary line
 *  when modifying one end of the line.
 *  The line is described by four <B>*w_current</B> variables : the first end
 *  of the line is (<B>first_wx</B>,<B>first_wy</B>), the second end is
 *  (<B>second_wx</B>,<B>second_wy</B>).
 *  The first end is constant. The second end is updated to the (<B>w_x</B>,<B>w_y</B>).
 * 
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  g_assert( w_current->inside_action != 0 );

  if (w_current->rubber_visible)
    o_line_invalidate_rubber (w_current);

  /*
   * The coordinates of the moving end of the line are updated. Its new
   * coordinates are in <B>w_x</B> and <B>w_y</B> parameters and saved to
   * <B>w_current->second_wx</B> and <B>w_current->second_wy</B> respectively.
   */ 
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  
  /* if the control key was pressed then draw ortho lines */
  if (w_current->CONTROLKEY) {
    diff_x = abs(w_current->second_wx - w_current->first_wx);
    diff_y = abs(w_current->second_wy - w_current->first_wy);
    
    if (diff_x >= diff_y) {
      w_current->second_wy = w_current->first_wy;
    } else {
      w_current->second_wx = w_current->first_wx;
    }
  }

  o_line_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Draw line from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws a line with an exclusive or function over the sheet.
 *  The color of the box is <B>SELECT_COLOR</B>. The line is
 *  described by the two points (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_line_draw_rubber (GSCHEM_TOPLEVEL *w_current, EdaRenderer *renderer)
{
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_line (cr, flags, END_NONE, wwidth,
                  w_current->first_wx, w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}

/*! \brief
 *  \par Function Description
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] line
 *  \param [in] x1
 *  \param [in] y1
 *  \param [in] x2
 *  \param [in] y2
 *  \return int
 */
int o_line_visible (GSCHEM_TOPLEVEL *w_current, LINE *line,
                    int *x1, int *y1, int *x2, int *y2)
{
  *x1 = line->x[0];  *y1 = line->y[0];
  *x2 = line->x[1];  *y2 = line->y[1];

  /* Do we want to skip clipping? */
  if (!w_current->toplevel->object_clipping)
    return TRUE;

  return WORLDclip_change (w_current, x1, y1, x2, y2);
}
