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
#include <stdio.h>
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;
  int x1, y1, x2, y2; /* screen coords */

  if (o_current == NULL) {
    return;
  }

  if (o_current->line == NULL) {
    return;
  }

  /* reuse line's routine */
  if ( (toplevel->DONT_REDRAW == 1) ||
       (!o_line_visible(toplevel, o_current->line, &x1, &y1, &x2, &y2)) ) {
    return;
  }

#if DEBUG
  printf("drawing bus\n\n");
#endif

  size = 1;

  if (toplevel->bus_style == THICK ) {
    size = SCREENabs(toplevel, BUS_WIDTH);

    if (size < 1)
      size=1;
  }

  cairo_set_line_width (w_current->cr, size);
  cairo_set_line_cap (w_current->cr, CAIRO_LINE_CAP_SQUARE);

  if (toplevel->override_color != -1 ) {
    gdk_cairo_set_source_color (w_current->cr, x_get_color(toplevel->override_color));
  } else {
    gdk_cairo_set_source_color (w_current->cr, x_get_color(o_current->color));
  }

  gschem_cairo_line (w_current->cr, END_SQUARE, size, x1, y1, x2, y2);
  cairo_stroke (w_current->cr);

#if DEBUG
  printf("drawing bus\n");
#endif

  if (o_current->selected && w_current->draw_grips) {
    o_line_draw_grips (w_current, o_current);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;
  int color;
  int sx[2], sy[2];

  if (o_current->line == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));

  if (toplevel->bus_style == THICK ) {
    size = SCREENabs(toplevel, BUS_WIDTH);
    gdk_gc_set_line_attributes(w_current->outline_xor_gc, size+1,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  WORLDtoSCREEN(toplevel, o_current->line->x[0] + dx, o_current->line->y[0] + dy, &sx[0], &sy[0] );
  WORLDtoSCREEN(toplevel, o_current->line->x[1] + dx, o_current->line->y[1] + dy, &sx[1], &sy[1] );

  gdk_draw_line (w_current->drawable, w_current->outline_xor_gc,
                 sx[0], sy[0], sx[1], sy[1]);

  /* backing store ? not approriate here */

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->outline_xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_draw_xor_single(GSCHEM_TOPLEVEL *w_current,
			   int dx, int dy, int whichone, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int color;
  int dx1= - 1, dy1 = - 1, dx2 = -1, dy2 = -1;
  int sx[2], sy[2];

  if (o_current->line == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));

  if (whichone == 0) {
    dx1 = dx;
    dy1 = dy;
    dx2 = 0;
    dy2 = 0;
  } else if (whichone == 1) {
    dx2 = dx;
    dy2 = dy;
    dx1 = 0;
    dy1 = 0;
  } else {
    fprintf(stderr, _("Got an invalid which one in o_bus_draw_xor_single\n"));
  }

  WORLDtoSCREEN(toplevel, o_current->line->x[0] + dx1, o_current->line->y[0] + dy1, &sx[0], &sy[0]);
  WORLDtoSCREEN(toplevel, o_current->line->x[1] + dx2, o_current->line->y[1] + dy2, &sx[1], &sy[1]);

  gdk_draw_line (w_current->drawable, w_current->outline_xor_gc,
                 sx[0], sy[0], sx[1], sy[1]);
}

/*! \brief set the start point of a new bus
 *  \par Function Description
 *  This function sets the start point (<B>w_x</B>,<B>w_y</B>) of a new bus
 *  in the <B>GSCHEM_TOPLEVEL</B> structure.
 *  
 *  The start point is stored in <B>first_wx</B>, <B>first_wy</B>.
 *  
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        the x position in world coords
 *  \param [in] w_x        the y position in world coords
 */
void o_bus_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \brief finish a bus drawing action
 *  \par Function Description
 *  This function finishes a net drawing action. The function draws
 *  a bus from the point (<B>first_wx</B>,<B>first_wy</B>) to 
 *  (<B>second_wx</B>,<B>second_wy</B>). Both points are taken from
 *  the <B>GSCHEM_TOPLEVEL</B> structure.
 *
 *  The function returns TRUE if a bus object has been created and 
 *  FALSE if no bus object has been created.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_x        (unused)
 */
int o_bus_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;
  int color;
  GList *prev_conn_objects = NULL;

  g_assert( w_current->inside_action != 0 );

  if (toplevel->override_bus_color == -1) {
    color = w_current->bus_color;
  } else {
    color = toplevel->override_bus_color;
  }

  /* erase the rubberbus */
  /* o_bus_invalidate_rubber (w_current); */
  w_current->rubber_visible = 0;

  /* don't allow zero length bus */
  /* this ends the bus drawing behavior we want this? hack */
  if ( (w_current->first_wx == w_current->second_wx) &&
       (w_current->first_wy == w_current->second_wy) ) {
    return FALSE;
  }

  new_obj = o_bus_new(toplevel, OBJ_BUS, color,
                      w_current->first_wx, w_current->first_wy,
                      w_current->second_wx, w_current->second_wy, 0);
  s_page_append (toplevel->page_current, new_obj);

  o_invalidate (w_current, new_obj);

  /* connect the new bus to the other busses */
  prev_conn_objects = s_conn_return_others (prev_conn_objects, new_obj);
  o_invalidate_glist (w_current, prev_conn_objects);
  g_list_free (prev_conn_objects);

  toplevel->page_current->CHANGED=1;
  w_current->first_wx = w_current->second_wx;
  w_current->first_wy = w_current->second_wy;
  o_undo_savestate(w_current, UNDO_ALL);
  return TRUE;
}

/*! \brief draw the bus rubber when creating a bus
 *  \par Function Description
 *  This function draws
 *  a bus rubber from the point (<B>first_wx</B>,<B>first_wy</B>) from  
 *  the <B>GSCHEM_TOPLEVEL</B> structure to the input parameter
 *  (<B>w_x</B>, <B>w_y</B>).
 *
 *  The function stores creates an non-orthogonal bus segment if the 
 *  CONTROLKEY is pressed. The coordinates of the second rubberbus point
 *  is stored as (<B>second_wx</B>,<B>second_wy</B>) in the 
 *  <B>GSCHEM_TOPLEVEL</B> structure.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        current x position in world units
 *  \param [in] w_y        current y position in world units
 */
void o_bus_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  g_assert( w_current->inside_action != 0 );

  if (w_current->rubber_visible)
    o_bus_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* If you press the control key then you can draw non-ortho bus */
  if (!w_current->CONTROLKEY) {
    diff_x = abs(w_current->second_wx - w_current->first_wx);
    diff_y = abs(w_current->second_wy - w_current->first_wy);

    if (diff_x >= diff_y) {
      w_current->second_wy = w_current->first_wy;
    } else {
      w_current->second_wx = w_current->first_wx;
    }
  }

  o_bus_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int min_x, min_y, max_x, max_y;
  int bloat = 0;

  WORLDtoSCREEN (toplevel, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (toplevel, w_current->second_wx, w_current->second_wy, &x2, &y2);

  if (toplevel->bus_style == THICK ) {
    bloat = SCREENabs(toplevel, BUS_WIDTH) / 2;
  }

  min_x = min (x1, x2) - bloat;
  max_x = max (x1, x2) + bloat;
  min_y = min (y1, y2) - bloat;
  max_y = max (y1, y2) + bloat;

  o_invalidate_rect (w_current, min_x, min_y, max_x, max_y);
}

/*! \brief draw a rubberbus segment in XOR mode
 *  \par Function Description
 *  This function draws a bus segment in XOR mode from the point
 *  (<B>first_wx</B>,<B>first_wy</B>) to the point 
 *  (<B>second_wx</B>,<B>second_wy</B>) from the <B>GSCHEM_TOPLEVEL</B>
 *   structure.
 *
 *  The function can be used to draw or erase the rubberbus on the screen.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object
 */
void o_bus_rubberbus_xor(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2, size=0;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy, &x2, &y2);

  if (toplevel->bus_style == THICK ) {
    size = SCREENabs(toplevel, BUS_WIDTH);
    
    if (size < 0)
      size=0;

    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color));
  gdk_draw_line (w_current->drawable, w_current->xor_gc, x1, y1, x2, y2);

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }
}
