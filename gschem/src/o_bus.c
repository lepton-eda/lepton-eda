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
#include <stdio.h>
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

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

  if (toplevel->bus_style == THICK ) {
    size = SCREENabs(toplevel, BUS_WIDTH);

    if (size < 0)
      size=0;

    gdk_gc_set_line_attributes(w_current->gc, size, GDK_LINE_SOLID,
                               GDK_CAP_BUTT,
                               GDK_JOIN_MITER);
  }

  if (toplevel->override_color != -1 ) {
    gdk_gc_set_foreground(w_current->gc,
                          x_get_color(toplevel->override_color));
    gdk_draw_line(w_current->backingstore, w_current->gc,
                  x1, y1, x2, y2);
  } else {
    gdk_gc_set_foreground(w_current->gc,
                          x_get_color(o_current->color));
    gdk_draw_line(w_current->backingstore, w_current->gc,
                  x1, y1, x2, y2);
  }

  /* yes zero is right for the width -> use hardware lines */
  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

#if DEBUG
  printf("drawing bus\n");
#endif

  if (o_current->draw_grips && w_current->draw_grips == TRUE) {	
    /* pb20011109 - modified to use the new o_line_[draw|erase]_grips() */
    /*              reuse the line functions */
    if (!o_current->selected) {
      /* object is no more selected, erase the grips */
      o_current->draw_grips = FALSE;
      o_line_erase_grips(w_current, o_current);
    } else {
      /* object is selected, draw the grips */
      o_line_draw_grips(w_current, o_current);
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_erase(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  toplevel->override_color = toplevel->background_color;
  o_bus_draw(w_current, o_current);
  toplevel->override_color = -1;
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

  WORLDtoSCREEN( toplevel, o_current->line->x[0], o_current->line->y[0], &sx[0], &sy[0] );
  WORLDtoSCREEN( toplevel, o_current->line->x[1], o_current->line->y[1], &sx[1], &sy[1] );
  
  gdk_draw_line(w_current->backingstore, w_current->outline_xor_gc,
                sx[0]+dx, sy[0]+dy,
                sx[1]+dx, sy[1]+dy);

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

  WORLDtoSCREEN( toplevel, o_current->line->x[0], o_current->line->y[0], &sx[0], &sy[0] );
  WORLDtoSCREEN( toplevel, o_current->line->x[1], o_current->line->y[1], &sx[1], &sy[1] );

  gdk_draw_line(w_current->backingstore, w_current->outline_xor_gc,
                sx[0]+dx1, sy[0]+dy1,
                sx[1]+dx2, sy[1]+dy2);
  o_invalidate_rect(w_current,
                    sx[0] + dx1, sy[0] + dy1, sx[1] + dx2, sy[1] + dy2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_start(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;

  w_current->last_x = w_current->start_x = fix_x(toplevel, x);
  w_current->last_y = w_current->start_y = fix_y(toplevel, y);

  if (toplevel->bus_style == THICK ) {
    size = SCREENabs(toplevel, BUS_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->backingstore, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
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
int o_bus_end(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1;
  int x2, y2;
  int color;
  int size;
  GList *other_objects = NULL;

  g_assert( w_current->inside_action != 0 );

  if (toplevel->override_bus_color == -1) {
    color = w_current->bus_color;
  } else {
    color = toplevel->override_bus_color;
  }

  size = SCREENabs(toplevel, BUS_WIDTH);

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
    gdk_gc_set_line_attributes(w_current->gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  /* don't allow zero length bus */
  /* this ends the bus drawing behavior we want this? hack */
  if ( (w_current->start_x == w_current->last_x) &&
       (w_current->start_y == w_current->last_y) ) {
         w_current->start_x = (-1);
         w_current->start_y = (-1);
         w_current->last_x = (-1);
         w_current->last_y = (-1);
         w_current->inside_action=0;
	 i_set_state(w_current, STARTDRAWBUS);
         o_bus_eraserubber(w_current);
         return(FALSE);
       }

  gdk_gc_set_foreground(w_current->gc,
			x_get_color(color));
  gdk_draw_line(w_current->backingstore, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  SCREENtoWORLD(toplevel, w_current->start_x, w_current->start_y, &x1, &y1);
  SCREENtoWORLD(toplevel, w_current->last_x, w_current->last_y, &x2, &y2);
  x1 = snap_grid(toplevel, x1);
  y1 = snap_grid(toplevel, y1);
  x2 = snap_grid(toplevel, x2);
  y2 = snap_grid(toplevel, y2);

  w_current->save_x = w_current->last_x;
  w_current->save_y = w_current->last_y;

  toplevel->page_current->object_tail =
  o_bus_add(toplevel,
            toplevel->page_current->object_tail,
            OBJ_BUS,
            color,
            x1, y1, x2, y2, 0);

  /* conn stuff */
  other_objects = s_conn_return_others(other_objects,
                                       toplevel->page_current->
                                       object_tail);
  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);
  o_cue_draw_single(w_current, toplevel->page_current->object_tail);

  toplevel->page_current->CHANGED=1;
  w_current->start_x = w_current->save_x;
  w_current->start_y = w_current->save_y;
  o_undo_savestate(w_current, UNDO_ALL);
  return(TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_rubberbus(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;
  int size;

  g_assert( w_current->inside_action != 0 );

  if (toplevel->bus_style == THICK ) {
    size = SCREENabs(toplevel, BUS_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->backingstore, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  /* going into ortho mode (control key not pressed) */
  /* erase non-ortho line */

  /* going into non-ortho mode (control key pressed) */
  /* erase ortho line */

  w_current->last_x = fix_x(toplevel, x);
  w_current->last_y = fix_y(toplevel, y);

  /* If you press the control key then you can draw non-ortho bus */
  if (!w_current->CONTROLKEY) {
    diff_x = abs(w_current->last_x - w_current->start_x);
    diff_y = abs(w_current->last_y - w_current->start_y);

    if (diff_x >= diff_y) {
      w_current->last_y = w_current->start_y;
    } else {
      w_current->last_x = w_current->start_x;
    }
  }

  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->backingstore, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  used in button cancel code in x_events.c
 */
void o_bus_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;

  if (toplevel->bus_style == THICK ) {
    size = SCREENabs(toplevel, BUS_WIDTH);

    if (size < 0)
      size=0;

    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_draw_line(w_current->backingstore, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  if (toplevel->bus_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }
}
