/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include "../include/globals.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
  int size;
  int x1, y1, x2, y2; /* screen coords */

  if (o_current->line == NULL) {
    return;
  }

  /* reuse line's routine */
  if ( (w_current->DONT_REDRAW == 1) ||
       (!o_line_visible(w_current, o_current->line, &x1, &y1, &x2, &y2)) ) {
    return;
  }

#if DEBUG
  printf("drawing pin\n\n");
#endif

  if (w_current->pin_style == THICK ) {
    size = SCREENabs(w_current, PIN_WIDTH);
    gdk_gc_set_line_attributes(w_current->gc, size, GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  if (w_current->override_color != -1 ) {
    gdk_gc_set_foreground(w_current->gc,
			  x_get_color(w_current->override_color));
    if (w_current->DONT_REDRAW == 0) {
      gdk_draw_line(w_current->window, w_current->gc,
		    x1, y1, x2, y2);
      gdk_draw_line(w_current->backingstore, w_current->gc,
		    x1, y1, x2, y2);
    }
  } else {
    if (w_current->DONT_REDRAW == 0) {
      gdk_gc_set_foreground(w_current->gc, x_get_color(o_current->color));
      gdk_draw_line(w_current->window, w_current->gc,
		    x1, y1, x2, y2);
      gdk_draw_line(w_current->backingstore, w_current->gc,
		    x1, y1, x2, y2);
    }
  }

  /* draw the cue directly */
  o_cue_draw_lowlevel(w_current, o_current, o_current->whichend);
  
  /* yes zero is right for the width -> use hardware lines */
  if (w_current->pin_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

#if DEBUG
  printf("drawing pin\n");
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
void o_pin_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
  w_current->override_color = w_current->background_color;
  o_pin_draw(w_current, o_current);
  w_current->override_color = -1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
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

  gdk_gc_set_foreground(w_current->xor_gc, x_get_darkcolor(color));

  if (w_current->pin_style == THICK ) {
    size = SCREENabs(w_current, PIN_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  WORLDtoSCREEN( w_current, o_current->line->x[0], o_current->line->y[0], &sx[0], &sy[0] );
  WORLDtoSCREEN( w_current, o_current->line->x[1], o_current->line->y[1], &sx[1], &sy[1] );

  gdk_draw_line(w_current->window, w_current->xor_gc,
                sx[0]+dx, sy[0]+dy,
                sx[1]+dx, sy[1]+dy);

  if (w_current->pin_style == THICK ) {
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
void o_pin_start(TOPLEVEL *w_current, int x, int y)
{
  int size;
  w_current->last_x = w_current->start_x = fix_x(w_current, x);
  w_current->last_y = w_current->start_y = fix_y(w_current, y);

  if (w_current->pin_style == THICK ) {
    size = SCREENabs(w_current, PIN_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->window, w_current->xor_gc, 
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);

  if (w_current->pin_style == THICK ) {
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
void o_pin_end(TOPLEVEL *w_current, int x, int y)
{
  int x1, y1;
  int x2, y2;
  int color;
  GList *other_objects = NULL;
  OBJECT *o_current, *o_current_pin;

  if (w_current->inside_action == 0) {
    o_redraw(w_current, w_current->page_current->object_head, TRUE);
    return;
  }

  if (w_current->override_pin_color == -1) {
    color = w_current->pin_color;
  } else {
    color = w_current->override_pin_color;
  }

  /* removed 3/15 to see if we can get pins to be ortho only */
  /* w_current->last_x = fix_x(w_current, x);
     w_current->last_y = fix_y(w_current, y);*/

  /* don't allow zero length pins */
  if ( (w_current->start_x == w_current->last_x) &&
       (w_current->start_y == w_current->last_y) ) {
         w_current->start_x = (-1);
         w_current->start_y = (-1);
         w_current->last_x = (-1);
         w_current->last_y = (-1);
         return;
  }

  SCREENtoWORLD(w_current, w_current->start_x,w_current->start_y, &x1, &y1);
  SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y, &x2, &y2);
  x1 = snap_grid(w_current, x1);
  y1 = snap_grid(w_current, y1);
  x2 = snap_grid(w_current, x2);
  y2 = snap_grid(w_current, y2);

  w_current->page_current->object_tail =
  o_pin_add(w_current,
            w_current->page_current->object_tail,
            OBJ_PIN, color,
            x1, y1, x2, y2,
            PIN_TYPE_NET, 0);

  o_current = o_current_pin = w_current->page_current->object_tail;

  if (scm_hook_empty_p(add_pin_hook) == SCM_BOOL_F &&
      o_current != NULL) {
    scm_run_hook(add_pin_hook,
		 scm_cons(g_make_object_smob(w_current, o_current),
			  SCM_EOL));
  }

  other_objects = s_conn_return_others(other_objects, o_current_pin);
  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);
  o_cue_draw_single(w_current, o_current_pin); 
  o_pin_draw(w_current, o_current_pin);

  w_current->start_x = (-1);
  w_current->start_y = (-1);
  w_current->last_x = (-1);
  w_current->last_y = (-1);
  w_current->page_current->CHANGED=1;

  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_rubberpin(TOPLEVEL *w_current, int x, int y)
{
  int size;
  int diff_x, diff_y;

  if (w_current->inside_action == 0) {
    o_redraw(w_current, w_current->page_current->object_head, TRUE);
    return;
  }

  size = SCREENabs(w_current, PIN_WIDTH);

  if (w_current->pin_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->window, w_current->xor_gc, 
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);

  w_current->last_x = fix_x(w_current, x);
  w_current->last_y = fix_y(w_current, y);

  diff_x = abs(w_current->last_x - w_current->start_x);
  diff_y = abs(w_current->last_y - w_current->start_y);

  if (diff_x >= diff_y) {
    w_current->last_y = w_current->start_y;
  } else {
    w_current->last_x = w_current->start_x;
  }

  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->window, w_current->xor_gc, 
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);

  if (w_current->pin_style == THICK ) {
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
 *  used in o_stretch.c
 */
void o_pin_eraserubber(TOPLEVEL *w_current)
{
  int size;

  if (w_current->net_style == THICK ) {
    size = SCREENabs(w_current, PIN_WIDTH);

    if (size < 0)
      size=0;

    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

#if 0
  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_color(w_current->background_color) );
#endif

  gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);

  if (w_current->net_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }
}
