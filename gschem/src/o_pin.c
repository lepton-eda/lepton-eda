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

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;
  int x1, y1, x2, y2; /* screen coords */

  if (o_current->line == NULL) {
    return;
  }

  /* reuse line's routine */
  if ( (toplevel->DONT_REDRAW == 1) ||
       (!o_line_visible(toplevel, o_current->line, &x1, &y1, &x2, &y2)) ) {
    return;
  }

#if DEBUG
  printf("drawing pin\n\n");
#endif

  if (toplevel->pin_style == THICK ) {
    size = SCREENabs(toplevel, PIN_WIDTH);
    gdk_gc_set_line_attributes(w_current->gc, size, GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  if (toplevel->override_color != -1 ) {
    gdk_gc_set_foreground(w_current->gc,
			  x_get_color(toplevel->override_color));
    if (toplevel->DONT_REDRAW == 0) {
      gdk_draw_line(w_current->backingstore, w_current->gc,
		    x1, y1, x2, y2);
    }
  } else {
    if (toplevel->DONT_REDRAW == 0) {
      gdk_gc_set_foreground(w_current->gc, x_get_color(o_current->color));
      gdk_draw_line(w_current->backingstore, w_current->gc,
		    x1, y1, x2, y2);
    }
  }

  /* draw the cue directly */
  o_cue_draw_lowlevel(w_current, o_current, o_current->whichend);
  
  /* yes zero is right for the width -> use hardware lines */
  if (toplevel->pin_style == THICK ) {
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
void o_pin_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
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

  gdk_gc_set_foreground(w_current->xor_gc, x_get_darkcolor(color));

  if (toplevel->pin_style == THICK ) {
    size = SCREENabs(toplevel, PIN_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  WORLDtoSCREEN(toplevel, o_current->line->x[0] + dx, o_current->line->y[0] + dy, &sx[0], &sy[0]);
  WORLDtoSCREEN(toplevel, o_current->line->x[1] + dx, o_current->line->y[1] + dy, &sx[1], &sy[1]);

  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
                sx[0], sy[0], sx[1], sy[1]);

  if (toplevel->pin_style == THICK ) {
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
void o_pin_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_end(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int color;
  GList *other_objects = NULL;
  OBJECT *o_current, *o_current_pin;

  g_assert( w_current->inside_action != 0 );

  if (toplevel->override_pin_color == -1) {
    color = w_current->pin_color;
  } else {
    color = toplevel->override_pin_color;
  }

  /* undraw rubber line */
  o_pin_rubberpin_xor(w_current);
  w_current->rubber_visible = 0;

  /* don't allow zero length pins */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    return;
  }

  toplevel->page_current->object_tail =
    o_pin_add(toplevel,
	      toplevel->page_current->object_tail,
	      OBJ_PIN, color,
	      w_current->first_wx, w_current->first_wy,
	      w_current->second_wx, w_current->second_wy,
	      PIN_TYPE_NET, 0);

  o_current = o_current_pin = toplevel->page_current->object_tail;

  if (scm_hook_empty_p(add_pin_hook) == SCM_BOOL_F &&
      o_current != NULL) {
    scm_run_hook(add_pin_hook,
		 scm_cons(g_make_object_smob(toplevel, o_current),
			  SCM_EOL));
  }

  /* look for connected objects */
  other_objects = s_conn_return_others(other_objects, o_current_pin);
  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);
  o_cue_draw_single(w_current, o_current_pin); 
  o_pin_draw(w_current, o_current_pin);

  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_rubberpin(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  g_assert( w_current->inside_action != 0 );

  /* erase the rubberpin if it is visible */
  if (w_current->rubber_visible)
    o_pin_rubberpin_xor(w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* decide whether to draw the pin vertical or horizontal */
  if (abs(w_current->second_wx - w_current->first_wx)
      >= abs(w_current->second_wy - w_current->first_wy)) {
    w_current->second_wy = w_current->first_wy;
  } else {
    w_current->second_wx = w_current->first_wx;
  }

  o_pin_rubberpin_xor(w_current);
  w_current->rubber_visible = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  used in o_stretch.c
 */
void o_pin_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
  o_pin_rubberpin_xor(w_current);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_rubberpin_xor(GSCHEM_TOPLEVEL *w_current) 
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int size = 0;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy, &x2, &y2);

  if (toplevel->net_style == THICK ) {
    size = SCREENabs(toplevel, PIN_WIDTH);

    if (size < 0)
      size=0;

    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color));
  gdk_draw_line(w_current->backingstore, w_current->xor_gc, x1, y1, x2, y2);
  o_invalidate_rect(w_current, 
		    min(x1, x2) - size/2, min(y1, y2) - size/2,
		    max(x1, x2) + size/2, max(y1, y2) + size/2);

  if (toplevel->net_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }
}
