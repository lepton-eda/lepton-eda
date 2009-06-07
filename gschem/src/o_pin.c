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
void o_pin_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int size = 0;
  COLOR *color;

  if (o_current->line == NULL) {
    return;
  }

  /* reuse line's routine */
  if ( (toplevel->DONT_REDRAW == 1) ||
       (!o_line_visible (w_current, o_current->line, &x1, &y1, &x2, &y2)) ) {
    return;
  }

  if (toplevel->override_color != -1 ) {
    color = x_color_lookup (toplevel->override_color);
  } else {
    color = x_color_lookup (o_current->color);
  }

  if (toplevel->DONT_REDRAW == 0) {
    if (toplevel->pin_style == THICK)
      size = o_current->line_width;

    gschem_cairo_line (w_current, END_NONE, size, x1, y1, x2, y2);

    gschem_cairo_set_source_color (w_current, color);
    gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, size, -1, -1);
  }

  /* draw the cue directly */
  o_cue_draw_lowlevel(w_current, o_current, o_current->whichend);

#if DEBUG
  printf("drawing pin\n");
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
void o_pin_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size = 0;
  int color;

  if (o_current->line == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  if (toplevel->pin_style == THICK)
    size = o_current->line_width;

  gschem_cairo_line (w_current, END_NONE, size,
                     o_current->line->x[0] + dx, o_current->line->y[0] + dy,
                     o_current->line->x[1] + dx, o_current->line->y[1] + dy);

  gschem_cairo_set_source_color (w_current, x_color_lookup_dark (color));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, size, -1, -1);
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
  OBJECT *new_obj;
  int color;
  GList *prev_conn_objects = NULL;
  OBJECT *o_current, *o_current_pin;

  g_assert( w_current->inside_action != 0 );

  if (toplevel->override_pin_color == -1) {
    color = PIN_COLOR;
  } else {
    color = toplevel->override_pin_color;
  }

  /* undraw rubber line */
  /* o_pin_invalidate_rubber (w_current); */
  w_current->rubber_visible = 0;

  /* don't allow zero length pins */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    return;
  }

  new_obj = o_pin_new(toplevel, OBJ_PIN, color,
                      w_current->first_wx, w_current->first_wy,
                      w_current->second_wx, w_current->second_wy,
                      PIN_TYPE_NET, 0);
  s_page_append (toplevel, toplevel->page_current, new_obj);

  o_current = o_current_pin = new_obj;

  if (scm_hook_empty_p(add_pin_hook) == SCM_BOOL_F &&
      o_current != NULL) {
    scm_run_hook(add_pin_hook,
		 scm_cons(g_make_object_smob(toplevel, o_current),
			  SCM_EOL));
  }

  /* look for connected objects */
  prev_conn_objects = s_conn_return_others(prev_conn_objects, o_current_pin);
  o_invalidate_glist (w_current, prev_conn_objects);
  g_list_free (prev_conn_objects);
  o_invalidate (w_current, o_current_pin);

  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  g_assert( w_current->inside_action != 0 );

  /* erase the rubberpin if it is visible */
  if (w_current->rubber_visible)
    o_pin_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* decide whether to draw the pin vertical or horizontal */
  if (abs(w_current->second_wx - w_current->first_wx)
      >= abs(w_current->second_wy - w_current->first_wy)) {
    w_current->second_wy = w_current->first_wy;
  } else {
    w_current->second_wx = w_current->first_wx;
  }

  o_pin_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void o_pin_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int min_x, min_y, max_x, max_y;
  int bloat = 0;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  /* Pins are always first created as net pins, use net pin width */
  if (toplevel->net_style == THICK ) {
    bloat = SCREENabs (w_current, PIN_WIDTH_NET) / 2;
  }

  min_x = min (x1, x2) - bloat;
  max_x = max (x1, x2) + bloat;
  min_y = min (y1, y2) - bloat;
  max_y = max (y1, y2) + bloat;

  o_invalidate_rect (w_current, min_x, min_y, max_x, max_y);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_pin_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  int size = 0;

  /* Pins are always first created as net pins, use net pin width */
  if (w_current->toplevel->net_style == THICK)
    size = PIN_WIDTH_NET;

  gschem_cairo_line (w_current, END_NONE, size,
                     w_current->first_wx,  w_current->first_wy,
                     w_current->second_wx, w_current->second_wy);

  gschem_cairo_set_source_color (w_current,
                                 x_color_lookup_dark (SELECT_COLOR));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, size, -1, -1);
}
