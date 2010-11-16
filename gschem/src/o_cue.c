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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#include <string.h>
#include <math.h> /* For M_PI */

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_cue_redraw_all (GSCHEM_TOPLEVEL *w_current, GList *list, gboolean draw_selected)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  GList *iter;

  iter = list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    switch(o_current->type) {
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_PIN):
	if (!(o_current->dont_redraw ||
              (o_current->selected && !draw_selected))) {
          o_cue_draw_single(w_current, o_current);
          if (o_current->selected && w_current->draw_grips) {
            o_line_draw_grips (w_current, o_current);
          }
        }
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
	if (!(o_current->dont_redraw ||
              (o_current->selected && !draw_selected))) {
          o_cue_redraw_all(w_current, o_current->complex->prim_objs, 
                           draw_selected);
        }
        break;
    }
    
    iter = g_list_next (iter);
  }
}


/*! 
 *  \brief Set the color on the gc depending on the passed in color id
 */
static void o_cue_set_color(GSCHEM_TOPLEVEL *w_current, int color)
{
  if (w_current->toplevel->override_color != -1 ) {
    gschem_cairo_set_source_color (w_current, x_color_lookup (w_current->toplevel->override_color));
  } else {
    gschem_cairo_set_source_color (w_current, x_color_lookup (color));
  }
}


/*! \brief Draws a circular junction cue
 *
 *  \par Function Description
 *  Draws a cue at the given world coordinate, picking the size based
 *  on whether a bus forms a part of the connection.
 *
 *  The cue's drawn position is hinted to align with the widht a net or
 *  bus would be drawn on screen. This helps to keep the cue looking
 *  central when lines being hinted onto the pixel grid.
 *
 *  \param [in] w_current     The GSCHEM_TOPLEVEL object
 *  \param [in] x             The X coordinate of the cue (world units)
 *  \param [in] y             The Y coordinate of the cue (world units)
 *  \param [in] bus_involved  If a bus forms part of the connection (TRUE/FALSE)
 */
static void draw_junction_cue (GSCHEM_TOPLEVEL *w_current,
                               int x, int y, int bus_involved)
{
  int size;
  int line_width;

  if (bus_involved) {
    size = JUNCTION_CUE_SIZE_BUS / 2;
    line_width = BUS_WIDTH;
  } else {
    size = JUNCTION_CUE_SIZE_NET / 2;
    line_width = NET_WIDTH;
  }

  gschem_cairo_center_arc (w_current, line_width, -1, x, y, size, 0, 360);
  o_cue_set_color (w_current, JUNCTION_COLOR);
  cairo_fill (w_current->cr);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_cue_draw_lowlevel(GSCHEM_TOPLEVEL *w_current, OBJECT *object, int whichone)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x, y;
  GList *cl_current;
  CONN *conn;
  int type, count = 0;
  int done = FALSE;
  int size, pinsize;
  int otherone;
  int bus_involved=FALSE;

  if (whichone < 0 || whichone > 1) return;
  
  x = object->line->x[whichone];
  y = object->line->y[whichone];

  type = CONN_ENDPOINT;

  if (object->type == OBJ_BUS ||
       (object->type == OBJ_PIN && object->pin_type == PIN_TYPE_BUS))
    bus_involved = TRUE;

  cl_current = object->conn_list;
  while(cl_current != NULL && !done) {
    conn = (CONN *) cl_current->data;
   
    if (conn->x == x && conn->y == y) {

      if (conn->other_object &&
           (conn->other_object->type == OBJ_BUS ||
             (conn->other_object->type == OBJ_PIN &&
              conn->other_object->pin_type == PIN_TYPE_BUS)))
        bus_involved=TRUE;

      switch(conn->type) {
        
        case(CONN_ENDPOINT):
          count++;
          break;

        case(CONN_MIDPOINT):
          type = CONN_MIDPOINT;
          done = TRUE;
          count = 0;
          break;
      }
    }

    cl_current = g_list_next(cl_current);
  }

#if DEBUG
  printf("type: %d count: %d\n", type, count);
#endif

  switch(type) {

    case(CONN_ENDPOINT):
      if (object->type == OBJ_NET) { /* only nets have these cues */
        if (count < 1) { /* Didn't find anything connected there */
          size = CUE_BOX_SIZE;
          gschem_cairo_center_box (w_current, -1, -1, x, y, size, size);
          o_cue_set_color (w_current, NET_ENDPOINT_COLOR);
          cairo_fill (w_current->cr);

        } else if (count >= 2) {
          draw_junction_cue (w_current, x, y, bus_involved);
        }
      } else if (object->type == OBJ_PIN) {
        /* Didn't find anything connected there */
        if (count < 1 && object->whichend == whichone) {
          size = (bus_involved) ? PIN_CUE_SIZE_BUS : PIN_CUE_SIZE_NET;

          otherone = !whichone;

          pinsize = 0;
          if (toplevel->pin_style == THICK )
            pinsize = object->line_width;

          o_cue_set_color (w_current, NET_ENDPOINT_COLOR);
          if (object->line->y[whichone] == object->line->y[otherone]) {
            /* horizontal line */
            if (object->line->x[whichone] <= object->line->x[otherone]) {
              gschem_cairo_line (w_current, END_NONE, pinsize, x,        y,
                                 x + size, y);
            } else {
              gschem_cairo_line (w_current, END_NONE, pinsize, x,        y,
                                 x - size, y);
            }
            gschem_cairo_stroke (w_current, TYPE_SOLID,
                                 END_NONE, pinsize, -1, -1);
          } else if (object->line->x[0] == object->line->x[1]) {
            /* vertical line */
            if (object->line->y[whichone] <= object->line->y[otherone]) {
                gschem_cairo_line (w_current, END_NONE, pinsize, x, y,
                                   x, y + size);
            } else {
              gschem_cairo_line (w_current, END_NONE, pinsize, x, y,
                                 x, y - size);
            }
            gschem_cairo_stroke (w_current, TYPE_SOLID,
                                 END_NONE, pinsize, -1, -1);
          } else {
            /* angled line */
            /* not supporting rendering of que for angled pin for now. hack */
          }

        }
      }
      break;

    case(CONN_MIDPOINT):
      draw_junction_cue (w_current, x, y, bus_involved);
      break;

      /* here is where you draw bus rippers */
      
  }
  
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_cue_draw_lowlevel_midpoints(GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  GList *iter;
  int bus_involved;

  for (iter = object->conn_list; iter != NULL; iter = g_list_next (iter)) {
    CONN *conn = iter->data;

    if (conn->type == CONN_MIDPOINT) {
      bus_involved = (object->type == OBJ_BUS ||
                       (conn->other_object &&
                        conn->other_object->type == OBJ_BUS));
      draw_junction_cue (w_current, conn->x, conn->y, bus_involved);
    }
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_cue_draw_single(GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  if (!object) {
    return;
  }

  if (object->type != OBJ_NET && object->type != OBJ_PIN &&
      object->type != OBJ_BUS) {
        return;
      }

  if (object->type != OBJ_PIN) {
    o_cue_draw_lowlevel(w_current, object, 0);
    o_cue_draw_lowlevel(w_current, object, 1);
    o_cue_draw_lowlevel_midpoints(w_current, object);
  } else {
    o_cue_draw_lowlevel(w_current, object, object->whichend);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_cue_draw_list(GSCHEM_TOPLEVEL *w_current, GList *object_list)
{
  OBJECT *object;
  GList *ol_current;

  ol_current = object_list;
  while(ol_current != NULL) {
    object = (OBJECT *) ol_current->data;

    o_cue_draw_single(w_current, object);
    
    ol_current = g_list_next(ol_current);
  }
}
