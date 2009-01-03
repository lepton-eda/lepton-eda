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


/*! \brief Draw an arc on the screen.
 *  \par Function Description
 *  This function is used to draw an arc on screen. The arc is described
 *  in the object which is referred by <B>o_current</B>. The arc is displayed
 *  according to the current state, described in the GSCHEM_TOPLEVEL object
 *  pointed by <B>w_current</B>.
 *
 *  It first checkes if the object is valid or not. If not it returns
 *  and do not output anything. That should never happen though.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  The arc OBJECT to draw.
 */
void o_arc_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int sx1, sy1, sx2, sy2;
  int line_width;
  COLOR *color;
  int length, space;

  if (o_current->arc == NULL) {
    return;
  }

  if (toplevel->DONT_REDRAW == 1)
    return;

  /*
   * As an arc is definetely not a closed shape there is no need to
   * define and call any filling function. Another way to say that is
   * that an arc can not be filled. It simply draws the arc according
   * to the type line.
   *
   * The values describing the line type are extracted from the
   * <B>o_current</B> pointed structure. These are the width of the line,
   * the field called length and the field called space and the desired
   * end type for the arc.
   *
   * Depending on the type of line desired the appropriate function is
   * called. Values of space and length are adapted to the type of line.
   * The possible functions are the following : #o_arc_draw_solid(),
   * #o_arc_draw_dotted(), #o_arc_draw_dashed() and #o_arc_draw_phantom().
   *
   * The combination <B>length</B> == 0 and <B>space</B> == 0 is avoided as it
   * leads to an endless loop in function called after. If such a case is
   * encountered the arc is drawn as a solid arc independently of its
   * initial type.
   */

  if (toplevel->override_color != -1 )
    color = x_color_lookup (toplevel->override_color);
  else
    color = x_color_lookup (o_current->color);

  line_width = SCREENabs (w_current, o_current->line_width);
  if(line_width <= 0) {
    line_width = 1;
  }

  length = SCREENabs (w_current, o_current->line_length);
  space = SCREENabs (w_current, o_current->line_space);

  WORLDtoSCREEN (w_current, o_current->arc->x - o_current->arc->width / 2,
                           o_current->arc->y + o_current->arc->height / 2,
                           &sx1, &sy1);
  WORLDtoSCREEN (w_current, o_current->arc->x + o_current->arc->width / 2,
                           o_current->arc->y - o_current->arc->height / 2,
                           &sx2, &sy2);

  cairo_translate (w_current->cr, (double)(sx1 + sx2) / 2.,
                                  (double)(sy1 + sy2) / 2.);

  /* Adjust for non-uniform X/Y scale factor. Note that the + 1
     allows for the case where sx2 == sx1 or sy2 == sy1 */
  cairo_scale (w_current->cr, (double)(sx2 - sx1 + 1) /
                              (double)(sy2 - sy1 + 1), 1.);
  gschem_cairo_arc (w_current->cr, line_width,
                    0., 0., (double)(sy2 - sy1) / 2.,
                    o_current->arc->start_angle, o_current->arc->end_angle);
  cairo_identity_matrix (w_current->cr);

  gschem_cairo_set_source_color (w_current->cr, color);
  gschem_cairo_stroke (w_current->cr, o_current->line_type,
                       o_current->line_end, line_width, length, space);

  if (o_current->selected && w_current->draw_grips == TRUE) {
    o_arc_draw_grips (w_current, o_current);
  }

#if DEBUG
  printf("drawing arc\n");
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_arc_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  int cx, cy, radius;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &cx, &cy);
  radius = SCREENabs (w_current, w_current->distance);

  /* FIXME: This isn't a tight bounding box */
  o_invalidate_rect (w_current, cx - radius, cy - radius,
                                cx + radius, cy + radius);
}

/*! \brief Draw an arc described by OBJECT with translation
 *  \par Function Description
 *  This function draws the arc object described by <B>*o_current</B>
 *  translated by the vector (<B>dx</B>,<B>dy</B>).
 *  The translation vector is in screen unit.
 *
 *  The arc is displayed with the color of the object.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for arc.
 *  \param [in] dy         Delta y coordinate for arc.
 *  \param [in] o_current  Arc OBJECT to draw.
 */
void o_arc_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  int sx1, sy1, sx2, sy2;
  int line_width = 1;
  int color;

  if (o_current->arc == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  WORLDtoSCREEN (w_current, o_current->arc->x + dx - o_current->arc->width / 2,
                           o_current->arc->y + dy + o_current->arc->height / 2,
                           &sx1, &sy1);
  WORLDtoSCREEN (w_current, o_current->arc->x + dx + o_current->arc->width / 2,
                           o_current->arc->y + dy- o_current->arc->height / 2,
                           &sx2, &sy2);

  cairo_translate (w_current->cr, (double)(sx1 + sx2) / 2.,
                                  (double)(sy1 + sy2) / 2.);

  /* Adjust for non-uniform X/Y scale factor. Note that the + 1
     allows for the case where sx2 == sx1 or sy2 == sy1 */
  cairo_scale (w_current->cr, (double)(sx2 - sx1 + 1) /
                              (double)(sy2 - sy1 + 1), 1.);
  gschem_cairo_arc (w_current->cr, line_width,
                    0., 0., (double)(sy2 - sy1) / 2.,
                    o_current->arc->start_angle, o_current->arc->end_angle);
  cairo_identity_matrix (w_current->cr);

  gschem_cairo_set_source_color (w_current->cr, x_color_lookup_dark (color));
  gschem_cairo_stroke (w_current->cr, TYPE_SOLID, END_NONE, 1, -1, -1);
}

/*! \brief Start process to input a new arc.
 *  \par Function Description
 *  This function starts the process to input a new arc. Parameters for
 *  this arc are put into/extracted from the <B>w_current</B> toplevel structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in screen unit.
 *
 *  First step of the arc input is to set the radius of the arc. The center
 *  of the arc is kept in (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>).
 *  The radius of the arc is in <B>w_current->distance</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_arc_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* set the center of the arc */
  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  /* set the radius */
  w_current->distance = 0;

  /* set the start and end angles */
  w_current->second_wx = w_current->second_wy = 0;

  /* start the rubberbanding process of the radius */
  o_arc_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of an arc.
 *  \par Function Description
 *  This function ends the input of the radius of the arc.
 *  The (<B>w_x</B>,<B>w_y</B>) point is taken as the other end of the radius segment.
 *  The distance between this point and the center is the radius of the arc.
 *  <B>w_x</B> and <B>w_y</B> are in world coords.
 *
 *  At the end of this function, the center of the arc is at
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and its radius is
 *  <B>w_current->distance</B>.
 *
 *  The two angles needs to be input to fully define the arc.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_arc_end1(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  g_assert( w_current->inside_action != 0 );

  /* erases the previous temporary radius segment */
  /* o_arc_invalidate_rubber (w_current); */
  w_current->rubber_visible = 0;

  /* ack! zero length radius */
  if (w_current->distance == 0) {
    return;
  }

#if DEBUG
  printf("DIST: %d\n", w_current->distance);
#endif

  /* open a dialog to input the start and end angle */
  arc_angle_dialog(w_current, NULL);
}

/*! \brief Ends the process of arc input.
 *  \par Function Description
 *  The #o_arc_end4() function ends the process of the input of an arc.
 *  <B>start_angle</B> and <B>end_angle</B> are the start and end angle of the
 *  arc in degrees. The partial internal representation of the arc, i.e.
 *  the center and the radius of the arc, are converted in world units.
 *  A new object is created and linked to the object list.
 *
 *  \param [in] w_current    The GSCHEM_TOPLEVEL object.
 *  \param [in] radius       Radius of the arc
 *  \param [in] start_angle  Start of angle in degrees.
 *  \param [in] end_angle    End of angle in degrees.
 */
void o_arc_end4(GSCHEM_TOPLEVEL *w_current, int radius, 
		int start_angle, int end_angle)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;

  /* create, initialize and link the new arc object */
  new_obj = o_arc_new (toplevel, OBJ_ARC, GRAPHIC_COLOR,
                       w_current->first_wx, w_current->first_wy,
                       radius, start_angle, end_angle);
  s_page_append (toplevel->page_current, new_obj);

  /* draw the new object */
  o_invalidate (w_current, new_obj);

  w_current->first_wx  = -1;
  w_current->first_wy  = -1;
  w_current->distance = 0;

  toplevel->page_current->CHANGED = 1;
  
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw an arc using one angle modification.
 *  \par Function Description
 *  This function draws an arc according to its internal representation
 *  and allows the modification of one of its angle. The start or end
 *  angle of the arc is updated according to <B>whichone</B> with the angle
 *  that the current pointer and the arc center are making with the horizontal.
 *
 *  The previous temporary arc is erased, the angle is then computed
 *  and updated and finally a new temporary arc with the new angle is drawn.
 *
 *  The arc is internally described by :
 *  <DL>
 *    <DT>*</DT><DD>(<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) as
 *                   its center.
 *    <DT>*</DT><DD><B>w_current->distance</B> as its radius.
 *    <DT>*</DT><DD><B>w_current->second_wx</B> and <B>w_current->second_wx</B> as its
 *                  start and end angle respectively.
 *  </DL>
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 *  \param [in] whichone   Which angle to change.
 *
 *  <B>whichone</B> can have one of the following values:
 *  <DL>
 *    <DT>ARC_RADIUS</DT>
 *    <DD>at the center of the arc. This grip is used to modify
 *        the radius of the arc.
 *    <DT>ARC_START_ANGLE</DT>
 *    <DD>at one end of the arc. It corresponds to the starting
 *        angle of the arc.
 *    <DT>ARC_END_ANGLE</DT>
 *    <DD>at the other end of the arc. It corresponds to the
 *        ending angle of the arc.
 *  </DL>
 */
void o_arc_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y, int whichone)
{
  int diff_x, diff_y, angle_deg;

  /* erase the previous temporary arc */
  if (w_current->rubber_visible)
    o_arc_invalidate_rubber (w_current);

  if(whichone == ARC_RADIUS) {
    /*
     * The radius is taken as the biggest distance on the x and y
     * axis between the center of the arc and the mouse position.
     */		
    diff_x = abs(w_current->first_wx - snap_grid (w_current, w_x));
    diff_y = abs(w_current->first_wy - snap_grid (w_current, w_y));
    w_current->distance = max(diff_x, diff_y);
  }
  else if((whichone == ARC_START_ANGLE) || (whichone == ARC_END_ANGLE)) {
    /* compute the angle */
    diff_x = w_current->first_wx - w_x;
    diff_y = w_current->first_wy - w_y;
    angle_deg = atan2(diff_y, diff_x) * 180 / M_PI;

    /* set the start or end angle with this angle */
    switch(whichone) {
    case ARC_START_ANGLE:
      w_current->second_wx = (angle_deg + 360 + 180) % 360;
      break;
	
    case ARC_END_ANGLE:
      w_current->second_wy = (angle_deg - w_current->second_wx + 720 + 180) % 360;
      break;
	
    default:
      return;
    }

  }
	
  /* draw the new temporary arc */
  o_arc_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Draw arc from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws the arc from the variables in the GSCHEM_TOPLEVEL
 *  structure <B>*w_current</B>.
 *  The center of the arc is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>), its radius equal to <B>w_current->distance</B>,
 *  and the start and end angle are given by <B>w_current->second_wx</B> and
 *  <B>w_current->second_wy</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_arc_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  double rad_angle;
  double radius;
  double cx, cy;
  int sx1, sy1, sx2, sy2, rx, ry;

  WORLDtoSCREEN (w_current, w_current->first_wx - w_current->distance,
                            w_current->first_wy + w_current->distance,
                            &sx1, &sy1);
  WORLDtoSCREEN (w_current, w_current->first_wx + w_current->distance,
                            w_current->first_wy - w_current->distance,
                            &sx2, &sy2);

  radius = (double)(sy2 - sy1) / 2.;
  cx = (double)(sx1 + sx2) / 2.;
  cy = (double)(sy1 + sy2) / 2.;

  cairo_translate (w_current->cr, cx, cy);

  /* Adjust for non-uniform X/Y scale factor. Note that the + 1
     allows for the case where sx2 == sx1 or sy2 == sy1 */
  cairo_scale (w_current->cr, (double)(sx2 - sx1 + 1) /
                              (double)(sy2 - sy1 + 1), 1.);
  gschem_cairo_arc (w_current->cr, 1, 0., 0., radius,
                    w_current->second_wx, w_current->second_wy);
  cairo_identity_matrix (w_current->cr);

  gschem_cairo_set_source_color (w_current->cr,
                                 x_color_lookup_dark (SELECT_COLOR));

  /* draw the radius segment from the w_current variables */
  rad_angle = ((double) w_current->second_wx) * M_PI / 180;
  rx = cx + radius * cos (rad_angle);
  ry = cy - radius * sin (rad_angle);
  gschem_cairo_line (w_current->cr, END_NONE, 1, cx, cy, rx, ry);

  gschem_cairo_stroke (w_current->cr, TYPE_SOLID, END_NONE, 1, -1, -1);
}

/*! \brief Draw grip marks on arc.
 *  \par Function Description
 *  This function draws three grips on the center and on the ends of
 *  the arc object described by <B>*o_current</B>.
 *
 *  \param [in] w_current  The GSCHE_TOPLEVEL object.
 *  \param [in] o_current  Arc OBJECT to draw grip points on.
 */
void o_arc_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  int radius, x, y, start_angle, end_angle;
  int x1, y1, x2, y2;

  if (w_current->draw_grips == FALSE)
    return;

  /*
   * An arc has three grips:
   * <DL>
   *   <DT>*</DT><DD>one at the center that allows changes on the
   *                 radius - at (<B>x</B>,<B>y</B>).
   *   <DT>*</DT><DD>one at the start of the arc - at (<B>x1</B>,<B>y1</B>).
   *   <DT>*</DT><DD>one at the end of the arc - at (<B>x2</B>,<B>y2</B>).
   */

  WORLDtoSCREEN (w_current, o_current->arc->x, o_current->arc->y, &x, &y);
  radius      = SCREENabs (w_current, o_current->arc->width / 2);
  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;

  x1 = x + radius * cos(((double) start_angle) * M_PI / 180);
  y1 = y - radius * sin(((double) start_angle) * M_PI / 180);
  x2 = x + radius * cos(((double) (start_angle + end_angle)) * M_PI / 180);
  y2 = y - radius * sin(((double) (start_angle + end_angle)) * M_PI / 180);

  /* draw the grip at the center */
  o_grips_draw(w_current,  x,  y);

  /* draw the grip at the start_angle end of the arc */
  o_grips_draw(w_current, x1, y1);

  /* draw the grip at the end_angle end of the arc */
  o_grips_draw(w_current, x2, y2);

}
