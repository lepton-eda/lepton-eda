/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_arc_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);

  /* FIXME: This isn't a tight bounding box */

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->first_wx - w_current->distance,
                                          w_current->first_wy - w_current->distance,
                                          w_current->first_wx + w_current->distance,
                                          w_current->first_wy + w_current->distance);
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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_arc_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_arc_end1(GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert( w_current->inside_action != 0 );

  /* erases the previous temporary radius segment */
  /* o_arc_invalidate_rubber (w_current); */
  w_current->rubber_visible = 0;

  /* ack! zero length radius */
  if (w_current->distance != 0) {

#if DEBUG
  printf("DIST: %d\n", w_current->distance);
#endif

    /* open a dialog to input the start and end angle */
    arc_angle_dialog(w_current, NULL);
  }

  i_action_stop (w_current);
}

/*! \brief Ends the process of arc input.
 *  \par Function Description
 *  The #o_arc_end4() function ends the process of the input of an arc.
 *  <B>start_angle</B> and <B>sweep_angle</B> are the start and sweep angle of the
 *  arc in degrees. The partial internal representation of the arc, i.e.
 *  the center and the radius of the arc, are converted in world units.
 *  A new object is created and linked to the object list.
 *
 *  \param [in] w_current    The GschemToplevel object.
 *  \param [in] radius       Radius of the arc
 *  \param [in] start_angle  Start of angle in degrees.
 *  \param [in] sweep_angle  Angle sweep in degrees.
 */
void o_arc_end4(GschemToplevel *w_current, int radius,
                int start_angle, int sweep_angle)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  LeptonObject *new_obj;

  /* create, initialize and link the new arc object */
  new_obj = lepton_arc_object_new (GRAPHIC_COLOR,
                                   w_current->first_wx,
                                   w_current->first_wy,
                                   radius,
                                   start_angle,
                                   sweep_angle);

  lepton_page_append (page, new_obj);

  w_current->first_wx  = -1;
  w_current->first_wy  = -1;
  w_current->distance = 0;

  /* Call add-objects-hook */
  g_run_hook_object (w_current, "add-objects-hook", new_obj);

  gschem_toplevel_page_content_changed (w_current, page);
  o_undo_savestate(w_current, page, UNDO_ALL);
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
 *  \param [in] w_current  The GschemToplevel object.
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
 *    <DT>ARC_SWEEP_ANGLE</DT>
 *    <DD>at the other end of the arc. It corresponds to the
 *        ending angle of the arc.
 *  </DL>
 */
void o_arc_motion (GschemToplevel *w_current, int w_x, int w_y, int whichone)
{
  int diff_x, diff_y, angle_deg;

  g_assert (w_current->inside_action != 0);

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
    w_current->distance = MAX(diff_x, diff_y);
  }
  else if((whichone == ARC_START_ANGLE) || (whichone == ARC_SWEEP_ANGLE)) {
    /* compute the angle */
    diff_x = w_x - w_current->first_wx;
    diff_y = w_y - w_current->first_wy;
    angle_deg = atan2 (diff_y, diff_x) * 180 / M_PI;

    /* set the start or end angle with this angle */
    switch(whichone) {
    case ARC_START_ANGLE:
      w_current->second_wx = (angle_deg + 360) % 360;
      break;

    case ARC_SWEEP_ANGLE:
      w_current->second_wy = (((angle_deg + 360) % 360) -
                              w_current->second_wx + 360) % 360;
      if (w_current->which_object->arc->sweep_angle < 0)
        w_current->second_wy = w_current->second_wy - 360;
      if (w_current->second_wy == 0)
        w_current->second_wy = 360;
      break;

      /*
       * No default required - one of above two branches
       * *must* be taken - Coverity ID 201571
       */
    }

  }

  /* draw the new temporary arc */
  o_arc_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Draw arc from GschemToplevel object.
 *  \par Function Description
 *  This function draws the arc from the variables in the GschemToplevel
 *  structure <B>*w_current</B> using \a renderer.
 *  The center of the arc is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>), its radius equal to <B>w_current->distance</B>,
 *  and the start and end angle are given by <B>w_current->second_wx</B> and
 *  <B>w_current->second_wy</B>.
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void o_arc_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  double rad_angle;
  int rdx, rdy;
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_arc (cr, flags, wwidth,
                 w_current->first_wx, w_current->first_wy,
                 w_current->distance,
                 w_current->second_wx, w_current->second_wy);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);

  /* draw the radius line */
  rad_angle = ((double) w_current->second_wx) * M_PI / 180;
  rdx = (double) w_current->distance * cos (rad_angle);
  rdy = (double) w_current->distance * sin (rad_angle);

  eda_cairo_line (cr, flags, END_NONE, wwidth,
                  w_current->first_wx, w_current->first_wy,
                  w_current->first_wx + rdx, w_current->first_wy + rdy);

  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}
