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
void o_circle_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->first_wx - w_current->distance,
                                          w_current->first_wy - w_current->distance,
                                          w_current->first_wx + w_current->distance,
                                          w_current->first_wy + w_current->distance);
}

/*! \brief Start process to input a new circle.
 *  \par Function Description
 *  This function starts the process to input a new circle. Parameters for
 *  this circle are pu into/extracted from the <B>w_current</B> toplevel
 *  structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the mouse pointer in
 *  world units.
 *
 *  The first step of the circle input is to set the center of the arc.
 *  This center is kept in (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>).
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_circle_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

  /* center of circle */
  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  /* radius */
  w_current->distance = 0;

  /* first temporary circle */
  o_circle_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of a circle.
 *  \par Function Description
 *  This function ends the input of the radius of the circle.
 *  The (<B>w_x</B>,<B>w_y</B>) point is taken as the other end of the radius
 *  segment, i.e. on the circle. The distance between this point and the
 *  center is the radius of the circle.
 *  <B>w_x</B> and <B>w_y</B> are in world coords.
 *
 *  The center has previously been input and saved as
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>).
 *
 *  The temporary circle drawn during the input of the radius is erased.
 *  A new object is allocated, initialized and linked in the object list.
 *  This new object is finally drawn.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_circle_end(GschemToplevel *w_current, int w_x, int w_y)
{
  LeptonObject *new_obj;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  /* erase the temporary circle */
  /* o_circle_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 0);

  /* circle with null radius are not allowed */
  if (w_current->distance == 0) {
    /* cancel the object creation */
    return;
  }

  /* create the object */
  new_obj = lepton_circle_object_new (GRAPHIC_COLOR,
                                      w_current->first_wx,
                                      w_current->first_wy,
                                      w_current->distance);

  lepton_page_append (page, new_obj);

  /* Call add-objects-hook */
  g_run_hook_object (w_current, "add-objects-hook", new_obj);

  gschem_toplevel_page_content_changed (w_current, page);
  o_undo_savestate(w_current, page, UNDO_ALL);

  i_action_stop (w_current);
}

/*! \brief Draw temporary circle while dragging edge.
 *  \par Function Description
 *  This function draws a circle according to its internal representation and
 *  allows the modification of its radius. The radius is updated according to
 *  the current mouse position in <B>w_x</B> and <B>w_y</B>.
 *  It draws a full circle and the horizontal segment of the radius in the
 *  right half of the circle.
 *
 *  The previous temporary circle is erased, the radius is then computed and
 *  updated and finally a new temporary circle is drawn.
 *
 *  The arc is internally described by :
 *  <DL>
 *    <DT>*</DT><DD>(<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) as its
 *                   center ;
 *    <DT>*</DT><DD><B>w_current->distance</B> as its radius.
 *  </DL>
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_circle_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  /* erase the previous temporary circle if it is visible */
  if (schematic_window_get_rubber_visible (w_current))
    o_circle_invalidate_rubber (w_current);

  /*
   * The radius is taken as the biggest distance on the x and y axis between
   * the center of the circle and the mouse position.
   */
  diff_x = abs(w_current->first_wx - w_x);
  diff_y = abs(w_current->first_wy - w_y);
  w_current->distance = MAX(diff_x, diff_y);

  /* draw the new temporary circle */
  o_circle_invalidate_rubber (w_current);
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \brief Draw circle from GschemToplevel object.
 *  \par Function Description
 *  This function draws the circle from the variables in the GschemToplevel
 *  structure <B>*w_current</B> using \a renderer.
 *  The center of the circle is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and its radius is in <B>w_current->distance</B>.
 *
 *  It draws a horizontal radius segment on the right half of the circle and
 *  the circle with the selection color.
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void o_circle_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_center_arc (cr, flags, wwidth, wwidth,
                        w_current->first_wx, w_current->first_wy,
                        w_current->distance,
                        0, 360);

  eda_cairo_line (cr, flags, END_NONE, wwidth,
                  w_current->first_wx,
                  w_current->first_wy,
                  w_current->first_wx + w_current->distance,
                  w_current->first_wy);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}
