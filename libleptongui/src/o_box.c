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
#include <math.h>
#include <stdio.h>

#include "gschem.h"

#define GET_BOX_WIDTH(w)                        \
        abs((w)->second_wx - (w)->first_wx)
#define GET_BOX_HEIGHT(w)                       \
        abs((w)->second_wy - (w)->first_wy)
#define GET_BOX_LEFT(w)                         \
        MIN((w)->first_wx, (w)->second_wx)
#define GET_BOX_TOP(w)                          \
        MAX((w)->first_wy, (w)->second_wy)

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_box_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->first_wx,
                                          w_current->first_wy,
                                          w_current->second_wx,
                                          w_current->second_wy);
}

/*! \brief Start process to input a new box.
 *  \par Function Description
 *  This function starts the process to input a new box. Parameters for this
 *  box are put into/extracted from the <B>w_current</B> toplevel structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in world
 *  coordinates.
 *
 *  The first step is to input one corner of the box. This corner is
 *  (<B>w_x</B>,<B>w_y</B>) snapped to the grid and saved in <B>w_current->first_wx</B>
 *  and <B>w_current->first_wy</B>.
 *
 *  The other corner will be saved in (<B>w_current->second_wx</B>,
 *  <B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world.
 *  \param [in] w_y        Current y coordinate of pointer in world.
 */
void o_box_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

  /* init first_w[x|y], second_w[x|y] to describe box */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  /* start to draw the box */
  o_box_invalidate_rubber (w_current);
}

/*! \brief End the input of a box.
 *  \par Function Description
 *  This function ends the input of the second corner of a box.
 *  The (<B>w_x</B>,<B>w_y</B>) point is set to be this second corner. The box is
 *  then defined by (<B>w_current->first_wx</B>,<B>w_current->first_wy</B> and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *  <B>w_x</B> and <B>w_y</B> are in screen unit.
 *
 *  The temporary box is erased ; a new box object is allocated, initialized
 *  and linked to the object list ; The object is finally drawn on the
 *  current sheet.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_box_end(GschemToplevel *w_current, int w_x, int w_y)
{
  LeptonObject *new_obj;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  int box_width, box_height;
  int box_left, box_top;

  /* get the last coords of the pointer */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* erase the temporary box */
  /* o_box_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 0);

  box_width  = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT(w_current);
  box_left   = GET_BOX_LEFT  (w_current);
  box_top    = GET_BOX_TOP   (w_current);

  /* boxes with null width or height are not allowed */
  if ((box_width == 0) || (box_height == 0)) {
    /* cancel the object creation */
    w_current->first_wx = (-1);
    w_current->first_wy = (-1);
    w_current->second_wx  = (-1);
    w_current->second_wy  = (-1);

  } else {

    /* create the object */
    new_obj = lepton_box_object_new (GRAPHIC_COLOR,
                                     box_left,
                                     box_top,
                                     box_left + box_width,
                                     box_top - box_height);
    lepton_page_append (page, new_obj);

#if DEBUG
  printf("coords: %d %d %d %d\n", box_left, box_top, box_width, box_height);
#endif

    w_current->first_wx = (-1);
    w_current->first_wy = (-1);
    w_current->second_wx  = (-1);
    w_current->second_wy  = (-1);

    /* Call add-objects-hook */
    g_run_hook_object (w_current, "add-objects-hook", new_obj);

    gschem_toplevel_page_content_changed (w_current, page);
    o_undo_savestate(w_current, page, UNDO_ALL);
  }

  i_action_stop (w_current);
}

/*! \brief Draw temporary box while dragging edge.
 *  \par Function Description
 *  This function is used to draw the box while dragging one of its edge or
 *  angle. It erases the previous temporary box drawn before, and draws a new
 *  updated one. <B>w_x</B> and <B>w_y</B> are the new position of the mobile point,
 *  ie the mouse.
 *
 *  The old values are inside the <B>w_current</B> pointed structure. Old width,
 *  height and left and top values are recomputed by the corresponding macros.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_box_motion (GschemToplevel *w_current, int w_x, int w_y)
{

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  /* erase the previous temporary box if it is visible */
  if (w_current->rubber_visible)
    o_box_invalidate_rubber (w_current);

  /*
   * New values are fixed according to the <B>w_x</B> and <B>w_y</B> parameters.
   * These are saved in <B>w_current</B> pointed structure as new temporary
   * values. The new box is then drawn.
   */

  /* update the coords of the corner */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* draw the new temporary box */
  o_box_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Draw box from GschemToplevel object.
 *  \par Function Description
 *  This function draws the box from the variables in the GschemToplevel
 *  structure <B>*w_current</B> using \a renderer..
 *  One corner of the box is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and the second corner is at
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void o_box_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_box (cr, flags, wwidth, w_current->first_wx, w_current->first_wy,
                 w_current->second_wx, w_current->second_wy);
  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}
