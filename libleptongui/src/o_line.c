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

/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 */
void o_line_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->first_wx,
                                          w_current->first_wy,
                                          w_current->second_wx,
                                          w_current->second_wy);
}

/*! \brief Start process to input a new line.
 *  \par Function Description
 *  This function starts the process of interactively adding a line to
 *  the current sheet.
 *
 *  During all the process, the line is internally represented by the two
 *  ends of the line as (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  A temporary line is drawn during the process with the selection color
 *  and changed according to the position of the mouse pointer.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

  /* init first_w[x|y], second_w[x|y] to describe line */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  o_line_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of a line.
 *  \par Function Description
 *  This function ends the process of interactively adding a line to the
 *  current sheet.
 *
 *  It first erases the last temporary line displayed, calculates the
 *  corresponding world coordinates of the two ends of the line and finally
 *  adds a new initialized line object to the list of object of the current
 *  sheet.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_line_end(GschemToplevel *w_current, int w_x, int w_y)
{
  LeptonObject *new_obj;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  /* Don't bother.. the real object is invalidated, its in the same place */
  /* o_line_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 0);

  /* don't allow zero length lines */
  if ( (w_current->first_wx != w_current->second_wx) ||
       (w_current->first_wy != w_current->second_wy) ) {

    /* create the line object and draw it */
    new_obj = lepton_line_object_new (GRAPHIC_COLOR,
                                      w_current->first_wx,
                                      w_current->first_wy,
                                      w_current->second_wx,
                                      w_current->second_wy);

    lepton_page_append (page, new_obj);

    /* Call add-objects-hook */
    g_run_hook_object (w_current, "add-objects-hook", new_obj);

    gschem_toplevel_page_content_changed (w_current, page);
    o_undo_savestate(w_current, page, UNDO_ALL);
  }

  i_action_stop (w_current);
}

/*! \brief Draw temporary line while dragging end.
 *  \par Function Description
 *  This function manages the erase/update/draw process of temporary line
 *  when modifying one end of the line.
 *  The line is described by four <B>*w_current</B> variables : the first end
 *  of the line is (<B>first_wx</B>,<B>first_wy</B>), the second end is
 *  (<B>second_wx</B>,<B>second_wy</B>).
 *  The first end is constant. The second end is updated to the (<B>w_x</B>,<B>w_y</B>).
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  if (schematic_window_get_rubber_visible (w_current))
    o_line_invalidate_rubber (w_current);

  /*
   * The coordinates of the moving end of the line are updated. Its new
   * coordinates are in <B>w_x</B> and <B>w_y</B> parameters and saved to
   * <B>w_current->second_wx</B> and <B>w_current->second_wy</B> respectively.
   */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* if the control key was pressed then draw ortho lines */
  if (w_current->CONTROLKEY) {
    diff_x = abs(w_current->second_wx - w_current->first_wx);
    diff_y = abs(w_current->second_wy - w_current->first_wy);

    if (diff_x >= diff_y) {
      w_current->second_wy = w_current->first_wy;
    } else {
      w_current->second_wx = w_current->first_wx;
    }
  }

  o_line_invalidate_rubber (w_current);
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \brief Draw line from GschemToplevel object.
 *  \par Function Description
 *  This function draws a line with an exclusive or function over
 *  the sheet using \a renderer.
 *  The color of the box is <B>SELECT_COLOR</B>. The line is
 *  described by the two points (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void o_line_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  double wwidth = 0;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_line (cr, flags, END_NONE, wwidth,
                  w_current->first_wx, w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}
