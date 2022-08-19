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


/*! \brief set the start point of a new bus
 *  \par Function Description
 *  This function sets the start point (<B>w_x</B>,<B>w_y</B>) of a new bus
 *  in the <B>GschemToplevel</B> structure.
 *
 *  The start point is stored in <B>first_wx</B>, <B>first_wy</B>.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        the x position in world coords
 *  \param [in] w_y        the y position in world coords
 */
void o_bus_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \brief finish a bus drawing action
 *  \par Function Description
 *  This function finishes a net drawing action. The function draws
 *  a bus from the point (<B>first_wx</B>,<B>first_wy</B>) to
 *  (<B>second_wx</B>,<B>second_wy</B>). Both points are taken from
 *  the <B>GschemToplevel</B> structure.
 *
 *  The function returns TRUE if a bus object has been created and
 *  FALSE if no bus object has been created.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_bus_end(GschemToplevel *w_current, int w_x, int w_y)
{
  LeptonObject *new_obj;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  GList *prev_conn_objects = NULL;

  /* erase the rubberbus */
  /* o_bus_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 0);

  /* don't allow zero length bus */
  /* this ends the bus drawing behavior we want this? hack */
  if ( (w_current->first_wx != w_current->second_wx) ||
       (w_current->first_wy != w_current->second_wy) ) {

    new_obj = lepton_bus_object_new (BUS_COLOR,
                                     w_current->first_wx,
                                     w_current->first_wy,
                                     w_current->second_wx,
                                     w_current->second_wy,
                                     0);
    lepton_page_append (page, new_obj);

    /* connect the new bus to the other busses */
    prev_conn_objects = s_conn_return_others (prev_conn_objects, new_obj);
    o_invalidate_glist (w_current, prev_conn_objects);
    g_list_free (prev_conn_objects);

    /* Call add-objects-hook */
    g_run_hook_object (w_current, "add-objects-hook", new_obj);

    w_current->first_wx = w_current->second_wx;
    w_current->first_wy = w_current->second_wy;

    gschem_toplevel_page_content_changed (w_current, page);
    o_undo_savestate(w_current, page, UNDO_ALL);
  }

  /* Don't reset w_current->inside_action here since we want to continue drawing */
}

/*! \brief draw the bus rubber when creating a bus
 *  \par Function Description
 *  This function draws
 *  a bus rubber from the point (<B>first_wx</B>,<B>first_wy</B>) from
 *  the <B>GschemToplevel</B> structure to the input parameter
 *  (<B>w_x</B>, <B>w_y</B>).
 *
 *  The function stores creates an non-orthogonal bus segment if the
 *  CONTROLKEY is pressed. The coordinates of the second rubberbus point
 *  is stored as (<B>second_wx</B>,<B>second_wy</B>) in the
 *  <B>GschemToplevel</B> structure.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        current x position in world units
 *  \param [in] w_y        current y position in world units
 */
void o_bus_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  if (w_current->rubber_visible)
    o_bus_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* If you press the control key then you can draw non-ortho bus */
  if (!w_current->CONTROLKEY) {
    diff_x = abs(w_current->second_wx - w_current->first_wx);
    diff_y = abs(w_current->second_wy - w_current->first_wy);

    if (diff_x >= diff_y) {
      w_current->second_wy = w_current->first_wy;
    } else {
      w_current->second_wx = w_current->first_wx;
    }
  }

  o_bus_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_bus_invalidate_rubber (GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->first_wx,
                                          w_current->first_wy,
                                          w_current->second_wx,
                                          w_current->second_wy);
}

/*! \brief draw a rubberbus segment
 *  \par Function Description
 *  This function draws a bus segment from the point
 *  (<B>first_wx</B>,<B>first_wy</B>) to the point
 *  (<B>second_wx</B>,<B>second_wy</B>) from the <B>GschemToplevel</B>
 *   structure using \a renderer.
 *
 *  The function can be used to draw or erase the rubberbus on the screen.
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void
o_bus_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  int size = BUS_WIDTH;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_line (cr, flags, END_NONE, size,
                  w_current->first_wx,  w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);
  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, size, -1, -1);
}



void
o_bus_reset (GschemToplevel* w_current)
{
  o_bus_invalidate_rubber (w_current);
  i_action_stop (w_current);
}
