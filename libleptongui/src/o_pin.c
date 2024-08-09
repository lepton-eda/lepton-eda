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
void
o_pin_start (SchematicWindow *w_current,
             int w_x,
             int w_y)
{
  i_action_start (w_current);

  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_pin_end (SchematicWindow *w_current,
           int x,
           int y)
{
  LeptonObject *new_obj;

  g_assert (schematic_window_get_inside_action (w_current) != 0);

  SchematicCanvas *page_view = schematic_window_get_current_canvas (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = schematic_canvas_get_page (page_view);
  g_return_if_fail (page != NULL);

  /* undraw rubber line */
  /* o_pin_invalidate_rubber (w_current); */
  schematic_window_set_rubber_visible (w_current, 0);

  /* don't allow zero length pins */
  if ((w_current->first_wx == w_current->second_wx) &&
      (w_current->first_wy == w_current->second_wy)) {
    return;
  }

  new_obj = lepton_pin_object_new (PIN_COLOR,
                                   w_current->first_wx,
                                   w_current->first_wy,
                                   w_current->second_wx,
                                   w_current->second_wy,
                                   PIN_TYPE_NET,
                                   0);
  lepton_page_append (page, new_obj);

  /* Call add-objects-hook */
  g_run_hook_object (w_current, "add-objects-hook", new_obj);

  schematic_window_page_content_changed (w_current, page);
  o_undo_savestate(w_current, page, FALSE);
  i_action_stop (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_pin_motion (SchematicWindow *w_current,
              int w_x,
              int w_y)
{
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  /* erase the rubberpin if it is visible */
  if (schematic_window_get_rubber_visible (w_current))
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
  schematic_window_set_rubber_visible (w_current, 1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void
o_pin_invalidate_rubber (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  SchematicCanvas *page_view = schematic_window_get_current_canvas (w_current);

  schematic_canvas_invalidate_world_rect (page_view,
                                          w_current->first_wx,
                                          w_current->first_wy,
                                          w_current->second_wx,
                                          w_current->second_wy);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_pin_draw_rubber (SchematicWindow *w_current,
                   EdaRenderer *renderer)
{
  double wwidth = PIN_WIDTH_NET;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);

  eda_cairo_line (cr, flags, END_NONE, wwidth,
                  w_current->first_wx, w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
}
