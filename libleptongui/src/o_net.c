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


/* magnetic options */
/* half size of the magnetic marker on the screen. */
#define MAGNETIC_HALFSIZE 6

/* define how far the cursor could be to activate magnetic */
#define MAGNETIC_PIN_REACH 50
#define MAGNETIC_NET_REACH 20
#define MAGNETIC_BUS_REACH 30

/* weighting factors to tell that a pin is more important than a net */
#define MAGNETIC_PIN_WEIGHT 5.0
#define MAGNETIC_NET_WEIGHT 2.0
#define MAGNETIC_BUS_WEIGHT 3.0

/* Bit definitions for the four quardrants of the direction guessing */
#define QUADRANT1  0x01
#define QUADRANT2  0x02
#define QUADRANT3  0x04
#define QUADRANT4  0x08

typedef struct st_bus_ripper BUS_RIPPER;

struct st_bus_ripper
{
  int x[2];
  int y[2];
};

/*! \brief Reset all variables used for net drawing
 *  \par Function Description
 *  This function resets all variables from GschemToplevel that are used
 *  for net drawing. This function should be called when escaping from
 *  a net drawing action or before entering it.
 */
void o_net_reset(GschemToplevel *w_current)
{
  o_net_invalidate_rubber (w_current);
  w_current->first_wx = w_current->first_wy = -1;
  w_current->second_wx = w_current->second_wy = -1;
  w_current->third_wx = w_current->third_wy = -1;
  w_current->magnetic_wx = w_current->magnetic_wy = -1;
  w_current->rubber_visible = 0;
  i_action_stop (w_current);
}

/*! \brief guess the best direction for the next net drawing action
 *  \par Function Description
 *  This function checks all connectable objects at a starting point.
 *  It determines the best drawing direction for each quadrant of the
 *  possible net endpoint.
 *
 *  The directions are stored in the GschemToplevel->net_direction variable
 *  as a bitfield.
 */
void o_net_guess_direction(GschemToplevel *w_current,
                           int wx, int wy)
{
  int up=0, down=0, left=0, right=0;
  int x1, y1, x2, y2;
  int xmin, ymin, xmax, ymax;
  int orientation;
  GList *object_list, *iter1, *iter2;
  LeptonObject *o_current;

  int *current_rules;
  /* badness values       {OVERWRITE, ORTHO, CONTINUE} */
  const int pin_rules[] = {100, 50, 0};
  const int bus_rules[] = {90, 0, 40};
  const int net_rules[] = {80, 30, 0};

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  object_list = g_list_append (NULL, page->connectible_list);

  for (iter1 = object_list; iter1 != NULL; iter1 = g_list_next(iter1)) {
    for (iter2 = (GList*) iter1->data; iter2 != NULL; iter2 = g_list_next(iter2)) {
      o_current = (LeptonObject*) iter2->data;

      if ((orientation = lepton_net_object_orientation (o_current)) == NEITHER)
        continue;

      switch (lepton_object_get_type (o_current)) {
      case OBJ_NET:
        current_rules = (int*) net_rules;
        break;
      case OBJ_PIN:
        current_rules = (int*) pin_rules;
        break;
      case OBJ_BUS:
        current_rules = (int*) bus_rules;
        break;
      default:
        current_rules = (int*) net_rules;
        g_assert_not_reached ();
      }

      x1 = o_current->line->x[0];
      x2 = o_current->line->x[1];
      y1 = o_current->line->y[0];
      y2 = o_current->line->y[1];

      xmin = MIN(x1, x2);
      ymin = MIN(y1, y2);
      xmax = MAX(x1, x2);
      ymax = MAX(y1, y2);

      if (orientation == HORIZONTAL && wy == y1) {
        if (wx == xmin) {
          up = MAX(up, current_rules[1]);
          down = MAX(down, current_rules[1]);
          right = MAX(right, current_rules[0]);
          left = MAX(left, current_rules[2]);
        }
        else if (wx == xmax) {
          up = MAX(up, current_rules[1]);
          down = MAX(down, current_rules[1]);
          right = MAX(right, current_rules[2]);
          left = MAX(left, current_rules[0]);
        }
        else if (xmin < wx && wx < xmax) {
          up = MAX(up, current_rules[1]);
          down = MAX(down, current_rules[1]);
          right = MAX(right, current_rules[0]);
          left = MAX(left, current_rules[0]);
        }
        else {
          continue;
        }
      }
      if (orientation == VERTICAL && wx == x1) {
        if (wy == ymin) {
          up = MAX(up, current_rules[0]);
          down = MAX(down, current_rules[2]);
          right = MAX(right, current_rules[1]);
          left = MAX(left, current_rules[1]);
        }
        else if (wy == ymax) {
          up = MAX(up, current_rules[2]);
          down = MAX(down, current_rules[0]);
          right = MAX(right, current_rules[1]);
          left = MAX(left, current_rules[1]);
        }
        else if (ymin < wy && wy < ymax) {
          up = MAX(up, current_rules[0]);
          down = MAX(down, current_rules[0]);
          right = MAX(right, current_rules[1]);
          left = MAX(left, current_rules[1]);
        }
        else {
          continue;
        }
      }
    }
  }

  w_current->net_direction = 0;
  w_current->net_direction |= up >= right ? 0 : QUADRANT1;
  w_current->net_direction |= up >= left ? 0 : QUADRANT2;
  w_current->net_direction |= down >= left ? 0 : QUADRANT3;
  w_current->net_direction |= down >= right ? 0 : QUADRANT4;

#if 0
  printf("o_net_guess_direction: up=%d down=%d left=%d right=%d direction=%d\n",
         up, down, left, right, w_current->net_direction);
#endif
  g_list_free (object_list);
}

/*! \brief find the closest possible location to connect to
 *  \par Function Description
 *  This function calculates the distance to all connectable objects
 *  and searches the closest connection point.
 *  It searches for pins, nets and busses.
 *
 *  The connection point is stored in GschemToplevel->magnetic_wx and
 *  GschemToplevel->magnetic_wy. If no connection is found. Both variables
 *  are set to -1.
 */
void o_net_find_magnetic(GschemToplevel *w_current,
                         int w_x, int w_y)
{
  int x1, x2, y1, y2, min_x, min_y;
  double mindist, minbest, dist1, dist2;
  double weight, min_weight;
  int magnetic_reach = 0;
  LeptonObject *o_current;
  LeptonObject *o_magnetic = NULL;
  GList *object_list, *iter1, *iter2;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  minbest = min_x = min_y = 0;
  min_weight = 0;

  /* max distance of all the different reaches */
  magnetic_reach = MAX(MAGNETIC_PIN_REACH, MAGNETIC_NET_REACH);
  magnetic_reach = MAX(magnetic_reach, MAGNETIC_BUS_REACH);

  object_list = g_list_append (NULL, page->connectible_list);

  for (iter1 = object_list; iter1 != NULL; iter1 = g_list_next(iter1)) {
    for (iter2 = (GList*) iter1->data; iter2 != NULL; iter2 = g_list_next(iter2)) {
      int left, top, right, bottom;
      o_current = (LeptonObject*) iter2->data;

      if (!lepton_object_calculate_visible_bounds (o_current,
                                                   FALSE,
                                                   &left,
                                                   &top,
                                                   &right,
                                                   &bottom) ||
          !visible (w_current, left, top, right, bottom))
        continue; /* skip invisible objects */

      if (lepton_object_is_pin (o_current))
      {
        min_x = o_current->line->x[o_current->whichend];
        min_y = o_current->line->y[o_current->whichend];

        mindist = hypot(w_x - min_x, w_y - min_y);
        weight = mindist / MAGNETIC_PIN_WEIGHT;
      }

      else if (lepton_object_is_net (o_current)
               || lepton_object_is_bus (o_current))
      {
        /* we have 3 possible points to connect:
           2 endpoints and 1 midpoint point */
        x1 = o_current->line->x[0];
        y1 = o_current->line->y[0];
        x2 = o_current->line->x[1];
        y2 = o_current->line->y[1];
        /* endpoint tests */
        dist1 = hypot(w_x - x1, w_y - y1);
        dist2 = hypot(w_x - x2, w_y - y2);
        if (dist1 < dist2) {
          min_x = x1;
          min_y = y1;
          mindist = dist1;
        }
        else {
          min_x = x2;
          min_y = y2;
          mindist = dist2;
        }

        /* midpoint tests */
        if ((x1 == x2)  /* vertical net */
            && ((y1 >= w_y && w_y >= y2)
                || (y2 >= w_y && w_y >= y1))) {
          if (abs(w_x - x1) < mindist) {
            mindist = abs(w_x - x1);
            min_x = x1;
            min_y = w_y;
          }
        }
        if ((y1 == y2)  /* horitontal net */
            && ((x1 >= w_x && w_x >= x2)
                || (x2 >= w_x && w_x >= x1))) {
          if (abs(w_y - y1) < mindist) {
            mindist = abs(w_y - y1);
            min_x = w_x;
            min_y = y1;
          }
        }

        if (lepton_object_is_bus (o_current))
          weight = mindist / MAGNETIC_BUS_WEIGHT;
        else /* OBJ_NET */
          weight = mindist / MAGNETIC_NET_WEIGHT;
      }
      else { /* neither pin nor net or bus */
        continue;
      }

      if (o_magnetic == NULL
          || weight < min_weight) {
        minbest = mindist;
        min_weight = weight;
        o_magnetic = o_current;
        w_current->magnetic_wx = min_x;
        w_current->magnetic_wy = min_y;
      }
    }
  }

  /* check whether we found an object and if it's close enough */
  if (o_magnetic != NULL) {
    switch (lepton_object_get_type (o_magnetic)) {
    case (OBJ_PIN): magnetic_reach = MAGNETIC_PIN_REACH; break;
    case (OBJ_NET): magnetic_reach = MAGNETIC_NET_REACH; break;
    case (OBJ_BUS): magnetic_reach = MAGNETIC_BUS_REACH; break;
    }
    if (minbest > gschem_page_view_WORLDabs (page_view, magnetic_reach)) {
      w_current->magnetic_wx = -1;
      w_current->magnetic_wy = -1;
    }
  }
  else {
    w_current->magnetic_wx = -1;
    w_current->magnetic_wy = -1;
  }

  g_list_free (object_list);
}

/*! \brief calcutates the net route to the magnetic marker
 *  \par Function Description
 *  Depending on the two rubbernet lines from start to last and from
 *  last to second, the 3 coordinates are manipulated to find
 *  a way to the magnetic marker.
 */
void o_net_finishmagnetic(GschemToplevel *w_current)
{
  int primary_zero_length, secondary_zero_length;

  primary_zero_length = ((w_current->first_wx == w_current->second_wx)
                         && (w_current->first_wy == w_current->second_wy));

  secondary_zero_length = ((w_current->second_wx == w_current->third_wx)
                           && (w_current->second_wy == w_current->third_wy));

  if (!primary_zero_length && secondary_zero_length) {
    if (w_current->first_wx == w_current->second_wx) {
      /* expand vertical line to magnetic_wy */
      w_current->second_wy = w_current->magnetic_wy;
    }
    else if (w_current->first_wy == w_current->second_wy) {
      /* expand horitontal line to vertical to magnetic_wx */
      w_current->second_wx = w_current->magnetic_wx;
    }
    /* connect to magnetic */
    w_current->third_wx = w_current->magnetic_wx;
    w_current->third_wy = w_current->magnetic_wy;
  }

  if (primary_zero_length && !secondary_zero_length) {
    /* move second line to the first (empty line) */
    w_current->first_wx = w_current->second_wx;
    w_current->first_wy = w_current->second_wy;
    if (w_current->second_wx == w_current->third_wx) {
      /* expand vertical line to magnetic_wy */
      w_current->second_wy = w_current->magnetic_wy;
    }
    else if (w_current->second_wy == w_current->third_wy) {
      /* expand horitontal line to magnetic_wx */
      w_current->second_wx = w_current->magnetic_wx;
    }
    /* connect to magnetic */
    w_current->third_wx = w_current->magnetic_wx;
    w_current->third_wy = w_current->magnetic_wy;
  }

  if (!primary_zero_length && !secondary_zero_length) {
    /* expand line in both directions */
    if (w_current->first_wx == w_current->second_wx) {
      w_current->second_wy = w_current->magnetic_wy;
    }
    else {
      w_current->second_wx = w_current->magnetic_wx;
    }
    w_current->third_wx = w_current->magnetic_wx;
    w_current->third_wy = w_current->magnetic_wy;
  }
}

/*! \brief callback function to draw a net marker in magnetic mode
 *  \par Function Description
 *  If the mouse is moved, this function is called to update the
 *  position of the magnetic marker.
 *  If the controllkey is pressed the magnetic marker follows the mouse.
 */
void o_net_start_magnetic(GschemToplevel *w_current, int w_x, int w_y)
{
  if (!(gschem_options_get_magnetic_net_mode (w_current->options))) {
    return;
  }

  o_net_invalidate_rubber (w_current);

  if (w_current->CONTROLKEY) {
    w_current->magnetic_wx = w_x;
    w_current->magnetic_wy = w_y;
  }
  else {
    o_net_find_magnetic(w_current, w_x, w_y);
  }

  o_net_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief set the start point of a new net
 *  \par Function Description
 *  This function sets the start point of a new net at the position of the
 *  cursor. If we have a visible magnetic marker, we use that instead of
 *  the cursor position
 */
void o_net_start(GschemToplevel *w_current, int w_x, int w_y)
{
  i_action_start (w_current);

  if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {
    w_current->first_wx = w_current->magnetic_wx;
    w_current->first_wy = w_current->magnetic_wy;
  }
  else {
    w_current->first_wx = w_x;
    w_current->first_wy = w_y;
  }

  w_current->second_wx = w_current->third_wx = w_current->first_wx;
  w_current->second_wy = w_current->third_wy = w_current->first_wy;

  if (w_current->first_wx != snap_grid (w_current, w_current->first_wx)
      || w_current->first_wy != snap_grid (w_current, w_current->first_wy))
      g_message (_("Warning: Starting net at off grid coordinate"));

  if (w_current->net_direction_mode)
    o_net_guess_direction(w_current, w_current->first_wx, w_current->first_wy);
}

/*! \brief finish a net drawing action
 * \par Function Description
 * This function finishes the drawing of a net. If we have a visible
 * magnetic marker, we use that instead of the current cursor
 * position.
 *
 * The rubber nets are removed, the nets and cues are drawn and the
 * net is added to the LeptonToplevel structure.
 *
 * The function returns TRUE if it has drawn a net, FALSE otherwise.
 */
void o_net_end(GschemToplevel *w_current, int w_x, int w_y)
{
  int primary_zero_length, secondary_zero_length;
  int found_primary_connection = FALSE;
  int save_wx, save_wy;

  GList *prev_conn_objects;
  LeptonObject *new_net = NULL;

  /* Save a list of added objects to run the add-objects-hook later */
  GList *added_objects = NULL;

  g_assert( w_current->inside_action != 0 );

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  o_net_invalidate_rubber (w_current);

  if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1)
    o_net_finishmagnetic(w_current);

  w_current->rubber_visible = 0;

  /* See if either of the nets are zero length.  We'll only add */
  /* the non-zero ones */
  primary_zero_length = (w_current->first_wx == w_current->second_wx) &&
    (w_current->first_wy == w_current->second_wy);

  secondary_zero_length = (w_current->second_wx == w_current->third_wx) &&
      (w_current->second_wy == w_current->third_wy);

  /* If both nets are zero length... */
  /* this ends the net drawing behavior */
  if ( primary_zero_length && secondary_zero_length ) {
    o_net_reset(w_current);
    return;
  }

  save_wx = w_current->third_wx;
  save_wy = w_current->third_wy;

  if (w_current->third_wx != snap_grid (w_current, w_current->third_wx)
      || w_current->third_wy != snap_grid (w_current, w_current->third_wy))
      g_message (_("Warning: Ending net at off grid coordinate"));

  if (!primary_zero_length ) {
  /* create primary net */
    new_net = lepton_net_object_new (NET_COLOR,
                                     w_current->first_wx,
                                     w_current->first_wy,
                                     w_current->second_wx,
                                     w_current->second_wy);
    lepton_page_append (page, new_net);

      added_objects = g_list_prepend (added_objects, new_net);

      /* conn stuff */
      /* LEAK CHECK 1 */
      prev_conn_objects = s_conn_return_others (NULL, new_net);
      o_net_add_busrippers (w_current, new_net, prev_conn_objects);
      g_list_free (prev_conn_objects);

#if DEBUG
      printf("primary:\n");
      s_conn_print(new_net->conn_list);
#endif

      /* Go off and search for valid connection on this newly created net */
      found_primary_connection = s_conn_net_search(new_net, 1,
                                                   new_net->conn_list);
      if (found_primary_connection)
      {
        /* if a net connection is found, reset start point of next net */
        save_wx = w_current->second_wx;
        save_wy = w_current->second_wy;
      }
  }


  /* If the second net is not zero length, add it as well */
  /* Also, a valid net connection from the primary net was not found */
  if (!secondary_zero_length && !found_primary_connection) {

      /* Add secondary net */
    new_net = lepton_net_object_new (NET_COLOR,
                                     w_current->second_wx,
                                     w_current->second_wy,
                                     w_current->third_wx,
                                     w_current->third_wy);
    lepton_page_append (page, new_net);

      added_objects = g_list_prepend (added_objects, new_net);

      /* conn stuff */
      /* LEAK CHECK 2 */
      prev_conn_objects = s_conn_return_others (NULL, new_net);
      o_net_add_busrippers (w_current, new_net, prev_conn_objects);
      g_list_free (prev_conn_objects);
#if DEBUG
      s_conn_print(new_net->conn_list);
#endif
  }

  /* Call add-objects-hook */
  if (added_objects != NULL) {
    g_run_hook_object_list (w_current, "add-objects-hook", added_objects);
    g_list_free (added_objects);
  }

  w_current->first_wx = save_wx;
  w_current->first_wy = save_wy;

  gschem_toplevel_page_content_changed (w_current, page);
  o_undo_savestate_old(w_current, UNDO_ALL);

  /* Continue net drawing */
  o_net_start(w_current, w_current->first_wx, w_current->first_wy);
}

/*! \brief erase and redraw the rubber lines when drawing a net
 *  \par Function Description
 *  This function draws the rubbernet lines when drawing a net.
 */
void o_net_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  int ortho, horizontal, quadrant;
  gboolean magnetic_net_mode;

  g_return_if_fail (w_current != NULL);
  g_assert( w_current->inside_action != 0 );

  magnetic_net_mode = gschem_options_get_magnetic_net_mode (w_current->options);

  /* Orthognal mode enabled when Control Key is NOT pressed or
     if we are using magnetic mode */
  ortho = !w_current->CONTROLKEY || magnetic_net_mode;

  if (w_current->rubber_visible)
    o_net_invalidate_rubber (w_current);

  if (magnetic_net_mode) {
    if (w_current->CONTROLKEY) {
      /* set the magnetic marker position to current xy if the
         controlkey is pressed. Thus the net will not connect to
         the closest net if we finish the net drawing */
      w_current->magnetic_wx = w_x;
      w_current->magnetic_wy = w_y;
    }
    else {
      o_net_find_magnetic(w_current, w_x, w_y);
    }
  }

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* In orthogonal mode secondary line is the same as the first */
  if (!ortho) {
      w_current->third_wx = w_current->second_wx;
      w_current->third_wy = w_current->second_wy;
  }
  /* If you press the control key then you can draw non-ortho nets */
  else {
    if (w_current->second_wy > w_current->first_wy)
      quadrant = w_current->second_wx > w_current->first_wx ? QUADRANT1: QUADRANT2;
    else
      quadrant = w_current->second_wx > w_current->first_wx ? QUADRANT4: QUADRANT3;

    horizontal = w_current->net_direction & quadrant;

    if (!w_current->SHIFTKEY)
      horizontal = !horizontal;

    /* calculate the co-ordinates necessary to draw the lines*/
    /* Pressing the shift key will cause the vertical and horizontal lines to switch places */
    if ( horizontal ) {
      w_current->second_wy = w_current->first_wy;
      w_current->third_wx = w_current->second_wx;
      w_current->third_wy = w_y;
    } else {
      w_current->second_wx = w_current->first_wx;
      w_current->third_wx = w_x;
      w_current->third_wy = w_current->second_wy;
    }
  }

  o_net_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief draw rubbernet lines to the gc
 *  \par Function Description
 *  This function draws the rubbernets to the graphic context
 */
void
o_net_draw_rubber(GschemToplevel *w_current, EdaRenderer *renderer)
{
  int size = NET_WIDTH, w_magnetic_halfsize;
  cairo_t *cr = eda_renderer_get_cairo_context (renderer);
  GArray *color_map = eda_renderer_get_color_map (renderer);
  int flags = eda_renderer_get_cairo_flags (renderer);
  gboolean magnetic_net_mode;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);

  magnetic_net_mode = gschem_options_get_magnetic_net_mode (w_current->options);

  if (magnetic_net_mode) {
    if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {
      w_magnetic_halfsize = MAX (4 * size,
                                 gschem_page_view_WORLDabs (page_view, MAGNETIC_HALFSIZE));
      eda_cairo_arc (cr, flags, size,
                     w_current->magnetic_wx, w_current->magnetic_wy,
                     w_magnetic_halfsize, 0, 360);
    }
  }

  /* Primary line */
  eda_cairo_line (cr, flags, END_NONE, size,
                  w_current->first_wx,  w_current->first_wy,
                  w_current->second_wx, w_current->second_wy);

  /* Secondary line */
  eda_cairo_line (cr, flags, END_NONE, size,
                     w_current->second_wx, w_current->second_wy,
                     w_current->third_wx,  w_current->third_wy);

  eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, size, -1, -1);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_invalidate_rubber (GschemToplevel *w_current)
{
  int size = 0, magnetic_halfsize;
  int magnetic_x, magnetic_y;
  gboolean magnetic_net_mode;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  gschem_page_view_WORLDtoSCREEN (page_view,
                                  w_current->magnetic_wx, w_current->magnetic_wy,
                                  &magnetic_x, &magnetic_y);

  size = gschem_page_view_SCREENabs (page_view, NET_WIDTH);

  magnetic_net_mode = gschem_options_get_magnetic_net_mode (w_current->options);

  if (magnetic_net_mode) {
    if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {
      magnetic_halfsize = MAX (4 * size, MAGNETIC_HALFSIZE);

      o_invalidate_rect (w_current, magnetic_x - magnetic_halfsize,
                                    magnetic_y - magnetic_halfsize,
                                    magnetic_x + magnetic_halfsize,
                                    magnetic_y + magnetic_halfsize);
    }
  }

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->first_wx,
                                          w_current->first_wy,
                                          w_current->second_wx,
                                          w_current->second_wy);

  gschem_page_view_invalidate_world_rect (page_view,
                                          w_current->second_wx,
                                          w_current->second_wy,
                                          w_current->third_wx,
                                          w_current->third_wy);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_net_add_busrippers(GschemToplevel *w_current, LeptonObject *net_obj,
                         GList *prev_conn_objects)

{
  LeptonObject *new_obj;
  GList *cl_current = NULL;
  LeptonObject *bus_object = NULL;
  LeptonConn *found_conn = NULL;
  int done;
  int otherone;
  BUS_RIPPER rippers[2];
  int ripper_count = 0;
  int i;
  double length;
  int sign;
  double distance1, distance2;
  int first, second;
  int made_changes = FALSE;
  const int ripper_size = w_current->bus_ripper_size;
  int component_angle = 0;
  const CLibSymbol *rippersym = NULL;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_val_if_fail (page_view != NULL, FALSE);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_val_if_fail (page != NULL, FALSE);

  length = lepton_line_object_length (net_obj);

  if (!prev_conn_objects) {
    return(FALSE);
  }

  if (length <= ripper_size) {
    return(FALSE);
  }

  /* check for a bus connection and draw rippers if so */
  cl_current = prev_conn_objects;
  while (cl_current != NULL) {

    bus_object = (LeptonObject *) cl_current->data;
    if (lepton_object_is_bus (bus_object))
    {
      /* yes, using the net routine is okay */
      int bus_orientation = lepton_net_object_orientation (bus_object);
      int net_orientation = lepton_net_object_orientation (net_obj);

      /* find the LeptonConn structure which is associated with this object */
      GList *cl_current2 = net_obj->conn_list;
      done = FALSE;
      while (cl_current2 != NULL && !done) {
        LeptonConn *tmp_conn = (LeptonConn *) cl_current2->data;

        if (tmp_conn && tmp_conn->other_object &&
            tmp_conn->other_object == bus_object) {

          found_conn = tmp_conn;
          done = TRUE;
        }

        cl_current2 = g_list_next(cl_current2);
      }

      if (!found_conn) {
        return(FALSE);
      }

      otherone = !found_conn->whichone;

      /* now deal with the found connection */
      if (bus_orientation == HORIZONTAL && net_orientation == VERTICAL) {
        /* printf("found horiz bus %s %d!\n", bus_object->name,
           found_conn->whichone);*/

        sign = lepton_bus_object_get_ripper_direction (bus_object);
        if (!sign) {
          if (bus_object->line->x[0] < bus_object->line->x[1]) {
            first = 0;
            second = 1;
          } else {
            first = 1;
            second = 0;
          }

          distance1 = abs(bus_object->line->x[first] -
                          net_obj->line->x[found_conn->whichone]);
          distance2 = abs(bus_object->line->x[second] -
                          net_obj->line->x[found_conn->whichone]);

          if (distance1 <= distance2) {
            sign = 1;
          } else {
            sign = -1;
          }
          lepton_bus_object_set_ripper_direction (bus_object, sign);
        }
        /* printf("hor sign: %d\n", sign); */

        if (net_obj->line->y[otherone] < bus_object->line->y[0]) {
          /* new net is below bus */
          /*printf("below\n");*/

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            fprintf(stderr, _("Tried to add more than two bus rippers. Internal gschem error.\n"));
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              component_angle = 0;
            } else {
              component_angle = 90;
            }
          } else {
            /* symmetric */
            component_angle = 0;
          }

          net_obj->line->y[found_conn->whichone] -= ripper_size;
          rippers[ripper_count].x[0] =
            net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] =
            net_obj->line->y[found_conn->whichone];
          rippers[ripper_count].x[1] =
            net_obj->line->x[found_conn->whichone] + sign*ripper_size;
          rippers[ripper_count].y[1] =
            net_obj->line->y[found_conn->whichone] + ripper_size;
          ripper_count++;
          /* printf("done\n"); */
          made_changes++;

        } else {
          /* new net is above bus */
          /* printf("above\n"); */

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            fprintf(stderr, _("Tried to add more than two bus rippers. Internal gschem error.\n"));
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              component_angle = 270;
            } else {
              component_angle = 180;
            }
          } else {
            /* symmetric */
            component_angle = 180;
          }

          net_obj->line->y[found_conn->whichone] += ripper_size;
          rippers[ripper_count].x[0] =
            net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] =
            net_obj->line->y[found_conn->whichone];
          rippers[ripper_count].x[1] =
            net_obj->line->x[found_conn->whichone] + sign*ripper_size;
          rippers[ripper_count].y[1] =
            net_obj->line->y[found_conn->whichone] - ripper_size;
            ripper_count++;

            /* printf("done\n"); */
          made_changes++;
        }


      } else if (bus_orientation == VERTICAL &&
                 net_orientation == HORIZONTAL) {

        /* printf("found vert bus %s %d!\n", bus_object->name,
           found_conn->whichone); */

        sign = lepton_bus_object_get_ripper_direction (bus_object);
        if (!sign) {
          if (bus_object->line->y[0] < bus_object->line->y[1]) {
            first = 0;
            second = 1;
          } else {
            first = 1;
            second = 0;
          }

          distance1 = abs(bus_object->line->y[first] -
                          net_obj->line->y[found_conn->whichone]);
          distance2 = abs(bus_object->line->y[second] -
                          net_obj->line->y[found_conn->whichone]);

          if (distance1 <= distance2) {
            sign = 1;
          } else {
            sign = -1;
          }
          lepton_bus_object_set_ripper_direction (bus_object, sign);
        }
        /* printf("ver sign: %d\n", sign); */


        if (net_obj->line->x[otherone] < bus_object->line->x[0]) {
          /* new net is to the left of the bus */
          /* printf("left\n"); */

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            fprintf(stderr, _("Tried to add more than two bus rippers. Internal gschem error.\n"));
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              component_angle = 0;
            } else {
              component_angle = 270;
            }
          } else {
            /* symmetric */
            component_angle = 270;
          }

          net_obj->line->x[found_conn->whichone] -= ripper_size;
          rippers[ripper_count].x[0] =
            net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] =
            net_obj->line->y[found_conn->whichone];
          rippers[ripper_count].x[1] =
            net_obj->line->x[found_conn->whichone] + ripper_size;
          rippers[ripper_count].y[1] =
            net_obj->line->y[found_conn->whichone] + sign*ripper_size;
          ripper_count++;

          made_changes++;
        } else {
          /* new net is to the right of the bus */
          /* printf("right\n"); */

          if (ripper_count >= 2) {
            /* try to exit gracefully */
            fprintf(stderr, _("Tried to add more than two bus rippers. Internal gschem error.\n"));
            made_changes = FALSE;
            break;
          }

          if (w_current->bus_ripper_rotation == NON_SYMMETRIC) {
            /* non-symmetric */
            if (sign == 1) {
              component_angle = 90;
            } else {
              component_angle = 180;
            }
          } else {
            /* symmetric */
            component_angle = 90;
          }

          net_obj->line->x[found_conn->whichone] += ripper_size;
          rippers[ripper_count].x[0] =
            net_obj->line->x[found_conn->whichone];
          rippers[ripper_count].y[0] =
            net_obj->line->y[found_conn->whichone];
          rippers[ripper_count].x[1] =
            net_obj->line->x[found_conn->whichone] - ripper_size;
          rippers[ripper_count].y[1] =
            net_obj->line->y[found_conn->whichone] + sign*ripper_size;
          ripper_count++;

          made_changes++;
        }
      }
    }


    cl_current = g_list_next(cl_current);
  }

  if (made_changes) {
    s_conn_remove_object_connections (net_obj);

    if (w_current->bus_ripper_type == COMP_BUS_RIPPER) {
      GList *symlist =
        s_clib_search (w_current->bus_ripper_symname, CLIB_EXACT);
      if (symlist != NULL) {
        rippersym = (CLibSymbol *) symlist->data;
      }
      g_list_free (symlist);
    }

    for (i = 0; i < ripper_count; i++) {
      if ((w_current->bus_ripper_type == NET_BUS_RIPPER) &&
          /* Don't add a new net if the coords are the same.
           * Otherwise it will be zero sized. */
          ! ((rippers[i].x[0] == rippers[i].x[1]) &&
             (rippers[i].y[0] == rippers[i].y[1])))
      {
        new_obj = lepton_net_object_new (NET_COLOR,
                                         rippers[i].x[0],
                                         rippers[i].y[0],
                                         rippers[i].x[1],
                                         rippers[i].y[1]);
        lepton_page_append (page, new_obj);
      } else {

        if (rippersym != NULL) {
          new_obj = lepton_component_new (page,
                                          default_color_id(),
                                          rippers[i].x[0],
                                          rippers[i].y[0],
                                          component_angle,
                                          0,
                                          rippersym,
                                          w_current->bus_ripper_symname,
                                          1);
          lepton_page_append_list (page,
                              lepton_component_promote_attribs (new_obj));
          lepton_page_append (page, new_obj);
        } else {
          g_message (_("Bus ripper symbol [%1$s] was not found in any component library"),
                     w_current->bus_ripper_symname);
        }
      }
    }

    s_conn_update_object (page, net_obj);
    return(TRUE);
  }

  return(FALSE);
}
