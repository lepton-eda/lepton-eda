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


/*! \brief Reset all variables used for net drawing
 *  \par Function Description
 *  This function resets all variables from GSCHEM_TOPLEVEL that are used
 *  for net drawing. This function should be called when escaping from
 *  a net drawing action or before entering it.
 */
void o_net_reset(GSCHEM_TOPLEVEL *w_current) 
{
  w_current->first_wx = w_current->first_wy = -1;
  w_current->second_wx = w_current->second_wy = -1;
  w_current->third_wx = w_current->third_wy = -1;
  w_current->magnetic_wx = w_current->magnetic_wy = -1;
  w_current->rubber_visible = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;
  int x1, y1, x2, y2; /* screen coords */

#if NET_DEBUG /* debug */
  char *tempstring;
  GdkFont *font;
#endif

  if (o_current == NULL) {
    return;
  }

  if (o_current->line == NULL) {
    return;
  }

  /* reuse line's routine */
  if ( (toplevel->DONT_REDRAW == 1) ||
       (!o_line_visible(toplevel, o_current->line, &x1, &y1, &x2, &y2)) ) {
    return;
  }

#if DEBUG
  printf("drawing net\n\n");
#endif

  size = 1;

  if (toplevel->net_style == THICK ) {
    size = SCREENabs(toplevel, NET_WIDTH);

    if (size < 1)
      size=1;
  }

  cairo_set_line_width (w_current->cr, size);
  cairo_set_line_cap (w_current->cr, CAIRO_LINE_CAP_SQUARE);

  if (toplevel->override_color != -1 ) {
    gschem_cairo_set_source_color (w_current->cr, x_color_lookup (toplevel->override_color));
  } else {
    gschem_cairo_set_source_color (w_current->cr, x_color_lookup (o_current->color));
  }

  gschem_cairo_line (w_current->cr, END_SQUARE, size, x1, y1, x2, y2);
  cairo_stroke (w_current->cr);

#if DEBUG 
  printf("drew net\n\n");
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
void o_net_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;
  int color;
  int sx[2], sy[2];

  if (o_current->line == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));

  if (toplevel->net_style == THICK ) {
    size = SCREENabs(toplevel, NET_WIDTH);
    gdk_gc_set_line_attributes(w_current->outline_xor_gc, size+1,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  WORLDtoSCREEN(toplevel, o_current->line->x[0] + dx, o_current->line->y[0] + dy, &sx[0], &sy[0]);
  WORLDtoSCREEN(toplevel, o_current->line->x[1] + dx, o_current->line->y[1] + dy, &sx[1], &sy[1]);

  gdk_draw_line (w_current->drawable, w_current->outline_xor_gc,
                 sx[0], sy[0], sx[1], sy[1]);

  if (toplevel->net_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->outline_xor_gc, 0,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_draw_xor_single(GSCHEM_TOPLEVEL *w_current, int dx, int dy, int whichone,
			   OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int color;
  int dx1 = -1, dx2 = -1, dy1 = -1,dy2 = -1;
  int sx[2], sy[2];

  if (o_current->line == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));

  if (whichone == 0) {
    dx1 = dx;
    dy1 = dy;
    dx2 = 0;
    dy2 = 0;
  } else if (whichone == 1) {
    dx2 = dx;
    dy2 = dy;
    dx1 = 0;
    dy1 = 0;
  } else {
    fprintf(stderr, _("Got an invalid which one in o_net_draw_xor_single\n"));
  }

  WORLDtoSCREEN( toplevel, o_current->line->x[0] + dx1, o_current->line->y[0] + dy1, &sx[0], &sy[0] );
  WORLDtoSCREEN( toplevel, o_current->line->x[1] + dx2, o_current->line->y[1] + dy2, &sx[1], &sy[1] );

  gdk_draw_line (w_current->drawable, w_current->outline_xor_gc,
                 sx[0], sy[0], sx[1], sy[1]);
}


/*! \brief guess the best direction for the next net drawing action
 *  \par Function Description
 *  This function checks all connectable objects at a starting point.
 *  It determines the best drawing direction for each quadrant of the
 *  possible net endpoint.
 *  
 *  The directions are stored in the GSCHEM_TOPLEVEL->net_direction variable
 *  as a bitfield.
 */
void o_net_guess_direction(GSCHEM_TOPLEVEL *w_current,
			   int wx, int wy)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int up=0, down=0, left=0, right=0;
  int x1, y1, x2, y2;
  int xmin, ymin, xmax, ymax;
  int orientation;
  GList *objectlists, *iter1, *iter2;
  OBJECT *o_current;

  int *current_rules;
  /* badness values       {OVERWRITE, ORTHO, CONTINUE} */
  const int pin_rules[] = {100, 50, 0};
  const int bus_rules[] = {90, 0, 40};
  const int net_rules[] = {80, 30, 0};
  
  objectlists = s_tile_get_objectlists(toplevel, wx, wy, wx, wy);

  for (iter1 = objectlists; iter1 != NULL; iter1 = g_list_next(iter1)) {
    for (iter2 = (GList*) iter1->data; iter2 != NULL; iter2 = g_list_next(iter2)) {
      o_current = (OBJECT*) iter2->data;

      if ((orientation = o_net_orientation(o_current)) == NEITHER)
	continue;

      switch (o_current->type) {
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
	g_assert_not_reached ();
      }
      
      x1 = o_current->line->x[0];
      x2 = o_current->line->x[1];
      y1 = o_current->line->y[0];
      y2 = o_current->line->y[1];
      
      xmin = min(x1, x2);
      ymin = min(y1, y2);
      xmax = max(x1, x2);
      ymax = max(y1, y2);
      
      if (orientation == HORIZONTAL && wy == y1) {
	if (wx == xmin) {
	  up = max(up, current_rules[1]);
	  down = max(down, current_rules[1]);
	  right = max(right, current_rules[0]);
	  left = max(left, current_rules[2]);
	}
	else if (wx == xmax) {
	  up = max(up, current_rules[1]);
	  down = max(down, current_rules[1]);
	  right = max(right, current_rules[2]);
	  left = max(left, current_rules[0]);
	}
	else if (xmin < wx && wx < xmax) {
	  up = max(up, current_rules[1]);
	  down = max(down, current_rules[1]);
	  right = max(right, current_rules[0]);
	  left = max(left, current_rules[0]);
	}
	else {
	  continue;
	}
      }
      if (orientation == VERTICAL && wx == x1) {
	if (wy == ymin) {
	  up = max(up, current_rules[0]);
	  down = max(down, current_rules[2]);
	  right = max(right, current_rules[1]);
	  left = max(left, current_rules[1]);
	}
	else if (wy == ymax) {
	  up = max(up, current_rules[2]);
	  down = max(down, current_rules[0]);
	  right = max(right, current_rules[1]);
	  left = max(left, current_rules[1]);
	}
	else if (ymin < wy && wy < ymax) {
	  up = max(up, current_rules[0]);
	  down = max(down, current_rules[0]);
	  right = max(right, current_rules[1]);
	  left = max(left, current_rules[1]);
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
  g_list_free(objectlists);
}

/*! \brief find the closest possible location to connect to
 *  \par Function Description
 *  This function calculates the distance to all connectable objects 
 *  and searches the closest connection point.
 *  It searches for pins, nets and busses.
 *  
 *  The connection point is stored in GSCHEM_TOPLEVEL->magnetic_wx and
 *  GSCHEM_TOPLEVEL->magnetic_wy. If no connection is found. Both variables
 *  are set to -1.
 */
void o_net_find_magnetic(GSCHEM_TOPLEVEL *w_current,
			 int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, x2, y1, y2, min_x, min_y, w_magnetic_reach;
  double mindist, minbest, dist1, dist2;
  double weight, min_weight;
  int magnetic_reach = 0;
  OBJECT *o_current;
  OBJECT *o_magnetic = NULL;
  GList *objectlists, *iter1, *iter2;

  minbest = min_x = min_y = 0;
  min_weight = 0;

  /* max distance of all the different reaches */
  magnetic_reach = max(MAGNETIC_PIN_REACH, MAGNETIC_NET_REACH);
  magnetic_reach = max(magnetic_reach, MAGNETIC_BUS_REACH);
  w_magnetic_reach = WORLDabs(toplevel, magnetic_reach);

  /* get the objects of the tiles around the reach region */
  x1 = w_x - w_magnetic_reach;
  y1 = w_y - w_magnetic_reach;
  x2 = w_x + w_magnetic_reach;
  y2 = w_y + w_magnetic_reach;
  objectlists = s_tile_get_objectlists(toplevel, x1, y1, x2, y2);

  for (iter1 = objectlists; iter1 != NULL; iter1 = g_list_next(iter1)) {
    for (iter2 = (GList*) iter1->data; iter2 != NULL; iter2 = g_list_next(iter2)) {
      o_current = (OBJECT*) iter2->data;

      if (!visible(toplevel,  o_current->w_left, o_current->w_top, 
		   o_current->w_right, o_current->w_bottom))
	continue; /* skip invisible objects */

      if (o_current->type == OBJ_PIN) {
	min_x = o_current->line->x[o_current->whichend];
	min_y = o_current->line->y[o_current->whichend];

	mindist = sqrt((double) (w_x - min_x)*(w_x - min_x)
		       + (double) (w_y - min_y)*(w_y - min_y));
	weight = mindist / MAGNETIC_PIN_WEIGHT;
      }

      else if (o_current->type == OBJ_NET
	       || o_current->type == OBJ_BUS) {
	/* we have 3 possible points to connect: 
	   2 endpoints and 1 midpoint point */
	x1 = o_current->line->x[0];
	y1 = o_current->line->y[0];
	x2 = o_current->line->x[1];
	y2 = o_current->line->y[1];
	/* endpoint tests */
	dist1 = sqrt((double) (w_x - x1)*(w_x - x1)
		     + (double) (w_y - y1)*(w_y - y1));
	dist2 = sqrt((double) (w_x - x2)*(w_x - x2)
		     + (double) (w_y - y2)*(w_y - y2));
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

	if (o_current->type == OBJ_BUS)
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
    switch (o_magnetic->type) {
    case (OBJ_PIN): magnetic_reach = MAGNETIC_PIN_REACH; break;
    case (OBJ_NET): magnetic_reach = MAGNETIC_NET_REACH; break;
    case (OBJ_BUS): magnetic_reach = MAGNETIC_BUS_REACH; break;
    }
    if (minbest > WORLDabs(toplevel, magnetic_reach)) {
      w_current->magnetic_wx = -1;
      w_current->magnetic_wy = -1;
    }
  }
  else {
    w_current->magnetic_wx = -1;
    w_current->magnetic_wy = -1;
  }

  g_list_free(objectlists);
}

/*! \brief calcutates the net route to the magnetic marker
 *  \par Function Description
 *  Depending on the two rubbernet lines from start to last and from
 *  last to second, the 3 coordinates are manipulated to find
 *  a way to the magnetic marker.
 */
void o_net_finishmagnetic(GSCHEM_TOPLEVEL *w_current)
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
void o_net_start_magnetic(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
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
  w_current->inside_action = 1;
}

/*! \brief set the start point of a new net
 *  \par Function Description
 *  This function sets the start point of a new net at the position of the 
 *  cursor. If we have a visible magnetic marker, we use that instead of 
 *  the cursor position
 */
void o_net_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;

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

  if (w_current->first_wx != snap_grid(toplevel, w_current->first_wx)
      || w_current->first_wy != snap_grid(toplevel, w_current->first_wy))
      s_log_message(_("Warning: Starting net at off grid coordinate\n"));

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
 * net is added to the TOPLEVEL structure.  
 *
 * The function returns TRUE if it has drawn a net, FALSE otherwise.
 */
int o_net_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int color;
  int primary_zero_length, secondary_zero_length;
  int found_primary_connection = FALSE;
  int save_wx, save_wy;

  GList *prev_conn_objects = NULL;
  OBJECT *new_net = NULL;

  g_assert( w_current->inside_action != 0 );

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
    return FALSE;
  }

  save_wx = w_current->third_wx;
  save_wy = w_current->third_wy;

  if (toplevel->override_net_color == -1) {
    color = NET_COLOR;
  } else {
    color = toplevel->override_net_color;
  }

  if (w_current->third_wx != snap_grid(toplevel, w_current->third_wx)
      || w_current->third_wy != snap_grid(toplevel, w_current->third_wy))
      s_log_message(_("Warning: Ending net at off grid coordinate\n"));

  if (!primary_zero_length ) {
  /* create primary net */
      new_net = o_net_new(toplevel, OBJ_NET, color,
                          w_current->first_wx, w_current->first_wy,
                          w_current->second_wx, w_current->second_wy);
      s_page_append (toplevel->page_current, new_net);

      /* conn stuff */
      /* LEAK CHECK 1 */
      prev_conn_objects = s_conn_return_others (prev_conn_objects, new_net);

      if (o_net_add_busrippers (w_current, new_net, prev_conn_objects)) {
        g_list_free (prev_conn_objects);
        prev_conn_objects = NULL;
        prev_conn_objects = s_conn_return_others (prev_conn_objects, new_net);
      }

#if DEBUG 
      printf("primary:\n"); 
      s_conn_print(new_net->conn_list);
#endif

      o_invalidate (w_current, new_net);

      o_invalidate_glist (w_current, prev_conn_objects);

      g_list_free (prev_conn_objects);
      prev_conn_objects = NULL;

      /* Go off and search for valid connection on this newly created net */
      found_primary_connection = s_conn_net_search(new_net, 1, 
                                                   new_net->conn_list);
      if (found_primary_connection)
      {
      	/* if a net connection is found, reset start point of next net */
	save_wx = w_current->second_wx;
	save_wy = w_current->second_wy;
      }

      /* you don't want to consolidate nets which are drawn non-ortho */
      if (toplevel->net_consolidate == TRUE && !w_current->CONTROLKEY) {
        /* CAUTION: Object list will change when nets are consolidated, don't
         *          keep pointers to other objects than new_net after this. */
        o_net_consolidate_segments(toplevel, new_net);
      }
  }


  /* If the second net is not zero length, add it as well */
  /* Also, a valid net connection from the primary net was not found */
  if (!secondary_zero_length && !found_primary_connection) {
      
      /* Add secondary net */
      new_net = o_net_new(toplevel, OBJ_NET, color,
                          w_current->second_wx, w_current->second_wy,
                          w_current->third_wx, w_current->third_wy);
      s_page_append (toplevel->page_current, new_net);

      /* conn stuff */
      /* LEAK CHECK 2 */
      prev_conn_objects = s_conn_return_others (prev_conn_objects, new_net);

      if (o_net_add_busrippers (w_current, new_net, prev_conn_objects)) {
        g_list_free (prev_conn_objects);
        prev_conn_objects = NULL;
        prev_conn_objects = s_conn_return_others (prev_conn_objects, new_net);
      }
#if DEBUG
      s_conn_print(new_net->conn_list);
#endif

      o_invalidate (w_current, new_net);

      o_invalidate_glist (w_current, prev_conn_objects);

      g_list_free (prev_conn_objects);
      prev_conn_objects = NULL;

      /* you don't want to consolidate nets which are drawn non-ortho */
      if (toplevel->net_consolidate == TRUE && !w_current->CONTROLKEY) {
        /* CAUTION: Object list will change when nets are consolidated, don't
         *          keep pointers to other objects than new_net after this. */
        o_net_consolidate_segments(toplevel, new_net);
      }
  }

  toplevel->page_current->CHANGED = 1;
  w_current->first_wx = save_wx;
  w_current->first_wy = save_wy;
  o_undo_savestate(w_current, UNDO_ALL);

  return (TRUE);
}

/*! \brief erase and redraw the rubber lines when drawing a net
 *  \par Function Description
 *  This function draws the rubbernet lines when drawing a net.
 */
void o_net_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  int ortho, horizontal, quadrant;

  g_assert( w_current->inside_action != 0 );

  /* Orthognal mode enabled when Control Key is NOT pressed or
     if we are using magnetic mode */
  ortho = !w_current->CONTROLKEY || w_current->magneticnet_mode;

  if (w_current->rubber_visible)
    o_net_invalidate_rubber (w_current);

  if (w_current->magneticnet_mode) {
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
void o_net_draw_rubber(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size=0, magnetic_halfsize;
  int magnetic_x, magnetic_y;
  int first_x, first_y, third_x, third_y, second_x, second_y;

  WORLDtoSCREEN(toplevel, w_current->magnetic_wx, w_current->magnetic_wy,
		&magnetic_x, &magnetic_y);
  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy,
		&first_x, &first_y);
  WORLDtoSCREEN(toplevel, w_current->third_wx, w_current->third_wy,
		&third_x, &third_y);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy,
		&second_x, &second_y);

  if (toplevel->net_style == THICK) {
    size = SCREENabs(toplevel, NET_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
			       GDK_LINE_SOLID,
			       GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
  }
  size = max(size, 0);

  gdk_gc_set_foreground (w_current->xor_gc, x_get_darkcolor (SELECT_COLOR));

  if (w_current->magneticnet_mode) {
    if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {
      magnetic_halfsize = max(4*size, MAGNETIC_HALFSIZE);
      gdk_draw_arc (w_current->drawable, w_current->xor_gc, FALSE,
                    magnetic_x - magnetic_halfsize,
                    magnetic_y - magnetic_halfsize,
                    2 * magnetic_halfsize, 2 * magnetic_halfsize,
                    0, FULL_CIRCLE);
    }
  }

  /* draw primary line */
  gdk_draw_line (w_current->drawable, w_current->xor_gc,
                 first_x, first_y, second_x, second_y);

  /* Draw secondary line */
  gdk_draw_line (w_current->drawable, w_current->xor_gc,
                 second_x, second_y, third_x, third_y);

  if (toplevel->net_style == THICK) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
			       GDK_LINE_SOLID,
			       GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size = 0, magnetic_halfsize;
  int bloat;
  int magnetic_x, magnetic_y;
  int first_x, first_y, third_x, third_y, second_x, second_y;
  int x1, y1, x2, y2;

  WORLDtoSCREEN (toplevel, w_current->magnetic_wx, w_current->magnetic_wy,
                 &magnetic_x, &magnetic_y);
  WORLDtoSCREEN (toplevel, w_current->first_wx, w_current->first_wy,
                 &first_x, &first_y);
  WORLDtoSCREEN (toplevel, w_current->third_wx, w_current->third_wy,
                 &third_x, &third_y);
  WORLDtoSCREEN (toplevel, w_current->second_wx, w_current->second_wy,
                 &second_x, &second_y);

  if (toplevel->net_style == THICK) {
    size = SCREENabs (toplevel, NET_WIDTH);
  }
  size = max (size, 0);
  bloat = size / 2;

  if (w_current->magneticnet_mode) {
    if (w_current->magnetic_wx != -1 && w_current->magnetic_wy != -1) {
      magnetic_halfsize = max (4 * size, MAGNETIC_HALFSIZE);

      o_invalidate_rect (w_current, magnetic_x - magnetic_halfsize,
                                    magnetic_y - magnetic_halfsize,
                                    magnetic_x + magnetic_halfsize,
                                    magnetic_y + magnetic_halfsize);
    }
  }

  x1 = min (first_x, second_x) - bloat;
  x2 = max (first_x, second_x) + bloat;
  y1 = min (first_y, second_y) - bloat;
  y2 = max (first_y, second_y) + bloat;
  o_invalidate_rect (w_current, x1, y1, x2, y2);

  x1 = min (second_x, third_x) - bloat;
  x2 = max (second_x, third_x) + bloat;
  y1 = min (second_y, third_y) - bloat;
  y2 = max (second_y, third_y) + bloat;
  o_invalidate_rect (w_current, x1, y1, x2, y2);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_net_add_busrippers(GSCHEM_TOPLEVEL *w_current, OBJECT *net_obj,
                         GList *prev_conn_objects)

{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;
  int color;
  GList *cl_current = NULL;
  OBJECT *bus_object = NULL;
  CONN *found_conn = NULL;
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
  int complex_angle = 0;
  const CLibSymbol *rippersym = NULL;
  
  length = o_line_length(net_obj);

  if (!prev_conn_objects) {
    return(FALSE);
  }
  
  if (length <= ripper_size) {
    return(FALSE);
  }

  if (toplevel->override_net_color == -1) {
    color = NET_COLOR;
  } else {
    color = toplevel->override_net_color;
  }

  
  /* check for a bus connection and draw rippers if so */
  cl_current = prev_conn_objects;
  while (cl_current != NULL) {
    
    bus_object = (OBJECT *) cl_current->data;
    if (bus_object && bus_object->type == OBJ_BUS) {
      /* yes, using the net routine is okay */
      int bus_orientation = o_net_orientation(bus_object);
      int net_orientation = o_net_orientation(net_obj);

      /* find the CONN structure which is associated with this object */
      GList *cl_current2 = net_obj->conn_list;
      done = FALSE;
      while (cl_current2 != NULL && !done) {
	CONN *tmp_conn = (CONN *) cl_current2->data;

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

        sign = bus_object->bus_ripper_direction;
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
          bus_object->bus_ripper_direction = sign;
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
              complex_angle = 0;
            } else {
              complex_angle = 90;
            }
          } else {
            /* symmetric */
            complex_angle = 0;
          }

          net_obj->line->y[found_conn->whichone] -= ripper_size;
          o_recalc_single_object(toplevel, net_obj);
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
              complex_angle = 270;
            } else {
              complex_angle = 180;
            }
          } else {
            /* symmetric */
            complex_angle = 180;
          }
          
          net_obj->line->y[found_conn->whichone] += ripper_size;
          o_recalc_single_object(toplevel, net_obj);
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

        sign = bus_object->bus_ripper_direction;
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
          bus_object->bus_ripper_direction = sign;
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
              complex_angle = 0;
            } else {
              complex_angle = 270;
            }
          } else {
            /* symmetric */
            complex_angle = 270;
          }

          net_obj->line->x[found_conn->whichone] -= ripper_size;
          o_recalc_single_object(toplevel, net_obj);
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
              complex_angle = 90;
            } else {
              complex_angle = 180;
            }
          } else {
            /* symmetric */
            complex_angle = 90;
          }

          net_obj->line->x[found_conn->whichone] += ripper_size;
          o_recalc_single_object(toplevel, net_obj);
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
    s_conn_remove_object (toplevel, net_obj);

    if (w_current->bus_ripper_type == COMP_BUS_RIPPER) {
      GList *symlist = 
	s_clib_search (toplevel->bus_ripper_symname, CLIB_EXACT);
      if (symlist != NULL) {
        rippersym = (CLibSymbol *) symlist->data;
      }
      g_list_free (symlist);
    }
    
    for (i = 0; i < ripper_count; i++) {
      if (w_current->bus_ripper_type == NET_BUS_RIPPER) {
        new_obj = o_net_new(toplevel, OBJ_NET, color,
                  rippers[i].x[0], rippers[i].y[0],
                  rippers[i].x[1], rippers[i].y[1]);
        s_page_append (toplevel->page_current, new_obj);
      } else {

        if (rippersym != NULL) {
          new_obj = o_complex_new (toplevel, OBJ_COMPLEX, DEFAULT_COLOR,
                                   rippers[i].x[0], rippers[i].y[0],
                                   complex_angle, 0,
                                   rippersym,
                                   toplevel->bus_ripper_symname, 1);
          s_page_append (toplevel->page_current, new_obj);
          o_complex_promote_attribs (toplevel, new_obj,
                                     &toplevel->page_current->object_list);

          o_invalidate (w_current, new_obj);
        } else {
          s_log_message(_("Bus ripper symbol [%s] was not found in any component library\n"),
                        toplevel->bus_ripper_symname);
        }
      }
    }
    
    s_conn_update_object (toplevel, net_obj);
    return(TRUE);
  }

  return(FALSE);
}
