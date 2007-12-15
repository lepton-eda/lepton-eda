/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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

  if (toplevel->net_style == THICK ) {
    size = SCREENabs(toplevel, NET_WIDTH);

    if (size < 0)
      size=0;

    gdk_gc_set_line_attributes(w_current->gc, size, 
                               GDK_LINE_SOLID,
                               GDK_CAP_BUTT,
                               GDK_JOIN_MITER);

    gdk_gc_set_line_attributes(w_current->bus_gc, size, 
                               GDK_LINE_SOLID,
                               GDK_CAP_BUTT,
                               GDK_JOIN_MITER);
  }

  if (toplevel->override_color != -1 ) {

    gdk_gc_set_foreground(w_current->gc,
                          x_get_color(toplevel->override_color));

    gdk_draw_line(w_current->backingstore, w_current->gc,
                  x1, y1, x2, y2);
  } else {

    gdk_gc_set_foreground(w_current->gc,
                          x_get_color(o_current->color));
    gdk_draw_line(w_current->backingstore, w_current->gc,
                  x1, y1, x2, y2);

#if NET_DEBUG
    /* temp debug only */
    font = gdk_fontset_load ("10x20");
    tempstring = g_strdup_printf("%s", o_current->name);
    gdk_draw_text (w_current->backingstore,
                   font,
                   w_current->gc,
                   x1+20, y1+20,
                   tempstring,
                   strlen(tempstring));
    gdk_font_unref(font);
    g_free(tempstring);
#endif
  }

#if DEBUG 
  printf("drew net\n\n");
#endif

  /* yes zero is right for the width -> use hardware lines */
  if (toplevel->net_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->gc, 0, 
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);

    gdk_gc_set_line_attributes(w_current->bus_gc, 0, 
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  if (o_current->draw_grips && w_current->draw_grips == TRUE) {	

	  /* pb20011109 - modified to use the new o_line_[draw|erase]_grips() */
	  /*              reuse the line functions */
	  if (!o_current->selected) {
		  /* object is no more selected, erase the grips */
		  o_current->draw_grips = FALSE;
		  o_line_erase_grips(w_current, o_current);
	  } else {
		  /* object is selected, draw the grips */
		  o_line_draw_grips(w_current, o_current);
	  }

  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_erase(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  toplevel->override_color = toplevel->background_color;
  o_net_draw(w_current, o_current);
  toplevel->override_color = -1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
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

  WORLDtoSCREEN( toplevel, o_current->line->x[0], o_current->line->y[0], &sx[0], &sy[0] );
  WORLDtoSCREEN( toplevel, o_current->line->x[1], o_current->line->y[1], &sx[1], &sy[1] );

  gdk_draw_line(w_current->backingstore, w_current->outline_xor_gc,
                sx[0]+dx, sy[0]+dy,
                sx[1]+dx, sy[1]+dy);

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

  WORLDtoSCREEN( toplevel, o_current->line->x[0], o_current->line->y[0], &sx[0], &sy[0] );
  WORLDtoSCREEN( toplevel, o_current->line->x[1], o_current->line->y[1], &sx[1], &sy[1] );

  gdk_draw_line(w_current->backingstore, w_current->outline_xor_gc,
                sx[0] + dx1, sy[0] + dy1, sx[1] + dx2, sy[1] + dy2);
  o_invalidate_rect(w_current,
                    sx[0] + dx1, sy[0] + dy1, sx[1] + dx2, sy[1] + dy2);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_start(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;

  /* initalize all parameters used when drawing the new net */
  w_current->last_x = w_current->start_x = w_current->second_x = 
    fix_x(toplevel, x);
  w_current->last_y = w_current->start_y = w_current->second_y = 
    fix_y(toplevel, y);

  if (toplevel->net_style == THICK ) {
    size = SCREENabs(toplevel, NET_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                               w_current->last_x, w_current->last_y);
  if (toplevel->net_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
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
int o_net_end(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1;
  int x2, y2;
  int x3, y3;
  int color;
  int size;
  int primary_zero_length, secondary_zero_length;
  int found_primary_connection = FALSE;
  int sx[2], sy[2];

  /*int temp_x, temp_y;*/
  /* OBJECT *o_current;*/
  GList *other_objects = NULL;
  OBJECT *new_net = NULL;

  g_assert( w_current->inside_action != 0 );

  if (toplevel->override_net_color == -1) {
    color = w_current->net_color;
  } else {
    color = toplevel->override_net_color;
  }

  size = SCREENabs(toplevel, NET_WIDTH);

  if (toplevel->net_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color) );

  /* Erase primary rubber net line */
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->start_x, w_current->start_y,
		w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                               w_current->last_x, w_current->last_y);


  /* Erase secondary rubber net line */
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		 w_current->last_x, w_current->last_y,
		 w_current->second_x, w_current->second_y);
  o_invalidate_rect(w_current, w_current->last_x, w_current->last_y,
                               w_current->second_x, w_current->second_y);

  if (toplevel->net_style == THICK) {
    gdk_gc_set_line_attributes(w_current->xor_gc, 0,
			       GDK_LINE_SOLID,
			       GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
    gdk_gc_set_line_attributes(w_current->gc, size,
			       GDK_LINE_SOLID,
			       GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
  }


  /* See if either of the nets are zero length.  We'll only add */
  /* the non-zero ones */
  primary_zero_length = (w_current->start_x == w_current->last_x) &&
      (w_current->start_y == w_current->last_y);
 
  secondary_zero_length = (w_current->last_x == w_current->second_x) &&
      (w_current->last_y == w_current->second_y);

  /* If both nets are zero length... */
  /* this ends the net drawing behavior we want this? hack */
  if ( primary_zero_length && secondary_zero_length ) {
    w_current->start_x = (-1);
    w_current->start_y = (-1);
    w_current->last_x = (-1);
    w_current->last_y = (-1);
    w_current->second_x = (-1);
    w_current->second_y = (-1);
    w_current->inside_action = 0;
    i_set_state(w_current, STARTDRAWNET);
    o_net_eraserubber(w_current);
    
    return (FALSE);
  }

  /* Primary net runs from (x1,y1)-(x2,y2) */
  /* Secondary net from (x2,y2)-(x3,y3) */
  SCREENtoWORLD(toplevel, w_current->start_x, w_current->start_y, &x1,	&y1);
  SCREENtoWORLD(toplevel, w_current->last_x, w_current->last_y, &x2, &y2);
  SCREENtoWORLD(toplevel, w_current->second_x, w_current->second_y, &x3, &y3);

  /* Snap points to closest grid location */
  x1 = snap_grid(toplevel, x1);
  y1 = snap_grid(toplevel, y1);
  x2 = snap_grid(toplevel, x2);
  y2 = snap_grid(toplevel, y2);
  x3 = snap_grid(toplevel, x3);
  y3 = snap_grid(toplevel, y3);

  w_current->save_x = w_current->second_x;
  w_current->save_y = w_current->second_y;

  if (!primary_zero_length ) {
  /* create primary net */
      toplevel->page_current->object_tail =
	  new_net = o_net_add(toplevel,
			      toplevel->page_current->object_tail,
			      OBJ_NET, color, x1, y1, x2, y2);
  
      /* conn stuff */
      /* LEAK CHECK 1 */
      other_objects = s_conn_return_others(other_objects,
					   toplevel->page_current->
					   object_tail);

      if (o_net_add_busrippers(w_current, new_net, other_objects)) {
	  g_list_free(other_objects);
	  other_objects = NULL;
	  other_objects = s_conn_return_others(other_objects, new_net);
      }

#if DEBUG 
      printf("primary:\n"); 
      s_conn_print(new_net->conn_list);
#endif
  
      WORLDtoSCREEN( toplevel, new_net->line->x[0], new_net->line->y[0], &sx[0], &sy[0] );
      WORLDtoSCREEN( toplevel, new_net->line->x[1], new_net->line->y[1], &sx[1], &sy[1] );

      gdk_gc_set_foreground(w_current->gc, x_get_color(color));
      gdk_draw_line(w_current->backingstore, w_current->gc,
                    sx[0], sy[0], sx[1], sy[1]);
      o_invalidate_rect(w_current, sx[0], sy[0], sx[1], sy[1]);

      if (toplevel->net_style == THICK) {
	  gdk_gc_set_line_attributes(w_current->gc, 0,
				     GDK_LINE_SOLID,
				     GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
      }

      o_cue_undraw_list(w_current, other_objects);
      o_cue_draw_list(w_current, other_objects);
      o_cue_draw_single(w_current, new_net);

      /* Go off and search for valid connection on this newly created net */
      found_primary_connection = s_conn_net_search(new_net, 1, 
                                                   new_net->conn_list);
      if (found_primary_connection)
      {
      	/* if a net connection is found, reset start point of next net */
	w_current->save_x = w_current->last_x;
	w_current->save_y = w_current->last_y;
      }

      /* you don't want to consolidate nets which are drawn non-ortho */
      if (toplevel->net_consolidate == TRUE && !w_current->CONTROLKEY) {
	  o_net_consolidate_segments(toplevel, new_net);
      }
  }


  /* If the second net is not zero length, add it as well */
  /* Also, a valid net connection from the primary net was not found */
  if (!secondary_zero_length && !found_primary_connection) {
      
      /* Add secondary net */
      toplevel->page_current->object_tail =
	  new_net = o_net_add(toplevel,
			      toplevel->page_current->object_tail,
			      OBJ_NET, color, x2, y2, x3, y3);
  
      /* conn stuff */
      /* LEAK CHECK 2 */
      other_objects = s_conn_return_others(other_objects,
					   toplevel->page_current->
					   object_tail);

      if (o_net_add_busrippers(w_current, new_net, other_objects)) {
	  g_list_free(other_objects);
	  other_objects = NULL;
	  other_objects = s_conn_return_others(other_objects, new_net);
      }
#if DEBUG
      s_conn_print(new_net->conn_list);
#endif

      WORLDtoSCREEN( toplevel, new_net->line->x[0], new_net->line->y[0], &sx[0], &sy[0] );
      WORLDtoSCREEN( toplevel, new_net->line->x[1], new_net->line->y[1], &sx[1], &sy[1] );

      gdk_gc_set_foreground(w_current->gc, x_get_color(color));
      gdk_draw_line(w_current->backingstore, w_current->gc,
                    sx[0], sy[0], sx[1], sy[1]);
      o_invalidate_rect(w_current, sx[0], sy[0], sx[1], sy[1]);
      
      if (toplevel->net_style == THICK) {
	  gdk_gc_set_line_attributes(w_current->gc, 0,
				     GDK_LINE_SOLID,
				     GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
      }

      o_cue_undraw_list(w_current, other_objects);
      o_cue_draw_list(w_current, other_objects);
      o_cue_draw_single(w_current, new_net);

      /* you don't want to consolidate nets which are drawn non-ortho */
      if (toplevel->net_consolidate == TRUE && !w_current->CONTROLKEY) {
	  o_net_consolidate_segments(toplevel, new_net);
      }
  }
  
  /* LEAK CHECK 3 */
  g_list_free(other_objects);

  toplevel->page_current->CHANGED = 1;
  w_current->start_x = w_current->save_x;
  w_current->start_y = w_current->save_y;
  o_undo_savestate(w_current, UNDO_ALL);

  return (TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_net_rubbernet(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;
  int size;
  int ortho;

  g_assert( w_current->inside_action != 0 );

  if (toplevel->net_style == THICK) {
    size = SCREENabs(toplevel, NET_WIDTH);
    gdk_gc_set_line_attributes(w_current->xor_gc, size,
			       GDK_LINE_SOLID,
			       GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
  }
  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color));

  /* Orthognal mode enabled when Control Key is NOT pressed */
  ortho = !w_current->CONTROLKEY;

  /* Erase primary line */
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->start_x, w_current->start_y,
		w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  /* Erase secondary line*/
  if ( w_current->second_x != -1 && w_current->second_y != -1 ) {
      gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		    w_current->last_x, w_current->last_y,
		    w_current->second_x, w_current->second_y);
    o_invalidate_rect(w_current, w_current->last_x, w_current->last_y,
                      w_current->second_x, w_current->second_y);
  }
 
  /* In orthogonal mode secondary line is the same as the first */
  if (!ortho)
  {
      w_current->second_x = w_current->last_x;
      w_current->second_y = w_current->last_y;
  }

  w_current->last_x = fix_x(toplevel, x);
  w_current->last_y = fix_y(toplevel, y);

  /* If you press the control key then you can draw non-ortho nets */
  if (ortho) {
    diff_x = abs(w_current->last_x - w_current->start_x);
    diff_y = abs(w_current->last_y - w_current->start_y);

    /* calculate the co-ordinates necessary to draw the lines*/
    /* Pressing the shift key will cause the vertical and horizontal lines to switch places */
    if ( !w_current->SHIFTKEY ) {
      w_current->last_y = w_current->start_y;
      w_current->second_x = w_current->last_x;
      w_current->second_y = fix_y(toplevel,y);
    } else {
      w_current->last_x = w_current->start_x;
      w_current->second_x = fix_x(toplevel,x);
      w_current->second_y = w_current->last_y;
    }
  }

  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color));
  
  /* draw primary line */
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->start_x, w_current->start_y,
		w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                    w_current->last_x, w_current->last_y);

  /* Draw secondary line */
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->last_x, w_current->last_y,
		w_current->second_x, w_current->second_y);
  o_invalidate_rect(w_current, w_current->last_x, w_current->last_y,
                    w_current->second_x, w_current->second_y);

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
 *  \note
 *  used in button cancel code in x_events.c
 */
void o_net_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;

  if (toplevel->net_style == THICK) {
    size = SCREENabs(toplevel, NET_WIDTH);

    if (size < 0)
      size = 0;

    gdk_gc_set_line_attributes(w_current->xor_gc, size,
			       GDK_LINE_SOLID,
			       GDK_CAP_NOT_LAST, GDK_JOIN_MITER);
  }

  /* Erase primary primary rubber net line */
  gdk_draw_line(w_current->backingstore, w_current->xor_gc, w_current->start_x,
		w_current->start_y, w_current->last_x, w_current->last_y);
  o_invalidate_rect(w_current, w_current->start_x, w_current->start_y,
                               w_current->last_x, w_current->last_y);

  /* Erase secondary rubber net line */
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->last_x, w_current->last_y,
		w_current->second_x, w_current->second_y);
  o_invalidate_rect(w_current, w_current->last_x, w_current->last_y,
                               w_current->second_x, w_current->second_y);

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
 *  \note
 *  used in x_event_expose() in x_events.c
 */
void o_net_xorrubber(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int size;

  if (toplevel->net_style == THICK ) {

    size = SCREENabs(toplevel, NET_WIDTH);

    if (size < 0)
      size=0;

    gdk_gc_set_line_attributes(w_current->gc, size,
                               GDK_LINE_SOLID,
                               GDK_CAP_NOT_LAST,
                               GDK_JOIN_MITER);
  }

  gdk_gc_set_foreground(w_current->gc,
			x_get_darkcolor(w_current->select_color) );
  gdk_draw_line(w_current->window, w_current->gc, 
		w_current->start_x, w_current->start_y, 
		w_current->last_x, w_current->last_y);
  gdk_draw_line(w_current->window, w_current->gc, 
		w_current->second_x, w_current->second_y, 
		w_current->last_x, w_current->last_y);

  if (toplevel->net_style == THICK ) {
    gdk_gc_set_line_attributes(w_current->gc, 0,
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
int o_net_add_busrippers(GSCHEM_TOPLEVEL *w_current, OBJECT *net_obj,
			 GList *other_objects)

{
  TOPLEVEL *toplevel = w_current->toplevel;
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

  if (!other_objects) {
    return(FALSE);
  }
  
  if (length <= ripper_size) {
    return(FALSE);
  }

  if (toplevel->override_net_color == -1) {
    color = w_current->net_color;
  } else {
    color = toplevel->override_net_color;
  }

  
  /* check for a bus connection and draw rippers if so */
  cl_current = other_objects;
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
          o_net_recalc(toplevel, net_obj);
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
          o_net_recalc(toplevel, net_obj);
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
          o_net_recalc(toplevel, net_obj);
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
          o_net_recalc(toplevel, net_obj);
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
    s_conn_remove(toplevel, net_obj);

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
        toplevel->page_current->object_tail =
          o_net_add(toplevel, toplevel->page_current->object_tail,
                    OBJ_NET, color,
                    rippers[i].x[0], rippers[i].y[0],
                    rippers[i].x[1], rippers[i].y[1]);
      } else {

        if (rippersym != NULL) {
          toplevel->page_current->object_tail =
          (OBJECT *) o_complex_add(
                                   toplevel,
                                   toplevel->page_current->object_tail,
				   NULL,
                                   OBJ_COMPLEX, WHITE,
                                   rippers[i].x[0], rippers[i].y[0],
                                   complex_angle, 0,
                                   rippersym,
                                   toplevel->bus_ripper_symname, 1, TRUE);
          
          o_complex_draw(w_current,toplevel->page_current->object_tail);
        } else {
          s_log_message(_("Bus ripper symbol [%s] was not found in any component library\n"),
                        toplevel->bus_ripper_symname);
        }
      }
    }
    
    s_conn_update_object(toplevel, net_obj);
    return(TRUE);
  }

  return(FALSE);
}
