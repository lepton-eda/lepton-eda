/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
 * Copyright (C) 1998 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>


#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/prototype.h>
#include <libgeda/colors.h>

#include "../include/x_states.h"
#include "../include/prototype.h"


void
o_net_ales_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
	int cue;

	gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->background_color));

	cue = o_ales_query_table(w_current->page_current->ales_table,
				o_current->line_points->x1,
				o_current->line_points->y1);


#if DEBUG	
	printf("efirst: %d\n",cue);
#endif
	switch(cue) {

		case(NET_DANGLING_CUE):
		case(NO_CUE):
			o_ales_draw_endpoint(w_current, w_current->gc,
                                    o_current->line_points->screen_x1,
                                    o_current->line_points->screen_y1);
		break;

		case(MIDPOINT_CUE):
			o_ales_draw_midpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x1,
                                 o_current->line_points->screen_y1);
		break;

		/* bus case */
	}

	cue = o_ales_query_table(w_current->page_current->ales_table,
				 o_current->line_points->x2,
				 o_current->line_points->y2);
#if DEBUG
	printf("esecond: %d\n",cue);
#endif
	switch(cue) {

		case(NET_DANGLING_CUE):
		case(NO_CUE):
			o_ales_draw_endpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x2,
                                 o_current->line_points->screen_y2);
		break;

		case(MIDPOINT_CUE):
			o_ales_draw_midpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x2,
                                 o_current->line_points->screen_y2);
		break;

		/* bus case */
	}
}

void
o_net_ales_erase_force(TOPLEVEL *w_current, OBJECT *o_current)
{
	int cue;

	gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->background_color));

	o_ales_draw_endpoint(w_current, w_current->gc,
       				o_current->line_points->screen_x1,
                                o_current->line_points->screen_y1);

	o_ales_draw_endpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x2,
                                 o_current->line_points->screen_y2);
}

void
o_net_ales_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int cue;

#if 0
	if (w_current->override_color != -1) {
		gdk_gc_set_foreground(w_current->gc,
			x_get_color(w_current->override_color));
	} else {
	}
#endif

	gdk_gc_set_foreground(w_current->gc,
		x_get_color(w_current->net_endpoint_color));

	cue = o_ales_query_table(w_current->page_current->ales_table,
				o_current->line_points->x1,
				o_current->line_points->y1);

#if DEBUG
	printf("dfirst: %d\n", cue);
#endif
	switch(cue) {

		case(NO_CUE):

		break;

		case(NET_DANGLING_CUE):
			o_ales_draw_endpoint(w_current, w_current->gc,
                                    o_current->line_points->screen_x1,
                                    o_current->line_points->screen_y1);
		break;

		case(MIDPOINT_CUE):
			o_ales_draw_midpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x1,
                                 o_current->line_points->screen_y1);
		break;

		/* bus case */
	}

	cue = o_ales_query_table(w_current->page_current->ales_table,
				 o_current->line_points->x2,
				 o_current->line_points->y2);

#if DEBUG
	printf("dsecond: %d\n", cue);
#endif
	switch(cue) {

		case(NO_CUE):

		break;

		case(NET_DANGLING_CUE):
			o_ales_draw_endpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x2,
                                 o_current->line_points->screen_y2);
		break;

		case(MIDPOINT_CUE):
			o_ales_draw_midpoint(w_current, w_current->gc,
                                 o_current->line_points->screen_x2,
                                 o_current->line_points->screen_y2);
		break;

		/* bus case */
	}
}

void
o_net_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int size;
	int cue;
	int x1, y1, x2, y2; /* screen coords */

	if (o_current == NULL) {
		return;
	}

	if (o_current->line_points == NULL) {
		return;
	}

	o_net_recalc(w_current, o_current);

	/* reuse line's routine */
        if (!o_line_visible(w_current, o_current->line_points, 
	     &x1, &y1, &x2, &y2)) {
                return;
        }


#if DEBUG 
	printf("drawing net\n\n");
#endif

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		size = SCREENabs(w_current, 10); 
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/

		if (size < 0) 
			size=0;

		gdk_gc_set_line_attributes(w_current->gc, size, GDK_LINE_SOLID,
				GDK_CAP_BUTT,
				GDK_CAP_NOT_LAST);
				/*GDK_CAP_PROJECTING,
				/GDK_JOIN_MITER);*/
	}  

	if (w_current->override_color != -1 ) {
	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->override_color));
	gdk_draw_line(w_current->window, w_current->gc, 
				       x1, y1, x2, y2);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
				       x1, y1, x2, y2);
	} else {
	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(o_current->color));
	gdk_draw_line(w_current->window, w_current->gc, 
				       x1, y1, x2, y2);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
				       x1, y1, x2, y2);
	}

        /* yes zero is right for the width -> use hardware lines */
	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}

	/* ALES stuff, not sure if I'm going to leave this here */
	/* only draw the connection points, if: */
	/* - you are drawing regular lines, */
	/* - you aren't redrawing selected (DONT_DRAW_CONN), */ 
	/* - And, you are erasing them */
       if ( ((w_current->override_color == -1) && 
	    (!w_current->DONT_DRAW_CONN)) ||
            (w_current->override_color == w_current->background_color ) ) {

		if (w_current->override_color != -1) {
                        gdk_gc_set_foreground(w_current->gc,
                                x_get_color(w_current->override_color));
                } else {
                        gdk_gc_set_foreground(w_current->gc,
                                x_get_color(w_current->net_endpoint_color));
                }

		cue = o_ales_query_table(w_current->page_current->ales_table,
					 o_current->line_points->x1,
					 o_current->line_points->y1);
		switch(cue) {

			case(NO_CUE):

			break;

			case(NET_DANGLING_CUE):
				o_ales_draw_endpoint(w_current, w_current->gc,
                                          o_current->line_points->screen_x1,
                                          o_current->line_points->screen_y1);
			break;

			case(MIDPOINT_CUE):
				o_ales_draw_midpoint(w_current, w_current->gc,
                                          o_current->line_points->screen_x1,
                                          o_current->line_points->screen_y1);
			break;

			/* bus case */
		}

		cue = o_ales_query_table(w_current->page_current->ales_table,
					 o_current->line_points->x2,
					 o_current->line_points->y2);
		switch(cue) {

			case(NO_CUE):

			break;

			case(NET_DANGLING_CUE):
				o_ales_draw_endpoint(w_current, w_current->gc,
                                          o_current->line_points->screen_x2,
                                          o_current->line_points->screen_y2);
			break;

			case(MIDPOINT_CUE):
				o_ales_draw_midpoint(w_current, w_current->gc,
                                          o_current->line_points->screen_x2,
                                          o_current->line_points->screen_y2);
			break;

			/* bus case */
		}
	}

#if DEBUG
	printf("drawing net\n");
#endif
				
}

void
o_net_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int size;
        if (o_current->line_points == NULL) {
                return;
        }

        gdk_gc_set_foreground(w_current->outline_xor_gc, 
			x_get_darkcolor(o_current->color));

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
                size = SCREENabs(w_current, 10);
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
		gdk_gc_set_line_attributes(w_current->outline_xor_gc, size+1,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}   	

        gdk_draw_line(w_current->window, w_current->outline_xor_gc,
                       o_current->line_points->screen_x1+dx,
                       o_current->line_points->screen_y1+dy,
                       o_current->line_points->screen_x2+dx,
                       o_current->line_points->screen_y2+dy);

	/* backing store ? not approriate here */

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->outline_xor_gc, 0,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
	}
}            


void
o_net_start(TOPLEVEL *w_current, int x, int y)
{
	int size;

	w_current->last_x = w_current->start_x = fix_x(w_current, x);
	w_current->last_y = w_current->start_y = fix_y(w_current, y);


#if 0 /* not ready for prime time use, this is the snap any point #if 0 */
	int distance1;
	int distance2;
	OBJECT *real;
	OBJECT *o_current;
	int temp_x, temp_y;
	o_current = o_CONN_search_closest_range(w_current, 
			w_current->page_current->object_head,
			w_current->start_x, w_current->start_y,	
			&temp_x, &temp_y, 200, NULL, NULL);

	if (o_current) {
		w_current->last_x = w_current->start_x = temp_x;
		w_current->last_y = w_current->start_y = temp_y;
	} else {
		w_current->last_x = w_current->start_x = fix_x(w_current, x);
		w_current->last_y = w_current->start_y = fix_y(w_current, y);
	}
#endif

#if 0 /* not ready for prime time use */
	/* new net extenstion stuff */
	o_current = w_current->page_current->selection_head->next;	
	if (o_current != NULL && w_current->event_state == STARTDRAWNET) {
		if (o_current->type == OBJ_NET) {
			if (o_current->line_points) {

			   real = o_list_search(
					w_current->page_current->object_head,	
					o_current);

			   if (!real) {
				fprintf(stderr, "selected a nonexistant object!\n");
				exit(-1);
			   }
			   distance1 = dist(
				      real->line_points->screen_x1,
				      real->line_points->screen_y1,
				      w_current->start_x, w_current->start_y);

			   distance2 = dist(
				      real->line_points->screen_x2,
				      real->line_points->screen_y2,
				      w_current->start_x, w_current->start_y);

			   printf("%d %d\n", distance1, distance2);

			   if (distance1 < distance2) {
			 	w_current->last_x = w_current->start_x = 
			 	   real->line_points->screen_x1;
				w_current->last_y = w_current->start_y = 
				   real->line_points->screen_y1;
			   } else {
				w_current->last_x = w_current->start_x = 
			  	   real->line_points->screen_x2;
				w_current->last_y = w_current->start_y = 
				   real->line_points->screen_y2;
			   }
			}
		} else if (o_current->type == OBJ_COMPLEX) {
			real = o_list_search(
				w_current->page_current->object_head,	
				o_current);

			if (!real) {
				fprintf(stderr, "selected a nonexistant object!\n");
				exit(-1);
			   }

			o_CONN_search_closest(w_current, o_current->complex, 
				w_current->start_x, w_current->start_y,	
				&temp_x, &temp_y, NULL);
			w_current->last_x = w_current->start_x = temp_x;
			w_current->last_y = w_current->start_y = temp_y;
		}

	}
#endif




	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
                size = SCREENabs(w_current, 10);
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
		gdk_gc_set_line_attributes(w_current->xor_gc, size,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}   

	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        } 
}

void
o_net_end(TOPLEVEL *w_current, int x, int y)
{
	int x1, y1;
	int x2, y2;
	int color;
	int size;
	/*int temp_x, temp_y;*/
	/* OBJECT *o_current;*/

	if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
                return;
        }

	if (w_current->override_net_color == -1) {
                color = w_current->net_color;
        } else {
                color = w_current->override_net_color;
        }          

	size = SCREENabs(w_current, 10);
	/*size = return_zoom_number(w_current->page_current->zoom_factor);*/

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, size,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
        } 

	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
		gdk_gc_set_line_attributes(w_current->gc, size,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
	}      


        /* don't allow zero length nets */
	/* this ends the net drawing behavior we want this? hack */
        if ( (w_current->start_x == w_current->last_x) && 
             (w_current->start_y == w_current->last_y) ) {
                w_current->start_x = (-1);
                w_current->start_y = (-1);
                w_current->last_x = (-1);
                w_current->last_y = (-1);
		w_current->inside_action=0;
                w_current->event_state = SELECT;
                i_update_status(w_current, "Select Mode");
		o_net_eraserubber(w_current);
                return;
        }

#if 0 /* not ready for prime time use */
	/* second attempt at all snapping */
	o_current = o_CONN_search_closest_range(w_current, 
			w_current->page_current->object_head,
			w_current->last_x, w_current->last_y,	
			&temp_x, &temp_y, 200, NULL, NULL);

	if (o_current) {
		w_current->last_x = temp_x;
		w_current->last_y = temp_y;
	} else {
		w_current->last_x = fix_x(w_current, x);
		w_current->last_y = fix_y(w_current, y);
	}
#endif

	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(color));
	gdk_draw_line(w_current->window, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	
	gdk_draw_line(w_current->backingstore, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        } 

	SCREENtoWORLD(w_current, w_current->start_x, w_current->start_y, &x1, &y1); 
	SCREENtoWORLD(w_current, w_current->last_x, w_current->last_y, &x2, &y2); 
	x1 = snap_grid(w_current, x1);
	y1 = snap_grid(w_current, y1);
	x2 = snap_grid(w_current, x2);
	y2 = snap_grid(w_current, y2);

	w_current->save_x = w_current->last_x;
	w_current->save_y = w_current->last_y;

	w_current->page_current->object_tail = o_net_add(w_current, w_current->page_current->object_tail, OBJ_NET, color, x1, y1, x2, y2);

	/* ales stuff */ 
	o_ales_disconnect_update(w_current->page_current);

	o_net_ales_erase(w_current, w_current->page_current->object_tail);	
	o_net_ales_draw(w_current, w_current->page_current->object_tail);	
	o_ales_draw_objects(w_current, w_current->page_current->object_tail);

	w_current->page_current->CHANGED=1;
	w_current->start_x = w_current->save_x;
        w_current->start_y = w_current->save_y;
}

void
o_net_rubbernet(TOPLEVEL *w_current, int x, int y)
{
	int diff_x, diff_y;
	int size;

	if (w_current->inside_action == 0) {
		o_redraw(w_current, w_current->page_current->object_head);
		return;
	}

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		size = SCREENabs(w_current, 10);
		/*size = return_zoom_number(w_current->page_current->zoom_factor);*/
		gdk_gc_set_line_attributes(w_current->xor_gc, size,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
        }  

	gdk_gc_set_foreground(w_current->xor_gc, x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	/* going into ortho mode (control key not pressed) */
		/* erase non-ortho line */

	/* going into non-ortho mode (control key pressed) */
		/* erase ortho line */

	w_current->last_x = fix_x(w_current, x);
        w_current->last_y = fix_y(w_current, y);


	/* If you press the control key then you can draw non-ortho nets */
	if (!w_current->CONTROLKEY) {
		diff_x = abs(w_current->last_x - w_current->start_x);
		diff_y = abs(w_current->last_y - w_current->start_y);

		if (diff_x >= diff_y) {
			w_current->last_y = w_current->start_y;	
		} else {
			w_current->last_x = w_current->start_x;
		}
	}

	gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->xor_gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->xor_gc, 0,
					GDK_LINE_SOLID,
					GDK_CAP_NOT_LAST,
					GDK_JOIN_MITER);
        }
}

/* used in button cancel code in x_events.c */
void
o_net_eraserubber(TOPLEVEL *w_current)
{
	int size;

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		size = SCREENabs(w_current, 10); 

		if (size < 0) 
			size=0;

		gdk_gc_set_line_attributes(w_current->gc, size,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        }   

	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->background_color) );
	gdk_draw_line(w_current->window, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}  
}

/* used in button cancel code in x_events.c */
void
o_net_xorrubber(TOPLEVEL *w_current)
{
	int size;

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {

		size = SCREENabs(w_current, 10); 

		if (size < 0) 
			size=0;

		gdk_gc_set_line_attributes(w_current->gc, size,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
        }   

	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->select_color) );
	gdk_draw_line(w_current->window, w_current->gc, w_current->start_x, w_current->start_y, w_current->last_x, w_current->last_y);	

	if (w_current->page_current->zoom_factor > 0 && w_current->net_style == THICK ) {
		gdk_gc_set_line_attributes(w_current->gc, 0,
				GDK_LINE_SOLID,
				GDK_CAP_NOT_LAST,
				GDK_JOIN_MITER);
	}  
}

