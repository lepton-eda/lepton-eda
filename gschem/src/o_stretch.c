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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

static int whichone_changing=-1;

int
o_stretch_start(TOPLEVEL *w_current, int x, int y)
{
	OBJECT *object;
	OBJECT *closest;
	int whichone;

	object = o_select_return_first_object(w_current);

	if (object) {

		w_current->last_drawb_mode = -1;
		w_current->event_state = STRETCH; 

#if DEBUG
		printf("objects selected and inside stretch now\n");
#endif
	
		closest = o_conn_find_closest(object, x, y, 
					      &whichone, NULL, NULL);

		if (closest) {

			switch(whichone) {

			case(0):
				whichone_changing = -1;
			break;

			case(1):

				whichone_changing = 1;

				w_current->last_x = closest->
						    line_points->
						    screen_x1;
				w_current->last_y = closest->
						    line_points->
						    screen_y1;
				w_current->start_x = closest->
						    line_points->
						    screen_x2;
				w_current->start_y = closest->
						    line_points->
						    screen_y2;
#if 0
				w_current->last_x = fix_x(w_current, 
							  closest->
							  line_points->
							  screen_x1);

				w_current->last_y = fix_x(w_current, 
							  closest->
							  line_points->
							  screen_y1);

				w_current->start_x = fix_x(w_current, 
							   closest->
							   line_points->
							   screen_x2);

				w_current->start_y = fix_y(w_current, 
							   closest->
							   line_points->
							   screen_y2);
#endif

#if DEBUG
				gdk_draw_arc(w_current->window, 
					     w_current->gc, FALSE,
                                       	     closest->line_points->screen_x1,
                                       	     closest->line_points->screen_y1,
                                       	     100, 100, 0, FULL_CIRCLE);
#endif

				break;

			case(2):

				whichone_changing = 2;

				w_current->last_x = closest->
						    line_points->
						    screen_x2;
				w_current->last_y = closest->
						    line_points->
						    screen_y2;
				w_current->start_x = closest->
						    line_points->
						    screen_x1;
				w_current->start_y = closest->
						    line_points->
						    screen_y1;

#if 0
				w_current->last_x = fix_x(w_current, 
							  found->
							  line_points->
							  screen_x2);

				w_current->last_y = fix_x(w_current, 
							  found->
							  line_points->
							  screen_y2);

				w_current->start_x = fix_x(w_current, 
							   found->
							   line_points->
							   screen_x1);

				w_current->start_y = fix_y(w_current, 
							   found->
							   line_points->
							   screen_y1);
#endif

#if DEBUG
				gdk_draw_arc(w_current->window, 
					     w_current->gc, FALSE,
                                       	     closest->line_points->screen_x2,
                                       	     closest->line_points->screen_y2,
                                       	     100, 100, 0, FULL_CIRCLE);
#endif
				break;


			}
		
		}		

#if DEBUG
		printf("whichone: %d\n", whichone);
#endif

		if (!closest) {
			printf("Stretching not supported for this object type\n");
			return(FALSE);
		}
		
		if (closest->type == OBJ_NET) {
			gdk_gc_set_foreground(w_current->gc,
                                              x_get_color(w_current->
							  background_color));

			o_conn_draw_endpoint(w_current, w_current->gc, 
				             w_current->last_x, 
					     w_current->last_y);
		}

		w_current->inside_action = 1;
	}
	return(TRUE);
}

void
o_stretch_end(TOPLEVEL *w_current)
{
	OBJECT *found=NULL;
	OBJECT *object=NULL;
	int x, y;

	object = o_select_return_first_object(w_current);

	if (!object) {
		/* actually this is an error condition hack */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}


	/* maybe put in a while (...) to stretch many objects... */
	switch(object->type) {
			case(OBJ_LINE):

			break;

			case(OBJ_NET):
	
	        		/* don't allow zero length nets 
        			 * this ends the net drawing behavior 
			         * we want this? hack */
        			if ((w_current->start_x == w_current->last_x) &&
             			    (w_current->start_y == w_current->last_y)) {
                			w_current->start_x = (-1);
                			w_current->start_y = (-1);
                			w_current->last_x = (-1);
                			w_current->last_y = (-1);
                			w_current->inside_action=0;
                			w_current->event_state = SELECT;
                			i_update_status(w_current, 
							"Select Mode");
                			o_net_eraserubber(w_current);
                			return;
        			}

				
        			SCREENtoWORLD(w_current, 
					      w_current->last_x, 
					      w_current->last_y, 
					      &x, &y);

        			x = snap_grid(w_current, x);
        			y = snap_grid(w_current, y);

#if DEBUG
		printf("previous endpoints: %d %d %d %d\n", 
				object->line_points->x1,
				object->line_points->y1,  
				object->line_points->x2,
				object->line_points->y2);  

		printf("new end points: %d %d\n", x, y);
#endif

				o_net_modify(w_current, object, 
					     x, y, whichone_changing);

#if DEBUG
		printf("final endpoints: %d %d %d %d\n", 
				object->line_points->x1,
				object->line_points->y1,  
				object->line_points->x2,
				object->line_points->y2);  
#endif
			break;

#if 0 /* not ready yet */
			case(OBJ_BUS):
	
	        		/* don't allow zero length nets 
        			 * this ends the net drawing behavior 
			         * we want this? hack */
        			if ((w_current->start_x == w_current->last_x) &&
             			    (w_current->start_y == w_current->last_y)) {
                			w_current->start_x = (-1);
                			w_current->start_y = (-1);
                			w_current->last_x = (-1);
                			w_current->last_y = (-1);
                			w_current->inside_action=0;
                			w_current->event_state = SELECT;
                			i_update_status(w_current, 
							"Select Mode");
                			o_bus_eraserubber(w_current);
                			return;
        			}

				
        			SCREENtoWORLD(w_current, 
					      w_current->last_x, 
					      w_current->last_y, 
					      &x, &y);

        			x = snap_grid(w_current, x);
        			y = snap_grid(w_current, y);

#if DEBUG
		printf("previous endpoints: %d %d %d %d\n", 
				object->line_points->x1,
				object->line_points->y1,  
				object->line_points->x2,
				object->line_points->y2);  

		printf("new end points: %d %d\n", x, y);
#endif

				o_bus_modify(w_current, object, 
					     x, y, whichone_changing);

#if DEBUG
		printf("final endpoints: %d %d %d %d\n", 
				found->line_points->x1,
				found->line_points->y1,  
				found->line_points->x2,
				found->line_points->y2);  
#endif
			break;
#endif

			case(OBJ_PIN):
	
	        		/* don't allow zero length nets 
        			 * this ends the net drawing behavior 
			         * we want this? hack */
        			if ((w_current->start_x == w_current->last_x) &&
             			    (w_current->start_y == w_current->last_y)) {
                			w_current->start_x = (-1);
                			w_current->start_y = (-1);
                			w_current->last_x = (-1);
                			w_current->last_y = (-1);
                			w_current->inside_action=0;
                			w_current->event_state = SELECT;
                			i_update_status(w_current, 
							"Select Mode");

                			o_pin_eraserubber(w_current);
                			return;
        			}

				
        			SCREENtoWORLD(w_current, 
					      w_current->last_x, 
					      w_current->last_y, 
					      &x, &y);

        			x = snap_grid(w_current, x);
        			y = snap_grid(w_current, y);

#if DEBUG
		printf("previous endpoints: %d %d %d %d\n", 
				object->line_points->x1,
				object->line_points->y1,  
				object->line_points->x2,
				object->line_points->y2);  

		printf("new end points: %d %d\n", x, y);
#endif

				o_pin_modify(w_current, object, 
					     x, y, whichone_changing);

#if DEBUG
		printf("final endpoints: %d %d %d %d\n", 
				object->line_points->x1,
				object->line_points->y1,  
				object->line_points->x2,
				object->line_points->y2);  
#endif

			break;


			case(OBJ_BOX):

			break;

			case(OBJ_CIRCLE):

			break;

			case(OBJ_COMPLEX):

			break;

			case(OBJ_TEXT):

			break;

			case(OBJ_ARC):

			break;
	}

	w_current->page_current->CHANGED=1;

	o_conn_disconnect_update(w_current->page_current);

	/* o_conn_erase_all(w_current, w_current->page_current->object_head);*/

	/* TODO this is questionable, is it really needed? */
	o_redraw(w_current, w_current->page_current->object_head);

	whichone_changing = -1;
	o_undo_savestate(w_current, UNDO_ALL);
}

/* this also needs support for bus and the other objects... */
/* so in otherwords, it currently only works for nets */
void
o_stretch_motion(TOPLEVEL *w_current, int x, int y)
{
	int diff_x, diff_y;
        int size;

        if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
                return;
        }

	
        if (w_current->page_current->zoom_factor > 0 && 
            w_current->net_style == THICK ) {
                size = SCREENabs(w_current, 10);
                gdk_gc_set_line_attributes(w_current->xor_gc, size,
                                           GDK_LINE_SOLID, GDK_CAP_NOT_LAST,
                                           GDK_JOIN_MITER);
        }


        gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
        gdk_draw_line(w_current->window, w_current->xor_gc, 
                      w_current->start_x, w_current->start_y, 
                      w_current->last_x, w_current->last_y);

	/* ortho stuff */
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
        gdk_draw_line(w_current->window, w_current->xor_gc, 
                      w_current->start_x, w_current->start_y, 
	              w_current->last_x, w_current->last_y);

        if (w_current->page_current->zoom_factor > 0 && 
	    w_current->net_style == THICK ) {
                gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                                           GDK_LINE_SOLID, GDK_CAP_NOT_LAST,
                                           GDK_JOIN_MITER);
        }
}
