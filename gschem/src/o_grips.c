/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

/* Kazu on July 16, 1999 - Added these macros to simplify the code */
#define GET_BOX_WIDTH(w)                        \
        abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)                       \
	        abs((w)->last_y - (w)->start_y)
#define GET_BOX_LEFT(w)                         \
	        min((w)->start_x, (w)->last_x);
#define GET_BOX_TOP(w)                          \
	        min((w)->start_y, (w)->last_y);


static int whichone_changing=-1;
static OBJECT *object_changing;

OBJECT *
o_grips_search(TOPLEVEL *w_current, int x, int y, int *whichone)
{
	OBJECT *object=NULL;
	SELECTION *s_current;
	int top, left, right, bottom;
	int size, x2size;
	int x1, y1;
	int factor;

	if (!whichone) {
		return(NULL);
	}

	factor = (int) w_current->page_current->to_world_x_constant;
	if (factor > SMALL_ZOOMFACTOR1) {
		size = SCREENabs(w_current, GRIP_SIZE1);
	} else if (factor > SMALL_ZOOMFACTOR2) {
		size = SCREENabs(w_current, GRIP_SIZE2);
	} else {
		size = SCREENabs(w_current, GRIP_SIZE3);
	}
	x2size = size * 2;

	/* skip over head */
	s_current = w_current->page_current->selection2_head->next;
	while (s_current) {
	  if (s_current->selected_object) {
		object = s_current->selected_object;
		switch(object->type) {

			case(OBJ_LINE):
			case(OBJ_PIN):
			case(OBJ_NET):
			case(OBJ_BUS):

				left = object->line->screen_x[0] - size;
				top = object->line->screen_y[0] - size;
				right = left + x2size;
				bottom = top + x2size;

				if (inside_region(left, top, right, bottom, 
					x, y)) {
					/* printf("found something 0!\n");*/
					*whichone = 0;
					return(object);
				}

				left = object->line->screen_x[1] - size;
				top = object->line->screen_y[1] - size;
				right = left + x2size;
				bottom = top + x2size;

				if (inside_region(left, top, right, bottom, 
						x, y)) {
					/* printf("found something 1!\n");*/
					*whichone = 1;
					return(object);
				}

			break;

			case(OBJ_BOX):
				left = object->box->screen_upper_x - size;
				top = object->box->screen_upper_y - size;
				right = left + x2size;
				bottom = top + x2size;

				if (inside_region(left, top, right, bottom, 
					x, y)) {
					/* printf("found something 0!\n");*/
					*whichone = 0;
					return(object);
				}

				left = object->box->screen_lower_x - size;
				top = object->box->screen_lower_y - size;
				right = left + x2size;
				bottom = top + x2size;

				if (inside_region(left, top, right, bottom, 
						x, y)) {
					/*printf("found something 1!\n");*/
					*whichone = 1;
					return(object);
				}

				left = object->box->screen_lower_x - size;
				top = object->box->screen_upper_y - size;
				right = left + x2size;
				bottom = top + x2size;

				if (inside_region(left, top, right, bottom, 
						x, y)) {
					/*printf("found something 2!\n");*/
					*whichone = 2;
					return(object);
				}

				left = object->box->screen_upper_x - size;
				top = object->box->screen_lower_y - size;
				right = left + x2size;
				bottom = top + x2size;

				if (inside_region(left, top, right, bottom, 
						x, y)) {
					/*printf("found something 3!\n");*/
					*whichone = 3;
					return(object);
				}
			break;

			case(OBJ_CIRCLE):
				x1 = object->circle->screen_x + 
					object->circle->screen_radius;
				y1 = object->circle->screen_y + 
					object->circle->screen_radius;
				left = x1 - size;
				top = y1 - size;
				right = left + x2size;
				bottom = top + x2size;

				if (inside_region(left, top, right, bottom, 
					x, y)) {
					/* printf("found something 0!\n"); */
					*whichone = 0;
					return(object);
				}
			break;
		}
	  }
	  s_current = s_current->next;	
	}

	return(NULL);
}

int
o_grips_start(TOPLEVEL *w_current, int x, int y)
{
	OBJECT *object;
	int whichone;
	int x1, y1;
	int box_width, box_height, box_top, box_left;

	if (w_current->draw_grips == FALSE) {
		return(FALSE);
	}

	object = o_grips_search(w_current, x, y, &whichone);

	if (object) {
	  switch(object->type) {

	    case(OBJ_LINE): 
		w_current->last_drawb_mode = -1;
		w_current->last_x = object->line->screen_x[whichone];
		w_current->last_y = object->line->screen_y[whichone];
		w_current->start_x = object->line->screen_x[!whichone];
		w_current->start_y = object->line->screen_y[!whichone];

        	gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
        	gdk_draw_line(w_current->window, w_current->xor_gc, 
                      		w_current->start_x, w_current->start_y, 
                      		w_current->last_x, w_current->last_y);

		o_line_erase_grips(w_current, object);

		whichone_changing = whichone;
		object_changing = object;
		return(TRUE);
            break;

	    case(OBJ_NET): 
		w_current->last_drawb_mode = -1;
		w_current->last_x = object->line->screen_x[whichone];
		w_current->last_y = object->line->screen_y[whichone];
		w_current->start_x = object->line->screen_x[!whichone];
		w_current->start_y = object->line->screen_y[!whichone];

        	gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
        	gdk_draw_line(w_current->window, w_current->xor_gc, 
                      		w_current->start_x, w_current->start_y, 
                      		w_current->last_x, w_current->last_y);
		o_line_erase_grips(w_current, object);

		whichone_changing = whichone;
		object_changing = object;
		gdk_gc_set_foreground(w_current->gc, 
				x_get_color(w_current->background_color)); 
		o_conn_draw_endpoint(w_current, w_current->gc, 
				w_current->last_x, w_current->last_y);
		return(TRUE);
	   
	    break; 

	    case(OBJ_PIN): 

		w_current->last_drawb_mode = -1;
		w_current->last_x = object->line->screen_x[whichone];
		w_current->last_y = object->line->screen_y[whichone];
		w_current->start_x = object->line->screen_x[!whichone];
		w_current->start_y = object->line->screen_y[!whichone];

        	gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
        	gdk_draw_line(w_current->window, w_current->xor_gc, 
                      		w_current->start_x, w_current->start_y, 
                      		w_current->last_x, w_current->last_y);
		o_line_erase_grips(w_current, object);

		whichone_changing = whichone;
		object_changing = object;
		return(TRUE);

	    break; 

	    case(OBJ_BUS): 
		w_current->last_drawb_mode = -1;
		w_current->last_x = object->line->screen_x[whichone];
		w_current->last_y = object->line->screen_y[whichone];
		w_current->start_x = object->line->screen_x[!whichone];
		w_current->start_y = object->line->screen_y[!whichone];

        	gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
        	gdk_draw_line(w_current->window, w_current->xor_gc, 
                      		w_current->start_x, w_current->start_y, 
                      		w_current->last_x, w_current->last_y);
		o_line_erase_grips(w_current, object);

		whichone_changing = whichone;
		object_changing = object;
		gdk_gc_set_foreground(w_current->gc, 
				x_get_color(w_current->background_color)); 
		o_conn_draw_endpoint(w_current, w_current->gc, 
				w_current->last_x, w_current->last_y);
		return(TRUE);
	   
	    break; 

	    case(OBJ_BOX): 
		w_current->last_drawb_mode = -1;

		switch(whichone) {
			case(0): 
				w_current->last_x = object->box->screen_upper_x;
				w_current->last_y = object->box->screen_upper_y;
				w_current->start_x = object->box->screen_lower_x;
				w_current->start_y = object->box->screen_lower_y;
			break;

			case(1): 
				w_current->last_x = object->box->screen_lower_x;
				w_current->last_y = object->box->screen_lower_y;
				w_current->start_x = object->box->screen_upper_x;
				w_current->start_y = object->box->screen_upper_y;
			break;

			case(2): 
				w_current->last_x = object->box->screen_lower_x;
				w_current->last_y = object->box->screen_upper_y;
				w_current->start_x = object->box->screen_upper_x;
				w_current->start_y = object->box->screen_lower_y;
			break;

			case(3): 
				w_current->last_x = object->box->screen_upper_x;
				w_current->last_y = object->box->screen_lower_y;
				w_current->start_x = object->box->screen_lower_x;
				w_current->start_y = object->box->screen_upper_y;
			break;

		}

        	box_width  = GET_BOX_WIDTH (w_current);
	        box_height = GET_BOX_HEIGHT(w_current);
		box_left   = GET_BOX_LEFT  (w_current);
		box_top    = GET_BOX_TOP   (w_current);

        	gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
		gdk_draw_rectangle(w_current->window, w_current->xor_gc,
				   FALSE, 
				   box_left, box_top,
				   box_width, box_height);

		o_box_erase_grips(w_current, object);

		whichone_changing = whichone;
		object_changing = object;
		return(TRUE);
            break;

	    case(OBJ_CIRCLE): 
		x1 = object->circle->screen_x + object->circle->screen_radius;
		y1 = object->circle->screen_y; 
		w_current->last_x = x1;
		w_current->last_y = y1;
		w_current->start_x = object->circle->screen_x;
		w_current->start_y = object->circle->screen_y;

		w_current->distance = dist(w_current->start_x, 
					   w_current->start_y, 
					   w_current->last_x, 
					   w_current->last_y);

        	gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );

		gdk_draw_arc(w_current->window, w_current->xor_gc, FALSE,
			     w_current->start_x - w_current->distance,
			     w_current->start_y - w_current->distance,
			     w_current->distance * 2, w_current->distance * 2,
			     0, FULL_CIRCLE);

		o_circle_erase_grips(w_current, object);

		whichone_changing = whichone;
		object_changing = object;
		return(TRUE);
	    break;
	  }
	}

	return(FALSE);
}


void
o_grips_motion(TOPLEVEL *w_current, int x, int y)
{
	int diff_x, diff_y;
	int box_height, box_width, box_left, box_top;

        if (w_current->inside_action == 0) {
                o_redraw(w_current, w_current->page_current->object_head);
                return;
        }

	if (object_changing == NULL) {
                o_redraw(w_current, w_current->page_current->object_head);
		return;
	}


#if 0 /* doing this causes excessize mouse droppings */	
        if (w_current->net_style == THICK ) {
                size = SCREENabs(w_current, 10);
                gdk_gc_set_line_attributes(w_current->xor_gc, size,
                                           GDK_LINE_SOLID, GDK_CAP_NOT_LAST,
                                           GDK_JOIN_MITER);
        }
#endif


	switch(object_changing->type) {

		case(OBJ_LINE):
		case(OBJ_NET):
		case(OBJ_PIN):
		case(OBJ_BUS):
        		gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
        		gdk_draw_line(w_current->window, w_current->xor_gc, 
                      		w_current->start_x, w_current->start_y, 
                      		w_current->last_x, w_current->last_y);

			/* ortho stuff */
			w_current->last_x = fix_x(w_current, x);
        		w_current->last_y = fix_y(w_current, y);

        		/* If you press the control key then you can draw 
			 * ortho lines */
        		if (w_current->CONTROLKEY && 
	    		    !(object_changing->type == OBJ_NET || 
	     		      object_changing->type == OBJ_PIN ||
	     		      object_changing->type == OBJ_BUS)) {
                		diff_x = abs(w_current->last_x - w_current->start_x);
                		diff_y = abs(w_current->last_y - w_current->start_y);
                                                                     
                		if (diff_x >= diff_y) { 
                        		w_current->last_y = w_current->start_y;
                		} else {                                       
                        		w_current->last_x = w_current->start_x;
                		}      
        		}

        		if (!w_current->CONTROLKEY && 
	    		    (object_changing->type == OBJ_NET || 
	     		     object_changing->type == OBJ_PIN || 
	     	             object_changing->type == OBJ_BUS)) {
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

		break;

		case(OBJ_BOX):

			box_width  = GET_BOX_WIDTH (w_current);
			box_height = GET_BOX_HEIGHT(w_current);
			box_left   = GET_BOX_LEFT  (w_current);
			box_top    = GET_BOX_TOP   (w_current);

			gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
			gdk_draw_rectangle(w_current->window, w_current->xor_gc,
				   FALSE, box_left, box_top,
				   box_width, box_height);

        		w_current->last_x = fix_x(w_current, x);
        		w_current->last_y = fix_y(w_current, y);

			box_width  = GET_BOX_WIDTH (w_current);
			box_height = GET_BOX_HEIGHT(w_current);
			box_left   = GET_BOX_LEFT  (w_current);
			box_top    = GET_BOX_TOP   (w_current);

			gdk_gc_set_foreground(w_current->xor_gc,
			      x_get_color(w_current->select_color));
			gdk_draw_rectangle(w_current->window, w_current->xor_gc,
				   FALSE,
				   box_left  ,
				   box_top   ,
				   box_width ,
				   box_height);
		break;

		case(OBJ_CIRCLE):

			w_current->distance = dist(w_current->start_x, 
					   w_current->start_y, 
					   w_current->last_x, 
					   w_current->last_y);

        		gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );

			gdk_draw_arc(w_current->window, w_current->xor_gc, 
				     FALSE,
			     	     w_current->start_x - w_current->distance,
			             w_current->start_y - w_current->distance,
			             w_current->distance * 2, 
				     w_current->distance * 2,
			             0, FULL_CIRCLE);

			w_current->last_x = fix_x(w_current, x);
			w_current->last_y = fix_y(w_current, y);

			diff_x = GET_BOX_WIDTH (w_current);
			diff_y = GET_BOX_HEIGHT(w_current);

			if (diff_x >= diff_y) {
				w_current->last_y = w_current->start_y; 
			} else {
				w_current->last_x = w_current->start_x;
			}

        		w_current->distance = dist(w_current->start_x, 
						w_current->start_y, 
						w_current->last_x, 
						w_current->last_y); 

			gdk_draw_arc(w_current->window, w_current->xor_gc, 
				     FALSE,
			     	     w_current->start_x - w_current->distance,
			             w_current->start_y - w_current->distance,
			             w_current->distance * 2, 
				     w_current->distance * 2,
			             0, FULL_CIRCLE);
		break;
	}

#if 0 /* doing this causes excessize mouse droppings */
        if (w_current->net_style == THICK ) {
                gdk_gc_set_line_attributes(w_current->xor_gc, 0,
                                           GDK_LINE_SOLID, GDK_CAP_NOT_LAST,
                                           GDK_JOIN_MITER);
        }
#endif
}

void
o_grips_end(TOPLEVEL *w_current)
{
	OBJECT *object=NULL;
	int x, y;
	int box_height, box_width, box_top, box_left;

	object = object_changing;

	if (!object) {
		/* actually this is an error condition hack */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");
		w_current->inside_action = 0;
		return;
	}

	switch(object->type) {

		case(OBJ_LINE):
	
	       		/* don't allow zero length nets / lines / pins
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
               			return;
        		}

				
        		SCREENtoWORLD(w_current, 
				      w_current->last_x, 
				      w_current->last_y, &x, &y);

        		x = snap_grid(w_current, x);
        		y = snap_grid(w_current, y);

			o_line_erase(w_current, object);

			/* erase xor line */
			gdk_gc_set_foreground(w_current->xor_gc,
			                 x_get_color(w_current->select_color));
			gdk_draw_line(w_current->window, w_current->xor_gc,
			              w_current->start_x, w_current->start_y,
			              w_current->last_x, w_current->last_y);
			o_line_erase_grips(w_current, object);

			o_line_modify(w_current, object, x, y, 
				      whichone_changing);

		break;

		case(OBJ_NET):
	       		/* don't allow zero length nets / lines / pins
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
				      w_current->last_y, &x, &y);

        		x = snap_grid(w_current, x);
        		y = snap_grid(w_current, y);

			o_net_erase(w_current, object);
			/* erase xor line */
			gdk_gc_set_foreground(w_current->xor_gc,
			                 x_get_color(w_current->select_color));
			gdk_draw_line(w_current->window, w_current->xor_gc,
			              w_current->start_x, w_current->start_y,
			              w_current->last_x, w_current->last_y);
			o_line_erase_grips(w_current, object);

			o_net_modify(w_current, object, x, y, 
				      whichone_changing);

		break;

		case(OBJ_PIN):
	       		/* don't allow zero length nets / lines / pins
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
               			return;
        		}

				
        		SCREENtoWORLD(w_current, 
				      w_current->last_x, 
				      w_current->last_y, &x, &y);

        		x = snap_grid(w_current, x);
        		y = snap_grid(w_current, y);

			o_pin_erase(w_current, object);
			/* erase xor line */
			gdk_gc_set_foreground(w_current->xor_gc,
			                 x_get_color(w_current->select_color));
			gdk_draw_line(w_current->window, w_current->xor_gc,
			              w_current->start_x, w_current->start_y,
			              w_current->last_x, w_current->last_y);
			o_line_erase_grips(w_current, object);

			o_pin_modify(w_current, object, x, y, 
				      whichone_changing);

		break;

		case(OBJ_BUS):
	       		/* don't allow zero length nets / lines / pins
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
				      w_current->last_y, &x, &y);

        		x = snap_grid(w_current, x);
        		y = snap_grid(w_current, y);


			o_bus_erase(w_current, object);
			/* erase xor line */
			gdk_gc_set_foreground(w_current->xor_gc,
			                 x_get_color(w_current->select_color));
			gdk_draw_line(w_current->window, w_current->xor_gc,
			              w_current->start_x, w_current->start_y,
			              w_current->last_x, w_current->last_y);
			o_line_erase_grips(w_current, object);

			o_bus_modify(w_current, object, x, y, 
				      whichone_changing);
		break;

		case(OBJ_BOX):
	       		/* don't allow zero length nets / lines / pins
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
               			return;
        		}

        		SCREENtoWORLD(w_current, 
				      w_current->last_x, 
				      w_current->last_y, &x, &y);

        		x = snap_grid(w_current, x);
        		y = snap_grid(w_current, y);

			o_box_erase(w_current, object);
        		box_width  = GET_BOX_WIDTH (w_current);
	        	box_height = GET_BOX_HEIGHT(w_current);
			box_left   = GET_BOX_LEFT  (w_current);
			box_top    = GET_BOX_TOP   (w_current);

        		gdk_gc_set_foreground(w_current->xor_gc, 
                              x_get_color(w_current->select_color) );
			gdk_draw_rectangle(w_current->window, w_current->xor_gc,
				   FALSE, 
				   box_left, box_top,
				   box_width, box_height);
			o_box_erase_grips(w_current, object);

			o_box_modify(w_current, object, x, y, 
				      whichone_changing);
		break;

		case(OBJ_CIRCLE):

	       		/* don't allow zero length nets / lines / pins
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
               			return;
        		}

        		SCREENtoWORLD(w_current, 
				      w_current->last_x, 
				      w_current->last_y, &x, &y);

        		x = snap_grid(w_current, x);
        		y = snap_grid(w_current, y);

			o_circle_erase(w_current, object);

			w_current->distance = dist(w_current->start_x, 
					   w_current->start_y, 
					   w_current->last_x, 
					   w_current->last_y);

        		gdk_gc_set_foreground( w_current->xor_gc, 
                              x_get_color(w_current->select_color) );

			gdk_draw_arc(w_current->window, w_current->xor_gc, 
				     FALSE,
			     	     w_current->start_x - w_current->distance,
			             w_current->start_y - w_current->distance,
			             w_current->distance * 2, 
				     w_current->distance * 2,
			             0, FULL_CIRCLE);

			o_circle_erase_grips(w_current, object);

			o_circle_modify(w_current, object, x, y, 
				      whichone_changing);
		break;
	}

			
	w_current->page_current->CHANGED=1;

	o_conn_disconnect_update(w_current->page_current);

	/*o_redraw_single(w_current, object);*/

	/* not sure I want to do this.. but let's try it */	
	o_redraw(w_current, w_current->page_current->object_head);

	whichone_changing = -1;
	object_changing = NULL;
	o_undo_savestate(w_current, UNDO_ALL);
}

