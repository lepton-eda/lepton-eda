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
#include <stdlib.h>
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

/* used in key_press, since it isn't passed this information */
/* filled in x_event_motion */
int mouse_x, mouse_y;

/* used by mouse pan */
extern int current_center_x, current_center_y;
int start_pan_x, start_pan_y;
int throttle = 0;

/* used for the stroke stuff */
#ifdef HAS_LIBSTROKE
#define MAX_SEQUENCE 20
static int DOING_STROKE = FALSE;
char sequence[MAX_SEQUENCE+1];

/* libstroke prototypes */
void stroke_init (void);
void stroke_record (int x, int y);
int stroke_trans (char *sequence);
#endif

gint
x_event_expose(GtkWidget *widget, GdkEventExpose *event, TOPLEVEL *w_current)
{
#if DEBUG
	printf("EXPOSE\n");
#endif

	exit_if_null(w_current);
	global_window_current = w_current;

	gdk_draw_pixmap(widget->window,
			widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
			w_current->backingstore,
			event->area.x, event->area.y,
			event->area.x, event->area.y,
			event->area.width, event->area.height);

	/* either this or put xor's and friends into backingstore */
	/* take care of ghosting when you get an expose event */
	if (w_current->inside_action) {
                switch(w_current->event_state) {
		case(ENDMOVE):
		case(ENDCOPY):
			o_drawbounding(
				w_current,
				w_current->page_current->selection_head->next,
				x_get_color(w_current->bb_color));
                        break;

		case(DRAWNET):
		case(NETCONT):
		case(DRAWBUS):
		case(BUSCONT):
			/* do nothing for now */
			break;
                }
        }

	return(0);
}

gint
x_event_button_pressed(GtkWidget *widget, GdkEventButton *event,
		       TOPLEVEL *w_current)
{
	exit_if_null(w_current);
	global_window_current = w_current;

#if DEBUG
	printf("pressed! %d \n", event_state);
#endif

	w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
	w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
	w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

	if (event->button == 1) {
		switch(w_current->event_state) {

		case(SELECT):
			w_current->event_state = STARTSELECT;
			w_current->start_x = w_current->last_x =
				(int) event->x;
			w_current->start_y = w_current->last_y =
				(int) event->y;
			break;

		case(STARTCOPY):
			/* make sure the list is not empty */
			if (w_current->page_current->selection_head->next !=
			    NULL) {
				o_copy_start(w_current,
					     (int) event->x,
					     (int) event->y);
				w_current->event_state = COPY;
				w_current->inside_action = 1;
			}
                        break;

		case(STARTMOVE):
			/* make sure the list is not empty */
			if (w_current->page_current->selection_head->next !=
			    NULL) {
				o_move_start(w_current,
					     (int) event->x,
					     (int) event->y);
				w_current->event_state = MOVE;
				w_current->inside_action = 1;
			}
                        break;

		case(STARTSTRETCH):
			/* make sure the list is not empty */
			if (w_current->page_current->selection_head->next != 
			    NULL) {

				/* only stretch if it's a valid object */
				if (o_stretch_start(w_current, 
						(int) event->x, 
						(int) event->y)) {
					w_current->event_state = STRETCH;
					w_current->inside_action = 1;
				} else {
					w_current->event_state=SELECT;
					i_update_status(w_current, "Select Mode");
					w_current->inside_action = 0;
				}
			}
                        break;       

		case(DRAWLINE):
			o_line_start(w_current,
				     (int) event->x,
				     (int) event->y);
			w_current->event_state = ENDLINE;
			w_current->inside_action = 1;
			break;

		case(ENDLINE):
			o_line_end(w_current,
				   (int) event->x,
				   (int) event->y);
			w_current->inside_action = 0;
			w_current->event_state = DRAWLINE;
			break;

		case(DRAWBOX):
			o_box_start(w_current,
				    (int) event->x,
				    (int) event->y);
			w_current->event_state = ENDBOX;
			w_current->inside_action = 1;
			break;

		case(ENDBOX):
			o_box_end(w_current,
				  (int) event->x,
				  (int) event->y);
			w_current->inside_action = 0;
			w_current->event_state = DRAWBOX;
			break;

		case(DRAWCIRCLE):
			o_circle_start(w_current,
				       (int) event->x,
				       (int) event->y);
			w_current->event_state = ENDCIRCLE;
			w_current->inside_action = 1;
			break;

		case(ENDCIRCLE):
			o_circle_end(w_current,
				     (int) event->x,
				     (int) event->y);
			w_current->inside_action = 0;
			w_current->event_state = DRAWCIRCLE;
			break;

		case(DRAWARC):
			o_arc_start(w_current,
				    (int) event->x,
				    (int) event->y);
			w_current->event_state = ENDARC;
			w_current->inside_action = 1;
			break;

		case(ENDARC):
			o_arc_end1(w_current,
				   (int) event->x,
				   (int) event->y);
			w_current->inside_action = 0;
			w_current->event_state = DRAWARC;
			break;

		case(DRAWPIN):
			o_pin_start(w_current,
				    (int) event->x,
				    (int) event->y);
			w_current->event_state = ENDPIN;
			w_current->inside_action = 1;
			break;

		case(ENDPIN):
			o_pin_end(w_current,
				  (int) event->x,
				  (int) event->y);
			w_current->inside_action = 0;
			w_current->event_state = DRAWPIN;
			break;

		case(STARTDRAWNET):  /* TODO: change state name? */
			o_net_start(w_current,
				    (int) event->x,
				    (int) event->y);
			w_current->inside_action = 1;
			w_current->event_state=DRAWNET;

			break;

		case(STARTDRAWBUS):  
			o_bus_start(w_current,
				    (int) event->x,
				    (int) event->y);
			w_current->inside_action = 1;
			w_current->event_state=DRAWBUS;

			break;

		case(DRAWNET):
		case(NETCONT):
			o_net_end(w_current,
				  (int) event->x,
				  (int) event->y);
			o_net_start(w_current,
				    (int) w_current->save_x,
				    (int) w_current->save_y);
			w_current->event_state=NETCONT;
			break;

		case(DRAWBUS):
		case(BUSCONT):
			o_bus_end(w_current,
				  (int) event->x,
				  (int) event->y);
			o_bus_start(w_current,
				    (int) w_current->save_x,
				    (int) w_current->save_y);
			w_current->event_state=BUSCONT;
			break;

#if 0 /* old way with the text dialog box which was around only once */
		case(DRAWTEXT):
			w_current->start_x = fix_x(w_current, (int) event->x);
			w_current->start_y = fix_y(w_current, (int) event->y);
			o_text_input(w_current);
			w_current->inside_action = 1;
			break;
#endif

		case(ENDCOMP):
			o_complex_end(w_current,
				      fix_x(w_current, (int) event->x),
				      fix_y(w_current, (int) event->y));
				/* not sure on this one */
				/* probably keep this one */
			w_current->event_state=SELECT;
			i_update_status(w_current, "Select Mode");
			w_current->inside_action = 0;
			o_redraw_single(w_current, w_current->page_current->object_tail);
			o_redraw_selected(w_current);
                        break;

		case(ENDATTRIB):
			o_attrib_end(w_current);
				/* not sure on this one either... */
				/* keep it as well */
			w_current->event_state=SELECT;
			i_update_status(w_current, "Select Mode");
			w_current->inside_action = 0;
				/* the following happen inside attrib_end */
				/* therefore they are commeneted out here */
                                /* o_redraw_single(object_tail);*/
                                /* o_redraw_selected(); not sure on this */
                        break;

		case(ENDROTATEP):
			o_rotate_90(
				w_current,
				w_current->page_current->selection_head->next,
				(int) event->x, (int) event->y);

			w_current->inside_action = 0;
			w_current->event_state = SELECT;
			i_update_status(w_current, "Select Mode");
			break;

		case(ENDMIRROR):
			o_mirror(w_current,
				 w_current->page_current->selection_head->next,
				 (int) event->x, (int) event->y);

			w_current->inside_action = 0;
			w_current->event_state = SELECT;
			i_update_status(w_current, "Select Mode");
			break;

		case(ENDTEXT):
			o_text_end(w_current);
				/* not sure on this one either... */
				/* keep it as well */
			w_current->event_state=SELECT;
			i_update_status(w_current, "Select Mode");
			w_current->inside_action = 0;
				/* the following happen inside attrib_end */
				/* therefore they are commeneted out here */
                                /* o_redraw_single(object_tail);*/
                                /* o_redraw_selected(); not sure on this */
                        break;

		case(STARTPAN):
			a_pan(w_current,
			      (int) event->x,
			      (int) event->y);

				/* keep this one too */
			w_current->event_state = SELECT;
			i_update_status(w_current, "Select Mode");
				/* go to select state or not? hack */
			break;

		case(ZOOMBOXSTART):
			a_zoom_box_start(w_current,
					 (int) event->x,
					 (int) event->y);
			w_current->event_state = ZOOMBOXEND;
			w_current->inside_action = 1;
			break;

		}
	} else if (event->button == 2) {

		/* try this out and see how it behaves */
		if (w_current->inside_action) {
			i_callback_cancel(w_current, 0, NULL);
		}

		switch(w_current->middle_button) {

			case(ACTION): 
				/* determine here if copy or move */
				/* for now do move only */
				/* make sure the list is not empty */
				if (w_current->page_current->
					selection_head->next == NULL) {
					o_find(w_current, 
					       (int) event->x, 
					       (int) event->y);
				} else {
					o_unselect_all(w_current);
					o_find(w_current, 
					       (int) event->x, 
					       (int) event->y);
				}

				if (w_current->page_current->selection_head->
					next == NULL) {
					/* this means the above find did not 
					 * find anything */
					w_current->event_state = SELECT;
				 	w_current->inside_action = 0;
					i_update_status(w_current,
							"Select Mode");
					return(0);
				}


				if (w_current->ALTKEY) {
					o_copy_start(w_current,
				     		     (int) event->x,
				                     (int) event->y);
				 	w_current->event_state = COPY;
					i_update_status(w_current, "Copy");
				 	w_current->inside_action = 1;
				} else {
					o_move_start(w_current,
				     		     (int) event->x,
				                     (int) event->y);
				 	w_current->event_state = MOVE;
					i_update_status(w_current, "Move");
				 	w_current->inside_action = 1;
				}
				break;

			case(REPEAT):	
				if (w_current->last_callback != NULL) {
					(*w_current->last_callback)(w_current, 	
								    0, NULL);
				}
				break;
#ifdef HAS_LIBSTROKE
			case(STROKE):
				DOING_STROKE=TRUE;
				break;

#endif
		}

	} else if (event->button == 3) {
		if (!w_current->inside_action) {
			if (w_current->third_button == POPUP_ENABLED) {
				do_popup(w_current, event);
			} else {
				w_current->event_state = MOUSEPAN; /* start */
				w_current->inside_action = 1;
				start_pan_x = (int) event->x;
				start_pan_y = (int) event->y;
				throttle=0;
			}
		} else {
			switch (w_current->event_state) {
			case(DRAWNET):
			case(NETCONT):
				w_current->inside_action = 0;
				w_current->event_state = SELECT;
				i_update_status(w_current, "Select Mode");
				o_net_eraserubber(w_current);
                        	break;

			case(DRAWBUS):
			case(BUSCONT):
				w_current->inside_action = 0;
				w_current->event_state = SELECT;
				i_update_status(w_current, "Select Mode");
				o_bus_eraserubber(w_current);
                        	break;

				/* TODO: go and seperate each draw to
				 * call the currect eraserubber */
			default:
				i_callback_cancel(w_current, 0, NULL);
                        	break;
			}
		}
	}
	return(0);
}

gint
x_event_button_released(GtkWidget *widget, GdkEventButton *event,
			TOPLEVEL *w_current)
{
	exit_if_null(w_current);
	global_window_current = w_current;

#if DEBUG
	printf("released! %d \n", event_state);
#endif

	w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
	w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
	w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

	if (event->button == 1) {
		switch(w_current->event_state) {
		case(SELECT):
			/* do nothing */
			break;

		case(MOVE):
			w_current->event_state = ENDMOVE;
			break;

		case(COPY):
			w_current->event_state = ENDCOPY;
			break;

		case(STRETCH):
			w_current->event_state = ENDSTRETCH;
			break;

		case(ENDMOVE):
			o_move_end(w_current);
			/* having this stay in copy was driving me nuts*/
			w_current->event_state = SELECT;
			i_update_status(w_current, "Select Mode");
			w_current->inside_action = 0;
			break;

		case(ENDCOPY):
			o_copy_end(w_current);
			/* having this stay in copy was driving me nuts*/
			w_current->event_state = SELECT;
			i_update_status(w_current, "Select Mode");
			w_current->inside_action = 0;
			break;

		case(ENDSTRETCH):
			o_stretch_end(w_current);
			/* having this stay in copy was driving me nuts*/
			w_current->event_state = SELECT;
			i_update_status(w_current, "Select Mode");
			w_current->inside_action = 0;
			break;

		case(SBOX):
			/* fix_x,y was removed to allow more flex */
			w_current->last_x = (int) event->x;
			w_current->last_y = (int) event->y;
			i_sbox_end(w_current,
				   (int) event->x,
				   (int) event->y);
			/* this one stays */
			w_current->event_state = SELECT;
			w_current->inside_action = 0;
                        break;

		case(ZOOMBOXEND):
			/* fix_x,y was removed to allow more flex */
			w_current->last_x = (int) event->x;
			w_current->last_y = (int) event->y;
			a_zoom_box_end(w_current,
				       (int) event->x,
				       (int) event->y);
			/* this one stays */
			i_update_status(w_current, "Select Mode");
			w_current->event_state = SELECT;
			w_current->inside_action = 0;
                        break;

		case(STARTSELECT):
			o_find(w_current, (int) event->x, (int) event->y);
			w_current->event_state = SELECT;
			w_current->inside_action = 0;
                        break;

		}
	} else if (event->button == 2) {

		switch(w_current->middle_button) { 
			case(ACTION): 	
				switch(w_current->event_state) {
					case(MOVE):
						o_move_end(w_current);
						w_current->event_state = SELECT;
						i_update_status(w_current, 
							"Select Mode");
						w_current->inside_action = 0;
					break;

					case(COPY):
						o_copy_end(w_current);
						w_current->event_state = SELECT;
						i_update_status(w_current, 
							"Select Mode");
						w_current->inside_action = 0;
					break;
				}
				break;

#if HAS_LIBSTROKE
			case(STROKE):

				DOING_STROKE = FALSE;

				if (stroke_trans (sequence) == TRUE) {
					if (stroke_info_mode) {
						printf ("LibStroke Translation"
							" succeeded: ");
					}
				} else {
					if (stroke_info_mode) {
						printf ("LibStroke Translation"
						 	" failed: ");
					}
				}
	
				if (stroke_info_mode) {
					printf ("Sequence=\"%s\"\n",sequence);
				}
	
				/* new way written by Stefan Petersen */ 
				/* much better */
				if (x_stroke_search_execute(sequence)) {

					if (stroke_info_mode) {
						printf("Sequence understood\n");
					}
                       			x_stroke_erase_all(w_current);
				}
				break;
#endif
		}

	} else if (event->button == 3) {
                w_current->event_state = SELECT;
                w_current->inside_action = 0;
	}
	return(0);
}

gint
x_event_motion(GtkWidget *widget, GdkEventMotion *event, TOPLEVEL *w_current)
{
	int temp_x, temp_y;
	int pdiff_x, pdiff_y;

	int zoom_scale;
	int diff_x;

	exit_if_null(w_current);
	global_window_current = w_current;

	w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
	w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
	w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

	mouse_x = (int) event->x;
	mouse_y = (int) event->y;

	if (w_current->cowindow) {
		coord_display_update(w_current, mouse_x, mouse_y);
	}

#if DEBUG
	/* printf("MOTION!\n");*/
#endif

#if HAS_LIBSTROKE
	if (DOING_STROKE == TRUE) {
		x_stroke_add_point(w_current, (int) event->x, (int) event->y);

		stroke_record ((int) event->x, (int) event->y);
		return(0);
	}
#endif

	if (w_current->third_button == MOUSEPAN_ENABLED) {
		if((w_current->event_state == MOUSEPAN) &&
		   w_current->inside_action) {
			pdiff_x = mouse_x - start_pan_x;
			pdiff_y = mouse_y - start_pan_y;

#if 0
			printf("current center: %d %d\n", current_center_x, current_center_y);
			printf("pdiff: %d %d\n", pdiff_x, pdiff_y);
#endif

			if (!(throttle % 5)) {
				a_pan_mouse(w_current, pdiff_x*5, pdiff_y*5);

				start_pan_x = (int) event->x;
				start_pan_y = (int) event->y;
			}
			throttle++;
			return(0);
		}
        }

	switch(w_current->event_state) {

	case(SELECT):
		/* do nothing */
		break;

	case(ENDMOVE):
	case(MOVE):
		if (w_current->inside_action) {
			o_drawbounding(
				w_current,
				w_current->page_current->selection_head->next,
				x_get_color(w_current->bb_color));
			w_current->last_x = fix_x(w_current,  (int) event->x);
			w_current->last_y = fix_y(w_current,  (int) event->y);
			o_drawbounding(
				w_current,
				w_current->page_current->selection_head->next,
				x_get_color(w_current->bb_color));
		}
                break;

	case(ENDSTRETCH):
	case(STRETCH):
		if (w_current->inside_action) {
			o_stretch_motion(w_current, 
			                 (int) event->x, (int) event->y);
			/* printf("inside stretch\n");*/
		}
		break;

	case(ENDCOPY):
	case(COPY):
		if (w_current->inside_action) {
			o_drawbounding(
				w_current,
				w_current->page_current->selection_head->next,
				x_get_color(w_current->bb_color));
			w_current->last_x = fix_x(w_current,  (int) event->x);
			w_current->last_y = fix_y(w_current,  (int) event->y);
			o_drawbounding(
				w_current,
				w_current->page_current->selection_head->next,
				x_get_color(w_current->bb_color));
		}
                break;

	case(ENDLINE):
		if (w_current->inside_action)
			o_line_rubberline(w_current,
					  (int) event->x,
					  (int) event->y);
		break;

	case(ENDBOX):
		if (w_current->inside_action)
			o_box_rubberbox( w_current,
					 (int) event->x,
					 (int) event->y);
		break;

	case(ENDCIRCLE):
		if (w_current->inside_action)
			o_circle_rubbercircle(w_current,
					      (int) event->x,
					      (int) event->y);
		break;

	case(ENDARC):
		if (w_current->inside_action)
			o_arc_rubberline(w_current,
					 (int) event->x,
					 (int) event->y);
		break;

	case(DRAWNET):
	case(NETCONT):
		if (w_current->inside_action)
			o_net_rubbernet(w_current,
					(int) event->x,
				 	(int) event->y);
		break;

	case(DRAWBUS):
	case(BUSCONT):
		if (w_current->inside_action)
			o_bus_rubberbus(w_current,
					(int) event->x,
				 	(int) event->y);
		break;

	case(ENDPIN):
		if (w_current->inside_action)
			o_pin_rubberpin(w_current,
					(int) event->x,
				 	(int) event->y);
		break;

	case(DRAWCOMP):
		o_complex_start(w_current,
				(int) event->x,
				(int) event->y);
		w_current->event_state = ENDCOMP;
		w_current->inside_action = 1;
		break;

	case(ENDCOMP):
		o_complex_rubbercomplex(w_current);
		w_current->last_x = fix_x(w_current, (int) event->x);
		w_current->last_y = fix_y(w_current, (int) event->y);
		o_complex_rubbercomplex(w_current);
                break;

	case(DRAWATTRIB):
		o_attrib_start(w_current, (int) event->x, (int) event->y);
		w_current->event_state = ENDATTRIB;
		w_current->inside_action = 1;
		break;

	case(DRAWTEXT):
		o_text_start(w_current, (int) event->x, (int) event->y);
		w_current->event_state = ENDTEXT;
		w_current->inside_action = 1;
		break;

	case(ENDATTRIB):
		o_attrib_rubberattrib(w_current);
		w_current->last_x = fix_x(w_current, (int) event->x);
		w_current->last_y = fix_y(w_current, (int) event->y);
		o_attrib_rubberattrib(w_current);
		break;

	case(ENDTEXT):
		o_text_rubberattrib(w_current);
		w_current->last_x = fix_x(w_current, (int) event->x);
		w_current->last_y = fix_y(w_current, (int) event->y);
		o_text_rubberattrib(w_current);
		break;

	case(STARTSELECT):
		temp_x = fix_x(w_current, (int) event->x);
		temp_y = fix_y(w_current, (int) event->y);
		/* is eight enough of a threshold? */
		/* make this configurable anyways */
		diff_x = fabs(w_current->page_current->right -
			      w_current->page_current->left);

#ifdef HAS_RINT
		zoom_scale = (int) rint(w_current->init_right / diff_x);
#else
		zoom_scale = (int) w_current->init_right / diff_x;
#endif

		if (zoom_scale < 10) {
			zoom_scale = 10;
		}

		if ( (abs(temp_x - w_current->start_x) > zoom_scale) ||
		     (abs(temp_y - w_current->start_y) > zoom_scale) ) {
			w_current->event_state = SBOX;
			i_sbox_start(w_current,
				     (int) event->x,
				     (int) event->y);
			w_current->inside_action = 1;
		}
		break;

	case(SBOX):
		if (w_current->inside_action)
			i_sbox_rubberbox(w_current,
					 (int) event->x,
					 (int) event->y);
		break;

	case(ZOOMBOXEND):
		if (w_current->inside_action)
			a_zoom_box_rubberband( w_current,
					       (int) event->x,
					       (int) event->y);
		break;

	}
	return(0);
}

gint
x_event_configure(GtkWidget *widget, GdkEventConfigure *event,
	TOPLEVEL *w_current)
{
	int new_height, new_width;

	/* this callback is for drawing areas only! */
	/* things like changing a label causes a resize */
	/* this code is no longer needed.*/

#if DEBUG
	printf("RESIZE\n");
#endif

	/* get the new width/height */
	new_width  = widget->allocation.width;
        new_height = widget->allocation.height;

	/* if it's equal to the current width/height don't do anything */
	if (new_width == w_current->win_width &&
    	    new_height == w_current->win_height) {
		return(0);
	}

#if 0 /* my experiments with getting resize to work differently */
	diff_width  = (new_width - w_current->win_width)*100;
	diff_height = (new_height - w_current->win_height)*100;

	printf("diff %d %d\n", diff_width, diff_height);

	printf("world %d %d\n", SCREENabs(w_current, diff_width),
	       SCREENabs(w_current, diff_height));

	w_current->page_current->right = w_current->page_current->right + diff_width;
	w_current->page_current->bottom = w_current->page_current->bottom + diff_height;

	diff_x =
		w_current->page_current->right -
		w_current->page_current->left;
	diff_y =
		w_current->page_current->bottom -
		w_current->page_current->top;

	new_aspect =
		(float) fabs(w_current->page_current->right -
			     w_current->page_current->left) /
		(float) fabs(w_current->page_current->bottom -
			     w_current->page_current->top);

#if DEBUG
	printf("wxh: %d %d\n", diff_x, diff_y);
        printf("diff is: %f\n", fabs(new_aspect - coord_aspectratio));
#endif

	/* Make sure aspect ratio is correct */
	if (fabs(new_aspect - w_current->page_current->coord_aspectratio)) {
		if (new_aspect > w_current->page_current->coord_aspectratio) {
#if DEBUG
			printf("new larger then coord\n");
			printf("implies that height is too large\n");
#endif
			w_current->page_current->bottom =
				w_current->page_current->top +
				(w_current->page_current->right -
				 w_current->page_current->left) /
				w_current->page_current->coord_aspectratio;
		} else {
#if DEBUG
			printf("new smaller then coord\n");
			printf("implies that width is too small\n");
#endif
			w_current->page_current->right =
				w_current->page_current->left +
				(w_current->page_current->bottom -
				 w_current->page_current->top) *
				w_current->page_current->coord_aspectratio;
                }
#if DEBUG
                printf("invalid aspectratio corrected\n");
#endif
        }

#endif

	/* of the actual win window (drawing_area) */
	w_current->win_width  = widget->allocation.width;
        w_current->win_height = widget->allocation.height;

	w_current->width  = w_current->win_width;
	w_current->height = w_current->win_height;

	/* need to do this every time you change width / height */
	set_window(w_current,
		   w_current->page_current->left,
		   w_current->page_current->right,
		   w_current->page_current->top,
		   w_current->page_current->bottom);

	if (w_current->backingstore) {
		gdk_pixmap_unref(w_current->backingstore);
        }

	w_current->backingstore = gdk_pixmap_new(widget->window,
						 widget->allocation.width,
						 widget->allocation.height,
						 -1);

	if (!w_current->DONT_REDRAW)
		o_redraw_all(w_current);

	return(0);
}

/* this is used during an open command */
/* to setup the correct sizes */
void
x_manual_resize(TOPLEVEL *w_current)
{
	/* of the actual win window (drawing_area) */
	w_current->win_width  = w_current->drawing_area->allocation.width;
        w_current->win_height = w_current->drawing_area->allocation.height;

#if DEBUG
	printf("manual: %d %d\n", win_width, win_height);
#endif

	w_current->width = w_current->win_width;
	w_current->height = w_current->win_height;

	/* need to do this every time you change width / height */
	set_window(w_current,
		   w_current->page_current->left,
		   w_current->page_current->right,
		   w_current->page_current->top,
		   w_current->page_current->bottom);

#if DEBUG
	printf("Coord aspect: %f Window aspect: %f\n",
	       coord_aspectratio,
	       (float) win_width / (float) win_height);
        printf("w: %d h: %d\n", width, height);
        printf("aw: %d ah: %d\n", win_width, win_height);
#endif

	/* I'm assuming that the backingstore pixmap is of the right
	 * size */
}

void
x_event_hschanged (GtkAdjustment *adj, TOPLEVEL *w_current)
{
	int current_left;
	int new_left;
        GtkAdjustment        *hadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

        hadjustment =
		gtk_range_get_adjustment(GTK_RANGE(w_current->h_scrollbar));

	current_left = w_current->page_current->left;
        new_left = (int) hadjustment->value;

	if (!w_current->DONT_RECALC) {
		w_current->page_current->left = new_left;
		w_current->page_current->right =
			w_current->page_current->right -
			(current_left - new_left);
	}

	if (!w_current->DONT_REDRAW) {
		o_redraw_all(w_current);
	}
}

void
x_event_vschanged (GtkAdjustment *adj, TOPLEVEL *w_current)
{
	int current_bottom;
	int new_bottom;
        GtkAdjustment        *vadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

        vadjustment = gtk_range_get_adjustment(
		GTK_RANGE(w_current->v_scrollbar));

	current_bottom = w_current->page_current->bottom;
        new_bottom = w_current->init_bottom - (int) vadjustment->value;

	if (!w_current->DONT_RECALC) {
		w_current->page_current->bottom = new_bottom;
		w_current->page_current->top =
			w_current->page_current->top -
			(current_bottom - new_bottom);
	}

#if DEBUG
	printf("vrange %f %f\n", vadjustment->lower, vadjustment->upper);
	printf("vvalue %f\n", vadjustment->value);
	printf("actual: %d %d\n", top, bottom);
#endif

	if (!w_current->DONT_REDRAW) {
		o_redraw_all(w_current);
	}
}

gint
x_event_enter(GtkWidget *widget, GdkEventCrossing *event,
	TOPLEVEL *w_current)
{
	/* do nothing or now */
	return(0);
}

gint
x_event_key_press (GtkWidget *widget, GdkEventKey *event, TOPLEVEL *w_current)
{
	exit_if_null(w_current);
	global_window_current = w_current;

	set_window_current_key(w_current);

	if (event) {
		g_key_execute(event->state, event->keyval);
	}

	return(0);
}
