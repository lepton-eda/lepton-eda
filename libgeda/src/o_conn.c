/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "s_passing.h"
#include "o_types.h"
#include "colors.h"

#include "../include/prototype.h"


/* CONNECTION */
/* This file is completely removed */
#if 0
OBJECT *
o_CONN_search(int x, int y, int sid, OBJECT *head, int *whichone)
{
	OBJECT *o_current=NULL;
	OBJECT *ret=NULL;
	int local_whichone;
	int min_x, min_y, max_x, max_y;

	o_current = head; 

	while (o_current != NULL) {

		switch(o_current->type) {

			case(OBJ_PIN):
				if ( (o_current->line_points->x1 == x) &&
					(o_current->line_points->y1 == y) && 
				        (o_current->sid != sid) ) {
					*whichone = 1;
					return(o_current);
				}

				if ( (o_current->line_points->x2 == x) &&
					(o_current->line_points->y2 == y) &&
					(o_current->sid != sid) ) {
					*whichone = 2;
					return(o_current);
				}

				break;

			case(OBJ_NET):
				if ( (o_current->line_points->x1 == x) &&
					(o_current->line_points->y1 == y) && 
				        (o_current->sid != sid) ) {
					*whichone = 1;
					return(o_current);
				}

				if ( (o_current->line_points->x2 == x) &&
					(o_current->line_points->y2 == y) &&
					(o_current->sid != sid) ) {
					*whichone = 2;
					return(o_current);
				}

				min_y = min(o_current->line_points->y1, o_current->line_points->y2); 
				max_y = max(o_current->line_points->y1, o_current->line_points->y2); 

#if DEBUG
printf("first\nx1==x: %d y>m_min_y: %d y<max_y: %d\n\n", o_current->line_points->x1 == x, y > min_y, y < max_y);
#endif

				/* vertical */
				if ( (o_current->line_points->x1 == x) &&
					(y > min_y) && (y < max_y) &&
					(o_current->line_points->x1 == 
						o_current->line_points->x2) ) {
					*whichone = 3;
					return(o_current);
				}

				min_x = min(o_current->line_points->x1, o_current->line_points->x2); 
				max_x = max(o_current->line_points->x1, o_current->line_points->x2); 
		
#if DEBUG	
printf("second\ny1==y: %d x>m_min_x: %d x<max_x: %d\n\n", o_current->line_points->y1 == y, x > min_x, x < max_x);
#endif
				/* horizontal */
				if ( (o_current->line_points->y1 == y) &&
					(x > min_x) && (x < max_x) &&
					(o_current->line_points->y1 == 
						o_current->line_points->y2) ) {
					*whichone = 3;
					return(o_current);
				}
		
				break;

			case(OBJ_COMPLEX): 
				ret = o_conn_search(x, y, 
						sid, o_current->complex, 
						&local_whichone);

				if (ret != NULL) {
					*whichone = local_whichone;
					return(ret);
				}
					
				break;

		}

		o_current = o_current->next;
	}	

	*whichone = 0;
	return(NULL);
}

/* remove redraws hmmmm... */
/* libhack */
void
o_conn_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{

	if (o_current == NULL)
		return;
	
	switch(o_current->type) {

		case(OBJ_PIN):
			o_pin_conn_recalc(w_current, o_current); 
			o_pin_conn_recalc(w_current, o_current->connected_to_1); 
			o_pin_conn_recalc(w_current, o_current->connected_to_2); 
			o_redraw_single(w_current, o_current->connected_to_1);
			o_redraw_single(w_current, o_current->connected_to_2); 
			o_redraw_single(w_current, o_current); 
			break;

		case(OBJ_NET):
			o_net_conn_recalc(w_current, o_current);
			o_net_conn_recalc(w_current, o_current->connected_to_1); 
			o_net_conn_recalc(w_current, o_current->connected_to_2); 
			o_redraw_single(w_current, o_current->connected_to_1);
			o_redraw_single(w_current, o_current->connected_to_2);
			o_redraw_single(w_current, o_current); 
			break;

		case(OBJ_COMPLEX): 
			o_conn_recalc(w_current, o_current->complex); 
			break;
	}
}

void
o_conn_recalc_all(TOPLEVEL *w_current, OBJECT *head)
{
	OBJECT *o_current=NULL;

	o_current = head; 
	
	while (o_current != NULL) {

		switch(o_current->type) {

			case(OBJ_PIN):
				o_pin_conn_recalc(w_current, o_current);
				break;

			case(OBJ_NET):
				o_net_conn_recalc(w_current, o_current);
				break;

			case(OBJ_COMPLEX): 
				o_conn_recalc(w_current, o_current->complex); 
				break;

		}
		o_current = o_current->next;
	}	
}


/* this is experimental, and needs to be tested further */
void
o_conn_disconnect_all(OBJECT *list_head, OBJECT *object)
{
	OBJECT *o_current=NULL;

	o_current = list_head; 

	while (o_current != NULL) {

		switch(o_current->type) {

			case(OBJ_PIN):
			case(OBJ_NET):
				if (o_current->connected_to_1 == object) {
					o_current->connected_to_1 = NULL;
				}

				if (o_current->connected_to_2 == object) {
					o_current->connected_to_2 = NULL;
				}

				break;


			case(OBJ_COMPLEX): 
				o_conn_disconnect_all(o_current->complex, object); 
				break;

		}
		o_current = o_current->next;
	}	
}
#endif
