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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGDGEDA
#include <gdgeda/gd.h>
#endif

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"

#include "colors.h"
#include "funcs.h"

#include "../include/prototype.h"


void
get_net_bounds(TOPLEVEL *w_current, LINEPTS *points, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->width;
	*top = w_current->height;
	*right = 0;
	*bottom = 0;

	if (points->screen_x1 < *left) *left = points->screen_x1;
	if (points->screen_x1 > *right) *right = points->screen_x1;
	if (points->screen_y1 < *top) *top = points->screen_y1;
	if (points->screen_y1 > *bottom) *bottom = points->screen_y1;

	if (points->screen_x2 < *left) *left = points->screen_x2;
	if (points->screen_x2 > *right) *right = points->screen_x2;
	if (points->screen_y2 < *top) *top = points->screen_y2;
	if (points->screen_y2 > *bottom) *bottom = points->screen_y2;

	*left = *left - 4;
	*top = *top - 4;

	*right = *right + 4;
	*bottom = *bottom + 4;
}

void
world_get_net_bounds(TOPLEVEL *w_current, LINEPTS *points, int *left, int *top, int *right, int *bottom)
{
	*left = w_current->init_right;
        *top = w_current->init_bottom;
        *right = 0;
        *bottom = 0;

	if (points->x1 < *left) *left = points->x1;
	if (points->x1 > *right) *right = points->x1;
	if (points->y1 < *top) *top = points->y1;
	if (points->y1 > *bottom) *bottom = points->y1;

	if (points->x2 < *left) *left = points->x2;
	if (points->x2 > *right) *right = points->x2;
	if (points->y2 < *top) *top = points->y2;
	if (points->y2 > *bottom) *bottom = points->y2;

}


OBJECT *
o_net_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x1, int y1, int x2, int y2)
{
	int screen_x, screen_y;
	int left, right, top, bottom;
	OBJECT *new_node;

	new_node = s_basic_init_object("net");
	new_node->type = type;
	new_node->color = color;

	new_node->line_points = (LINEPTS *) malloc(sizeof(LINEPTS));
/* check for null */	

	new_node->line_points->x1 = x1;
	new_node->line_points->y1 = y1;
	new_node->line_points->x2 = x2;
	new_node->line_points->y2 = y2;

	WORLDtoSCREEN(w_current, 
		  new_node->line_points->x1, new_node->line_points->y1, 
		  &screen_x,
                  &screen_y);  
	
	new_node->line_points->screen_x1 = screen_x;
	new_node->line_points->screen_y1 = screen_y;

	WORLDtoSCREEN(w_current, 
		  new_node->line_points->x2, new_node->line_points->y2, 
		  &screen_x,
                  &screen_y);  

	new_node->line_points->screen_x2 = screen_x;
	new_node->line_points->screen_y2 = screen_y;

	get_net_bounds(w_current, new_node->line_points, 
			&left, &top, &right, &bottom);
	
	new_node->left = left;
	new_node->top = top;
	new_node->right = right;
	new_node->bottom = bottom;	

	/* TODO: questionable cast */
	new_node->draw_func = (void *) net_draw_func;  
	/* TODO: questionable cast */
	new_node->sel_func = (void *) select_func;  

	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

	/* new CONN stuff */
	if (!w_current->ADDING_SEL) {
		o_conn_update(w_current->page_current, object_list);
	}

	return(object_list);
}

void
o_net_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (o_current == NULL) {
		return;
	}

	if (o_current->line_points == NULL) {
		return;
	}

	WORLDtoSCREEN(w_current, o_current->line_points->x1, 
		  o_current->line_points->y1, 
		  &screen_x1,
                  &screen_y1);  

	o_current->line_points->screen_x1 = screen_x1;
	o_current->line_points->screen_y1 = screen_y1;

	WORLDtoSCREEN(w_current, o_current->line_points->x2, 
		  o_current->line_points->y2, 
		  &screen_x2,
                  &screen_y2);  

	o_current->line_points->screen_x2 = screen_x2;
	o_current->line_points->screen_y2 = screen_y2;

	get_net_bounds(w_current, o_current->line_points, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;


}


OBJECT *
o_net_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
{
	char type; 
	int x1, y1;
	int x2, y2;
	int d_x1, d_y1;
	int d_x2, d_y2;
	int color;

	sscanf(buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color);	
	d_x1 = x1; 
	d_y1 = y1; 
	d_x2 = x2; 
	d_y2 = y2; 

	if (x1 == x2 && y1 == y2) {
		fprintf(stderr, "Found a zero length net [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
		s_log_message("Found a zero length net [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
	}


	if (w_current->override_net_color != -1) {
                color = w_current->override_net_color;
        }
              

	object_list = o_net_add(w_current, object_list, type, color, d_x1, d_y1, d_x2, d_y2);
	return(object_list);
}

char *
o_net_save(char *buf, OBJECT *object)
{
	int x1, x2, y1, y2;
	int color;

        x1 = object->line_points->x1;
        y1 = object->line_points->y1;
        x2 = object->line_points->x2;
        y2 = object->line_points->y2;

	color = object->color;

        sprintf(buf, "%c %d %d %d %d %d", object->type,
                        x1, y1, x2, y2, color);
        return(buf);
}
       

void
o_net_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;

	if (object == NULL) printf("nt NO!\n");


	/* Do world coords */
	object->line_points->screen_x1 = object->line_points->screen_x1 + dx;
	object->line_points->screen_y1 = object->line_points->screen_y1 + dy;
	object->line_points->screen_x2 = object->line_points->screen_x2 + dx;
	object->line_points->screen_y2 = object->line_points->screen_y2 + dy;

	/* do we want snap grid here? */
	SCREENtoWORLD(w_current, object->line_points->screen_x1, 
		  object->line_points->screen_y1, 
		  &x,
                  &y);  
	
	object->line_points->x1 = snap_grid(w_current, x);
	object->line_points->y1 = snap_grid(w_current, y);
	
	SCREENtoWORLD(w_current, object->line_points->screen_x2, 
		  object->line_points->screen_y2, 
		  &x,
                  &y);  
	
	object->line_points->x2 = snap_grid(w_current, x);
	object->line_points->y2 = snap_grid(w_current, y);
}


void
o_net_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int screen_x1, screen_y1;
	int screen_x2, screen_y2;	
	int left, right, top, bottom;

	if (object == NULL) printf("ntw NO!\n");


	/* Do world coords */
	object->line_points->x1 = object->line_points->x1 + x1;
	object->line_points->y1 = object->line_points->y1 + y1;
	object->line_points->x2 = object->line_points->x2 + x1;
	object->line_points->y2 = object->line_points->y2 + y1;

	/* update screen coords */
	WORLDtoSCREEN(w_current, object->line_points->x1, 
		  object->line_points->y1, 
		  &screen_x1,
                  &screen_y1);  

	object->line_points->screen_x1 = screen_x1;
	object->line_points->screen_y1 = screen_y1;

	WORLDtoSCREEN(w_current, object->line_points->x2, 
		  object->line_points->y2, 
		  &screen_x2,
                  &screen_y2);  

	object->line_points->screen_x2 = screen_x2;
	object->line_points->screen_y2 = screen_y2;

	/* update bounding box */
	get_net_bounds(w_current, object->line_points, &left, &top, &right, &bottom);

	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}


OBJECT *
o_net_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;
	ATTRIB *a_current;

	/* CONN stuff... */
	/* make sure you fix this in pin and bus as well */
	/* still doesn't work... you need to pass in the new values */
	/* or don't update and update later */
	/* I think for now I'll disable the update and manually update */
	new_obj = o_net_add(w_current, list_tail, OBJ_NET, o_current->color, 
			0,0,0,0);

	new_obj->line_points->screen_x1 = o_current->line_points->screen_x1;
	new_obj->line_points->screen_y1 = o_current->line_points->screen_y1;
	new_obj->line_points->screen_x2 = o_current->line_points->screen_x2;
	new_obj->line_points->screen_y2 = o_current->line_points->screen_y2;

	new_obj->line_points->x1 = o_current->line_points->x1;
	new_obj->line_points->y1 = o_current->line_points->y1;
	new_obj->line_points->x2 = o_current->line_points->x2;
	new_obj->line_points->y2 = o_current->line_points->y2;

	a_current = o_current->attribs;

	if (a_current && !w_current->ADDING_SEL) {
		while ( a_current ) {

			/* head attrib node has prev = NULL */
			if (a_current->prev != NULL) {
				a_current->copied_to = new_obj;
			}
			a_current = a_current->next;
		}
	}

	return(new_obj);
}

void
o_net_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
	int origin_x, int origin_y)
{
	OBJECT *bus_object;
	int cue;
	int offset, offset2;
	int cross;
	int x1, y1;
	int x2, y2;

	if (o_current == NULL) {
		printf("got null in o_net_print\n");
		return;
	}

	offset = 7*6;
        offset2 = 7;  

	cross = offset;

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	fprintf(fp, "gsave\n");
	fprintf(fp, "newpath\n");
	if (w_current->net_style == THICK) {
		fprintf(fp, "1.5 setlinewidth\n");	
	}

	x1 = o_current->line_points->x1-origin_x,
	y1 = o_current->line_points->y1-origin_y;
	x2 = o_current->line_points->x2-origin_x,
	y2 = o_current->line_points->y2-origin_y;

	fprintf(fp, "%d mils %d mils moveto\n", x1, y1);
	fprintf(fp, "%d mils %d mils lineto\n", x2, y2);

	fprintf(fp, "stroke\n");

	/* maybe move this inside, so it doesn't clutter the ps file */
	if (w_current->print_color) {
		f_print_set_color(fp, w_current->net_endpoint_color);
	}


	cue = o_conn_query_table(w_current->page_current->conn_table,
				o_current->line_points->x1,
				o_current->line_points->y1);

	switch(cue) {

	  case(NET_DANGLING_CUE):

		switch(w_current->net_endpoint_mode) {

			/* hack clean this up so that you save some code */	
			case(FILLEDBOX): 
				fprintf(fp, "newpath\n");
				fprintf(fp, "%d mils %d mils moveto\n",
					x1-offset, y1-offset);
				fprintf(fp, "%d %d fbox\n", offset2, offset2);
				break;

			case(EMPTYBOX): 
				fprintf(fp, "newpath\n");
				fprintf(fp, "%d mils %d mils moveto\n",
					x1-offset, y1-offset);
				fprintf(fp, "%d %d box\n", offset2, offset2);
				break;

			case(X):
				fprintf(fp, "newpath\n");
        			fprintf(fp, "%d mils %d mils moveto\n", 
					x1-cross, y1-cross);
        			fprintf(fp, "%d mils %d mils lineto\n", 
					x1+cross, y1+cross);
        			fprintf(fp, "stroke\n");                                              
				fprintf(fp, "newpath\n");
        			fprintf(fp, "%d mils %d mils moveto\n", 
					x1-cross, y1+cross);
        			fprintf(fp, "%d mils %d mils lineto\n", 
					x1+cross, y1-cross);
        			fprintf(fp, "stroke\n");
				break;

			case(NONE):
				break;			

			default:
				fprintf(stderr, "Unexpected endpoint mode\n");
			
		}
	   break;
		
	   case(MIDPOINT_CUE):
		if (w_current->net_midpoint_mode != NONE) {

			fprintf(fp, "newpath\n");
			fprintf(fp, "%d mils %d mils\n", x1, y1); 
			fprintf(fp, "%d\n", offset2/2);
        		fprintf(fp, "0 360 arc\n");

			if (w_current->net_midpoint_mode == FILLED) {
		        	fprintf(fp, "fill\n");      
			} else if (w_current->net_midpoint_mode == EMPTY) {
        			fprintf(fp, "stroke\n");      
			}
		}
	   break;

	   case(BUS_MIDPOINT_CUE):

			bus_object = o_conn_return_bus_object(w_current->
							      page_current->
							      conn_table,
							      o_current->
							      line_points->x1,
							      o_current->
							      line_points->y1);

			
			if (!bus_object) {
				fprintf(stderr, "Got a null bus_object!\n");
			} else {
				o_conn_print_busmidpoint(w_current, bus_object,
							fp, 
						        o_current->
						        line_points->x1,
							o_current->
						        line_points->y1,
							o_current->
							line_points->x2,
							o_current->
							line_points->y2);
			}
	   break;
	}

	cue = o_conn_query_table(w_current->page_current->conn_table,
				o_current->line_points->x2,
				o_current->line_points->y2);

	switch(cue) {

	  case(NET_DANGLING_CUE):

		switch(w_current->net_endpoint_mode) {
	
			/* clean this up so that you save some code */	
			case(FILLEDBOX):
				fprintf(fp, "newpath\n");
				fprintf(fp, "%d mils %d mils moveto\n",
					x2-offset, y2-offset);
				fprintf(fp, "%d %d fbox\n", offset2, offset2);
				break;

			case(EMPTYBOX):
				fprintf(fp, "newpath\n");
				fprintf(fp, "%d mils %d mils moveto\n",
					x2-offset, y2-offset);
				fprintf(fp, "%d %d box\n", offset2, offset2);
				break;
			
			case(X):
				fprintf(fp, "newpath\n");
        			fprintf(fp, "%d mils %d mils moveto\n", 
					x2-cross, y2-cross);
        			fprintf(fp, "%d mils %d mils lineto\n", 
					x2+cross, y2+cross);
        			fprintf(fp, "stroke\n");                                              
				fprintf(fp, "newpath\n");
        			fprintf(fp, "%d mils %d mils moveto\n", 
					x2-cross, y2+cross);
        			fprintf(fp, "%d mils %d mils lineto\n", 
					x2+cross, y2-cross);
        			fprintf(fp, "stroke\n"); 
				break;

			case(NONE):
				break;

			default:
				fprintf(stderr, "Unexpected endpoint mode\n");
			
		}
	   break;	

	   case(MIDPOINT_CUE):
		if (w_current->net_midpoint_mode != NONE) {

			fprintf(fp, "newpath\n");
			fprintf(fp, "%d mils %d mils\n", x2, y2); 
			fprintf(fp, "%d \n", offset2/2);
        		fprintf(fp, "0 360 arc\n");

			if (w_current->net_midpoint_mode == FILLED) {
        			fprintf(fp, "fill\n");      
			} else if (w_current->net_midpoint_mode == EMPTY) {
        			fprintf(fp, "stroke\n");      
			}
		}
           break;

	   case(BUS_MIDPOINT_CUE):

			bus_object = o_conn_return_bus_object(w_current->
							      page_current->
							      conn_table,
							      o_current->
							      line_points->x2,
							      o_current->
							      line_points->y2);

			
			if (!bus_object) {
				fprintf(stderr, "Got a null bus_object!\n");
			} else {
				o_conn_print_busmidpoint(w_current, bus_object,
							fp, 
						        o_current->
						        line_points->x2,
							o_current->
						        line_points->y2,
							o_current->
							line_points->x1,
							o_current->
							line_points->y1);
			}


	   break;
	}
	fprintf(fp, "grestore\n");

}

void
o_net_image_write(TOPLEVEL *w_current, OBJECT *o_current,
        int origin_x, int origin_y, int color_mode)
{
	int offset, offset2;
	int cross;
	int x1, y1;
	int x2, y2;
	int cue;
	int endpoint_color;
        int color;
	int i;
	OBJECT *bus_object;

        if (o_current == NULL) {
                printf("got null in o_net_image_write\n");
                return;
        }

	if (color_mode == TRUE) {
		color = o_image_geda2gd_color(o_current->color);
	} else {
		color = image_black;
	}

	offset = SCREENabs(w_current, 30);

/* 
	offset = 7 * (float) w_current->height/ (float) w_current->width;
        offset2 = 7 * (float) w_current->height/ (float) w_current->width*2;  

	printf("%f %d %d\n", (float) ( (float) w_current->height/ (float) w_current->width), 
				offset, offset2);
*/

	offset2 = offset*2;

	cross = offset;

	x1 = o_current->line_points->screen_x1;
	y1 = o_current->line_points->screen_y1;
	x2 = o_current->line_points->screen_x2;
	y2 = o_current->line_points->screen_y2;

        /* assumes screen coords are already calculated correctly */
#ifdef HAS_LIBGDGEDA
        gdImageLine(current_im_ptr, x1, y1, x2, y2, color);
#endif

	cue = o_conn_query_table(w_current->page_current->conn_table,
				o_current->line_points->x1,
				o_current->line_points->y1);

	if (color_mode == TRUE) {
		endpoint_color = o_image_geda2gd_color(w_current->net_endpoint_color);
	} else {
		endpoint_color = image_black;
	}

	switch(cue) {

	  case(NET_DANGLING_CUE):

		switch(w_current->net_endpoint_mode) {

			/* hack clean this up so that you save some code */	
			case(FILLEDBOX): 
#ifdef HAS_LIBGDGEDA
				gdImageFilledRectangle(current_im_ptr, 
						x1-offset, y1-offset, 
					        x1-offset+offset2, 
						y1-offset+offset2,
						endpoint_color);
#endif
			break;

			case(EMPTYBOX): 
#ifdef HAS_LIBGDGEDA
				gdImageRectangle(current_im_ptr, 
						x1-offset, y1-offset, 
					        x1-offset+offset2, 
						y1-offset+offset2,
						endpoint_color);
#endif
			break;

			case(X):
#ifdef HAS_LIBGDGEDA
        			gdImageLine(current_im_ptr, x1-cross, y1-cross,
						x1+cross, y1+cross,
						endpoint_color);
        			gdImageLine(current_im_ptr, x1-cross, y1+cross,
						x1+cross, y1-cross,
						endpoint_color);
#endif
			break;

			case(NONE):
				break;			

			default:
				fprintf(stderr, "Unexpected endpoint mode\n");
			
		}
	   break;
	
	   case(MIDPOINT_CUE):
		if (w_current->net_midpoint_mode != NONE) {
			if (w_current->net_midpoint_mode == FILLED) {
#ifdef HAS_LIBGDGEDA
				gdImageArc(current_im_ptr, x1, y1, 
					   offset2*1.25, offset2*1.25, 
					   0, 360, endpoint_color);
				for (i = 0 ; i < offset2*1.25; i++) {
					gdImageArc(current_im_ptr, x1, y1,
                                           i, i, 0, 360, endpoint_color);
				}
#endif
			} else if (w_current->net_midpoint_mode == EMPTY) {
#ifdef HAS_LIBGDGEDA
				gdImageArc(current_im_ptr, x1, y1, 
					   offset2*1.25, offset2*1.25, 
					   0, 360, endpoint_color);
#endif
			}
		}
           break;

	   case(BUS_MIDPOINT_CUE):

			bus_object = o_conn_return_bus_object(w_current->
							      page_current->
							      conn_table,
							      o_current->
							      line_points->x1,
							      o_current->
							      line_points->y1);

			
			if (!bus_object) {
				fprintf(stderr, "Got a null bus_object!\n");
			} else {
				o_conn_image_busmidpoint(w_current, bus_object,
						        x1, y1,
							o_current->
							line_points->x2,
							o_current->
							line_points->y2);
			}
	   break;
	}

	cue = o_conn_query_table(w_current->page_current->conn_table,
				o_current->line_points->x2,
				o_current->line_points->y2);

	switch(cue) {

	  case(NET_DANGLING_CUE):

		switch(w_current->net_endpoint_mode) {
	
			/* clean this up so that you save some code */	
			case(FILLEDBOX):
#ifdef HAS_LIBGDGEDA
				gdImageFilledRectangle(current_im_ptr, 
						x2-offset, y2-offset, 
					        x2-offset+offset2, 
						y2-offset+offset2,
						endpoint_color);
#endif
				break;

			case(EMPTYBOX): 
#ifdef HAS_LIBGDGEDA
				gdImageRectangle(current_im_ptr, 
						x2-offset, y2-offset, 
					        x2-offset+offset2, 
						y2-offset+offset2,
						endpoint_color);
#endif
			break;

			case(X):
#ifdef HAS_LIBGDGEDA
        			gdImageLine(current_im_ptr, x2-cross, y2-cross,
						x2+cross, y2+cross,
						endpoint_color);
        			gdImageLine(current_im_ptr, x2-cross, y2+cross,
						x2+cross, y2-cross,
						endpoint_color);
#endif
			break;
			
			case(NONE):
				break;

			default:
				fprintf(stderr, "Unexpected endpoint mode\n");
			
		}
	   break;	

	   case(MIDPOINT_CUE):
		if (w_current->net_midpoint_mode != NONE) {
			if (w_current->net_midpoint_mode == FILLED) {
#ifdef HAS_LIBGDGEDA
				gdImageArc(current_im_ptr, x2, y2, 
					   offset2*1.25, offset2*1.25, 
					   0, 360, endpoint_color);

				for (i = 0 ; i < offset2*1.25; i++) {
					gdImageArc(current_im_ptr, x2, y2,
                                           i, i, 0, 360, endpoint_color);
				}
#endif
			
			} else if (w_current->net_midpoint_mode == EMPTY) {
#ifdef HAS_LIBGDGEDA
				gdImageArc(current_im_ptr, x2, y2, 
				 	   offset2*1.25, offset2*1.25, 
					   0, 360, endpoint_color);
#endif
			}
		}
           break;

	   case(BUS_MIDPOINT_CUE):

			bus_object = o_conn_return_bus_object(w_current->
							      page_current->
							      conn_table,
							      o_current->
							      line_points->x2,
							      o_current->
							      line_points->y2);

			
			if (!bus_object) {
				fprintf(stderr, "Got a null bus_object!\n");
			} else {
				o_conn_image_busmidpoint(w_current, bus_object,
						        x2, y2,
							o_current->
							line_points->x1,
							o_current->
							line_points->y1);
			}
	   break;
	}
}


/* takes in screen coordinates for the centerx,y, and then does the rotate 
 * in world space */
/* also ignores angle argument... for now, rotate only in 90 degree 
 * increments */
/* fully functional */
void
o_net_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle,
	OBJECT *object)
{
	int world_centerx, world_centery;
	int newx, newy;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_net_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line_points->x1, object->line_points->y1, angle,
			&newx, &newy);

	object->line_points->x1 = newx;
	object->line_points->y1 = newy;

	rotate_point_90(object->line_points->x2, object->line_points->y2, angle,
			&newx, &newy);

	object->line_points->x2 = newx;
	object->line_points->y2 = newy;

	o_net_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_net_rotate_world(TOPLEVEL *w_current, 
	int world_centerx, int world_centery, int angle,
	OBJECT *object)
{
	int newx, newy;

	if (angle == 0)
		return;

	/* translate object to origin */
	o_net_translate_world(w_current, -world_centerx, -world_centery, object);

	rotate_point_90(object->line_points->x1, object->line_points->y1, angle,
			&newx, &newy);

	object->line_points->x1 = newx;
	object->line_points->y1 = newy;

	rotate_point_90(object->line_points->x2, object->line_points->y2, angle,
			&newx, &newy);

	object->line_points->x2 = newx;
	object->line_points->y2 = newy;

	o_net_translate_world(w_current, world_centerx, world_centery, object);
}


void
o_net_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int world_centerx, world_centery;

	SCREENtoWORLD(w_current, centerx, centery, 
		  &world_centerx,
                  &world_centery);  

	/* translate object to origin */
	o_net_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line_points->x1 = -object->line_points->x1;

	object->line_points->x2 = -object->line_points->x2;

	o_net_translate_world(w_current, world_centerx, world_centery, object);
}

void
o_net_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	/* translate object to origin */
	o_net_translate_world(w_current, -world_centerx, -world_centery, object);

	object->line_points->x1 = -object->line_points->x1;

	object->line_points->x2 = -object->line_points->x2;

	o_net_translate_world(w_current, world_centerx, world_centery, object);
}


int
o_net_orientation(OBJECT *object)
{
	if (object->line_points->y1 == object->line_points->y2) {
		return(HORIZONTAL);
	}

	if (object->line_points->x1 == object->line_points->x2) {
		return(VERTICAL);
	}

	return(NEITHER);	
}

/* this function does the actual work of making one net segment out of two */
/* connected segments */
/* The second object (del_object) is the object that should be deleted */
void
o_net_consolidate_lowlevel(OBJECT *object, OBJECT *del_object, int orient) 
{
	int temp1, temp2;
	int final1, final2;
	int changed=0;
	ATTRIB *tail;

#if DEBUG
	printf("o %d %d %d %d\n", object->line_points->x1, object->line_points->y1, object->line_points->x2, object->line_points->y2);
	printf("d %d %d %d %d\n", del_object->line_points->x1, del_object->line_points->y1, del_object->line_points->x2, del_object->line_points->y2);
#endif


	if (orient == HORIZONTAL) {

		temp1 = min(object->line_points->x1, 
				      del_object->line_points->x1);
		temp2 = min(object->line_points->x2, 
				      del_object->line_points->x2);

		final1 = min(temp1, temp2);

		temp1 = max(object->line_points->x1, 
				      del_object->line_points->x1);
		temp2 = max(object->line_points->x2, 
				      del_object->line_points->x2);

		final2 = max(temp1, temp2);

		object->line_points->x1 = final1;
		object->line_points->x2 = final2;
		changed=1;
	}

	if (orient == VERTICAL) {
		temp1 = min(object->line_points->y1, 
				      del_object->line_points->y1);
		temp2 = min(object->line_points->y2, 
				      del_object->line_points->y2);

		final1 = min(temp1, temp2);

		temp1 = max(object->line_points->y1, 
				      del_object->line_points->y1);
		temp2 = max(object->line_points->y2, 
				      del_object->line_points->y2);

		final2 = max(temp1, temp2);

		object->line_points->y1 = final1;
		object->line_points->y2 = final2;
		changed=1;
	}

#if DEBUG
	printf("fo %d %d %d %d\n", object->line_points->x1, object->line_points->y1, object->line_points->x2, object->line_points->y2);
#endif

	if (changed) {

		/* first check for attributes */
		if (del_object->attribs) {
			printf("yeah... del object has attributes\n");
			printf("reconnecting them to the right place\n");
			if (object->attribs) {

		printf("object DID have attributes\n");

#if 0
	printf("object->attribs\n");
	o_attrib_print(object->attribs);
	printf("--\n");
	printf("del_object->attribs\n");
	o_attrib_print(del_object->attribs);
	printf("--\n");
#endif
				tail = o_attrib_return_tail(object->attribs);

				/* skip over old attrib head */
				tail->next = del_object->attribs->next;

				/* step prev object to point to last object */
				tail->next->prev = tail; 


				/* delete old attrib head */
				/* and nothing else */
				del_object->attribs->object=NULL;
				del_object->attribs->next=NULL;
				del_object->attribs->prev=NULL;
				o_attrib_delete(del_object->attribs);

				/* you don't need to free the attribs list */
				/* since it's been relinked into object's */
				/* attribs list */

				del_object->attribs = NULL;
#if 0
	printf("\n\nfinal object->attribs\n");
	o_attrib_print(object->attribs);
	printf("--\n");
#endif

			} else {

		printf("object didn't have any attributes\n");
				object->attribs = del_object->attribs;
/* TODO: what should this be? */
				object->attribs->prev = NULL;

				/* setup parent attribute */
				object->attribs->object = object;

				/* you don't need to free the attribs list */
				/* since it's been used by object */
				
				del_object->attribs = NULL;
			}	
		}
	}

}


int 
o_net_consolidate_segments(TOPLEVEL *w_current, OBJECT *object)
{
	char *key=NULL;
	CONN *conn_list;
	CONN *c_current;
	int object_orient;
	int current_orient;
	int cue;
	int changed=0;

	if (object == NULL) {
		return(0);
	}

	object_orient = o_net_orientation(object);

	key = o_conn_return_key(object->line_points->x1,
                                object->line_points->y1);

	conn_list = g_hash_table_lookup(w_current->page_current->conn_table,
                                        key);

	if (conn_list) {
/* TODO: bus here as well? */
	  if (conn_list->visual_cue != MIDPOINT_CUE) {
		cue = conn_list->visual_cue;
                c_current = conn_list;
                while (c_current != NULL) {
                        if (c_current->object != NULL) {
				current_orient = o_net_orientation(c_current->object);

				if (current_orient == object_orient &&
				    c_current->object->sid != object->sid &&
				    object->type == OBJ_NET && 
				    c_current->object->type == OBJ_NET) {

#if DEBUG 
					printf("yeah, can connect %s and %s %d\n",
						object->name, 
						c_current->object->name, cue);
#endif

					o_net_consolidate_lowlevel(object, 
							c_current->object, 
							current_orient);

					changed++;
				
					s_delete(w_current, c_current->object);
			                o_conn_disconnect_update(w_current->page_current);
					o_net_recalc(w_current, object);
					w_current->page_current->object_tail = 	
						return_tail(w_current->page_current->object_head);
					free(key);
					return(-1);
				}

                        }                                                     
                        c_current = c_current->next;
                }
	  }
        }

	free(key);

	key = o_conn_return_key(object->line_points->x2,
                                object->line_points->y2);

	conn_list = g_hash_table_lookup(w_current->page_current->conn_table,
                                        key);

	if (conn_list) {
/* TODO: bus here as well? */
	  if (conn_list->visual_cue != MIDPOINT_CUE) {
		cue = conn_list->visual_cue;
                c_current = conn_list;
                while (c_current != NULL) {
                        if (c_current->object != NULL) {
				current_orient = o_net_orientation(c_current->object);

				if (current_orient == object_orient &&
				    c_current->object->sid != object->sid &&
				    object->type == OBJ_NET && 
				    c_current->object->type == OBJ_NET) {

#if DEBUG
					printf("yeah, can connect %s and %s %d\n",
						object->name, 
						c_current->object->name, cue);
#endif
					o_net_consolidate_lowlevel(object, 
							c_current->object,
							current_orient);
					changed++;
					s_delete(w_current, c_current->object);
			                o_conn_disconnect_update(w_current->page_current);
				
					o_net_recalc(w_current, object);
					w_current->page_current->object_tail = 	
						return_tail(w_current->page_current->object_head);
					free(key);
					return(-1);
				}

                        }                                                     
                        c_current = c_current->next;
                }
          }
        }

	free(key);

	return(changed);
}

void
o_net_consolidate(TOPLEVEL *w_current)
{
	OBJECT *o_current;
	int status = 0;

	o_current = w_current->page_current->object_head;

	while(o_current != NULL) {

		if (o_current->type == OBJ_NET) {
			status = o_net_consolidate_segments(w_current, o_current);
		}

		if (status == -1) {
			o_current = w_current->page_current->object_head;
			status = 0;
		} else {
			o_current = o_current->next;
		}
	}
}

void
o_net_modify(TOPLEVEL *w_current, OBJECT *object, 
	     int x, int y, int whichone)
{
	int screen_x, screen_y;
	int left, right, top, bottom;

	switch(whichone) {

	case(1):
		object->line_points->x1 = x;
		object->line_points->y1 = y;

		WORLDtoSCREEN(w_current, 
		              object->line_points->x1, 
			      object->line_points->y1, 
		  	      &screen_x, &screen_y);  
	
		object->line_points->screen_x1 = screen_x;
		object->line_points->screen_y1 = screen_y;
		break;

	case(2):
		object->line_points->x2 = x;
		object->line_points->y2 = y;

		WORLDtoSCREEN(w_current, 
		  	      object->line_points->x2, 
			      object->line_points->y2, 
		  	      &screen_x,
                              &screen_y);  

		object->line_points->screen_x2 = screen_x;
		object->line_points->screen_y2 = screen_y;
		break;
	}

	get_net_bounds(w_current, object->line_points, 
			&left, &top, &right, &bottom);
	
	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;	
}
