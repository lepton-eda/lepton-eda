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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <math.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "o_types.h"

#include "colors.h"
#include "funcs.h"

#include "../include/prototype.h"

void
get_complex_bounds(TOPLEVEL *w_current, OBJECT *complex, int *left, int *top, int *right, int *bottom)
{
	OBJECT *o_current=NULL;
	int rleft, rtop, rright, rbottom;
	
	*left = rleft = 999999;
	*top = rtop = 9999999;
	*right = rright = 0;
	*bottom = rbottom = 0;
	


	o_current = complex;
	
	while ( o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_LINE):
					get_line_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_NET):
					/* same as a line (diff name)*/
					get_net_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_BUS):
					/* same as a line (diff name)*/
					get_bus_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;
	
			case(OBJ_BOX):
					get_box_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_CIRCLE):
					get_circle_bounds(w_current, o_current->circle, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_COMPLEX):
					/* recursive objects ?*/
					get_complex_bounds(w_current, o_current->complex, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_NTEXT):
					/* only do bounding boxes for visble */
					/* you might lose some attrs though */
					if (o_current->visibility == VISIBLE) {
						get_ntext_bounds(w_current, o_current, &rleft, &rtop, &rright, &rbottom);
					}
			break;

			case(OBJ_PIN):
					get_pin_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
					break;

/* experimental mod */
/* actually more of a hack... since arcs have HUGE bounding boxes, and this */
/* is really effecting complex selects, well why not NOT use them in calcing */
/* the BB for the complex? */
/* BUG that is caused by this is that when you move a single arc and you */
/* have BB actionfeedback mode you don't see anything! hack */
/* */
/* try with a fix on the bounding box code ? ?? hack */
/* taken out again */
#if 0
			case(OBJ_ARC):
					get_arc_bounds(w_current, o_current, &rleft, &rtop, &rright, &rbottom);
					break;
#endif
			default:
					break;
		}

		if (rleft < *left) *left = rleft;
		if (rtop < *top) *top = rtop;
		if (rright > *right) *right = rright;
		if (rbottom > *bottom) *bottom = rbottom;
	

		o_current=o_current->next;
	}

}

void
world_get_complex_bounds(TOPLEVEL *w_current, OBJECT *complex, int *left, int *top, int *right, int *bottom)
{
	OBJECT *o_current=NULL;
	int rleft, rtop, rright, rbottom;
	
	*left = rleft = w_current->init_right;
	*top = rtop = w_current->init_bottom;;
	*right = rright = 0;
	*bottom = rbottom = 0;
	
	o_current = complex;
	
	while ( o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_LINE):
					world_get_line_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_NET):
					/* same as a line (diff name)*/
					world_get_net_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_BUS):
					/* same as a line (diff name)*/
					world_get_bus_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;
	
	
			case(OBJ_BOX):
					world_get_box_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_CIRCLE):
					world_get_circle_bounds(w_current, o_current->circle, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_COMPLEX):
					/* recursive objects ?*/
					world_get_complex_bounds(w_current, o_current->complex, &rleft, &rtop, &rright, &rbottom);
			break;

			case(OBJ_NTEXT):
					/* only do bounding boxes for visble */
					/* you might lose some attrs though */
					if (o_current->visibility == VISIBLE) {
						world_get_ntext_bounds(w_current, o_current, &rleft, &rtop, &rright, &rbottom);
					}
			break;

			case(OBJ_PIN):
					world_get_pin_bounds(w_current, o_current->line_points, &rleft, &rtop, &rright, &rbottom);
			break;

/* experimental mod */
/* actually more of a hack... since arcs have HUGE bounding boxes, and this */
/* is really effecting complex selects, well why not NOT use them in calcing */
/* the BB for the complex? */
/* BUG that is caused by this is that when you move a single arc and you */
/* have BB actionfeedback mode you don't see anything! hack */
/* */
/* try with a fix on the bounding box code ? ?? hack */
/* taken out again */
#if 0 
			case(OBJ_ARC):
					world_get_arc_bounds(w_current, o_current, &rleft, &rtop, &rright, &rbottom);
			break;
#endif

			default:
			break;
		}

		if (rleft < *left) *left = rleft;
		if (rtop < *top) *top = rtop;
		if (rright > *right) *right = rright;
		if (rbottom > *bottom) *bottom = rbottom;
	

		o_current=o_current->next;
	}

}
OBJECT *
add_head(void)
{
	OBJECT *new_node=NULL;

	new_node = s_basic_init_object("complex_head"); 

        new_node->type = OBJ_HEAD; /* make this of head type hack */

	/* don't need to do this for head nodes */
        /* ret = (OBJECT *) s_basic_link_object(new_node, NULL);*/
	return(new_node);
}

OBJECT *
o_complex_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int angle, int mirror, char *clib, char *basename, int selectable)
{
	OBJECT *new_node=NULL;
	OBJECT *complex=NULL;
	OBJECT *temp_tail=NULL;
	OBJECT *temp_parent=NULL;
	int left, right, top, bottom;
	
	char filename[256]; /* hack */

	new_node = s_basic_init_object("complex");
	new_node->type = type;
	new_node->x = x;
	new_node->y = y;

	WORLDtoSCREEN(w_current, x, y, 
			&new_node->screen_x, &new_node->screen_y);    
	

	new_node->complex_basename = strdup(basename);
	new_node->complex_clib = strdup(clib);

	new_node->color = color;
	
	new_node->angle = angle;
	new_node->mirror = mirror;

	/* TODO: questionable caste? */
	new_node->draw_func = (void *) complex_draw_func;  

	if (selectable) { 

		new_node->sel_func = (void *) select_func;
	} else {
		new_node->sel_func = NULL;
	}

	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);	

	/* this was at the beginning and p_complex was = to complex */
	complex = (OBJECT *) add_head();
	
	/* set the parent field now */
	complex->complex_parent = object_list;

	/* is the bit with the temp and object_tail needed? */
	/* I don't like this at all hack */
	/* careful whenever you select, you get a read from disk */
	/* for the objects, UGG! there foreattribs are being copied */
	/* you need to override them if there are attached ones */
	/* on the main list */
	temp_tail = w_current->page_current->object_tail;
	temp_parent = w_current->page_current->object_parent;	
	w_current->page_current->object_parent = complex;
	/* reason this works is because it has a head, see add_head above */

	/* check for validity */
	sprintf(filename, "%s/%s", clib, basename);

	if (access(filename, R_OK)) {
		fprintf(stderr, "Could not open [%s]\n", filename); 
	} else {
		o_read(w_current, complex, filename); 
	}

	w_current->page_current->object_tail = temp_tail;
	w_current->page_current->object_parent = temp_parent;

	object_list->complex = complex;

	if (mirror) {
		o_complex_mirror_lowlevel(w_current, x, y, object_list);
	}


	o_complex_rotate_lowlevel(w_current, x, y, angle, angle, object_list);

	o_complex_world_translate(w_current, x, y, complex);
	
	return(object_list);
}

/* this is TOTALLY broken now */
/* since the complex hasn't been read yet... and not rotate */
OBJECT *
o_complex_add_embedded(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int angle, char *clib, char *basename, int selectable)
{
	OBJECT *complex=NULL;
	OBJECT *new_node=NULL;
	int left, right, top, bottom;

	new_node = s_basic_init_object("complex");
	new_node->type = type;
	new_node->x = x;
	new_node->y = y;

	WORLDtoSCREEN(w_current, x, y, 
			&new_node->screen_x, &new_node->screen_y);    
	
	new_node->complex_basename = strdup(basename);
	new_node->complex_clib = strdup(clib);

	new_node->color = color;

	/* TODO: questionable cast */
	new_node->draw_func = (void *) complex_draw_func;  

	/* (for a title block) an object that isn't selectable */
	if (selectable) { 

		new_node->sel_func = (void *) select_func;

	} else {
		new_node->sel_func = NULL;

	}

	new_node->angle = angle;

	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

	/* this was at the beginning and p_complex was = to complex */
	complex = (OBJECT *) add_head();
	object_list->complex = complex;
	
	/* don't have to translate/rotate/mirror here at all since the 
	/* object is in place */
	
	return(object_list);
}

void
o_complex_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int left, right, top, bottom;

	/* realc routine Add this somewhere */
	/* libhack */
	/* o_recalc(w_current, o_current->complex);*/

	get_complex_bounds(w_current, o_current->complex, &left, &top, &right, &bottom);
	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;

	WORLDtoSCREEN(w_current, o_current->x,
                  o_current->y,
                  &o_current->screen_x,
                  &o_current->screen_y);
}

OBJECT *
o_complex_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char *version)
{
	char type; 
	int x1, y1;
	int angle;
	char filename[256]; /* hack */

	char basename[256]; /* hack */
	char *clib=NULL;
	
	int selectable;
	int mirror;

	sscanf(buf, "%c %d %d %d %d %d %s\n", &type, &x1, &y1, &selectable, &angle, &mirror, basename);

	switch(angle) {

		case(0):
		case(90):
		case(180):
		case(270):
		break;

		default:
                	fprintf(stderr, "Found a component with an invalid rotation [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
                	s_log_message("Found a component with an invalid rotation [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
		 break;
	}

	switch(mirror) {

		case(0):
		case(1):

		break;
		
		default:
                	fprintf(stderr, "Found a component with an invalid mirror flag [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
                	s_log_message("Found a component with an invalid mirror flag [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
		break;
	}
	
	if (strncmp(basename, "EMBEDDED", 8) == 0) {
		object_list = o_complex_add_embedded(w_current, 
				object_list, type, 
				WHITE, x1, y1, angle,
				"EMBEDDED/", basename, 
				selectable);
		return(object_list);
	}

	clib = (char *) s_clib_search(basename); 

	if (clib == NULL) {
		/* do something here? hack */
	} else {
		
		/* check size? hack */
		sprintf(filename, "%s/%s", clib, basename);

		object_list = o_complex_add(w_current, object_list, type, 
				WHITE, 
				x1, y1, 
				angle, mirror,
				clib, basename, selectable);

		if (clib)
			free(clib);

	}

	return(object_list);
}

char *
o_complex_save(char *buf, OBJECT *object)
{
	int selectable;

	if (object->sel_func != NULL) 
		selectable = 1;
	else 
		selectable = 0;
	
	
        sprintf(buf, "%c %d %d %d %d %d %s", object->type,
                        object->x, object->y, selectable, 
			object->angle, object->mirror, object->complex_basename);
        return(buf);
}
     

void 
o_complex_set_filename(TOPLEVEL *w_current, char *clib, char *basename) 
{
	int len;

	if (basename == NULL) {
		fprintf(stderr, "Got NULL basename in o_complex_set_filename!\n");
		exit(-1);
	}

	if (clib == NULL) {
		fprintf(stderr, "Got NULL clib in o_complex_set_filename!\n");
		exit(-1);
	}

	if (w_current->internal_basename) {
		free(w_current->internal_basename);
	}

	if (w_current->internal_clib) {
		free(w_current->internal_clib);
	}

	len = strlen(basename);
	w_current->internal_basename = (char *) malloc(sizeof(char)*len+1);

	len = strlen(clib) + 1;	
	w_current->internal_clib = (char *) malloc(sizeof(char)*len+1);

	strcpy(w_current->internal_basename, basename);	
	strcpy(w_current->internal_clib, clib);	
} 

void
o_complex_free_filename(TOPLEVEL *w_current)
{
	if (w_current->internal_basename) {
		free(w_current->internal_basename);
	}

	if (w_current->internal_clib) {
		free(w_current->internal_clib);
	}
}

/* now I think it works fine */
/* no there is a bug with snap locking.  Basically if you don't snap/lock in */
/* PCtoW, then this doesn't work... :(  I don't know why yet */
void
o_complex_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	int x, y;
	int prevx, prevy;

	if (object == NULL) {
		printf("cmpt NO!\n");
		return;
	}

	object->screen_x = object->screen_x + dx;
	object->screen_y = object->screen_y + dy;


	/* this fixing makes me nervious hack */
	SCREENtoWORLD(w_current, object->screen_x,
                  object->screen_y,
                  &x,
                  &y);

	prevx = object->x;
	prevy = object->y;
	object->x = snap_grid(w_current, x);
	object->y = snap_grid(w_current, y);

	o_complex_world_translate(w_current, x - prevx,y - prevy, object->complex);
}

OBJECT *
o_complex_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj=NULL;
	ATTRIB *a_current;

	new_obj = o_complex_add(w_current, list_tail, OBJ_COMPLEX, o_current->color,
			o_current->x, o_current->y, 
			o_current->angle, o_current->mirror,
			o_current->complex_clib, o_current->complex_basename, 
			1); 
			/* 1 for sel is a hack */

	/*if (!w_current->ADDING_SEL) {*/
		o_attrib_slot_copy(w_current, o_current, new_obj);
	/*}*/

	/* deal with stuff that has changed */

	/* here you need to create a list of attributes which need to be 
	 * connected to the new list, probably make an attribute list and
	 * fill it with sid's of the attributes */
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

OBJECT *
o_complex_copy_embedded(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj=NULL;
	OBJECT *temp_list;
	ATTRIB *a_current;

	new_obj = o_complex_add_embedded(w_current, list_tail, OBJ_COMPLEX, 
			o_current->color,
			o_current->x, o_current->y, o_current->angle, 
			o_current->complex_clib, o_current->complex_basename, 
			1); 
	/* 1 for sel is a hack */
	/* deal with stuff that has changed */
	
	temp_list = o_list_copy_all(w_current,
			         	o_current->complex->next,
					new_obj->complex, NORMAL);
	
	new_obj->complex = return_head(temp_list);

	/* here you need to create a list of attributes which need to be 
	 * connected to the new list, probably make an attribute list and
	 * fill it with sid's of the attributes */
#if 1
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
#endif

	return(new_obj);
}

/* really verify this */
void
o_complex_delete(TOPLEVEL *w_current, OBJECT *delete)
{
	/* first remove complex pointer */
	if (delete->complex) {
		s_delete_list_fromstart(w_current, delete->complex);
		delete->complex = NULL;
	}

	/* then remove the actual node */	
	s_delete(w_current, delete);
	delete = NULL;
	w_current->page_current->object_tail = (OBJECT *) return_tail(w_current->page_current->object_head);
}

/* this needs work remove display stuff */
/* libhack */
/* and recalc stuff */
/* this function takes in a complex list */
void
o_complex_world_translate(TOPLEVEL *w_current, int x1, int y1, OBJECT *complex)
{
	OBJECT *o_current=NULL;
	OBJECT *one=NULL;
	OBJECT *two=NULL;
	unsigned long temp_color;

	o_current = complex;

	while ( o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_LINE):
				o_line_translate_world(w_current, x1, y1, o_current);
			break;

			case(OBJ_NET):
				/* same as a line, don't do this */
				o_line_translate_world(w_current, x1, y1, o_current);
				temp_color = w_current->override_color;
				w_current->override_color = -1;
				o_redraw_single(w_current, one); /* trying loop? hack*/
				o_redraw_single(w_current, two);
				w_current->override_color = temp_color;
			break;

			case(OBJ_BUS):
				/* same as a line, don't do this */
				o_line_translate_world(w_current, x1, y1, o_current);
				temp_color = w_current->override_color;
				w_current->override_color = -1;
				o_redraw_single(w_current, one); /* trying loop? hack*/
				o_redraw_single(w_current, two);
				w_current->override_color = temp_color;
			break;
	
			case(OBJ_BOX):
				o_box_translate_world(w_current, x1, y1, o_current);
			break;

			case(OBJ_CIRCLE):
				o_circle_translate_world(w_current, x1, y1, o_current);
			break;
	
			case(OBJ_COMPLEX):
				o_complex_world_translate_toplevel(w_current, x1, y1, o_current);
			break;

			case(OBJ_NTEXT):
				o_ntext_translate_world(w_current, x1, y1, o_current);
			break;

			/* same note as above */
			case(OBJ_PIN):
				o_pin_translate_world(w_current, x1, y1, o_current);
			break;

			case(OBJ_ARC):
				o_arc_translate_world(w_current, x1, y1, o_current);
			break;
		}
		o_current=o_current->next;
	}
}

/* this function takes the toplevel object and then also translates the
 * complex */
void
o_complex_world_translate_toplevel(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
	int left, right, top, bottom;

	object->x = object->x + x1;
	object->y = object->y + y1;

	WORLDtoSCREEN(w_current, object->x,
                  object->y,
                  &object->screen_x,
                  &object->screen_y);

	o_complex_world_translate(w_current, x1, y1, object->complex);

	get_complex_bounds(w_current, object->complex, &left, &top, &right, &bottom);
	
	object->left = left;
	object->top = top;
	object->right = right;
	object->bottom = bottom;
}

void
o_complex_set_color(TOPLEVEL *w_current, int color, OBJECT *complex)
{
	OBJECT *o_current=NULL;

	o_current = complex;

	while ( o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_LINE):
			case(OBJ_NET):
			case(OBJ_BUS):
			case(OBJ_BOX):
			case(OBJ_CIRCLE):
			case(OBJ_PIN):
			case(OBJ_ARC):
				o_current->color = color;

			case(OBJ_NTEXT):
			case(OBJ_COMPLEX):
				o_complex_set_color(w_current, color, o_current->complex);

			break;

		}
		o_current=o_current->next;
	}
}


/* returns the counter'th pin in o_list */
/* NULL if there is no more pins */
OBJECT *
o_complex_return_pin(OBJECT *o_list, int counter)
{
	OBJECT *o_current;
	int internal_counter=0;
	
	o_current = o_list;
	
	while (o_current != NULL) {
		if (o_current->type == OBJ_PIN) {
			
			if (counter == internal_counter) {
				return(o_current);
			} else {
				internal_counter++;	
			}
		}	
		o_current = o_current->next;
	}
	
	return(NULL);

}


/* pass in top level object */
void
o_complex_rotate_lowlevel(TOPLEVEL *w_current, int world_centerx, 
	int world_centery, 
	int angle,
	int angle_change,
	OBJECT *object)
{
	OBJECT *o_current=NULL;

#if DEBUG 
	printf("------- a %d ac %d\n", angle, angle_change);
#endif

/* do individual complex objects */
	o_current = object->complex;

	while ( o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_LINE):
				o_line_rotate_world(w_current, 0, 0, angle_change, o_current);
			break;

			case(OBJ_NET):
				o_net_rotate_world(w_current, 0, 0, angle_change, o_current);
			break;

			case(OBJ_BUS):
				o_bus_rotate_world(w_current, 0, 0, angle_change, o_current);
			break;
	
			case(OBJ_BOX):
				o_box_rotate_world(w_current, 0, 0, angle_change, o_current);
			break;

			case(OBJ_CIRCLE):
				o_circle_rotate_world(w_current, 0, 0, angle_change, o_current);
			break;

			case(OBJ_PIN):
				o_pin_rotate_world(w_current, 0, 0, angle_change, o_current);
			break;

			case(OBJ_ARC):
				o_arc_rotate_world(w_current, 0, 0, angle_change, o_current);
			break;

#if 0	/* complex within a complex? not right now */
			case(OBJ_COMPLEX):
			break;
#endif

			case(OBJ_NTEXT):
				o_ntext_rotate_world(w_current, 0, 0, angle, angle_change, o_current);
			break;

		}
		o_current=o_current->next;
	}
}


/* pass in top level object */
void
o_complex_mirror_lowlevel(TOPLEVEL *w_current, 
	int world_centerx, int world_centery, OBJECT *object)
{
	OBJECT *o_current=NULL;

/* do individual complex objects */
	o_current = object->complex;

	while ( o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_LINE):
				o_line_mirror_world(w_current, 0, 0, o_current);
			break;

			case(OBJ_NET):
				o_net_mirror_world(w_current, 0, 0, o_current);
			break;

			case(OBJ_BUS):
				o_bus_mirror_world(w_current, 0, 0, o_current);
			break;
	
			case(OBJ_BOX):
				o_box_mirror_world(w_current, 0, 0, o_current);
			break;

			case(OBJ_CIRCLE):
				o_circle_mirror_world(w_current, 0, 0, o_current);
			break;

			case(OBJ_PIN):
				o_pin_mirror_world(w_current, 0, 0, o_current);
			break;

			case(OBJ_ARC):
				o_arc_mirror_world(w_current, 0, 0, o_current);
			break;

#if 0	/* complex within a complex? not right now */
			case(OBJ_COMPLEX):

			break;
#endif

			case(OBJ_NTEXT):
				o_ntext_mirror_world(w_current, 0, 0, o_current);
			break;

		}
		o_current=o_current->next;
	}

	/* mirror origin point */
/*	object->x = -object->x;*/
}


