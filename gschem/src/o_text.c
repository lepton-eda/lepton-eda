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
#include <math.h>
#include <string.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

#define WINONLY	1
#define BACKING 2

/* font storage and friends are staying global so that all can access */
#define NUM_CHARS 255

void
o_text_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int small_dist;
	int length, dx=0, dy=0;

	if (o_current->visibility == INVISIBLE) {
		return;
	}

	if (!w_current->fast_mousepan || !w_current->doing_pan) {
		o_complex_draw(w_current, o_current);
	} else {
		if (w_current->doing_pan) {
			o_complex_recalc(w_current, o_current);
		
			/* text is too small so go through and draw a line in
	 		   it's place */
	
			screen_x1 = o_current->screen_x;
			screen_y1 = o_current->screen_y;
	
			gdk_gc_set_foreground(w_current->gc,
					      x_get_color(o_current->color));

#if 0 /* new way, but doesn't always look so good */	
			length = SCREENabs(w_current, 
					   o_text_width(w_current, 
					   o_current->text_string,
					   o_current->text_size/2)); 
#endif

			length = SCREENabs(w_current,
					   o_current->displayed_text_len*10*
					   o_current->text_size);

#if DEBUG
			printf("%d %d %d\n",
			       o_current->displayed_text_len,
			       o_current->displayed_text_len * 20 *
			       o_current->text_size, length);
#endif

			switch(o_current->angle) {
			case(0):
				gdk_draw_line(w_current->window,
					      w_current->gc,
					      screen_x1+dx,
					      screen_y1+dy,
					      screen_x1+dx+length,
					      screen_y1+dy);
				break;
	
			case(90):
				gdk_draw_line(w_current->window,
					      w_current->gc,
					      screen_x1+dx,
					      screen_y1+dy,
					      screen_x1+dx,
					      screen_y1+dy-length);
				break;
	
			case(180):
				gdk_draw_line(w_current->window,
					      w_current->gc,
					      screen_x1+dx,
					      screen_y1+dy,
					      screen_x1+dx-length,
					      screen_y1+dy);
				break;
	
			case(270):
				gdk_draw_line(w_current->window,
					      w_current->gc,
					      screen_x1+dx,
					      screen_y1+dy,
					      screen_x1+dx,
					      screen_y1+dy+length);
				break;
			}
			return;
		}
	}
	
	/* return if text origin marker displaying is disabled */ 
	if (w_current->text_origin_marker == FALSE) {
		return;
	}

	small_dist = SCREENabs(w_current, 10);

	screen_x1 = o_current->screen_x;
	screen_y1 = o_current->screen_y;


   	if (w_current->override_color != -1 ) {
		gdk_gc_set_foreground(w_current->gc, 
		                      x_get_color(w_current->override_color));
	} else {

		gdk_gc_set_foreground(w_current->gc, 
	                              x_get_color(o_current->color));
	}

	gdk_draw_line(w_current->window, w_current->gc, 
		      screen_x1-small_dist, 
		      screen_y1+small_dist, 
                      screen_x1+small_dist, 
		      screen_y1-small_dist);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
		      screen_x1-small_dist, 
		      screen_y1+small_dist, 
                      screen_x1+small_dist, 
		      screen_y1-small_dist);

	gdk_draw_line(w_current->window, w_current->gc, 
		      screen_x1+small_dist, 
		      screen_y1+small_dist, 
                      screen_x1-small_dist, 
		      screen_y1-small_dist);
	gdk_draw_line(w_current->backingstore, w_current->gc, 
		      screen_x1+small_dist, 
		      screen_y1+small_dist, 
                      screen_x1-small_dist, 
		      screen_y1-small_dist);
}

void
o_text_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
        w_current->override_color = w_current->background_color;
	o_text_draw(w_current, o_current);
        w_current->override_color = -1;
}

void
o_text_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
	int screen_x1, screen_y1;
	int length;
	int color;

	if (o_current->visibility == INVISIBLE) {
		return;
	}

	/* always display text which is 12 or larger */
	if ((w_current->page_current->zoom_factor >
	     w_current->text_display_zoomfactor) ||
	    o_current->text_size >= 12 ||
	    w_current->text_feedback == ALWAYS) {
		o_complex_draw_xor(w_current, dx, dy, o_current->complex);
	} else {
		/* text is too small so go through and draw a line in
	 	   it's place */

		screen_x1 = o_current->screen_x;
		screen_y1 = o_current->screen_y;

	        if (o_current->saved_color != -1) {
       	        	color = o_current->saved_color;
		} else {
                	color = o_current->color;
        	}

		gdk_gc_set_foreground(w_current->outline_xor_gc,
				      x_get_darkcolor(color));

		length = SCREENabs(w_current,
				   o_current->displayed_text_len*10*
				   o_current->text_size);

#if 0 /* new way, but doesn't look so good */
		length = SCREENabs(w_current, o_text_width(w_current, 
					   o_current->text_string,
					   o_current->text_size/2)); 
#endif


#if DEBUG
		printf("%d %d %d\n",
		       o_current->displayed_text_len,
		       o_current->displayed_text_len * 20 *
		       o_current->text_size, length);
#endif

		switch(o_current->angle) {
		case(0):
			gdk_draw_line(w_current->window,
				      w_current->outline_xor_gc,
				      screen_x1+dx,
				      screen_y1+dy,
				      screen_x1+dx+length,
				      screen_y1+dy);
			break;

		case(90):
			gdk_draw_line(w_current->window,
				      w_current->outline_xor_gc,
				      screen_x1+dx,
				      screen_y1+dy,
				      screen_x1+dx,
				      screen_y1+dy-length);
			break;

		case(180):
			gdk_draw_line(w_current->window,
				      w_current->outline_xor_gc,
				      screen_x1+dx,
				      screen_y1+dy,
				      screen_x1+dx-length,
				      screen_y1+dy);
			break;

		case(270):
			gdk_draw_line(w_current->window,
				      w_current->outline_xor_gc,
				      screen_x1+dx,
				      screen_y1+dy,
				      screen_x1+dx,
				      screen_y1+dy+length);
			break;
		}
	}
}

void
o_text_input(TOPLEVEL *w_current)
{
	text_input_dialog(w_current);
}

void
o_text_start(TOPLEVEL *w_current, int screen_x, int screen_y)
{
	int x, y;
	char *value;

	w_current->last_x = w_current->start_x = fix_x(w_current, screen_x);
	w_current->last_y = w_current->start_y = fix_y(w_current, screen_y);

	w_current->last_drawb_mode = -1;

	/* make sure list is null first, so that you don't have a mem leak */
        SCREENtoWORLD(w_current,
		      w_current->start_x,
		      w_current->start_y,
		      &x,
		      &y);

	/* remove the old attrib list if it exists without killing the
           head structure */
        o_list_delete_rest(w_current,
			   w_current->page_current->attrib_place_head);

	value = w_current->current_attribute;

	switch(w_current->text_caps) {
	case(LOWER):
		string_tolower(value, value);
		break;

	case(UPPER):
		string_toupper(value, value);
		break;

	case(BOTH):
	default:
		/* do nothing */
		break;
	}

	/* here you need to add OBJ_TEXT when it's done */
	w_current->page_current->attrib_place_tail =
		(OBJECT *) o_text_add(
			w_current,
			w_current->page_current->attrib_place_head,
				/* type changed from TEXT to TEXT */
			OBJ_TEXT, w_current->text_color,
			x, y, LOWER_LEFT, 0, /* zero is angle */
			w_current->current_attribute,
			w_current->text_size,
			/* has to be visible so you can place it */
			/* visibility is set when you create the object */
			VISIBLE, SHOW_NAME_VALUE);

	o_drawbounding(w_current,
		       w_current->page_current->attrib_place_head->next,
		       NULL,
		       x_get_color(w_current->bb_color));
}

void
o_text_end(TOPLEVEL *w_current)
{
	/* TODO: get consistant names */
	int world_x, world_y;

        SCREENtoWORLD(w_current,
		      w_current->last_x,
		      w_current->last_y,
		      &world_x,
		      &world_y);

        world_x = snap_grid(w_current, world_x);
        world_y = snap_grid(w_current, world_y);

	/* here you need to add OBJ_TEXT when it's done */
        /* TODO: make this VIS and SHOW default configurable */
        w_current->page_current->object_tail =
		o_text_add(w_current, w_current->page_current->object_tail,
				/* type changed from TEXT to TEXT */
			    OBJ_TEXT,
			    w_current->text_color,
			    world_x, world_y, LOWER_LEFT, 0, /* zero is angle */
			    w_current->current_attribute,
			    w_current->text_size,
			    VISIBLE, SHOW_NAME_VALUE);

	/* if the text is invisible then you need to erase the outline
	   left by the place */
	if (w_current->current_visible == INVISIBLE) {
		o_drawbounding(
			w_current,
			w_current->page_current->attrib_place_head->next,
			NULL,
			x_get_color(w_current->bb_color));
	}
	/* TODO: you need to erase the bounding box if have that mode
           set!!! */

	/* erase the old bounding box / outline */
	if (w_current->actionfeedback_mode == OUTLINE) {
		o_drawbounding(
			w_current,
			w_current->page_current->attrib_place_head->next,
			NULL,
			x_get_color(w_current->text_color));
	} else {
		o_drawbounding(
			w_current,
			w_current->page_current->attrib_place_head->next,
			NULL,
			x_get_color(w_current->select_color));
	}

        w_current->override_color = -1;

        w_current->page_current->CHANGED=1;

	o_selection_remove_most(w_current,
       				w_current->page_current->selection2_head);
	o_selection_add(w_current->page_current->selection2_head, 
			w_current->page_current->object_tail);
	

	/* object_tail is the object that was just added */
	if (w_current->page_current->object_tail->draw_func != NULL &&
	    w_current->page_current->object_tail->type != OBJ_HEAD) {
        	(*w_current->page_current->object_tail->draw_func)(
			w_current,
			w_current->page_current->object_tail);
        }

	w_current->override_color = -1;
	o_undo_savestate(w_current, UNDO_ALL);
}

void
o_text_rubberattrib(TOPLEVEL *w_current)
{
	o_drawbounding(w_current,
		       w_current->page_current->attrib_place_head->next,
		       NULL,
		       x_get_color(w_current->bb_color));
}

void
o_text_edit(TOPLEVEL *w_current, OBJECT *o_current)
{
	/* you need to check to make sure only one object is selected */
	/* no actually this is okay... not here in o_edit */
	text_edit_dialog(w_current,
			 o_current->text_string, o_current->text_size,
			 o_current->text_alignment);
}

void
o_text_edit_end(TOPLEVEL *w_current, char *string, int len, int text_size,
		int text_alignment)
{
	OBJECT *object;

	object = o_select_return_first_object(w_current);

	if (object != NULL) {
		if (object->text_string) {
			free(object->text_string);
		}

		/* Kazu <kazu@seul.org> on August 5, 1999 - I am not
                   sure if strlen(string) == len. If so, activate the
                   second part of this "if".*/
#if 1
		object->text_string = malloc(sizeof(char) * len + 1);
		strcpy(object->text_string, string);
#else
		object->text_string = u_basic_strdup(string);
#endif

		object->text_size = text_size;
		object->text_alignment = text_alignment;
		
		o_text_erase(w_current, object);
		o_text_recreate(w_current, object);
		o_text_draw(w_current, object);

		w_current->page_current->CHANGED = 1;
		o_undo_savestate(w_current, UNDO_ALL);
	} else {
		fprintf(stderr, "uggg! you tried to text edit something "
			"that doesn't exist!\n");
		exit(-1);
	}
}

/* The object passed in should be the REAL object, NOT any copy in any */
/* selection list */
void
o_text_change(TOPLEVEL *w_current, OBJECT *object, char *string, 
	      int visibility, int show)
{
	if (object == NULL) {
		return;
	}

	if (object->type != OBJ_TEXT) {
		return;
	}

	/* erase old object */
	o_text_erase(w_current, object);

	/* second change the real object */
	if (object->text_string) {
		free(object->text_string);
	}

	object->text_string = u_basic_strdup(string);
	object->visibility = visibility;
	object->show_name_value = show;
	o_text_recreate(w_current, object);
	o_text_draw(w_current, object);

	w_current->page_current->CHANGED = 1;
}

