/* Lots of Gross code... needs lots of cleanup */
/* mainly readability issues */

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

/* instrumentation code */
#if 0
#include <sys/time.h>
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/s_passing.h>
#include <libgeda/o_types.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

void
o_redraw_all(TOPLEVEL *w_current)
{
#if 0
 	struct timeval tv1;
	struct timeval tv2;
#endif

	o_ales_disconnect_update(w_current->page_current);

	x_repaint_background(w_current);
/*	gettimeofday(&tv1, NULL);*/
	o_redraw(w_current, w_current->page_current->object_head);

/*	gettimeofday(&tv2, NULL);*/

#if 0
	printf("secs: %d\n", tv2.tv_sec - tv1.tv_sec);
	printf("usecs: %d\n\n", tv2.tv_usec - tv1.tv_usec);
#endif



	o_redraw_selected(w_current);

	if (w_current->inside_action) {
		switch(w_current->event_state) {
			case(ENDMOVE):
			case(ENDCOPY):
				o_drawbounding(w_current, 
					w_current->page_current->selection_head->next, 
					x_get_color(w_current->bb_color));
				break;

			case(DRAWCOMP):
			case(ENDCOMP):
				o_drawbounding(w_current,
					w_current->page_current->complex_place_head->next, 
					x_get_color(w_current->bb_color));
				break;

			case(DRAWATTRIB):
			case(ENDATTRIB):
				o_drawbounding(w_current, 
					w_current->page_current->attrib_place_head->next, 
					x_get_color(w_current->bb_color));
				break;
		}
	}
}

void
o_redraw(TOPLEVEL *w_current, OBJECT *object_list)
{
	OBJECT *o_current=NULL;

	o_current = object_list;

	while (o_current != NULL) {

		if (o_current->draw_func != NULL && o_current->type != OBJ_HEAD) {

			w_current->inside_redraw = 1;
			(*o_current->draw_func)(w_current, o_current);		
			w_current->inside_redraw = 0;
		}

		o_current = o_current->next;
	}


	/* taken out since it looped complex unselects... do i need it? */	
	/* we need it because if you redraw the current selected object */
	/* won't be draw */
	/* so, we need to use the inside_draw flag */
	if (!w_current->inside_redraw) {
		w_current->inside_redraw = 1;
		o_redraw_selected(w_current);
		w_current->inside_redraw = 0;
	}
}


void
o_unselect_all(TOPLEVEL *w_current)
{
	/* The proper way to do this is to not unredraw_selected, but to 
	   redraw the original object, to reflect if any colors have changed */
	/* Why is this needed, because when you update some element in the
	   object_head list, then you want that object to be reflected not
	   the selected one! */
	/* now if you want both the selected and the real object to reflect
	   a change, then you need to do something completely different */

	/* didn't find anything */
        if (!w_current->SHIFTKEY) {
		/* this was changed to undraw the real */
                /* o_unredraw_selected(); */
		o_unredraw_real(w_current, w_current->page_current->selection_head);
                o_list_delete_rest(w_current, w_current->page_current->selection_head);
                w_current->page_current->selection_head->next = NULL;

                w_current->page_current->selection_tail = w_current->page_current->selection_head;
        }      
}

void
o_find(TOPLEVEL *w_current, int x, int y)
{
	OBJECT *o_current=NULL;

	if (w_current->page_current->object_lastplace == NULL) {
		o_current = w_current->page_current->object_head;
	} else {
		o_current = w_current->page_current->object_lastplace;
	}

	while (o_current != NULL) {

		if (inside_region(o_current->left, 
				  o_current->top,
				  o_current->right,
				  o_current->bottom, x, y)) {
			if (o_current->sel_func != NULL && 
				o_current->visibility == VISIBLE &&
				o_current->type != OBJ_HEAD) {
				(*o_current->sel_func)(w_current, o_current);		
				w_current->page_current->object_lastplace = 
					o_current->next;			

				return;
			}
		}
		o_current = o_current->next;
	}

	o_current = w_current->page_current->object_head;
	while (o_current != NULL && 
			o_current != w_current->page_current->object_lastplace) {

		if (inside_region(o_current->left, 
				  o_current->top,
				  o_current->right,
				  o_current->bottom, x, y)) {
			if (o_current->sel_func != NULL && 
				o_current->visibility == VISIBLE &&
				o_current->type != OBJ_HEAD) {
				(*o_current->sel_func)(w_current, o_current);		
				w_current->page_current->object_lastplace = o_current;
				/* same thing as above comment before ret */
				return;
			}
		}
		o_current = o_current->next;
	}

#if DEBUG 
	printf("found nothing\n");
#endif

	/* here we start to sbox draw box */
	/* if we start to move */

	/* not sure if we want to keep this */
	w_current->event_state = SELECT;
	/* didn't find anything */
	o_unselect_all(w_current); 
}

void 
o_select(TOPLEVEL *w_current, OBJECT *selected)
{
	OBJECT *found=NULL;

	if (selected == NULL)
		return;	

	/* The list is empty */
	if (w_current->page_current->selection_head->next == NULL) {
		/*printf("list is empty\n");*/
		/* returning tail is okay since it's just peachy. */
		w_current->page_current->selection_tail = (OBJECT *) o_list_copy_to(w_current, 
				   w_current->page_current->selection_head, selected, SELECTION);
		w_current->override_color = w_current->select_color;
#if DEBUG
		printf("selected is: %s\n", selected->name);
#endif

		if (selected->draw_func != NULL && selected->type != OBJ_HEAD) {
			(*selected->draw_func)(w_current, selected);
		}

		w_current->override_color = -1;

		/* don't know if I like this behavior hack*/
		deal_attr(w_current, selected);

		/* update selection tail to point to the right place */	
		w_current->page_current->selection_tail = (OBJECT *) 
			return_tail(w_current->page_current->selection_head);

		return;
	}
	
	if (w_current->page_current->selection_head->next != NULL) {

		/*printf("list is NOT empty -- ");*/

		if (w_current->SHIFTKEY) { 
			/*printf("with shift key pressed\n");*/
			/*printf("searching for %d\n", selected->sid);*/
			/* search in sel list to see if object is there */
			found = (OBJECT *) 
				o_list_search(w_current->page_current->selection_head, 
					selected);	
			if (found) {
				/*printf("found\n");*/
				/* okay it was found so delete it */

				/* single object */
				o_list_delete(w_current, w_current->page_current->selection_head, 
					found);

				w_current->override_color = -1;
				if (selected->draw_func != NULL 
				   && selected->type != OBJ_HEAD) {
					(*selected->draw_func)(w_current, selected);
				}
			} else {
				/*printf("NOT found\n");*/
				w_current->page_current->selection_tail = (OBJECT *) 
						o_list_copy_to(w_current, 
					  	  w_current->page_current->selection_tail,
						  selected, SELECTION);	
				w_current->override_color = w_current->select_color;
				if (selected->draw_func != NULL && selected->type != OBJ_HEAD) {
					(*selected->draw_func)(w_current, selected);
				}
				w_current->override_color = -1;
			}
		} else {
			/*printf("withOUT shift key pressed\n");*/

			/*printf("searching for %d\n", selected->sid);*/
			found = (OBJECT *) o_list_search(w_current->page_current->selection_head, selected);	
			if (found) {
				/* selected is the same what is already in 
				   list */

				/* hack */
				/* here is where you will check for CTRL KEY */
				/* and just remove the object from the selection */
				/* careful bug here I don't know? hack */
				if (found->sid == selected->sid) {
					/* this was just unredraw_selected */
					/* verify this is really right hack */
				   if (!w_current->CONTROLKEY) {
					o_unredraw_real(w_current, w_current->page_current->selection_head); 
					o_list_delete_rest(w_current, w_current->page_current->selection_head);
					w_current->page_current->selection_head->next = NULL;

					/* put that selected object back 
					   into the list */
					w_current->page_current->selection_tail = 
						(OBJECT *)
						 o_list_copy_to(w_current, 
					    w_current->page_current->selection_head, 
					       selected, SELECTION);
					w_current->override_color = w_current->select_color;
					if (selected->draw_func != NULL && selected->type != OBJ_HEAD) {
						(*selected->draw_func)(w_current, selected);
					}
					w_current->override_color = -1;

				   } else {
					/* test this */
					o_list_delete(w_current, w_current->page_current->selection_head, found);

				        w_current->override_color = -1;
					if (selected->draw_func != NULL && selected->type != OBJ_HEAD) {
				        	(*selected->draw_func)(w_current, selected);
					}
				   }
				}
			} else {

				/* new object unslect old */
				/* this was just unredraw_selected */
				o_unredraw_real(w_current, w_current->page_current->selection_head); 
				o_list_delete_rest(w_current, w_current->page_current->selection_head);
				w_current->page_current->selection_head->next = NULL;

				/* add new selected to list */
				w_current->page_current->selection_tail = (OBJECT *) 
				  o_list_copy_to(w_current, w_current->page_current->selection_head, 
								selected, SELECTION); 
				w_current->override_color = w_current->select_color;
				if (selected->draw_func != NULL && selected->type != OBJ_HEAD) {
					(*selected->draw_func)(w_current, selected);
				}
				w_current->override_color = -1;
			}

		}
	}

	/* don't know if I like this behavior hack*/
	deal_attr(w_current, selected);

	/* update selection tail to point to the right place */	
	w_current->page_current->selection_tail = (OBJECT *) 
			return_tail(w_current->page_current->selection_head);

	return;
}


void 
o_select_many(TOPLEVEL *w_current, OBJECT *selected, int count)
{
	OBJECT *found=NULL;

	if (selected == NULL) {
		printf("nothing selected\n");
		return;	
	}

	/* The list is empty */
	if (w_current->page_current->selection_head->next == NULL) {
		/*printf("list is empty\n");*/
		w_current->page_current->selection_tail = (OBJECT *) o_list_copy_to(w_current, 
				w_current->page_current->selection_head, selected, SELECTION); 
		if (selected->draw_func != NULL && selected->type != OBJ_HEAD) {
			w_current->override_color = w_current->select_color;
			(*selected->draw_func)(w_current, selected);
			w_current->override_color = -1;
		}
	
		/* don't know if I like this behavior hack*/
		deal_attr(w_current, selected);	

		/* update selection tail to point to the right place */	
		w_current->page_current->selection_tail = (OBJECT *) 
			return_tail(w_current->page_current->selection_head);

		return;
	}
	
	if (w_current->page_current->selection_head->next != NULL) {

		/*printf("list is NOT empty -- ");*/

		/* not holding down shift, and first object */
		if (!w_current->SHIFTKEY && count == 0 && 
			!w_current->CONTROLKEY) {
			/* this was just unredraw_selected */
			o_unredraw_real(w_current, w_current->page_current->selection_head); 
			o_list_delete_rest(w_current, w_current->page_current->selection_head);
			w_current->page_current->selection_head->next = NULL;

			w_current->page_current->selection_tail = 
				w_current->page_current->selection_head;
		}

	/* comment: do we want to unselect stuff when we are inserting many
	   stuff ???? hack */

	/* problem: drawing sboxes always acts like the shift key was down */
	/* what you want is if you draw a sbox then select the stuff */
	/* but then if you draw another sbox, and the SHIFT key isn't down */
	/* the deselect the stuff */
	/* is this still right? 3/1 ??? */
	/* doesn't look like it */

		/*printf("searching for %d\n", selected->sid);*/
		/* search in sel list to see if object is there */
		found = (OBJECT *) o_list_search(
			w_current->page_current->selection_head, selected);	
		if (found) {
			/*printf("found\n");*/
			/* okay it was found so delete it */

			/*hack experimental */
			if (w_current->CONTROLKEY) {  
				/* single object */
				o_list_delete(w_current, w_current->page_current->selection_head, 
						found);

				w_current->override_color = -1;
				if (selected->draw_func && selected->type != OBJ_HEAD) {
					(*selected->draw_func)(w_current, selected);
				}
			}
		} else {
			/*printf("NOT found\n");*/
			w_current->page_current->selection_tail = 
				  (OBJECT *) o_list_copy_to(w_current, 
				  w_current->page_current->selection_tail, selected, SELECTION);	
			w_current->override_color = w_current->select_color;
			if (selected->draw_func && selected->type != OBJ_HEAD) {
				(*selected->draw_func)(w_current, selected);
			}
			w_current->override_color = -1;
		}
	}

		/* don't know if I like this behavior hack*/
	deal_attr(w_current, selected);	

	/* update selection tail to point to the right place */	
	w_current->page_current->selection_tail = (OBJECT *) 
			return_tail(w_current->page_current->selection_head);

	return;
}

void
o_redraw_selected(TOPLEVEL *w_current)
{
	OBJECT *o_current=NULL;


	if (w_current->page_current->selection_head == NULL) {
		return;
	}

	o_current = w_current->page_current->selection_head;

	if (w_current->inside_redraw)
		return;



	w_current->DONT_DRAW_CONN = 1;
	w_current->override_color = w_current->select_color;
	if (w_current->page_current->selection_head->next != NULL) {
		while (o_current != NULL) {
			if (o_current->draw_func && 
			      o_current->type != OBJ_HEAD) {
				(*o_current->draw_func)(w_current, o_current);
			}
			o_current = o_current->next;
		}
	}
	w_current->override_color = -1;
	w_current->DONT_DRAW_CONN = 0;
}


void
o_unredraw_selected(TOPLEVEL *w_current)
{
	OBJECT *o_current=NULL;

	if (w_current->inside_redraw)
		return;

	o_current = w_current->page_current->selection_head;



	w_current->DONT_DRAW_CONN = 1;
	w_current->override_color = -1;
	if (w_current->page_current->selection_head->next != NULL) {
		while (o_current != NULL) {
			if (o_current->draw_func && 
			      o_current->type != OBJ_HEAD) {
				(*o_current->draw_func)(w_current, o_current);
			}
			o_current = o_current->next;
		}
	}
	w_current->DONT_DRAW_CONN = 0;
}

void
o_erase_selected(TOPLEVEL *w_current)
{
	OBJECT *o_current=NULL;

	if (w_current->inside_redraw)
		return;

	o_current = w_current->page_current->selection_head;

	w_current->DONT_DRAW_CONN = 1;
	w_current->override_color = w_current->background_color;
	if (w_current->page_current->selection_head->next != NULL) {
		while (o_current != NULL) {
			if (o_current->draw_func && 
			      o_current->type != OBJ_HEAD) {
				(*o_current->draw_func)(w_current, o_current);
			}
			o_current = o_current->next;
		}
	}
	w_current->override_color = -1;
	w_current->DONT_DRAW_CONN = 0;
}

void
o_erase_single(TOPLEVEL *w_current, OBJECT *object)
{
	OBJECT *o_current;

	if (w_current->inside_redraw)
		return;

	o_current = object;

	w_current->DONT_DRAW_CONN = 1;
	w_current->override_color = w_current->background_color;
	if (o_current != NULL) {
		if (o_current->draw_func && 
		      o_current->type != OBJ_HEAD) {
			(*o_current->draw_func)(w_current, o_current);
		}
	}
	w_current->override_color = -1;
	w_current->DONT_DRAW_CONN = 0;
}

void 
o_unredraw_real(TOPLEVEL *w_current, OBJECT *list)
{
	OBJECT *o_current=NULL;
	OBJECT *found=NULL;

	o_current = list;

	if (w_current->inside_redraw)
		return;

	if (list == NULL)
		return;

	w_current->DONT_DRAW_CONN = 1;
	w_current->override_color = -1;
	if (w_current->page_current->selection_head->next != NULL) {
		while (o_current != NULL) {

			found = (OBJECT *) 
				o_list_search(w_current->page_current->object_head, 
						o_current);
	
			if (found) {	
				/*printf("found real redrawing\n");*/
				if (found->draw_func && found->type != OBJ_HEAD) {
					(*found->draw_func)(w_current, found);
				}
			}
			o_current = o_current->next;
		}
	}
	w_current->DONT_DRAW_CONN = 0;
}


void 
o_redraw_real(TOPLEVEL *w_current, OBJECT *list)
{
	OBJECT *o_current=NULL;
	OBJECT *found=NULL;

	o_current = list;

	if (w_current->inside_redraw)
		return;

	if (list == NULL)
		return;

	w_current->override_color = -1;
	if (w_current->page_current->selection_head->next != NULL) {
		while (o_current != NULL) {

			found = (OBJECT *) 
				o_list_search(w_current->page_current->object_head, 
						o_current);
	
			if (found) {	
				/*printf("found real redrawing\n");*/
				if (found->draw_func && found->type != OBJ_HEAD) {
					(*found->draw_func)(w_current, found);
				}
			}
			o_current = o_current->next;
		}
	}
}

/* both outline and boundingbox work! */
/* make this general purpose, get rid of selection_head and pass that in */
/* name is blah */
void
o_drawbounding(TOPLEVEL *w_current, OBJECT *o_current, GdkColor *color)
{
	int diff_x, diff_y;
	int test_x, test_y;
	int rleft, rtop, rbottom, rright;

	if ( (w_current->last_drawb_mode == OUTLINE) && 
		(w_current->actionfeedback_mode == BOUNDINGBOX) ) {

#if DEBUG 
		printf("going to bounding\n");
#endif

		diff_x = w_current->last_x - w_current->start_x;
        	diff_y = w_current->last_y - w_current->start_y;

        	gdk_gc_set_foreground(w_current->bounding_xor_gc, 
				x_get_color(w_current->background_color) );
		o_complex_translate_display(w_current, diff_x, diff_y, o_current);
        	gdk_gc_set_foreground(w_current->bounding_xor_gc, color);

		get_complex_bounds(w_current, o_current, &rleft, &rtop, 
						&rright, &rbottom); 
        	gdk_draw_rectangle(w_current->window, 
				   w_current->bounding_xor_gc, FALSE,
                		   rleft+diff_x, rtop+diff_y,
                		   rright-rleft, rbottom-rtop); 

	}

	if ((w_current->last_drawb_mode == BOUNDINGBOX) && 
		(w_current->actionfeedback_mode == OUTLINE)) {

#if DEBUG
		printf("going to outline\n");
#endif

		get_complex_bounds(w_current, o_current, &rleft, &rtop, 
						&rright, &rbottom); 
		diff_x = w_current->last_x - w_current->start_x;
        	diff_y = w_current->last_y - w_current->start_y;
        	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->background_color) );
        	gdk_draw_rectangle(w_current->window, 
				   w_current->gc, FALSE,
                	           rleft+diff_x, rtop+diff_y,
                	           rright-rleft, rbottom-rtop); 

		o_complex_translate_display(w_current, diff_x, diff_y, o_current);
	}

	w_current->last_drawb_mode = w_current->actionfeedback_mode;

/* everything above is okay */


	/* much replicated code... this is the behaviour we need, but
	   we need to clean it up !!! hack */

	/* erase old outline */
	/* going to constrained from free */
	if ( (w_current->CONTROLKEY) && 
		(w_current->drawbounding_action_mode == FREE) ) {
		diff_x = w_current->last_x - w_current->start_x;
		diff_y = w_current->last_y - w_current->start_y;
		/*printf("switching to contrained\n");*/
		w_current->drawbounding_action_mode = CONSTRAINED;	

		if (w_current->actionfeedback_mode == OUTLINE) {
			o_complex_translate_display(w_current, diff_x, diff_y, o_current);
		} else {
			get_complex_bounds(w_current, o_current, &rleft, &rtop, 
						&rright, &rbottom); 
		
			gdk_gc_set_foreground(w_current->bounding_xor_gc, 
						color);
        		gdk_draw_rectangle(w_current->window, 
				w_current->bounding_xor_gc, FALSE,
                		rleft+diff_x, rtop+diff_y,
                		rright-rleft, rbottom-rtop); 
		}

		test_x = abs(w_current->last_x - w_current->start_x);
        	test_y = abs(w_current->last_y - w_current->start_y);
        	if (test_x >= test_y) {
               		w_current->last_y = w_current->start_y;
        	} else {
               		w_current->last_x = w_current->start_x;
        	}            

		diff_x = w_current->last_x - w_current->start_x;
		diff_y = w_current->last_y - w_current->start_y;

		if (w_current->actionfeedback_mode == OUTLINE) {
			o_complex_translate_display(w_current, diff_x, diff_y, o_current);
		} else {
			get_complex_bounds(w_current, o_current, &rleft, &rtop, 
						&rright, &rbottom); 
			gdk_gc_set_foreground(w_current->bounding_xor_gc,
					 color); 
        		gdk_draw_rectangle(w_current->window, 
					w_current->bounding_xor_gc,
					FALSE, rleft+diff_x, rtop+diff_y,
                		 	rright-rleft, rbottom-rtop); 
		}
	}

	/* erase old outline */
	/* going to free from constrained */
	if ( (!w_current->CONTROLKEY) && 
		(w_current->drawbounding_action_mode == CONSTRAINED) ) { 
		/*printf("switching to free\n");*/
		diff_x = w_current->last_x - w_current->start_x;
		diff_y = w_current->last_y - w_current->start_y;
		w_current->drawbounding_action_mode = FREE;
		if (w_current->actionfeedback_mode == OUTLINE) {
			o_complex_translate_display(w_current, diff_x, diff_y, o_current);
			/* do it twice to get rid of old outline */
			o_complex_translate_display(w_current, diff_x, diff_y, o_current);
		} else {
		/* why are we doing this here...? probably a reason hack */
			get_complex_bounds(w_current, o_current, &rleft, &rtop, 
						&rright, &rbottom); 
		}
	}


	if (w_current->CONTROLKEY) {
		test_x = abs(w_current->last_x - w_current->start_x);
        	test_y = abs(w_current->last_y - w_current->start_y);
        	if (test_x >= test_y) {
               		w_current->last_y = w_current->start_y;
        	} else {
               		w_current->last_x = w_current->start_x;
        	}            
	}

	if (w_current->actionfeedback_mode == BOUNDINGBOX) {
		/* this slows things down quite a bit hack */
		get_complex_bounds(w_current, o_current, &rleft, &rtop, 
						&rright, &rbottom); 
		diff_x = w_current->last_x - w_current->start_x;
        	diff_y = w_current->last_y - w_current->start_y;
        	gdk_gc_set_foreground(w_current->bounding_xor_gc, color);
        	gdk_draw_rectangle(w_current->window, 
				w_current->bounding_xor_gc, FALSE,
                		rleft+diff_x, rtop+diff_y,
                		rright-rleft, rbottom-rtop); 

		return;
	}

	diff_x = w_current->last_x - w_current->start_x;
	diff_y = w_current->last_y - w_current->start_y;

	/* have I mentioned how temp this is? */
	/* make this general so that all lists can be moved ... hack */
	o_complex_translate2(w_current, diff_x, diff_y, o_current);	

}


void
o_erasebounding(TOPLEVEL *w_current, OBJECT *o_current)
{
	int diff_x, diff_y;
	int rleft, rtop, rright, rbottom;

	if (o_current == NULL) {
		/* this is an error condition */
		w_current->event_state = SELECT;
		w_current->inside_action = 0;
		return;
	}

	if (w_current->actionfeedback_mode == OUTLINE) {
		return;
	}

	get_complex_bounds(w_current, o_current, &rleft, &rtop, 
				&rright, &rbottom); 

	diff_x = w_current->last_x - w_current->start_x;
        diff_y = w_current->last_y - w_current->start_y;

	gdk_gc_set_foreground(w_current->gc, 
			x_get_color(w_current->background_color) );
        gdk_draw_rectangle(w_current->window, w_current->gc, FALSE, 
        		rleft+diff_x, rtop+diff_y,
        		rright-rleft, rbottom-rtop); 
}

