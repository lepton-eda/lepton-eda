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
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include <guile/gh.h>

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "s_passing.h"
#include "o_types.h"

#include "colors.h"

#include "../include/globals.h"
#include "../include/prototype.h"


#ifdef HAS_LIBGD
/* what happens if snap is off? */
/* hack deal with this !!!!!!!! */
void
f_image_write_objects(TOPLEVEL *w_current, OBJECT *head, 
	int start_x, int start_y, float scale)
{
	OBJECT *o_current=NULL;
	int origin_x, origin_y, bottom, right;
	
	if (start_x == -1 && start_y == -1) {
		world_get_complex_bounds(w_current, head, &origin_x, &origin_y, 
			&right, &bottom);


/* right, bottom b-o r-o*/
		if (w_current->image_orientation == PORTRAIT) {

		/*temp remove if (origin_x != 0 && origin_y != 0) { */
			origin_x = origin_x - (int)  ((w_current->paper_height - (float) (right-origin_x) * (float) scale)/2 / (float) scale);
			origin_y = origin_y - (int) ((w_current->paper_width - (float) (bottom-origin_y) * (float) scale)/2 / (float) scale);

		/*}*/
		} else { /* landscape */

			origin_x = origin_x - (int)  ((w_current->paper_width - (float) (right-origin_x) * (float) scale)/2 / (float) scale);
			origin_y = origin_y - (int) ((w_current->paper_height - (float) (bottom-origin_y) * (float) scale)/2 / (float) scale);

		}
	} else {
		origin_x = start_x;
		origin_y = start_y;
	}

	if (head == NULL) {
		return;
	}

#if 0
	/* highly temp, might break everything */
	/* what a hack!!!!!!!!!! fix this... (by removing the dead code that */
	/* deals with the origin_x, and origin_y stuff */
	if (origin_x != 0 && origin_y != 0) {
		fprintf(fp, "\n%% Translate origin to the right place...\n");

		if (origin_x < 0) {
			fprintf(fp, "%d mils ", -origin_x);
		} else {
			fprintf(fp, "-%d mils ", origin_x);
		}

		if (origin_y < 0) {
			fprintf(fp, "%d mils ", -origin_y);
		} else {
			fprintf(fp, "-%d mils ", origin_y);
		}

		fprintf(fp, "translate\n\n");
		/*fprintf(fp, "-%d mils -%d mils translate\n\n", origin_x, origin_y); */
	} 
#endif


	origin_x = 0;
	origin_y = 0;

	o_current = head;


	while ( o_current != NULL ) {

		if (o_current->type != OBJ_HEAD) {

			switch (o_current->type) {
				case(OBJ_LINE):
					o_line_image_write(w_current, o_current,
						origin_x, origin_y);
				break;

				case(OBJ_PIN):
					o_pin_image_write(w_current, o_current,
						origin_x, origin_y);
				break;

				case(OBJ_COMPLEX):

					f_image_write_objects(w_current, 
						o_current->complex,
						origin_x, origin_y, scale);
				break;

				case(OBJ_NTEXT):
					if (o_current->visibility == VISIBLE) {
			
					//if (w_current->text_output == VECTOR_FONTS) {	
						f_image_write_objects(w_current, 
							o_current->complex,
							origin_x, origin_y, scale);
					//} else {
#if 0
						o_ntext_image_write(w_current, fp, 
						o_current,
						origin_x, origin_y);

					//}
#endif

					}
				break;

				case(OBJ_NET):
					o_net_image_write(w_current, o_current,
						origin_x, origin_y);

				break;

				case(OBJ_CIRCLE):
					o_circle_image_write(w_current, 
						o_current,
						origin_x, origin_y);
				break;

				case(OBJ_ARC):
					o_arc_image_write(w_current, o_current,
						origin_x, origin_y);
				break;

				case(OBJ_BOX):
					o_box_image_write(w_current, o_current,
						origin_x, origin_y);
				break;
			
				default:
					fprintf(stderr, "Error type!\n");
					exit(-1);
				break;
			}

		} 
	o_current = o_current->next;
	}

	return;
}

void
f_image_write(TOPLEVEL *w_current, char *filename, int width, int height)
{
	int origin_x, origin_y, bottom, right;
	float scale;
	int save_height, save_width;

	/* new ALES stuff */
	o_ales_disconnect_update(w_current->page_current);

	/* dots are breaking my filename selection hack hack !!!! */
	

/*	printf("%d %d\n", w_current->paper_width, w_current->paper_height);*/

	world_get_complex_bounds(w_current, 
			w_current->page_current->object_head, 
			&origin_x, &origin_y, 
			&right, &bottom);


	save_width = w_current->width;
	save_height = w_current->height;

	w_current->width = width;
	w_current->height = height;

	o_image_create(width, height);

	/* try to use recalc here */
	o_redraw_all(w_current);

#if 0
	if (w_current->image_output_type == LIMITS) {

		if (w_current->print_orientation == LANDSCAPE) {
			scale = f_print_header(w_current, fp, 
				w_current->paper_width, 
				w_current->paper_height,  
			 	right-origin_x, bottom-origin_y);	
		} else {
			scale = f_print_header(w_current, fp, 
				w_current->paper_height,  
				w_current->paper_width, 
			 	right-origin_x, bottom-origin_y);	
		}

#if DEBUG
		printf("scale: %f\n", scale);
#endif

		f_image_write_objects(w_current, fp, 
			w_current->page_current->object_head,
			-1, -1, scale);
	} else {

#if DEBUG 
		printf("scale: %f\n", scale);
#endif	
		f_image_write_objects(w_current, fp, 
			w_current->page_current->object_head,
			w_current->page_current->left,
			w_current->page_current->top, scale);
	} 
#endif

	f_image_write_objects(w_current,
			w_current->page_current->object_head,
			w_current->page_current->left,
			w_current->page_current->top, scale);
	
	o_image_write(filename);
	o_image_close();

	w_current->width = save_width;
	w_current->height = save_height;

	/* try to use recalc here... */
	o_redraw_all(w_current);


}


void
f_image_set_type(TOPLEVEL *w_current, int type)
{
	w_current->image_output_type = type;
}
#endif



/* don't comment this in, since it is outside of a function... */
#if 0
				case(OBJ_COMPLEX):
					fprintf(fp, "gsave\n");
					fprintf(fp, "%d mils %d mils translate\n",
						o_current->y, o_current->x);

					temp_tail = object_tail;
        				temp_parent = object_parent;
        				object_parent = o_complex;
					o_complex = add_head();
				        ADDING_SEL = 1;
        				o_complex = o_read(o_complex, o_current->complex_filename);
				        ADDING_SEL = 0;
					o_complex = return_head(o_complex);
        				object_tail = temp_tail;
        				object_parent = temp_parent;   

					f_print_objects(fp, o_complex);
				        REMOVING_SEL = 1;
					s_delete_list_fromstart(o_complex);
				        REMOVING_SEL = 0;
					o_complex=NULL;
					fprintf(fp, "grestore\n");
					break;
#endif


