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

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#ifdef HAS_LIBGDGEDA
#include <gdgeda/gd.h>
#endif

#include <guile/gh.h>

#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "o_types.h"

#include "colors.h"

#include "../include/globals.h"
#include "../include/prototype.h"


#ifdef HAS_LIBGDGEDA
/* what happens if snap is off? */
/* hack deal with this !!!!!!!! */
void
f_image_write_objects(TOPLEVEL *w_current, OBJECT *head, 
	int start_x, int start_y, float scale, int color_mode)
{
	OBJECT *o_current=NULL;
	int origin_x, origin_y;
	
	if (head == NULL) {
		return;
	}

	origin_x = 0;
	origin_y = 0;

	o_current = head;


	while ( o_current != NULL ) {

		if (o_current->type != OBJ_HEAD) {

			switch (o_current->type) {
				case(OBJ_LINE):
					o_line_image_write(w_current, o_current,
						origin_x, origin_y, color_mode);
				break;

				case(OBJ_PIN):
					o_pin_image_write(w_current, o_current,
						origin_x, origin_y, color_mode);
				break;

				case(OBJ_COMPLEX):

					f_image_write_objects(w_current, 
						o_current->complex,
						origin_x, origin_y, scale, color_mode);
				break;

				case(OBJ_NTEXT):
					if (o_current->visibility == VISIBLE) {
			
					/*if (w_current->text_output == VECTOR_FONTS) {	*/
						f_image_write_objects(w_current, 
							o_current->complex,
							origin_x, origin_y, scale, color_mode);
					/*} else {*/
#if 0
						o_ntext_image_write(w_current, fp, 
						o_current,
						origin_x, origin_y);

					/*}*/
#endif

					}
				break;

				case(OBJ_NET):
					o_net_image_write(w_current, o_current,
						origin_x, origin_y, color_mode);

				break;

				case(OBJ_CIRCLE):
					o_circle_image_write(w_current, 
						o_current,
						origin_x, origin_y, color_mode);
				break;

				case(OBJ_ARC):
					o_arc_image_write(w_current, o_current,
						origin_x, origin_y, color_mode);
				break;

				case(OBJ_BOX):
					o_box_image_write(w_current, o_current,
						origin_x, origin_y, color_mode);
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
#endif

void
f_image_write(TOPLEVEL *w_current, char *filename, int width, int height, 
	int color_mode)
{

#ifdef HAS_LIBGDGEDA

	int origin_x, origin_y, bottom, right;
	float scale=0.0;

	/* new ALES stuff */
	o_ales_disconnect_update(w_current->page_current);

	/* dots are breaking my filename selection hack hack !!!! */
	

/*	printf("%d %d\n", w_current->paper_width, w_current->paper_height);*/

	world_get_complex_bounds(w_current, 
			w_current->page_current->object_head, 
			&origin_x, &origin_y, 
			&right, &bottom);


	o_image_create(width, height, color_mode);

	f_image_write_objects(w_current,
			w_current->page_current->object_head,
			w_current->page_current->left,
			w_current->page_current->top, scale, color_mode);
	
	o_image_write(filename);
	o_image_close();
#endif

}


void
f_image_set_type(TOPLEVEL *w_current, int type)
{
	w_current->image_output_type = type;
}



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


