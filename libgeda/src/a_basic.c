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

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "../include/prototype.h"


void
o_save_embedded(TOPLEVEL *w_current, OBJECT *object_list, FILE *fp)
{
	OBJECT *o_current=NULL;
	char buf[1024];
	char *out;

        /* make sure you init net_consolide to false (default) in all */
        /* programs */
        if (w_current->net_consolidate == TRUE) {
		o_net_consolidate(w_current);
	}
	
	o_current = object_list;

	while ( o_current != NULL ) {

		if (o_current->type != OBJ_HEAD) {

			if (o_current->attribute == 0) {

				switch (o_current->type) {

					case(OBJ_LINE):
						out = (char *) o_line_save(buf, o_current);
					break;
	
					case(OBJ_NET):
						out = (char *) o_net_save(buf, o_current);
					break;

					case(OBJ_BUS):
						out = (char *) o_bus_save(buf, o_current);
					break;
	
					case(OBJ_BOX):
						out = (char *) o_box_save(buf, o_current);
					break;
			
					case(OBJ_CIRCLE):
						out = (char *) o_circle_save(buf, o_current);
					break;

					case(OBJ_COMPLEX):
						out = (char *) o_complex_save(buf, o_current);
						if (strncmp(
						  o_current->complex_clib, 
						  "EMBEDDED", 8) == 0) {
						     fprintf(fp, "[\n");
								
							o_save_embedded(
							   w_current,
							   o_current->complex,
							   fp);

						     fprintf(fp, "]\n");
						}
					break;

					case(OBJ_TEXT):
						out = (char *) o_text_save(buf, o_current);
					break;

					case(OBJ_PIN):
						out = (char *) o_pin_save(buf, o_current);
					break;
	
					case(OBJ_ARC):
						out = (char *) o_arc_save(buf, o_current);
					break;

					default:
						fprintf(stderr, "Error type!\n");
						exit(-1);
					break;
				}

				/* output the line */
				fprintf(fp, "%s\n", out);

				/* save those attributes */
				if (o_current->attribs != NULL && o_current->attribs->next != NULL) {
					o_save_attribs(fp, o_current->attribs->next);
				}

			}
		} 
	o_current = o_current->next;
	}
}

void
o_save_write_header(FILE *fp)
{
	fprintf(fp, "v %s\n", VERSION);
}

void
o_save(TOPLEVEL *w_current, char *filename)
{
	OBJECT *o_current=NULL;
	FILE *fp;
	char buf[200];
	char *out;
	int already_wrote=0;
	
	fp = fopen(filename, "w");
	
	if (fp == NULL) {
		s_log_message("o_save: Could not open [%s]\n", filename);
		return;
	}


	o_current = w_current->page_current->object_head;

	/* make sure you init net_consolide to false (default) in all */
        /* programs */
        if (w_current->net_consolidate == TRUE) {
		o_net_consolidate(w_current);
	}

	o_save_write_header(fp);

	while ( o_current != NULL ) {

		if (o_current->type != OBJ_HEAD) {

			if (o_current->attribute == 0) {

				switch (o_current->type) {

					case(OBJ_LINE):
						out = (char *) o_line_save(buf, o_current);
					break;
	
					case(OBJ_NET):
						out = (char *) o_net_save(buf, o_current);
					break;

					case(OBJ_BUS):
						out = (char *) o_bus_save(buf, o_current);
					break;
	
					case(OBJ_BOX):
						out = (char *) o_box_save(buf, o_current);
					break;
			
					case(OBJ_CIRCLE):
						out = (char *) o_circle_save(buf, o_current);
					break;

					case(OBJ_COMPLEX):
						out = (char *) o_complex_save(buf, o_current);
						fprintf(fp, "%s\n", out);
						already_wrote=1;
						if (strncmp(
						  o_current->complex_clib, 
						  "EMBEDDED", 8) == 0) {
						     fprintf(fp, "[\n");
								
							o_save_embedded(
							   w_current,
							   o_current->complex,
							   fp);

						     fprintf(fp, "]\n");
						}
					break;

					case(OBJ_TEXT):
						out = (char *) o_text_save(buf, o_current);
					break;

					case(OBJ_PIN):
						out = (char *) o_pin_save(buf, o_current);
					break;
	
					case(OBJ_ARC):
						out = (char *) o_arc_save(buf, o_current);
					break;

					default:
						fprintf(stderr, "Error type!\n");
						exit(-1);
					break;
				}

				/* output the line */
				if (!already_wrote) {
					fprintf(fp, "%s\n", out);
				} else {
					already_wrote=0;
				}

				/* save those attributes */
				if (o_current->attribs != NULL && o_current->attribs->next != NULL) {
					o_save_attribs(fp, o_current->attribs->next);
				}

			}
		} 
	o_current = o_current->next;
	}

	fclose(fp);

}

OBJECT *
o_read(TOPLEVEL *w_current, OBJECT *object_list, char *filename)
{
	FILE *fp;
	char buf[1024];
	char string[1024];
	char objtype;
	char version[20]; /* 20 should be big enough hack */
	OBJECT *object_list_save=NULL;
	OBJECT *temp_tail=NULL;
	OBJECT *temp_parent=NULL;
	OBJECT *object_before_attr=NULL;

	/* fill version with default string, 19981013 was the last version */ 
	/* which didn't have versioning */
	strcpy(version, "19981013"); 

 	fp = fopen(filename, "r");

	if (fp == NULL) {
		s_log_message("Could not open [%s]\n", filename);
		return(NULL);
	}

	while ( fgets(buf, 1024, fp) != NULL) {

		sscanf(buf, "%c", &objtype);
		switch (objtype) {

			case(OBJ_LINE):
				object_list = (OBJECT *) o_line_read(w_current, object_list, buf, version);
			break;


			case(OBJ_NET):
				object_list = (OBJECT *) o_net_read(w_current, object_list, buf, version);
			break;

			case(OBJ_BUS):
				object_list = (OBJECT *) o_bus_read(w_current, object_list, buf, version);
			break;

			case(OBJ_BOX):
				object_list = (OBJECT *) o_box_read(w_current, object_list, buf, version);
			break;
		
			case(OBJ_CIRCLE):
				object_list = (OBJECT *) o_circle_read(w_current, object_list, buf, version);
			break;

			case(OBJ_COMPLEX):
				object_list = (OBJECT *) o_complex_read(w_current, object_list, buf, version);

				/* this is necessary because complex may add
				   attributes which float */
				/* needed now? */
				object_list = (OBJECT *) return_tail(object_list);
			break;

			case(OBJ_TEXT):
				fgets(string, 1024, fp); /* check if invalid */
				object_list = (OBJECT *) o_text_read(w_current, object_list, buf, string, version);
			break;

			case(OBJ_PIN):
				object_list = (OBJECT *) o_pin_read(w_current, object_list, buf, version);
			break;

			case(OBJ_ARC):
				object_list = (OBJECT *) o_arc_read(w_current, object_list, buf, version);
			break;

			case(STARTATTACH_ATTR): 
				object_before_attr = object_list;
				/* first is the fp */
				/* 2nd is the object to get the attributes */ 
				object_list = (OBJECT *) o_read_attribs(w_current, fp, object_list, version);

				/* slots only apply to complex objects */
				if (object_before_attr->type == OBJ_COMPLEX) {
					o_attrib_slot_update(w_current, object_before_attr);	
				}

				/* need this? */
				/*object_list = return_tail(object_list);*/
				object_before_attr = NULL;
			break;

			case(START_EMBEDDED): 
				object_list_save = object_list;
				object_list = object_list_save->complex;
				
				temp_tail =
					w_current->page_current->object_tail;
				temp_parent =
					w_current->page_current->object_parent;
				w_current->page_current->object_parent = 
					object_list;
			break;

			case(END_EMBEDDED): 
				object_list = object_list_save;
				/* don't do this since objects are already
				 * stored/read translated 
				o_complex_world_translate(w_current, 
						object_list->x, 
						object_list->y, 
						object_list->complex);
				*/
				w_current->page_current->object_tail = temp_tail;
				w_current->page_current->object_parent = temp_parent;
			break;

			case(ENDATTACH_ATTR): 
			break;	

			case(INFO_FONT): 
				o_text_set_info_font(buf);
			break;	

			case(COMMENT):
				/* do nothing */
			break;

			case(VERSION_CHAR):
				sscanf(buf, "v %s\n", version);

#if DEBUG
				printf("Found version: %s\n", version);
#endif
				/* do nothing for now */
			break;

			default:
				fprintf(stderr, "Read garbage in [%s] :\n>>\n%s<<\n",
					filename, buf);
			break;
		}

	}
	fclose(fp);
	return(object_list);
}

/* this really doesn't belong here */
/* you need more of a core routine first */
/* yes.. this is the core routine, just strip out the drawing stuff */
/* move it to o_complex_scale */
void
o_scale(TOPLEVEL *w_current, OBJECT *list, int x_scale, int y_scale)
{
	OBJECT *o_current;

	/* this is okay if you just hit scale and have nothing selected */
	if (list == NULL) { 
		/* w_current->event_state = SELECT;*/
		/* i_update_status(w_current, "Select Mode"); not here */
/*		w_current->inside_action = 0;*/
		return;
	}


	o_current = list;


	while (o_current != NULL) {

#if 0
		/* first get the real object */	
		real = (OBJECT *) 
			o_list_search(w_current->page_current->object_head, 
					o_current);

		if (real == NULL) {
			printf("Oops! you tried to scale an object which doesn't exists\n");
			return;	
		}
#endif


		switch(o_current->type) {

			case(OBJ_LINE):
				/* erase the current selection */
				w_current->override_color =
                                        w_current->background_color;
				o_redraw_single(w_current, o_current);
                                /* o_line_draw(w_current, o_current);*/
                                w_current->override_color = -1;

#if 0
				o_line_scale_world(w_current, 
					x_scale, y_scale, real);
#endif

				o_line_scale_world(w_current, 
					x_scale, y_scale, o_current);


#if 0
				o_redraw_single(w_current, o_current);
				o_line_draw(w_current, real);
#endif
			break;
		}

		o_current = o_current->next;
	}

	/* don't do this at this level */
	/* w_current->page_current->CHANGED=1;*/
#if 0 
	o_redraw_selected(w_current);
#endif
}
