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

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "funcs.h"

#include "colors.h"

#include "../include/prototype.h"

struct st_pslines {
	char *line;
};

struct st_pslines header[] = {
#if 0
	{ "%!\n" 			},
	{ "\n" 				}, 

	{ "/inch {72 mul} def\n" 	}, 
	{ "\n" 				}, 
 	{ "/setcoords\n" 		}, 
	{ "{\n" 			}, 
	{ "8.5 72 mul 0 translate\n" 	}, 
	{ "90 rotate\n" 		}, 
	{ ".46 .46 scale\n" 		}, /* was .46 .46 */
	{ "} def\n" 			}, 


	{ "\n"				},
	{ "/mils\n"			},
	{ "{\n"				},
	{ "1000 div inch\n"		},
	{ "} def\n"			},
#endif


/* box function */
	{ "\n"				},
	{ "/box {\n"			},
 	{ "exch dup 0 rlineto\n"	},
        { "exch 0 exch rlineto\n"	},
        { "neg 0 rlineto\n"		},
        { "closepath\n"			},
        { "stroke\n"			},
	{ "} def\n"			},     

/* filled box function */
	{ "/fbox {\n"			},
 	{ "exch dup 0 rlineto\n"	},
        { "exch 0 exch rlineto\n"	},
        { "neg 0 rlineto\n"		},
        { "closepath\n"			},
        { "fill\n"			},
	{ "} def\n"			},     

#if 0
	{ "\n"				},
	{ "/text\n"			},
	{ "{\n"				},
	{ "gsave\n"			},
 	{ "moveto\n"			},
	{ "2 -2 scale\n"		},
	{ "-90 rotate\n"		},

 	{ "moveto\n"			},
	{ "2 2 scale\n"			},
	{ "show\n"			},
	{ "grestore\n"			},
	{ "} def\n"			},    
	{ "\n"				},
#endif

/* set the coords to the wanted scale and rotation */
	{ "\n\nsetcoords\n"			},
	{ "\n"				},
	{ NULL }
};

struct st_pslines footer[] = {
	{ "\n"				},
	{ "showpage\n"			},
	{ NULL }
};

void
f_print_set_color(FILE *fp, int color) 
{
	char *string;

	/* DO NOT free string... it's a reference to a malloced */
	/* string, there is *no* memory leak here */
	string = s_color_ps_string(color);

	if (string) {
			fprintf(fp, "%s setrgbcolor\n", string);
	} else {
			fprintf(fp, "0 0 0 setrgbcolor\n");
	}
}

/* paper size is in inches and floats */
float
f_print_header(TOPLEVEL *w_current, FILE *fp, int paper_size_x, int paper_size_y, 
	int world_right, int world_bottom)
{
        int i;
        struct st_pslines *ptr;
	float x, y;
	float final;

/* define an inch */
	fprintf(fp, "%%!\n");
	fprintf(fp, "/inch {72 mul} def\n");

/* define mils */
	fprintf(fp, "\n");
	fprintf(fp, "/mils\n");
	fprintf(fp, "{\n");
	fprintf(fp, "1000 div inch\n");
	fprintf(fp, "} def\n");

	fprintf(fp, "\n");

/* coordinate setup function */
 	fprintf(fp, "/setcoords\n{\n");


	/* regular if it's portrait */
	if (w_current->print_orientation == LANDSCAPE) {
		fprintf(fp, "%d mils 0 translate\n", paper_size_y);
		fprintf(fp, "90 rotate\n");
	}

	x = (float) (paper_size_x)/world_right;
	y = (float) (paper_size_y)/world_bottom;

	final = min(x, y);

/* make the finale scale factor a bit smaller */
	final = final - final/10;

	fprintf(fp, "%f %f scale\n", final, final);

	fprintf(fp, "} def\n");

	if (w_current->print_color) {
		fprintf(fp, "\n");	
		f_print_set_color(fp, w_current->print_color_background);
		fprintf(fp, "0 0 moveto\n612 0 rlineto 0 792 rlineto -612 0 rlineto\nclosepath fill\n");
	}


/* print out rest of the header */
        i = 0;
        ptr = &header[0];
        while( ptr->line != NULL ) {
                fprintf(fp, "%s", ptr->line);
                i++;
                ptr = &header[i];
        }

	if (w_current->print_output_capstyle == BUTT_CAP) {
		fprintf(fp, "0 setlinecap\n");
	} else if (w_current->print_output_capstyle == SQUARE_CAP) {
		fprintf(fp, "2 setlinecap\n");
	} else if (w_current->print_output_capstyle == ROUND_CAP) {
		fprintf(fp, "1 setlinecap\n");
	}

	return(final);
}

void
f_print_footer(FILE *fp)
{
	int i;
	struct st_pslines *ptr;
	i = 0;

	ptr = &footer[0];

	while( ptr->line != NULL ) {
		fprintf(fp, "%s", ptr->line);
		i++;
		ptr = &footer[i];
	}

}

/* what happens if snap is off? */
/* hack deal with this !!!!!!!! */
void
f_print_objects(TOPLEVEL *w_current, FILE *fp, OBJECT *head, 
	int start_x, int start_y, float scale)
{
	OBJECT *o_current=NULL;
	int origin_x, origin_y, bottom, right;
	
	if (start_x == -1 && start_y == -1) {
		world_get_complex_bounds(w_current, head, &origin_x, &origin_y, 
			&right, &bottom);


/* right, bottom b-o r-o*/
		if (w_current->print_orientation == PORTRAIT) {

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


	origin_x = 0;
	origin_y = 0;

	o_current = head;

	while ( o_current != NULL ) {

		if (o_current->type != OBJ_HEAD) {

			switch (o_current->type) {
				case(OBJ_LINE):
					o_line_print(w_current, fp, o_current,
						origin_x, origin_y);
				break;
	
				case(OBJ_NET):
					o_net_print(w_current, fp, o_current,
						origin_x, origin_y);
				break;

				case(OBJ_BUS):
					o_bus_print(w_current, fp, o_current,
						origin_x, origin_y);
				break;
	
				case(OBJ_BOX):
					o_box_print(w_current, fp, o_current,
						origin_x, origin_y);
				break;
			
				case(OBJ_CIRCLE):
					o_circle_print(w_current, fp, o_current,
						origin_x, origin_y);
				break;

				case(OBJ_COMPLEX):
					fprintf(fp, "gsave\n");

					f_print_objects(w_current, fp, 
						o_current->complex,
						origin_x, origin_y, scale);
					fprintf(fp, "grestore\n");
				break;

				case(OBJ_TEXT):
					if (o_current->visibility == VISIBLE) {
						fprintf(fp, "gsave\n");
			
					if (w_current->text_output == VECTOR_FONTS) {	
						f_print_objects(w_current, 
							fp, 
							o_current->complex,
							origin_x, origin_y, scale);
					} else {

						o_text_print(w_current, fp, 
						o_current,
						origin_x, origin_y);

					}

						fprintf(fp, "grestore\n");
					}
				break;


				case(OBJ_PIN):
					o_pin_print(w_current, fp, o_current,
						origin_x, origin_y);
				break;
	
				case(OBJ_ARC):
					o_arc_print(w_current, fp, o_current,
						origin_x, origin_y);
				break;

				default:
					fprintf(stderr, "Error type!\n");
					exit(-1);
				break;
			}

			fprintf(fp, "\n");
		} 
	o_current = o_current->next;
	}

	return;
}

void
f_print(TOPLEVEL *w_current, char *filename)
{
	FILE *fp;
	int origin_x, origin_y, bottom, right;
	float scale;

	/* new CONN stuff */
	o_conn_disconnect_update(w_current->page_current);

	/* dots are breaking my filename selection hack hack !!!! */
	fp = fopen(filename, "w");
	/* check to see if it worked */ 

/*	printf("%d %d\n", w_current->paper_width, w_current->paper_height);*/

	world_get_complex_bounds(w_current, 
			w_current->page_current->object_head, 
			&origin_x, &origin_y, 
			&right, &bottom);


	if (w_current->print_output_type == LIMITS) {

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

		f_print_objects(w_current, fp, 
			w_current->page_current->object_head,
			-1, -1, scale);
	} else {
		scale = f_print_header(w_current, fp, 
			w_current->paper_width, w_current->paper_height, 
			w_current->page_current->right - 
			w_current->page_current->left,  
			w_current->page_current->bottom -
			w_current->page_current->top);

#if DEBUG 
		printf("scale: %f\n", scale);
#endif	
		f_print_objects(w_current, fp, 
			w_current->page_current->object_head,
			w_current->page_current->left,
			w_current->page_current->top, scale);
	} 

	f_print_footer(fp);

	fclose(fp);
}


void
f_print_set_type(TOPLEVEL *w_current, int type)
{
	w_current->print_output_type = type;
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


