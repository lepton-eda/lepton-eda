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
#include <string.h>
#include <sys/stat.h>
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
#include "s_passing.h"
#include "o_types.h"
#include "colors.h"
#include "funcs.h"

#include "../include/prototype.h"

#define WINONLY	1
#define BACKING 2

/* font storage and friends are staying global so that all can access */
#define NUM_CHARS 256

OBJECT font_set[NUM_CHARS];

void
get_ntext_bounds(TOPLEVEL *w_current, OBJECT *o_current, int *left, int *top, int *right, int *bottom)
{
	get_complex_bounds(w_current, o_current->complex, left, top, right, bottom);
}

void
world_get_ntext_bounds(TOPLEVEL *w_current, OBJECT *o_current, int *left, int *top, int *right, int *bottom)
{
	world_get_complex_bounds(w_current, o_current->complex, left, top, right, bottom);
}

OBJECT *
o_ntext_add_head(void)
{
	OBJECT *ret=NULL;

	/* you had a nasty memory leak here */
	/* you don't create the memory here, it's created for you in */
	/* add_object */

	strcpy(p_name, "ntext_head"); 

	/* max numbers ?? */
	p_left = 99999;
	p_top = 99999;
	p_right = 0;
	p_bottom = 0;
	p_x = p_y = -1;
	p_screen_x = p_screen_y = -1;
	p_color = -1;

        p_type = OBJ_HEAD; /* make this of head type hack */
        p_line_points = NULL;
        p_circle = NULL;
        p_action_func = error_if_called;
        p_sel_func = error_if_called;
        p_draw_func = error_if_called;
	p_complex = NULL;
	p_complex_basename[0] = '\0';
	p_complex_clib[0] = '\0';
	p_attached_to = NULL;
	p_attribs = NULL;
	/* add p_attrib and p_attached_to */
	p_text_string[0] = '\0';

        ret = (OBJECT *) add_object(NULL);
	return(ret);
}

void
o_ntext_init(void)
{
	int i;

	for (i = 0 ; i < NUM_CHARS; i++) {
		font_set[i].complex = NULL;
		font_set[i].text_size = 100;
	}
}

void
o_ntext_print_set(void)
{
	OBJECT *o_current;
	int i;
	
	for (i = 'A' ; i < 'Z'+1; i++) {
		if (font_set[i].complex != NULL) {
			printf("%c: LOADED\n", i);	
			/* for (o_current=font_set[i].complex; o_current; 
					o_current=o_current->next) */
			for (o_current=return_tail(font_set[i].complex); o_current; 
					o_current=o_current->prev) 
			{
				printf("  %s\n", o_current->name);	
			}
		} else {
			printf("%c: unloaded\n", i);	
		}
	}
}

OBJECT *
o_ntext_load_font(TOPLEVEL *w_current, char needed_char)
{
	char temp_string[256]; /* remove me HACK !!!!!!!!!!!!!!!!!!!!! */
	OBJECT *temp_parent;

	switch(needed_char) {
		
		case(' '):
			sprintf(temp_string, "%s/space.sym", 
					w_current->font_directory);
		break;
		
		case('!'):
			sprintf(temp_string, "%s/excl.sym", 
					w_current->font_directory);
		break;
		
		case(','):
			sprintf(temp_string, "%s/comma.sym", 
					w_current->font_directory);
		break;

		case('('):
			sprintf(temp_string, "%s/lparen.sym", 
					w_current->font_directory);
		break;

		case(')'):
			sprintf(temp_string, "%s/rparen.sym", 
					w_current->font_directory);
		break;

		case('-'):
			sprintf(temp_string, "%s/minus.sym", 
					w_current->font_directory);
		break;

		case('+'):
			sprintf(temp_string, "%s/plus.sym", 
					w_current->font_directory);
		break;

		case('#'):
			sprintf(temp_string, "%s/pound.sym", 
					w_current->font_directory);
		break;

		case('?'):
			sprintf(temp_string, "%s/quest.sym", 
					w_current->font_directory);
		break;

		case('"'):
			sprintf(temp_string, "%s/quote.sym", 
					w_current->font_directory);
		break;

		case(':'):
			sprintf(temp_string, "%s/colon.sym", 
					w_current->font_directory);
		break;

		case('@'):
			sprintf(temp_string, "%s/at.sym", 
					w_current->font_directory);
		break;

		case('='):
			sprintf(temp_string, "%s/equal.sym", 
					w_current->font_directory);
		break;

		case('>'):
			sprintf(temp_string, "%s/more.sym", 
					w_current->font_directory);
		break;

		case('<'):
			sprintf(temp_string, "%s/less.sym", 
					w_current->font_directory);
		break;

		case('/'):
			sprintf(temp_string, "%s/slash.sym", 
					w_current->font_directory);
		break;

		case('$'):
			sprintf(temp_string, "%s/dollar.sym", 
					w_current->font_directory);
		break;

		case(';'):
			sprintf(temp_string, "%s/semi.sym", 
					w_current->font_directory);
		break;

		case('&'):
			sprintf(temp_string, "%s/amper.sym", 
					w_current->font_directory);
		break;

		case('\\'):
			sprintf(temp_string, "%s/backslash.sym", 
					w_current->font_directory);
		break;

		case('{'):
			sprintf(temp_string, "%s/lbrace.sym", 
					w_current->font_directory);
		break;

		case('}'):
			sprintf(temp_string, "%s/rbrace.sym", 
					w_current->font_directory);
		break;

		case('\''):
			sprintf(temp_string, "%s/apost.sym", 
					w_current->font_directory);
		break;

		case('`'):
			sprintf(temp_string, "%s/backtick.sym", 
					w_current->font_directory);
		break;

		case('^'):
			sprintf(temp_string, "%s/caret.sym", 
					w_current->font_directory);
		break;

		case('%'):
			sprintf(temp_string, "%s/percent.sym", 
					w_current->font_directory);
		break;

		case('['):
			sprintf(temp_string, "%s/lbrack.sym", 
					w_current->font_directory);
		break;

		case(']'):
			sprintf(temp_string, "%s/rbrack.sym", 
					w_current->font_directory);
		break;

		case('*'):
			sprintf(temp_string, "%s/astericks.sym", 
					w_current->font_directory);
		break;

		case('.'):
			sprintf(temp_string, "%s/period.sym", 
					w_current->font_directory);
		break;

		case('_'):
			sprintf(temp_string, "%s/under.sym", 
					w_current->font_directory);
		break;

		case('~'):
			sprintf(temp_string, "%s/tilde.sym", 
					w_current->font_directory);
		break;

		case('|'):
			sprintf(temp_string, "%s/vbar.sym", 
					w_current->font_directory);
		break;
		/* finnish / swedish characters */
		case('Ä'):
			sprintf(temp_string, "%s/A-diaeresis.sym", 
					w_current->font_directory);
		break;

		case('ä'):
			sprintf(temp_string, "%s/a-diaeresis.sym", 
					w_current->font_directory);
		break;

		case('Ö'):
			sprintf(temp_string, "%s/O-diaeresis.sym", 
					w_current->font_directory);
		break;

		case('ö'):
			sprintf(temp_string, "%s/o-diaeresis.sym", 
					w_current->font_directory);
		break;

		case('Å'):
			sprintf(temp_string, "%s/A-ring.sym", 
					w_current->font_directory);
		break;

		case('å'):
			sprintf(temp_string, "%s/a-ring.sym", 
					w_current->font_directory);
		break;

		default:	
			sprintf(temp_string, "%s/%c.sym", 
					w_current->font_directory,
				 	needed_char);
		break;

	}

	if ( access(temp_string, R_OK) != 0 ) {
		s_log_message("Could not find character %c definition\n",needed_char, temp_string);
		sprintf(temp_string, "%s/quest.sym", w_current->font_directory);
		if ( access(temp_string, R_OK) != 0 ) {
			fprintf(stderr, "Could not load question font char -- check font-directory keyword\n");
			exit(-1);
		}
    	}

	/* printf("loading: %s\n", temp_string);*/

	font_set[(int) needed_char].complex = o_ntext_add_head();

        temp_parent = w_current->page_current->object_parent;
	/* set the addition of attributes to the head node */
	w_current->page_current->object_parent = font_set[(int) needed_char].complex;

	font_set[(int) needed_char].complex = o_read(w_current, font_set[(int) needed_char].complex, temp_string);
	w_current->page_current->object_parent = temp_parent;

	font_set[(int) needed_char].complex = return_head(font_set[(int) needed_char].complex);
	
	return(font_set[(int) needed_char].complex);
}

int
o_ntext_width(TOPLEVEL *w_current, char *string, int size) 
{
	int i;
	int len;
	int width=0;

	len = strlen(string);

	for (i = 0 ; i < len ; i++ ) {
		if (font_set[(int) string[i]].complex == NULL) {
			o_ntext_load_font(w_current, string[i]);
		}

		/* if (string[i] == ' ') {
			width = width + size*14; 
		} else { 
		}*/

			width = width + size*font_set[(int) string[i]].text_size;
	}

	/* the size is a fudge factor */
	return(width - size*10);
}


OBJECT *
o_ntext_create_string(TOPLEVEL *w_current, OBJECT *object_list, 
	char *string, int size, int color, int x, int y, int angle)
{
        OBJECT *temp_tail=NULL;
	OBJECT *temp_list;
	OBJECT *start_of_char;
	int i;
	int len;
	int x_offset;
	int y_offset;


	temp_list = object_list;


	/* error condition hack */
	if (string == NULL) {
		return(NULL);
	}
	
	len = strlen(string);
	/* now read in the chars */
	temp_tail = w_current->page_current->object_tail;


	x_offset = x;
	y_offset = y;

#if 1
	switch(angle) {
		case(180):
			x_offset = x_offset - o_ntext_width(w_current, string, size/2);
			y_offset = y_offset - 26*size/2;
			angle = 0;
		break;
	}
#endif

#if DEBUG
	printf("width: %d\n", o_ntext_width(w_current, string, size/2));
	printf("1 %d %d\n", x_offset, y_offset);
#endif

	for (i = 0 ; i < len ; i++ ) {
		
		if (font_set[(int) string[i]].complex == NULL) {
			o_ntext_load_font(w_current, string[i]);
		}

		start_of_char = temp_list;

		if (font_set[(int) string[i]].complex->next) {
			temp_list = o_list_copy_all(w_current, 
				    font_set[(int) string[i]].complex->next, 
				    temp_list, NORMAL);
			start_of_char = start_of_char->next;
			o_complex_set_color(w_current, color, start_of_char);	
			o_scale(w_current, start_of_char, size/2, size/2);
		
			/* do this if you want to stack chars */
			/* we don't want to do that for now */
			o_ntext_rotate_lowlevel(w_current, x, y, angle, start_of_char);
			o_complex_world_translate(w_current, 
					x_offset, y_offset, 
					start_of_char);
		}

		switch(angle) {
			case(0):	
				x_offset = (x_offset) + 
			 	  size/2*font_set[(int) string[i]].text_size;
			break;
		
			case(90):
				y_offset = (y_offset) + 
				  size/2*font_set[(int) string[i]].text_size;
			break;

			case(180):
				x_offset = (x_offset) - 
			 	  size/2*font_set[(int) string[i]].text_size;
			break;
		
			case(270):
				y_offset = (y_offset) - 
				  size/2*font_set[(int) string[i]].text_size;
			break;
		}
	}

	/* don't set the head */	

	w_current->page_current->object_tail = temp_tail;

#if DEBUG
	printf("2 %d %d\n", x_offset, y_offset);
#endif
	return(object_list);
}

OBJECT *
o_ntext_add(TOPLEVEL *w_current, OBJECT *object_list, char type, int color, int x, int y, int angle, char *string, int size, int visibility, int show_name_value)
{
	OBJECT *temp_list=NULL;
        OBJECT *temp_parent=NULL;
	int screen_x, screen_y;
	int left, right, top, bottom;
	char name[1025];
	char value[1025]; /* ugg hack */
	char output_string[1025];

	if (string == NULL) {
		return(NULL);
	}

	/* allocate it hack */
	if ( strlen(string) > 1024 ) {
		fprintf(stderr, "text string too long!\n");
		return(NULL);
	} 

	p_type = type;
	strcpy(p_name, "ntext");
	strcpy(p_text_string, string);
	p_text_len = strlen(string);
	p_text_size = size;
	p_x = x;
	p_y = y;
	
	p_angle = angle;

	WORLDtoSCREEN(w_current, x, y, &screen_x, &screen_y);
	p_screen_x = screen_x;
	p_screen_y = screen_y;

	p_draw_func = (void *) ntext_draw_func;  /* questionable cast */
	p_sel_func = (void *) select_func;  /* questionable cast */
	p_line_points = NULL;
        p_circle = NULL;
	p_color = color;
	p_complex = NULL;
	p_visibility = visibility; 
	p_show_name_value = show_name_value;

	/* create the object in the main list */
	object_list = (OBJECT *) add_object(object_list);
	
#if 1

	/* object_list->text_string = malloc(sizeof(char)*strlen(string)+1);
	strcpy(object_list->text_string, string);*/


	/* fix up actual string here */
	if (o_attrib_get_name_value(string, name, value)) {
		switch(show_name_value) {
			case(SHOW_NAME_VALUE):
				strcpy(output_string, string);
			break;

			case(SHOW_NAME):
				if (name[0] != '\0') {
					strcpy(output_string, name);
				}
			break;

			case(SHOW_VALUE):
				if (value[0] != '\0') {
					strcpy(output_string, value);
				}
			break;
		}
	} else {
		strcpy(output_string, string);
	}

#endif


	/* now start working on the complex */
	temp_list = o_ntext_add_head();

        temp_parent = w_current->page_current->object_parent;
	/* set the addition of attributes to the head node */
	w_current->page_current->object_parent = temp_list;


	if (visibility == VISIBLE) {
		object_list->complex = 
			o_ntext_create_string(w_current, temp_list, 
					      output_string, size, color,
					      x, y, angle); 
	} else {
		object_list->complex = NULL;
		s_delete(w_current, temp_list);
	}

	w_current->page_current->object_parent = temp_parent;

	get_ntext_bounds(w_current, object_list, &left, &top, &right, &bottom);

	/* deal with this */	
	object_list->left = left;
	object_list->top = top;
	object_list->right = right;
	object_list->bottom = bottom;

	return(object_list);
}

void
o_ntext_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	
	if (o_current->visibility == INVISIBLE) {
		return;
	}

	/* libhack */
	/* o_complex_recalc(w_current, o_current);*/
}


OBJECT *
o_ntext_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char string[], char *version)
{
	char type; 
	int x, y;
	int color;
	int size;
	int visibility;
	int show_name_value;
	int angle;

	string = remove_nl(string);	

	sscanf(buf, "%c %d %d %d %d %d %d %d\n", &type, &x, &y, &color, &size,
						&visibility, &show_name_value, 
						&angle);	

	object_list = o_ntext_add(w_current, object_list, type, color, x, y, 
				angle, string, 
				size, visibility, show_name_value);

	return(object_list);
}

void
o_ntext_set_info_font(char buf[])
{
	char type; 
	int width;
	char character;
	int temp;
	int special=0;
	char string[81]; /* not sure if this is right ? hack */
	
	strcpy(string, remove_nl(buf));	

	/* the right way to do this is to use the ascii code in the character
	 * field hack
	 */ 
	sscanf(string, "%c %c %d %d\n", &type, &character, &width, &special); 

	if (special == 1) {
		character = 32;
	}

	temp = character;
	if ( temp >= 0 && temp <= 255) {
		font_set[(int) character].text_size = width;
	}
}

char *
o_ntext_save(char *buf, OBJECT *object)
{
	int x, y;
	int color;
	int size;
	char *string;

        x = object->x;
        y = object->y;

	color = object->color;
	string = object->text_string;
	size = object->text_size;

        sprintf(buf, "%c %d %d %d %d %d %d %d\n%s", object->type, x, y, color, size,  object->visibility, object->show_name_value, object->angle, string);
        return(buf);
}
       

void
o_ntext_recreate(TOPLEVEL *w_current, OBJECT *o_current)
{
	OBJECT *temp_parent;
	char name[1025];
	char value[1025]; /* ugg hack */
	char output_string[1025]; /* uggg hack */

	if (o_attrib_get_name_value(o_current->text_string, name, value)) {
		switch(o_current->show_name_value) {
			case(SHOW_NAME_VALUE):
				strcpy(output_string, o_current->text_string);
			break;

			case(SHOW_NAME):
				if (name[0] != '\0') {
					strcpy(output_string, name);
				}
			break;

			case(SHOW_VALUE):
				if (value[0] != '\0') {
					strcpy(output_string, value);
				}
			break;
		}
	} else {
		strcpy(output_string, o_current->text_string);
	}

	o_list_delete_rest(w_current, o_current->complex);

	temp_parent = w_current->page_current->object_parent;
	/* set the addition of attributes to the head node */
	w_current->page_current->object_parent = o_current->complex;

	if (o_current->visibility == VISIBLE) {

		/* need to create that head node if complex is null */
		if (o_current->complex == NULL) {
			o_current->complex = o_ntext_add_head();
		}

		o_current->complex = 
			o_ntext_create_string(w_current, o_current->complex, 
					      output_string, 
					      o_current->text_size, 
					      o_current->color, 
					      o_current->x, o_current->y,
					      o_current->angle); 
	} else {
		o_current->complex = NULL;
	}

	w_current->page_current->object_parent = temp_parent;
}

void
o_ntext_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
	o_complex_translate(w_current, dx, dy, object);	
}

void
o_ntext_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *o_current)
{
	int screen_x, screen_y;
	int left, right, top, bottom;
	
	
	o_current->x = o_current->x + x1;
	o_current->y = o_current->y + y1;
			
	/* update screen coords */
	WORLDtoSCREEN(w_current, o_current->x, o_current->y, 
			&screen_x, &screen_y);

	o_current->screen_x = screen_x;
	o_current->screen_y = screen_y;
						
	o_complex_world_translate(w_current, x1, y1, o_current->complex);

	/* update bounding box */
	/* do it */
	get_ntext_bounds(w_current, o_current, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;
}

OBJECT *
o_ntext_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;

	new_obj = o_ntext_add(w_current, list_tail, OBJ_NTEXT, 
	        o_current->color, 
		o_current->x, o_current->y, o_current->angle,
		o_current->text_string, o_current->text_size, 
		o_current->visibility, o_current->show_name_value); 


	/* yes it is attached to something */	
#if 0 /* no longer here */
	if (o_current->attached_to && !w_current->ADDING_SEL) {
		if (o_current->attached_to->copied_to) {
			o_attrib_attach(w_current, 
				w_current->page_current->object_parent,
		                new_obj, o_current->attached_to->copied_to);		
		
				/* satisfied copy request */	
				o_current->attached_to->copied_to = NULL;
		} else {
			/* not destined to be copied so... */
			new_obj->attribute = 0; /* no longer an attribute */
		}
	} 
#endif


	/* don't do this anymore
	new_obj->attribute = 0;*/

	return(new_obj);
}

void
o_ntext_freeallfonts(TOPLEVEL *w_current)
{
	int i;

	for (i = 0 ; i < NUM_CHARS; i++) {
		if (font_set[i].complex != NULL) {
	 		s_delete_list_fromstart(w_current, font_set[i].complex);
			font_set[i].complex = NULL;
		}
	}

}

void
o_ntext_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	char output_string[1025]; /* hack */
	char name[1025]; /* hack */
        char value[1025]; /* hack */
	int len;
	int i;


	if (!o_current->text_string) {
		return;
	}

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	fprintf(fp, "/Helvetica findfont\n");
        fprintf(fp, "%f scalefont\n", (float) o_current->text_size*1.4);
        fprintf(fp, "setfont\n");
        fprintf(fp, "\n");

	/* fprintf(fp, "newpath\n");*/

	if (o_attrib_get_name_value(o_current->text_string, name, value)) {
		switch(o_current->show_name_value) {
			case(SHOW_NAME_VALUE):
					strcpy(output_string, 
						o_current->text_string);
					break;

			case(SHOW_NAME):
					if (name[0] != '\0') {
						strcpy(output_string, name);
					}
					break;

			case(SHOW_VALUE):
					if (value[0] != '\0') {
						strcpy(output_string, value);
					}
					break;
		}
	} else {
		strcpy(output_string, o_current->text_string);
	}
	
	fprintf(fp, "%d mils %d mils moveto\n", 
			o_current->x-origin_x,
			o_current->y-origin_y);

	if (o_current->angle) {
		fprintf(fp, "%d rotate\n", o_current->angle); 
	}

	/* fprintf(fp, "(%s) show\n", output_string);*/

	fprintf(fp, "(");
	len = strlen(output_string);
	for (i = 0 ; i < len; i++) {  
		if (output_string[i] == '(' || output_string[i] == ')' ||
		    output_string[i] == '\\' ) {
			fprintf(fp, "\\");
		}

		fprintf(fp, "%c", output_string[i]);

	}
	fprintf(fp, ") show\n");

#if 0 /* old way */
	fprintf(fp, "(%s) true charpath\n", output_string);

	fprintf(fp, ".1 setlinewidth\n");
	fprintf(fp, "fill\n");
#endif
}

/* takes world coords as the center point as well as a true angle */
void
o_ntext_rotate_lowlevel(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object)
{
	OBJECT *o_current=NULL;

	/* translate object to origin */
	/* o_ntext_translate_world(w_current, -world_centerx, -world_centery, object);*/

	/* rotate_point_90(object->x, object->y, &newx, &newy);*/
	
	/* o_ntext_translate_world(w_current, world_centerx, world_centery, object);*/
	
	o_current = object;

	while ( o_current != NULL ) {
		switch(o_current->type) {
			case(OBJ_LINE):
				o_line_rotate_world(w_current, 0, 0, angle, o_current);
			break;
		}
		o_current=o_current->next;
	}
}

void
o_ntext_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, int angle_change, OBJECT *object)
{
	int newx, newy;
	int origx, origy;
	int x, y;
	
	origx = object->x;
	origy = object->y;

	object->angle = ( object->angle + angle_change ) % 360;

#if DEBUG 
	printf("rotating text angle: %d\n", angle);
	printf("rotating text angle_change: %d\n", angle_change);
	printf("final text angle: %d\n",  object->angle);
#endif

	x = origx + (-world_centerx);
	y = origy + (-world_centery);
	
	rotate_point_90(x, y, angle_change, &newx, &newy);

	x = newx + (world_centerx);
	y = newy + (world_centery);
	
	o_ntext_translate_world(w_current, x-object->x, y-object->y, object);

	o_ntext_recreate(w_current, object);
}

void
o_ntext_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle, int angle_change, OBJECT *object)
{
	int newx, newy;
	int origx, origy;
	int world_centerx, world_centery;
	int x, y;
	
	SCREENtoWORLD(w_current, centerx, centery,
			&world_centerx,
			&world_centery);
	

	origx = object->x;
	origy = object->y;

	object->angle = angle;

	x = origx + (-world_centerx);
	y = origy + (-world_centery);
	
	rotate_point_90(x, y, angle_change, &newx, &newy);

	x = newx + (world_centerx);
	y = newy + (world_centery);
	
	o_ntext_translate_world(w_current, x-object->x, y-object->y, object);

	o_ntext_recreate(w_current, object);
}

void
o_ntext_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int newx=0, newy=0;
	int origx, origy;
	int world_centerx, world_centery;
	int x, y;
	char output_string[1025]; /* hack */
	char name[1025]; /* hack */
        char value[1025]; /* hack */
	
	SCREENtoWORLD(w_current, centerx, centery,
			&world_centerx,
			&world_centery);

	origx = object->x;
	origy = object->y;

	/* translate to origin */
	x = origx + (-world_centerx);
	y = origy + (-world_centery);

	if (o_attrib_get_name_value(object->text_string, name, value)) {
		switch(object->show_name_value) {
			case(SHOW_NAME_VALUE):
					strcpy(output_string, 
						object->text_string);
					break;

			case(SHOW_NAME):
					if (name[0] != '\0') {
						strcpy(output_string, name);
					}
					break;

			case(SHOW_VALUE):
					if (value[0] != '\0') {
						strcpy(output_string, value);
					}
					break;
		}
	} else {
		strcpy(output_string, object->text_string);
	}

	switch (object->angle) {

		case(0): 
			newx = -(x + o_ntext_width(w_current, 
						   output_string, 
						   object->text_size/2)); 
		break;

		case(90):
			newx = -( x - 26*object->text_size/2);
		break;

		case(180):
			newx = -(x - o_ntext_width(w_current, 
						   output_string, 
						   object->text_size/2)); 
		break;

		case(270):
			newx = -( x + 26*object->text_size/2);
		break;


		default:
			fprintf(stderr, "Invalid angle used!\n");
			return;
		break;

	}

	newy = y;

	x = newx + (world_centerx);
	y = newy + (world_centery);
	
	/* don't know if this is needed? */	
	o_ntext_translate_world(w_current, x-object->x, y-object->y, object);

	o_ntext_recreate(w_current, object);
}

void
o_ntext_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	int newx=0, newy=0;
	int origx, origy;
	int x, y;
	char output_string[1025]; /* hack */
	char name[1025]; /* hack */
        char value[1025]; /* hack */
	
	origx = object->x;
	origy = object->y;

	/* translate to origin */
	x = origx + (-world_centerx);
	y = origy + (-world_centery);

	if (o_attrib_get_name_value(object->text_string, name, value)) {
		switch(object->show_name_value) {
			case(SHOW_NAME_VALUE):
					strcpy(output_string, 
						object->text_string);
					break;

			case(SHOW_NAME):
					if (name[0] != '\0') {
						strcpy(output_string, name);
					}
					break;

			case(SHOW_VALUE):
					if (value[0] != '\0') {
						strcpy(output_string, value);
					}
					break;
		}
	} else {
		strcpy(output_string, object->text_string);
	}

	switch (object->angle) {

		case(0): 
			newx = -(x + o_ntext_width(w_current, 
						   output_string, 
						   object->text_size/2)); 

		break;

		case(90):
			newx = -( x - 26*object->text_size/2);
		break;

		case(180):
			newx = -(x - o_ntext_width(w_current, 
						   output_string, 
						   object->text_size/2)); 
		break;

		case(270):
			newx = -( x + 26*object->text_size/2);
		break;

		default:
			fprintf(stderr, "Invalid angle used!\n");
			return;
		break;

	}

	newy = y;

	x = newx + (world_centerx);
	y = newy + (world_centery);

	object->x = x;
	object->y = y;

	/* don't know if this is needed ?*/	
	/* o_ntext_translate_world(w_current, x-object->x, y-object->y, object);*/
	o_ntext_recreate(w_current, object);
}

#if 0 /* interesting, but currently unused code */
void
o_ntext_return_center(TOPLEVEL *w_current, OBJECT *o_current, int *centerx, int *centery)
{
	int text_height; 
	int text_width;

	text_height = 26*o_current->text_size/2;

	/* this will NOT NOT NOT work with attributes */
	text_width = o_ntext_width(w_current, o_current->text_string, 
				   o_current->text_size/2); 
	
	switch(o_current->angle) {
		case(0):
			*centerx = o_current->x + text_width/2;
			*centery = o_current->y + text_height/2;
		break;

		case(90):
			*centerx = o_current->x - text_height/2;
			*centery = o_current->y + text_width/2;
		break;

		case(180):
			*centerx = o_current->x - text_width/2;
			*centery = o_current->y - text_height/2;
		break;

		case(270):
			*centerx = o_current->x + text_height/2;
			*centery = o_current->y - text_width/2;
		break;
	}	
}

/* the complex here is the complex of a complex object */
o_ntext_change_angle(TOPLEVEL *w_current, OBJECT *complex, int new_angle)
{
	OBJECT *o_current;
	int centerx, centery;

	o_current = complex;

	while (o_current != NULL) {
		if (o_current->type == OBJ_NTEXT) {
			o_current->angle = new_angle;

			/* change world to non */
			o_ntext_return_center(w_current, o_current, &centerx, &centery);

			o_ntext_translate_world(w_current, 
						-centerx, -centery, o_current);

			o_ntext_mirror_world(w_current, 0, 0, 
				o_current);
/* 
			o_ntext_rotate_world(w_current, 0, 0, 
				new_angle, 180, o_current);
*/

/*			o_ntext_rotate_world(w_current, 0, 0, new_angle, 
					180, o_current)*/

			o_ntext_translate_world(w_current, 
						centerx, centery, o_current);

/* 			o_ntext_rotate_lowlevel(w_current, 
				0, 0, new_angle, 180, o_current->complex);*/

#if 0
			w_current->override_color =
                                        w_current->background_color;
                        o_ntext_draw(w_current, o_current);
                        w_current->override_color = -1;
#endif

			o_ntext_recreate(w_current, o_current);
		}
		o_current = o_current->next;
	}
}
#endif
