/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
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
get_text_bounds(TOPLEVEL *w_current, OBJECT *o_current, int *left, int *top, int *right, int *bottom)
{
	get_complex_bounds(w_current, o_current->text->prim_objs, left, top, right, bottom);
}

void
world_get_text_bounds(TOPLEVEL *w_current, OBJECT *o_current, int *left, int *top, int *right, int *bottom)
{
	world_get_complex_bounds(w_current, o_current->text->prim_objs, left, top, right, bottom);
}

OBJECT *
o_text_add_head(void)
{
	OBJECT *new_node=NULL;

	new_node = s_basic_init_object("text_head");
	new_node->type = OBJ_HEAD;

	/* don't need to do this for head nodes */
        /* ret = s_basic_link_object(new_node, NULL);*/
	return(new_node);
}

void
o_text_init(void)
{
	int i;

	for (i = 0 ; i < NUM_CHARS; i++) {
		font_set[i].font_prim_objs = NULL;
		font_set[i].font_text_size = 100;
	}
}

void
o_text_print_set(void)
{
	OBJECT *o_current;
	int i;
	
	for (i = 'A' ; i < 'Z'+1; i++) {
		if (font_set[i].font_prim_objs != NULL) {
			printf("%c: LOADED\n", i);	
			/* for (o_current=font_set[i].font_prim_objs; o_current; 
					o_current=o_current->next) */
			for (o_current=return_tail(font_set[i].font_prim_objs); o_current; 
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
o_text_load_font(TOPLEVEL *w_current, char needed_char)
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
#ifdef __CYGWIN32__
			/* this is needed since WinNT file systems are 
			 * case insensitive, and cannot tell the difference 
			 * between A.sym and a.sym.  So we create a_.sym -  
			 * z_.sym, this loads up the chars */
			if (needed_char >= 'a' && needed_char <= 'z') {
				sprintf(temp_string, "%s/%c_.sym", 
					w_current->font_directory,
				 	needed_char);
			} else {
				sprintf(temp_string, "%s/%c.sym", 
					w_current->font_directory,
				 	needed_char);
			}
#else
			sprintf(temp_string, "%s/%c.sym", 
					w_current->font_directory,
				 	needed_char);
#endif
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

	font_set[(int) needed_char].font_prim_objs = o_text_add_head();

        temp_parent = w_current->page_current->object_parent;
	/* set the addition of attributes to the head node */
	w_current->page_current->object_parent = font_set[(int) needed_char].font_prim_objs;

	font_set[(int) needed_char].font_prim_objs = o_read(w_current, font_set[(int) needed_char].font_prim_objs, temp_string);
	w_current->page_current->object_parent = temp_parent;

	font_set[(int) needed_char].font_prim_objs = return_head(font_set[(int) needed_char].font_prim_objs);
	
	return(font_set[(int) needed_char].font_prim_objs);
}

int
o_text_height(TOPLEVEL *w_current, int size) 
{
	/* 26 is the height of a single char */
	/* which represents a character which is 2 pts high */
	/* So size has to be divided in half */
	return(26*size/2);

}

int
o_text_width(TOPLEVEL *w_current, char *string, int size) 
{
	int i;
	int len;
	int width=0;

	len = strlen(string);

	for (i = 0 ; i < len ; i++ ) {
		if (font_set[(int) string[i]].font_prim_objs == NULL) {
			o_text_load_font(w_current, string[i]);
		}

		/* if (string[i] == ' ') {
			width = width + size*14; 
		} else { 
		}*/

			width = width + size*font_set[(int) string[i]].font_text_size;
	}

	/* the size is a fudge factor */
	return(width - size*10);
}


OBJECT *
o_text_create_string(TOPLEVEL *w_current, OBJECT *object_list, 
	char *string, int size, int color, int x, int y, int alignment, 
	int angle)
{
        OBJECT *temp_tail=NULL;
	OBJECT *temp_list;
	OBJECT *start_of_char;
	int i;
	int len;
	int x_offset;
	int y_offset;
	int text_width;
	int text_height;
	int sign=1;

	temp_list = object_list;


	/* error condition hack */
	if (string == NULL) {
		return(NULL);
	}
	
	len = strlen(string);
	/* now read in the chars */
	temp_tail = w_current->page_current->object_tail;

	text_height = o_text_height(w_current, size);
	text_width = o_text_width(w_current, string, size/2);

	switch(angle) {
		case(0):
			sign = -1;
			break;
		case(90):
			sign = 1;
			break;
		case(180):
			sign = 1;
			break;
		case(270):
			sign = -1;
			break;
	}

	if (angle == 0 || angle == 180) {
		switch(alignment) {

			case(LOWER_LEFT):
				x_offset = x;
				y_offset = y;
			break;
		
			case(MIDDLE_LEFT):
				x_offset = x;
				y_offset = y + sign*0.5*text_height;
			break;
	
			case(UPPER_LEFT):
				x_offset = x;
				y_offset = y + sign*text_height;
			break;
	
			case(LOWER_MIDDLE):
				x_offset = x + sign*0.5*text_width;
				y_offset = y;
			break;
	
			case(MIDDLE_MIDDLE):
				x_offset = x + sign*0.5*text_width;
				y_offset = y + sign*0.5*text_height;
			break;
	
			case(UPPER_MIDDLE):
				x_offset = x + sign*0.5*text_width;
				y_offset = y + sign*text_height;
	
			break;
	
			case(LOWER_RIGHT):
				x_offset = x + sign*text_width;
				y_offset = y;
			break;
	
			case(MIDDLE_RIGHT):
				x_offset = x + sign*text_width;
				y_offset = y + sign*0.5*text_height;
			break;
	
			case(UPPER_RIGHT):
				x_offset = x + sign*text_width;
				y_offset = y + sign*text_height;
			break;

			default: 
				fprintf(stderr, "Got an invalid text alignment [%d]\n",
					alignment); 
				fprintf(stderr, "Defaulting to Lower Left");
				alignment = LOWER_LEFT;
				x_offset = x;
				y_offset = y;
			break;
		}
	} else { /* angle is 90 or 270 */
		switch(alignment) {

			case(LOWER_LEFT):
				x_offset = x;
				y_offset = y;
			break;
		
			case(MIDDLE_LEFT):
				x_offset = x + sign*0.5*text_height;
				y_offset = y;
			break;
	
			case(UPPER_LEFT):
				x_offset = x + sign*text_height;
				y_offset = y;
			break;
	
			case(LOWER_MIDDLE):
				x_offset = x;
				y_offset = y - sign*0.5*text_width;
			break;
	
			case(MIDDLE_MIDDLE):
				x_offset = x + sign*0.5*text_height;
				y_offset = y - sign*0.5*text_width;
			break;
	
			case(UPPER_MIDDLE):
				x_offset = x + sign*text_height;
				y_offset = y - sign*0.5*text_width;
	
			break;
	
			case(LOWER_RIGHT):
				x_offset = x;
				y_offset = y - sign*text_width;
			break;
	
			case(MIDDLE_RIGHT):
				x_offset = x + sign*0.5*text_height;
				y_offset = y - sign*text_width;
			break;
	
			case(UPPER_RIGHT):
				x_offset = x + sign*text_height;
				y_offset = y - sign*text_width;
			break;

			default: 
				fprintf(stderr, "Got an invalid text alignment [%d]\n",
					alignment); 
				fprintf(stderr, "Defaulting to Lower Left");
				alignment = LOWER_LEFT;
				x_offset = x;
				y_offset = y;
			break;
		}

	}

	switch(angle) {
		case(180):
			x_offset = x_offset - text_width;
			y_offset = y_offset - text_height;
			angle = 0;
		break;
	}

#if DEBUG
	printf("width: %d\n", o_text_width(w_current, string, size/2));
	printf("1 %d %d\n", x_offset, y_offset);
#endif

	for (i = 0 ; i < len ; i++ ) {
		
		if (font_set[(int) string[i]].font_prim_objs == NULL) {
			o_text_load_font(w_current, string[i]);
		}

		start_of_char = temp_list;

		if (font_set[(int) string[i]].font_prim_objs->next) {
			temp_list = o_list_copy_all(w_current, 
				    font_set[(int) string[i]].font_prim_objs->next, 
				    temp_list, NORMAL_FLAG);
			start_of_char = start_of_char->next;
			o_complex_set_color(start_of_char, color);	
			o_scale(w_current, start_of_char, size/2, size/2);
		
			/* do this if you want to stack chars */
			/* we don't want to do that for now */
			o_text_rotate_lowlevel(w_current, x, y, angle, start_of_char);
			o_complex_world_translate(w_current, 
					x_offset, y_offset, 
					start_of_char);
		}

		switch(angle) {
			case(0):	
				x_offset = (x_offset) + 
			 	  size/2*font_set[(int) string[i]].font_text_size;
			break;
		
			case(90):
				y_offset = (y_offset) + 
				  size/2*font_set[(int) string[i]].font_text_size;
			break;

			case(180):
				x_offset = (x_offset) - 
			 	  size/2*font_set[(int) string[i]].font_text_size;
			break;
		
			case(270):
				y_offset = (y_offset) - 
				  size/2*font_set[(int) string[i]].font_text_size;
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
o_text_add(TOPLEVEL *w_current, 
	OBJECT *object_list, 
	char type, int color, int x, int y, int alignment,
	int angle, char *string, int size, 
	int visibility, int show_name_value)
{
	OBJECT *new_node=NULL;
	OBJECT *temp_list=NULL;
        OBJECT *temp_parent=NULL;
	TEXT *text;
	int left, right, top, bottom;
	char name[1025];
	char value[1025]; /* ugg hack */
	char output_string[1025];

	if (string == NULL) {
		return(NULL);
	}

	/* TODO remove the 1024 max string limitation somehow? */
	if ( strlen(string) > 1024 ) {
		fprintf(stderr, "text string too long!\n");
		return(NULL);
	} 

	new_node = s_basic_init_object("text");
	new_node->type = type;

	text = (TEXT *) malloc(sizeof(TEXT));

	text->string = u_basic_strdup(string);
	text->length = strlen(string);
	text->size = size;
	text->alignment = alignment;
	text->x = x;
	text->y = y;
	WORLDtoSCREEN(w_current, x, y, &text->screen_x, &text->screen_y);
	text->angle = angle;

	new_node->text = text;

	/* TODO: questionable cast */
	new_node->draw_func = (void *) text_draw_func;  
	/* TODO: questionable cast */
	new_node->sel_func = (void *) select_func;  

	new_node->color = color;
	new_node->visibility = visibility; 
	new_node->show_name_value = show_name_value;

	/* create the object in the main list */
	/* object_list points to the object */
	/* I use it below as a sanity check to make sure it was linked */
	/* properly */ 
	object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

	/* fix up actual string here */ 
	if (o_attrib_get_name_value(string, name, value)) {

		switch(show_name_value) {
			case(SHOW_NAME_VALUE):
				strcpy(output_string, string);
			break;

			case(SHOW_NAME):
				if (name[0] != '\0') {
					strcpy(output_string, name);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
					    "Got an improper attribute: %s\n", 
					    string);
					strcpy(output_string, "invalid");

				}
			break;

			case(SHOW_VALUE):
				if (value[0] != '\0') {
					strcpy(output_string, value);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
					    "Got an improper attribute: %s\n", 
					    string);
					strcpy(output_string, "invalid");
				}
			break;
		}
	} else {
		strcpy(output_string, string);
	}


	/* now start working on the complex */
	temp_list = o_text_add_head();

        temp_parent = w_current->page_current->object_parent;
	/* set the addition of attributes to the head node */
	w_current->page_current->object_parent = temp_list;

	if (visibility == VISIBLE) {
		object_list->text->prim_objs = 
			o_text_create_string(w_current, temp_list, 
					      output_string, size, color,
					      x, y, alignment, angle); 
			object_list->text->displayed_length = strlen(output_string);
	} else {
		object_list->text->prim_objs = NULL;
		object_list->text->displayed_length = 0;
		s_delete(w_current, temp_list);
	}

	w_current->page_current->object_parent = temp_parent;

	get_text_bounds(w_current, object_list, &left, &top, &right, &bottom);

	/* set the new object's bounding box */
	object_list->left = left;
	object_list->top = top;
	object_list->right = right;
	object_list->bottom = bottom;

	return(object_list);
}

void
o_text_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
	int left, right, top, bottom;

	if (o_current->visibility == INVISIBLE) {
		return;
	}

	get_complex_bounds(w_current, o_current->text->prim_objs, 
	                   &left, &top, &right, &bottom);
	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;

	WORLDtoSCREEN(w_current, o_current->text->x,
                  o_current->text->y,
                  &o_current->text->screen_x,
                  &o_current->text->screen_y);
}


OBJECT *
o_text_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[], char string[], char *version)
{
	char type; 
	int x, y;
	int color;
	int size;
	int visibility;
	int show_name_value;
	int angle;
	int alignment;
	int int_version;

	string = remove_nl(string);	

	int_version = atoi(version);

	if (int_version < 20000220) {
		sscanf(buf, "%c %d %d %d %d %d %d %d\n", &type, &x, &y, 
					        &color, &size,
						&visibility, &show_name_value, 
						&angle);	
		alignment = LOWER_LEFT; /* older versions didn't have this */
	} else {
		sscanf(buf, "%c %d %d %d %d %d %d %d %d\n", &type, &x, &y, 
					        &color, &size,
						&visibility, &show_name_value, 
						&angle, &alignment);	
	}

        if (size == 0) {
                fprintf(stderr, "Found a zero size text string [ %c %d %d %d %d %d %d %d %d ]\n", type, x, y, color, size, visibility, show_name_value, angle, alignment);
                s_log_message("Found a zero size text string [ %c %d %d %d %d %d %d %d %d ]\n", type, x, y, color, size, visibility, show_name_value, angle, alignment);
        }

	switch(angle) {
	
		case(0):
		case(90):
		case(180):
		case(270):
		break;

		default:
                	fprintf(stderr, "Found an unsupported text angle [ %c %d %d %d %d %d %d %d %d ]\n", type, x, y, color, size, visibility, show_name_value, angle, alignment);
                	s_log_message("Found an unsupported text angle [ %c %d %d %d %d %d %d %d %d ]\n", type, x, y, color, size, visibility, show_name_value, angle, alignment);
                	s_log_message("Setting angle to 0\n");
			angle=0;
		break;

	}

	switch(alignment) {
		case(LOWER_LEFT):	
		case(MIDDLE_LEFT):	
		case(UPPER_LEFT):	
		case(LOWER_MIDDLE):	
		case(MIDDLE_MIDDLE):	
		case(UPPER_MIDDLE):	
		case(LOWER_RIGHT):	
		case(MIDDLE_RIGHT):	
		case(UPPER_RIGHT):	
			
		break;
		
		default:
                	fprintf(stderr, "Found an unsupported text alignment [ %c %d %d %d %d %d %d %d %d ]\n", type, x, y, color, size, visibility, show_name_value, angle, alignment);
                	s_log_message("Found an unsupported text alignment [ %c %d %d %d %d %d %d %d %d ]\n", type, x, y, color, size, visibility, show_name_value, angle, alignment);
                	s_log_message("Setting alignment to LOWER_LEFT\n");
			alignment = LOWER_LEFT; 
		break;
	}

	if (color < 0 || color > MAX_COLORS) {
                fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
                s_log_message("Found an invalid color [ %s ]\n", buf);
		s_log_message("Setting color to WHITE\n");
		color = WHITE;
	}

	object_list = o_text_add(w_current, object_list, type, color, x, y, 
				alignment, angle, string, 
				size, visibility, show_name_value);

	return(object_list);
}

void
o_text_set_info_font(char buf[])
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
		font_set[(int) character].font_text_size = width;
	}
}

char *
o_text_save(char *buf, OBJECT *object)
{
	int x, y;
	int color;
	int size;
	char *string;

        x = object->text->x;
        y = object->text->y;

	string = object->text->string;
	size = object->text->size;

	/* Use the right color */
	if (object->saved_color == -1) {
		color = object->color;
	} else {
		color = object->saved_color;
	}

        sprintf(buf, "%c %d %d %d %d %d %d %d %d\n%s", object->type, x, y, 
		color, size,  object->visibility, 
	        object->show_name_value, object->text->angle, 
		object->text->alignment, string);

        return(buf);
}
       

void
o_text_recreate(TOPLEVEL *w_current, OBJECT *o_current)
{
	OBJECT *temp_parent;
	char name[1025];
	char value[1025]; /* ugg hack */
	char output_string[1025]; /* uggg hack */

	if (o_attrib_get_name_value(o_current->text->string, name, value)) {
		switch(o_current->show_name_value) {
			case(SHOW_NAME_VALUE):
				strcpy(output_string, o_current->text->string);
			break;

			case(SHOW_NAME):
				if (name[0] != '\0') {
					strcpy(output_string, name);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
					    "Got an improper attribute: %s\n", 
					    o_current->text->string);
					strcpy(output_string, "invalid");
				}
			break;

			case(SHOW_VALUE):
				if (value[0] != '\0') {
					strcpy(output_string, value);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
					    "Got an improper attribute: %s\n", 
					    o_current->text->string);
					strcpy(output_string, "invalid");
				}
			break;
		}
	} else {
		strcpy(output_string, o_current->text->string);
	}

	o_list_delete_rest(w_current, o_current->text->prim_objs);

	temp_parent = w_current->page_current->object_parent;
	/* set the addition of attributes to the head node */
	w_current->page_current->object_parent = o_current->text->prim_objs;

	if (o_current->visibility == VISIBLE) {

		/* need to create that head node if complex is null */
		if (o_current->text->prim_objs == NULL) {
			o_current->text->prim_objs = o_text_add_head();
		}

		o_current->text->prim_objs = 
			o_text_create_string(w_current, 
					      o_current->text->prim_objs, 
					      output_string, 
					      o_current->text->size, 
					      o_current->color, 
					      o_current->text->x, 
					      o_current->text->y,
					      o_current->text->alignment,
					      o_current->text->angle); 

		o_complex_set_saved_color_only(o_current->text->prim_objs, 
					       o_current->saved_color);
		o_current->text->displayed_length = strlen(output_string);
	} else {
		/* make sure list is truely free */
		s_delete_list_fromstart(w_current, o_current->text->prim_objs);
		o_current->text->prim_objs = NULL;
		o_current->text->displayed_length = 0;
	}

	w_current->page_current->object_parent = temp_parent;
}

void
o_text_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *o_current)
{
	int screen_x, screen_y;
	int left, right, top, bottom;
	
	
	o_current->text->x = o_current->text->x + x1;
	o_current->text->y = o_current->text->y + y1;
			
	/* update screen coords */
	WORLDtoSCREEN(w_current, o_current->text->x, o_current->text->y, 
			&screen_x, &screen_y);

	o_current->text->screen_x = screen_x;
	o_current->text->screen_y = screen_y;
						
	o_complex_world_translate(w_current, x1, y1, o_current->text->prim_objs);

	/* update bounding box */
	/* do it */
	get_text_bounds(w_current, o_current, &left, &top, &right, &bottom);

	o_current->left = left;
	o_current->top = top;
	o_current->right = right;
	o_current->bottom = bottom;
}

OBJECT *
o_text_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
{
	OBJECT *new_obj;
	int color;

	if (o_current->saved_color == -1) {
		color = o_current->color;
	} else {
		color = o_current->saved_color;
	}

	new_obj = o_text_add(w_current, list_tail, OBJ_TEXT, 
	                     color, 
		             o_current->text->x, o_current->text->y, 
	                     o_current->text->alignment, 
	                     o_current->text->angle,
		             o_current->text->string, 
			     o_current->text->size, 
		             o_current->visibility, 
			     o_current->show_name_value); 

	return(new_obj);
}

void
o_text_freeallfonts(TOPLEVEL *w_current)
{
	int i;

	for (i = 0 ; i < NUM_CHARS; i++) {
		if (font_set[i].font_prim_objs != NULL) {
	 		s_delete_list_fromstart(w_current, font_set[i].font_prim_objs);
			font_set[i].font_prim_objs = NULL;
		}
	}

}

void
o_text_print_text_width(FILE *fp, char *output_string)
{
	int len, i;

	fprintf(fp, "(");
	len = strlen(output_string);
	for (i = 0 ; i < len; i++) {  
	    if (output_string[i] == '(' || output_string[i] == ')' || output_string[i] == '\\' ) {
			fprintf(fp, "\\");
		}

		fprintf(fp, "%c", output_string[i]);
	}

	/* convert width to mils */
	/* .95 is a fudge factor */
	fprintf(fp, ") stringwidth pop\n");
}

void
o_text_print_text_height(FILE *fp, int size)
{
        fprintf(fp, "%f\n", (float) size);
}

void
o_text_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current, 
	int origin_x, int origin_y)
{
	char output_string[1025]; /* hack */
	char name[1025]; /* hack */
        char value[1025]; /* hack */
	int sign=1;
	int len;
	int i;
	int x, y;


	if (!o_current->text->string) {
		return;
	}

	if (w_current->print_color) {
		f_print_set_color(fp, o_current->color);
	}

	fprintf(fp, "/Helvetica findfont\n");
        fprintf(fp, "%f scalefont\n", (float) o_current->text->size*1.4);
        fprintf(fp, "setfont\n");
        fprintf(fp, "\n");

	/* fprintf(fp, "newpath\n");*/

	if (o_attrib_get_name_value(o_current->text->string, name, value)) {
		switch(o_current->show_name_value) {
			case(SHOW_NAME_VALUE):
					strcpy(output_string, 
						o_current->text->string);
				break;

			case(SHOW_NAME):
					if (name[0] != '\0') {
						strcpy(output_string, name);
					} else {
						fprintf(stderr, 
					    	    "Got an improper attribute: %s\n", 
					    	o_current->text->string);
						strcpy(output_string, "invalid");
					}
				break;

			case(SHOW_VALUE):
					if (value[0] != '\0') {
						strcpy(output_string, value);
					} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
						fprintf(stderr, 
						    "Got an improper attribute: %s\n", 
					    	o_current->text->string);
						strcpy(output_string, "invalid");
					}
				break;
		}
	} else {
		strcpy(output_string, o_current->text->string);
	}

	switch(o_current->text->angle) {
		case(0):
			sign = -1;
			break;
		case(90):
			sign = 1;
			break;
		case(180):
			sign = 1;
			break;
		case(270):
			sign = -1;
			break;
	}

	x = o_current->text->x;
	y = o_current->text->y;
	if (o_current->text->angle == 0 || o_current->text->angle == 180 ) {

		switch(o_current->text->alignment) {
			case(LOWER_LEFT):
				fprintf(fp, "%d mils %d mils moveto\n", x, y);
/*				x_offset = x; */
/*				y_offset = y; */
			break;

			case(MIDDLE_LEFT):
				fprintf(fp, "%d mils %d mils\n", x, y);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, ".5 mul %d mul add moveto\n", sign);
/*				x_offset = x; */
/*				y_offset = y + sign*0.5*text_height; */
			break;

			case(UPPER_LEFT):
				fprintf(fp, "%d mils %d mils\n", x, y);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "%d mul add moveto\n", sign);
/*				x_offset = x; */
/*				y_offset = y + sign*text_height; */
			break;

			case(LOWER_MIDDLE):
				fprintf(fp, "%d mils ", x);

				o_text_print_text_width(fp, output_string);
				fprintf(fp, ".5 mul %d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				fprintf(fp, "moveto\n");
/*				x_offset = x + sign*0.5*text_width; */
/*				y_offset = y; */
			break;

			case(MIDDLE_MIDDLE):
				fprintf(fp, "%d mils ", x);

				o_text_print_text_width(fp, output_string);
				fprintf(fp, ".5 mul %d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, ".5 mul %d mul add moveto\n", sign);
/*				x_offset = x + sign*0.5*text_width; */
/*				y_offset = y + sign*0.5*text_height; */
			break;

			case(UPPER_MIDDLE):
				fprintf(fp, "%d mils ", x);

				o_text_print_text_width(fp, output_string);
				fprintf(fp, ".5 mul %d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "%d mul add moveto\n", sign);
/*				x_offset = x + sign*0.5*text_width; */
/*				y_offset = y + sign*text_height; */
			break;

			case(LOWER_RIGHT):
				fprintf(fp, "%d mils ", x);

				o_text_print_text_width(fp, output_string);
				fprintf(fp, "%d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				fprintf(fp, "moveto\n");
/*				x_offset = x + sign*text_width; */
/*				y_offset = y; */
			break;

			case(MIDDLE_RIGHT):
				fprintf(fp, "%d mils ", x);

				o_text_print_text_width(fp, output_string);
				fprintf(fp, "%d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, ".5 mul %d mul add moveto\n", sign);
/*				x_offset = x + sign*text_width; */
/*				y_offset = y + sign*0.5*text_height; */
			break;

			case(UPPER_RIGHT):
				fprintf(fp, "%d mils ", x);

				o_text_print_text_width(fp, output_string);
				fprintf(fp, "%d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "%d mul add moveto\n", sign);
/*				x_offset = x + sign*text_width; */
/*				y_offset = y + sign*text_height; */
			break;
		}

#if 0 /* no longer needed */
		if (o_current->text->angle != 0) {
			fprintf(fp, "%d rotate\n", o_current->text->angle); 
		}
#endif

	} else if (o_current->text->angle == 90 || o_current->text->angle == 270) {
		switch(o_current->text->alignment) {

			case(LOWER_LEFT):
				fprintf(fp, "%d mils %d mils moveto\n", x, y);
/*				x_offset = x; */
/*				y_offset = y; */
			break;
		
			case(MIDDLE_LEFT):
				fprintf(fp, "%d mils\n", x); 
						
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, ".5 mul %d mul add\n", sign);
				fprintf(fp, "%d mils moveto\n", y);
/*				x_offset = x + sign*0.5*text_height; */
/*				y_offset = y; */
			break;
	
			case(UPPER_LEFT):
				fprintf(fp, "%d mils\n", x); 
						
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "%d mul add\n", sign);
				fprintf(fp, "%d mils moveto\n", y);
/*				x_offset = x + sign*text_height; */
/*				y_offset = y; */
			break;
	
			case(LOWER_MIDDLE):
				fprintf(fp, "%d mils\n", x);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_width(fp, output_string);
				fprintf(fp, "0.5 mul %d mul sub moveto\n", sign);
/*				x_offset = x; */
/*				y_offset = y - sign*0.5*text_width; */
			break;
	
			case(MIDDLE_MIDDLE):
				fprintf(fp, "%d mils\n", x);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "0.5 mul %d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_width(fp, output_string);
				fprintf(fp, "0.5 mul %d mul sub moveto\n", sign);
/*				x_offset = x + sign*0.5*text_height; */
/*				y_offset = y - sign*0.5*text_width; */
			break;
	
			case(UPPER_MIDDLE):
				fprintf(fp, "%d mils\n", x);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "%d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_width(fp, output_string);
				fprintf(fp, "0.5 mul %d mul sub moveto\n", sign);
/*				x_offset = x + sign*text_height; */
/*				y_offset = y - sign*0.5*text_width; */
	
			break;
	
			case(LOWER_RIGHT):
				fprintf(fp, "%d mils\n", x);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_width(fp, output_string);
				fprintf(fp, "%d mul sub moveto\n", sign);
/*				x_offset = x; */
/*				y_offset = y - sign*text_width; */
			break;
	
			case(MIDDLE_RIGHT):
				fprintf(fp, "%d mils\n", x);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "0.5 mul %d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_width(fp, output_string);
				fprintf(fp, "%d mul sub moveto\n", sign);
/*				x_offset = x + sign*0.5*text_height; */
/*				y_offset = y - sign*text_width; */
			break;
	
			case(UPPER_RIGHT):
				fprintf(fp, "%d mils\n", x);
				o_text_print_text_height(fp, o_current->text->size);
				fprintf(fp, "%d mul add\n", sign);
				fprintf(fp, "%d mils\n", y);
				o_text_print_text_width(fp, output_string);
				fprintf(fp, "%d mul sub moveto\n", sign);
/*				x_offset = x + sign*text_height; */
/*				y_offset = y - sign*text_width; */
			break;
		}

		if (o_current->text->angle) {
			fprintf(fp, "%d rotate\n", o_current->text->angle); 
		}
	} 
	
	if (o_current->text->angle == 180) {

/* old way of doing 180 rotated text */
#if 0
		/* 180 degree rotated text is special... */
		/* subtract width and height from origin and don't */
		/* do the rotation */

#if DEBUG 
		printf("tangle: %f\n", (float) o_current->text->size*1.4*0.0139*1000);	
#endif
		fprintf(fp, "%%%% 180 rotated text\n");
		fprintf(fp, "(");
		len = strlen(output_string);
		for (i = 0 ; i < len; i++) {  
		    if (output_string[i] == '(' || output_string[i] == ')' || output_string[i] == '\\' ) {
				fprintf(fp, "\\");
			}

			fprintf(fp, "%c", output_string[i]);
		}

		/* convert width to mils */
		/* .95 is a fudge factor */
		fprintf(fp, ") stringwidth pop 72 div 1000 mul .95 mul\n");

		fprintf(fp, "%d exch sub mils\n",
				o_current->text->x-origin_x);

		/* the 1.1 is a fudge factor */
		/* 0.0139 is 1/72 points per inch */
		/* 1000 mils per inch */
		fprintf(fp, "%d mils moveto\n", 
			o_current->text->y-origin_y - (int) rint(o_current->text->size*1.1*0.0139*1000));
/* old way of doing 180 rotated text */
#endif


		o_text_print_text_width(fp, output_string); 
		fprintf(fp, "-1.0 mul\n"); /* x distance back */
		o_text_print_text_height(fp, o_current->text->size);
		fprintf(fp, "-1.0 mul\n"); /* y distance down */
		fprintf(fp, "rmoveto\n");
	}


	/* old way, which doesn't allow ('s and )'s to be used in strings */
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
o_text_rotate_lowlevel(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, OBJECT *object)
{
	OBJECT *o_current=NULL;

	/* translate object to origin */
	/* o_text_translate_world(w_current, -world_centerx, -world_centery, object);*/

	/* rotate_point_90(object->text->x, object->text->y, &newx, &newy);*/
	
	/* o_text_translate_world(w_current, world_centerx, world_centery, object);*/
	
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
o_text_rotate_world(TOPLEVEL *w_current, int world_centerx, int world_centery, int angle, int angle_change, OBJECT *object)
{
	int newx, newy;
	int origx, origy;
	int x, y;
	
	origx = object->text->x;
	origy = object->text->y;

	object->text->angle = ( object->text->angle + angle_change ) % 360;

#if DEBUG 
	printf("rotating text angle: %d\n", angle);
	printf("rotating text angle_change: %d\n", angle_change);
	printf("final text angle: %d\n",  object->text->angle);
#endif

	x = origx + (-world_centerx);
	y = origy + (-world_centery);
	
	rotate_point_90(x, y, angle_change, &newx, &newy);

	x = newx + (world_centerx);
	y = newy + (world_centery);
	
	o_text_translate_world(w_current, x-object->text->x, y-object->text->y, object);

	o_text_recreate(w_current, object);
}

void
o_text_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle, int angle_change, OBJECT *object)
{
	int newx, newy;
	int origx, origy;
	int world_centerx, world_centery;
	int x, y;
	
	SCREENtoWORLD(w_current, centerx, centery,
			&world_centerx,
			&world_centery);
	

	origx = object->text->x;
	origy = object->text->y;

	object->text->angle = angle;

	x = origx + (-world_centerx);
	y = origy + (-world_centery);
	
	rotate_point_90(x, y, angle_change, &newx, &newy);

	x = newx + (world_centerx);
	y = newy + (world_centery);
	
	o_text_translate_world(w_current, x-object->text->x, y-object->text->y, object);

	o_text_recreate(w_current, object);
}

#if 0 /* code which is no longer needed, replaced by new functions below */
void
o_text_mirror_old(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int newx=0, newy=0;
	int origx, origy;
	int world_centerx, world_centery;
	int x, y;
	char output_string[1025]; /* hack */
	char name[1025]; /* hack */
        char value[1025]; /* hack */
	int height_mod=0;
	int sign=1;
	
	SCREENtoWORLD(w_current, centerx, centery,
			&world_centerx,
			&world_centery);

	origx = object->text->x;
	origy = object->text->y;

	x = origx + (-world_centerx);
	y = origy + (-world_centery);

	if (o_attrib_get_name_value(object->text->string, name, value)) {
		switch(object->show_name_value) {
			case(SHOW_NAME_VALUE):
				strcpy(output_string, 
						object->text->string);
				break;

			case(SHOW_NAME):
				if (name[0] != '\0') {
					strcpy(output_string, name);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
				    "Got an improper attribute: %s\n", 
					object->text->string);
					strcpy(output_string, "invalid");
				}
				break;

			case(SHOW_VALUE):
				if (value[0] != '\0') {
					strcpy(output_string, value);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
				    "Got an improper attribute: %s\n", 
					object->text->string);
					strcpy(output_string, "invalid");
				}
				break;
		}
	} else {
		strcpy(output_string, object->text->string);
	}

	switch(object->text->alignment) {
		case(LOWER_LEFT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				height_mod = 0;
			}
		break;

		case(MIDDLE_LEFT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				height_mod = o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(UPPER_LEFT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				height_mod = 2*o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(LOWER_MIDDLE):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 0.5;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 0;
			}
		break;

		case(MIDDLE_MIDDLE): 
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 0.5;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(UPPER_MIDDLE): 
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 0.5;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 2*o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(LOWER_RIGHT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = -1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 0;
			}
		break;

		case(MIDDLE_RIGHT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = -1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(UPPER_RIGHT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = -1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 2*o_text_height(w_current, 
				                           object->text->size);
			}
		break;
	}

	switch (object->text->angle) {

		case(0): 
			newx = -(x + sign*o_text_width(w_current, 
						   output_string, 
						   object->text->size/2)); 
		break;

		case(90):
			newx = -(x - sign*o_text_height(w_current, 
				                        object->text->size)+
							height_mod);
		break;

		case(180):
			newx = -(x - sign*o_text_width(w_current, 
						   output_string, 
						   object->text->size/2)); 
		break;

		case(270):
			newx = -(x + sign*o_text_height(w_current, 
						        object->text->size)-
							height_mod);
		break;


		default:
			fprintf(stderr, "Invalid angle specified!\n");
			return;
		break;

	}

	newy = y;

	x = newx + (world_centerx);
	y = newy + (world_centery);
	
	/* don't know if this is needed? */	
	o_text_translate_world(w_current, x-object->text->x, y-object->text->y, object);

	o_text_recreate(w_current, object);
}

void
o_text_mirror_world_old(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	int newx=0, newy=0;
	int origx, origy;
	int x, y;
	char output_string[1025]; /* hack */
	char name[1025]; /* hack */
        char value[1025]; /* hack */
	int sign=1;
	int height_mod=0;
	
	origx = object->text->x;
	origy = object->text->y;

	/* translate to origin */
	x = origx + (-world_centerx);
	y = origy + (-world_centery);


	if (o_attrib_get_name_value(object->text->string, name, value)) {
		switch(object->show_name_value) {
			case(SHOW_NAME_VALUE):
				strcpy(output_string, object->text->string);
				break;

			case(SHOW_NAME):
				if (name[0] != '\0') {
					strcpy(output_string, name);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
				    "Got an improper attribute: %s\n", 
					object->text->string);
					strcpy(output_string, "invalid");
				}
				break;

			case(SHOW_VALUE):
				if (value[0] != '\0') {
						strcpy(output_string, value);
				} else {
			/* you probably can remove this now... */
			/* since improper attributes will never get here */
					fprintf(stderr, 
				    "Got an improper attribute: %s\n", 
					object->text->string);
					strcpy(output_string, "invalid");
				}
				break;
		}
	} else {
		strcpy(output_string, object->text->string);
	}

	switch(object->text->alignment) {
		case(LOWER_LEFT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				height_mod = 0;
			}
		break;

		case(MIDDLE_LEFT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				height_mod = o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(UPPER_LEFT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				height_mod = 2*o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(LOWER_MIDDLE): 
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 0.5;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 0;
			}
		break;

		case(MIDDLE_MIDDLE): 
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 0.5;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(UPPER_MIDDLE): 
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = 0.5;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 2*o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(LOWER_RIGHT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = -1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 0;
			}
		break;

		case(MIDDLE_RIGHT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = -1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = o_text_height(w_current, 
				                           object->text->size);
			}
		break;

		case(UPPER_RIGHT):
			if (object->text->angle == 0 || object->text->angle == 180) {
				sign = -1;
				height_mod = 0;
			} else if (object->text->angle == 90 || object->text->angle == 270) {
				sign = 1;
				height_mod = 2*o_text_height(w_current, 
				                           object->text->size);
			}
		break;
	}

	switch (object->text->angle) {

		case(0): 
			newx = -(x + sign*o_text_width(w_current, 
						   output_string, 
						   object->text->size/2)); 

		break;

		case(90):
			newx = -(x - sign*o_text_height(w_current, 
					object->text->size)+height_mod);
		break;

		case(180):
			newx = -(x - sign*o_text_width(w_current, 
						   output_string, 
						   object->text->size/2)); 
		break;

		case(270):
			newx = -(x + sign*o_text_height(w_current, 
					object->text->size)+height_mod);
		break;

		default:
			fprintf(stderr, "Invalid angle specified!\n");
			return;
		break;

	}

	newy = y;

	x = newx + (world_centerx);
	y = newy + (world_centery);

	object->text->x = x;
	object->text->y = y;

	/* don't know if this is needed ?*/	
	/* o_text_translate_world(w_current, x-object->text->x, y-object->text->y, object);*/
	o_text_recreate(w_current, object);
}
#endif

#if 0 /* interesting, but currently unused code */
void
o_text_return_center(TOPLEVEL *w_current, OBJECT *o_current, int *centerx, int *centery)
{
	int text_height; 
	int text_width;

	text_height = o_text_height(w_current, o_current->text->size);

	/* this will NOT NOT NOT work with attributes */
	text_width = o_text_width(w_current, o_current->text->string, 
				   o_current->text->size/2); 
	
	switch(o_current->text->angle) {
		case(0):
			*centerx = o_current->text->x + text_width/2;
			*centery = o_current->text->y + text_height/2;
		break;

		case(90):
			*centerx = o_current->text->x - text_height/2;
			*centery = o_current->text->y + text_width/2;
		break;

		case(180):
			*centerx = o_current->text->x - text_width/2;
			*centery = o_current->text->y - text_height/2;
		break;

		case(270):
			*centerx = o_current->text->x + text_height/2;
			*centery = o_current->text->y - text_width/2;
		break;
	}	
}

/* the complex here is the complex of a complex object */
o_text_change_angle(TOPLEVEL *w_current, OBJECT *prim_objs, int new_angle)
{
	OBJECT *o_current;
	int centerx, centery;

	o_current = prim_objs;

	while (o_current != NULL) {
		if (o_current->type == OBJ_TEXT) {
			o_current->text->angle = new_angle;

			/* change world to non */
			o_text_return_center(w_current, o_current, &centerx, &centery);

			o_text_translate_world(w_current, 
						-centerx, -centery, o_current);

			o_text_mirror_world(w_current, 0, 0, 
				o_current);
/* 
			o_text_rotate_world(w_current, 0, 0, 
				new_angle, 180, o_current);
*/

/*			o_text_rotate_world(w_current, 0, 0, new_angle, 
					180, o_current)*/

			o_text_translate_world(w_current, 
						centerx, centery, o_current);

/* 			o_text_rotate_lowlevel(w_current, 
				0, 0, new_angle, 180, o_current->text->prim_objs);*/

#if 0
			w_current->override_color =
                                        w_current->background_color;
                        o_text_draw(w_current, o_current);
                        w_current->override_color = -1;
#endif

			o_text_recreate(w_current, o_current);
		}
		o_current = o_current->next;
	}
}
#endif

void
o_text_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery, OBJECT *object)
{
	int origx, origy;
	int x, y;
	
	origx = object->text->x;
	origy = object->text->y;

	x = origx + (-world_centerx);
	y = origy + (-world_centery);

	if ((object->text->angle%180)==0) {
	  switch(object->text->alignment) {
	  case(LOWER_LEFT):
	    object->text->alignment=LOWER_RIGHT;
	    break;

	  case(MIDDLE_LEFT):
	    object->text->alignment=MIDDLE_RIGHT;
	    break;

	  case(UPPER_LEFT):
	    object->text->alignment=UPPER_RIGHT;
	    break;

	  case(LOWER_RIGHT):
	    object->text->alignment=LOWER_LEFT;
	    break;

	  case(MIDDLE_RIGHT):
	    object->text->alignment=MIDDLE_LEFT;
	    break;

	  case(UPPER_RIGHT):
	    object->text->alignment=UPPER_LEFT;
	    break;

	  default:
	    break;
	  }
	} else {
	  switch(object->text->alignment) {
	  case(LOWER_LEFT):
	    object->text->alignment=UPPER_LEFT;
	    break;

	  case(UPPER_LEFT):
	    object->text->alignment=LOWER_LEFT;
	    break;

	  case(LOWER_RIGHT):
	    object->text->alignment=UPPER_RIGHT;
	    break;

	  case(UPPER_RIGHT):
	    object->text->alignment=LOWER_RIGHT;
	    break;

	  case(LOWER_MIDDLE):
	    object->text->alignment=UPPER_MIDDLE;
	    break;

	  case(UPPER_MIDDLE):
	    object->text->alignment=LOWER_MIDDLE;
	    break;

	  default:
	    break;
	  }
	}

	object->text->x = -x + (world_centerx);
	object->text->y =  y + (world_centery);
	
	o_text_recreate(w_current, object);
}

void
o_text_mirror(TOPLEVEL *w_current, int centerx, int centery, OBJECT *object)
{
	int world_centerx, world_centery;

	SCREENtoWORLD(w_current, centerx, centery,
			&world_centerx,
			&world_centery);

	o_text_mirror_world(w_current, world_centerx, world_centery, object);
}
