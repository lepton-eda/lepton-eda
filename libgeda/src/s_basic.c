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
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

#ifndef HAVE_VSNPRINTF
#include <stdarg.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>


#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "s_passing.h"
#include "globals.h"

#include "o_types.h"

#include "../include/prototype.h"

/* this is modified here and in o_list.c */
int global_sid=0;

#define NUMCOLORS 9

struct st_colors {
        char *name;
        int value;
};

/* Colors must be in alphabetical order */
/* be sure that you update above define */
struct st_colors colors[] = {
	{ "black", 0 },
	{ "blue", 4 },
	{ "cyan", 6 },
	{ "green", 3 },
	{ "grey", 7 }, 
	{ "grey90", 8 },
	{ "red", 2 },
	{ "white", 1 },
	{ "yellow", 5 },
};



void error_if_called(void)
{
	fprintf(stderr, "Somebody called error_if_called!\n");
	assert(0);
}

void exit_if_null(void *ptr) 
{
	if (ptr == NULL) {
		fprintf(stderr, "gEDA: Got NULL ptr!, please e-mail maintainer\n");
		assert(0);
		exit(-1);
	}	
}


/* hack rename this to be s_return_tail */
/* update object_tail or any list of that matter */
OBJECT *
return_tail(OBJECT *head)
{
	OBJECT *o_current=NULL;
	OBJECT *ret_struct=NULL;

	o_current = head;
	while ( o_current != NULL ) { /* goto end of list */
		ret_struct = o_current;	
		o_current = o_current->next;
	}
	
	return(ret_struct);	
}

/* hack rename this to be s_return_head */
/* update object_tail or any list of that matter */
OBJECT *
return_head(OBJECT *tail)
{
	OBJECT *o_current=NULL;
	OBJECT *ret_struct=NULL;

	o_current = tail;
	while ( o_current != NULL ) { /* goto end of list */
		ret_struct = o_current;	
		o_current = o_current->prev;
	}
	
	return(ret_struct);	
}

OBJECT *
add_object( OBJECT *ptr ) 
{
	OBJECT *new_node;

	new_node = (OBJECT *) malloc(sizeof(OBJECT));	

	/* setup sid */
	new_node->sid = global_sid++;

	/* Setup the name */
	/* size is a hack */
	/* dangerous magic constant 10 */
	new_node->name = (char *) malloc(sizeof(char)*(strlen(p_name)+10));
	sprintf(new_node->name, "%s.%d", p_name, new_node->sid);

	/* Setup the type */
	new_node->type = p_type;

	/* Setup the bounding box */
	new_node->top = p_top;
	new_node->left = p_left;
	new_node->right = p_right;
	new_node->bottom = p_bottom;

	/* Setup line points */

	if (p_line_points) {
	 	new_node->line_points = p_line_points;
	} else {
	 	new_node->line_points = p_line_points;
	}
	
	if (p_circle) {
		new_node->circle = p_circle;
	} else {
	 	new_node->circle = p_circle;
	}



	new_node->visited = 0;

	/* Setup complex part stuff */
	if (p_complex_basename[0] != '\0') {
		new_node->complex_basename = (char *) 
			malloc(sizeof(char)*(strlen(p_complex_basename)+1));
		strcpy(new_node->complex_basename, p_complex_basename);
	} else {
		new_node->complex_basename = NULL;
	}

	if (p_complex_clib[0] != '\0') {
		new_node->complex_clib = (char *) 
			malloc(sizeof(char)*(strlen(p_complex_clib)+1));
		strcpy(new_node->complex_clib, p_complex_clib);
	} else {
		new_node->complex_clib = NULL;
	}
		
	/* link in the actual stuff */
	new_node->complex = p_complex;
	
	/* usually null, code after add_object needs to change this */
	new_node->complex_parent = NULL;

	/* origin of complex */
	new_node->x = p_x;
	new_node->y = p_y;

	new_node->screen_x = p_screen_x;
	new_node->screen_y = p_screen_y;

	/* Setup the color */
	new_node->color = p_color;
	
	/* Set the angle */
	new_node->angle = p_angle;
	new_node->mirror = p_mirror;

	/* init saved_color */
	new_node->saved_color = -1;

	/* Setup text */
	if (p_text_string[0] != '\0') {
		new_node->text_string = (char *) malloc(sizeof(char)*
						(strlen(p_text_string)+1));

		strcpy(new_node->text_string, p_text_string);
	} else {
		new_node->text_string = NULL;
	}

	new_node->text_len = p_text_len;
	new_node->text_size = p_text_size;


	/* Setup attributes */
	/* will be null for now (forever?) do we ever add something that */
	/* will not be null here hack */
	/* when we are adding true attributes this needs to be p_attribs */
	/* second will stay NULL, unless it's a floating hack ???? */
	new_node->attribs = NULL;
	new_node->visibility = p_visibility;
	new_node->show_name_value = p_show_name_value;
	new_node->attached_to = NULL;
	new_node->attribute = 0; /* change to p_attribute */
	

	/* Setup functions */
	new_node->action_func = p_action_func;
	new_node->sel_func = p_sel_func;
	new_node->draw_func = p_draw_func;

	/* Setup link list stuff */
	new_node->next = NULL;
	/* object_tail = new_node;*/ /* careful hack BAD! out for ever */


	if (ptr == NULL) {
		new_node->prev = NULL; /* setup previous link */
		return(new_node);
	} else {
		new_node->prev = ptr; /* setup previous link */
		ptr->next = new_node;
		return(ptr->next);
	}
}

void
print_struct_forw(OBJECT *ptr)
{
	OBJECT *o_current=NULL;
	ATTRIB *attr=NULL;
	int i;

	o_current = ptr;

	if (o_current == NULL) {

		printf("AGGGGGGGGGGG NULLLLL PRINT\n");
	}
	printf("TRYING to PRINT\n");
	while (o_current != NULL) {
		printf("Name: %s\n", o_current->name);
		printf("Type: %d\n", o_current->type);
		printf("Sid: %d\n", o_current->sid);
/*		if (o_current->line_points != NULL) {
			printf("Line points.x1: %d\n", o_current->line_points->x1);
			printf("Line points.y1: %d\n", o_current->line_points->y1);
			printf("Line points.x2: %d\n", o_current->line_points->x2);
			printf("Line points.y2: %d\n", o_current->line_points->y2);
		}*/

		if (o_current->type == OBJ_COMPLEX) {
                        print_struct_forw(o_current->complex);
                }

	
		 if (o_current->attribs) {
                        attr = o_current->attribs;
                        i = 0;
                        while (attr != NULL) {
				if (attr->object != NULL) 
                                	printf("%d attribute %s\n", i, attr->object->name);
                                attr = attr->next;
                        }    
		}

		printf("----\n");
		o_current = o_current->next;
	}
}

void
print_struct_back(OBJECT *ptr)
{
	OBJECT *o_current=NULL;

	o_current = ptr;

	while (o_current != NULL) {
		printf("Name: %s\n", o_current->name);
		printf("Type: %d\n", o_current->type);
		printf("Sid: %d\n", o_current->sid);
/*		if (o_current->line_points != NULL) {
			printf("Line points.x1: %d\n", o_current->line_points->x1);
			printf("Line points.y1: %d\n", o_current->line_points->y1);
			printf("Line points.x2: %d\n", o_current->line_points->x2);
			printf("Line points.y2: %d\n", o_current->line_points->y2);
		}*/
		printf("----\n");
		o_current = o_current->prev;
	}
}

void
print_struct(OBJECT *ptr)
{
	OBJECT *o_current=NULL;
	ATTRIB *attr=NULL;
	int i;

	o_current = ptr;

	if (o_current != NULL) {
		printf("Name: %s\n", o_current->name);
		printf("Type: %d\n", o_current->type);
		printf("Sid: %d\n", o_current->sid);
		if (o_current->line_points != NULL) {
			printf("Line points.x1: %d\n", o_current->line_points->x1);
			printf("Line points.y1: %d\n", o_current->line_points->y1);
			printf("Line points.x2: %d\n", o_current->line_points->x2);
			printf("Line points.y2: %d\n", o_current->line_points->y2);
		}

		if (o_current->attribs) {
			attr = o_current->attribs;
			i = 0;
			while (attr != NULL) {
				printf("%d attribute %s\n", i, attr->object->name);
				attr = attr->next;
			}
		
		}
		printf("----\n");
	}
}

void
s_delete(TOPLEVEL *w_current, OBJECT *o_current)
{
	if (o_current != NULL) {

		/*printf("sdel: %s\n", o_current->name);
		printf("sdel: %d\n", o_current->sid);*/

		if (o_current->next) 
			o_current->next->prev = o_current->prev;
		else
			o_current->next = NULL;

		if (o_current->prev) 
			o_current->prev->next = o_current->next;
		else
			o_current->prev = NULL;

/* move this flag up out of this function ? so that we don't have to pass
   the window TRANSFORM hack */
if (!w_current->REMOVING_SEL) {
		/* find all instances of points to current and remove them */

		/* ALES new stuff */
		o_ales_disconnect(w_current->page_current); 

} /* end of REMOVING_SEL */


		/* second half of if is odd that we need it? hack */
		/* need to do this early so we can do the printfs */
		if (o_current->attached_to != NULL && o_current->attribute == 1) {
			if (o_current->attached_to->object) {
				/*printf("removing %s\n", o_current->attached_to->object->name);*/
			} else {
				printf("found a null I didn't expect!!!!!!!!!\n");
			}

			/* do the actual remove */
			o_attrib_delete(o_current->attached_to);
		}

		if (w_current->page_current->object_lastplace == o_current) {
			w_current->page_current->object_lastplace = NULL; 
		}

		if (o_current->line_points) {
		/*	printf("sdeleting line_points\n");*/
			free(o_current->line_points);
		}
		o_current->line_points = NULL;

		if (o_current->circle) {
		/*	printf("sdeleting circle\n");*/
			free(o_current->circle);
		}
		o_current->circle = NULL;

		if (o_current->name) {
		/*	printf("sdeleting name\n");*/
			free(o_current->name);
		}
		o_current->name = NULL;

		if (o_current->text_string) {
		/*	printf("sdeleting text_string\n");*/
                       free(o_current->text_string); 
		}
		o_current->text_string = NULL;

		if (o_current->complex_basename) {
		/*	printf("sdeleting complex_basename\n");*/
                       free(o_current->complex_basename); 
		}
		o_current->complex_basename = NULL;

		if (o_current->complex_clib) {
		/*	printf("sdeleting complex_clib\n");*/
                       free(o_current->complex_clib); 
		}
		o_current->complex_clib = NULL;

		if (o_current->complex) {
		/*	printf("sdeleting complex\n");*/
			s_delete_list_fromstart(w_current, o_current->complex);
		}
		o_current->complex = NULL;



		if (o_current->attribs) {
			o_attrib_free_all(w_current, o_current->attribs);
		}
		o_current->attribs = NULL;


		free(o_current);	/* assuming it is not null */

		o_current=NULL;		/* misc clean up */
	}
}

void
s_delete_head(TOPLEVEL *w_current, OBJECT *head)
{
	if (head != NULL) {

		/* deleted the object_lastplace thingy -> it happend */
	
		/*printf("dhead: %s\n", head->name);*/

		/* This was part of an early warning, but isn't relevant */
		/* anymore */
		/* if (head->next) {
			printf("head->next wasn't freed!!!!!!!!!!!!!\n");	
		} */

		if (head->line_points) {
/*			printf("freeing line_points\n");*/
			free(head->line_points);
		}

		if (head->circle) {
/*			printf("freeing circle\n");*/
			free(head->circle);
		}

		if (head->name) { 
/*			printf("freeing name\n");*/
			free(head->name);
		}

		if (head->text_string) {
		/*	printf("freeing text_string\n");*/
                       free(head->text_string); 
		}

		if (head->complex_basename) {
		/*	printf("freeing complex_basename\n");*/
                        free(head->complex_basename); 
		}

		if (head->complex_clib) {
		/*	printf("freeing complex_clib\n");*/
                        free(head->complex_clib); 
		}
	
		if (head->complex) {
		/*	printf("freeing complex\n");*/
			/* is this the right thing to do ? */
			/* UGGGGG.. move this out of here... TRANSFORM hack */
			s_delete_list_fromstart(w_current, head->complex);
                        free(head->complex);
		}

		free(head);		/* assuming it is not null */

		head=NULL;		/* misc clean up */
	}

}

/* deletes everything include the head */
void
s_delete_list_fromstart(TOPLEVEL *w_current, OBJECT *start)
{
	OBJECT *temp=NULL; /* literally is a temp */
	OBJECT *current=NULL; /* ugg... you have both o_current and current? */
	OBJECT *o_current=NULL; /* hack */

	temp = start;
	current = return_tail(start);

	/* do the delete backwards */
	while(current != NULL && current->type != OBJ_HEAD ) {
		o_current = current->prev;
		s_delete(w_current, current);
		current = o_current;
	}

	/* now delete the head node */
	/* might not need this but what the hell */
	s_delete_head(w_current, start);
}

#if 0 /* old way of doing this */
s_delete_list_fromstart(OBJECT *start)
{
	OBJECT *traverse=NULL;
	OBJECT *o_current=NULL;

	for (traverse = start; traverse ; traverse = o_current) {
		o_current = traverse->next;
		if (traverse->type != OBJ_HEAD) /* don't delete any head nodes */
			s_delete(traverse);
		else 
			break; /* found a head node */
	}
	s_delete(traverse);
}
#endif


void
string_toupper(char *in, char *out)
{
	int len;
	int i;

	len = strlen(in);

	for (i = 0 ; i < len ; i++) {
		out[i] = toupper(in[i]);
	}
}

void
string_tolower(char *in, char *out)
{
	int len;
	int i;

	len = strlen(in);

	for (i = 0 ; i < len ; i++) {
		out[i] = tolower(in[i]);
	}
}

/* this routine must stay */
int
colornametovalue(char *string)
{
	
	int lower = 0; 
	int upper = NUMCOLORS - 1;
	int middle;
	int val;
	struct st_colors *ptr=NULL;

	if (!string) {
		return(-1);
	}

	string_tolower(string, string);
	while (lower <= upper) {
		middle = (lower + upper) / 2;

		ptr = &colors[middle];
		val = strcmp (ptr->name, string);

		if (val < 0) {
			lower = middle + 1;
		} else if (val == 0) {
			return(ptr->value);
		} else {
			upper = middle - 1;
		}
	}                
	return(-1);
}

/* used by o_text_read */
char *
remove_nl(char *string)
{
	int i;
		
	i = 0;
	while(string[i] != '\0' && string[i] != '\n') {
	i++; 
	}

	string[i] = '\0';

	return(string);
}

#ifndef HAVE_VSNPRINTF
void vsnprintf(char *buff, size_t bufsiz, const char *fmt, va_list ap)
{
    char *tmpbuf = buff;

    vsprintf(tmpbuf, fmt, ap);
}
#endif 


/* this function is called by expand_env_variables */
/* changes and returns new string, frees the one that was passed in */
char *
remove_string(char *string, int start, int end) 
{
	char *return_string;
	int i;
	int len;
	int j;

	if (!string) {
		return(NULL);
	}

	len = strlen(string);

	return_string = (char *) malloc(sizeof(char)*(len+1));

	j = 0;
	for (i = 0 ; i < len; i++) {
		if (i >= start && i <= end) {
			/* do nothing */
			/* removing characters */
		} else {
			return_string[j] = string[i];
			j++;
		}
	}
 	return_string[j] = '\0';

	/* free original string */
	free(string);

	return(return_string);
}

/* this function is called by expand_env_variables */
/* changes and returns new string, frees the one that was passed in */
char *
insert_string(char *string, int start, char *insert_string)
{
	char *new_string=NULL;
	int i;
	int len;
	int insert_len;
	int total_len;
	int j;
	int orig_count=0;

	/* this should never happen */
	if (!insert_string) {
		return(NULL);	
	}

	/* this should never happen either */
	if (!string) {
		return(NULL);	
	}

	len = strlen(string);
	insert_len = strlen(insert_string);
	total_len = len+insert_len;

	new_string = (char *) malloc(sizeof(char)*(total_len+1));

	i = 0;
	while (i < total_len) {
		if (i == start) {
			for (j = 0 ; j < insert_len; j++) {
				new_string[i+j] = insert_string[j];
			}
			i = j+i;
		} else {
			new_string[i] = string[orig_count];
			i++;
			orig_count++;
		}
	}

	new_string[i] = '\0';

	/* now free the original string */
	free(string);

	return(new_string);
}


/* this function changes and then returns the string which has the 
 * expanded environment variables, frees passed in string */
/* Environment variables MUST be in the form ${variable_name} */
/* $variable_name is not valid here */
char *
expand_env_variables(char *string)
{
	char wanted_var[80]; /* size is hack */
	char *return_string=NULL;
	char *environment_string=NULL;
	int changed=1;
	int found_dollar=0;
	int found_lbrac=0; 
	int found_rbrac=0;
	int start_of_variable=-1;
	int end_of_variable=-1;
	int count=0;
	int i,j;

	if (!string) {
		return(NULL);
	}

	return_string = string;	

	while(changed) {
	
		changed=0;
		j=0;
		for (i = 0 ; i < strlen(return_string); i++) {

			switch(return_string[i]) {

				case('$'):

#if DEBUG
					printf("found a $\n");
#endif
					found_dollar=1;	
					start_of_variable=i;
				break;

				case('{'):
					if (found_dollar) {
						found_lbrac=1;
						count=1;
					}
				break;

				case('}'):
					if (found_dollar) {
						found_rbrac=1;
					/* ends filling of wanted_var */	
						found_lbrac=0;
						end_of_variable=i;
					}
				break;

			}

			/* the > 1 bit is so that we don't store the { */
			if (found_dollar && found_lbrac && (count > 1)) {
				wanted_var[j] = return_string[i];
				j++; /* check for size */
			}

			/* skip over initial { */ 
			count++;

			if (found_rbrac && !found_lbrac) {
				wanted_var[j] = '\0';
#if DEBUG
				printf("variable wanted: _%s_\n",  wanted_var);
				printf("Between index: %d and %d\n", 
						start_of_variable,
						end_of_variable);
#endif

		
				environment_string = getenv(wanted_var);	

#if DEBUG
				if (environment_string) {
					printf("%s = _%s_\n", wanted_var, 
							environment_string);
				}
#endif

				return_string = remove_string(return_string,
							   start_of_variable,
							   end_of_variable);

#if DEBUG
				printf("removed string: _%s_\n", return_string);
#endif

				if (environment_string) {
					return_string = insert_string(
							return_string,
							start_of_variable,
							environment_string);

				}

#if DEBUG
				printf("final string: _%s_\n", return_string);
#endif
				changed=1;

				/* end of search */
				found_dollar=0;
				found_rbrac=0;
				count=0;
				start_of_variable=-1;
				end_of_variable=-1;

				break;
			}
		}
	}

	if (found_dollar) {
		fprintf(stderr, "Found malformed environment variable (use ${varname})!\n");
	}

	return(return_string);
}

