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

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "s_passing.h"
#include "globals.h"

#include "o_types.h"
#include "../include/prototype.h"


static int global_pid=0;


PAGE *
s_page_return_tail(PAGE *head)
{
	PAGE *p_current=NULL;
        PAGE *ret_struct=NULL;

        p_current = head;
        while ( p_current != NULL ) { /* goto end of list */
                ret_struct = p_current;
                p_current = p_current->next;
        }

        return(ret_struct);
}


/* this frees the pages associated with w_current and p_current */
/* need to change this... */
void 
s_page_free(TOPLEVEL *w_current, PAGE *p_current)
{
	/* printf("freeing\n");*/
	s_delete_list_fromstart(w_current, p_current->object_head);
	w_current->REMOVING_SEL = 1;
	s_delete_list_fromstart(w_current, p_current->selection_head);
	s_delete_list_fromstart(w_current, p_current->complex_place_head);
	s_delete_list_fromstart(w_current, p_current->attrib_place_head);
	w_current->REMOVING_SEL = 0;  

	p_current->object_head = NULL;
	p_current->object_tail = NULL;
	p_current->object_parent = NULL;
	p_current->object_lastplace = NULL;
	p_current->selection_head = NULL;
	p_current->selection_tail = NULL;
	p_current->complex_place_head = NULL;
	p_current->complex_place_tail = NULL;
	p_current->attrib_place_head = NULL;
	p_current->attrib_place_tail = NULL;

	if (p_current->page_filename) {
		free(p_current->page_filename);
	}

	if (p_current->next)
                p_current->next->prev = p_current->prev;

        if (p_current->prev)
                p_current->prev->next = p_current->next;



	/* be sure table is empty ALES */
	/* g_hash_table_foreach_remove(hash_table, free_func, NULL);*/
	g_hash_table_destroy(p_current->ales_table);

	free(p_current);

	p_current=NULL;

	w_current->page_tail = s_page_return_tail(w_current->page_head);

#if 0 /* don't do this for now hack */ /* this is a per window free */
	o_attrib_free_current(w_current);
	o_complex_free_filename(w_current);
#endif
}

/* should only be called when you are finishing up */
void
s_page_free_all(TOPLEVEL *w_current, PAGE *p_tail)
{
	PAGE *p_current;
	PAGE *p_prev;

	p_current = p_tail;

	while (p_current != NULL && p_current->pid != -1) {
		p_prev = p_current->prev;
#if DEBUG
		printf("about to free %d\n", p_current->pid);
#endif
                s_page_free(w_current, p_current);
                p_current = p_prev;
		w_current->page_current = p_current;
	}	

	/* Now free the head */
	s_page_free_head(w_current, w_current->page_head);
	w_current->page_head = NULL;
	w_current->page_tail = NULL;
}


/* p_current is the current tail of pages */
PAGE *
s_page_add(TOPLEVEL *w_current, PAGE *p_tail, char *page_filename)
{
	PAGE *p_new;

	/* be sure to free this somewhere */
	p_new = (PAGE *) malloc(sizeof(PAGE)); 

	p_new->CHANGED = 0;
	p_new->zoom_factor = 0;
	p_new->page_filename = malloc(sizeof(char)*strlen(page_filename)+1);
	strcpy(p_new->page_filename, page_filename);
        p_new->coord_aspectratio = (float) w_current->init_right /
                                   (float) w_current->init_bottom;


	if (p_tail == NULL) {
		p_new->pid = -1; /* head node */
                p_new->prev = NULL;
                p_new->next = NULL;


                return(p_new);
        } else {
		p_new->pid = global_pid++;
                p_new->prev = p_tail;
		p_new->next = NULL;
		p_tail->next = p_new;
                return(p_tail->next);
        }
}

void
s_page_print(PAGE *p_list) 
{
	PAGE *p_current;

	p_current = p_list;

	while (p_current != NULL) {

		printf("pid: %d\n", p_current->pid);
		printf("page_filename: %s\n", p_current->page_filename);

		p_current = p_current->next;
	}
}

void
s_page_add_head(TOPLEVEL *w_current)
{
	w_current->page_head = w_current->page_tail = s_page_add(
							w_current,
							NULL, 
							"page_head");
}

void
s_page_free_head(TOPLEVEL *w_current, PAGE *p_head)
{
	if (p_head->page_filename) {
		free(p_head->page_filename);
	}
	
	free(p_head);
	p_head = NULL;
}

/* needed ? */
void
s_page_delete()
{

}

void
s_page_new(TOPLEVEL *w_current, char *page_filename)
{
	/* Now create a blank page */
        w_current->page_tail = s_page_add(w_current,
					  w_current->page_tail, page_filename);

        /* setup page_current link */
        w_current->page_current = w_current->page_tail;

	s_page_setup(w_current->page_current);
	w_current->page_current->object_lastplace=NULL;
        w_current->page_current->object_selected=NULL;


	set_window(w_current, w_current->init_left, w_current->init_right,
                   w_current->init_top, w_current->init_bottom);

	w_current->page_current->zoom_factor = 0;
}

void
s_page_setup(PAGE *p_current)
{

	/* First one to setup head */
	strcpy(p_name, "object_head");
	p_type = OBJ_HEAD;
	p_action_func = NULL;
        p_line_points = NULL;
        p_circle = NULL;
	p_action_func = error_if_called;
	p_sel_func = error_if_called;
	p_draw_func = error_if_called;
	p_complex = NULL;
	p_left = p_right = p_top = p_bottom = -1;
	p_x = p_y = -1;
	p_screen_x = p_screen_y = -1;
	p_color = -1;
	p_text_string[0] = '\0';
	p_complex_basename[0] = '\0';
	p_complex_clib[0] = '\0';
	p_attribs = NULL;
	p_attached_to = NULL;
        /* add p_attrib and p_attached_to */
	p_current->object_head = add_object(NULL);

	strcpy(p_name, "sel_head");
	p_type = OBJ_HEAD;
	p_line_points = NULL;
        p_circle = NULL;
	p_action_func = error_if_called;
	p_sel_func = error_if_called;
	p_draw_func = error_if_called;
	p_complex = NULL;
	p_left = p_right = p_top = p_bottom = -1;
	p_x = p_y = -1;
	p_screen_x = p_screen_y = -1;
	p_color = -1;
	p_text_string[0] = '\0';
	p_complex_basename[0] = '\0';
	p_complex_clib[0] = '\0';
	p_attribs = NULL;
	p_attached_to = NULL;
        /* add p_attrib and p_attached_to */
	p_current->selection_tail = p_current->selection_head = 
							add_object(NULL);

	strcpy(p_name, "complex_place_head");
	p_type = OBJ_HEAD;
	p_line_points = NULL;
        p_circle = NULL;
	p_action_func = error_if_called;
	p_sel_func = error_if_called;
	p_draw_func = error_if_called;
	p_complex = NULL;
	p_left = p_right = p_top = p_bottom = -1;
	p_x = p_y = -1;
	p_screen_x = p_screen_y = -1;
	p_color = -1;
	p_text_string[0] = '\0';
	p_complex_basename[0] = '\0';
	p_complex_clib[0] = '\0';
	p_attribs = NULL;
	p_attached_to = NULL;
        /* add p_attrib and p_attached_to */
	p_current->complex_place_tail = p_current->complex_place_head = 
						add_object(NULL);

	strcpy(p_name, "attrib_place_head");
	p_type = OBJ_HEAD;
	p_line_points = NULL;
	p_circle = NULL;
	p_action_func = error_if_called;
	p_sel_func = error_if_called;
	p_draw_func = error_if_called;
	p_complex = NULL;
	p_left = p_right = p_top = p_bottom = -1;
	p_x = p_y = -1;
	p_screen_x = p_screen_y = -1;
	p_color = -1;
	p_text_string[0] = '\0';
	p_complex_basename[0] = '\0';
	p_complex_clib[0] = '\0';
	p_attribs = NULL;
	p_attached_to = NULL;
        /* add p_attrib and p_attached_to */
	p_current->attrib_place_tail = p_current->attrib_place_head = 
						add_object(NULL);

	p_current->object_tail = return_tail(p_current->object_head);

	/* new ALES stuff */
	p_current->ales_table = g_hash_table_new(g_str_hash, g_str_equal);

	/* setup parent to point to list */
	/* this is used for attributes so */
	/* that we know which list to search */
	p_current->object_parent = p_current->object_head; 
}

int
s_page_check_changed(PAGE *p_head)
{
	PAGE *p_current;

	p_current = p_head;
	
	while(p_current != NULL) {
		if (p_current->CHANGED) {
			return(1);
		}

		p_current = p_current->next;
	}
	return(0);
}

void
s_page_clear_changed(PAGE *p_head)
{
	PAGE *p_current;

	p_current = p_head;
	
	while(p_current != NULL) {
		p_current->CHANGED = 0;
		p_current = p_current->next;
	}

}

void
s_page_goto(TOPLEVEL *w_current, PAGE *p_new) 
{
	w_current->page_current = p_new;
}

PAGE *
s_page_search(TOPLEVEL *w_current, char *filename)
{
	PAGE *p_current;

	p_current = w_current->page_head;

	while(p_current != NULL) {
		
		if (strcmp(p_current->page_filename, filename) == 0) {
				return(p_current);	
		}

		p_current = p_current->next;
	}

	return(NULL);
}

int
s_page_search_row(TOPLEVEL *w_current, PAGE *p_findme)
{
	PAGE *p_current;

	p_current = w_current->page_head;

	while(p_current != NULL) {
		if (p_current->clist_row == p_findme->clist_row) {
			return(p_current->clist_row);
		}
	
		p_current = p_current->next;
	}

	return(0); /* can't find page... well just select row 0 */
}

void
s_page_print_all(TOPLEVEL *w_current)
{
        PAGE *p_current;

        p_current = w_current->page_head;

        while(p_current != NULL) {

                if (p_current->pid != -1) {
                        print_struct_forw(p_current->object_head);
                }

                p_current = p_current->next;
        }
}

void
s_page_save_all(TOPLEVEL *w_current)
{
        PAGE *p_current;
        PAGE *p_save;

	p_save = w_current->page_current;
        p_current = w_current->page_head;

        while(p_current != NULL) {

                if (p_current->pid != -1) {
			w_current->page_current = p_current;
			f_save(w_current, p_current->page_filename);
			s_log_message("Saved [%s]\n", w_current->page_current->page_filename);
			p_current->CHANGED = 0;
                }

                p_current = p_current->next;
        }

	w_current->page_current = p_save;
}
