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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>
#include <math.h>

#include "../include/x_states.h"
#include "../include/prototype.h"


#define GET_PAGE_WIDTH(w)                                       \
        ((w)->page_current->right  - (w)->page_current->left)

static int undo_file_index=0;
static int prog_pid=0;


/* this is additional number of levels (or history) at which point the */
/* undo stack will be trimmed, it's used a safety to prevent running out */ 
/* of entries to free */
#define UNDO_PADDING  5

void
o_undo_init(void)
{
	prog_pid = getpid();	
}

/* flag can either by UNDO_ALL or UNDO_VIEWPORT_ONLY */
void
o_undo_savestate(TOPLEVEL *w_current, int flag)
{
	char *filename = NULL;
	OBJECT *object_head = NULL;
	int levels;
	UNDO *u_current;
	UNDO *u_current_next;

	if (w_current->undo_control == FALSE) {
		return;	
	}

	if (w_current->undo_type == UNDO_DISK && flag == UNDO_ALL) {
		/* 32 is? for max length of pid and index */
		filename = malloc(sizeof(char)*strlen("/tmp/gschem.save_.sch")+32);

		sprintf(filename, "/tmp/gschem.save%d_%d.sch", prog_pid, 
						       undo_file_index++);

		f_save(w_current, filename);
		
	} else if (w_current->undo_type == UNDO_MEMORY && flag == UNDO_ALL) {
		object_head = s_basic_init_object("undo_head");	
		
		o_list_copy_all(w_current, 
				w_current->page_current->object_head->next,
				object_head, NORMAL_FLAG);
	}

	/* Clear Anything above current */
	if (w_current->page_current->undo_current) {
		s_undo_remove_rest(w_current, 
				   w_current->page_current->undo_current->next);
		w_current->page_current->undo_current->next = NULL;
	} else { /* undo current is NULL */
		s_undo_remove_rest(w_current, 
				   w_current->page_current->undo_bottom);
		w_current->page_current->undo_bottom = NULL;
	}
	
	w_current->page_current->undo_tos = 
		w_current->page_current->undo_current;

	w_current->page_current->undo_tos = s_undo_add(
					 w_current->page_current->undo_tos, 
					 flag, filename, object_head,
					 w_current->page_current->left, 
					 w_current->page_current->top,
					 w_current->page_current->right,
					 w_current->page_current->bottom,
					 w_current->page_current->page_control,
					 w_current->page_current->up);

	w_current->page_current->undo_current = 
			w_current->page_current->undo_tos;

	if (w_current->page_current->undo_bottom == NULL) {
		w_current->page_current->undo_bottom = 
				w_current->page_current->undo_tos;	
	}

#if DEBUG
	printf("\n\n---Undo----\n");
	s_undo_print_all(w_current->page_current->undo_bottom);
	printf("BOTTOM: %s\n", w_current->page_current->undo_bottom->filename);
	printf("TOS: %s\n", w_current->page_current->undo_tos->filename);
	printf("CURRENT: %s\n", w_current->page_current->undo_current->filename);
	printf("----\n");
#endif

	if (filename) {	
		free(filename);
	}
	
	/* Now go through and see if we need to free/remove some undo levels */ 
	/* so we stay within the limits */

	/* only check history every 10 undo savestates */
	if (undo_file_index % 10) {
		return;
	}

	levels = s_undo_levels(w_current->page_current->undo_bottom);
	
#if DEBUG
	printf("levels: %d\n", levels);
#endif
	
	if (levels >= w_current->undo_levels + UNDO_PADDING) {
		levels = levels - w_current->undo_levels;

#if DEBUG
		printf("Trimming: %d levels\n", levels);
#endif

		u_current = w_current->page_current->undo_bottom;
		while(u_current && levels > 0) {
			u_current_next = u_current->next;
		
			if (u_current->filename) {
#if DEBUG
				printf("Freeing: %s\n", u_current->filename);
#endif
				unlink(u_current->filename);
				free(u_current->filename);	
			}	
			
			if (u_current->object_head) {
				w_current->REMOVING_SEL = 1;
				s_delete_list_fromstart(w_current, 
							u_current->object_head);
				w_current->REMOVING_SEL = 0; 
				u_current->object_head = NULL; 
			}

			u_current->next = NULL;
			u_current->prev = NULL;
			free(u_current);
			
			u_current = u_current_next;
			levels--;
		}

		/* Because we use a pad you are always garanteed to never */	
		/* exhaust the list */
		u_current->prev = NULL;
		w_current->page_current->undo_bottom = u_current;

#if DEBUG		
		printf("New current is: %s\n", u_current->filename);
#endif
	}
	
#if DEBUG
	printf("\n\n---Undo----\n");
	s_undo_print_all(w_current->page_current->undo_bottom);
	printf("BOTTOM: %s\n", w_current->page_current->undo_bottom->filename);
	printf("TOS: %s\n", w_current->page_current->undo_tos->filename);
	printf("CURRENT: %s\n", w_current->page_current->undo_current->filename);
	printf("----\n");
#endif

}

char *
o_undo_find_prev_filename(UNDO *start)
{
	UNDO *u_current;
	
	u_current = start->prev;
	
	while(u_current) {
		if (u_current->filename) {
			return(u_current->filename);	
		}	
		u_current = u_current->prev;
	}
	
	return(NULL); 
}

OBJECT *
o_undo_find_prev_object_head(UNDO *start)
{
	UNDO *u_current;
	
	u_current = start->prev;
	
	while(u_current) {
		if (u_current->object_head) {
			return(u_current->object_head);	
		}	
		u_current = u_current->prev;
	}
	
	return(NULL); 
}


/* type can be either UNDO_ACTION or REDO_ACTION */
void
o_undo_callback(TOPLEVEL *w_current, int type)
{
	UNDO *u_current;
	UNDO *u_next;
	UNDO *save_bottom;
	UNDO *save_tos;
	UNDO *save_current;
	int save_logging;
	int diff_x;
	int find_prev_data=FALSE;

	char *save_filename;
	
	if (w_current->undo_control == FALSE) {
		s_log_message("Undo/Redo disabled in rc file\n");
		return;	
	}

	if (w_current->page_current->undo_current == NULL) {
		return;	
	}

	if (type == UNDO_ACTION) {
		u_current = w_current->page_current->undo_current->prev;
	} else {
		u_current = w_current->page_current->undo_current->next;
	}

	u_next = w_current->page_current->undo_current;

	if (u_current == NULL) {
		return;	
	}

	if (u_next->type == UNDO_ALL && u_current->type == UNDO_VIEWPORT_ONLY) {
#if DEBUG
		printf("Type: %d\n", u_current->type);
		printf("Current is an undo all, next is viewport only!\n");	
#endif
		find_prev_data = TRUE;
	
		if (w_current->undo_type == UNDO_DISK) {	
			u_current->filename = 
				o_undo_find_prev_filename(u_current);	
		} else {
			u_current->object_head = 
				o_undo_find_prev_object_head(u_current);	
		}
	}

	/* save filename */
	save_filename = u_basic_strdup(w_current->page_current->page_filename);


	/* save structure so it's not nuked */	
	save_bottom = w_current->page_current->undo_bottom;
	save_tos = w_current->page_current->undo_tos;
	save_current = w_current->page_current->undo_current;
	w_current->page_current->undo_bottom = NULL; 
	w_current->page_current->undo_tos =  NULL;
	w_current->page_current->undo_current =  NULL;

	if (w_current->undo_type == UNDO_DISK && u_current->filename) {
		s_page_free(w_current, w_current->page_current);
		s_page_new(w_current, u_current->filename);
	} else if (w_current->undo_type == UNDO_MEMORY && 
			u_current->object_head) {
		s_page_free(w_current, w_current->page_current);
		s_page_new(w_current, save_filename);
	}

	/* temporarily disable logging */
	save_logging = do_logging;
	w_current->DONT_REDRAW = 1;
	do_logging = FALSE;

	if (w_current->undo_type == UNDO_DISK && u_current->filename) {

		f_open(w_current, u_current->filename);

		x_window_setup_world(w_current);
		x_manual_resize(w_current);
		w_current->page_current->page_control = u_current->page_control;
		w_current->page_current->up = u_current->up;
		w_current->page_current->CHANGED=1;

	} else if (w_current->undo_type == UNDO_MEMORY && u_current->object_head) { 

		s_delete_list_fromstart(w_current, 
					w_current->page_current->object_head);
		
		w_current->page_current->object_head = 
					s_basic_init_object("object_head");
		w_current->page_current->object_head->type = OBJ_HEAD;
		
		o_list_copy_all(w_current, u_current->object_head->next, 
				w_current->page_current->object_head, 
				NORMAL_FLAG);
		
		w_current->page_current->object_tail = return_tail(
					w_current->page_current->object_head);
		x_window_setup_world(w_current);
		x_manual_resize(w_current);
		w_current->page_current->page_control = u_current->page_control;
		w_current->page_current->up = u_current->up;
		w_current->page_current->CHANGED=1;
	}

	/* do misc setups */
	set_window(w_current, u_current->left, u_current->right, 
	           u_current->top, u_current->bottom);
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);

	/* restore logging */
	do_logging = save_logging;

	/* set filename right */
	free(w_current->page_current->page_filename);
	w_current->page_current->page_filename = save_filename;

	/* final redraw */
	update_page_manager(NULL, w_current);
	w_current->DONT_REDRAW = 0;

	diff_x = GET_PAGE_WIDTH (w_current);

#if 0 /* zoom factor is no longer used */
#ifdef HAS_RINT
	w_current->page_current->ZOOM_FACTOR= (int) rint(w_current->init_right /
							diff_x);
#else
	w_current->page_current->ZOOM_FACTOR = (int) (w_current->init_right /
							diff_x);
#endif
#endif

	o_redraw_all(w_current);

	/* restore saved undo structures */
	w_current->page_current->undo_bottom = save_bottom;
	w_current->page_current->undo_tos = save_tos;
	w_current->page_current->undo_current = save_current;

	if (type == UNDO_ACTION) { 
		if (w_current->page_current->undo_current) {
			w_current->page_current->undo_current = 
				w_current->page_current->undo_current->prev;
			if (w_current->page_current->undo_current == NULL) {
				w_current->page_current->undo_current = 
					w_current->page_current->undo_bottom;
			}
		}
	} else { /* type is REDO_ACTION */
		if (w_current->page_current->undo_current) {
			w_current->page_current->undo_current = 
				w_current->page_current->undo_current->next;
			if (w_current->page_current->undo_current == NULL) {
				w_current->page_current->undo_current = 
					w_current->page_current->undo_tos;
			}
		}
	}

	/* don't have to free data here since filename, object_head are */
	/* just pointers to the real data (lower in the stack) */
	if (find_prev_data) {
		u_current->filename = NULL;	
		u_current->object_head = NULL;	
	}

#if DEBUG
	printf("\n\n---Undo----\n");
	s_undo_print_all(w_current->page_current->undo_bottom);
	printf("TOS: %s\n", w_current->page_current->undo_tos->filename);
	printf("CURRENT: %s\n", w_current->page_current->undo_current->filename);
	printf("----\n");
#endif
}


void
o_undo_cleanup(void)
{
	int i;
	char filename[128];

	for (i = 0 ; i < undo_file_index; i++) {
		sprintf(filename, "/tmp/gschem.save%d_%d.sch", prog_pid, i);
		unlink(filename);
	}
}
