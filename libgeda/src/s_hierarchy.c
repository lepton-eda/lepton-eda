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
#include <stdlib.h>
#include <assert.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "globals.h"

#include "o_types.h"
#include "../include/prototype.h"


static int page_control_counter=0;

/* This function goes and finds the associated source files and loads ALL up */
/* only works for schematic files though */
/* this is basically push */
int
s_hierarchy_down_schematic_single(TOPLEVEL *w_current, char *filename, 
				  PAGE *parent, int page_control) 
{
	char *string=NULL;
	PAGE *found;

	string = s_slib_search_single(filename);

	if (!string) {
		return(-1);
	}

	found = s_page_new(w_current, string);

	if (found) {
		w_current->page_current = found;
		s_page_goto(w_current, found);
		if (page_control != 0) {
			w_current->page_current->page_control = page_control;
		}
		w_current->page_current->up = parent->pid;
		if (string) 
			free(string);
		return(w_current->page_current->page_control);
	}

	f_open(w_current, w_current->page_current->page_filename);

	if (page_control == 0) {
		page_control_counter++;
		w_current->page_current->page_control = 
					page_control_counter;
	} else {
		w_current->page_current->page_control = 
					page_control;
	}

	w_current->page_current->up = parent->pid;

	s_page_goto(w_current, w_current->page_current);

	if (string) 
		free(string);

	return(page_control_counter);
}


/* This function goes and finds the associated source files and loads ALL up */
/* only works for schematic files though */
/* this is basically push */
void
s_hierarchy_down_schematic_multiple(TOPLEVEL *w_current, char *filename, 
				    PAGE *parent) 
{
	char *string=NULL;
	PAGE *save_first_page=NULL;
	PAGE *found;
	int loaded_schematics=0;

	s_slib_search(NULL, SLIB_SEARCH_START);

	string = s_slib_search(filename, SLIB_SEARCH_NEXT);
	while (string != NULL) {

		found = s_page_new(w_current, string);

		if (found) {
			w_current->page_current = found;
			s_page_goto(w_current, found);
			if (string) 
				free(string);
			return;
		}

		f_open(w_current, w_current->page_current->page_filename);

		if (loaded_schematics == 0) {
			page_control_counter++;
			save_first_page = w_current->page_current;
			/* parent->down = w_current->page_current; not needed */
			w_current->page_current->page_control = 
						page_control_counter;
			loaded_schematics=1;
		} else {
			w_current->page_current->page_control = 
						page_control_counter;
		}

		w_current->page_current->up = parent->pid;
		/* w_current->page_current->down = NULL; not needed */

		if (string) 
			free(string);

		string = s_slib_search(filename, SLIB_SEARCH_NEXT);
	}

	s_slib_search(NULL, SLIB_SEARCH_DONE);

	if (string) 
		free(string);

	if (loaded_schematics) {
		w_current->page_current = save_first_page;
	}

	s_page_goto(w_current, w_current->page_current);
}

void
s_hierarchy_down_symbol(TOPLEVEL *w_current, char *filename, PAGE *parent)
{
	PAGE *found;

	/* stupid way of doing this */
	/* absolutely NO error detection */
	found = s_page_new(w_current, filename);

	if (found) {
		w_current->page_current = found;
		s_page_goto(w_current, found);
		return;
	}

	f_open(w_current, w_current->page_current->page_filename);

	w_current->page_current->up = parent->pid;
	/* w_current->page_current->down = NULL; not needed */
	/* parent->down = w_current->page_current; not needed */
	page_control_counter++;
	w_current->page_current->page_control = page_control_counter;

	s_page_goto(w_current, w_current->page_current);
}

void
s_hierarchy_up(TOPLEVEL *w_current, int pid)
{
	PAGE *p_current;

	if (pid < 0) {
		s_log_message("There are no schematics above the current one!\n");
		return;
	}

	p_current = s_hierarchy_find_page(w_current->page_head, pid);

	if (p_current) {
		s_page_goto(w_current, p_current);
	} else {
		s_log_message("Cannot find any schematics above the current one!\n");
		s_log_message("Maybe toplevel schematic page was closed/discarded?\n");
	}
}

void
s_hierarch_traverse(void) 
{

}

PAGE *
s_hierarchy_find_prev_page(PAGE *p_start, int page_control) 
{
	PAGE *p_current;	

	p_current = p_start->prev;

	while(p_current != NULL) {
		if (p_current->page_control == page_control) {
			return(p_current);
		}
		p_current = p_current->prev;
	}

	return(NULL);
}

PAGE *
s_hierarchy_find_next_page(PAGE *p_start, int page_control)
{
	PAGE *p_current;	

	p_current = p_start->next;

	while(p_current != NULL) {
		if (p_current->page_control == page_control) {
			return(p_current);
		}
		p_current = p_current->next;
	}

	return(NULL);
}

PAGE *
s_hierarchy_find_page(PAGE *p_start, int pid)
{
	PAGE *p_current;	

	p_current = p_start;

	while(p_current != NULL) {
		if (p_current->pid == pid) {
			return(p_current);
		}
		p_current = p_current->next;
	}

	return(NULL);
}
