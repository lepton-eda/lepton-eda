
/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
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

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/globals.h"
#include "../include/prototype.h"

o_buffer_copy(TOPLEVEL *w_current, int buf_num)
{
	SELECTION *s_current = NULL;
        OBJECT *o_current = NULL;

	if (buf_num < 0 || buf_num > MAX_BUFFERS) {
		fprintf(stderr, "Got an invalid buffer_number [o_buffer_copy]\n");
		return;
	}

	s_current = w_current->page_current->selection2_head->next;

	if (object_buffer[buf_num] == NULL) {
		object_buffer[buf_num] = s_basic_init_object("buffer0_head");
		object_buffer[buf_num]->type = OBJ_HEAD;
	} else {
		o_list_delete_rest(w_current, object_buffer[buf_num]);
		object_buffer[buf_num]->next = NULL;
	}

	o_list_copy_all_selection2(w_current, s_current, 
				   object_buffer[buf_num], NORMAL_FLAG);		

#if DEBUG
	o_current = object_buffer[buf_num];
	while(o_current != NULL) {
		printf("- %s\n", o_current->name);
		o_current = o_current->next;
	}
#endif
}

o_buffer_cut(TOPLEVEL *w_current, int buf_num)
{
	SELECTION *s_current = NULL;
        OBJECT *o_current = NULL;

	if (buf_num < 0 || buf_num > MAX_BUFFERS) {
		fprintf(stderr, "Got an invalid buffer_number [o_buffer_cut]\n");
		return;
	}

	s_current = w_current->page_current->selection2_head->next;

	if (object_buffer[buf_num] == NULL) {
		object_buffer[buf_num] = s_basic_init_object("buffer0_head");
		object_buffer[buf_num]->type = OBJ_HEAD;
	} else {
		o_list_delete_rest(w_current, object_buffer[buf_num]);
		object_buffer[buf_num]->next = NULL;
	}

	o_list_copy_all_selection2(w_current, s_current, 
				   object_buffer[buf_num], NORMAL_FLAG);
	o_delete(w_current);

	w_current->page_current->CHANGED = 1;

#if DEBUG
	o_current = object_buffer[buf_num];
	while(o_current != NULL) {
		printf("- %s\n", o_current->name);
		o_current = o_current->next;
	}
#endif
}

o_buffer_paste_start(TOPLEVEL *w_current, int screen_x, int screen_y, 
		     int buf_num)
{
	int rleft, rtop, rbottom, rright;
	int x, y;

	if (buf_num < 0 || buf_num > MAX_BUFFERS) {
		fprintf(stderr, "Got an invalid buffer_number [o_buffer_paste_start]\n");
		return;
	}

	world_get_complex_bounds(w_current, object_buffer[buf_num], 
			         &rleft, &rtop, 
			         &rright, &rbottom);

	/* snap x and y to the grid, pointed out by Martin Benes */
        x = snap_grid(w_current, rleft);
        y = snap_grid(w_current, rtop);

	o_complex_world_translate(w_current, -x, -y, object_buffer[buf_num]);

	/* now translate selection to current position */
	SCREENtoWORLD(w_current, screen_x, screen_y, &x, &y);
	o_complex_world_translate(w_current, x, y, object_buffer[buf_num]);

        w_current->last_x = w_current->start_x = fix_x(w_current, screen_x);
        w_current->last_y = w_current->start_y = fix_y(w_current, screen_y);
	w_current->event_state = ENDPASTE;

	/* store the buffer number for future use */
	w_current->buffer_number = buf_num;

	o_drawbounding(w_current,
                       object_buffer[buf_num]->next,
                       NULL,
                       x_get_color(w_current->bb_color));
}

o_buffer_paste_end(TOPLEVEL *w_current, int screen_x, int screen_y, 
		   int buf_num)
{
	int w_x, w_y;
	int w_start_x, w_start_y;
	int w_diff_x, w_diff_y;
	OBJECT *o_current;
	OBJECT *o_saved;
	SELECTION *temp_list;
	PAGE *p_current;

	if (buf_num < 0 || buf_num > MAX_BUFFERS) {
		fprintf(stderr, "Got an invalid buffer_number [o_buffer_paste_end]\n");
		return;
	}

	/* erase old image */
        o_drawbounding(w_current,
		       object_buffer[buf_num]->next,
                       NULL,
                       x_get_color(w_current->bb_color));

	/* get the location where we ended */
	SCREENtoWORLD(w_current, screen_x, screen_y, &w_x, &w_y);
	SCREENtoWORLD(w_current, w_current->start_x, w_current->start_y, 
		      &w_start_x, &w_start_y);

#if DEBUG 
	printf("%d %d\n", w_x - w_start_x,  w_y - w_start_y);
#endif
	/* calc and translate objects to their final position */
	w_diff_x = w_x - w_start_x;
	w_diff_y = w_y - w_start_y;
	o_complex_world_translate(w_current, w_diff_x, w_diff_y, 
				  object_buffer[buf_num]);

	o_current = object_buffer[buf_num]->next;
	p_current = w_current->page_current;

	o_saved = p_current->object_tail;	
		o_list_copy_all(w_current, o_current, p_current->object_tail, 
			       	NORMAL_FLAG);

	p_current->object_tail = return_tail(p_current->object_head);
	o_current = o_saved->next;
	temp_list = o_selection_new_head();

	/* now add new objects to the selection list */
	while (o_current != NULL) {
		o_selection_add(temp_list, o_current);
		o_current = o_current->next;
	}

	o_selection_remove_most(w_current,
                                w_current->page_current->selection2_head);
        o_selection_destroy_head(w_current->page_current->selection2_head);
        w_current->page_current->selection2_head = temp_list;
        w_current->page_current->selection2_tail = o_selection_return_tail(
                                                        temp_list);

	w_current->page_current->CHANGED = 1;
        o_conn_disconnect_update(w_current->page_current);
	o_redraw(w_current, w_current->page_current->object_head);
}

o_buffer_paste_rubberpaste(TOPLEVEL *w_current, int buf_num)
{
        o_drawbounding(w_current,
		       object_buffer[buf_num]->next,
                       NULL,
                       x_get_color(w_current->bb_color));
}


void
o_buffer_init(void)
{
	int i;

	for (i = 0 ; i < MAX_BUFFERS; i++) {
		object_buffer[i] = NULL;
	}
}

void
o_buffer_free(TOPLEVEL *w_current)
{
	int i;

	for (i = 0 ; i < MAX_BUFFERS; i++) {
		if (object_buffer[i]) {
			s_delete_list_fromstart(w_current, 
						object_buffer[i]);
			object_buffer[i] = NULL;
		}
	}
}


