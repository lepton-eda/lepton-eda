/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_copy(TOPLEVEL *w_current, int buf_num)
{
  GList *s_current = NULL;

  if (buf_num < 0 || buf_num > MAX_BUFFERS) {
    fprintf(stderr, _("Got an invalid buffer_number [o_buffer_copy]\n"));
    return;
  }

  s_current = geda_list_get_glist( w_current->page_current->selection_list );

  if (object_buffer[buf_num] != NULL) {
    s_delete_object_glist(w_current, object_buffer[buf_num]);
    object_buffer[buf_num] = NULL;
  }

  w_current->ADDING_SEL = 1;
  object_buffer[buf_num] =
    o_glist_copy_all_to_glist(w_current, s_current,
                              object_buffer[buf_num], SELECTION_FLAG);
  w_current->ADDING_SEL = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_cut(TOPLEVEL *w_current, int buf_num)
{
  GList *s_current = NULL;

  if (buf_num < 0 || buf_num > MAX_BUFFERS) {
    fprintf(stderr, _("Got an invalid buffer_number [o_buffer_cut]\n"));
    return;
  }

  s_current = geda_list_get_glist( w_current->page_current->selection_list );

  if (object_buffer[buf_num] != NULL) {
    s_delete_object_glist(w_current, object_buffer[buf_num]);
    object_buffer[buf_num] = NULL;
  }
        
  w_current->ADDING_SEL = 1;
  object_buffer[buf_num] =
    o_glist_copy_all_to_glist(w_current, s_current,
                              object_buffer[buf_num], SELECTION_FLAG);
  w_current->ADDING_SEL = 0;
  o_delete(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_paste_start(TOPLEVEL *w_current, int screen_x, int screen_y, 
			  int buf_num)
{
  int rleft, rtop, rbottom, rright;
  int x, y;

  if (buf_num < 0 || buf_num > MAX_BUFFERS) {
    fprintf(stderr, _("Got an invalid buffer_number [o_buffer_paste_start]\n"));
    return;
  }

  if (!world_get_object_glist_bounds(w_current, object_buffer[buf_num],
                                     &rleft, &rtop,
                                     &rright, &rbottom)) {
    /* If the paste buffer doesn't have any objects
     * to define its any bounds, we drop out here */
    return;
  }

  /* snap x and y to the grid, pointed out by Martin Benes */
  x = snap_grid(w_current, rleft);
  y = snap_grid(w_current, rtop);

  w_current->ADDING_SEL = 1;
  o_glist_translate_world(w_current, -x, -y, object_buffer[buf_num]);
  w_current->ADDING_SEL = 0;

  /* now translate selection to current position */
  SCREENtoWORLD(w_current, screen_x, screen_y, &x, &y);
  x = snap_grid(w_current, x);
  y = snap_grid(w_current, y);

  w_current->ADDING_SEL = 1;
  o_glist_translate_world(w_current, x, y, object_buffer[buf_num]);
  w_current->ADDING_SEL = 0;

  w_current->last_x = w_current->start_x = fix_x(w_current, screen_x);
  w_current->last_y = w_current->start_y = fix_y(w_current, screen_y);
  w_current->event_state = ENDPASTE;

  /* store the buffer number for future use */
  w_current->buffer_number = buf_num;

  o_drawbounding(w_current, object_buffer[buf_num],
                 x_get_darkcolor(w_current->bb_color), TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_paste_end(TOPLEVEL *w_current, int screen_x, int screen_y, 
			int buf_num)
{
  int w_x, w_y;
  int w_start_x, w_start_y;
  int w_diff_x, w_diff_y;
  OBJECT *o_current;
  OBJECT *o_saved;
  SELECTION *temp_list = o_selection_new();
  PAGE *p_current;
  GList *connected_objects = NULL;

  if (buf_num < 0 || buf_num > MAX_BUFFERS) {
    fprintf(stderr, _("Got an invalid buffer_number [o_buffer_paste_end]\n"));
    return;
  }

  /* erase old image */
  o_drawbounding(w_current, object_buffer[buf_num],
                 x_get_darkcolor(w_current->bb_color), FALSE);

  /* get the location where we ended */
  SCREENtoWORLD(w_current, screen_x, screen_y, &w_x, &w_y);
  SCREENtoWORLD(w_current, w_current->start_x, w_current->start_y, 
                &w_start_x, &w_start_y);
  w_x = snap_grid(w_current, w_x);
  w_y = snap_grid(w_current, w_y);
  w_start_x = snap_grid(w_current, w_start_x);
  w_start_y = snap_grid(w_current, w_start_y);

#if DEBUG 
  printf("%d %d\n", w_x - w_start_x,  w_y - w_start_y);
#endif
  /* calc and translate objects to their final position */
  w_diff_x = w_x - w_start_x;
  w_diff_y = w_y - w_start_y;
  w_current->ADDING_SEL = 1;
  o_glist_translate_world(w_current, w_diff_x, w_diff_y,
                          object_buffer[buf_num]);
  w_current->ADDING_SEL = 0;

  o_current = object_buffer[buf_num]->data;
  p_current = w_current->page_current;

  o_saved = p_current->object_tail;	
  o_list_copy_all(w_current, o_current, p_current->object_tail, 
                  NORMAL_FLAG);

  p_current->object_tail = return_tail(p_current->object_head);
  o_current = o_saved->next;

  /* now add new objects to the selection list */
  while (o_current != NULL) {
    o_selection_add( temp_list, o_current );
    s_conn_update_object(w_current, o_current);
    if (o_current->type == OBJ_COMPLEX || o_current->type == OBJ_PLACEHOLDER) {
      connected_objects = s_conn_return_complex_others(
                                                       connected_objects,
                                                       o_current);
    } else {
      connected_objects = s_conn_return_others(connected_objects,
                                               o_current);
    }
    o_current = o_current->next;
  }

  o_cue_redraw_all(w_current, o_saved->next, TRUE);
  o_cue_undraw_list(w_current, connected_objects);
  o_cue_draw_list(w_current, connected_objects);
  g_list_free(connected_objects);
  connected_objects = NULL;

  o_select_unselect_all( w_current );
  geda_list_add_glist( w_current->page_current->selection_list, geda_list_get_glist( temp_list ) );

  g_object_unref( temp_list );

  w_current->page_current->CHANGED = 1;
  o_redraw(w_current, o_saved->next, TRUE); /* only redraw new objects */
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_paste_rubberpaste(TOPLEVEL *w_current, int buf_num)
{
  o_drawbounding(w_current, object_buffer[buf_num],
                 x_get_darkcolor(w_current->bb_color), FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_init(void)
{
	int i;

	for (i = 0 ; i < MAX_BUFFERS; i++) {
		object_buffer[i] = NULL;
	}
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_buffer_free(TOPLEVEL *w_current)
{
  int i;

  for (i = 0 ; i < MAX_BUFFERS; i++) {
    if (object_buffer[i]) {
      s_delete_object_glist(w_current, object_buffer[i]);
      object_buffer[i] = NULL;
    }
  }
}
