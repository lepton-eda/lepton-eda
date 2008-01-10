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
#include <math.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
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
void o_complex_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  g_return_if_fail (o_current != NULL); 
  g_return_if_fail (o_current->complex != NULL);
  g_return_if_fail (o_current->complex->prim_objs != NULL);

  if (!w_current->toplevel->DONT_REDRAW) {
    o_redraw(w_current, o_current->complex->prim_objs, TRUE);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  g_assert( (object->type == OBJ_COMPLEX ||
             object->type == OBJ_PLACEHOLDER) );

  o_list_draw_xor( w_current, dx, dy, object->complex->prim_objs);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_start(GSCHEM_TOPLEVEL *w_current, int screen_x, int screen_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x, y;
  int i, temp;
  const CLibSymbol *sym;
  int redraw_state;

  w_current->last_x = w_current->start_x = fix_x(toplevel, screen_x);
  w_current->last_y = w_current->start_y = fix_y(toplevel, screen_y);

  w_current->last_drawb_mode = -1;

  /* make sure list is null first, so that you don't have a mem
   * leak */
  SCREENtoWORLD(toplevel,
                w_current->start_x,
                w_current->start_y,
                &x,
                &y);

  toplevel->ADDING_SEL = 1; /* reuse this flag, rename later hack */
  sym = s_clib_get_symbol_by_name (toplevel->internal_symbol_name);
  o_complex_add(toplevel, NULL,
		&(toplevel->page_current->complex_place_list),
		OBJ_COMPLEX, WHITE, x, y, 0, 0,
		sym, toplevel->internal_symbol_name,
		1, TRUE);
  toplevel->ADDING_SEL = 0;

  if (w_current->complex_rotate) {
    temp = w_current->complex_rotate / 90;
    for (i = 0; i < temp; i++) {
      o_complex_place_rotate(w_current);
    }
  }

  /* Run the complex place list changed hook without redrawing */
  /* since the complex place list is going to be redrawn afterwards */
  redraw_state = toplevel->DONT_REDRAW;
  toplevel->DONT_REDRAW = 1;
  o_complex_place_changed_run_hook (w_current);
  toplevel->DONT_REDRAW = redraw_state;

  o_drawbounding(w_current, toplevel->page_current->complex_place_list,
                 x_get_darkcolor(w_current->bb_color), TRUE);
}

/*! \brief Run the complex place list changed hook. 
 *  \par Function Description
 *  The complex place list is usually used when placing new components
 *  in the schematic. This function should be called whenever that list
 *  is modified.
 *  \param [in] w_current GSCHEM_TOPLEVEL structure.
 *
 */
void o_complex_place_changed_run_hook(GSCHEM_TOPLEVEL *w_current) {
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *ptr = NULL;

  /* Run the complex place list changed hook */
  if (scm_hook_empty_p(complex_place_list_changed_hook) == SCM_BOOL_F &&
      toplevel->page_current->complex_place_list != NULL) {
    ptr = toplevel->page_current->complex_place_list;
    while (ptr) {
      scm_run_hook(complex_place_list_changed_hook, 
		   scm_cons (g_make_object_smob
			     (toplevel,
			      (OBJECT *) ptr->data), SCM_EOL));
      ptr = g_list_next(ptr);
    }

  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_place_rotate(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  GList *ptr;
  int x_local = -1;
  int y_local = -1;

  ptr = toplevel->page_current->complex_place_list;
  while(ptr) {
    o_current = (OBJECT *) ptr->data;
    switch(o_current->type) {	
      case(OBJ_COMPLEX):
        x_local = o_current->complex->x; 
        y_local = o_current->complex->y;
        break;
    }
    ptr = g_list_next(ptr);
  }

  if (x_local == -1) {
    printf(_("Could not find complex in new component placement!\n"));
    return;
  }

  ptr = toplevel->page_current->complex_place_list;
  while(ptr) {
    o_current = (OBJECT *) ptr->data;
    switch(o_current->type) {	

      case(OBJ_TEXT):
        o_text_rotate_world(toplevel, x_local, y_local, 90, o_current);
        break;

      case(OBJ_COMPLEX):
        o_complex_rotate_world(toplevel, x_local, y_local, 90, o_current);
        break;

    }
    ptr = g_list_next(ptr);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_end(GSCHEM_TOPLEVEL *w_current, int screen_x, int screen_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;
  int x, y;
  OBJECT *o_current;
  OBJECT *o_start;
  OBJECT *o_temp;
  char *buffer;
  int temp, i;
  GList *connected_objects=NULL;
  const CLibSymbol *sym;

  diff_x = w_current->last_x - w_current->start_x;
  diff_y = w_current->last_y - w_current->start_y;

  SCREENtoWORLD(toplevel, screen_x, screen_y, &x, &y);
  x = snap_grid(toplevel, x);
  y = snap_grid(toplevel, y);

#if DEBUG
  printf("place_basename: %s\n",internal_basename);
  printf("place_clib: %s\n",internal_clib);
#endif

  o_drawbounding(w_current,
                 w_current->toplevel->page_current->complex_place_list,
                 x_get_darkcolor(w_current->bb_color), FALSE);

  if (w_current->include_complex) {
    buffer = s_clib_symbol_get_data_by_name (toplevel->internal_symbol_name);

    toplevel->ADDING_SEL=1;
    o_start = toplevel->page_current->object_tail;
    toplevel->page_current->object_tail =
      o_read_buffer(toplevel,
		    toplevel->page_current->object_tail,
		    buffer, -1,
		    toplevel->internal_symbol_name);
    o_start = o_start->next;
    toplevel->ADDING_SEL=0;
    
    o_list_translate_world(toplevel, x, y, o_start);

    o_temp = o_start;
    while (o_temp != NULL) {
      if (o_temp->type == OBJ_NET || o_temp->type == OBJ_PIN ||
          o_temp->type == OBJ_BUS) {
        s_conn_update_object(toplevel, o_temp);
                  
        connected_objects = s_conn_return_others(connected_objects,
                                                 o_temp);
      }
      o_temp = o_temp->next;
    }
    o_cue_undraw_list(w_current, connected_objects);
    o_cue_draw_list(w_current, connected_objects);
    g_list_free(connected_objects);

    g_free(buffer);

    o_redraw(w_current, o_start, TRUE);
    toplevel->page_current->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);
    i_update_menus(w_current);
    s_delete_object_glist(toplevel, toplevel->page_current->
                          complex_place_list);
    toplevel->page_current->complex_place_list = NULL;
    return;
  }

  o_temp = toplevel->page_current->object_tail;
  sym = s_clib_get_symbol_by_name (toplevel->internal_symbol_name);
  toplevel->page_current->object_tail =
    o_complex_add(toplevel,
                  toplevel->page_current->object_tail, NULL,
                  OBJ_COMPLEX, WHITE, x, y, w_current->complex_rotate, 0,
                  sym, toplevel->internal_symbol_name,
		  1, TRUE);

  /* complex rotate post processing */
  o_temp = o_temp->next; /* skip over last object */
  while (o_temp != NULL) {
    switch(o_temp->type) {
      case(OBJ_TEXT):
        temp = w_current->complex_rotate / 90;
        for (i = 0; i < temp; i++) {
          o_text_rotate_world(toplevel, x, y, 90, o_temp);
        }
        break;
    }
		
    o_temp = o_temp->next;
  }

  /* 1 should be define fix everywhere hack */
  o_current = toplevel->page_current->object_tail;

  if (scm_hook_empty_p(add_component_hook) == SCM_BOOL_F &&
      o_current != NULL) {
    scm_run_hook(add_component_hook,
                 scm_cons(g_make_attrib_smob_list(w_current, o_current),
                          SCM_EOL));
  }

  if (scm_hook_empty_p(add_component_object_hook) == SCM_BOOL_F &&
      o_current != NULL) {
    scm_run_hook(add_component_object_hook,
		 scm_cons(g_make_object_smob(toplevel, o_current),
			  SCM_EOL));
  }

  /* put code here to deal with emebedded stuff */
  if (w_current->embed_complex) {
    o_current->complex_embedded = TRUE;
  }

  /*! \todo redraw has to happen at the end of all this hack or
   * maybe not? */
  s_delete_object_glist(toplevel, toplevel->page_current->
                        complex_place_list);
  toplevel->page_current->complex_place_list = NULL;

  /* This doesn't allow anything else to be in the selection
   * list when you add a component */

  o_select_unselect_list( w_current, toplevel->page_current->selection_list );
  o_selection_add( toplevel->page_current->selection_list, toplevel->page_current->object_tail);
  /* the o_redraw_selected is in x_events.c after this call
   * returns */
  o_attrib_add_selected(w_current, toplevel->page_current->selection_list,
                        toplevel->page_current->object_tail);

  s_conn_update_complex(toplevel, o_current->complex->prim_objs);
  connected_objects = s_conn_return_complex_others(connected_objects,
                                                   o_current);
  o_cue_undraw_list(w_current, connected_objects);
  o_cue_draw_list(w_current, connected_objects);
  g_list_free(connected_objects);
  o_cue_draw_single(w_current, toplevel->page_current->object_tail);
        
  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_rubbercomplex(GSCHEM_TOPLEVEL *w_current)
{
  o_drawbounding(w_current,
                 w_current->toplevel->page_current->complex_place_list,
                 x_get_darkcolor(w_current->bb_color), FALSE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't know if this belongs yet
 */
void o_complex_translate_all(GSCHEM_TOPLEVEL *w_current, int offset)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int w_rleft, w_rtop, w_rright, w_rbottom;
  OBJECT *o_current;
  int x, y;

  /* first zoom extents */
  a_zoom_extents(w_current, toplevel->page_current->object_head,
                 A_PAN_DONT_REDRAW);
  o_redraw_all(w_current);

  world_get_object_list_bounds(toplevel, toplevel->page_current->object_head,
                               &w_rleft,
                               &w_rtop,
                               &w_rright,
                               &w_rbottom);

  /*! \todo do we want snap grid here? */
  x = snap_grid( toplevel, w_rleft );
  /* WARNING: w_rtop isn't the top of the bounds, it is the smaller
   * y_coordinate, which represents in the bottom in world coords.
   * These variables are as named from when screen-coords (which had 
   * the correct sense) were in use . */
  y = snap_grid( toplevel, w_rtop );

  o_current = toplevel->page_current->object_head;
  while(o_current != NULL) {
    if (o_current->type != OBJ_COMPLEX && o_current->type != OBJ_PLACEHOLDER) {
      s_conn_remove(toplevel, o_current);
    } else {
      s_conn_remove_complex(toplevel, o_current);
    }
    o_current = o_current->next;
  }
        
  if (offset == 0) {
    s_log_message(_("Translating schematic [%d %d]\n"), -x, -y);
    o_list_translate_world(toplevel, -x, -y,
                           toplevel->page_current->object_head);
  } else {
    s_log_message(_("Translating schematic [%d %d]\n"),
                  offset, offset);
    o_list_translate_world(toplevel, offset, offset,
                           toplevel->page_current->object_head);
  }

  o_current = toplevel->page_current->object_head;
  while(o_current != NULL) {
    if (o_current->type != OBJ_COMPLEX && o_current->type != OBJ_PLACEHOLDER) {
      s_conn_update_object(toplevel, o_current);
    } else {
      s_conn_update_complex(toplevel, o_current->complex->prim_objs);
    }
    o_current = o_current->next;
  }

  /* this is an experimental mod, to be able to translate to all
   * places */
  a_zoom_extents(w_current, toplevel->page_current->object_head,
                 A_PAN_DONT_REDRAW);
  if (!w_current->SHIFTKEY) o_select_unselect_all(w_current);
  o_redraw_all(w_current);
  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}
