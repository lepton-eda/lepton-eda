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
/*! The code in this file is sometimes not obvious, especially
 * o_select_object (which implements the selection of objects either
 * when doing a single or multi select)
 *
 * Also, there are cases where it looks like there is redundant code, which
 * could be removed/merged, but I purposely didn't do so to keep the code
 * readable
 *
 * the count == 0 stuff really only applies to when you are coming from a
 * multi select case
 */
#include <config.h>

#include <math.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_run_hooks(TOPLEVEL *w_current, OBJECT *o_current, int flag)
{
  /*
   * Run the select_component_hook if the hook has been defined and we
   * are selecting a component.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(deselect_all_hook) == SCM_BOOL_F) 
       && flag == 2 )
  {
    scm_run_hook(deselect_all_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current), 
			   SCM_EOL));
  }

  /*
   * Run the select_component_hook if the hook has been defined and we
   * are selecting a component.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(select_component_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_COMPLEX) 
       && flag == 1 )
  {
    scm_run_hook(select_component_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current), 
			   SCM_EOL));
  }

  /*
   * Run the deselect_component_hook if the hook has been defined and we
   * are deselecting a component.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(deselect_component_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_COMPLEX) 
       && flag == 0 )
  {
    scm_run_hook(deselect_component_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current), 
			   SCM_EOL));
  }

  /*
   * Run the select_net_hook if the hook has been defined and we
   * are selecting a net.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(select_net_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_NET) 
       && flag == 1) 
  {
    scm_run_hook(select_net_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current), 
			   SCM_EOL));
  }

  /*
   * Run the deselect_net_hook if the hook has been defined and we
   * are deselecting a net.  This will likely be used for cross probing
   * between schematics and PCB layout or schematics and simulation results.
   */
  if ( (scm_hook_empty_p(select_net_hook) == SCM_BOOL_F) 
       && o_current
       && (o_current->type == OBJ_NET) 
       && flag == 0) 
  {
    scm_run_hook(deselect_net_hook, 
		 scm_cons (g_make_attrib_smob_list(w_current, o_current), 
			   SCM_EOL));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  type can be either SINGLE meaning selection is a single mouse click 
 *      or it can be MULTIPLE meaning selection is a selection box
 */
void o_select_object(TOPLEVEL *w_current, OBJECT *o_current, 
		     int type, int count)
{
  int SHIFTKEY;
  int CONTROLKEY;

  SHIFTKEY = w_current->SHIFTKEY;
  CONTROLKEY = w_current->CONTROLKEY;

#if DEBUG 
  printf("OBJECT id: %d\n", o_current->sid);
#endif

  switch(o_current->selected) {

    case(FALSE): /* object not selected */

      switch(SHIFTKEY) { /* shift key pressed? */

        case(TRUE): /* shift key pressed */
          /* just fall through */
          break;

        case(FALSE):

          /* condition: first object being added */
          /* condition: control key not pressed */
          /* condition: for both multiple and single object added */
          /* result: remove all objects from selection */
          if (count == 0 && !CONTROLKEY) {
            o_select_run_hooks(w_current, NULL, 2);
	    o_selection_unselect_list (w_current,
				       &(w_current->page_current->selection_list));
          }
          break;

      } /* end shift key switch */

      /* object not select, add it to the selection list */
      o_select_run_hooks(w_current, o_current, 1);
      o_selection_add(&(w_current->page_current->selection_list),
		      o_current);

      break;


    case(TRUE): /* object was already selected */

      switch(SHIFTKEY) { /* shift key pressed ? */

        case(TRUE): /* shift key pressed */

          /* condition: not doing multiple */
          /* result: remove object from selection */
          if (type != MULTIPLE) {
            o_select_run_hooks(w_current, o_current, 0);
            o_selection_remove(&(w_current->page_current->selection_list),
                               o_current);
          }

          break;

        case(FALSE): /* shift key not pressed */

          /* condition: doing multiple */
          /* condition: first object being added */
          /* condition: control key not pressed */
          /* 1st result: remove all objects from selection */
          /* 2nd result: add object to selection */
          if (type == MULTIPLE && count == 0 && !CONTROLKEY) {
            o_select_run_hooks(w_current, NULL, 2);
	    o_selection_unselect_list (w_current,
				       &(w_current->page_current->selection_list));
	    
	    o_select_run_hooks(w_current, o_current, 1);
	    o_selection_add(&(w_current->page_current->selection_list),
			    o_current);
          }	

          /* condition: doing single object add */
          /* condition: control key not pressed */
          /* 1st result: remove all objects from selection */
          /* 2nd result: add object to selection list */
          if (type == SINGLE && !CONTROLKEY) {
            o_select_run_hooks(w_current, NULL, 2);
	    o_selection_unselect_list (w_current,
				       &(w_current->page_current->selection_list));

            o_select_run_hooks (w_current, o_current, 1);
	    o_selection_add(&(w_current->page_current->selection_list),
			    o_current);
          }

          if (CONTROLKEY) {
            o_select_run_hooks(w_current, o_current, 0);
            o_selection_remove(&(w_current->page_current->selection_list),
                               o_current);
          }

          break;
      } 
      break; /* end object selected switch */
  }

  /* do the attributes */
  o_attrib_add_selected(w_current, &(w_current->page_current->selection_list),
                        o_current);

  /* finally redraw object */
  o_redraw_single(w_current, o_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_start(TOPLEVEL *w_current, int x, int y)
{
  int box_width, box_height;

  /* don't set these to the passed in x, y */
  w_current->last_x = w_current->start_x; 
  w_current->last_y = w_current->start_y; 

  box_width = abs(w_current->last_x - w_current->start_x);
  box_height = abs(w_current->last_y - w_current->start_y);

  gdk_gc_set_foreground(w_current->xor_gc,
                        x_get_darkcolor(w_current->select_color));
  gdk_draw_rectangle(w_current->window, w_current->xor_gc,
                     FALSE,
                     w_current->start_x,
                     w_current->start_y,
                     box_width,
                     box_height);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_end(TOPLEVEL *w_current, int x, int y)
{
  int box_width, box_height;
  int box_left, box_top;

#if 0
  if (w_current->inside_action == 0) {
    o_redraw(w_current, w_current->page_current->object_head, TRUE);
    return;
  }
#endif

  box_width = abs(w_current->last_x - w_current->start_x);
  box_height = abs(w_current->last_y - w_current->start_y);	

  if( w_current->last_y < w_current->start_y )
  box_top = w_current->last_y;
  else
  box_top = w_current->start_y;

  if( w_current->last_x < w_current->start_x )
  box_left = w_current->last_x;
  else
  box_left = w_current->start_x;

  gdk_gc_set_foreground(w_current->xor_gc,
                        x_get_darkcolor(w_current->select_color));
  gdk_draw_rectangle(w_current->window, w_current->xor_gc,
                     FALSE,
                     box_left,
                     box_top,
                     box_width,
                     box_height);

  o_select_box_search(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_rubberband(TOPLEVEL *w_current, int x, int y)
{
  int box_width, box_height;
  int box_left, box_top;

#if 0
  if (w_current->inside_action == 0) {
    o_redraw(w_current, w_current->page_current->object_head, TRUE);
    return;
  }
#endif

  box_width = abs(w_current->last_x - w_current->start_x);
  box_height = abs(w_current->last_y - w_current->start_y);

  if( w_current->last_y < w_current->start_y )
  box_top = w_current->last_y;
  else
  box_top = w_current->start_y;

  if( w_current->last_x < w_current->start_x )
  box_left = w_current->last_x;
  else
  box_left = w_current->start_x;


  gdk_gc_set_foreground(w_current->xor_gc,
                        x_get_darkcolor(w_current->select_color));
  gdk_draw_rectangle(w_current->window, w_current->xor_gc,
                     FALSE,
                     box_left  ,
                     box_top   ,
                     box_width ,
                     box_height);


  /* removed fix_x, fix_y to unrestrict sels */
  w_current->last_x = (int) x; 
  w_current->last_y = (int) y;

  box_width = abs(w_current->last_x - w_current->start_x);
  box_height = abs(w_current->last_y - w_current->start_y);

  if( w_current->last_y < w_current->start_y )
  box_top = w_current->last_y;
  else
  box_top = w_current->start_y;

  if( w_current->last_x < w_current->start_x )
  box_left = w_current->last_x;
  else
  box_left = w_current->start_x;

  gdk_draw_rectangle(w_current->window, w_current->xor_gc,
                     FALSE,
                     box_left,
                     box_top,
                     box_width,
                     box_height);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_box_search(TOPLEVEL *w_current) 
{
  OBJECT *o_current=NULL;
  int count = 0; /* object count */
  int SHIFTKEY = w_current->SHIFTKEY;
  int w_start_x, w_start_y, w_last_x, w_last_y;
	
  int tmp;
	
  if( w_current->last_x < w_current->start_x ) {
    tmp = w_current->last_x;
    w_current->last_x = w_current->start_x;
    w_current->start_x = tmp;
  }

  if( w_current->last_y < w_current->start_y ) {
    tmp = w_current->last_y;
    w_current->last_y = w_current->start_y;
    w_current->start_y = tmp;
  }

  SCREENtoWORLD( w_current, w_current->start_x, w_current->start_y, &w_start_x, &w_start_y );
  SCREENtoWORLD( w_current, w_current->last_x, w_current->last_y, &w_last_x, &w_last_y );

  o_current = w_current->page_current->object_head;

  while (o_current != NULL) {
    /* only select visible objects */
    if (o_current->type != OBJ_HEAD && 
        (o_current->visibility == VISIBLE ||
        (o_current->visibility == INVISIBLE && w_current->show_hidden_text))) {
      if ( (o_current->left >= w_current->start_x && 
            o_current->top >= w_current->start_y) &&
           (o_current->left >= w_current->start_x && 
            o_current->bottom <= w_current->last_y) &&
           (o_current->right <= w_current->last_x && 
            o_current->top >= w_current->start_y ) &&
           (o_current->right <= w_current->last_x && 
            o_current->bottom <= w_current->last_y) ) {

        o_select_object(w_current, o_current, 	
                        MULTIPLE, count);
        count++;
      }
    }
    o_current = o_current->next;
  }

  /* if there were no objects to be found in select box, count will be */
  /* zero, and you need to deselect anything remaining (unless the shift */
  /* key was pressed */
  if (count == 0 && !SHIFTKEY)  {
    o_select_run_hooks(w_current, NULL, 2);
    o_selection_unselect_list (w_current,
			       &(w_current->page_current->selection_list));
  }
  i_update_menus(w_current);
}

/* This is a wrapper for o_selection_return_first_object */
/* This function always looks at the current page selection list */ 
OBJECT *o_select_return_first_object(TOPLEVEL *w_current) 
{
  if (! (w_current && w_current->page_current && w_current->page_current->selection_list))
    return NULL;
  else
    return((OBJECT *) g_list_first(w_current->page_current->selection_list)->data);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * \return TRUE if the selection list is not empty, otherwise false.
 * also make sure item is valid
 */
int o_select_selected(TOPLEVEL *w_current)
{
  if (w_current->page_current->selection_list) {
    return(TRUE);
  }
  return(FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_select_unselect_all(TOPLEVEL *w_current)
{
  o_select_run_hooks(w_current, NULL, 2);
  o_selection_unselect_list (w_current,
			     &(w_current->page_current->selection_list));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_select_move_to_place_list(TOPLEVEL *w_current)
{
  GList *selection;
  OBJECT *o_current;
  
  selection= w_current->page_current->selection_list;

  if (!selection) {
    return;
  }

  while (selection) {
    o_current = (OBJECT *) selection->data;
    if (o_current) {
      w_current->page_current->complex_place_list = g_list_append(w_current->page_current->complex_place_list,
								  o_current);
    }
    selection = selection->next;
  }
}
