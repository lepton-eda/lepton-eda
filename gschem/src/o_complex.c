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
#include <math.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

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
void o_complex_draw(TOPLEVEL *w_current, OBJECT *o_current)
{
  g_return_if_fail (o_current != NULL); 
  g_return_if_fail (o_current->complex != NULL);
  g_return_if_fail (o_current->complex->prim_objs != NULL);

  if (!w_current->DONT_REDRAW) {
    o_redraw(w_current, o_current->complex->prim_objs, TRUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_erase(TOPLEVEL *w_current, OBJECT *o_current)
{
  w_current->override_color = w_current->background_color;
  o_complex_draw(w_current, o_current);
  w_current->override_color = -1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_draw_xor(TOPLEVEL *w_current, int dx, int dy, OBJECT *complex)
{
  OBJECT *o_current = complex;

  while(o_current != NULL) {
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_NET):
        o_net_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_BUS):
        o_bus_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_BOX):
        o_box_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_PICTURE):
        o_picture_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_CIRCLE):
        o_circle_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_complex_draw_xor(w_current, dx, dy,
                           o_current->complex->prim_objs);
        break;

      case(OBJ_TEXT):
        o_text_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_PIN):
        o_pin_draw_xor(w_current, dx, dy, o_current);
        break;

      case(OBJ_ARC):
        o_arc_draw_xor(w_current, dx, dy, o_current);
        break;
    }
    o_current = o_current->next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_start(TOPLEVEL *w_current, int screen_x, int screen_y)
{
  int x, y;
  int i, temp;

  w_current->last_x = w_current->start_x = fix_x(w_current, screen_x);
  w_current->last_y = w_current->start_y = fix_y(w_current, screen_y);

  w_current->last_drawb_mode = -1;

  /* make sure list is null first, so that you don't have a mem
   * leak */
  SCREENtoWORLD(w_current,
                w_current->start_x,
                w_current->start_y,
                &x,
                &y);

  w_current->DONT_DRAW_CONN = 1;
  w_current->ADDING_SEL = 1; /* reuse this flag, rename later hack */
  o_complex_add(w_current, NULL,
		&(w_current->page_current->complex_place_list),
		OBJ_COMPLEX, WHITE, x, y, 0, 0,
		w_current->internal_clib,
		w_current->internal_basename, 1, TRUE);
  w_current->ADDING_SEL = 0;
  w_current->DONT_DRAW_CONN = 0;

  if (w_current->complex_rotate) {
    temp = w_current->complex_rotate / 90;
    for (i = 0; i < temp; i++) {
      o_complex_place_rotate(w_current);
    }
  }

  /* Run the complex place list changed hook */
  o_complex_place_changed_run_hook (w_current);

  o_drawbounding(w_current, 
                 NULL,
                 w_current->page_current->complex_place_list,
                 x_get_darkcolor(w_current->bb_color), TRUE);
}

/*! \brief Run the complex place list changed hook. 
 *  \par Function Description
 *  The complex place list is usually used when placing new components
 *  in the schematic. This function should be called whenever that list
 *  is modified.
 *  \param [in] w_current TOPLEVEL structure.
 *
 */
void o_complex_place_changed_run_hook(TOPLEVEL *w_current) {
  GList *ptr = NULL;

  /* Run the complex place list changed hook */
  if (scm_hook_empty_p(complex_place_list_changed_hook) == SCM_BOOL_F &&
      w_current->page_current->complex_place_list != NULL) {
    ptr = w_current->page_current->complex_place_list;
    while (ptr) {
      scm_run_hook(complex_place_list_changed_hook, 
		   scm_cons (g_make_object_smob
			     (w_current, 
			      (OBJECT *) ptr->data), SCM_EOL));
      ptr = ptr->next;
    }

  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_place_rotate(TOPLEVEL *w_current)
{
  OBJECT *o_current;
  GList *ptr;
  int x_local = -1;
  int y_local = -1;
  int new_angle;

  ptr = w_current->page_current->complex_place_list;
  while(ptr) {
    o_current = (OBJECT *) ptr->data;
    switch(o_current->type) {	
      case(OBJ_COMPLEX):
        x_local = o_current->complex->x; 
        y_local = o_current->complex->y;
        break;
    }
    ptr = ptr->next;
  }

  if (x_local == -1) {
    printf(_("Could not find complex in new componet placement!\n"));
    return;
  }

  ptr = w_current->page_current->complex_place_list;
  while(ptr) {
    o_current = (OBJECT *) ptr->data;
    switch(o_current->type) {	

      case(OBJ_TEXT):
        new_angle = (o_current->text->angle + 90) % 360;
        o_text_rotate_world(w_current, x_local, y_local,
                            new_angle, 90, o_current);
        break;

      case(OBJ_COMPLEX):
        new_angle = (o_current->complex->angle + 90) % 360;
        o_complex_rotate_world(w_current, x_local, y_local,
                               new_angle, 90, o_current);
        break;

    }
    ptr = ptr->next;
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_end(TOPLEVEL *w_current, int screen_x, int screen_y)
{
  int diff_x, diff_y;
  int x, y;
  int rleft, rtop, rbottom, rright;
  int w_rleft, w_rtop, w_rbottom, w_rright;
  OBJECT *o_current;
  OBJECT *o_start;
  OBJECT *o_temp;
  char *include_filename;
  int temp, new_angle, i;
  GList *connected_objects=NULL;

  diff_x = w_current->last_x - w_current->start_x;
  diff_y = w_current->last_y - w_current->start_y;

  SCREENtoWORLD(w_current, screen_x, screen_y, &x, &y);
  x = snap_grid(w_current, x);
  y = snap_grid(w_current, y);

#if DEBUG
  printf("place_basename: %s\n",internal_basename);
  printf("place_clib: %s\n",internal_clib);
#endif

  if (w_current->include_complex) {
    include_filename = g_strconcat (w_current->internal_clib,
                                    G_DIR_SEPARATOR_S,
                                    w_current->internal_basename,
                                    NULL);

    w_current->ADDING_SEL=1;
    o_start = w_current->page_current->object_tail;
    w_current->page_current->object_tail =
      o_read(w_current,
             w_current->page_current->object_tail,
             include_filename);
    o_start = o_start->next;
    w_current->ADDING_SEL=0;

    o_complex_world_translate(w_current, x, y, o_start);

    o_temp = o_start;
    while (o_temp != NULL) {
      if (o_temp->type == OBJ_NET || o_temp->type == OBJ_PIN ||
          o_temp->type == OBJ_BUS) {
        s_conn_update_object(w_current, o_temp);
                  
        connected_objects = s_conn_return_others(connected_objects,
                                                 o_temp);
      }
      o_temp = o_temp->next;
    }
    o_cue_undraw_list(w_current, connected_objects);
    o_cue_draw_list(w_current, connected_objects);
    g_list_free(connected_objects);

    g_free(include_filename);

    if (w_current->actionfeedback_mode == OUTLINE) {
#if 0
      printf("inside draw outline here\n");
#endif
      /* erase outline */
      
      o_complex_translate_display_object_glist(w_current,
					       diff_x, diff_y,
					       w_current->page_current->
					       complex_place_list); 
    } else {
#if 0
      printf("inside draw bounding here\n");
#endif
      world_get_object_glist_bounds(w_current,
			      w_current->page_current->
			      complex_place_list,
			      &w_rleft, &w_rtop, &w_rright, &w_rbottom);

      WORLDtoSCREEN( w_current, w_rleft, w_rtop, &rleft, &rtop );
      WORLDtoSCREEN( w_current, w_rright, w_rbottom, &rright, &rbottom );

      gdk_gc_set_foreground(
                            w_current->gc,
                            x_get_color(w_current->background_color));
      gdk_draw_rectangle(w_current->window, w_current->gc,
                         FALSE,
                         rleft   + diff_x,
                         rtop    + diff_y,
                         rright  - rleft,
                         rbottom - rtop);
    }

    o_redraw(w_current, o_start, TRUE);
    w_current->page_current->CHANGED = 1;
    o_undo_savestate(w_current, UNDO_ALL);
    i_update_menus(w_current);
    s_delete_object_glist(w_current, w_current->page_current->
                          complex_place_list);
    w_current->page_current->complex_place_list = NULL;
    return;
  }

  o_temp = w_current->page_current->object_tail;
  w_current->page_current->object_tail =
    o_complex_add(w_current,
                  w_current->page_current->object_tail, NULL,
                  OBJ_COMPLEX, WHITE, x, y, w_current->complex_rotate, 0,
                  w_current->internal_clib,
                  w_current->internal_basename, 1, TRUE);

  /* complex rotate post processing */
  o_temp = o_temp->next; /* skip over last object */
  while (o_temp != NULL) {
    switch(o_temp->type) {
      case(OBJ_TEXT):
        temp = w_current->complex_rotate / 90;
        for (i = 0; i < temp; i++) {
          new_angle = (o_temp->
                       text->angle + 90) % 360;
          o_text_rotate_world(w_current, 
                              x, y,
                              new_angle, 90, o_temp);
        }
        break;
    }
		
    o_temp = o_temp->next;
  }

  /* 1 should be define fix everywhere hack */
  o_current = w_current->page_current->object_tail;

  if (scm_hook_empty_p(add_component_hook) == SCM_BOOL_F &&
      o_current != NULL) {
    scm_run_hook(add_component_hook,
                 scm_cons(g_make_attrib_smob_list(w_current, o_current),
                          SCM_EOL));
  }

  if (scm_hook_empty_p(add_component_object_hook) == SCM_BOOL_F &&
      o_current != NULL) {
    scm_run_hook(add_component_object_hook,
		 scm_cons(g_make_object_smob(w_current, o_current),
			  SCM_EOL));
  }

  /* put code here to deal with emebedded stuff */
  if (w_current->embed_complex) {
    char* new_basename;

    g_free(o_current->complex_clib);

    o_current->complex_clib = g_strdup ("EMBEDDED");

    new_basename = g_strconcat ("EMBEDDED",
                                o_current->complex_basename,
                                NULL);

    g_free(o_current->complex_basename);

    o_current->complex_basename = g_strdup (new_basename);

    g_free(new_basename);
  }

  /* check for nulls in all this hack */
  if (w_current->actionfeedback_mode == OUTLINE) {
#if 0
    printf("inside draw outline here\n");
#endif
    /* erase outline */
    o_complex_translate_display_object_glist(w_current,
					     diff_x, diff_y,
					     w_current->page_current->complex_place_list);
  } else {
#if 0
    printf("inside draw bounding here\n");
#endif
    world_get_object_glist_bounds(w_current,
                                  w_current->page_current->complex_place_list,
                                  &w_rleft, &w_rtop,
                                  &w_rright, &w_rbottom);
    
    WORLDtoSCREEN( w_current, w_rleft, w_rtop, &rleft, &rtop );
    WORLDtoSCREEN( w_current, w_rright, w_rbottom, &rright, &rbottom );

    gdk_gc_set_foreground(
                          w_current->gc,
                          x_get_color(w_current->background_color));
    gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
                       rleft   + diff_x,
                       rtop    + diff_y,
                       rright  - rleft,
                       rbottom - rtop);
  }

  /*! \todo redraw has to happen at the end of all this hack or
   * maybe not? */
  g_list_free(w_current->page_current->complex_place_list);
  w_current->page_current->complex_place_list = NULL;

  /* This doesn't allow anything else to be in the selection
   * list when you add a component */

  o_selection_unselect_list(w_current,
			    &(w_current->page_current->selection_list));
  o_selection_add(&(w_current->page_current->selection_list), 
		    w_current->page_current->object_tail);
  /* the o_redraw_selected is in x_events.c after this call
   * returns */
  o_attrib_add_selected(w_current, &(w_current->page_current->selection_list),
                        w_current->page_current->object_tail);

  s_conn_update_complex(w_current, o_current->complex->prim_objs);
  connected_objects = s_conn_return_complex_others(connected_objects,
                                                   o_current);
  o_cue_undraw_list(w_current, connected_objects);
  o_cue_draw_list(w_current, connected_objects);
  g_list_free(connected_objects);
  o_cue_draw_single(w_current, w_current->page_current->object_tail);
        
  w_current->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_rubbercomplex(TOPLEVEL *w_current)
{
  o_drawbounding(w_current,
                 NULL,
                 w_current->page_current->complex_place_list,
                 x_get_darkcolor(w_current->bb_color), FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_complex_translate_display_single_object(TOPLEVEL *w_current,
					  int x1, int y1, OBJECT *o_current)
{
  if (o_current != NULL) {
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_NET):
        o_net_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_BUS):
        o_bus_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_BOX):
        o_box_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_PICTURE):
        o_picture_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_CIRCLE):
        o_circle_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_complex_draw_xor(w_current, x1, y1, 
                           o_current->complex->prim_objs);
        break;

      case(OBJ_TEXT):
        o_text_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_PIN):
        o_pin_draw_xor(w_current, x1, y1, o_current);
        break;

      case(OBJ_ARC):
        o_arc_draw_xor(w_current, x1, y1, o_current);
        break;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_complex_translate_display_object_glist(TOPLEVEL *w_current,
			                 int x1, int y1, GList *object_list)
{
  GList *ptr = object_list;

  while (ptr != NULL) {
    o_complex_translate_display_single_object (w_current, x1, y1, 
                                               (OBJECT *)ptr->data);
    ptr = ptr->next;
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_complex_translate_display(TOPLEVEL *w_current,
			    int x1, int y1, OBJECT *complex)
{
  OBJECT *o_current = complex;

  while (o_current != NULL) {
    o_complex_translate_display_single_object (w_current, x1, y1, o_current);
    o_current = o_current->next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_translate_display_selection(TOPLEVEL *w_current,
					   int x1, int y1, GList *head)
{
  GList *s_current = head;
  OBJECT *o_current;

  while (s_current != NULL) {

    o_current = (OBJECT *) s_current->data;

    if (!o_current) {
      fprintf(stderr, _("Got NULL in o_complex_translate_display_selection\n"));
      exit(-1);
    }
    o_complex_translate_display_single_object (w_current, x1, y1, o_current);
    s_current = s_current->next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  experimental
 */
void o_complex_translate2(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  if (object == NULL)  {
    printf("cmt2 NO!\n");
    return;
  }

  o_complex_translate_display(w_current, dx, dy, object);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't know if this belongs yet
 */
void o_complex_translate_all(TOPLEVEL *w_current, int offset)
{
  int w_rleft, w_rtop, w_rright, w_rbottom;
  OBJECT *o_current;
  int x, y;

  /* first zoom extents */
  a_zoom_extents(w_current, w_current->page_current->object_head, 
                 A_PAN_DONT_REDRAW);
  o_redraw_all(w_current);

  world_get_object_list_bounds(w_current, w_current->page_current->object_head,
                               &w_rleft,
                               &w_rtop,
                               &w_rright,
                               &w_rbottom);

  /*! \todo do we want snap grid here? */
  x = snap_grid( w_current, w_rleft );
  /* WARNING: w_rtop isn't the top of the bounds, it is the smaller
   * y_coordinate, which represents in the bottom in world coords.
   * These variables are as named from when screen-coords (which had 
   * the correct sense) were in use . */
  y = snap_grid( w_current, w_rtop );

  o_current = w_current->page_current->object_head;
  while(o_current != NULL) {
    if (o_current->type != OBJ_COMPLEX && o_current->type != OBJ_PLACEHOLDER) {
      s_conn_remove(w_current, o_current);
    } else {
      s_conn_remove_complex(w_current, o_current);
    }
    o_current = o_current->next;
  }
        
  if (offset == 0) {
    s_log_message(_("Translating schematic [%d %d]\n"), -x, -y);
    o_complex_world_translate(
                              w_current,
                              -x, -y,
                              w_current->page_current->object_head);
  } else {
    s_log_message(_("Translating schematic [%d %d]\n"),
                  offset, offset);
    o_complex_world_translate(
                              w_current,
                              offset, offset,
                              w_current->page_current->object_head);
  }

  o_current = w_current->page_current->object_head;
  while(o_current != NULL) {
    if (o_current->type != OBJ_COMPLEX && o_current->type != OBJ_PLACEHOLDER) {
      s_conn_update_object(w_current, o_current);
    } else {
      s_conn_update_complex(w_current, o_current->complex->prim_objs);
    }
    o_current = o_current->next;
  }

  /* this is an experimental mod, to be able to translate to all
   * places */
#if 0
  o_complex_world_translate(1000, 1000, object_head);
  printf("symbol -%d -%d\n", x, y);
  o_complex_world_translate(-x, -y, object_head); /* to zero, zero */
#endif

  a_zoom_extents(w_current, w_current->page_current->object_head, 
                 A_PAN_DONT_REDRAW);
  o_unselect_all(w_current);
  o_redraw_all(w_current);
  w_current->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_translate_selection(TOPLEVEL *w_current, int dx, int dy, 
				   GList *head)
{
  if (head == NULL)  {
    printf(_("Got NULL in o_complex_translate_selection!\n"));
    return;
  }

  o_complex_translate_display_selection(w_current, dx, dy, head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_rotate_world(TOPLEVEL *w_current, int centerx, int centery,
		            int angle, int angle_change, OBJECT *object)
{
  int x, y;
  int newx, newy;

  x = object->complex->x + (-centerx);
  y = object->complex->y + (-centery);

  rotate_point_90(x, y, 90, &newx, &newy);

  x = newx + (centerx);
  y = newy + (centery);

  o_complex_world_translate_toplevel(w_current,
                                     -object->complex->x, 
                                     -object->complex->y, object);
  o_complex_rotate_lowlevel(w_current,
                            0, 0, angle, angle_change, object);

  object->complex->x = 0;
  object->complex->y = 0;

  o_complex_world_translate_toplevel(w_current, x, y, object);

  object->complex->angle = angle;

#if DEBUG
  printf("setting final rotated angle to: %d\n\n", object->angle);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_complex_mirror_world(TOPLEVEL *w_current, int world_centerx, int world_centery,
		     OBJECT *object)
{
  int x, y;
  int newx, newy;
  int origx, origy;
  int change = 0;

  origx = object->complex->x;
  origy = object->complex->y;

  x = object->complex->x + (-world_centerx);
  y = object->complex->y + (-world_centery);

  newx = -x;
  newy = y;

  x = newx + (world_centerx);
  y = newy + (world_centery);

  o_complex_world_translate_toplevel(w_current,
                                     -object->complex->x, 
                                     -object->complex->y, object);

  o_complex_mirror_lowlevel(w_current, 0, 0, object);

  switch(object->complex->angle) {
    case(90):
      object->complex->angle = 270;
#if 0
      o_text_change_angle(w_current, object->complex->prim_objs,
                          object->complex->angle);
#endif

      change = 1;
      break;

    case(270):
      object->complex->angle = 90;
#if 0
      o_text_change_angle(w_current, object->complex->prim_objs
                          object->complex->angle);
#endif
      change = 1;
      break;

  }
#if 0
  object->complex->angle = (object->complex->angle + 180) % 360;
#endif

  object->complex->mirror = !object->complex->mirror;
#if 0
  object->complex->x = 0;
  object->complex->y = 0;
#endif

  o_complex_world_translate_toplevel(w_current, x, y, object);

#if DEBUG
  printf("final res %d %d\n", object->complex->x,  object->complex->y);
#endif
#if 0
  object->complex->x = x;
  object->complex->y = y;
#endif
  return(change);
}
