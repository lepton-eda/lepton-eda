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

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_iter;

  if (o_select_selected (w_current)) {

    /* Save the current state. When rotating the selection when moving,
       we have to come back to here */
    o_undo_savestate(w_current, UNDO_ALL);
    w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;
    w_current->event_state = MOVE;

    w_current->first_wx = w_current->second_wx = w_x;
    w_current->first_wy = w_current->second_wy = w_y;

    o_invalidate_glist (w_current,
       geda_list_get_glist (toplevel->page_current->selection_list));

    if (w_current->netconn_rubberband) {
      o_move_prep_rubberband(w_current);

      /* Set the dont_redraw flag on rubberbanded objects and invalidate them.
       * This ensures that they are not drawn (in their un-stretched position)
       * during screen updates. */
      for (s_iter = toplevel->page_current->stretch_list;
           s_iter != NULL; s_iter = g_list_next (s_iter)) {
        STRETCH *stretch = s_iter->data;
        stretch->object->dont_redraw = TRUE;
        o_invalidate (w_current, stretch->object);
      }
    }

    o_select_move_to_place_list(w_current);
    w_current->inside_action = 1;

    o_move_rubbermove_xor (w_current, TRUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_move_end_lowlevel_glist (GSCHEM_TOPLEVEL *w_current,
                                       GList *list,
                                       int diff_x, int diff_y,
                                       GList** prev_conn_objects,
                                       GList** connected_objects)
{
  OBJECT *object;
  GList *iter;

  iter = list;
  while (iter != NULL) {
    object = (OBJECT *)iter->data;
    o_move_end_lowlevel (w_current, object, diff_x, diff_y,
                         prev_conn_objects, connected_objects);
    iter = g_list_next (iter);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_end_lowlevel (GSCHEM_TOPLEVEL *w_current,
                         OBJECT *object,
                         int diff_x, int diff_y,
                         GList** prev_conn_objects,
                         GList** connected_objects)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  switch (object->type) {

    case (OBJ_NET):
    case (OBJ_BUS):
    case (OBJ_PIN):
      /* save the other objects and remove object's connections */
      *prev_conn_objects = s_conn_return_others (*prev_conn_objects, object);
      s_conn_remove_object (toplevel, object);

      /* do the actual translation */
      o_translate_world (toplevel, diff_x, diff_y, object);
      s_conn_update_object (toplevel, object);
      *connected_objects = s_conn_return_others (*connected_objects, object);
      break;

    default:
      o_translate_world (toplevel, diff_x, diff_y, object);
      break;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_end(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_current = NULL;
  OBJECT *object;
  int diff_x, diff_y;
  int left, top, right, bottom;
  GList *s_iter;
  GList *prev_conn_objects = NULL;
  GList *connected_objects = NULL;
  GList *rubbernet_objects = NULL; 
  GList *rubbernet_prev_conn_objects = NULL;
  GList *rubbernet_connected_objects = NULL;

  object = o_select_return_first_object(w_current);

  if (!object) {
    /* actually this is an error condition hack */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  o_move_rubbermove_xor (w_current, FALSE);
  w_current->rubber_visible = 0;

  if (w_current->netconn_rubberband) {
    o_move_end_rubberband(w_current, diff_x, diff_y,
                          &rubbernet_objects, &rubbernet_prev_conn_objects,
                          &rubbernet_connected_objects);
  }

  /* Unset the dont_redraw flag on rubberbanded objects.
   * We set this above, in o_move_start(). */
  for (s_iter = toplevel->page_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *stretch = s_iter->data;
    stretch->object->dont_redraw = FALSE;
  }

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );

  while (s_current != NULL) {

    object = (OBJECT *) s_current->data;

    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object in o_move_end!\n"));
      exit(-1);
    }


    switch (object->type) {
      case (OBJ_COMPLEX):
      case (OBJ_PLACEHOLDER):

        if (scm_hook_empty_p(move_component_hook) == SCM_BOOL_F &&
            object != NULL) {
          scm_run_hook(move_component_hook,
                       scm_cons (g_make_attrib_smob_list
                                 (w_current, object), SCM_EOL));
        }

        /* TODO: Fix so we can just pass the complex to o_move_end_lowlevel,
         * IE.. by falling through the bottom of this case statement. */

        /* this next section of code is from */
        /* o_complex_world_translate_world */
        object->complex->x = object->complex->x + diff_x;
        object->complex->y = object->complex->y + diff_y;

        o_move_end_lowlevel_glist (w_current, object->complex->prim_objs,
                                   diff_x, diff_y,
                                   &prev_conn_objects, &connected_objects);


        world_get_object_glist_bounds (toplevel, object->complex->prim_objs,
                                       &left, &top, &right, &bottom);

        object->w_left = left;
        object->w_top = top;
        object->w_right = right;
        object->w_bottom = bottom;

        break;

      default:
        o_move_end_lowlevel (w_current, object, diff_x, diff_y,
                            &prev_conn_objects, &connected_objects);
        break;
    }

    s_current = g_list_next(s_current);
  }

  /* Remove the undo saved in o_move_start */
  o_undo_remove_last_undo(w_current);

  /* Draw the objects that were moved (and connected/disconnected objects) */
  o_invalidate_glist (w_current,
    geda_list_get_glist (toplevel->page_current->selection_list));
  o_cue_invalidate_glist (w_current, prev_conn_objects);
  o_cue_invalidate_glist (w_current, connected_objects);

  /* Draw the connected nets/buses that were also changed */
  o_invalidate_glist (w_current, rubbernet_objects);
  o_cue_invalidate_glist (w_current, rubbernet_objects);
  o_cue_invalidate_glist (w_current, rubbernet_prev_conn_objects);
  o_cue_invalidate_glist (w_current, rubbernet_connected_objects);
 
  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);

  g_list_free(prev_conn_objects);
  g_list_free(connected_objects);
  g_list_free(rubbernet_objects);
  g_list_free(rubbernet_prev_conn_objects);
  g_list_free(rubbernet_connected_objects);

  g_list_free(toplevel->page_current->place_list);
  toplevel->page_current->place_list = NULL;

  s_stretch_destroy_all (toplevel->page_current->stretch_list);
  toplevel->page_current->stretch_list = NULL;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_cancel (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_iter;

  /* Unset the dont_redraw flag on rubberbanded objects.
   * We set this above, in o_move_start(). */
  for (s_iter = toplevel->page_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *stretch = s_iter->data;
    stretch->object->dont_redraw = FALSE;
  }
  g_list_free(w_current->toplevel->page_current->place_list);
  w_current->toplevel->page_current->place_list = NULL;

  s_stretch_destroy_all (toplevel->page_current->stretch_list);
  toplevel->page_current->stretch_list = NULL;

  w_current->inside_action = 0;
  i_set_state (w_current, SELECT);

  o_undo_callback(w_current, UNDO_ACTION);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  o_move_rubbermove_xor (w_current, FALSE);
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  o_move_rubbermove_xor (w_current, TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_rubbermove_xor (GSCHEM_TOPLEVEL *w_current, int drawing)
{
  o_place_rubberplace_xor (w_current, drawing);
  if (w_current->netconn_rubberband)
    o_move_stretch_rubberband(w_current);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_move_return_whichone(OBJECT * object, int x, int y)
{
  if (object->line->x[0] == x && object->line->y[0] == y) {
    return (0);
  }

  if (object->line->x[1] == x && object->line->y[1] == y) {
    return (1);
  }

  fprintf(stderr,
          _("DOH! tried to find the whichone, but didn't find it!\n"));
  return (-1);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_check_endpoint(GSCHEM_TOPLEVEL *w_current, OBJECT * object)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *cl_current;
  CONN *c_current;
  int whichone;

  if (!object)
  return;

  if (object->type != OBJ_NET && object->type != OBJ_PIN &&
      object->type != OBJ_BUS) {
    fprintf(stderr, _("Got a non line object in o_move_check_endpoint\n"));
    return;
  }

  for (cl_current = object->conn_list;
       cl_current != NULL;
       cl_current = g_list_next(cl_current)) {

    c_current = (CONN *) cl_current->data;

    if (c_current->other_object == NULL)
      continue;

    /* really make sure that the object is not selected */
    if (c_current->other_object->saved_color != -1 ||
        c_current->other_object->selected == TRUE)
      continue;

    if (c_current->type != CONN_ENDPOINT &&
        (c_current->type != CONN_MIDPOINT ||
         c_current->other_whichone == -1))
      continue;

    /* Only attempt to stretch nets and buses */
    if (c_current->other_object->type != OBJ_NET &&
        c_current->other_object->type != OBJ_BUS)
      continue;

    whichone = o_move_return_whichone(c_current->other_object,
                                      c_current->x,
                                      c_current->y);

#if DEBUG
    printf ("FOUND: %s type: %d, whichone: %d, x,y: %d %d\n",
            c_current->other_object->name, c_current->type,
            whichone, c_current->x, c_current->y);

    printf("other x,y: %d %d\n", c_current->x, c_current->y);
    printf("type: %d return: %d real: [ %d %d ]\n",
           c_current->type, whichone, c_current->whichone,
           c_current->other_whichone);
#endif

    if (whichone >= 0 && whichone <= 1) {
      toplevel->page_current->stretch_list =
        s_stretch_add (toplevel->page_current->stretch_list,
                       c_current->other_object,
                       c_current, whichone);
    }
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_prep_rubberband(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_current;
  OBJECT *object;
  OBJECT *o_current;
  GList *iter;

#if DEBUG
  printf("\n\n\n");
  s_stretch_print_all (toplevel->page_current->stretch_list);
  printf("\n\n\n");
#endif

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  while (s_current != NULL) {
    object = (OBJECT *) s_current->data;
    if (object) {
      switch (object->type) {
        case (OBJ_NET):
        case (OBJ_PIN):
        case (OBJ_BUS):
          o_move_check_endpoint(w_current, object);
          break;

        case (OBJ_COMPLEX):
        case (OBJ_PLACEHOLDER):
          iter = object->complex->prim_objs;
          while (iter != NULL) {
            o_current = (OBJECT *)iter->data;

            if (o_current->type == OBJ_PIN) {
              o_move_check_endpoint(w_current, o_current);
            }

            iter = g_list_next (iter);
          }

          break;

      }
    }
    s_current = g_list_next(s_current);
  }

#if DEBUG
  printf("\n\n\n\nfinished building scretch list:\n");
  s_stretch_print_all (toplevel->page_current->stretch_list);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_move_zero_length(OBJECT * object)
{
#if DEBUG
  printf("x: %d %d y: %d %d\n",
         object->line->x[0], object->line->x[1],
         object->line->y[0], object->line->y[1]);
#endif

  if (object->line->x[0] == object->line->x[1] &&
      object->line->y[0] == object->line->y[1]) {
    return TRUE;
  } else {
    return FALSE;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_end_rubberband (GSCHEM_TOPLEVEL *w_current,
                            int w_dx, int w_dy,
                            GList** objects,
                            GList** prev_conn_objects,
                            GList** connected_objects)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_iter, *s_iter_next;
  GList *iter;

  /* save a list of objects the stretched objects
     are connected to before we move them. */
  for (s_iter = toplevel->page_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    OBJECT *object = ((STRETCH *)s_iter->data)->object;

    if (object->type == OBJ_NET ||
        object->type == OBJ_BUS) {
      *prev_conn_objects = s_conn_return_others (*prev_conn_objects, object);
    }
  }

  for (s_iter = toplevel->page_current->stretch_list;
       s_iter != NULL; s_iter = s_iter_next) {
    STRETCH *s_current = s_iter->data;
    OBJECT *object = s_current->object;
    int whichone = s_current->whichone;

    /* Store this now, since we may delete the current item */
    s_iter_next = g_list_next (s_iter);

    if (object->type == OBJ_NET ||
        object->type == OBJ_BUS) {

      /* remove the object's connections */
      s_conn_remove_object (toplevel, object);

      object->line->x[whichone] += w_dx;
      object->line->y[whichone] += w_dy;

      if (o_move_zero_length (object)) {
        toplevel->page_current->stretch_list =
          s_stretch_remove (toplevel->page_current->stretch_list, object);
        *prev_conn_objects = g_list_remove_all (*prev_conn_objects, object);
        o_delete (w_current, object);
        continue;
      }

      o_recalc_single_object (toplevel, object);
      s_tile_update_object (toplevel, object);
      s_conn_update_object (toplevel, object);
      *objects = g_list_append (*objects, object);
    }
  }

  /* save a list of objects the stretched objects
     are now connected to after we moved them. */
  for (iter = *objects; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *object = iter->data;
    *connected_objects = s_conn_return_others (*connected_objects, object);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_stretch_rubberband(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_iter;
  int diff_x, diff_y;

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  for (s_iter = toplevel->page_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *s_current = s_iter->data;
    OBJECT *object = s_current->object;
    int whichone = s_current->whichone;

    switch (object->type) {
      case (OBJ_NET):
        o_net_draw_xor_single(w_current, diff_x, diff_y, whichone, object);
        break;

      case (OBJ_BUS):
        o_bus_draw_xor_single(w_current, diff_x, diff_y, whichone, object);
        break;
    }
  }
}
