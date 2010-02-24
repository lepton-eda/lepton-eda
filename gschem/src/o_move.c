/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

    o_move_invalidate_rubber (w_current, TRUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_move_end_lowlevel_glist (GSCHEM_TOPLEVEL *w_current,
                                       GList *list,
                                       int diff_x, int diff_y)
{
  OBJECT *object;
  GList *iter;

  iter = list;
  while (iter != NULL) {
    object = (OBJECT *)iter->data;
    o_move_end_lowlevel (w_current, object, diff_x, diff_y);
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
                         int diff_x, int diff_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  switch (object->type) {

    case (OBJ_NET):
    case (OBJ_BUS):
    case (OBJ_PIN):
      s_conn_remove_object (toplevel, object);
      o_translate_world (toplevel, diff_x, diff_y, object);
      s_conn_update_object (toplevel, object);
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
  GList *rubbernet_objects = NULL; 

  object = o_select_return_first_object(w_current);

  if (!object) {
    /* actually this is an error condition hack */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  o_move_invalidate_rubber (w_current, FALSE);
  w_current->rubber_visible = 0;

  if (w_current->netconn_rubberband) {
    o_move_end_rubberband (w_current, diff_x, diff_y, &rubbernet_objects);
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
                                   diff_x, diff_y);


        world_get_object_glist_bounds (toplevel, object->complex->prim_objs,
                                       &left, &top, &right, &bottom);

        object->w_left = left;
        object->w_top = top;
        object->w_right = right;
        object->w_bottom = bottom;

        break;

      default:
        o_move_end_lowlevel (w_current, object, diff_x, diff_y);
        break;
    }

    s_current = g_list_next(s_current);
  }

  /* Remove the undo saved in o_move_start */
  o_undo_remove_last_undo(w_current);

  /* Draw the objects that were moved */
  o_invalidate_glist (w_current,
    geda_list_get_glist (toplevel->page_current->selection_list));

  /* Draw the connected nets/buses that were also changed */
  o_invalidate_glist (w_current, rubbernet_objects);

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);

  g_list_free(rubbernet_objects);

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
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *selection, *s_current;
  OBJECT *object;
  gint object_x, object_y;
  gboolean resnap = FALSE;

  selection = geda_list_get_glist( toplevel->page_current->selection_list );

  /* realign the object if we are in resnap mode */
  if (selection != NULL
      && w_current->snap == SNAP_RESNAP) {

    if (g_list_length(selection) > 1) {
      /* find an object that is not attached to any other object */
      for (s_current = selection;
           s_current != NULL;
           s_current = g_list_next(s_current)) {
        if (((OBJECT *) s_current->data)->attached_to == NULL) {
          object = (OBJECT *) s_current->data;
          resnap = TRUE;
          break;
        }
      }

      /* Only resnap single elements. This is also the case if
         the selection contains one object and all other object
         elements are attributes of the object element.*/
      for (s_current = selection;
           s_current != NULL && resnap == TRUE;
           s_current = g_list_next(s_current)) {
        if (!(object == (OBJECT *) s_current->data)
            && !o_attrib_is_attached(toplevel,
                                     (OBJECT *) s_current->data, object)) {
          resnap = FALSE;
        }
      }
    } else { /* single object */
      resnap = TRUE;
      object = (OBJECT *) selection->data;
    }

    /* manipulate w_x and w_y in a way that will lead to a position
       of the object that is aligned with the grid */
    if (resnap) {
      if (o_get_position(toplevel, &object_x, &object_y, object)) {
        w_x += snap_grid (w_current, object_x) - object_x;
        w_y += snap_grid (w_current, object_y) - object_y;
      }
    }
  }

  o_move_invalidate_rubber (w_current, FALSE);
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  o_move_invalidate_rubber (w_current, TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_invalidate_rubber (GSCHEM_TOPLEVEL *w_current, int drawing)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_iter;
  int dx1, dx2, dy1, dy2;
  int x1, y1, x2, y2;

  o_place_invalidate_rubber (w_current, drawing);
  if (w_current->netconn_rubberband) {

    for (s_iter = toplevel->page_current->stretch_list;
         s_iter != NULL; s_iter = g_list_next (s_iter)) {
      STRETCH *s_current = s_iter->data;
      OBJECT *object = s_current->object;

      switch (object->type) {
        case (OBJ_NET):
        case (OBJ_BUS):
          if (s_current->whichone == 0) {
            dx1 = w_current->second_wx - w_current->first_wx;
            dy1 = w_current->second_wy - w_current->first_wy;
            dx2 = dy2 = 0;
          } else {
            dx1 = dy1 = 0;
            dx2 = w_current->second_wx - w_current->first_wx;
            dy2 = w_current->second_wy - w_current->first_wy;
          }

          WORLDtoSCREEN (w_current, object->line->x[0] + dx1,
                                    object->line->y[0] + dy1, &x1, &y1);
          WORLDtoSCREEN (w_current, object->line->x[1] + dx2,
                                    object->line->y[1] + dy2, &x2, &y2);

          o_invalidate_rect (w_current, x1, y1, x2, y2);
      }
    }
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_draw_rubber (GSCHEM_TOPLEVEL *w_current, int drawing)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_iter;
  int diff_x, diff_y;

  o_place_draw_rubber (w_current, drawing);

  if (!w_current->netconn_rubberband)
    return;

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  for (s_iter = toplevel->page_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *s_current = s_iter->data;
    OBJECT *object = s_current->object;
    int whichone = s_current->whichone;

    switch (object->type) {
      case (OBJ_NET):
        o_net_draw_stretch (w_current, diff_x, diff_y, whichone, object);
        break;

      case (OBJ_BUS):
        o_bus_draw_stretch (w_current, diff_x, diff_y, whichone, object);
        break;
    }
  }
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
  OBJECT *other;
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
    other = c_current->other_object;

    if (other == NULL)
      continue;

    /* really make sure that the object is not selected */
    if (other->selected)
      continue;

    /* Catch pins, whos parent object is selected. */
    if (other->parent != NULL && other->parent->selected)
      continue;

    if (c_current->type != CONN_ENDPOINT &&
        (c_current->type != CONN_MIDPOINT ||
         c_current->other_whichone == -1))
      continue;

    if (/* (net)pin to (net)pin contact */
        (object->type == OBJ_PIN && object->pin_type == PIN_TYPE_NET &&
          other->type == OBJ_PIN &&  other->pin_type == PIN_TYPE_NET)) {

     /* net to (net)pin contact */
     /* (object->type == OBJ_NET &&
          other->type == OBJ_PIN && other->pin_type == PIN_TYPE_NET) */

      OBJECT *new_net;
      /* other object is a pin, insert a net */
      new_net = o_net_new (toplevel, OBJ_NET, NET_COLOR,
                           c_current->x, c_current->y,
                           c_current->x, c_current->y);
      s_page_append (toplevel, toplevel->page_current, new_net);
      /* This new net object is only picked up for stretching later,
       * somewhat of a kludge. If the move operation is cancelled, these
       * new 0 length nets are removed by the "undo" operation invoked.
       */
    }

    /* Only attempt to stretch nets and buses */
    if (other->type != OBJ_NET && other->type != OBJ_BUS)
      continue;

    whichone = o_move_return_whichone (other, c_current->x, c_current->y);

#if DEBUG
    printf ("FOUND: %s type: %d, whichone: %d, x,y: %d %d\n",
            other->name, c_current->type,
            whichone, c_current->x, c_current->y);

    printf("other x,y: %d %d\n", c_current->x, c_current->y);
    printf("type: %d return: %d real: [ %d %d ]\n",
           c_current->type, whichone, c_current->whichone,
           c_current->other_whichone);
#endif

    if (whichone >= 0 && whichone <= 1) {
      toplevel->page_current->stretch_list =
        s_stretch_add (toplevel->page_current->stretch_list,
                       other, c_current, whichone);
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
                            GList** objects)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_iter, *s_iter_next;

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
        o_delete (w_current, object);
        continue;
      }

      o_recalc_single_object (toplevel, object);
      s_tile_update_object (toplevel, object);
      s_conn_update_object (toplevel, object);
      *objects = g_list_append (*objects, object);
    }
  }
}
