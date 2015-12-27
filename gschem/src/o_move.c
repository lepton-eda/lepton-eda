/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#include <stdio.h>

#include "gschem.h"

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_start(GschemToplevel *w_current, int w_x, int w_y)
{
  GList *s_iter;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  g_return_if_fail (w_current->stretch_list == NULL);

  if (o_select_selected (w_current)) {
    i_set_state (w_current, MOVEMODE);

    gboolean net_rubber_band_mode;

    net_rubber_band_mode = gschem_options_get_net_rubber_band_mode (w_current->options);

    w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

    w_current->first_wx = w_current->second_wx = w_x;
    w_current->first_wy = w_current->second_wy = w_y;

    o_invalidate_glist (w_current, geda_list_get_glist (page->selection_list));

    if (net_rubber_band_mode) {
      o_move_prep_rubberband(w_current);

      /* Set the dont_redraw flag on rubberbanded objects and invalidate them.
       * This ensures that they are not drawn (in their un-stretched position)
       * during screen updates. */
      for (s_iter = w_current->stretch_list;
           s_iter != NULL; s_iter = g_list_next (s_iter)) {
        STRETCH *stretch = s_iter->data;
        stretch->object->dont_redraw = TRUE;
        o_invalidate (w_current, stretch->object);
      }
    }

    o_select_move_to_place_list(w_current);
    i_action_start (w_current);

    o_move_invalidate_rubber (w_current, TRUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_move_end_lowlevel_glist (GschemToplevel *w_current,
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
void o_move_end_lowlevel (GschemToplevel *w_current,
                         OBJECT *object,
                         int diff_x, int diff_y)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  switch (object->type) {

    case (OBJ_NET):
    case (OBJ_BUS):
    case (OBJ_PIN):
      s_conn_remove_object_connections (page->toplevel, object);
      geda_object_translate (object, diff_x, diff_y);
      s_conn_update_object (page, object);
      break;

    default:
      geda_object_translate (object, diff_x, diff_y);
      break;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_end(GschemToplevel *w_current)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  GList *s_current = NULL;
  OBJECT *object;
  int diff_x, diff_y;
  GList *s_iter;
  GList *rubbernet_objects = NULL;
  gboolean net_rubber_band_mode;

  g_return_if_fail (w_current != NULL);
  g_return_if_fail (page != NULL);

  g_assert (w_current->inside_action != 0);

  object = o_select_return_first_object(w_current);

  if (!object) {
    /* actually this is an error condition hack */
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
    return;
  }

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  o_move_invalidate_rubber (w_current, FALSE);
  w_current->rubber_visible = 0;

  net_rubber_band_mode = gschem_options_get_net_rubber_band_mode (w_current->options);

  if (net_rubber_band_mode) {
    o_move_end_rubberband (w_current, diff_x, diff_y, &rubbernet_objects);
  }

  /* Unset the dont_redraw flag on rubberbanded objects.
   * We set this above, in o_move_start(). */
  for (s_iter = w_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *stretch = s_iter->data;
    stretch->object->dont_redraw = FALSE;
  }

  s_current = geda_list_get_glist( page->selection_list );

  while (s_current != NULL) {

    object = (OBJECT *) s_current->data;

    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object in o_move_end!\n"));
      exit(-1);
    }


    switch (object->type) {
      case (OBJ_COMPLEX):
      case (OBJ_PLACEHOLDER):

        /* TODO: Fix so we can just pass the complex to o_move_end_lowlevel,
         * IE.. by falling through the bottom of this case statement. */

        /* this next section of code is from */
        /* o_complex_world_translate_world */
        object->complex->x = object->complex->x + diff_x;
        object->complex->y = object->complex->y + diff_y;

        o_move_end_lowlevel_glist (w_current, object->complex->prim_objs,
                                   diff_x, diff_y);
        object->w_bounds_valid_for = NULL;
        break;

      default:
        o_move_end_lowlevel (w_current, object, diff_x, diff_y);
        break;
    }

    s_current = g_list_next(s_current);
  }

  /* Draw the objects that were moved */
  o_invalidate_glist (w_current, geda_list_get_glist (page->selection_list));

  /* Draw the connected nets/buses that were also changed */
  o_invalidate_glist (w_current, rubbernet_objects);

  /* Call move-objects-hook for moved objects and changed connected
   * nets/buses */
  GList *moved_list = g_list_concat (page->place_list, rubbernet_objects);
  page->place_list = NULL;
  rubbernet_objects = NULL;
  g_run_hook_object_list (w_current, "%move-objects-hook", moved_list);
  g_list_free (moved_list);

  gschem_toplevel_page_content_changed (w_current, page);
  o_undo_savestate_old(w_current, UNDO_ALL);

  s_stretch_destroy_all (w_current->stretch_list);
  w_current->stretch_list = NULL;

  i_set_state(w_current, SELECT);
  i_action_stop (w_current);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_cancel (GschemToplevel *w_current)
{
  GList *s_iter;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  /* Unset the dont_redraw flag on rubberbanded objects.
   * We set this above, in o_move_start(). */
  for (s_iter = w_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *stretch = s_iter->data;
    stretch->object->dont_redraw = FALSE;
  }
  g_list_free(page->place_list);
  page->place_list = NULL;

  s_stretch_destroy_all (w_current->stretch_list);
  w_current->stretch_list = NULL;

  i_action_stop (w_current);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  GList *selection, *s_current;
  OBJECT *object;
  gint object_x, object_y;
  gboolean resnap = FALSE;
  SNAP_STATE snap_mode;

  g_assert (w_current->inside_action != 0);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);
  g_return_if_fail (page->place_list != NULL);

  snap_mode = gschem_options_get_snap_mode (w_current->options);

  selection = geda_list_get_glist (page->selection_list);

  /* realign the object if we are in resnap mode */
  if ((selection != NULL) && (snap_mode == SNAP_RESNAP)) {
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
            && !o_attrib_is_attached(page->toplevel,
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
      if (o_get_position(page->toplevel, &object_x, &object_y, object)) {
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
void o_move_invalidate_rubber (GschemToplevel *w_current, int drawing)
{
  GList *s_iter;
  int dx1, dx2, dy1, dy2;
  gboolean net_rubber_band_mode;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  net_rubber_band_mode = gschem_options_get_net_rubber_band_mode (w_current->options);

  o_place_invalidate_rubber (w_current, drawing);
  if (net_rubber_band_mode) {

    for (s_iter = w_current->stretch_list;
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

          gschem_page_view_invalidate_world_rect (page_view,
                                                  object->line->x[0] + dx1,
                                                  object->line->y[0] + dy1,
                                                  object->line->x[1] + dx2,
                                                  object->line->y[1] + dy2);
      }
    }
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
o_move_draw_rubber (GschemToplevel *w_current,
                    EdaRenderer *renderer)
{
  GList *s_iter;
  int diff_x, diff_y;
  gboolean net_rubber_band_mode;

  g_return_if_fail (w_current != NULL);

  o_place_draw_rubber (w_current, renderer);

  net_rubber_band_mode = gschem_options_get_net_rubber_band_mode (w_current->options);

  if (!net_rubber_band_mode)
    return;

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  for (s_iter = w_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *s_current = s_iter->data;
    OBJECT *object = s_current->object;
    int whichone = s_current->whichone;

    /* We can only stretch nets and buses */
    switch (object->type) {
      case OBJ_NET:
      case OBJ_BUS:
        break;
    default:
      continue;
    }

    g_return_if_fail ((whichone >= 0) && (whichone < 2));

    /* Apply stretch */
    object->line->x[whichone] += diff_x;
    object->line->y[whichone] += diff_y;

    /* Draw stretched object */
    eda_renderer_draw (renderer, object);

    /* Restore original geometry */
    object->line->x[whichone] -= diff_x;
    object->line->y[whichone] -= diff_y;
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
void o_move_check_endpoint(GschemToplevel *w_current, OBJECT * object)
{
  GList *cl_current;
  CONN *c_current;
  OBJECT *other;
  int whichone;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

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
      new_net = o_net_new (page->toplevel, OBJ_NET, NET_COLOR,
                           c_current->x, c_current->y,
                           c_current->x, c_current->y);
      s_page_append (page->toplevel, page, new_net);
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
      w_current->stretch_list = s_stretch_add (w_current->stretch_list,
                                               other, whichone);
    }
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_move_prep_rubberband(GschemToplevel *w_current)
{
  GList *s_current;
  OBJECT *object;
  OBJECT *o_current;
  GList *iter;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  for (s_current = geda_list_get_glist (page->selection_list);
       s_current != NULL; s_current = g_list_next (s_current)) {
    object = s_current->data;

    if (object == NULL)
      continue;

    switch (object->type) {
      case (OBJ_NET):
      case (OBJ_PIN):
      case (OBJ_BUS):
        o_move_check_endpoint (w_current, object);
        break;

      case (OBJ_COMPLEX):
      case (OBJ_PLACEHOLDER):
        for (iter = object->complex->prim_objs;
             iter != NULL; iter = g_list_next (iter)) {
          o_current = iter->data;

          if (o_current->type == OBJ_PIN) {
            o_move_check_endpoint (w_current, o_current);
          }
        }
        break;
    }
  }
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
void o_move_end_rubberband (GschemToplevel *w_current,
                            int w_dx, int w_dy,
                            GList** objects)
{
  GList *s_iter, *s_iter_next;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  PAGE *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  for (s_iter = w_current->stretch_list;
       s_iter != NULL; s_iter = s_iter_next) {
    STRETCH *s_current = s_iter->data;
    OBJECT *object = s_current->object;
    int whichone = s_current->whichone;

    /* Store this now, since we may delete the current item */
    s_iter_next = g_list_next (s_iter);

    if (object->type == OBJ_NET ||
        object->type == OBJ_BUS) {

      /* remove the object's connections */
      s_conn_remove_object_connections (page->toplevel, object);

      object->line->x[whichone] += w_dx;
      object->line->y[whichone] += w_dy;

      if (o_move_zero_length (object)) {
        w_current->stretch_list =
          s_stretch_remove (w_current->stretch_list, object);
        o_delete (w_current, object);
        continue;
      }

      object->w_bounds_valid_for = NULL;
      s_conn_update_object (page, object);
      *objects = g_list_append (*objects, object);
    }
  }
}
