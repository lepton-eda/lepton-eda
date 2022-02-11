/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  g_return_if_fail (w_current->stretch_list == NULL);

  if (o_select_selected (w_current)) {
    i_set_state (w_current, MOVEMODE);

    gboolean net_rubber_band_mode;

    net_rubber_band_mode = gschem_options_get_net_rubber_band_mode (w_current->options);

    w_current->last_drawb_mode = LAST_DRAWB_MODE_NONE;

    w_current->first_wx = w_current->second_wx = w_x;
    w_current->first_wy = w_current->second_wy = w_y;

    o_invalidate_glist (w_current, lepton_list_get_glist (page->selection_list));

    if (net_rubber_band_mode) {
      o_move_prep_rubberband(w_current);

      /* Set the dont_redraw flag on rubberbanded objects and invalidate them.
       * This ensures that they are not drawn (in their un-stretched position)
       * during screen updates. */
      for (s_iter = w_current->stretch_list;
           s_iter != NULL; s_iter = g_list_next (s_iter)) {
        STRETCH *stretch = (STRETCH*) s_iter->data;
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
  LeptonObject *object;
  GList *iter;

  iter = list;
  while (iter != NULL) {
    object = (LeptonObject *)iter->data;
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
                         LeptonObject *object,
                         int diff_x, int diff_y)
{
  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  switch (lepton_object_get_type (object)) {

    case (OBJ_NET):
    case (OBJ_BUS):
    case (OBJ_PIN):
      s_conn_remove_object_connections (object);
      lepton_object_translate (object, diff_x, diff_y);
      s_conn_update_object (page, object);
      break;

    default:
      lepton_object_translate (object, diff_x, diff_y);
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

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  GList *s_current = NULL;
  LeptonObject *object;
  int diff_x, diff_y;
  GList *s_iter;
  GList *primitives = NULL;
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
    STRETCH *stretch = (STRETCH*) s_iter->data;
    stretch->object->dont_redraw = FALSE;
  }

  s_current = lepton_list_get_glist( page->selection_list );

  while (s_current != NULL) {

    object = (LeptonObject *) s_current->data;

    if (object == NULL) {
      fprintf (stderr, "o_move_end: ERROR: Got an unexpected NULL\n");
      exit(-1);
    }


    switch (lepton_object_get_type (object)) {
      case (OBJ_COMPONENT):

        /* TODO: Fix so we can just pass the component to o_move_end_lowlevel,
         * IE.. by falling through the bottom of this case statement. */
        lepton_component_object_set_x (object, lepton_component_object_get_x (object) + diff_x);
        lepton_component_object_set_y (object, lepton_component_object_get_y (object) + diff_y);

        primitives = lepton_component_object_get_contents (object);
        o_move_end_lowlevel_glist (w_current,
                                   primitives,
                                   diff_x,
                                   diff_y);
        break;

      default:
        o_move_end_lowlevel (w_current, object, diff_x, diff_y);
        break;
    }

    s_current = g_list_next(s_current);
  }

  /* Draw the objects that were moved */
  o_invalidate_glist (w_current, lepton_list_get_glist (page->selection_list));

  /* Draw the connected nets/buses that were also changed */
  o_invalidate_glist (w_current, rubbernet_objects);

  /* Call move-objects-hook for moved objects and changed connected
   * nets/buses */
  GList *moved_list = g_list_concat (page->place_list, rubbernet_objects);
  page->place_list = NULL;
  rubbernet_objects = NULL;
  g_run_hook_object_list (w_current, "move-objects-hook", moved_list);
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

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  /* Unset the dont_redraw flag on rubberbanded objects.
   * We set this above, in o_move_start(). */
  for (s_iter = w_current->stretch_list;
       s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *stretch = (STRETCH*) s_iter->data;
    stretch->object->dont_redraw = FALSE;
  }
  g_list_free(page->place_list);
  page->place_list = NULL;

  s_stretch_destroy_all (w_current->stretch_list);
  w_current->stretch_list = NULL;

  i_action_stop (w_current);
}


/*! \brief Handle motion during a move operation, resnapping if necessary
 * \par Function Description
 * Handle movement during a move operation, by updating the global
 * candidate transformation parameters.  The \a w_x and \b w_y
 * parameters are the incremental translation to be handled.
 *
 * This function mostly exists to implement the "resnapping" logic,
 * which destructively puts objects back onto the grid during a move
 * operation, as long as specific criteria are met.
 *
 * \param w_current  Global gschem state structure.
 * \param w_x        X-axis translation
 * \param w_y        Y-axis translation
 */
void o_move_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  GList *selection, *s_current;
  LeptonObject *object = NULL;
  gint object_x, object_y;
  SNAP_STATE snap_mode;

  g_assert (w_current->inside_action != 0);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);
  g_return_if_fail (page->place_list != NULL);

  snap_mode = gschem_options_get_snap_mode (w_current->options);

  selection = lepton_list_get_glist (page->selection_list);

  /* There are three posssibilities:
   *
   * 1. There is exactly one object selected, in which case it is
   *    snapped.
   *
   * 2. There are multiple objects selected, but there is some object
   *    O[i] such that all other selected objects O[j!=i] are attached
   *    as attributes of O[i].  In that case, the O[i] is snapped.
   *
   * 3. Other cases, in which case no snapping occurs.
   */

  if (NULL == selection || SNAP_RESNAP != snap_mode) {
    object = NULL;

  } else if (1 == g_list_length (selection)) {
    object = (LeptonObject *) selection->data;

  } else {

    /* The object that things are supposed to be attached to */
    LeptonObject *attached = NULL;

    /* Scan the selection, searching for an object that's not attached
     * to any other object.  As we go, also check whether everything
     * in the list that *is* attached as an attribute is attached to
     * the same object. */
    for (s_current = selection;
         NULL != s_current;
         s_current = g_list_next (s_current)) {

      LeptonObject *candidate = (LeptonObject *) s_current->data;

      if (NULL == lepton_object_get_attached_to (candidate))
      {
        /* If the object is *not* attached as an attribute, then check
         * whether we previously found an independent object.  If we
         * did, we can't do snapping, so give up. */
        if (NULL == object) {
          object = candidate;
        } else if (candidate != object) {
          break; /* Give up */
        }

      } else {

        /* If the object is attached as an attribute, then check if
         * it's attached as an attribute of the same object as
         * everything else is.  If not, we can't do snapping, so give
         * up. */
        if (NULL == attached)
        {
          attached = lepton_object_get_attached_to (candidate);
        }
        else if (attached != lepton_object_get_attached_to (candidate))
        {
          break; /* Give up */
        }
      }
    }

    if (NULL == object ||
        (NULL != attached && object != attached)) {
      object = NULL;
    } else {
      g_assert (NULL != object);
    }
  }

  /* manipulate w_x and w_y in a way that will lead to a position
     of the object that is aligned with the grid */
  if (NULL != object) {
    if (lepton_object_get_position (object, &object_x, &object_y))
    {
      w_x += snap_grid (w_current, object_x) - object_x;
      w_y += snap_grid (w_current, object_y) - object_y;
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
      STRETCH *s_current = (STRETCH*) s_iter->data;
      LeptonObject *object = s_current->object;

      switch (lepton_object_get_type (object)) {
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
    STRETCH *s_current = (STRETCH*) s_iter->data;
    LeptonObject *object = s_current->object;
    int whichone = s_current->whichone;

    /* We can only stretch nets and buses */
    switch (lepton_object_get_type (object)) {
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
int o_move_return_whichone(LeptonObject * object, int x, int y)
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
void o_move_check_endpoint(GschemToplevel *w_current, LeptonObject * object)
{
  GList *cl_current;
  LeptonConn *c_current;
  LeptonObject *other;
  int whichone;

  g_return_if_fail (object != NULL);
  g_return_if_fail (lepton_object_is_bus (object) ||
                    lepton_object_is_net (object) ||
                    lepton_object_is_pin (object));

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  for (cl_current = object->conn_list;
       cl_current != NULL;
       cl_current = g_list_next(cl_current)) {

    c_current = (LeptonConn *) cl_current->data;
    other = c_current->other_object;

    if (other == NULL)
      continue;

    /* really make sure that the object is not selected */
    if (other->selected)
      continue;

    LeptonObject *parent = lepton_object_get_parent (other);
    /* Catch pins, whos parent object is selected. */
    if (parent != NULL && parent->selected)
      continue;

    if (c_current->type != CONN_ENDPOINT &&
        (c_current->type != CONN_MIDPOINT ||
         c_current->other_whichone == -1))
      continue;

    if (/* (net)pin to (net)pin contact */
        (lepton_object_is_pin (object) && object->pin_type == PIN_TYPE_NET &&
         lepton_object_is_pin (other) && other->pin_type  == PIN_TYPE_NET)) {

     /* net to (net)pin contact */
     /* (lepton_object_is_net (object) &&
         lepton_object_is_pin (other) && other->pin_type == PIN_TYPE_NET) */

      LeptonObject *new_net;
      /* other object is a pin, insert a net */
      new_net = lepton_net_object_new (NET_COLOR,
                                       c_current->x,
                                       c_current->y,
                                       c_current->x,
                                       c_current->y);
      lepton_page_append (page, new_net);
      /* This new net object is only picked up for stretching later,
       * somewhat of a kludge. If the move operation is cancelled, these
       * new 0 length nets are removed by the "undo" operation invoked.
       */
    }

    /* Only attempt to stretch nets and buses */
    if (!lepton_object_is_net (other) && !lepton_object_is_bus (other))
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
  LeptonObject *object;
  LeptonObject *o_current;
  GList *iter;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  for (s_current = lepton_list_get_glist (page->selection_list);
       s_current != NULL; s_current = g_list_next (s_current)) {
    object = (LeptonObject*) s_current->data;

    if (object == NULL)
      continue;

    switch (lepton_object_get_type (object)) {
      case (OBJ_NET):
      case (OBJ_PIN):
      case (OBJ_BUS):
        o_move_check_endpoint (w_current, object);
        break;

      case (OBJ_COMPONENT):
        for (iter = lepton_component_object_get_contents (object);
             iter != NULL; iter = g_list_next (iter)) {
          o_current = (LeptonObject*) iter->data;

          if (lepton_object_is_pin (o_current))
          {
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
int o_move_zero_length(LeptonObject * object)
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

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  for (s_iter = w_current->stretch_list;
       s_iter != NULL; s_iter = s_iter_next) {
    STRETCH *s_current = (STRETCH*) s_iter->data;
    LeptonObject *object = s_current->object;
    int whichone = s_current->whichone;

    /* Store this now, since we may delete the current item */
    s_iter_next = g_list_next (s_iter);

    if (lepton_object_is_net (object) ||
        lepton_object_is_bus (object))
    {
      /* remove the object's connections */
      s_conn_remove_object_connections (object);

      object->line->x[whichone] += w_dx;
      object->line->y[whichone] += w_dy;

      if (o_move_zero_length (object)) {
        w_current->stretch_list =
          s_stretch_remove (w_current->stretch_list, object);
        o_delete (w_current, object);
        continue;
      }

      s_conn_update_object (page, object);
      *objects = g_list_append (*objects, object);
    }
  }
}
