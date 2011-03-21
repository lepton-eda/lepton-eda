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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>
#include <stdio.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define INVALIDATE_MARGIN 1

/*! \todo Lots of Gross code... needs lots of cleanup - mainly
 * readability issues
 */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_redraw_rects (GSCHEM_TOPLEVEL *w_current,
                     GdkRectangle *rectangles, int n_rectangles)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  gboolean draw_selected;
  int grip_half_size;
  int cue_half_size;
  int bloat;
  int i;
  GList *obj_list;
  GList *iter;
  BOX *world_rects;

  for (i = 0; i < n_rectangles; i++) {
    x_repaint_background_region (w_current, rectangles[i].x, rectangles[i].y,
                                 rectangles[i].width, rectangles[i].height);
  }

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (toplevel->page_current != NULL);

  grip_half_size = o_grips_size (w_current);
  cue_half_size = SCREENabs (w_current, CUE_BOX_SIZE);
  bloat = MAX (grip_half_size, cue_half_size);

  world_rects = g_new (BOX, n_rectangles);

  for (i = 0; i < n_rectangles; i++) {
    int x, y, width, height;

    x = rectangles[i].x;
    y = rectangles[i].y;
    width = rectangles[i].width;
    height = rectangles[i].height;

    SCREENtoWORLD (w_current, x - bloat, y + height + bloat,
                   &world_rects[i].lower_x, &world_rects[i].lower_y);
    SCREENtoWORLD (w_current, x + width + bloat, y - bloat,
                   &world_rects[i].upper_x, &world_rects[i].upper_y);
  }

  obj_list = s_page_objects_in_regions (toplevel, toplevel->page_current,
                                        world_rects, n_rectangles);
  g_free (world_rects);

  draw_selected = !(w_current->inside_action &&
                    ((w_current->event_state == MOVE) ||
                     (w_current->event_state == ENDMOVE)));

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->dont_redraw ||
        (!draw_selected && o_current->selected))
      continue;

    o_redraw_single (w_current, o_current);
  }

  o_cue_redraw_all (w_current, obj_list, draw_selected);

  if (w_current->inside_action) {
    /* Redraw the rubberband objects (if they were previously visible) */
    switch (w_current->event_state) {
      case MOVE:
      case ENDMOVE:
        if (w_current->last_drawb_mode != -1) {
          o_move_draw_rubber (w_current, TRUE);
        }
        break;

      case ENDCOPY:
      case ENDMCOPY:
      case ENDCOMP:
      case ENDTEXT:
      case ENDPASTE:
        if (w_current->rubber_visible)
          o_place_draw_rubber (w_current, TRUE);
        break;

      case STARTDRAWNET:
      case DRAWNET:
      case NETCONT:
        if (w_current->rubber_visible)
          o_net_draw_rubber (w_current);
        break;

      case STARTDRAWBUS:
      case DRAWBUS:
      case BUSCONT:
        if (w_current->rubber_visible)
          o_bus_draw_rubber(w_current);
        break;

      case GRIPS:
        if (w_current->rubber_visible)
          o_grips_draw_rubber (w_current);
        break;

      case SBOX:
        if (w_current->rubber_visible)
          o_select_box_draw_rubber (w_current);
        break;

      case ZOOMBOXEND:
        if (w_current->rubber_visible)
          a_zoom_box_draw_rubber (w_current);
        break;

      case ENDLINE:
        if (w_current->rubber_visible)
          o_line_draw_rubber (w_current);
        break;

      case ENDBOX:
        if (w_current->rubber_visible)
          o_box_draw_rubber (w_current);
        break;

      case ENDPICTURE:
        if (w_current->rubber_visible)
          o_picture_draw_rubber (w_current);
        break;

      case ENDCIRCLE:
        if (w_current->rubber_visible)
          o_circle_draw_rubber (w_current);
        break;

      case ENDARC:
        if (w_current->rubber_visible)
          o_arc_draw_rubber (w_current);
        break;

      case ENDPIN:
        if (w_current->rubber_visible)
          o_pin_draw_rubber (w_current);
        break;
    }
  }

  g_list_free (obj_list);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_redraw (GSCHEM_TOPLEVEL *w_current, GList *object_list, gboolean draw_selected)
{
  OBJECT *o_current;
  GList *iter;

  for (iter = object_list; iter != NULL; iter = g_list_next (iter)) {
    o_current = (OBJECT *)iter->data;

    if (o_current->dont_redraw ||
        (!draw_selected && o_current->selected))
      continue;

    o_redraw_single (w_current, o_current);
  }
}

/*! \brief Redraw an object on the screen.
 *  \par Function Description
 *  This function will redraw a single object on the screen.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  The OBJECT to redraw.
 *
 */
void o_redraw_single(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  void (*func) (GSCHEM_TOPLEVEL *, OBJECT *) = NULL;

  if (o_current == NULL)
    return;

  switch (o_current->type) {
      case OBJ_LINE:    func = o_line_draw;    break;
      case OBJ_NET:     func = o_net_draw;     break;
      case OBJ_BUS:     func = o_bus_draw;     break;
      case OBJ_BOX:     func = o_box_draw;     break;
      case OBJ_PICTURE: func = o_picture_draw; break;
      case OBJ_CIRCLE:  func = o_circle_draw;  break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_draw; break;
      case OBJ_TEXT:    func = o_text_draw;    break;
      case OBJ_PATH:    func = o_path_draw;    break;
      case OBJ_PIN:     func = o_pin_draw;     break;
      case OBJ_ARC:     func = o_arc_draw;     break;
      default:
        g_critical ("o_redraw_single: object %p has bad type '%c'\n",
                    o_current, o_current->type);
  }

  if (func != NULL)
    (*func) (w_current, o_current);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  /* return FALSE if it did not erase anything */

  if (!w_current->inside_action)
    return(FALSE);

  switch(w_current->event_state) {

    case(STARTDRAWBUS):
    case(DRAWBUS):
    case(BUSCONT):
      o_bus_invalidate_rubber (w_current);
    break;

    case(STARTDRAWNET):
    case(DRAWNET):
    case(NETCONT):
      o_net_invalidate_rubber (w_current);
    break;

    case(DRAWPIN):
    case(ENDPIN):
      o_pin_invalidate_rubber (w_current);
    break;

    case(DRAWLINE):
    case(ENDLINE):
      o_line_invalidate_rubber (w_current);
    break;

    case(DRAWBOX):
    case(ENDBOX):
      o_box_invalidate_rubber (w_current);
    break;

    case(DRAWPICTURE):
    case(ENDPICTURE):
      o_picture_invalidate_rubber (w_current);
    break;

    case(DRAWCIRCLE):
    case(ENDCIRCLE):
      o_circle_invalidate_rubber (w_current);
    break;

    case(DRAWARC):
    case(ENDARC):
      o_arc_invalidate_rubber (w_current);
    break;

    default:
      return(FALSE);
    break;
  }

  return(TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  This function is neccesary to make jumps between event_states.
 *  If we are inside an drawing action that created something on the dc,
 *  e.g. if we are drawing a box and then jump to line drawing without
 *  leaving the box drawing mode, there will remain some rubberbands on the
 *  screen.
 *  Usually a intermediate select state would clean (redraw) the screen.
 */
int o_redraw_cleanstates(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  /* returns FALSE if the function was'nt nessecary */
  if (w_current->inside_action == 0) {
    return FALSE;
  }

  switch (w_current->event_state) {
    /* all states with something on the dc */
    case(ENDCOMP):
      /* De-select the lists in the component selector */
      x_compselect_deselect (w_current);

      /* Fall through */
    case(COPY):
    case(MCOPY):
    case(DRAWBUS):
    case(DRAWNET):
    case(ENDARC):
    case(ENDBOX):
    case(ENDCIRCLE):
    case(ENDCOPY):
    case(ENDMCOPY):
    case(ENDLINE):
    case(ENDMOVE):
    case(ENDPASTE):
    case(ENDPIN):
    case(ENDTEXT):
    case(GRIPS):
    case(MOVE):
    case(NETCONT):
    case(ZOOMBOXEND):
      /* it is possible to cancel in the middle of a place,
       * so lets be sure to clean up the place_list structure */

      /* If we're cancelling from a move action, re-wind the
       * page contents back to their state before we started. */
      if ((w_current->event_state == MOVE) ||
          (w_current->event_state == ENDMOVE)) {
        o_move_cancel (w_current);
      }

      /* If we're cancelling from a grip action, call the specific cancel
       * routine to reset the visibility of the object being modified */
      if (w_current->event_state == GRIPS)
        o_grips_cancel (w_current);

      /* Free the place list and its contents. If we were in a move
       * action, the list (refering to objects on the page) would
       * already have been cleared in o_move_cancel(), so this is OK. */
      s_delete_object_glist(toplevel, toplevel->page_current->place_list);
      toplevel->page_current->place_list = NULL;

      w_current->inside_action = 0;

      /* touch the select state */
      i_set_state(w_current, SELECT);

      /* from i_callback_cancel() */
      o_invalidate_all (w_current);
      return TRUE;

    /* all remaining states without dc changes */
    case(NONE):
    case(SELECT):
    case(DRAWLINE):
    case(DRAWBOX):
    case(DRAWCIRCLE):
    case(ZOOM):
    case(PAN):
    case(BUSCONT):
    case(DRAWARC):
    case(DRAWPICTURE):
    case(DRAWPIN):
    case(ENDMIRROR):
    case(ENDPICTURE):
    case(ENDROTATEP):
    case(ENDROUTENET):
    case(MOUSEPAN):
    case(SBOX):
    case(STARTCOPY):
    case(STARTMCOPY):
    case(STARTDRAWBUS):
    case(STARTDRAWNET):
    case(STARTMOVE):
    case(STARTPAN):
    case(STARTPASTE):
    case(STARTROUTENET):
    case(STARTSELECT):
    case(ZOOMBOXSTART):
      return FALSE;
  }

  return FALSE;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  void (*func) (GSCHEM_TOPLEVEL *, int, int, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_LINE:    func = o_line_draw_place;       break;
      case OBJ_NET:     func = o_net_draw_place;        break;
      case OBJ_BUS:     func = o_bus_draw_place;        break;
      case OBJ_BOX:     func = o_box_draw_place;        break;
      case OBJ_PICTURE: func = o_picture_draw_place;    break;
      case OBJ_CIRCLE:  func = o_circle_draw_place;     break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_draw_place;    break;
      case OBJ_TEXT:    func = o_text_draw_place;       break;
      case OBJ_PATH:    func = o_path_draw_place;       break;
      case OBJ_PIN:     func = o_pin_draw_place;        break;
      case OBJ_ARC:     func = o_arc_draw_place;        break;
      default:
        g_assert_not_reached ();
  }

  if (func != NULL) {
    (*func) (w_current, dx, dy, object);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_glist_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, GList *list)
{
  GList *iter = list;

  while (iter != NULL) {
    o_draw_place (w_current, dx, dy, (OBJECT *)iter->data);
    iter = g_list_next(iter);
  }
}


/*! \brief Invalidates a rectangular region of the on screen drawing area
 *  \par Function Description
 *
 *  Given a pair of (x,y) coordinates in SCREEN units, invalidate the
 *  rectangular on-screen drawing area which has those two coordinate
 *  pairs as opposite corners of its region. This will cause that region
 *  to be blitted from the back-buffer once the mainloop reaches idle.
 *
 *  A margin, INVALIDATE_MARGIN is added to the invalidated region as
 *  a hacky workaround for rounding errors which may occur in the
 *  WORLD -> SCREEN coordinate transform. This margin may also be used
 *  to expand the invalidated region if anti-aliased drawing is ever
 *  used.
 *
 *  A further, larger margin is added to account for invalidating the
 *  size occupied by an object's grips.
 *
 *  If the GSCHEM_TOPLEVEL in question is not rendering to a GDK_WINDOW,
 *  (e.g. image export), this function call is a no-op. A test is used:
 *  GDK_IS_WINDOW(), which should be safe since in either case,
 *  w_current->window is a GObject. This is really a _HACK_,
 *  and should be fixed with a re-worked drawing model.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL who's drawing area is being invalidated.
 *  \param [in] x1         X coord for corner 1 (SCREEN units)
 *  \param [in] y1         Y coord for corner 1 (SCREEN units)
 *  \param [in] x2         X coord for corner 2 (SCREEN units)
 *  \param [in] y2         Y coord for corner 2 (SCREEN units)
 */
void o_invalidate_rect (GSCHEM_TOPLEVEL *w_current,
                        int x1, int y1, int x2, int y2)
{
  GdkRectangle rect;
  int grip_half_size;
  int cue_half_size;
  int bloat;

  /* BUG: We get called when rendering an image, and w_current->window
   *      is a GdkPixmap. Ensure we only invalidate GdkWindows. */
  if (!GDK_IS_WINDOW( w_current->window ))
    return;

  grip_half_size = o_grips_size (w_current);
  cue_half_size = SCREENabs (w_current, CUE_BOX_SIZE);
  bloat = MAX (grip_half_size, cue_half_size) + INVALIDATE_MARGIN;

  rect.x = MIN(x1, x2) - bloat;
  rect.y = MIN(y1, y2) - bloat;
  rect.width = 1 + abs( x1 - x2 ) + 2 * bloat;
  rect.height = 1 + abs( y1 - y2 ) + 2 * bloat;
  gdk_window_invalidate_rect( w_current->window, &rect, FALSE );
}


/*! \brief Invalidate the whole on-screen area
 *
 *  \par Function Description
 *  This function calls gdk_window_invalidate_rect() with a rect
 *  of NULL, causing the entire drawing area to be invalidated.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_invalidate_all (GSCHEM_TOPLEVEL *w_current)
{
  gdk_window_invalidate_rect (w_current->window, NULL, FALSE);
}


/*! \brief Invalidate on-screen area for an object
 *
 *  \par Function Description
 *  This function calls o_invalidate_rect() with the bounds of the
 *  passed OBJECT, converted to screen coordinates.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] object     The OBJECT invalidated on screen.
 */
void o_invalidate (GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;
  if (world_get_single_object_bounds(toplevel, object, &left,  &top,
                                                       &right, &bottom)) {
    WORLDtoSCREEN (w_current, left, top, &s_left, &s_top);
    WORLDtoSCREEN (w_current, right, bottom, &s_right, &s_bottom);
    o_invalidate_rect (w_current, s_left, s_top, s_right, s_bottom);
  }
}


/*! \brief Invalidate on-screen area for a GList of objects
 *
 *  \par Function Description
 *  This function calls o_invalidate_rect() with the bounds of the
 *  passed GList, converted to screen coordinates.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] list       The glist objects invalidated on screen.
 */
void o_invalidate_glist (GSCHEM_TOPLEVEL *w_current, GList *list)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;
  if (world_get_object_glist_bounds (toplevel, list, &left,  &top,
                                                     &right, &bottom)) {
    WORLDtoSCREEN (w_current, left, top, &s_left, &s_top);
    WORLDtoSCREEN (w_current, right, bottom, &s_right, &s_bottom);
    o_invalidate_rect (w_current, s_left, s_top, s_right, s_bottom);
  }
}


/*! \brief Returns the color an object should be drawn in
 *
 *  \par Function Description
 *  This function looks up and returns either the SELECT_COLOR, or the
 *  OBJECT's natural colour, as appropriate. If toplevel->override_color
 *  is set, that takes precedence.
 *
 *  The parent field of the OBJECT structure is used to recurse down
 *  and check whether the OBJECT being drawn is a prim_obj belonging
 *  to some selected OBJECT. If so, SELECT_COLOR is used.
 *
 *  As a convenience, the appropriate color index is looked up using
 *  x_color_lookup(), so that code is not duplicated in each drawing
 *  function.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] object     The OBJECT whos color to return.
 */
COLOR *o_drawing_color (GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  int color_idx;
  OBJECT *temp;

  color_idx = object->color;

  if (object->selected)
    color_idx = SELECT_COLOR;

  /* Check if the object, or its parent(s) are selected */
  for (temp = object; temp != NULL; temp = temp->parent) {
    if (temp->selected) {
      color_idx = SELECT_COLOR;
      break;
    }
  }

  if (w_current->toplevel->override_color != -1)
    color_idx = w_current->toplevel->override_color;

  return x_color_lookup (color_idx);
}
