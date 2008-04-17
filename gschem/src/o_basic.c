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

#include "../include/gschem_struct.h"
#include "../include/x_states.h"
#include "../include/prototype.h"

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
void o_redraw_all(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  gboolean draw_selected = TRUE;

  if (!toplevel->DONT_REDRAW) {
    x_repaint_background(w_current);
  }

  draw_selected = !(w_current->inside_action &&
                    ((w_current->event_state == MOVE) ||
                     (w_current->event_state == ENDMOVE) ||
                     (w_current->event_state == GRIPS)));
  o_redraw(w_current, toplevel->page_current->object_head, draw_selected);
  o_cue_redraw_all(w_current,
                   toplevel->page_current->object_head, draw_selected);

  if (w_current->inside_action) {
    switch(w_current->event_state) {
      case(MOVE):
      case(ENDMOVE):
      case(ENDCOPY):
      case(ENDMCOPY):
        o_drawbounding(w_current,
                       geda_list_get_glist( toplevel->page_current->selection_list ),
                       x_get_darkcolor(w_current->bb_color), TRUE);
        break;

      case(DRAWCOMP):
      case(ENDCOMP):
        o_drawbounding(w_current, toplevel->page_current->complex_place_list,
                       x_get_darkcolor(w_current->bb_color), TRUE);
        break;

      case(DRAWTEXT):
      case(ENDTEXT):
        o_drawbounding(w_current, toplevel->page_current->attrib_place_list,
                       x_get_darkcolor(w_current->bb_color), TRUE);
        break;
      case(STARTDRAWNET):
      case(DRAWNET):
      case(NETCONT):
	w_current->magnetic_visible=0;
        break;
    }
    w_current->rubber_visible=0;
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_redraw(GSCHEM_TOPLEVEL *w_current, OBJECT *list, gboolean draw_selected)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current = list;
  int redraw_state = toplevel->DONT_REDRAW;
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;

  w_current->inside_redraw = 1;
  while (o_current != NULL) {
    if ((o_current->draw_func != NULL) &&
        (o_current->type != OBJ_HEAD)) {
      toplevel->DONT_REDRAW = redraw_state ||
                              (!draw_selected && o_current->selected);
      (*o_current->draw_func)(w_current, o_current);
    }

    o_current = o_current->next;
  }
  w_current->inside_redraw = 0;
  toplevel->DONT_REDRAW = redraw_state;

  if (world_get_object_list_bounds(toplevel, list, &left,  &top,
                                                   &right, &bottom)) {
    WORLDtoSCREEN( toplevel, left, top, &s_left, &s_top );
    WORLDtoSCREEN( toplevel, right, bottom, &s_right, &s_bottom );
    o_invalidate_rect( w_current, s_left, s_top, s_right, s_bottom );
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
  TOPLEVEL *toplevel = w_current->toplevel;
  int left, top, right, bottom;
  int s_left, s_top, s_right, s_bottom;

  if (o_current == NULL)
  return;

  if (toplevel->DONT_REDRAW) /* highly experimental */
  return;

  if (o_current->draw_func != NULL && o_current->type != OBJ_HEAD) {
    w_current->inside_redraw = 1;
    (*o_current->draw_func)(w_current, o_current);
    w_current->inside_redraw = 0;
  }

  if (world_get_single_object_bounds(toplevel, o_current, &left,  &top,
                                                          &right, &bottom)) {
    WORLDtoSCREEN(toplevel, left, top, &s_left, &s_top);
    WORLDtoSCREEN(toplevel, right, bottom, &s_right, &s_bottom);
    o_invalidate_rect (w_current, s_left,  s_bottom, s_right, s_top);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_draw_list(GSCHEM_TOPLEVEL *w_current, GList* list)
{
  OBJECT* o_current;
  GList *l_current;

  if (w_current->inside_redraw) {
    return;
  }

  l_current = list;
  while (l_current != NULL) {

    o_current = (OBJECT *) l_current->data;

    if (o_current) {
      o_redraw_single(w_current, o_current);
    }
    
    l_current = g_list_next(l_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_draw_selected(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList* s_current;
  OBJECT* o_current;
  if (w_current->inside_redraw) {
    return;
  }

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  while (s_current != NULL) {
    o_current = (OBJECT *) s_current->data;

    if (o_current) {
      o_redraw_single(w_current, o_current);
      o_cue_draw_single(w_current, o_current);
    }
    s_current=g_list_next(s_current);
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_erase_selected(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *list;
  GList *iter;
  OBJECT* o_current;
  int left, top, right, bottom;
  int s_left, s_top, s_right, s_bottom;

  if (w_current->inside_redraw) {
    return;
  }

  list = iter = geda_list_get_glist( toplevel->page_current->selection_list );
  while (iter != NULL) {
    o_current = iter->data;

    if (o_current) {
      o_cue_erase_single(w_current, o_current);
      o_erase_single(w_current, o_current);
    }

    iter = g_list_next( iter );
  }

  if (world_get_object_glist_bounds(toplevel, list, &left,  &top,
                                                    &right, &bottom)) {
    WORLDtoSCREEN(toplevel, left, top, &s_left, &s_top);
    WORLDtoSCREEN(toplevel, right, bottom, &s_right, &s_bottom);
    o_invalidate_rect (w_current, s_left,  s_bottom, s_right, s_top);
  }
}

/*! \brief Erase a given OBJECT
 *
 *  \par Function Description
 *  This function erases the passed OBJECT, <B>object</B>.
 *
 *  It makes a call to object's draw function after having set a
 *  color override to the background color. The object is drawn in
 *  the background color, causing it to dissapear.
 *
 *  \bug No redrawing is done of occluded objects, including the grid.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Circle OBJECT to erase.
 */
void o_erase_single(GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  int left, top, right, bottom;
  int s_left, s_top, s_right, s_bottom;

  if (w_current->inside_redraw) {
    return;
  }

  o_current = object;

  toplevel->override_color = toplevel->background_color;
  if (o_current != NULL) {
    if (o_current->draw_func &&
        o_current->type != OBJ_HEAD) {
      (*o_current->draw_func)(w_current, o_current);
    }
  }
  toplevel->override_color = -1;

  if (world_get_single_object_bounds(toplevel, o_current, &left,  &top,
                                                          &right, &bottom)) {
    WORLDtoSCREEN(toplevel, left, top, &s_left, &s_top);
    WORLDtoSCREEN(toplevel, right, bottom, &s_right, &s_bottom);
    o_invalidate_rect (w_current, s_left,  s_bottom, s_right, s_top);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_erase_list(GSCHEM_TOPLEVEL *w_current, GList* list)
{
  OBJECT *o_current;
  GList *iter;

  if (w_current->inside_redraw) {
    return;
  }

  iter = list;
  while (iter != NULL) {
    o_current = iter->data;
    o_erase_single(w_current, o_current);
    iter = g_list_next(iter);
  }
}

/*! \brief XOR draw a bounding box or outline for OBJECT placement
 *
 *  \par Function Description
 *  This function XOR draws either the OBJECTS in the passed GList,
 *  or a rectangle around their bounding box, depending upon the
 *  currently selected w_current->actionfeedback_mode. This takes the
 *  value BOUNDINGBOX or OUTLINE.
 *
 * The function applies manhatten mode constraints to the coordinates
 * before drawing if the CONTROL key is recording as being pressed in
 * the w_current structure.
 *
 * The "drawing" parameter is used to indicate if this drawing should
 * immediately use the selected feedback mode and positioning constraints.
 *
 * With drawing=TRUE, the selected conditions are used immediately,
 * otherwise the conditions from the last drawing operation are used,
 * saving the new state for next time.
 *
 * o_drawbounding() should be called with drawing=TRUE when starting a
 * rubberbanding operation and when otherwise refreshing the rubberbanded
 * outline (e.g. after a screen redraw). For any undraw operation, should
 * be called with drawing=FALSE, ensuring that the undraw XOR matches the
 * mode and constraints of the corresponding "draw" operation.
 *
 * If any mode / constraint changes are made between a undraw, redraw XOR
 * pair, the latter (draw) operation must be called with drawing=TRUE. If
 * no mode / constraint changes were made between the pair, it is not
 * harmful to call the draw operation with "drawing=FALSE".
 *
 *  \param [in] w_current   GSCHEM_TOPLEVEL which we're drawing for.
 *  \param [in] o_glst      GList of objects to XOR draw.
 *  \param [in] color       GdkColor used for drawing in BOUNDINGBOX mode.
 *  \param [in] drawing     Set to FALSE for undraw operations to ensure
 *                            matching conditions to a previous draw operation.
 */
void o_drawbounding(GSCHEM_TOPLEVEL *w_current, GList *o_glist,
		    GdkColor *color, int drawing)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;
  int left, top, bottom, right;
  int s_left, s_top, s_bottom, s_right;

  if (o_glist == NULL) {
    return;
  }

  /* BUG: temporary fix while switching to world corrds */
  if (!(w_current->event_state == MOVE)
      && !(w_current->event_state == ENDMOVE)
      && !(w_current->event_state == DRAWTEXT)
      && !(w_current->event_state == ENDTEXT)
      && !(w_current->event_state == ENDPASTE)) {
    SCREENtoWORLD(toplevel, w_current->start_x, w_current->start_y,
		  &(w_current->first_wx), &(w_current->first_wy));
    SCREENtoWORLD(toplevel, w_current->last_x, w_current->last_y,
		  &(w_current->second_wx), &(w_current->second_wy));
  }

  /* If drawing is true, then don't worry about the previous drawing
   * method and movement constraints, use with the current settings */
  if (drawing) {
    w_current->last_drawb_mode = w_current->actionfeedback_mode;
    w_current->drawbounding_action_mode = (w_current->CONTROLKEY)
                                            ? CONSTRAINED : FREE;
  }

  /* Calculate delta of X-Y positions from buffer's origin */
  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  /* Adjust the coordinates according to the movement constraints */
  if (w_current->drawbounding_action_mode == CONSTRAINED ) {
    if (abs(diff_x) >= abs(diff_y)) {
      w_current->second_wy = w_current->first_wy;
    } else {
      w_current->second_wx = w_current->first_wx;
    }
  }

  /* Find the bounds of the drawing to be done */
  world_get_object_glist_bounds(toplevel, o_glist,
                                &left, &top, &right, &bottom);
  WORLDtoSCREEN(toplevel, left + diff_x, top + diff_y, &s_left, &s_top);
  WORLDtoSCREEN(toplevel, right + diff_x, bottom + diff_y, &s_right, &s_bottom);

  /* XOR draw with the appropriate mode */
  if (w_current->last_drawb_mode == BOUNDINGBOX) {
    gdk_gc_set_foreground(w_current->bounding_xor_gc, color);
    gdk_draw_rectangle(w_current->backingstore,
                       w_current->bounding_xor_gc, FALSE,
                       s_left, s_bottom,
                       s_right - s_left, s_top - s_bottom);
  } else {
    o_glist_draw_xor (w_current, diff_x, diff_y, o_glist);
  }

  /* Invalidate the screen buffer where we drew */
  o_invalidate_rect(w_current, s_left, s_top,
                               s_right, s_bottom);

  /* Save movement constraints and drawing method for any
   * corresponding undraw operation. */
  w_current->last_drawb_mode = w_current->actionfeedback_mode;
  w_current->drawbounding_action_mode = (w_current->CONTROLKEY)
                                          ? CONSTRAINED : FREE;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_erase_rubber(GSCHEM_TOPLEVEL *w_current)
{
   /* return FALSE if it did not erase anything */
 
   if (!w_current->inside_action)
     return(FALSE);

   switch(w_current->event_state) {

     case(STARTDRAWBUS):
     case(DRAWBUS):
     case(BUSCONT):
        o_bus_eraserubber(w_current);
     break;

     case(STARTDRAWNET):
     case(DRAWNET):
     case(NETCONT):
        o_net_eraserubber(w_current);
     break;

     case(DRAWPIN):
     case(ENDPIN):
        o_pin_eraserubber(w_current);
     break;

     case(DRAWLINE):
     case(ENDLINE):
        o_line_eraserubber(w_current);
     break;

     case(DRAWBOX):
     case(ENDBOX):
        o_box_eraserubber(w_current);
     break;

     case(DRAWPICTURE):
     case(ENDPICTURE):
        o_picture_eraserubber(w_current);
     break;

     case(DRAWCIRCLE):
     case(ENDCIRCLE):
        o_circle_eraserubber(w_current);
     break;

     case(DRAWARC):
     case(ENDARC):
        o_arc_eraserubber(w_current);
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
    case(DRAWCOMP):
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
       * so lets be sure to clean up the complex_place_list
       * structure and also clean up the attrib_place_list. */

      /* If it is a move command, then free the complex place list WITHOUT
         freeing the individual objects. */
      if ((w_current->event_state == MOVE) ||
          (w_current->event_state == ENDMOVE)) {
        g_list_free (toplevel->page_current->complex_place_list);
      } else {
        s_delete_object_glist(toplevel,
                              toplevel->page_current->complex_place_list);
      }
      toplevel->page_current->complex_place_list = NULL;

      s_delete_object_glist (toplevel,
                             toplevel->page_current->attrib_place_list);
      toplevel->page_current->attrib_place_list = NULL;

      /* also free internal current_attribute */
      o_attrib_free_current(toplevel);
      w_current->inside_action = 0;

      /* touch the select state */
      i_set_state(w_current, SELECT);

      /* from i_callback_cancel() */
      o_redraw_all(w_current);
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
    case(DRAWTEXT): 
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
    case(TEXTENTRY): 
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
void o_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  void (*func) (GSCHEM_TOPLEVEL *, int, int, OBJECT*) = NULL;

  switch (object->type) {
      case OBJ_HEAD:    /* Do nothing for head nodes */   break;
      case OBJ_LINE:    func = o_line_draw_xor;           break;
      case OBJ_NET:     func = o_net_draw_xor;            break;
      case OBJ_BUS:     func = o_bus_draw_xor;            break;
      case OBJ_BOX:     func = o_box_draw_xor;            break;
      case OBJ_PICTURE: func = o_picture_draw_xor;        break;
      case OBJ_CIRCLE:  func = o_circle_draw_xor;         break;
      case OBJ_PLACEHOLDER:
      case OBJ_COMPLEX: func = o_complex_draw_xor;        break;
      case OBJ_TEXT:    func = o_text_draw_xor;           break;
      case OBJ_PIN:     func = o_pin_draw_xor;            break;
      case OBJ_ARC:     func = o_arc_draw_xor;            break;
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
void o_list_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *list)
{
  OBJECT *o_current = list;

  while(o_current != NULL) {
    o_draw_xor(w_current, dx, dy, o_current);
    o_current = o_current->next;
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_glist_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, GList *list)
{
  GList *iter = list;

  while (iter != NULL) {
    o_draw_xor(w_current, dx, dy, (OBJECT *)iter->data);
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
void o_invalidate_rect( GSCHEM_TOPLEVEL *w_current,
                        int x1, int y1, int x2, int y2 )
{
  GdkRectangle rect;

  /* BUG: We get called when rendering an image, and w_current->window
   *      is a GdkPixmap. Ensure we only invalidate GdkWindows. */
  if (!GDK_IS_WINDOW( w_current->window ))
    return;

  rect.x = MIN(x1, x2) - INVALIDATE_MARGIN;
  rect.y = MIN(y1, y2) - INVALIDATE_MARGIN;
  rect.width = 1 + abs( x1 - x2 ) + 2 * INVALIDATE_MARGIN;
  rect.height = 1 + abs( y1 - y2 ) + 2 * INVALIDATE_MARGIN;
  gdk_window_invalidate_rect( w_current->window, &rect, FALSE );
}
