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

/* instrumentation code */
#if 0
#include <sys/time.h>
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Lots of Gross code... needs lots of cleanup - mainly
 * readability issues
 */

/* Kazu on July 16, 1999 - Added these macros to simplify the code */
#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_redraw_all(TOPLEVEL *w_current)
{
  o_redraw_all_fast(w_current);

  if (w_current->inside_action) {
    switch(w_current->event_state) {
      case(MOVE):
      case(ENDMOVE):
	o_erase_selected(w_current);	
	/* continue */
      case(ENDCOPY):
      case(ENDMCOPY):
	o_drawbounding(w_current, NULL,
                       w_current->page_current->selection2_head->next,
                       x_get_darkcolor(w_current->bb_color), FALSE);

        break;

      case(DRAWCOMP):
      case(ENDCOMP):
        o_drawbounding(w_current, 
		       w_current->page_current->complex_place_head->next,
                       NULL,
                       x_get_darkcolor(w_current->bb_color), FALSE);
        break;

      case(DRAWATTRIB):
      case(ENDATTRIB):
      case(DRAWTEXT):
      case(ENDTEXT):
        o_drawbounding(w_current, w_current->
                       page_current->
                       attrib_place_head->next,
                       NULL, 
                       x_get_darkcolor(w_current->bb_color), FALSE);
        break;
      case (GRIPS):
	o_erase_selected(w_current);	
	break;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* basically like above but doesn't do the o_conn_disconnect_update */
void o_redraw_all_fast(TOPLEVEL *w_current)
{
  gboolean draw_selected = TRUE;

  if (!w_current->DONT_REDRAW) {
    x_repaint_background(w_current);
  }

  o_recalc(w_current, w_current->page_current->object_head);
  /* Uncomment this when using the complex_place_list for moving and copying */
  /*  o_recalc_glist(w_current, w_current->page_current->complex_place_list); */

  draw_selected = !(w_current->inside_action &&
		    ((w_current->event_state == MOVE) ||
		     (w_current->event_state == ENDMOVE)));
  o_redraw(w_current, w_current->page_current->object_head, draw_selected);
  o_cue_redraw_all(w_current,
		   w_current->page_current->object_head, draw_selected);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_redraw(TOPLEVEL *w_current, OBJECT *object_list, gboolean draw_selected)
{
  OBJECT *o_current = object_list;
  int redraw_state = w_current->DONT_REDRAW;

  while (o_current != NULL) {
    if ((o_current->draw_func != NULL) &&
        (o_current->type != OBJ_HEAD)) {
      if (o_current->selected && !draw_selected) {
	w_current->DONT_REDRAW = 1 || redraw_state;
      }
      else {
	w_current->DONT_REDRAW = 0 || redraw_state;
      }
      w_current->inside_redraw = 1;
      (*o_current->draw_func)(w_current, o_current);
      w_current->inside_redraw = 0;
    }

    o_current = o_current->next;
  }
  w_current->DONT_REDRAW = redraw_state;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_unselect_all(TOPLEVEL *w_current)
{
  if (!w_current->SHIFTKEY) {
    o_select_run_hooks(w_current, NULL, 2);
    o_selection_remove_most(w_current, w_current->page_current->
                            selection2_head); 
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_draw_list(TOPLEVEL *w_current, GList* list)
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
    
    l_current = l_current->next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_draw_selected(TOPLEVEL *w_current)
{
  SELECTION* s_current;
  OBJECT* o_current;
  if (w_current->inside_redraw) {
    return;
  }

  s_current = w_current->page_current->selection2_head->next;
  while (s_current != NULL) {
    o_current=s_current->selected_object;

    if (o_current) {
      o_redraw_single(w_current, o_current);
    }
    
    s_current=s_current->next;
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_erase_selected(TOPLEVEL *w_current)
{
  SELECTION* s_current;
  OBJECT* o_current;
  if (w_current->inside_redraw) {
    return;
  }

  s_current = w_current->page_current->selection2_head->next;
  while (s_current != NULL) {
    o_current=s_current->selected_object;

    if (o_current) {
      o_cue_erase_single(w_current, o_current);
      o_erase_single(w_current, o_current);
    }
    
    s_current=s_current->next;
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_erase_single(TOPLEVEL *w_current, OBJECT *object)
{
  OBJECT *o_current;

  if (w_current->inside_redraw) {
    return;
  }

  o_current = object;

  w_current->DONT_DRAW_CONN = 1;
  w_current->override_color = w_current->background_color;
  if (o_current != NULL) {
    if (o_current->draw_func &&
        o_current->type != OBJ_HEAD) {
      (*o_current->draw_func)(w_current, o_current);
    }
  }
  w_current->override_color = -1;
  w_current->DONT_DRAW_CONN = 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* both outline and boundingbox work! */
/* name is blah */
void o_drawbounding(TOPLEVEL *w_current, OBJECT *o_list, SELECTION *s_list, 
		    GdkColor *color, int firsttime)
{
  int diff_x, diff_y;
  int test_x, test_y;

  /* static is highly temp */	
  /* you have to make these static... for the once mode */
  static int rleft, rtop, rbottom, rright;

  if (!o_list && !s_list) {
    return;
  }

  if ((w_current->last_drawb_mode == OUTLINE) &&
      (w_current->actionfeedback_mode == BOUNDINGBOX)) {
#if DEBUG
    printf("going to bounding\n");
#endif

    diff_x = w_current->last_x - w_current->start_x;
    diff_y = w_current->last_y - w_current->start_y;

    gdk_gc_set_foreground(w_current->bounding_xor_gc,
                          x_get_color(
                                      w_current->background_color));
    if (o_list) {
      o_complex_translate_display(w_current,
                                  diff_x,
                                  diff_y,
                                  o_list);
    } else if (s_list) { 
      o_complex_translate_display_selection(w_current,
					    diff_x,
					    diff_y,
					    s_list);
    }

    gdk_gc_set_foreground(w_current->bounding_xor_gc, color);

    if (o_list) {
      get_complex_bounds(w_current, o_list,
                         &rleft  ,
                         &rtop   ,
                         &rright ,
                         &rbottom);
    } else if (s_list) {
      get_complex_bounds_selection(w_current, s_list,
				   &rleft  ,
				   &rtop   ,
				   &rright ,
				   &rbottom);
    }

    gdk_draw_rectangle(w_current->window,
                       w_current->bounding_xor_gc, FALSE,
                       rleft + diff_x,
                       rtop  + diff_y,
                       rright  - rleft,
                       rbottom - rtop);

  }

  if ((w_current->last_drawb_mode == BOUNDINGBOX) &&
      (w_current->actionfeedback_mode == OUTLINE)) {
#if DEBUG
        printf("going to outline\n");
#endif

        if (o_list) {
          get_complex_bounds(w_current, o_list,
                             &rleft  ,
                             &rtop   ,
                             &rright ,
                             &rbottom);
        } else if (s_list) {
          get_complex_bounds_selection(w_current, s_list,
                                       &rleft  ,
                                       &rtop   ,
                                       &rright ,
                                       &rbottom);
        }

        diff_x = w_current->last_x - w_current->start_x;
        diff_y = w_current->last_y - w_current->start_y;
        gdk_gc_set_foreground(w_current->gc,
                              x_get_color(
                                          w_current->background_color) );
        gdk_draw_rectangle(w_current->window,
                           w_current->gc, FALSE,
                           rleft   + diff_x,
                           rtop    + diff_y,
                           rright  - rleft ,
                           rbottom - rtop  );

        if (o_list) {
          o_complex_translate_display(w_current,
                                      diff_x,
                                      diff_y,
                                      o_list);
        } else if (s_list) { 
          o_complex_translate_display_selection(w_current,
                                                diff_x,
                                                diff_y,
                                                s_list);
        }
      }

  w_current->last_drawb_mode = w_current->actionfeedback_mode;

  /* everything above is okay */

  /*! \todo much replicated code... this is the behaviour we need, but
   * we need to clean it up !!!
   */

  /* erase old outline */
  /* going to constrained from free */
  if ( (w_current->CONTROLKEY) &&
       (w_current->drawbounding_action_mode == FREE)) {
    diff_x = w_current->last_x - w_current->start_x;
    diff_y = w_current->last_y - w_current->start_y;
#if 0
    printf("switching to contrained\n");
#endif
    w_current->drawbounding_action_mode = CONSTRAINED;

    if (w_current->actionfeedback_mode == OUTLINE) {
      if (o_list) {
        o_complex_translate_display(w_current,
                                    diff_x,
                                    diff_y,
                                    o_list);
      } else if (s_list) { 
        o_complex_translate_display_selection(w_current,
                                              diff_x,
                                              diff_y,
                                              s_list);
      }
    } else {
      if (o_list) {
        get_complex_bounds(w_current, o_list,
                           &rleft  ,
                           &rtop   ,
                           &rright ,
                           &rbottom);
      } else if (s_list) {
        get_complex_bounds_selection(w_current, s_list,
                                     &rleft  ,
                                     &rtop   ,
                                     &rright ,
                                     &rbottom);
      }

      gdk_gc_set_foreground(w_current->bounding_xor_gc,
                            color);
      gdk_draw_rectangle(w_current->window,
                         w_current->bounding_xor_gc, FALSE,
                         rleft + diff_x,
                         rtop  + diff_y,
                         rright  - rleft,
                         rbottom - rtop);
    }

    test_x = GET_BOX_WIDTH (w_current);
    test_y = GET_BOX_HEIGHT(w_current);
    if (test_x >= test_y) {
      w_current->last_y = w_current->start_y;
    } else {
      w_current->last_x = w_current->start_x;
    }

    diff_x = w_current->last_x - w_current->start_x;
    diff_y = w_current->last_y - w_current->start_y;

    if (w_current->actionfeedback_mode == OUTLINE) {
      if (o_list) {
        o_complex_translate_display(w_current,
                                    diff_x,
                                    diff_y,
                                    o_list);
      } else if (s_list) { 
        o_complex_translate_display_selection(w_current,
                                              diff_x,
                                              diff_y,
                                              s_list);
      }
    } else {
      if (o_list) {
        get_complex_bounds(w_current, o_list,
                           &rleft  ,
                           &rtop   ,
                           &rright ,
                           &rbottom);
      } else if (s_list) {
        get_complex_bounds_selection(w_current, s_list,
                                     &rleft  ,
                                     &rtop   ,
                                     &rright ,
                                     &rbottom);
      }
      gdk_gc_set_foreground(w_current->bounding_xor_gc,
                            color);
      gdk_draw_rectangle(w_current->window,
                         w_current->bounding_xor_gc,
                         FALSE,
                         rleft   + diff_x,
                         rtop    + diff_y,
                         rright  - rleft,
                         rbottom - rtop);
    }

    if (w_current->netconn_rubberband) {
      o_move_stretch_rubberband(w_current);
      o_move_stretch_rubberband(w_current);
    }
  }

  /* erase old outline */
  /* going to free from constrained */
  if ((!w_current->CONTROLKEY) &&
      (w_current->drawbounding_action_mode == CONSTRAINED)) {
#if 0
        printf("switching to free\n");
#endif
        diff_x = w_current->last_x - w_current->start_x;
        diff_y = w_current->last_y - w_current->start_y;
        w_current->drawbounding_action_mode = FREE;
        if (w_current->actionfeedback_mode == OUTLINE) {
          /* do it twice to get rid of old outline */
          if (o_list) {
            o_complex_translate_display(w_current,
                                        diff_x,
                                        diff_y,
                                        o_list);
            o_complex_translate_display(w_current,
                                        diff_x,
                                        diff_y,
                                        o_list);
          } else if (s_list) { 
            o_complex_translate_display_selection(w_current,
                                                  diff_x,
                                                  diff_y,
                                                  s_list);
            o_complex_translate_display_selection(w_current,
                                                  diff_x,
                                                  diff_y,
                                                  s_list);
          }
        } else {
          /*! \todo why are we doing this here...?
           * probably a reason */
          if (o_list) {
            get_complex_bounds(w_current, o_list,
                               &rleft  ,
                               &rtop   ,
                               &rright ,
                               &rbottom);
          } else if (s_list) {
            get_complex_bounds_selection(w_current, s_list,
                                         &rleft  ,
                                         &rtop   ,
                                         &rright ,
                                         &rbottom);
          }
        }
        if (w_current->netconn_rubberband) {
          o_move_stretch_rubberband(w_current);
          o_move_stretch_rubberband(w_current);
        }
      }

  if (w_current->CONTROLKEY) {
    test_x = GET_BOX_WIDTH (w_current);
    test_y = GET_BOX_HEIGHT(w_current);
    if (test_x >= test_y) {
      w_current->last_y = w_current->start_y;
    } else {
      w_current->last_x = w_current->start_x;
    }
  }

  if (w_current->actionfeedback_mode == BOUNDINGBOX) {

    if (firsttime == TRUE) {
      if (o_list) {
        get_complex_bounds(w_current, o_list,
                           &rleft  ,
                           &rtop   ,
                           &rright ,
                           &rbottom);
      } else if (s_list) {
        get_complex_bounds_selection(w_current, s_list,
                                     &rleft  ,
                                     &rtop   ,
                                     &rright ,
                                     &rbottom);
      }
      /*printf("once\n");*/
    
    }
    diff_x = w_current->last_x - w_current->start_x;
    diff_y = w_current->last_y - w_current->start_y;
    gdk_gc_set_foreground(w_current->bounding_xor_gc, color);
    gdk_draw_rectangle(w_current->window,
                       w_current->bounding_xor_gc, FALSE,
                       rleft   + diff_x,
                       rtop    + diff_y,
                       rright  - rleft,
                       rbottom - rtop);

    return;
  }

  diff_x = w_current->last_x - w_current->start_x;
  diff_y = w_current->last_y - w_current->start_y;

  /*! \todo have I mentioned how temp this is? Make this general
   * so that all lists can be moved ...
   */
  if (o_list) {
    o_complex_translate2(w_current, diff_x, diff_y, o_list);
  } else if (s_list) {
    o_complex_translate_selection(w_current, diff_x, diff_y, 
                                  s_list);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_erasebounding(TOPLEVEL *w_current, OBJECT *o_list, SELECTION *s_list)
{
  int diff_x, diff_y;
  int rleft, rtop, rright, rbottom;

  if (o_list == NULL) {
    /* this is an error condition */
    w_current->event_state = SELECT;
    w_current->inside_action = 0;
    return;
  }

  if (w_current->actionfeedback_mode == OUTLINE) {
    return;
  }

  if (o_list) {
    get_complex_bounds(w_current, o_list,
                       &rleft  ,
                       &rtop   ,
                       &rright ,
                       &rbottom);
  } else if (s_list) {
    get_complex_bounds_selection(w_current, s_list,
                                 &rleft  ,
                                 &rtop   ,
                                 &rright ,
                                 &rbottom);
  }

  diff_x = w_current->last_x - w_current->start_x;
  diff_y = w_current->last_y - w_current->start_y;

  gdk_gc_set_foreground(w_current->gc,
                        x_get_color(w_current->background_color) );
  gdk_draw_rectangle(w_current->window, w_current->gc, FALSE,
                     rleft   + diff_x,
                     rtop    + diff_y,
                     rright  - rleft ,
                     rbottom - rtop  );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int o_erase_rubber(TOPLEVEL *w_current)
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
int o_redraw_cleanstates(TOPLEVEL *w_current)
{
  /* returns FALSE if the function was'nt nessecary */
  if (w_current->inside_action == 0) {
    return FALSE;
  }

  switch (w_current->event_state) {
    /* all states with something on the dc */
    case(COPY): 
    case(MCOPY): 
    case(DRAWBUS): 
    case(DRAWCOMP): 
    case(DRAWNET):   
    case(ENDARC): 
    case(ENDATTRIB):
    case(ENDBOX): 
    case(ENDCIRCLE): 
    case(ENDCOMP): 
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
      /* reset all rubberband variables and touch the select state */
      w_current->start_x = w_current->second_x = w_current->last_x = -1;
      w_current->start_y = w_current->second_y = w_current->last_y = -1;
      w_current->loc_x = w_current->loc_y = w_current->distance = -1;
      i_set_state(w_current, SELECT);

      /* from i_callback_cancel() */
      o_redraw_all(w_current);
      /* it is possible to cancel in the middle of a complex place
       * so lets be sure to clean up the complex_place_head
       * structure and also clean up the attrib_place_head.
       * remember these don't remove the head structure */
      o_list_delete_rest(w_current,
			 w_current->page_current->complex_place_head);
      o_list_delete_rest(w_current,
			 w_current->page_current->attrib_place_head);
 
      /* also free internal current_attribute */
      o_attrib_free_current(w_current);     
      w_current->inside_action = 0;
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
    case(DRAWATTRIB): 
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
