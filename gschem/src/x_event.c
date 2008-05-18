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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* used in key_press, since it isn't passed this information */
/* filled in x_event_motion */
int mouse_wx, mouse_wy;

/* used by mouse pan */
int start_pan_x, start_pan_y;
int throttle = 0;

/* used for the stroke stuff */
#ifdef HAS_LIBSTROKE

#define MAX_SEQUENCE 20
static int DOING_STROKE = FALSE;
char sequence[MAX_SEQUENCE+1];

/* libstroke prototypes */
void stroke_init (void);
void stroke_record (int x, int y);
int stroke_trans (char *sequence);
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_event_expose(GtkWidget *widget, GdkEventExpose *event,
                    GSCHEM_TOPLEVEL *w_current)
{
#if DEBUG
  printf("EXPOSE\n");
#endif

  exit_if_null(w_current);
  /* nasty global variable */
  global_window_current = w_current;

  gdk_draw_pixmap(widget->window,
                  widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
                  w_current->backingstore,
                  event->area.x, event->area.y,
                  event->area.x, event->area.y,
                  event->area.width, event->area.height);

  /* raise the dialog boxes if this feature is enabled */
  if (w_current->raise_dialog_boxes) {
    x_dialog_raise_all(w_current);
  }

  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_event_button_pressed(GtkWidget *widget, GdkEventButton *event,
                            GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int prev_state;
  int w_x, w_y;
  int unsnapped_wx, unsnapped_wy;

  exit_if_null(w_current);
  global_window_current = w_current;

#if DEBUG
  printf("pressed button %d! \n", event->button);
  printf("event state: %d \n", event->state);
  printf("w_current state: %d \n", w_current->event_state);
  printf("Selection is:\n");
  o_selection_print_all(&(toplevel->page_current->selection_list));
  printf("\n");
#endif

  SCREENtoWORLD( toplevel, (int) event->x, (int) event->y,
                 &unsnapped_wx, &unsnapped_wy );
  w_x = snap_grid(toplevel, unsnapped_wx);
  w_y = snap_grid(toplevel, unsnapped_wy);

  if (event->type == GDK_2BUTTON_PRESS &&
      (w_current->event_state == STARTSELECT ||
       w_current->event_state == SELECT)) {
    o_find_object(w_current, w_x, w_y, TRUE);
    if ( geda_list_get_glist( toplevel->page_current->selection_list )) {
       o_edit(w_current, geda_list_get_glist( toplevel->page_current->selection_list ));
       return(0);
    }
  }

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

  if (event->button == 1) {
    switch(w_current->event_state) {

      case(SELECT):
        /* look for grips or fall through if not enabled */
        if (!o_grips_start(w_current, unsnapped_wx, unsnapped_wy)) {
          /* now go into normal SELECT */
          w_current->event_state = STARTSELECT;
          w_current->first_wx = w_current->second_wx = unsnapped_wx;
          w_current->first_wy = w_current->second_wy = unsnapped_wy;
        } else {
          /* a grip was found */
          w_current->event_state = GRIPS;
          w_current->inside_action = 1;
        }
        break;

      case(STARTCOPY):
        if (o_select_selected(w_current)) {
          w_current->rotated_inside = 0;
          o_copy_start(w_current, w_x, w_y);
          w_current->event_state = COPY;
          w_current->inside_action = 1;
        }
        break;

      case(STARTMCOPY):
        if (o_select_selected(w_current)) {
          w_current->rotated_inside = 0;
          o_copy_start(w_current, w_x, w_y);
          w_current->event_state = MCOPY;
          w_current->inside_action = 1;
        }
        break;

      case(STARTMOVE):
        if (o_select_selected(w_current)) {
          w_current->rotated_inside = 0;
          o_move_start(w_current, w_x, w_y);
          w_current->event_state = MOVE;
          w_current->inside_action = 1;
        }
        break;

      case(STARTPASTE):
        o_buffer_paste_start(w_current, w_x, w_y, w_current->buffer_number);
        w_current->event_state = ENDPASTE;
        w_current->inside_action = 1;
        break;

      case(DRAWLINE):
        o_line_start(w_current, w_x, w_y);
        w_current->event_state = ENDLINE;
        w_current->inside_action = 1;
        break;

      case(ENDLINE):
        o_line_end(w_current, w_x, w_y);
        w_current->inside_action = 0;
        w_current->event_state = DRAWLINE;
        break;

      case(DRAWBOX):
        o_box_start(w_current, w_x, w_y);
        w_current->event_state = ENDBOX;
        w_current->inside_action = 1;
        break;

      case(ENDBOX):
        o_box_end(w_current, w_x, w_y);
        w_current->inside_action = 0;
        w_current->event_state = DRAWBOX;
        break;

      case(DRAWPICTURE):
        o_picture_start(w_current, w_x, w_y);
        w_current->event_state = ENDPICTURE;
        w_current->inside_action = 1;
        break;

      case(ENDPICTURE):
        o_picture_end(w_current, w_x, w_y);
        w_current->inside_action = 0;
        w_current->event_state = DRAWPICTURE;
        break;

      case(DRAWCIRCLE):
        o_circle_start(w_current, w_x, w_y);
        w_current->event_state = ENDCIRCLE;
        w_current->inside_action = 1;
        break;

      case(ENDCIRCLE):
        o_circle_end(w_current, w_x, w_y);
        w_current->inside_action = 0;
        w_current->event_state = DRAWCIRCLE;
        break;

      case(DRAWARC):
        o_arc_start(w_current, w_x, w_y);
        w_current->event_state = ENDARC;
        w_current->inside_action = 1;
        break;

      case(ENDARC):
        o_arc_end1(w_current, w_x, w_y);
        w_current->inside_action = 0;
        w_current->event_state = DRAWARC;
        break;

      case(DRAWPIN):
        o_pin_start(w_current, w_x, w_y);
        w_current->event_state = ENDPIN;
        w_current->inside_action = 1;
        break;

      case(ENDPIN):
        o_pin_end(w_current, w_x, w_y);
        w_current->inside_action = 0;
        w_current->event_state = DRAWPIN;
        break;

      case(STARTDRAWNET):  /*! \todo change state name? */
        o_net_start(w_current, w_x, w_y);
        w_current->inside_action = 1;
        w_current->event_state=DRAWNET;

        break;

      case(STARTDRAWBUS):
        o_bus_start(w_current, w_x, w_y);
        w_current->inside_action = 1;
        w_current->event_state=DRAWBUS;

        break;

      case(DRAWNET):
      case(NETCONT):
        /* Only continue the net if net end worked */
        if (o_net_end(w_current, w_x, w_y)) {
          o_net_start(w_current, w_current->first_wx, w_current->first_wy);
          w_current->event_state=NETCONT;
        } else { /* cleanup and start a new net */
          o_net_eraserubber(w_current);
          o_net_reset(w_current);
          i_set_state(w_current, STARTDRAWNET);
          w_current->inside_action = 0;
        }
        break;

      case(DRAWBUS):
      case(BUSCONT):
        /* Only continue the net if net end worked */
        if (o_bus_end(w_current, w_x, w_y)) {
          o_bus_start(w_current, w_current->first_wx, w_current->first_wy);
          w_current->event_state=BUSCONT;
        } else {
          w_current->inside_action=0;
          i_set_state(w_current, STARTDRAWBUS);
        }
        break;

      case(ENDCOMP):
        o_complex_end(w_current, w_x, w_y);
        o_redraw_single(w_current, toplevel->page_current->
                        object_tail);
        if (w_current->continue_component_place) {
          o_complex_start(w_current, w_x, w_y);
        } else {
          w_current->inside_action = 0;
          i_set_state(w_current, SELECT);
          i_update_toolbar(w_current);
        }
        break;

      case(ENDPASTE):
        o_buffer_paste_end(w_current, w_x, w_y,
                           w_current->buffer_number);
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ENDROTATEP):
        prev_state = toplevel->DONT_REDRAW;
        toplevel->DONT_REDRAW = 0;
        o_rotate_world_update(w_current, w_x, w_y, 90,
          geda_list_get_glist(toplevel->page_current->selection_list ));
        toplevel->DONT_REDRAW = prev_state;

        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ENDMIRROR):
        o_mirror_world_update(w_current, w_x, w_y,
                              geda_list_get_glist(
                                toplevel->page_current->selection_list ));

        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ENDTEXT):
        o_text_end(w_current);
                                /* not sure on this one either... */
                                /* keep it as well */
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
                                /* the following happen inside attrib_end */
                                /* therefore they are commeneted out here */
                                /* o_redraw_single(object_tail);*/
                                /* o_redraw_selected(); not sure on this */
        break;

      case(STARTPAN):
        a_pan(w_current, w_x, w_y);
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ZOOMBOXSTART):
        a_zoom_box_start(w_current, unsnapped_wx, unsnapped_wy);
        w_current->event_state = ZOOMBOXEND;
        w_current->inside_action = 1;
        break;

    }
  } else if (event->button == 2) {

    /* try this out and see how it behaves */
    if (w_current->inside_action) {
      if (w_current->event_state == ENDCOMP ||
          w_current->event_state == ENDTEXT ||
          w_current->event_state == ENDMOVE ||
          w_current->event_state == ENDCOPY ||
          w_current->event_state == ENDMCOPY) {
            return(0);
          } else {
            i_callback_cancel(w_current, 0, NULL);
            return(0);
          }
    }

    switch(w_current->middle_button) {

      case(ACTION):
                                /* determine here if copy or move */
                                /* for now do move only */
                                /* make sure the list is not empty */
      if (o_select_selected(w_current)) {

        /* don't want to search if shift */
        /* key is depresed */
        if (!w_current->SHIFTKEY) {
          o_find_object(w_current, unsnapped_wx, unsnapped_wy, TRUE);
        }
      } else {
        o_select_unselect_all(w_current);
        /* don't want to search if shift */
        /* key is depresed */
        if (!w_current->SHIFTKEY) {
          o_find_object(w_current, unsnapped_wx, unsnapped_wy, TRUE);
        }
      }

      if (!o_select_selected(w_current)) {
        /* this means the above find did not
         * find anything */
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        return(0);
      }

      if (w_current->ALTKEY) {
        o_copy_start(w_current, w_x, w_y);
        w_current->inside_action = 1;
        i_set_state(w_current, COPY);
      } else {
        o_move_start(w_current, w_x, w_y);
        w_current->inside_action = 1;
        i_set_state(w_current, MOVE);
      }
      break;

      case(REPEAT):
      if (w_current->last_callback != NULL) {
        (*w_current->last_callback)(w_current, 0, NULL);
      }
      break;
#ifdef HAS_LIBSTROKE
      case(STROKE):
      DOING_STROKE=TRUE;
      break;

#endif
      case(MID_MOUSEPAN_ENABLED):
      w_current->event_state = MOUSEPAN; /* start */
      w_current->inside_action = 1;
      w_current->doing_pan = TRUE;
      start_pan_x = (int) event->x;
      start_pan_y = (int) event->y;
      throttle=0;
      break;
    }

  } else if (event->button == 3) {
    if (!w_current->inside_action) {
      if (w_current->third_button == POPUP_ENABLED) {
        i_update_menus(w_current);  /* update menus before popup  */
        do_popup(w_current, event);
      } else {
        w_current->event_state = MOUSEPAN; /* start */
        w_current->inside_action = 1;
        w_current->doing_pan = TRUE;
        start_pan_x = (int) event->x;
        start_pan_y = (int) event->y;
        throttle=0;
      }
    } else { /* this is the default cancel */
      switch (w_current->event_state) {
        case(STARTDRAWNET):
        case(DRAWNET):
        case(NETCONT):
          w_current->inside_action = 0;
          i_set_state(w_current, STARTDRAWNET);
          o_net_eraserubber(w_current);
          o_net_reset(w_current);
          break;

        case(STARTDRAWBUS):
        case(DRAWBUS):
        case(BUSCONT):
          w_current->inside_action = 0;
          i_set_state(w_current, STARTDRAWBUS);
          o_bus_eraserubber(w_current);
          break;

        case(DRAWPIN):
        case(ENDPIN):
          w_current->inside_action = 0;
          i_set_state(w_current, DRAWPIN);
          o_pin_eraserubber(w_current);
          break;

        case(DRAWLINE):
        case(ENDLINE):
          w_current->inside_action = 0;
          i_set_state(w_current, DRAWLINE);
          o_line_eraserubber(w_current);
          break;

        case(DRAWBOX):
        case(ENDBOX):
          w_current->inside_action = 0;
          i_set_state(w_current, DRAWBOX);
          o_box_eraserubber(w_current);
          break;

        case(DRAWPICTURE):
        case(ENDPICTURE):
          w_current->inside_action = 0;
          i_set_state(w_current, DRAWPICTURE);
          o_picture_eraserubber(w_current);
          break;

        case(DRAWCIRCLE):
        case(ENDCIRCLE):
          w_current->inside_action = 0;
          i_set_state(w_current, DRAWCIRCLE);
          o_circle_eraserubber(w_current);
          break;

        case(DRAWARC):
        case(ENDARC):
          w_current->inside_action = 0;
          i_set_state(w_current, DRAWARC);
          o_arc_eraserubber(w_current);
          break;

        default:
          i_callback_cancel(w_current, 0, NULL);
          break;
      }
      i_update_toolbar(w_current);
    }
  }
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_event_button_released(GtkWidget *widget, GdkEventButton *event,
                             GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int prev_state;
  int redraw_state;
  int w_x, w_y;
  int unsnapped_wx, unsnapped_wy;

  exit_if_null(w_current);
  global_window_current = w_current;

#if DEBUG
  printf("released! %d \n", w_current->event_state);
#endif

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

  SCREENtoWORLD( toplevel, (int) event->x, (int) event->y,
                 &unsnapped_wx, &unsnapped_wy );
  w_x = snap_grid(toplevel, unsnapped_wx);
  w_y = snap_grid(toplevel, unsnapped_wy);

  if (event->button == 1) {
    switch(w_current->event_state) {
      case(SELECT):
        /* do nothing */
        break;

      case(MOVE):
        w_current->event_state = ENDMOVE;
        break;

      case(COPY):
        w_current->event_state = ENDCOPY;
        break;

      case(MCOPY):
        w_current->event_state = ENDMCOPY;
        break;

      case(GRIPS):
        o_grips_end(w_current),
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ENDMOVE):
        o_move_end(w_current);
        /* having this stay in copy was driving me nuts*/
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ENDCOPY):
        o_copy_end(w_current);
        /* having this stay in copy was driving me nuts*/
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ENDMCOPY):
        o_copy_end(w_current);
        /* having this stay in copy was driving me nuts*/
        w_current->inside_action = 1;
        /* Keep the state and the inside_action, as the copy has not finished. */
        w_current->first_wx = w_current->second_wx = w_x;
        w_current->first_wy = w_current->second_wy = w_y;
        o_complex_rubbercomplex_xor (w_current, TRUE);
        i_set_state(w_current, ENDMCOPY);
        i_update_toolbar(w_current);
        o_undo_savestate(w_current, UNDO_ALL);
        break;

      case(SBOX):
        o_select_box_end(w_current, unsnapped_wx, unsnapped_wy);
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(ZOOMBOXEND):
        a_zoom_box_end(w_current, unsnapped_wx, unsnapped_wy);
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

      case(STARTSELECT):
        /* first look for grips */
        if (!o_grips_start(w_current, unsnapped_wx, unsnapped_wy)) {
                                /* now go looking for objects to select */
          o_find_object(w_current, unsnapped_wx, unsnapped_wy, TRUE);
          w_current->event_state = SELECT;
          w_current->inside_action = 0;
        } else {
                                /* an grip was found */
          w_current->event_state = GRIPS;
          w_current->inside_action = 1;
        }
        break;

    }
  } else if (event->button == 2) {

    if (w_current->inside_action) {
      if (w_current->event_state == ENDCOMP) {
        o_complex_rubbercomplex_xor (w_current, FALSE);

        w_current->complex_rotate =
          (w_current->complex_rotate + 90) % 360;

        o_complex_place_rotate(w_current);

        /* Run the complex place list changed hook without redrawing */
        /* since all objects are being redrawn afterwards */
        prev_state = toplevel->DONT_REDRAW;
        toplevel->DONT_REDRAW = 1;
        o_complex_place_changed_run_hook (w_current);
        toplevel->DONT_REDRAW = prev_state;

        o_complex_rubbercomplex_xor (w_current, TRUE);
        return(0);
      } else if (w_current->event_state == ENDTEXT) {
        o_text_rubberattrib_xor (w_current, FALSE);

        w_current->complex_rotate =
        (w_current->complex_rotate + 90) % 360;

        o_text_place_rotate(w_current);

        o_text_rubberattrib_xor (w_current, TRUE);
        return(0);

      }
      else if (w_current->event_state == ENDMOVE) {
        prev_state = w_current->event_state;

        o_complex_rubbercomplex_xor (w_current, FALSE);

        /* Don't allow o_rotate_90 to erase the selection, neither to
           redraw the objects after rotating */
        /* skip over head node */
        redraw_state = toplevel->DONT_REDRAW;
        toplevel->DONT_REDRAW = 1;
        o_rotate_world_update(w_current, w_current->first_wx,
                                         w_current->first_wy, 90,
                              toplevel->page_current->complex_place_list );
        toplevel->DONT_REDRAW = redraw_state;
        w_current->rotated_inside ++;
        w_current->event_state = prev_state;

        o_complex_rubbercomplex_xor (w_current, TRUE);

        return(0);
      }
      else if ((w_current->event_state == ENDCOPY) ||
               (w_current->event_state == ENDMCOPY)) {
        /* Rotating while copying is still not supported, so do nothing */
        return 0;
      }

    }

    switch(w_current->middle_button) {
      case(ACTION):
      switch(w_current->event_state) {
        case(MOVE):
        o_move_end(w_current);
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;

        case(COPY):
        o_copy_end(w_current);
        w_current->inside_action = 0;
        i_set_state(w_current, SELECT);
        i_update_toolbar(w_current);
        break;
      }
      break;

#ifdef HAS_LIBSTROKE
      case(STROKE):

      DOING_STROKE = FALSE;

      if (stroke_trans (sequence) == TRUE) {
        if (stroke_info_mode) {
          printf ("LibStroke Translation"
                  " succeeded: ");
        }
      } else {
        if (stroke_info_mode) {
          printf ("LibStroke Translation"
                  " failed: ");
        }
      }

      if (stroke_info_mode) {
        printf ("Sequence=\"%s\"\n",sequence);
      }

      x_stroke_erase_all(w_current);

                                /* new way written by Stefan Petersen */
                                /* much better */
      if (x_stroke_search_execute(sequence)) {

        if (stroke_info_mode) {
          printf("Sequence understood\n");
        }
      }
      break;
#endif

      case(MID_MOUSEPAN_ENABLED):
      w_current->doing_pan=FALSE;
      o_redraw_all(w_current);
      if (w_current->undo_panzoom) {
        o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
      }
      /* this needs to be REDONE */
      /* if you mouse pan, you will be thrown out of the current mode. */
      /* not good */
      w_current->inside_action = 0;
      i_set_state(w_current, SELECT);
      i_update_toolbar(w_current);
      break;
    }

  } else if (event->button == 3) {
    if (w_current->doing_pan) { /* just for ending a mouse pan */
      w_current->doing_pan=FALSE;
      o_redraw_all(w_current);

      if (w_current->undo_panzoom) {
        o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
      }
      /* this needs to be REDONE */
      /* if you mouse pan, you will be thrown out of the current mode. */
      /* not good */
      w_current->inside_action = 0;
      i_set_state(w_current, SELECT);
      i_update_toolbar(w_current);
    }
  }
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_event_motion(GtkWidget *widget, GdkEventMotion *event,
                    GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int pdiff_x, pdiff_y;
  int w_x, w_y;
  int unsnapped_wx, unsnapped_wy;
  int skip_event=0;
  GdkEvent *test_event;

  exit_if_null(w_current);
  global_window_current = w_current;

  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

#if DEBUG
  /*  printf("MOTION!\n");*/
#endif

#ifdef HAS_LIBSTROKE
  if (DOING_STROKE == TRUE) {
    x_stroke_add_point(w_current, (int) event->x, (int) event->y);

    stroke_record ((int) event->x, (int) event->y);
    return(0);
  }
#endif

  /* skip the moving event if there are other moving events in the
     gdk event queue (Werner)
     Only skip the event if is the same event and no buttons or modifier
     keys changed*/
  if ((test_event = gdk_event_get()) != NULL) {
    if (test_event->type == GDK_MOTION_NOTIFY
        && ((GdkEventMotion *) test_event)->state == event->state) {
      skip_event= 1;
    }
    gdk_event_put(test_event); /* put it back in front of the queue */
    gdk_event_free(test_event);
    if (skip_event == 1)
      return 0;
  }

  SCREENtoWORLD( toplevel, (int) event->x, (int) event->y,
                 &unsnapped_wx, &unsnapped_wy );
  w_x = snap_grid(toplevel, unsnapped_wx);
  w_y = snap_grid(toplevel, unsnapped_wy);

  mouse_wx = w_x;
  mouse_wy = w_y;

  if (w_current->cowindow) {
    coord_display_update(w_current, (int) event->x, (int) event->y);
  }

  if (w_current->third_button == MOUSEPAN_ENABLED || w_current->middle_button == MID_MOUSEPAN_ENABLED) {
    if((w_current->event_state == MOUSEPAN) &&
       w_current->inside_action) {
         pdiff_x = (int) event->x - start_pan_x;
         pdiff_y = (int) event->y - start_pan_y;

         if (!(throttle % 5)) {
           a_pan_mouse(w_current, pdiff_x*w_current->mousepan_gain,
                       pdiff_y*w_current->mousepan_gain);

           start_pan_x = (int) event->x;
           start_pan_y = (int) event->y;
         }
         throttle++;
         return(0);
       }
  }

  switch(w_current->event_state) {

    case(SELECT):
    /* do nothing */
    break;

    case(GRIPS):
    o_grips_motion(w_current, unsnapped_wx, unsnapped_wy);
    break;

    case(STARTSELECT):
    if ( (!w_current->drag_can_move) ||
         (w_current->drag_can_move &&
          (! o_find_selected_object(w_current,
                                    w_current->first_wx, w_current->first_wy)))) {
      if (o_select_box_start(w_current, unsnapped_wx, unsnapped_wy)) {
        w_current->event_state = SBOX;
        w_current->inside_action = 1;
      }
      break;
    } else {
      /* Start the object movement */
      w_current->rotated_inside = 0;
      o_move_start(w_current, w_x, w_y);
      w_current->event_state = ENDMOVE;
      w_current->inside_action = 1;
    }
    /* Fall through */
    case(ENDMOVE):
    case(MOVE):
    if (w_current->inside_action)
      o_move_rubbermove(w_current, w_x, w_y);
    break;

    case(ENDCOPY):
    case(COPY):
    case(ENDMCOPY):
    case(MCOPY):
    if (w_current->inside_action)
      o_copy_rubbercopy(w_current, w_x, w_y);
    break;

    case(ENDLINE):
    if (w_current->inside_action)
      o_line_rubberline(w_current, w_x, w_y);
    break;

    case(ENDBOX):
    if (w_current->inside_action)
      o_box_rubberbox( w_current, w_x, w_y);
    break;

    case(ENDPICTURE):
    if (w_current->inside_action)
      o_picture_rubberbox( w_current, w_x, w_y);
    break;

    case(ENDCIRCLE):
    if (w_current->inside_action)
      o_circle_rubbercircle(w_current, w_x, w_y);
    break;

    case(ENDARC):
    if (w_current->inside_action)
      o_arc_rubberarc(w_current, w_x, w_y, ARC_RADIUS);
    break;


    case(STARTDRAWNET):
    if(w_current->magneticnet_mode == 1) {
      o_net_start_magnetic(w_current, w_x, w_y);
    }
    break;

    case(DRAWNET):
    case(NETCONT):
    if (w_current->inside_action)
      o_net_rubbernet(w_current, w_x, w_y);
    break;

    case(DRAWBUS):
    case(BUSCONT):
    if (w_current->inside_action)
      o_bus_rubberbus(w_current, w_x, w_y);
    break;

    case(ENDPIN):
    if (w_current->inside_action)
      o_pin_rubberpin(w_current, w_x, w_y);
    break;

    case(DRAWCOMP):
    w_current->complex_rotate = 0; /* reset to known state */
    o_complex_start(w_current, w_x, w_y);
    w_current->event_state = ENDCOMP;
    w_current->inside_action = 1;
    break;

    case(ENDCOMP):
    o_complex_rubbercomplex(w_current, w_x, w_y);
    break;

    case(ENDPASTE):
    o_buffer_paste_rubberpaste(w_current, w_current->buffer_number, w_x, w_y);
    break;

    case(DRAWTEXT):
    w_current->complex_rotate = 0; /* reset to known state */
    o_text_start(w_current, w_x, w_y);
    w_current->event_state = ENDTEXT;
    w_current->inside_action = 1;
    break;

    case(ENDTEXT):
    o_text_rubberattrib(w_current, w_x, w_y);
    break;

    case(SBOX):
    if (w_current->inside_action)
    o_select_box_rubberband(w_current, unsnapped_wx, unsnapped_wy);
    break;

    case(ZOOMBOXEND):
    if (w_current->inside_action)
      a_zoom_box_rubberband(w_current, unsnapped_wx, unsnapped_wy);
    break;

  }
  return(0);
}

/*! \brief Updates the GSCHEM_TOPLEVEL and display when drawing area is configured.
 *  \par Function Description
 *  This is the callback function connected to the configure event of
 *  the drawing area of the main window.
 *
 *  It updates the size of the backingstore for the associated
 *  toplevel structure (creates a new pixmap) and re-pans each of its
 *  pages to keep their contents centered in the drawing area.
 *
 *  When the window is maximised, the zoom of every page is changed to
 *  best fit the previously displayed area of the page in the new
 *  area. Otherwise the current zoom level is left unchanged.
 *
 *  \param [in] widget    The drawing area which received the signal.
 *  \param [in] event     The event structure of signal configure-event.
 *  \param [in] user_data The toplevel environment as user data.
 *  \returns FALSE to propagate the event further.
 */
gboolean
x_event_configure (GtkWidget         *widget,
                   GdkEventConfigure *event,
                   gpointer           user_data)
{
  GSCHEM_TOPLEVEL *w_current = (GSCHEM_TOPLEVEL*)user_data;
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *iter;
  PAGE *old_page_current, *p_current;
  gint old_win_width, old_win_height, new_win_width, new_win_height;
  gdouble relativ_zoom_factor = 1.0;

  g_assert (toplevel != NULL);
  global_window_current = w_current;

  if (toplevel->page_current == NULL) {
    /* don't want to call this if the current page isn't setup yet */
    return FALSE;
  }

  old_win_width  = w_current->win_width;
  old_win_height = w_current->win_height;
  new_win_width  = event->width;
  new_win_height = event->height;

  if (old_win_width  == new_win_width &&
      old_win_height == new_win_height) {
    /* the size of the drawing area has not changed */
    /* nothing to do here */
    return FALSE;
  }

  /* update the backingstore of toplevel */
  if (w_current->backingstore != NULL) {
    gdk_pixmap_unref (w_current->backingstore);
  }
  w_current->backingstore = gdk_pixmap_new (widget->window,
                                           new_win_width,
                                           new_win_height,
                                           -1);
  /* update the GSCHEM_TOPLEVEL with new size of drawing area */
  w_current->win_width   = toplevel->width  = new_win_width;
  w_current->win_height  = toplevel->height = new_win_height;


  /* in the case the user has maximised the window (hence the */
  /* configure event) fit the view by playing with zoom level */
  if (gdk_window_get_state (
        (gtk_widget_get_toplevel (
          widget))->window) & GDK_WINDOW_STATE_MAXIMIZED) {
    gdouble width_ratio, height_ratio;

    /* tweak relative_zoom to better fit page in maximized window */
    width_ratio  = ((gdouble)new_win_width)  / ((gdouble)old_win_width);
    height_ratio = ((gdouble)new_win_height) / ((gdouble)old_win_height);
    /* keep smallest ratio as relative zoom factor when panning */
    relativ_zoom_factor =
      (width_ratio < height_ratio) ? width_ratio : height_ratio;

  }

  /* save current page */
  old_page_current = toplevel->page_current;

  /* re-pan each page of the TOPLEVEL */
  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    gdouble cx, cy;
    p_current = (PAGE *)iter->data;

    /* doing this the aspectratio is kept when changing (hw)*/
    cx = ((gdouble)(p_current->left + p_current->right))  / 2;
    cy = ((gdouble)(p_current->top  + p_current->bottom)) / 2;
    s_page_goto (toplevel, p_current);
    a_pan_general (w_current, cx, cy, relativ_zoom_factor, A_PAN_DONT_REDRAW);

  }
  /* restore current page to saved value */
  s_page_goto (toplevel, old_page_current);

  if (!toplevel->DONT_REDRAW) {
    /* redraw the current page and update UI */
    o_redraw_all (w_current);
    x_scrollbars_update (w_current);
  }

  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is used during an open command
 *  to setup the correct sizes
 */
void x_manual_resize(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  /* of the actual win window (drawing_area) */
  w_current->win_width  = w_current->drawing_area->allocation.width;
  w_current->win_height = w_current->drawing_area->allocation.height;

#if DEBUG
  printf("manual: %d %d\n", w_current->win_width, w_current->win_height);
#endif

  toplevel->width = w_current->win_width;
  toplevel->height = w_current->win_height;

  /* need to do this every time you change width / height */
  set_window(toplevel, toplevel->page_current,
             toplevel->page_current->left,
             toplevel->page_current->right,
             toplevel->page_current->top,
             toplevel->page_current->bottom);

#if DEBUG
  printf("Window aspect: %f\n",
         (float) w_current->win_width / (float) w_current->win_height);
  /* No longer used?
     printf("w: %d h: %d\n", width, height); */
  printf("aw: %d ah: %d\n", w_current->win_width, w_current->win_height);
#endif

  /* I'm assuming that the backingstore pixmap is of the right
   * size */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_event_hschanged (GtkAdjustment *adj, GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int current_left;
  int new_left;
  GtkAdjustment        *hadjustment;

  exit_if_null(w_current);
  global_window_current = w_current;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  hadjustment =
  gtk_range_get_adjustment(GTK_RANGE(w_current->h_scrollbar));

  current_left = toplevel->page_current->left;
  new_left = (int) hadjustment->value;

  toplevel->page_current->left = new_left;
  toplevel->page_current->right =
    toplevel->page_current->right -
    (current_left - new_left);

  if (!toplevel->DONT_REDRAW) {
    o_redraw_all(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_event_vschanged (GtkAdjustment *adj, GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int current_bottom;
  int new_bottom;
  GtkAdjustment        *vadjustment;

  exit_if_null(w_current);
  global_window_current = w_current;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  vadjustment = gtk_range_get_adjustment(
                                         GTK_RANGE(w_current->v_scrollbar));

  current_bottom = toplevel->page_current->bottom;
  new_bottom = toplevel->init_bottom - (int) vadjustment->value;

  toplevel->page_current->bottom = new_bottom;
  toplevel->page_current->top =
    toplevel->page_current->top -
    (current_bottom - new_bottom);

#if DEBUG
  printf("vrange %f %f\n", vadjustment->lower, vadjustment->upper);
  printf("vvalue %f\n", vadjustment->value);
  printf("actual: %d %d\n", toplevel->page_current->top,
         toplevel->page_current->bottom);
#endif

  if (!toplevel->DONT_REDRAW) {
    o_redraw_all(w_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_event_enter(GtkWidget *widget, GdkEventCrossing *event,
                   GSCHEM_TOPLEVEL *w_current)
{
  exit_if_null(w_current);
  global_window_current = w_current;
  /* do nothing or now */
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gboolean x_event_key_press (GtkWidget *widget, GdkEventKey *event,
                            GSCHEM_TOPLEVEL *w_current)
{
  int retval;

  retval = FALSE;

  exit_if_null(w_current);
  global_window_current = w_current;

  if (event) {
#if DEBUG
    printf("x_event_key_pressed: Pressed key %i.\n", event->keyval);
#endif
    retval = g_keys_execute(w_current, event->state, event->keyval) ? TRUE : FALSE;
  }

  return retval;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_event_scroll (GtkWidget *widget, GdkEventScroll *event,
                     GSCHEM_TOPLEVEL *w_current)
{
  GtkAdjustment *adj;
  gboolean pan_xaxis = FALSE;
  gboolean pan_yaxis = FALSE;
  gboolean zoom = FALSE;
  int pan_direction = 1;
  int zoom_direction = ZOOM_IN;

  exit_if_null(w_current);
  global_window_current = w_current;

  /* update the state of the modifiers */
  w_current->SHIFTKEY   = (event->state & GDK_SHIFT_MASK  ) ? 1 : 0;
  w_current->CONTROLKEY = (event->state & GDK_CONTROL_MASK) ? 1 : 0;
  w_current->ALTKEY     = (event->state & GDK_MOD1_MASK) ? 1 : 0;

  if (w_current->scroll_wheel == SCROLL_WHEEL_CLASSIC) {
    /* Classic gschem behaviour */
    zoom =      !w_current->CONTROLKEY && !w_current->SHIFTKEY;
    pan_yaxis = !w_current->CONTROLKEY &&  w_current->SHIFTKEY;
    pan_xaxis =  w_current->CONTROLKEY && !w_current->SHIFTKEY;
  } else {
    /* GTK style behaviour */
    zoom =       w_current->CONTROLKEY && !w_current->SHIFTKEY;
    pan_yaxis = !w_current->CONTROLKEY && !w_current->SHIFTKEY;
    pan_xaxis = !w_current->CONTROLKEY &&  w_current->SHIFTKEY;
  }

  /* You must have scrollbars enabled if you want to use the scroll wheel to pan */
  if (!w_current->scrollbars_flag) {
    pan_xaxis = FALSE;
    pan_yaxis = FALSE;
  }

  switch (event->direction) {
    case GDK_SCROLL_UP:
    case GDK_SCROLL_LEFT:
      pan_direction = -1;
      zoom_direction = ZOOM_IN;
      break;
    case GDK_SCROLL_DOWN:
    case GDK_SCROLL_RIGHT:
      pan_direction =  1;
      zoom_direction = ZOOM_OUT;
      break;
  }

  if (zoom) {
    /*! \todo Change "HOTKEY" TO new "MOUSE" specifier? */
    a_zoom(w_current, zoom_direction, HOTKEY, 0);
  }

  if (pan_xaxis) {
    adj = gtk_range_get_adjustment(GTK_RANGE(w_current->h_scrollbar));
    gtk_adjustment_set_value(adj, min(adj->value + pan_direction *
                                        (adj->page_increment / 4),
                                      adj->upper - adj->page_size));
  }

  if (pan_yaxis) {
    adj = gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));
    gtk_adjustment_set_value(adj, min(adj->value + pan_direction *
                                        (adj->page_increment / 4),
                                      adj->upper - adj->page_size));
  }

  if (w_current->undo_panzoom && (zoom || pan_xaxis || pan_yaxis)) {
    o_undo_savestate(w_current, UNDO_VIEWPORT_ONLY);
  }

  return 0;
}
