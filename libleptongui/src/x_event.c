/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#include <math.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "schematic.h"
#include <gdk/gdkkeysyms.h>


/* used for the stroke stuff */
#ifdef HAVE_LIBSTROKE
static int DOING_STROKE = FALSE;
#endif /* HAVE_LIBSTROKE */

gboolean
schematic_event_get_doing_stroke ()
{
#ifdef HAVE_LIBSTROKE
  return DOING_STROKE;
#else
  return FALSE;
#endif /* HAVE_LIBSTROKE */
}

void
schematic_event_set_doing_stroke (gboolean val)
{
#ifdef HAVE_LIBSTROKE
  DOING_STROKE = val;
#endif /* HAVE_LIBSTROKE */
}


gint
schematic_event_shift_mask ()
{
  return GDK_SHIFT_MASK;
}


gint
schematic_event_control_mask ()
{
  return GDK_CONTROL_MASK;
}


gint
schematic_event_alt_mask ()
{
  return GDK_MOD1_MASK;
}

gboolean
schematic_event_is_double_button_press (GdkEvent *event)
{
#ifdef ENABLE_GTK3
  return gdk_event_get_event_type (event) == GDK_2BUTTON_PRESS;
#else
  return ((GdkEventButton*) event)->type == GDK_2BUTTON_PRESS;
#endif
}


guint
schematic_event_get_button (GdkEvent *event)
{
#ifdef ENABLE_GTK3
  guint button;
  gdk_event_get_button (event, &button);
  return button;
#else
  return ((GdkEventButton*) event)->button;
#endif
}


/*! \brief Check if a moving event has to be skipped.
 *
 *  \par Function Description
 *
 *  The function checks if there are more moving events in the GDK
 *  event queue.  If the event in the queue is a motion event and
 *  has the same state (depressed buttons and modifiers are the
 *  same) as \a event has, the function returns TRUE.  Otherwise
 *  it returns FALSE.
 *
 *  \param event The event to compare state with.
 *  \returns TRUE if event in the queue matches, otherwise FALSE.
 */

gboolean
schematic_event_skip_motion_event (GdkEvent *event)
{
  gboolean skip_event = FALSE;
  GdkEvent *test_event;

  if ((test_event = gdk_event_get()) != NULL)
  {
    GdkModifierType state;
    gdk_event_get_state (event, &state);

    /* Only skip the event if it is a motion event and no buttons
     * or modifier keys changed. */
    if (test_event->type == GDK_MOTION_NOTIFY
        && ((GdkEventMotion *) test_event)->state == state)
    {
      skip_event = TRUE;
    }
    /* Put it back in front of the queue. */
    gdk_event_put (test_event);
    gdk_event_free (test_event);
  }

  return skip_event;
}


/*! \brief Updates the display when drawing area is configured.
 *  \par Function Description
 *  This is the callback function connected to the configure event of
 *  the SchematicCanvas of the main window.
 *
 *  It re-pans each of its pages to keep their contents centered in the
 *  SchematicCanvas.
 *
 *  When the window is maximised, the zoom of every page is changed to
 *  best fit the previously displayed area of the page in the new
 *  area. Otherwise the current zoom level is left unchanged.
 *
 *  \param [in] page_view The SchematicCanvas which received the signal.
 *  \param [in] event     The event structure of signal configure-event.
 *  \param [in] unused
 *  \returns FALSE to propagate the event further.
 */
gboolean
x_event_configure (SchematicCanvas   *page_view,
                   GdkEventConfigure *event,
                   gpointer           unused)
{
  GtkAllocation current_allocation;
  GList *iter;
  LeptonPage *p_current = schematic_canvas_get_page (page_view);

  if (p_current == NULL) {
    /* don't want to call this if the current page isn't setup yet */
    return FALSE;
  }

  LeptonToplevel *toplevel = lepton_page_get_toplevel (p_current);
  g_return_val_if_fail (toplevel != NULL, FALSE);

  gtk_widget_get_allocation (GTK_WIDGET(page_view), &current_allocation);

  if ((current_allocation.width == page_view->previous_allocation.width) &&
      (current_allocation.height == page_view->previous_allocation.height)) {
    /* the size of the drawing area has not changed -- nothing to do here */
    return FALSE;
  }

  page_view->previous_allocation = current_allocation;


  /* tabbed GUI: zoom/pan, mark page_view as configured and return:
   * there is only one page per page view.
  */
  if (x_tabs_enabled())
  {
    if (page_view->configured)
    {
      schematic_canvas_pan_mouse (page_view, 0, 0);
    }
    else
    {
      schematic_canvas_zoom_extents (page_view, NULL);
    }

    page_view->configured = TRUE;
    return FALSE;
  }


  /* re-pan each page of the LeptonToplevel */
  for ( iter = lepton_list_get_glist (toplevel->pages);
        iter != NULL;
        iter = g_list_next (iter) ) {

    schematic_canvas_set_page (page_view, (LeptonPage *)iter->data);

    if (page_view->configured) {
      schematic_canvas_pan_mouse (page_view, 0, 0);
    } else {
      schematic_canvas_zoom_extents (page_view, NULL);
    }
  }

  page_view->configured = TRUE;

  schematic_canvas_set_page (page_view, p_current);

  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint
x_event_enter (GtkWidget *widget,
               GdkEventCrossing *event,
               SchematicWindow *w_current)
{
  g_return_val_if_fail ((w_current != NULL), 0);
  /* do nothing or now */
  return(0);
}

/*! \brief Callback to handle key events in the drawing area.
 *  \par Function Description
 *
 *  GTK+ callback function (registered in x_window_setup_draw_events() ) which
 *  handles key press and release events from the GTK+ system.
 *
 * \param [in] page_view  The #SchematicCanvas widget that generated
 *                        the event.
 * \param [in] event      The event itself.
 * \param      w_current  The toplevel environment.
 * \returns TRUE if the event has been handled.
 */
GdkEventKey*
x_event_key (SchematicCanvas *page_view,
             GdkEventKey *event,
             SchematicWindow *w_current)
{
  int pressed;
  gboolean special = FALSE;

  g_return_val_if_fail (page_view != NULL, FALSE);

#if DEBUG
  printf("x_event_key_pressed: Pressed key %i.\n", event->keyval);
#endif

  /* update the state of the modifiers */
  schematic_window_set_alt_key_pressed (w_current, (event->state & GDK_MOD1_MASK) ? 1 : 0);
  schematic_window_set_shift_key_pressed (w_current, (event->state & GDK_SHIFT_MASK) ? 1 : 0);
  schematic_window_set_control_key_pressed (w_current, (event->state & GDK_CONTROL_MASK) ? 1 : 0);

  pressed = (event->type == GDK_KEY_PRESS) ? 1 : 0;

  switch (event->keyval) {
    case GDK_KEY_Alt_L:
    case GDK_KEY_Alt_R:
      schematic_window_set_alt_key_pressed (w_current, pressed);
      break;

    case GDK_KEY_Shift_L:
    case GDK_KEY_Shift_R:
      schematic_window_set_shift_key_pressed (w_current, pressed);
      special = TRUE;
      break;

    case GDK_KEY_Control_L:
    case GDK_KEY_Control_R:
      schematic_window_set_control_key_pressed (w_current, pressed);
      special = TRUE;
      break;
  }

  /* Special case to update the object being drawn or placed after
   * scrolling when Shift or Control were pressed */
  if (special) {
    x_event_faked_motion (page_view, event);
  }

  return pressed ? event : NULL;
}

/* Helper function for GTK2 port which doesn't have the getter for
   event scroll direction. */
GdkScrollDirection
schematic_event_get_scroll_direction (GdkEventScroll *event)
{
  return event->direction;
}


static guint last_scroll_event_time = GDK_CURRENT_TIME;

guint
schematic_event_get_last_scroll_event_time ()
{
  return last_scroll_event_time;
}

void
schematic_event_set_last_scroll_event_time (guint val)
{
  last_scroll_event_time = val;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] widget The SchematicCanvas with the scroll event.
 *  \param [in] w_current
 *  \param [in] zoom
 *  \param [in] pan_yaxis
 *  \param [in] pan_xaxis
 */
gint
x_event_scroll (GtkWidget *widget,
                SchematicWindow *w_current,
                gboolean zoom,
                gboolean pan_xaxis,
                gboolean pan_yaxis)
{
  SchematicCanvas *view = NULL;

  view = SCHEMATIC_CANVAS (widget);

  if (schematic_window_get_undo_panzoom (w_current) &&
      (zoom || pan_xaxis || pan_yaxis))
  {
    o_undo_savestate_viewport (w_current);
  }

  x_event_faked_motion (view, NULL);
  /* Stop further processing of this signal */
  return TRUE;
}


/*! \brief get the pointer position of a given SchematicWindow
 *  \par Function Description
 *  This function gets the pointer position of the drawing area of the
 *  current workspace <b>SchematicWindow</b>. The flag <b>snapped</b> specifies
 *  whether the pointer position should be snapped to the current grid.
 *
 *  \param [in] w_current  The SchematicWindow object.
 *  \param [in] snapped    An option flag to specify the wished coords
 *  \param [out] wx        snapped/unsnapped world x coordinate
 *  \param [out] wy        snapped/unsnapped world y coordinate
 *
 *  \return Returns TRUE if the pointer position is inside the drawing area.
 *
 */
gboolean
x_event_get_pointer_position (SchematicWindow *w_current,
                              gboolean snapped,
                              gint *wx,
                              gint *wy)
{
  int width;
  int height;
  int sx;
  int sy;
  int x;
  int y;

  SchematicCanvas *page_view = schematic_window_get_current_canvas (w_current);
  g_return_val_if_fail (page_view != NULL, FALSE);

  GdkWindow *window = gtk_widget_get_window (GTK_WIDGET (page_view));
  g_return_val_if_fail (window != NULL, FALSE);

  width = gdk_window_get_width (window);
  height = gdk_window_get_height (window);

#ifdef ENABLE_GTK3
  GdkDisplay *display = gdk_window_get_display (window);
  GdkSeat *seat = gdk_display_get_default_seat (display);
  GdkDevice *pointer = gdk_seat_get_pointer (seat);

  gdk_window_get_device_position (window, pointer, &sx, &sy, NULL);
#else
  gtk_widget_get_pointer(GTK_WIDGET (page_view), &sx, &sy);
#endif

  /* check if we are inside the drawing area */
  if ((sx < 0) || (sx >= width) || (sy < 0) || (sy >= height)) {
    return FALSE;
  }

  schematic_canvas_SCREENtoWORLD (page_view, sx, sy, &x, &y);

  if (snapped) {
    x = snap_grid (w_current, x);
    y = snap_grid (w_current, y);
  }

  *wx = x;
  *wy = y;

  return TRUE;
}

/*! \brief Emits a faked motion event to update objects being drawn or placed
 *  \par Function Description
 *  This function emits an additional "motion-notify-event" to
 *  update objects being drawn or placed while zooming, scrolling, or
 *  panning.
 *
 *  If its event parameter is not NULL, the current state of Shift
 *  and Control is preserved to correctly deal with special cases.
 *
 *  \param [in] view      The SchematicCanvas object which received the signal.
 *  \param [in] event     The event structure of the signal or NULL.
 *  \returns FALSE to propagate the event further.
 */
gboolean
x_event_faked_motion (SchematicCanvas *view, GdkEventKey *event)
{
  gint x, y;
  gboolean ret;
  GdkEventMotion *newevent;

#ifdef ENABLE_GTK3
  GdkWindow *window = gtk_widget_get_window (GTK_WIDGET (view));
  g_return_val_if_fail (window != NULL, FALSE);

  GdkDisplay *display = gdk_window_get_display (window);
  GdkSeat *seat = gdk_display_get_default_seat (display);
  GdkDevice *pointer = gdk_seat_get_pointer (seat);

  gdk_window_get_device_position (window, pointer, &x, &y, NULL);
#else
  gtk_widget_get_pointer (GTK_WIDGET (view), &x, &y);
#endif
  newevent = (GdkEventMotion*)gdk_event_new(GDK_MOTION_NOTIFY);
  newevent->x = x;
  newevent->y = y;

  if (event != NULL ) {
    switch (event->keyval) {
      case GDK_KEY_Control_L:
      case GDK_KEY_Control_R:
        if (event->type == GDK_KEY_PRESS) {
          newevent->state |= GDK_CONTROL_MASK;
        } else {
          newevent->state &= ~GDK_CONTROL_MASK;
        }
        break;

      case GDK_KEY_Shift_L:
      case GDK_KEY_Shift_R:
        if (event->type == GDK_KEY_PRESS) {
          newevent->state |= GDK_SHIFT_MASK;
        } else {
          newevent->state &= ~GDK_SHIFT_MASK;
        }
        break;
    }
  }

  g_signal_emit_by_name (view, "motion-notify-event", newevent, &ret);

  gdk_event_free((GdkEvent*)newevent);

  return FALSE;
}
