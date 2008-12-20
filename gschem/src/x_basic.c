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

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_repaint_background_region (GSCHEM_TOPLEVEL *w_current,
                                  int x, int y, int width, int height)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  gdk_gc_set_foreground (w_current->gc,
                         x_get_color (toplevel->background_color));

  gdk_draw_rectangle (w_current->drawable,
                      w_current->gc, TRUE, x, y, width, height);

  x_grid_draw_region (w_current, x, y, width, height);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_hscrollbar_set_ranges(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
        GtkAdjustment        *hadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

	hadjustment =
		gtk_range_get_adjustment(GTK_RANGE(w_current->h_scrollbar));

	hadjustment->lower = toplevel->init_left;
	hadjustment->upper = toplevel->init_right;

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_hscrollbar_update(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GtkAdjustment *hadjustment;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  if (w_current->h_scrollbar == NULL) {
    return;
  }

  hadjustment = gtk_range_get_adjustment (GTK_RANGE (
                                                     w_current->h_scrollbar));

  hadjustment->value = toplevel->page_current->left;

  hadjustment->page_size = fabs(toplevel->page_current->right -
                                toplevel->page_current->left);

  hadjustment->page_increment = hadjustment->page_size - 100.0;

#if DEBUG
  printf("H %f %f\n", hadjustment->lower, hadjustment->upper);
  printf("Hp %f\n", hadjustment->page_size);
#endif

  gtk_signal_emit_by_name(GTK_OBJECT(hadjustment), "changed");
  gtk_signal_emit_by_name(GTK_OBJECT(hadjustment), "value_changed");
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_vscrollbar_set_ranges(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GtkAdjustment *vadjustment;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  vadjustment =
  gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));

  vadjustment->lower = toplevel->init_top;
  vadjustment->upper = toplevel->init_bottom;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_vscrollbar_update(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GtkAdjustment *vadjustment;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  if (w_current->v_scrollbar == NULL) {
    return;
  }

  vadjustment =
  gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));

  vadjustment->page_size = fabs(toplevel->page_current->bottom -
                                toplevel->page_current->top);

  vadjustment->page_increment = vadjustment->page_size - 100.0;

  vadjustment->value =
  toplevel->init_bottom - toplevel->page_current->bottom;

#if DEBUG
  printf("V %f %f\n", vadjustment->lower, vadjustment->upper);
  printf("Vp %f\n", vadjustment->page_size);
#endif

  gtk_signal_emit_by_name(GTK_OBJECT(vadjustment), "changed");
  gtk_signal_emit_by_name(GTK_OBJECT(vadjustment), "value_changed");
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_scrollbars_update(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  toplevel->DONT_REDRAW = 1;
  x_hscrollbar_update(w_current);
  x_vscrollbar_update(w_current);
  toplevel->DONT_REDRAW = 0;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_basic_warp_cursor (GtkWidget* widget, gint x, gint y)
{
  GdkScreen *screen;
  GdkDisplay *display;
  int window_x, window_y;

  gdk_window_get_origin (widget->window, &window_x, &window_y);

  screen = gtk_widget_get_screen (widget);
  display = gdk_screen_get_display (screen);

  gdk_display_warp_pointer (display, screen, window_x + x, window_y + y);
}
