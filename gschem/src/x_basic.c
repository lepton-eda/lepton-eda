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

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#ifndef __MINGW32__
#include <gdk/gdkx.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_repaint_background(TOPLEVEL *w_current)
{
  if (!w_current->inside_redraw) {
    gdk_gc_set_foreground(
                          w_current->gc,
                          x_get_color(w_current->background_color));

    gdk_draw_rectangle(w_current->window,
                       w_current->gc, TRUE, 0, 0,
                       w_current->win_width,
                       w_current->win_height);

    gdk_draw_rectangle(w_current->backingstore,
                       w_current->gc, TRUE, 0, 0,
                       w_current->win_width,
                       w_current->win_height);

    x_grid_draw(w_current);

  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_hscrollbar_set_ranges(TOPLEVEL *w_current)
{
        GtkAdjustment        *hadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

	hadjustment =
		gtk_range_get_adjustment(GTK_RANGE(w_current->h_scrollbar));

	hadjustment->lower = w_current->init_left;
	hadjustment->upper = w_current->init_right;

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_hscrollbar_update(TOPLEVEL *w_current)
{
  GtkAdjustment *hadjustment;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  if (w_current->h_scrollbar == NULL) {
    return;
  }

  hadjustment = gtk_range_get_adjustment (GTK_RANGE (
                                                     w_current->h_scrollbar));

  hadjustment->value = w_current->page_current->left;

  hadjustment->page_size = fabs(w_current->page_current->right -
                                w_current->page_current->left);

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
void x_vscrollbar_set_ranges(TOPLEVEL *w_current)
{
  GtkAdjustment *vadjustment;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  vadjustment =
  gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));

  vadjustment->lower = w_current->init_top;
  vadjustment->upper = w_current->init_bottom;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_vscrollbar_update(TOPLEVEL *w_current)
{
  GtkAdjustment *vadjustment;

  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  if (w_current->v_scrollbar == NULL) {
    return;
  }

  vadjustment =
  gtk_range_get_adjustment(GTK_RANGE(w_current->v_scrollbar));

  vadjustment->page_size = fabs(w_current->page_current->bottom -
                                w_current->page_current->top);

  vadjustment->page_increment = vadjustment->page_size - 100.0;

  vadjustment->value =
  w_current->init_bottom - w_current->page_current->bottom;

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
void x_scrollbars_update(TOPLEVEL *w_current)
{
  if (w_current->scrollbars_flag == FALSE) {
    return;
  }

  w_current->DONT_RECALC = 1;
  w_current->DONT_RESIZE = 1;
  w_current->DONT_REDRAW = 1;
  x_hscrollbar_update(w_current);
  x_vscrollbar_update(w_current);
  w_current->DONT_REDRAW = 0;
  w_current->DONT_RECALC = 0;
  w_current->DONT_RESIZE = 0;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_basic_warp_cursor(GtkWidget* widget, gint x, gint y, gboolean relative)
{
#ifndef __MINGW32__
   XWarpPointer(GDK_WINDOW_XDISPLAY(widget->window), None, 
                GDK_WINDOW_XWINDOW(widget->window),0,0,0,0,x,y);
#endif
}
