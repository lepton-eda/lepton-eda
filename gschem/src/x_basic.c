/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
 * Copyright (C) 1998 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <signal.h>
#include <math.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>


#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/x_event.h"
#include "../include/prototype.h"


GdkColor *
x_get_color(int color)
{

	switch(color) {
	
		case(RED):
			return(&red);
			break;

		case(BLUE):
			return(&blue);
			break;

		case(GREEN):
			return(&green);
			break;

		case(YELLOW):
			return(&yellow);
			break;

		case(CYAN):
			return(&cyan);
			break;

		case(GREY):
			return(&grey);
			break;

		case(GREY90):
			return(&grey90);
			break;

		case(BLACK):
			return(&black);
			break;

		case(WHITE):
			return(&white);
			break;

		default:
			return(&white);
			break;
	}

}

GdkColor *
x_get_darkcolor(int color)
{
	switch(color) {
	
		case(RED):
			return(&darkred);
			break;

		case(BLUE):
			return(&darkblue);
			break;

		case(GREEN):
			return(&darkgreen);
			break;

		case(YELLOW):
			return(&darkyellow);
			break;

		case(CYAN):
			return(&darkcyan);
			break;

		case(GREY):
			return(&darkgrey);
			break;

		case(BLACK):
			return(&black);
			break;

		case(WHITE):
			return(&grey); /* no such thing as darkwhite */
			break;

		default:
			return(&white);
			break;
	}
}

void
x_repaint_background(TOPLEVEL *w_current)
{
        if (!w_current->inside_redraw) {
		gdk_gc_set_foreground(w_current->gc, 
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

void
x_hscrollbar_set_ranges(TOPLEVEL *w_current)
{
        GtkAdjustment        *hadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

	hadjustment = gtk_range_get_adjustment (GTK_RANGE (
				w_current->h_scrollbar));

	hadjustment->lower = 0.0;
	hadjustment->upper = w_current->init_right;

}

void
x_hscrollbar_update(TOPLEVEL *w_current)
{
        GtkAdjustment        *hadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}
	
	if (w_current->h_scrollbar == NULL)
		return;

	hadjustment = gtk_range_get_adjustment (GTK_RANGE (
				w_current->h_scrollbar));

	hadjustment->value = w_current->page_current->left; 

	hadjustment->page_size = fabs(
			w_current->page_current->right - w_current->page_current->left); 

	hadjustment->page_increment = hadjustment->page_size - 100.0;

#if DEBUG
	printf("H %f %f\n", hadjustment->lower, hadjustment->upper);
	printf("Hp %f\n", hadjustment->page_size);
#endif

	gtk_signal_emit_by_name (GTK_OBJECT (hadjustment), "changed");
	gtk_signal_emit_by_name (GTK_OBJECT (hadjustment), "value_changed");
}

void
x_vscrollbar_set_ranges(TOPLEVEL *w_current)
{
	GtkAdjustment        *vadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

	vadjustment = gtk_range_get_adjustment (GTK_RANGE (
						w_current->v_scrollbar));

	vadjustment->lower = 0.0;
	vadjustment->upper = w_current->init_bottom;
}


void
x_vscrollbar_update(TOPLEVEL *w_current)
{
	GtkAdjustment        *vadjustment;

	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

	if (w_current->v_scrollbar == NULL)
		return;

	vadjustment = gtk_range_get_adjustment (GTK_RANGE (
						w_current->v_scrollbar));

	vadjustment->page_size = fabs(w_current->page_current->bottom - w_current->page_current->top);

	vadjustment->page_increment = vadjustment->page_size - 100.0;

	vadjustment->value = w_current->init_bottom - w_current->page_current->bottom;

#if DEBUG
	printf("V %f %f\n", vadjustment->lower, vadjustment->upper);
	printf("Vp %f\n", vadjustment->page_size);
#endif

        gtk_signal_emit_by_name (GTK_OBJECT (vadjustment), "changed");
        gtk_signal_emit_by_name (GTK_OBJECT (vadjustment), "value_changed");
}

void
x_scrollbars_update(TOPLEVEL *w_current)
{
	if (w_current->scrollbars_flag == FALSE) {
		return;
	}

        w_current->DONT_REDRAW=1;
        w_current->DONT_RECALC=1;
        w_current->DONT_RESIZE=1;
        x_hscrollbar_update(w_current);
        x_vscrollbar_update(w_current);
        w_current->DONT_REDRAW=0;
        w_current->DONT_RECALC=0;
        w_current->DONT_RESIZE=0;
}
