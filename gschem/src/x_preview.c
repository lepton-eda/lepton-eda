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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/o_types.h>
#include <libgeda/colors.h>
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/papersizes.h"
#include "../include/x_states.h"
#include "../include/prototype.h"
#include "../include/globals.h"

extern int mouse_x, mouse_y;

void
x_preview_update(TOPLEVEL *preview, char *directory, char *filename) 
{
	char *temp;

#if DEBUB
	printf("%s %s\n", directory, filename);
#endif
	temp = u_basic_strdup_multiple(directory, filename, NULL);

	s_page_free(preview, preview->page_current);

	s_page_new(preview, temp);
	preview->page_current = preview->page_tail;
	f_open(preview, temp);

	a_zoom_limits(preview, preview->page_current->object_head);

	o_redraw_all(preview);
	free(temp);
}

void
x_preview_close(TOPLEVEL *w_current)
{
        o_attrib_free_current(w_current);
        o_complex_free_filename(w_current);

	if (w_current->series_name) {
		free(w_current->series_name);
	}

	if (w_current->untitled_name) {
		free(w_current->untitled_name);
	}

	if (w_current->font_directory) {
		free(w_current->font_directory);
	}

	if (w_current->scheme_directory) {
		free(w_current->scheme_directory);
	}

	
	if (w_current->backingstore) {
                gdk_pixmap_unref(w_current->backingstore);
        }

	x_window_free_gc(w_current);
	s_page_free_all(w_current, w_current->page_tail);
	free(w_current);
	w_current = NULL;
}

gint
x_preview_expose(GtkWidget *widget, GdkEventExpose *event, TOPLEVEL *w_current)
{
        exit_if_null(w_current);

#if DEBUG
        printf("yeah expose: %d %d\n", event->area.width, event->area.height);
#endif

        gdk_draw_pixmap(widget->window,
                        widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
                        w_current->backingstore,
                        event->area.x, event->area.y,
                        event->area.x, event->area.y,
                        event->area.width, event->area.height);

}

gint
x_preview_button_pressed(GtkWidget *widget, GdkEventButton *event,
                       TOPLEVEL *w_current)
{
	exit_if_null(w_current);

	global_window_current = w_current;

#if DEBUG
	printf("preview pressed\n");
#endif

	if (event->button == 1) { 
		i_callback_view_zoom_in_hotkey(w_current, 0, NULL);
	} else if (event->button == 2) {
		i_callback_view_pan_hotkey(w_current, 0, NULL);
	} else if (event->button == 3) {
		i_callback_view_zoom_out_hotkey(w_current, 0, NULL);
	}
}

gint
x_preview_motion(GtkWidget *widget, GdkEventMotion *event, TOPLEVEL *w_current)
{
	mouse_x = (int) event->x;
        mouse_y = (int) event->y;

#if DEBUG
	printf("preview motion\n");
#endif
}

#if 0
gint
x_preview_button_released(GtkWidget *widget, GdkEventButton *event,
                       TOPLEVEL *w_current)
{
	exit_if_null(w_current);

	global_window_current = w_current;
	printf("preview released\n");
}
#endif


gint
x_preview_key_press (GtkWidget *widget, GdkEventKey *event, TOPLEVEL *w_current)
{
	char *string;
        exit_if_null(w_current);
        global_window_current = w_current;

	if (event->keyval == 0) {
                return;
        }

}

void
x_preview_create_drawing(GtkWidget *drawbox, TOPLEVEL *w_current)
{
        /* drawing next */
        w_current->drawing_area = gtk_drawing_area_new ();
        /* Set the size here.  Be sure that it has an aspect ratio of 1.333
         * We could calculate this based on root window size, but for now
         * lets just set it to:
         * Width = root_width*3/4   Height = Width/1.3333333333
         * 1.3333333 is the desired aspect ratio!
         */

        gtk_drawing_area_size (GTK_DRAWING_AREA (w_current->drawing_area),
                               w_current->win_width,
                               w_current->win_height);

        gtk_box_pack_start (GTK_BOX (drawbox), w_current->drawing_area,
                               FALSE, FALSE, 0);
        gtk_widget_show (w_current->drawing_area);

}

void
x_preview_setup_rest(TOPLEVEL *preview)
{

	preview->window = preview->drawing_area->window;
	gtk_widget_grab_focus(preview->drawing_area);

	preview->backingstore = gdk_pixmap_new(
			preview->window,
                        preview->drawing_area->
			allocation.width,
                        preview->drawing_area->
			allocation.height, -1);


	x_window_setup_gc(preview);
        x_window_setup_rest(preview);

	s_page_add_head(preview);

	preview->page_tail = s_page_add(preview,
                        preview->page_tail,
                        "unknown");

	s_page_setup(preview->page_tail);
	preview->page_current = preview->page_tail;

	/* Special case init */
        /* move this elsewhere eventually */
        /* w_current->page_current->object_parent=NULL; not
         * this one */
        preview->page_current->object_lastplace=NULL;
        preview->page_current->object_selected=NULL;

	set_window(preview, preview->init_left,
                           preview->init_right,
                           preview->init_top,
                           preview->init_bottom);

#if DEBUG
        printf("%d %d %d %d\n", preview->init_left,
                           preview->init_right,
                           preview->init_top,
                           preview->init_bottom);
#endif

	i_vars_set(preview);

	/* be sure to turn off the grid */
	preview->grid = FALSE;


	x_repaint_background(preview);

/*	
	f_open(preview, "/home/ahvezda/devel/gschem/examples/filter_1.sch");
	f_open(preview, "/home/ahvezda/devel/symbols/74/74191-1.sym");
*/
	
#if 0	
		world_get_complex_bounds(preview, 
					 preview->page_current->object_head, 
					 &left, &top, &right, &bottom);
		set_window(preview, left, right, top, bottom);
#endif
	a_zoom_limits(preview, preview->page_current->object_head);

	o_redraw_all(preview);
}




TOPLEVEL *
x_preview_setup(GtkWidget *xfwindow, GtkWidget *drawbox) 
{
	TOPLEVEL *preview_window;

	preview_window = (TOPLEVEL *) malloc(sizeof(TOPLEVEL));
	preview_window->wid = -1;

	preview_window->init_left = 0;
        preview_window->init_top = 0;
	preview_window->init_right = WIDTH_C;
        preview_window->init_bottom = HEIGHT_C;
        x_window_setup_world(preview_window);
        preview_window->width = 160;
        preview_window->height = 120;
        preview_window->win_width = preview_window->width;
        preview_window->win_height = preview_window->height;
	/* be sure to turn off scrollbars */
	preview_window->scrollbars_flag = FALSE;
        x_preview_create_drawing(drawbox, preview_window);

	preview_window->filename_label = NULL;
        preview_window->h_scrollbar = NULL;
        preview_window->v_scrollbar = NULL;

	
        gtk_widget_set_events (preview_window->drawing_area, 
			       GDK_EXPOSURE_MASK | 
			       GDK_POINTER_MOTION_MASK |
			       GDK_BUTTON_PRESS_MASK);

	gtk_signal_connect (GTK_OBJECT (preview_window->drawing_area),
                            "expose_event",
                            GTK_SIGNAL_FUNC (x_preview_expose),
                            preview_window);

	gtk_signal_connect (GTK_OBJECT (preview_window->drawing_area),
                            "button_press_event",
                            (GtkSignalFunc) x_preview_button_pressed,
                            preview_window);

#if 0
	gtk_signal_connect (GTK_OBJECT (preview_window->drawing_area),
                            "button_release_event",
                            (GtkSignalFunc) x_preview_button_released,
                            preview_window);
	gtk_signal_connect (GTK_OBJECT (preview_window->drawing_area),
                            "key_press_event",
                            (GtkSignalFunc) x_preview_key_press,
                            preview_window);
#endif

	gtk_signal_connect (GTK_OBJECT (preview_window->drawing_area),
                            "motion_notify_event",
                            (GtkSignalFunc) x_preview_motion,
                            preview_window);



	return(preview_window);	
}


