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
#include <signal.h>

#include <libgeda/libgeda.h>

#include "../include/papersizes.h"
#include "../include/x_event.h"
#include "../include/i_vars.h"
#include "../include/x_states.h"
#include "../include/globals.h"
#include "../include/prototype.h"

/* global_wid always increments, it needs to be unique per gschem run */
static int num_windows=0;
static int global_wid=0;

/* head pointer to window structure, this points to all the windows that
   currently exist */
static TOPLEVEL *window_head=NULL;
static TOPLEVEL *window_tail=NULL;

/* add to the end of the list */
TOPLEVEL *
x_window_add(TOPLEVEL *w_head, TOPLEVEL *w_current)
{
	if (w_head == NULL) {
		w_current->prev = NULL;
		w_current->next = NULL;
		return(w_current);
	} else {
		w_current->prev = w_head;
		w_current->next = NULL;
		w_head->next = w_current;
		return(w_head->next);
	}
}

void
x_window_add_head()
{
	window_tail = window_head = (TOPLEVEL *) malloc(sizeof(TOPLEVEL));
	window_head->wid = -1;
}

void
x_window_free_head()
{
	free(window_head);
}

/* deletes specified window from w_head list */
/* doesn't do the actual destroy though */
void
x_window_delete(TOPLEVEL *w_head, TOPLEVEL *w_current)
{

	if (w_head == NULL || w_current == NULL) {
		/* error condition hack */
		return;
	}

	if (w_current->next)
		w_current->next->prev = w_current->prev;

        if (w_current->prev)
		w_current->prev->next = w_current->next;

	s_page_free_all(w_current, w_current->page_tail);

}

void
x_window_setup_world(TOPLEVEL *w_current)
{
        w_current->init_left = 0;
        w_current->init_top = 0;
	/* init_right and _bottom are set before this function is called */
        w_current->min_zoom = 0;
        w_current->max_zoom = 256;  /* was 128 */

	if (w_current->display_width <= 800) {
/* old way */
#if 0
		w_current->width = 700;
		w_current->height = 525;
#endif

/* new way */
		w_current->width = 672;
		w_current->height = 504;
	} else {
		w_current->width = 800;
		w_current->height = 600;
	}

        w_current->starting_width = w_current->width;

	w_current->win_width = w_current->width;
        w_current->win_height = w_current->height;

/* part of page mechanism addition commented out
	w_current->zoom_factor = 0;
*/
}

void
x_window_setup_rest(TOPLEVEL *w_current)
{
	w_current->num_untitled=0;

	w_current->start_x=-1;
	w_current->start_y=-1;
	w_current->save_x=-1;
	w_current->save_y=-1;
	w_current->last_x=-1;
	w_current->last_y=-1;
	w_current->loc_x=-1;
	w_current->loc_y=-1;
	w_current->distance=-1;
	w_current->event_state=SELECT;
	w_current->inside_action=0;
	w_current->snap=1;
	w_current->grid=1;

	w_current->current_attribute=NULL;
	w_current->current_visible=-1; /* not sure on these */
	w_current->current_show=-1;

	w_current->internal_basename=NULL;
	w_current->internal_clib=NULL;

	w_current->series_name = NULL;
	w_current->untitled_name = NULL;
	w_current->font_directory = NULL;
	w_current->scheme_directory = NULL;

/* part of page mechanism addition commented out
	w_current->zoom_factor=0;
*/
	w_current->override_color=-1;
	w_current->inside_redraw=0;

	/* Don't init these to zero here... once we are done with all init
	 *  will these be inited to zero
	 * w_current->DONT_DRAW_CONN=0;
	 * w_current->DONT_RESIZE=0;
	 * w_current->DONT_EXPOSE=0;
	 * w_current->DONT_REDRAW=0;
	 * w_current->DONT_RECALC=0;
	 */

	w_current->FORCE_CONN_UPDATE=0;
	w_current->ADDING_SEL=0;
	w_current->REMOVING_SEL=0;

	w_current->drawbounding_action_mode=FREE;
	w_current->last_drawb_mode = -1;
	w_current->CONTROLKEY=0;
	w_current->SHIFTKEY=0;
	w_current->last_callback=NULL;

	w_current->status_label = NULL;
	w_current->middle_label = NULL;
	w_current->filename_label = NULL;

	w_current->cswindow = NULL;
	w_current->aswindow = NULL;
	w_current->fowindow = NULL;
	w_current->sowindow = NULL;
	w_current->fswindow = NULL;

	w_current->tiwindow = NULL;
	w_current->tewindow = NULL;
	w_current->exwindow = NULL;
	w_current->aawindow = NULL;
	w_current->trwindow = NULL;
	w_current->tswindow = NULL;
	w_current->pswindow = NULL;
	w_current->pwindow = NULL;
	w_current->iwindow = NULL;
	w_current->abwindow = NULL;
	w_current->hkwindow = NULL;
	w_current->cowindow = NULL;
	w_current->clwindow = NULL;
	w_current->fileselect[FILESELECT].xfwindow = NULL;
	w_current->fileselect[FILESELECT].directory = NULL;
	w_current->fileselect[FILESELECT].filename = NULL;
	x_fileselect_init_list_buffers(&w_current->fileselect[FILESELECT]);
	w_current->fileselect[COMPSELECT].xfwindow = NULL;
	w_current->fileselect[COMPSELECT].directory = NULL;
	w_current->fileselect[COMPSELECT].filename = NULL;
	x_fileselect_init_list_buffers(&w_current->fileselect[COMPSELECT]);

	w_current->coord_world = NULL;
	w_current->coord_screen = NULL;
	/* w_current->preview = NULL;experimental widget */

}

void
x_window_setup_colors(void)
{
	int ret;

	gdk_color_parse("black", &black);
	ret = gdk_color_alloc(colormap, &black);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color black!\n");
		exit(-1);
	}

	gdk_color_parse("white", &white);
	ret = gdk_color_alloc(colormap, &white);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color white!\n");
		exit(-1);
	}

	x_color_allocate_all();

#if 0
	gdk_color_parse("grey", &grey);
	ret = gdk_color_alloc(colormap, &grey);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color grey!\n");
		exit(-1);
	}

	gdk_color_parse("grey90", &grey90);
	ret = gdk_color_alloc(colormap, &grey90);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color grey90!\n");
		exit(-1);
	}

	gdk_color_parse("yellow", &yellow);
	ret = gdk_color_alloc(colormap, &yellow);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color yellow!\n");
		exit(-1);
	}

	gdk_color_parse("cyan", &cyan);
	ret = gdk_color_alloc(colormap, &cyan);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color cyan!\n");
		exit(-1);
	}

	gdk_color_parse("red", &red);
	ret = gdk_color_alloc(colormap, &red);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color red!\n");
		exit(-1);
	}

	gdk_color_parse("blue", &blue);
	ret = gdk_color_alloc(colormap, &blue);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color blue!\n");
		exit(-1);
	}

	gdk_color_parse("green", &green);
	ret = gdk_color_alloc(colormap, &green);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color green!\n");
		exit(-1);
	}

	gdk_color_parse("red3", &darkred);
	ret = gdk_color_alloc(colormap, &darkred);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color darkred!\n");
		exit(-1);
	}

	gdk_color_parse("blue3", &darkblue);
	ret = gdk_color_alloc(colormap, &darkblue);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color darkblue!\n");
		exit(-1);
	}

	gdk_color_parse("green3", &darkgreen);
	ret = gdk_color_alloc(colormap, &darkgreen);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color darkgreen!\n");
		exit(-1);
	}

	gdk_color_parse("cyan3", &darkcyan);
	ret = gdk_color_alloc(colormap, &darkcyan);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color darkcyan!\n");
		exit(-1);
	}

	gdk_color_parse("darkgrey", &darkgrey);
	ret = gdk_color_alloc(colormap, &darkgrey);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color darkgrey!\n");
		exit(-1);
	}

	gdk_color_parse("yellow3", &darkyellow);
	ret = gdk_color_alloc(colormap, &darkyellow);
	if (ret == 0) {
		fprintf(stderr, "Could not allocate the color darkyellow!\n");
		exit(-1);
	}
#endif
}

void
x_window_free_colors(TOPLEVEL *w_current)
{
	/* to be done later */
}

void
x_window_setup_gc(TOPLEVEL *w_current)
{
	GdkGCValues     values;
	GdkGCValuesMask  values_mask;

	w_current->gc = gdk_gc_new(w_current->window);

	if (w_current->gc == NULL) {
		fprintf(stderr, "Couldn't allocate gc\n");
		exit(-1);
	}

	values.foreground = white;
	values.background = black;

	values.function = GDK_XOR;
	values_mask = GDK_GC_FOREGROUND | GDK_GC_BACKGROUND | GDK_GC_FUNCTION;
	w_current->xor_gc = gdk_gc_new_with_values(w_current->window,
							&values, values_mask);

	if (w_current->xor_gc == NULL) {
		fprintf(stderr, "Couldn't allocate xor_gc\n");
		exit(-1);
	}

	values.foreground = white;
	values.background = black;

	values.function = GDK_XOR;
	values_mask = GDK_GC_FOREGROUND | GDK_GC_BACKGROUND | GDK_GC_FUNCTION;
	w_current->outline_xor_gc = gdk_gc_new_with_values(w_current->window,
							&values, values_mask);

	if (w_current->outline_xor_gc == NULL) {
		fprintf(stderr, "Couldn't allocate outline_xor_gc\n");
		exit(-1);
	}

	values.foreground = white;
	values.background = black;

	values.function = GDK_XOR;
	values.line_style = GDK_LINE_ON_OFF_DASH;
	values_mask = GDK_GC_FOREGROUND | GDK_GC_BACKGROUND |
			GDK_GC_LINE_STYLE | GDK_GC_FUNCTION;

	w_current->bounding_xor_gc = gdk_gc_new_with_values(w_current->window,
							 &values, values_mask);

	if (w_current->bounding_xor_gc == NULL) {
		fprintf(stderr, "Couldn't allocate bounding_xor_gc\n");
		exit(-1);
	}

	w_current->bus_gc = gdk_gc_new(w_current->window);

	if (w_current->bus_gc == NULL) {
		fprintf(stderr, "Couldn't allocate bus_gc\n");
		exit(-1);
	}
}

void
x_window_free_gc(TOPLEVEL *w_current)
{
	gdk_gc_unref(w_current->gc);
	gdk_gc_unref(w_current->xor_gc);
	gdk_gc_unref(w_current->bounding_xor_gc);
	gdk_gc_unref(w_current->outline_xor_gc);
}

void
x_window_create_drawing(GtkWidget *drawbox, TOPLEVEL *w_current)
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
			       TRUE, TRUE, 0);
        gtk_widget_show (w_current->drawing_area);

}

void
x_window_setup_draw_events(TOPLEVEL *w_current)
{

	/* is the configure event type missing here? hack */
	gtk_widget_set_events (w_current->drawing_area,
			       GDK_EXPOSURE_MASK |
			       GDK_POINTER_MOTION_MASK |
                               GDK_BUTTON_PRESS_MASK   |
			       GDK_ENTER_NOTIFY_MASK |
			       GDK_KEY_PRESS_MASK |
                               GDK_BUTTON_RELEASE_MASK);

	gtk_signal_connect (GTK_OBJECT (w_current->drawing_area),
			    "expose_event",
			    GTK_SIGNAL_FUNC (x_event_expose),
			    w_current);

	gtk_signal_connect (GTK_OBJECT (w_current->drawing_area),
			    "button_press_event",
                            (GtkSignalFunc) x_event_button_pressed,
			    w_current);

	gtk_signal_connect (GTK_OBJECT (w_current->drawing_area),
		  	    "button_release_event",
                            (GtkSignalFunc) x_event_button_released,
			    w_current);

	gtk_signal_connect (GTK_OBJECT (w_current->drawing_area),
			    "motion_notify_event",
                            (GtkSignalFunc) x_event_motion,
			    w_current);

	gtk_signal_connect (GTK_OBJECT(w_current->drawing_area),
			   "configure_event",
                           (GtkSignalFunc) x_event_configure,
			   w_current);

	gtk_signal_connect (GTK_OBJECT(w_current->drawing_area),
			   "enter_notify_event",
                           (GtkSignalFunc) x_event_enter,
			   w_current);

	gtk_signal_connect (GTK_OBJECT (w_current->main_window),
			    "key_press_event",
                            (GtkSignalFunc) x_event_key_press,
			    w_current);
}

void
x_window_create_main(TOPLEVEL *w_current)
{
	GtkWidget *label;
	GtkWidget *main_box;
	GtkWidget *menubar;
	GtkWidget *drawbox;
	GtkWidget *bottom_box;

        w_current->main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);

	gtk_widget_set_name (w_current->main_window, "gschem");

	/* We want the widgets to flow around the drawing area, so we don't
  	 * set a size of the main window.  The drawing area's size is fixed,
	 * see below
	 */
  	/* commented out so that window manager prompts for the location
         * gtk_widget_set_uposition (w_current->main_window, 0, 0);
	 */

	/* I could not get the destroy signal to work. always got a: */
	/* Gdk-ERROR **: an x io error occurred */
	/* aborting... */
	/* message */
#if 0
	gtk_signal_connect (GTK_OBJECT (w_current->main_window), "destroy",
                            GTK_SIGNAL_FUNC(i_callback_destroy_wm),
                            w_current);
#endif

	/* this should work fine */
        gtk_signal_connect (GTK_OBJECT (w_current->main_window), "delete_event",
                            GTK_SIGNAL_FUNC (i_callback_close_wm),
                            w_current);

	/* Containers first */
        main_box = gtk_vbox_new(FALSE, 1);
        gtk_container_border_width(GTK_CONTAINER(main_box), 0);
        gtk_container_add(GTK_CONTAINER(w_current->main_window), main_box);
        gtk_widget_show(main_box);

        get_main_menu(w_current, &menubar);
        gtk_box_pack_start(GTK_BOX(main_box), menubar, FALSE, FALSE, 0);
        gtk_widget_show(menubar);

	get_main_popup(w_current, &(w_current->popup_menu));

        drawbox = gtk_hbox_new(FALSE, 0);
        gtk_container_border_width(GTK_CONTAINER(drawbox), 0);
        gtk_container_add(GTK_CONTAINER(main_box), drawbox);
        gtk_widget_show(drawbox);

	x_window_create_drawing(drawbox, w_current);
	x_window_setup_draw_events(w_current);

	if (w_current->scrollbars_flag == TRUE) {
		/* setup scroll bars */
		w_current->v_adjustment =
			gtk_adjustment_new (w_current->init_bottom,
 						0.0, w_current->init_bottom,
						100.0, 100.0, 10.0);

		w_current->v_scrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (
						w_current->v_adjustment));

		gtk_range_set_update_policy (GTK_RANGE (w_current->v_scrollbar),
			  	     GTK_UPDATE_CONTINUOUS);

		gtk_box_pack_start (GTK_BOX (drawbox), w_current->v_scrollbar,
			    FALSE, FALSE, 0);

		gtk_signal_connect (GTK_OBJECT (w_current->v_adjustment),
			    "value_changed",
			    GTK_SIGNAL_FUNC (x_event_vschanged),
			    w_current);

		gtk_widget_show (w_current->v_scrollbar);

		w_current->h_adjustment = gtk_adjustment_new (0.0, 0.0,
						w_current->init_right,
						100.0, 100.0, 10.0);

		w_current->h_scrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (
						     w_current->h_adjustment));

		gtk_range_set_update_policy (GTK_RANGE (w_current->h_scrollbar),
				     GTK_UPDATE_CONTINUOUS);

 		gtk_box_pack_start (GTK_BOX (main_box), w_current->h_scrollbar,
			    FALSE, FALSE, 0);

		gtk_signal_connect (GTK_OBJECT (w_current->h_adjustment),
			    "value_changed",
			    GTK_SIGNAL_FUNC (x_event_hschanged),
			    w_current);

		gtk_widget_show (w_current->h_scrollbar);
	}

	/* bottom box */
        bottom_box = gtk_hbox_new(FALSE, 0);
        gtk_container_border_width(GTK_CONTAINER(bottom_box), 1);
	gtk_box_pack_start (GTK_BOX (main_box), bottom_box, FALSE, FALSE, 0);
        gtk_widget_show(bottom_box);

/*	label = gtk_label_new ("Mouse buttons:");
        gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 10);
        gtk_widget_show (label);
*/

	label = gtk_label_new (" ");
        gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 2);
        gtk_widget_show (label);

	w_current->left_label = gtk_label_new ("Pick");
	gtk_box_pack_start (GTK_BOX (bottom_box), w_current->left_label,
			    FALSE, FALSE, 0);
        gtk_widget_show (w_current->left_label);

	label = gtk_label_new ("|");
        gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 5);
        gtk_widget_show (label);

	if (w_current->middle_button == STROKE) {
#if HAS_LIBSTROKE
		w_current->middle_label = gtk_label_new ("Stroke");
#else
		w_current->middle_label = gtk_label_new ("none");
#endif
	} else if (w_current->middle_button == ACTION) {
		w_current->middle_label = gtk_label_new ("Action");
	} else {
		w_current->middle_label = gtk_label_new ("Repeat/none");
	}

	gtk_box_pack_start (GTK_BOX (bottom_box), w_current->middle_label,
			    FALSE, FALSE, 0);
        gtk_widget_show (w_current->middle_label);

	label = gtk_label_new ("|");
        gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 5);
        gtk_widget_show (label);

	if (default_third_button == POPUP_ENABLED) {
		w_current->right_label = gtk_label_new ("Menu/Cancel");
	} else {
		w_current->right_label = gtk_label_new ("Pan/Cancel");
	}
	gtk_box_pack_start (GTK_BOX (bottom_box), w_current->right_label,
                       	    FALSE, FALSE, 0);
        gtk_widget_show (w_current->right_label);

	label = gtk_label_new (" ");
        gtk_box_pack_start (GTK_BOX (bottom_box), label, FALSE, FALSE, 5);
        gtk_widget_show (label);

	w_current->filename_label = gtk_label_new (" ");
        gtk_box_pack_start (GTK_BOX (bottom_box), w_current->filename_label,
 			    FALSE, FALSE, 10);
        gtk_widget_show (w_current->filename_label);

	w_current->status_label = gtk_label_new ("Select Mode");
        gtk_box_pack_end (GTK_BOX (bottom_box), w_current->status_label, FALSE,
			  FALSE, 10);
        gtk_widget_show (w_current->status_label);

        gtk_widget_show(w_current->main_window);

	w_current->window = w_current->drawing_area->window;

	/* draw a black rectangle in drawing area just to make it look nice */
	/* don't do this now */
	/* gdk_draw_rectangle(window, main_window->style->black_gc, TRUE, 0, 0,
	 *				win_width, win_height);
	 *
	 */

        w_current->backingstore = gdk_pixmap_new(w_current->window,
                               w_current->drawing_area->allocation.width,
                               w_current->drawing_area->allocation.height,
                               -1);
	x_window_setup_gc(w_current);
}

TOPLEVEL *
x_window_create_new(void)
{
	TOPLEVEL *w_current=NULL;

	/* allocate new window structure */
	w_current = (TOPLEVEL *) malloc(sizeof(TOPLEVEL));

	/* do init var fill in */
	x_window_setup_rest(w_current);

	/* immediately setup user params */
	i_vars_set(w_current);

	w_current->wid = global_wid;

	/* make sure none of these events happen till we are done */
	w_current->DONT_DRAW_CONN=1;
	w_current->DONT_RESIZE=1;
	w_current->DONT_EXPOSE=1;
	w_current->DONT_REDRAW=1;
	w_current->DONT_RECALC=1;

	/* the default coord sizes */
	/* real ones set in rc file */
        w_current->init_right = default_init_right;
        w_current->init_bottom = default_init_bottom;

#if 1 /* X related stuff */
	w_current->display_height = gdk_screen_height();
	w_current->display_width = gdk_screen_width();
#endif

	x_window_setup_world(w_current);

#if 1 /* X related stuff */
	/* do X fill in first */
	x_window_create_main(w_current);
#endif

	/* Put head node on page list... be sure to free this somewhere hack */
	s_page_add_head(w_current);

	/* Now create a blank page */
	w_current->page_tail = s_page_add(w_current,
					  w_current->page_tail,
					  "unknown");
					/* this is correct */

	s_page_setup(w_current->page_tail);

	/* setup page_current link */
	w_current->page_current = w_current->page_tail;

	/* Special case init */
	/* move this elsewhere eventually */
	/* w_current->page_current->object_parent=NULL; not this one */
	w_current->page_current->object_lastplace=NULL;
	w_current->page_current->object_selected=NULL;
        set_window(w_current, w_current->init_left, w_current->init_right,
		   w_current->init_top, w_current->init_bottom);

#if 1 /* X related stuff */
	/* now update the scrollbars */
	x_hscrollbar_update(w_current);
	x_vscrollbar_update(w_current);
#endif

	global_wid++;
	num_windows++;

	window_tail = x_window_add(window_tail, w_current);

	/* renable the events */
	w_current->DONT_DRAW_CONN=0;
	w_current->DONT_RESIZE=0;
	w_current->DONT_EXPOSE=0;
	w_current->DONT_REDRAW=0;
	w_current->DONT_RECALC=0;

	return(w_current);
}

void
x_window_close(TOPLEVEL *w_current)
{

	if (s_page_check_changed(w_current->page_head)) {
		exit_dialog(w_current);
		return;
	}

#if DEBUG
	o_conn_print_hash(w_current->page_current->conn_table);
#endif

	/* make sure window_tail stays correct and doesn't dangle */
	/* window_head can't dangle since it has a head node, which is */
	/* NEVER deallocated (only at the very end) */
	if (window_tail == w_current) {
		window_tail = w_current->prev;
	}

	/* close all the dialog boxes */
	if (w_current->fowindow)
		gtk_widget_destroy(w_current->fowindow);

	if (w_current->sowindow)
		gtk_widget_destroy(w_current->sowindow);

	if (w_current->fswindow)
		gtk_widget_destroy(w_current->fswindow);

	if (w_current->aswindow)
		gtk_widget_destroy(w_current->aswindow);

	if (w_current->cswindow)
		gtk_widget_destroy(w_current->cswindow);

	if (w_current->tiwindow)
		gtk_widget_destroy(w_current->tiwindow);

	if (w_current->tewindow)
		gtk_widget_destroy(w_current->tewindow);

	if (w_current->aawindow)
		gtk_widget_destroy(w_current->aawindow);

	if (w_current->trwindow)
		gtk_widget_destroy(w_current->trwindow);

	if (w_current->pswindow)
		gtk_widget_destroy(w_current->pswindow);

	if (w_current->exwindow)
		gtk_widget_destroy(w_current->exwindow);

	if (w_current->tswindow)
		gtk_widget_destroy(w_current->tswindow);

	if (w_current->abwindow)
		gtk_widget_destroy(w_current->abwindow);

	if (w_current->iwindow)
		gtk_widget_destroy(w_current->iwindow);

	if (w_current->pwindow)
		gtk_widget_destroy(w_current->pwindow);

	if (w_current->hkwindow)
		gtk_widget_destroy(w_current->hkwindow);

	if (w_current->cowindow)
		gtk_widget_destroy(w_current->cowindow);

	if (w_current->clwindow)
		gtk_widget_destroy(w_current->clwindow);

	if (w_current->fileselect[FILESELECT].xfwindow) {
		gtk_widget_destroy(w_current->fileselect[FILESELECT].xfwindow);
	}

	if (w_current->fileselect[COMPSELECT].xfwindow) {
		gtk_widget_destroy(w_current->fileselect[COMPSELECT].xfwindow);
	}

	x_fileselect_free_list_buffers(&w_current->fileselect[FILESELECT]);
	x_fileselect_free_list_buffers(&w_current->fileselect[COMPSELECT]);

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

	/* stuff that has to be done before we free w_current */
	if ( (num_windows - 1) == 0) {
		/* free all fonts */
		o_text_freeallfonts(w_current);
		/* close the log file */
		s_log_close();
	}

	x_window_delete(window_head, w_current);
	if (w_current->backingstore) {
		gdk_pixmap_unref(w_current->backingstore);
        }

	x_window_free_gc(w_current);

	/* finally close the main window */
	gtk_widget_destroy(w_current->main_window);

	num_windows = num_windows - 1;

	free(w_current);

	/* just closed last window, so quit */
	if (num_windows == 0) {
		gschem_quit();
	}
}

void
x_window_close_all()
{
	TOPLEVEL *w_current;
	TOPLEVEL *w_prev;

	w_current = window_tail;

	/* loop over all windows to close */
	/* going backwards */
	/* wid == -1 is the head and we are done. */
	while(w_current != NULL && w_current->wid != -1) {
		w_prev = w_current->prev;
		x_window_close(w_current);
		w_current = w_prev;
	}

	/* now free the head */
	/* only if all the windows are gone */

	if (window_head->next == NULL && num_windows == 0)  {
        	x_window_free_head();
	}
}

TOPLEVEL *
x_window_get_ptr(int wid)
{
	TOPLEVEL *w_current;

	w_current = window_head;

	while(w_current != NULL) {
		if (w_current->wid == wid) {
			return(w_current);
		}

		w_current = w_current->next;
	}

	return(NULL);
}

/* GROSS! but this is required because clist widgets don't seem to allow you
 * pass data to callback functions, so I need to get w_current by searching
 * the entire window list for page_clist widget :(  If somebody knows a better
 * way of doing this, please let me know!
 */
TOPLEVEL *
x_window_search_page_clist(GtkWidget *findme)
{
	TOPLEVEL *w_current;

	w_current = window_head;

	while(w_current != NULL) {
		if (w_current->page_clist == findme) {
			return(w_current);
		}

		w_current = w_current->next;
	}

	return(NULL);
}

