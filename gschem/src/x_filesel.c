/* CLEAN up line length in this file */

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
#include <stdlib.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>



#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/colors.h>   
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

void
destroy_window (GtkWidget  *widget, GtkWidget **window)
{
  *window = NULL;
}


void
file_selection_ok_open (GtkWidget *w, TOPLEVEL *w_current)
{
	int len;
	char *string;
	string = gtk_file_selection_get_filename (GTK_FILE_SELECTION (w_current->fowindow));

	if (string != NULL) {
		len = strlen(string);

#if DEBUG 
  		g_print ("pre open: %s\n", string);
#endif

		if (string[len - 1] != '/') {

#if DEBUG 
  			g_print ("open: %s\n", string);
#endif

			/* don't free the current page */
			/* s_page_free(w_current, w_current->page_current);*/

			s_page_new(w_current, string);

			w_current->DONT_REDRAW = 1; /* highly experimental */

			f_open(w_current, w_current->
					page_current->page_filename);
			i_set_filename(w_current, w_current->page_current->page_filename);
		

			x_repaint_background(w_current); 
			x_window_setup_world(w_current);
			x_manual_resize(w_current);
			a_zoom_limits(w_current, w_current->page_current->object_head);	
			/* now update the scrollbars */
			x_hscrollbar_update(w_current);
			x_vscrollbar_update(w_current);
			update_page_manager(NULL, w_current);
			w_current->DONT_REDRAW = 0; /* highly experimental */

			/* x_hscrollbar_update(w_current);
			x_vscrollbar_update(w_current);*/

			o_redraw_all(w_current);
		}
	}
	/* would like to move this earlier! */
        gtk_grab_remove(w_current->fowindow);
	gtk_widget_destroy (GTK_WIDGET (w_current->fowindow)); 
        w_current->fowindow = NULL;
	
}

void
file_selection_cancel_open (GtkWidget *w, TOPLEVEL *w_current)
{
        gtk_grab_remove(w_current->fowindow);
	gtk_widget_destroy (GTK_WIDGET (w_current->fowindow)); 
        w_current->fowindow = NULL;
}

void
setup_open_file_selector (TOPLEVEL *w_current)
{

	if (!w_current->fowindow) {
		w_current->fowindow = gtk_file_selection_new ("Open...");
		/* set next line to project dir */ 
		/*gtk_file_selection_set_filename (w_current->fowindow, COMPONENT_LIBRARY);*/
		gtk_window_position (GTK_WINDOW (w_current->fowindow), GTK_WIN_POS_MOUSE);
		/* added 4/6/98 */
		gtk_file_selection_hide_fileop_buttons (GTK_FILE_SELECTION (w_current->fowindow));
		/* gtk_window_position (GTK_WINDOW (w_current->fowindow), GTK_WIN_POS_NONE);*/
		gtk_signal_connect (GTK_OBJECT (w_current->fowindow), "destroy",
			GTK_SIGNAL_FUNC(destroy_window),
			&w_current->fowindow);

		gtk_signal_connect (GTK_OBJECT (w_current->fowindow), "delete_event",
			GTK_SIGNAL_FUNC(destroy_window),
			&w_current->fowindow);

		/* consistant function names for connect connect_object */
		/* hack */
		gtk_signal_connect (GTK_OBJECT (
			GTK_FILE_SELECTION (w_current->fowindow)->ok_button),
			"clicked", GTK_SIGNAL_FUNC(file_selection_ok_open),
			w_current);

		gtk_signal_connect (GTK_OBJECT (
			GTK_FILE_SELECTION (w_current->fowindow)->cancel_button),
			"clicked", GTK_SIGNAL_FUNC(file_selection_cancel_open),
			w_current);   

	}

	if (!GTK_WIDGET_VISIBLE (w_current->fowindow)) {
		gtk_widget_show (w_current->fowindow);
		gtk_grab_add (w_current->fowindow);	
	}

}

void
file_selection_ok_saveas (GtkWidget *w, TOPLEVEL *w_current)
{

	int len;
	char *string; 

	string = gtk_file_selection_get_filename (GTK_FILE_SELECTION (w_current->fswindow));

	if (string != NULL) {
		len = strlen(string);

		if (string[len - 1] != '/') {
#if DEBUG
  			g_print ("saveas: %s\n", string);
#endif

			if (w_current->page_current->page_filename) {
				free(w_current->page_current->page_filename);
			}

			w_current->page_current->page_filename =
					malloc(sizeof(char)*strlen(string)+1);
			strcpy(w_current->page_current->page_filename, string);
			
			f_save(w_current, string);

			s_log_message("Saved As [%s]\n", w_current->page_current->page_filename);

			i_set_filename(w_current, string);

        		w_current->page_current->CHANGED = 0;
			update_page_manager(NULL, w_current);
		}
	}

        gtk_grab_remove(w_current->fswindow);
	gtk_widget_destroy (GTK_WIDGET (w_current->fswindow)); 
        w_current->fswindow = NULL;


	if (w_current->saveas_flag == QUIT) {
                 x_window_close(w_current);
                 /* i_callback_file_quit(NULL, w_current); commented out 5/9*/ 
        }         

	if (w_current->saveas_flag == OPEN) {
                 i_callback_file_open(w_current, 0, NULL);
	}

	if (w_current->saveas_flag == NEW) {
                 i_callback_file_new(w_current, 0, NULL);
	}

	if (w_current->saveas_flag == CLOSE) {
                 i_callback_page_close(w_current, 0, NULL);
	}

	/* Do nothing if it is SAVEAS */
}

void
file_selection_cancel_saveas (GtkWidget *w, TOPLEVEL *w_current)
{
        gtk_grab_remove(w_current->fswindow);
	gtk_widget_destroy (GTK_WIDGET (w_current->fswindow)); 
        w_current->fswindow = NULL;

	if (w_current->saveas_flag == QUIT) {
		exit_dialog(w_current);
	} 

	if (w_current->saveas_flag == OPEN) {
		setup_open_file_selector(w_current);
	}

	/* eh... you really should do the dialog thing here like QUIT */
	/* or maybe not? NOT, since pages are replaced, new ones added only */
	if (w_current->saveas_flag == NEW) {
		w_current->page_current->CHANGED=0;
		i_callback_file_new(w_current, 0, NULL);
	}

	/* do nothing if cancel is pressed for CLOSE case */
}

void
setup_saveas_file_selector (TOPLEVEL *w_current, int flag, char *filename)
{

	w_current->saveas_flag = flag;

  	if (!w_current->fswindow) {
		w_current->fswindow = gtk_file_selection_new ("Save As...");
		gtk_file_selection_set_filename (GTK_FILE_SELECTION(w_current->fswindow),
						 filename);
		gtk_window_position (GTK_WINDOW (w_current->fswindow), GTK_WIN_POS_MOUSE);
		/* added 4/6/98 */
		gtk_file_selection_hide_fileop_buttons (GTK_FILE_SELECTION (w_current->fswindow));

		gtk_signal_connect (GTK_OBJECT (w_current->fswindow), "destroy",
			GTK_SIGNAL_FUNC(destroy_window),
			&w_current->fswindow);

		gtk_signal_connect (GTK_OBJECT (w_current->fswindow), "delete_event",
			GTK_SIGNAL_FUNC(destroy_window),
			&w_current->fswindow);

		gtk_signal_connect (GTK_OBJECT (
			GTK_FILE_SELECTION (w_current->fswindow)->ok_button),
			"clicked", GTK_SIGNAL_FUNC(file_selection_ok_saveas),
			w_current);

	/* This was changed from _object.... */
		gtk_signal_connect (GTK_OBJECT (
			GTK_FILE_SELECTION (w_current->fswindow)->cancel_button),
			"clicked", GTK_SIGNAL_FUNC(file_selection_cancel_saveas),
			w_current);   

	}

	if (!GTK_WIDGET_VISIBLE (w_current->fswindow)) {
		gtk_widget_show (w_current->fswindow);
		gtk_grab_add (w_current->fswindow);	
	}	

}
