/* STILL NEED to clean up line lengths in aa and tr */

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
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>
#include <glib.h>

#include <guile/gh.h>



#include <libgeda/struct.h>   
#include <libgeda/defines.h>
#include <libgeda/colors.h>  
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

static GtkWidget *stwindow=NULL;
static GtkWidget *sttext=NULL;

void
x_log_read(void)
{
	char buf[81];
	int tmp_fd;
	int len;

	if (do_logging == FALSE) {
		return;
	}

 	tmp_fd = open("gschem.log", O_RDONLY, 0600);

	if (tmp_fd == -1) {
		do_logging = FALSE;
		return;
	}

	while( (len = read(tmp_fd, &buf, 79)) != 0) {
		/* not sure if this nukes the last char or not... */

		buf[len] = '\0'; /* null terminate the buffer */
		gtk_text_insert (GTK_TEXT (sttext), 
			NULL, NULL, NULL, 
			buf, len);
	}

	close(tmp_fd);
}

void
x_log_update(char *buf)
{
	int nchars;

	if (do_logging == FALSE) {
		return;
	}

	if (buf == NULL) {
		return;
	}

	switch(logging_dest) {

		case(LOG_WINDOW): 
			if (!stwindow) {
				return;
			}

			nchars = strlen(buf);
			gtk_text_insert (GTK_TEXT (sttext), 
				NULL, NULL, NULL, 
				buf, nchars);
		break;

		case(STDOUT_TTY):
			fputs (buf, stdout);
		break;

		case(BOTH):
			fputs (buf, stdout);

			if (!stwindow) {
				break;
			}

			nchars = strlen(buf);
			gtk_text_insert (GTK_TEXT (sttext), 
				NULL, NULL, NULL, 
				buf, nchars);
		break;

	}
}


void
x_log_close(GtkWidget *w, TOPLEVEL *w_current)
{
	gtk_widget_destroy(stwindow);
	stwindow=NULL;

	/*gtk_grab_remove(w_current->stwindow);*/
}

void
x_log_setup_win (TOPLEVEL *w_current)
{
	GtkWidget *buttoncancel=NULL;
	GtkWidget *hscrollbar=NULL;
	GtkWidget *vscrollbar=NULL;
	GtkWidget *table=NULL;
	
	if (do_logging == FALSE) {
		return;
	}

	if (!stwindow)
	{
		stwindow = gtk_dialog_new ();

		/* comment this out if you want the log window to have wm */
		/* decorations */

		if (w_current->log_window_type == TRANSIENT) {
			gtk_window_set_transient_for (GTK_WINDOW (stwindow), GTK_WINDOW (w_current->main_window));
			gtk_window_position (GTK_WINDOW (stwindow), 
                                     GTK_WIN_POS_MOUSE);
		} else {
			gtk_window_position (GTK_WINDOW (stwindow), 
				     GTK_WIN_POS_NONE);
		}

		gtk_widget_set_usize(stwindow, 600, 150);

		gtk_signal_connect (GTK_OBJECT (stwindow), 
				    "destroy", GTK_SIGNAL_FUNC(destroy_window),
                          	    &stwindow);

      		gtk_signal_connect (GTK_OBJECT (stwindow), 
				    "delete_event",
                          	    GTK_SIGNAL_FUNC(destroy_window),
                          	    &stwindow);    

		gtk_window_set_title (GTK_WINDOW (stwindow), 
				      "Status");
                gtk_container_border_width (GTK_CONTAINER (stwindow), 0);       

		table = gtk_table_new (2, 2, FALSE);
		gtk_table_set_row_spacing (GTK_TABLE (table), 0, 2);
		gtk_table_set_col_spacing (GTK_TABLE (table), 0, 2);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG(stwindow)->vbox), 
			table, TRUE, TRUE, 0);
		gtk_widget_show (table);

		sttext = gtk_text_new (NULL, NULL);
		gtk_text_set_editable (GTK_TEXT (sttext), FALSE);
		gtk_table_attach (GTK_TABLE (table), sttext, 
			0, 1, 0, 1,
                        GTK_EXPAND | GTK_SHRINK | GTK_FILL,
                        GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);
      		gtk_widget_show (sttext);

		hscrollbar = gtk_hscrollbar_new (GTK_TEXT (sttext)->hadj);
		gtk_table_attach (GTK_TABLE (table), hscrollbar, 0, 1, 1, 2,
		GTK_EXPAND | GTK_FILL | GTK_SHRINK, GTK_FILL, 0, 0);
		gtk_widget_show (hscrollbar);

		vscrollbar = gtk_vscrollbar_new (GTK_TEXT (sttext)->vadj);
		gtk_table_attach (GTK_TABLE (table), vscrollbar, 1, 2, 0, 1,
		GTK_FILL, GTK_EXPAND | GTK_SHRINK | GTK_FILL, 0, 0);
		gtk_widget_show (vscrollbar);

		buttoncancel = gtk_button_new_with_label ("Close");   
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG(
				    stwindow)->action_area),
                          	    buttoncancel, TRUE, TRUE, 0);
      		gtk_signal_connect (GTK_OBJECT (buttoncancel), "clicked",
			  	    GTK_SIGNAL_FUNC(x_log_close), NULL);    
      		gtk_widget_show (buttoncancel); 
	}

  	if (!GTK_WIDGET_VISIBLE (stwindow)) {
		gtk_widget_show (stwindow);
		x_log_read(); /* update the contents of the log */
	} else {
		gdk_window_raise(stwindow->window);
	}
}

