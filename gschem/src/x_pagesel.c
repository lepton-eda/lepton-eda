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

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

/* delete me */
gint change_page(GtkWidget *widget, gint row, gint column,
		 GdkEventButton * bevent);

#define NUM_ROWS 2

/* Do NOT use the widget parameter. It is set to NULL most of the
 * time. */
void
update_page_manager(GtkWidget *widget, TOPLEVEL *w_current)
{
	PAGE *p_current;
	char text[NUM_ROWS][512]; /* size is hack */
	char *texts[NUM_ROWS];
	int row;
	int i;

	if (w_current->pswindow == NULL) {
		return;
	}

	p_current = w_current->page_head->next; /* skip over head */

	/* shouldn't happen, but you never know */
	if (p_current == NULL) {
		return;
	}

	gtk_signal_disconnect(GTK_OBJECT(w_current->page_clist),
			      w_current->clist_sig);

	gtk_clist_freeze(GTK_CLIST(w_current->page_clist));

	/* First clear the list */
	gtk_clist_clear(GTK_CLIST(w_current->page_clist));

	for (i = 0; i < NUM_ROWS; i++) {
		texts[i] = text[i];
	}

	i = 0;

	while(p_current != NULL) {
		if (p_current->page_filename == NULL) {
			break;
		}

		sprintf (text[0], "%s", p_current->page_filename);
		if (p_current->CHANGED) {
			sprintf (text[1], "YES");
		} else {
			sprintf (text[1], "-");
		}
		gtk_clist_append (GTK_CLIST (w_current->page_clist), texts);
		p_current->clist_row = i++;

		p_current = p_current->next;
	}

	row = s_page_search_row(w_current, w_current->page_current);
	gtk_clist_select_row(GTK_CLIST(w_current->page_clist), row, 1);

	/* scroll the display so that selection is in the middle */
	gtk_clist_moveto (GTK_CLIST(w_current->page_clist), row, 0, 1, 1);

	gtk_clist_thaw(GTK_CLIST(w_current->page_clist));

        w_current->clist_sig = gtk_signal_connect(
		GTK_OBJECT(w_current->page_clist),
		"select_row",
		GTK_SIGNAL_FUNC(change_page),
		NULL);
}

gint
save_page (GtkWidget *gtklist, TOPLEVEL *w_current)
{
	/* Don't need to search for any rows or whatever since
	 * page_current already points to the page we want to save */
	i_callback_file_save(w_current, 0, NULL);

	return(0);
}

gint
file_new (GtkWidget *gtklist, TOPLEVEL *w_current)
{
	i_callback_file_new(w_current, 0, NULL);

	return(0);
}

gint
file_open (GtkWidget *gtklist, TOPLEVEL *w_current)
{
	i_callback_file_open(w_current, 0, NULL);

	return(0);
}

gint
page_close (GtkWidget *gtklist, TOPLEVEL *w_current)
{
	i_callback_page_close(w_current, 0, NULL);

	return(0);
}

gint
page_discard (GtkWidget *gtklist, TOPLEVEL *w_current)
{
	i_callback_page_discard(w_current, 0, NULL);

	return(0);
}

gint
change_page (GtkWidget *widget, gint row, gint column, GdkEventButton * bevent)
{
	PAGE *p_new;
	TOPLEVEL *w_current;
	gchar *text;

#if DEBUG
	g_print ("GtkCList Selection: row %d column %d button\n",
		 row, column);
#endif

	gtk_clist_get_text (GTK_CLIST (widget), row, 0, &text);

	/* gross, yes... see comment in x_window.c */
	w_current = x_window_search_page_clist(widget);
	p_new = s_page_search(w_current, text);

	s_page_goto(w_current, p_new);
	i_set_filename(w_current, w_current->page_current->page_filename);
	x_scrollbars_update(w_current);
	o_redraw_all(w_current);

	return(0);
}

void
setup_page_selector (TOPLEVEL *w_current)
{
	GtkWidget *buttonnew;
	GtkWidget *scrolled_win;
	GtkWidget *buttonopen;
	GtkWidget *buttonsave;
	GtkWidget *buttonclose;
	GtkWidget *buttondiscard;
	GtkWidget *buttonclosewin;
	GtkWidget *buttonupdatewin;
	GtkWidget *box;
	GtkWidget *hbox;
	GtkWidget *box2;
	GtkWidget *separator;
	PAGE *p_current;
	char *string=NULL;
	int i;
	int row;

	char text[NUM_ROWS][512]; /* size is hack */
	char *texts[NUM_ROWS];

	static char *titles[] = {
    		"Filename",
    		"Changed",
  	};

	if (!w_current->pswindow) {

		p_current = w_current->page_head->next; /* skip over head */

		/* shouldn't happen, but you never know */
		if (p_current == NULL) {
			return;
		}

		w_current->pswindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
		gtk_container_border_width(
			GTK_CONTAINER(w_current->pswindow), 0);
		gtk_widget_set_usize(w_current->pswindow, 500, 220);

		gtk_window_position(GTK_WINDOW(w_current->pswindow),
				    GTK_WIN_POS_NONE);

		gtk_signal_connect(GTK_OBJECT(w_current->pswindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->pswindow);

#if 0 /* this was causing the dialog box to not die */
		gtk_signal_connect(GTK_OBJECT (w_current->pswindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->pswindow);
#endif

		gtk_window_set_title(GTK_WINDOW(w_current->pswindow),
				     "Page Manager");

		box = gtk_vbox_new (FALSE, 2);
      		gtk_container_border_width (GTK_CONTAINER (box), 2);
		gtk_container_add (GTK_CONTAINER (w_current->pswindow), box);
      		gtk_widget_show (box);

		/* change to clist */
		w_current->page_clist =
			gtk_clist_new_with_titles (NUM_ROWS, titles);

		gtk_widget_show(w_current->page_clist);
		scrolled_win = gtk_scrolled_window_new (NULL, NULL);
      		gtk_scrolled_window_set_policy(
			GTK_SCROLLED_WINDOW(scrolled_win),
			GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC);
		gtk_widget_show(scrolled_win);
      		gtk_container_add(GTK_CONTAINER(scrolled_win),
				  w_current->page_clist);

		gtk_clist_set_row_height(GTK_CLIST(w_current->page_clist),
					 20);
		gtk_clist_set_column_width(GTK_CLIST(w_current->page_clist),
					   0, 400);

		gtk_clist_set_selection_mode(GTK_CLIST(w_current->page_clist),
					     GTK_SELECTION_BROWSE);

		gtk_clist_set_column_justification(
			GTK_CLIST(w_current->page_clist),
			0, GTK_JUSTIFY_LEFT);
		gtk_clist_set_column_justification(
			GTK_CLIST (w_current->page_clist),
			1, GTK_JUSTIFY_CENTER);

		for (i = 0; i < NUM_ROWS; i++) {
          		texts[i] = text[i];
        	}

		i = 0;
		string = p_current->page_filename;
		while(p_current != NULL && string != NULL) {
			sprintf (text[0], "%s", p_current->page_filename);

			if (p_current->CHANGED) {
	      			sprintf (text[1], "YES");
			} else {
	      			sprintf (text[1], "-");
			}

			gtk_clist_append(GTK_CLIST(w_current->page_clist),
					 texts);

			p_current->clist_row = i++;
			p_current = p_current->next;

			if (p_current == NULL) {
				break;
			} else {
				string = p_current->page_filename;
			}
		}

		/* we need to store the signal handler here since we
		 * disconnect it and then reconnect it later */
       		w_current->clist_sig = gtk_signal_connect
			(GTK_OBJECT(w_current->page_clist),
			 "select_row",
			 GTK_SIGNAL_FUNC(change_page),
			 NULL);

		gtk_container_border_width(
			GTK_CONTAINER(w_current->page_clist), 0);

		gtk_container_set_border_width(
			GTK_CONTAINER(scrolled_win), 5);
                gtk_box_pack_start(GTK_BOX (box), scrolled_win, TRUE, TRUE, 0);

		row = s_page_search_row(w_current, w_current->page_current);
		gtk_clist_select_row(GTK_CLIST(w_current->page_clist), row, 1);
		gtk_clist_moveto(GTK_CLIST(w_current->page_clist),
				 5, 0, 0.5, 0.5);
		gtk_clist_moveto(GTK_CLIST(w_current->page_clist),
				 5, 0, 0.5, 0.5);
		gtk_clist_moveto(GTK_CLIST(w_current->page_clist),
				 5, 0, 0.5, 0.5);

		hbox = gtk_hbox_new (TRUE, 5);
      		gtk_container_border_width (GTK_CONTAINER (hbox), 5);
 		gtk_box_pack_start (GTK_BOX (box), hbox, FALSE, FALSE, 0);
      		gtk_widget_show (hbox);

		separator = gtk_hseparator_new ();
                gtk_box_pack_start (GTK_BOX (box),
				    separator, FALSE, TRUE, 0);
                gtk_widget_show (separator);

		box2 = gtk_hbox_new (TRUE, 5);
      		gtk_container_border_width (GTK_CONTAINER (box2), 5);
 		gtk_box_pack_start (GTK_BOX (box), box2, FALSE, FALSE, 0);
      		gtk_widget_show (box2);

		/* new page */
		buttonnew = gtk_button_new_with_label ("New Page");
		GTK_WIDGET_SET_FLAGS (buttonnew, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX (hbox),
				   buttonnew, TRUE, TRUE, 0);
		gtk_signal_connect(GTK_OBJECT (buttonnew), "clicked",
				   GTK_SIGNAL_FUNC(file_new), w_current);
		gtk_widget_show (buttonnew);

		/* open page */
		buttonopen = gtk_button_new_with_label ("Open Page");
		GTK_WIDGET_SET_FLAGS (buttonopen, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (hbox),
				    buttonopen, TRUE, TRUE, 0);
		gtk_signal_connect (GTK_OBJECT (buttonopen), "clicked",
				    GTK_SIGNAL_FUNC(file_open), w_current);
		gtk_widget_show (buttonopen);

		/* save page */
		buttonsave = gtk_button_new_with_label ("Save Page");
		GTK_WIDGET_SET_FLAGS (buttonsave, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (hbox),
				    buttonsave, TRUE, TRUE, 0);
		gtk_signal_connect (GTK_OBJECT (buttonsave), "clicked",
				    GTK_SIGNAL_FUNC(save_page), w_current);
		gtk_widget_show (buttonsave);

		/* close page */
		buttonclose = gtk_button_new_with_label ("Close Page");
		GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (hbox),
				    buttonclose, TRUE, TRUE, 0);
		gtk_signal_connect (GTK_OBJECT (buttonclose), "clicked",
				    GTK_SIGNAL_FUNC(page_close), w_current);
		gtk_widget_show (buttonclose);

		/* discard page */
		buttondiscard = gtk_button_new_with_label ("Discard Page");
		GTK_WIDGET_SET_FLAGS (buttondiscard, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (hbox),
				    buttondiscard, TRUE, TRUE, 0);
		gtk_signal_connect (GTK_OBJECT (buttondiscard), "clicked",
				    GTK_SIGNAL_FUNC(page_discard), w_current);
		gtk_widget_show (buttondiscard);

		/* This is a window control */
		buttonupdatewin = gtk_button_new_with_label ("Update Manager");
		gtk_box_pack_start (GTK_BOX (box2),
				    buttonupdatewin, TRUE, TRUE, 10);
		gtk_signal_connect (GTK_OBJECT (buttonupdatewin), "clicked",
				    GTK_SIGNAL_FUNC(update_page_manager),
				    w_current);
		gtk_widget_show (buttonupdatewin);

		/* This is a window control */
		buttonclosewin = gtk_button_new_with_label ("Close Manager");
		gtk_box_pack_start (GTK_BOX (box2),
				    buttonclosewin, TRUE, TRUE, 10);
		gtk_signal_connect_object(GTK_OBJECT(buttonclosewin),
					  "clicked",
					  GTK_SIGNAL_FUNC(gtk_widget_destroy),
					  GTK_OBJECT (w_current->pswindow));
		gtk_widget_show (buttonclosewin);
	}

	if (!GTK_WIDGET_VISIBLE (w_current->pswindow)) {
		gtk_widget_show (w_current->pswindow);
		gdk_window_raise(w_current->pswindow->window);
 	} else {
		/* window should already be mapped, otherwise this
		 * will core */
		gdk_window_raise(w_current->pswindow->window);
	}
}
