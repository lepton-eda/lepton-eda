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
#include <libgeda/globals.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

const static   gchar   *list_item_data_key="list_item_data";

gint
default_components(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->embed_complex = 0;
	w_current->include_complex = 0;
	return(0);
}

gint
embed_components(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->embed_complex = 1;
	w_current->include_complex = 0;
	return(0);
}

gint
include_components(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->include_complex = 1;
	w_current->embed_complex = 0;
	return(0);
}

/* this is from gtktest.c */
static GtkWidget*
create_menu (TOPLEVEL *w_current)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	GSList *group;
	char buf[100];

	menu = gtk_menu_new ();
	group = NULL;

	sprintf(buf, "Default behavior - reference component");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) default_components,
			   w_current);

	gtk_widget_show(menuitem);

	sprintf(buf, "Embed component in schematic");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) embed_components,
			   w_current);
	gtk_widget_show(menuitem);

	sprintf(buf, "Include component as individual objects");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) include_components,
			   w_current);
	gtk_widget_show(menuitem);

	if (w_current->embed_complex) {
		gtk_menu_set_active(GTK_MENU (menu),1);
		embed_components(NULL, w_current);
	} else {
		default_components(NULL, w_current);
	}

	return menu;
}

gint
change_clib (GtkWidget *gtklist, TOPLEVEL *w_current)
{
	GtkWidget       *label;
        GList   *dlist;
        GtkObject       *item;
        GtkWidget       *list_item;
        gchar           *item_data_string;
	char *file;
	int i;

        dlist = GTK_LIST(w_current->clib_list)->selection;

        if (!dlist) {
#if 0
		g_print("Selection cleared\n");
#endif
		return(0);
        }

        item = GTK_OBJECT(dlist->data);
        item_data_string = gtk_object_get_data(item, list_item_data_key);

#if DEBUG
	printf("%s\n", item_data_string);
#endif

	strcpy(w_current->current_clib, item_data_string);

	/* is this deleting everything? */
	gtk_list_clear_items(GTK_LIST(w_current->basename_list), 0, 1000);

	/* now read in new dir */
	s_clib_getfiles(item_data_string, OPEN_DIR);
	i = 0;

	file = (char *) s_clib_getfiles(item_data_string, READ_DIR);
	while(file != NULL) {
		if (strstr(file, ".sym")) {
			label = gtk_label_new(file);
			gtk_misc_set_alignment (GTK_MISC (label), 0, 0);
			list_item = gtk_list_item_new();
			gtk_container_add(GTK_CONTAINER(list_item), label);
			gtk_widget_show(label);
          		gtk_container_add(
				GTK_CONTAINER(w_current->basename_list),
				list_item);
          		gtk_widget_show(list_item);

			gtk_label_get(GTK_LABEL(label), &file);
            		gtk_object_set_data(GTK_OBJECT(list_item),
					    list_item_data_key,
					    file);
		}
		file = (char *) s_clib_getfiles(item_data_string, READ_DIR);
		i++;
	}

	s_clib_getfiles(NULL, CLOSE_DIR);

	return(0);
}

gint
change_basename (GtkWidget *gtklist, TOPLEVEL *w_current)
{
        GList   *dlist = NULL;
        GtkObject *item = NULL;
        gchar *item_data_string = NULL;
	int diff_x, diff_y;

	exit_if_null(w_current);

        dlist = GTK_LIST(w_current->basename_list)->selection;

        if (!dlist) {
#if 0
		g_print("Selection cleared\n");
#endif

		/* erase any existing component while it's being placed */
		/* do this instead of the below o_redraw_all */
		if (w_current->inside_action &&
		    (w_current->event_state == ENDCOMP ||
		     w_current->event_state == DRAWCOMP)) {
			o_complex_rubbercomplex(w_current);
		}

		o_list_delete_rest(w_current, w_current->page_current->
				   complex_place_head);
		o_complex_set_filename(w_current, w_current->current_clib,
				       w_current->current_basename);
		/* this one is okay */
		w_current->event_state = SELECT;
		i_update_status(w_current, "Select Mode");

		/* this redraw all is no longer needed */
#if 0
		o_redraw_all(w_current);
#endif

		return(0);
        }

	item = GTK_OBJECT(dlist->data);
	item_data_string = gtk_object_get_data(item, list_item_data_key);

#if DEBUG
        printf("%s\n", item_data_string);
#endif

        dlist = dlist->next;

	strcpy(w_current->current_basename, item_data_string);

#if DEBUG
	printf("complete: %s/%s\n",
	       w_current->current_clib,
	       w_current->current_basename);
#endif

	/* erase any outstanding outline */
	if (w_current->event_state == ENDCOMP) {
		diff_x = w_current->last_x - w_current->start_x;
                diff_y = w_current->last_y - w_current->start_y;

		o_complex_translate_display(
			w_current,
			diff_x, diff_y,
			w_current->page_current->complex_place_head);
	}

	o_list_delete_rest(w_current,
			   w_current->page_current->complex_place_head);
	o_complex_set_filename(w_current,
			       w_current->current_clib,
			       w_current->current_basename);

	w_current->event_state = DRAWCOMP;
	return(0);
}

void
setup_place_file_selector (TOPLEVEL *w_current)
{
	GtkWidget *box2;
	GtkWidget *buttonapply;
	GtkWidget *buttonclose;
	GtkWidget *scrolled_win;
	GtkWidget *list_item;
	GtkWidget *optionmenu;
	GtkWidget *vbox, *action_area;
	char *string = NULL;
	int i;

	if (!w_current->cswindow) {

		w_current->cswindow = x_create_dialog_box(&vbox, &action_area); 

		gtk_window_position(GTK_WINDOW(w_current->cswindow),
				    GTK_WIN_POS_NONE);

		gtk_signal_connect(GTK_OBJECT(w_current->cswindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->cswindow);

#if 0 /* this was causing the dialog box to not die */
		gtk_signal_connect(GTK_OBJECT(w_current->cswindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->cswindow);
#endif

		gtk_window_set_title(GTK_WINDOW(w_current->cswindow),
				     "Select Component");

		buttonapply = gtk_button_new_with_label ("Apply");
		GTK_WIDGET_SET_FLAGS(buttonapply, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX(action_area),
				   buttonapply, TRUE, TRUE, 0);
		gtk_signal_connect(GTK_OBJECT(buttonapply),
				   "clicked",
				   GTK_SIGNAL_FUNC(change_basename),
				   w_current);
		gtk_widget_show(buttonapply);

		buttonclose = gtk_button_new_with_label ("Close");
		GTK_WIDGET_SET_FLAGS (buttonclose, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX(action_area),
				   buttonclose, TRUE, TRUE, 0);
		gtk_signal_connect_object(GTK_OBJECT(buttonclose),
					  "clicked",
					  GTK_SIGNAL_FUNC(gtk_widget_destroy),
					  GTK_OBJECT(w_current->cswindow));
		gtk_widget_show(buttonclose);

		scrolled_win = gtk_scrolled_window_new(NULL, NULL);

      		gtk_scrolled_window_set_policy(
			GTK_SCROLLED_WINDOW(scrolled_win),
			GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC);
      		gtk_box_pack_start(GTK_BOX(vbox),
			           scrolled_win, TRUE, TRUE, 10);
	        gtk_widget_show(scrolled_win);
		box2 = gtk_vbox_new(FALSE, 0);
		gtk_scrolled_window_add_with_viewport(
			GTK_SCROLLED_WINDOW (scrolled_win), box2);

		gtk_widget_show(box2);

		gtk_widget_set_usize(GTK_WIDGET(scrolled_win), 400, 70);

		/* this can be deleted since we are now using an
		 * option menu */
#if 0
		button = gtk_radio_button_new_with_label(NULL, "Default");
      		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);

		gtk_signal_connect(GTK_OBJECT(button),
				   "clicked",
				   GTK_SIGNAL_FUNC(default_components),
				   w_current);

		gtk_widget_show(button);

		group = gtk_radio_button_group(GTK_RADIO_BUTTON (button));
		button = gtk_radio_button_new_with_label(group,
							 "Embed components");
      		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);

#if 0
		if (w_current->embed_complex) {
			gtk_toggle_button_set_state(
				GTK_TOGGLE_BUTTON (button), TRUE);
		} else {
			gtk_toggle_button_set_state(
				GTK_TOGGLE_BUTTON (button), FALSE);
		}
#endif

		gtk_signal_connect(GTK_OBJECT(button),
				   "clicked",
				   GTK_SIGNAL_FUNC(embed_components),
				   w_current);

		gtk_widget_show(button);

		group = gtk_radio_button_group(GTK_RADIO_BUTTON (button));
		button = gtk_radio_button_new_with_label(group,
							 "Include components");
      		gtk_box_pack_start(GTK_BOX(vbox), button, TRUE, TRUE, 0);
		if (w_current->include_complex) {
			gtk_toggle_button_set_state(
				GTK_TOGGLE_BUTTON (button), TRUE);
		} else {
			gtk_toggle_button_set_state(
				GTK_TOGGLE_BUTTON (button), FALSE);
		}

		gtk_signal_connect(GTK_OBJECT(button),
				   "clicked",
				   GTK_SIGNAL_FUNC(embed_components),
				   w_current);

		gtk_widget_show(button);
#endif

		optionmenu = gtk_option_menu_new ();
		gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
					 create_menu (w_current));
		gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), 4);
		gtk_box_pack_start(GTK_BOX(vbox), optionmenu, FALSE, FALSE, 0);
		gtk_widget_show (optionmenu);

		w_current->clib_list = gtk_list_new ();
		gtk_container_add(GTK_CONTAINER(box2),
				  w_current->clib_list);
		gtk_widget_show(w_current->clib_list);

		i = 0;
		string = (char *) s_clib_getdir(i);
		while (string != NULL) {
			GtkWidget *label = gtk_label_new(string);

			gtk_misc_set_alignment(GTK_MISC(label), 0, 0);

		        list_item = gtk_list_item_new();
			gtk_container_add(GTK_CONTAINER(list_item), label);
			gtk_widget_show(label);
          		gtk_container_add(
				GTK_CONTAINER(w_current->clib_list),
				list_item);
          		gtk_widget_show (list_item);
			gtk_label_get(GTK_LABEL(label), &string);
            		gtk_object_set_data(GTK_OBJECT(list_item),
					    list_item_data_key,
					    string);
			i++;
			string = (char *) s_clib_getdir(i);

        	}

		gtk_signal_connect(GTK_OBJECT(w_current->clib_list),
				   "selection_changed",
				   GTK_SIGNAL_FUNC(change_clib),
				   w_current);

		scrolled_win = gtk_scrolled_window_new (NULL, NULL);

      		gtk_scrolled_window_set_policy(
			GTK_SCROLLED_WINDOW(scrolled_win),
			GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC);
      		gtk_box_pack_start(GTK_BOX(vbox), scrolled_win, TRUE, TRUE, 10);
	        gtk_widget_show(scrolled_win);
		box2 = gtk_vbox_new (FALSE, 0);
		gtk_scrolled_window_add_with_viewport(
			GTK_SCROLLED_WINDOW(scrolled_win), box2);

		gtk_widget_show(box2);

		gtk_widget_set_usize(GTK_WIDGET(scrolled_win), 300, 100);

		w_current->basename_list = gtk_list_new();

		gtk_container_add(GTK_CONTAINER(box2),
				  w_current->basename_list);

		gtk_signal_connect(GTK_OBJECT(w_current->basename_list),
				   "selection_changed",
				   GTK_SIGNAL_FUNC(change_basename),
				   w_current);

		gtk_widget_show(w_current->basename_list);
	}

	if (!GTK_WIDGET_VISIBLE(w_current->cswindow)) {
		gtk_widget_show(w_current->cswindow);
		gdk_window_raise(w_current->cswindow->window);
 	} else {
		/* window should already be mapped, otherwise this
		 * will core */
		gdk_window_raise(w_current->cswindow->window);
	}
}
