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

static const gchar *list_item_data_key = "list_item_data";

gint
print_landscape(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->print_orientation = LANDSCAPE;
	return(0);
}

gint
print_portrait(GtkWidget *w, TOPLEVEL *w_current)
{
	w_current->print_orientation = PORTRAIT;
	return(0);
}

gint
x_print_set_window(GtkWidget *w, TOPLEVEL *w_current)
{
	f_print_set_type(w_current, WINDOW);
	return(0);
}

gint
x_print_set_limits(GtkWidget *w, TOPLEVEL *w_current )
{
	f_print_set_type(w_current, LIMITS);
	return(0);
}

/* this is from gtktest.c and only used in this file, */
/* there are other create_menus... */
static GtkWidget*
create_menu_orient (TOPLEVEL *w_current)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	GSList *group;
	char buf[100];

	menu = gtk_menu_new ();
	group = NULL;

	sprintf (buf, "Landscape");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) print_landscape,
			   w_current);

	gtk_widget_show (menuitem);

	sprintf (buf, "Portrait");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) print_portrait,
			   w_current);
	gtk_widget_show (menuitem);

	if (w_current->print_orientation == PORTRAIT) {
		gtk_menu_set_active(GTK_MENU (menu),1);
		x_print_set_window (NULL, w_current);
	} else {
		print_landscape (NULL, w_current);
	}

	return menu;
}

/* this is from gtktest.c and only used in this file, */
/* there are other create_menus... */
static GtkWidget*
create_menu_type (TOPLEVEL *w_current)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	GSList *group;
	char buf[100];

	menu = gtk_menu_new ();
	group = NULL;

	sprintf (buf, "Limits");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) x_print_set_limits,
			   w_current);

	gtk_widget_show (menuitem);

	sprintf (buf, "Current Window");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
			   (GtkSignalFunc) x_print_set_window,
			   w_current);
	gtk_widget_show (menuitem);

	if (w_current->print_output_type == WINDOW) {
		gtk_menu_set_active(GTK_MENU (menu),1);
		f_print_set_type(w_current, WINDOW);
	} else {
		f_print_set_type(w_current, LIMITS);
	}

	return menu;
}

gint
x_print_change_size (GtkWidget *gtklist, TOPLEVEL *w_current)
{
        GList		*dlist;
        GtkObject       *listitem;
        gchar           *item_data_string;

        dlist = GTK_LIST(w_current->plib_list)->selection;

        if (!dlist) {
		/* g_print("Selection cleared\n");*/
		return(0);
        }

        listitem = GTK_OBJECT(dlist->data);
        item_data_string=gtk_object_get_data(listitem, list_item_data_key);

#if DEBUG
	printf("paper_size string: %s\n", item_data_string);
	len = strlen(item_data_string);
	/* strcpy(current_attr_name, item_data_string);*/
#endif

	s_papersizes_get_size(item_data_string,
			      &w_current->paper_width,
			      &w_current->paper_height);

#if 0
        gtk_entry_set_text(GTK_ENTRY(w_current->asentry_name),
			   item_data_string);
        gtk_entry_select_region(GTK_ENTRY(w_current->asentry_name), 0, len);
#endif

	return(0);
}

gint
x_print_print(GtkWidget *w, TOPLEVEL *w_current)
{
	char *filename =
		gtk_entry_get_text(GTK_ENTRY(w_current->pfilename_entry));

	if (filename[0] != '\0') {
		f_print(w_current, filename);
	        s_log_message("Printed current schematic to [%s]\n", filename);
	}

#if 0
	gtk_widget_destroy(w_current->pwindow);
	w_current->pwindow = NULL;
#endif
	return(0);
}

gint
x_print_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
#if 0
	gtk_grab_remove(w_current->pwindow);
#endif
	gtk_widget_destroy(w_current->pwindow);
	w_current->pwindow = NULL;
	return(0);
}

void
x_print_setup (TOPLEVEL *w_current, char *filename)
{
	GtkWidget *label;
	GtkWidget *separator;
	GtkWidget *box;
	GtkWidget *box2;
	GtkWidget *buttonprint;
	GtkWidget *buttoncancel;
	GtkWidget *scrolled_win;
	GtkWidget *list_item;
	GtkWidget *optionmenu;
	GtkWidget *vbox, *action_area;
	char *string = NULL;
	int i;

	/* freeze the window_current pointer so that it doesn't change */

	if (!w_current->pwindow) {

                w_current->pwindow = x_create_dialog_box(&vbox, &action_area); 

		gtk_window_position(GTK_WINDOW (w_current->pwindow),
				    GTK_WIN_POS_MOUSE);

		gtk_signal_connect(GTK_OBJECT (w_current->pwindow),
				   "destroy",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->pwindow);

		gtk_signal_connect(GTK_OBJECT (w_current->pwindow),
				   "delete_event",
				   GTK_SIGNAL_FUNC(destroy_window),
				   &w_current->pwindow);

		gtk_window_set_title(GTK_WINDOW(w_current->pwindow),
				     "Print...");

		buttonprint = gtk_button_new_with_label ("Print");
		GTK_WIDGET_SET_FLAGS (buttonprint, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX(action_area),
			           buttonprint, TRUE, TRUE, 0);
		gtk_signal_connect(GTK_OBJECT(buttonprint), "clicked",
				   GTK_SIGNAL_FUNC(x_print_print), w_current);
		gtk_widget_show (buttonprint);
		gtk_widget_grab_default (buttonprint);

		buttoncancel = gtk_button_new_with_label ("Close");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start(GTK_BOX(action_area),
			           buttoncancel, TRUE, TRUE, 0);
		gtk_signal_connect(GTK_OBJECT(buttoncancel),
				   "clicked", GTK_SIGNAL_FUNC(x_print_cancel),
				   w_current);
		gtk_widget_show (buttoncancel);

		label = gtk_label_new ("Output paper size");
                gtk_misc_set_padding (GTK_MISC (label), 5, 5);
                gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

                gtk_widget_show (label);

		scrolled_win = gtk_scrolled_window_new (NULL, NULL);

		gtk_scrolled_window_set_policy(
			GTK_SCROLLED_WINDOW(scrolled_win),
			GTK_POLICY_AUTOMATIC,
			GTK_POLICY_AUTOMATIC);
		gtk_box_pack_start(GTK_BOX(vbox), scrolled_win, TRUE, TRUE, 10);
		gtk_widget_set_usize(GTK_WIDGET(scrolled_win), 150, 70);
		gtk_widget_show (scrolled_win);
		box2 = gtk_vbox_new (FALSE, 0);
		gtk_scrolled_window_add_with_viewport(
			GTK_SCROLLED_WINDOW (scrolled_win), box2);
		gtk_widget_show(box2);

		separator = gtk_hseparator_new ();
	        gtk_box_pack_start(GTK_BOX(vbox), separator, FALSE, TRUE, 0);
  		gtk_widget_show (separator);

		w_current->plib_list = gtk_list_new ();

		gtk_container_add(GTK_CONTAINER (box2), w_current->plib_list);
		gtk_widget_show (w_current->plib_list);

		i = 0;
		string = (char *) s_papersizes_get(i);
		while (string != NULL) {
			GtkWidget *label = gtk_label_new(string);

			gtk_misc_set_alignment(GTK_MISC (label), 0, 0);

		        list_item = gtk_list_item_new();
			gtk_container_add(GTK_CONTAINER(list_item), label);
			gtk_widget_show(label);
          		gtk_container_add(GTK_CONTAINER(w_current->plib_list),
					  list_item);
          		gtk_widget_show (list_item);
			gtk_label_get(GTK_LABEL(label), &string);
            		gtk_object_set_data(GTK_OBJECT(list_item),
					    list_item_data_key,
					    string);
			i++;
			string = (char *) s_papersizes_get(i);
        	}

		gtk_signal_connect(GTK_OBJECT(w_current->plib_list),
				   "selection_changed",
				   GTK_SIGNAL_FUNC(x_print_change_size),
				   w_current);

		box = gtk_hbox_new(FALSE, 0);
        	gtk_container_border_width(GTK_CONTAINER(box), 5);
        	gtk_container_add(GTK_CONTAINER(vbox), box);
        	gtk_widget_show(box);

		label = gtk_label_new ("Filename");
		gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
                gtk_misc_set_padding (GTK_MISC (label), 0, 0);
                gtk_box_pack_start (GTK_BOX (box),
				    label, FALSE, FALSE, 0);

                gtk_widget_show (label);

		w_current->pfilename_entry =
			gtk_entry_new_with_max_length(79);
                gtk_editable_select_region(
			GTK_EDITABLE(w_current->pfilename_entry), 0, -1);
                gtk_box_pack_start(GTK_BOX (box),
				   w_current->pfilename_entry, TRUE, TRUE, 10);
		gtk_signal_connect(GTK_OBJECT(w_current->pfilename_entry),
				   "activate",
				   GTK_SIGNAL_FUNC(x_print_print),
				   w_current);
                gtk_widget_show (w_current->pfilename_entry);

		separator = gtk_hseparator_new ();
	        gtk_box_pack_start(GTK_BOX(vbox), separator, FALSE, TRUE, 0);
  		gtk_widget_show (separator);

		label = gtk_label_new ("Type");
                gtk_misc_set_padding (GTK_MISC (label), 5, 5);
                gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);
                gtk_widget_show (label);
		optionmenu = gtk_option_menu_new ();
		gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
					 create_menu_type (w_current));
		gtk_option_menu_set_history(GTK_OPTION_MENU (optionmenu), 4);
		gtk_box_pack_start(GTK_BOX(vbox), optionmenu, TRUE, TRUE, 0);
		gtk_widget_show(optionmenu);

		label = gtk_label_new ("Orientation");
                gtk_misc_set_padding (GTK_MISC (label), 5, 5);
                gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 0);

                gtk_widget_show (label);
		optionmenu = gtk_option_menu_new ();
		gtk_option_menu_set_menu(GTK_OPTION_MENU (optionmenu),
					 create_menu_orient (w_current));
		gtk_option_menu_set_history(GTK_OPTION_MENU (optionmenu), 4);
		gtk_box_pack_start(GTK_BOX(vbox), optionmenu, TRUE, TRUE, 0);
		gtk_widget_show (optionmenu);

		/* set some defaults */
		/* TODO: this should be whatever the last value was
		 * set to??? */
		f_print_set_type(w_current, LIMITS);
	}

	if (!GTK_WIDGET_VISIBLE (w_current->pwindow)) {
        	gtk_entry_set_text(GTK_ENTRY(w_current->pfilename_entry),
				   filename);
#if 0
        	gtk_entry_select_region(GTK_ENTRY(w_current->pfilename_entry),
					0, strlen(filename));
#endif

		gtk_widget_show (w_current->pwindow);
		gdk_window_raise(w_current->pwindow->window);
#if 0
		gtk_grab_add (w_current->pwindow);
#endif
 	} else {
		/* window should already be mapped, otherwise this
		 * will core */
		gdk_window_raise(w_current->pwindow->window);
	}
}

