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

/* static const   gchar   *list_item_data_key="list_item_data";	*/

gint
image_320(GtkWidget *w, TOPLEVEL *w_current)
{
        w_current->image_width = 320;
        w_current->image_height = 240;
        return(0);
}

gint
image_640(GtkWidget *w, TOPLEVEL *w_current)
{
        w_current->image_width = 640;
        w_current->image_height = 480;
        return(0);
}

gint
image_800(GtkWidget *w, TOPLEVEL *w_current)
{
        w_current->image_width = 800;
        w_current->image_height = 600;
        return(0);
}

gint
image_1024(GtkWidget *w, TOPLEVEL *w_current)
{
        w_current->image_width = 1024;
        w_current->image_height = 768;
        return(0);
}

gint
image_1280(GtkWidget *w, TOPLEVEL *w_current)
{
        w_current->image_width = 1280;
        w_current->image_height = 1024;
        return(0);
}

gint
image_1600(GtkWidget *w, TOPLEVEL *w_current)
{
        w_current->image_width = 1600;
        w_current->image_height = 1200;
        return(0);
}

/* this is from gtktest.c and only used in this file, */
/* there are other create_menus... */
static GtkWidget*
create_menu_size (TOPLEVEL *w_current)
{
	GtkWidget *menu;
	GtkWidget *menuitem;
	GSList *group;
	char buf[100];

	menu = gtk_menu_new ();
	group = NULL;

	sprintf (buf, "320x240");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_320,
                              w_current);

	gtk_widget_show (menuitem);

	sprintf (buf, "640x480");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_640,
                              w_current);

	gtk_widget_show (menuitem);

	sprintf (buf, "800x600");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_800,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "1024x768");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_1024,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "1280x1024");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_1280,
                              w_current);
	gtk_widget_show (menuitem);

	sprintf (buf, "1600x1200");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_1600,
                              w_current);
	gtk_widget_show (menuitem);

	gtk_menu_set_active(GTK_MENU (menu),2);

	return menu;
}

gint
x_image_write(GtkWidget *w, TOPLEVEL *w_current)
{
	char *filename=NULL;
	int width, height;
	int save_height, save_width;

	filename = gtk_entry_get_text(GTK_ENTRY(w_current->ifilename_entry));

/*
	c_width = gtk_entry_get_text(GTK_ENTRY(w_current->iwidth_entry));
	c_height = gtk_entry_get_text(GTK_ENTRY(w_current->iheight_entry));
*/

	if (filename[0] != '\0') { 

		width = w_current->image_width;
		height = w_current->image_height;

		save_width = w_current->width;
        	save_height = w_current->height;

        	w_current->width = width;
        	w_current->height = height;

		/* need to do this every time you change width / height */
		set_window(w_current, 
			w_current->page_current->left, 
			w_current->page_current->right,
                   	w_current->page_current->top, 
			w_current->page_current->bottom);
	

		/* try to use recalc here */
		o_redraw_all(w_current);

		f_image_write(w_current, filename, width, height, w_current->image_color);

		w_current->width = save_width;
		w_current->height = save_height;

        
		/* try to use recalc here... */
		o_redraw_all(w_current);

		/* need to do this every time you change width / height */
		set_window(w_current, 
			w_current->page_current->left, 
			w_current->page_current->right,
                   	w_current->page_current->top, 
			w_current->page_current->bottom);

		if (w_current->image_color == TRUE) {
	        	s_log_message("Wrote color image to [%s] [%d x %d]\n", filename, width, height);
		} else {
	        	s_log_message("Wrote black and white image to [%s] [%d x %d]\n", filename, width, height);
		}
	}

	gtk_widget_destroy(w_current->iwindow);
	w_current->iwindow=NULL;
	return(0);
}

gint
x_image_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
	/* gtk_grab_remove(w_current->iwindow);*/
	gtk_widget_destroy(w_current->iwindow);
	w_current->iwindow=NULL;	
	return(0);
}


void
x_image_setup (TOPLEVEL *w_current, char *filename)
{
	GtkWidget *label;
	GtkWidget *separator;
	GtkWidget *box;
	GtkWidget *buttonwrite;
	GtkWidget *buttoncancel;
	GtkWidget *optionmenu;

	/* freeze the window_current pointer so that it doesn't change */

	if (!w_current->iwindow) {

		w_current->iwindow = gtk_dialog_new ();
	
		gtk_window_position (GTK_WINDOW (w_current->iwindow), 
			GTK_WIN_POS_MOUSE);

		gtk_signal_connect (GTK_OBJECT (w_current->iwindow), "destroy",
			GTK_SIGNAL_FUNC(destroy_window),
			&w_current->iwindow);

		gtk_signal_connect (GTK_OBJECT (w_current->iwindow), "delete_event",
			GTK_SIGNAL_FUNC(destroy_window),
			&w_current->iwindow);

		gtk_window_set_title (GTK_WINDOW (w_current->iwindow), "Write Image...");

		buttonwrite = gtk_button_new_with_label ("Write");
		GTK_WIDGET_SET_FLAGS (buttonwrite, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
			GTK_DIALOG(w_current->iwindow)->action_area),
			buttonwrite, TRUE, TRUE, 0);
		gtk_signal_connect (GTK_OBJECT (buttonwrite), "clicked",
			GTK_SIGNAL_FUNC(x_image_write), w_current);
		gtk_widget_show (buttonwrite);
		gtk_widget_grab_default (buttonwrite);

		buttoncancel = gtk_button_new_with_label ("Close");
		GTK_WIDGET_SET_FLAGS (buttoncancel, GTK_CAN_DEFAULT);
		gtk_box_pack_start (GTK_BOX (
			GTK_DIALOG(w_current->iwindow)->action_area),
			buttoncancel, TRUE, TRUE, 0);
		gtk_signal_connect ( GTK_OBJECT(buttoncancel),
                        "clicked", GTK_SIGNAL_FUNC(x_image_cancel),
                        w_current);
		gtk_widget_show (buttoncancel);



#if 0
		separator = gtk_hseparator_new ();
	        gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->iwindow)->vbox), 
			separator, FALSE, TRUE, 0);
  		gtk_widget_show (separator); 
#endif


		box = gtk_vbox_new(FALSE, 0);
        	gtk_container_border_width(GTK_CONTAINER(box), 5);
        	gtk_container_add(GTK_CONTAINER(GTK_DIALOG(w_current->iwindow)->vbox), box);
        	gtk_widget_show(box);            

#if 0
		label = gtk_label_new ("Width");
		gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
                gtk_misc_set_padding (GTK_MISC (label), 0, 0);
                gtk_box_pack_start (GTK_BOX (box),
                          label, FALSE, FALSE, 0);
                gtk_widget_show (label); 

		w_current->iwidth_entry = gtk_entry_new_with_max_length (5);
                gtk_editable_select_region (GTK_EDITABLE (w_current->iwidth_entry), 0, -1);
                gtk_box_pack_start (GTK_BOX (box),
                          w_current->iwidth_entry, TRUE, TRUE, 10);
/* 
		gtk_signal_connect(GTK_OBJECT(w_current->width_entry), 
			"activate",
                       GTK_SIGNAL_FUNC(x_image_write),
                       w_current);
*/
                gtk_widget_show (w_current->iwidth_entry); 

		label = gtk_label_new ("Height");
		gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
                gtk_misc_set_padding (GTK_MISC (label), 0, 0);
                gtk_box_pack_start (GTK_BOX (box),
                          label, FALSE, FALSE, 0);
                gtk_widget_show (label); 

		w_current->iheight_entry = gtk_entry_new_with_max_length (5);
                gtk_editable_select_region (GTK_EDITABLE (w_current->iheight_entry), 0, -1);
                gtk_box_pack_start (GTK_BOX (box),
                          w_current->iheight_entry, TRUE, TRUE, 10);
/* 
		gtk_signal_connect(GTK_OBJECT(w_current->height_entry), 
			"activate",
                       GTK_SIGNAL_FUNC(x_image_write),
                       w_current);
*/
                gtk_widget_show (w_current->iheight_entry); 
#endif
		label = gtk_label_new ("Width x Height");
		gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
                gtk_misc_set_padding (GTK_MISC (label), 0, 0);
                gtk_box_pack_start (GTK_BOX (box),
                          label, FALSE, FALSE, 0);
                gtk_widget_show (label); 

                optionmenu = gtk_option_menu_new ();
                gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu), create_menu_size (w_current));
                gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), 2);
                gtk_box_pack_start (GTK_BOX (box), optionmenu, TRUE, TRUE, 0);
                gtk_widget_show (optionmenu);

		label = gtk_label_new ("Filename");
		gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
                gtk_misc_set_padding (GTK_MISC (label), 0, 0);
                gtk_box_pack_start (GTK_BOX (box),
                          label, FALSE, FALSE, 0);
                gtk_widget_show (label); 

		w_current->ifilename_entry = gtk_entry_new_with_max_length (79);
                gtk_editable_select_region (GTK_EDITABLE (w_current->ifilename_entry), 0, -1);
                gtk_box_pack_start (GTK_BOX (box),
                          w_current->ifilename_entry, TRUE, TRUE, 10);
		gtk_signal_connect(GTK_OBJECT(w_current->ifilename_entry), 
			"activate",
                       GTK_SIGNAL_FUNC(x_image_write),
                       w_current);
                gtk_widget_show (w_current->ifilename_entry); 

		separator = gtk_hseparator_new ();
	        gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->iwindow)->vbox), 
			separator, FALSE, TRUE, 0);
  		gtk_widget_show (separator); 

	}

	if (!GTK_WIDGET_VISIBLE (w_current->iwindow)) {
        	gtk_entry_set_text(GTK_ENTRY(w_current->ifilename_entry), filename);
        	/* gtk_entry_set_text(GTK_ENTRY(w_current->iwidth_entry), "800");
        	gtk_entry_set_text(GTK_ENTRY(w_current->iheight_entry), "600");*/

        	/*gtk_entry_select_region(GTK_ENTRY(w_current->ifilename_entry), 0, strlen(filename)); 	*/
		w_current->image_width = 800;
		w_current->image_height = 600;
		gtk_widget_show (w_current->iwindow);
		gdk_window_raise(w_current->iwindow->window); 
		/* gtk_grab_add (w_current->iwindow);*/
 	} else {
		/* window should already be mapped */
		/* otherwise this will core */
		gdk_window_raise(w_current->iwindow->window); 
	}
}

