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

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/colors.h>   
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

static const   gchar   *list_item_data_key="list_item_data";	


#if 0
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
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_landscape,
                              w_current);

	gtk_widget_show (menuitem);

	sprintf (buf, "Portrait");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) image_portrait,
                              w_current);
	gtk_widget_show (menuitem);

	if (w_current->image_orientation == PORTRAIT) {
		gtk_menu_set_active(GTK_MENU (menu),1);
		x_image_set_window (NULL, w_current);
	} else {
		image_landscape (NULL, w_current);
	} 

	return menu;
}
#endif

#if 0
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
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) x_image_set_limits,
                              w_current);

	gtk_widget_show (menuitem);

	sprintf (buf, "Current Window");
	menuitem = gtk_radio_menu_item_new_with_label (group, buf);
	group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
	gtk_menu_append (GTK_MENU (menu), menuitem);
	gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                              (GtkSignalFunc) x_image_set_window,
                              w_current);
	gtk_widget_show (menuitem);


	if (w_current->image_output_type == WINDOW) {
		gtk_menu_set_active(GTK_MENU (menu),1);
		f_image_set_type(w_current, WINDOW);
	} else {
		f_image_set_type(w_current, LIMITS);
	} 

	return menu;
}

gint
x_image_change_size (GtkWidget *gtklist, TOPLEVEL *w_current)
{
        GList		*dlist;
        GtkObject       *listitem;
        gchar           *item_data_string;
     
#if 0 
        dlist=GTK_LIST(w_current->plib_list)->selection;
        
        if (!dlist) {
            /* g_print("Selection cleared\n");*/
            return(0);
        }

        listitem=GTK_OBJECT(dlist->data);
        item_data_string=gtk_object_get_data(listitem, list_item_data_key);

#if DEBUG 
            printf("paper_size string: %s\n", item_data_string);
	len = strlen(item_data_string);
	/* strcpy(current_attr_name, item_data_string);*/
#endif
            


	s_papersizes_get_size(item_data_string, &w_current->paper_width, &w_current->paper_height);


#if 0
        gtk_entry_set_text(GTK_ENTRY(w_current->asentry_name), item_data_string);
        gtk_entry_select_region(GTK_ENTRY(w_current->asentry_name), 0, len); 	
#endif

#endif

	return(0);

}
#endif


gint
x_image_write(GtkWidget *w, TOPLEVEL *w_current)
{
	char *filename=NULL;
	char *c_width;
	char *c_height;
	int width, height;
	int save_height, save_width;

	filename = gtk_entry_get_text(GTK_ENTRY(w_current->ifilename_entry));

	c_width = gtk_entry_get_text(GTK_ENTRY(w_current->iwidth_entry));
	c_height = gtk_entry_get_text(GTK_ENTRY(w_current->iheight_entry));

	if (filename[0] != '\0' && c_width[0] != '\0' && c_height[0] != '\0') {

		width = atoi(c_width);
		height = atoi(c_height);

		save_width = w_current->width;
        	save_height = w_current->height;

        	w_current->width = width;
        	w_current->height = height;

		/* try to use recalc here */
		o_redraw_all(w_current);

		f_image_write(w_current, filename, width, height, w_current->image_color);

		w_current->width = save_width;
		w_current->height = save_height;
        
		/* try to use recalc here... */
		o_redraw_all(w_current);


		
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
	GtkWidget *box2;
	GtkWidget *buttonwrite;
	GtkWidget *buttoncancel;
	GtkWidget *scrolled_win;
	GtkWidget *list_item;
	GtkWidget *optionmenu;
	char *string=NULL;
	int i;

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



		separator = gtk_hseparator_new ();
	        gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->iwindow)->vbox), 
			separator, FALSE, TRUE, 0);
  		gtk_widget_show (separator); 


		box = gtk_vbox_new(FALSE, 0);
        	gtk_container_border_width(GTK_CONTAINER(box), 5);
        	gtk_container_add(GTK_CONTAINER(GTK_DIALOG(w_current->iwindow)->vbox), box);
        	gtk_widget_show(box);            

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

#if 0
		label = gtk_label_new ("Type");
                gtk_misc_set_padding (GTK_MISC (label), 5, 5);
                gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->iwindow)->vbox),
                          label, TRUE, TRUE, 0);
                gtk_widget_show (label); 
		optionmenu = gtk_option_menu_new ();
		gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu), create_menu_type (w_current));
		gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), 4);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->iwindow)->vbox), optionmenu, TRUE, TRUE, 0);
		gtk_widget_show (optionmenu);
#endif


#if 0
		label = gtk_label_new ("Orientation");
                gtk_misc_set_padding (GTK_MISC (label), 5, 5);
                gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->iwindow)->vbox),
                          label, TRUE, TRUE, 0);

                gtk_widget_show (label); 
		optionmenu = gtk_option_menu_new ();
		gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu), create_menu_orient (w_current));
		gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), 4);
		gtk_box_pack_start (GTK_BOX (GTK_DIALOG (w_current->iwindow)->vbox), optionmenu, TRUE, TRUE, 0);
		gtk_widget_show (optionmenu);
#endif

	}

	if (!GTK_WIDGET_VISIBLE (w_current->iwindow)) {
        	gtk_entry_set_text(GTK_ENTRY(w_current->ifilename_entry), filename);
        	gtk_entry_set_text(GTK_ENTRY(w_current->iwidth_entry), "800");
        	gtk_entry_set_text(GTK_ENTRY(w_current->iheight_entry), "600");

        	/*gtk_entry_select_region(GTK_ENTRY(w_current->ifilename_entry), 0, strlen(filename)); 	*/
		gtk_widget_show (w_current->iwindow);
		gdk_window_raise(w_current->iwindow->window); 
		/* gtk_grab_add (w_current->iwindow);*/
 	} else {
		/* window should already be mapped */
		/* otherwise this will core */
		gdk_window_raise(w_current->iwindow->window); 
	}
}

