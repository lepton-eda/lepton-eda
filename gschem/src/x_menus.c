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
#include <gtk/gtk.h>

#include <guile/gh.h>

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif


#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>
#include <libgeda/prototype.h>

#include "../include/prototype.h"


#ifndef GTK_DEVEL 
void menus_init(TOPLEVEL *w_current);
void menus_create(TOPLEVEL *w_current, GtkMenuEntry * entries, int nmenu_entries);
#endif


/* gtk+ 1.0.x menu definitions */
#ifndef GTK_DEVEL 
    /* this is the GtkMenuEntry structure used to create new menus.  The
     * first member is the menu definition string.  The second, the
     * default accelerator key used to access this menu function with
     * the keyboard.  The third is the callback function to call when
     * this menu item is selected (by the accelerator key, or with the
     * mouse.) The last member is the data to pass to your callback function.
     */
/* no longer static */
/* but it can be static again */
GtkMenuEntry menu_items[] =
{
	{"<Main>/File/New Window", NULL, i_callback_file_new_window, NULL},
	{"<Main>/File/New Page", NULL, i_callback_file_new, NULL},
	{"<Main>/File/Open Page...", NULL, i_callback_file_open, NULL},
	{"<Main>/File/Save Page", NULL, i_callback_file_save, NULL},
	{"<Main>/File/Save Page as....", NULL, i_callback_file_save_as, NULL},
	{"<Main>/File/Save All", NULL, i_callback_file_save_all, NULL},
	{"<Main>/File/Print...", NULL, i_callback_file_print, NULL},
	{"<Main>/File/<separator>", NULL, NULL, NULL},
	{"<Main>/File/Script Execute...", NULL, i_callback_file_script, NULL},
	{"<Main>/File/<separator>", NULL, NULL, NULL},
	{"<Main>/File/Close Window", NULL, i_callback_file_close, NULL},
	{"<Main>/File/Quit gschem", NULL, i_callback_file_quit, NULL},
	/* because i_callback_file_quit returns an int, the compiler complains*/
	{"<Main>/Edit/Select Mode", NULL, i_callback_edit_select, NULL},
	{"<Main>/Edit/Edit...", NULL, i_callback_edit_edit, NULL},
	{"<Main>/Edit/Copy Mode", NULL, i_callback_edit_copy, NULL},
	{"<Main>/Edit/Move Mode", NULL, i_callback_edit_move, NULL},
	{"<Main>/Edit/Delete", NULL, i_callback_edit_delete, NULL},
	{"<Main>/Edit/Rotate 90 Mode", NULL, i_callback_edit_rotate_90, NULL},
	{"<Main>/Edit/Mirror Mode", NULL, i_callback_edit_mirror, NULL},
	{"<Main>/Edit/<separator>", NULL, NULL, NULL},
	{"<Main>/Edit/Slot...", NULL, i_callback_edit_slot, NULL},
	{"<Main>/Edit/Color...", NULL, i_callback_edit_color, NULL},
	{"<Main>/Edit/Lock", NULL, i_callback_edit_lock, NULL},
	{"<Main>/Edit/Unlock", NULL, i_callback_edit_unlock, NULL},
	{"<Main>/Edit/Symbol Translate...", NULL, i_callback_edit_translate, NULL},
	{"<Main>/Edit/Embed Component", NULL, i_callback_edit_embed, NULL},
	{"<Main>/Edit/Unembed Component", NULL, i_callback_edit_unembed, NULL},
	{"<Main>/Edit/Show Hidden Text", NULL, i_callback_edit_show_hidden, NULL},
	{"<Main>/View/Redraw", NULL, i_callback_view_redraw, NULL},
	{"<Main>/View/Pan", NULL, i_callback_view_pan, NULL},
	{"<Main>/View/Zoom box", NULL, i_callback_view_zoom_box, NULL},
	{"<Main>/View/Zoom limits", NULL, i_callback_view_zoom_limits, NULL},
	{"<Main>/View/Zoom in", NULL, i_callback_view_zoom_in, NULL},
	{"<Main>/View/Zoom out", NULL, i_callback_view_zoom_out, NULL},
	{"<Main>/View/Zoom full", NULL, i_callback_view_zoom_full, NULL},
	{"<Main>/View/Update nets", NULL,  i_callback_view_updatenets, NULL},
	/* change name of update nets */
	{"<Main>/Page/Manager...", NULL,  i_callback_page_manager, NULL},
	{"<Main>/Page/Next", NULL,  i_callback_page_next, NULL},
	{"<Main>/Page/Prev", NULL,  i_callback_page_prev, NULL},
	{"<Main>/Page/Close", NULL,  i_callback_page_close, NULL},
	{"<Main>/Page/Discard", NULL,  i_callback_page_discard, NULL},
	{"<Main>/Page/Print struct", NULL,  i_callback_page_print, NULL},

	{"<Main>/Add/Component...", NULL, i_callback_add_component, NULL},
	{"<Main>/Add/Net", NULL, i_callback_add_net, NULL},
	{"<Main>/Add/Attribute...", NULL, i_callback_add_attribute, NULL},
	{"<Main>/Add/Text", NULL, i_callback_add_text, NULL},
	{"<Main>/Add/<separator>", NULL, NULL, NULL},
	{"<Main>/Add/Line", NULL, i_callback_add_line, NULL},
	{"<Main>/Add/Box", NULL, i_callback_add_box, NULL},
	{"<Main>/Add/Circle", NULL, i_callback_add_circle, NULL},
	{"<Main>/Add/Arc", NULL, i_callback_add_arc, NULL},
	{"<Main>/Add/Pin", NULL, i_callback_add_pin, NULL},
	{"<Main>/Hierarchy/Open Symbol", NULL, i_callback_hierarchy_open_symbol, NULL},
	{"<Main>/Attributes/Attach", NULL, i_callback_attributes_attach, NULL},
	{"<Main>/Attributes/Deattach", NULL, i_callback_attributes_detach, NULL},
	{"<Main>/Attributes/Show Value", NULL, i_callback_attributes_show_value, NULL},
	{"<Main>/Attributes/Show Name", NULL, i_callback_attributes_show_name, NULL},
	{"<Main>/Attributes/Show Both", NULL, i_callback_attributes_show_both, NULL},
	{"<Main>/Attributes/Toggle Vis", NULL, i_callback_attributes_visibility_toggle, NULL},
	{"<Main>/Options/Text Size...", NULL, i_callback_options_text_size, NULL},
	{"<Main>/Options/Toggle Grid", NULL, i_callback_options_grid, NULL},
	{"<Main>/Options/Toggle Snap", NULL, i_callback_options_snap, NULL},
	{"<Main>/Options/Snap Grid Spacing...", NULL, i_callback_options_snap_size, NULL},
	{"<Main>/Options/Toggle Outline", NULL, i_callback_options_afeedback, NULL},
	{"<Main>/Options/Show Log Window", NULL, i_callback_options_show_status, NULL},
	{"<Main>/Options/Show Coord Window", NULL, i_callback_options_show_coord, NULL},
	{"<Main>/Help/About...", NULL, i_callback_help_about, NULL},

	{"<Popup>/Component...", NULL, i_callback_add_component, NULL},
	{"<Popup>/Net", NULL, i_callback_add_net, NULL},
	{"<Popup>/Attribute...", NULL, i_callback_add_attribute, NULL},
	{"<Popup>/Text", NULL, i_callback_add_text, NULL},
	{"<Popup>/<separator>", NULL, NULL, NULL},
	{"<Popup>/Select", NULL, i_callback_edit_select, NULL},
	{"<Popup>/Edit...", NULL, i_callback_edit_edit, NULL},
	{"<Popup>/Copy", NULL, i_callback_edit_copy, NULL},
	{"<Popup>/Move", NULL, i_callback_edit_move, NULL},
	{"<Popup>/Delete", NULL, i_callback_edit_delete, NULL},
};
#endif

#if GTK_DEVEL
void
print_hello (gpointer data, guint callback_action, 
		GtkWidget *widget)
{
  TOPLEVEL *w_current;
  
  w_current = (TOPLEVEL *) data;

  exit_if_null(w_current); 

  printf("inside callback: %d %s\n", w_current->wid, w_current->untitled_name);
}

/* gtk+ 1.2.x menu definitions */
static GtkItemFactoryEntry menu_items[] = {
	{"/File", NULL, NULL, 0, "<Branch>"},
  	{"/File/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/File/New Window", NULL, i_callback_file_new_window, 0, NULL},
	{"/File/New Page", NULL, i_callback_file_new, 0, NULL},
	{"/File/Open Page...", NULL, i_callback_file_open, 0, NULL},
	{"/File/Save Page", NULL, i_callback_file_save, 0, NULL},
	{"/File/Save Page as....", NULL, i_callback_file_save_as, 0, NULL},
	{"/File/Save All", NULL, i_callback_file_save_all, 0, NULL},
	{"/File/Print...", NULL, i_callback_file_print, 0, NULL},
	{"/File/sep1",     NULL,         NULL, 0, "<Separator>"},
	{"/File/Script Execute...", NULL, i_callback_file_script, 0, NULL},
	{"/File/sep2",     NULL,         NULL, 0, "<Separator>"},
	{"/File/Close Window", NULL, i_callback_file_close, 0, NULL},
	{"/File/Quit gschem", NULL, i_callback_file_quit, 0, NULL},
	/* because i_callback_file_quit returns an int, the compiler complains*/
	{"/Edit", NULL, NULL, 0, "<Branch>"},
  	{"/Edit/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/Edit/Select Mode", NULL, i_callback_edit_select, 0, NULL},
	{"/Edit/Edit...", NULL, i_callback_edit_edit, 0, NULL},
	{"/Edit/Copy Mode", NULL, i_callback_edit_copy, 0, NULL},
	{"/Edit/Move Mode", NULL, i_callback_edit_move, 0, NULL},
	{"/Edit/Delete", NULL, i_callback_edit_delete, 0, NULL},
	{"/Edit/Rotate 90 Mode", NULL, i_callback_edit_rotate_90, 0, NULL},
	{"/Edit/Mirror Mode", NULL, i_callback_edit_mirror, 0, NULL},
	{"/Edit/sep1",     NULL,         NULL, 0, "<Separator>"},
	{"/Edit/Slot...", NULL, i_callback_edit_slot, 0, NULL},
	{"/Edit/Color...", NULL, i_callback_edit_color, 0, NULL},
	{"/Edit/Lock", NULL, i_callback_edit_lock, 0, NULL},
	{"/Edit/Unlock", NULL, i_callback_edit_unlock, 0, NULL},
	{"/Edit/Symbol Translate...", NULL, i_callback_edit_translate, 0, NULL},
	{"/Edit/Embed Component", NULL, i_callback_edit_embed, 0, NULL},
	{"/Edit/Unembed Component", NULL, i_callback_edit_unembed, 0, NULL},
	{"/Edit/Show Hidden Text", NULL, i_callback_edit_show_hidden, 0, NULL},
	{"/View", NULL, NULL, 0, "<Branch>"},
  	{"/View/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/View/Redraw", NULL, i_callback_view_redraw, 0, NULL},
	{"/View/Pan", NULL, i_callback_view_pan, 0, NULL},
	{"/View/Zoom box", NULL, i_callback_view_zoom_box, 0, NULL},
	{"/View/Zoom limits", NULL, i_callback_view_zoom_limits, 0, NULL},
	{"/View/Zoom in", NULL, i_callback_view_zoom_in, 0, NULL},
	{"/View/Zoom out", NULL, i_callback_view_zoom_out, 0, NULL},
	{"/View/Zoom full", NULL, i_callback_view_zoom_full, 0, NULL},
	{"/View/Update nets", NULL,  i_callback_view_updatenets, 0, NULL},
	/* change name of update nets */
	{"/Page", NULL, NULL, 0, "<Branch>"},
  	{"/Page/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/Page/Manager...", NULL,  i_callback_page_manager, 0, NULL},
	{"/Page/Next", NULL,  i_callback_page_next, 0, NULL},
	{"/Page/Prev", NULL,  i_callback_page_prev, 0, NULL},
	{"/Page/Close", NULL,  i_callback_page_close, 0, NULL},
	{"/Page/Discard", NULL,  i_callback_page_discard, 0, NULL},
	{"/Page/Print struct", NULL,  i_callback_page_print, 0, NULL},

	{"/Add", NULL, NULL, 0, "<Branch>"},
  	{"/Add/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/Add/Component...", NULL, i_callback_add_component, 0, NULL},
	{"/Add/Net", NULL, i_callback_add_net, 0, NULL},
	{"/Add/Attribute...", NULL, i_callback_add_attribute, 0, NULL},
	{"/Add/Text", NULL, i_callback_add_text, 0, NULL},
	{"/Add/sep1",     NULL,         NULL, 0, "<Separator>"},
	{"/Add/Line", NULL, i_callback_add_line, 0, NULL},
	{"/Add/Box", NULL, i_callback_add_box, 0, NULL},
	{"/Add/Circle", NULL, i_callback_add_circle, 0, NULL},
	{"/Add/Arc", NULL, i_callback_add_arc, 0, NULL},
	{"/Add/Pin", NULL, i_callback_add_pin, 0, NULL},
	{"/Hierarchy", NULL, NULL, 0, "<Branch>"},
  	{"/Hierarchy/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/Hierarchy/Open Symbol", NULL, i_callback_hierarchy_open_symbol, 0, NULL},
	{"/Attributes", NULL, NULL, 0, "<Branch>"},
  	{"/Attributes/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/Attributes/Attach", NULL, i_callback_attributes_attach, 0, NULL},
	{"/Attributes/Deattach", NULL, i_callback_attributes_detach, 0, NULL},
	{"/Attributes/Show Value", NULL, i_callback_attributes_show_value, 0, NULL},
	{"/Attributes/Show Name", NULL, i_callback_attributes_show_name, 0, NULL},
	{"/Attributes/Show Both", NULL, i_callback_attributes_show_both, 0, NULL},
	{"/Attributes/Toggle Vis", NULL, i_callback_attributes_visibility_toggle, 0, NULL},
	{"/Options", NULL, NULL, 0, "<Branch>"},
  	{"/Options/tearoff", NULL, NULL, 0, "<Tearoff>" },
	{"/Options/Text Size...", NULL, i_callback_options_text_size, 0, NULL},
	{"/Options/Toggle Grid", NULL, i_callback_options_grid, 0, NULL},
	{"/Options/Toggle Snap", NULL, i_callback_options_snap, 0, NULL},
	{"/Options/Snap Grid Spacing...", NULL, i_callback_options_snap_size, 0, NULL},
	{"/Options/Toggle Outline", NULL, i_callback_options_afeedback, 0, NULL},
	{"/Options/Show Log Window", NULL, i_callback_options_show_status, 0, NULL},
	{"/Options/Show Coord Window", NULL, i_callback_options_show_coord, 0, NULL},
        {"/Help", NULL, NULL, 0, "<LastBranch>"},
        {"/Help/About", NULL, i_callback_help_about, 0, NULL},
};

static GtkItemFactoryEntry popup_items[] = {
	{"/Component...", 	NULL, i_callback_add_component, 0, NULL},
	{"/Net", 		NULL, i_callback_add_net, 0, NULL},
	{"/Attribute...", 	NULL, i_callback_add_attribute, 0, NULL},
	{"/Text", 		NULL, i_callback_add_text, 0, NULL},
  	{"/sep1", NULL, NULL, 0, "<Separator>"},
	{"/Select", 		NULL, i_callback_edit_select, 0, NULL},
	{"/Edit...", 		NULL, i_callback_edit_edit, 0, NULL},
	{"/Copy", 		NULL, i_callback_edit_copy, 0, NULL},
	{"/Move", 		NULL, i_callback_edit_move, 0, NULL},
	{"/Delete", 		NULL, i_callback_edit_delete, 0, NULL},
};
#endif

/* calculate the number of menu_item's */
/* no longer static */
int nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);

#if GTK_DEVEL
int npopup_items = sizeof(popup_items) / sizeof(popup_items[0]);

void 
get_main_menu(TOPLEVEL *w_current, GtkWidget ** menubar)
{
  GtkItemFactory *item_factory;
  GtkAccelGroup *accel_group;

  accel_group = gtk_accel_group_new();
  /* This function initializes the item factory.
     Param 1: The type of menu - can be GTK_TYPE_MENU_BAR, GTK_TYPE_MENU,
              or GTK_TYPE_OPTION_MENU.
     Param 2: The path of the menu.
     Param 3: A pointer to a gtk_accel_group.  The item factory sets up
              the accelerator table while generating menus.
  */

  item_factory = gtk_item_factory_new(GTK_TYPE_MENU_BAR, "<main>", 
                               accel_group);
  /* This function generates the menu items. Pass the item factory,
     the number of items in the array, the array itself, and any
     callback data for the the menu items. */
  gtk_item_factory_create_items(item_factory, nmenu_items, menu_items, w_current);

  /* Attach the new accelerator group to the window. */
  gtk_accel_group_attach (accel_group, GTK_OBJECT (w_current->main_window));

  if (menubar)
    /* Finally, return the actual menu bar created by the item factory. */ 
    *menubar = gtk_item_factory_get_widget(item_factory, "<main>");
}
#endif

#if GTK_DEVEL
void
get_main_popup(TOPLEVEL *w_current, GtkWidget ** menu)
{
  GtkItemFactory *item_factory;
  GtkAccelGroup *accel_group;

  accel_group = gtk_accel_group_new();
  /* This function initializes the item factory.
     Param 1: The type of menu - can be GTK_TYPE_MENU_BAR, GTK_TYPE_MENU,
              or GTK_TYPE_OPTION_MENU.
     Param 2: The path of the menu.
     Param 3: A pointer to a gtk_accel_group.  The item factory sets up
              the accelerator table while generating menus.
  */

  item_factory = gtk_item_factory_new(GTK_TYPE_MENU, "<popup>", 
                               accel_group);
  /* This function generates the menu items. Pass the item factory,
     the number of items in the array, the array itself, and any
     callback data for the the menu items. */
  gtk_item_factory_create_items(item_factory, npopup_items, popup_items, w_current);

  /* Attach the new accelerator group to the window. */
  gtk_accel_group_attach (accel_group, GTK_OBJECT (w_current->main_window));

  if (menu)
    /* Finally, return the actual menu bar created by the item factory. */ 
    *menu = gtk_item_factory_get_widget(item_factory, "<popup>");
}
#endif


/************************************************************************/
/************************************************************************/
/******** Here starts gtk+ 1.0.x functions for the menus ****************/
/************************************************************************/
/************************************************************************/

#ifndef GTK_DEVEL
void 
get_main_menu(TOPLEVEL *w_current, GtkWidget ** menubar, GtkAcceleratorTable ** table)
{
	int i;
	GtkMenuPath *menu_path;

	w_current->factory = NULL;	
	w_current->entry_ht = NULL;	

	/* Init the callback data to the passed in window */
	for ( i = 0 ; i < nmenu_items; i++) {
		menu_items[i].callback_data = w_current;	
	}

	menus_init(w_current);
        
        if (menubar)
                *menubar = w_current->subfactory[0]->widget;


        if (table)
                *table = w_current->subfactory[0]->table;

	menu_path = gtk_menu_factory_find (w_current->factory, "<Main>/Help");
	gtk_menu_item_right_justify(GTK_MENU_ITEM(menu_path->widget));
}
#endif

#ifndef GTK_DEVEL
void
get_main_popup(TOPLEVEL *w_current, GtkWidget ** menu, GtkAcceleratorTable ** table)
{
	/* be sure that you called get_main_menu first */
	
        if (menu)
                *menu = w_current->subfactory[1]->widget;

        if (table)
                *table = w_current->subfactory[1]->table;
}
#endif

#ifndef GTK_DEVEL
void 
menus_init(TOPLEVEL *w_current)
{
           
        w_current->factory = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);

        w_current->subfactory[0] = gtk_menu_factory_new(GTK_MENU_FACTORY_MENU_BAR);
       	gtk_menu_factory_add_subfactory(w_current->factory, w_current->subfactory[0], "<Main>");

	w_current->subfactory[1] = gtk_menu_factory_new (GTK_MENU_FACTORY_MENU);
	gtk_menu_factory_add_subfactory (w_current->factory, w_current->subfactory[1], "<Popup>"); 

	/* menurc? hack */
	menus_create(w_current, menu_items, nmenu_items);
}

void 
menus_create(TOPLEVEL *w_current, GtkMenuEntry * entries, int nmenu_entries)
{
	char *accelerator;
        int i;
        
        if (w_current->entry_ht)
                for (i = 0; i < nmenu_entries; i++) {
                    accelerator = g_hash_table_lookup(w_current->entry_ht, entries[i].path);
			if (accelerator) {
				if (accelerator[0] == '\0')
                               		entries[i].accelerator = NULL;
                        	else
                                	entries[i].accelerator = accelerator;
                    	}
                }

        gtk_menu_factory_add_entries(w_current->factory, entries, nmenu_entries);
       
}
#endif

/************************************************************************/
/************************************************************************/
/*********** End of gtk+ 1.0.x functions for the menus ******************/
/************************************************************************/
/************************************************************************/



/* need to look at this... here and the setup */
gint
do_popup (TOPLEVEL *w_current, GdkEventButton *event)
{
	GtkWidget *menu=NULL; /* was static */

	if (!menu)
		menu = w_current->popup_menu; 

	if (menu == NULL) {
		printf("null menu\n");
	}

	gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 
		event->button, event->time);

	return FALSE;
}

