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

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/prototype.h"

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
	{"/File/Close Page", NULL, i_callback_page_close, 0, NULL},
	{"/File/Save Page", NULL, i_callback_file_save, 0, NULL},
	{"/File/Save Page as....", NULL, i_callback_file_save_as, 0, NULL},
	{"/File/Save All", NULL, i_callback_file_save_all, 0, NULL},
	{"/File/Print...", NULL, i_callback_file_print, 0, NULL},
	{"/File/Write PNG...", NULL, i_callback_file_write_png, 0, NULL},
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
	{"/Edit/Stretch Mode", NULL, i_callback_edit_stretch, 0, NULL},
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
	{"/View/Update Cues", NULL,  i_callback_view_update_cues, 0, NULL},
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
	{"/Add/Bus", NULL, i_callback_add_bus, 0, NULL},
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
	{"/Options/Show Log Window", NULL, i_callback_options_show_log_window, 0, NULL},
	{"/Options/Show Coord Window", NULL, i_callback_options_show_coord_window, 0, NULL},
        {"/Help", NULL, NULL, 0, "<LastBranch>"},
  	{"/Help/tearoff", NULL, NULL, 0, "<Tearoff>" },
        {"/Help/About...", NULL, i_callback_help_about, 0, NULL},
        {"/Help/Hotkeys...", NULL, i_callback_help_hotkeys, 0, NULL},
};

static GtkItemFactoryEntry popup_items[] = {
	{"/Net", 		NULL, i_callback_add_net, 0, NULL},
	{"/Attribute...", 	NULL, i_callback_add_attribute, 0, NULL},
	{"/Component...", 	NULL, i_callback_add_component, 0, NULL},
	{"/Bus", 		NULL, i_callback_add_bus, 0, NULL},
	{"/Text", 		NULL, i_callback_add_text, 0, NULL},
  	{"/sep1", NULL, NULL, 0, "<Separator>"},
	{"/Zoom in",            NULL, i_callback_view_zoom_in, 0, NULL},
	{"/Zoom out",           NULL, i_callback_view_zoom_out, 0, NULL},
  	{"/sep1", NULL, NULL, 0, "<Separator>"},
	{"/Select", 		NULL, i_callback_edit_select, 0, NULL},
	{"/Edit...", 		NULL, i_callback_edit_edit, 0, NULL},
	{"/Copy", 		NULL, i_callback_edit_copy, 0, NULL},
	{"/Move", 		NULL, i_callback_edit_move, 0, NULL},
	{"/Delete", 		NULL, i_callback_edit_delete, 0, NULL},
};

/* calculate the number of menu_item's */
/* no longer static */
int nmenu_items = sizeof(menu_items) / sizeof(menu_items[0]);

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

