/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003 Stuart D. Brorson.
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

/*------------------------------------------------------------------
 * This file holds fcns used to display dialog boxes.  
 *------------------------------------------------------------------*/


/*------------------------------------------------------------------
 * Includes required to run graphical widgets.
 *------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkkeysyms.h>

#include <glib.h>
#ifdef HAS_GTK22
#include <glib-object.h>
#endif


#ifdef HAVE_STRING_H
#include <string.h>
#endif


#ifdef HAS_GTK22
#include "gtksheet_2_2.h"
#include "gtkitementry_2_2.h"
#else
#include "gtksheet_1_2.h"
#include "gtkitementry_1_2.h"
#endif


/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <config.h>
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/x_menu.h"

/* --------------------------------------------------------- *
 * Help/about dialog box fcns
 * --------------------------------------------------------- */
int x_dialog_about_keypress_callback(GtkWidget * widget, GdkEventKey * event,
			    GtkWidget *window)
{
  if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
#ifdef DEBUG
    printf("In x_dialog_about_keypress, trying to close window.\n");
#endif
    x_dialog_close_window(window);
    return TRUE;
  }

  return FALSE;
}

/* --------------------------------------------------------- */
int x_dialog_about_close_callback(GtkWidget * widget, GtkWidget *window)
{
  x_dialog_close_window(window);
}


/* --------------------------------------------------------- */
void x_dialog_about_dialog()
{
  GtkWidget *about_window;
  GtkWidget *label = NULL;
  GtkWidget *buttonclose = NULL;
  GtkWidget *vbox, *action_area;
  char *string;


  about_window = x_dialog_create_dialog_box(&vbox, &action_area);
  /*   about_window =  gtk_window_new(GTK_WINDOW_POPUP);  */

  gtk_window_position(GTK_WINDOW(about_window),
		      GTK_WIN_POS_MOUSE);
  
  gtk_window_set_title(GTK_WINDOW(about_window), "About...");
  gtk_container_border_width(GTK_CONTAINER(about_window), 5);
  
  gtk_signal_connect(GTK_OBJECT(about_window),
		     "destroy", GTK_SIGNAL_FUNC(x_dialog_about_close_callback),
		     GTK_WIDGET(about_window) );
  
  gtk_signal_connect(GTK_OBJECT(about_window),
		     "key_press_event", GTK_SIGNAL_FUNC(x_dialog_about_keypress_callback),
		     GTK_WIDGET(about_window) );

  /* Now create text string to place in vbox area */
  string = g_strdup_printf("gEDA : GPL Electronic Design Automation");
  label = gtk_label_new(string);
  free(string);
  gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 5);
  gtk_widget_show(label);
  
  string = g_strdup_printf("This is gattrib -- gEDA's attribute editor");
  label = gtk_label_new(string);
  free(string);
  gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 5);
  gtk_widget_show(label);
  
  string = g_strdup_printf("Gattrib version: %s", VERSION);
  label = gtk_label_new(string);
  free(string);
  gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 5);
  gtk_widget_show(label);
  
  string = 
    g_strdup_printf("Gattrib is written by: Stuart Brorson (sdb@cloud9.net)\n");
  string = 
    g_strdup_printf("%swith generous helpings of code from gschem, gnetlist, \n", 
		    string);
  string =
    g_strdup_printf("%sand gtkextra, as well as support from the gEDA community.", string);
  label = gtk_label_new(string);
  free(string);
  gtk_box_pack_start(GTK_BOX(vbox), label, TRUE, TRUE, 5);
  gtk_widget_show(label);
  
  /* Now create button to stick in action area */
  buttonclose = gtk_button_new_with_label("Close");
  GTK_WIDGET_SET_FLAGS(buttonclose, GTK_CAN_DEFAULT);
  gtk_box_pack_start(GTK_BOX(action_area), buttonclose, TRUE, TRUE, 0);
  gtk_signal_connect(GTK_OBJECT(buttonclose), "clicked",
		     GTK_SIGNAL_FUNC(x_dialog_about_close_callback), 
		     GTK_WIDGET(about_window) );
  gtk_widget_show(buttonclose);

  if (!GTK_WIDGET_VISIBLE(about_window)) {
    gtk_widget_show(about_window);
  }
}


/* --------------------------------------------------------- *
 * Fcns common to all dialog boxes
 * This code also stolen from gschem & adapted for gattrib. 
 * --------------------------------------------------------- */

/* ---------------------------------------------------- *
 * This creates a dialog box.  It has two areas: the vbox
 * area, and the action area.  The idea is that the vbox
 * area holds text, and the action area holds buttons or
 * other active widgets.  There is a separating line between
 * the two area.  You load one or the other areas like this (for example):
 * gtk_box_pack_start(GTK_BOX(action_area), buttonclose, TRUE, TRUE, 0);
 * --------------------------------------------------------- */
GtkWidget *x_dialog_create_dialog_box(GtkWidget ** out_vbox,
                               GtkWidget ** out_action_area)
{
  GtkWidget *separator;
  GtkWidget *vbox;
  GtkWidget *action_area;
  GtkWidget *dialog;

  if (!out_vbox)
    return (NULL);

  if (!out_action_area)
    return (NULL);

  dialog = gtk_window_new(GTK_WINDOW_TOPLEVEL);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_add(GTK_CONTAINER(dialog), vbox);
  gtk_widget_show(vbox);

  action_area = gtk_hbox_new(TRUE, 5);
  gtk_container_set_border_width(GTK_CONTAINER(action_area), 10);
  gtk_box_pack_end(GTK_BOX(vbox), action_area, FALSE, TRUE, 0);
  gtk_widget_show(action_area);

  separator = gtk_hseparator_new();
  gtk_box_pack_end(GTK_BOX(vbox), separator, FALSE, TRUE, 0);
  gtk_widget_show(separator);

  *out_vbox = vbox;
  *out_action_area = action_area;

  return (dialog);
}

/* ---------------------------------------------------- */
void x_dialog_close_window(GtkWidget * window)
{
  gtk_widget_destroy(window);
}




