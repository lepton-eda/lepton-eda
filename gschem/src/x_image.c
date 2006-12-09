/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GLADE_HOOKUP_OBJECT(component,widget,name) \
  g_object_set_data_full (G_OBJECT (component), name, \
    gtk_widget_ref (widget), (GDestroyNotify) gtk_widget_unref)

/* static const   gchar   *list_item_data_key="list_item_data";	*/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint image_320(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->image_width = 320;
  w_current->image_height = 240;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint image_640(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->image_width = 640;
  w_current->image_height = 480;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint image_800(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->image_width = 800;
  w_current->image_height = 600;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint image_1024(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->image_width = 1024;
  w_current->image_height = 768;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint image_1280(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->image_width = 1280;
  w_current->image_height = 960;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint image_1600(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->image_width = 1600;
  w_current->image_height = 1200;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint image_3200(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->image_width = 3200;
  w_current->image_height = 2400;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is from gtktest.c and only used in this file,
 *  there are other create_menus...
 */
static GtkWidget *create_menu_size(TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList *group;
  char *buf;

  menu = gtk_menu_new ();
  group = NULL;

  buf = g_strdup_printf("320x240");
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                      (GtkSignalFunc) image_320,
                      w_current);

  gtk_widget_show (menuitem);

  buf = g_strdup_printf("640x480");
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                      (GtkSignalFunc) image_640,
                      w_current);

  gtk_widget_show (menuitem);

  buf = g_strdup_printf("800x600");
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                      (GtkSignalFunc) image_800,
                      w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf("1024x768");
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                      (GtkSignalFunc) image_1024,
                      w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf("1280x960");
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                      (GtkSignalFunc) image_1280,
                      w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf("1600x1200");
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                      (GtkSignalFunc) image_1600,
                      w_current);
  gtk_widget_show (menuitem);

  buf = g_strdup_printf("3200x2400");
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect (GTK_OBJECT (menuitem), "activate",
                      (GtkSignalFunc) image_3200,
                      w_current);
  gtk_widget_show (menuitem);

  gtk_menu_set_active(GTK_MENU (menu),2);

  return menu;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_image_lowlevel(TOPLEVEL *w_current, const char* filename)
{
  int width, height;
  int save_height, save_width;
  int save_page_left, save_page_right, save_page_top, save_page_bottom;
  int page_width, page_height, page_center_left, page_center_top;
#ifndef HAS_LIBGD
  GdkPixbuf *pixbuf;
  char *filetype;
#endif
  float prop;

  width = w_current->image_width;
  height = w_current->image_height;

  save_width = w_current->width;
  save_height = w_current->height;

  w_current->width = width;
  w_current->height = height;

  save_page_left = w_current->page_current->left;
  save_page_right = w_current->page_current->right;
  save_page_top = w_current->page_current->top;
  save_page_bottom = w_current->page_current->bottom;
  
  page_width = save_page_right - save_page_left;
  page_height = save_page_bottom - save_page_top;

  page_center_left = save_page_left + (page_width / 2);
  page_center_top = save_page_top + (page_height / 2);

  /* Preserve proportions */
  prop = (float)width / height;
  if(page_width > page_height) {
    page_height = (page_width / prop);
  }else{
    page_width = (page_height * prop);
  }

  /* need to do this every time you change width / height */
  set_window(w_current, w_current->page_current,
             page_center_left - (page_width / 2),
             page_center_left + (page_width / 2),
             page_center_top - (page_height / 2),
             page_center_top + (page_height / 2));

  /* de select everything first */
  o_select_run_hooks(w_current, NULL, 2); 
  o_selection_remove_most(w_current,
                          w_current->page_current->
                          selection2_head);
		

#ifdef HAS_LIBGD
  /* try to use recalc here */
  o_redraw_all(w_current);

  printf("Calling f_image_write.\n");
  f_image_write(w_current, filename, width, height, 
                w_current->image_color);
#else
  pixbuf = x_image_get_pixbuf(w_current);
  if (pixbuf != NULL) {
    filetype = g_strdup("png");
    if (!gdk_pixbuf_save(pixbuf, filename, filetype, NULL, NULL)) {
      fprintf(stderr, "x_image_lowlevel: Unable to save PNG file  %s.\n", filename);
      s_log_message(_("x_image_lowlevel: Unable to write PNG file.\n"));
    }
    else {
      if (w_current->image_color == TRUE) {
	s_log_message(_("Wrote color image to [%s] [%d x %d]\n"), filename, width, height);
      } else {
	s_log_message(_("Wrote black and white image to [%s] [%d x %d]\n"), filename, width, height);
      }
    }
    if (filetype != NULL)
      g_free(filetype);
    if (pixbuf != NULL)
      g_object_unref(pixbuf); 
  }
  else {
    fprintf(stderr, "x_image_lowlevel: Unable to get pixbuf from gschem's window.\n");
    s_log_message(_("x_image_lowlevel: Unable to get pixbuf from gschem's window.\n"));
  }
#endif

  w_current->width = save_width;
  w_current->height = save_height;

  /* need to do this every time you change width / height */
  set_window(w_current, w_current->page_current,
             save_page_left,
             save_page_right,
             save_page_top,
             save_page_bottom);

  /* try to use recalc here... */
  o_redraw_all(w_current);

#ifdef HAS_LIBGD
  if (w_current->image_color == TRUE) {
    s_log_message(_("Wrote color image to [%s] [%d x %d]\n"), filename, width, height);
  } else {
    s_log_message(_("Wrote black and white image to [%s] [%d x %d]\n"), filename, width, height);
  }
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_image_write(GtkWidget *w, TOPLEVEL *w_current)
{
  const char *filename=NULL;
  GtkWidget *widget;

  widget = g_object_get_data (G_OBJECT (w_current->iwindow), "filename_entry");

  filename = gtk_entry_get_text(GTK_ENTRY(widget));
  if (filename[0] != '\0') {
    x_image_lowlevel(w_current, filename);
  }

  gtk_widget_destroy(w_current->iwindow);
  w_current->iwindow=NULL;
  return 0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_image_cancel(GtkWidget *w, TOPLEVEL *w_current)
{
  /* gtk_grab_remove(w_current->iwindow);*/
  gtk_widget_destroy(w_current->iwindow);
  w_current->iwindow=NULL;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_image_response(GtkWidget * widget, gint response, TOPLEVEL *w_current)
{
  switch(response) {
  case GTK_RESPONSE_REJECT:
  case GTK_RESPONSE_DELETE_EVENT:
    x_image_cancel(widget, w_current);
    break;
  case GTK_RESPONSE_ACCEPT:
    x_image_write(widget, w_current);
    break;
  default:
    printf("x_image_response(): strange signal %d\n", response);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_image_select_filename(GtkWidget *w, GtkWidget *image_dialog)
{
  GtkWidget *widget;
  GtkWidget *filechooser;
  const gchar *filename;

  filechooser = gtk_file_chooser_dialog_new (_("Select Image Filename..."),
					     GTK_WINDOW (image_dialog),
					     GTK_FILE_CHOOSER_ACTION_SAVE,
					     GTK_STOCK_CANCEL,
					     GTK_RESPONSE_CANCEL,
					     GTK_STOCK_OK,
					     GTK_RESPONSE_ACCEPT, NULL);

  widget = g_object_get_data (G_OBJECT (image_dialog), "filename_entry");
  filename = gtk_entry_get_text (GTK_ENTRY (widget));

  gtk_file_chooser_set_filename (GTK_FILE_CHOOSER (filechooser), filename);

  gtk_dialog_set_default_response(GTK_DIALOG(filechooser),
				  GTK_RESPONSE_ACCEPT);

  if (gtk_dialog_run (GTK_DIALOG (filechooser)) == GTK_RESPONSE_ACCEPT) {
    filename =
      gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (filechooser));
    gtk_entry_set_text (GTK_ENTRY (widget), filename);
   }

  gtk_widget_destroy (filechooser);  
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_image_setup (TOPLEVEL *w_current, char *filename)
{
  GtkWidget *label;
  GtkWidget *box;
  GtkWidget *hbox;
  GtkWidget *optionmenu;
  GtkWidget *filename_entry;
  GtkWidget *button;

  /* only create the dialog if it's not there yet */
  if (!w_current->iwindow) {

    w_current->iwindow = gtk_dialog_new_with_buttons(_("Write Image..."),
						     GTK_WINDOW(w_current->main_window),
						     GTK_DIALOG_MODAL,
						     GTK_STOCK_CANCEL,
						     GTK_RESPONSE_REJECT,
						     GTK_STOCK_OK,
						     GTK_RESPONSE_ACCEPT,
						     NULL);

    gtk_dialog_set_default_response(GTK_DIALOG(w_current->iwindow),
				    GTK_RESPONSE_ACCEPT);

    gtk_window_position (GTK_WINDOW (w_current->iwindow),
                         GTK_WIN_POS_MOUSE);

    gtk_signal_connect(GTK_OBJECT(w_current->iwindow), "response",
		       GTK_SIGNAL_FUNC(x_image_response), w_current);

    box = GTK_DIALOG(w_current->iwindow)->vbox;
    gtk_container_set_border_width(GTK_CONTAINER(w_current->iwindow),5);
    gtk_box_set_spacing(GTK_BOX(box),5);

    label = gtk_label_new (_("Width x Height:"));
    gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
    gtk_misc_set_padding (GTK_MISC (label), 0, 0);
    gtk_box_pack_start (GTK_BOX (box), label, FALSE, FALSE, 0);

    optionmenu = gtk_option_menu_new ();
    gtk_option_menu_set_menu (GTK_OPTION_MENU (optionmenu), 
			      create_menu_size (w_current));
    gtk_option_menu_set_history (GTK_OPTION_MENU (optionmenu), 2);
    gtk_box_pack_start (GTK_BOX (box), optionmenu, TRUE, TRUE, 0);

    label = gtk_label_new (_("Filename:"));
    gtk_misc_set_alignment( GTK_MISC (label), 0, 0);
    gtk_misc_set_padding (GTK_MISC (label), 0, 0);
    gtk_box_pack_start (GTK_BOX (box),
                        label, FALSE, FALSE, 0);

    hbox = gtk_hbox_new(FALSE,10);
    gtk_box_pack_start(GTK_BOX(box), hbox, FALSE, FALSE, 0);
    filename_entry = gtk_entry_new_with_max_length (200);
    gtk_editable_select_region (GTK_EDITABLE (filename_entry), 0, -1);
    gtk_box_pack_start (GTK_BOX (hbox),
                        filename_entry, TRUE, TRUE, 0);

    button = gtk_button_new_with_mnemonic(_("_Browse"));
    gtk_box_pack_start(GTK_BOX(hbox), button, FALSE, FALSE, 0);
    g_signal_connect(button, "clicked",
		     GTK_SIGNAL_FUNC (x_image_select_filename),
		     w_current->iwindow);

    GLADE_HOOKUP_OBJECT(w_current->iwindow,filename_entry,"filename_entry");

    gtk_widget_show_all (box);
  }
 
  if (!GTK_WIDGET_VISIBLE (w_current->iwindow)) {
    filename_entry = g_object_get_data (G_OBJECT (w_current->iwindow), "filename_entry");
    gtk_entry_set_text(GTK_ENTRY(filename_entry), filename);
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void x_image_convert_to_greyscale(GdkPixbuf *pixbuf)
{
  int width, height, rowstride, n_channels;
  guchar *pixels, *p, new_value;
  int i, j;

  n_channels = gdk_pixbuf_get_n_channels (pixbuf);
  
  if (n_channels != 3)
  {
    return;
  }
 
  if (gdk_pixbuf_get_colorspace (pixbuf) != GDK_COLORSPACE_RGB)
  {
    return;
  }

  if (gdk_pixbuf_get_bits_per_sample (pixbuf) != 8)
  {
    return;
  }

  width = gdk_pixbuf_get_width (pixbuf);
  height = gdk_pixbuf_get_height (pixbuf);

  rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  pixels = gdk_pixbuf_get_pixels (pixbuf);

  for (j = 0; j < height; j++)
  {
    for (i = 0; i < width; i++)
    {
      p = pixels + j * rowstride + i * n_channels;

      new_value = 0.3 * p[0] + 0.59 * p[1] + 0.11 * p[2];
      p[0] = new_value;
      p[1] = new_value;
      p[2] = new_value;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GdkPixbuf *x_image_get_pixbuf (TOPLEVEL *w_current)
{
  GdkPixbuf *pixbuf;
  int origin_x, origin_y, bottom, right;
  int size_x, size_y, s_right, s_left, s_top,s_bottom;
  TOPLEVEL toplevel;
  OBJECT *aux;
  char object_found = 0;

  /* Do a copy of the toplevel struct and work with it */
  memcpy(&toplevel, w_current, sizeof(TOPLEVEL));

  WORLDtoSCREEN(&toplevel, 
		w_current->page_current->right,
		w_current->page_current->left,
		&s_right,
		&s_left);
  WORLDtoSCREEN(&toplevel, 
		w_current->page_current->bottom,
		w_current->page_current->top,
		&s_bottom,
		&s_top);

  size_x = s_left - s_right;
  size_y = s_bottom - s_top;

  size_x = toplevel.image_width;
  size_y = toplevel.image_height;

  toplevel.window = gdk_pixmap_new(w_current->window, size_x, size_y, -1);
  toplevel.backingstore = gdk_pixmap_new(w_current->window, size_x, size_y, -1);
  toplevel.grid = 0;
  toplevel.text_origin_marker = FALSE;

  toplevel.display_width = toplevel.image_width;
  toplevel.display_height = toplevel.image_height;

  toplevel.win_width = toplevel.image_width;
  toplevel.win_height = toplevel.image_height;

  if (toplevel.image_color == FALSE)
  {
    /* We are going to be doing black&white (grayscale) output, so change the */
    /* color of all objects to a nice and dark color, say black */
    toplevel.override_color = BLACK;  

    /* also reset the background to white */
    toplevel.background_color = WHITE;
  }

  origin_x = origin_y = 0;
  right = size_x;
  bottom = size_y;

  /* ------------------  Begin optional code ------------------------ */
  /* If the the code in this region is commented, the PNG returned will
     be the same as the one returned using libgd.
     I mean: there will be some border all around the schematic.
     This code is used to adjust the schematic to the border of the image */
#if 0

  /* Do a zoom extents to get fit all the schematic in the window */
  /* Commented so the image returned will be the same as with libgd */  
  a_zoom_extents (&toplevel,
		  toplevel.page_current->object_head,
		  A_PAN_DONT_REDRAW);

  
  /* See if there are objects */
  
  aux = w_current->page_current->object_head;
  while (aux != NULL) {
    if (aux->type != -1) {
      object_found = 1;
      break;
    }
    aux = aux->next;
  }

  
  /* If there are no objects, can't use zoom_extents */
  if (object_found) {
    o_redraw_all (&toplevel); 
    get_complex_bounds(&toplevel, 
		       toplevel.page_current->object_head, 
		       &origin_x, &origin_y, 
		       &right, &bottom);
  }
#endif
  /* ------------------  End optional code ------------------------ */
  
  o_redraw_all (&toplevel); 

  /* Get the pixbuf */
  pixbuf=gdk_pixbuf_get_from_drawable(NULL,toplevel.backingstore, NULL, 
				      origin_x, origin_y, 0, 0,           
				      right-origin_x, 
				      bottom-origin_y);

  if (toplevel.image_color == FALSE)
  {
    x_image_convert_to_greyscale(pixbuf); 
  }

  if (toplevel.window != NULL) {
    g_object_unref(toplevel.window);
  }
  if (toplevel.backingstore != NULL) {
    g_object_unref(toplevel.backingstore);
  }

  return(pixbuf);
}
