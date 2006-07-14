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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

extern int mouse_x, mouse_y;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_preview_update(TOPLEVEL *preview,
		      const gchar *directory, const gchar *filename) 
{
  gchar *cwd, *temp;
  PAGE *page;

  /* Since f_open now changes the directory, we need to 
   * use this to reset the cwd at end of fcn */
  cwd = g_get_current_dir ();

#ifdef __MINGW32__
  if (u_basic_has_trailing (directory, G_DIR_SEPARATOR)) {
     temp = g_strconcat (directory, filename, NULL);
  } else {
#endif
     temp = g_strconcat (directory, G_DIR_SEPARATOR_S, filename, NULL);
#ifdef __MINGW32__
  }
#endif
  s_page_delete (preview, preview->page_current);

  page = s_page_new (preview, temp);
  s_page_goto (preview, page);

  /* open up file for preview */
  f_open (preview, temp);

  a_zoom_extents (preview,
                  page->object_head,
                  A_PAN_DONT_REDRAW);

  o_redraw_all (preview);

  chdir (cwd); /* Go back to original directory */
  g_free (cwd);
  g_free (temp);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_preview_update_gtk24 (GtkFileChooser *file_chooser, gpointer data)
{
#if ((GTK_MAJOR_VERSION > 2) || \
     ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >= 4)) )
  FILEDIALOG *f_current;
  char *filename = NULL;
  GdkPixbuf *pixbuf;
  gboolean have_preview=FALSE;

  f_current =  (FILEDIALOG *) data;
  printf("x_preview_update_gtk24: Getting filename.\n");
  filename = gtk_file_chooser_get_preview_filename (file_chooser);
  printf("x_preview_update_gtk24: Filename: %s.\n", filename);

  /* If no file is selected, then don't set the preview and exit */
  if (g_file_test (filename, G_FILE_TEST_IS_DIR)) {
    gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview);
    return;
  }

  /* open up file for preview */
  f_open (f_current->preview, filename);

  pixbuf = x_image_get_pixbuf (f_current->preview);
  have_preview = (pixbuf != NULL);
  if (pixbuf != NULL) {
    printf("x_preview_update_gtk24: setting pixbuf.\n");
    gtk_image_set_from_pixbuf (GTK_IMAGE(gtk_file_chooser_get_preview_widget(file_chooser)),
			       pixbuf);
    if (pixbuf)
      gdk_pixbuf_unref (pixbuf);
  }
  else {
    fprintf (stderr, "x_preview_update_gtk24: Can't get pixbuf from preview struct.\n");
    s_log_message(
      _("x_preview_update_gtk24: Can't get pixbuf from preview struct.\n"));
  }

  g_free (filename);
  
  gtk_file_chooser_set_preview_widget_active (file_chooser, have_preview);
#endif
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_preview_close (TOPLEVEL *w_current)
{
  o_attrib_free_current (w_current);
  o_complex_free_filename (w_current);

  if (w_current->backingstore) {
    gdk_pixmap_unref (w_current->backingstore);
  }

  x_window_free_gc (w_current);
  
  s_toplevel_delete (w_current);  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_preview_expose(GtkWidget *widget, GdkEventExpose *event,
		      TOPLEVEL *w_current)
{
  exit_if_null(w_current);

#if DEBUG
  printf("yeah expose: %d %d\n", event->area.width, event->area.height);
#endif

  gdk_draw_pixmap(widget->window,
                  widget->style->fg_gc[GTK_WIDGET_STATE (widget)],
                  w_current->backingstore,
                  event->area.x, event->area.y,
                  event->area.x, event->area.y,
                  event->area.width, event->area.height);

  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_preview_button_pressed(GtkWidget *widget, GdkEventButton *event,
			      TOPLEVEL *w_current)
{
  exit_if_null(w_current);

  global_window_current = w_current;

#if DEBUG
  printf("preview pressed\n");
#endif

  if (event->button == 1) { 
    i_callback_view_zoom_in_hotkey(w_current, 0, NULL);
  } else if (event->button == 2) {
    i_callback_view_pan_hotkey(w_current, 0, NULL);
  } else if (event->button == 3) {
    i_callback_view_zoom_out_hotkey(w_current, 0, NULL);
  }
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_preview_motion(GtkWidget *widget, GdkEventMotion *event,
		      TOPLEVEL *w_current)
{
  mouse_x = (int) event->x;
  mouse_y = (int) event->y;

#if DEBUG
  printf("preview motion\n");
#endif
  return(0);
}

#if 0
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_preview_button_released(GtkWidget *widget, GdkEventButton *event,
			       TOPLEVEL *w_current)
{
  exit_if_null(w_current);

  global_window_current = w_current;
  printf("preview released\n");
}
#endif

#if 0
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_preview_key_press (GtkWidget *widget, GdkEventKey *event,
			  TOPLEVEL *w_current)
{
  exit_if_null(w_current);
  global_window_current = w_current;

  if (event->keyval == 0) {
    return;
  }

}
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_preview_create_drawing(GtkWidget *drawbox, TOPLEVEL *w_current)
{
  /* drawing next */
  w_current->drawing_area = gtk_drawing_area_new ();
  /* Set the size here.  Be sure that it has an aspect ratio of 1.333
   * We could calculate this based on root window size, but for now
   * lets just set it to:
   * Width = root_width*3/4   Height = Width/1.3333333333
   * 1.3333333 is the desired aspect ratio!
   */

  gtk_drawing_area_size (GTK_DRAWING_AREA (w_current->drawing_area),
                         w_current->win_width,
                         w_current->win_height);

  gtk_box_pack_start (GTK_BOX (drawbox), w_current->drawing_area,
                      FALSE, FALSE, 0);
  gtk_widget_show (w_current->drawing_area);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_preview_setup_rest (TOPLEVEL *preview)
{
  PAGE *preview_page;

  preview->window = preview->drawing_area->window;
  gtk_widget_grab_focus (preview->drawing_area);

  preview->backingstore = gdk_pixmap_new (
    preview->window,
    preview->drawing_area->allocation.width,
    preview->drawing_area->allocation.height, -1);

  x_window_setup_gc (preview);

  preview_page = s_page_new (preview, "unknown");
  s_page_goto (preview, preview_page);

  i_vars_set (preview);

  /* i_vars_set will set auto_save_interval, so disable it 
     We don't want to autosave previews!! */
  preview->auto_save_interval = 0;

  /* be sure to turn off the grid */
  preview->grid = FALSE;

  /* preview windows don't have toolbars */
  preview->handleboxes = FALSE; 
  preview->toolbars    = FALSE;

  x_repaint_background(preview);
	
#if 0	
  world_get_complex_bounds(preview, 
                           preview_page->object_head, 
                           &left, &top, &right, &bottom);
  set_window(preview, preview->current_page, left, right, top, bottom);
#endif

  preview->DONT_RECALC = 0;
  preview->DONT_RESIZE = 0;
  preview->DONT_REDRAW = 0;

  a_zoom_extents(preview,
                 preview_page->object_head,
                 A_PAN_DONT_REDRAW);

  o_redraw_all(preview);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
TOPLEVEL *x_preview_setup(GtkWidget *xfwindow, GtkWidget *drawbox) 
{
  struct event_reg_t {
    gchar *detailed_signal;
    void (*c_handler)(void);
  } drawing_area_events[] = {
    { "expose_event",         G_CALLBACK (x_preview_expose)          },
    { "button_press_event",   G_CALLBACK (x_preview_button_pressed)  },
#if 0
    { "button_release_event", G_CALLBACK (x_preview_button_released) },
    { "key_press_event",      G_CALLBACK (x_preview_key_press)       },
#endif
    { "motion_notify_event",  G_CALLBACK (x_preview_motion)          },
    { NULL,                   NULL                                   }
  }, *tmp;
  TOPLEVEL *preview_toplevel;

  preview_toplevel = s_toplevel_new ();

  preview_toplevel->init_left   = 0;
  preview_toplevel->init_top    = 0;
  preview_toplevel->init_right  = WIDTH_C;
  preview_toplevel->init_bottom = HEIGHT_C;
  preview_toplevel->width  = 160;
  preview_toplevel->height = 120;
  preview_toplevel->win_width  = preview_toplevel->width;
  preview_toplevel->win_height = preview_toplevel->height;
  /* be sure to turn off scrollbars */
  preview_toplevel->scrollbars_flag = FALSE;

  x_preview_create_drawing (drawbox, preview_toplevel);

  gtk_widget_set_events (preview_toplevel->drawing_area, 
                         GDK_EXPOSURE_MASK | 
                         GDK_POINTER_MOTION_MASK |
                         GDK_BUTTON_PRESS_MASK);
  for (tmp = drawing_area_events; tmp->detailed_signal != NULL; tmp++) {
    g_signal_connect (preview_toplevel->drawing_area,
                      tmp->detailed_signal,
                      tmp->c_handler,
                      preview_toplevel);
  }
  return preview_toplevel;
}
