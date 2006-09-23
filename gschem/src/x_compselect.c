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
#include <stdlib.h>
#include <sys/types.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <libgeda/libgeda.h>
#include <gtk/gtk.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define DIR_LIST_WIDTH   180
#define DIR_LIST_HEIGHT  180
#define FILE_LIST_WIDTH  180
#define FILE_LIST_HEIGHT 180

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_destroy_window(GtkWidget *widget, FILEDIALOG *f_current)
{

#if DEBUG
  printf("destroy\n");
#endif
  x_compselect_free_list_buffers(f_current);

  if (f_current->directory) {
    free(f_current->directory);
    f_current->directory = NULL;
  }

  if (f_current->filename) {
    free(f_current->filename);
    f_current->filename = NULL;
  }

  x_preview_close(f_current->preview);
  gtk_grab_remove(f_current->xfwindow);
  f_current->toplevel = NULL;
  f_current->xfwindow = NULL;
  /* *window = NULL;*/
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int
x_compselect_keypress(GtkWidget * widget, GdkEventKey * event, 
                      FILEDIALOG* f_current)
{
  if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
    x_compselect_close (NULL, f_current);
    return TRUE;
  }

  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_init_list_buffers(FILEDIALOG *f_current) 
{
  int i;

  for (i = 0; i < MAX_FILES; i++) {
    f_current->file_entries[i] = NULL;
  }

  for (i = 0; i < MAX_DIRS; i++) {
    f_current->directory_entries[i] = NULL;
  }
        
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_free_list_buffers(FILEDIALOG *f_current) 
{
  int i;

  for (i = 0; i < MAX_FILES; i++) {
    if (f_current->file_entries[i]) 
      free(f_current->file_entries[i]);

    f_current->file_entries[i] = NULL;
  }

  for (i = 0; i < MAX_DIRS; i++) {
    if (f_current->directory_entries[i]) 
    free(f_current->directory_entries[i]);

    f_current->directory_entries[i] = NULL;
  }
}

/*! \section component-place-specific-code Component Place Specific Code.
 *  \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint
default_components(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->embed_complex = 0;
  w_current->include_complex = 0;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint
embed_components(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->embed_complex = 1;
  w_current->include_complex = 0;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint
include_components(GtkWidget *w, TOPLEVEL *w_current)
{
  w_current->include_complex = 1;
  w_current->embed_complex = 0;
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is from gtktest.c
 */
static GtkWidget*
create_menu (TOPLEVEL *w_current)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList *group;
  char *buf;

  menu = gtk_menu_new ();
  group = NULL;

  buf = g_strdup_printf(_("Default behavior - reference component"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) default_components,
                     w_current);

  gtk_widget_show(menuitem);

  buf = g_strdup_printf(_("Embed component in schematic"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) embed_components,
                     w_current);
  gtk_widget_show(menuitem);

  buf = g_strdup_printf(_("Include component as individual objects"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  free(buf);
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_comp_fill_libs(TOPLEVEL *w_current, FILEDIALOG *f_current)
{
  char *text[2];
  char *temp;
  int i;
  int max_width=0;
  int width;
  int first,last,done,j;                /* variables for the sort */
  const GSList *dirs, *dir;

  gtk_clist_freeze (GTK_CLIST (f_current->dir_list));
  gtk_clist_clear (GTK_CLIST (f_current->dir_list));

  i = 0;
  text[0] = NULL;
  text[1] = NULL;
  max_width = 0;

  /* populate the directory list */
  dirs = s_clib_get_directories ();
  for (dir = dirs; dir != NULL; dir = g_slist_next (dir)) {
    gchar *string = (gchar*)dir->data;

    temp = strrchr(string, G_DIR_SEPARATOR);
    if (temp) {
      temp++; /* get past last '/' */
      text[0] = temp;
    } else {
      text[0] = string;
    }

    f_current->directory_entries[i++] = g_strdup (string);
    
    gtk_clist_append (GTK_CLIST (f_current->dir_list), text);

    width = gdk_string_width(gtk_style_get_font(f_current->dir_list->style), 
                             text[0]);

    if (width > max_width) {
      gtk_clist_set_column_width(GTK_CLIST(f_current->
                                           dir_list), 0, width);
      max_width = width;
    }
  }

  gtk_clist_thaw (GTK_CLIST (f_current->dir_list));
  f_current->last_search_lib = -1;

  /* added sort for the directory list so it would match the 
     automatically sorted clist of directories
     Chris Ellec - May 2001                           */
  if (w_current->sort_component_library == TRUE) {
    done = 0;
    first = 0;
    last = i;
    while(!done) {
      done = 1;
      for (j = first ; j < last-1; j++) {
        /*printf ("%i:",j);*/
        if (strcmp(f_current->directory_entries[j], 
                   f_current->directory_entries[j+1]) > 0) {
          temp = f_current->directory_entries[j];
          f_current->directory_entries[j] = 
            f_current->directory_entries[j+1];
          f_current->directory_entries[j+1] = temp;
          done = 0;
        }
      }
      last = last - 1;

#if DEBUG 
      pass_count++;
#endif
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_comp_fill_components(FILEDIALOG *f_current, int row)
{
  GSList *filenames, *filename;
  gint width = 0, max_width = 0;
        
  gtk_clist_freeze (GTK_CLIST (f_current->file_list));
  gtk_clist_clear (GTK_CLIST (f_current->file_list));

  /* update current_clib in toplevel with new directory name */
  if (f_current->toplevel->current_clib) {
    free (f_current->toplevel->current_clib);
  }
  f_current->toplevel->current_clib = g_strdup (
    f_current->directory_entries[row]);

  /* get the list of filenames in directory */
  filenames = s_clib_get_files (f_current->directory_entries[row], ".sym");

  filename = filenames;
  while (filename != NULL) {
    gchar *text[2];

    text[0] = (gchar*) filename->data;
    text[1] = NULL;
    
    /* add filename to the clist */
    gtk_clist_append (GTK_CLIST (f_current->file_list), text);
    
    width = gdk_string_width (gtk_style_get_font (
                                f_current->file_list->style),
                              (gchar*) filename->data);
    if (width > max_width) {
      /* increase the width of the column */
      gtk_clist_set_column_width (GTK_CLIST (f_current->file_list),
                                  0, width);
      max_width = width;
    }
    
    /* continue with the next filename */
    filename = g_slist_next (filename);
  }

  /* get ride of the list of filenames */
  g_slist_foreach (filenames, (GFunc)free, NULL);
  g_slist_free (filenames);

  /* allow visual updates to clist */
  gtk_clist_thaw (GTK_CLIST (f_current->file_list));
      
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't pass in f_current->filename or f_current->directory for component
 *  or library
 */
void
x_compselect_comp_update_current(FILEDIALOG *f_current, 
                                 char *library, char *component)
{
  char *temp=NULL;

  /* component */
  if (f_current->filename) {
    free(f_current->filename);
    f_current->filename = NULL;
  }

  /* library */
  if (f_current->directory) {
    free(f_current->directory);
    f_current->directory = NULL;
  }

  if (library) {
    f_current->directory = g_strdup(library);
  } else {
    f_current->directory = NULL;
  }

  if (component) {
    f_current->filename = g_strdup(component);
  } else {
    f_current->filename = NULL;
  }

  if (f_current->directory && f_current->filename) {
#ifdef __MINGW32__
    if (u_basic_has_trailing(f_current->directory, G_DIR_SEPARATOR)) {
        temp = g_strconcat (f_current->directory, 
                            f_current->filename, NULL);
    } else {
#endif
        temp = g_strconcat (f_current->directory, 
                            G_DIR_SEPARATOR_S,
                            f_current->filename, NULL);
#ifdef __MINGW32__
    }
#endif
    gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), temp);
    free(temp);
  } else if (f_current->directory && !f_current->filename) {
    gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), 
                       f_current->directory);
  } else if (!f_current->directory) {
    gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), 
                       "NONE");
  }

#if 0 /* old code */
  if (f_current->directory && f_current->filename) {
    temp = g_strconcat (f_current->directory, 
                        f_current->filename, NULL);
    gtk_label_set(GTK_LABEL(f_current->filename_entry), temp);
  } else if (f_current->directory && !f_current->filename) {
    gtk_label_set(GTK_LABEL(f_current->filename_entry), 
                  f_current->directory);
  } else if (!f_current->directory) {
    gtk_label_set(GTK_LABEL(f_current->filename_entry), 
                  " ");
  }
#endif

#if DEBUG 
  printf("directory: %s\n", f_current->directory);
  printf("filename: %s\n", f_current->filename);
#endif

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_change_clib(FILEDIALOG *f_current, char *new_clib,
                         int row)
{
  x_compselect_comp_update_current(f_current, new_clib, NULL);
  x_compselect_comp_fill_components(f_current, row);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_lib_select (GtkWidget *widget, gint row, gint column,
                         GdkEventButton *bevent, FILEDIALOG *f_current)
{
  char *temp = NULL;

  gtk_clist_get_text (GTK_CLIST (f_current->dir_list), row, 0, &temp);

  if (temp) {   
#if DEBUG 
    printf("selected: %d _%s_ _%s_\n", row, temp, 
           f_current->directory_entries[row]);
#endif
    if (bevent) {
      switch (bevent->type) {
        /*      case(GDK_2BUTTON_PRESS): */
        default:
          x_compselect_change_clib(f_current, 
                                   f_current->directory_entries[row],
                                   row);
          break;

      }
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_comp_select (GtkWidget *widget, gint row, gint column,
                         GdkEventButton *bevent, FILEDIALOG *f_current)
{
  char *comp = NULL;
  int diff_x, diff_y;
  TOPLEVEL *w_current;

  w_current = f_current->toplevel;

  gtk_clist_get_text (GTK_CLIST (f_current->file_list), row, 0, &comp);

  if (comp) {   
    strcpy(w_current->current_basename, comp);

    if (f_current->preview_control && w_current->current_clib && comp) { 
      x_preview_update(f_current->preview, 
                       w_current->current_clib,
                       comp);
    }

    x_compselect_comp_update_current(f_current, 
                                     w_current->current_clib, comp);

    if (w_current->event_state == ENDCOMP) {
      diff_x = w_current->last_x - w_current->start_x;
      diff_y = w_current->last_y - w_current->start_y;

      o_complex_translate_display(w_current,
                                  diff_x, diff_y,
                                  w_current->page_current->complex_place_head);
    }

    o_list_delete_rest(w_current,
                       w_current->page_current->complex_place_head);
    o_complex_set_filename(w_current, w_current->current_clib,
                           w_current->current_basename);

    w_current->event_state = DRAWCOMP;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_comp_apply(GtkWidget *w, FILEDIALOG *f_current)
{
  TOPLEVEL *w_current;
  int diff_x, diff_y;
        
  w_current = f_current->toplevel;

  if (w_current->current_basename && w_current->current_clib) {
    if (w_current->event_state == ENDCOMP) {
      diff_x = w_current->last_x - w_current->start_x;
      diff_y = w_current->last_y - w_current->start_y;

      o_complex_translate_display(w_current,
                                  diff_x, diff_y,
                                  w_current->page_current->complex_place_head);
    }

    o_list_delete_rest(w_current,
                       w_current->page_current->complex_place_head);
    o_complex_set_filename(w_current, w_current->current_clib,
                           w_current->current_basename);

    w_current->event_state = DRAWCOMP;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_comp_close (GtkWidget *w, FILEDIALOG *f_current)
{
  TOPLEVEL *w_current;

  w_current = f_current->toplevel;

  /* erase any existing component while it's being placed */
  /* do this instead of the below o_redraw_all */
  if (w_current->inside_action &&
      (w_current->event_state == ENDCOMP ||
       w_current->event_state == DRAWCOMP)) {
    o_complex_rubbercomplex(w_current);
  }

  o_list_delete_rest(w_current, w_current->page_current->
                     complex_place_head);

  i_set_state(w_current, SELECT);
  i_update_toolbar(w_current);

  gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
  f_current->xfwindow = NULL;
  /* do nothing if close is pressed for SAVEAS_CLOSE case */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int
x_compselect_search_library(FILEDIALOG *f_current,
                            char *library, const char *string) 
{
  GSList *filenames, *filename;
  int ret;

  /* get the filenames with '.sym' in library */
  filenames = s_clib_get_files(library, ".sym");

  if (f_current->last_search == -1) {
    /* last search failed: start over */
    f_current->last_search = 0;
  }

  /* start search from last known position */
  filename = g_slist_nth (filenames, (guint)f_current->last_search);
  while (filename != NULL) {
    /* increase position in search */
    f_current->last_search++;

    /* does filename match the query? */
    if (strstr ((gchar*) filename->data, string)) {
      /* yes, stop the search, prepare to return last_search */
#if DEBUG
      printf("found: %s %s %s %d\n", library, filename->data, string, f_current->last_search - 1);
#endif
      break;
    }
    
    filename = g_slist_next (filename);
  }

  /* free the list of filenames */
  g_slist_foreach (filenames, (GFunc)free, NULL);
  g_slist_free (filenames);

  /* nothing found? */
  if (filename == NULL) {
    /* no, reset for next search */
    f_current->last_search = -1;
    ret = -1;
  } else {
    /* yes, return the position in the list of the filename found */
    ret = f_current->last_search - 1;
  }

  return ret;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use widget, since it can be NULL
 */
void
x_compselect_comp_search(GtkWidget *w, FILEDIALOG *f_current)
{
  TOPLEVEL *w_current;
  const char *string;
  int lib_count;
  int flag;

  w_current = f_current->toplevel;

  string = gtk_entry_get_text(GTK_ENTRY(f_current->search_entry));
        
  if (!string) {
    return;
  }

  gtk_entry_select_region(GTK_ENTRY(f_current->search_entry), 0, -1);

  if (f_current->last_search_lib != -1) {
    lib_count = f_current->last_search_lib;     
    gtk_label_set(GTK_LABEL(f_current->search_label),
                  _("Search in Components")); 
  } else {
    lib_count = 0;
    gtk_label_set(GTK_LABEL(f_current->search_label),
                  _("Search in Components")); 
  }

  while(f_current->directory_entries[lib_count] != NULL) {
    flag = x_compselect_search_library(f_current, 
                                       f_current->directory_entries[lib_count], 
                                       string);
    if (flag != -1) {
      gtk_clist_select_row(GTK_CLIST(f_current->dir_list), 
                           lib_count, 0);

      gtk_clist_moveto(GTK_CLIST(
                                 f_current->dir_list), 
                       lib_count, 0, -1, -1);

      x_compselect_change_clib(f_current, 
                               f_current->
                               directory_entries[lib_count],
                               lib_count);

      gtk_clist_select_row(GTK_CLIST(f_current->file_list), 
                           flag, 0);

      gtk_clist_moveto(GTK_CLIST(
                                 f_current->file_list), 
                       flag, 0, -1, -1);

      f_current->last_search_lib = lib_count; 
      return;
    } else {
      lib_count++;
    }
  }


  f_current->last_search_lib = -1;
  f_current->last_search = -1;

#if 0 /* I'm not sure this is worth the effort and the confusion it causes */
  /* now search the library names */
  lib_count = 0;
  while(f_current->directory_entries[lib_count] != NULL) {
    if (strstr(f_current->directory_entries[lib_count], string)) {

      printf("%s %s\n", f_current->directory_entries[lib_count], string);

      gtk_clist_select_row(GTK_CLIST(f_current->dir_list), 
                           lib_count, 0);

      gtk_clist_moveto(GTK_CLIST(
                                 f_current->dir_list), 
                       lib_count, 0, -1, -1);

      x_compselect_change_clib(f_current, 
                               f_current->
                               directory_entries[lib_count],
                               lib_count);

      gtk_label_set(GTK_LABEL(f_current->search_label),
                    _("Search in Components - Found library only")); 
      return;
    }
    lib_count++;
  }
  f_current->last_search_lib = -1;
#endif

  gtk_label_set(GTK_LABEL(f_current->search_label),
                _("Search in Components - End of list")); 

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_setup_list_buffers(FILEDIALOG *f_current, 
                                int num_files, int num_directories)
{
  int i;

  for (i = 0; i < num_files+1; i++) {
    if (f_current->file_entries[i]) {
      free(f_current->file_entries[i]);
    }
    f_current->file_entries[i] = NULL;
  }

  for (i = 0; i < num_directories+1; i++) {
    if (f_current->directory_entries[i]) {
      free(f_current->directory_entries[i]);
    }
    f_current->directory_entries[i] = NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int
x_compselect_preview_checkbox(GtkWidget *widget, FILEDIALOG *f_current)
{
  if (f_current == NULL) {
    fprintf(stderr, _("x_compselect_preview_checkbox: Oops got a null f_current!\n"));
    exit(-1);
  }

  if (f_current->preview_control) {
    f_current->preview_control = FALSE;
    x_repaint_background(f_current->preview);
  } else {
    f_current->preview_control = TRUE;

    if (f_current->directory && f_current->filename) {
       x_preview_update(f_current->preview, f_current->directory, 
                     f_current->filename);
    }
  }
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_close (GtkWidget *w, FILEDIALOG *f_current)
{
  gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
  f_current->xfwindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
x_compselect_setup (TOPLEVEL *toplevel)
{
  GtkWidget *buttonapply = NULL;
  GtkWidget *buttonclose = NULL;
  GtkWidget *scrolled_win;
  GtkWidget *action_area;
  GtkWidget *separator;
  GtkWidget *optionmenu;
  GtkWidget *drawbox;
  GtkWidget *label;
  GtkWidget *searchbox;
        
  FILEDIALOG *f_current;

  GtkWidget *vbox;
  GtkWidget *list_hbox;
  char *dir_title [2];
  char *file_title [2];


  f_current = &toplevel->fileselect[COMPSELECT];

  if (!f_current->xfwindow) {

    f_current->xfwindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    f_current->toplevel = toplevel;
    f_current->type = COMPSELECT;
    f_current->filesel_type = -1;
    f_current->last_search = -1;
    f_current->filename = NULL;
    f_current->directory = NULL;

    gtk_window_position(GTK_WINDOW(f_current->xfwindow),
                        GTK_WIN_POS_NONE);
    gtk_window_set_title(GTK_WINDOW(f_current->xfwindow),
                         _("Select Component..."));

    gtk_signal_connect(GTK_OBJECT(f_current->xfwindow),
                       "destroy",
                       GTK_SIGNAL_FUNC(x_compselect_destroy_window),
                       f_current);

    gtk_signal_connect(GTK_OBJECT(f_current->xfwindow), "key_press_event",
                       (GtkSignalFunc) x_compselect_keypress, f_current);

    vbox = gtk_vbox_new (FALSE, 0);
    gtk_container_set_border_width(GTK_CONTAINER (f_current->xfwindow), 10);
    gtk_container_add(GTK_CONTAINER (f_current->xfwindow), 
                      vbox);
    gtk_widget_show (vbox);

    action_area = gtk_hbutton_box_new ();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(action_area), 
                              GTK_BUTTONBOX_END);
    gtk_button_box_set_spacing(GTK_BUTTON_BOX(action_area), 5);
    gtk_box_pack_end (GTK_BOX (vbox), action_area, TRUE, FALSE, 10);
    gtk_widget_show (action_area);


    /*  ----- Create the filter selection area -----  */
    f_current->filter_type = FILEDIALOG_SYM_ONLY;

    list_hbox = gtk_hbox_new (FALSE, 5);
    gtk_box_pack_start (GTK_BOX (vbox), list_hbox, TRUE, TRUE, 0);
    gtk_widget_show (list_hbox);

    separator = gtk_hseparator_new ();
    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, TRUE, 0);
    gtk_widget_show (separator);

    drawbox = gtk_hbox_new (FALSE, 0);
    gtk_box_pack_start (GTK_BOX (vbox), drawbox, TRUE, FALSE, 5);
    gtk_widget_show (drawbox);

    searchbox = gtk_vbox_new (FALSE, 0);
    gtk_box_pack_end (GTK_BOX (drawbox), searchbox, TRUE, TRUE, 10);
    gtk_widget_show (searchbox);

    
    /*  -----  Create the "directories"/"libraries" clist widgets -----  */
    dir_title[0] = g_strdup(_("Libraries"));
    dir_title[1] = NULL;
    f_current->dir_list = gtk_clist_new_with_titles(1,
                                                    (char**) dir_title);
    gtk_widget_set_usize(f_current->dir_list, 
                         DIR_LIST_WIDTH, DIR_LIST_HEIGHT);
    gtk_signal_connect (GTK_OBJECT (f_current->dir_list), 
                        "select_row", (GtkSignalFunc) 
                        x_compselect_lib_select, f_current);
    gtk_clist_column_titles_passive(GTK_CLIST(f_current->dir_list));

    scrolled_win = gtk_scrolled_window_new(NULL, NULL);
    gtk_container_add(GTK_CONTAINER (scrolled_win), 
                      f_current->dir_list);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(
                                                       scrolled_win),
                                   GTK_POLICY_AUTOMATIC, 
                                   GTK_POLICY_ALWAYS);
    gtk_container_set_border_width(GTK_CONTAINER (scrolled_win), 5);
    gtk_box_pack_start(GTK_BOX (list_hbox), scrolled_win, 
                       TRUE, TRUE, 0);
    if (toplevel->sort_component_library) {
      gtk_clist_set_auto_sort(GTK_CLIST(f_current->dir_list), TRUE);
    }
    gtk_widget_show (f_current->dir_list);
    gtk_widget_show (scrolled_win);
    free(dir_title[0]);
    
    /*  ----- Create the files clist -----  */
    file_title[0] = g_strdup (_("Components"));
    file_title[1] = NULL;
    f_current->file_list = gtk_clist_new_with_titles(1, 
                                                     (gchar**) file_title);
    gtk_widget_set_usize(f_current->file_list, 
                         FILE_LIST_WIDTH, FILE_LIST_HEIGHT);

    gtk_signal_connect(GTK_OBJECT (f_current->file_list), 
                       "select_row",
                       (GtkSignalFunc) x_compselect_comp_select,
                       f_current);
    gtk_clist_column_titles_passive(GTK_CLIST(f_current->file_list));

    scrolled_win = gtk_scrolled_window_new (NULL, NULL);
    gtk_container_add(GTK_CONTAINER(scrolled_win), 
                      f_current->file_list);
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(
                                                       scrolled_win),
                                   GTK_POLICY_AUTOMATIC, 
                                   GTK_POLICY_ALWAYS);
    gtk_container_set_border_width(GTK_CONTAINER(scrolled_win), 5);
    gtk_box_pack_start(GTK_BOX (list_hbox), scrolled_win, 
                       TRUE, TRUE, 0);
    if (toplevel->sort_component_library) {
      gtk_clist_set_auto_sort(GTK_CLIST(f_current->file_list), TRUE);
    }
    gtk_widget_show (f_current->file_list);
    gtk_widget_show (scrolled_win);
    free(file_title[0]);

    /*  ----- create the preview widget -----  */
    f_current->preview = x_preview_setup(f_current->xfwindow, 
                                         drawbox);

    f_current->preview_checkbox = gtk_check_button_new_with_label(
                                                                  _("Preview"));
    gtk_box_pack_start(GTK_BOX(searchbox), 
                       f_current->preview_checkbox, 
                       FALSE, FALSE, 0);
    /* other checkbox stuff is done AFTER drawing area is mapped */
    gtk_widget_show(f_current->preview_checkbox);

    /* -----  Create the search input text box -----  */
    f_current->search_label=gtk_label_new(_("Search in Components"));
    gtk_misc_set_alignment(GTK_MISC(f_current->search_label), 0, 0);
    gtk_box_pack_start(GTK_BOX(searchbox), f_current->search_label,
                       FALSE, FALSE, 5);
    gtk_widget_show(f_current->search_label);


    f_current->search_entry = gtk_entry_new_with_max_length (255);
    gtk_editable_select_region(GTK_EDITABLE(
                                            f_current->search_entry), 0, -1);
    gtk_box_pack_start(GTK_BOX (searchbox), 
                       f_current->search_entry, FALSE, FALSE, 0);
    gtk_signal_connect(GTK_OBJECT(f_current->search_entry), 
                       "activate", 
                       GTK_SIGNAL_FUNC(x_compselect_comp_search),
                       f_current);
    gtk_widget_grab_focus(f_current->search_entry);
    gtk_widget_show(f_current->search_entry);

    /*  ----- Create the "Filename" text entry area -----  */
    optionmenu = gtk_option_menu_new ();
    gtk_option_menu_set_menu(GTK_OPTION_MENU(optionmenu),
                             create_menu (toplevel));
    gtk_box_pack_start(GTK_BOX(vbox), optionmenu, 
                       FALSE, FALSE, 10);
    gtk_widget_show (optionmenu);

    label=gtk_label_new(_("Filename"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 5);
    gtk_widget_show(label);

    f_current->filename_entry = 
      gtk_entry_new_with_max_length(1024);
    gtk_editable_select_region(GTK_EDITABLE(
                                            f_current->filename_entry), 0, -1);
    gtk_box_pack_start(GTK_BOX (vbox), 
                       f_current->filename_entry, FALSE, FALSE, 0);

    gtk_entry_set_editable(GTK_ENTRY(f_current->filename_entry), FALSE);
        
    gtk_widget_show(f_current->filename_entry);

    buttonapply = gtk_button_new_from_stock (GTK_STOCK_APPLY);
    gtk_signal_connect(GTK_OBJECT(buttonapply),
                       "clicked",
                       GTK_SIGNAL_FUNC(x_compselect_comp_apply),
                       f_current);

    GTK_WIDGET_SET_FLAGS(buttonapply, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area),
                       buttonapply, TRUE, TRUE, 0);
    /* This makes the "open" button the default */
    gtk_widget_grab_default (buttonapply);
    gtk_widget_show(buttonapply);

    buttonclose = gtk_button_new_from_stock (GTK_STOCK_CLOSE);
    GTK_WIDGET_SET_FLAGS(buttonclose, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area),
                       buttonclose, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonclose),
                       "clicked",
                       GTK_SIGNAL_FUNC(x_compselect_comp_close),
                       f_current);
    gtk_widget_show(buttonclose);
    
    /* files data structure is not used for components */
    x_compselect_setup_list_buffers(f_current,
                                    g_slist_length ((GSList *) s_clib_get_directories()), 0);
    x_compselect_comp_update_current(f_current, NULL, NULL);
    x_compselect_comp_fill_libs(toplevel, f_current);
  }

  if (!GTK_WIDGET_VISIBLE(f_current->xfwindow)) {
    gtk_widget_show(f_current->xfwindow);
    gdk_window_raise(f_current->xfwindow->window);
    x_preview_setup_rest(f_current->preview);

    /* need to delay this till the drawing area is created and
     * is showing */
    gtk_signal_connect (GTK_OBJECT(f_current->preview_checkbox), 
                        "toggled", GTK_SIGNAL_FUNC(x_compselect_preview_checkbox),
                        f_current);
    if (toplevel->file_preview) {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
                                                     f_current->preview_checkbox), 
                                   TRUE);
      f_current->preview_control = TRUE;
    } else {
      gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(
                                                     f_current->preview_checkbox), 
                                   FALSE);
      f_current->preview_control = FALSE;
    }

  } else {
    /* window should already be mapped, otherwise this
     * will core */
    gdk_window_raise(f_current->xfwindow->window);
  }

}


