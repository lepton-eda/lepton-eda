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
#include "../include/x_preview.h"

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
void x_fileselect_destroy_window(GtkWidget *widget, FILEDIALOG *f_current)
{

#if DEBUG
  printf("destroy\n");
#endif
  x_fileselect_free_list_buffers(f_current);

  if (f_current->directory) {
    g_free(f_current->directory);
    f_current->directory = NULL;
  }

  if (f_current->filename) {
    g_free(f_current->filename);
    f_current->filename = NULL;
  }

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
int x_fileselect_keypress(GtkWidget * widget, GdkEventKey * event, 
			  FILEDIALOG* f_current)
{
  if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
    x_fileselect_close (NULL, f_current);
    return TRUE;
  }

  return FALSE;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_init_list_buffers(FILEDIALOG *f_current) 
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
void x_fileselect_free_list_buffers(FILEDIALOG *f_current) 
{
  int i;

  for (i = 0; i < MAX_FILES; i++) {
    if (f_current->file_entries[i]) 
      g_free(f_current->file_entries[i]);

    f_current->file_entries[i] = NULL;
  }

  for (i = 0; i < MAX_DIRS; i++) {
    if (f_current->directory_entries[i]) 
    g_free(f_current->directory_entries[i]);

    f_current->directory_entries[i] = NULL;
  }
}

/*********** File Open/Save As... specific code starts here ***********/
/*! \section file-open-save-as File Open/Save As Functions.
 *  \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_update_dirfile(FILEDIALOG *f_current, char *filename)
{
  char *temp=NULL;

  if (f_current->filename) {
    g_free(f_current->filename);
    f_current->filename = NULL;
  }

  if (f_current->directory) {
    g_free(f_current->directory);
    f_current->directory = NULL;
  }

  /* this may cause problems on non POSIX complient systems */	
  temp = getcwd(NULL, 1024);
	
  if (filename) {
    f_current->directory = g_strdup (temp); 
    f_current->filename = g_strdup (filename);
					
    g_free(temp); 
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

  } else {
    f_current->directory = g_strdup (temp);

    if (f_current->filename) { 
      g_free(f_current->filename);
      f_current->filename=NULL;
    }

    gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), 
                       f_current->directory);
  }

  g_free(temp);

#if DEBUG
  printf("directory: %s\n", f_current->directory);
#endif

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_setup_list_buffers(FILEDIALOG *f_current, 
				     int num_files, int num_directories)
{
  int i;

  for (i = 0; i < num_files+1; i++) {
    if (f_current->file_entries[i]) {
      g_free(f_current->file_entries[i]);
    }
    f_current->file_entries[i] = NULL;
  }

  for (i = 0; i < num_directories+1; i++) {
    if (f_current->directory_entries[i]) {
      g_free(f_current->directory_entries[i]);
    }
    f_current->directory_entries[i] = NULL;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \return TRUE if the file should be included (passes the filter),
 *          FALSE otherwise.
 */
int x_fileselect_include_file(char *filename, int filter_type)
{
  switch(filter_type) {
    case(FILEDIALOG_SCH_ONLY):
    if (strstr(filename, ".sch")) {
      return(TRUE);
    }
    break;

    case(FILEDIALOG_SYM_ONLY):
    if (strstr(filename, ".sym")) {
      return(TRUE);
    }
    break;

    case(FILEDIALOG_SCH_SYM):
    if (strstr(filename, ".sch") || 
        strstr(filename, ".sym")) {
      return(TRUE);
    }
    break;

    case(FILEDIALOG_ALL_FILES):
    return(TRUE);
    break;
  }

  return(FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_fill_lists(FILEDIALOG *f_current)
{
  DIR* directory;
  struct dirent *dirent_ptr;
  int num_files=0;
  int num_directories=0;
  int file_count = 0;
  int dir_count = 0;
  struct stat stat_en;
  char path_buf[MAXPATHLEN*2];
  char *text[2];
  char *temp;
  int i;
  int max_width=0;
  int width;
  int first, last, j, done=0;
#ifdef __MINGW32__
  int has_trailing = FALSE;
#endif

  directory = opendir(f_current->directory);
#ifdef __MINGW32__
  has_trailing = u_basic_has_trailing(f_current->directory, 
				      G_DIR_SEPARATOR);
#endif

  if (!directory) {
    fprintf(stderr, _("Agg, could not open directory: %s\n"), f_current->directory);
    return;
  }

  while((dirent_ptr = readdir(directory)) != NULL) {
#ifdef __MINGW32__
    if (has_trailing) {
    	sprintf(path_buf, "%s%s", f_current->directory, dirent_ptr->d_name);
    } else {
#endif
    	sprintf(path_buf, "%s%c%s", f_current->directory, G_DIR_SEPARATOR,
	        dirent_ptr->d_name);
#ifdef __MINGW32__
    }
#endif

    if(stat(path_buf, &stat_en) >= 0 && S_ISDIR(stat_en.st_mode)) {
/*     	printf("dir: %s\n", path_buf);	 */
      num_directories++;	
    } else {
/*     	printf("file: %s\n", path_buf);	*/
      num_files++;	
    }
  }


  if (num_directories > MAX_DIRS) {
    fprintf(stderr, _("Too many directories! Increase MAX_DIRS\n"));
    exit(-1);
  }

  if (num_files > MAX_FILES) {
    fprintf(stderr, _("Too many files! Increase MAX_FILES\n"));
    exit(-1);
  }

  x_fileselect_setup_list_buffers(f_current, num_directories, num_files);

  rewinddir(directory);

  while((dirent_ptr = readdir(directory)) != NULL) {
#ifdef __MINGW32__
    if (has_trailing) {
    	sprintf(path_buf, "%s%s", f_current->directory, dirent_ptr->d_name);
    } else {
#endif
    	sprintf(path_buf, "%s%c%s", f_current->directory, G_DIR_SEPARATOR,
	        dirent_ptr->d_name);
#ifdef __MINGW32__
    }
#endif
    if(stat(path_buf, &stat_en) >= 0 && S_ISDIR(stat_en.st_mode) &&
       (strcmp(dirent_ptr->d_name, ".") != 0)) {

      f_current->directory_entries[dir_count] = (char *)
        g_malloc(sizeof(char)*(strlen(dirent_ptr->d_name)+2));
	
      sprintf(f_current->directory_entries[dir_count], 
              "%s", dirent_ptr->d_name);
      dir_count++;

    } else {
      if (x_fileselect_include_file(dirent_ptr->d_name,
                                    f_current->filter_type)) {	
        f_current->file_entries[file_count] = (char *)
          g_malloc(sizeof(char)*(strlen(dirent_ptr->d_name)+1));
        strcpy(f_current->file_entries[file_count], 
               dirent_ptr->d_name);
        file_count++;
      } 
    }
  }

#if DEBUG
  printf("FILE COUNT: %d\n", file_count);
#endif

  /* lame bubble sort */
  first = 0;
  last = file_count;
  while(!done) {

    done = 1;
    for (j = first ; j < last-1; j++) {
      if (strcmp(f_current->file_entries[j], 
                 f_current->file_entries[j+1]) > 0) {
        temp = f_current->file_entries[j];
        f_current->file_entries[j] = 
          f_current->file_entries[j+1];
        f_current->file_entries[j+1] = temp;
        done = 0;
      }
    }
    last = last - 1;

#if DEBUG 
    pass_count++;
#endif
  }

#if DEBUG 
  printf("file passes: %d\n", pass_count);
  pass_count = 0;
  printf("test: %d\n", strcmp("./", "../"));
  printf("DIR COUNT: %d\n", dir_count);
#endif


  /* lame bubble sort */
  done = 0;
  first = 0;
  last = dir_count;
  while(!done) {
    done = 1;
    for (j = first ; j < last-1; j++) {
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

#if DEBUG
  printf("directory passes: %d\n", pass_count);
#endif


  gtk_clist_freeze (GTK_CLIST (f_current->dir_list));
  gtk_clist_clear (GTK_CLIST (f_current->dir_list));
  gtk_clist_freeze (GTK_CLIST (f_current->file_list));
  gtk_clist_clear (GTK_CLIST (f_current->file_list));

  text[0] = NULL;
  text[1] = NULL;
  max_width = 0;
  for (i = 0 ; i < dir_count; i++) {
    temp = g_strconcat (f_current->directory_entries[i],
                        G_DIR_SEPARATOR_S, NULL);
    text[0] = temp; 
    gtk_clist_append (GTK_CLIST (f_current->dir_list), text);

    width = gdk_string_width(gtk_style_get_font(f_current->dir_list->style),
                             f_current->directory_entries[i]);

    if (width > max_width) {
      gtk_clist_set_column_width(GTK_CLIST(f_current->
                                           dir_list), 0, width);
      max_width = width;
    }

    g_free(temp);
#if DEBUG
    printf("directory: %s\n", f_current->directory_entries[i]);
#endif
  }

  max_width = 0;
  for (i = 0 ; i < file_count; i++) {
    text[0] = f_current->file_entries[i]; 
    gtk_clist_append (GTK_CLIST (f_current->file_list), text);

    width = gdk_string_width(gtk_style_get_font(f_current->dir_list->style), 
                             f_current->file_entries[i]);

    if (width > max_width) {
      gtk_clist_set_column_width(GTK_CLIST(f_current->
                                           file_list), 0, width);
      max_width = width;
    }

#if DEBUG
    printf("file: %s\n", f_current->file_entries[i]);
#endif
  }

  closedir(directory);
  gtk_clist_thaw (GTK_CLIST (f_current->file_list));
  gtk_clist_thaw (GTK_CLIST (f_current->dir_list));
  f_current->last_search = -1;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_fileselect_sch_files(GtkWidget *w, FILEDIALOG *f_current)
{
  f_current->filter_type = FILEDIALOG_SCH_ONLY;
  x_fileselect_fill_lists(f_current);
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_fileselect_sym_files(GtkWidget *w, FILEDIALOG *f_current)
{
  f_current->filter_type = FILEDIALOG_SYM_ONLY;
  x_fileselect_fill_lists(f_current);
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_fileselect_both_files(GtkWidget *w, FILEDIALOG *f_current)
{
  f_current->filter_type = FILEDIALOG_SCH_SYM;
  x_fileselect_fill_lists(f_current);
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
gint x_fileselect_all_files(GtkWidget *w, FILEDIALOG *f_current)
{
  f_current->filter_type = FILEDIALOG_ALL_FILES;
  x_fileselect_fill_lists(f_current);
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  this is from gtktest.c
 */
static GtkWidget *x_fileselect_filter_menu (FILEDIALOG *f_current)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList *group;
  char *buf;

  menu = gtk_menu_new ();
  group = NULL;

  buf = g_strdup_printf(_("sch - Schematics"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) x_fileselect_sch_files,
                     f_current);
  gtk_widget_show(menuitem);

  buf = g_strdup_printf( _("sym - Symbols "));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) x_fileselect_sym_files,
                     f_current);
  gtk_widget_show(menuitem);

  buf = g_strdup_printf(_("sym/sch - Schematics and Symbols"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) x_fileselect_both_files,
                     f_current);
  gtk_widget_show(menuitem);

  buf = g_strdup_printf( _("* - All Files"));
  menuitem = gtk_radio_menu_item_new_with_label (group, buf);
  g_free(buf);
  group = gtk_radio_menu_item_group (GTK_RADIO_MENU_ITEM (menuitem));
  gtk_menu_append (GTK_MENU (menu), menuitem);
  gtk_signal_connect(GTK_OBJECT (menuitem), "activate",
                     (GtkSignalFunc) x_fileselect_all_files,
                     f_current);
  gtk_widget_show(menuitem);

  switch(f_current->filter_type) {

    case(FILEDIALOG_SCH_ONLY):
      gtk_menu_set_active(GTK_MENU (menu),0);
      break;

    case(FILEDIALOG_SYM_ONLY):
      gtk_menu_set_active(GTK_MENU (menu),1);
      break;

    case(FILEDIALOG_SCH_SYM):
      gtk_menu_set_active(GTK_MENU (menu),2);
      break;

    case(FILEDIALOG_ALL_FILES):
      gtk_menu_set_active(GTK_MENU (menu),3);
      break;
  }

  return menu;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int x_fileselect_preview_checkbox(GtkWidget *widget, FILEDIALOG *f_current)
{
  if (f_current == NULL) {
    fprintf(stderr, _("x_fileselect_preview_checkbox: Oops got a null f_current!\n"));
    exit(-1);
  }

  if (f_current->preview_control) {
    f_current->preview_control = FALSE;
  } else {
    f_current->preview_control = TRUE;
  }
  g_object_set (f_current->preview,
                "active", f_current->preview_control,
                NULL);
  {
    gchar *filename;
    g_object_get (f_current->preview,
                  "filename", &filename,
                  NULL);
  }
  
  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_saveas_close (GtkWidget *w, FILEDIALOG *f_current)
{
  gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
  f_current->xfwindow = NULL;

#if 0 /* this isn't relavent anymore */
  w_current = f_current->toplevel;

  if (f_current->filesel_type == SAVEAS_QUIT) {
    exit_dialog(w_current);
  }

  if (f_current->filesel_type == SAVEAS_OPEN) {
    x_fileselect_setup (w_current, FILESELECT, SAVEAS_OPEN);
  }

  if (f_current->filesel_type == SAVEAS_NEW) {
    w_current->page_current->CHANGED = 0;
    i_callback_file_new(w_current, 0, NULL);
  }
#endif

  /* do nothing if close is pressed for SAVEAS_CLOSE case */
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_saveas(GtkWidget *w, FILEDIALOG *f_current)
{
  TOPLEVEL *w_current;
  const char *string = NULL;
  int len;

  w_current = f_current->toplevel;
#if ((GTK_MAJOR_VERSION > 2) || \
     ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >= 4)) )
  string = gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(f_current->xfwindow));
#else
  string = gtk_entry_get_text(GTK_ENTRY(f_current->filename_entry));
#endif
  if (!string) {
    return;
  }

  len = strlen(string);

  if (string[len - 1] != G_DIR_SEPARATOR) {
    if (w_current->page_current->page_filename) {
      g_free(w_current->page_current->page_filename);
    }

    w_current->page_current->page_filename = g_strdup (string);

    if (f_save(w_current, string)) {
      s_log_message(_("Saved As [%s]\n"), 
                    w_current->page_current->page_filename);
    
      i_set_filename(w_current, string);

      w_current->page_current->CHANGED = 0;
      x_pagesel_update (w_current);
    } else {
      s_log_message(_("Could NOT save [%s]\n"),w_current->page_current->page_filename);
      i_set_state_msg(w_current, SELECT, _("Error while trying to save"));
      i_update_toolbar(w_current);
    }

    x_fileselect_close (NULL, f_current);
    if (f_current->filesel_type == SAVEAS_QUIT) {
      x_window_close(w_current);
    } else if (f_current->filesel_type == SAVEAS_OPEN) {
      i_callback_file_open(w_current, 0, NULL);
    } else if (f_current->filesel_type == SAVEAS_NEW) {
      i_callback_file_new(w_current, 0, NULL);
    } else if (f_current->filesel_type == SAVEAS_CLOSE) {
      i_callback_page_close(w_current, 0, NULL);
    }

    /* do nothing if SAVEAS_NONE */
  } else {
    x_fileselect_close (NULL, f_current);
    s_log_message(_("Specify a Filename!\n"));
  }
  if (string != NULL) {
    g_free ( (void *) string);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_change_dir(FILEDIALOG *f_current, char *new_directory)
{
  if (new_directory) {
    chdir(new_directory);
    x_fileselect_update_dirfile(f_current, NULL);
    x_fileselect_fill_lists(f_current);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use widget, since it can be NULL
 *
 *  \par SDB notes
 *       This is the fcn which opens the file(s) selected in the "open file"
 *       dialog box.
 */
void x_fileselect_open_file(GtkWidget *w, FILEDIALOG *f_current)
{
  TOPLEVEL *w_current;
  PAGE *found_page;
  char *string;
  int len;

  char *filename = NULL;
#if ((GTK_MAJOR_VERSION > 2) || \
     ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >= 4)) )
  GSList *files;
#else
  GList *files;
  int row;
#endif

  /* get GList of selected files  */
#if ((GTK_MAJOR_VERSION > 2) || \
     ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >= 4)) )
  files = gtk_file_chooser_get_filenames (GTK_FILE_CHOOSER(f_current->xfwindow));
#else
  files = (GTK_CLIST(f_current->file_list))->selection;
#endif
  if (files) {
    /* iterate over selected files  */
    for (; files ; files = files->next) {     
#if ((GTK_MAJOR_VERSION > 2) || \
     ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >= 4)) )
      filename = files->data;
      string = filename;
#else
      row = (int) files->data;     /* Why do we need to do cast here?  */
                                   /* because files->data is a void *  */
      gtk_clist_get_text (GTK_CLIST(f_current->file_list), row, 0, &filename);
      /* allocate space, then stick full, absolute filename into string */
      string = g_build_path (G_DIR_SEPARATOR_S,
                             f_current->directory,
                             filename,
                             NULL);

#endif
      w_current = f_current->toplevel;
	
      len = strlen(string);
      if (string[len - 1] != G_DIR_SEPARATOR) {
        found_page = s_page_search (f_current->toplevel, string);
        
        if (found_page == NULL) {
          /* create a new page and make it the current page */
          s_page_goto (w_current,
                       s_page_new (w_current, string));
          
          w_current->DONT_REDRAW = 1;

#if DEBUG
          printf("In x_fileselect_open_file, after checking, full filename to open = %s\n", string);
#endif
          (void)f_open(w_current, 
                       w_current->page_current->page_filename);
          i_set_filename(w_current, w_current->page_current->
                         page_filename);

          x_repaint_background(w_current);
          x_manual_resize(w_current);
          a_zoom_extents(w_current, 
                         w_current->page_current->object_head,
                         A_PAN_DONT_REDRAW);
          o_undo_savestate(w_current, UNDO_ALL);

          /* now update the scrollbars */
          x_hscrollbar_update(w_current);
          x_vscrollbar_update(w_current);
          x_pagesel_update (w_current);

          w_current->DONT_REDRAW = 0;
          o_redraw_all(w_current);

        } else {   /* page already exists . . . */
          s_page_goto(w_current, found_page);
          x_pagesel_update (w_current);
          i_set_filename(w_current, w_current->
                         page_current->page_filename);
          x_scrollbars_update(w_current);
          o_redraw_all(w_current);
        }
      } else {       /* no filename given . . . . */
        s_log_message(_("Specify a Filename!\n"));
      }

      g_free(string);
    }       /* end for files . . .     */

    /* Now close file dialog window . . . . */
    gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
    f_current->xfwindow = NULL;

#if ((GTK_MAJOR_VERSION > 2) || \
     ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >= 4)) )
    g_slist_free(files);
#endif
  }     /* end of if string . . .  */                   
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \par SDB notes
 *  This fcn is called for almost every event which occurs
 *  to the "open file" widget, except when the actual file is to be
 *  opened.  Stuff handled by this fcn are e.g. highlighting the file
 *  selected in the file list, updating the dir/file lists upon change
 *  of directory, etc.
 */
void x_fileselect_dir_button (GtkWidget *widget, gint row, gint column,
			      GdkEventButton *bevent, FILEDIALOG *f_current)
{
  char *temp = NULL;

  gtk_clist_get_text (GTK_CLIST (f_current->dir_list), row, 0, &temp);

  if (temp) {	
#if DEBUG
    printf("In x_fileselect_dir_button, selected: %d _%s_\n", row, temp);
#endif
    if (bevent) {
      switch (bevent->type) {
        case(GDK_2BUTTON_PRESS):
          x_fileselect_change_dir(f_current, 
                                  temp);
          break;

        default:
					
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
void x_fileselect_file_button (GtkWidget *widget, gint row, gint column,
			       GdkEventButton *bevent, FILEDIALOG *f_current)
/*
 * SDB notes:  This fcn is apparently called for each event occuring to
 * the file clist.  This fcn determines if the button event captured was to
 * update the directory/file display or was a doubleclick on a file
 * in the clist meant to open the file.  It then
 * calls the appropriate handler.  Unfortunately, in the event of 
 * selecting multiple files (using ENHANCED mode), bevent is nonnull 
 * only on the first call.
 */
{
  char *temp = NULL;

  /* put name of desired file into temp */
  gtk_clist_get_text (GTK_CLIST (f_current->file_list), row, 0, &temp);

  if (temp) {	

#if DEBUG
    printf("In x_fileselect_file_button, file selected: %d %s\n", row, temp);
    if (bevent) {
      printf("in x_fileselect_file_button, bevent->type = %d\n", bevent->type);
    }
    else {
      printf("In x_fileselect_file_button, bevent = NULL\n");
    }
#endif
    
    if (bevent) {
      switch (bevent->type) {
        case(GDK_2BUTTON_PRESS):
          x_fileselect_open_file(NULL, f_current);
          break;

        default:
          x_fileselect_update_dirfile(
                                      f_current, temp);
          if (f_current->preview_control && f_current->directory && temp) {
            gchar *filename = g_build_filename (f_current->directory,
                                                temp,
                                                NULL);
            g_object_set (f_current->preview,
                          "filename", filename,
                          NULL);
            g_free (filename);
          }
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
void x_fileselect_update_dirfile_saveas(FILEDIALOG *f_current,
					char *new_filename)
{
  char *temp=NULL;
  char *ptr=NULL;
  char *filename=NULL;
  char *directory=NULL;
  int i;

  if (f_current->filename) {
    g_free(f_current->filename);
    f_current->filename = NULL;
  }

  if (f_current->directory) {
    g_free(f_current->directory);
    f_current->directory = NULL;
  }

  if (new_filename == NULL) {
    return;
  }

  directory = (char *) g_malloc(sizeof(char)*(strlen(new_filename)+1));
  filename = (char *) g_malloc(sizeof(char)*(strlen(new_filename)+1));

  ptr = new_filename;	
  temp = strrchr(new_filename, G_DIR_SEPARATOR);	
  if (temp) {
    i = 0;
    while(ptr != temp && ptr[0] != '\0') {
      directory[i] = *ptr;	
      ptr++;
      i++;
    }
    directory[i] = '\0';
    ptr++; /* skip over last '/' */
#if DEBUG
    printf("directory: %s\n", directory);
#endif
    i = 0;
    while(ptr[0] != '\0') {
      filename[i] = *ptr;	
      ptr++;	
      i++;
    }
    filename[i] = '\0';
#if DEBUG
    printf("filename: %s\n", filename);
#endif
  } else {
    printf("somehow got a filename which does not have a / in it\n");
  }

  if (directory) {
    f_current->directory = g_strdup (directory);
    g_free(directory);
  }

  if (filename) {
    f_current->filename = g_strdup (filename);
    g_free(filename);
  }
				
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

  g_free(temp);

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
void x_fileselect_close (GtkWidget *w, FILEDIALOG *f_current)
{
  gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
  f_current->xfwindow = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  don't use widget, since it can be NULL
 */
void x_fileselect_search(GtkWidget *w, FILEDIALOG *f_current)
{
  TOPLEVEL *w_current;
  const char *string;
  int i;

  w_current = f_current->toplevel;

  string = gtk_entry_get_text(GTK_ENTRY(f_current->search_entry));
	
  if (!string) {
    return;
  }

  gtk_entry_select_region(GTK_ENTRY(f_current->search_entry), 0, -1);

#if 0 /* not used right now */
  /* search directories */
  i = 0;
  if (f_current->file_entries[0] == NULL) {
    while (f_current->directory_entries[i] != NULL) {
      printf("compare: %s %s\n", f_current->directory_entries[i], string);
      if (strstr(f_current->directory_entries[i], string)) {

				/*text[0] = f_current->directory_entries[i];
                                  text[1] = NULL; 
                                  row = gtk_clist_find_row_from_data(GTK_CLIST(
                                  f_current->dir_list),
                                  f_current->directory_entries[i]);
				*/
				
        gtk_clist_select_row(GTK_CLIST(
                                       f_current->dir_list), 
                             i, 0);
        printf("%d found: %s\n", i, f_current->directory_entries[i]);
				
				/*x_fileselect_update_dirfile(f_current, NULL);
                                  x_fileselect_fill_lists(f_current);*/
        return;
      }
      i++;
    }
  }
#endif

  if (f_current->last_search != -1) {
    i = f_current->last_search;	
    gtk_label_set(GTK_LABEL(f_current->search_label), 
                  _("Search in Files")); 
  } else {
    gtk_label_set(GTK_LABEL(f_current->search_label), 
                  _("Search in Files")); 
    i = 0;
  }

  while (f_current->file_entries[i] != NULL) {
    if (strstr(f_current->file_entries[i], string)) {
      gtk_clist_select_row(GTK_CLIST(f_current->file_list), 
                           i, 0);

      gtk_clist_moveto(GTK_CLIST(
                                 f_current->file_list), 
                       i, 0, -1, -1);

      x_fileselect_update_dirfile(f_current, 
                                  f_current->file_entries[i]);
      f_current->last_search = i + 1;
      return;
    }
    i++;
  }
  f_current->last_search = -1;
  gtk_label_set(GTK_LABEL(f_current->search_label), 
		_("Search in Files - End of list")); 
}
/*********** File Open/Save As... specific code ends here ***********/

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void x_fileselect_setup_old (TOPLEVEL *w_current, int type, int filesel_type)
{
  GtkWidget *buttonapply = NULL;
  GtkWidget *buttonclose = NULL;
  GtkWidget *scrolled_win;
  GtkWidget *action_area;
  GtkWidget *separator;
  GtkWidget *drawbox;
  GtkWidget *label;
  GtkWidget *searchbox;
	
  FILEDIALOG *f_current;

  GtkWidget *vbox;
  GtkWidget *list_hbox;
  char *dir_title [2];
  char *file_title [2];


  if (type < 0 || type > 2) {
    return;
  }

  f_current = &w_current->fileselect[type];

  if (!f_current->xfwindow) {

    f_current->xfwindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
    f_current->toplevel = w_current;
    f_current->type = type;
    f_current->filesel_type = filesel_type;
    f_current->last_search = -1;
    f_current->filename = NULL;
    f_current->directory = NULL;

    gtk_window_position(GTK_WINDOW(f_current->xfwindow),
                        GTK_WIN_POS_MOUSE);
    
    if (filesel_type == OPEN) {
      gtk_window_set_title(GTK_WINDOW(
                             f_current->xfwindow),
                           _("Open..."));
    } else if (filesel_type == SAVEAS) {
      gtk_window_set_title(GTK_WINDOW(
                             f_current->xfwindow),
                           _("Save As..."));
    } else if (filesel_type == SAVEAS_CLOSE) {
      gtk_window_set_title(GTK_WINDOW(
                             f_current->xfwindow),
                           _("Save As..."));
    }

    gtk_signal_connect(GTK_OBJECT(f_current->xfwindow),
                       "destroy",
                       GTK_SIGNAL_FUNC(x_fileselect_destroy_window),
                       f_current);

    gtk_signal_connect(GTK_OBJECT(f_current->xfwindow), "key_press_event",
                       (GtkSignalFunc) x_fileselect_keypress, f_current);

    vbox = gtk_vbox_new (FALSE, 0);
    gtk_container_set_border_width(GTK_CONTAINER (
                                                  f_current->xfwindow), 10);
    gtk_container_add(GTK_CONTAINER (f_current->xfwindow), 
                      vbox);
    gtk_widget_show (vbox);

#if 0
    action_area = gtk_hbox_new (TRUE, 0);
    gtk_container_set_border_width(GTK_CONTAINER (
                                                  f_current->xfwindow), 10);
    gtk_box_pack_end(GTK_BOX (vbox), action_area, FALSE, FALSE, 0);
    gtk_widget_show (action_area);
#endif

    action_area = gtk_hbutton_box_new ();
    gtk_button_box_set_layout(GTK_BUTTON_BOX(action_area), 
                              GTK_BUTTONBOX_END);
    gtk_button_box_set_spacing(GTK_BUTTON_BOX(action_area), 5);
    gtk_box_pack_end (GTK_BOX (vbox), action_area, TRUE, FALSE, 10);
    gtk_widget_show (action_area);


    /*  ----- Create the filter selection area -----  */
    f_current->filter_type = FILEDIALOG_SCH_ONLY;

    label=gtk_label_new(_("Filter"));
    gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
    gtk_box_pack_start(GTK_BOX(vbox), label, 
                       FALSE, FALSE, 0);
    gtk_widget_show(label);
    
    f_current->filter = gtk_option_menu_new ();
    gtk_option_menu_set_menu(GTK_OPTION_MENU(f_current->filter),
                             x_fileselect_filter_menu(f_current));
    /* gtk_option_menu_set_history(GTK_OPTION_MENU(f_current->filter),
       4);*/
    gtk_box_pack_start(GTK_BOX(vbox), f_current->filter, 
                       FALSE, FALSE, 0);
    gtk_widget_show (f_current->filter);

    list_hbox = gtk_hbox_new (FALSE, 5);
    gtk_box_pack_start (GTK_BOX (vbox), list_hbox, TRUE, TRUE, 0);
    gtk_widget_show (list_hbox);

    separator = gtk_hseparator_new ();
    gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, TRUE, 0);
    gtk_widget_show (separator);

#if 0 /* for demonstration only */
    frame = gtk_frame_new (NULL);
    gtk_box_pack_start (GTK_BOX (vbox), frame, TRUE, TRUE, 0);
    gtk_widget_show (frame);
    /*		gtk_container_add (GTK_CONTAINER (frame), drawbox); */
#endif

    drawbox = gtk_hbox_new (FALSE, 0);
    gtk_box_pack_start (GTK_BOX (vbox), drawbox, TRUE, FALSE, 5);
    gtk_widget_show (drawbox);

    searchbox = gtk_vbox_new (FALSE, 0);
    gtk_box_pack_end (GTK_BOX (drawbox), searchbox, TRUE, TRUE, 10);
    gtk_widget_show (searchbox);

    
    /*  -----  Create the "directories"/"libraries" clist widgets -----  */
    dir_title[0] = g_strdup(_("Directories"));
    dir_title[1] = NULL;
    f_current->dir_list = gtk_clist_new_with_titles(1, 
                                                    (char**) dir_title);
    gtk_widget_set_usize(f_current->dir_list, 
                         DIR_LIST_WIDTH, DIR_LIST_HEIGHT);
    gtk_signal_connect (GTK_OBJECT (f_current->dir_list), 
                        "select_row", (GtkSignalFunc) 
                        x_fileselect_dir_button, f_current);
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
    if (w_current->sort_component_library) {
      gtk_clist_set_auto_sort(GTK_CLIST(f_current->dir_list), TRUE);
    }
    gtk_widget_show (f_current->dir_list);
    gtk_widget_show (scrolled_win);
    g_free(dir_title[0]);
    
    /*  ----- Create the files clist -----  */
    file_title[0] = g_strdup (_("Files"));
    f_current->file_list = gtk_clist_new_with_titles(1, 
                                                     (gchar**) file_title);
    gtk_widget_set_usize(f_current->file_list, 
                         FILE_LIST_WIDTH, FILE_LIST_HEIGHT);
    
    /*  Stuff added by SDB to enable opening multiple files at once   */
    gtk_clist_set_selection_mode(GTK_CLIST(f_current->file_list),
                                 GTK_SELECTION_EXTENDED);
    
    gtk_signal_connect(GTK_OBJECT (f_current->file_list), 
                       "select_row", 
                       /* This is file opening callback */
                       (GtkSignalFunc) x_fileselect_file_button, 
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
    if (w_current->sort_component_library) {
      gtk_clist_set_auto_sort(GTK_CLIST(f_current->file_list), TRUE);
    }
    gtk_widget_show (f_current->file_list);
    gtk_widget_show (scrolled_win);
    g_free(file_title[0]);

    /*  ----- create the preview widget -----  */
    f_current->preview = GTK_WIDGET (g_object_new (TYPE_PREVIEW,
                                                   "active", FALSE,
                                                   NULL));
    gtk_widget_show (f_current->preview);
    gtk_box_pack_start (GTK_BOX (drawbox), f_current->preview,
                        FALSE, FALSE, 0);
    
    f_current->preview_checkbox = gtk_check_button_new_with_label(
                                                                  _("Preview"));
    gtk_box_pack_start(GTK_BOX(searchbox), 
                       f_current->preview_checkbox, 
                       FALSE, FALSE, 0);
    /* other checkbox stuff is done AFTER drawing area is mapped */
    gtk_widget_show(f_current->preview_checkbox);

    /* -----  Create the search input text box -----  */
    f_current->search_label=gtk_label_new(_("Search in Files"));
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
                       GTK_SIGNAL_FUNC(x_fileselect_search),
                       f_current);
    gtk_widget_grab_focus(f_current->search_entry);
    gtk_widget_show(f_current->search_entry);

    /*  ----- Create the "Filename" text entry area -----  */
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

    if (filesel_type == OPEN) {
      gtk_signal_connect(GTK_OBJECT(f_current->filename_entry),
                         /* Here we connect the callback to
                            fileselect_open_file to the filename in
                            the filename text entry field..  */
                         "activate", 
                         GTK_SIGNAL_FUNC(
                           x_fileselect_open_file),
                         f_current);
    } else if ((filesel_type == SAVEAS_NONE) ||
               (filesel_type == SAVEAS_QUIT) ||
               (filesel_type == SAVEAS_OPEN) ||
               (filesel_type == SAVEAS_CLOSE) ||
               (filesel_type == SAVEAS_NEW)) { 
      gtk_signal_connect(GTK_OBJECT(
                           f_current->filename_entry), 
                         "activate", 
                         GTK_SIGNAL_FUNC(x_fileselect_saveas),
                         f_current);
    }
    gtk_editable_select_region(GTK_EDITABLE(
                                 f_current->filename_entry), 0, -1);
	
    gtk_widget_show(f_current->filename_entry);

    /*  ----- Here we create the "open"/"save as"/"apply" buttons -----  */
    if (filesel_type == OPEN) {
      buttonapply = gtk_button_new_from_stock (GTK_STOCK_OPEN);
      gtk_signal_connect(GTK_OBJECT(buttonapply),
                         /* Here we attach callback fileselect_open_file to
                            the "Open" button */
                         "clicked",
                         /* here's where I need another handler */
                         GTK_SIGNAL_FUNC(x_fileselect_open_file),
                         f_current);
    } else if ((filesel_type == SAVEAS_NONE) ||
               (filesel_type == SAVEAS_QUIT) ||
               (filesel_type == SAVEAS_OPEN) ||
               (filesel_type == SAVEAS_CLOSE) ||
               (filesel_type == SAVEAS_NEW)) { 
      buttonapply = gtk_button_new_from_stock (GTK_STOCK_SAVE_AS);
      gtk_signal_connect(GTK_OBJECT(buttonapply),
                         "clicked",
                         GTK_SIGNAL_FUNC(x_fileselect_saveas),
                         f_current);
    }

    GTK_WIDGET_SET_FLAGS(buttonapply, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area),
                       buttonapply, TRUE, TRUE, 0);
    /* This makes the "open" button the default */
    gtk_widget_grab_default (buttonapply);
    gtk_widget_show(buttonapply);

    /*  ----- Here we create the "cancel"/"close" buttons -----  */
    if (filesel_type == OPEN) {
      buttonclose = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
      GTK_WIDGET_SET_FLAGS(buttonclose, 
                           GTK_CAN_DEFAULT);
      gtk_box_pack_start(GTK_BOX(action_area),
                         buttonclose, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(buttonclose),
                         "clicked",
                         GTK_SIGNAL_FUNC(x_fileselect_close),
                         f_current);
      gtk_widget_show(buttonclose);
      
      x_fileselect_update_dirfile(f_current, NULL);
      x_fileselect_fill_lists(f_current);
    } else if ((filesel_type == SAVEAS_NONE) ||
               (filesel_type == SAVEAS_QUIT) ||
               (filesel_type == SAVEAS_OPEN) ||
               (filesel_type == SAVEAS_CLOSE) ||
               (filesel_type == SAVEAS_NEW)) { 
      buttonclose = gtk_button_new_from_stock (GTK_STOCK_CANCEL);
      GTK_WIDGET_SET_FLAGS(buttonclose, 
                           GTK_CAN_DEFAULT);
      gtk_box_pack_start(GTK_BOX(action_area),
                         buttonclose, TRUE, TRUE, 0);
      gtk_signal_connect(GTK_OBJECT(buttonclose),
                         "clicked",
                         GTK_SIGNAL_FUNC(
                           x_fileselect_saveas_close),
                         f_current);
      gtk_widget_show(buttonclose);
      
      x_fileselect_update_dirfile_saveas(f_current, 
                                         w_current->page_current->page_filename);
      x_fileselect_fill_lists(f_current);
    }
  }

  if (!GTK_WIDGET_VISIBLE(f_current->xfwindow)) {
    gtk_widget_show(f_current->xfwindow);
    gdk_window_raise(f_current->xfwindow->window);

    gtk_grab_add (f_current->xfwindow);

    /* need to delay this till the drawing area is created and
     * is showing */
    f_current->preview_control = FALSE;
    gtk_signal_connect (GTK_OBJECT(f_current->preview_checkbox), 
                        "toggled", GTK_SIGNAL_FUNC(x_fileselect_preview_checkbox),
                        f_current);
    if (w_current->file_preview) {
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

/*! \todo Finish function documentation!!!
 *  \brief Create "open/save file" dialog box.
 *  \par Function Description
 *  This function creates the "open/save file" dialog box.
 *
 *  \param [in] w_current     The TOPLEVEL object.
 *  \param [in] type          \todo What is do?
 *  \param [in] filesel_type  \todo What to do?
 */
void x_fileselect_setup (TOPLEVEL *w_current, int type, int filesel_type)
{
#if ((GTK_MAJOR_VERSION > 2) || \
     ((GTK_MAJOR_VERSION == 2) && (GTK_MINOR_VERSION >= 4)) )

  gchar *title;
  FILEDIALOG *f_current;
  if (type < 0 || type > 2) {
    return;
  }

  if ( (filesel_type != OPEN) &&
       (filesel_type != SAVEAS) &&
       (filesel_type != SAVEAS_CLOSE) ) {
    x_fileselect_setup_old(w_current, type, filesel_type);
  }

  f_current = &w_current->fileselect[type];

  if (!f_current->xfwindow) {
    GtkFileFilter *sch_filefilter, *sym_filefilter, 
      *sch_sym_filefilter, *all_filefilter;

    f_current->toplevel = w_current;
    f_current->type = type;
    f_current->filesel_type = filesel_type;
    f_current->last_search = -1;
    f_current->filename = NULL;
    f_current->directory = NULL;
    
    /* Create the filefilter */
    sch_filefilter = gtk_file_filter_new ();
    gtk_file_filter_set_name(sch_filefilter, _("Schematics"));
    gtk_file_filter_add_pattern(sch_filefilter, "*.sch");
    sym_filefilter = gtk_file_filter_new ();
    gtk_file_filter_set_name(sym_filefilter, _("Symbols"));
    gtk_file_filter_add_pattern(sym_filefilter, "*.sym");
    sch_sym_filefilter = gtk_file_filter_new ();
    gtk_file_filter_set_name(sch_sym_filefilter, _("Schematics and symbols"));
    gtk_file_filter_add_pattern(sch_sym_filefilter, "*.sym");
    gtk_file_filter_add_pattern(sch_sym_filefilter, "*.sch");
    all_filefilter = gtk_file_filter_new ();
    gtk_file_filter_set_name(all_filefilter, _("All files"));
    gtk_file_filter_add_pattern(all_filefilter, "*");

    if (filesel_type == OPEN) {
      title = g_strdup(_("Open..."));
      f_current->xfwindow = gtk_file_chooser_dialog_new (title, 
							 GTK_WINDOW(w_current->main_window),
							 GTK_FILE_CHOOSER_ACTION_OPEN,
							 GTK_STOCK_OPEN, 
							 GTK_RESPONSE_ACCEPT,
							 GTK_STOCK_CANCEL, 
							 GTK_RESPONSE_CANCEL,
							 NULL);	     

      gtk_file_chooser_set_select_multiple(GTK_FILE_CHOOSER(f_current->xfwindow), TRUE);
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  sch_filefilter);
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  sym_filefilter);
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  sch_sym_filefilter);
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  all_filefilter);

      /* Code for handle the file preview. Not working yet!! */

      /* Always have a file preview */
      /*
      f_current->preview_control = TRUE;
      
      f_current->preview = x_preview_setup(f_current->xfwindow, 
					   NULL);
					   
      preview_widget = ???
      gtk_file_chooser_set_preview_widget (GTK_FILE_CHOOSER(f_current->xfwindow), 
					   GTK_WIDGET(preview_widget));
      */
      /* There is a problem with setup_rest : 
	 gdk_pixmap_new: Assertion drawable != NULL || depth != -1 failed.
	 gdk_gc_new: drawable != NULL failed 
	 x_preview_setup_rest (f_current->preview); */
      /*
      g_signal_connect (GTK_FILE_CHOOSER(f_current->xfwindow), 
			"update-preview",
			G_CALLBACK (x_preview_update_gtk24), 
			f_current);
      */

      if (gtk_dialog_run (GTK_DIALOG (f_current->xfwindow)) == GTK_RESPONSE_ACCEPT)
	{
	  char *filename;
	  
	  filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (f_current->xfwindow));
	  x_fileselect_open_file(NULL, f_current);
	  g_free (filename);
	}
      else {
	x_fileselect_close (NULL, f_current);
      }
      
      g_free (title);
						   
    } else if ( (filesel_type == SAVEAS) || (filesel_type == SAVEAS_CLOSE) ){
      title = g_strdup(_("Save As..."));
      if ( filesel_type == SAVEAS_CLOSE ) {
        f_current->xfwindow = gtk_file_chooser_dialog_new (title, 
  							 GTK_WINDOW(w_current->main_window),
  							 GTK_FILE_CHOOSER_ACTION_SAVE,
  							 GTK_STOCK_SAVE_AS, 
  							 GTK_RESPONSE_ACCEPT,
  							 GTK_STOCK_CANCEL, 
  							 GTK_RESPONSE_CANCEL,
  							 _("Discard changes"),
  							 GTK_RESPONSE_REJECT,
  							 NULL);
      } else {
        f_current->xfwindow = gtk_file_chooser_dialog_new (title, 
  							 GTK_WINDOW(w_current->main_window),
  							 GTK_FILE_CHOOSER_ACTION_SAVE,
  							 GTK_STOCK_SAVE_AS, 
  							 GTK_RESPONSE_ACCEPT,
  							 GTK_STOCK_CANCEL, 
  							 GTK_RESPONSE_CANCEL,
  							 NULL);
      }
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  sch_filefilter);
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  sym_filefilter);
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  sch_sym_filefilter);
      gtk_file_chooser_add_filter(GTK_FILE_CHOOSER(f_current->xfwindow), 
				  all_filefilter);

      /* Select the filename */
      if (!gtk_file_chooser_set_filename(GTK_FILE_CHOOSER (f_current->xfwindow),
					 w_current->page_current->page_filename)) {
	gtk_file_chooser_set_current_name(GTK_FILE_CHOOSER (f_current->xfwindow),
					  w_current->page_current->page_filename);
      }
      switch (gtk_dialog_run (GTK_DIALOG (f_current->xfwindow))) {

        case GTK_RESPONSE_ACCEPT:
	  {
	    char *filename;
	    
	    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (f_current->xfwindow));
	    x_fileselect_saveas(NULL, f_current);
	    g_free (filename);
	  }
	  break;
	  
        case GTK_RESPONSE_REJECT:
	  x_fileselect_close (NULL, f_current);
	  /* Set the CHANGED flag to 0, so i_callback_page_close won't
	     ask again if save the page or not. */
	  w_current->page_current->CHANGED = 0;
	  i_callback_page_close (w_current, 0, NULL);
	  break;

          /* Catch any cancel, and any mechanisms which close the dialog, 
           * e.g. "Escape" key which returns GTK_RESPONSE_DELETE_EVENT */
        case GTK_RESPONSE_CANCEL:
        case GTK_RESPONSE_DELETE_EVENT:
        default:
          x_fileselect_close (NULL, f_current);
          break;
      }

      g_free (title);
    }
    else {
      /* If it's not file load/save, then it's component library selection.
	 Use the old interface */
      x_fileselect_setup_old(w_current, type, filesel_type);
    }
    /* This is not the way to free filefilters. Really do they need to be freed?  */
    /*
    gtk_object_destroy(GTK_OBJECT(sch_filefilter));
    gtk_object_destroy(GTK_OBJECT(sym_filefilter));
    gtk_object_destroy(GTK_OBJECT(sch_sym_filefilter));
    gtk_object_destroy(GTK_OBJECT(all_filefilter));
    */
  } else {
    /* window should already be mapped, otherwise this
     * will core */
    gdk_window_raise(f_current->xfwindow->window);
  }
#else
  x_fileselect_setup_old(w_current, type, filesel_type);
#endif
}

/*! \brief Load/Backup selection dialog.
 *  \par Function Description
 *  This function opens a message dialog and wait for the user to choose
 *  if load the backup or the original file.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] message   Message to display to user.
 *  \return TRUE if the user wants to load the backup file, FALSE otherwise.
 */
int x_fileselect_load_backup(TOPLEVEL *toplevel, GString *message)
{
  GtkWidget *dialog;

  g_string_append(message, "\nIf you load the original file, the backup file will be overwritten in the next autosave timeout and it will be lost.\n\nDo you want to load the backup file?\n");

  dialog = gtk_message_dialog_new(GTK_WINDOW(toplevel->main_window),
			  GTK_DIALOG_MODAL,
			  GTK_MESSAGE_QUESTION,
			  GTK_BUTTONS_YES_NO,
			  message->str);
  gtk_widget_show (dialog);
  if (gtk_dialog_run ((GtkDialog*)dialog) == GTK_RESPONSE_YES) {
    gtk_widget_destroy(dialog);  
    return TRUE;
  }
  else {
    gtk_widget_destroy(dialog);  
    return FALSE;
  }
}
