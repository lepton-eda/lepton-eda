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
 * This file holds fcns used to display the file open/save dialog box.
 * It was cloned from x_fileselect.c in gschem/src, and then hacked
 * by SDB for use in gattrib.
 *------------------------------------------------------------------*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

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



/* ----- x_fileselect stuff begins here ----- */
 
#define DIR_LIST_WIDTH   180
#define DIR_LIST_HEIGHT  180
#define FILE_LIST_WIDTH  180
#define FILE_LIST_HEIGHT 180

/* ------------------------------------------------------------- *
 * This destroys the entire FILEDIALOG structure & frees its memory.
 * There is another fcn which just closes the window.
 * ------------------------------------------------------------- */
void
x_fileselect_destroy_window(GtkWidget * widget, FILEDIALOG * f_current)
{

#if DEBUG
  printf("In x_fileselect_destroy_window, about to destroy window!\n");
#endif
  x_fileselect_free_list_buffers(f_current);

  if (f_current->directory) {
    free(f_current->directory);
    f_current->directory = NULL;
  }

  if (f_current->filename) {
    free(f_current->filename);
    f_current->filename = NULL;
  }

  gtk_grab_remove(f_current->xfwindow);
  f_current->toplevel = NULL;
  f_current->xfwindow = NULL;
  free(f_current);   
  /* *window = NULL; */
  return;
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
int
x_fileselect_keypress(GtkWidget * widget, GdkEventKey * event,
		      FILEDIALOG * f_current)
{
  if (strcmp(gdk_keyval_name(event->keyval), "Escape") == 0) {
    x_fileselect_close(NULL, f_current);
    return TRUE;
  }

  return FALSE;
}

/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void x_fileselect_init_list_buffers(FILEDIALOG * f_current)
{
  int i;

  /* Shouldn't we malloc something in here?? */
  for (i = MAX_FILES; i >= 0; i--) {
    f_current->file_entries[i] = NULL;
  }

  for (i = MAX_DIRS; i >= 0; i--) {
    f_current->directory_entries[i] = NULL;
  }

  return;
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void x_fileselect_free_list_buffers(FILEDIALOG * f_current)
{
  int i;

  for (i = MAX_FILES; i >= 0; i--) {
    if (f_current->file_entries[i])
      free(f_current->file_entries[i]);

    f_current->file_entries[i] = NULL;
  }

  for (i = MAX_DIRS; i >= 0; i--) {
    if (f_current->directory_entries[i])
      free(f_current->directory_entries[i]);

    f_current->directory_entries[i] = NULL;
  }
  return;
}



/*********** File Open/Save As... specific code starts here ***********/
/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void x_fileselect_update_dirfile(FILEDIALOG * f_current, char *filename)
{
  char *temp = NULL;

  if (f_current->filename) {
    free(f_current->filename);
    f_current->filename = NULL;
  }

  if (f_current->directory) {
    free(f_current->directory);
    f_current->directory = NULL;
  }

  /* this may cause problems on non POSIX complient systems */
  temp = getcwd(NULL, 1024);

  if (filename) {
    f_current->directory = u_basic_strdup(temp);
    f_current->filename = u_basic_strdup(filename);

    free(temp);
#ifdef __MINGW32__
    if (u_basic_has_trailing(f_current->directory, PATH_SEPARATER_CHAR)) {
      temp = u_basic_strdup_multiple(f_current->directory,
				     f_current->filename, NULL);
    } else {
#endif
      temp = u_basic_strdup_multiple(f_current->directory,
				     PATH_SEPARATER_STRING,
				     f_current->filename, NULL);
#ifdef __MINGW32__
    }
#endif

    gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), temp);

  } else {
    f_current->directory = u_basic_strdup(temp);

    if (f_current->filename) {
      free(f_current->filename);
      f_current->filename = NULL;
    }

    gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry),
		       f_current->directory);
  }

  free(temp);

#if DEBUG
  printf("In x_fileselect_update_dirfile, directory: %s\n", f_current->directory);
#endif

  return;
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void
x_fileselect_setup_list_buffers(FILEDIALOG * f_current,
				int num_directories, int num_files)
{
  int i;

  for (i = num_files+1; i >= 0; i--) {
    if (f_current->file_entries[i]) {
      free(f_current->file_entries[i]);
    }
    f_current->file_entries[i] = NULL;
  }

  for (i = num_directories+1; i >= 0; i--) {
    if (f_current->directory_entries[i]) {
      free(f_current->directory_entries[i]);
    }
    f_current->directory_entries[i] = NULL;
  }
  return;
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
/* returns TRUE if the file should be included (passes the filter) */
/* else returns FALSE */
int x_fileselect_include_file(char *filename, int filter_type)
{
  switch (filter_type) {
  case (FILEDIALOG_SCH_ONLY):
    if (strstr(filename, ".sch")) {
      return (TRUE);
    }
    break;

  case (FILEDIALOG_SYM_ONLY):
    if (strstr(filename, ".sym")) {
      return (TRUE);
    }
    break;

  case (FILEDIALOG_SCH_SYM):
    if (strstr(filename, ".sch") || strstr(filename, ".sym")) {
      return (TRUE);
    }
    break;

  case (FILEDIALOG_ALL_FILES):
    return (TRUE);
    break;
  }

  return (FALSE);
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void x_fileselect_fill_lists(FILEDIALOG * f_current)
{
  DIR *directory;
  struct dirent *dirent_ptr;
  int num_files = 0;
  int num_directories = 0;
  int file_count = 0;
  int dir_count = 0;
  struct stat stat_en;
  char path_buf[MAXPATHLEN * 2];
  char *text[2];
  char *temp;
  int i;
  int max_width = 0;
  int width;
  int first, last, j, done = 0;
#ifdef __MINGW32__
  int has_trailing = FALSE;
#endif

  directory = opendir(f_current->directory);
#ifdef DEBUG
  printf("In x_fileselect_fill_lists, directory = %s\n", directory);
#endif


#ifdef __MINGW32__
  has_trailing = u_basic_has_trailing(f_current->directory,
				      PATH_SEPARATER_CHAR);
#endif

  if (!directory) {
    fprintf(stderr, "Agg, could not open directory: %s\n",
	    f_current->directory);
    return;
  }

  while ((dirent_ptr = readdir(directory)) != NULL) {
#ifdef __MINGW32__
    if (has_trailing) {
      sprintf(path_buf, "%s%s", f_current->directory, dirent_ptr->d_name);
    } else {
#endif
      sprintf(path_buf, "%s%c%s", f_current->directory,
	      PATH_SEPARATER_CHAR, dirent_ptr->d_name);
#ifdef __MINGW32__
    }
#endif

    if (stat(path_buf, &stat_en) >= 0 && S_ISDIR(stat_en.st_mode)) {
      /* printf("dir: %s\n", path_buf);	 */
      num_directories++;
    } else {
      /* printf("file: %s\n", path_buf);	*/
      num_files++;
    }
  }


  if (num_directories > MAX_DIRS) {
    fprintf(stderr, "Too many directories! Increase MAX_DIRS\n");
    exit(-1);
  }

  if (num_files > MAX_FILES) {
    fprintf(stderr, "Too many files! Increase MAX_FILES\n");
    exit(-1);
  }

  x_fileselect_setup_list_buffers(f_current, num_directories, num_files);

  rewinddir(directory);

  while ((dirent_ptr = readdir(directory)) != NULL) {
#ifdef __MINGW32__
    if (has_trailing) {
      sprintf(path_buf, "%s%s", f_current->directory, dirent_ptr->d_name);
    } else {
#endif
      sprintf(path_buf, "%s%c%s", f_current->directory,
	      PATH_SEPARATER_CHAR, dirent_ptr->d_name);
#ifdef __MINGW32__
    }
#endif
    if (stat(path_buf, &stat_en) >= 0 && S_ISDIR(stat_en.st_mode) &&
	(strcmp(dirent_ptr->d_name, ".") != 0)) {

      f_current->directory_entries[dir_count] = (char *)
	  malloc(sizeof(char) * (strlen(dirent_ptr->d_name) + 2));

      sprintf(f_current->directory_entries[dir_count],
	      "%s", dirent_ptr->d_name);
      dir_count++;

    } else {
      if (x_fileselect_include_file(dirent_ptr->d_name,
				    f_current->filter_type)) {
	f_current->file_entries[file_count] = (char *)
	    malloc(sizeof(char) * (strlen(dirent_ptr->d_name) + 1));
	strcpy(f_current->file_entries[file_count], dirent_ptr->d_name);
	file_count++;
      }
    }
  }

#if DEBUG
  printf("In x_fileselect_fill_lists, FILE COUNT: %d\n", file_count);
#endif

  /* lame bubble sort */
  first = 0;
  last = file_count;
  while (!done) {

    done = 1;
    for (j = first; j < last - 1; j++) {
      if (strcmp(f_current->file_entries[j],
		 f_current->file_entries[j + 1]) > 0) {
	temp = f_current->file_entries[j];
	f_current->file_entries[j] = f_current->file_entries[j + 1];
	f_current->file_entries[j + 1] = temp;
	done = 0;
      }
    }
    last = last - 1;

  }

  /* lame bubble sort */
  done = 0;
  first = 0;
  last = dir_count;
  while (!done) {
    done = 1;
    for (j = first; j < last - 1; j++) {
      if (strcmp(f_current->directory_entries[j],
		 f_current->directory_entries[j + 1]) > 0) {
	temp = f_current->directory_entries[j];
	f_current->directory_entries[j] =
	    f_current->directory_entries[j + 1];
	f_current->directory_entries[j + 1] = temp;
	done = 0;
      }
    }
    last = last - 1;

  }

  gtk_clist_freeze(GTK_CLIST(f_current->dir_list));
  gtk_clist_clear(GTK_CLIST(f_current->dir_list));
  gtk_clist_freeze(GTK_CLIST(f_current->file_list));
  gtk_clist_clear(GTK_CLIST(f_current->file_list));

  text[0] = NULL;
  text[1] = NULL;
  max_width = 0;
  for (i = 0; i < dir_count; i++) {
    temp = u_basic_strdup_multiple(f_current->directory_entries[i],
				   PATH_SEPARATER_STRING, NULL);
    text[0] = temp;
    gtk_clist_append(GTK_CLIST(f_current->dir_list), text);

#ifdef HAS_GTK22
    width =
	gdk_string_width(gtk_style_get_font(f_current->dir_list->style),
			 f_current->directory_entries[i]);
#else
    width = gdk_string_width(f_current->dir_list->style->font,
			     f_current->directory_entries[i]);
#endif

    if (width > max_width) {
      gtk_clist_set_column_width(GTK_CLIST(f_current->dir_list), 0, width);
      max_width = width;
    }

    free(temp);
#if DEBUG
    printf("In x_fileselect_fill_lists, directory: %s\n", f_current->directory_entries[i]);
#endif
  }

  max_width = 0;
  for (i = 0; i < file_count; i++) {
    text[0] = f_current->file_entries[i];
    gtk_clist_append(GTK_CLIST(f_current->file_list), text);

#ifdef HAS_GTK22
    width =
	gdk_string_width(gtk_style_get_font(f_current->dir_list->style),
			 f_current->file_entries[i]);
#else
    width = gdk_string_width(f_current->dir_list->style->font,
			     f_current->file_entries[i]);
#endif

    if (width > max_width) {
      gtk_clist_set_column_width(GTK_CLIST(f_current->
					   file_list), 0, width);
      max_width = width;
    }
#if DEBUG
    printf("In x_fileselect_fill_lists, file: %s\n", f_current->file_entries[i]);
#endif
  }

  closedir(directory);
  gtk_clist_thaw(GTK_CLIST(f_current->file_list));
  gtk_clist_thaw(GTK_CLIST(f_current->dir_list));
  f_current->last_search = -1;
  return;
}


/* ------------------------------------------------------------- *
 *  This is a support fcn for the filter menu
 * ------------------------------------------------------------- */
gint x_fileselect_sch_files(GtkWidget * w, FILEDIALOG * f_current)
{
  f_current->filter_type = FILEDIALOG_SCH_ONLY;
  x_fileselect_fill_lists(f_current);
  return (0);
}



/* ------------------------------------------------------------- *
 *  This is a support fcn for the filter menu
 * ------------------------------------------------------------- */
gint x_fileselect_all_files(GtkWidget * w, FILEDIALOG * f_current)
{
  f_current->filter_type = FILEDIALOG_ALL_FILES;
  x_fileselect_fill_lists(f_current);
  return (0);
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
/* this is from gtktest.c */
static GtkWidget *x_fileselect_filter_menu(FILEDIALOG * f_current)
{
  GtkWidget *menu;
  GtkWidget *menuitem;
  GSList *group;
  char *buf;

  menu = gtk_menu_new();
  group = NULL;

  buf = g_strdup_printf("sch - Schematics");
  menuitem = gtk_radio_menu_item_new_with_label(group, buf);
  free(buf);
  group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menuitem));
  gtk_menu_append(GTK_MENU(menu), menuitem);
  gtk_signal_connect(GTK_OBJECT(menuitem), "activate",
		     (GtkSignalFunc) x_fileselect_sch_files, f_current);
  gtk_widget_show(menuitem);

  buf = g_strdup_printf("* - All Files");
  menuitem = gtk_radio_menu_item_new_with_label(group, buf);
  free(buf);
  group = gtk_radio_menu_item_group(GTK_RADIO_MENU_ITEM(menuitem));
  gtk_menu_append(GTK_MENU(menu), menuitem);
  gtk_signal_connect(GTK_OBJECT(menuitem), "activate",
		     (GtkSignalFunc) x_fileselect_all_files, f_current);
  gtk_widget_show(menuitem);

  switch (f_current->filter_type) {

  case (FILEDIALOG_SCH_ONLY):
    gtk_menu_set_active(GTK_MENU(menu), 0);
    break;

  case (FILEDIALOG_SYM_ONLY):
    gtk_menu_set_active(GTK_MENU(menu), 1);
    break;

  case (FILEDIALOG_SCH_SYM):
    gtk_menu_set_active(GTK_MENU(menu), 2);
    break;

  case (FILEDIALOG_ALL_FILES):
    gtk_menu_set_active(GTK_MENU(menu), 3);
    break;
  }

  return menu;
}




/* ------------------------------------------------------------- *
 * This fcn just closes the window and does nothing else.
 * It is invoked when you click "cancel" during a save
 * operation.
 * ------------------------------------------------------------- */
void x_fileselect_saveas_close(GtkWidget * w, FILEDIALOG * f_current)
{
  gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));

#if 0                           /* this isn't relavent anymore */
  w_current = f_current->toplevel;

  if (f_current->filesel_type == SAVEAS_QUIT) {
    exit_dialog(w_current);
  }

  if (f_current->filesel_type == SAVEAS_OPEN) {
    x_fileselect_setup(w_current, FILESELECT, SAVEAS_OPEN);
  }

  if (f_current->filesel_type == SAVEAS_NEW) {
    w_current->page_current->CHANGED = 0;
    i_callback_file_new(w_current, 0, NULL);
  }
#endif

  /* do nothing if close is pressed for SAVEAS_CLOSE case */
  return;
}


/* ------------------------------------------------------------- *
 * This fcn saves out the files and then closes the fileselect
 * dialog box.  It is invoked when you click "save" during a save
 * operation.
 * ------------------------------------------------------------- */
void x_fileselect_saveas(GtkWidget * w, FILEDIALOG * f_current)
{

  TOPLEVEL *w_current;
  gchar *string;
  int len;

  w_current = f_current->toplevel;

  string = (gchar *) gtk_entry_get_text(GTK_ENTRY(f_current->filename_entry));

  if (!string) {
    return;
  }

  len = strlen(string);

  if (string[len - 1] != PATH_SEPARATER_CHAR) {
    if (w_current->page_current->page_filename) {
      free(w_current->page_current->page_filename);
    }

    w_current->page_current->page_filename = u_basic_strdup(string);

    /* Try to do save by calling f_save . . . . */
    if (f_save(w_current, string)) {
      s_log_message("Saved As [%s]\n",
		    w_current->page_current->page_filename);

      /* Update filename for "saveas" operation */
      x_fileselect_set_filename(w_current, string); 

      w_current->page_current->CHANGED = 0;
    } else {
      s_log_message("Could NOT save [%s]\n",
		    w_current->page_current->page_filename);
    }

    x_fileselect_close(NULL, f_current);

#if 0
    /* What do these do? */
    if (f_current->filesel_type == SAVEAS_QUIT) {
      x_window_close(w_current);
    } else if (f_current->filesel_type == SAVEAS_OPEN) {
      i_callback_file_open(w_current, 0, NULL);
    } else if (f_current->filesel_type == SAVEAS_NEW) {
      i_callback_file_new(w_current, 0, NULL);
    } else if (f_current->filesel_type == SAVEAS_CLOSE) {
      i_callback_page_close(w_current, 0, NULL);
    }
#endif

    /* do nothing if SAVEAS_NONE */
  } else {
    s_log_message("Specify a Filename!\n");
  }
  return;
}


/* ------------------------------------------------------------- *
 * I think this puts the new filename back into pr_current as part
 * of a "save as" operation.  This was originally i_set_filename
 * in gschem/src/i_basic.c
 * ------------------------------------------------------------- */
void x_fileselect_set_filename(TOPLEVEL * w_current, const char *string)
{
  char trunc_string[41];
  int len;
  int i;

  if (!w_current->filename_label) {
    return;
  }

  if (string) {
    len = strlen(string);
    w_current->DONT_RESIZE = 1;

    if (w_current->filename_label) {
      if (len > 40) {

	trunc_string[0] = '.';
	trunc_string[1] = '.';
	trunc_string[2] = '.';

	trunc_string[40] = '\0';
	for (i = 39; i > 2; i--) {
	  if (len >= 0) {
	    trunc_string[i] = string[len];
	  } else {
	    break;
	  }
	  len--;
	}

	gtk_label_set(GTK_LABEL(w_current->filename_label), trunc_string);

      } else {

	gtk_label_set(GTK_LABEL(w_current->
				filename_label), (char *) string);
      }
    }
  }
  return;
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void x_fileselect_change_dir(FILEDIALOG * f_current, char *new_directory)
{
  if (new_directory) {
    chdir(new_directory);
    x_fileselect_update_dirfile(f_current, NULL);
    x_fileselect_fill_lists(f_current);
  }
}


/* ------------------------------------------------------------- *
 * This is the callback from the fileselect "open" button.  It does
 * the following:
 * 1. It loops over all filenames returned in the CLIST
 * 2. For each filenname, it calls s_toplevel_open_file, which
 *    reads in the file & fills out pr_current (which is a gloabal).
 *    If this is a new file, it also calls s_sheet_data_add_master_*_list
 *    to fill out the master lists.
 * ------------------------------------------------------------- */
/* don't use widget, since it can be NULL */
void x_fileselect_open_file(GtkWidget *w, FILEDIALOG *f_current)
{
  PAGE *p_local;
  char *string;
  int len;
  int return_code = 0;
  int old_num_rows, old_num_cols;  /* There is a better way . . . */

  char *filename = NULL;
  GList *files;
  int row;

#ifdef DEBUG
  printf("We have just entered x_fileselect_open_file.\n");
#endif

  /* get GList of selected files  */
  files = (GTK_CLIST(f_current->file_list))->selection;
  if (files) {

    old_num_rows = sheet_head->comp_count;
    old_num_cols = sheet_head->comp_attrib_count;

    /* iterate over selected files  */
    for (; files; files = files->next) {
      row = (int) files->data;	/* Why do we need to do cast here?  */
                                /* because files->data is a void *  */
      gtk_clist_get_text(GTK_CLIST(f_current->file_list), row, 0,
			 &filename);

      /* allocate space, then stick full, absolute filename into string */
      string = malloc(strlen(f_current->directory) + strlen(filename) + 2);
      if (!string) {		/* sanity check . . . */
	/* free(string); freeing a null pointer is bad. */
	return;
      }
      /* string is full name of file. */
      sprintf(string, "%s/%s", f_current->directory, filename);

#if DEBUG
      printf("In x_fileselect_open_file, opening string = %s\n", string);
#endif
    
      if (first_page == 1) {
	if (pr_current->page_current->page_filename) {
	  /* Page structure & first page has already been created in 
	   * s_project_create_new.  Therefore, just rename the first page
	   * and open the project.  */
	  free(pr_current->page_current->page_filename);
	}
	return_code |= s_toplevel_read_page(string); /* read in first page, or in return code */
	first_page = 0;
      } else {
	return_code |= s_toplevel_read_page(string); /* read in secondary page, or in return code */
      }
      free(string);
      
      /* Now add all items found to the master lists */
      s_sheet_data_add_master_comp_list_items(pr_current->page_current->object_head); 
      s_sheet_data_add_master_comp_attrib_list_items(pr_current->page_current->object_head); 
#if 0
      /* Note that this must be changed.  We need to input the entire project
       * before doing anything with the nets because we need to first
       * determine where they are all connected!   */
      s_sheet_data_add_master_net_list_items(pr_current->page_current->object_head);    
      s_sheet_data_add_master_net_attrib_list_items(pr_current->page_current->object_head); 
#endif

      s_sheet_data_add_master_pin_list_items(pr_current->page_current->object_head);    
      s_sheet_data_add_master_pin_attrib_list_items(pr_current->page_current->object_head); 

            
    }  	/* end of loop over files     */

    /* Now update rest of project if we had at least one new file */
    if (return_code) {
      /* ---------- Sort the master lists  ---------- */
      s_string_list_sort_master_comp_list();
      s_string_list_sort_master_comp_attrib_list();
#if 0
  /* Note that this must be changed.  We need to input the entire project
   * before doing anything with the nets because we need to first
   * determine where they are all connected!   */
      s_string_list_sort_master_net_list();
      s_string_list_sort_master_net_attrib_list();
#endif

      s_string_list_sort_master_pin_list();
      s_string_list_sort_master_pin_attrib_list();
                                                                                                       

      /* ---------- Now create and load the tables  ---------- */
      sheet_head->component_table = s_table_new(sheet_head->comp_count, sheet_head->comp_attrib_count);
      sheet_head->net_table = s_table_new(sheet_head->net_count, sheet_head->net_attrib_count);
      sheet_head->pin_table = s_table_new(sheet_head->pin_count, sheet_head->pin_attrib_count);

      
      p_local = pr_current->page_head; /* must iterate over all pages in design */
      while (p_local != NULL) {
	if (p_local->pid != -1) {   /* only traverse pages which are toplevel */
	  if (p_local->object_head && p_local->page_control == 0) {
	    s_table_add_toplevel_comp_items_to_comp_table(p_local->object_head);    /* adds all objects from page */
#if 0
  /* Note that this must be changed.  We need to input the entire project
   * before doing anything with the nets because we need to first
   * determine where they are all connected!   */
	    s_table_add_toplevel_net_items_to_net_table(p_local->object_head);     /* adds all objects from page */
#endif

	    s_table_add_toplevel_pin_items_to_pin_table(p_local->object_head);     /* adds all objects from page */

	  }
	}
	p_local = p_local->next;  /* iterate to next schematic page */
      }   /* while(p_local != NULL) */

#if DEBUG
      printf("In x_fileselect_open_file -- we have just added more files to the project.\n");
#endif  

      /* -------------- update windows --------------- */
      x_window_add_items();    /* This updates the top level stuff,
				* and then calls another fcn to update
				* the GtkSheet itself.  */

#ifdef DEBUG
      printf("In x_fileselect_open_file -- we have just returned from x_window_add_items.\n");
#endif
    } else {
      fprintf(stderr, "Couldn't open any file!.\n");
    }
  }   /* if (files) */
  
  /* Now close file dialog window . . . . */
  gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));

  /* try showing all windows now */
  gtk_widget_show( GTK_WIDGET(notebook));
  gtk_widget_show( GTK_WIDGET(window));

  /* ---------- Now verify correctness of entire design.  ---------- */
  s_toplevel_verify_design(pr_current);  /* pr_current is a global */


  return;
}



/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void
x_fileselect_dir_button(GtkWidget * widget, gint row, gint column,
			GdkEventButton * bevent, FILEDIALOG * f_current)
/*
 *  SDB notes:  This fcn is called for almost every event which occurs
 *  to the "open file" widget, except when the actual file is to be
 *  opened.  Stuff handled by this fcn are e.g. highlighting the file
 *  selected in the file list, updating the dir/file lists upon change
 *  of directory, etc.
 */
{
  char *temp = NULL;

  gtk_clist_get_text(GTK_CLIST(f_current->dir_list), row, 0, &temp);

  if (temp) {
#if DEBUG
    printf("In x_fileselect_dir_button, selected: %d _%s_\n", row, temp);
#endif
    if (bevent) {
      switch (bevent->type) {
      case (GDK_2BUTTON_PRESS):
	x_fileselect_change_dir(f_current, temp);
	break;

      default:

	break;
      }
    }
  }
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void
x_fileselect_file_button(GtkWidget * widget, gint row, gint column,
			 GdkEventButton * bevent, FILEDIALOG * f_current)
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
  gtk_clist_get_text(GTK_CLIST(f_current->file_list), row, 0, &temp);

  if (temp) {

#if DEBUG
    printf("In x_fileselect_file_button, file selected: %d %s\n", row,
	   temp);
    if (bevent) {
      printf("in x_fileselect_file_button, bevent->type = %d\n",
	     bevent->type);
    } else {
      printf("In x_fileselect_file_button, bevent = NULL\n");
    }
#endif

    if (bevent) {
      switch (bevent->type) {
      case (GDK_2BUTTON_PRESS):
	x_fileselect_open_file(NULL, f_current);
	break;

      default:
	x_fileselect_update_dirfile(f_current, temp);
	break;
      }
    }
  }
}


/* ------------------------------------------------------------- *
 *
 * ------------------------------------------------------------- */
void
x_fileselect_update_dirfile_saveas(FILEDIALOG * f_current,
				   char *new_filename)
{
  char *temp = NULL;
  char *ptr = NULL;
  char *filename = NULL;
  char *directory = NULL;
  int i;

#ifdef DEBUG
  printf("In x_fileselect_update_dirfile_saveas, new_filename = [%s]\n", new_filename);
#endif

  if (new_filename == NULL) {
    return;
  }

  if (f_current->filename) {
    free(f_current->filename);
    f_current->filename = NULL;
  }

  if (f_current->directory) {
    free(f_current->directory);
    f_current->directory = NULL;
  }

#if 0
  directory = (char *) malloc(sizeof(char) * (strlen(new_filename) + 1));
  filename = (char *) malloc(sizeof(char) * (strlen(new_filename) + 1));

  ptr = new_filename;
  temp = strrchr(new_filename, PATH_SEPARATER_CHAR);
  if (temp) {
    /* SDB asks: What is all this stuff for? */
    i = 0;
    while (ptr != temp && ptr[0] != '\0') {
      directory[i] = *ptr;
      ptr++;
      i++;
    }
    directory[i] = '\0';
    ptr++;			/* skip over last '/' */
    i = 0;
    while (ptr[0] != '\0') {
      filename[i] = *ptr;
      ptr++;
      i++;
    }
    filename[i] = '\0';
  } else {
#endif
    /* SDB says: This is what generally is run.  What is the above stuff for? */
    directory = getcwd(NULL, 1024);
    filename = u_basic_strdup(new_filename);
#if 0
  }
#endif

#if DEBUG
    printf("In x_fileselect_update_dirfile_saveas, directory = %s\n", directory);
    printf("In x_fileselect_update_dirfile_saveas, filename = %s\n", filename);
#endif

  if (directory) {
    f_current->directory = u_basic_strdup(directory);
    free(directory);
  }

  if (filename) {
    f_current->filename = u_basic_strdup(filename);
    free(filename);
  }
#ifdef __MINGW32__
  if (u_basic_has_trailing(f_current->directory, PATH_SEPARATER_CHAR)) {
    temp = u_basic_strdup_multiple(f_current->directory,
				   f_current->filename, NULL);
  } else {
#endif
    temp = u_basic_strdup_multiple(f_current->directory,
				   PATH_SEPARATER_STRING,
				   f_current->filename, NULL);
#ifdef __MINGW32__
  }
#endif
  gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), temp);

  free(temp);

#if DEBUG
  printf("In x_fileselect_update_dirfile_saveas, f_current->directory = %s\n", f_current->directory);
  printf("In x_fileselect_update_dirfile_saveas, f_current->filename = %s\n", f_current->filename);
#endif

  return;
}


/* ------------------------------------------------------------- *
 * This fcn closes the filedialog window, but doesn't kill the
 * filedialog object.  There is another fcn which will kill
 * the whole object.
 * ------------------------------------------------------------- */
void x_fileselect_close(GtkWidget * w, FILEDIALOG * f_current)
{
  if(f_current->filter)
    gtk_widget_destroy(GTK_WIDGET(f_current->filter));

  if(f_current->search_entry)
    gtk_widget_destroy(GTK_WIDGET(f_current->search_entry));

  if(f_current->search_label)
    gtk_widget_destroy(GTK_WIDGET(f_current->search_label));

  if(f_current->filename_entry)
    gtk_widget_destroy(GTK_WIDGET(f_current->filename_entry));

  if(f_current->dir_list)
    gtk_widget_destroy(GTK_WIDGET(f_current->dir_list));

  if(f_current->file_list)
    gtk_widget_destroy(GTK_WIDGET(f_current->file_list));

  if(f_current->preview_checkbox)
    gtk_widget_destroy(GTK_WIDGET(f_current->preview_checkbox));

  if(f_current->component_pulldown)
    gtk_widget_destroy(GTK_WIDGET(f_current->component_pulldown));

  /* Finally kill main widget */
  gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
  return;
}



/*********** File Open/Save As... specific code ends here ***********/


/******************************************************************/
/*------------------------------------------------------------------
 * This is the main fcn which creates the "open file" dialog box.
 * filesel_type can be one of: 
 *------------------------------------------------------------------*/
void x_fileselect_setup(TOPLEVEL *pr_current, int filesel_type)
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

  int type = FILESELECT;  /* This was a calling arg in gschem */
                          /* here, I have just made it fixed. */

  FILEDIALOG *f_current; 

  GtkWidget *vbox;
  GtkWidget *list_hbox;
  char *dir_title[2];
  char *file_title[2];

  /* Initialize filedialog object.  This object holds pointers
   * to all information in filedialog window, including pointers
   * to several widgets in the window. */
  f_current = malloc(sizeof(FILEDIALOG));
  /* (pr_current->fileselect)[0] = f_current; */ /* Keep a pointer to the file dialog in TOPLEVEL */

  f_current->xfwindow = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  f_current->toplevel = pr_current;  /* This points back to toplevel for reading in files. */
  f_current->type = 0;  /* used to be type */
  f_current->filesel_type = filesel_type;
  f_current->last_search = -1;
  f_current->filename = NULL;
  f_current->directory = NULL;
  f_current->dir_list = NULL;  
  f_current->file_list = NULL;
  x_fileselect_init_list_buffers(f_current); /* initializes the directory and file lists */


  /* These widgets are not used in gattrib.  I need to make them
   * null so that I don't get segfaults when manipulating windows */
  f_current->filter = NULL;
  f_current->search_entry = NULL;
  f_current->search_label = NULL;
  f_current->filename_entry = NULL;
  f_current->preview = NULL;
  f_current->preview_checkbox = NULL;
  f_current->component_pulldown = NULL;

#ifdef DEBUG
  printf("We have just entered x_fileselect_setup.\n");
#endif

  /*  ----- Create main top-level window  -----  */
  gtk_window_position(GTK_WINDOW(f_current->xfwindow),
		      GTK_WIN_POS_MOUSE);
    
  if (filesel_type == OPEN) {
    gtk_window_set_title(GTK_WINDOW(f_current->xfwindow),
			 "Open...");
  } else if (filesel_type == SAVEAS) {
    gtk_window_set_title(GTK_WINDOW(f_current->xfwindow),
			 "Save As...");
  } else if (filesel_type == SAVEAS_CLOSE) {
    gtk_window_set_title(GTK_WINDOW(f_current->xfwindow),
			 "Save As...");
  }
    
  gtk_signal_connect(GTK_OBJECT(f_current->xfwindow),
		     "destroy",
		     GTK_SIGNAL_FUNC(x_fileselect_destroy_window),
		     f_current);
  
  gtk_signal_connect(GTK_OBJECT(f_current->xfwindow), "key_press_event",
		     (GtkSignalFunc) x_fileselect_keypress, f_current);

  vbox = gtk_vbox_new(FALSE, 0);
  gtk_container_set_border_width(GTK_CONTAINER(f_current->xfwindow), 10);
  gtk_container_add(GTK_CONTAINER(f_current->xfwindow), vbox);
  gtk_widget_show(vbox);
  
  action_area = gtk_hbutton_box_new();
  gtk_button_box_set_layout(GTK_BUTTON_BOX(action_area),
			    GTK_BUTTONBOX_END);
  gtk_button_box_set_spacing(GTK_BUTTON_BOX(action_area), 5);
  gtk_box_pack_end(GTK_BOX(vbox), action_area, TRUE, FALSE, 10);
  gtk_widget_show(action_area);


  /*  ----- Create the filter selection area -----  */
  f_current->filter_type = FILEDIALOG_SCH_ONLY; /* we only handle schematics in gattrib */
  
  label = gtk_label_new("Filter");
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
  gtk_widget_show(label);
  
  f_current->filter = gtk_option_menu_new();
  gtk_option_menu_set_menu(GTK_OPTION_MENU(f_current->filter),
			   x_fileselect_filter_menu(f_current));
  /* gtk_option_menu_set_history(GTK_OPTION_MENU(f_current->filter),
     4); */
  gtk_box_pack_start(GTK_BOX(vbox), f_current->filter,
		     FALSE, FALSE, 0);
  gtk_widget_show(f_current->filter);
  
  
  list_hbox = gtk_hbox_new(FALSE, 5);
  gtk_box_pack_start(GTK_BOX(vbox), list_hbox, TRUE, TRUE, 0);
  gtk_widget_show(list_hbox);
  
  separator = gtk_hseparator_new();
  gtk_box_pack_start(GTK_BOX(vbox), separator, FALSE, TRUE, 0);
  gtk_widget_show(separator);
  
  drawbox = gtk_hbox_new(FALSE, 0);
  gtk_box_pack_start(GTK_BOX(vbox), drawbox, TRUE, FALSE, 5);
  gtk_widget_show(drawbox);
  
  searchbox = gtk_vbox_new(FALSE, 0);
  gtk_box_pack_end(GTK_BOX(drawbox), searchbox, TRUE, TRUE, 10);
  gtk_widget_show(searchbox);
  
  
  /*  -----  Create the "directories"/"libraries" clist widgets -----  */
  dir_title[0] = u_basic_strdup("Directories");
  dir_title[1] = NULL;
  f_current->dir_list = gtk_clist_new_with_titles(1,
						  (char **) dir_title);
  gtk_widget_set_usize(f_current->dir_list,
		       DIR_LIST_WIDTH, DIR_LIST_HEIGHT);
  gtk_signal_connect(GTK_OBJECT(f_current->dir_list),
		     "select_row", (GtkSignalFunc)
		     x_fileselect_dir_button, f_current);
  
  gtk_clist_column_titles_passive(GTK_CLIST(f_current->dir_list));
  
  scrolled_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_container_add(GTK_CONTAINER(scrolled_win), f_current->dir_list);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				 GTK_POLICY_AUTOMATIC,
				 GTK_POLICY_ALWAYS);
  gtk_container_set_border_width(GTK_CONTAINER(scrolled_win), 5);
  gtk_box_pack_start(GTK_BOX(list_hbox), scrolled_win, TRUE, TRUE, 0);
  gtk_clist_set_auto_sort(GTK_CLIST(f_current->dir_list), TRUE);

  gtk_widget_show(f_current->dir_list);
  gtk_widget_show(scrolled_win);
  free(dir_title[0]);
  
  /*  ----- Create the files clist -----  */
  file_title[0] = u_basic_strdup("Files");
  file_title[1] = NULL;
  f_current->file_list = gtk_clist_new_with_titles(1,
						   (gchar **)
						   file_title);
  gtk_widget_set_usize(f_current->file_list, FILE_LIST_WIDTH,
		       FILE_LIST_HEIGHT);
  
  /*  Stuff added by SDB to enable opening multiple files at once   */
  gtk_clist_set_selection_mode(GTK_CLIST(f_current->file_list),
			       GTK_SELECTION_EXTENDED);
  
  gtk_signal_connect(GTK_OBJECT(f_current->file_list), "select_row",
		     /* This is file opening callback */
		     (GtkSignalFunc) x_fileselect_file_button,
		     f_current);
  
  gtk_clist_column_titles_passive(GTK_CLIST(f_current->file_list));
  
  scrolled_win = gtk_scrolled_window_new(NULL, NULL);
  gtk_container_add(GTK_CONTAINER(scrolled_win), f_current->file_list);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_win),
				 GTK_POLICY_AUTOMATIC,
				 GTK_POLICY_ALWAYS);
  gtk_container_set_border_width(GTK_CONTAINER(scrolled_win), 5);
  gtk_box_pack_start(GTK_BOX(list_hbox), scrolled_win, TRUE, TRUE, 0);
  gtk_clist_set_auto_sort(GTK_CLIST(f_current->file_list), TRUE);

  gtk_widget_show(f_current->file_list);
  gtk_widget_show(scrolled_win);
  free(file_title[0]);
  
  /*  ----- Create the "Filename" text entry area -----  */
  label = gtk_label_new("Filename");
  gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 5);
  gtk_widget_show(label);
  
  f_current->filename_entry = gtk_entry_new_with_max_length(1024);
  gtk_editable_select_region(GTK_EDITABLE(f_current->filename_entry), 0,
			     -1);
  gtk_box_pack_start(GTK_BOX(vbox), f_current->filename_entry, FALSE,
		     FALSE, 0);
  
  if (filesel_type == OPEN) {
    gtk_signal_connect(GTK_OBJECT(f_current->filename_entry),
		       /* Here we connect the callback to
			  fileselect_open_file to the filename in
			  the filename text entry field..  */
		       "activate",
		       GTK_SIGNAL_FUNC(x_fileselect_open_file),
		       f_current);
  } else if ((filesel_type == SAVEAS_NONE) ||
	     (filesel_type == SAVEAS_QUIT) ||
	     (filesel_type == SAVEAS_OPEN) ||
	     (filesel_type == SAVEAS_CLOSE) ||
	     (filesel_type == SAVEAS_NEW)) {
    gtk_signal_connect(GTK_OBJECT(f_current->filename_entry),
		       "activate",
		       GTK_SIGNAL_FUNC(x_fileselect_saveas),
		       f_current);
  }
  gtk_editable_select_region(GTK_EDITABLE(f_current->filename_entry),
			     0, -1);
  
  gtk_widget_show(f_current->filename_entry);
  
  /*  ----- Here we create the "open"/"save as"/"apply" buttons -----  */
  if (filesel_type == OPEN) {
    buttonapply = gtk_button_new_with_label("Open");
    gtk_signal_connect(GTK_OBJECT(buttonapply),
		       "clicked",
		       GTK_SIGNAL_FUNC(x_fileselect_open_file),
		       f_current);  /* Note that f_current points to pr_current
				       which is important when reading in
				       files. */
  } else if ((filesel_type == SAVEAS_NONE) ||
	     (filesel_type == SAVEAS_QUIT) ||
	     (filesel_type == SAVEAS_OPEN) ||
	     (filesel_type == SAVEAS_CLOSE) ||
	     (filesel_type == SAVEAS_NEW)) {
    buttonapply = gtk_button_new_with_label("SaveAs");
    gtk_signal_connect(GTK_OBJECT(buttonapply),
		       "clicked",
		       GTK_SIGNAL_FUNC(x_fileselect_saveas), f_current);
  }
  
  GTK_WIDGET_SET_FLAGS(buttonapply, GTK_CAN_DEFAULT);
  gtk_box_pack_start(GTK_BOX(action_area), buttonapply, TRUE, TRUE, 0);
  /* This makes the "open" button the default */
  gtk_widget_grab_default(buttonapply);
  gtk_widget_show(buttonapply);
  
  /*  ----- Here we create the "cancel"/"close" buttons -----  */
  if (filesel_type == OPEN) {
    buttonclose = gtk_button_new_with_label("Cancel");
    GTK_WIDGET_SET_FLAGS(buttonclose, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area),
		       buttonclose, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonclose),
		       "clicked",
		       GTK_SIGNAL_FUNC(x_fileselect_close), f_current);
    gtk_widget_show(buttonclose);
    
    x_fileselect_update_dirfile(f_current, NULL);
    x_fileselect_fill_lists(f_current);
  } else if ((filesel_type == SAVEAS_NONE) ||
	     (filesel_type == SAVEAS_QUIT) ||
	     (filesel_type == SAVEAS_OPEN) ||
	     (filesel_type == SAVEAS_CLOSE) ||
	     (filesel_type == SAVEAS_NEW)) {
    buttonclose = gtk_button_new_with_label("Cancel");
    GTK_WIDGET_SET_FLAGS(buttonclose, GTK_CAN_DEFAULT);
    gtk_box_pack_start(GTK_BOX(action_area),
		       buttonclose, TRUE, TRUE, 0);
    gtk_signal_connect(GTK_OBJECT(buttonclose),
		       "clicked",
		       GTK_SIGNAL_FUNC(x_fileselect_saveas_close),
		       f_current);
    gtk_widget_show(buttonclose);

    x_fileselect_update_dirfile_saveas(f_current,
				       pr_current->page_current->
				       page_filename);

    x_fileselect_fill_lists(f_current);
  }

  if (!GTK_WIDGET_VISIBLE(f_current->xfwindow)) {
    gtk_widget_show(f_current->xfwindow);
    gdk_window_raise(f_current->xfwindow->window);

    gtk_grab_add(f_current->xfwindow);

  } else {
    /* window should already be mapped, otherwise this
     * will core */
    gdk_window_raise(f_current->xfwindow->window);
  }
}
