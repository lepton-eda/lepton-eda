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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <dirent.h>

#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/globals.h>
#include <libgeda/colors.h>
#include <libgeda/prototype.h>

#include "../include/x_states.h"
#include "../include/prototype.h"

#define DIR_LIST_WIDTH   180
#define DIR_LIST_HEIGHT  180
#define FILE_LIST_WIDTH  180
#define FILE_LIST_HEIGHT 180

void
x_fileselect_destroy_window(GtkWidget *widget, FILEDIALOG *f_current)
{

#if DEBUG
	printf("destroy\n");
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

	x_preview_close(f_current->preview);
	gtk_grab_remove(f_current->xfwindow);
	f_current->xfwindow = NULL;
        /* *window = NULL;*/
}

void
x_fileselect_update_dirfile(FILEDIALOG *f_current, char *filename)
{
	char *temp=NULL;

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
		f_current->directory = u_basic_strdup_multiple(temp, "/", NULL);
		f_current->filename = u_basic_strdup(filename);
					
		free(temp); 
		temp = u_basic_strdup_multiple(f_current->directory,
				      	       f_current->filename, NULL);
		gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), temp);

	} else {
		f_current->directory = u_basic_strdup_multiple(temp, "/", NULL);

		if (f_current->filename) {
			free(f_current->filename);
			f_current->filename=NULL;
		}

		gtk_entry_set_text(GTK_ENTRY(f_current->filename_entry), 
			   f_current->directory);
	}

	free(temp);

#if DEBUG
	printf("directory: %s\n", f_current->directory);
#endif

}

void
x_fileselect_init_list_buffers(FILEDIALOG *f_current) 
{
	int i;

	for (i = 0; i < 1024; i++) {
		f_current->file_entries[i] = NULL;
	}

	for (i = 0; i < 1024; i++) {
		f_current->directory_entries[i] = NULL;
	}
	
}

void
x_fileselect_free_list_buffers(FILEDIALOG *f_current) 
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

void
x_fileselect_setup_list_buffers(FILEDIALOG *f_current, 
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

void
x_fileselect_fill_lists(FILEDIALOG *f_current)
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
	int pass_count = 0;

	directory = opendir(f_current->directory);

	if (!directory) {
		fprintf(stderr, "Agg, could open directory: %s\n", f_current->directory);
		return;
	}


	while((dirent_ptr = readdir(directory)) != NULL) {
		sprintf(path_buf, "%s%s", f_current->directory, dirent_ptr->d_name);
		if(stat(path_buf, &stat_en) >= 0 && S_ISDIR(stat_en.st_mode)) {
		//	printf("dir: %s\n", path_buf);	
			num_directories++;	
		} else {
		//	printf("file: %s\n", path_buf);	
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

	while((dirent_ptr = readdir(directory)) != NULL) {
		sprintf(path_buf, "%s%s", f_current->directory, dirent_ptr->d_name);
		if(stat(path_buf, &stat_en) >= 0 && S_ISDIR(stat_en.st_mode)) {

			f_current->directory_entries[dir_count] = (char *)
					malloc(sizeof(char)*(strlen(
							dirent_ptr->d_name)+2));
	
			sprintf(f_current->directory_entries[dir_count], 
				"%s", dirent_ptr->d_name);
			dir_count++;

		} else {
			f_current->file_entries[file_count] = (char *)
					malloc(sizeof(char)*(strlen(
							dirent_ptr->d_name)+1));
			strcpy(f_current->file_entries[file_count], 
			       dirent_ptr->d_name);
			file_count++;
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
	for (i = 0 ; i < num_directories; i++) {
		temp = u_basic_strdup_multiple(f_current->directory_entries[i],
					      "/", NULL);
		text[0] = temp; 
		gtk_clist_append (GTK_CLIST (f_current->dir_list), text);
		width = gdk_string_width(f_current->dir_list->style->font,
                                         f_current->directory_entries[i]);
		if (width > max_width) {
			gtk_clist_set_column_width(GTK_CLIST(f_current->
						   dir_list), 0, width);
			max_width = width;
		}

#if DEBUG
		printf("directory: %s\n", f_current->directory_entries[i]);
#endif
	}

	max_width = 0;
	for (i = 0 ; i < num_files; i++) {
		text[0] = f_current->file_entries[i]; 
		gtk_clist_append (GTK_CLIST (f_current->file_list), text);
		width = gdk_string_width(f_current->dir_list->style->font,
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
}


int
x_fileselect_preview_checkbox(GtkWidget *widget, FILEDIALOG *f_current)
{
	if (f_current == NULL) {
		fprintf(stderr, "x_fileselect_preview_checkbox: Oops got a null f_current!\n");
		exit(-1);
	}

	if (f_current->preview_control) {
		f_current->preview_control = FALSE;
		x_repaint_background(f_current->preview);
	} else {
		f_current->preview_control = TRUE;

		if (f_current->filename) {
			x_preview_update(f_current->preview,	
					 f_current->directory,
					 f_current->filename);
		}
	}
}

void
x_fileselect_close (GtkWidget *w, FILEDIALOG *f_current)
{
	gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
	
}

void
x_fileselect_close_saveas(GtkWidget *w, FILEDIALOG *f_current)
{

}

void
x_fileselect_change_dir(FILEDIALOG *f_current, char *new_directory)
{
	if (new_directory) {
		chdir(new_directory);
		x_fileselect_update_dirfile(f_current, NULL);
		x_fileselect_fill_lists(f_current);
	}
}

/* don't use widget, since it can be NULL */
void
x_fileselect_open_file(GtkWidget *w, FILEDIALOG *f_current)
{
	TOPLEVEL *w_current;
	PAGE *found_page;
	char *string;

	w_current = f_current->toplevel;

	string = gtk_entry_get_text(GTK_ENTRY(f_current->filename_entry));

	
	if (!string) {
		return;
	}

#if DEBUG
	printf("opening: %s\n", string);
#endif

	if ( !(found_page = s_page_new(w_current, string)) ) {
		w_current->DONT_REDRAW = 1;
		f_open(w_current, w_current->page_current->page_filename);
		i_set_filename(w_current, w_current->page_current->
					  page_filename);

		x_repaint_background(w_current);
		x_window_setup_world(w_current);
		x_manual_resize(w_current);
		a_zoom_limits(w_current, w_current->page_current->object_head);

		/* now update the scrollbars */
		x_hscrollbar_update(w_current);
		x_vscrollbar_update(w_current);
		update_page_manager(NULL, w_current);
		w_current->DONT_REDRAW = 0;

		o_redraw_all(w_current);
	} else {
		s_page_goto(w_current, found_page);
		update_page_manager(NULL, w_current);
		i_set_filename(w_current, w_current->
		page_current->page_filename);
		x_scrollbars_update(w_current);
		o_redraw_all(w_current);
	}
	gtk_widget_destroy(GTK_WIDGET(f_current->xfwindow));
}

void
x_fileselect_dir_button (GtkWidget *widget, gint row, gint column,
                         GdkEventButton *bevent, FILEDIALOG *f_current)
{
	char *filename, *temp = NULL;

	gtk_clist_get_text (GTK_CLIST (f_current->dir_list), row, 0, &temp);

	if (temp) {	
#if DEBUG
		printf("selected: %d _%s_\n", row, temp);
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

void
x_fileselect_file_button (GtkWidget *widget, gint row, gint column,
                         GdkEventButton *bevent, FILEDIALOG *f_current)
{
	char *filename, *temp = NULL;

	gtk_clist_get_text (GTK_CLIST (f_current->file_list), row, 0, &temp);

	if (temp) {	

#if DEBUG
		printf("selected: %d %s\n", row, temp);
#endif
		if (bevent) {
			switch (bevent->type) {
				case(GDK_2BUTTON_PRESS):
					x_fileselect_open_file(NULL, f_current); 
					break;

				default:
					x_fileselect_update_dirfile(
							f_current, temp);

					if (f_current->preview_control) { 
						x_preview_update(f_current->
						   	preview, 
							f_current->directory,
				 			temp);
					}
					break;
			}
		}
	}
}

void
x_fileselect_setup (TOPLEVEL *w_current, int type, int filesel_type)
{
	GtkWidget *buttonapply;
	GtkWidget *buttonclose;
	GtkWidget *scrolled_win;
	GtkWidget *list_item;
	GtkWidget *optionmenu;
	GtkWidget *hbox, *action_area;
	GtkWidget *scroll_box;
	GtkWidget *separator;
	GtkWidget *frame;
	GtkWidget *drawbox;
	GtkWidget *label;
	GtkWidget *searchbox;
	int left, top, right, bottom;
	
	FILEDIALOG *f_current;

	GtkWidget *vbox;
	GtkWidget *list_hbox;
	char *dir_title [2];
	char *file_title [2];

	char *string = NULL;
	int i;

	if (type < 0 || type > 2) {
		return;
	}

	f_current = &w_current->fileselect[type];

	if (!f_current->xfwindow) {

		f_current->xfwindow = gtk_window_new (GTK_WINDOW_TOPLEVEL);
		f_current->toplevel = w_current;

		if (type == FILESELECT) {

			gtk_window_position(GTK_WINDOW(f_current->xfwindow),
                                            GTK_WIN_POS_MOUSE);

			if (filesel_type == OPEN) {
				gtk_window_set_title(GTK_WINDOW(
						     f_current->xfwindow),
                                     		     "Open...");
			} else if (filesel_type == SAVEAS) {
				gtk_window_set_title(GTK_WINDOW(
						     f_current->xfwindow),
                                     		     "Save As...");
			}
		} else {
			gtk_window_position(GTK_WINDOW(f_current->xfwindow),
                                            GTK_WIN_POS_NONE);
			gtk_window_set_title(GTK_WINDOW(f_current->xfwindow),
                                     "Select Component...");
		}

		gtk_signal_connect(GTK_OBJECT(f_current->xfwindow),
                                   "destroy",
                                   GTK_SIGNAL_FUNC(x_fileselect_destroy_window),
                                   f_current);

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

		label=gtk_label_new("Filter");
		gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  		gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 0);
		gtk_widget_show(label);

 		f_current->filter_entry = gtk_entry_new_with_max_length (255);
 		gtk_editable_select_region(GTK_EDITABLE(
					   f_current->filter_entry), 0, -1);
 		gtk_box_pack_start(GTK_BOX (vbox), 
				   f_current->filter_entry, FALSE, FALSE, 0);
 		gtk_widget_show(f_current->filter_entry);


  		list_hbox = gtk_hbox_new (FALSE, 5);
  		gtk_box_pack_start (GTK_BOX (vbox), list_hbox, TRUE, TRUE, 0);
  		gtk_widget_show (list_hbox);

		separator = gtk_hseparator_new ();
		gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, TRUE, 0);
		gtk_widget_show (separator);

#if 0
		frame = gtk_frame_new (NULL);
  		gtk_box_pack_start (GTK_BOX (vbox), frame, TRUE, TRUE, 0);
  		gtk_widget_show (frame);
#endif

  		drawbox = gtk_hbox_new (FALSE, 0);
/*		gtk_container_add (GTK_CONTAINER (frame), drawbox); */
  		gtk_box_pack_start (GTK_BOX (vbox), drawbox, TRUE, FALSE, 5);
  		gtk_widget_show (drawbox);

  		searchbox = gtk_vbox_new (FALSE, 0);
  		gtk_box_pack_end (GTK_BOX (drawbox), searchbox, TRUE, TRUE, 10);
  		gtk_widget_show (searchbox);

		dir_title[0] = u_basic_strdup("Directories");
  		dir_title[1] = NULL;
  		f_current->dir_list = gtk_clist_new_with_titles(1, 
							   (char**) dir_title);
  		gtk_widget_set_usize(f_current->dir_list, 
				     DIR_LIST_WIDTH, DIR_LIST_HEIGHT);
  		gtk_signal_connect (GTK_OBJECT (f_current->dir_list), "select_row",
                      (GtkSignalFunc) x_fileselect_dir_button, f_current);
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
  		gtk_widget_show (f_current->dir_list);
  		gtk_widget_show (scrolled_win);

  		/* The files clist */
  		file_title[0] = u_basic_strdup("Files");
  		file_title[1] = NULL;
  		f_current->file_list = gtk_clist_new_with_titles(1, 
							(gchar**) file_title);
  		gtk_widget_set_usize(f_current->file_list, 
				     FILE_LIST_WIDTH, FILE_LIST_HEIGHT);
  		gtk_signal_connect(GTK_OBJECT (f_current->file_list), 
				   "select_row", (GtkSignalFunc) 
				   x_fileselect_file_button,
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
  		gtk_widget_show (f_current->file_list);
  		gtk_widget_show (scrolled_win);



  		f_current->preview = x_preview_setup(f_current->xfwindow, 
						     drawbox);

		f_current->preview_checkbox = gtk_check_button_new_with_label(
						"Preview");
  		gtk_box_pack_start(GTK_BOX(searchbox), 
				   f_current->preview_checkbox, 
				   FALSE, FALSE, 0);
		/* other checkbox stuff is done AFTER drawing area is mapped */
  		gtk_widget_show(f_current->preview_checkbox);


		label=gtk_label_new("Search");
		gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  		gtk_box_pack_start(GTK_BOX(searchbox), label, FALSE, FALSE, 5);
		gtk_widget_show(label);


 		f_current->search_entry = gtk_entry_new_with_max_length (255);
 		gtk_editable_select_region(GTK_EDITABLE(
					   f_current->search_entry), 0, -1);
 		gtk_box_pack_start(GTK_BOX (searchbox), 
				   f_current->search_entry, FALSE, FALSE, 0);
 		gtk_widget_show(f_current->search_entry);

		label=gtk_label_new("Filename");
		gtk_misc_set_alignment(GTK_MISC(label), 0, 0);
  		gtk_box_pack_start(GTK_BOX(vbox), label, FALSE, FALSE, 5);
		gtk_widget_show(label);

 		f_current->filename_entry = gtk_entry_new_with_max_length(1024);
 		gtk_editable_select_region(GTK_EDITABLE(
					   f_current->filename_entry), 0, -1);
 		gtk_box_pack_start(GTK_BOX (vbox), 
				   f_current->filename_entry, FALSE, FALSE, 0);
		gtk_signal_connect(GTK_OBJECT(f_current->filename_entry), 
				   "activate", 
				   GTK_SIGNAL_FUNC(x_fileselect_open_file),
				   f_current);
 		gtk_widget_show(f_current->filename_entry);

#if 0
  		separator = gtk_hseparator_new ();
  		gtk_box_pack_start (GTK_BOX (vbox), separator, FALSE, TRUE, 0);
  		gtk_widget_show (separator);
#endif
		buttonapply = gtk_button_new_with_label ("Apply");
                GTK_WIDGET_SET_FLAGS(buttonapply, GTK_CAN_DEFAULT);
                gtk_box_pack_start(GTK_BOX(action_area),
                                   buttonapply, TRUE, TRUE, 0);
		gtk_widget_grab_default (buttonapply);
		if (filesel_type == OPEN) {
			gtk_signal_connect(GTK_OBJECT(buttonapply),
				   "clicked",
				   GTK_SIGNAL_FUNC(x_fileselect_open_file),
				   f_current);

#if 0 /* to be added eventually */
		} else if (filesel_type == SAVEAS) {
			gtk_signal_connect(GTK_OBJECT(buttonclose),
				   "clicked",
				   GTK_SIGNAL_FUNC(x_fileselect_close_saveas),
				   f_current);
		} else if (type == COMPSELECT) {
			gtk_signal_connect(GTK_OBJECT(buttonclose),
				   "clicked",
				   GTK_SIGNAL_FUNC(x_fileselect_close),
				   f_current);
#endif
		}
                gtk_widget_show(buttonapply);

		buttonclose = gtk_button_new_with_label ("Close");
                GTK_WIDGET_SET_FLAGS(buttonclose, GTK_CAN_DEFAULT);
                gtk_box_pack_start(GTK_BOX(action_area),
                                   buttonclose, TRUE, TRUE, 0);

		if (filesel_type == OPEN) {
			gtk_signal_connect(GTK_OBJECT(buttonclose),
				   "clicked",
				   GTK_SIGNAL_FUNC(x_fileselect_close),
				   f_current);

		} else if (filesel_type == SAVEAS) {
			gtk_signal_connect(GTK_OBJECT(buttonclose),
				   "clicked",
				   GTK_SIGNAL_FUNC(x_fileselect_close_saveas),
				   f_current);
		} else if (type == COMPSELECT) {
			gtk_signal_connect(GTK_OBJECT(buttonclose),
				   "clicked",
				   GTK_SIGNAL_FUNC(x_fileselect_close),
				   f_current);
		}

                gtk_widget_show(buttonclose);

		x_fileselect_update_dirfile(f_current, NULL);
		if (type == FILESELECT) {
			x_fileselect_fill_lists(f_current);
		}	
	}

	if (!GTK_WIDGET_VISIBLE(f_current->xfwindow)) {
		gtk_widget_show(f_current->xfwindow);
		gdk_window_raise(f_current->xfwindow->window);
		x_preview_setup_rest(f_current->preview);
		gtk_grab_add (f_current->xfwindow);

		/* need to delay this till the drawing area is created and
		 * is showing */
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
