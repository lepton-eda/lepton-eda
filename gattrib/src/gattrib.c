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

/*****************************************************************************
 * In the spirit of open source/free software, major sections of             *
 * gattrib's code were borrowed from other sources, and hacked               *
 * together by SDB in Dec. 2003.  Particularly rich sources for code were    *
 * gEDA/gnetlist, and the gtkextra program testgtksheet.c.  Thanks to their  *
 * authors for providing the foundation upon which this is built.            *
 *                                                                           *
 * Of course, I *did* write major portions of the code too . . . . .         *
 * Some documentation about the internal operation of this program can be    *
 * found in the "NOTES" file  in the gattrib top-level directory.            *
 * -- SDB  December 2003 -                                                   *
 *****************************************************************************/

#include <config.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

/*------------------------------------------------------------------
 * Includes originally from testgtksheet -- stuff needed to deal with 
 * spreadsheet widget.
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

/*------------------------------------------------------------------
 * Gattrib specific includes -- stuff dealing with gattrib data structs.
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
/* #include "../include/x_menu.h" */


/*------------------------------------------------------------------
 * gattrib_quit -- wrap up and quit fcn.
 *------------------------------------------------------------------*/
void gattrib_quit(void)
{
    s_clib_cache_free();
    s_clib_free();
    s_slib_free();
    /* s_rename_destroy_all(); */
#ifdef DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In gattrib_quit, calling gtk_main_quit()\n");
#endif
    gtk_main_quit();
    return;
}

/*------------------------------------------------------------------
 * gattrib_main -- main gattrib fcn.
 *------------------------------------------------------------------*/
void gattrib_main(int argc, char *argv[])
{
  /* TOPLEVEL *pr_current is a global */
  /* SHEET_DATA *sheet_head is a global */
  /* GtkWidget *main_window is a global */

  int i;
  int return_code = 0;  /* used when invoking s_toplevel_read_page */
  int argv_index;
  char *cwd;
  char *full_filename;
  PAGE *p_local;
  

  
  /* Initialize gEDA stuff */
  libgeda_init();

  /* Note that argv_index holds index to first non-flag command line option 
   * (that is, to the first file name) */
  argv_index = parse_commandline(argc, argv);
  cwd = u_basic_strdup(getcwd(NULL, 1024));
  
  /* ----------  create log file right away ---------- */
  /* ----------  even if logging is enabled ---------- */
  s_log_init(cwd, "gattrib.log");
  
  s_log_message
    ("gEDA/gattrib version %s\n", VERSION);
  s_log_message
    ("gEDA/gattrib comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
  s_log_message
    ("This is free software, and you are welcome to redistribute it under certain\n");
  s_log_message
    ("conditions; please see the COPYING file for more details.\n\n");
  
  if (!quiet_mode) {
    fflush(stderr);
    fflush(stdout);
    fprintf(stderr, 
	    "gEDA/gattrib version %s\n", VERSION);
    fprintf(stderr,
	    "gEDA/gattrib comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
    fprintf(stderr,
	    "This is free software, and you are welcome to redistribute it under certain\n");
    fprintf(stderr,
	    "conditions; please see the COPYING file for more details.\n\n");
  }
  

  /* ------  register guile (scheme) functions.  Necessary to parse RC file.  ------ */
  g_register_funcs();
#if DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_main -- we have just registered the guile functions.\n");
#endif  


  /* ------  These libraries are defined in libgeda  ------ */
#if DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_main -- we have just initialized the component and source libs.\n");
#endif  
  
  /* ----- Read in all rc files and set up relevant directory pointers ----- */
  g_rc_parse("gattribrc", rc_filename);
#if DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_main -- we have just read in and parsed the RC files.\n");
#endif  

  /* ---------- This creates pointer to new project: (TOPLEVEL *pr_current) ---------- */
  pr_current = s_project_create_new();    /* pr_current declared in globals.h */
  s_toplevel_init(pr_current);  /* This finishes initialization of pr_current */
#if DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_main -- we have just created and init'ed a new pr_current\n");
#endif  
  

  /* --------  Initialize main_window.  -------- */
#if DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_main -- calling gtk_init. . . ..\n");
#endif  
  gtk_init(&argc, &argv);

  x_window_init();  
#if DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_main -- we have just initialized the main_window.\n");
#endif  
 
  
  /* ---------- Initialize SHEET_DATA data structure ---------- */
  sheet_head = s_sheet_data_new();   /* sheet_head was declared in globals.h */


  /* ---------- Now loop on the files specified on the cmd line & read them in ---------- */
  /* argv[0] = name of this prog (gattrib).  argv_index holds the 
   * position of the first filename  */
  i = argv_index;
  while(argv[i]) {
    full_filename = u_basic_strdup(argv[i]);
    
#if DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In gattrib_main, opening full_filename = %s\n", full_filename);
#endif
    
    if (first_page == 1) {
      if (pr_current->page_current->page_filename) {
	/* Page structure & first page has already been created in 
	 * s_project_create_new.  Therefore, just rename the first page
	 * and open the project.  First free "unknown" name.  */
	free(pr_current->page_current->page_filename);
      }
      return_code |= s_toplevel_read_page(full_filename); /* read in first page */
      first_page = 0;
    } else {
      return_code |= s_toplevel_read_page(full_filename); /* read in secondary page */
    }  
    free(full_filename);
    
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

    i++;
    
  }  /* while(argv[i])  */
  free(cwd);


  /* ---------- Now complete read-in of project  ---------- */
  if (first_page != 1) { 


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

    /* ---------- Create and load the tables  ---------- */
    sheet_head->component_table = s_table_new(sheet_head->comp_count, sheet_head->comp_attrib_count);
    sheet_head->net_table = s_table_new(sheet_head->net_count, sheet_head->net_attrib_count);
    sheet_head->pin_table = s_table_new(sheet_head->pin_count, sheet_head->pin_attrib_count);
    
    p_local = pr_current->page_head; /* must iterate over all pages in design */
    while (p_local != NULL) {
      if (p_local->pid != -1) {   /* only traverse pages which are toplevel */
	if (p_local->object_head && p_local->page_control == 0) {
	  s_table_add_toplevel_comp_items_to_comp_table(p_local->object_head);    /* adds all components from page to comp_table */

#if 0
	  /* Note that this must be changed.  We need to input the entire project
	   * before doing anything with the nets because we need to first
	   * determine where they are all connected!   */
	  s_table_add_toplevel_net_items_to_net_table(p_local->object_head);     /* adds all nets from page to net_table */
#endif

	  s_table_add_toplevel_pin_items_to_pin_table(p_local->object_head);    /* adds all pins from page to pin_table */

	}
      }
      p_local = p_local->next;  /* iterate to next schematic page */
    }
#if DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In gattrib_main -- we have just returned from adding to the tables.\n");
    fflush(stderr);
    fflush(stdout);
#endif  


#if DEBUG
    /*  -----  Make debug printout of entire object list  -----  */
    fflush(stderr);
    fflush(stdout);
    printf("In gattrib_main -- we have just read in the project and filled out pr_current\n");
    printf("----------------------------  Object list -----------------------------\n");
    s_page_print_all(pr_current);
    printf("-----------------------------------------------------------------------\n");
#endif

    
    /* -------------- Next, update windows --------------- */
    x_window_add_items();    /* This updates the top level stuff,
			      * and then calls another fcn to update
			      * the GtkSheet itself.  */
#if DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In gattrib_main -- we have just returned from x_window_add_items.\n");
#endif  
    
  }  /* if (first_page != 1) */
  else {
    /* no filename found on command line, therefore we are still on the first page */
#if DEBUG
    fflush(stderr);
    fflush(stdout);
    printf("In gattrib_main -- no files specified on command line.  Throw up filedialog.\n");
#endif
    x_fileselect_setup(pr_current, OPEN);

    gtk_widget_show( GTK_WIDGET(notebook) );
    gtk_widget_show( GTK_WIDGET(window) );

    while( gtk_events_pending () ) {
#ifdef DEBUG
      printf("In gattrib_main, trying to flush gtk event queue before running gtk_main. . . . \n");
#endif
      gtk_main_iteration();  /* force window exposure by running event handler once */
    }
    
  }

  /* ---------- Now verify correctness of read-in design.  ---------- */
  s_toplevel_verify_design(pr_current);


  /* ---------- Now enter main event loop for spreadsheet.  ---------- */
  gtk_widget_show( GTK_WIDGET(window) );  /*  One final show for good measure  */
  gtk_main_iteration();  /* force window exposure by running event handler once */
  gtk_main();


  /* ---------- Spreadsheet has been killed; we are quitting.  ---------- */
#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("In gattrib_main, we have exited gtk_main. \n");
#endif  

  return;
}

/*------------------------------------------------------------------
 * main -- entry point to gattrib.  This is just a wrapper which 
 * invokes the guile stuff, and points to the real main prog, 
 * gattrib_main.  Note that I still need some vestigal
 * guile stuff in order to read the rc files.
 *------------------------------------------------------------------*/
int main(int argc, char *argv[])
{

  /* This is i18n stuff */
#if ENABLE_NLS
  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);
#ifdef HAS_GTK22
  bind_textdomain_codeset(PACKAGE, "UTF-8");
#endif
#endif

  /* disable the deprecated warnings in guile 1.6.3 */
  /* Eventually the warnings will need to be fixed */
  if(getenv("GUILE_WARN_DEPRECATED")==NULL)
    putenv("GUILE_WARN_DEPRECATED=no");
  
  gh_enter(argc, argv, gattrib_main);

#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("Now exiting main . . . Bye!\n");
#endif
  /* return 0 */
  exit(0);
}
