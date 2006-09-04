/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtk/gtk.h>
#include <glib.h>
#include <libguile.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "funcs.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static gint global_pid = 0;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Creates a new page and add it to <B>toplevel</B>'s list of pages.
 *
 *  It initializes the #PAGE structure and set its <B>page_filename</B>
 *  to <B>filename</B>. <B>toplevel</B>'s current page is not changed by
 *  this function.
 */
PAGE *s_page_new (TOPLEVEL *toplevel, const gchar *filename)
{
  PAGE *page;

  /* Now create a blank page */
  page = (PAGE*)g_new (PAGE, 1);

  page->pid = global_pid++;

  page->CHANGED = 0;

  /* big assumption here that page_filename isn't null */
  if (g_path_is_absolute (filename)) {
    page->page_filename = g_strdup (filename);
  } else {
    gchar *pwd = g_get_current_dir ();
    page->page_filename = g_strconcat (pwd,
                                       G_DIR_SEPARATOR_S,
                                       filename,
                                       NULL);
    g_free (pwd);
  }
	
  g_assert (toplevel->init_bottom != 0);
  page->coord_aspectratio = (
    ((float) toplevel->init_right) / ((float) toplevel->init_bottom));

  page->up = -2;
  page->page_control = 0;

  /* Init tile array */
  s_tile_init (toplevel, page);

  /* First one to setup head */
  page->object_head = s_basic_init_object("object_head");
  page->object_head->type = OBJ_HEAD;

  /* new selection mechanism */
  page->selection2_head = page->selection2_tail = 
  o_selection_new_head();

  /* net/pin/bus stretch when doing moves */
  page->stretch_head = page->stretch_tail = s_stretch_new_head();

  page->complex_place_tail = page->complex_place_head = 
  s_basic_init_object("complex_place_head");
  page->complex_place_tail->type = OBJ_HEAD;

  /* add p_attrib and p_attached_to */
  page->attrib_place_tail = page->attrib_place_head = 
  s_basic_init_object("attrib_place_head");
  page->attrib_place_tail->type = OBJ_HEAD;
	
  /* do this just to be sure that object tail is truely correct */
  page->object_tail = return_tail(page->object_head);

  /* setup parent to point to list */
  /* this is used for attributes so */
  /* that we know which list to search */
  page->object_parent = page->object_head; 

  /* init undo struct pointers */
  s_undo_init(page);
  
  page->object_lastplace = NULL;
  page->object_selected  = NULL;
  
  set_window (toplevel, page,
              toplevel->init_left, toplevel->init_right,
              toplevel->init_top,  toplevel->init_bottom);

  /* Backup variables */
  g_get_current_time (&page->last_load_or_save_time);
  page->ops_since_last_backup = 0;
  page->saved_since_first_loaded = 0;
  page->do_autosave_backup = 0;

  page->load_newer_backup_func = load_newer_backup_func;

  /* now append page to page list of toplevel */
  toplevel->page_tail->next = page;
  page->prev = toplevel->page_tail;
  page->next = NULL;
  toplevel->page_tail = page;
  
  return page;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Deletes a single page <B>page</B> from <B>toplevel</B>'s list of pages.
 *
 *  This function is not appropriate for deleting page head. 
 *  See #s_page_delete_list() for that.
 *
 *  If the current page of toplevel is given as parameter <B>page</B>,
 *  the function sets the field <B>page_current</B> of the TOPLEVEL
 *  struct to NULL.
 */
void s_page_delete (TOPLEVEL *toplevel, PAGE *page)
{
  PAGE *tmp;
  gchar *backup_filename;
  gchar *real_filename;
  gchar *only_filename;
  gchar *dirname;
  
  g_assert (page->pid != -1);

  /* we need to play with page_current because s_delete_list_fromstart() */
  /* make use of it (see s_tile_remove_object_all) */
  
  /* save page_current and switch to page */
  if (page == toplevel->page_current) {
    tmp = NULL;
  } else {
    tmp = toplevel->page_current;
    s_page_goto (toplevel, page);
  }

  /* Get the real filename and file permissions */
  real_filename = follow_symlinks (page->page_filename, NULL);
  
  if (real_filename == NULL) {
    s_log_message ("s_page_delete: Can't get the real filename of %s.", page->page_filename);
    fprintf (stderr, "s_page_delete: Can't get the real filename of %s.\n", page->page_filename);
  }
  else {
    /* Get the directory in which the real filename lives */
    dirname = g_path_get_dirname (real_filename);
    only_filename = g_path_get_basename(real_filename);  
    
    backup_filename = g_strdup_printf("%s%c"AUTOSAVE_BACKUP_FILENAME_STRING,
				      dirname, G_DIR_SEPARATOR, only_filename);

    /* Delete the backup file */
    if ( (g_file_test (backup_filename, G_FILE_TEST_EXISTS)) && 
	 (!g_file_test(backup_filename, G_FILE_TEST_IS_DIR)) )
    {
      if (unlink(backup_filename) != 0) {
	s_log_message("s_page_delete: Unable to delete backup file %s.", backup_filename);      }
    }
    g_free (dirname);
    g_free (only_filename);
    g_free (backup_filename);
  }
  g_free(real_filename);

  /* first delete objects of page */
  s_delete_list_fromstart (toplevel, page->object_head);
  
  toplevel->REMOVING_SEL = 1;
  s_delete_list_fromstart (toplevel, page->complex_place_head);
  s_delete_list_fromstart (toplevel, page->attrib_place_head);
  o_selection_destroy_all (page->selection2_head);
  toplevel->REMOVING_SEL = 0;  

#if DEBUG
  printf("Freeing page: %s\n", page->page_filename);
  s_tile_print(toplevel);
#endif
  s_tile_free_all (page);

  s_stretch_destroy_all (page->stretch_head);

  /* free current page undo structs */
  s_undo_free_all (toplevel, page); 

  /* ouch, deal with parents going away and the children still around */
  page->up = -2;
  /* p_current->down = NULL; not needed */

  g_free (page->page_filename);

  /* now unlink page from its list */
  if (page->next) {
    page->next->prev = page->prev;
  } else {
    /* page if the tail of page list: update toplevel */
    g_assert (toplevel->page_tail == page);
    toplevel->page_tail = page->prev;
  }
  if (page->prev) {
    page->prev->next = page->next;
  }
  
#if DEBUG
  s_tile_print (toplevel);
#endif

  g_free (page);

#if 0 /* don't do this for now hack */ /* this is a per window free */
  o_attrib_free_current(w_current);
  o_complex_free_filename(w_current);
#endif

  /* restore page_current */
  if (tmp != NULL) {
    s_page_goto (toplevel, tmp);
  } else {
    /* page was page_current */
    toplevel->page_current = NULL;
    /* page_current must be updated by calling function */
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief Initializes <B>toplevel</B>'s list of pages.
 *  \par Function Description
 *  This function creates a head page and set the toplevel fields relative
 *  to page management.
 */
void s_page_init_list (TOPLEVEL *toplevel)
{
  PAGE *head;

  g_assert (toplevel->page_head == NULL);

  head = (PAGE*)g_new (PAGE, 1);

  head->pid = -1;
  head->CHANGED = 0;
  head->page_filename = g_strdup ("page_head");
  head->prev = NULL;
  head->next = NULL;
  /* this is important so that page_next and page_prev ignore the 
   * page head node 
   */
  head->page_control = -1; 

  /* add head as page head of toplevel */
  toplevel->page_head = toplevel->page_tail = head;
 
}

/*! \todo Finish function documentation!!!
 *  \brief Deletes the list of pages of <B>toplevel</B>.
 *  \par Function Description
 *  Deletes the list of pages of <B>toplevel</B>.
 *  This function should only be called when you are finishing up.
 */
void s_page_delete_list(TOPLEVEL *toplevel)
{
  PAGE *p_current, *p_prev;

  p_current = toplevel->page_tail;

  while (p_current != NULL && p_current->pid != -1) {
    p_prev = p_current->prev;
    s_page_delete (toplevel, p_current);
    p_current = p_prev;
  }	

  g_assert (p_current->pid == -1 &&
            p_current->prev == NULL && p_current->next == NULL);
  
  /* Now free the head */
  g_free (p_current->page_filename);
  g_free (p_current);

  /* reset toplevel fields */
  toplevel->page_head    = NULL;
  toplevel->page_tail    = NULL;
  toplevel->page_current = NULL;
  
}

/*! \todo Finish function documentation!!!
 *  \brief Changes the current page in <B>toplevel</B> to be <B>p_new</B>.
 *  \par Function Description
 *  Changes the current page in <B>toplevel</B> to be <B>p_new</B>.
 *
 */
void s_page_goto (TOPLEVEL *toplevel, PAGE *p_new) 
{
  gchar *dirname;
  
  toplevel->page_current = p_new;

  dirname = g_dirname (p_new->page_filename);
  chdir (dirname);
  g_free (dirname);
  
}

/*! \todo Finish function documentation!!!
 *  \brief Search for pages by filename.
 *  \par Function Description
 *  Searches in <B>toplevel</B>'s list of pages for a page with a filename
 *  equal to <B>filename</B>.
 *  
 *  \return PAGE pointer to a matching page, NULL otherwise.
 */
PAGE *s_page_search (TOPLEVEL *toplevel, const gchar *filename)
{
  PAGE *p_current;

  for (p_current = toplevel->page_head;
       p_current != NULL;
       p_current = p_current->next) {
    if (g_strcasecmp (p_current->page_filename, filename) == 0) {
      return p_current;
    }
  }

  return NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief Search for pages by page id.
 *  \par Function Description
 *  This function tries to find a page refered by its <B>page_id</B>.
 *
 *  \return PAGE pointer to matching page, NULL otherwise.
 */
PAGE *s_page_search_pid(TOPLEVEL * toplevel, gint page_id) 
{
  PAGE* p_current;

  for (p_current = toplevel->page_head;
       p_current != NULL;
       p_current = p_current->next) {
    if (p_current->pid == page_id)
      return p_current;
  }

  return NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief 
 *  \par Function Description
 *
 */
gint s_page_search_row(TOPLEVEL *toplevel, PAGE *p_findme)
{
  PAGE *p_current;

  for (p_current = toplevel->page_head;
       p_current != NULL;
       p_current = p_current->next) {
    if (p_current->clist_row == p_findme->clist_row) {
      return p_current->clist_row;
    }
  }

  return 0; /* can't find page... well just select row 0 */
}

/*! \brief Print full TOPLEVEL structure.
 *  \par Function Description
 *  This function prints the internal structure of <B>toplevel</B>'s
 *  list of pages.
 *
 *  \param [in] toplevel  The TOPLEVEL object to print.
 */
void s_page_print_all (TOPLEVEL *toplevel)
{
  PAGE *p_current;

  g_assert (toplevel->page_head != NULL &&
            toplevel->page_head->pid == -1);

  for (p_current = toplevel->page_head->next;
       p_current != NULL;
       p_current = p_current->next) {
    printf ("FILENAME: %s\n", p_current->page_filename);
    print_struct_forw (p_current->object_head);
  }
  
}

/*! \todo Finish function documentation!!!
 *  \brief Saves all the pages of a TOPLEVEL object.
 *  \par Function Description
 *  Saves all the pages in the <B>toplevel</B> parameter.
 *
 *  \param [in] toplevel  The TOPLEVEL to save pages from.
 *  \return The number of failed tries to save a page.
 */
gint s_page_save_all (TOPLEVEL *toplevel)
{
  PAGE *p_save, *p_current;
  gint status = 0;

  g_assert (toplevel->page_head != NULL &&
            toplevel->page_head->pid == -1);

  /* save current page */
  p_save = toplevel->page_current;
  
  for (p_current = toplevel->page_head->next;
       p_current != NULL;
       p_current = p_current->next) {
    /* make p_current the current page of toplevel */
    s_page_goto (toplevel, p_current);

    if (f_save (toplevel, p_current->page_filename)) {
      s_log_message ("Saved [%s]\n",
                     toplevel->page_current->page_filename);
      /* reset the CHANGED flag of p_current */
      p_current->CHANGED = 0;
      
    } else {
      s_log_message ("Could NOT save [%s]\n", 
                     toplevel->page_current->page_filename);
      /* increase the error counter */
      status++;
    }
    
  }

  /* restore current page */
  s_page_goto (toplevel, p_save);

  return status;
}

/*! \todo Finish function documentation!!!
 *  \brief Check if CHANGED flag is set for any page in list.
 *  \par Function Description
 *  This function checks the CHANGED flag for all pages in the <B>head</B>
 *  object.
 *
 *  \param [in] head  PAGES list to check CHANGED flag in.
 *  \return 1 if any page has the CHANGED flag set, 0 otherwise.
 */
gboolean s_page_check_changed (PAGE *head)
{
  PAGE *p_current;

  for (p_current = head;
       p_current != NULL;
       p_current = p_current->next) {
    if (p_current->CHANGED) {
      return TRUE;
    }
  }

  return FALSE;
}

/*! \brief Reset the CHANGED flag of all pages.
 *  \par Function Description
 *  This function resets the CHANGED flag of each page following <B>head</B>.
 *
 *  \param [in,out] head  PAGE list to set CHANGED flags in.
 */
void s_page_clear_changed (PAGE *head)
{
  PAGE *p_current;

  for (p_current = head;
       p_current != NULL;
       p_current = p_current->next) {
    p_current->CHANGED = 0;
  }

}

/*! \brief Autosave initialization function.
 *  \par Function Description
 *  This function sets up the autosave callback function.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 */
void s_page_autosave_init(TOPLEVEL *toplevel)
{
  if (toplevel->auto_save_interval != 0) {

    /* 1000 converts seconds into milliseconds */
    toplevel->auto_save_timeout = 
      g_timeout_add(toplevel->auto_save_interval*1000,
                    (GSourceFunc) s_page_autosave,
                    toplevel);
  }
}

/*! \brief Autosave callback function.
 *  \par Function Description
 *  This function is a callback of the glib g_timeout functions.
 *  It is called every "interval" milliseconds and it sets a flag to save
 *  a backup copy of the opened pages.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \return The length in milliseconds to set for next interval.
 */
gint s_page_autosave (TOPLEVEL *toplevel) 
{
  PAGE *p_current;

  if (toplevel == NULL) {
    return 0;
  }

  /* Do nothing if the interval is 0 */
  if (toplevel->auto_save_interval == 0) {
    return toplevel->auto_save_interval;
  }
  
  /* In which situation can be page_head = NULL?
     Should we just disable the autosave timeout returning 0 or
     just wait for more pages to be added? */
  if ( (toplevel->page_head == NULL) ||
       (toplevel->page_head->next == NULL) ) {
    return (toplevel->auto_save_interval);
  }

  for (p_current = toplevel->page_head->next;
       p_current != NULL;
       p_current = p_current->next) {

    if (p_current->ops_since_last_backup != 0) {
      /* Real autosave is done in o_undo_savestate */
      p_current->do_autosave_backup = 1;
    }
    
  }

  return toplevel->auto_save_interval;
  
}
