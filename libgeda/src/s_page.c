/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "libgeda_priv.h"

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
  page = (PAGE*)g_new0 (PAGE, 1);

  page->pid = global_pid++;

  page->CHANGED = 0;

  /* big assumption here that page_filename isn't null */
  if (g_path_is_absolute (filename)) {
    page->page_filename = g_strdup (filename);
  } else {
    gchar *pwd = g_get_current_dir ();
    page->page_filename = g_build_filename (pwd, filename, NULL);
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
  page->selection_list = o_selection_new();

  /* net/pin/bus stretch when doing moves */
  page->stretch_head = page->stretch_tail = s_stretch_new_head();

  page->complex_place_list = NULL;
  page->attrib_place_list = NULL;

  /* do this just to be sure that object tail is truely correct */
  page->object_tail = return_tail(page->object_head);

  /* setup parent to point to list */
  /* this is used for attributes so */
  /* that we know which list to search */
  page->object_parent = page->object_head; 

  /* init undo struct pointers */
  s_undo_init(page);
  
  page->object_lastplace = NULL;
  
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
  geda_list_add( toplevel->pages, page );

  return page;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Deletes a single page <B>page</B> from <B>toplevel</B>'s list of pages.
 *
 *  See #s_page_delete_list() to delete all pages of a <B>toplevel</B>
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
    s_log_message (_("s_page_delete: Can't get the real filename of %s."),
                   page->page_filename);
  }
  else {
    backup_filename = f_get_autosave_filename (real_filename);

    /* Delete the backup file */
    if ( (g_file_test (backup_filename, G_FILE_TEST_EXISTS)) && 
	 (!g_file_test(backup_filename, G_FILE_TEST_IS_DIR)) )
    {
      if (unlink(backup_filename) != 0) {
	s_log_message(_("s_page_delete: Unable to delete backup file %s."),
                      backup_filename);
      }
    }
    g_free (backup_filename);
  }
  g_free(real_filename);

  /* Free the selection object */
  g_object_unref( page->selection_list );

  /* then delete objects of page */
  s_delete_list_fromstart (toplevel, page->object_head);

  /* The complex place list contain a reference to the objects in the page */
  /* So don't free the objects there. */
  g_list_free (page->complex_place_list);
  page->complex_place_list = NULL;
  /* Free the objects in the attrib place list. */
  s_delete_object_glist (toplevel, page->attrib_place_list);
  page->attrib_place_list = NULL;

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
  g_free (page->page_filename);

  geda_list_remove( toplevel->pages, page );

#if DEBUG
  s_tile_print (toplevel);
#endif

  g_free (page);

#if 0 /* don't do this for now hack */ /* this is a per window free */
  o_attrib_free_current(toplevel);
  o_complex_free_filename(toplevel);
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
 *  \brief Deletes the list of pages of <B>toplevel</B>.
 *  \par Function Description
 *  Deletes the list of pages of <B>toplevel</B>.
 *  This function should only be called when you are finishing up.
 */
void s_page_delete_list(TOPLEVEL *toplevel)
{
  GList *list_copy, *iter;
  PAGE *page;

  /* s_page_delete removes items from the page list, so make a copy */
  list_copy = g_list_copy (geda_list_get_glist (toplevel->pages));

  for (iter = list_copy; iter != NULL; iter = g_list_next (iter)) {
    page = (PAGE *)iter->data;

    s_page_delete (toplevel, page);
  }

  g_list_free (list_copy);

  /* reset toplevel fields */
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
  const GList *iter;
  PAGE *page;

  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    page = (PAGE *)iter->data;
    if ( g_strcasecmp( page->page_filename, filename ) == 0 )
      return page;
  }
  return NULL;
}

/*! \brief Search for a page given its page id in a page list.
 *  \par Function Description
 *  This functions returns the page that have the page id \a pid in
 *  the list of pages starting at \a page_list, or NULL if there is no
 *  such page.
 *
 *  \param [in] page_list The list of page to search the page in.
 *  \param [in] pid       The ID of the page to find.
 *  \returns A pointer on the page found or NULL if not found.
 */
PAGE *s_page_search_by_page_id (GedaPageList *list, int pid)
{
  const GList *iter;

  for ( iter = geda_list_get_glist (list);
        iter != NULL;
        iter = g_list_next (iter) ) {
    PAGE *page = (PAGE *)iter->data;
    if (page->pid == pid) {
      return page;
    }
  }

  return NULL;
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
  const GList *iter;
  PAGE *page;

  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    page = (PAGE *)iter->data;
    printf ("FILENAME: %s\n", page->page_filename);
    print_struct_forw (page->object_head);
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
  const GList *iter;
  PAGE *p_save, *p_current;
  gint status = 0;

  /* save current page */
  p_save = toplevel->page_current;

  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;

    /* make p_current the current page of toplevel */
    s_page_goto (toplevel, p_current);

    if (f_save (toplevel, p_current->page_filename)) {
      s_log_message (_("Saved [%s]\n"),
                     toplevel->page_current->page_filename);
      /* reset the CHANGED flag of p_current */
      p_current->CHANGED = 0;

    } else {
      s_log_message (_("Could NOT save [%s]\n"),
                     toplevel->page_current->page_filename);
      /* increase the error counter */
      status++;
    }

  }

  /* restore current page */
  if (p_save != NULL) 
  {
     s_page_goto (toplevel, p_save);
  }

  return status;
}

/*! \todo Finish function documentation!!!
 *  \brief Check if CHANGED flag is set for any page in list.
 *  \par Function Description
 *  This function checks the CHANGED flag for all pages in the <B>list</B>
 *  object.
 *
 *  \param [in] list  GedaPageList to check CHANGED flag in.
 *  \return 1 if any page has the CHANGED flag set, 0 otherwise.
 */
gboolean s_page_check_changed (GedaPageList *list)
{
  const GList *iter;
  PAGE *p_current;

  for ( iter = geda_list_get_glist( list );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;
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
void s_page_clear_changed (GedaPageList *list)
{
  const GList *iter;
  PAGE *p_current;

  for ( iter = geda_list_get_glist( list );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;
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
  const GList *iter;
  PAGE *p_current;

  if (toplevel == NULL) {
    return 0;
  }

  /* Do nothing if the interval is 0 */
  if (toplevel->auto_save_interval == 0) {
    return toplevel->auto_save_interval;
  }

  /* Should we just disable the autosave timeout returning 0 or
     just wait for more pages to be added? */
  if ( toplevel->pages == NULL)
    return toplevel->auto_save_interval;

  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;

    if (p_current->ops_since_last_backup != 0) {
      /* Real autosave is done in o_undo_savestate */
      p_current->do_autosave_backup = 1;
    }
  }

  return toplevel->auto_save_interval;
}
