/* Lepton EDA library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*! \file toplevel.c
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "liblepton_priv.h"

/*!
 *  \brief Create a LeptonToplevel object
 *  \par Function Description
 *  Create and return an empty LeptonToplevel object with sensible
 *  defaults for its properties.
 *
 *  \returns the newly created LeptonToplevel.
 */
LeptonToplevel*
s_toplevel_new ()
{
  LeptonToplevel *toplevel;

  toplevel = (LeptonToplevel*) g_new (LeptonToplevel, 1);

  toplevel->RC_list = NULL;

  toplevel->pages = lepton_list_new();
  toplevel->page_current = NULL;

  toplevel->change_notify_funcs = NULL;

  /* Auto-save interval */
  toplevel->auto_save_interval = 0;
  toplevel->auto_save_timeout = 0;

  toplevel->weak_refs = NULL;
  return toplevel;
}


/*! \brief Deletes the list of pages of <B>toplevel</B>.
 *  \par Function Description
 *  Deletes the list of pages of <B>toplevel</B>.
 *  This function should only be called when you are finishing up.
 *
 *  \param toplevel  The LeptonToplevel object.
 */
static void
lepton_toplevel_delete_pages (LeptonToplevel *toplevel)
{
  GList *list_copy, *iter;
  LeptonPage *page;

  /* lepton_page_delete removes items from the page list, so make a copy */
  list_copy = g_list_copy (lepton_list_get_glist (toplevel->pages));

  for (iter = list_copy; iter != NULL; iter = g_list_next (iter)) {
    page = (LeptonPage *)iter->data;

    lepton_page_delete (toplevel, page);
  }

  g_list_free (list_copy);

  /* reset toplevel fields */
  lepton_toplevel_set_page_current (toplevel, NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
s_toplevel_delete (LeptonToplevel *toplevel)
{
  GList *iter;

  if (toplevel->auto_save_timeout != 0) {
    /* Assume this works */
    g_source_remove (toplevel->auto_save_timeout);
  }

  /* delete all pages */
  lepton_toplevel_delete_pages (toplevel);

  /* Delete the page list */
  g_object_unref(toplevel->pages);

  /* Remove all change notification handlers */
  for (iter = toplevel->change_notify_funcs;
       iter != NULL; iter = g_list_next (iter)) {
    g_free (iter->data);
  }
  g_list_free (toplevel->change_notify_funcs);

  toplevel->weak_refs = s_weakref_notify (toplevel, toplevel->weak_refs);

  g_free (toplevel);

}

/*\ brief Get the current page.
 *
 *  \param [in] toplevel This toplevel.
 *  \return The \a page_current field of the \a toplevel.
 */
LeptonPage*
lepton_toplevel_get_page_current (LeptonToplevel *toplevel)
{
  g_return_val_if_fail (toplevel != NULL, NULL);

  return toplevel->page_current;
}


/*\ brief Set the current page
 *
 *  \param [in,out] toplevel This toplevel
 *  \param [in]     page     The new current page
 */
void
lepton_toplevel_set_page_current (LeptonToplevel *toplevel,
                             LeptonPage *page)
{
  g_return_if_fail (toplevel != NULL);

  toplevel->page_current = page;
}


/*\ brief Get the list of current toplevel pages.
 *
 *  \param [in] toplevel This toplevel.
 *  \return The \a pages field of the \a toplevel.
 */
LeptonPageList*
lepton_toplevel_get_pages (LeptonToplevel *toplevel)
{
  g_return_val_if_fail (toplevel != NULL, NULL);

  return toplevel->pages;
}


/*\ brief Set the list of current toplevel pages.
 *
 *  \param [in,out] toplevel This toplevel.
 *  \param [in] pages The new list of pages.
 */
void
lepton_toplevel_set_pages (LeptonToplevel *toplevel,
                           LeptonPageList *pages)
{
  g_return_if_fail (toplevel != NULL);

  toplevel->pages = pages;
}


/*! \brief changes the current page in toplevel
 *  \par Function Description
 *  Changes the current page in \a toplevel to the page \a p_new.
 *
 *  \param toplevel  The LeptonToplevel object
 *  \param p_new     The LeptonPage to go to
 */
void
lepton_toplevel_goto_page (LeptonToplevel *toplevel,
                           LeptonPage *p_new)
{
  gchar *dirname;

  lepton_toplevel_set_page_current (toplevel, p_new);

  dirname = g_path_get_dirname (lepton_page_get_filename (p_new));
  if (chdir (dirname)) {
    /* An error occured with chdir */
    /* FIXME[2017-02-21] Libraries should not be changing the
     * current working directory.  If it is not possible to avoid a
     * chdir() call, then the error needs to be handled and/or
     * reported. */
  }
  g_free (dirname);

}


/*! \brief Search for pages by filename.
 *  \par Function Description
 *  Searches in \a toplevel's list of pages for a page with a filename
 *  equal to \a filename.
 *
 *  \param toplevel  The LeptonToplevel object
 *  \param filename  The filename string to search for
 *
 *  \return LeptonPage pointer to a matching page, NULL otherwise.
 */
LeptonPage*
lepton_toplevel_search_page (LeptonToplevel *toplevel,
                             const gchar *filename)
{
  const GList *iter;
  LeptonPage *page;

  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    page = (LeptonPage *)iter->data;
    /* FIXME this may not be correct on platforms with
     * case-insensitive filesystems. */
    if (strcmp (lepton_page_get_filename (page), filename) == 0)
      return page;
  }
  return NULL;
}


/*! \brief Search for page by its filename's basename.
 *  \par Function Description
 *  Searches in \a toplevel's list of pages for a page with
 *  basename( filename ) equal to \a filename.
 *
 *  \param toplevel  The LeptonToplevel object
 *  \param filename  The filename string to search for
 *
 *  \return LeptonPage pointer to a matching page, NULL otherwise.
 */
LeptonPage*
lepton_toplevel_search_page_by_basename (LeptonToplevel *toplevel,
                                         const gchar *filename)
{
  const GList* iter   = NULL;
  LeptonPage*  page   = NULL;
  LeptonPage*  result = NULL;

  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) )
  {
    page = (LeptonPage*) iter->data;

    const gchar* fname = lepton_page_get_filename (page);
    gchar* bname = g_path_get_basename (fname);

    /* FIXME this may not be correct on platforms with
     * case-insensitive filesystems. */

    if ( strcmp( bname, filename ) == 0 )
    {
      result = page;
      g_free (bname);
      break;
    }

    g_free (bname);
  }

  return result;
}


/*! \brief Search for a page given its page id in a page list.
 *  \par Function Description
 *  This functions returns the page that have the page id \a pid in
 *  the list of pages starting at \a page_list, or NULL if there is no
 *  such page.
 *
 *  \param [in] list      The list of page to search the page in.
 *  \param [in] pid       The ID of the page to find.
 *  \returns A pointer on the page found or NULL if not found.
 */
LeptonPage*
lepton_toplevel_search_page_by_id (LeptonPageList *list,
                                   int pid)
{
  const GList *iter;

  for ( iter = lepton_list_get_glist (list);
        iter != NULL;
        iter = g_list_next (iter) ) {
    LeptonPage *page = (LeptonPage *)iter->data;
    if (page->pid == pid) {
      return page;
    }
  }

  return NULL;
}


/*! \brief Add a weak reference watcher to an LeptonToplevel.
 * \par Function Description
 * Adds the weak reference callback \a notify_func to \a toplevel.  When
 * \a toplevel is destroyed, \a notify_func will be called with two
 * arguments: the \a toplevel, and the \a user_data.
 *
 * \sa s_toplevel_weak_unref
 *
 * \param [in,out] toplevel   Toplevel to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void
s_toplevel_weak_ref (LeptonToplevel *toplevel,
                     void (*notify_func)(void *, void *),
                     void *user_data)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_add (toplevel->weak_refs,
                                       notify_func, user_data);
}

/*! \brief Remove a weak reference watcher from an LeptonToplevel.
 * \par Function Description
 * Removes the weak reference callback \a notify_func from \a toplevel.
 *
 * \sa s_toplevel_weak_ref()
 *
 * \param [in,out] toplevel       Toplevel to weak-reference.
 * \param [in] notify_func    Notify function to search for.
 * \param [in] user_data      Data to to search for.
 */
void
s_toplevel_weak_unref (LeptonToplevel *toplevel,
                       void (*notify_func)(void *, void *),
                       void *user_data)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_remove (toplevel->weak_refs,
                                          notify_func, user_data);
}

/*! \brief Add a weak pointer to an LeptonToplevel.
 * \par Function Description
 * Adds the weak pointer at \a weak_pointer_loc to \a toplevel. The
 * value of \a weak_pointer_loc will be set to NULL when \a toplevel is
 * destroyed.
 *
 * \sa s_toplevel_remove_weak_ptr
 *
 * \param [in,out] toplevel      Toplevel to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
s_toplevel_add_weak_ptr (LeptonToplevel *toplevel,
                         void *weak_pointer_loc)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_add_ptr (toplevel->weak_refs,
                                           (void**) weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an LeptonToplevel.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a toplevel.
 *
 * \sa s_toplevel_add_weak_ptr()
 *
 * \param [in,out] toplevel      Toplevel to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
s_toplevel_remove_weak_ptr (LeptonToplevel *toplevel,
                            void *weak_pointer_loc)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_remove_ptr (toplevel->weak_refs,
                                              (void**) weak_pointer_loc);
}


/*! \brief Print full LeptonToplevel structure.
 *  \par Function Description
 *  This function prints the internal structure of <B>toplevel</B>'s
 *  list of pages.
 *
 *  \param [in] toplevel  The LeptonToplevel object to print.
 */
void
lepton_toplevel_print_all (LeptonToplevel *toplevel)
{
  const GList *iter;
  LeptonPage *page;

  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    page = (LeptonPage *) iter->data;
    printf ("FILENAME: %1$s\n", lepton_page_get_filename (page));
    lepton_object_list_print (page->_object_list);
  }
}


/*! \brief Autosave callback function.
 *  \par Function Description
 *  This function is a callback of the glib g_timeout functions.
 *  It is called every "interval" milliseconds and it sets a flag to save
 *  a backup copy of the opened pages.
 *
 *  \param [in] toplevel  The LeptonToplevel object.
 *  \return The length in milliseconds to set for next interval.
 */
static gint
lepton_toplevel_autosave (LeptonToplevel *toplevel)
{
  const GList *iter;
  LeptonPage *p_current;

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

  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (LeptonPage *) iter->data;

    if (p_current->ops_since_last_backup != 0) {
      /* Real autosave is done in o_undo_savestate */
      p_current->do_autosave_backup = 1;
    }
  }

  return toplevel->auto_save_interval;
}


/*! \brief Autosave initialization function.
 *  \par Function Description
 *  This function sets up the autosave callback function.
 *
 *  \param [in] toplevel  The LeptonToplevel object.
 */
void
lepton_toplevel_init_autosave (LeptonToplevel *toplevel)
{
  if (toplevel->auto_save_interval != 0) {

    /* 1000 converts seconds into milliseconds */
    toplevel->auto_save_timeout =
      g_timeout_add(toplevel->auto_save_interval*1000,
                    (GSourceFunc) lepton_toplevel_autosave,
                    toplevel);
  }
}
