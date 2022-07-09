/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
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

/*! \file page.c
 *  \brief The page system
 *
 *  liblepton can handle multiple schematic or symbol
 *  pages. liblepton keeps track of the currently opened pages
 *  with a managed _LeptonList.  The currently used page is
 *  refered with an extra pointer.
 *
 *  Each page carries a list of the objects that are on the page.
 *  The first and the last element are referenced by the head and tail
 *  pointers.
 *
 *  \image html s_page_overview.png
 *  \image latex s_page_overview.pdf "page overview" width=14cm
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

#include "liblepton_priv.h"

static gint global_pid = 0;

/*! \brief Get page's CHANGED flag value.
 *
 *  \param [in] page The page to obtain the flag of.
 *  \return The value of the CHANGED flag.
 */
int
lepton_page_get_changed (LeptonPage *page)
{
  g_return_val_if_fail (page != NULL, 0);

  return page->CHANGED;
}

/*! \brief Set page's CHANGED flag value.
 *
 *  \param [in] page    The page.
 *  \param [in] changed The new CHANGED value.
 */
void
lepton_page_set_changed (LeptonPage *page,
                         int changed)
{
  g_return_if_fail (page != NULL);

  page->CHANGED = changed;
}


/*! \brief Get page's \a page_control field value.
 *
 *  \param [in] page The page to obtain the field of.
 *  \return The value of the \a page_control field.
 */
int
lepton_page_get_page_control (LeptonPage *page)
{
  g_return_val_if_fail (page != NULL, 0);

  return page->page_control;
}

/*! \brief Set page's \a page_control field value.
 *
 *  \param [in] page The page.
 *  \param [in] page_control The new \a page_control value.
 */
void
lepton_page_set_page_control (LeptonPage *page,
                              int page_control)
{
  g_return_if_fail (page != NULL);

  page->page_control = page_control;
}


/*! \brief Get page's \a place_list field value.
 *
 *  \param [in] page The page to obtain the field of.
 *  \return The value of the \a place_list field.
 */
GList*
lepton_page_get_place_list (LeptonPage *page)
{
  g_return_val_if_fail (page != NULL, NULL);

  return page->place_list;
}

/*! \brief Set page's \a place_list field value.
 *
 *  \param [in] page The page.
 *  \param [in] place_list The new \a place_list value.
 */
void
lepton_page_set_place_list (LeptonPage *page,
                            GList *place_list)
{
  g_return_if_fail (page != NULL);

  page->place_list = place_list;
}


/*! \brief Get page's \a selection_list field value.
 *
 *  \param [in] page The page to obtain the field of.
 *  \return The value of the \a selection_list field.
 */
LeptonSelection*
lepton_page_get_selection_list (LeptonPage *page)
{
  g_return_val_if_fail (page != NULL, NULL);

  return page->selection_list;
}

/*! \brief Set page's \a selection_list field value.
 *
 *  \param [in] page The page.
 *  \param [in] selection_list The new \a selection_list value.
 */
void
lepton_page_set_selection_list (LeptonPage *page,
                                LeptonSelection *selection_list)
{
  g_return_if_fail (page != NULL);

  page->selection_list = selection_list;
}


/* Called just before removing an LeptonObject from a LeptonPage
 * or after appending an LeptonObject to a LeptonPage. */
static void
object_added (LeptonPage *page,
              LeptonObject *object)
{
  /* Set up object parent pointer */
#ifndef NDEBUG
  if (object->page != NULL) {
    g_critical ("Object %1$p already has parent page %2$p!", object, object->page);
  }
#endif
  object->page = page;

  /* Update object connection tracking */
  s_conn_update_object (page, object);

  lepton_object_emit_change_notify (object);
}

/* Called just before removing an LeptonObject from a LeptonPage. */
static void
pre_object_removed (LeptonPage *page,
                    LeptonObject *object)
{
  lepton_object_emit_pre_change_notify (object);

  /* Remove object from the list of connectible objects */
  s_conn_remove_object (page, object);

  /* Clear object parent pointer */
#ifndef NDEBUG
  if (object->page == NULL) {
    g_critical ("Object %1$p has NULL parent page!", object);
  }
#endif
  object->page = NULL;

  /* Clear page's object_lastplace pointer if set */
  if (page->object_lastplace == object) {
    page->object_lastplace = NULL;
  }

  /* Remove object from connection system */
  s_conn_remove_object_connections (object);
}

/*! \brief create a new page object
 *  \par Function Description
 *  Creates a new page and add it to <B>toplevel</B>'s list of pages.
 *
 *  It initializes the #LeptonPage structure and set its
 *  <B>page_filename</B> to <B>filename</B>. <B>toplevel</B>'s
 *  current page is not changed by this function.
 */
LeptonPage*
lepton_page_new (LeptonToplevel *toplevel,
                 const gchar *filename)
{
  g_return_val_if_fail (toplevel, NULL);
  g_return_val_if_fail (filename, NULL);

  LeptonPage *page;

  /* Now create a blank page */
  page = (LeptonPage*)g_new0 (LeptonPage, 1);

  page->pid = global_pid++;

  lepton_page_set_changed (page, 0);

  /* big assumption here that page_filename isn't null */
  lepton_page_set_filename (page, filename);

  page->up = -2;
  page->page_control = 0;

  /* Init connectible objects array */
  page->connectible_list = NULL;

  /* Init the object list */
  page->_object_list = NULL;

  /* new selection mechanism */
  lepton_page_set_selection_list (page, o_selection_new());

  lepton_page_set_place_list (page, NULL);

  /* init undo struct pointers */
  s_undo_init(page);

  page->object_lastplace = NULL;

  page->weak_refs = NULL;

  /* Backup variables */
  page->ops_since_last_backup = 0;
  page->saved_since_first_loaded = 0;
  page->do_autosave_backup = 0;

  page->major_changed_refdes = NULL;

  /* now append page to page list of toplevel */
  lepton_list_add( toplevel->pages, page );
  page->toplevel = toplevel;

  return page;
}

/*! \brief delete a page and it's contents
 *  \par Function Description
 *  Deletes a single page <B>page</B> from <B>toplevel</B>'s list of pages.
 *
 *  If the current page of toplevel is given as parameter <B>page</B>,
 *  the function sets the field <B>page_current</B> of the LeptonToplevel
 *  struct to NULL.
 */
void
lepton_page_delete (LeptonToplevel *toplevel,
                    LeptonPage *page)
{
  LeptonPage *tmp;
  gchar *backup_filename;
  gchar *real_filename;

  /* We need to temporarily make the page being deleted current because
   * various functions called below (some indirectly) assume they are
   * deleting objects from the current page.
   *
   * These functions are known to include:
   *   lepton_object_delete ()
   */

  /* save page_current and switch to page */
  if (page == lepton_toplevel_get_page_current (toplevel))
  {
    tmp = NULL;
  }
  else
  {
    tmp = lepton_toplevel_get_page_current (toplevel);
    lepton_toplevel_goto_page (toplevel, page);
  }

  /* Get the real filename and file permissions */
  real_filename = follow_symlinks (lepton_page_get_filename(page), NULL);

  if (real_filename == NULL) {
    g_message ("lepton_page_delete:");
    g_message (_("Can't get the real filename of %1$s."),
               lepton_page_get_filename (page));
  }
  else {
    backup_filename = f_get_autosave_filename (real_filename);

    /* Delete the backup file */
    if ( (g_file_test (backup_filename, G_FILE_TEST_EXISTS)) &&
         (!g_file_test(backup_filename, G_FILE_TEST_IS_DIR)) )
    {
      if (unlink(backup_filename) != 0) {
        g_message ("lepton_page_delete:");
        g_message (_("Unable to delete backup file %1$s."),
                   backup_filename);
      }
    }
    g_free (backup_filename);
  }
  g_free(real_filename);

  /* Free the selection object */
  g_object_unref( page->selection_list );

  /* then delete objects of page */
  lepton_page_delete_objects (page);

  /* Free the objects in the place list. */
  lepton_object_list_delete (page->place_list);
  page->place_list = NULL;

  /*  This removes all objects from the list of connectible objects
   *  of the given \a page. */
  if (g_list_length (page->connectible_list) != 0) {
    fprintf (stderr,
            "OOPS! page->connectible_list had something in it when it was freed!\n");
    fprintf (stderr, "Length: %1$d\n", g_list_length (page->connectible_list));
  }

  g_list_free (page->connectible_list);
  page->connectible_list = NULL;

  /* free current page undo structs */
  s_undo_free_all (page);

  /* ouch, deal with parents going away and the children still around */
  page->up = -2;
  g_free (page->_filename);

  lepton_list_remove( toplevel->pages, page );

  page->weak_refs = s_weakref_notify (page, page->weak_refs);


  if (page->major_changed_refdes)
  {
    g_list_free_full (page->major_changed_refdes, &g_free);
  }


  g_free (page);

  /* restore page_current */
  if (tmp != NULL) {
    lepton_toplevel_goto_page (toplevel, tmp);
  } else {
    /* page was page_current */
    lepton_toplevel_set_page_current (toplevel, NULL);
    /* page_current must be updated by calling function */
  }

}


/*! \brief Add a weak reference watcher to an LeptonPage.
 * \par Function Description
 * Adds the weak reference callback \a notify_func to \a page.  When
 * \a page is destroyed, \a notify_func will be called with two
 * arguments: the \a page, and the \a user_data.
 *
 * \sa lepton_page_weak_unref
 *
 * \param [in,out] page       Page to weak-reference.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 */
void
lepton_page_weak_ref (LeptonPage *page,
                      void (*notify_func)(void *, void *),
                      void *user_data)
{
  g_return_if_fail (page != NULL);
  page->weak_refs = s_weakref_add (page->weak_refs, notify_func, user_data);
}

/*! \brief Remove a weak reference watcher from an LeptonPage.
 * \par Function Description
 * Removes the weak reference callback \a notify_func from \a page.
 *
 * \sa lepton_page_weak_ref()
 *
 * \param [in,out] page       Page to weak-reference.
 * \param [in] notify_func    Notify function to search for.
 * \param [in] user_data      Data to to search for.
 */
void
lepton_page_weak_unref (LeptonPage *page,
                        void (*notify_func)(void *, void *),
                        void *user_data)
{
  g_return_if_fail (page != NULL);
  page->weak_refs = s_weakref_remove (page->weak_refs,
                                      notify_func, user_data);
}

/*! \brief Add a weak pointer to an LeptonPage.
 * \par Function Description
 * Adds the weak pointer at \a weak_pointer_loc to \a page. The
 * value of \a weak_pointer_loc will be set to NULL when \a page is
 * destroyed.
 *
 * \sa lepton_page_remove_weak_ptr
 *
 * \param [in,out] page          Page to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
lepton_page_add_weak_ptr (LeptonPage *page,
                          void *weak_pointer_loc)
{
  g_return_if_fail (page != NULL);
  page->weak_refs = s_weakref_add_ptr (page->weak_refs,
                                       (void**) weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an LeptonPage.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a page.
 *
 * \sa lepton_page_add_weak_ptr()
 *
 * \param [in,out] page          Page to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
lepton_page_remove_weak_ptr (LeptonPage *page,
                             void *weak_pointer_loc)
{
  g_return_if_fail (page != NULL);
  page->weak_refs = s_weakref_remove_ptr (page->weak_refs,
                                          (void**) weak_pointer_loc);
}


/*! \brief Append an LeptonObject to the LeptonPage
 *
 *  \par Function Description
 *  Links the passed LeptonObject to the end of the LeptonPage's
 *  linked list of objects.
 *
 *  \param [in] page      The LeptonPage the object is being added to.
 *  \param [in] object    The LeptonObject being added to the page.
 */
void
lepton_page_append (LeptonPage *page,
                    LeptonObject *object)
{
  page->_object_list = g_list_append (page->_object_list, object);
  object_added (page, object);
}

/*! \brief Append a GList of LeptonObjects to the LeptonPage
 *
 *  \par Function Description
 *  Links the passed LeptonObject GList to the end of the LeptonPage's
 *  object_list.
 *
 *  \param [in] page      The LeptonPage the objects are being added to.
 *  \param [in] obj_list  The LeptonObject list being added to the page.
 */
void
lepton_page_append_list (LeptonPage *page,
                         GList *obj_list)
{
  GList *iter;
  page->_object_list = g_list_concat (page->_object_list, obj_list);
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    object_added (page, (LeptonObject*) iter->data);
  }
}

/*! \brief Remove an LeptonObject from the LeptonPage
 *
 *  \par Function Description
 *  Removes the passed LeptonObject from the LeptonPage's
 *  linked list of objects.
 *
 *  \param [in] page      The LeptonPage the object is being removed from.
 *  \param [in] object    The LeptonObject being removed from the page.
 */
void
lepton_page_remove (LeptonPage *page,
                    LeptonObject *object)
{
  pre_object_removed (page, object);
  page->_object_list = g_list_remove (page->_object_list, object);
}

/*! \brief Replace an LeptonObject in a LeptonPage, in the same list position.
 *
 * \par Function Description
 * Removes \a object1 from \a page's linked list of objects, and puts
 * \a object2 in the position thus vacated. If \a object1 is not in \a
 * page, object2 is appended to \a page.
 *
 * \param [in] page      The LeptonPage to be modified.
 * \param [in] object1   The LeptonObject being removed from the page.
 * \param [in] object2   The LeptonObject being added to the page.
 */
void
lepton_page_replace (LeptonPage *page,
                     LeptonObject *object1,
                     LeptonObject *object2)
{
  GList *iter = g_list_find (page->_object_list, object1);

  /* If object1 not found, append object2 */
  if (iter == NULL) {
    lepton_page_append (page, object2);
    return;
  }

  pre_object_removed (page, object1);
  iter->data = object2;
  object_added (page, object2);
}

/*! \brief Remove and free all LeptonObjects from the LeptonPage
 *
 *  \param [in] page      The LeptonPage being cleared.
 */
void
lepton_page_delete_objects (LeptonPage *page)
{
  GList *objects = page->_object_list;
  GList *iter;
  for (iter = objects; iter != NULL; iter = g_list_next (iter)) {
    pre_object_removed (page, (LeptonObject*) iter->data);
  }
  page->_object_list = NULL;
  lepton_object_list_delete (objects);
}


/*! \brief Return a GList of LeptonObjects on the LeptonPage
 *
 *  \par Function Description
 *  An accessor for the LeptonPage's GList of objects.
 *
 *  NB: This GList is owned by the LeptonPage, and must not be
 *      free'd or modified by the caller.
 *
 *  \param [in] page      The LeptonPage to get objects on.
 *  \returns a const pointer to the LeptonPage's GList of objects
 */
const GList*
lepton_page_objects (LeptonPage *page)
{
  return page->_object_list;
}


/*! \brief Find the objects in a given region
 *
 *  \par Function Description
 *  Finds the objects which are inside, or intersect
 *  the passed box shaped region.
 *
 *  \param [in] page      The LeptonPage to find objects on.
 *  \param [in] rects     The LeptonBox regions to check.
 *  \param [in] n_rects   The number of regions.
 *  \param [in] include_hidden Calculate bounds of hidden objects.
 *  \return The GList of LeptonObjects in the region.
 */
GList*
lepton_page_objects_in_regions (LeptonPage *page,
                                LeptonBox *rects,
                                int n_rects,
                                gboolean include_hidden)
{
  GList *iter;
  GList *list = NULL;
  int i;

  for (iter = page->_object_list; iter != NULL; iter = g_list_next (iter)) {
    LeptonObject *object = (LeptonObject*) iter->data;
    int left, top, right, bottom;
    int visible;

    visible = lepton_object_calculate_visible_bounds (object,
                                                      include_hidden,
                                                      &left,
                                                      &top,
                                                      &right,
                                                      &bottom);
    if (visible) {
      for (i = 0; i < n_rects; i++) {
        if (right  >= rects[i].lower_x &&
            left   <= rects[i].upper_x &&
            top    <= rects[i].upper_y &&
            bottom >= rects[i].lower_y) {
          list = g_list_prepend (list, object);
          break;
        }
      }
    }
  }

  list = g_list_reverse (list);
  return list;
}

/*! \brief Get the file path associated with a page
 * \par Function Description
 * Retrieve the filename associated with \a page.  The returned string
 * remains owned by \a page and must not be modified or freed.
 *
 * \param page  A #LeptonPage pointer
 * \return the full path to the underlying file of the \a page
 */
const gchar *
lepton_page_get_filename (const LeptonPage *page)
{
  g_return_val_if_fail (page, "");
  g_return_val_if_fail (page->_filename, "");

  return page->_filename;
}

/*! \brief Set the file path associated with a page
 * \par Function Description
 * Set the path and filename associated with \a page.  If \a filename
 * is relative, it will be made absolute relative the current working
 * directory.
 *
 * \param page      The #LeptonPage for which to set the filename
 * \param filename  The new file path for \a page
 */
void
lepton_page_set_filename (LeptonPage *page,
                          const char *filename)
{
  g_return_if_fail (page);
  g_return_if_fail (filename);

  GFile *file = g_file_new_for_path (filename);
  gchar *absolute = g_file_get_path (file);
  g_object_unref (file);

  g_return_if_fail (absolute);

  g_free (page->_filename);
  page->_filename = absolute;
}


/*! \brief Get GList of pages from a #LeptonPageList list.
 *
 *  \param [in] page_list The #LeptonPageList object.
 *  \return The pointer to GList of pages.
 */
GList*
lepton_page_list_get_glist (LeptonPageList *page_list)
{
  g_return_val_if_fail (page_list != NULL, NULL);

  return lepton_list_get_glist (page_list);
}
