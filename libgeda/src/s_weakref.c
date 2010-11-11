/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 2010 gEDA Contributors (see ChangeLog for details)
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

#include "libgeda_priv.h"

/*!
 * \file s_weakref.c
 * \brief Utility functions for weak references and pointers.
 *
 * \warning Do not write code which relies on the order in which weak
 * reference callback functions are notified.
 */

struct WeakRef
{
  void (*notify_func)(void *, void *);
  void *user_data;
};

/*! \brief Notify weak reference watchers that a structure is dead.
 * \par Function Description
 * For each entry in \a weak_refs, call notify function with the dead
 * pointer \a dead_ptr and the entry's specified user data, and free
 * \a weak_refs. Should be called during destruction of an structure
 * that allows weak references.
 *
 * \param [in] dead_ptr       Pointer to structure being destroyed.
 * \param [in,out] weak_refs  List of registered weak references.
 */
void
s_weakref_notify (void *dead_ptr, GList *weak_refs)
{
  GList *iter;
  struct WeakRef *entry;
  for (iter = weak_refs; iter != NULL; iter = g_list_next (iter)) {
    entry = (struct WeakRef *) iter->data;
    if (entry != NULL && entry->notify_func != NULL) {
      entry->notify_func (dead_ptr, entry->user_data);
    }
    g_free (entry);
  }
  g_list_free (weak_refs);
}

/*! \brief Add a weak reference watcher to a weak ref list.
 * \par Function Description
 * Adds the weak reference callback \a notify_func to the weak
 * reference list \a weak_refs, returning the new head of \a
 * weak_refs. \a notify_func will be called with two arguments: a
 * pointer to the object being destroyed, and the \a user_data.
 *
 * \param [in,out] weak_refs  List of registered weak references.
 * \param [in] notify_func    Weak reference notify function.
 * \param [in] user_data      Data to be passed to \a notify_func.
 *
 * \return new head of \a weak_refs list.
 */
GList *
s_weakref_add (GList *weak_refs, void (*notify_func)(void *, void *),
               void *user_data)
{
  struct WeakRef *entry = g_malloc0 (sizeof (struct WeakRef));
  entry->notify_func = notify_func;
  entry->user_data = user_data;
  return g_list_prepend (weak_refs, entry);
}

/*! \brief Remove a weak reference watcher from a weak ref list.
 * \par Function Description
 * Removes a weak reference callback from the weak reference list \a
 * weak_refs, returning the new head of \a weak_refs.
 *
 * \param [in,out] weak_refs    List of registered weak references.
 * \param [in] notify_func      Notify function to search for.
 * \param [in] user_data        User data to search for.
 *
 * \return new head of \a weak_refs list.
 */
GList *
s_weakref_remove (GList *weak_refs, void (*notify_func)(void *, void *),
                  void *user_data)
{
  GList *iter;
  struct WeakRef *entry;
  for (iter = weak_refs; iter != NULL; iter = g_list_next (iter)) {
    entry = iter->data;
    if ((entry != NULL) &&
        (entry->notify_func == notify_func) &&
        (entry->user_data == user_data)) {
      g_free (entry);
      iter->data = NULL;
    }
  }
  return g_list_remove_all (weak_refs, NULL);
}

static void
weak_ptr_notify_func (void *dead_ptr, void *user_data) {
  void **weak_pointer_loc = (void **) user_data;
  if (weak_pointer_loc != NULL) {
    *weak_pointer_loc = NULL;
  }
}

/*! \brief Add a weak pointer to a weak ref list.
 * \par Function Description
 * Adds a weak reference for \a weak_pointer_loc to the weak reference
 * list \a weak_refs, returning the new head of \a weak_refs.
 *
 * \param [in,out] weak_refs     List of registered weak references.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 *
 * \return new head of \a weak_refs list.
 */
GList *
s_weakref_add_ptr (GList *weak_refs, void **weak_pointer_loc)
{
  return s_weakref_add (weak_refs, weak_ptr_notify_func, weak_pointer_loc);
}

/*! \brief Remove a weak pointer from a weak ref list.
 * \par Function Description
 * Removes the weak reference for \a weak_pointer_loc from the weak
 * reference list \a weak_refs, returning the new head of \a
 * weak_refs.
 *
 * \param [in,out] weak_refs     List of registered weak references.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 *
 * \return new head of \a weak_refs list.
 */
GList *
s_weakref_remove_ptr (GList *weak_refs, void **weak_pointer_loc)
{
  return s_weakref_remove (weak_refs, weak_ptr_notify_func, weak_pointer_loc);
}
