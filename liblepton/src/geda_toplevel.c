/* Lepton EDA library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2017 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
/*! \file geda_toplevel.c
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "libgeda_priv.h"

/*!
 *  \brief Create a TOPLEVEL object
 *  \par Function Description
 *  Create and return an empty TOPLEVEL object with sensible defaults
 *  for its properties.
 *
 *  \returns the newly created TOPLEVEL.
 */
TOPLEVEL *s_toplevel_new (void)
{
  TOPLEVEL *toplevel;

  toplevel = (TOPLEVEL*)g_new (TOPLEVEL, 1);

  toplevel->RC_list = NULL;

  /* These values are the default extents of the schematic drawing area.
   *
   * The negative values allow symbols, residing at the origin, to be
   * edited without translation to other coordinates.
   */
  toplevel->init_left = -60500;
  toplevel->init_top  = -45375;
  toplevel->init_right  = 121000;
  toplevel->init_bottom = 90750;

  toplevel->pages = geda_list_new();
  toplevel->page_current = NULL;

  toplevel->show_hidden_text = 0;

  toplevel->major_changed_refdes = NULL;

  /* The following is an attempt at getting (deterministic) defaults */
  /* for the following variables */
  toplevel->change_notify_funcs = NULL;

  toplevel->load_newer_backup_func = NULL;
  toplevel->load_newer_backup_data = NULL;

  /* Auto-save interval */
  toplevel->auto_save_interval = 0;
  toplevel->auto_save_timeout = 0;

  toplevel->weak_refs = NULL;
  return toplevel;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_toplevel_delete (TOPLEVEL *toplevel)
{
  GList *iter;

  if (toplevel->auto_save_timeout != 0) {
    /* Assume this works */
    g_source_remove (toplevel->auto_save_timeout);
  }

  /* delete all pages */
  s_page_delete_list (toplevel);

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



/*\ brief Set the current page
 *
 *  \param [in,out] toplevel This toplevel
 *  \param [in]     page     The new current page
 */
void
s_toplevel_set_page_current (TOPLEVEL *toplevel, PAGE *page)
{
  g_return_if_fail (toplevel != NULL);

  toplevel->page_current = page;
}


/*! \brief Add a weak reference watcher to an TOPLEVEL.
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
s_toplevel_weak_ref (TOPLEVEL *toplevel,
                     void (*notify_func)(void *, void *),
                     void *user_data)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_add (toplevel->weak_refs,
                                       notify_func, user_data);
}

/*! \brief Remove a weak reference watcher from an TOPLEVEL.
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
s_toplevel_weak_unref (TOPLEVEL *toplevel,
                       void (*notify_func)(void *, void *),
                       void *user_data)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_remove (toplevel->weak_refs,
                                          notify_func, user_data);
}

/*! \brief Add a weak pointer to an TOPLEVEL.
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
s_toplevel_add_weak_ptr (TOPLEVEL *toplevel,
                         void *weak_pointer_loc)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_add_ptr (toplevel->weak_refs,
                                           (void**) weak_pointer_loc);
}

/*! \brief Remove a weak pointer from an TOPLEVEL.
 * \par Function Description
 * Removes the weak pointer at \a weak_pointer_loc from \a toplevel.
 *
 * \sa s_toplevel_add_weak_ptr()
 *
 * \param [in,out] toplevel      Toplevel to weak-reference.
 * \param [in] weak_pointer_loc  Memory address of a pointer.
 */
void
s_toplevel_remove_weak_ptr (TOPLEVEL *toplevel,
                            void *weak_pointer_loc)
{
  g_return_if_fail (toplevel != NULL);
  toplevel->weak_refs = s_weakref_remove_ptr (toplevel->weak_refs,
                                              (void**) weak_pointer_loc);
}
