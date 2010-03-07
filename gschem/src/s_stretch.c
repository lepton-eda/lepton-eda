/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GList *s_stretch_add (GList *list, OBJECT *object, int whichone)
{
  GList *s_iter;
  STRETCH *s_new;

  /* Check if the object is already in the stretch list */
  for (s_iter = list; s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *s_current = s_iter->data;
    if (s_current->object->sid == object->sid) {
      return list;
    }
  }

  s_new = g_malloc (sizeof (STRETCH));
  s_new->object = object;
  s_new->whichone = whichone;

  return g_list_append (list, s_new);
}


/*! \brief Test if a STRETCH structure points at a given OBJECT
 *
 *  \brief
 *  \par Function Description
 *  Compares if (STRETCH *)a->object == (OBJECT *)b
 *
 * \param [in] a  The STRETCH structure
 * \param [in] b  The OBJECT to test for
 * \returns 0 if STRETCH *a points to OBJECT *b, otherwise 1.
 */
static gint find_object (gconstpointer a, gconstpointer b)
{
  return (((STRETCH *)a)->object == (OBJECT *)b) ? 0 : 1;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GList *s_stretch_remove (GList *list, OBJECT *object)
{
  GList *item;

  g_return_val_if_fail (object != NULL, list);

  item = g_list_find_custom (list, object, find_object);
  g_free (item->data);

  return g_list_delete_link (list, item);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_stretch_print_all (GList *list)
{
  GList *iter;

  printf("START printing stretch ********************\n");
  for (iter = list; iter != NULL; iter = g_list_next (iter)) {
    STRETCH *s_current = iter->data;

    if (s_current->object) {
      printf("Object: %s\n", s_current->object->name);
    } else {
      printf("Object is NULL\n");
    }

    printf("which one: %d\n", s_current->whichone);
  }
  printf("DONE printing stretch ********************\n\n");
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_stretch_destroy_all (GList *list)
{
  g_list_foreach (list, (GFunc)g_free, NULL);
  g_list_free (list);
}
