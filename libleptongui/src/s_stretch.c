/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#include <config.h>

#include <stdio.h>

#include "schematic.h"


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GList *s_stretch_add (GList *list, LeptonObject *object, int whichone)
{
  GList *s_iter;
  STRETCH *s_new;

  /* Check if the object is already in the stretch list */
  for (s_iter = list; s_iter != NULL; s_iter = g_list_next (s_iter)) {
    STRETCH *s_current = (STRETCH*) s_iter->data;
    if (lepton_object_get_id (s_current->object) == lepton_object_get_id (object)) {
      return list;
    }
  }

  s_new = (STRETCH*) g_malloc (sizeof (STRETCH));
  s_new->object = object;
  s_new->whichone = whichone;

  return g_list_append (list, s_new);
}


/*! \brief Test if a STRETCH structure points at a given LeptonObject
 *
 *  \brief
 *  \par Function Description
 *  Compares if (STRETCH *)a->object == (LeptonObject *)b
 *
 * \param [in] a  The STRETCH structure
 * \param [in] b  The LeptonObject to test for
 * \returns 0 if STRETCH *a points to LeptonObject *b, otherwise 1.
 */
static gint find_object (gconstpointer a, gconstpointer b)
{
  return (((STRETCH *)a)->object == (LeptonObject *)b) ? 0 : 1;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
GList *s_stretch_remove (GList *list, LeptonObject *object)
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
void s_stretch_destroy_all (GList *list)
{
  g_list_foreach (list, (GFunc)g_free, NULL);
  g_list_free (list);
}
