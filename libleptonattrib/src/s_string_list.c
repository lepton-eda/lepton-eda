/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2013 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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

/*------------------------------------------------------------------*/
/*! \file
 *  \brief Functions involved in manipulating the STRING_LIST
 *         structure.
 *
 * This file holds functions involved in manipulating the STRING_LIST
 * structure.  STRING_LIST is basically a linked list of strings
 * (text).
 *
 * \todo This could be implemented using an underlying GList
 *       structure.  The count parameter could also be eliminated -
 *       either store it in the struct or preferably, calculate it
 *       when needed - I don't think the speed penalty of traversing
 *       the list is significant at all. GDE
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <liblepton/liblepton.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


/*! \brief Get the \a data field of a string list.
 *
 *  \par Function Description
 *
 *  Returns the \a data field of a string list.  It points to a
 *  zero-terminated string.
 *
 *  \param [in] list The string list.
 *  \return The data.
 */
char*
attrib_string_list_get_data (STRING_LIST *list)
{
  return list->data;
}


/*! \brief Set the \a data field of a string list item.
 *
 *  \par Function Description
 *
 *  Sets the \a data field of a string list item to the given
 *  value.
 *
 *  \param [in] list The string list item.
 *  \param [in] data The new data.
 */
void
attrib_string_list_set_data (STRING_LIST *list,
                             char *data)
{
  list->data = data;
}


/*! \brief Get the \a pos field of a string list.
 *
 *  \par Function Description
 *
 *  Returns the \a pos field of a string list.  It is a position
 *  of string list data on spreadsheet.
 *
 *  \param [in] list The string list.
 *  \return The position.
 */
int
attrib_string_list_get_pos (STRING_LIST *list)
{
  return list->pos;
}


/*! \brief Set the \a pos field of a string list.
 *
 *  \par Function Description
 *
 *  Sets the \a pos field of a string list to the given value.  It
 *  is a position of string list data on spreadsheet.
 *
 *  \param [in] list The string list.
 *  \param [in] pos The new position.
 */
void
attrib_string_list_set_pos (STRING_LIST *list,
                            int pos)
{
  list->pos = pos;
}


/*! \brief Get the \a prev field of a string list.
 *
 *  \par Function Description
 *
 *  Returns the \a prev field of a string list.  It is a pointer
 *  to the previous item in the linked list.
 *
 *  \param [in] list The string list.
 *  \return The previous list item.
 */
STRING_LIST*
attrib_string_list_get_prev (STRING_LIST *list)
{
  return list->prev;
}


/*! \brief Set the \a prev field of a string list.
 *
 *  \par Function Description
 *
 *  Sets the \a prev field of a string list.  It is a pointer to
 *  the previous item in the linked list.
 *
 *  \param [in] list The string list.
 *  \param [in] item The new previous list item.
 */
void
attrib_string_list_set_prev (STRING_LIST *list,
                             STRING_LIST *item)
{
  list->prev = item;
}


/*! \brief Get the \a next field of a string list.
 *
 *  \par Function Description
 *
 *  Returns the \a next field of a string list.  It is a pointer
 *  to the next item in the linked list.
 *
 *  \param [in] list The string list.
 *  \return The next list item.
 */
STRING_LIST*
attrib_string_list_get_next (STRING_LIST *list)
{
  return list->next;
}


/*! \brief Set the \a next field of a string list.
 *
 *  \par Function Description
 *
 *  Sets the \a next field of a string list.  It is a pointer to
 *  the next item in the linked list.
 *
 *  \param [in] list The string list.
 *  \param [in] item The new next list item.
 */
void
attrib_string_list_set_next (STRING_LIST *list,
                             STRING_LIST *item)
{
  list->next = item;
}


/*------------------------------------------------------------------*/
/*! \brief Return a pointer to a new STRING_LIST
 *
 * Returns a pointer to a new STRING_LIST struct. This list is empty.
 * \returns pointer to the new STRING_LIST struct.
 */
STRING_LIST *s_string_list_new() {
  STRING_LIST *local_string_list;

  local_string_list = (STRING_LIST*) g_malloc (sizeof (STRING_LIST));
  local_string_list->data = NULL;
  local_string_list->next = NULL;
  local_string_list->prev = NULL;
  local_string_list->pos = -1;   /* can look for this later . . .  */

  return local_string_list;
}


/*------------------------------------------------------------------*/
/*! \brief Add an item to a STRING_LIST
 *
 * Inserts the item into a STRING_LIST.
 * It first passes through the
 * list to make sure that there are no duplications.
 * \param prev pointer to STRING_LIST to be added to.
 * \param count FIXME Don't know what this does - input or output? both?
 * \param item pointer to string to be added
 */
void
s_string_list_add_item (STRING_LIST *prev,
                        int *count,
                        char *item)
{
  STRING_LIST *local_list;

  /* If we are here, it's 'cause we didn't find the item pre-existing in the list. */
  /* In this case, we insert it. */

  local_list = (STRING_LIST *) g_malloc(sizeof(STRING_LIST));  /* allocate space for this list entry */
  /* Copy data into list. */
  attrib_string_list_set_data (local_list, (gchar *) g_strdup (item));
  attrib_string_list_set_next (local_list, NULL);
  /* Point this item to last entry in old list. */
  attrib_string_list_set_prev (local_list, prev);
  /* Make last item in old list point to this one. */
  attrib_string_list_set_next (prev, local_list);
  /* This enumerates the pos on the list.  Value is reset later by
   * sorting. */
  attrib_string_list_set_pos (local_list, *count);
  (*count)++;  /* increment count */
  /*   list = local_list;  */
  return;

}
