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


/*! \brief Create a new #STRING_LIST.
 *
 *  \par Function Description
 *
 *  Allocates and returns a new #STRING_LIST list consisting of
 *  one uninitialized item.  The list must be freed with g_free()
 *  after use.
 *
 *  \return The string list.
 */
STRING_LIST*
attrib_string_list_new ()
{
  return (STRING_LIST*) g_malloc (sizeof (STRING_LIST));
}
