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
 * \param list pointer to STRING_LIST to be added to.
 * \param count FIXME Don't know what this does - input or output? both?
 * \param item pointer to string to be added
 */
void s_string_list_add_item(STRING_LIST *list, int *count, char *item) {

  gchar *trial_item = NULL;
  STRING_LIST *prev;
  STRING_LIST *local_list;

  if (list == NULL) {
    fprintf (stderr, "s_string_list_add_item: ");
    fprintf (stderr, _("Tried to add to a NULL list.\n"));
    return;
  }

  /* First check to see if list is empty.  Handle insertion of first item
     into empty list separately.  (Is this necessary?) */
  if (list->data == NULL) {
    g_debug ("s_string_list_add_item: "
             "About to place first item in list.\n");
    list->data = (gchar *) g_strdup(item);
    list->next = NULL;
    list->prev = NULL;  /* this may have already been initialized. . . . */
    list->pos = *count; /* This enumerates the pos on the list.  Value is reset later by sorting. */
    (*count)++;  /* increment count to 1 */
    return;
  }

  /* Otherwise, loop through list looking for duplicates */
  prev = list;
  while (list != NULL) {
    trial_item = (gchar *) g_strdup(list->data);
    if (strcmp(trial_item, item) == 0) {
      /* Found item already in list.  Just return. */
      g_free(trial_item);
      return;
    }
    g_free(trial_item);
    prev = list;
    list = list->next;
  }

  /* If we are here, it's 'cause we didn't find the item pre-existing in the list. */
  /* In this case, we insert it. */

  local_list = (STRING_LIST *) g_malloc(sizeof(STRING_LIST));  /* allocate space for this list entry */
  local_list->data = (gchar *) g_strdup(item);   /* copy data into list */
  local_list->next = NULL;
  local_list->prev = prev;  /* point this item to last entry in old list */
  prev->next = local_list;  /* make last item in old list point to this one. */
  local_list->pos = *count; /* This enumerates the pos on the list.  Value is reset later by sorting. */
  (*count)++;  /* increment count */
  /*   list = local_list;  */
  return;

}


void
attrib_string_list_delete_found_item (STRING_LIST **list,
                                      int *count,
                                      STRING_LIST *list_item,
                                      STRING_LIST *prev_item,
                                      STRING_LIST *next_item)
{
  if (next_item == NULL && prev_item != NULL)
  {
    /* at list's end */
    attrib_string_list_set_next (prev_item, NULL);
  }
  else if (next_item != NULL && prev_item == NULL)
  {
    /* at list's beginning */
    attrib_string_list_set_prev (next_item, NULL);
    /* also need to fix pointer to list head */
    (*list) = next_item;
    /*  g_free(list);  */
  }
  else
  {
    /* normal case of element in middle of list */
    attrib_string_list_set_next (prev_item, next_item);
    attrib_string_list_set_prev (next_item, prev_item);
  }
}
