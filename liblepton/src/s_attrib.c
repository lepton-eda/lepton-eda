/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2025 Lepton EDA Contributors
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
#include <sys/types.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "liblepton_priv.h"

#define MAX_ATTRIBS 128

/*! \brief */
struct st_attrib_names {
  char *attrib_name;
};

/*! \brief */
static int attrib_index=0;

/*! \brief */
/* and eventually make this unlimited */
/* hack hack */
static struct st_attrib_names attrib[MAX_ATTRIBS];

/*! \brief Add an attribute to the list of attributes shown in
 *  dialogs
 *
 *  \par Function Description
 *
 *  The function adds a new attribute to the list of attributes
 *  that will be shown in the "Add attribute" and "Edit attribute"
 *  dialogs in lepton-schematic.  Returns the index of the
 *  attribute in the array of known attributes, or -1 if the
 *  attribute cannot be added, that is, the attribute is NULL or
 *  the array is full and the new index is greater than the
 *  maximum allowed value.
 *
 * \param [in] new_attrib The new attribute string.
 * \return The index in the attribute array, or -1 if the
 *         attribute cannot be added to the array.
 */
int
s_attrib_add_entry (char *new_attrib)
{
  if (new_attrib == NULL)
  {
    return(-1);
  }

  if (attrib_index >= MAX_ATTRIBS)
  {
    return(-1);
  }

  attrib[attrib_index].attrib_name = g_strdup (new_attrib);

  attrib_index++;
  return(attrib_index);
}


/*! \brief Check uniqueness of an attribute
 *
 *  \par Function Description
 *
 *  Check if the attribute \a name is already present in the
 *  current attribute array \a attrib.  Return 1 if it is missing
 *  there, otherwise 0.
 *
 *  \param [in] name The attribute name.
 *  \return TRUE (1) if the attribute is unique, FALSE (0) if it
 *          is duplicate.
 */
int s_attrib_uniq(char *name)
{
  int i;

  for (i = 0; i < attrib_index; i++) {
    if (strcmp(attrib[i].attrib_name, name) == 0) {
      return(0);
    }
  }

  return(1);
}


/*! \brief Free the array of attributes
 *  \par Function Description
 *  Free the \a attrib array on program exit.
 */
void s_attrib_free()
{
  int i;

  for (i = 0; i < attrib_index; i++) {
     g_free(attrib[i].attrib_name);
  }

  attrib_index=0;
}

/*! \brief Perform initialization of array of attributes.
 *  \par Function Description
 *  This function performs initialization of the \a attrib array
 *  by setting all its members to NULL.
 *
 */
void s_attrib_init()
{
  int i;
  for (i = 0; i < MAX_ATTRIBS; i++)
  {
    attrib[i].attrib_name = NULL;
  }
}

/*! \brief Obtain an attrib name by index
 *  \par Function Description
 *  Get the name of an attribute with the given index from the
 *  array \a attrib.
 *
 *  \param [in] counter The attribute index.
 *  \return The attribute string.
 */
char *s_attrib_get(int counter)
{
  if (counter < attrib_index) {
    return(attrib[counter].attrib_name);
  }

  return(NULL);
}
