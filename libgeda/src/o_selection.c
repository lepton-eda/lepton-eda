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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
#include <config.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Returns a pointer to a new SELECTION object.
 *  \par Returns a pointer to a new SELECTION object.
 *  \return pointer to the new SELECTION object.
 */
SELECTION *o_selection_new( void )
{
  return (SELECTION*)geda_list_new();
}

/*! \brief Selects the given object and adds it to the selection list
 *  \par Selects the given object and does the needed work to make the
 *  object visually selected.
 *  Skip objects that are already selected.
 *
 *  \param [in] toplevel   The TOPLEVEL object
 *  \param [in] selection  Pointer to the selection list
 *  \param [in] o_selected Object to select.
 */
void o_selection_add (TOPLEVEL *toplevel, SELECTION *selection, OBJECT *o_selected)
{
  if (o_selected->selected == FALSE)
  {
    o_selection_select (toplevel, o_selected);
    geda_list_add( (GedaList *)selection, o_selected );
  }
}

/*! \brief Removes the given object from the selection list
 *  \par Removes the given object from the selection list and does the 
 *  needed work to make the object visually unselected.
 *  It's ok to call this function with an object which is not necessarily
 *  selected.
 *
 *  \param [in] toplevel   The TOPLEVEL object
 *  \param [in] selection  Pointer to the selection list
 *  \param [in] o_selected Object to unselect and remove from the list.
 */
void o_selection_remove (TOPLEVEL *toplevel, SELECTION *selection, OBJECT *o_selected)
{
  if (o_selected == NULL) {
    fprintf(stderr, "Got NULL for o_selected in o_selection_remove\n");
    return;
  }

  if (g_list_find( geda_list_get_glist( selection ), o_selected ) != NULL) {
    o_selection_unselect (toplevel, o_selected);
    geda_list_remove( (GedaList *)selection, o_selected );
  }
}


/*! \brief Prints the given selection list.
 *  \par Prints the given selection list.
 *  \param [in] selection Pointer to selection list to print.
 *
 */
void o_selection_print_all(const SELECTION *selection)
{
  const GList *s_current;

  s_current = geda_list_get_glist( selection );

  printf("START printing selection ********************\n");
  while(s_current != NULL) {
    if (s_current->data) {
      printf("Selected object: %d\n", ((OBJECT *)s_current->data)->sid );
    }
    s_current = g_list_next( s_current );
  }
  printf("DONE printing selection ********************\n");
  printf("\n");
}

/*! \brief Selects the given object.
 *  \par Sets the select flag, saves the color, and then selects the 
 *  given object
 *
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] object    Object to select.
 */
void o_selection_select(TOPLEVEL *toplevel, OBJECT *object)
{
  if (object->selected == TRUE) {
    printf("object already selected == TRUE\n");
    return;
  }

  o_emit_pre_change_notify (toplevel, object);
  object->selected = TRUE;
  o_emit_change_notify (toplevel, object);
}

/*! \brief Unselects the given object.
 *  \par Unsets the select flag, restores the original color of the
 *  given object.
 *  This function should not be called by anybody outside of this file.
 *
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] object    Object to unselect.
 */
void o_selection_unselect (TOPLEVEL *toplevel, OBJECT *object)
{
  o_emit_pre_change_notify (toplevel, object);
  object->selected = FALSE;
  o_emit_change_notify (toplevel, object);
}

