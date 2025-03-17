/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
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
/*! \file undo.c
 */

#include <config.h>

#include <stdio.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "liblepton_priv.h"


/*! \brief Get undo structure's \a filename field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a filename field.
 */
char*
lepton_undo_get_filename (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, NULL);

  return undo->filename;
}

/*! \brief Set undo structure's \a filename field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] filename The new value of the \a filename field.
 */
void
lepton_undo_set_filename (LeptonUndo *undo,
                          const char *filename)
{
  g_return_if_fail (undo);

  g_free (undo->filename);
  undo->filename = g_strdup (filename);
}


/*! \brief Get undo structure's \a object_list field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a object_list field.
 */
GList*
lepton_undo_get_object_list (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, NULL);

  return undo->object_list;
}

/*! \brief Set undo structure's \a object_list field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] object_list The new value of the \a object_list field.
 */
void
lepton_undo_set_object_list (LeptonUndo *undo,
                             GList *object_list)
{
  g_return_if_fail (undo != NULL);
  undo->object_list = object_list;
}


/*! \brief Get undo structure's \a next field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a next field.
 */
LeptonUndo*
lepton_undo_get_next (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, NULL);

  return undo->next;
}

/*! \brief Set undo structure's \a next field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] next The new value of the \a next field.
 */
void
lepton_undo_set_next (LeptonUndo *undo,
                      LeptonUndo *next)
{
  g_return_if_fail (undo != NULL);

  undo->next = next;
}


/*! \brief Get undo structure's \a page_control field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a page_control field.
 */
int
lepton_undo_get_page_control (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, 0);

  return undo->page_control;
}

/*! \brief Set undo structure's \a page_control field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] page_control The new value of the \a page_control field.
 */
void
lepton_undo_set_page_control (LeptonUndo *undo,
                              int page_control)
{
  g_return_if_fail (undo != NULL);

  undo->page_control = page_control;
}


/*! \brief Get undo structure's \a prev field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a prev field.
 */
LeptonUndo*
lepton_undo_get_prev (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, NULL);

  return undo->prev;
}

/*! \brief Set undo structure's \a prev field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] prev The new value of the \a prev field.
 */
void
lepton_undo_set_prev (LeptonUndo *undo,
                      LeptonUndo *prev)
{
  g_return_if_fail (undo != NULL);

  undo->prev = prev;
}


/*! \brief Get undo structure's \a scale field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a scale field.
 */
double
lepton_undo_get_scale (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, 0);

  return undo->scale;
}

/*! \brief Set undo structure's \a scale field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] scale The new value of the \a scale field.
 */
void
lepton_undo_set_scale (LeptonUndo *undo,
                       double scale)
{
  g_return_if_fail (undo != NULL);

  undo->scale = scale;
}


/*! \brief Get undo structure's \a type field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a type field.
 */
int
lepton_undo_get_type (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, FALSE);

  return undo->type;
}


/*! \brief Set undo structure's \a type field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] type The new value of the \a type field.
 */
void
lepton_undo_set_type (LeptonUndo *undo,
                      int type)
{
  g_return_if_fail (undo != NULL);

  undo->type = type;
}


/*! \brief Get undo structure's \a up field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a up field.
 */
int
lepton_undo_get_up (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, 0);

  return undo->up;
}

/*! \brief Set undo structure's \a up field value.
 *
 *  \param [in,out] undo The #LeptonUndo structure to set the field of.
 *  \param [in] up The new value of the \a up field.
 */
void
lepton_undo_set_up (LeptonUndo *undo,
                    int up)
{
  g_return_if_fail (undo != NULL);

  undo->up = up;
}


/*! \brief Get undo structure's \a x field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a x field.
 */
int
lepton_undo_get_x (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, 0);

  return undo->x;
}


/*! \brief Set undo structure's \a x field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] x The new value of the \a x field.
 */
void
lepton_undo_set_x (LeptonUndo *undo,
                   int x)
{
  g_return_if_fail (undo != NULL);

  undo->x = x;
}


/*! \brief Get undo structure's \a y field value.
 *
 *  \param [in] undo The #LeptonUndo structure to obtain the field of.
 *  \return The value of the \a y field.
 */
int
lepton_undo_get_y (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, 0);

  return undo->y;
}


/*! \brief Set undo structure's \a y field value.
 *
 *  \param [in] undo The #LeptonUndo structure to set the field of.
 *  \param [in] y The new value of the \a y field.
 */
void
lepton_undo_set_y (LeptonUndo *undo,
                   int y)
{
  g_return_if_fail (undo != NULL);

  undo->y = y;
}


/*! \brief Obtain the last element of an undo list
 *  \par Function Description
 *  Iterate over the elements of the given #LeptonUndo list and
 *  return its last element.
 *  \param [in] head The undo list.
 *  \return The last element of the undo list.
 */
LeptonUndo*
lepton_undo_return_tail (LeptonUndo *head)
{
  LeptonUndo *u_current=NULL;
  LeptonUndo *ret_struct=NULL;

  u_current = head;
  while ( u_current != NULL ) { /* goto end of list */
    ret_struct = u_current;
    u_current = u_current->next;
  }

  return(ret_struct);
}


/*! \brief Add a new undo element into an undo list
 *
 *  \par Function Description
 *
 *  Create a new undo element initializing it with given
 *  parameters and add it to the end of the undo list \a head.
 *
 *  \param [in,out] head The head of the undo list.
 *  \param [in] type The type of info to save, either only info on
 *                   the current viewport, or the complete info.
 *  \param [in] filename The name of the file to save the undo
 *                       information to.
 *  \param [in] object_list The list of objects if the undo
 *                          element is saved in memory.
 *  \param [in] x The X coordinate of the viewport center.
 *  \param [in] y The Y coordinate of the viewport center.
 *  \param [in] scale The scale factor of the viewport.
 *  \param [in] page_control The page hierarchical level for
 *                           moving around in the hierarchy.
 *  \param [in] up The page ID of the parent page in the
 *                 hierarchy.
 */
LeptonUndo*
lepton_undo_add (LeptonUndo *head,
                 int type,
                 char *filename,
                 GList *object_list,
                 int x,
                 int y,
                 double scale,
                 int page_control,
                 int up)
{
  LeptonUndo *tail;
  LeptonUndo *u_new;

  u_new = (LeptonUndo *) g_malloc (sizeof (LeptonUndo));

  u_new->filename = g_strdup (filename);

  u_new->object_list = object_list;

  u_new->type = type;

  u_new->x = x;
  u_new->y = y;
  u_new->scale = scale;

  u_new->page_control = page_control;
  u_new->up = up;

  if (head == NULL) {
    u_new->prev = NULL; /* setup previous link */
    u_new->next = NULL;
    return(u_new);
  } else {
    tail = lepton_undo_return_tail (head);
    u_new->prev = tail; /* setup previous link */
    u_new->next = NULL;
    tail->next = u_new;
    return(tail->next);
  }
}

/*! \brief Debug print the last undo information
 *  \par Function Description
 *  Print the last stored undo information:
 *  - the undo filename or the list of objects if the undo
 *    information is stored in memory,
 *  - the undo viewport info,
 *  - the current number of undo levels.
 *
 *  \param [in] head The head of the current undo list.
 */
void
lepton_undo_print_all (LeptonUndo *head)
{
  LeptonUndo *u_current;

  u_current = head;

  g_debug ("START printing undo ********************\n");
  g_debug ("BOTTOM\n");
  while(u_current != NULL) {

    if (lepton_undo_get_filename (u_current))
    {
      g_debug ("%s\n", lepton_undo_get_filename (u_current));
    }

    if (lepton_undo_get_object_list (u_current))
    {
      lepton_object_list_print (lepton_undo_get_object_list (u_current));
    }

    g_debug ("\t%d %d %f\n",
            lepton_undo_get_x (u_current),
            lepton_undo_get_y (u_current),
            lepton_undo_get_scale (u_current));
    u_current = lepton_undo_get_next (u_current);
  }
  g_debug ("TOS\n");
  g_debug ("Number of levels: %d\n", lepton_undo_levels (head));
  g_debug ("DONE printing undo ********************\n");
  g_debug ("\n");

}

/*! \brief Destroy all undo information from undo list
 *  \par Function Description
 *  Destroy all undo information from the given undo list freeing
 *  either the filenames affected or the object lists.
 *
 * \param [in] head The undo list to destroy.
 */
void
lepton_undo_destroy_all (LeptonUndo *head)
{
  LeptonUndo *u_current;
  LeptonUndo *u_prev;

  u_current = lepton_undo_return_tail (head);

  while (u_current != NULL) {
    u_prev = u_current->prev;
    g_free(u_current->filename);

    if (u_current->object_list) {
      lepton_object_list_delete (u_current->object_list);
      u_current->object_list = NULL;
    }

    g_free(u_current);
    u_current = u_prev;
  }
}


/*! \brief Remove all undo information from undo list from some
 *         point in the past
 *  \par Function Description
 *  Remove all recent undo information from the undo list starting
 *  with the item \a head.  This is usually done when the undo
 *  information is no longer needed after several redo commands
 *  when a new undo history is started.  If the undo information
 *  is stored in temporary files, the affected files are unlinked.
 *  If the undo information is stored in memory, the affected
 *  object lists are freed.
 *
 * \param [in] head The pointer to the first undo item to remove.
 */
void
lepton_undo_remove_rest (LeptonUndo *head)
{
  LeptonUndo *u_current;
  LeptonUndo *u_next;

  u_current = head;

  while (u_current != NULL) {
    u_next = u_current->next;

    if (u_current->filename) {
      unlink(u_current->filename);
      g_free(u_current->filename);
    }

    if (u_current->object_list) {
      lepton_object_list_delete (u_current->object_list);
      u_current->object_list = NULL;
    }

    g_free(u_current);
    u_current = u_next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int
lepton_undo_levels (LeptonUndo *head)
{
  LeptonUndo *u_current;
  int count = 0;

  u_current = head;
  while (u_current != NULL) {
    if (u_current->filename || u_current->object_list) {
      count++;
    }

    u_current = u_current->next;
  }

  return(count);
}


/*! \brief Initialize the undo list pointers
 *
 *  \par Function Description
 *
 *  Initialize the top, bottom, and current pointer of the undo
 *  list by setting them to NULL.
 *
 *  \param [in,out] p_current The page to initialize the undo list
 *                            pointers for.
 */
void
lepton_undo_init (LeptonPage *p_current)
{
  p_current->undo_tos = p_current->undo_bottom = NULL;
  p_current->undo_current = NULL;
}


/*! \brief Free the undo list of a page
 *
 *  \par Function Description
 *
 *  Wholly destroy the undo list for the given page \a p_current.
 *  The function has to be called when a page is closed.
 *
 *  \param [in,out] p_current The page to free the undo list for.
 */
void
lepton_undo_free_all (LeptonPage *p_current)
{
  lepton_undo_destroy_all (p_current->undo_bottom);
  p_current->undo_bottom = NULL;
  p_current->undo_tos = NULL;
  p_current->undo_current = NULL;
}
