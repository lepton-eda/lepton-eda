/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to set the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to set the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to set the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to set the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
 *  \return The value of the \a type field.
 */
int
lepton_undo_get_type (LeptonUndo *undo)
{
  g_return_val_if_fail (undo != NULL, UNDO_ALL);

  return undo->type;
}


/*! \brief Set undo structure's \a type field value.
 *
 *  \param [in] undo The undo structure to set the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to set the field of.
 *  \param [in] undo The new value of the \a up field.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to set the field of.
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
 *  \param [in] undo The undo structure to obtain the field of.
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
 *  \param [in] undo The undo structure to set the field of.
 *  \param [in] y The new value of the \a y field.
 */
void
lepton_undo_set_y (LeptonUndo *undo,
                   int y)
{
  g_return_if_fail (undo != NULL);

  undo->y = y;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
lepton_undo_print_all (LeptonUndo *head)
{
  LeptonUndo *u_current;

  u_current = head;

  printf("START printing undo ********************\n");
  printf("BOTTOM\n");
  while(u_current != NULL) {

    if (u_current->filename) printf("%s\n", u_current->filename);

    if (u_current->object_list) {
      lepton_object_list_print (u_current->object_list);
    }

    printf("\t%d %d %f\n", u_current->x, u_current->y, u_current->scale);
    u_current = u_current->next;
  }
  printf("TOS\n");
  printf("Number of levels: %d\n", lepton_undo_levels (head));
  printf("DONE printing undo ********************\n");
  printf("\n");

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
lepton_undo_init (LeptonPage *p_current)
{
  p_current->undo_tos = p_current->undo_bottom = NULL;
  p_current->undo_current = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void
lepton_undo_free_all (LeptonPage *p_current)
{
  lepton_undo_destroy_all (p_current->undo_bottom);
  p_current->undo_bottom = NULL;
  p_current->undo_tos = NULL;
  p_current->undo_current = NULL;
}
