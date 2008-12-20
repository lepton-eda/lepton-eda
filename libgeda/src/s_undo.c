/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
UNDO *s_undo_return_tail(UNDO *head)
{
  UNDO *u_current=NULL;
  UNDO *ret_struct=NULL;

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
UNDO *s_undo_return_head(UNDO *tail)
{
  UNDO *u_current=NULL;
  UNDO *ret_struct=NULL;

  u_current = tail;
  while ( u_current != NULL ) { /* goto end of list */
    ret_struct = u_current;
    u_current = u_current->prev;
  }

  return(ret_struct);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
UNDO *s_undo_new_head(void)
{
  UNDO *u_new;

  u_new = (UNDO *) g_malloc(sizeof(UNDO));
  u_new->type = -1;
  u_new->filename = NULL;
  u_new->object_list = NULL;
  u_new->left = u_new->right = u_new->top = u_new->bottom = -1;

  u_new->page_control = 0;
  u_new->up = -2;

  u_new->prev = NULL;
  u_new->next = NULL;

  return(u_new);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_undo_destroy_head(UNDO *u_head)
{
  g_free(u_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
UNDO *s_undo_add (UNDO *head, int type, char *filename, GList *object_list,
		 int left, int top, int right, int bottom, int page_control,
		 int up)
{
  UNDO *tail;
  UNDO *u_new;

  u_new = (UNDO *) g_malloc(sizeof(UNDO));

  u_new->filename = g_strdup (filename);
	
  u_new->object_list = object_list;

  u_new->type = type;

  u_new->left = left;
  u_new->top = top;
  u_new->right = right;
  u_new->bottom = bottom;

  u_new->page_control = page_control;
  u_new->up = up;

  if (head == NULL) {
    u_new->prev = NULL; /* setup previous link */
    u_new->next = NULL;
    return(u_new);
  } else {
    tail = s_undo_return_tail(head);
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
void s_undo_print_all( UNDO *head )
{
  UNDO *u_current;

  u_current = head;

  printf("START printing undo ********************\n");
  printf("BOTTOM\n");
  while(u_current != NULL) {

    if (u_current->filename) printf("%s\n", u_current->filename);
		
    if (u_current->object_list) {
      print_struct_forw (u_current->object_list);
    }
		
    printf("\t%d %d %d %d\n", u_current->left, u_current->top,
           u_current->right, u_current->bottom);
    u_current = u_current->next;
  }
  printf("TOS\n");
  printf("Number of levels: %d\n", s_undo_levels(head));
  printf("DONE printing undo ********************\n");
  printf("\n");

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_undo_destroy_all(TOPLEVEL *toplevel, UNDO *head)
{
  UNDO *u_current;
  UNDO *u_prev;

  u_current = s_undo_return_tail(head);

  while (u_current != NULL) {
    u_prev = u_current->prev;	
    g_free(u_current->filename);
		
    if (u_current->object_list) {
      s_delete_object_glist (toplevel, u_current->object_list);
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
void s_undo_remove(TOPLEVEL *toplevel, UNDO *head, UNDO *u_tos)
{
  UNDO *u_current;

  if (u_tos == NULL) {
    fprintf(stderr, "Got NULL for u_tos in s_undo_remove\n");
    return;
  }

  u_current = head;	

  while (u_current != NULL) {
    if (u_current == u_tos) {
      if (u_current->next)
        u_current->next->prev = u_current->prev;
      else
        u_current->next = NULL;

      if (u_current->prev)
        u_current->prev->next = u_current->next;
      else
        u_current->prev = NULL;

      g_free(u_current->filename);	

      if (u_current->object_list) {
        s_delete_object_glist (toplevel, u_current->object_list);
        u_current->object_list = NULL;
      }

      g_free(u_current);
      return;
    }
    u_current = u_current->next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_undo_remove_rest(TOPLEVEL *toplevel, UNDO *head)
{
  UNDO *u_current;
  UNDO *u_next;

  u_current = head;

  while (u_current != NULL) {
    u_next = u_current->next;	

    if (u_current->filename) {
      unlink(u_current->filename);
      g_free(u_current->filename);
    }

    if (u_current->object_list) {
      s_delete_object_glist (toplevel, u_current->object_list);
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
int s_undo_levels(UNDO *head)
{
  UNDO *u_current;
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
void s_undo_init(PAGE *p_current)
{
	p_current->undo_tos = p_current->undo_bottom = NULL;
	p_current->undo_current = NULL;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_undo_free_all(TOPLEVEL *toplevel, PAGE *p_current)
{
  s_undo_destroy_all(toplevel, p_current->undo_bottom);
  p_current->undo_bottom = NULL;
  p_current->undo_tos = NULL;
  p_current->undo_current = NULL;
}
