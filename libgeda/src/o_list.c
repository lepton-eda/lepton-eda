/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! global which is used in o_list_copy_all */
extern int global_sid;

/*! \todo Finish documentation!!!!
 *  \brief
 *  \par Function Description
 *  returns head !!!!!!!!!!!!!!!!!!!
 *  look at above.. this returns what was passed in!!!!
 *  copies selected to list_head (!! returns new list)
 *  flag is either NORMAL_FLAG or SELECTION_FLAG
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  list_head
 *  \param [in]  selected
 *  \param [in]  flag
 *  \param [out] return_end  
 *  \return OBJECT pointer.
 */
OBJECT *o_list_copy_to(TOPLEVEL *toplevel, OBJECT *list_head,
		       OBJECT *selected, int flag, OBJECT **return_end)
{
  OBJECT *end=NULL;

  /* are we adding a selection or the real object list */
  toplevel->ADDING_SEL = flag;

  end = (OBJECT *) return_tail(list_head);

  switch(selected->type) {

    case(OBJ_LINE):
      /* do we do anything with the return value) ? */
      end = (OBJECT *) o_line_copy(toplevel, end, selected);
      break;

    case(OBJ_NET):
      end = (OBJECT *) o_net_copy(toplevel, end, selected);
      break;

    case(OBJ_BUS):
      end = (OBJECT *) o_bus_copy(toplevel, end, selected);
      break;

    case(OBJ_BOX):
      end = (OBJECT *) o_box_copy(toplevel, end, selected);
      break;

    case(OBJ_PICTURE):
      end = (OBJECT *) o_picture_copy(toplevel, end, selected);
      break;

    case(OBJ_CIRCLE):
      end = (OBJECT *) o_circle_copy(toplevel, end, selected);
      break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
      if (o_complex_is_embedded (selected)) {
        end = (OBJECT *) o_complex_copy_embedded(toplevel, end, selected);
      } else {
        end = (OBJECT *) o_complex_copy(toplevel, end, selected);
      }
      break;

    case(OBJ_TEXT):
      end = (OBJECT *) o_text_copy(toplevel, end, selected);
      if (selected->attribute && 
          selected->visibility == INVISIBLE) {
        end->visibility = INVISIBLE;
      }
      break;

    case(OBJ_PIN):
      end = (OBJECT *) o_pin_copy(toplevel, end, selected);
      break;

    case(OBJ_ARC):
      end = (OBJECT *) o_arc_copy(toplevel, end, selected);
      break;
  }

  /* Store a reference in the copied object to where it was copied.
   * Used to retain associations when copying attributes */
  selected->copied_to = end;

  if (list_head == NULL)
    list_head = end;

  /* make sure sid is the same! */
  if (selected) {
    end->sid = selected->sid;
  }

  /* I don't think this is a good idea at all */
  /* toplevel->ADDING_SEL = 0; */
        
  if (return_end) {
    *return_end = end;	
  }

  return(list_head);
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *  you need to pass in a head_node for dest_list_head
 *  flag is either NORMAL_FLAG or SELECTION_FLAG
 *
 *  \param [in] toplevel       The TOPLEVEL object.
 *  \param [in] src_list_head   
 *  \param [in] dest_list_head  
 *  \param [in] flag
 *  \return OBJECT pointer.
 */
OBJECT *o_list_copy_all(TOPLEVEL *toplevel, OBJECT *src_list_head,
                        OBJECT *dest_list_head, int flag)
{
  OBJECT *src;
  OBJECT *dest;
  int adding_sel_save;

  src = src_list_head;
  dest = dest_list_head;

  if (src == NULL || dest == NULL) {
    return(NULL);
  }

  /* Save ADDING_SEL as o_list_copy_to() sets it */
  adding_sel_save = toplevel->ADDING_SEL;

  /* first do all NON text items */
  while(src != NULL) {

    if (src->type != OBJ_TEXT) {
      dest->next = o_list_copy_to(toplevel, NULL, src, flag,
                                  NULL);

      dest->next->prev = dest;
      dest = dest->next;
      dest->sid = global_sid++;
    }

    src = src->next;
  }

  src = src_list_head;
  /*dest = dest_list_head; out since we want to add to the end */

  /* then do all text items */
  while(src != NULL) {

    if (src->type == OBJ_TEXT) {
      dest->next = o_list_copy_to(toplevel, NULL, src, flag,
                                  NULL);

      dest->next->prev = dest;
      dest = dest->next;
      dest->sid = global_sid++;

      if (src->attached_to /*&& !toplevel->ADDING_SEL*/) {
        if (src->attached_to->copied_to) {
          o_attrib_attach(toplevel, dest_list_head,
                          dest, src->attached_to->copied_to);
        }
      }
    }

    src = src->next;
  }

  /* Clean up dangling copied_to pointers */
  src = src_list_head;
  while(src != NULL) {
    src->copied_to = NULL;
    src = src->next;
  }

  toplevel->ADDING_SEL = adding_sel_save;

  return(dest);
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *  you need to pass in a head_node for dest_list_head
 *  flag is either NORMAL_FLAG or SELECTION_FLAG
 *  this function copies the objects in the src GList src_list
 *  to the destination GList dest_list
 *  this routine assumes that objects in src_list are selected
 *  objects are unselected before they are copied and then reselected
 *  this is necessary to preserve the color info
 *
 *  \param [in] toplevel       The TOPLEVEL object.
 *  \param [in] src_list       The GList to copy from.
 *  \param [in] dest_list      The GList to copy to.
 *  \param [in] flag
 *  \return the dest_list GList with objects appended to it.
 */
GList *o_glist_copy_all_to_glist(TOPLEVEL *toplevel,
                                 GList *src_list,
                                 GList *dest_list, int flag)
{
  GList *src, *dest;
  OBJECT *src_object, *dst_object;
  int adding_sel_save;

  src = src_list;
  /* Reverse any existing items, as we will prepend, then reverse at the end */
  dest = g_list_reverse (dest_list);

  if (src == NULL) {
    return(NULL);
  }

  /* Save ADDING_SEL as o_list_copy_to() sets it */
  adding_sel_save = toplevel->ADDING_SEL;

  /* first do all NON text items */
  while(src != NULL) {
    src_object = (OBJECT *) src->data;

    /* unselect the object before the copy */
    o_selection_unselect(src_object);

    if (src_object->type != OBJ_TEXT && src_object->type != OBJ_HEAD) {
      dst_object = o_list_copy_to (toplevel, NULL, src_object, flag, NULL);
      dst_object->sid = global_sid++;
      /* Link the OBJECT nodes in the GList to allow attrib attaching */
      if (dest != NULL) {
        dst_object->prev = (OBJECT *)dest->data;
        dst_object->prev->next = dst_object;
      } else {
        dst_object->prev = NULL;
      }
      dst_object->next = NULL;
      dest = g_list_prepend (dest, dst_object);
    }

    /* reselect it */
    o_selection_select(src_object, SELECT_COLOR);

    src = g_list_next(src);
  }

  src = src_list;

  /* then do all text items */
  while(src != NULL) {
    src_object = (OBJECT *) src->data;

    /* unselect the object before the copy */
    o_selection_unselect(src_object);

    if (src_object->type == OBJ_TEXT) {
      dst_object = o_list_copy_to (toplevel, NULL, src_object, flag, NULL);
      dst_object->sid = global_sid++;
      /* Link the OBJECT nodes in the GList to allow attrib attaching */
      if (dest != NULL) {
        dst_object->prev = (OBJECT *)dest->data;
        dst_object->prev->next = dst_object;
      } else {
        dst_object->prev = NULL;
      }
      dst_object->next = NULL;
      dest = g_list_prepend (dest, dst_object);

      if (src_object->attached_to /*&& !toplevel->ADDING_SEL*/) {
        if (src_object->attached_to->copied_to) {
          o_attrib_attach(toplevel,
                          (OBJECT *)dest->data, /* This param is a hack */
                          dst_object, src_object->attached_to->copied_to);
        }
      }
    }

    /* reselect it */
    o_selection_select(src_object, SELECT_COLOR);

    src = g_list_next(src);
  }

  /* Clean up dangling copied_to pointers */
  src = src_list;
  while(src != NULL) {
    src_object = src->data;
    src_object->copied_to = NULL;
    src = g_list_next (src);
  }

  /* Reverse the list to be in the correct order */
  dest = g_list_reverse (dest);

  toplevel->ADDING_SEL = adding_sel_save;

  return(dest);
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *  returns entry in the list
 *
 *  \param [in] list
 *  \param [in] current
 *  \return OBJECT pointer.
 */
OBJECT *o_list_search(OBJECT *list, OBJECT *current)
{
  OBJECT *o_current;

  o_current = list ;

  if (current == NULL) {
    return(NULL);
  }

  if (list == NULL) {
    return(NULL);
  }

  while(o_current != NULL) {
    /* look for uniq sid */
    if (current->sid == o_current->sid) {
      return(o_current);
    }
    o_current = o_current->next;
  }
  return(NULL);
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] list
 *  \param [in] delete
 */
void o_list_delete(TOPLEVEL *toplevel, OBJECT *list, OBJECT *delete)
{
  OBJECT *find;

  find = o_list_search(list, delete);

  if (find != NULL)
  s_delete(toplevel, find);

}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *  assuming list is head
 *  head will NOT be deleted
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] list
 */
void o_list_delete_rest(TOPLEVEL *toplevel, OBJECT *list)
{
  OBJECT *o_current=NULL;
  OBJECT *o_prev=NULL;
	
  o_current = (OBJECT *) return_tail(list);

  /* remove list backwards */
  while(o_current != NULL) {
    if (o_current->type != OBJ_HEAD) {
      o_prev = o_current->prev;
      s_delete(toplevel, o_current);
      o_current = o_prev;
    } else {
      o_current->next = NULL; /* set sel_head->next to be empty */
      o_current = NULL;
    }
  }
}


/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 */
void o_list_translate_world (TOPLEVEL *toplevel, int x, int y, OBJECT *list)
{
  OBJECT *o_current = list;

  while ( o_current != NULL ) {
    o_translate_world (toplevel, x, y, o_current);
    o_current = o_current->next;
  }
}


/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 */
void o_glist_translate_world (TOPLEVEL *toplevel, int x, int y, GList *list)
{
  GList *iter = list;
  OBJECT *o_current;

  while ( iter != NULL ) {
    o_current = (OBJECT *)iter->data;
    o_translate_world (toplevel, x, y, o_current);
    iter = g_list_next (iter);
  }
}

