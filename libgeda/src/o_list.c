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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

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
 *  \param [in]  selected
 *  \param [in]  flag
 *  \param [out] return_end  
 *  \return OBJECT pointer.
 */
OBJECT *o_object_copy (TOPLEVEL *toplevel,
                       OBJECT *selected, int flag)
{
  OBJECT *new_obj;

  /* are we adding a selection or the real object list */
  toplevel->ADDING_SEL = flag;

  switch(selected->type) {

    case(OBJ_LINE):
      new_obj = o_line_copy (toplevel, selected);
      break;

    case(OBJ_NET):
      new_obj = o_net_copy (toplevel, selected);
      break;

    case(OBJ_BUS):
      new_obj = o_bus_copy (toplevel, selected);
      break;

    case(OBJ_BOX):
      new_obj = o_box_copy (toplevel, selected);
      break;

    case(OBJ_PICTURE):
      new_obj = o_picture_copy (toplevel, selected);
      break;

    case(OBJ_CIRCLE):
      new_obj = o_circle_copy (toplevel, selected);
      break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
      if (o_complex_is_embedded (selected)) {
        new_obj = o_complex_copy_embedded (toplevel, selected);
      } else {
        new_obj = o_complex_copy (toplevel, selected);
      }
      break;

    case(OBJ_TEXT):
      new_obj = o_text_copy (toplevel, selected);
      break;

    case(OBJ_PATH):
      new_obj = o_path_copy (toplevel, selected);
      break;

    case(OBJ_PIN):
      new_obj = o_pin_copy (toplevel, selected);
      break;

    case(OBJ_ARC):
      new_obj = o_arc_copy (toplevel, selected);
      break;

    default:
      g_critical ("o_list_copy_to: object %p has bad type '%c'\n",
                  selected, selected->type);
      return NULL;
  }

  /* Store a reference in the copied object to where it was copied.
   * Used to retain associations when copying attributes */
  selected->copied_to = new_obj;

  /* make sure sid is the same! */
  if (selected) {
    new_obj->sid = selected->sid;
  }

  /* I don't think this is a good idea at all */
  /* toplevel->ADDING_SEL = 0; */

  return new_obj;
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
GList *o_glist_copy_all (TOPLEVEL *toplevel,
                         const GList *src_list,
                         GList *dest_list, int flag)
{
  const GList *src;
  GList *dest;
  OBJECT *src_object, *dst_object;
  int adding_sel_save;
  int selected_save;

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
    selected_save = src_object->selected;
    if (selected_save)
      o_selection_unselect(src_object);

    if (src_object->type != OBJ_TEXT) {
      dst_object = o_object_copy (toplevel, src_object, flag);
      dst_object->sid = global_sid++;
      dest = g_list_prepend (dest, dst_object);
    }

    /* reselect it */
    if (selected_save)
      o_selection_select(src_object, SELECT_COLOR);

    src = g_list_next(src);
  }

  src = src_list;

  /* then do all text items */
  while(src != NULL) {
    src_object = (OBJECT *) src->data;

    /* unselect the object before the copy */
    selected_save = src_object->selected;
    if (selected_save)
      o_selection_unselect(src_object);

    if (src_object->type == OBJ_TEXT) {
      dst_object = o_object_copy (toplevel, src_object, flag);
      dst_object->sid = global_sid++;
      dest = g_list_prepend (dest, dst_object);

      if (src_object->attached_to != NULL &&
          src_object->attached_to->copied_to != NULL) {
        o_attrib_attach(toplevel, dst_object,
                        src_object->attached_to->copied_to, FALSE);
        /* handle slot= attribute, it's a special case */
        if (g_ascii_strncasecmp (dst_object->text->string, "slot=", 5) == 0)
          o_attrib_slot_update (toplevel, src_object->attached_to->copied_to);
      }
    }

    /* reselect it */
    if (selected_save)
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
 */
void o_glist_translate_world(TOPLEVEL *toplevel, int dx, int dy, const GList *list)
{
  const GList *iter = list;
  OBJECT *o_current;

  while ( iter != NULL ) {
    o_current = (OBJECT *)iter->data;
    o_translate_world(toplevel, dx, dy, o_current);
    iter = g_list_next (iter);
  }
}


/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 */
void o_glist_rotate_world (TOPLEVEL *toplevel, int x, int y, int angle, const GList *list)
{
  const GList *iter = list;
  OBJECT *o_current;

  while ( iter != NULL ) {
    o_current = (OBJECT *)iter->data;
    o_rotate_world (toplevel, x, y, angle, o_current);
    iter = g_list_next (iter);
  }
}


/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 */
void o_glist_mirror_world (TOPLEVEL *toplevel, int x, int y, const GList *list)
{
  const GList *iter = list;
  OBJECT *o_current;

  while ( iter != NULL ) {
    o_current = (OBJECT *)iter->data;
    o_mirror_world (toplevel, x, y, o_current);
    iter = g_list_next (iter);
  }
}
