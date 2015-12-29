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
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

/*! global which is used in o_list_copy_all */
extern int global_sid;

/*! \todo Finish documentation!!!!
 *  \brief
 *  \par Function Description
 *  returns head !!!!!!!!!!!!!!!!!!!
 *  look at above.. this returns what was passed in!!!!
 *  copies selected to list_head (!! returns new list)
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  selected
 *  \return OBJECT pointer.
 */
OBJECT *o_object_copy (TOPLEVEL *toplevel,
                       OBJECT *selected)
{
  OBJECT *new_obj;

  g_return_val_if_fail (toplevel != NULL, NULL);
  g_return_val_if_fail (selected != NULL, NULL);

  switch(selected->type) {

    case(OBJ_LINE):
      new_obj = geda_line_object_copy (toplevel, selected);
      break;

    case(OBJ_NET):
      new_obj = geda_net_object_copy (toplevel, selected);
      break;

    case(OBJ_BUS):
      new_obj = geda_bus_object_copy (toplevel, selected);
      break;

    case(OBJ_BOX):
      new_obj = geda_box_object_copy (toplevel, selected);
      break;

    case(OBJ_PICTURE):
      new_obj = o_picture_copy (toplevel, selected);
      break;

    case(OBJ_CIRCLE):
      new_obj = geda_circle_object_copy (toplevel, selected);
      break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
      new_obj = o_complex_copy (toplevel, selected);
      break;

    case(OBJ_TEXT):
      new_obj = o_text_copy (toplevel, selected);
      break;

    case(OBJ_PATH):
      new_obj = geda_path_object (toplevel, selected);
      break;

    case(OBJ_PIN):
      new_obj = geda_pin_object_copy (toplevel, selected);
      break;

    case(OBJ_ARC):
      new_obj = geda_arc_object_copy (toplevel, selected);
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
 *  \return the dest_list GList with objects appended to it.
 */
GList *o_glist_copy_all (TOPLEVEL *toplevel,
                         const GList *src_list,
                         GList *dest_list)
{
  const GList *src;
  GList *dest;
  OBJECT *src_object, *dst_object;
  int selected_save;

  src = src_list;
  /* Reverse any existing items, as we will prepend, then reverse at the end */
  dest = g_list_reverse (dest_list);

  if (src == NULL) {
    return(NULL);
  }

  /* first do all NON text items */
  while(src != NULL) {
    src_object = (OBJECT *) src->data;

    /* unselect the object before the copy */
    selected_save = src_object->selected;
    if (selected_save)
      o_selection_unselect (toplevel, src_object);

    if (src_object->type != OBJ_TEXT) {
      dst_object = o_object_copy (toplevel, src_object);
      dst_object->sid = global_sid++;
      dest = g_list_prepend (dest, dst_object);
    }

    /* reselect it */
    if (selected_save)
      o_selection_select (toplevel, src_object);

    src = g_list_next(src);
  }

  src = src_list;

  /* then do all text items */
  while(src != NULL) {
    src_object = (OBJECT *) src->data;

    /* unselect the object before the copy */
    selected_save = src_object->selected;
    if (selected_save)
      o_selection_unselect (toplevel, src_object);

    if (src_object->type == OBJ_TEXT) {
      dst_object = o_object_copy (toplevel, src_object);
      dst_object->sid = global_sid++;
      dest = g_list_prepend (dest, dst_object);

      if (src_object->attached_to != NULL &&
          src_object->attached_to->copied_to != NULL) {
        o_attrib_attach(toplevel, dst_object,
                        src_object->attached_to->copied_to, FALSE);
        /* handle slot= attribute, it's a special case */
        if (g_ascii_strncasecmp (dst_object->text->string, "slot=", 5) == 0)
          s_slot_update_object (toplevel, src_object->attached_to->copied_to);
      }
    }

    /* reselect it */
    if (selected_save)
      o_selection_select (toplevel, src_object);

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

  return(dest);
}


/*! \brief Translate a list of objects
 *
 *  \param [in,out] objects A GList of objects to translate.
 *  \param [in]     dx      The x distance to move.
 *  \param [in]     dy      The y distance to move.
 */
void
geda_object_list_translate (const GList *objects, int dx, int dy)
{
  const GList *iter = objects;

  while (iter != NULL) {
    GedaObject *object = (GedaObject*)iter->data;

    geda_object_translate (object, dx, dy);
    iter = g_list_next (iter);
  }
}

/*! \brief Rotate a list of objects
 *
 *  \param [in,out] objects  A GList of objects to translate.
 *  \param [in]     x        The x center of rotation.
 *  \param [in]     y        The y center of rotation.
 *  \param [in]     angle    The angle rotation in multiples of 90 degrees.
 *  \param [in]     toplevel The toplevel object. (used for change notification)
 */
void
geda_object_list_rotate (const GList *objects, int x, int y, int angle, TOPLEVEL *toplevel)
{
  const GList *iter = objects;

  while (iter != NULL) {
    GedaObject *object = (GedaObject*)iter->data;

    geda_object_rotate (toplevel, x, y, angle, object);
    iter = g_list_next (iter);
  }
}

/*! \brief Mirror a list of objects
 *
 *  \param [in,out] objects  A GList of objects to mirror.
 *  \param [in]     x        The x center of mirroring
 *  \param [in]     y        Unused, essentially
 *  \param [in]     toplevel The toplevel object. (used for change notification)
 */
void
geda_object_list_mirror (const GList *objects, int x, int y, TOPLEVEL *toplevel)
{
  const GList *iter = objects;

  while (iter != NULL) {
    GedaObject *object = (GedaObject*)iter->data;

    geda_object_mirror (toplevel, x, y, object);
    iter = g_list_next (iter);
  }
}


/*! \brief Change the color of a list of objects
 *
 *  \param [in,out] objects  A GList of objects to mirror.
 *  \param [in]     color    The new color.
 *  \param [in]     toplevel The toplevel object. (used for change notification)
 */
void
geda_object_list_set_color (const GList *objects, int color, TOPLEVEL *toplevel)
{
  const GList *iter = objects;

  while (iter != NULL) {
    GedaObject *object = (GedaObject*)iter->data;

    o_set_color (toplevel, object, color);
    iter = g_list_next (iter);
  }
}
