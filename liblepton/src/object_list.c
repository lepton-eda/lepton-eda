/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
#include <version.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "liblepton_priv.h"

/*! \file object_list.c
 *  \brief Functions dealing with object lists.
 */

static const gchar*
o_file_format_header ();

static gchar*
o_save_objects (const GList *object_list, gboolean save_attribs);


/*! global which is used in o_list_copy_all */
extern int global_sid;

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
 *  \param [in] src_list       The GList to copy from.
 *  \param [in] dest_list      The GList to copy to.
 *  \return the dest_list GList with objects appended to it.
 */
GList*
o_glist_copy_all (const GList *src_list,
                  GList *dest_list)
{
  const GList *src;
  GList *dest;
  LeptonObject *src_object, *dst_object;
  int selected_save;

  src = src_list;
  /* Reverse any existing items, as we will prepend, then reverse at the end */
  dest = g_list_reverse (dest_list);

  if (src == NULL) {
    return(NULL);
  }

  /* first do all NON text items */
  while(src != NULL) {
    src_object = (LeptonObject *) src->data;

    /* unselect the object before the copy */
    selected_save = lepton_object_get_selected (src_object);
    if (selected_save)
      o_selection_unselect (src_object);

    if (!lepton_object_is_text (src_object))
    {
      dst_object = lepton_object_copy (src_object);
      lepton_object_set_id (dst_object, global_sid++);
      dest = g_list_prepend (dest, dst_object);
    }

    /* reselect it */
    if (selected_save)
      o_selection_select (src_object);

    src = g_list_next(src);
  }

  src = src_list;

  /* then do all text items */
  while(src != NULL) {
    src_object = (LeptonObject *) src->data;

    /* unselect the object before the copy */
    selected_save = lepton_object_get_selected (src_object);
    if (selected_save)
      o_selection_unselect (src_object);

    if (lepton_object_is_text (src_object))
    {
      dst_object = lepton_object_copy (src_object);
      lepton_object_set_id (dst_object, global_sid++);
      dest = g_list_prepend (dest, dst_object);

      LeptonObject *attachment = lepton_object_get_attached_to (src_object);
      if (attachment != NULL &&
          attachment->copied_to != NULL)
      {
        o_attrib_attach (dst_object,
                         attachment->copied_to,
                         FALSE);
        /* handle slot= attribute, it's a special case */
        if (g_ascii_strncasecmp (lepton_text_object_get_string (dst_object),
                                 "slot=", 5) == 0)
          s_slot_update_object (attachment->copied_to);
      }
    }

    /* reselect it */
    if (selected_save)
      o_selection_select (src_object);

    src = g_list_next(src);
  }

  /* Clean up dangling copied_to pointers */
  src = src_list;
  while(src != NULL) {
    src_object = (LeptonObject*) src->data;
    src_object->copied_to = NULL;
    src = g_list_next (src);
  }

  /* Reverse the list to be in the correct order */
  dest = g_list_reverse (dest);

  return(dest);
}

/*! \brief Delete a list of objects
 *
 *  This function deletes everything, including the GList.
 *
 *  \param [in] objects A GList of objects to delete.
 */
void
lepton_object_list_delete (GList *objects)
{
  LeptonObject *o_current=NULL;
  GList *ptr;

  ptr = g_list_last(objects);

  /* do the delete backwards */
  while(ptr != NULL) {
    o_current = (LeptonObject *) ptr->data;
    lepton_object_delete (o_current);
    ptr = g_list_previous (ptr);
  }
  g_list_free(objects);
}

/*! \brief Print a list of objects
 *
 *  Prints a list of objects to stdout, for debugging.
 *
 *  \param [in] objects A GList of objects to print.
 */
void
lepton_object_list_print (GList *objects)
{
  LeptonObject *o_current=NULL;
  GList *iter;

  iter = objects;
  printf("TRYING to PRINT\n");
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;
    printf("Name: %s\n", o_current->name);
    printf("Type: %d\n", lepton_object_get_type (o_current));
    printf("Sid: %d\n", lepton_object_get_id (o_current));

    if (lepton_object_is_component (o_current))
    {
      GList *primitives = lepton_component_object_get_contents (o_current);
      lepton_object_list_print (primitives);
    }

    o_attrib_print (lepton_object_get_attribs (o_current));

    printf("----\n");
    iter = g_list_next (iter);
  }
}

/*! \brief Translate a list of objects
 *
 *  \param [in,out] objects A GList of objects to translate.
 *  \param [in]     dx      The x distance to move.
 *  \param [in]     dy      The y distance to move.
 */
void
lepton_object_list_translate (const GList *objects,
                              int dx,
                              int dy)
{
  const GList *iter = objects;

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*)iter->data;

    lepton_object_translate (object, dx, dy);
    iter = g_list_next (iter);
  }
}

/*! \brief Rotate a list of objects
 *
 *  \param [in,out] objects  A GList of objects to translate.
 *  \param [in]     x        The x center of rotation.
 *  \param [in]     y        The y center of rotation.
 *  \param [in]     angle    The angle rotation in multiples of 90 degrees.
 */
void
lepton_object_list_rotate (const GList *objects,
                           int x,
                           int y,
                           int angle)
{
  const GList *iter = objects;

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*)iter->data;

    lepton_object_rotate (x, y, angle, object);
    iter = g_list_next (iter);
  }
}

/*! \brief Mirror a list of objects
 *
 *  \param [in,out] objects  A GList of objects to mirror.
 *  \param [in]     x        The x center of mirroring
 *  \param [in]     y        Unused, essentially
 */
void
lepton_object_list_mirror (const GList *objects,
                           int x,
                           int y)
{
  const GList *iter = objects;

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*)iter->data;

    lepton_object_mirror (x, y, object);
    iter = g_list_next (iter);
  }
}


/*! \brief Change the color of a list of objects
 *
 *  \param [in,out] objects  A GList of objects to mirror.
 *  \param [in]     color    The new color.
 */
void
lepton_object_list_set_color (const GList *objects,
                              int color)
{
  const GList *iter = objects;

  while (iter != NULL) {
    LeptonObject *object = (LeptonObject*)iter->data;

    lepton_object_set_color (object, color);
    iter = g_list_next (iter);
  }
}


/*! \brief "Save" a file into a string buffer
 *  \par Function Description
 *  This function saves a whole schematic into a buffer in libgeda
 *  format. The buffer should be freed when no longer needed.
 *
 *  \param [in] objects The head of a GList of LeptonObjects to
 *  save.
 *  \returns a buffer containing schematic data or NULL on failure.
 */
gchar*
lepton_object_list_to_buffer (const GList *objects)
{
  GString *acc;
  gchar *buffer;

  acc = g_string_new (o_file_format_header());

  buffer = o_save_objects (objects, FALSE);
  g_string_append (acc, buffer);
  g_free (buffer);

  return g_string_free (acc, FALSE);
}

/*! \brief Get the file header string.
 *  \par Function Description
 *  This function simply returns the DATE_VERSION and
 *  FILEFORMAT_VERSION formatted as a gEDA file header.
 *
 *  \warning <em>Do not</em> free the returned string.
 */
static const gchar
*o_file_format_header ()
{
  static gchar *header = NULL;

  if (header == NULL)
    header = g_strdup_printf("v %s %u\n", PACKAGE_DATE_VERSION,
                             FILEFORMAT_VERSION);

  return header;
}

/*! \brief Save a series of objects into a string buffer
 *  \par Function Description
 *  This function recursively saves a set of objects into a buffer in
 *  libgeda format.  User code should not normally call this function;
 *  they should call o_save_buffer() instead.
 *
 *  With save_attribs passed as FALSE, attribute objects are skipped over,
 *  and saved separately - after the objects they are attached to. When
 *  we recurse for saving out those attributes, the function must be called
 *  with save_attribs passed as TRUE.
 *
 *  \param [in] object_list   The head of a GList of objects to save.
 *  \param [in] save_attribs  Should attribute objects encounterd be saved?
 *  \returns a buffer containing schematic data or NULL on failure.
 */
static gchar*
o_save_objects (const GList *object_list, gboolean save_attribs)
{
  LeptonObject *o_current;
  const GList *iter;
  gchar *out;
  GString *acc;
  gboolean already_wrote = FALSE;

  acc = g_string_new("");

  iter = object_list;

  while ( iter != NULL ) {
    o_current = (LeptonObject *)iter->data;

    if (save_attribs ||
        lepton_object_get_attached_to (o_current) == NULL)
    {

      switch (lepton_object_get_type (o_current)) {

        case(OBJ_LINE):
          out = lepton_line_object_to_buffer (o_current);
          break;

        case(OBJ_NET):
          out = lepton_net_object_to_buffer (o_current);
          break;

        case(OBJ_BUS):
          out = lepton_bus_object_to_buffer (o_current);
          break;

        case(OBJ_BOX):
          out = lepton_box_object_to_buffer (o_current);
          break;

        case(OBJ_CIRCLE):
          out = lepton_circle_object_to_buffer (o_current);
          break;

        case(OBJ_COMPONENT):
          out = lepton_component_object_to_buffer (o_current);
          g_string_append_printf(acc, "%s\n", out);
          already_wrote = TRUE;
          g_free(out); /* need to free here because of the above flag */

          if (lepton_component_object_get_embedded (o_current)) {
            g_string_append(acc, "[\n");

            GList *primitives = lepton_component_object_get_contents (o_current);
            out = o_save_objects (primitives, FALSE);
            g_string_append (acc, out);
            g_free(out);

            g_string_append(acc, "]\n");
          }
          break;

        case(OBJ_TEXT):
          out = lepton_text_object_to_buffer (o_current);
          break;

        case(OBJ_PATH):
          out = lepton_path_object_to_buffer (o_current);
          break;

        case(OBJ_PIN):
          out = lepton_pin_object_to_buffer (o_current);
          break;

        case(OBJ_ARC):
          out = lepton_arc_object_to_buffer (o_current);
          break;

        case(OBJ_PICTURE):
          out = lepton_picture_object_to_buffer (o_current);
          break;

        default:
          /*! \todo Maybe we can continue instead of just failing
           *  completely? In any case, failing gracefully is better
           *  than killing the program, which is what this used to
           *  do... */
          g_critical (_("o_save_objects: object %1$p has unknown type '%2$c'\n"),
                      o_current, lepton_object_get_type (o_current));
          /* Dump string built so far */
          g_string_free (acc, TRUE);
          return NULL;
      }

      /* output the line */
      if (!already_wrote) {
        g_string_append_printf(acc, "%s\n", out);
        g_free(out);
      } else {
        already_wrote = FALSE;
      }

      /* save any attributes */
      GList *attribs = lepton_object_get_attribs (o_current);
      if (attribs != NULL)
      {
        g_string_append (acc, "{\n");

        out = o_save_objects (attribs, TRUE);
        g_string_append (acc, out);
        g_free(out);

        g_string_append (acc, "}\n");
      }
    }

    iter = g_list_next (iter);
  }

  return g_string_free (acc, FALSE);
}
