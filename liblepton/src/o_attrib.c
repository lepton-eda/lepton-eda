/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

/*! \file o_attrib.c
 *  \brief utility functions for attributes
 *
 *  Attributes are normal text objects. An attribute is a text object
 *  that has a text string that is delimited by an equal "=" character.
 *  The part before the equal character is called <b>name</b> the
 *  part of the string behind the equal character is called <b>value</b>
 *
 *  Attributes are attached to LeptonObjects (st_object). Each
 *  attribute has a reference to the object it is attached
 *  to. Each object that has attributes has a list of pointers to
 *  its attributes.
 *
 *  \image html o_attrib_overview.png
 *  \image latex o_attrib_overview.pdf "attribute overview" width=14cm
 *
 *  \note
 *  Be sure in o_copy o_move o_delete you maintain the attributes
 *  delete is a bare, because you will have to unattach the other end
 *  and in o_save o_read as well
 *  and in o_select when selecting objects, select the attributes
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include "liblepton_priv.h"


/*! \brief Add an attribute to an existing attribute list.
 *
 *  \param [in]  object     The LeptonObject we're adding the attribute to.
 *  \param [in]  item       The item you want to add as an attribute.
 */
void
o_attrib_add (LeptonObject *object, LeptonObject *item)
{
  /* Add link from item to attrib listing */
  item->attached_to = object;
  object->attribs = g_list_append (object->attribs, item);
}


/*! \brief Attach existing attribute to an object.
 *
 *  \param [in]  attrib       The attribute to be added.
 *  \param [out] object       The object where you want to add item as an attribute.
 *  \param [in]  set_color    Whether or not we should set the new attribute's color.
 */
void
o_attrib_attach (LeptonObject *attrib,
                 LeptonObject *object,
                 int set_color)
{
  g_return_if_fail (attrib != NULL);
  g_return_if_fail (object != NULL);

  /* is the object already part of the list ? */
  if (g_list_find (object->attribs, attrib)) {
    g_warning (_("Attribute [%1$s] already attached\n"),
               lepton_text_object_get_string (attrib));
    return;
  }

  if (!lepton_object_is_text (attrib))
  {
    g_warning (_("Attempt to attach non text item as an attribute!\n"));
    return;
  }

  if (attrib->attached_to != NULL) {
    g_warning (_("Attempt to attach attribute [%1$s] to more than one object\n"),
               lepton_text_object_get_string (attrib));
    return;
  }

  /* attribute inherit its selectable status from the object:
  */
  attrib->selectable = object->selectable;

  o_attrib_add (object, attrib);

  if (set_color)
    lepton_object_set_color (attrib, ATTRIBUTE_COLOR);
}


/*! \brief Attach list of existing attributes to an object.
 *
 *  \param [in]  attr_list  The list of attributes to be added.
 *  \param [out] object     The object where you want to add item as an attribute.
 *  \param [in]  set_color    Whether or not we should set the new attribute's color.
 */
void
o_attrib_attach_list (GList *attr_list,
                      LeptonObject *object,
                      int set_color)
{
  GList *iter;

  for (iter = attr_list; iter != NULL; iter = g_list_next (iter))
    o_attrib_attach ((LeptonObject*) iter->data, object, set_color);
}


/*! \brief Detach all attributes from an object.
 *
 *  \param [in,out] object    The object whose attributes to detach.
 */
void
o_attrib_detach_all (LeptonObject *object)
{
  LeptonObject *a_current;
  GList *a_iter;

  for (a_iter = object->attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = (LeptonObject*) a_iter->data;

    a_current->attached_to = NULL;
    lepton_object_set_color (a_current, DETACHED_ATTRIBUTE_COLOR);
  }

  g_list_free (object->attribs);
  object->attribs = NULL;
}

/*! \brief Print all attributes to a Postscript document.
 *
 *  \param [in] attributes  List of attributes to print.
 */
void o_attrib_print(GList *attributes)
{
  LeptonObject *a_current;
  GList *a_iter;

  a_iter = attributes;

  while (a_iter != NULL) {
    a_current = (LeptonObject*) a_iter->data;
    printf("Attribute points to: %1$s\n", a_current->name);
    if (a_current->text) {
      printf("\tText is: %1$s\n", lepton_text_object_get_string (a_current));
    }

    a_iter = g_list_next (a_iter);
  }
}

/*! \todo Finish function.
 *  \brief Remove an attribute item from an attribute list.
 *  \par Function Description
 *  This function removes the given attribute from an attribute list.
 *  This function should be used when detaching an attribute.
 *
 *  \param [in] list      The attribute list to remove attribute from.
 *  \param [in] remove    The LeptonObject to remove from list.
 */
void
o_attrib_remove (GList **list,
                 LeptonObject *remove)
{
  g_return_if_fail (remove != NULL);

  remove->attached_to = NULL;

  *list = g_list_remove (*list, remove);
}

/*! \brief Read attributes from a TextBuffer.
 *
 *  \param [in]  page                   The LeptonPage object.
 *  \param [in]  object_to_get_attribs  Object which gets these attribs.
 *  \param [in]  tb                     The text buffer to read from.
 *  \param [in]  release_ver            libgeda release version number.
 *  \param [in]  fileformat_ver         file format version number.
 *  \return GList of attributes read, or NULL on error.
 */
GList*
o_read_attribs (LeptonPage *page,
                LeptonObject *object_to_get_attribs,
                TextBuffer *tb,
                unsigned int release_ver,
                unsigned int fileformat_ver,
                GError ** err)
{
  GList *object_list = NULL;
  LeptonObject *new_obj;
  const char *line = NULL;
  char objtype;
  int ATTACH=FALSE;

  while (1) {

    line = s_textbuffer_next_line (tb);
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);
    switch (objtype) {

      case(OBJ_LINE):
        if ((new_obj = o_line_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;


      case(OBJ_NET):
        if ((new_obj = o_net_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BUS):
        if ((new_obj = o_bus_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BOX):
        if ((new_obj = lepton_box_object_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        if ((new_obj = o_circle_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_COMPONENT):
        if ((new_obj = o_component_read (page, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PATH):
        new_obj = o_path_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PIN):
        if ((new_obj = lepton_pin_object_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_ARC):
        if ((new_obj = lepton_arc_object_read (line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_TEXT):
        new_obj = lepton_text_object_read (line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        ATTACH=TRUE;

        break;

      case(ENDATTACH_ATTR):
        return object_list;
        break;
    }

    if (ATTACH) {
      o_attrib_attach (new_obj, object_to_get_attribs, FALSE);
      ATTACH=FALSE;
    } else {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Tried to attach a non-text item as an attribute"));
      goto error;
    }
  }

  /* The attribute list wasn't terminated, so it's a parse error! */
  g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE,
               _("Unexpected end-of-file in attribute list"));

error:
  lepton_object_list_delete (object_list);
  return NULL;
}


/*! \brief Get name and value from an attribute 'name=value' string.
 *
 *  \par Function Description
 *  This function parses the character string \a string expected to be
 *  an attribute string of the form 'name=value'.
 *
 *  It returns TRUE if it has been able to parse the string into the
 *  name and value parts of an attribute. Otherwise it returns FALSE,
 *  in that case \a *name_ptr and \a *value_ptr are set to NULL.
 *
 *  \a name_ptr and/or \a value_ptr can be NULL.
 *  If not NULL, the caller must g_free these returned strings.
 *
 *  \note
 *  If you get an invalid attribute (improper) with a name and no
 *  value, then it is NOT an attribute. Also, there cannot be any
 *  spaces beside the equals sign
 *
 *  \param [in]  string     String to split into name/value pair.
 *  \param [out] name_ptr   The return location for the name, or NULL.
 *  \param [out] value_ptr  The return location for the value, or NULL.
 *  \return TRUE on success, FALSE otherwise.
 */
gboolean
o_attrib_string_get_name_value (const gchar *string, gchar **name_ptr, gchar **value_ptr)
{
  gchar *ptr, *prev_char, *next_char;

  if (name_ptr != NULL)
    *name_ptr = NULL;
  if (value_ptr != NULL)
    *value_ptr = NULL;

  g_return_val_if_fail (string != NULL, FALSE);

  ptr = g_utf8_strchr (string, -1, g_utf8_get_char ("="));
  if (ptr == NULL) {
    return FALSE;
  }

  prev_char = g_utf8_find_prev_char (string, ptr);
  next_char = g_utf8_find_next_char (ptr, NULL);
  if (prev_char == NULL || *prev_char == ' ' ||
      next_char == NULL || *next_char == ' ' || *next_char == '\0' ) {
    return FALSE;
  }

  if (name_ptr != NULL) {
    *name_ptr = g_strndup (string, (ptr - string));
  }

  if (value_ptr != NULL) {
    *value_ptr = g_strdup (next_char);
  }

  return TRUE;
}

/*! \brief Find all floating attributes in the given object list.
 *
 *  \param [in] list  GList of LeptonObjects to search for floating attributes.
 *  \return GList of floating attributes from the input list
 *
 *  Caller must g_list_free() the returned list.
 */
GList *o_attrib_find_floating_attribs (const GList *list)
{
  GList *floating_attributes = NULL;
  const GList *iter;
  LeptonObject *o_current;

  for (iter = list; iter != NULL; iter = g_list_next (iter)) {
    o_current = (LeptonObject*) iter->data;

    /* Skip non text objects, attached attributes and text which doesn't
     * constitute a valid attributes (e.g. general text placed on the page)
     */
    if (lepton_object_is_attrib (o_current) &&
        o_current->attached_to == NULL) {

      floating_attributes = g_list_prepend (floating_attributes, o_current);
    }
  }

  return g_list_reverse (floating_attributes);
}


/*! \brief Find an attribute in a list.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurrence of the attribute, and starts searching
 *  from zero.  Zero is the first occurrence of an attribute.
 *
 *  \param [in] list     GList of attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] count    Which occurrence to return.
 *  \return The n'th attribute object in the given list with the given name.
 */
LeptonObject *o_attrib_find_attrib_by_name (const GList *list,
                                      const char *name,
                                      int count)
{
  g_return_val_if_fail (name, NULL);

  const gchar *needle = g_intern_string (name);
  int num_found = 0;

  for (const GList *iter = list; iter; iter = g_list_next (iter)) {
    LeptonObject *attrib = (LeptonObject*) iter->data;
    g_return_val_if_fail (lepton_object_is_text (attrib), NULL);

    if ((needle == lepton_text_object_get_name (attrib)) &&
        (num_found++ == count)) {
      return attrib;
    }
  }

  return NULL;
}


/*! \brief Search attribute list by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurrence of the attribute, and starts searching
 *  from zero.  Zero is the first occurrence of an attribute.
 *
 *  \param [in] list     GList of attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurrence to return.
 *  \return Character string with attribute value, NULL otherwise.
 */
static char *o_attrib_search_attrib_list_by_name (const GList *list,
                                                  const char *name,
                                                  int counter)
{
  LeptonObject *attrib;
  char *value = NULL;

  attrib = o_attrib_find_attrib_by_name (list, name, counter);

  if (attrib != NULL)
    value = g_strdup (lepton_text_object_get_value (attrib));

  return value;
}


/*! \brief Search floating attribute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurrence of the attribute, and starts searching
 *  from zero.  Zero is the first occurrence of an attribute.
 *
 *  \param [in] list     GList of LeptonObjects to search for floating attributes.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurrence to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_floating_attribs_by_name (const GList *list,
                                                const char *name,
                                                int counter)
{
  char *result;
  GList *attributes;

  attributes = o_attrib_find_floating_attribs (list);
  result = o_attrib_search_attrib_list_by_name (attributes, name, counter);
  g_list_free (attributes);

  return result;
}


/*! \brief Search attached attributes by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurrence of the attribute, and starts searching
 *  from zero.  Zero is the first occurrence of an attribute.
 *
 *  \param [in] object   The LeptonObject whose attached attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurrence to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_attached_attribs_by_name (LeptonObject *object,
                                                const char *name,
                                                int counter)
{
  return o_attrib_search_attrib_list_by_name (object->attribs, name, counter);
}


/*! \brief Search inherited attribute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurrence of the attribute, and starts searching
 *  from zero.  Zero is the first occurrence of an attribute.
 *
 *  \param [in] object   The LeptonObject whose inherited attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurrence to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_inherited_attribs_by_name (LeptonObject *object,
                                                 const char *name,
                                                 int counter)
{
  g_return_val_if_fail (lepton_object_is_component (object), NULL);

  return o_attrib_search_floating_attribs_by_name (object->component->prim_objs, name, counter);
}


/*! \brief Search attached and inherited attributes of object by name.
 *
 *  \par Function Description
 *
 *  Counter is the n'th occurrence of the attribute, and starts searching
 *  from zero.  Zero is the first occurrence of an attribute.
 *
 *  Caller must g_free() the returned character string.
 *
 *  \param [in] object   LeptonObject who's attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurrence to return.
 *  \return              Attribute value, NULL if not found.
 */
char *o_attrib_search_object_attribs_by_name (LeptonObject *object,
                                              const char *name,
                                              int counter)
{
  char *result;
  GList *attributes;

  attributes = o_attrib_return_attribs (object);
  result = o_attrib_search_attrib_list_by_name (attributes, name, counter);
  g_list_free (attributes);

  return result;
}


/*! \brief Get object's attached and inherited attributes.
 *
 *  \par Function Description
 *
 *  This function aggregates the attached and inherited attributes
 *  belonging to a given LeptonObject into one GList and returns it.
 *  Inherited attributes are those which live as toplevel un-attached
 *  attributes inside in a component LeptonObject's prim_objs.
 *
 *  Caller must g_list_free() the returned GList.
 *
 *  \param [in] object  LeptonObject whose attributes to return.
 *  \return             A GList of attached and inherited attributes.
 */
GList * o_attrib_return_attribs (LeptonObject *object)
{
  GList *attribs = NULL;
  GList *inherited_attribs;
  LeptonObject *a_current;
  GList *a_iter;

  g_return_val_if_fail (object != NULL, NULL);

  /* Directly attached attributes */
  for (a_iter = object->attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = (LeptonObject*) a_iter->data;

    if (!lepton_object_is_text (a_current))
      continue;

    /* Don't add invalid attributes to the list */
    if (!lepton_object_is_attrib (a_current))
      continue;

    attribs = g_list_prepend (attribs, a_current);
  }

  attribs = g_list_reverse (attribs);

  /* Inherited attributes (inside component objects) */
  if (lepton_object_is_component (object))
  {
    inherited_attribs =
      o_attrib_find_floating_attribs (object->component->prim_objs);

    attribs = g_list_concat (attribs, inherited_attribs);
  }

  return attribs;
}


/*! \brief Query whether a given attribute LeptonObject is "inherited".
 *
 *  \par Function Description
 *  This function returns TRUE if the given attribute LeptonObject is a
 *  toplevel un-attached attribute inside a component's prim_objs.
 *
 *  \param [in] attrib       LeptonObject who's status to query.
 *  \return TRUE if the attribute is inherited, FALSE otherwise.
 */
int o_attrib_is_inherited (const LeptonObject *attrib)
{
  return (attrib->attached_to == NULL &&
          attrib->parent != NULL);
}
