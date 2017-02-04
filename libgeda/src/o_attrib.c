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

/*! \file o_attrib.c
 *  \brief utility functions for attributes
 *
 *  Attributes are normal text objects. An attribute is a text object
 *  that has a text string that is delimited by an equal "=" character.
 *  The part before the equal character is called <b>name</b> the
 *  part of the string behind the equal character is called <b>value</b>
 *
 *  Attributes are attached to OBJECTs (st_object). Each attribute has
 *  a reference to the object it is attached to. Each object that has
 *  attributes has a list of pionters to its attributes.
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

#include "libgeda_priv.h"


/*! \brief Add an attribute to an existing attribute list.
 *  \par Function Description
 *  Add an attribute to an existing attribute list.
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  object     The OBJECT we're adding the attribute to.
 *  \param [in]  item       The item you want to add as an attribute.
 *  \return nothing.
 */
void o_attrib_add(TOPLEVEL *toplevel, OBJECT *object, OBJECT *item)
{
  /* Add link from item to attrib listing */
  item->attached_to = object;
  object->attribs = g_list_append (object->attribs, item);
}


/*! \brief Check whether a attrib is attached to another object
 *  \par Function Description
 *  This function checks whether the object \a attrib is attached to
 *  the \a object.
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  attrib     The attribute to be checket.
 *  \param [in]  object     The object where you want to add item as an attribute.
 *  \return TRUE if attrib is an attribute of object, FALSE otherwise
 */
gboolean o_attrib_is_attached (TOPLEVEL *toplevel,
                               OBJECT *attrib, OBJECT *object)
{
  if (attrib == NULL || object == NULL)
    return FALSE;

  if (attrib->attached_to == object)
    return TRUE;

  return FALSE;
}


/*! \brief Attach existing attribute to an object.
 *  \par Function Description
 *  Attach existing attribute to an object.
 *
 *  \param [in]  toplevel     The TOPLEVEL object.
 *  \param [in]  attrib       The attribute to be added.
 *  \param [out] object       The object where you want to add item as an attribute.
 *  \param [in]  set_color    Whether or not we should set the new attribute's color.
 */
void o_attrib_attach (TOPLEVEL *toplevel, OBJECT *attrib, OBJECT *object,
                      int set_color)
{
  g_return_if_fail (attrib != NULL);
  g_return_if_fail (object != NULL);

  /* is the object already part of the list ? */
  if (g_list_find (object->attribs, attrib)) {
    g_warning (_("Attribute [%s] already attached\n"), attrib->text->string);
    return;
  }

  if (attrib->type != OBJ_TEXT) {
    g_warning (_("Attempt to attach non text item as an attribute!\n"));
    return;
  }

  if (attrib->attached_to != NULL) {
    g_warning (_("Attempt to attach attribute [%s] to more than one object\n"),
                attrib->text->string);
    return;
  }

  o_attrib_add (toplevel, object, attrib);

  if (set_color)
    o_set_color (toplevel, attrib, ATTRIBUTE_COLOR);
}


/*! \brief Attach list of existing attributes to an object.
 *  \par Function Description
 *  Attach list of existing attributes to an object.
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  attr_list  The list of attributes to be added.
 *  \param [out] object     The object where you want to add item as an attribute.
 *  \param [in]  set_color    Whether or not we should set the new attribute's color.
 */
void o_attrib_attach_list (TOPLEVEL *toplevel,
                           GList *attr_list, OBJECT *object, int set_color)
{
  GList *iter;

  for (iter = attr_list; iter != NULL; iter = g_list_next (iter))
    o_attrib_attach (toplevel, iter->data, object, set_color);
}


/*! \brief Detach all attribute items in a list.
 *  \par Function Description
 *  Detach all attributes from an object.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object    The object whos attributes to detach.
 */
void o_attrib_detach_all(TOPLEVEL *toplevel, OBJECT *object)
{
  OBJECT *a_current;
  GList *a_iter;

  for (a_iter = object->attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = a_iter->data;

    a_current->attached_to = NULL;
    o_set_color (toplevel, a_current, DETACHED_ATTRIBUTE_COLOR);
  }

  g_list_free (object->attribs);
  object->attribs = NULL;
}

/*! \brief Print all attributes to a Postscript document.
 *  \par Function Description
 *  Print all attributes to a Postscript document.
 *
 *  \param [in] attributes  List of attributes to print.
 */
void o_attrib_print(GList *attributes)
{
  OBJECT *a_current;
  GList *a_iter;

  a_iter = attributes;

  while (a_iter != NULL) {
    a_current = a_iter->data;
    printf("Attribute points to: %s\n", a_current->name);
    if (a_current->text) {
      printf("\tText is: %s\n", a_current->text->string);
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
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] list      The attribute list to remove attribute from.
 *  \param [in] remove    The OBJECT to remove from list.
 */
void o_attrib_remove(TOPLEVEL *toplevel, GList **list, OBJECT *remove)
{
  g_return_if_fail (remove != NULL);

  remove->attached_to = NULL;

  *list = g_list_remove (*list, remove);
}

/*! \brief Read attributes from a buffer.
 *  \par Function Description
 *  Read attributes from a TextBuffer.
 *
 *  \param [in]  toplevel               The TOPLEVEL object.
 *  \param [in]  object_to_get_attribs  Object which gets these attribs.
 *  \param [in]  tb                     The text buffer to read from.
 *  \param [in]  release_ver            libgeda release version number.
 *  \param [in]  fileformat_ver         file format version number.
 *  \return GList of attributes read, or NULL on error.
 */
GList *o_read_attribs (TOPLEVEL *toplevel,
                       OBJECT *object_to_get_attribs,
                       TextBuffer *tb,
                       unsigned int release_ver, unsigned int fileformat_ver, GError ** err)
{
  GList *object_list = NULL;
  OBJECT *new_obj;
  const char *line = NULL;
  char objtype;
  int ATTACH=FALSE;

  while (1) {

    line = s_textbuffer_next_line (tb);
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);
    switch (objtype) {

      case(OBJ_LINE):
        if ((new_obj = o_line_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;


      case(OBJ_NET):
        if ((new_obj = o_net_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BUS):
        if ((new_obj = o_bus_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BOX):
        if ((new_obj = o_box_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        if ((new_obj = o_circle_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        if ((new_obj = o_complex_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PATH):
        new_obj = o_path_read (toplevel, line, tb, release_ver, fileformat_ver, err);
        if (new_obj == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PIN):
        if ((new_obj = o_pin_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_ARC):
        if ((new_obj = o_arc_read (toplevel, line, release_ver, fileformat_ver, err)) == NULL)
          goto error;
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_TEXT):
        new_obj = o_text_read (toplevel, line, tb, release_ver, fileformat_ver, err);
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
      o_attrib_attach (toplevel, new_obj, object_to_get_attribs, FALSE);
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
  geda_object_list_delete (toplevel, object_list);
  return NULL;
}


/*! \brief Get name and value from an attribute 'name=value' string.
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


/*! \brief Get name and value from an attribute OBJECT
 *  \par Function Description
 *  See o_attrib_string_get_name_value() for more details
 *
 *  \param [in]  attrib     The attribute OBJECT whos name/value to return.
 *  \param [out] name_ptr   The return location for the name, or NULL.
 *  \param [out] value_ptr  The return location for the value, or NULL.
 *  \return TRUE on success, FALSE otherwise.
 */
gboolean
o_attrib_get_name_value (const OBJECT *attrib,
                         gchar **name_ptr,
                         gchar **value_ptr)
{
  g_return_val_if_fail (attrib->type == OBJ_TEXT, FALSE);

  return o_attrib_string_get_name_value (attrib->text->string,
                                         name_ptr, value_ptr);
}


/*! \brief Find all floating attributes in the given object list.
 *  \par Function Description
 *  Find all floating attributes in the given object list.
 *
 *  \param [in] list  GList of OBJECTs to search for floating attributes.
 *  \return GList of floating attributes from the input list
 *
 *  \warning
 *  Caller must g_list_free returned list.
 */
GList *o_attrib_find_floating_attribs (const GList *list)
{
  GList *floating_attributes = NULL;
  const GList *iter;
  OBJECT *o_current;

  for (iter = list; iter != NULL; iter = g_list_next (iter)) {
    o_current = iter->data;

    /* Skip non text objects, attached attributes and text which doesn't
     * constitute a valid attributes (e.g. general text placed on the page)
     */
    if (o_current->type == OBJ_TEXT &&
        o_current->attached_to == NULL &&
        o_attrib_get_name_value (o_current, NULL, NULL)) {

      floating_attributes = g_list_prepend (floating_attributes, o_current);
    }
  }

  return g_list_reverse (floating_attributes);
}


/*! \brief Find an attribute in a list.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     GList of attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] count    Which occurance to return.
 *  \return The n'th attribute object in the given list with the given name.
 */
OBJECT *o_attrib_find_attrib_by_name (const GList *list, char *name, int count)
{
  OBJECT *a_current;
  const GList *iter;
  char *found_name;
  int internal_counter = 0;

  for (iter = list; iter != NULL; iter = g_list_next (iter)) {
    a_current = iter->data;

    g_return_val_if_fail (a_current->type == OBJ_TEXT, NULL);

    if (!o_attrib_get_name_value (a_current, &found_name, NULL))
      continue;

    if (strcmp (name, found_name) == 0) {
      if (internal_counter == count) {
        g_free (found_name);
        return a_current;
      }
      internal_counter++;
    }

    g_free (found_name);
  }

  return NULL;
}


/*! \brief Search attribute list by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     GList of attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 */
static char *o_attrib_search_attrib_list_by_name (const GList *list, char *name, int counter)
{
  OBJECT *attrib;
  char *value = NULL;

  attrib = o_attrib_find_attrib_by_name (list, name, counter);

  if (attrib != NULL)
    o_attrib_get_name_value (attrib, NULL, &value);

  return value;
}


/*! \brief Search floating attribute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     GList of OBJECTs to search for floating attributes.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_floating_attribs_by_name (const GList *list, char *name, int counter)
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
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] object   The OBJECT whos attached attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_attached_attribs_by_name (OBJECT *object, char *name, int counter)
{
  return o_attrib_search_attrib_list_by_name (object->attribs, name, counter);
}


/*! \brief Search inherited attribute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] object   The OBJECT whos inherited attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_inherited_attribs_by_name (OBJECT *object, char *name, int counter)
{
  g_return_val_if_fail (object->type == OBJ_COMPLEX ||
                        object->type == OBJ_PLACEHOLDER, NULL);

  return o_attrib_search_floating_attribs_by_name (object->complex->prim_objs, name, counter);
}


/*! \brief Search attributes of object by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] object   OBJECT who's attributes to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_object_attribs_by_name (OBJECT *object, char *name, int counter)
{
  char *result;
  GList *attributes;

  attributes = o_attrib_return_attribs (object);
  result = o_attrib_search_attrib_list_by_name (attributes, name, counter);
  g_list_free (attributes);

  return result;
}


/*! \brief Get all attached attributes of the specified OBJECT.
 *  \par Function Description
 *  This function returns all attributes of the specified object.
 *
 *  The returned GList should be freed using the #g_list_free().
 *
 *  This function aggregates the attached and inherited attributes
 *  belonging to a given OBJECT. (inherited attributes are those
 *  which live as toplevel un-attached attributes inside in a
 *  complex OBJECT's prim_objs).
 *
 *  \param [in] object       OBJECT whos attributes to return.
 *  \return A GList of attributes belinging to the passed object.
 */
GList * o_attrib_return_attribs (OBJECT *object)
{
  GList *attribs = NULL;
  GList *inherited_attribs;
  OBJECT *a_current;
  GList *a_iter;

  g_return_val_if_fail (object != NULL, NULL);

  /* Directly attached attributes */
  for (a_iter = object->attribs; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = a_iter->data;

    if (a_current->type != OBJ_TEXT)
      continue;

    /* Don't add invalid attributes to the list */
    if (!o_attrib_get_name_value (a_current, NULL, NULL))
      continue;

    attribs = g_list_prepend (attribs, a_current);
  }

  attribs = g_list_reverse (attribs);

  /* Inherited attributes (inside complex objects) */
  if (object->type == OBJ_COMPLEX ||
      object->type == OBJ_PLACEHOLDER) {

    inherited_attribs =
      o_attrib_find_floating_attribs (object->complex->prim_objs);

    attribs = g_list_concat (attribs, inherited_attribs);
  }

  return attribs;
}


/*! \brief Query whether a given attribute OBJECT is "inherited"
 *  \par Function Description
 *  This function returns TRUE if the given attribute OBJECT is a
 *  toplevel un-attached attribute inside a complex's prim_objs.
 *
 *  \param [in] attrib       OBJECT who's status to query.
 *  \return TRUE if the given attribute is inside a symbol
 */
int o_attrib_is_inherited (const OBJECT *attrib)
{
  return (attrib->attached_to == NULL &&
          attrib->parent != NULL);
}
