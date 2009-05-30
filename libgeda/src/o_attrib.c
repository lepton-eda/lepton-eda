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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! Basic string splitting delimiters */
#define DELIMITERS ",; "


/*! \brief Set attribute color
 *  \par Function Description
 *  This function sets an attribute object to the given color.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] attrib    The attribute OBJECT to set the colors of.
 *  \param [in]     color     The color to set.
 *
 */
static void o_attrib_set_color(TOPLEVEL *toplevel, OBJECT *attrib, int color)
{
  g_return_if_fail (attrib->type == OBJ_TEXT);

  if (attrib->saved_color == -1) {
    o_complex_set_color (attrib->text->prim_objs, color);
    attrib->color = color;
  } else {
    o_complex_set_saved_color_only (attrib->text->prim_objs, color);
    attrib->saved_color = color;
  }
}


/*! \brief Add an attribute to an existing attribute list.
 *  \par Function Description
 *  Add an attribute to an existing attribute list.
 *
 *  \param [in]  toplevel   The TOPLEVEL object.
 *  \param [in]  object     The OBJECT we're adding the attribute to.
 *  \param [in]  item       The item you want to add as an attribute.
 *  \return The new head of the attributes list.
 */
void o_attrib_add(TOPLEVEL *toplevel, OBJECT *object, OBJECT *item)
{
  /* Add link from item to attrib listing */
  item->attached_to = object;
  object->attribs = g_list_append (object->attribs, item);
}

/*! \brief Free single item in attribute list.
 *  \par Function Description
 *  Free single item in attribute list.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] current   OBJECT pointer to remove attribute-ness from.
 *
 *  \note
 *  this routine is only called from free_all
 */
void o_attrib_free(TOPLEVEL *toplevel, OBJECT *current)
{
  if (current == NULL)
    return;

  /* \todo this makes me nervous... very nervous */
  current->attached_to=NULL;
  o_attrib_set_color (toplevel, current, DETACHED_ATTRIBUTE_COLOR);
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
    g_warning ("Attribute [%s] already attached\n", attrib->text->string);
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
    o_attrib_set_color (toplevel, attrib, ATTRIBUTE_COLOR);

  /* can't do this here since just selecting something */
  /* will cause this to be set */
  /* toplevel->page_current->CHANGED=1;*/
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


/*! \brief Free all attribute items in a list.
 *  \par Function Description
 *  Free all attribute items in a list.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] list       The list to free.
 *
 */
void o_attrib_free_all(TOPLEVEL *toplevel, GList *list)
{
  OBJECT *a_current;
  GList *a_iter;

  a_iter = list;

  while (a_iter != NULL) {
    a_current = a_iter->data;
    o_attrib_free(toplevel, a_current);
    a_iter = g_list_next (a_iter);
  }
  g_list_free (list);
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
 *  \param [in] list    The attribute list to remove attribute from.
 *  \param [in] remove  The OBJECT to remove from list.
 *
 */
void o_attrib_remove(GList **list, OBJECT *remove)
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
 *  \param [out] list                   Storage for attributes.
 *  \param [in]  object_to_get_attribs  Object which gets these attribs.
 *  \param [in]  tb                     The text buffer to read from.
 *  \param [in]  release_ver            libgeda release version number.
 *  \param [in]  fileformat_ver         file format version number.
 *  \return GList of attributes read.
 */
GList *o_read_attribs (TOPLEVEL *toplevel,
                       GList *list,
                       OBJECT *object_to_get_attribs,
                       TextBuffer *tb,
                       unsigned int release_ver, unsigned int fileformat_ver)
{
  GList *object_list;
  OBJECT *new_obj;
  char *line = NULL;
  char objtype;
  int ATTACH=FALSE;

  object_list = g_list_reverse (list);

  while (1) {

    line = s_textbuffer_next_line (tb);
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);
    switch (objtype) {

      case(OBJ_LINE):
        new_obj = o_line_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;


      case(OBJ_NET):
        new_obj = o_net_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BUS):
        new_obj = o_bus_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_BOX):
        new_obj = o_box_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_CIRCLE):
        new_obj = o_circle_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        new_obj = o_complex_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PATH):
        line = g_strdup (line);
        new_obj = o_path_read (toplevel, line, tb, release_ver, fileformat_ver);
        g_free (line);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_PIN):
        new_obj = o_pin_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_ARC):
        new_obj = o_arc_read (toplevel, line, release_ver, fileformat_ver);
        object_list = g_list_prepend (object_list, new_obj);
        break;

      case(OBJ_TEXT):
        line = g_strdup (line);
        new_obj = o_text_read (toplevel, line, tb, release_ver, fileformat_ver);
        g_free (line);
        object_list = g_list_prepend (object_list, new_obj);
        ATTACH=TRUE;

        break;

      case(ENDATTACH_ATTR): 
        object_list = g_list_reverse (object_list);
        return(object_list);
        break;

    }

    if (ATTACH) {
      o_attrib_attach (toplevel, new_obj, object_to_get_attribs, FALSE);
      ATTACH=FALSE;
    } else {
      fprintf(stderr, "Tried to attach a non-text item as an attribute\n");
    }
  }
  object_list = g_list_reverse (object_list);
  return(object_list);
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
o_attrib_get_name_value (const gchar *string, gchar **name_ptr, gchar **value_ptr)
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


/*! \brief Search for attibute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  \warning
 *  The list is the top level list. Do not pass it an object_list list
 *  unless you know what you are doing.
 *  
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     GList to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_name (const GList *list, char *name, int counter)
{
  OBJECT *o_current;
  OBJECT *a_current;
  GList *a_iter;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;
  const GList *iter;

  iter = list;

  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    if (o_current->attribs != NULL) {
      a_iter = o_current->attribs;
      while(a_iter != NULL) {
        a_current = a_iter->data;
        if (a_current->type == OBJ_TEXT) {
          val = o_attrib_get_name_value(a_current->text->string,
                                        &found_name, &found_value);

          if (val) {
            if (strcmp(name, found_name) == 0) {
              if (counter != internal_counter) {
                internal_counter++;
              } else {
                return_string = g_strdup (found_value);
                g_free(found_name);
                g_free(found_value);
                return(return_string);
              }
            }
            if (found_name) { g_free(found_name); found_name = NULL; }
            if (found_value) { g_free(found_value); found_value = NULL; }
          }

#if DEBUG
          printf("0 _%s_\n", a_current->text->string);
          printf("1 _%s_\n", found_name);
          printf("2 _%s_\n", found_value);
#endif
        }
        a_iter = g_list_next (a_iter);
      }
    }

    /* search for attributes outside */

    if (o_current->type == OBJ_TEXT) {
      g_free(found_name);
      g_free(found_value);
      val = o_attrib_get_name_value(o_current->text->string, 
                                    &found_name, &found_value);
      if (val) {
        if (strcmp(name, found_name) == 0) {
          if (counter != internal_counter) {
            internal_counter++;	
          } else {
            return_string = g_strdup (found_value);
	    g_free(found_name);
	    g_free(found_value);
            return(return_string);
          }
        }
	if (found_name) { g_free(found_name); found_name = NULL; }
	if (found_value) { g_free(found_value); found_value = NULL; }
      }	
    }

    iter = g_list_next (iter);
  }
	
  g_free(found_name);
  g_free(found_value);
  return (NULL);
} 

/*! \brief Search OBJECT list for text string.
 *  \par Function Description
 *  Given an OBJECT list (i.e. OBJECTs on schematic page or
 *  inside symbol), search for the attribute called out in
 *  "string".  It iterates over all objects in the OBJECT list
 *  and dives into the attached attribute OBJECT list for
 *  each OBJECT if it finds one.
 *  Inside the attribute OBJECT list it looks for an attached text
 *  attribute matching "string".  It returns the
 *  pointer to the associated OBJECT if found.  If the attribute
 *  string is not found in the attribute OBJECT list, then the fcn
 *  looks on the OBJECT itself for the attribute.  Then it
 *  iterates to the next OBJECT.
 *
 *  \warning
 *  The list is the top level list. Do not pass it an object_list list
 *  unless you know what you are doing.
 *  
 *  \param [in] list    OBJECT list to search.
 *  \param [in] string  Character string to search for.
 *  \return A matching OBJECT if found, NULL otherwise.
 */
OBJECT *o_attrib_search_string_list (GList *list, char *string)
{
  OBJECT *o_current;
  OBJECT *a_current;
  GList *a_iter;
  GList *o_iter;

  o_iter = list;

  while (o_iter != NULL) {
    o_current = o_iter->data;
    /* first search attribute list */
    if (o_current->attribs != NULL) {
      a_iter = o_current->attribs;

      while(a_iter != NULL) {
        a_current = a_iter->data;
        if (a_current->type == OBJ_TEXT) {
#if DEBUG
    printf("libgeda:o_attrib.c:o_attrib_search_string_list --");
    printf("found OBJ_TEXT, string = %s\n", found->text->string);
#endif
          if (strcmp(string, a_current->text->string) == 0) {
            return a_current;
          }
        }
        a_iter = g_list_next (a_iter);
      }
    }

    /* search for attributes outside (ie the actual object) */
    if (o_current->type == OBJ_TEXT) {
      if (strcmp(string, o_current->text->string) == 0) {
        return(o_current);
      }
    }
    
    o_iter = g_list_next (o_iter);
  }

  return (NULL);
} 

/*! \brief Search list for partial string match.
 *  \par Function Description
 *  Search list for partial string match.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] object      The OBJECT list to search.
 *  \param [in] search_for  Partial character string to search for.
 *  \param [in] counter     Which occurance to return.
 *  \return Matching object value if found, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_string_partial(OBJECT *object, char *search_for,
				     int counter) 
{
  OBJECT *o_current;
  int val;
  int internal_counter=0;
  char *found_value = NULL;
  char *return_string = NULL;

  o_current = object;

  if (o_current == NULL) {
    return(NULL);
  }

  if (o_current->type == OBJ_TEXT) {
    if (strstr(o_current->text->string, search_for)) {
      if (counter != internal_counter) {
        internal_counter++;	
      } else {
        val = o_attrib_get_name_value(o_current->text->string, 
                                      NULL, &found_value);
        if (val) {
          return_string = g_strdup(found_value);
	  g_free(found_value);
	  return(return_string);
        }
      }
    }
  }	
	
  g_free(found_value);
  return (NULL);
} 

/*! \brief Check if object matches string.
 *  \par Function Description
 *  This function will check if the text->string value of
 *  the passed OBJECT matches the <B>search_for</B> parameter.
 *  If not, it then searches the object's attribute list
 *  (if it has one.)
 *  Only this single OBJECT and its attribute list is
 *  checked, and other OBJECTs on the page are not checked.
 *  \param [in] object      The OBJECT to compare.
 *  \param [in] search_for  Character string to compare against.  
 *  Usually name=value
 *  \return The OBJECT passed in <B>object</B> parameter, NULL otherwise.
 */
OBJECT *o_attrib_search_string_single(OBJECT *object, char *search_for)
{
  OBJECT *o_current;
  OBJECT *a_current;
  GList *a_iter;

  o_current = object;

#if DEBUG
  printf("In libgeda:o_attrib.c:o_attrib_search_string_single\n");
  printf("   Examining object->name = %s\n", object->name);
#endif

  if (o_current == NULL) {
    return(NULL);
  }

  /* First check to see if this OBJECT itself is the attribute we want */
  if (o_current->type == OBJ_TEXT) {
    if (strcmp(o_current->text->string, search_for) == 0) {
#if DEBUG
      printf("   This object is searched-for attribute\n");
#endif
      return(o_current);
    }
  }

  /* Next check to see if this OBJECT has an attribute list we */
  /* can search.  If not return NULL.  If so, search it. */
  if (o_current->attribs == NULL) 
    return(NULL);

  a_iter = o_current->attribs;
  while(a_iter != NULL) {
    a_current = a_iter->data;
    if (a_current->type == OBJ_TEXT) {
      if(strcmp(a_current->text->string, search_for) == 0) {
        return a_current;
      }
    }
    a_iter = g_list_next (a_iter);
  }

  return (NULL);
} 

/*! \brief Search for attribute by value and name.
 *  \par Function Description
 *  Search for attribute by value and name.
 *  
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  The value is the primary search key, but name is checked before
 *  an OBJECT is returned to ensure the correct OBJECT has been found.
 *
 *  \param [in] list     The attribute OBJECT list to search.
 *  \param [in] value    Character string with value to search for.
 *  \param [in] name     Character string with name to compare.
 *  \param [in] counter  Which occurance to return.
 *  \return The attribute OBJECT if found, NULL otherwise.
 *
 */
OBJECT *o_attrib_search_attrib_value(GList *list, char *value, char *name,
				     int counter) 
{
  OBJECT *a_current;
  GList *a_iter;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;

  a_iter = list;
	
  if (!value) 
  return(NULL);

  if (!name) 
  return(NULL);

  while(a_iter != NULL) {
    a_current = a_iter->data;
    if (a_current->type == OBJ_TEXT) {
      val = o_attrib_get_name_value(a_current->text->string,
                                    &found_name, &found_value);

      if (val) {
#if DEBUG
        printf("found value: %s\n", found_value);
        printf("looking for: %s\n", value);
#endif
        if (strcmp(value, found_value) == 0) {
          if (counter != internal_counter) {
            internal_counter++;
          } else {
            if (strstr(found_name, name)) {
              g_free(found_name);
              g_free(found_value);
              return a_current;
            }
          }
        }
        if (found_name) { g_free(found_name); found_name = NULL; }
        if (found_value) { g_free(found_value); found_value = NULL; }
      }

    }
    a_iter = g_list_next (a_iter);
  }

  g_free(found_name);
  g_free(found_value);
  return (NULL);
} 

/*! \brief Search for an attribute by name.
 *  \par Function Description
 *  Search for an attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     attribute OBJECT list to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *
o_attrib_search_attrib_name(GList *list, char *name, int counter)
{
  OBJECT *a_current;
  GList *a_iter;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;

  a_iter = list;

  while(a_iter != NULL) {
    a_current = a_iter->data;
    if (a_current->type == OBJ_TEXT) {
      val = o_attrib_get_name_value(a_current->text->string,
                                    &found_name, &found_value);

      if (val) {
#if DEBUG
        printf("found name: %s\n", found_name);
        printf("looking for: %s\n", name);
#endif
        if (strcmp(name, found_name) == 0) {
          if (counter != internal_counter) {
            internal_counter++;
          } else {
            return_string = g_strdup (found_value);
            g_free(found_name);
            g_free(found_value);
            return(return_string);
          }
        }
        if (found_name) { g_free(found_name); found_name = NULL; }
        if (found_value) { g_free(found_value); found_value = NULL; }
      }
    }
    a_iter = g_list_next (a_iter);
  }

  g_free(found_name);
  g_free(found_value);
  return (NULL);
} 

/*! \brief Search TOPLEVEL attributes.
 *  \par Function Description
 *  This function should only be used to search for TOPLEVEL attributes.
 *  \warning
 *  The list is the top level list. Do not pass it an object_list list
 *  unless you know what you are doing.
 *  
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 * 
 *  \param [in] list     The GList to search (TOPLEVEL only).
 *  \param [in] name     Character string of attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_toplevel (const GList *list, char *name, int counter)
{
  OBJECT *o_current;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;
  const GList *iter;

  iter = list;

  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    /* search for attributes outside */

    if (o_current->type == OBJ_TEXT) {
      val = o_attrib_get_name_value(o_current->text->string, 
                                    &found_name, &found_value);
      if (val) {
        if (strcmp(name, found_name) == 0) {
          if (counter != internal_counter) {
            internal_counter++;	
          } else {
            return_string = g_strdup (found_value);
	    g_free(found_name);
	    g_free(found_value);
            return(return_string);
          }
        }
	if (found_name) { g_free(found_name); found_name = NULL; }
	if (found_value) { g_free(found_value); found_value = NULL; }
      }	
    }

    iter = g_list_next (iter);
  }
	
  g_free(found_name);
  g_free(found_value);
  return (NULL);
} 


/*! \brief Search for first occurance of a named attribute.
 *  \par Function Description
 *  Search for first occurance of a named attribute.
 *
 *  \param [in]  object        The OBJECT list to search.
 *  \param [in]  name          Character string of attribute name to search for.
 *  \param [out] return_found  Contains attribute OBJECT if found, NULL otherwise.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_name_single(OBJECT *object, char *name,
				  OBJECT **return_found) 
{
  OBJECT *o_current;
  OBJECT *a_current;
  GList *a_iter;
  int val;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;

  o_current = object;

  if (o_current == NULL) {
    return(NULL);
  }

  if (o_current->attribs != NULL) {
    a_iter = o_current->attribs;

    while(a_iter != NULL) {
      a_current = a_iter->data;
      if (a_current->type == OBJ_TEXT) {
        val = o_attrib_get_name_value(a_current->text->string,
                                      &found_name, &found_value);

        if (val) {
          if (strcmp(name, found_name) == 0) {
            return_string = g_strdup (found_value);
            if (return_found) {
              *return_found = a_current;
            }
            g_free(found_name);
            g_free(found_value);
            return(return_string);
          }
          if (found_name) { g_free(found_name); found_name = NULL; }
          if (found_value) { g_free(found_value); found_value = NULL; }
        }

#if DEBUG
        printf("0 _%s_\n", found->text->string);
        printf("1 _%s_\n", found_name);
        printf("2 _%s_\n", found_value);
#endif
      }
      a_iter = g_list_next (a_iter);
    }
  }
  /* search for attributes outside */

  if (o_current->type == OBJ_TEXT) {
    g_free(found_name);
    g_free(found_value);
    val = o_attrib_get_name_value(o_current->text->string, 
                                  &found_name, &found_value);

    if (val) {
      if (strcmp(name, found_name) == 0) {
        return_string = g_strdup (found_value);
        if (return_found) {
          *return_found = o_current;
        }
	g_free(found_name);
	g_free(found_value);
        return(return_string);
      }
      if (found_name) { g_free(found_name); found_name = NULL; }
      if (found_value) { g_free(found_value); found_value = NULL; }
    }
  }

  if (return_found) {
    *return_found = NULL;
  }
  
  g_free(found_name);
  g_free(found_value);
  return (NULL);
}

/*! \brief Search for N'th occurance of a named attribute.
 *  \par Function Description
 *  Search for N'th occurance of a named attribute.
 *
 *  \param [in] object   The OBJECT list to search.
 *  \param [in] name     Character string of attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *  
 *  \warning
 *  Caller must g_free returned character string.
 */
/* be sure caller free's return value */
/* this function is like above, except that it returns the n'th occurance */
/* of the attribute.  counter starts counting at zero */
char *o_attrib_search_name_single_count(OBJECT *object, char *name, 
					int counter) 
{
  OBJECT *o_current;
  OBJECT *a_current;
  GList *a_iter;
  int val;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;
  int internal_counter=0;

  o_current = object;

  if (o_current == NULL) {
    return(NULL);
  }

  a_iter = o_current->attribs;

  while(a_iter != NULL) {
    a_current = a_iter->data;
    if (a_current->type == OBJ_TEXT) {
      val = o_attrib_get_name_value(a_current->text->string,
                                    &found_name, &found_value);

      if (val) {
        if (strcmp(name, found_name) == 0) {
          if (counter != internal_counter) {
            internal_counter++;
          } else {
            return_string = g_strdup (found_value);
            g_free(found_name);
            g_free(found_value);
            return(return_string);
          }
        }
        if (found_name) { g_free(found_name); found_name = NULL; }
        if (found_value) { g_free(found_value); found_value = NULL; }
      }

#if DEBUG
      printf("0 _%s_\n", a_current->text->string);
      printf("1 _%s_\n", found_name);
      printf("2 _%s_\n", found_value);
#endif
    }
    a_iter = g_list_next (a_iter);
  }

  /* search for attributes outside */

  if (o_current->type == OBJ_TEXT) {
    g_free(found_name);
    g_free(found_value);
    val = o_attrib_get_name_value(o_current->text->string, 
                                  &found_name, &found_value);

    if (val) {
      if (strcmp(name, found_name) == 0) {
        if (counter != internal_counter) {
          internal_counter++;
        } else {
          return_string = g_strdup (found_value);
	  g_free(found_name);
	  g_free(found_value);
          return(return_string);
        }
      }
      if (found_name) { g_free(found_name); found_name = NULL; }
      if (found_value) { g_free(found_value); found_value = NULL; }
    }
  }	
  
  g_free(found_name);
  g_free(found_value);
  return (NULL);
}

/*! \brief Search for slot attribute.
 *  \par Function Description
 *  Search for slot attribute.
 *
 *  \param [in] object        OBJECT list to search.
 *  \param [in] return_found  slot attribute if found, NULL otherwise.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string
 */
char *o_attrib_search_slot(OBJECT *object, OBJECT **return_found)
{
  char *return_value;

  /* search for default value attribute buried inside the complex */
  return_value = o_attrib_search_name_single(object, "slot", return_found);

  /* I'm confused here does the next if get ever called? */
  if (return_value) {
    return(return_value);
  }

  if (return_found) {
    *return_found = NULL;
  }
  return(NULL);
}

/*! \brief Search for numslots attribute.
 *  \par Function Description
 *  Search for numslots attribute.
 *
 *  \param [in] object        OBJECT to search.
 *  \param [in] return_found  numslots attribute if found, NULL otherwise.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \note
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_numslots(OBJECT *object, OBJECT **return_found)
{
  char *return_value = NULL;

  /* search for numslots attribute buried inside the complex */
  if (object->type == OBJ_COMPLEX) {
    return_value = o_attrib_search_name(object->complex->prim_objs, 
					"numslots", 0);
  }

  if (return_value) {
    return(return_value);
  }

  if (return_found) {
    *return_found = NULL;
  }
  return(NULL);
}

/*! \brief Search for default slot attribute.
 *  \par Function Description
 *  Search for default slot attribute.
 *
 *  \param [in] object  OBJECT list to search.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_default_slot(OBJECT *object)
{
  char *return_value;

  /* search for default value attribute buried inside the complex */
  return_value = o_attrib_search_name(object->complex->prim_objs, 
                                      "slot", 0);

  if (return_value) {
    return(return_value);
  }

  return(NULL);
}

/*! \brief Search pinseq attribute.
 *  \par Function Description
 *  Given list of objects (generally a pin with attached attribs), 
 *  and a pinnumber, 
 *  search for and return pinseq= attrib (object).
 *
 *  \param [in] list        GList list to search.
 *  \param [in] pin_number  pin number to search for.
 *  \return OBJECT containing pinseq data, NULL otherwise.
 */
OBJECT *o_attrib_search_pinseq (GList *list, int pin_number)
{
  OBJECT *pinseq_text_object;
  char *search_for;

  search_for = g_strdup_printf ("pinseq=%d", pin_number);
  pinseq_text_object = o_attrib_search_string_list(list, search_for);
  g_free(search_for);
  
  if (pinseq_text_object && pinseq_text_object->attached_to) {
    return pinseq_text_object->attached_to;
  }
  
  return(NULL);
}

/*! \brief Search for slotdef attribute.
 *  \par Function Description
 *  Search for slotdef attribute.
 *
 *  \param [in] object      The OBJECT list to search.
 *  \param [in] slotnumber  The slot number to search for.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_slotdef(OBJECT *object, int slotnumber)
{
  char *return_value=NULL;
  char *search_for=NULL;
  OBJECT *o_current;
  GList *iter;

  search_for = g_strdup_printf ("slotdef=%d:", slotnumber);

  iter = object->complex->prim_objs;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    return_value = o_attrib_search_string_partial(o_current, search_for, 0);
    if (return_value) {
      break;
    }
    iter = g_list_next (iter);
  }
  g_free(search_for);

  if (return_value) {
    return(return_value);
  }

  return(NULL);
}

/*! \brief Search for component.
 *  \par Function Description
 *  Search for component.
 *
 *  \param [in] object  The OBJECT list to search.
 *  \param [in] name    Character string containing component name to match.
 *  \return Character string with the component value, NULL otherwise.
 */
char *o_attrib_search_component(OBJECT *object, char *name)
{
  char *return_value = NULL;

  if (!name) {
    return(NULL);
  }

  if (object->type != OBJ_COMPLEX && object->type != OBJ_PLACEHOLDER) {
    return(NULL);
  }

  /* first look inside the complex object */
  return_value = o_attrib_search_name(object->complex->prim_objs, 
                                      name, 0);

  if (return_value) {
    return(return_value);
  }

  /* now look outside to see if it was attached externally */
  return_value = o_attrib_search_name_single(object, name, NULL);

  if (return_value) {
    return(return_value);
  }

  return(NULL);
}

/*! \brief Update all slot attributes in an object.
 *  \par Function Description
 *  Update pinnumber attributes in a graphic object.  
 *  The interesting case is where the object is an
 *  instantiation of a slotted part.  This means that 
 *  o_attrib_slot_update iterates through all pins
 *  found on object and sets the pinnumber= attrib
 *  on each.  This doesn't matter for non-slotted
 *  parts, but on slotted parts, this is what sets the 
 *  pinnumber= attribute on slots 2, 3, 4....
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object     The OBJECT to update.
 */
void o_attrib_slot_update(TOPLEVEL *toplevel, OBJECT *object)
{
  OBJECT *o_current;  /* o_current points to the sch level complex object */
  OBJECT *o_slot_attrib;
  OBJECT *o_pin_object;
  OBJECT *o_pinnum_object;
  char *string;
  char *slotdef;
  int slot;
  int slot_string;
  int pin_counter;    /* Internal pin counter private to this fcn. */
  char* current_pin;  /* text from slotdef= to be made into pinnumber= */
  char* cptr;         /* char pointer pointing to pinnumbers in slotdef=#:#,#,# string */

  o_current = object;
  /* For this particular graphic object (component instantiation) */
  /* get the slot number as a string */
  string = o_attrib_search_slot(o_current, &o_slot_attrib);

  if (!string) {
    /* "Did not find slot= attribute", this is true if 
     *  * there is no slot attribut
     *  * or the slot attribute was deleted and we have to assume to use the 
     *    first slot now
     */
    slot = 1;
    slot_string = 0;
  } 
  else {
    slot_string = 1;
    slot = atoi(string);
    g_free(string);
  }
  
  /* OK, now that we have the slot number, use it to get the */
  /* corresponding slotdef=#:#,#,# string.  */
  slotdef = o_attrib_search_slotdef(o_current, slot);
  
  if (!slotdef) { 
    if (slot_string) /* only an error if there's a slot string */
      s_log_message(_("Did not find slotdef=#:#,#,#... attribute\n"));
    return;
  }

  if (!strstr(slotdef, ":")) {
    /* Didn't find proper slotdef=#:... put warning into log */
    s_log_message(_("Improper slotdef syntax: missing \":\".\n"));
    g_free(slotdef);    
    return;
  }

  /* skip over slotdef number */
  /* slotdef is in the form #:#,#,# */
  /* this code skips first #: */
  cptr = slotdef;
  while (*cptr != '\0' && *cptr != ':') {
    cptr++;
  }
  cptr++; /* skip colon */

  if (*cptr == '\0') {
    s_log_message(_("Did not find proper slotdef=#:#,#,#... attribute\n"));
    g_free(slotdef);    
    return;
  }

  /* loop on all pins found in slotdef= attribute */
  pin_counter = 1;  /* internal pin_counter */
  /* get current pinnumber= from slotdef= attrib */
  current_pin = strtok(cptr, DELIMITERS); 
  while(current_pin != NULL) {
    /* get pin on this component with pinseq == pin_counter */
    o_pin_object = o_attrib_search_pinseq(o_current->complex->prim_objs, 
                                          pin_counter);

    if (o_pin_object) {
      /* Now rename pinnumber= attrib on this part with value found */
      /* in slotdef attribute  */
      string = o_attrib_search_name_single(o_pin_object, "pinnumber",
                                           &o_pinnum_object);
  
      if (string && o_pinnum_object && o_pinnum_object->type == OBJ_TEXT &&
          o_pinnum_object->text->string) {
        g_free(o_pinnum_object->text->string);

        o_pinnum_object->text->string = g_strdup_printf ("pinnumber=%s", current_pin);
        
        o_text_recreate(toplevel, o_pinnum_object);
      }
      g_free(string);

      pin_counter++;
    } else {
      s_log_message(_("component missing pinseq= attribute\n"));
    }
    
    current_pin = strtok(NULL, DELIMITERS);
  } 
  
  g_free(slotdef);
}

/*! \brief Copy attributes from OBJECT to OBJECT.
 *  \par Function Description
 *  This function will perform a slot copy of the <B>original</B> OBJECT
 *  to the <B>target</B> OBJECT.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  original   The original OBJECT to slot copy from.
 *  \param [out] target     The target OBJECT to slot copy to.
 */
void o_attrib_slot_copy(TOPLEVEL *toplevel, OBJECT *original, OBJECT *target)
{

  OBJECT *o_current;
  OBJECT *o_slot_attrib;
  OBJECT *o_pin_object;
  OBJECT *o_pinnum_object;
  char *string;
  char *slotdef;
  int slot;
  int pin_counter;
  char* current_pin;
  char* cptr;

  o_current = original;
	
  string = o_attrib_search_slot(o_current, &o_slot_attrib);
	
  if (!string) {
    /* s_log_message("Did not find slot= attribute\n"); */
    /* not a serious error */
    return;
  } 
  slot = atoi(string);
  g_free(string);
  
  slotdef = o_attrib_search_slotdef(o_current, slot);
 
  if (!slotdef) {
    s_log_message(_("Did not find slotdef=#:#,#,#... attribute\n"));
    return;
  }

  if (!strstr(slotdef, ":")) {
    /*! \todo didn't proper slotdef=#:... TODO into log*/
    return;
  }

  /* skip over slotdef number */
  /* slotdef is in the form #:#,#,# */
  /* this code skips first #: */
  cptr = slotdef;
  while (*cptr != '\0' && *cptr != ':') {
    cptr++;
  }
  cptr++; /* skip colon */

  if (*cptr == '\0') {
    s_log_message(_("Did not find proper slotdef=#:#,#,#... attribute\n"));
    return;
  }
  
  pin_counter = 1;
  current_pin = strtok(cptr, DELIMITERS);
  while(current_pin != NULL) {

    o_pin_object = o_attrib_search_pinseq(target->complex->prim_objs, 
                                          pin_counter);

    if (o_pin_object) {

      string = o_attrib_search_name_single(o_pin_object, "pinnumber",
                                           &o_pinnum_object);
  
      if (string && o_pinnum_object && o_pinnum_object->type == OBJ_TEXT &&
          o_pinnum_object->text->string) {

        g_free(string);
        g_free(o_pinnum_object->text->string);

        o_pinnum_object->text->string = g_strdup_printf ("pinnumber=%s", current_pin);
        
        o_text_recreate(toplevel, o_pinnum_object);
      }
      
      pin_counter++;
    } else {
      s_log_message(_("component missing pinseq= attribute\n"));
    }
    
    current_pin = strtok(NULL, DELIMITERS);
  } 
  
  g_free(slotdef);
}


/*! \brief Search for first TOPLEVEL attribute.
 *  \par Function Description
 *  This function searches all loaded pages for the first
 *  TOPLEVEL attribute found.
 *  The caller is responsible for freeing the returned value.
 *  See #o_attrib_search_toplevel() for other comments.
 *
 *  \param [in] page_list  Page list to search through.
 *  \param [in] name       Character string name to search for.
 *  \return Character string from the found attribute, NULL otherwise.
 */
char *o_attrib_search_toplevel_all(GedaPageList *page_list, char *name)
{
  const GList *iter;
  PAGE *p_current;
  char *ret_value=NULL;

  iter = geda_list_get_glist( page_list );

  while( iter != NULL ) {
    p_current = (PAGE *)iter->data;

    /* only look for first occurrance of the attribute */
    ret_value = o_attrib_search_toplevel (s_page_objects (p_current), name, 0);

    if (ret_value != NULL) {
      return(ret_value);
    }

    iter = g_list_next( iter );
  }

  return(NULL);
}

/*! \brief Get all attached attributes to specified OBJECT.
 *  \par Function Description
 *  This function returns all attached attribute objects to the
 *  specified object.
 *  The returned list is an array of objects and should be freed using the
 *  #o_attrib_free_returned() function.
 *  This function will only look for attached attributes and not
 *  unattached free floating attribs.
 *
 *  \param [in] object       OBJECT whos attributes to return.
 *  \return An array of objects that attached to object, NULL otherwise.
 */
OBJECT ** o_attrib_return_attribs(OBJECT *object)
{
  OBJECT **found_objects;
  int num_attribs=0;
  int i=0;
  OBJECT *a_current;
  GList *a_iter;

  if (!object || !object->attribs) {
    return(NULL);
  }

  /* first count the number of attribs */
  num_attribs = g_list_length (object->attribs);

  found_objects = (OBJECT **) g_malloc(sizeof(OBJECT *)*(num_attribs+1));

  /* now actually fill the array of objects */
  a_iter = object->attribs;
  while(a_iter != NULL) {
    a_current = a_iter->data;
    if (a_current->type == OBJ_TEXT &&
        a_current->text->string) {
      found_objects[i] = a_current;
      i++;
    }
    a_iter = g_list_next (a_iter);
  }

  found_objects[i] = NULL;

#if DEBUG 
  i=0;
  while(found_objects[i] != NULL) {
    /*for (i = 0 ; i < num_attribs; i++) {*/
    printf("%d : %s\n", i, found_objects[i]->text->string);
    i++;
  }
#endif

  return(found_objects);
}

/*! \brief Free attached attribute list.
 *  \par Function Description
 *  Free attached attribute list. Use only on a list created
 *  by #o_attrib_return_attribs().
 *
 *  \param [in] found_objects  List returned by #o_attrib_return_attribs().
 */
void o_attrib_free_returned(OBJECT **found_objects)
{
  int i=0;

  if (!found_objects) {
    return;
  }

  /* don't free the insides of found_objects, since the contents are */
  /* just pointers into the real object list */
  while(found_objects[i] != NULL) {
    found_objects[i] = NULL;
    i++;	
  }

  g_free(found_objects);
}
