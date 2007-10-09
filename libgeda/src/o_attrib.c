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
#include <math.h>

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "geda_list.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! Basic string splitting delimiters */
#define DELIMITERS ",; "

/*! \note
 *  No special type for attributes
 *  You can only edit text attributes
 *
 *  be sure in o_copy o_move o_delete you maintain the attributes
 *  delete is a bare, because you will have to unattach the other end
 *  and in o_save o_read as well
 *  and in o_select when selecting objects, select the attributes
 *
 *  \todo there needs to be a modifier (in struct.h, such as a flag) which
 *        signifies that this is an attribute (really? why?) 
 *
 *  \note
 *  return pointer from attrib_list
 */


/*! \brief Search for an item in an attribute list.
 *  \par Function Description
 *  Search for an item in an attribute list.
 *
 *  \param [in] list  ATTRIB pointer to the list to be searched.
 *  \param [in] item  item to be found.
 */
ATTRIB *o_attrib_search(ATTRIB *list, OBJECT *item)
{
  ATTRIB *a_current;

  if (item == NULL) {
    return(NULL);
  }

  a_current = list;

  while(a_current != NULL) {
    if (a_current->object != NULL) {
      if (item->sid == a_current->object->sid) {	
        return(a_current);	
      }
    }

    a_current = a_current->next;
  }

  return(NULL);
}

/*! \brief Get the last attribute in the list.
 *  \par Function Description
 *  Get the last attribute in the list.
 *
 *  \param [in] head  ATTRIB pointer to beginning of list.
 *  \return Returns an ATTRIB pointer to the last attribute in the list.
 */
ATTRIB *o_attrib_return_tail(ATTRIB *head) 
{
  ATTRIB *a_current=NULL;
  ATTRIB *current=NULL;

  a_current = head;
  while ( a_current != NULL ) { /* goto end of list */
    current = a_current;
    a_current = a_current->next;
  }
  return(current); 
}

/*! \brief Create an attribute list head item.
 *  \par Function Description
 *  Create an attribute list head item.
 *
 *  \param [in] parent  OBJECT pointer that will become the parent
 *                      of this new head item.
 *  \return Returns an ATTRIB pointer to the newly created head item.
 *
 *  \todo Rename this function to be consistant.
 */
ATTRIB *add_attrib_head(OBJECT *parent)
{
  ATTRIB *head = NULL;

  head = (ATTRIB *) g_malloc(sizeof(ATTRIB));
  head->next = NULL;

  /* highly experimental hack */
  head->object = parent; 
  head->copied_to = NULL;
  head->prev = NULL;

  /* \todo
   * why the grief? well everywhere a attribute is refered to
   * you have to skip over the head, you really ought to robustify
   * all references to this object pointer when talking to attributes
   * hack of course I think this is okay now though
   */

  return(head);
}

/*! \brief Add an attribute to an existing attribute list.
 *  \par Function Description
 *  Add an attribute to an existing attribute list.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] list_head  The list where you want to add item to.
 *  \param [in]     item       The item you want to add as an attribute.
 *  \return An ATTRIB pointer to the newly created attribute.
 */
ATTRIB *o_attrib_add(TOPLEVEL *toplevel, ATTRIB *list_head, OBJECT *item)
{
  ATTRIB *end = NULL;
  ATTRIB *new = NULL;

  /* get tail of list_head */
  end = o_attrib_return_tail(list_head);

  /* create an new st_attrib object */
  new = (ATTRIB *) g_malloc(sizeof(ATTRIB));

  /* fill item with correct data (mainly item) */
  new->next = NULL;
  new->prev = end;
  new->object = item;
  new->copied_to = NULL;
  new->object->attribute = 1; /* Set the attribute to true, hack define */
  /* Show that that item is an attribute */
  new->object->color = toplevel->attribute_color;

  if (new->object->type == OBJ_TEXT) {
    o_complex_set_color(new->object->text->prim_objs,
                        new->object->color);
  } else if (new->object->type == OBJ_COMPLEX || 
             new->object->type == OBJ_PLACEHOLDER) {
    o_complex_set_color(new->object->complex->prim_objs,
                        new->object->color);
  }

  /* Add link from item to attrib listing */
  new->object->attached_to = new;

  /* set next of tail of end->attrib to item */
  if (end) {
    end->next = new;
    return(new);
  } else {
    return(new);
  }
}

/*! \brief Free single item in attribute list.
 *  \par Function Description
 *  Free single item in attribute list.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] current    ATTRIB pointer to free.
 *
 *  \note
 *  this routine is not nice to next and prev
 *  this routine is only called from free_all
 */
void o_attrib_free(TOPLEVEL *toplevel, ATTRIB *current)
{
  if (current != NULL) {

    /* \todo this makes me nervous... very nervous */
    if (current->object != NULL && current->prev != NULL) {
      current->object->attribute = 0;	
      current->object->attached_to=NULL;
      current->object->color = toplevel->detachedattr_color;

      if (current->object->type == OBJ_TEXT) {
        o_complex_set_color(current->object->text->prim_objs, 
                            current->object->color);
      } else {
        printf("Tried to set the color on a complex!\nlibgeda/src/o_attrib_free 1\n");
      }

      /* \todo not sure on this */
      if (current->object->saved_color != -1) {
        if (current->object->type == OBJ_TEXT) {
          o_complex_set_saved_color_only(
                                         current->object->text->prim_objs, 
                                         toplevel->detachedattr_color);
        } else {
          printf("Tried to set the color on a complex!\nlibgeda/src/o_attrib_free 2\n");
        }
        current->object->saved_color = toplevel->
        detachedattr_color;
      }
    }

    /* \todo were do we detach the object->attached_to? above */
    current->object=NULL;

    g_free(current);

  }
}

/*! \brief Attach existing attribute to an object.
 *  \par Function Description
 *  Attach existing attribute to an object.
 *
 *  \param [in]  toplevel    The TOPLEVEL object.
 *  \param [in]  parent_list  List where actual attribute objects live.
 *  \param [in]  text_object  The attribute to be added.
 *  \param [out] object       The object where you want to add item as an attribute.
 *
 *  \par IMPORTANT:
 *  Lists first then specific single item.
 *
 *  \note
 *  typically parent_list is object_parent (object_head), but it is
 *  overridden in o_complex_add so that it points to head node of the complex
 *  
 */
void o_attrib_attach(TOPLEVEL *toplevel, OBJECT *parent_list,
		     OBJECT *text_object, OBJECT *object)
{
  OBJECT *o_current = NULL;

  ATTRIB *found = NULL;
  OBJECT *found2 = NULL; /* object in main list */

  o_current = text_object; 

  if (object == NULL) {
    printf("ah.. object was not found in the parent list!\n");
    return;
  }

  /* is the object already part of the list ? */
  found = o_attrib_search(object->attribs, o_current);
  if (!found) { /* no it's not, add it to the list */
		
    found2 = (OBJECT *) o_list_search(parent_list, o_current);	

    /* check to see if found2 is not null hack */
    if (found2) {
      if (found2->type == OBJ_TEXT) {

        if (object->attribs == NULL) {
          object->attribs = 
            add_attrib_head(object);
        }


        if (found2->attached_to) {
          fprintf(stderr, "You cannot attach this attribute [%s] to more than one object\n", found2->text->string);
        } else {

          o_attrib_add(toplevel,
                       object->attribs, 
                       found2);

          o_current->color = toplevel->
            attribute_color; 

          o_complex_set_color(
                              o_current->text->prim_objs,
                              o_current->color);

          if (o_current->saved_color != -1) {
            o_complex_set_saved_color_only(
                                           o_current->text->prim_objs, 
                                           o_current->color);
            o_current->saved_color = 
              o_current->color;
          }
          /* can't do this here since just selecting something */
          /* will cause this to be set */
          /* toplevel->page_current->CHANGED=1;*/
        }
      } else {
        fprintf(stderr, "You cannot attach non text items as attributes!\n");
      }	
    }
  } else {
    if (o_current->text->string) { 	
      printf("Attribute [%s] already attached\n", 
             o_current->text->string);
    }
  }
}


/*! \brief Free all attribute items in a list.
 *  \par Function Description
 *  Free all attribute items in a list.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] list       The list to free.
 *
 *  \note
 *  this routine uses o_attrib_free (which isn't nice to next, prev)
 *  so it should only be used when an object is being destroyed
 *  goes backwards
 */
void o_attrib_free_all(TOPLEVEL *toplevel, ATTRIB *list)
{
  ATTRIB *a_current; 
  ATTRIB *a_next;

  a_current = list;

  while (a_current != NULL) {
    a_next = a_current->next;
    o_attrib_free(toplevel, a_current);
    a_current = a_next;
  }
}

/*! \brief Print all attributes to a Postscript document.
 *  \par Function Description
 *  Print all attributes to a Postscript document.
 *
 *  \param [in] attributes  List of attributes to print.
 */
void o_attrib_print(ATTRIB *attributes) 
{
  ATTRIB *a_current;

  a_current = attributes;

  while (a_current != NULL) {
    printf("Attribute points to: %s\n", a_current->object->name);
    if (a_current->object && a_current->object->text) {
      printf("\tText is: %s\n", a_current->object->text->string);
    }

    if (!a_current->object) {
      printf("oops found a null attrib object\n");
    }
    a_current = a_current->next;
  }
}

/*! \brief Delete an attribute.
 *  \par Function Description
 *  This function goes out and removes the current attribute,
 *  while preserving the next, prev pointers.
 *  This function should be used when detaching an attribute.
 *
 *  \param [in] a_current  The attribute to be deleted.
 */
void o_attrib_delete(ATTRIB *a_current)
{
  if (a_current != NULL) {

    if (a_current->next)
    a_current->next->prev = a_current->prev;
    else
    a_current->next = NULL;

    if (a_current->prev)
    a_current->prev->next = a_current->next;
    else
    a_current->prev = NULL;

    if (a_current->object) {
      a_current->object->attribute=0;
      a_current->object->attached_to=NULL;
    }
    a_current->object = NULL;

    g_free(a_current);
  }
}

/*! \todo Finish function.
 *  \brief Remove an attribute item from an attribute list.
 *  \par Function Description
 *  This function goes out and removes an attribute from a list.
 *  It searches for the attribute and then removes it using the
 *  good #o_attrib_delete() routine.
 *
 *  \param [in] list    ATTRIB list to remove attribute from.
 *  \param [in] remove  The ATTRIB to remove from list.
 *
 *  \note
 *  This function is the detach_all routine.
 *  It is not currently being used.
 *  It is not even done.
 */
void o_attrib_remove(ATTRIB *list, ATTRIB *remove) 
{
  ATTRIB *a_current;

  a_current = list;

  while (a_current != NULL) {
	
    if (a_current == remove) {
			
    }	
    a_current = a_current->next;
  }
}

/*! \brief Read attributes from a buffer.
 *  \par Function Description
 *  Read attributes from a TextBuffer.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [out] object_to_get_attribs  Storage for attributes.
 *  \param [in]  tb                     The text buffer to read from.
 *  \param [in]  release_ver            libgeda release version number.
 *  \param [in]  fileformat_ver         file format version number.
 *  \return Pointer to object_to_get_attribs.
 */
OBJECT *o_read_attribs(TOPLEVEL *toplevel,
		       OBJECT *object_to_get_attribs,
   		       TextBuffer *tb,
		       unsigned int release_ver, unsigned int fileformat_ver)
{
  OBJECT *object_list=NULL;
  char *line = NULL;
  char objtype;
  int ATTACH=FALSE;
  int saved_color = -1;

  object_list = object_to_get_attribs;

  while (1) {

    line = s_textbuffer_next_line (tb);
    if (line == NULL) break;

    sscanf(line, "%c", &objtype);
    switch (objtype) {

      case(OBJ_LINE):
        object_list = (OBJECT *) o_line_read(toplevel,
                                             object_list,
                                             line, 
                                             release_ver, fileformat_ver);
        break;


      case(OBJ_NET):
        object_list = (OBJECT *) o_net_read(toplevel,
                                            object_list, 
                                            line, 
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_BUS):
        object_list = (OBJECT *) o_bus_read(toplevel,
                                            object_list, 
                                            line, 
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_BOX):
        object_list = (OBJECT *) o_box_read(toplevel,
                                            object_list, 
                                            line, 
                                            release_ver, fileformat_ver);
        break;
		
      case(OBJ_CIRCLE):
        object_list = (OBJECT *) o_circle_read(
                                               toplevel,
                                               object_list, 
                                               line, 
                                               release_ver, fileformat_ver);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
			
        object_list = (OBJECT *) o_complex_read(
                                                toplevel,
                                                object_list, 
                                                line, 
                                                release_ver, fileformat_ver);

				/* this is necessary because complex may add
				   attributes which float */
				/* still needed? */
        object_list = (OBJECT *) return_tail(
                                             object_list);
        break;

      case(OBJ_PIN):
        object_list = (OBJECT *) o_pin_read(toplevel,
                                            object_list, 
                                            line, 
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_ARC):
        object_list = (OBJECT *) o_arc_read(toplevel,
                                            object_list, 
                                            line, 
                                            release_ver, fileformat_ver);
        break;

      case(OBJ_TEXT):
	line = g_strdup (line);
        object_list = (OBJECT *) o_text_read(toplevel,
                                             object_list, 
                                             line,
					     tb,
                                             release_ver, fileformat_ver);
	g_free (line);
        saved_color = object_list->color;
        ATTACH=TRUE;
		
        break;

      case(ENDATTACH_ATTR): 
        return(object_list);
        break;	

    }

    if (ATTACH) {
      o_attrib_attach(toplevel,
                      toplevel->page_current->object_parent,
                      object_list, object_to_get_attribs);
      /* check color to set it to the right value */
      if (object_list->color != saved_color) {
        object_list->color = saved_color;

        if (object_list->type == OBJ_TEXT) {	
          o_complex_set_color(
                              object_list->text->prim_objs,
                              object_list->color);
        } else {
          printf("Tried to set the color on a complex in libgeda/src/o_read_attribs\n");
        }
      }
      ATTACH=FALSE;
    } else {
      fprintf(stderr, "Tried to attach a non-text item as an attribute\n");
    }
  }
  return(object_list);
}

/*! \brief Save attributes to a file.
 *  \par Function Description
 *  Save attributes to a file.
 *
 *  \param [in] fp       FILE pointer to write attributes to.
 *  \param [in] attribs  attributes to write.
 *  \todo
 *  this should be trimmed down to only save attributes which are text items
 */
void o_save_attribs(FILE *fp, ATTRIB *attribs)
{
  ATTRIB *a_current=NULL;
  OBJECT *o_current=NULL;
  char *out;

  a_current = attribs;

  fprintf(fp, "{\n");
	
  while ( a_current != NULL ) {

    o_current = a_current->object;	

    if (o_current->type != OBJ_HEAD) {

#if DEBUG
      printf("type: %d %c  ref: %d %c\n", o_current->type, o_current->type,
             OBJ_PIN, OBJ_PIN);
#endif
      
      switch (o_current->type) {

        case(OBJ_LINE):
          out = (char *) o_line_save(o_current);
          break;

        case(OBJ_NET):
          out = (char *) o_net_save(o_current);
          break;

        case(OBJ_BUS):
          out = (char *) o_bus_save(o_current);
          break;

        case(OBJ_BOX):
          out = (char *) o_box_save(o_current);
          break;
		
        case(OBJ_CIRCLE):
          out = (char *) o_circle_save(o_current);
          break;

        case(OBJ_COMPLEX):
        case(OBJ_PLACEHOLDER):  /* new type -- SDB 1.20.2005 */
          out = (char *) o_complex_save(o_current);
          break;

        case(OBJ_TEXT):
          out = (char *) o_text_save(o_current);
          break;

        case(OBJ_PIN):
          out = (char *) o_pin_save(o_current);
          break;

        case(OBJ_ARC):
          out = (char *) o_arc_save(o_current);
          break;

  	case(OBJ_PICTURE):
	  out = (char *) o_picture_save(o_current); 
	  break;

        default:
          fprintf(stderr, "Error type!\n");
          exit(-1);
          break;
      }
      /* output the line */
      fprintf(fp, "%s\n", out);
      g_free(out);
    }
    a_current = a_current->next;
  } 

  fprintf(fp, "}\n");
}

/*! \brief Get name and value from name=value attribute.
 *  \par Function Description
 *  Get name and value from a name=value attribute.
 *
 *  \param [in]  string     String to split into name/value pair.
 *  \param [out] name_ptr   Name if found in string, NULL otherwise.
 *  \param [out] value_ptr  Value if found in string, NULL otherwise.
 *  \return TRUE if string had equals in it, FALSE otherwise.
 *
 *  \note
 *  both name and value must be pre allocated
 *  And if you get an invalid attribute (improper) with a name and no
 *  value, then it is NOT an attribute.
 *  Also, there cannot be any spaces beside the equals sign
 *  Changed: now it allocates memory for name and value strings.
 *  \warning
 *  Caller must g_free these strings when not needed.
 */
int o_attrib_get_name_value(char *string, char **name_ptr, char **value_ptr )
{
  char *equal_ptr;
  char **str_array;

  if (name_ptr == NULL || value_ptr == NULL) {
    return(FALSE);
  }

  *name_ptr = NULL;  /* force these values to null */
  *value_ptr = NULL;

  if (!string) {
    return(FALSE);
  }

  /* make sure there are no spaces in between equals */
  equal_ptr = strchr(string, '=');
  if (equal_ptr == NULL) {
    return(FALSE);
  }


  /*! \todo Technically this isn't a correct if statement.  This if will 
   * cause an invalid read for strings:  =name and value= 
   */
  if ( (*(equal_ptr + 1) == ' ') || (*(equal_ptr - 1) == ' ') ) {
     /* sometimes you have text with an ='s in it, it shouldn't be */
     /* treated like an attribute */

#if DEBUG 
    s_log_message("Found attrib/text with spaces beside the ='s [%s]\n", 
	          string);
    s_log_message("You can ignore the above message if the text is not intended to be an attribute\n");
    fprintf(stderr, "Found an attribute with spaces beside the ='s [%s]\n", 
            string);
#endif

    return(FALSE);
  }

  str_array = g_strsplit (string, "=", 2);
  
  *name_ptr = g_strdup(str_array[0]);
  *value_ptr = g_strdup(str_array[1]);
  g_strfreev(str_array);
  
  if (*value_ptr && (*value_ptr)[0] == '\0') {
    s_log_message("Found an improper attribute: _%s_\n", string);
#if 0 /* for now leak this memory till this is verified correct everywhere */
    g_free(*name_ptr); *name_ptr = NULL;
    g_free(*value_ptr); *value_ptr = NULL;
#endif
    return(FALSE);
  } else {
    return(TRUE);
  }
}

/*! \brief Free the currently selected attribute.
 *  \par Function Description
 *  Free the currently selected attribute.
 *
 *  \param [in] toplevel  The TOPLEVEL object containing current attribute.
 *
 */
void o_attrib_free_current(TOPLEVEL *toplevel)
{
  if (toplevel->current_attribute) {
    g_free(toplevel->current_attribute);
  }
  toplevel->current_attribute=NULL;
}

/*! \brief Set an attribute's string.
 *  \par Function Description
 *  Set an attribute's string.
 *
 *  \param [in] toplevel  The TOPLEVEL object that holds the attribute.
 *  \param [in] string     The value to set attribute string to.
 *
 *  \note
 *  The user of this function must g_free the
 *  <B>toplevel->current_attribute</B> string after done using it.
 *  They must also free the input string.
 */
void o_attrib_set_string(TOPLEVEL *toplevel, char *string)
{
  int len;

  /* need to put an error messages here */
  if (string == NULL)  {
    fprintf(stderr, "error! string in set_string was NULL\n");
    return;
  }

  if (toplevel->current_attribute != NULL) {
    g_free(toplevel->current_attribute);
    toplevel->current_attribute=NULL;
  }

  len = strlen(string);

  toplevel->current_attribute = (char *) g_malloc(sizeof(char)*len+1);

  strcpy(toplevel->current_attribute,string);
	
  /* be sure to g_free this string somewhere and free the input string */
}

/*! \brief Get the parent OBJECT of an attribute.
 *  \par Function Description
 *  Get the parent OBJECT of an attribute.
 *
 *  \param [in] attribute  ATTRIB pointer to get parent of.
 *  \return The parent OBJECT if it exists, otherwise NULL.
 */
OBJECT *o_attrib_return_parent(ATTRIB *attribute) 
{
  ATTRIB *a_current;

  a_current = attribute;

  if (!a_current) {
    return(NULL);
  }
  
  while (a_current->prev != NULL) {
    a_current = a_current->prev;	
  }	

  /* should be pointing to the parent */
	
  return(a_current->object);	
}

/*! \brief Set attribute color
 *  \par Function Description
 *  This function sets all attribute objects to the right
 *  color (attribute_color).
 *
 *  \param [in]     toplevel   The TOPLEVEL object.
 *  \param [in,out] attributes  ATTRIB list to set colors on.
 *
 */
void o_attrib_set_color(TOPLEVEL *toplevel, ATTRIB *attributes)
{
  ATTRIB *a_current;

  a_current = attributes;

  /* skip over head */
  if (a_current) 
  a_current = a_current->next;

  while (a_current != NULL) {

    if (a_current->object) {	
			
      if (a_current->object->type == OBJ_TEXT &&
          a_current->object->text->prim_objs) {

				/* I'm not terribly happy with this */
		
        if (a_current->object->saved_color != -1) {

          /* if the object is selected, make */
          /* sure it it say selected */
          o_complex_set_color(
                              a_current->object->text->prim_objs,
                              SELECT_COLOR);
          a_current->object->color = 
            SELECT_COLOR;

          o_complex_set_saved_color_only(
                                         a_current->object->text->prim_objs,
                                         toplevel->attribute_color);
          a_current->object->saved_color = toplevel->
            attribute_color;

        } else {
          o_complex_set_color(
                              a_current->object->text->prim_objs,
                              toplevel->attribute_color);
          a_current->object->color = 
            toplevel->attribute_color;
        }
      }	

      a_current = a_current->next;	
    }
  }
}

/*! \brief Search for attibute by name.
 *  \par Function Description
 *  Search for attribute by name.
 *
 *  \warning
 *  The list is the top level list. Do not pass it an object_head list
 *  unless you know what you are doing.
 *  
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     OBJECT list to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_name(OBJECT *list, char *name, int counter) 
{
  OBJECT *o_current;
  ATTRIB *a_current;
  OBJECT *found;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;

  o_current = list;

  while(o_current != NULL) {
    if (o_current->attribs != NULL) {
      a_current = o_current->attribs;

      while(a_current != NULL) {
        found = a_current->object;
        if (found != NULL) {
          if (found->type == OBJ_TEXT) {
            val = o_attrib_get_name_value(found->text->string, 
                                          &found_name, &found_value);

            if (val) {
              if (strcmp(name, found_name) == 0) {
                if (counter != internal_counter) {
                  internal_counter++;	
                } else {
                  return_string = (char *) 
                    g_malloc(sizeof(char)*strlen(found_value)+1);
                  strcpy(return_string, found_value);
		  if (found_name) g_free(found_name);
		  if (found_value) g_free(found_value);
                  return(return_string);
                }
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
        }
        a_current=a_current->next;
      }	
    }

    /* search for attributes outside */

    if (o_current->type == OBJ_TEXT) {
      if (found_name) g_free(found_name);
      if (found_value) g_free(found_value);
      val = o_attrib_get_name_value(o_current->text->string, 
                                    &found_name, &found_value);
      if (val) {
        if (strcmp(name, found_name) == 0) {
          if (counter != internal_counter) {
            internal_counter++;	
          } else {
            return_string = (char *) 
              g_malloc(sizeof(char)* strlen(found_value)+1);
            strcpy(return_string, found_value);
	    if (found_name) g_free(found_name);
	    if (found_value) g_free(found_value);
            return(return_string);
          }
        }
	if (found_name) { g_free(found_name); found_name = NULL; }
	if (found_value) { g_free(found_value); found_value = NULL; }
      }	
    }

    o_current=o_current->next;
  }
	
  if (found_name) g_free(found_name);
  if (found_value) g_free(found_value);
  return (NULL);
} 

/*! \brief Search OBJECT list for text string.
 *  \par Function Description
 *  Given an OBJECT list (i.e. OBJECTs on schematic page or
 *  inside symbol), search for the attribute called out in
 *  "string".  It iterates over all objects in the OBJECT list
 *  and dives into the attached ATTRIB list for 
 *  each OBJECT if it finds one.
 *  Inside the ATTRIB list it looks for an attached text 
 *  attribute matching "string".  It returns the
 *  pointer to the associated OBJECT if found.  If the attribute
 *  string is not found in the ATTRIB list, then the fcn
 *  looks on the OBJECT itself for the attribute.  Then it
 *  iterates to the next OBJECT.
 *
 *  \warning
 *  The list is the top level list. Do not pass it an object_head list
 *  unless you know what you are doing.
 *  
 *  \param [in] list    OBJECT list to search.
 *  \param [in] string  Character string to search for.
 *  \return A matching OBJECT if found, NULL otherwise.
 */
OBJECT *o_attrib_search_string_list(OBJECT *list, char *string)
{
  OBJECT *o_current;
  ATTRIB *a_current;
  OBJECT *found;

  o_current = list;

  while(o_current != NULL) {
    /* first search attribute list */
    if (o_current->attribs != NULL) {
      a_current = o_current->attribs;

      while(a_current != NULL) {
        found = a_current->object;
        if (found != NULL) {
          if (found->type == OBJ_TEXT) {
#if DEBUG
	    printf("libgeda:o_attrib.c:o_attrib_search_string_list --");
	    printf("found OBJ_TEXT, string = %s\n", found->text->string);
#endif 
            if (strcmp(string, found->text->string) == 0) {
              return(found);
            }
          }	
        }
        a_current=a_current->next;
      }
    }	
  
    /* search for attributes outside (ie the actual object) */
    if (o_current->type == OBJ_TEXT) {
      if (strcmp(string, o_current->text->string) == 0) {
        return(o_current);
      }
    }
    
    o_current=o_current->next;
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
  char *found_name = NULL;
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
                                      &found_name, &found_value);
        if (val) {
          return_string = g_strdup(found_value);
	  if (found_name) g_free(found_name);
	  if (found_value) g_free(found_value);
	  return(return_string);
        }
      }
    }
  }	
	
  if (found_name) g_free(found_name);
  if (found_value) g_free(found_value);
  return (NULL);
} 

/*! \brief Check if object matches string.
 *  \par Function Description
 *  This function will check if the text->string value of
 *  the passed OBJECT matches the <B>search_for</B> parameter.
 *  If not, it then searches the object's ATTRIB list (if 
 *  it has one.)
 *  Only this single OBJECT and its ATTRIB list is
 *  checked, and other OBJECTs on the page are not checked.
 *  \param [in] object      The OBJECT to compare.
 *  \param [in] search_for  Character string to compare against.  
 *  Usually name=value
 *  \return The OBJECT passed in <B>object</B> parameter, NULL otherwise.
 */
OBJECT *o_attrib_search_string_single(OBJECT *object, char *search_for)
{
  OBJECT *o_current;
  OBJECT *found;
  ATTRIB *a_current;

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

  /* Next check to see if this OBJECT has an ATTRIB list we */
  /* can search.  If not return NULL.  If so, search it. */
  if (o_current->attribs == NULL) 
    return(NULL);

  a_current = o_current->attribs;
  while(a_current != NULL) {
    found = a_current->object;
    if (found != NULL) {
      if (found->type == OBJ_TEXT) {
	if(strcmp(found->text->string, search_for) == 0) {
	    return(found);
        }
      }
    }
    a_current=a_current->next;
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
 *  \param [in] list     The ATTRIB list to search.
 *  \param [in] value    Character string with value to search for.
 *  \param [in] name     Character string with name to compare.
 *  \param [in] counter  Which occurance to return.
 *  \return The attribute OBJECT if found, NULL otherwise.
 *
 */
OBJECT *o_attrib_search_attrib_value(ATTRIB *list, char *value, char *name, 
				     int counter) 
{
  OBJECT *found;
  ATTRIB *a_current;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;

  a_current = list;
	
  if (!value) 
  return(NULL);

  if (!name) 
  return(NULL);

  while(a_current != NULL) {
    found = a_current->object;
    if (found != NULL) {
      if (found->type == OBJ_TEXT) {
        val = o_attrib_get_name_value(found->text->string, 
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
		if (found_name) g_free(found_name);
		if (found_value) g_free(found_value);
                return(found);
              }
            }
          }
	  if (found_name) { g_free(found_name); found_name = NULL; }
	  if (found_value) { g_free(found_value); found_value = NULL; }
        }	

      }
    }
    a_current=a_current->next;
  }

  if (found_name) g_free(found_name);
  if (found_value) g_free(found_value);
  return (NULL);
} 

/*! \brief Search for an attribute by name.
 *  \par Function Description
 *  Search for an attribute by name.
 *
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 *
 *  \param [in] list     ATTRIB list to search.
 *  \param [in] name     Character string with attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *
o_attrib_search_attrib_name(ATTRIB *list, char *name, int counter) 
{
  OBJECT *found;
  ATTRIB *a_current;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;

  a_current = list;

  while(a_current != NULL) {
    found = a_current->object;
    if (found != NULL) {
      if (found->type == OBJ_TEXT) {
        val = o_attrib_get_name_value(found->text->string, 
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
              return_string = (char *) 
                g_malloc(sizeof(char)* strlen(found_value)+1);
              strcpy(return_string, found_value);
	      if (found_name) g_free(found_name);
	      if (found_value) g_free(found_value);
              return(return_string);
            }
          }
	  if (found_name) { g_free(found_name); found_name = NULL; }
	  if (found_value) { g_free(found_value); found_value = NULL; }
        }	
      }
    }
    a_current=a_current->next;
  }

  if (found_name) g_free(found_name);
  if (found_value) g_free(found_value);
  return (NULL);
} 

/*! \brief Search TOPLEVEL attributes.
 *  \par Function Description
 *  This function should only be used to search for TOPLEVEL attributes.
 *  \warning
 *  The list is the top level list. Do not pass it an object_head list
 *  unless you know what you are doing.
 *  
 *  Counter is the n'th occurance of the attribute, and starts searching
 *  from zero.  Zero is the first occurance of an attribute.
 * 
 *  \param [in] list     The OBJECT list to search (TOPLEVEL only).
 *  \param [in] name     Character string of attribute name to search for.
 *  \param [in] counter  Which occurance to return.
 *  \return Character string with attribute value, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_attrib_search_toplevel(OBJECT *list, char *name, int counter) 
{
  OBJECT *o_current;
  int val;
  int internal_counter=0;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;

  o_current = list;

  while(o_current != NULL) {

    /* search for attributes outside */

    if (o_current->type == OBJ_TEXT) {
      val = o_attrib_get_name_value(o_current->text->string, 
                                    &found_name, &found_value);
      if (val) {
        if (strcmp(name, found_name) == 0) {
          if (counter != internal_counter) {
            internal_counter++;	
          } else {
            return_string = (char *) 
              g_malloc(sizeof(char)* strlen(found_value)+1);
            strcpy(return_string, found_value);
	    if (found_name) g_free(found_name);
	    if (found_value) g_free(found_value);
            return(return_string);
          }
        }
	if (found_name) { g_free(found_name); found_name = NULL; }
	if (found_value) { g_free(found_value); found_value = NULL; }
      }	
    }

    o_current=o_current->next;
  }
	
  if (found_name) g_free(found_name);
  if (found_value) g_free(found_value);
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
  ATTRIB *a_current;
  OBJECT *found;
  int val;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;

  o_current = object;

  if (o_current == NULL) {
    return(NULL);
  }

  if (o_current->attribs != NULL) {
    a_current = o_current->attribs;

    while(a_current != NULL) {
      found = a_current->object;
      if (found != NULL) {
        if (found->type == OBJ_TEXT) {
          val = o_attrib_get_name_value(found->text->string, 
					&found_name, &found_value);

          if (val) {
            if (strcmp(name, found_name) == 0) {
              return_string = (char *) 
                g_malloc(sizeof(char)* strlen(found_value)+1);
              strcpy(return_string, found_value);
              if (return_found) {
                *return_found = found;
              }
	      if (found_name) g_free(found_name);
	      if (found_value) g_free(found_value);
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
      }
      a_current=a_current->next;
    }	
  }
  /* search for attributes outside */

  if (o_current->type == OBJ_TEXT) {
    if (found_name) g_free(found_name);
    if (found_value) g_free(found_value);
    val = o_attrib_get_name_value(o_current->text->string, 
                                  &found_name, &found_value);

    if (val) {
      if (strcmp(name, found_name) == 0) {
        return_string = (char *) 
          g_malloc(sizeof(char)* strlen(found_value)+1);
        strcpy(return_string, found_value);
        if (return_found) {
          *return_found = o_current;
        }
	if (found_name) g_free(found_name);
	if (found_value) g_free(found_value);
        return(return_string);
      }
      if (found_name) { g_free(found_name); found_name = NULL; }
      if (found_value) { g_free(found_value); found_value = NULL; }
    }	
  }

  if (return_found) {
    *return_found = NULL;
  }
  
  if (found_name) g_free(found_name);
  if (found_value) g_free(found_value);
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
  ATTRIB *a_current;
  OBJECT *found=NULL;
  int val;
  char *found_name = NULL;
  char *found_value = NULL;
  char *return_string = NULL;
  int internal_counter=0;
	

  o_current = object;

  if (o_current == NULL) {
    return(NULL);
  }

  if (o_current->attribs != NULL) {
    a_current = o_current->attribs;

    while(a_current != NULL) {
      found = a_current->object;
      if (found != NULL) {
        if (found->type == OBJ_TEXT) {
          val = o_attrib_get_name_value(found->text->string, 
					&found_name, &found_value);

          if (val) {
            if (strcmp(name, found_name) == 0) {
              if (counter != internal_counter) {
                internal_counter++;
              } else {
                return_string = (char *) 
                  g_malloc(sizeof(char)* strlen(found_value)+1);
                strcpy(return_string, found_value);
		if (found_name) g_free(found_name);
		if (found_value) g_free(found_value);
                return(return_string);
              }
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
      }
      a_current=a_current->next;
    }	

  }
  /* search for attributes outside */

  if (o_current->type == OBJ_TEXT) {
    if (found_name) g_free(found_name);
    if (found_value) g_free(found_value);
    val = o_attrib_get_name_value(o_current->text->string, 
                                  &found_name, &found_value);

    if (val) {
      if (strcmp(name, found_name) == 0) {
        if (counter != internal_counter) {
          internal_counter++;
        } else {
          return_string = (char *) 
            g_malloc(sizeof(char)* strlen(found_value)+1);
          strcpy(return_string, found_value);
	  if (found_name) g_free(found_name);
	  if (found_value) g_free(found_value);
          return(return_string);
        }
      }
      if (found_name) { g_free(found_name); found_name = NULL; }
      if (found_value) { g_free(found_value); found_value = NULL; }
    }
  }	
  
  if (found_name) g_free(found_name);
  if (found_value) g_free(found_value);
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
 *  \param [in] list        OBJECT list to search.
 *  \param [in] pin_number  pin number to search for.
 *  \return OBJECT containing pinseq data, NULL otherwise.
 */
OBJECT *o_attrib_search_pinseq(OBJECT *list, int pin_number)
{
  OBJECT *pinseq_text_object;
  char *search_for;

  /* The 9 is the number of allowed digits plus null */
  search_for = (char *) g_malloc(sizeof(char)*(strlen("pinseq=")+9));
  sprintf(search_for, "pinseq=%d", pin_number);

  pinseq_text_object = o_attrib_search_string_list(list, search_for);
  g_free(search_for);
  
  if (pinseq_text_object && pinseq_text_object->attached_to) {
    return(o_attrib_return_parent(pinseq_text_object->attached_to));
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

  /* The 9 is the number of digits plus null */
  search_for = (char *) g_malloc(sizeof(char)*(strlen("slotdef=:")+9));

  sprintf(search_for, "slotdef=%d:", slotnumber);

  o_current = object->complex->prim_objs;
  while (o_current != NULL) {
    return_value = o_attrib_search_string_partial(o_current, search_for, 0);
    if (return_value) {
	break;
    }
    o_current = o_current->next; 
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
  OBJECT *o_pinseq_object;
  char *string;
  char *slotdef;
  int slot;
  int pin_counter;    /* Internal pin counter private to this fcn. */
  char *new_pinseq;   /* New pinseq = (slot*(number of pins -1) + pin_count */
  int numpins;        /* Total number of pins on this slot */
  char* current_pin;  /* text from slotdef= to be made into pinnumber= */
  char* cptr;         /* char pointer pointing to pinnumbers in slotdef=#:#,#,# string */

  o_current = object;
  /* For this particular graphic object (component instantiation) */
  /* get the slot number as a string */
  string = o_attrib_search_slot(o_current, &o_slot_attrib);
	
  if (!string) {
    /* s_log_message("Did not find slot= attribute\n"); */
    /* not a serious error */
    return;
  } 
  slot = atoi(string);
  g_free(string);
  
  /* OK, now that we have the slot number, use it to get the */
  /* corresponding slotdef=#:#,#,# string.  */
  slotdef = o_attrib_search_slotdef(o_current, slot);
  
  if (!slotdef) {
    s_log_message("Did not find slotdef=#:#,#,#... attribute\n");
    return;
  }

  if (!strstr(slotdef, ":")) {
    /* Didn't find proper slotdef=#:... put warning into log */
    s_log_message("Improper slotdef syntax: missing \":\".\n");
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
    s_log_message("Did not find proper slotdef=#:#,#,#... attribute\n");
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

        /* 9 is the size of one number plus null character */
        o_pinnum_object->text->string = (char *)
          g_malloc(sizeof(char)*(strlen("pinnumber=")+strlen(current_pin)+9));

        /* removed _int from current_pin */
        sprintf(o_pinnum_object->text->string, "pinnumber=%s", current_pin);
        
        o_text_recreate(toplevel, o_pinnum_object);
      }
      if (string) {
      	g_free(string);
      }


      /* Now update pinseq= attrib on this part. */
      /* Algorithm:
       * 1. Get pointer to pinseq= attrib (graphic object) on this part.
       * 2. Verify it has a pinseq= string attached.
       * 3. free pinseq string.
       * 4. figure out how many pins are on this part.
       * 5. Write new string pinseq=(slot * (number of pins-1)) + pin_counter
       *    into pinseq= object.
       */
      string = o_attrib_search_name_single(o_pin_object, "pinseq",
                                           &o_pinseq_object);
  
      if (string && o_pinseq_object && o_pinseq_object->type == OBJ_TEXT &&
          o_pinseq_object->text->string) {
	g_free(o_pinseq_object->text->string);  /* free old pinseq text */

	/* I need to check that the return is non-zero! */
	numpins = o_complex_count_pins(o_current);

#if DEBUG
	printf("libgeda:o_attrib.c:o_attrib_slot_update -- name = %s\n", o_current->name);
	printf("                                           numpins = %d\n", numpins);
#endif

	/* Now put new pinseq attrib onto pin. */
	new_pinseq = g_malloc(sizeof(char)*((numpins-1)*slot)+pin_counter);
	sprintf(new_pinseq, "%d", numpins*(slot-1)+pin_counter);
        /* Add 1 for EOL char */
	o_pinseq_object->text->string = (char *)
          g_malloc(sizeof(char)*(strlen("pinseq=") + 
				 strlen(new_pinseq) +1 ));

	sprintf(o_pinseq_object->text->string, "pinseq=%s", new_pinseq);
	g_free(new_pinseq);
#if DEBUG
	printf("libgeda:o_attrib.c:o_attrib_slot_update -- ");
	printf("new_pinseq attrib = %s \n", o_pinseq_object->text->string);
#endif
        
        o_text_recreate(toplevel, o_pinseq_object);
      }
      if (string) {
      	g_free(string);
      }
      
      pin_counter++;
    } else {
      s_log_message("component missing pinseq= attribute\n");
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
    s_log_message("Did not find slotdef=#:#,#,#... attribute\n");
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
    s_log_message("Did not find proper slotdef=#:#,#,#... attribute\n");
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

        /* 9 is the size of one number plus null character */
        o_pinnum_object->text->string = (char *)
          g_malloc(sizeof(char)*(strlen("pinnumber=")+strlen(current_pin)+9));

        /* removed _int from current_pin */
        sprintf(o_pinnum_object->text->string, "pinnumber=%s", current_pin);
        
        o_text_recreate(toplevel, o_pinnum_object);
      }
      
      pin_counter++;
    } else {
      s_log_message("component missing pinseq= attribute\n");
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
    ret_value = o_attrib_search_toplevel(
                                         p_current->object_head,
                                         name, 0);

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
 *  \param [in] object_list  OBJECT list to search.
 *  \param [in] sel_object   OBJECT to search for.
 *  \return An array of objects that matched sel_object, NULL otherwise.
 */
OBJECT ** o_attrib_return_attribs(OBJECT *object_list, OBJECT *sel_object) 
{
  OBJECT **found_objects;
  int num_attribs=0;
  int i=0;
  ATTRIB *a_current;	
  OBJECT *o_current;
  OBJECT *object;

  object = (OBJECT *) o_list_search(object_list, sel_object);

  if (!object) {
    return(NULL);	
  }

  if (!object->attribs) {
    return(NULL);
  }

  if (!object->attribs->next) {
    return(NULL);
  }


  /* first go through and count the number of attribs */
  a_current = object->attribs->next;	
  while(a_current != NULL) {
    num_attribs++;
    a_current = a_current->next;
  }

  found_objects = (OBJECT **) g_malloc(sizeof(OBJECT *)*(num_attribs+1));

  /* now actually fill the array of objects */
  a_current = object->attribs->next;	
  while(a_current != NULL) {
    if (a_current->object != NULL) {
      o_current = a_current->object;
      if (o_current->type == OBJ_TEXT && 
          o_current->text->string) {
        found_objects[i] = o_current;
        i++;
      }
    }
    a_current = a_current->next;
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


/*! \brief Set the copied_to property on a list of attributes
 *  \par Function Description
 *  Sets the copied_to property on a list of attributes.
 *  Used when copying objects with attributes.
 *
 *  \param [in] list    List of attributes to set
 *  \param [in] to_obj  OBEJCT to set copied_to
 */
void o_attrib_list_copied_to(ATTRIB *list, OBJECT *to_obj)
{
  ATTRIB *a_current;

  a_current = list;
  while ( a_current ) {

    /* head attrib node has prev = NULL */
    if (a_current->prev != NULL) {
      a_current->copied_to = to_obj;
    }
    a_current = a_current->next;
  }
}
