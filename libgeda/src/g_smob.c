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

#include <math.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

static long attrib_smob_tag; /*! Attribute SMOB tag */
static long object_smob_tag; /*! Object SMOB tag */
static long page_smob_tag;   /*! Page SMOB tag */

/*! \brief Free attribute smob memory.
 *  \par Function Description
 *  Free the memory allocated by the attribute smob and return its size.
 *
 *  \param [in] attrib_smob  The attribute smob to free.
 *  \return Size of attribute smob.
 */
static scm_sizet g_free_attrib_smob(SCM attrib_smob)
{
  struct st_attrib_smob *attribute = 
    (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);
  scm_sizet size = sizeof(struct st_attrib_smob);

  free(attribute); /* this should stay as free (allocated from guile) */
  return size;
}

/*! \brief Prints attribute smob to port.
 *  \par Function Description
 *  This function prints the given attribute smob to the port.
 *  It just prints a string showing it is an attribute and its string.
 *
 *  \param [in] attrib_smob  The attribute smob.
 *  \param [in] port         The port to print to.
 *  \param [in] pstate       Unused.
 *  \return non-zero means success.
 */
static int g_print_attrib_smob(SCM attrib_smob, SCM port,
			       scm_print_state *pstate)
{
  struct st_attrib_smob *attribute = 
    (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);

  if (attribute &&
      attribute->attribute &&
      attribute->attribute->text &&
      attribute->attribute->text->string ) {
    scm_puts("#<attribute ", port);
    scm_display (scm_from_locale_string (attribute->attribute->text->string),
                 port);
    scm_puts(">", port);
  }
	
  /* non-zero means success */
  return 1;
}


/*! \brief Creates a name-value smob
 *  \par Function Description
 *  This function Creates and returns a new attribute smob,
 *  based on the given TOPLEVEL curr_w and attribute curr_attr.
 *
 *  \param [in] curr_w     The current TOPLEVEL object.
 *  \param [in] curr_attr  The current attribute.
 *  \return SCM
 */
SCM g_make_attrib_smob(TOPLEVEL *curr_w, OBJECT *curr_attr)
{
  struct st_attrib_smob *smob_attribute;

  smob_attribute = (struct st_attrib_smob *)
    scm_malloc (sizeof(struct st_attrib_smob));

  smob_attribute->world     = curr_w;
  smob_attribute->attribute = curr_attr;

  /* Assumes Guile version >= 1.3.2 */
  SCM_RETURN_NEWSMOB(attrib_smob_tag, smob_attribute);
}

/*! \brief Get name and value of attribute.
 *  \par Function Description
 *  Returns a list with the name and value of the given attribute smob
 *  
 *  \param [in] attrib_smob  The attribute smob to get name and value from.
 *  \return A list containing the name and value of the attribute.
 */
SCM g_get_attrib_name_value(SCM attrib_smob)
{
  struct st_attrib_smob *attribute;
  char *name = NULL;
  char *value = NULL;
  SCM returned = SCM_EOL;

  SCM_ASSERT ( SCM_SMOB_PREDICATE (attrib_smob_tag, attrib_smob),
               attrib_smob, SCM_ARG1, "get-attribute-name-value");

  attribute = (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);

  if (attribute != NULL &&
      attribute->attribute != NULL &&
      o_attrib_get_name_value (attribute->attribute, &name, &value)) {
    returned = scm_cons (scm_from_locale_string (name),
                         scm_from_locale_string (value));
    g_free(name);
    g_free(value);
  }

  return returned;
}

/*! \brief Set the attribute value.
 *  \par Function Description
 *  This function puts the attribute smob name into a new_string and
 *  the new scm_value (attribute=value format). It also returns the
 *  TOPLEVEL and OBJECT pointers.
 *
 *  \param [in]     attrib_smob  The attribute to update.
 *  \param [in]     scm_value    The new value of the attribute.
 *  \param [in,out] world        The TOPLEVEL object.
 *  \param [in,out] o_attrib     Pointer to the updated attribute smob.
 *  \param [in]     new_string   Returns the attribute=value format string for the
 *                               updated attribute.
 *  \return Always SCM_UNDEFINED
 */
SCM g_set_attrib_value_internal(SCM attrib_smob, SCM scm_value, 
				TOPLEVEL **world, OBJECT **o_attrib,
				char *new_string[])
{
  struct st_attrib_smob *attribute;
  char *name = NULL;
  char *value = NULL;

  SCM_ASSERT (SCM_SMOB_PREDICATE (attrib_smob_tag, attrib_smob),
              attrib_smob, SCM_ARG1, "set-attribute-value!");
  SCM_ASSERT (scm_is_string(scm_value), scm_value, SCM_ARG2, 
	      "set-attribute-value!");

  attribute = (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);
  value = scm_to_locale_string (scm_value);

  if (attribute != NULL &&
      attribute->attribute != NULL) {

    o_attrib_get_name_value (attribute->attribute, &name, NULL);

    *new_string = g_strconcat (name, "=", value, NULL);
		
    *world = attribute->world;
    *o_attrib = attribute->attribute;

    g_free(name);
  }

  free (value);

  return SCM_UNDEFINED;
}

/*! \brief Calcule the attribute bounds as it has the given properties.
 *  \par Function Description
 *  Given an attribute, and a new angle, position and alignment, 
 *  this function calcules the bounds of the attribute with the new properties,
 *  but without modifying the attribute.
 *
 *  \param [in]     attrib_smob     The attribute.
 *  \param [in]     scm_alignment   The new alignment of the attribute.
 *     String with the alignment of the text. Possible values are:
 *       ""           : Keep the previous alignment.
 *       "Lower Left"
 *       "Middle Left"
 *       "Upper Left"
 *       "Lower Middle"
 *       "Middle Middle"
 *       "Upper Middle"
 *       "Lower Right"
 *       "Middle Right"
 *       "Upper Right"
 *  \param [in]     scm_angle       The new angle of the attribute, 
 *                                  or -1 to keep the previous angle.
 *  \param [in]     scm_x           The new x position of the attribute 
 *                                  or -1 to keep the previous value.
 *  \param [in]     scm_y           The new y position of the attribute 
 *                                  or -1 to keep the prevous value.
 *  \return A list of the form ( (x1 x2) (y1 y2) ) with:
 *       (x1, y1): bottom left corner.
 *       (x2, y2): upper right corner.
 */
SCM g_calcule_new_attrib_bounds (SCM attrib_smob, SCM scm_alignment,
				 SCM scm_angle, SCM scm_x, SCM scm_y) {

  TOPLEVEL *toplevel = NULL;
  OBJECT *object = NULL;
  struct st_attrib_smob *attribute;
  char *alignment_string;
  int alignment = -2;
  int angle = 0;
  int x = -1, y = -1;
  int old_angle, old_x, old_y, old_alignment;
  int left=0, right=0, top=0, bottom=0;
  SCM vertical = SCM_EOL;
  SCM horizontal = SCM_EOL;
  SCM returned = SCM_EOL;

  SCM_ASSERT (scm_is_string(scm_alignment), scm_alignment,
	      SCM_ARG2, "calcule-new-attrib-bounds");
  SCM_ASSERT ( scm_is_integer(scm_angle),
               scm_angle, SCM_ARG3, "calcule-new-attrib-bounds");
  SCM_ASSERT ( scm_is_integer(scm_x),
               scm_x, SCM_ARG4, "calcule-new-attrib-bounds");
  SCM_ASSERT ( scm_is_integer(scm_y),
               scm_y, SCM_ARG5, "calcule-new-attrib-bounds");

  angle = scm_to_int(scm_angle);
  x = scm_to_int(scm_x);
  y = scm_to_int(scm_y);
  
  alignment_string = scm_to_locale_string (scm_alignment);

  if (strlen(alignment_string) == 0) {
    alignment = -1;
  }
  if (strcmp(alignment_string, "Lower Left") == 0) {
    alignment = 0;
  }
  if (strcmp(alignment_string, "Middle Left") == 0) {
    alignment = 1;
  }
  if (strcmp(alignment_string, "Upper Left") == 0) {
    alignment = 2;
  }
  if (strcmp(alignment_string, "Lower Middle") == 0) {
    alignment = 3;
  }
  if (strcmp(alignment_string, "Middle Middle") == 0) {
    alignment = 4;
  }
  if (strcmp(alignment_string, "Upper Middle") == 0) {
    alignment = 5;
  }
  if (strcmp(alignment_string, "Lower Right") == 0) {
    alignment = 6;
  }
  if (strcmp(alignment_string, "Middle Right") == 0) {
    alignment = 7;
  }
  if (strcmp(alignment_string, "Upper Right") == 0) {
    alignment = 8;
  }

  free (alignment_string);

  if (alignment == -2) {
    /* Bad specified */
    SCM_ASSERT (scm_is_string(scm_alignment), scm_alignment,
		SCM_ARG2, "calcule-new-attrib-bounds");
  }

  attribute = (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);
  toplevel = attribute->world;
  
  SCM_ASSERT ( attribute &&
               attribute->attribute &&
               attribute->attribute->text &&
               attribute->attribute->text->string,
               attrib_smob, SCM_ARG1, "calcule-new-attrib-bounds");

  object = (OBJECT *) attribute->attribute;
  
  /* Store the previous values */
  old_alignment = object->text->alignment;
  old_angle = object->text->angle;
  old_x = object->text->x;
  old_y = object->text->y;
  
  /* Set the new ones */
  if (alignment != -1) 
    object->text->alignment = alignment;
  if (angle != -1)
    object->text->angle = angle;
  if (x != -1)
    object->text->x = x;
  if (y != -1)
	object->text->y = y;
  
  o_text_recreate(toplevel, object);

  /* Get the new bounds */
  world_get_text_bounds (toplevel, object,
			 &left, &top, &right, &bottom);
  
  /* Restore the original attributes */
  object->text->alignment = old_alignment;
  object->text->angle = old_angle;
  object->text->x = old_x;
  object->text->y = old_y;
  
  o_text_recreate(toplevel, object);

  /* Construct the return value */
  horizontal = scm_cons (scm_from_int(left), scm_from_int(right));
  vertical = scm_cons (scm_from_int(top), scm_from_int(bottom));
  returned = scm_cons (horizontal, vertical);

  return returned;
}

/*! \brief Initialize the framework to support an attribute smob.
 *  \par Function Description
 *  Initialize the framework to support an attribute smob.
 *
 */
void g_init_attrib_smob(void)
{

  attrib_smob_tag = scm_make_smob_type("attribute",
				       sizeof (struct st_attrib_smob));
  scm_set_smob_mark(attrib_smob_tag, 0);
  scm_set_smob_free(attrib_smob_tag, g_free_attrib_smob);
  scm_set_smob_print(attrib_smob_tag, g_print_attrib_smob);

  scm_c_define_gsubr("get-attribute-name-value", 1, 0, 0,
		     g_get_attrib_name_value);

  scm_c_define_gsubr ("get-attribute-bounds", 1, 0, 0, g_get_attrib_bounds);
  scm_c_define_gsubr ("get-attribute-angle", 1, 0, 0, g_get_attrib_angle);
  scm_c_define_gsubr ("calcule-new-attrib-bounds", 5, 0, 0, 
		      g_calcule_new_attrib_bounds);
  scm_c_define_gsubr ("attrib-inherited?", 1, 0, 0, g_attrib_is_inherited);
  

  return;
}

/*! \brief Get the bounds of an attribute.
 *  \par Function Description
 *  Get the bounds of an attribute.
 *  WARNING: top and bottom are mis-named in world-coords,
 *  top is the smallest "y" value, and bottom is the largest.
 *  Be careful! This doesn't correspond to what you'd expect,
 *  nor to the coordinate system who's origin is the bottom, left of the page.
 *  \param[in] attrib_smob the attribute.
 *  \return a list of the bounds of the <B>attrib smob</B>. 
 *  The list has the format: ( (left right) (top bottom) )
 */
SCM g_get_attrib_bounds(SCM attrib_smob)
{
  TOPLEVEL *toplevel;
  struct st_attrib_smob *attribute;
  SCM vertical = SCM_EOL;
  SCM horizontal = SCM_EOL;
  int left=0, right=0, bottom=0, top=0; 
  SCM returned = SCM_EOL;

  SCM_ASSERT (SCM_SMOB_PREDICATE (attrib_smob_tag, attrib_smob),
              attrib_smob, SCM_ARG1, "get-attribute-bounds");
  
  attribute = (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);
  toplevel = attribute->world;

  if (attribute &&
      attribute->attribute &&
      attribute->attribute->text &&
      attribute->attribute->text->string ) {

    world_get_text_bounds (toplevel, attribute->attribute, &left,
                           &top, &right, &bottom);

    horizontal = scm_cons (scm_from_int(left), scm_from_int(right));
    vertical = scm_cons (scm_from_int(top), scm_from_int(bottom));
    returned = scm_cons (horizontal, vertical);
  }

  return returned;
}

/*! \brief Get the angle of an attribute.
 *  \par Function Description
 *  Get the angle of an attribute.
 *  \param[in] attrib_smob the attribute.
 *  \return the angle of the <B>attrib smob</B>. 
 */
SCM g_get_attrib_angle(SCM attrib_smob)
{
  TOPLEVEL *toplevel;
  struct st_attrib_smob *attribute;

  SCM_ASSERT (SCM_SMOB_PREDICATE (attrib_smob_tag, attrib_smob),
              attrib_smob, SCM_ARG1, "get-attribute-angle");
  
  attribute = (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);
  toplevel = attribute->world;

  SCM_ASSERT ( attribute && 
               attribute->attribute &&
               attribute->attribute->text,
               attrib_smob, SCM_ARG1, "get-attribute-angle");

  return scm_from_int(attribute->attribute->text->angle);
}

/*! \brief Check if attribute is inherited.
 *  \par Function Description
 *  Return result of o_attrib_is_inherited().
 *  \param [in] attrib_smob Attribute to check.
 *  \return SCM_BOOL_F or SCM_BOOL_T.
 */
SCM g_attrib_is_inherited (SCM attrib_smob)
{
  struct st_attrib_smob *attribute =
    (struct st_attrib_smob *) SCM_SMOB_DATA (attrib_smob);

  if (attribute && attribute->attribute) {
    OBJECT *object = attribute->attribute;
    if (object && object->text && o_attrib_is_inherited (object))
       return SCM_BOOL_T;
  }

  return SCM_BOOL_F;
}

/*! \brief Free object smob memory.
 *  \par Function Description
 *  Free the memory allocated by the object smob and return its size.
 *
 *  \param [in] object_smob  The object smob to free.
 *  \return Size of object smob.
 */
static scm_sizet g_free_object_smob(SCM object_smob)
{
  struct st_object_smob *object = 
  (struct st_object_smob *)SCM_CDR(object_smob);
  scm_sizet size = sizeof(struct st_object_smob);

  free(object); /* this should stay as free (allocated from guile) */
  return size;
}

/*! \brief Prints object smob to port.
 *  \par Function Description
 *  This function prints the given object smob to the port.
 *  It just prints a string showing it is an object and the object name.
 *
 *  \param [in] object_smob  The object smob.
 *  \param [in] port         The port to print to.
 *  \param [in] pstate       Unused.
 *  \return non-zero means success.
 */
static int g_print_object_smob(SCM object_smob, SCM port,
			       scm_print_state *pstate)
{
  struct st_object_smob *object = 
  (struct st_object_smob *)SCM_CDR(object_smob);

  if (object &&
      object->object &&
      object->object->name) {
    scm_puts("#<object ", port);
    scm_display (scm_from_locale_string (object->object->name),
                 port);
    scm_puts(">", port);
  }
	
  /* non-zero means success */
  return 1;
}

/*! \brief Creates a object smob
 *  \par Function Description
 *  This function creates and returns a new object smob,
 *  from the given TOPLEVEL curr_w and object pointers.
 *
 *  \param [in] curr_w  The current TOPLEVEL object.
 *  \param [in] object  The current object.
 *  \return SCM
 */
SCM g_make_object_smob(TOPLEVEL *curr_w, OBJECT *object)
{
  struct st_object_smob *smob_object;

  smob_object = (struct st_object_smob *)
    scm_malloc (sizeof(struct st_object_smob));

  smob_object->world  = curr_w;
  smob_object->object = object;

  /* Assumes Guile version >= 1.3.2 */
  SCM_RETURN_NEWSMOB(object_smob_tag, smob_object);
}

/*! \brief Get all object attributes in a list.
 *  \par Function Description
 *  This function returns a list with all the attributes of a given object smob.
 *
 *  \param [in] object_smob  The object smob to get attributes from.
 *  \return A list of attributes associated with this object smob.
 */
SCM g_get_object_attributes(SCM object_smob)
{
  TOPLEVEL *toplevel;
  struct st_object_smob *object;
  SCM returned = SCM_EOL;
  GList *a_iter;
  OBJECT *a_current;

  SCM_ASSERT (SCM_SMOB_PREDICATE (object_smob_tag, object_smob),
              object_smob, SCM_ARG1, "get-object-attributes");

  object = (struct st_object_smob *)SCM_CDR(object_smob);

  if (object &&
      object->object) {

    toplevel = object->world;
    a_iter = object->object->attribs;
    while (a_iter != NULL) {
      a_current = a_iter->data;
      if (a_current && a_current->text) {
        returned = scm_cons (g_make_attrib_smob (toplevel, a_current),
                             returned);
      }
      a_iter = g_list_next (a_iter);
    }
  }

  return returned;
}

/*! \brief Get the value(s) of the attributes with the given name in the 
 *  given object.
 *  \par Function Description
 *  This function returns a list with all the attribute values, providing that
 *  its attribute name is the given name, in a given object smob.
 *
 *  \param [in] object_smob  The object smob to get attributes from.
 *  \param [in] scm_attrib_name  The name of the attribute you want the value.
 *  \return A list of attribute values.
 */
SCM g_get_attrib_value_by_attrib_name(SCM object_smob, SCM scm_attrib_name)
{
  TOPLEVEL *toplevel;
  struct st_object_smob *object;
  gchar *attrib_name=NULL;
  SCM returned = SCM_EOL;
  gchar *name=NULL, *value=NULL;
  GList *a_iter;
  OBJECT *a_current;

  SCM_ASSERT (SCM_SMOB_PREDICATE (object_smob_tag, object_smob),
              object_smob, SCM_ARG1, "get-attrib-value-by-attrib-name");

  SCM_ASSERT (scm_is_string(scm_attrib_name), scm_attrib_name,
	      SCM_ARG2, "get-attrib-value-by-attrib-name");

  /* Get parameters */
  object = (struct st_object_smob *)SCM_CDR(object_smob);
  attrib_name = scm_to_locale_string (scm_attrib_name);

  if (object && object->object) {

    toplevel = object->world;
    a_iter = object->object->attribs;
    while (a_iter != NULL) {
      a_current = a_iter->data;
      if (a_current != NULL &&
          o_attrib_get_name_value (a_current, &name, &value)) {
        if (strcmp(name, attrib_name) == 0)
          returned = scm_cons (scm_from_locale_string (value), returned);
        g_free (name);
        g_free (value);
      }
      a_iter = g_list_next (a_iter);
    }
  }

  free (attrib_name);

  return returned;
}

/*! \brief Get the object type.
 *  \par Function Description
 *  This function returns a string with the type of a given object smob.
 *
 *  \param [in] object_smob  The object smob to get the type from.
 *  \return A string with the type of the given object. 
 *   Actually it is the object->type character converted into a string.
 */
SCM g_get_object_type(SCM object_smob)
{
  struct st_object_smob *object_struct;
  OBJECT *object;
  SCM returned = SCM_EOL;

  SCM_ASSERT (SCM_SMOB_PREDICATE (object_smob_tag, object_smob),
              object_smob, SCM_ARG1, "get-object-type");

  object_struct = (struct st_object_smob *)SCM_CDR(object_smob);

  g_assert (object_struct && object_struct->object);
  
  object = (OBJECT *) object_struct->object;
  
  returned = SCM_MAKE_CHAR((unsigned char) object->type);

  return returned;
}

/*! \brief Get the line width used to draw an object.
 *  \par Function Description
 *  This function returns the line width used to draw an object.
 *
 *  \param [in] object_smob  The object smob to get the line width.
 *  \return The line width. 
 *   Actually it is the object->line_width.
 */
SCM g_get_line_width(SCM object_smob)
{
  struct st_object_smob *object_struct;
  OBJECT *object;
  SCM returned = SCM_EOL;

  SCM_ASSERT (SCM_SMOB_PREDICATE (object_smob_tag, object_smob),
              object_smob, SCM_ARG1, "get-line-width");

  object_struct = (struct st_object_smob *)SCM_CDR(object_smob);

  g_assert (object_struct && object_struct->object);
  
  object = (OBJECT *) object_struct->object;
  
  returned = scm_from_int(object->line_width);

  return returned;
}

/*! \brief Initialize the framework to support an object smob.
 *  \par Function Description
 *  Initialize the framework to support an object smob.
 *
 */
void g_init_object_smob(void)
{

  object_smob_tag = scm_make_smob_type("object", sizeof (struct st_object_smob));
  scm_set_smob_mark(object_smob_tag, 0);
  scm_set_smob_free(object_smob_tag, g_free_object_smob);
  scm_set_smob_print(object_smob_tag, g_print_object_smob);

  scm_c_define_gsubr("get-object-attributes", 1, 0, 0, g_get_object_attributes);
  scm_c_define_gsubr("get-attrib-value-by-attrib-name", 2, 0, 0, 
		     g_get_attrib_value_by_attrib_name);
  scm_c_define_gsubr("get-object-type", 1, 0, 0, g_get_object_type);
  scm_c_define_gsubr("get-line-width", 1, 0, 0, g_get_line_width);

  return;
}


/*! \brief Get the TOPLEVEL and OBJECT data from an object smob.
 *  \par Function Description
 *  Get the TOPLEVEL and OBJECT data from an object smob.
 *
 *  \param [in]  object_smob  The object smob to get data from.
 *  \param [out] toplevel     The TOPLEVEL to write data to.
 *  \param [out] object       The OBJECT to write data to.
 *  \return TRUE on success, FALSE otherwise
 */
gboolean g_get_data_from_object_smob(SCM object_smob, TOPLEVEL **toplevel, 
				     OBJECT **object)
{
  
  if (!SCM_SMOB_PREDICATE (object_smob_tag, object_smob)) {
    return(FALSE);
  }
  if (toplevel != NULL) {
    *toplevel = (TOPLEVEL *) 
    (((struct st_object_smob *)SCM_CDR(object_smob))->world);
  }
  if (object != NULL) {
    *object = (OBJECT *) 
    (((struct st_object_smob *)SCM_CDR(object_smob))->object);
  }
  return (TRUE);
}

/*! \brief Free page smob memory.
 *  \par Function Description
 *  Free the memory allocated by the page smob and return its size.
 *
 *  \param [in] page_smob  The page smob to free.
 *  \return Size of page smob.
 */
static scm_sizet g_free_page_smob(SCM page_smob)
{
  struct st_page_smob *page = 
  (struct st_page_smob *)SCM_CDR(page_smob);
  scm_sizet size = sizeof(struct st_page_smob);

  free(page); /* this should stay as free (allocated from guile) */
  return size;
}

/*! \brief Prints page smob to port.
 *  \par Function Description
 *  This function prints the given page smob to the port.
 *  It just prints a string showing it is a page and the page name.
 *
 *  \param [in] page_smob    The page smob.
 *  \param [in] port         The port to print to.
 *  \param [in] pstate       Unused.
 *  \return non-zero means success.
 */
static int g_print_page_smob(SCM page_smob, SCM port,
			       scm_print_state *pstate)
{
  struct st_page_smob *page = 
  (struct st_page_smob *)SCM_CDR(page_smob);

  if (page &&
      page->page &&
      page->page->page_filename) {
    scm_puts("#<page ", port);
    scm_display (scm_from_locale_string (page->page->page_filename),
                 port);
    scm_puts(">", port);
  }
	
  /* non-zero means success */
  return 1;
}

/*! \brief Compare two page smobs.
 *  \par Function Description
 *  This function compares two given page smobs for equality.
 *  Two smobs are equal if they point to same PAGE structure.
 *
 *  \param [in] page_smob1   The first page smob.
 *  \param [in] page_smob2   The second page smob.
 *  \return SCM_BOOL_T or SCM_BOOL_F
 */
static SCM g_equalp_page_smob (SCM page_smob1,
                               SCM page_smob2)
{
  struct st_page_smob *page1 =
    (struct st_page_smob *) SCM_SMOB_DATA (page_smob1);

  struct st_page_smob *page2 =
    (struct st_page_smob *) SCM_SMOB_DATA (page_smob2);

  if (page1 &&
      page2 &&
      page1->page == page2->page)
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}

/*! \brief Initialize the framework to support a page smob.
 *  \par Function Description
 *  Initialize the framework to support a page smob.
 *
 */
void g_init_page_smob(void)
{

  page_smob_tag = scm_make_smob_type("page",
				       sizeof (struct st_page_smob));
  scm_set_smob_mark(page_smob_tag, 0);
  scm_set_smob_free(page_smob_tag, g_free_page_smob);
  scm_set_smob_print(page_smob_tag, g_print_page_smob);
  scm_set_smob_equalp(page_smob_tag, g_equalp_page_smob);

  scm_c_define_gsubr ("get-page-filename", 1, 0, 0, g_get_page_filename);
  scm_c_define_gsubr ("set-page-filename", 2, 0, 0, g_set_page_filename);

  return;
}

/*! \brief Creates a page smob
 *  \par Function Description
 *  This function creates and returns a new page smob,
 *  from the given TOPLEVEL curr_w and page pointers.
 *
 *  \param [in] curr_w  The current TOPLEVEL object.
 *  \param [in] page    The page object.
 *  \return SCM         The new page smob
 */
SCM g_make_page_smob(TOPLEVEL *curr_w, PAGE *page)
{
  struct st_page_smob *smob_page;

  smob_page = (struct st_page_smob *)
    scm_malloc (sizeof(struct st_page_smob));

  smob_page->world  = curr_w;
  smob_page->page = page;

  /* Assumes Guile version >= 1.3.2 */
  SCM_RETURN_NEWSMOB(page_smob_tag, smob_page);
}

/*! \brief Get the TOPLEVEL and PAGE data from a page smob.
 *  \par Function Description
 *  Get the TOPLEVEL and OBJECT data from a page smob.
 *
 *  \param [in]  page_smob    The page smob to get data from.
 *  \param [out] toplevel     The TOPLEVEL to write data to.
 *  \param [out] page         The PAGE to write data to.
 *  \return TRUE on success, FALSE otherwise
 */
gboolean g_get_data_from_page_smob(SCM page_smob, TOPLEVEL **toplevel, 
				   PAGE **page)
{
  
  if (!SCM_SMOB_PREDICATE (page_smob_tag, page_smob)) {
    return(FALSE);
  }
  if (toplevel != NULL) {
    *toplevel = (TOPLEVEL *) 
    (((struct st_page_smob *) SCM_SMOB_DATA (page_smob))->world);
  }
  if (page != NULL) {
    *page = (PAGE *) 
    (((struct st_page_smob *) SCM_SMOB_DATA (page_smob))->page);
  }
  return (TRUE);
}

/*! \brief Get the page filename from a page smob.
 *  \par Function Description
 *  Get the page filename from a page smob.
 *
 *  \param [in]  page_smob    The page smob to get the filename from.
 *  \return the page filename or SCM_EOL if there was some error.
 */
SCM g_get_page_filename(SCM page_smob)
{
  SCM returned = SCM_EOL;
  PAGE *page;

  SCM_ASSERT (SCM_SMOB_PREDICATE (page_smob_tag, page_smob),
              page_smob, SCM_ARG1, "get-page-filename");

  page = (PAGE *) 
    (((struct st_page_smob *) SCM_SMOB_DATA (page_smob))->page);

  if (page->page_filename) 
    returned = scm_from_locale_string (page->page_filename);

  return (returned);
}

/*! \brief Set the page filename of the given page smob.
 *  \par Function Description
 *  Set the page filename of the given page smob.
 *
 *  \param [in]  page_smob    The page smob to set the filename from.
 *  \param [in]  scm_filename The filename to set.
 *  \return the page filename or SCM_EOL if there was some error.
 */
SCM g_set_page_filename(SCM page_smob, SCM scm_filename)
{
  PAGE *page;
  char *filename = NULL;

  SCM_ASSERT (SCM_SMOB_PREDICATE (page_smob_tag, page_smob),
              page_smob, SCM_ARG1, "set-page-filename");

  SCM_ASSERT (scm_is_string(scm_filename), scm_filename,
	      SCM_ARG2, "set-page-filename");

  page = (PAGE *) 
    (((struct st_page_smob *) SCM_SMOB_DATA (page_smob))->page);

  filename = SCM_STRING_CHARS (scm_filename);

  if (page->page_filename) 
    g_free(page->page_filename);

  page->page_filename = g_strdup(filename);

  return SCM_BOOL_T;
}

