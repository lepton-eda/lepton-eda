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

#include <math.h>
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"

#include "../include/prototype.h"

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
  (struct st_attrib_smob *)SCM_CDR(attrib_smob);
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
  (struct st_attrib_smob *)SCM_CDR(attrib_smob);

  if (attribute &&
      attribute->attribute &&
      attribute->attribute->object &&
      attribute->attribute->object->text &&
      attribute->attribute->object->text->string ) {
    scm_puts("#<attribute ", port);
    scm_display (scm_makfrom0str (attribute->attribute->object->text->string),
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
SCM g_make_attrib_smob(TOPLEVEL *curr_w, ATTRIB *curr_attr)
{
  struct st_attrib_smob *smob_attribute;

  smob_attribute = (struct st_attrib_smob *)
    			scm_must_malloc(sizeof(struct st_attrib_smob),
					"attribute");

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

  SCM_ASSERT ( SCM_NIMP(attrib_smob) && 
               ((long) SCM_CAR(attrib_smob) == attrib_smob_tag),
               attrib_smob, SCM_ARG1, "get-attribute-name-value");

  attribute = (struct st_attrib_smob *)SCM_CDR(attrib_smob);

  if (attribute &&
      attribute->attribute &&
      attribute->attribute->object &&
      attribute->attribute->object->text->string ) {
    o_attrib_get_name_value(attribute->attribute->object->text->string, 
                            &name, &value );
    returned = scm_cons (scm_makfrom0str (name),
                         scm_makfrom0str (value));
    if (name) g_free(name);
    if (value) g_free(value);
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
  char *old_value = NULL;

  SCM_ASSERT ( SCM_NIMP(attrib_smob) && 
               ((long) SCM_CAR(attrib_smob) == attrib_smob_tag),
               attrib_smob, SCM_ARG1, "set-attribute-value!");
  SCM_ASSERT (scm_is_string(scm_value), scm_value, SCM_ARG2, 
	      "set-attribute-value!");

  attribute = (struct st_attrib_smob *)SCM_CDR(attrib_smob);
  value = SCM_STRING_CHARS (scm_value);

  if (attribute &&
      attribute->attribute &&
      attribute->attribute->object &&
      attribute->attribute->object->text &&
      attribute->attribute->object->text->string ) {

    o_attrib_get_name_value(attribute->attribute->object->text->string, 
                            &name, &old_value );

    *new_string = g_strconcat (name, "=", value, NULL);
		
    *world = attribute->world;
    *o_attrib = attribute->attribute->object;

    if (name) g_free(name);
    if (old_value) g_free(old_value);
  }

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

  TOPLEVEL *w_current = NULL;
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
  
  alignment_string = SCM_STRING_CHARS(scm_alignment);

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
  if (alignment == -2) {
    /* Bad specified */
    SCM_ASSERT (scm_is_string(scm_alignment), scm_alignment,
		SCM_ARG2, "calcule-new-attrib-bounds");
  }

  attribute = (struct st_attrib_smob *)SCM_CDR(attrib_smob);
  w_current = attribute->world;
  
  SCM_ASSERT ( attribute &&
	       attribute->attribute &&
	       attribute->attribute->object &&
	       attribute->attribute->object->text &&
	       attribute->attribute->object->text->string,
	       attrib_smob, SCM_ARG1, "calcule-new-attrib-bounds");

  object = (OBJECT *) attribute->attribute->object;
  
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
  
  o_text_recreate(w_current, object);

  /* Get the new bounds */
  world_get_text_bounds (w_current, object, 
			 &left, &top, &right, &bottom);
  
  /* Restore the original attributes */
  object->text->alignment = old_alignment;
  object->text->angle = old_angle;
  object->text->x = old_x;
  object->text->y = old_y;
  
  o_text_recreate(w_current, object);

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
  TOPLEVEL *w_current;
  struct st_attrib_smob *attribute;
  SCM vertical = SCM_EOL;
  SCM horizontal = SCM_EOL;
  int left=0, right=0, bottom=0, top=0; 
  SCM returned = SCM_EOL;

  SCM_ASSERT ( SCM_NIMP(attrib_smob) && 
               ((long) SCM_CAR(attrib_smob) == attrib_smob_tag),
               attrib_smob, SCM_ARG1, "get-attribute-bounds");
  
  attribute = (struct st_attrib_smob *)SCM_CDR(attrib_smob);
  w_current = attribute->world;

  if (attribute &&
      attribute->attribute &&
      attribute->attribute->object &&
      attribute->attribute->object->text->string ) {

    world_get_text_bounds (w_current, attribute->attribute->object,
                           &left, &top, &right, &bottom);

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
  TOPLEVEL *w_current;
  struct st_attrib_smob *attribute;

  SCM_ASSERT ( SCM_NIMP(attrib_smob) && 
               ((long) SCM_CAR(attrib_smob) == attrib_smob_tag),
               attrib_smob, SCM_ARG1, "get-attribute-angle");
  
  attribute = (struct st_attrib_smob *)SCM_CDR(attrib_smob);
  w_current = attribute->world;

  SCM_ASSERT ( attribute && 
               attribute->attribute &&
	       attribute->attribute->object &&
	       attribute->attribute->object->text,
               attrib_smob, SCM_ARG1, "get-attribute-angle");

  return scm_from_int(attribute->attribute->object->text->angle);
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
    scm_display (scm_makfrom0str (object->object->name),
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
    scm_must_malloc(sizeof(struct st_object_smob), "object");

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
  TOPLEVEL *w_current;
  struct st_object_smob *object;
  SCM returned = SCM_EOL;

  SCM_ASSERT ( SCM_NIMP(object_smob) && 
               ((long) SCM_CAR(object_smob) == object_smob_tag),
               object_smob, SCM_ARG1, "get-object-attributes");

  object = (struct st_object_smob *)SCM_CDR(object_smob);

  if (object &&
      object->object) {
    ATTRIB *pointer;
    
    pointer = object->object->attribs;
    w_current = object->world;
    while (pointer != NULL) {
      if (pointer->object &&
	  pointer->object->text) {
	returned = scm_cons (g_make_attrib_smob (w_current, pointer), returned);
      }
      pointer = pointer->next;
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
 *  \param [in] attrib_name  The name of the attribute you want the value.
 *  \return A list of attribute values.
 */
SCM g_get_attrib_value_by_attrib_name(SCM object_smob, SCM scm_attrib_name)
{
  TOPLEVEL *w_current;
  struct st_object_smob *object;
  gchar *attrib_name=NULL;
  SCM returned = SCM_EOL;
  gchar *name=NULL, *value=NULL;

  SCM_ASSERT ( SCM_NIMP(object_smob) && 
               ((long) SCM_CAR(object_smob) == object_smob_tag),
               object_smob, SCM_ARG1, "get-attrib-value-by-attrib-name");

  SCM_ASSERT (scm_is_string(scm_attrib_name), scm_attrib_name,
	      SCM_ARG2, "get-attrib-value-by-attrib-name");

  /* Get parameters */
  object = (struct st_object_smob *)SCM_CDR(object_smob);
  attrib_name = SCM_STRING_CHARS(scm_attrib_name);

  if (object &&
      object->object) {
    ATTRIB *pointer;
    
    pointer = object->object->attribs;
    w_current = object->world;
    while (pointer != NULL) {
      if (pointer->object &&
	  pointer->object->text) {
	o_attrib_get_name_value(pointer->object->text->string, 
				&name, &value );
	if (strcmp(name, attrib_name) == 0) 
	  returned = scm_cons (scm_makfrom0str (value), returned);
      }
      pointer = pointer->next;
    }     
  }

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

  SCM_ASSERT ( SCM_NIMP(object_smob) && 
               ((long) SCM_CAR(object_smob) == object_smob_tag),
               object_smob, SCM_ARG1, "get-object-type");

  object_struct = (struct st_object_smob *)SCM_CDR(object_smob);

  g_assert (object_struct && object_struct->object);
  
  object = (OBJECT *) object_struct->object;
  
  returned = SCM_MAKE_CHAR((unsigned char) object->type);

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

  return;
}

#if 0
/*! \brief Gets the OBJECT data from the object smob.
 *  \par Function Description
 *  Get the OBJECT data from the object smob.
 *
 *  \param [in] object_smob  The object smob to get the OBJECT data from.
 *  \return The OBJECT data.
 *  \deprecated
 *  \todo check and remove?
 */
OBJECT *g_get_object_from_object_smob(SCM object_smob)
{
  
  SCM_ASSERT ( SCM_NIMP(object_smob) && 
               (SCM_CAR(object_smob) == object_smob_tag),
               object_smob, SCM_ARG1, "get_object_from_object_smob");
  return ((OBJECT *) (((struct st_object_smob *)SCM_CDR(object_smob))->object));
}

/*! \brief Get the TOPLEVEL data from the object smob.
 *  \par Function Description
 *  Get the TOPLEVEL data from the object smob.
 *
 *  \param [in] object_smob  The object smob to get the TOPLEVEL data from.
 *  \return The TOPLEVEL data.
 *  \deprecated
 *  \todo check and remove?
 */
TOPLEVEL *g_get_toplevel_from_object_smob(SCM object_smob)
{
  
  SCM_ASSERT ( SCM_NIMP(object_smob) && 
               (SCM_CAR(object_smob) == object_smob_tag),
               object_smob, SCM_ARG1, "get_toplevel_from_object_smob");
  return ((TOPLEVEL *) (((struct st_object_smob *)SCM_CDR(object_smob))->world));
}
#endif

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
  
  if ( (!SCM_NIMP(object_smob)) || 
       ((long) SCM_CAR(object_smob) != object_smob_tag) ) {
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
    scm_display (scm_makfrom0str (page->page->page_filename),
                 port);
    scm_puts(">", port);
  }
	
  /* non-zero means success */
  return 1;
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

  scm_c_define_gsubr ("get-page-filename", 1, 0, 0, g_get_page_filename);

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
    scm_must_malloc(sizeof(struct st_page_smob), "page");

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
  
  if ( (!SCM_NIMP(page_smob)) || 
       ((long) SCM_CAR(page_smob) != page_smob_tag) ) {
    return(FALSE);
  }
  if (toplevel != NULL) {
    *toplevel = (TOPLEVEL *) 
    (((struct st_page_smob *)SCM_CDR(page_smob))->world);
  }
  if (page != NULL) {
    *page = (PAGE *) 
    (((struct st_page_smob *)SCM_CDR(page_smob))->page);
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

  SCM_ASSERT ( SCM_NIMP(page_smob) &&
	       ((long) SCM_CAR(page_smob) == page_smob_tag),
               page_smob, SCM_ARG1, "get-page-filename");

  page = (PAGE *) 
    (((struct st_page_smob *)SCM_CDR(page_smob))->page);

  if (page->page_filename) 
    returned = scm_makfrom0str (page->page_filename);

  return (returned);
}

