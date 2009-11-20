/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* Private function declarations */
static void custom_world_get_single_object_bounds 
                                       (TOPLEVEL *toplevel, OBJECT *o_current,
                                        int *left, int *top, 
                                        int *right, int *bottom,
                                        GList *exclude_attrib_list,
					GList *exclude_obj_type_list);

static void custom_world_get_object_glist_bounds
  (TOPLEVEL *toplevel, GList *list,
   int *left, int *top, 
   int *right, int *bottom,
   GList *exclude_attrib_list,
   GList *exclude_obj_type_list);

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Makes a list of all attributes currently connected to object.
 * Uses the attribute list returned by o_attrib_return_attribs()
 */
SCM g_make_attrib_smob_list (GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  GList *attrib_list;
  GList *a_iter;
  OBJECT *a_current;
  SCM smob_list = SCM_EOL;

  if (object == NULL) {
    return SCM_EOL;
  }

  attrib_list = o_attrib_return_attribs (object);

  if (attrib_list == NULL)
    return SCM_EOL;

  /* go through attribs */
  for (a_iter = attrib_list; a_iter != NULL;
       a_iter = g_list_next (a_iter)) {
    a_current = a_iter->data;

    smob_list = scm_cons (g_make_attrib_smob (w_current->toplevel, a_current),
                          smob_list);
  }

  g_list_free (attrib_list);

  return smob_list;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/**************************************************************************
 * This function partly part of libgeda, since it belongs to the smob     *
 * definition. But since I use o_text_change, which is defined in gschem, *
 * we have to do it like this.                                            *
 **************************************************************************/
SCM g_set_attrib_value_x(SCM attrib_smob, SCM scm_value)
{
  SCM returned;
  TOPLEVEL *toplevel;
  OBJECT *o_attrib;
  char *new_string;

  returned = g_set_attrib_value_internal(attrib_smob, scm_value, 
                                         &toplevel, &o_attrib, &new_string);

  o_text_change(global_window_current, o_attrib, new_string,
                o_attrib->visibility, o_attrib->show_name_value);

  g_free(new_string);

  return returned;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/*
 * Adds an attribute <B>scm_attrib_name</B> with value <B>scm_attrib_value</B> to the given <B>object</B>.
The attribute has the visibility <B>scm_vis</B> and show <B>scm_show</B> flags.
The possible values are:
  - <B>scm_vis</B>: scheme boolean. Visible (TRUE) or hidden (FALSE).
  - <B>scm_show</B>: a list containing what to show: "name", "value", or both.
The return value is always TRUE.
 */
SCM g_add_attrib(SCM object, SCM scm_attrib_name, 
		 SCM scm_attrib_value, SCM scm_vis, SCM scm_show)
{
  GSCHEM_TOPLEVEL *w_current=global_window_current;
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current=NULL;
  gboolean vis;
  int show=0;
  gchar *attrib_name=NULL;
  gchar *attrib_value=NULL;
  gchar *value=NULL;
  int i;
  gchar *newtext=NULL;

  SCM_ASSERT (scm_is_string(scm_attrib_name), scm_attrib_name,
	      SCM_ARG2, "add-attribute-to-object");
  SCM_ASSERT (scm_is_string(scm_attrib_value), scm_attrib_value,
	      SCM_ARG3, "add-attribute-to-object");
  SCM_ASSERT (scm_boolean_p(scm_vis), scm_vis,
	      SCM_ARG4, "add-attribute-to-object");
  SCM_ASSERT (scm_list_p(scm_show), scm_show,
	      SCM_ARG5, "add-attribute-to-object");
  
  /* Get toplevel and o_current */
  SCM_ASSERT (g_get_data_from_object_smob (object, &toplevel, &o_current),
	      object, SCM_ARG1, "add-attribute-to-object");

  /* Get parameters */
  attrib_name = SCM_STRING_CHARS(scm_attrib_name);
  attrib_value = SCM_STRING_CHARS(scm_attrib_value);
  vis = SCM_NFALSEP(scm_vis);

  for (i=0; i<=scm_to_int(scm_length(scm_show))-1; i++) {
    /* Check every element in the list. It should be a string! */
    SCM_ASSERT(scm_list_ref(scm_show, scm_from_int(i)), 
	       scm_show,
	       SCM_ARG5, "add-attribute-to-object"); 
    SCM_ASSERT(scm_is_string(scm_list_ref(scm_show, scm_from_int(i))), 
	       scm_show,
	       SCM_ARG5, "add-attribute-to-object"); 
    
    value = SCM_STRING_CHARS(scm_list_ref(scm_show, scm_from_int(i)));
    
    SCM_ASSERT(value, scm_show,
	       SCM_ARG5, "add-attribute-to-object"); 

    /* Only "name" or "value" strings are allowed */
    SCM_ASSERT(!((strcasecmp(value, "name") != 0) &&
		 (strcasecmp(value, "value") != 0) ), scm_show,
	       SCM_ARG5, "add-attribute-to-object");
    
    /* show = 1 => show value; 
       show = 2 => show name; 
       show = 3 => show both */
    if (strcasecmp(value, "value") == 0) {
      show |= 1;
    }
    else if (strcasecmp(value, "name") == 0) {
      show |= 2;
    }	  
  }
  /* Show name and value (show = 3) => show=0 for gschem */
  if (show == 3) {
    show = 0;
  }
  
  newtext = g_strdup_printf("%s=%s", attrib_name, attrib_value);
  o_attrib_add_attrib (w_current, newtext, vis, show, o_current);
  g_free(newtext);

  return SCM_BOOL_T;

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/*
 * Returns a list with coords of the ends of  the given pin <B>object</B>.
The list is ( (x0 y0) (x1 y1) ), where the beginning is at (x0,y0) and the end at (x1,y1). 
The active connection end of the pin is the beginning, so this function cares about the whichend property of the pin object. If whichend is 1, then it has to reverse the ends.
 */
SCM g_get_pin_ends(SCM object)
{
  TOPLEVEL *toplevel;
  OBJECT *o_current;
  SCM coord1 = SCM_EOL;
  SCM coord2 = SCM_EOL;
  SCM coords = SCM_EOL;

  /* Get toplevel and o_current */
  SCM_ASSERT (g_get_data_from_object_smob (object, &toplevel, &o_current),
	      object, SCM_ARG1, "get-pin-ends");
  
  /* Check that it is a pin object */
  SCM_ASSERT (o_current != NULL,
	      object, SCM_ARG1, "get-pin-ends");
  SCM_ASSERT (o_current->type == OBJ_PIN,
	      object, SCM_ARG1, "get-pin-ends");
  SCM_ASSERT (o_current->line != NULL,
	      object, SCM_ARG1, "get-pin-ends");

  coord1 = scm_cons(scm_from_int(o_current->line->x[0]), 
		    scm_from_int(o_current->line->y[0]));
  coord2 = scm_cons(scm_from_int(o_current->line->x[1]),
		    scm_from_int(o_current->line->y[1]));
  if (o_current->whichend == 0) {
    coords = scm_cons(coord1, scm_list(coord2));
  } else {
    coords = scm_cons(coord2, scm_list(coord1));
  }    
		     
  return coords;  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/*
 * Sets several text properties of the given <B>attribute smob</B>:
  - <B>coloridx</B>: The index of the text color, or -1 to keep previous color.
  - <B>size</B>: Size (numeric) of the text, or -1 to keep the previous size.
  - <B>alignment</B>: String with the alignment of the text. Possible values are:
    * ""           : Keep the previous alignment.
    * "Lower Left"
    * "Middle Left"
    * "Upper Left"
    * "Lower Middle"
    * "Middle Middle"
    * "Upper Middle"
    * "Lower Right"
    * "Middle Right"
    * "Upper Right"
  - <B>rotation</B>: Angle of the text, or -1 to keep previous angle.
  - <B>x</B>, <B>y</B>: Coords of the text.
 */
SCM g_set_attrib_text_properties(SCM attrib_smob, SCM scm_coloridx,
				 SCM scm_size, SCM scm_alignment,
				 SCM scm_rotation, SCM scm_x, SCM scm_y)
{
  struct st_attrib_smob *attribute = 
  (struct st_attrib_smob *)SCM_CDR(attrib_smob);
  OBJECT *object;
  GSCHEM_TOPLEVEL *w_current = global_window_current;
  TOPLEVEL *toplevel = w_current->toplevel;

  int color = -1;
  int size = -1;
  char *alignment_string;
  int alignment = -2;
  int rotation = 0;
  int x = -1, y = -1;

  SCM_ASSERT (scm_is_integer(scm_coloridx), scm_coloridx,
	      SCM_ARG2, "set-attribute-text-properties!");
  SCM_ASSERT ( scm_is_integer(scm_size),
               scm_size, SCM_ARG3, "set-attribute-text-properties!");
  SCM_ASSERT (scm_is_string(scm_alignment), scm_alignment,
	      SCM_ARG4, "set-attribute-text-properties!");
  SCM_ASSERT ( scm_is_integer(scm_rotation),
               scm_rotation, SCM_ARG5, "set-attribute-text-properties!");
  SCM_ASSERT ( scm_is_integer(scm_x),
               scm_x, SCM_ARG6, "set-attribute-text-properties!");
  SCM_ASSERT ( scm_is_integer(scm_y),
               scm_y, SCM_ARG7, "set-attribute-text-properties!");

  color = scm_to_int(scm_coloridx);

  SCM_ASSERT (!(color < -1 || color >= MAX_COLORS),
              scm_coloridx, SCM_ARG2, "set-attribute-text-properties!");

  size = scm_to_int(scm_size);
  rotation = scm_to_int(scm_rotation);
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
		SCM_ARG4, "set-attribute-text-properties!");
  }

  if (attribute &&
      attribute->attribute) {
    object = attribute->attribute;
    if (object &&
	object->text) {
      o_invalidate (w_current, object);
      if (x != -1) {
	object->text->x = x;
      }
      if (y != -1) {
	object->text->y = y;
      }
      if (size != -1) {
	object->text->size = size;
      }
      if (alignment != -1) {
	object->text->alignment = alignment;
      }
      if (rotation != -1) {
	object->text->angle = rotation;
      }
      o_text_recreate(toplevel, object);
      if (!toplevel->DONT_REDRAW) {
        o_invalidate (w_current, object);
      }
    }
  }
  return SCM_BOOL_T;
}

/*! \brief Get the object bounds of the given object, excluding the object
 *  types given as parameters.
 *  \par Function Description
 *  Get the object bounds without considering the attributes in 
 *  exclude_attrib_list, neither the object types included in 
 *  exclude_obj_type_list
 *  \param [in] toplevel TOPLEVEL structure.
 *  \param [in] o_current The object we want to know the bounds of.
 *  \param [in] exclude_attrib_list A list with the attribute names we don't
 *  want to include when calculing the bounds.
 *  \param [in] exclude_obj_type_list A list with the object types we don't
 *  want to include when calculing the bounds. 
 *  The object types are those used in (OBJECT *)->type converted into strings.
 *  \param [out] left Left bound of the object.
 *  \param [out] top  Top bound of the object.
 *  \param [out] right Right bound of the object.
 *  \param [out] bottom  Bottom bound of the object.
 *
 */
static void custom_world_get_single_object_bounds 
                                       (TOPLEVEL *toplevel, OBJECT *o_current,
                                        int *left, int *top, 
                                        int *right, int *bottom,
                                        GList *exclude_attrib_list,
					GList *exclude_obj_type_list) {
    OBJECT *obj_ptr = NULL;
    OBJECT *a_current;
    GList *a_iter;
    int rleft, rright, rbottom, rtop;
    char *name_ptr, aux_ptr[2];
    gboolean include_text;

    *left = rleft = toplevel->init_right;
    *top = rtop = toplevel->init_bottom;;
    *right = *bottom = rright = rbottom = 0;
    
      obj_ptr = o_current;
      sprintf(aux_ptr, "%c", obj_ptr->type);
      include_text = TRUE;
      if (!g_list_find_custom(exclude_obj_type_list, aux_ptr, 
			      (GCompareFunc) &strcmp)) {

	switch(obj_ptr->type) {
          case (OBJ_PIN):
	    world_get_single_object_bounds (toplevel, obj_ptr,
					    &rleft, &rtop, &rright, &rbottom);
	    break;
          case (OBJ_TEXT):
            if (o_attrib_get_name_value (obj_ptr, &name_ptr, NULL) &&
                g_list_find_custom (exclude_attrib_list, name_ptr, (GCompareFunc) &strcmp)) {
              include_text = FALSE;
            }
            if (g_list_find_custom (exclude_attrib_list, "all",
                                    (GCompareFunc) &strcmp)) {
              include_text = FALSE;
            }
            if (include_text) {
              world_get_single_object_bounds (toplevel, obj_ptr,
                                              &rleft, &rtop, &rright, &rbottom);
            }
            g_free(name_ptr);
            break;
          case (OBJ_COMPLEX):
          case (OBJ_PLACEHOLDER):
	    custom_world_get_object_glist_bounds (toplevel,
						o_current->complex->prim_objs, 
						left, top, right, bottom,
						exclude_attrib_list,
						exclude_obj_type_list);
	    break;
	    
          default:
	    world_get_single_object_bounds (toplevel, obj_ptr,
					    &rleft, &rtop, &rright, &rbottom);
	    break;
	}

	if (rleft < *left) *left = rleft;
	if (rtop < *top) *top = rtop;
	if (rright > *right) *right = rright;
	if (rbottom > *bottom) *bottom = rbottom;
	
	/* If it's a pin object, check the pin attributes */
	if (obj_ptr->type == OBJ_PIN) {
	  a_iter = obj_ptr->attribs;
	  while (a_iter != NULL) {
      a_current = a_iter->data;

	    if (a_current->type == OBJ_TEXT) {
	      custom_world_get_single_object_bounds(toplevel,
						    a_current,
						    &rleft, &rtop, 
						    &rright, &rbottom,
						    exclude_attrib_list,
						    exclude_obj_type_list);
	      if (rleft < *left) *left = rleft;
	      if (rtop < *top) *top = rtop;
	      if (rright > *right) *right = rright;
	      if (rbottom > *bottom) *bottom = rbottom;
	    }
	    
	    a_iter = g_list_next (a_iter);
	  }
	}
      }      
}

static void custom_world_get_object_glist_bounds
  (TOPLEVEL *toplevel, GList *list,
   int *left, int *top, 
   int *right, int *bottom,
   GList *exclude_attrib_list,
   GList *exclude_obj_type_list) {
 
  OBJECT *o_current;
  GList *iter;
  int rleft, rtop, rright, rbottom;
	
  *left = rleft = 999999;
  *top = rtop = 9999999;
  *right = rright = 0;
  *bottom = rbottom = 0;
	

  iter = list;
	
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    custom_world_get_single_object_bounds (toplevel, o_current, &rleft, &rtop,
					  &rright, &rbottom,
					  exclude_attrib_list,
					  exclude_obj_type_list);
    if (rleft < *left) *left = rleft;
    if (rtop < *top) *top = rtop;
    if (rright > *right) *right = rright;
    if (rbottom > *bottom) *bottom = rbottom;

    iter = g_list_next (iter);
  }
}

/*! \brief Get the object bounds of the given object, excluding the object
 *  types or the attributes given as parameters.
 *  \par Function Description
 *  Get the object bounds without considering the attributes in 
 *  scm_exclude_attribs, neither the object types included in 
 *  scm_exclude_object_type
 *  \param [in] object_smob The object we want to know the bounds of.
 *  \param [in] scm_exclude_attribs A list with the attribute names we don't
 *  want to include when calculing the bounds.
 *  \param [in] scm_exclude_object_type A list with the object types we don't
 *  want to include when calculing the bounds. 
 *  The object types are those used in (OBJECT *)->type converted into strings.
 *  \return a list of the bounds of the <B>object smob</B>. 
 *  The list has the format: ( (left right) (top bottom) )
 *  WARNING: top and bottom are mis-named in world-coords,
 *  top is the smallest "y" value, and bottom is the largest.
 *  Be careful! This doesn't correspond to what you'd expect,
 *  nor to the coordinate system who's origin is the bottom, left of the page.
 */
SCM g_get_object_bounds (SCM object_smob, SCM scm_exclude_attribs, SCM scm_exclude_object_type)
{

  TOPLEVEL *toplevel=NULL;
  OBJECT *object=NULL;
  int left=0, right=0, bottom=0, top=0; 
  SCM returned = SCM_EOL;
  SCM vertical = SCM_EOL;
  SCM horizontal = SCM_EOL;
  GList *exclude_attrib_list = NULL, *exclude_obj_type_list = NULL;
  gboolean exclude_all_attribs = FALSE;
  int i;

  SCM_ASSERT (scm_list_p(scm_exclude_attribs), scm_exclude_attribs, 
 	      SCM_ARG2, "get-object-bounds"); 
  SCM_ASSERT (scm_list_p(scm_exclude_object_type), scm_exclude_object_type,
	      SCM_ARG3, "get-object-bounds");

  /* Build the exclude attrib list */
  for (i=0; i <= scm_to_int(scm_length(scm_exclude_attribs))-1; i++) {
    SCM_ASSERT (scm_is_string(scm_list_ref(scm_exclude_attribs, scm_from_int(i))), 
		scm_exclude_attribs, 
		SCM_ARG2, "get-object-bounds"); 
    exclude_attrib_list = g_list_append(exclude_attrib_list, 
					SCM_STRING_CHARS(scm_list_ref(scm_exclude_attribs,
								      scm_from_int(i))));
  }

  /* Build the exclude object type list */
  for (i=0; i <= scm_to_int(scm_length(scm_exclude_object_type))-1; i++) {
    SCM_ASSERT (scm_is_string(scm_list_ref(scm_exclude_object_type, scm_from_int(i))), 
		scm_exclude_object_type, 
		SCM_ARG3, "get-object-bounds"); 
    exclude_obj_type_list = g_list_append(exclude_obj_type_list, 
					SCM_STRING_CHARS(scm_list_ref(scm_exclude_object_type,
								      scm_from_int(i))));
  }

  /* Get toplevel and o_current. */
  g_get_data_from_object_smob (object_smob, &toplevel, &object);
  
  SCM_ASSERT (toplevel && object,
	      object_smob, SCM_ARG1, "get-object-bounds");

  if (g_list_find_custom(exclude_attrib_list, "all", (GCompareFunc) &strcmp))
    exclude_all_attribs = TRUE;

  custom_world_get_single_object_bounds (toplevel, object,
					 &left, &top, 
					 &right, &bottom, 
					 exclude_attrib_list,
					 exclude_obj_type_list);
  
  /* Free the exclude attrib_list. Don't free the nodes!! */
  g_list_free(exclude_attrib_list);

  /* Free the exclude attrib_list. Don't free the nodes!! */
  g_list_free(exclude_obj_type_list);
  
  horizontal = scm_cons (scm_from_int(left), scm_from_int(right));
  vertical = scm_cons (scm_from_int(top), scm_from_int(bottom));
  returned = scm_cons (horizontal, vertical);
  return (returned);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/*
 *Returns a list of the pins of the <B>object smob</B>.
 */
SCM g_get_object_pins (SCM object_smob)
{
  TOPLEVEL *toplevel=NULL;
  OBJECT *object=NULL;
  OBJECT *prim_obj;
  GList *iter;
  SCM returned=SCM_EOL;

  /* Get toplevel and o_current */
  SCM_ASSERT (g_get_data_from_object_smob (object_smob, &toplevel, &object),
	      object_smob, SCM_ARG1, "get-object-pins");

  if (!object) {
    return (returned);
  }
  if (object->complex && object->complex->prim_objs) {
    iter = object->complex->prim_objs;
    while (iter != NULL) {
      prim_obj = (OBJECT *)iter->data;
      if (prim_obj->type == OBJ_PIN) {
	returned = scm_cons (g_make_object_smob(toplevel, prim_obj),returned);
      }
      iter = g_list_next (iter);
    }
  }
  
  return (returned);
}

/*! \brief Add a component to the page.
 *  \par Function Description
 *  Adds a component <B>scm_comp_name</B> to the schematic, at 
 *  position (<B>scm_x</B>, <B>scm_y</B>), with some properties set by 
 *  the parameters:
 *  \param [in,out] page_smob Schematic page
 *  \param [in] scm_comp_name Component to be added
 *  \param [in] scm_x Coordinate X of the symbol.
 *  \param [in] scm_y Coordinate Y of the symbol.
 *  \param [in] scm_angle Angle of rotation of the symbol.
 *  \param [in] scm_selectable True if the symbol is selectable, false otherwise.
 *  \param [in] scm_mirror True if the symbol is mirrored, false otherwise.
 *  If scm_comp_name is a scheme empty list, SCM_BOOL_F, or an empty 
 *  string (""), then g_add_component returns SCM_BOOL_F without writing 
 *  to the log.
 *  \return TRUE if the component was added, FALSE otherwise.
 *
 */
SCM g_add_component(SCM page_smob, SCM scm_comp_name, SCM scm_x, SCM scm_y, 
		    SCM scm_angle, SCM scm_selectable, SCM scm_mirror)
{
  TOPLEVEL *toplevel;
  PAGE *page;
  gboolean selectable, mirror;
  gchar *comp_name;
  int x, y, angle;
  OBJECT *new_obj;
  const CLibSymbol *clib;

  /* Return if scm_comp_name is NULL (an empty list) or scheme's FALSE */
  if (SCM_NULLP(scm_comp_name) || 
      (SCM_BOOLP(scm_comp_name) && !(SCM_NFALSEP(scm_comp_name))) ) {
    return SCM_BOOL_F;
  }

  /* Get toplevel and the page */
  SCM_ASSERT (g_get_data_from_page_smob (page_smob, &toplevel, &page),
	      page_smob, SCM_ARG1, "add-component-at-xy");
  /* Check the arguments */
  SCM_ASSERT (scm_is_string(scm_comp_name), scm_comp_name,
	      SCM_ARG2, "add-component-at-xy");
  SCM_ASSERT ( scm_is_integer(scm_x), scm_x, 
               SCM_ARG3, "add-component-at-xy");
  SCM_ASSERT ( scm_is_integer(scm_y), scm_y, 
               SCM_ARG4, "add-component-at-xy");
  SCM_ASSERT ( scm_is_integer(scm_angle), scm_angle, 
               SCM_ARG5, "add-component-at-xy");
  SCM_ASSERT ( scm_boolean_p(scm_selectable), scm_selectable,
	       SCM_ARG6, "add-component-at-xy");
  SCM_ASSERT ( scm_boolean_p(scm_mirror), scm_mirror,
	       SCM_ARG7, "add-component-at-xy");

  /* Get the parameters */
  comp_name = SCM_STRING_CHARS(scm_comp_name);
  x = scm_to_int(scm_x);
  y = scm_to_int(scm_y);
  angle = scm_to_int(scm_angle);  
  selectable = SCM_NFALSEP(scm_selectable);
  mirror = SCM_NFALSEP(scm_mirror);

  SCM_ASSERT (comp_name, scm_comp_name,
	      SCM_ARG2, "add-component-at-xy");
  
  if (strcmp(comp_name, "") == 0) {
    return SCM_BOOL_F;
  }

  clib = s_clib_get_symbol_by_name (comp_name);

  new_obj = o_complex_new (toplevel, 'C', DEFAULT_COLOR, x, y, angle, mirror,
                           clib, comp_name, selectable);
  s_page_append_list (toplevel, page,
                      o_complex_promote_attribs (toplevel, new_obj));
  s_page_append (toplevel, page, new_obj);
  

  /* Run the add component hook for the new component */
  if (scm_hook_empty_p(add_component_object_hook) == SCM_BOOL_F) {
    scm_run_hook(add_component_object_hook,
		 scm_cons(g_make_object_smob(toplevel,
					     new_obj), SCM_EOL));
  }
  
  /* 
   * For now, do not redraw the newly added complex, since this might cause
   * flicker if you are zoom/panning right after this function executes 
   */
#if 0 
  /* Now the new component should be added to the object's list and 
     drawn in the screen */
  o_invalidate (toplevel, new_object);
#endif
  
  return SCM_BOOL_T;        
}

/*! \brief Return the objects in a page.
 *  \par Function Description
 *  Returns an object smob list with all the objects in the given page.
 *  \param [in] page_smob Page to look at.
 *  \return the object smob list with the objects in the page.
 *
 */
SCM g_get_objects_in_page(SCM page_smob) {

  TOPLEVEL *toplevel;
  PAGE *page;
  OBJECT *object;
  const GList *iter;
  SCM return_list=SCM_EOL;

  /* Get toplevel and the page */
  SCM_ASSERT (g_get_data_from_page_smob (page_smob, &toplevel, &page),
	      page_smob, SCM_ARG1, "add-component");

  if (page && s_page_objects (page)) {
    iter = s_page_objects (page);
    while (iter != NULL) {
      object = (OBJECT *)iter->data;
      return_list = scm_cons (g_make_object_smob(toplevel, object),
			      return_list);
      iter = g_list_next (iter);
    }
  }

  return return_list;
} 

SCM g_get_current_page(void)
{
  return (g_make_page_smob(global_window_current->toplevel,
			   global_window_current->toplevel->page_current));
}
