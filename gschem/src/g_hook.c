/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* Makes a list of all attributes currently connected to curr_object. *
 * Principle stolen from o_attrib_return_attribs */
SCM g_make_attrib_smob_list(TOPLEVEL *curr_w, OBJECT *curr_object)
{
  ATTRIB *a_current;      
  OBJECT *object;
  SCM smob_list = SCM_EOL;

  object = (OBJECT *) o_list_search(curr_object, curr_object);

  if (!object) {
    return(SCM_EOL);   
  }

  if (!object->attribs) {
    return(SCM_EOL);
  }

  if (!object->attribs->next) {
    return(SCM_EOL);
  }

  /* go through attribs */
  a_current = object->attribs->next;      
  while(a_current != NULL) {
    if (a_current->object->type == OBJ_TEXT && 
        a_current->object->text) {
      if (a_current->object->text->string) {
        smob_list = scm_cons (g_make_attrib_smob (curr_w, a_current), 
                              smob_list);
      }
    } else {
      printf(_("Attribute failed ot find.\n"));
    }
    a_current = a_current->next;
  }

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
  TOPLEVEL *world;
  OBJECT *o_attrib;
  char *new_string;

  returned = g_set_attrib_value_internal(attrib_smob, scm_value, 
                                         &world, &o_attrib, &new_string);

  o_text_change(world, o_attrib, new_string, o_attrib->visibility, o_attrib->show_name_value);

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
  TOPLEVEL *w_current=NULL; 
  OBJECT *o_current=NULL;
  gboolean vis;
  int show=0;
  gchar *attrib_name=NULL;
  gchar *attrib_value=NULL;
  gchar *value=NULL;
  int i;
  gchar *newtext=NULL;

  SCM_ASSERT (SCM_STRINGP(scm_attrib_name), scm_attrib_name,
	      SCM_ARG2, "add-attribute-to-object");
  SCM_ASSERT (SCM_STRINGP(scm_attrib_value), scm_attrib_value,
	      SCM_ARG3, "add-attribute-to-object");
  SCM_ASSERT (scm_boolean_p(scm_vis), scm_vis,
	      SCM_ARG4, "add-attribute-to-object");
  SCM_ASSERT (scm_list_p(scm_show), scm_show,
	      SCM_ARG5, "add-attribute-to-object");
  
  /* Get w_current and o_current */
  SCM_ASSERT (g_get_data_from_object_smob (object, &w_current, &o_current),
	      object, SCM_ARG1, "add-attribute-to-object");

  /* Get parameters */
  attrib_name = SCM_STRING_CHARS(scm_attrib_name);
  attrib_value = SCM_STRING_CHARS(scm_attrib_value);
  vis = SCM_NFALSEP(scm_vis);

  for (i=0; i<=SCM_INUM(scm_length(scm_show))-1; i++) {
    /* Check every element in the list. It should be a string! */
    SCM_ASSERT(scm_list_ref(scm_show, SCM_MAKINUM(i)), 
	       scm_show,
	       SCM_ARG5, "add-attribute-to-object"); 
    SCM_ASSERT(SCM_STRINGP(scm_list_ref(scm_show, SCM_MAKINUM(i))), 
	       scm_show,
	       SCM_ARG5, "add-attribute-to-object"); 
    
    value = SCM_STRING_CHARS(scm_list_ref(scm_show, SCM_MAKINUM(i)));
    
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
  TOPLEVEL *w_current;
  OBJECT *o_current;
  SCM coord1 = SCM_EOL;
  SCM coord2 = SCM_EOL;
  SCM coords = SCM_EOL;

  /* Get w_current and o_current */
  SCM_ASSERT (g_get_data_from_object_smob (object, &w_current, &o_current),
	      object, SCM_ARG1, "get-pin-ends");
  
  /* Check that it is a pin object */
  SCM_ASSERT (o_current != NULL,
	      object, SCM_ARG1, "get-pin-ends");
  SCM_ASSERT (o_current->type == OBJ_PIN,
	      object, SCM_ARG1, "get-pin-ends");
  SCM_ASSERT (o_current->line != NULL,
	      object, SCM_ARG1, "get-pin-ends");

  coord1 = scm_cons(SCM_MAKINUM(o_current->line->x[0]), 
		    SCM_MAKINUM(o_current->line->y[0]));
  coord2 = scm_cons(SCM_MAKINUM(o_current->line->x[1]),
		    SCM_MAKINUM(o_current->line->y[1]));
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
  - <B>colorname</B>: The colorname of the text, or "" to keep previous color.
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
SCM g_set_attrib_text_properties(SCM attrib_smob, SCM scm_colorname,
				 SCM scm_size, SCM scm_alignment,
				 SCM scm_rotation, SCM scm_x, SCM scm_y)
{
  struct st_attrib_smob *attribute = 
  (struct st_attrib_smob *)SCM_CDR(attrib_smob);
  OBJECT *object;
  TOPLEVEL *w_current = (TOPLEVEL *) attribute->world;

  char *colorname=NULL;
  int color=0;
  int size = -1;
  char *alignment_string;
  int alignment = -2;
  int rotation = 0;
  int x = -1, y = -1;
  gboolean changed = FALSE;

  SCM_ASSERT (SCM_STRINGP(scm_colorname), scm_colorname,
	      SCM_ARG2, "set-attribute-text-properties!");
  SCM_ASSERT ( SCM_INUMP(scm_size),
               scm_size, SCM_ARG3, "set-attribute-text-properties!");
  SCM_ASSERT (SCM_STRINGP(scm_alignment), scm_alignment,
	      SCM_ARG4, "set-attribute-text-properties!");
  SCM_ASSERT ( SCM_INUMP(scm_rotation),
               scm_rotation, SCM_ARG5, "set-attribute-text-properties!");
  SCM_ASSERT ( SCM_INUMP(scm_x),
               scm_x, SCM_ARG6, "set-attribute-text-properties!");
  SCM_ASSERT ( SCM_INUMP(scm_y),
               scm_y, SCM_ARG7, "set-attribute-text-properties!");

  colorname = SCM_STRING_CHARS(scm_colorname);

  if (colorname && strlen(colorname) != 0) {
    SCM_ASSERT ( (color = s_color_get_index(colorname)) != -1,
		 scm_colorname, SCM_ARG2, "set-attribute-text-properties!");
  }
  else {
    color = -1;
  }
  
  size = SCM_INUM(scm_size);
  rotation = SCM_INUM(scm_rotation);
  x = SCM_INUM(scm_x);
  y = SCM_INUM(scm_y);
  
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
    SCM_ASSERT (SCM_STRINGP(scm_alignment), scm_alignment,
		SCM_ARG4, "set-attribute-text-properties!");
  }

  if (attribute &&
      attribute->attribute &&
      attribute->attribute->object) {
    object = (OBJECT *) attribute->attribute->object;
    if (object &&
	object->text) {
      o_text_erase(w_current, object);
      if (x != -1) {
	object->text->x = x;
	changed = TRUE;
      }
      if (y != -1) {
	object->text->y = y;
	changed = TRUE;
      }
      if (changed) {
	WORLDtoSCREEN(w_current, x, y, &object->text->screen_x, &object->text->screen_y);
      }
      if (size != -1) {
	object->text->size = size;
	changed = TRUE;
      }
      if (alignment != -1) {
	object->text->alignment = alignment;
	changed = TRUE;
      }
      if (rotation != -1) {
	object->text->angle = rotation;
	changed = TRUE;
      }
      o_text_recreate(w_current, object);
      if (!w_current->DONT_REDRAW) {
	o_text_draw(w_current, object);
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
 *  \param [in] w_current TOPLEVEL structure.
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
static void custom_world_get_complex_bounds (TOPLEVEL *w_current, OBJECT *o_current,
                                        int *left, int *top, 
                                        int *right, int *bottom,
                                        GList *exclude_attrib_list,
					GList *exclude_obj_type_list) {
    OBJECT *obj_ptr = NULL;
    ATTRIB *attr_ptr = NULL;
    int rleft, rright, rbottom, rtop;
    char *text_value; 
    char *name_ptr, *value_ptr, aux_ptr[2];
    gboolean include_text;

    *left = rleft = w_current->init_right;
    *top = rtop = w_current->init_bottom;;
    *right = *bottom = rright = rbottom = 0;
    
    if (o_current->type == OBJ_PIN) {
      attr_ptr = o_current->attribs;
      if (attr_ptr)
	obj_ptr = attr_ptr->object;
      else 
	obj_ptr = NULL;
    } else {
      obj_ptr = o_current;
    }

    while (obj_ptr != NULL) {
      sprintf(aux_ptr, "%c", obj_ptr->type);
      include_text = TRUE;

      if (!g_list_find_custom(exclude_obj_type_list, aux_ptr, (GCompareFunc) &strcmp)) {
	switch(obj_ptr->type) {
          case (OBJ_PIN):
	    world_get_single_object_bounds (w_current, obj_ptr,
					    &rleft, &rtop, &rright, &rbottom);
	    break;
          case (OBJ_TEXT):
	    if (obj_ptr->text && obj_ptr->text->string) {
	      text_value = obj_ptr->text->string;
	      if (o_attrib_get_name_value(text_value, &name_ptr, &value_ptr) &&
		  g_list_find_custom(exclude_attrib_list, name_ptr, (GCompareFunc) &strcmp)) {
		include_text = FALSE;
	      }
	      if (g_list_find_custom(exclude_attrib_list, "all", (GCompareFunc) &strcmp))
		include_text = FALSE;
	      if (include_text) {
		world_get_single_object_bounds (w_current, obj_ptr, 
						&rleft, &rtop, &rright, &rbottom);
	      }
	      g_free(name_ptr);
	      g_free(value_ptr);
	    }
	    break;
          case (OBJ_COMPLEX):
          case (OBJ_PLACEHOLDER):
	    custom_world_get_complex_bounds(w_current, 
					    o_current->complex->prim_objs, 
					    left, top, right, bottom,
					    exclude_attrib_list,
					    exclude_obj_type_list);          
	    break;
	    
          default:
	    world_get_single_object_bounds (w_current, obj_ptr, 
					    &rleft, &rtop, &rright, &rbottom);
	    break;
	}
      }
      
      if (rleft < *left) *left = rleft;
      if (rtop < *top) *top = rtop;
      if (rright > *right) *right = rright;
      if (rbottom > *bottom) *bottom = rbottom;
      
      if (o_current->type == OBJ_PIN) {
	attr_ptr = attr_ptr->next;
	if (attr_ptr)
	  obj_ptr = attr_ptr->object;
	else
	  obj_ptr = NULL;
      }
      else {
	obj_ptr = obj_ptr->next;
      }
    }
  } 

/*! \brief Get the object bounds of the given object, excluding the object
 *  types or the attributes given as parameters.
 *  \par Function Description
 *  Get the object bounds without considering the attributes in 
 *  scm_exclude_attribs, neither the object types included in 
 *  scm_exclude_object_type
 *  \param [in] object_smob The object we want to know the bounds of.
 *  \param [in] exclude_attrib_list A list with the attribute names we don't
 *  want to include when calculing the bounds.
 *  \param [in] exclude_obj_type_list A list with the object types we don't
 *  want to include when calculing the bounds. 
 *  The object types are those used in (OBJECT *)->type converted into strings.
 *  \return a list of the bounds of the <B>object smob</B>. 
 *  The list has the format: ( (left right) (top bottom) )
 *  I got top and bottom values reversed from world_get_complex_bounds,
 *  so don\'t rely on the position in the list. 
 */
SCM g_get_object_bounds (SCM object_smob, SCM scm_exclude_attribs, SCM scm_exclude_object_type)
{

  TOPLEVEL *w_current=NULL;
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
  for (i=0; i <= SCM_INUM(scm_length(scm_exclude_attribs))-1; i++) {
    SCM_ASSERT (SCM_STRINGP(scm_list_ref(scm_exclude_attribs, SCM_MAKINUM(i))), 
		scm_exclude_attribs, 
		SCM_ARG2, "get-object-bounds"); 
    exclude_attrib_list = g_list_append(exclude_attrib_list, 
					SCM_STRING_CHARS(scm_list_ref(scm_exclude_attribs,
								      SCM_MAKINUM(i))));
  }

  /* Build the exclude object type list */
  for (i=0; i <= SCM_INUM(scm_length(scm_exclude_object_type))-1; i++) {
    SCM_ASSERT (SCM_STRINGP(scm_list_ref(scm_exclude_object_type, SCM_MAKINUM(i))), 
		scm_exclude_object_type, 
		SCM_ARG3, "get-object-bounds"); 
    exclude_obj_type_list = g_list_append(exclude_obj_type_list, 
					SCM_STRING_CHARS(scm_list_ref(scm_exclude_object_type,
								      SCM_MAKINUM(i))));
  }

  /* Get w_current and o_current. */
  g_get_data_from_object_smob (object_smob, &w_current, &object);
  
  SCM_ASSERT (w_current && object,
	      object_smob, SCM_ARG1, "get-object-bounds");

  if (g_list_find_custom(exclude_attrib_list, "all", (GCompareFunc) &strcmp))
    exclude_all_attribs = TRUE;

  custom_world_get_complex_bounds (w_current, object,
				   &left, &top, 
				   &right, &bottom, 
				   exclude_attrib_list,
				   exclude_obj_type_list);
  
  /* Free the exclude attrib_list. Don't free the nodes!! */
  g_list_free(exclude_attrib_list);

  /* Free the exclude attrib_list. Don't free the nodes!! */
  g_list_free(exclude_obj_type_list);
  
  horizontal = scm_cons (SCM_MAKINUM(left), SCM_MAKINUM(right));
  vertical = scm_cons (SCM_MAKINUM(top), SCM_MAKINUM(bottom));
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
  TOPLEVEL *w_current=NULL;
  OBJECT *object=NULL;
  OBJECT *prim_obj;
  SCM returned=SCM_EOL;

  /* Get w_current and o_current */
  SCM_ASSERT (g_get_data_from_object_smob (object_smob, &w_current, &object),
	      object_smob, SCM_ARG1, "get-object-pins");

  if (!object) {
    return (returned);
  }
  if (object->complex && object->complex->prim_objs) {
    prim_obj = object->complex->prim_objs;
    while (prim_obj != NULL) {
      if (prim_obj->type == OBJ_PIN) {
	returned = scm_cons (g_make_object_smob(w_current, prim_obj),returned);
      }
      prim_obj = prim_obj->next;
    }
  }
  
  return (returned);
}

/*! \brief Add a component to the page.
 *  \par Function Description
 *  Adds a component <B>scm_comp_name</B> to the schematic, at 
 *  position (<B>scm_x</B>, <B>scm_y</B>), with some properties set by 
 *  the parameters:
 *  \param [in] scm_x Coordinate X of the symbol.
 *  \param [in] scm_y Coordinate Y of the symbol.
 *  \param [in] angle Angle of rotation of the symbol. 
 *  \param [in] selectable True if the symbol is selectable, false otherwise.
 *  \param [in] mirror True if the symbol is mirrored, false otherwise.
 *  If scm_comp_name is a scheme empty list, SCM_BOOL_F, or an empty 
 *  string (""), then g_add_component returns SCM_BOOL_F without writing 
 *  to the log.
 *  \return TRUE if the component was added, FALSE otherwise.
 *
 */
SCM g_add_component(SCM page_smob, SCM scm_comp_name, SCM scm_x, SCM scm_y, 
		    SCM scm_angle, SCM scm_selectable, SCM scm_mirror)
{
  TOPLEVEL *w_current;
  PAGE *page;
  gboolean selectable, mirror;
  gchar *comp_name, *clib;
  int x, y, angle;
  GSList *clibs = NULL;
  OBJECT *new_object;

  /* Return if scm_comp_name is NULL (an empty list) or scheme's FALSE */
  if (SCM_NULLP(scm_comp_name) || 
      (SCM_BOOLP(scm_comp_name) && !(SCM_NFALSEP(scm_comp_name))) ) {
    return SCM_BOOL_F;
  }

  /* Get w_current and the page */
  SCM_ASSERT (g_get_data_from_page_smob (page_smob, &w_current, &page),
	      page_smob, SCM_ARG1, "add-component-at-xy");
  /* Check the arguments */
  SCM_ASSERT (SCM_STRINGP(scm_comp_name), scm_comp_name,
	      SCM_ARG2, "add-component-at-xy");
  SCM_ASSERT ( SCM_INUMP(scm_x), scm_x, 
               SCM_ARG3, "add-component-at-xy");
  SCM_ASSERT ( SCM_INUMP(scm_y), scm_y, 
               SCM_ARG4, "add-component-at-xy");
  SCM_ASSERT ( SCM_INUMP(scm_angle), scm_angle, 
               SCM_ARG5, "add-component-at-xy");
  SCM_ASSERT ( scm_boolean_p(scm_selectable), scm_selectable,
	       SCM_ARG6, "add-component-at-xy");
  SCM_ASSERT ( scm_boolean_p(scm_mirror), scm_mirror,
	       SCM_ARG7, "add-component-at-xy");

  /* Get the parameters */
  comp_name = SCM_STRING_CHARS(scm_comp_name);
  x = SCM_INUM(scm_y);
  y = SCM_INUM(scm_y);
  angle = SCM_INUM(scm_angle);  
  selectable = SCM_NFALSEP(scm_selectable);
  mirror = SCM_NFALSEP(scm_mirror);

  SCM_ASSERT (comp_name, scm_comp_name,
	      SCM_ARG2, "add-component-at-xy");
  
  if (strcmp(comp_name, "") == 0) {
    return SCM_BOOL_F;
  }

  clibs = (GSList *) s_clib_search_basename (comp_name);
  if (clibs == NULL) {
    /* Component not found */
    s_log_message ("add-component-at-xy: Component with name [%s] not found.\n",
		   comp_name);
    return SCM_BOOL_F;    
  } 

  g_assert(clibs != NULL);
  if (g_slist_next (clibs)) {
    s_log_message ("add-component-at-xy: More than one component found with name [%s]\n",
		   comp_name);
    /* PB: for now, use the first directory in clibs */
    /* PB: maybe open a dialog to select the right one? */
  }
  clib = (gchar*)clibs->data;
  
  new_object = page->object_tail = o_complex_add(w_current, 
						 page->object_tail, NULL, 
						 'C', 
						 WHITE, 
						 x, y, 
						 angle, mirror,
						 clib, comp_name, 
						 selectable, FALSE);
  
  /* 
   * For now, do not redraw the newly added complex, since this might cause
   * flicker if you are zoom/panning right after this function executes 
   */
#if 0 
  /* Now the new component should be added to the object's list and 
     drawn in the screen */
  o_redraw_single(w_current, new_object);
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

  TOPLEVEL *w_current;
  PAGE *page;
  OBJECT *object;
  SCM return_list=SCM_EOL;

  /* Get w_current and the page */
  SCM_ASSERT (g_get_data_from_page_smob (page_smob, &w_current, &page),
	      page_smob, SCM_ARG1, "add-component");

  if (page && page->object_head && page->object_head->next) {
    object = page->object_head->next;
    while (object) {
      return_list = scm_cons (g_make_object_smob(w_current, object),
			      return_list);
      object = object->next;
    }
  }

  return return_list;
} 
