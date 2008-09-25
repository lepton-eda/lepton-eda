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
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! Default setting for complex draw function. */
void (*complex_draw_func)() = NULL;

/*! \brief Return the bounds of the given object.
 *  \par Given an object, calculate the bounds coordinates.
 *  \param [in] toplevel The toplevel structure.
 *  \param [in] o_current The object to look the bounds for.
 *  \param [out] rleft   pointer to the left coordinate of the object.
 *  \param [out] rtop    pointer to the top coordinate of the object.
 *  \param [out] rright  pointer to the right coordinate of the object.
 *  \param [out] rbottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the object
 *  \retval 0 No bound was found
 *  \retval 1 Bound was found
 */
int world_get_single_object_bounds(TOPLEVEL *toplevel, OBJECT *o_current,
                                   int *rleft, int *rtop, int *rright, int *rbottom)
{
  if (o_current != NULL) {
    switch(o_current->type) {
      case(OBJ_TEXT):
        /* only do bounding boxes for visible or doing show_hidden_text*/
        /* you might lose some attrs though */
        if (! (o_current->visibility == VISIBLE ||
               toplevel->show_hidden_text )) {
          return 0;
        }
        /* This case falls through intentionally */
      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_BOX):
      case(OBJ_PICTURE):
      case(OBJ_CIRCLE):
      case(OBJ_PATH):
      case(OBJ_PIN):
      case(OBJ_ARC):
      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        if (!o_current->w_bounds_valid) {
          o_recalc_single_object (toplevel, o_current);
          if (!o_current->w_bounds_valid) {
            return 0;
          }
        }
        *rleft = o_current->w_left;
        *rtop = o_current->w_top;
        *rright = o_current->w_right;
        *rbottom = o_current->w_bottom;
        return 1;

      default:
        break;
    }
  }
  return 0;
}

/*! \brief Return the bounds of the given list of objects.
 *  \par Given a list of objects, calcule the bounds coordinates.
 *  \param [in] toplevel The toplevel structure.
 *  \param [in] complex   The list of objects to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the list of objects
 *  \retval 0 No bounds were found
 *  \retval 1 Bound was found
 */
int
world_get_object_list_bounds(TOPLEVEL *toplevel, OBJECT *complex,
		       int *left, int *top, int *right, int *bottom)
{
  OBJECT *o_current=NULL;
  int rleft, rtop, rright, rbottom;
  int found = 0;

  o_current = complex;

  // Find the first object with bounds, and set the bounds variables, then expand as necessary
  while ( o_current != NULL ) {
    if ( world_get_single_object_bounds( toplevel, o_current, &rleft, &rtop, &rright, &rbottom) ) {
      if ( found ) {
        *left = min( *left, rleft );
        *top = min( *top, rtop );
        *right = max( *right, rright );
        *bottom = max( *bottom, rbottom );
      } else {
        *left = rleft;
        *top = rtop;
        *right = rright;
        *bottom = rbottom;
        found = 1;
      }
    }
    o_current = o_current->next;
  }
  return found;
}

/*! \brief Return the bounds of the given GList of objects.
 *  \par Given a list of objects, calcule the bounds coordinates.
 *  \param [in] toplevel The toplevel structure.
 *  \param [in] complex   The list of objects to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 *  \return If any bounds were found for the list of objects
 *  \retval 0 No bounds were found
 *  \retval 1 Bound was found
 */
int world_get_object_glist_bounds(TOPLEVEL *toplevel, GList *head,
                                  int *left, int *top, int *right, int *bottom)
{
  GList *s_current=NULL;
  OBJECT *o_current=NULL;
  int rleft, rtop, rright, rbottom;
  int found = 0;

  s_current = head;

  // Find the first object with bounds, and set the bounds variables, then expand as necessary
  while ( s_current != NULL ) {
    o_current = (OBJECT *) s_current->data;
    g_assert (o_current != NULL);
    if ( world_get_single_object_bounds( toplevel, o_current, &rleft, &rtop, &rright, &rbottom) ) {
      if ( found ) {
        *left = min( *left, rleft );
        *top = min( *top, rtop );
        *right = max( *right, rright );
        *bottom = max( *bottom, rbottom );
      } else {
        *left = rleft;
        *top = rtop;
        *right = rright;
        *bottom = rbottom;
        found = 1;
      }
    }
    s_current = g_list_next (s_current);
  }
  return found;
}

/*! \brief Queries the bounds of a complex object.
 *  \par Function Description
 *  This function returns the bounding box of the complex object
 *  <B>object</B>.
 *
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  complex   The complex object.
 *  \param [out] left      The leftmost edge of the bounding box (in
 *                         world units).
 *  \param [out] top       The upper edge of the bounding box (in
 *                         world units).
 *  \param [out] right     The rightmost edge of the bounding box (in
 *                         world units).
 *  \param [out] bottom    The bottom edge of the bounding box (in
 *                         screen units).
 */
void world_get_complex_bounds(TOPLEVEL *toplevel, OBJECT *complex,
			      int *left, int *top, int *right, int *bottom)
{
  g_return_if_fail (complex != NULL &&
                    (complex->type == OBJ_COMPLEX ||
                     complex->type == OBJ_PLACEHOLDER) &&
                    complex->complex != NULL);

  world_get_object_list_bounds (toplevel, complex->complex->prim_objs->next,
                                left, top, right, bottom);

}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *add_head()
{
  OBJECT *new_node=NULL;

  new_node = s_basic_new_object(OBJ_HEAD, "complex_head");

  /* don't need to do this for head nodes */
  /* ret = (OBJECT *) s_basic_link_object(new_node, NULL);*/
  return(new_node);
}

/*! \brief
 *  \par Function Description
 *
 */
static int o_complex_is_eligible_attribute (TOPLEVEL *toplevel, OBJECT *object)
{
  char *name = NULL;
  char *padded_name = NULL;
  int promotableAttribute = FALSE;

  g_return_val_if_fail(object != NULL, FALSE);

  if (object->type != OBJ_TEXT || object->attribute || object->attached_to)
    return FALSE; /* not a text item or is already attached */

  /* Make sure text item is an attribute */
  if (!o_attrib_get_name_value (object->text->string, NULL, NULL))
    return FALSE;  /* not an attribute */

  /* always promote symversion= attribute, even if it is invisible */
  if (strncmp(object->text->string, "symversion=", 11) == 0)
    return TRUE;

  /* check list against attributes which can be promoted */
  if (toplevel->always_promote_attributes &&
      (strlen(toplevel->always_promote_attributes) != 0)) {

    if (o_attrib_get_name_value(object->text->string, &name, NULL)) {
      padded_name = g_strdup_printf(" %s ", name);
      if (strstr(toplevel->always_promote_attributes, padded_name)) {
        /* Name of the attribute was in the always promote attributes list */
        promotableAttribute = TRUE;
      }

      g_free(padded_name);
      g_free(name);
      if (promotableAttribute)
        return TRUE;
    }
  }

  /* object is invisible and we do not want to promote invisible text */
  if (object->visibility == INVISIBLE && toplevel->promote_invisible == FALSE)
    return FALSE; /* attribute not eligible for promotion */

  /* yup, attribute can be promoted */
  return TRUE;
}

/*! \brief
 *  \par Function Description
 *
 */
int o_complex_is_embedded(OBJECT *o_current)
{
  g_return_val_if_fail(o_current != NULL, 0);

  if(o_current->complex == NULL)
  return 0;

  if (o_current->complex_embedded) {
    return 1;
  } else {
    return 0;
  }

}


/*! \brief Get attributes eligible for promotion from inside a complex
 *
 *  \par Function Description
 *  Returns a GList of OBJECTs which are eligible for promotion from
 *  within the passed complex OBJECT.
 *
 *  If detach is TRUE, the function removes these attribute objects from
 *  the prim_objs of the complex. It detached, the returned OBJECTs are
 *  isolated from each other, having their next and prev pointers set to NULL.
 *
 *  If detach is FALSE, the OBJECTs are left in place. Their next and prev
 *  pointers form part of the complex's prim_objs linked list.
 *
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  object   The complex object being modified.
 *  \param [in]  detach   Should the attributes be detached?
 *  \returns              A linked list of OBJECTs to promote.
 */
GList *o_complex_get_promotable (TOPLEVEL *toplevel, OBJECT *object, int detach)
{
  GList *promoted = NULL;
  OBJECT *tmp, *next;

  if (!toplevel->attribute_promotion) /* controlled through rc file */
    return NULL;

  for (tmp = object->complex->prim_objs->next; tmp != NULL; tmp = next) {
    next = tmp->next;

    /* valid floating attrib? */
    if (!o_complex_is_eligible_attribute(toplevel, tmp))
      continue;

    if (detach) {
      /* Remove and isolate tmp from the complex list */
      if (tmp->next)
        tmp->next->prev = tmp->prev;
      if (tmp->prev)
        tmp->prev->next = tmp->next;
      tmp->next = tmp->prev = NULL;
      tmp->complex_parent = NULL;
    }

    promoted = g_list_prepend (promoted, tmp);
  }

  promoted = g_list_reverse (promoted);
  return promoted;
}


/*! \brief Promote attributes from the passed OBJECT
 *
 *  \par Function Description
 *  Promotes attributes from the passed OBJECT, linking them into the
 *  OBJECT linked list immediately prior to the passed OBJECT.
 *
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  object   The complex object who's attributes are being promtoed.
 */
void o_complex_promote_attribs (TOPLEVEL *toplevel, OBJECT *object)
{
  GList *promoted;
  OBJECT *first_promoted, *last_promoted;

  promoted = o_complex_get_promotable (toplevel, object, TRUE);

  if (promoted == NULL)
    return;

  /* Link the promoted OBJECTs together */
  o_glist_relink_objects (promoted);

  first_promoted = promoted->data;
  last_promoted = g_list_last (promoted)->data;

  /* Insert promoted attributes before the complex in the object list */
  first_promoted->prev = object->prev;
  object->prev->next = first_promoted;
  last_promoted->next = object;
  object->prev = last_promoted;

  /* Attach promoted attributes to the original complex object */
  o_attrib_attach_list (toplevel, promoted, object);

  g_list_free (promoted);
}


/*! \brief Delete or hide promotable from the passed OBJECT
 *
 *  \par Function Description
 *  Deletes or hides promotable attributes from the passed OBJECT.
 *  This is used when loading symbols during the load of a schematic from
 *  disk. The schematic will already contain local copies of symbol's
 *  promotable objects, so we delete or hide the symbol's copies.
 *
 *  Deletion / hiding is dependant on the setting of
 *  toplevel->keep_invisible. If true, attributes eligible for
 *  promotion are kept in memory but flagged as invisible.
 *
 *  \param [in]  toplevel The toplevel environment.
 *  \param [in]  object   The complex object being altered.
 */
void o_complex_remove_promotable_attribs (TOPLEVEL *toplevel, OBJECT *object)
{
  GList *promotable, *iter;

  promotable = o_complex_get_promotable (toplevel, object, FALSE);

  if (promotable == NULL)
    return;

  for (iter = promotable; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *a_object = iter->data;
    if (toplevel->keep_invisible == TRUE) {
      a_object->visibility = INVISIBLE; /* Hide promotable attributes */
    } else {
      s_delete (toplevel, a_object);    /* Delete promotable attributes */
    }
  }

  g_list_free (promotable);
}


/* Done */
/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_add(TOPLEVEL *toplevel, OBJECT *object_list,
		      GList **object_glist, char type,
		      int color, int x, int y, int angle,
		      int mirror, const CLibSymbol *clib,
		      const gchar *basename,
		      int selectable)
{
  OBJECT *new_node=NULL;
  OBJECT *prim_objs=NULL;
  OBJECT *tmp;
  int save_adding_sel = 0;
  int loaded_normally = FALSE;
  gboolean use_object_list;

  gchar *buffer;

  if (object_list) {
    use_object_list = TRUE;
  } else {
    use_object_list = FALSE;
  }

  new_node = s_basic_new_object(type, "complex");

  if (clib != NULL) {
    new_node->complex_basename = g_strdup (s_clib_symbol_get_name (clib));
  } else {
    new_node->complex_basename = g_strdup (basename);
  }


  new_node->complex_embedded = FALSE;

  new_node->color = color;
	
  new_node->complex = (COMPLEX *) g_malloc(sizeof(COMPLEX));
	
  new_node->complex->angle = angle;
  new_node->complex->mirror = mirror;

  new_node->complex->x = x;
  new_node->complex->y = y;

  new_node->draw_func = complex_draw_func;  

  if (selectable) { 
    new_node->sel_func = select_func;
  } else {
    new_node->sel_func = NULL;
  }

  /* this was at the beginning and p_complex was = to complex */
  prim_objs = (OBJECT *) add_head();

  /* get the symbol data */
  if (clib != NULL) {
    buffer = s_clib_symbol_get_data (clib);
  }

  save_adding_sel = toplevel->ADDING_SEL;
  toplevel->ADDING_SEL = 1;	/* name is hack, don't want to */

  if (clib == NULL || buffer == NULL) {

    OBJECT *save_prim_objs;
    char *not_found_text = NULL;
    int left, right, top, bottom;
    int x_offset, y_offset;

    /* filename was NOT found */
    loaded_normally = FALSE;

    /* save the prim_objs pointer, since the following code modifies it */
    save_prim_objs = prim_objs;

    /* Put placeholder into object list.  Changed by SDB on
     * 1.19.2005 to fix problem that symbols were silently
     * deleted by gattrib when RC files were messed up.  */
    new_node->type = OBJ_PLACEHOLDER;

    /* Mark the origin of the missing component */
    prim_objs = o_line_add(toplevel, prim_objs, OBJ_LINE,
                           DETACHED_ATTRIBUTE_COLOR,
                           x - 50, y, x + 50, y);
    prim_objs = o_line_add(toplevel, prim_objs, OBJ_LINE,
                           DETACHED_ATTRIBUTE_COLOR,
                           x, y + 50, x, y - 50); 

    /* Add some useful text */
    not_found_text = 
      g_strdup_printf (_("Component not found:\n %s"),
		       new_node->complex_basename);
    prim_objs = o_text_add(toplevel, prim_objs,
                           OBJ_TEXT, DETACHED_ATTRIBUTE_COLOR, 
                           x + NOT_FOUND_TEXT_X, 
                           y + NOT_FOUND_TEXT_Y, LOWER_LEFT, 0, 
                           not_found_text, 8,
                           VISIBLE, SHOW_NAME_VALUE);
    g_free(not_found_text);

    /* figure out where to put the hazard triangle */
    world_get_text_bounds(toplevel, prim_objs,
                          &left, &top, &right, &bottom);
    x_offset = (right - left) / 4;  
    y_offset = bottom - top + 100;  /* 100 is just an additional offset */

    /* add hazard triangle */
    prim_objs = o_line_add(toplevel, prim_objs, OBJ_LINE,
                           DETACHED_ATTRIBUTE_COLOR,
                           x + NOT_FOUND_TEXT_X + x_offset, 
                           y + NOT_FOUND_TEXT_Y + y_offset, 
                           x + NOT_FOUND_TEXT_X + x_offset + 600, 
                           y + NOT_FOUND_TEXT_Y + y_offset); 
    o_set_line_options(toplevel, prim_objs, END_ROUND, TYPE_SOLID,
                       50, -1, -1);
    prim_objs = o_line_add(toplevel, prim_objs, OBJ_LINE,
                           DETACHED_ATTRIBUTE_COLOR,
                           x + NOT_FOUND_TEXT_X + x_offset, 
                           y + NOT_FOUND_TEXT_Y + y_offset, 
                           x + NOT_FOUND_TEXT_X + x_offset + 300, 
                           y + NOT_FOUND_TEXT_Y + y_offset + 500); 
    o_set_line_options(toplevel, prim_objs, END_ROUND, TYPE_SOLID,
                       50, -1, -1);
    prim_objs = o_line_add(toplevel, prim_objs, OBJ_LINE,
                           DETACHED_ATTRIBUTE_COLOR,
                           x + NOT_FOUND_TEXT_X + x_offset + 300, 
                           y + NOT_FOUND_TEXT_Y + y_offset + 500, 
                           x + NOT_FOUND_TEXT_X + x_offset + 600, 
                           y + NOT_FOUND_TEXT_Y + y_offset); 
    o_set_line_options(toplevel, prim_objs, END_ROUND, TYPE_SOLID,
                       50, -1, -1);
    prim_objs = o_text_add(toplevel, prim_objs,
                           OBJ_TEXT, DETACHED_ATTRIBUTE_COLOR, 
                           x + NOT_FOUND_TEXT_X + x_offset + 270, 
                           y + NOT_FOUND_TEXT_Y + y_offset + 90, 
                           LOWER_LEFT, 0, "!", 18,
                           VISIBLE, SHOW_NAME_VALUE);
    prim_objs = save_prim_objs;

  } else {

    /* filename was found */
    loaded_normally = TRUE;

    /* add connections till translated */
    o_read_buffer(toplevel, prim_objs, buffer, -1, new_node->complex_basename);

    g_free (buffer);
    
  }
  toplevel->ADDING_SEL = save_adding_sel;

  /* do not mirror/rotate/translate/connect the primitive objects if the
   * component was not loaded via o_read 
   */
  if (loaded_normally == TRUE) {
    if (mirror) {
      o_list_mirror_world(toplevel, 0, 0, prim_objs);
    }

    o_list_rotate_world(toplevel, 0, 0, angle, prim_objs);
    o_list_translate_world(toplevel, x, y, prim_objs);

    if (!toplevel->ADDING_SEL) {
     s_conn_update_complex(toplevel, prim_objs);
    }
  }

  new_node->complex->prim_objs = prim_objs;

  /* set the parent field now */
  for (tmp = prim_objs; tmp != NULL; tmp = tmp->next) {
    tmp->complex_parent = new_node;
  }

  if (use_object_list) {
    object_list = (OBJECT *) s_basic_link_object(new_node, object_list);
  } else {
    if (object_glist) {
      *object_glist = g_list_append (*object_glist, new_node);
      o_glist_relink_objects (*object_glist);
    }
    object_list = new_node;
  }

  if (use_object_list)
    o_complex_recalc(toplevel, object_list);
  else
    o_complex_recalc(toplevel, new_node);

  return(object_list);
}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_add_embedded(TOPLEVEL *toplevel, OBJECT *object_list,
			       char type, int color, int x, int y, int angle, int mirror,
			       const gchar *basename, int selectable)
{
  OBJECT *prim_objs=NULL;
  OBJECT *new_node=NULL;
  OBJECT *tmp;

  new_node = s_basic_new_object(type, "complex");

  new_node->complex = (COMPLEX *) g_malloc(sizeof(COMPLEX));
  new_node->complex->x = x;
  new_node->complex->y = y;

  new_node->complex->angle = angle;
  new_node->complex->mirror = mirror;
	
  new_node->complex_basename = g_strdup(basename);

  new_node->complex_embedded = TRUE;

  new_node->color = color;

  new_node->draw_func = complex_draw_func;  

  /* (for a title block) an object that isn't selectable */
  if (selectable) { 
    new_node->sel_func = select_func;
  } else {
    new_node->sel_func = NULL;
  }

  object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

  /* this was at the beginning and p_complex was = to complex */
  prim_objs = (OBJECT *) add_head();
  object_list->complex->prim_objs = prim_objs;

  /* set the parent field now */
  for (tmp = prim_objs; tmp != NULL; tmp = tmp->next) {
    tmp->complex_parent = new_node;
  }

  /* don't have to translate/rotate/mirror here at all since the */
  /* object is in place */
  return(object_list);
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, right, top, bottom;

  /* realc routine Add this somewhere */
  /* libhack */
  /* o_recalc(toplevel, o_current->complex);*/

  if ((!o_current) || (o_current->type != OBJ_COMPLEX && o_current->type != OBJ_PLACEHOLDER))
    return;

  world_get_complex_bounds(toplevel, o_current, &left, &top, &right, &bottom);
  o_current->w_left = left;
  o_current->w_top = top;
  o_current->w_right = right;
  o_current->w_bottom = bottom;
  o_current->w_bounds_valid = TRUE;
}

/*! \brief
 *  \par Function Description
 *
 *  \todo Don't use fixed-length string for symbol basename
 */
OBJECT *o_complex_read(TOPLEVEL *toplevel, OBJECT *object_list,
		       char buf[], unsigned int release_ver,
		       unsigned int fileformat_ver)
{
  char type; 
  int x1, y1;
  int angle;

  char basename[256]; /* FIXME This is a hack */
	
  int selectable;
  int mirror;

  sscanf(buf, "%c %d %d %d %d %d %s\n",
         &type, &x1, &y1, &selectable, &angle, &mirror, basename);

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
      break;

    default:
      s_log_message(_("Found a component with an invalid rotation [ %c %d %d %d %d %d %s ]\n"), type, x1, y1, selectable, angle, mirror, basename);
      break;
  }

  switch(mirror) {

    case(0):
    case(1):

      break;
		
    default:
      s_log_message(_("Found a component with an invalid mirror flag [ %c %d %d %d %d %d %s ]\n"), type, x1, y1, selectable, angle, mirror, basename);
      break;
  }
  if (strncmp(basename, "EMBEDDED", 8) == 0) {
    
  object_list = o_complex_add_embedded(toplevel,
                                       object_list, type, 
                                       WHITE, x1, y1, angle, mirror,
                                       basename + 8, 
                                       selectable);
  } else {
    
    const CLibSymbol *clib = s_clib_get_symbol_by_name (basename);

    object_list = o_complex_add(toplevel, object_list, NULL, type,
                                WHITE, 
                                x1, y1, 
                                angle, mirror, clib,
                                basename, selectable);
    /* Delete or hide attributes eligible for promotion inside the complex */
     o_complex_remove_promotable_attribs (toplevel, object_list);
  }

  return object_list;
}

/*! \brief
 *  \par Function Description
 *
 */
char *o_complex_save(OBJECT *object)
{
  int selectable;
  char *buf = NULL;
  char *basename;

  g_return_val_if_fail (object != NULL, NULL);

  if (object->sel_func != NULL) 
  selectable = 1;
  else 
  selectable = 0;

  if ((object->type == OBJ_COMPLEX) || (object->type == OBJ_PLACEHOLDER)) {
    basename = g_strdup_printf ("%s%s",
				object->complex_embedded ? "EMBEDDED" : "",
				object->complex_basename);
    /* We force the object type to be output as OBJ_COMPLEX for both
     * these object types. */
    buf = g_strdup_printf("%c %d %d %d %d %d %s", OBJ_COMPLEX,
                          object->complex->x, object->complex->y,
                          selectable, object->complex->angle,
                          object->complex->mirror, basename);
    g_free (basename);
  }

  return(buf);
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_translate_world(TOPLEVEL *toplevel, int dx, int dy,
                               OBJECT *object)
{
  g_return_if_fail (object != NULL &&
                    (object->type == OBJ_COMPLEX ||
                     object->type == OBJ_PLACEHOLDER));

  object->complex->x = object->complex->x + dx;
  object->complex->y = object->complex->y + dy;

  o_list_translate_world(toplevel, dx, dy, object->complex->prim_objs);

  o_complex_recalc (toplevel, object);
}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_copy(TOPLEVEL *toplevel, OBJECT *list_tail,
		       OBJECT *o_current)
{
  OBJECT *new_obj=NULL;
  int color;
  int selectable;
  const CLibSymbol *clib = NULL;

  g_return_val_if_fail(o_current != NULL, NULL);

  if (o_current->saved_color == -1) {
    color = o_current->color;
  } else {
    color = o_current->saved_color;
  }
	
  if (o_current->sel_func) {
    selectable = TRUE;	
  } else {
    selectable = FALSE;	
  }

  clib = s_clib_get_symbol_by_name (o_current->complex_basename);

  new_obj = o_complex_add (toplevel, list_tail, NULL, o_current->type, color,
                           o_current->complex->x, o_current->complex->y,
                           o_current->complex->angle,
                           o_current->complex->mirror,
                           clib, o_current->complex_basename,
                           selectable);
  /* Delete or hide attributes eligible for promotion inside the complex */
   o_complex_remove_promotable_attribs (toplevel, new_obj);

  o_attrib_slot_copy(toplevel, o_current, new_obj);

  /* deal with stuff that has changed */

  /* here you need to create a list of attributes which need to be 
   * connected to the new list, probably make an attribute list and
   * fill it with sid's of the attributes */

  return(new_obj);
}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_copy_embedded(TOPLEVEL *toplevel, OBJECT *list_tail,
				OBJECT *o_current)
{
  OBJECT *new_obj=NULL;
  OBJECT *temp_list;
  OBJECT *tmp;
  int color;
  int selectable;

  g_return_val_if_fail(o_current != NULL, NULL);

  if (o_current->saved_color == -1) {
    color = o_current->color;
  } else {
    color = o_current->saved_color;
  }

  if (o_current->sel_func) {
    selectable = TRUE;	
  } else {
    selectable = FALSE;	
  }

  new_obj = o_complex_add_embedded(toplevel, list_tail, o_current->type,
                                   color,
                                   o_current->complex->x, o_current->complex->y, 
                                   o_current->complex->angle, 
                                   o_current->complex->mirror,
                                   o_current->complex_basename, 
                                   selectable); 
  /* deal with stuff that has changed */
	
  temp_list = o_list_copy_all(toplevel,
                              o_current->complex->prim_objs->next,
                              new_obj->complex->prim_objs, 
                              NORMAL_FLAG);
	
  new_obj->complex->prim_objs = return_head(temp_list);

  /* set the parent field now */
  for (tmp = new_obj->complex->prim_objs; tmp != NULL; tmp = tmp->next) {
    tmp->complex_parent = new_obj;
  }

  o_complex_recalc(toplevel, new_obj);

  /* here you need to create a list of attributes which need to be 
   * connected to the new list, probably make an attribute list and
   * fill it with sid's of the attributes */

  return(new_obj);
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_set_color(OBJECT *prim_objs, int color)
{
  OBJECT *o_current=NULL;

  o_current = prim_objs;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_BOX):
      case(OBJ_PICTURE):
      case(OBJ_CIRCLE):
      case(OBJ_PATH):
      case(OBJ_PIN):
      case(OBJ_ARC):
        o_current->color = color;
        break;

      case(OBJ_TEXT):
        o_current->color = color;
        o_complex_set_color(o_current->text->prim_objs, color);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_current->color = color;
        o_complex_set_color(o_current->complex->prim_objs, color);
        break;

    }
    o_current=o_current->next;
  }
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_set_color_single(OBJECT *o_current, int color)
{
  g_return_if_fail(o_current != NULL);

  switch(o_current->type) {
    case(OBJ_LINE):
    case(OBJ_NET):
    case(OBJ_BUS):
    case(OBJ_BOX):
    case(OBJ_PICTURE):
    case(OBJ_CIRCLE):
    case(OBJ_PATH):
    case(OBJ_PIN):
    case(OBJ_ARC):
    o_current->color = color;
    break;

    case(OBJ_TEXT):
    o_current->color = color;
    o_complex_set_color(o_current->text->prim_objs, color);
    break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
    o_current->color = color;
    o_complex_set_color(o_current->complex->prim_objs, 
                        color);
    break;

  }
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_set_color_save(OBJECT *complex, int color)
{
  OBJECT *o_current=NULL;

  o_current = complex;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_BOX):
      case(OBJ_PICTURE):
      case(OBJ_CIRCLE):
      case(OBJ_PATH):
      case(OBJ_PIN):
      case(OBJ_ARC):
        o_current->saved_color = o_current->color;
        o_current->color = color;
        break;

      case(OBJ_TEXT):
        o_current->saved_color = o_current->color;
        o_current->color = color;
        o_complex_set_color_save(
                                 o_current->text->prim_objs, 
                                 color);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_current->saved_color = o_current->color;
        o_current->color = color;
        o_complex_set_color_save(o_current->complex->
                                 prim_objs,
                                 color);
        break;

    }
    o_current=o_current->next;
  }
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_unset_color(OBJECT *complex)
{
  OBJECT *o_current=NULL;

  o_current = complex;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_BOX):
      case(OBJ_PICTURE):
      case(OBJ_CIRCLE):
      case(OBJ_PATH):
      case(OBJ_PIN):
      case(OBJ_ARC):
        o_current->color = o_current->saved_color;
        o_current->saved_color = -1;
        break;

      case(OBJ_TEXT):
        o_current->color = o_current->saved_color;
        o_current->saved_color = -1;
        o_complex_unset_color(o_current->text->prim_objs);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_current->color = o_current->saved_color;
        o_current->saved_color = -1;
        o_complex_unset_color(o_current->complex->
                              prim_objs);

        break;

    }
    o_current=o_current->next;
  }
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_unset_color_single(OBJECT *o_current)
{
  g_return_if_fail(o_current != NULL);

  switch(o_current->type) {
    case(OBJ_LINE):
    case(OBJ_NET):
    case(OBJ_BUS):
    case(OBJ_BOX):
    case(OBJ_PICTURE):
    case(OBJ_CIRCLE):
    case(OBJ_PATH):
    case(OBJ_PIN):
    case(OBJ_ARC):
    o_current->color = o_current->saved_color;
    o_current->saved_color = -1;
    break;

    case(OBJ_TEXT):
    o_current->color = o_current->saved_color;
    o_current->saved_color = -1;
    o_complex_unset_color(o_current->text->prim_objs);
    break;

    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
    o_current->color = o_current->saved_color;
    o_current->saved_color = -1;
    o_complex_unset_color(o_current->complex->prim_objs);

    break;
  }
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_set_saved_color_only(OBJECT *complex, int color)
{
  OBJECT *o_current=NULL;

  o_current = complex;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_BUS):
      case(OBJ_BOX):
      case(OBJ_PICTURE):
      case(OBJ_CIRCLE):
      case(OBJ_PATH):
      case(OBJ_PIN):
      case(OBJ_ARC):
        o_current->saved_color = color;
        break;

      case(OBJ_TEXT):
        o_current->saved_color = color;
        o_complex_set_saved_color_only(o_current->text->prim_objs, color);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_current->saved_color = color;
        o_complex_set_saved_color_only(o_current->complex->prim_objs, color);
        break;

    }
    o_current=o_current->next;
  }
}

/*! \brief
 *  \par Function Description
 *
 */
/* returns the counter'th pin in o_list */
/* NULL if there is no more pins */
OBJECT *o_complex_return_nth_pin(OBJECT *o_list, int counter)
{
  OBJECT *o_current;
  int internal_counter=0;
	
  o_current = o_list;
	
  while (o_current != NULL) {
    if (o_current->type == OBJ_PIN) {
			
      if (counter == internal_counter) {
        return(o_current);
      } else {
        internal_counter++;	
      }
    }	
    o_current = o_current->next;
  }
	
  return(NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_rotate_world(TOPLEVEL *toplevel,
                            int centerx, int centery,
                            int angle, OBJECT *object)
{
  int x, y;
  int newx, newy;

  g_return_if_fail (object!=NULL);
  g_return_if_fail ((object->type == OBJ_COMPLEX) ||
                    (object->type == OBJ_PLACEHOLDER));

  x = object->complex->x + (-centerx);
  y = object->complex->y + (-centery);

  rotate_point_90(x, y, angle, &newx, &newy);

  x = newx + (centerx);
  y = newy + (centery);

  o_complex_translate_world(toplevel,
                            -object->complex->x,
                            -object->complex->y, object);
  o_list_rotate_world(toplevel, 0, 0, angle, object->complex->prim_objs);

  object->complex->x = 0;
  object->complex->y = 0;

  o_complex_translate_world(toplevel, x, y, object);

  object->complex->angle = ( object->complex->angle + angle ) % 360;
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_complex_mirror_world(TOPLEVEL *toplevel,
                            int world_centerx, int world_centery,
                            OBJECT *object)
{
  int x, y;

  g_return_if_fail( object != NULL );
  g_return_if_fail( (object->type == OBJ_COMPLEX ||
                     object->type == OBJ_PLACEHOLDER) );
  g_return_if_fail( object->complex != NULL );

  x = 2 * world_centerx - object->complex->x;
  y = object->complex->y;

  o_complex_translate_world(toplevel,
                            -object->complex->x,
                            -object->complex->y, object);

  o_list_mirror_world( toplevel, 0, 0, object->complex->prim_objs );

  switch(object->complex->angle) {
    case(90):
      object->complex->angle = 270;
      break;

    case(270):
      object->complex->angle = 90;
      break;

  }

  object->complex->mirror = !object->complex->mirror;

  o_complex_translate_world(toplevel, x, y, object);
}


/*! \brief
 *  \par Function Description
 *
 */
/* pass in top level object */
OBJECT *o_complex_return_pin_object(OBJECT *object, char *pin) 
{
  OBJECT *o_current=NULL;
  OBJECT *found;

  g_return_val_if_fail(object != NULL, NULL);
  g_return_val_if_fail(((object->type == OBJ_COMPLEX) ||
			(object->type == OBJ_PLACEHOLDER)) , NULL);
  g_return_val_if_fail(object->complex != NULL, NULL);


  /* go inside complex objects */
  o_current = object->complex->prim_objs;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_PIN):

        /* Search for the pin making sure that */
        /* any found attribute starts with "pinnumber" */
        found = o_attrib_search_attrib_value(o_current->attribs, pin,
                                             "pinnumber", 0);
        if (found) {
#if DEBUG
          printf("%s: %s\n", found->name,
                 found->text->string);
#endif
          return(o_current);
        }
        break;
    }
    o_current=o_current->next;
  }
  return(NULL);
}

/*! \brief Counts pins on complex object
 *  \par Given a pointer to a complex object (sch level
 *  component object), this fcn iterates through the prim_objs
 *  list counting the number of pins it finds.
 *
 *  \param [in] pointer to complex object
 *  \return integer number of pins counted.
 */
/* pass in top level object */
int o_complex_count_pins(OBJECT *object) 
{
  OBJECT *o_current=NULL;
  int pin_counter=0;

  g_return_val_if_fail(object != NULL, 0);
  g_return_val_if_fail(((object->type == OBJ_COMPLEX) ||
			(object->type == OBJ_PLACEHOLDER)) , 0);
  g_return_val_if_fail(object->complex != NULL, 0);


  /* go inside complex object.  This means that we grab
   * a pointer to the head of the prim_objs list.
   * These are the graphical items stored in the lower-level
   * file (usually, objects from the .sym file).
   * Then iterate over this list looking for pins and
   * counting them. */
  o_current = object->complex->prim_objs;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_PIN):

	pin_counter++;
        break;
    }
    o_current=o_current->next;
  }
  return(pin_counter);
}

/*! \brief
 *  \par Function Description
 *
 */
/* pass in top level object */
void
o_complex_check_symversion(TOPLEVEL* toplevel, OBJECT* object)
{
  char *inside = NULL;
  char *outside = NULL;
  char *refdes = NULL;
  double inside_value = -1.0;
  double outside_value = -1.0;
  char *err_check = NULL;
  int inside_present = FALSE;
  int outside_present = FALSE;
  double inside_major, inside_minor;
  double outside_major, outside_minor;
  
  g_return_if_fail (object != NULL);
  g_return_if_fail ((object->type == OBJ_COMPLEX || 
		     object->type == OBJ_PLACEHOLDER));
  g_return_if_fail (object->complex != NULL);

  /* first look on the inside for the symversion= attribute */
  inside = o_attrib_search_name(object->complex->prim_objs, "symversion", 0);

  /* now look for the symversion= attached to object */
  outside = o_attrib_search_attrib_name(object->attribs, "symversion", 0);

  /* get the uref for future use */
  refdes = o_attrib_search_attrib_name(object->attribs, "refdes", 0);
  if (!refdes)
  {
    refdes = g_strdup ("unknown");
  }
  
  if (inside)
  {
    inside_value = strtod(inside, &err_check);
    if (inside_value == 0 && inside == err_check)
    {
      if (inside)
      { 
        s_log_message(_("WARNING: Symbol version parse error on refdes %s:\n"
                        "\tCould not parse symbol file symversion=%s\n"),
                      refdes, inside);
      } else {
        s_log_message(_("WARNING: Symbol version parse error on refdes %s:\n"
                        "\tCould not parse symbol file symversion=\n"),
                      refdes);
      }
      goto done;
    }
    inside_present = TRUE;
  } else {
    inside_present = FALSE;  /* attribute not inside */
  }

  if (outside)
  {
    outside_value = strtod(outside, &err_check);
    if (outside_value == 0 && outside == err_check)
    {
      s_log_message(_("WARNING: Symbol version parse error on refdes %s:\n"
                      "\tCould not parse attached symversion=%s\n"),
                    refdes, outside);
      goto done;
    }
    outside_present = TRUE; 
  } else {
    outside_present = FALSE;  /* attribute not outside */
  }

#if DEBUG
  printf("%s:\n\tinside: %.1f outside: %.1f\n\n", object->name,
         inside_value, outside_value);
#endif
  
  /* symversion= is not present anywhere */
  if (!inside_present && !outside_present)
  {
    /* symbol is legacy and versioned okay */
    goto done;
  }

  /* No symversion inside, but a version is outside, this is a weird case */
  if (!inside_present && outside_present)
  {
    s_log_message(_("WARNING: Symbol version oddity on refdes %s:\n"
                    "\tsymversion=%s attached to instantiated symbol, "
                    "but no symversion= inside symbol file\n"),
                  refdes, outside);
    goto done;
  }

  /* inside & not outside is a valid case, means symbol in library is newer */
  /* also if inside_value is greater than outside_value, then symbol in */
  /* library is newer */
  if ((inside_present && !outside_present) ||
      ((inside_present && outside_present) && (inside_value > outside_value)))
  {
    
    s_log_message(_("WARNING: Symbol version mismatch on refdes %s (%s):\n"
                    "\tSymbol in library is newer than "
                    "instantiated symbol\n"),
                  refdes, object->complex_basename);

    /* break up the version values into major.minor numbers */
    inside_major = floor(inside_value);
    inside_minor = inside_value - inside_major;

    if (outside_present)
    {
      outside_major = floor(outside_value);
      outside_minor = outside_value - outside_major;
    } else {
      /* symversion was not attached to the symbol, set all to zero */
      outside_major = 0.0;
      outside_minor = 0.0;
      outside_value = 0.0;
    }

#if DEBUG
    printf("i: %f %f %f\n", inside_value, inside_major, inside_minor);
    printf("o: %f %f %f\n", outside_value, outside_major, outside_minor);
#endif
    
    if (inside_major > outside_major)
    {
      char* refdes_copy;
      s_log_message(_("\tMAJOR VERSION CHANGE (file %.3f, "
                      "instantiated %.3f, %s)!\n"),
                    inside_value, outside_value, refdes);

      /* add the refdes to the major_changed_refdes GList */
      /* make sure refdes_copy is freed somewhere */
      refdes_copy = g_strconcat (refdes, " (",
                                 object->complex_basename,
                                 ")", NULL);
      toplevel->major_changed_refdes =
        g_list_append(toplevel->major_changed_refdes, refdes_copy);

      /* don't bother checking minor changes if there are major ones*/
      goto done; 
    }

    if (inside_minor > outside_minor)
    {
      s_log_message(_("\tMinor version change (file %.3f, "
                      "instantiated %.3f)\n"),
                    inside_value, outside_value);
    }

    goto done;
  }

  /* outside value is greater than inside value, this is weird case */
  if ((inside_present && outside_present) && (outside_value > inside_value))
  {
    s_log_message(_("WARNING: Symbol version oddity on refdes %s:\n"
                    "\tInstantiated symbol is newer than "
                    "symbol in library\n"),
                  refdes);
    goto done;
  }

  /* if inside_value and outside_value match, then symbol versions are okay */

done:
  g_free(inside);
  g_free(outside);
  g_free(refdes);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on an object within the complex object.
 *
 *  \param [in] object The object, where object->complex != NULL.
 *  \param [in] x The x coordinate of the given point.
 *  \param [in] y The y coordinate of the given point.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  With an invalid parameter, this function returns
 *  G_MAXDOUBLE.
 */
gdouble o_complex_shortest_distance(COMPLEX *complex, gint x, gint y)
{
  gdouble distance;
  gdouble shortest_distance = G_MAXDOUBLE;
  OBJECT *temp;

  if (complex == NULL) {
    g_critical("o_complex_shortest_distance(): complex == NULL\n");
    return G_MAXDOUBLE;
  }

  temp = complex->prim_objs;

  if (temp != NULL) {
    temp = temp->next;
  }

  while (temp != NULL) {
    distance = o_shortest_distance(temp, x, y);

    shortest_distance = min(shortest_distance, distance);

    temp = temp->next;
  }

  return shortest_distance;
}

