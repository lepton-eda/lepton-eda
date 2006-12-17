/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"
#include "funcs.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Return the bounds of the given object.
 *  \par Given an object, calcule the bounds coordinates.
 *  \param [in] w_current The toplevel structure.
 *  \param [in] o_current The object to look the bounds for.
 *  \param [out] rleft   pointer to the left coordinate of the object.
 *  \param [out] rtop    pointer to the top coordinate of the object.
 *  \param [out] rright  pointer to the right coordinate of the object.
 *  \param [out] rbottom pointer to the bottom coordinate of the object.
 *
 */
void get_single_object_bounds(TOPLEVEL *w_current, OBJECT *o_current, 
			      int *rleft, int *rtop, int *rright, int *rbottom)
{
  if (o_current != NULL) {
    switch(o_current->type) {
      case(OBJ_LINE):
        get_line_bounds(w_current, o_current->line, rleft, rtop, rright, rbottom);
        break;

      case(OBJ_NET):
        /* same as a line (diff name)*/
        get_net_bounds(w_current, o_current->line, rleft, rtop, rright, rbottom);
        break;

      case(OBJ_BUS):
        /* same as a line (diff name)*/
        get_bus_bounds(w_current, o_current->line, rleft, rtop, rright, rbottom);
        break;
	
      case(OBJ_BOX):
        get_box_bounds(w_current, o_current->box, rleft, rtop, rright, rbottom);
        break;

      case(OBJ_PICTURE):
        get_picture_bounds(w_current, o_current->picture, rleft, rtop, rright, rbottom);
        break;

      case(OBJ_CIRCLE):
        get_circle_bounds(w_current, o_current->circle, rleft, rtop, rright, rbottom);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        /* recursive objects ?*/
        get_object_list_bounds(w_current, o_current->complex->prim_objs, rleft, rtop, rright, rbottom);
        break;

      case(OBJ_TEXT):
        /* only do bounding boxes for visible or doing show_hidden_text*/
        /* you might lose some attrs though */
        if (o_current->visibility == VISIBLE ||
            (o_current->visibility == INVISIBLE && w_current->show_hidden_text)) {
          get_text_bounds(w_current, o_current, rleft, rtop, rright, rbottom);
        }
        break;

      case(OBJ_PIN):
        get_pin_bounds(w_current, o_current->line, rleft, rtop, rright, rbottom);
        break;

      case(OBJ_ARC):
        get_arc_bounds(w_current, o_current, rleft, rtop, rright, rbottom);
        break;

      default:
        break;
    }
  }
}

/*! \brief Return the bounds of the given list of objects.
 *  \par Given a list of objects, calcule the bounds coordinates.
 *  \param [in] w_current The toplevel structure.
 *  \param [in] complex   The list of objects to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 */
void
get_object_list_bounds(TOPLEVEL *w_current, OBJECT *complex, 
		       int *left, int *top, int *right, int *bottom)
{
  OBJECT *o_current=NULL;
  int rleft, rtop, rright, rbottom;
	
  *left = rleft = 999999;
  *top = rtop = 9999999;
  *right = rright = 0;
  *bottom = rbottom = 0;
	

  o_current = complex;
	
  while ( o_current != NULL ) {
    get_single_object_bounds(w_current, o_current, &rleft, &rtop, 
			     &rright, &rbottom);
    if (rleft < *left) *left = rleft;
    if (rtop < *top) *top = rtop;
    if (rright > *right) *right = rright;
    if (rbottom > *bottom) *bottom = rbottom;
	

    o_current=o_current->next;
  }

}

/*! \brief Return the bounds of the given GList of objects.
 *  \par Given a list of objects, calcule the bounds coordinates.
 *  \param [in] w_current The toplevel structure.
 *  \param [in] complex   The list of objects to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 */
void get_object_glist_bounds(TOPLEVEL *w_current, GList *head, 
			     int *left, int *top, int *right, int *bottom)
{
  GList *s_current=NULL;
  OBJECT *o_current=NULL;
  int rleft, rtop, rright, rbottom;
	
  *left = rleft = 999999;
  *top = rtop = 9999999;
  *right = rright = 0;
  *bottom = rbottom = 0;
	
  s_current = head;
	
  while ( s_current != NULL ) {

    o_current = (OBJECT *) s_current->data;

    g_assert (o_current != NULL);

    get_single_object_bounds(w_current, o_current, &rleft, &rtop, 
			     &rright, &rbottom);

    if (rleft < *left) *left = rleft;
    if (rtop < *top) *top = rtop;
    if (rright > *right) *right = rright;
    if (rbottom > *bottom) *bottom = rbottom;
	

    s_current=s_current->next;
  }

}

/*! \brief Return the bounds of the given object.
 *  \par Given an object, calcule the bounds coordinates.
 *  \param [in] w_current The toplevel structure.
 *  \param [in] o_current The object to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 *
 */
void world_get_single_object_bounds(TOPLEVEL *w_current, OBJECT *o_current, 
				    int *left, int *top, 
				    int *right, int *bottom)
{
  if (o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
        world_get_line_bounds(w_current, o_current->line, 
			      left, top, right, bottom);
        break;

      case(OBJ_NET):
        /* same as a line (diff name)*/
        world_get_net_bounds(w_current, o_current->line, 
			     left, top, right, bottom);
        break;

      case(OBJ_BUS):
        /* same as a line (diff name)*/
        world_get_bus_bounds(w_current, o_current->line, 
			     left, top, right, bottom);
        break;
	
	
      case(OBJ_BOX):
        world_get_box_bounds(w_current, o_current->box, 
			     left, top, right, bottom);
        break;

      case(OBJ_PICTURE):
        world_get_picture_bounds(w_current, o_current->picture, 
				 left, top, right, bottom);
        break;

      case(OBJ_CIRCLE):
        world_get_circle_bounds(w_current, o_current->circle, 
				left, top, right, bottom);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        /* recursive objects ?*/
        world_get_complex_bounds(w_current, o_current->complex->prim_objs, 
				 left, top, right, bottom);
        break;

      case(OBJ_TEXT):
        /* only do bounding boxes for visible or doing show hidden text */
        /* you might lose some attrs though */
        if (o_current->visibility == VISIBLE ||
            (o_current->visibility == INVISIBLE && 
	     w_current->show_hidden_text)) {
          world_get_text_bounds(w_current, o_current, 
				left, top, right, bottom);
        }
        break;

      case(OBJ_PIN):
        world_get_pin_bounds(w_current, o_current->line, 
			     left, top, right, bottom);
        break;

      case(OBJ_ARC):
        world_get_arc_bounds(w_current, o_current, 
			     left, top, right, bottom);
        break;

      default:
        break;
    }
  }
}


/*! \brief Return the bounds of the given complex (or list of OBJECTs).
 *  \par Given a complex (list of objects), calcule the bounds coordinates.
 *  \param [in] w_current The toplevel structure.
 *  \param [in] complex   The list of objects to look the bounds for.
 *  \param [out] left   pointer to the left coordinate of the object.
 *  \param [out] top    pointer to the top coordinate of the object.
 *  \param [out] right  pointer to the right coordinate of the object.
 *  \param [out] bottom pointer to the bottom coordinate of the object.
 */
void world_get_complex_bounds(TOPLEVEL *w_current, OBJECT *complex, 
			      int *left, int *top, int *right, int *bottom)
{
  OBJECT *o_current=NULL;
  int rleft, rtop, rright, rbottom;
	
  *left = rleft = w_current->init_right;
  *top = rtop = w_current->init_bottom;;
  *right = rright = 0;
  *bottom = rbottom = 0;
	
  o_current = complex;
	
  while ( o_current != NULL ) {
    world_get_single_object_bounds (w_current, o_current, 
				    &rleft, &rtop, &rright, &rbottom);

    if (rleft < *left) *left = rleft;
    if (rtop < *top) *top = rtop;
    if (rright > *right) *right = rright;
    if (rbottom > *bottom) *bottom = rbottom;
	
    o_current=o_current->next;
  }

}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *add_head(void)
{
  OBJECT *new_node=NULL;

  new_node = s_basic_init_object("complex_head"); 

  new_node->type = OBJ_HEAD; /* make this of head type hack */

  /* don't need to do this for head nodes */
  /* ret = (OBJECT *) s_basic_link_object(new_node, NULL);*/
  return(new_node);
}

/*! \brief
 *  \par Function Description
 *
 */
/* The promote_invisible flag is either TRUE or FALSE */
/* It controls if invisible text is promoted (TRUE) or not (FALSE) */
int o_complex_is_eligible_attribute (TOPLEVEL *w_current, OBJECT *object,
				     int promote_invisible) 
{
  char *name = NULL;
  char *value = NULL;
  char *padded_name = NULL;
  int promotableAttribute = FALSE;
  char *ptr;

  g_return_val_if_fail(object != NULL, FALSE);

  if (object->type != OBJ_TEXT || object->attribute || object->attached_to)
  {
    return FALSE; /* not a text item or is already attached */
  }

  /* Make sure text item is an attribute */
  ptr = strchr(object->text->string, '=');
  if (!ptr || ptr[1] == '\0' || ptr[1] == ' ')
  {
    return FALSE;  /* not an attribute */
  }

  /* always promote symversion= attribute, even if it is invisible */
  if (strncmp(object->text->string, "symversion=", 11) == 0)
  {
    return TRUE;
  }

  /* check list against attributes which can be promoted */
  if (w_current->always_promote_attributes &&
      (strlen(w_current->always_promote_attributes) != 0))
  {
    if (o_attrib_get_name_value(object->text->string, &name, &value))
    {
      padded_name = g_strdup_printf(" %s ", name);
      if (strstr(w_current->always_promote_attributes, padded_name))
      {
	/* Yes the name of the attribute was in the always promote */
        /* attributes list */
        promotableAttribute = TRUE;
      }
      
      g_free(padded_name);
      if (name) g_free(name);
      if (value) g_free(value);
      if (promotableAttribute)
	return TRUE;
    }
  }

  /* object is invisible and we do not want to promote invisible text */
  if (object->visibility == INVISIBLE && promote_invisible == FALSE)
  {
    return FALSE; /* attribute not eligible for promotion */
  }

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

  if (o_current->complex_clib && 
      strncmp(o_current->complex_clib, "EMBEDDED", 8) == 0) {
    return 1;
  } else {
    return 0;
  }

}

/* Done */
/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_add(TOPLEVEL *w_current, OBJECT *object_list, 
		      GList **object_glist, char type,
		      int color, int x, int y, int angle,
		      int mirror, char *clib,
		      char *basename, int selectable,
		      int attribute_promotion)
{
  OBJECT *new_node=NULL;
  OBJECT *prim_objs=NULL;
  OBJECT *temp_tail=NULL;
  OBJECT *temp_parent=NULL;
  int save_adding_sel = 0;
  int loaded_normally = FALSE;
  gboolean use_object_list;
  char *filename;
  GList *glist_ptr;

  if (object_list) {
    use_object_list = TRUE;
  } else {
    use_object_list = FALSE;
  }

  new_node = s_basic_init_object("complex");
  new_node->type = type;

  new_node->complex_basename = g_strdup(basename);
  if (clib)
    new_node->complex_clib = g_strdup(clib);
  else
    new_node->complex_clib = NULL;

  new_node->color = color;
	
  new_node->complex = (COMPLEX *) g_malloc(sizeof(COMPLEX));
	
  new_node->complex->angle = angle;
  new_node->complex->mirror = mirror;

  new_node->complex->x = x;
  new_node->complex->y = y;
  WORLDtoSCREEN(w_current, x, y, 
                &new_node->complex->screen_x, 
                &new_node->complex->screen_y);    

  new_node->draw_func = complex_draw_func;  

  if (selectable) { 
    new_node->sel_func = select_func;
  } else {
    new_node->sel_func = NULL;
  }

  /* this was at the beginning and p_complex was = to complex */
  prim_objs = (OBJECT *) add_head();
	
  /* set the parent field now */
  prim_objs->complex_parent = new_node;

  /* is the bit with the temp and object_tail needed? */
  /* I don't like this at all hack */
  /* careful whenever you select, you get a read from disk */
  /* for the objects, UGG! there foreattribs are being copied */
  /* you need to override them if there are attached ones */
  /* on the main list */
  temp_tail = w_current->page_current->object_tail;
  temp_parent = w_current->page_current->object_parent;	
  w_current->page_current->object_parent = prim_objs;
  /* reason this works is because it has a head, see add_head above */

  /* check for validity */
  if (clib && basename)
    filename = g_strdup_printf("%s%c%s", clib, G_DIR_SEPARATOR, basename);
  else 
    filename = g_strdup("unknown");

  save_adding_sel = w_current->ADDING_SEL;
  w_current->ADDING_SEL = 1;	/* name is hack, don't want to */

  if (access(filename, R_OK)) {

    OBJECT *save_prim_objs;
    char *not_found_text = NULL;
    int left, right, top, bottom;
    int x_offset, y_offset;

    if (clib == NULL) {
      s_log_message("Component library was not found or specified\n");
    } else if (basename == NULL) {
      s_log_message("Basename (component) was not found or specified\n");
    } else {
      s_log_message("Could not open symbol file [%s]\n", filename); 
    } 

    /* filename was NOT found */
    loaded_normally = FALSE;

    /* save the prim_objs pointer, since the following code modifies it */
    save_prim_objs = prim_objs;

    /* Put placeholder into object list.  Changed by SDB on
     * 1.19.2005 to fix problem that symbols were silently
     * deleted by gattrib when RC files were messed up.  */
    new_node->type = OBJ_PLACEHOLDER;

    /* Mark the origin of the missing component */
    prim_objs = o_line_add(w_current, prim_objs, OBJ_LINE, 
                           DETACHED_ATTRIBUTE_COLOR,
                           x - 50, y, x + 50, y);
    prim_objs = o_line_add(w_current, prim_objs, OBJ_LINE, 
                           DETACHED_ATTRIBUTE_COLOR,
                           x, y + 50, x, y - 50); 

    /* Add some useful text */
    not_found_text = 
      g_strdup_printf ("Component not found:\n %s", basename);
    prim_objs = o_text_add(w_current, prim_objs,
                           OBJ_TEXT, DETACHED_ATTRIBUTE_COLOR, 
                           x + NOT_FOUND_TEXT_X, 
                           y + NOT_FOUND_TEXT_Y, LOWER_LEFT, 0, 
                           not_found_text, 8,
                           VISIBLE, SHOW_NAME_VALUE);
    g_free(not_found_text);

    /* figure out where to put the hazard triangle */
    world_get_text_bounds(w_current, prim_objs,
                          &left, &top, &right, &bottom);
    x_offset = (right - left) / 4;  
    y_offset = bottom - top + 100;  /* 100 is just an additional offset */

    /* add hazard triangle */
    prim_objs = o_line_add(w_current, prim_objs, OBJ_LINE, 
                           DETACHED_ATTRIBUTE_COLOR,
                           x + NOT_FOUND_TEXT_X + x_offset, 
                           y + NOT_FOUND_TEXT_Y + y_offset, 
                           x + NOT_FOUND_TEXT_X + x_offset + 600, 
                           y + NOT_FOUND_TEXT_Y + y_offset); 
    o_set_line_options(w_current, prim_objs, END_ROUND, TYPE_SOLID, 
                       50, -1, -1);
    prim_objs = o_line_add(w_current, prim_objs, OBJ_LINE, 
                           DETACHED_ATTRIBUTE_COLOR,
                           x + NOT_FOUND_TEXT_X + x_offset, 
                           y + NOT_FOUND_TEXT_Y + y_offset, 
                           x + NOT_FOUND_TEXT_X + x_offset + 300, 
                           y + NOT_FOUND_TEXT_Y + y_offset + 500); 
    o_set_line_options(w_current, prim_objs, END_ROUND, TYPE_SOLID, 
                       50, -1, -1);
    prim_objs = o_line_add(w_current, prim_objs, OBJ_LINE, 
                           DETACHED_ATTRIBUTE_COLOR,
                           x + NOT_FOUND_TEXT_X + x_offset + 300, 
                           y + NOT_FOUND_TEXT_Y + y_offset + 500, 
                           x + NOT_FOUND_TEXT_X + x_offset + 600, 
                           y + NOT_FOUND_TEXT_Y + y_offset); 
    o_set_line_options(w_current, prim_objs, END_ROUND, TYPE_SOLID, 
                       50, -1, -1);
    prim_objs = o_text_add(w_current, prim_objs,
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
    o_read(w_current, prim_objs, filename);
    
  }
  w_current->ADDING_SEL = save_adding_sel;

  if (w_current->attribute_promotion) { /* controlled through rc file */

    OBJECT *tmp,*next;

    for (tmp=prim_objs->next;tmp;tmp=next) {

      next=tmp->next;

      /* valid floating attrib? */
      if (o_complex_is_eligible_attribute(w_current, tmp,
                                          w_current->promote_invisible))
      {
        /* Is attribute promotion called for? (passed in parameter) */
        if (attribute_promotion)
        {
          /* remove tmp from the complex list */
          if (tmp->next)
            tmp->next->prev=tmp->prev;
          if (tmp->prev)
            tmp->prev->next=tmp->next;

          /* Isolate tmp completely, now that it's removed from list */
          tmp->next=tmp->prev=NULL;
	  if (use_object_list) {
	    object_list = (OBJECT *) s_basic_link_object(tmp, object_list);
	    o_attrib_attach (w_current, object_list, tmp, new_node);
	  }
	  else {
	    if (object_glist) {
	      *object_glist = g_list_append (*object_glist, tmp);
	      
	      glist_ptr = *object_glist;
	      while (glist_ptr) {
		if (glist_ptr->prev == NULL) {
		  ((OBJECT *) glist_ptr->data)->prev = NULL;
		} else {
		  ((OBJECT *) glist_ptr->data)->prev = glist_ptr->prev->data;
		}
		if (glist_ptr->next == NULL) {
		  ((OBJECT *) glist_ptr->data)->next = NULL;
		} else {
		  ((OBJECT *) glist_ptr->data)->next = glist_ptr->next->data;
		}
		glist_ptr = glist_ptr->next;
	      }
	      
	      o_attrib_attach (w_current, ((OBJECT *) g_list_last(*object_glist)->data), 
			       tmp, new_node);
	    } else {
	      o_attrib_attach (w_current, NULL, tmp, new_node);
	    }
	  }
          o_text_translate_world(w_current, x, y, tmp);

        } else { /* not promoting now, but deal with floating attribs */

          if (w_current->keep_invisible == TRUE) {  
            /* if we are not promoting invisible attribs, keep them */
            /* around */
            tmp->visibility = INVISIBLE;
          } else {
            /* else do the default behavior of deleting the original */
            /* object */
            s_delete(w_current, tmp);
          }

        }
      }
    }
  }

  w_current->page_current->object_tail = temp_tail;
  w_current->page_current->object_parent = temp_parent;

  if (use_object_list) {
    object_list = (OBJECT *) s_basic_link_object(new_node, object_list);
    object_list->complex->prim_objs = prim_objs;
  }
  else {
    new_node->complex->prim_objs = prim_objs;
    if (object_glist) {
      *object_glist = g_list_append (*object_glist, new_node);

      glist_ptr = *object_glist;
      while (glist_ptr) {
	if (glist_ptr->prev == NULL) {
	  ((OBJECT *) glist_ptr->data)->prev = NULL;
	} else {
	  ((OBJECT *) glist_ptr->data)->prev = glist_ptr->prev->data;
	}
	if (glist_ptr->next == NULL) {
	  ((OBJECT *) glist_ptr->data)->next = NULL;
	} else {
	  ((OBJECT *) glist_ptr->data)->next = glist_ptr->next->data;
	}
	glist_ptr = glist_ptr->next;
      }
      object_list = (OBJECT *) (g_list_last(*object_glist)->data);
    } else {
      object_list = new_node;
    }
    
  }

  /* do not mirror/rotate/translate/connect the primitive objects if the
   * component was not loaded via o_read 
   */
  if (loaded_normally == TRUE) {
    if (mirror) {
      o_complex_mirror_lowlevel(w_current, x, y, new_node);
    } 
    
    o_complex_rotate_lowlevel(w_current, x, y, angle, angle, new_node); 
    o_complex_world_translate(w_current, x, y, prim_objs);

    if (!w_current->ADDING_SEL) {
     s_conn_update_complex(w_current, prim_objs);
    }
  }

  g_free(filename);
  return(object_list);
}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_add_embedded(TOPLEVEL *w_current, OBJECT *object_list,
			       char type, int color, int x, int y, int angle,
			       char *clib, char *basename, int selectable)
{
  OBJECT *prim_objs=NULL;
  OBJECT *new_node=NULL;

  new_node = s_basic_init_object("complex");
  new_node->type = type;

  new_node->complex = (COMPLEX *) g_malloc(sizeof(COMPLEX));
  new_node->complex->x = x;
  new_node->complex->y = y;
  WORLDtoSCREEN(w_current, x, y, 
                &new_node->complex->screen_x, 
                &new_node->complex->screen_y);    

  new_node->complex->angle = angle;
  new_node->complex->mirror = 0;
	
  new_node->complex_basename = g_strdup(basename);
  if (clib)
    new_node->complex_clib = g_strdup(clib);
  else
    new_node->complex_clib = NULL;

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
  prim_objs->complex_parent = object_list;

  /* don't have to translate/rotate/mirror here at all since the */
  /* object is in place */
  return(object_list);
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
  int left, right, top, bottom;

  /* realc routine Add this somewhere */
  /* libhack */
  /* o_recalc(w_current, o_current->complex);*/

  if ((!o_current) || (o_current->type != OBJ_COMPLEX && o_current->type != OBJ_PLACEHOLDER))
    return;

  get_object_list_bounds(w_current, o_current->complex->prim_objs, &left, &top, &right, &bottom);
  o_current->left = left;
  o_current->top = top;
  o_current->right = right;
  o_current->bottom = bottom;

  WORLDtoSCREEN(w_current, 
                o_current->complex->x, 
                o_current->complex->y,
                &o_current->complex->screen_x, 
                &o_current->complex->screen_y);

}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_read(TOPLEVEL *w_current, OBJECT *object_list,
		       char buf[], unsigned int release_ver,
		       unsigned int fileformat_ver)
{
  char type; 
  int x1, y1;
  int angle;

  char basename[256]; /* hack */
  char *clib=NULL;
	
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
      fprintf(stderr, "Found a component with an invalid rotation [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
      s_log_message("Found a component with an invalid rotation [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
      break;
  }

  switch(mirror) {

    case(0):
    case(1):

      break;
		
    default:
      fprintf(stderr, "Found a component with an invalid mirror flag [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
      s_log_message("Found a component with an invalid mirror flag [ %c %d %d %d %d %d %s ]\n", type, x1, y1, selectable, angle, mirror, basename); 
      break;
  }
  if (strncmp(basename, "EMBEDDED", 8) == 0) {
    
  object_list = o_complex_add_embedded(w_current, 
                                       object_list, type, 
                                       WHITE, x1, y1, angle,
                                       "EMBEDDED", basename, 
                                       selectable);
  } else {
    const GSList *clibs = s_clib_search_basename (basename);
    if (clibs == NULL) {
      s_log_message("Component [%s] was not found in any component library\n", 
		    basename);
      fprintf(stderr,
	      "Component [%s] was not found in any component library\n", 
	      basename);
      clib = NULL;
    } else {
      if (g_slist_next (clibs)) {
	s_log_message ("More than one component found with name [%s]\n",
		       basename);
	/* PB: for now, use the first directory in clibs */
	/* PB: maybe open a dialog to select the right one? */
      }
      clib = (gchar*)clibs->data;
    }
    
    object_list = o_complex_add(w_current, object_list, NULL, type, 
				WHITE, 
				x1, y1, 
				angle, mirror,
				clib, basename, selectable, FALSE);
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

  g_return_val_if_fail (object != NULL, NULL);

  if (object->sel_func != NULL) 
  selectable = 1;
  else 
  selectable = 0;

  if (object->type == OBJ_COMPLEX) {
    buf = g_strdup_printf("%c %d %d %d %d %d %s", object->type,
	    		  object->complex->x, object->complex->y, selectable, 
			  object->complex->angle, object->complex->mirror, 
			  object->complex_basename);
  } else if (object->type == OBJ_PLACEHOLDER) {
    buf = g_strdup_printf("C %d %d %d %d %d %s",
	    		  object->complex->x, object->complex->y, selectable, 
			  object->complex->angle, object->complex->mirror, 
			  object->complex_basename);  /* write 'C' manually */
  }

  return(buf);
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_set_filename(TOPLEVEL *w_current, char *clib, char *basename) 
{
  int len;

  if (basename == NULL) {
    fprintf(stderr, "Got NULL basename in o_complex_set_filename!\n");
    exit(-1);
  }

  if (clib == NULL) {
    fprintf(stderr, "Got NULL clib in o_complex_set_filename!\n");
    exit(-1);
  }

  if (w_current->internal_basename) {
    g_free(w_current->internal_basename);
  }

  if (w_current->internal_clib) {
    g_free(w_current->internal_clib);
  }

  len = strlen(basename);
  w_current->internal_basename = (char *) g_malloc(sizeof(char)*len+1);

  len = strlen(clib) + 1;	
  w_current->internal_clib = (char *) g_malloc(sizeof(char)*len+1);

  strcpy(w_current->internal_basename, basename);	
  strcpy(w_current->internal_clib, clib);	
} 

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_free_filename(TOPLEVEL *w_current)
{
  if (w_current->internal_basename) {
    g_free(w_current->internal_basename);
  }

  if (w_current->internal_clib) {
    g_free(w_current->internal_clib);
  }
}

/*! \brief
 *  \par Function Description
 *
 */
/* now I think it works fine */
/* no there is a bug with snap locking.  Basically if you don't snap/lock in */
/* PCtoW, then this doesn't work... :(  I don't know why yet */
void o_complex_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  int x, y;
  int prevx, prevy;

  if (object == NULL) {
    printf("cmpt NO!\n");
    return;
  }

  object->complex->screen_x = object->complex->screen_x + dx;
  object->complex->screen_y = object->complex->screen_y + dy;


  /* this fixing makes me nervious hack */
  SCREENtoWORLD(w_current, object->complex->screen_x,
                object->complex->screen_y, &x, &y);

  prevx = object->complex->x;
  prevy = object->complex->y;
  object->complex->x = snap_grid(w_current, x);
  object->complex->y = snap_grid(w_current, y);

  o_complex_world_translate(w_current, x - prevx,y - prevy, 
                            object->complex->prim_objs);
}

/*! \brief
 *  \par Function Description
 *
 */
/* this needs work remove display stuff */
/* libhack */
/* and recalc stuff */
/* this function takes in a complex list */
void o_complex_world_translate(TOPLEVEL *w_current, int x1, int y1, 
			       OBJECT *prim_objs)
{
  OBJECT *o_current=NULL;
  OBJECT *one=NULL;
  OBJECT *two=NULL;
  unsigned long temp_color;

  o_current = prim_objs;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_translate_world(w_current, x1, y1, o_current);
        break;

      case(OBJ_NET):
				/* same as a line, don't do this */
        o_line_translate_world(w_current, x1, y1, o_current);
        temp_color = w_current->override_color;
        w_current->override_color = -1;
        o_redraw_single(w_current, one); /* trying loop? hack*/
        o_redraw_single(w_current, two);
        w_current->override_color = temp_color;
        break;

      case(OBJ_BUS):
				/* same as a line, don't do this */
        o_line_translate_world(w_current, x1, y1, o_current);
        temp_color = w_current->override_color;
        w_current->override_color = -1;
        o_redraw_single(w_current, one); /* trying loop? hack*/
        o_redraw_single(w_current, two);
        w_current->override_color = temp_color;
        break;
	
      case(OBJ_BOX):
        o_box_translate_world(w_current, x1, y1, o_current);
        break;

      case(OBJ_PICTURE):
        o_picture_translate_world(w_current, x1, y1, o_current);
        break;

      case(OBJ_CIRCLE):
        o_circle_translate_world(w_current, x1, y1, o_current);
        break;
	
      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_complex_world_translate_toplevel(w_current, x1, y1, o_current);
        break;

      case(OBJ_TEXT):
        o_text_translate_world(w_current, x1, y1, o_current);
        break;

        /* same note as above */
      case(OBJ_PIN):
        o_pin_translate_world(w_current, x1, y1, o_current);
        break;

      case(OBJ_ARC):
        o_arc_translate_world(w_current, x1, y1, o_current);
        break;
    }
    o_current=o_current->next;
  }
}

/*! \brief
 *  \par Function Description
 *
 */
/* this function takes the toplevel object and then also translates the
 * complex */
void o_complex_world_translate_toplevel(TOPLEVEL *w_current,
					int x1, int y1, OBJECT *object)
{
  int left, right, top, bottom;

  g_return_if_fail(object != NULL);
  g_return_if_fail((object->type == OBJ_COMPLEX) ||
		   (object->type == OBJ_PLACEHOLDER));

  object->complex->x = object->complex->x + x1;
  object->complex->y = object->complex->y + y1;

  WORLDtoSCREEN(w_current, object->complex->x,
                object->complex->y,
                &object->complex->screen_x,
                &object->complex->screen_y);

  o_complex_world_translate(w_current, x1, y1, 
                            object->complex->prim_objs);

  get_object_list_bounds(w_current, object->complex->prim_objs, 
			 &left, &top, &right, &bottom);
	
  object->left = left;
  object->top = top;
  object->right = right;
  object->bottom = bottom;
}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_copy(TOPLEVEL *w_current, OBJECT *list_tail,
		       OBJECT *o_current)
{
  OBJECT *new_obj=NULL;
  ATTRIB *a_current;
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

  new_obj = o_complex_add(w_current, list_tail, NULL, o_current->type, color,
                          o_current->complex->x, o_current->complex->y, 
                          o_current->complex->angle, o_current->complex->mirror,
                          o_current->complex_clib, o_current->complex_basename, 
                          selectable, FALSE); 

  o_attrib_slot_copy(w_current, o_current, new_obj);

  /* deal with stuff that has changed */

  /* here you need to create a list of attributes which need to be 
   * connected to the new list, probably make an attribute list and
   * fill it with sid's of the attributes */
  a_current = o_current->attribs;
  if (a_current) {
    while ( a_current ) {

      /* head attrib node has prev = NULL */	
      if (a_current->prev != NULL) {
        a_current->copied_to = new_obj;	
      }
	
      a_current = a_current->next;
    }
  }

  return(new_obj);
}

/*! \brief
 *  \par Function Description
 *
 */
OBJECT *o_complex_copy_embedded(TOPLEVEL *w_current, OBJECT *list_tail,
				OBJECT *o_current)
{
  OBJECT *new_obj=NULL;
  OBJECT *temp_list;
  ATTRIB *a_current;
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

  new_obj = o_complex_add_embedded(w_current, list_tail, o_current->type, 
                                   color,
                                   o_current->complex->x, o_current->complex->y, 
                                   o_current->complex->angle, 
                                   o_current->complex_clib, 
                                   o_current->complex_basename, 
                                   selectable); 
  /* deal with stuff that has changed */
	
  temp_list = o_list_copy_all(w_current,
                              o_current->complex->prim_objs->next,
                              new_obj->complex->prim_objs, 
                              NORMAL_FLAG);
	
  new_obj->complex->prim_objs = return_head(temp_list);

  /* here you need to create a list of attributes which need to be 
   * connected to the new list, probably make an attribute list and
   * fill it with sid's of the attributes */
  a_current = o_current->attribs;
  if (a_current) {
    while ( a_current ) {

      /* head attrib node has prev = NULL */	
      if (a_current->prev != NULL) {
        a_current->copied_to = new_obj;	
      }
	
      a_current = a_current->next;
    }
  }

  return(new_obj);
}

/*! \brief
 *  \par Function Description
 *
 */
void o_complex_delete(TOPLEVEL *w_current, OBJECT *delete)
{
  g_return_if_fail(delete != NULL);

  /* first remove complex pointer */
  if (delete->complex) {
    if (delete->complex->prim_objs) {
      s_delete_list_fromstart(w_current, 
                              delete->complex->prim_objs);
    }
    delete->complex->prim_objs = NULL;
  }

  /* then remove the actual node */	
  s_delete(w_current, delete);
  delete = NULL;
  w_current->page_current->object_tail = (OBJECT *) 
  return_tail(w_current->page_current->object_head);
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

/*! \brief
 *  \par Function Description
 *
 */
/* pass in top level object */
void o_complex_rotate_lowlevel(TOPLEVEL *w_current, int world_centerx, 
			       int world_centery, 
			       int angle,
			       int angle_change,
			       OBJECT *object)
{
  OBJECT *o_current=NULL;

#if DEBUG 
  printf("------- a %d ac %d\n", angle, angle_change);
#endif

  g_return_if_fail(object != NULL);
  g_return_if_fail(((object->type == OBJ_COMPLEX) ||
		    (object->type == OBJ_PLACEHOLDER)));
  g_return_if_fail(object->complex != NULL);


  /* do individual complex objects */
  o_current = object->complex->prim_objs;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;

      case(OBJ_NET):
        o_net_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;

      case(OBJ_BUS):
        o_bus_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;
	
      case(OBJ_BOX):
        o_box_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;

      case(OBJ_PICTURE):
        o_picture_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;

      case(OBJ_CIRCLE):
        o_circle_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;

      case(OBJ_PIN):
        o_pin_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;

      case(OBJ_ARC):
        o_arc_rotate_world(w_current, 0, 0, angle_change, o_current);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
	o_complex_rotate_lowlevel(w_current, 0, 0, angle, angle_change, o_current);
        break; 

      case(OBJ_TEXT):
        o_text_rotate_world(w_current, 0, 0, angle, angle_change, o_current);
        break;

    }
    o_current=o_current->next;
  }
}

/*! \brief
 *  \par Function Description
 *
 */
/* pass in top level object */
void o_complex_mirror_lowlevel(TOPLEVEL *w_current, 
			       int world_centerx, int world_centery,
			       OBJECT *object)
{
  OBJECT *o_current=NULL;

  g_return_if_fail(object != NULL);
  g_return_if_fail(((object->type == OBJ_COMPLEX) ||
		    (object->type == OBJ_PLACEHOLDER)));
  g_return_if_fail(object->complex != NULL);

  /* do individual complex objects */
  o_current = object->complex->prim_objs;

  while ( o_current != NULL ) {
    switch(o_current->type) {
      case(OBJ_LINE):
        o_line_mirror_world(w_current, 0, 0, o_current);
        break;

      case(OBJ_NET):
        o_net_mirror_world(w_current, 0, 0, o_current);
        break;

      case(OBJ_BUS):
        o_bus_mirror_world(w_current, 0, 0, o_current);
        break;
	
      case(OBJ_BOX):
        o_box_mirror_world(w_current, 0, 0, o_current);
        break;

      case(OBJ_PICTURE):
        o_picture_mirror_world(w_current, 0, 0, o_current);
        break;

      case(OBJ_CIRCLE):
        o_circle_mirror_world(w_current, 0, 0, o_current);
        break;

      case(OBJ_PIN):
        o_pin_mirror_world(w_current, 0, 0, o_current);
        break;

      case(OBJ_ARC):
        o_arc_mirror_world(w_current, 0, 0, o_current);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
	o_complex_mirror_lowlevel(w_current, 0, 0, o_current);
	break;

      case(OBJ_TEXT):
        o_text_mirror_world(w_current, 0, 0, o_current);
        break;

    }
    o_current=o_current->next;
  }

  /* mirror origin point */
  /*	object->x = -object->x;*/
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

/*! \brief
 *  \par Function Description
 *
 */
/* pass in top level object */
void
o_complex_check_symversion(TOPLEVEL* w_current, OBJECT* object) 
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
        s_log_message("WARNING: Symbol version parse error on refdes %s:\n"
                    "\tCould not parse symbol file symversion=%s\n",
                     refdes, inside);
      } else {
        s_log_message("WARNING: Symbol version parse error on refdes %s:\n"
                    "\tCould not parse symbol file symversion=\n",
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
      s_log_message("WARNING: Symbol version parse error on refdes %s:\n"
                    "\tCould not parse attached symversion=%s\n",
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
    s_log_message("WARNING: Symbol version oddity on refdes %s:\n"
                  "\tsymversion=%s attached to instantiated symbol, "
                  "but no symversion= inside symbol file\n",
                  refdes, outside);
    goto done;
  }

  /* inside & not outside is a valid case, means symbol in library is newer */
  /* also if inside_value is greater than outside_value, then symbol in */
  /* library is newer */
  if ((inside_present && !outside_present) ||
      ((inside_present && outside_present) && (inside_value > outside_value)))
  {
    
    fprintf(stderr, "WARNING: Symbol version mismatch on refdes %s (%s):\n"
            "\tSymbol in library is newer than "
            "instantiated symbol\n",
            refdes, object->complex_basename);
    s_log_message("WARNING: Symbol version mismatch on refdes %s (%s):\n"
                  "\tSymbol in library is newer than "
                  "instantiated symbol\n",
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
      fprintf(stderr, "\tMAJOR VERSION CHANGE (file %.3f, "
              "instantiated %.3f, %s)!\n",
              inside_value, outside_value, refdes);
      s_log_message("\tMAJOR VERSION CHANGE (file %.3f, "
                    "instantiated %.3f)!\n",
                    inside_value, outside_value);

      /* add the refdes to the major_changed_refdes GList */
      /* make sure refdes_copy is freed somewhere */
      refdes_copy = g_strconcat (refdes, " (",
                                 object->complex_basename,
                                 ")", NULL);
      w_current->major_changed_refdes =
        g_list_append(w_current->major_changed_refdes, refdes_copy);

      /* don't bother checking minor changes if there are major ones*/
      goto done; 
    }

    if (inside_minor > outside_minor)
    {
      fprintf(stderr, "\tMinor version change (file %.3f, "
              "instantiated %.3f)\n",
              inside_value, outside_value);
      s_log_message("\tMinor version change (file %.3f, "
                    "instantiated %.3f)\n",
                    inside_value, outside_value);
    }

    goto done;
  }

  /* outside value is greater than inside value, this is weird case */
  if ((inside_present && outside_present) && (outside_value > inside_value))
  {
    s_log_message("WARNING: Symbol version oddity on refdes %s:\n"
                  "\tInstanciated symbol is newer than "
                  "symbol in library\n",
                  refdes);
    goto done;
  }

  /* if inside_value and outside_value match, then symbol versions are okay */

done:
  if (inside) g_free(inside);
  if (outside) g_free(outside);
  if (refdes) g_free(refdes);
}
