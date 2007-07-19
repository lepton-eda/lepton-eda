/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003-2007 Stuart D. Brorson.
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

/*------------------------------------------------------------------
 * This file holds fcns involved in manipulating the OBJECT data
 * structure.  OBJECT is defined in libgeda.  An OBJECT is a graphical
 * primitive normally used in gschem.  Example OBJECTs: some text, 
 * a component (complex), a pin, a line, etc. 
 *
 * The fcns herein are fcns which I wrote as wrappers to the 
 * fcns in libgeda.  
 *------------------------------------------------------------------ */

#include <config.h>
 
#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>
 
/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


                                                                                                           
/* ===================  Public Functions  ====================== */

/*------------------------------------------------------------------*/
/*! \brief This fcn adds a new attrib to o_current, when o_current is a
 * component.  It does it in the following 
 * way:
 * 1. It creates an object -- "attrib_graphic" -- and fills it in.
 * 2. It gets the position info from o_current's refdes attrib and
 *    calls o_text_add to add pos info and name=value string 
 *    to attrib_graphic.
 * 3. It calls o_attrib_add to wrap attrib_graphic with (ATTRIB )
 *
 *------------------------------------------------------------------ */
void s_object_add_comp_attrib_to_object(OBJECT *o_current, 
					char *new_attrib_name, 
					char *new_attrib_value,
					gint visibility,
					gint show_name_value)
{
  char *name_value_pair;
  OBJECT *attrib_graphic_object;


  /* One last sanity check, then add attrib */
  if (strlen(new_attrib_value) != 0) {
    name_value_pair = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
    attrib_graphic_object = s_object_attrib_add_attrib_in_object(pr_current, name_value_pair, visibility, 
							 show_name_value, o_current);
  }
  
  return;

}


/*------------------------------------------------------------------*/
/*! /brief This needs to be filled in.
 *
 *------------------------------------------------------------------*/
void s_object_add_net_attrib_to_object(OBJECT *o_current, char *new_attrib_name, 
				char *new_attrib_value)
{
  /* TBD */
}


/*------------------------------------------------------------------*/
/*! \brief This fcn adds a new attrib to o_current, when o_current is a
 * pin.  It does it in the following 
 * way:
 * 1. It creates an object -- "attrib_graphic" -- and fills it in.
 * 2. It gets the position info from o_current's refdes attrib and
 *    calls o_text_add to add pos info and name=value string 
 *    to attrib_graphic.
 * 3. It calls o_attrib_add to wrap attrib_graphic with (ATTRIB )
 * Question:  Do I really need separate fcns for comps, nets, and 
 * pins???
 *
 *------------------------------------------------------------------ */
void s_object_add_pin_attrib_to_object(OBJECT *o_current, char *new_attrib_name, 
				char *new_attrib_value)
{
  char *name_value_pair;
  OBJECT *attrib_graphic_object;


  /* One last sanity check */
  if (strlen(new_attrib_value) != 0) {
    name_value_pair = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
    attrib_graphic_object = s_object_attrib_add_attrib_in_object(pr_current, name_value_pair, 
								 INVISIBLE, 
								 SHOW_NAME_VALUE, o_current);
  }
  
  return;

}




/*------------------------------------------------------------------*/
/*! \brief This fcn finds the instance of attrib_name on o_current, and
 * replaces it's value wiht new_attrib_value.
 *
 *------------------------------------------------------------------*/
void s_object_replace_attrib_in_object(OBJECT *o_current, 
				       char *new_attrib_name, 
				       char *new_attrib_value,
				       gint visibility, 
				       gint show_name_value)
{
  ATTRIB *a_current;
  char *old_attrib_text;
  char *old_attrib_name;
  char *new_attrib_text;


  a_current = o_current->attribs;
  while (a_current != NULL) {
    if (a_current->object->type == OBJ_TEXT 
	&& a_current->object->text != NULL) {  /* found an attribute */
      
      /* may need to check more thoroughly here. . . . */
      old_attrib_text = g_strdup(a_current->object->text->string);
      old_attrib_name = u_basic_breakup_string(old_attrib_text, '=', 0);
      
      if (strcmp(old_attrib_name, new_attrib_name) == 0) {
	/* create attrib=value text string & stuff it back into pr_current */
	new_attrib_text = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
	g_free(a_current->object->text->string);   /* remove old attrib string */
	a_current->object->text->string = g_strdup(new_attrib_text);   /* insert new attrib string */
	if (visibility != LEAVE_VISIBILITY_ALONE) 
	  a_current->object->visibility = visibility;
	if (show_name_value != LEAVE_NAME_VALUE_ALONE)
	  a_current->object->show_name_value = show_name_value; 
	g_free(new_attrib_text);
	g_free(old_attrib_text);
	g_free(old_attrib_name);
	return;     /* we are done -- leave. */
      } else {
	g_free(old_attrib_text);
	g_free(old_attrib_name);
      }  /* if (strcmp . . . . */
    } /* if (a_current . . . . */
  
    a_current = a_current->next;
  }  /* while */

  /* if we get here, it's because we have failed to find the attrib on the component.
   * This is an error condition. */
  fprintf(stderr, 
	 "In s_object_replace_attrib_in_object, we have failed to find the attrib %s on the component.  Exiting . . .\n", 
	 new_attrib_name);
  exit(-1);
}


/*------------------------------------------------------------------*/
/*! \brief This fcn removes attrib from o_current.
 *
 *------------------------------------------------------------------*/
void s_object_remove_attrib_in_object(OBJECT *o_current, char *new_attrib_name) 
{
  
  ATTRIB *a_current;
  OBJECT *attribute_object;
  char *old_attrib_text;
  char *old_attrib_name;
  
  a_current = o_current->attribs;
  while (a_current != NULL) {
    if (a_current->object->type == OBJ_TEXT 
	&& a_current->object->text != NULL) {  /* found an attribute */
      
      /* may need to check more thoroughly here. . . . */
      old_attrib_text = g_strdup(a_current->object->text->string);
      old_attrib_name = u_basic_breakup_string(old_attrib_text, '=', 0);
      
      if (strcmp(old_attrib_name, new_attrib_name) == 0) {
	/* We've found the attrib.  Delete it and then return. */

#ifdef DEBUG
	printf("In s_object_remove_attrib_in_object, removing attrib with name = %s\n", old_attrib_name);
#endif

	attribute_object = a_current->object;
	s_object_delete_text_object_in_object(pr_current, attribute_object);

	g_free(old_attrib_text);
	g_free(old_attrib_name);
	return;     /* we are done -- leave. */
      }
    g_free(old_attrib_text);
    g_free(old_attrib_name);
    }
    a_current = a_current->next;
  }
  
  /* if we get here, it's because we have failed to find the attrib on the component.
   * This is an error condition. */
  fprintf(stderr, 
	 "In s_object_remove_attrib_in_object, we have failed to find the attrib %s on the component.  Exiting . . .\n", 
	 new_attrib_name);
  exit(-1);
}



/*------------------------------------------------------------------*/
/*! \brief This fcn attaches the name=value pair to the OBJECT "object"  It 
 * was stolen from gschem/src/o_attrib.c:o_attrib_add_attrib and
 * hacked for gattrib.  Does it need to return OBJECT?
 *
 *------------------------------------------------------------------*/
OBJECT *s_object_attrib_add_attrib_in_object(TOPLEVEL * pr_current, char *text_string,
			    int visibility, int show_name_value,
			    OBJECT * object)
{
  int world_x = -1, world_y = -1;
  int color;
  int left, right, top, bottom;
  OBJECT *o_current;

  color = pr_current->detachedattr_color;

  o_current = object;

  /* creating a toplevel or unattached attribute */
  if (o_current) {
    /* get coordinates of where to place the text object */
    switch (o_current->type) {
    case (OBJ_COMPLEX):
      world_x = o_current->complex->x;
      world_y = o_current->complex->y;
      color = pr_current->attribute_color;
      break;

    case (OBJ_NET):
      world_x = o_current->complex->x;
      world_y = o_current->complex->y;
      color = pr_current->attribute_color;
      break;

    default:
      fprintf(stderr, "In s_object_attrib_add_attrib_in_object, trying to add attrib to non-complex or non-net!\n");
      exit(-1);
    }
  } else {    /* This must be a floating attrib, but what is that !?!?!?!?!  */
    world_get_object_list_bounds(pr_current,
                                 pr_current->page_current->object_head,
                                 &left, &top, &right, &bottom);

    /* this really is the lower left hand corner */
    world_x = left;
    world_y = top;

    /* printf("%d %d\n", world_x, world_y); */
    color = pr_current->detachedattr_color;
  }

  /* first create text item */
#if DEBUG
  printf("===  In s_object_attrib_add_attrib_in_object, about to attach new text attrib with properties:\n");
  printf("     color = %d\n", color);
  printf("     text_string = %s \n", text_string);
  printf("     text_size = %d \n", pr_current->text_size);
  printf("     visibility = %d \n", visibility);
  printf("     show_name_value = %d \n", show_name_value);
#endif

  pr_current->page_current->object_tail = o_text_add(pr_current, 
						    pr_current->page_current->object_tail, 
						    OBJ_TEXT, 
						    color, 
						    world_x, 
						    world_y, 
						    LOWER_LEFT, 
						    0,	/* zero is angle */
						    text_string, 
						    pr_current->text_size,  /* current text size */
						    visibility,
						    show_name_value);

  /* now pr_current->page_current->object_tail contains new text item */

  /* now attach the attribute to the object (if o_current is not NULL) */
  /* remember that o_current contains the object to get the attribute */
  if (o_current) {
    o_attrib_attach(pr_current, pr_current->page_current->object_head,
		    pr_current->page_current->object_tail, o_current);
  }

  o_selection_add( pr_current->page_current->selection_list,
                   pr_current->page_current->object_tail);


  pr_current->page_current->CHANGED = 1;

  return (pr_current->page_current->object_tail);
}



 
/*------------------------------------------------------------------*/
/*! \brief This fcn deletes the text object pointed to by text_object.  It
 * was shamelessly stolen from gschem/src/o_delete.c and hacked
 * for gattrib by SDB.
 *------------------------------------------------------------------*/
void s_object_delete_text_object_in_object(TOPLEVEL * pr_current, OBJECT * text_object)
{
  s_delete(pr_current, text_object);
  pr_current->page_current->CHANGED = 1;

#if 0
  /*    What does this do?!?!?  Maybe I don't need it!!!  */
  pr_current->page_current->object_tail =
      (OBJECT *) return_tail(pr_current->page_current->object_head);
#endif 

}
                                                                                                    

/*------------------------------------------------------------------*/
/*! \brief This verifies that the object has a non-null symbol file.
 *
 * \returns It returns 0 = valid symbol file, 1 = no symbol file found.
 *
 *------------------------------------------------------------------*/
int s_object_has_sym_file(OBJECT *object)
{
  char *filename;

  filename = object->complex_basename;
  if (filename != NULL) {
#ifdef DEBUG
    printf("In s_object_has_sym_file, object has sym file = %s.\n", filename);
#endif
    return 0;
  } else {
#ifdef DEBUG
    printf("In s_object_has_sym_file, found object with no attached symbol file.\n");
#endif 
    return 1;
  }
}
