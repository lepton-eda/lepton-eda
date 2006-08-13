/* gEDA - GPL Electronic Design Automation
 * gattrib -- gEDA component and net attribute manipulation using spreadsheet.
 * Copyright (C) 2003 Stuart D. Brorson.
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
 * This file holds fcns involved in manipulating an entire SHEET_DATA
 * structure.  The SHEET_DATA structure is the intermediate structure
 * between TOPLEVEL (gEDA's native format) and the graphical gtksheet
 * widget (from gtkextra), which is the spreadsheet widget displaying 
 * the attribs.
 *------------------------------------------------------------------*/

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


/*------------------------------------------------------------------
 * This fcn is the sheet_data creator.  It returns a pointer to 
 * an initialized SHEET_DATA struct.
 *------------------------------------------------------------------*/
SHEET_DATA *s_sheet_data_new()
{
  SHEET_DATA *new_sheet;

  new_sheet = (SHEET_DATA *) g_malloc(sizeof(SHEET_DATA));

  /* We will malloc and fill out the comp table later. */
  new_sheet->component_table = NULL;

  /* We will malloc and fill out the net table later. */
  new_sheet->net_table = NULL;

  /* We will malloc and fill out the pin table later. */
  new_sheet->pin_table = NULL;

  /* Now we create the first cell in each master list. */
  new_sheet->master_comp_list_head = (STRING_LIST *) s_string_list_new();
  new_sheet->master_comp_attrib_list_head = (STRING_LIST *) s_string_list_new();
  new_sheet->comp_count = 0;
  new_sheet->comp_attrib_count = 0;

  new_sheet->master_net_list_head = (STRING_LIST *) s_string_list_new();
  new_sheet->master_net_attrib_list_head = (STRING_LIST *) s_string_list_new();
  new_sheet->net_count = 0;
  new_sheet->net_attrib_count = 0;

  new_sheet->master_pin_list_head = (STRING_LIST *) s_string_list_new();
  new_sheet->master_pin_attrib_list_head = (STRING_LIST *) s_string_list_new();
  new_sheet->pin_count = 0;
  new_sheet->pin_attrib_count = 0;

  new_sheet->CHANGED = FALSE;

  return (new_sheet);

}



/*------------------------------------------------------------------
 * This fcn adds to the master list of components refdeses by running
 * through the components and recording the comp refdeses
 * it discovers. Then it sorts them into alphabetical order.
 * Data struct being searched is: OBJECT->attribs(->next. . .)->object->text->string
 *------------------------------------------------------------------*/
void s_sheet_data_add_master_comp_list_items(OBJECT *start_obj) {
  char *temp_uref;
  OBJECT *o_current;
  
#ifdef DEBUG
  printf("=========== Just entered  s_sheet_data_add_master_comp_list_items!  ==============\n");
#endif

  if (verbose_mode) {
    printf("- Starting master comp list creation.\n");
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  o_current = start_obj;
  while (o_current != NULL) {

#ifdef DEBUG
      printf("In s_sheet_data_add_master_comp_list_items, examining o_current->name = %s\n", o_current->name);
#endif

      /*-----  only process if this is a non-graphical comp with attributes ----*/
      if ( (o_current->type == OBJ_COMPLEX) &&
	    o_current->attribs && 
	    !o_attrib_search_component(o_current, "graphical") ) {    
	
#if DEBUG
	printf("      In s_sheet_data_add_master_comp_list_items; found component on page\n");
	printf(". . . . complex_basename = %s.\n", o_current->complex_basename);
#endif
	verbose_print(" C");
      
	temp_uref = s_attrib_get_refdes(o_current);
	
	/* Now that we have refdes, store refdes and attach attrib list to component */
	if (temp_uref) {
#if DEBUG
	  printf("       In s_sheet_add_master_comp_list, about to add to master list refdes = %s\n", temp_uref);
#endif
	  s_string_list_add_item(sheet_head->master_comp_list_head, &(sheet_head->comp_count), temp_uref);
	  g_free(temp_uref);
	}
	
      } /*  if (o_current->type == OBJ_COMPLEX . . . . .) */
      
      o_current = o_current->next;  /* iterate to next object on page */
  }  /*  while o_current != NULL */
  
  return;
}


/*------------------------------------------------------------------
 * This fcn adds to the master list of comp attribs by running
 * through each component on the page and recording all attribs 
 * it discovers. Then it sorts them into an order used for the 
 * horiz listing of the attribs on the spreadsheet.
 * Data struct being searched is: sheet_head->component_list_head->attrib->name;
 *------------------------------------------------------------------*/
void s_sheet_data_add_master_comp_attrib_list_items(OBJECT *start_obj) {
  char *attrib_text;
  char *attrib_name;
  OBJECT *o_current;
  ATTRIB *a_current;
  
#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("=========== Just entered  s_sheet_data_add_master_comp_attrib_list_items!  ==============\n");
#endif

  if (verbose_mode) {
    printf("- Starting master comp attrib list creation.\n");
  }

  /* -----  Iterate through all objects found on page looking for components (OBJ_COMPLEX) ----- */
  o_current = start_obj;
  while (o_current != NULL) {

#ifdef DEBUG
      printf("In s_sheet_data_add_master_comp_attrib_list_items, examining o_current->name = %s\n", o_current->name);
#endif

      /*-----  only process if this is a non-graphical comp with attributes ----*/
      if ( (o_current->type == OBJ_COMPLEX) &&
	    o_current->attribs && 
	    !o_attrib_search_component(o_current, "graphical") ) {    

	
	verbose_print(" C");
	
	/*------ Iterate through all attribs found on component -----*/
	a_current = o_current->attribs; /* This has a side effect.  Why? */
	while (a_current != NULL) {
	  if (a_current->object->type == OBJ_TEXT 
	      && a_current->object->text != NULL) {  /* found an attribute */
	    attrib_text = g_strdup(a_current->object->text->string);
	    attrib_name = u_basic_breakup_string(attrib_text, '=', 0);

	      /* Don't include "refdes" or "slot" because they form the row name */
	    if ( (strcmp(attrib_name, "refdes") != 0) 
		 && (strcmp(attrib_name, "slot") != 0) ) {  
#if DEBUG
	      printf(" . . . from this component, about to add to master comp attrib list attrib = %s\n", attrib_name);
#endif
	      s_string_list_add_item(sheet_head->master_comp_attrib_list_head, 
				     &(sheet_head->comp_attrib_count), attrib_name);
	    }   /* if (strcmp(attrib_name, "refdes") != 0) */ 
	    g_free(attrib_name);
	    g_free(attrib_text);
	  }
	  a_current = a_current->next;
	}   /*  while  */
	
      }   /* if (o_current->type == OBJ_COMPLEX) */
      
      o_current = o_current->next;
  }   /* while (o_current != NULL) */
  
  /* -----  Now sort component list into alphabetical order  ----- */
  
  return;
}



/*------------------------------------------------------------------
 * This fcn builds the master list of net names by running
 * through the individual cells and recording the net refdeses
 * it discovers. 
 *------------------------------------------------------------------*/
void s_sheet_data_add_master_net_list_items(OBJECT *start_obj) {
  return;
}


/*------------------------------------------------------------------
 * This fcn builds the master list of net attribs.
 *------------------------------------------------------------------*/
void s_sheet_data_add_master_net_attrib_list_items(OBJECT *start_obj) {
  return;
}


/*------------------------------------------------------------------
 * This fcn builds the master list of pin names.  It writes the
 * label refdes:pinnumber into the global master pin list.
 * Algorithm:
 * 1.  Loop on o_current looking for OBJ_COMPLEX
 * 2.  When we find a complex, save the refdes.
 * 3.  Dive down to o_lower_current = o_current->complex->prim_objs
 * 4.  Loop on o_lower_current looking for OBJ_PIN
 * 5.  When we find a pin, find the pinnumber by calling
 *     o_attrib_search_name_single(o_lower_current, "pinnumber", NULL)
 * 6.  Create the pin list label as "refdes=XXX", and stick it into
 *     the master pin list.
 * Since this fcn operates on the global sheet_data->master_pin_list, 
 * it doesn't return a value.
 *------------------------------------------------------------------*/
void s_sheet_data_add_master_pin_list_items(OBJECT *start_obj) {
  char *temp_uref;
  char *temp_pinnumber;
  char *row_label;
  OBJECT *o_current;
  OBJECT *o_lower_current;
  
#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("=========== Just entered  s_sheet_data_add_master_pin_list_items!  ==============\n");
#endif

  if (verbose_mode) {
    printf("- Starting master pin list creation.\n");
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  o_current = start_obj;
  while (o_current != NULL) {

#ifdef DEBUG
      printf("In s_sheet_data_add_master_pin_list_items, examining o_current->name = %s\n", o_current->name);
#endif

      if (o_current->type == OBJ_COMPLEX) {
	temp_uref = s_attrib_get_refdes(o_current);
	if (temp_uref != NULL) {      /* make sure object complex has a refdes  */
	  
	  /* -----  Now iterate through lower level objects looking for pins.  ----- */
	  o_lower_current = o_current->complex->prim_objs;
	  while (o_lower_current != NULL) {
#if DEBUG
	    printf("In s_sheet_data_add_master_pin_list_items, examining object name %s\n", o_lower_current->name);
#endif
	    if (o_lower_current->type == OBJ_PIN) {
	      temp_pinnumber = o_attrib_search_name_single(o_lower_current, "pinnumber", NULL);
	      
	      if( temp_pinnumber != NULL) {
		row_label = g_strconcat(temp_uref, ":", temp_pinnumber, NULL);
#if DEBUG
		printf("In s_sheet_data_add_master_pin_list_items, about to add to master pin list row_label = %s\n", row_label);
#endif
		s_string_list_add_item(sheet_head->master_pin_list_head, &(sheet_head->pin_count), row_label);
		
	      } else {      /* didn't find pinnumber.  Report error to log. */
		fprintf(stderr, "In s_sheet_data_add_master_pin_list_items, found component pin with no pinnumber.\n");
#ifdef DEBUG
		fprintf(stderr, ". . . . refdes = %s.\n", temp_uref);
#endif
	      }
	      g_free(temp_pinnumber);
	      
	    }
	    o_lower_current = o_lower_current->next;
	  }   /*   while (o_lower_current != NULL)   */
	  
	} else {          /* didn't find refdes.  Report error to log. */
#ifdef DEBUG
	  fprintf(stderr, "In s_sheet_data_add_master_pin_list_items, found non-graphical component with no refdes.\n");
	  fprintf(stderr, ". . . . complex_basename = %s.\n", o_current->complex_basename);
#endif
	}
	g_free(temp_uref);

      }  /*  if (o_current->type == OBJ_COMPLEX)  */
      o_current = o_current->next;  
	
  }   /*  while o_current != NULL */
      
  return;
}


/*------------------------------------------------------------------
 * This fcn builds the master list of pin attributes.  It writes 
 * each attrib name into the master pin attrib list.
 * Algorithm:
 * 1.  Loop on o_current looking for OBJ_COMPLEX
 * 2.  When we find a complex, save the refdes.
 * 3.  Dive down to o_lower_current = o_current->complex->prim_objs
 * 4.  Loop on o_lower_current looking for OBJ_PIN
 * 5.  When we find a pin, get pin_attribs = o_lower_current->attribs
 * 6.  Loop on attribs looking for non-NULL text.
 * 7.  When we find a non-NULL text attrib, extract the attrib name
 *     and stick it in the master pin attrib list.
 *------------------------------------------------------------------*/
void s_sheet_data_add_master_pin_attrib_list_items(OBJECT *start_obj) {
  char *temp_uref;
  char *attrib_text;
  char *attrib_name;
  char *attrib_value;
  OBJECT *o_current;
  OBJECT *o_lower_current;
  ATTRIB *pin_attrib;
  
#ifdef DEBUG
  fflush(stderr);
  fflush(stdout);
  printf("=========== Just entered  s_sheet_data_add_master_pin_attrib_list_items!  ==============\n");
#endif

  if (verbose_mode) {
    printf("- Starting master pin attrib list creation.\n");
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  o_current = start_obj;
  while (o_current != NULL) {

#ifdef DEBUG
      printf("In s_sheet_data_add_master_pin_attrib_list_items, examining o_current->name = %s\n", o_current->name);
#endif

      if (o_current->type == OBJ_COMPLEX) {
	temp_uref = s_attrib_get_refdes(o_current);
	if (temp_uref != NULL) {      /* make sure object complex has a refdes  */
	  
	  /* -----  Now iterate through lower level objects looking for pins.  ----- */
	  o_lower_current = o_current->complex->prim_objs;
	  while (o_lower_current != NULL) {
#if DEBUG
	    printf("In s_sheet_data_add_master_pin_attrib_list_items, examining component refdes =  %s\n", temp_uref);
#endif
	    if (o_lower_current->type == OBJ_PIN) {
	      /* -----  Found a pin.  Now get attrib head and loop on attribs.  ----- */
	      pin_attrib = o_lower_current->attribs;
	      while (pin_attrib != NULL) {
		if (pin_attrib->object->type == OBJ_TEXT 
		    && pin_attrib->object->text != NULL) {  /* found an attribute */
		  attrib_text = g_strdup(pin_attrib->object->text->string);
		  attrib_name = u_basic_breakup_string(attrib_text, '=', 0);
		  attrib_value = s_misc_remaining_string(attrib_text, '=', 1);
		  if ( (strcmp(attrib_name, "pinnumber") != 0) 
		       && (attrib_value != NULL) ) {  
		    /* Don't include "pinnumber" because it is already in other master list.
		     * Also guard against pathalogical symbols which have non-attrib text inside pins. */

#if DEBUG
	    printf("In s_sheet_data_add_master_pin_attrib_list_items, found pin attrib =  %s\n", attrib_name);
	    printf(". . . . . adding it to master_pin_attrib_list\n");
#endif

		    s_string_list_add_item(sheet_head->master_pin_attrib_list_head, 
					   &(sheet_head->pin_attrib_count), attrib_name);
		  }   /* if (strcmp(attrib_name, "pinnumber") != 0) */ 
		  if (attrib_value != NULL) g_free(attrib_value);
		  g_free(attrib_name);
		  g_free(attrib_text);
		}
		pin_attrib = pin_attrib->next;
	      }   /*   while (pin_attrib != NULL)  */
	    }
	    o_lower_current = o_lower_current->next;
	  }   /*  while (o_lower_current != NULL)   */

	  g_free(temp_uref);
	}  /*  if (temp_uref != NULL )  */
	
      }  /* if (o_current->type == OBJ_COMPLEX)  */
      o_current = o_current->next;

  }  /*  while (o_current != NULL)  */
  return;

}




/*------------------------------------------------------------------
 * This fcn extracts the attribs from the gtksheet
 * cells, and places them back into SHEET_DATA.  This is the
 * first step in saving out a project.  Right now I just invoke
 * s_table_gtksheet_to_table.  Do I need to do anything else here?
 *------------------------------------------------------------------*/
void s_sheet_data_gtksheet_to_sheetdata() {
  s_table_gtksheet_to_all_tables();
  /* do I need to do anything else here? */

  return;
}




