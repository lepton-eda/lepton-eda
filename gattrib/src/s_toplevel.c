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
 * This file holds fcns involved in manipulating the TOPLEVEL data
 * structure.  TOPLEVEL is the data structure inherited from gEDA's
 * other programs, and holds all info about a project in a form
 * native to gEDA.
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


/* ===================  Public Functions  ====================== */

/*------------------------------------------------------------------
 * This fcn inits the toplevel data struct pr_current.  It basically
 * calls a fcn to initialize the window variables.
 *------------------------------------------------------------------*/
void
s_toplevel_init(TOPLEVEL *pr_current)
{
  i_window_vars_set(pr_current);   /* The window vars are used in gschem,
                                      but we need to set them here because
                                      graphical information is used
                                      when introducing new attributes. */
  return;
}

/*------------------------------------------------------------------
 * This fcn reads in a page & calls f_open, which fills out the 
 * pr_current structure.  
 *------------------------------------------------------------------*/
int s_toplevel_read_page(char *filename)
{
  PAGE local_page;
  int return_code;

  /* If this is not the first page, try to create a new page. */
  if (first_page != 1) {
    if (s_page_new(pr_current, filename) ) {
      printf("Schematic page [%s] already loaded!\n", filename);
      return;
    } else {
      /* if we get here, it's because this is a new page */
      if (!quiet_mode) {
	printf("Loading schematic [%s]\n", filename);
      }
    }
  } else {
    /* if we get here, it's because this is the first page */
    if (!quiet_mode) {
      printf("Loading schematic [%s]\n", filename);
    }
  }
    
  /* Now that we have a new page, set the new filename and read in the page */
  pr_current->page_current->page_filename = u_basic_strdup(filename);
  
  /* read in and fill out pr_current using f_open and its callees */
  return_code = f_open(pr_current, filename);
  if (!return_code) {        /* 1 = success reading in page */
    /* 0 = failure reading in page */
    fprintf(stderr, "Couldn't load schematic [%s]\n", filename);
  }

  return return_code;
}

/*------------------------------------------------------------------
 * This fcn returns 1 if the project is empty (i.e. pr_current is 
 * not filled out yet), and 0 if the project is non-empty (i.e. there
 * is some data in pr_current).
 *------------------------------------------------------------------*/
int s_toplevel_empty_project()
{
  /* Nothing here yet.  Is this necessary in current program
   * architecture? */
}



/*------------------------------------------------------------------
 * This fcn is called when the user invokes "save".  It first
 * places all data from gtksheet into SHEET_DATA.  Then it
 * loops through all pages & calls s_toplevel_sheetdata_to_toplevel to place all
 * stuff in SHEET_DATA into the libgeda TOPLEVEL structure.
 *------------------------------------------------------------------*/
void
s_toplevel_gtksheet_to_toplevel()
{
  PAGE *p_current;

#if DEBUG
  printf("---------------------   Entering  s_toplevel_gtksheet_to_toplevel   -------------------\n");
#endif  


  s_sheet_data_gtksheet_to_sheetdata();  /* read data from gtksheet into SHEET_DATA */
#if DEBUG
  printf("In s_toplevel_gtksheet_to_toplevel -- done writing stuff from gtksheet into SHEET_DATA.\n");
#endif  

  p_current = pr_current->page_head; /* must iterate over all pages in design */
  while (p_current != NULL) {
    if (p_current->pid != -1) {   /* only traverse pages which are toplevel */
      if (p_current->object_head && p_current->page_control == 0) {
        s_toplevel_sheetdata_to_toplevel(p_current->object_head);    /* adds all objects from page */
      }
    }
    p_current = p_current->next;  /* iterate to next schematic page */
  }
#if DEBUG
  printf("In s_toplevel_gtksheet_to_toplevel -- done writing SHEEET_DATA text back into pr_currnet.\n");
#endif  

  return;

}



/* =======================  Callbacks  ====================== */

/*------------------------------------------------------------------
 * This fcn is the callback from the menu bar.  It throws up the 
 * filedialog widget, and then accepts from it a list of files to
 * open.  Then it figures out if there is already an existing
 * project, and call the appropriate version of s_toplevel_read 
 * depending upon that.
 *------------------------------------------------------------------*/
void s_toplevel_menubar_file_open(TOPLEVEL *pr_current)
{
  int filesel_type = OPEN;

#ifdef DEBUG
  printf("In s_toplevel_menubar_file_open, about to create fileselect box\n");
#endif

  x_fileselect_setup(pr_current, filesel_type);
  return;
}


/*------------------------------------------------------------------
 * This fcn is the file->save callback from the menu bar.  It 
 * first updates the proect, and then saves the project without
 * throwing up the filedialog widget.
 *------------------------------------------------------------------*/
void s_toplevel_menubar_file_save(TOPLEVEL *pr_current)
{

#ifdef DEBUG
  printf("In s_toplevel_menubar_file_save, about to save out the project\n");
#endif

  s_toplevel_gtksheet_to_toplevel();
  s_page_save_all(pr_current);  /* saves all pages in design */

  return;
}


/*------------------------------------------------------------------
 * This fcn is a hack.  It gives a non-NULL value to the select_func
 * defined in globals.c for libgeda.  A non-NULL value for this fcn
 * makes sure that object->sel_func = 1 when the project is saved out,
 * which keeps the objects selectable in gschem.
 * Perhaps I should just set the variable myself when saving the 
 * project out . . . . .
 *------------------------------------------------------------------*/
void s_toplevel_select_object()
{
  /* I don't know if I should do anything in here to prevent
   * the function from being optimized away by gcc.  */
}


/* =======================  Private fcns  ====================== */

/*------------------------------------------------------------------
 * This fcn takes the updated SHEET_DATA->TABLE object and then
 * loops through all objects on (PAGE page)->(OBJECT *start_obj) 
 * & updates the attached 
 * attrib values using the updated values held in the SHEET_DATA->TABLE 
 * structure.  It does so in three steps:
 * 1.  First find and update component attribs.
 * 2.  Then find and update net attribs.
 * 3.  Finally find and update pin attribs.
 *------------------------------------------------------------------*/
void
s_toplevel_sheetdata_to_toplevel(OBJECT *start_obj)
{
  OBJECT *o_current;
  OBJECT *comp_prim_obj;
  char *temp_uref;
  char *temp_netname;
  char *temp_pin;
  STRING_LIST *new_comp_attrib_list;
  STRING_LIST *new_net_attrib_list;
  STRING_LIST *new_pin_attrib_list;

  /* -----  First deal with all components on the page.  ----- */
#ifdef DEBUG
	printf("-----  In s_toplevel_sheetdata_to_toplevel, handling components\n");
#endif
  o_current = start_obj;
  while (o_current != NULL) {

    /* ------- Object is a component.  Handle component attributes. ------- */
    if (o_current->type == OBJ_COMPLEX) {    /* Note that OBJ_COMPLEX = component + attribs */

#if 0
      if ( o_attrib_search_component(o_current, "graphical") ) {
        break;  /* Ignore graphical components */
      }
#endif

      temp_uref = o_attrib_search_name_single(o_current, "refdes", NULL);
      if (temp_uref != NULL) {
	new_comp_attrib_list = s_toplevel_get_component_attribs_in_sheet(temp_uref);
	s_toplevel_update_component_attribs_in_toplevel(o_current, new_comp_attrib_list);
	free(temp_uref);
      } else {
#ifdef DEBUG
	printf("In s_toplevel_sheetdata_to_toplevel, found complex with no refdes. name = %s\n", o_current->name);
#endif
      }
    }  /* if (o_current->type == OBJ_COMPLEX) */


    o_current = o_current->next;
  }  /* while (o_current != NULL) */


#if 0
  /* -----  Next deal with all nets on the page.  ----- */
#ifdef DEBUG
	printf("-----  In s_toplevel_sheetdata_to_toplevel, handling nets\n");
#endif
  o_current = start_obj;
  while (o_current != NULL) {
    if (o_current->type == OBJ_NET) { 

      temp_netname = o_attrib_search_name_single(o_current, "netname", NULL);
      if (temp_netname != NULL) {
	new_net_attrib_list = s_toplevel_get_net_attribs_in_sheet(temp_netname);
	s_toplevel_update_net_attribs_in_toplevel(o_current, new_net_attrib_list);
	free(temp_netname);
      }

    }
    o_current = o_current->next;
  }  /* while (o_current != NULL) */

#endif


  /* -----  Finally deal with all pins on the page.  ----- */
  /* -----  Next deal with all nets on the page.  ----- */
#ifdef DEBUG
	printf("-----  In s_toplevel_sheetdata_to_toplevel, handling pins\n");
#endif
  o_current = start_obj;
  while (o_current != NULL) {

    /* ------- Object is a complex.  Handle pins by looking ------ */
    /* ------- for all pins attached to a component.        ------ */
    if (o_current->type == OBJ_COMPLEX) { 
      /*  Upon finding a component, here's what to do:
       *  0.  Get refdes of component.
       *  1.  Loop over prim_objects, looking for pins.
       *  2.  When a pin is found, create refdes:pinnumber pair
       *      used in searching TABLE.
       *  3.  Search TABLE using refdes:pinnumber as key, and get list of 
       *      attribs corresponding to this refdes:pinnumber
       *  4.  Stick the attribs into the TOPLEVEL data structure.
       */
      temp_uref = o_attrib_search_name_single(o_current, "refdes", NULL);
      if ( (temp_uref != NULL) && (o_current->complex->prim_objs) ) {    /* make sure object complex has a refdes  */

	comp_prim_obj = o_current->complex->prim_objs;
	while (comp_prim_obj != NULL) {
	  if (comp_prim_obj->type == OBJ_PIN) { 
	    new_pin_attrib_list = s_toplevel_get_pin_attribs_in_sheet(temp_uref, comp_prim_obj);
	    s_toplevel_update_pin_attribs_in_toplevel(temp_uref, comp_prim_obj, new_pin_attrib_list);
	  }
	  comp_prim_obj = comp_prim_obj->next;  
	}    
      }     /* if(temp_uref  */
      
      free(temp_uref);
    }
    
    o_current = o_current->next;
  }  /* while (o_current != NULL) */
      
  return;
}


/*------------------------------------------------------------------
 * This fcn returns a list of attributes attached to obj_name = comp
 * refdes or netlist.  The returned list is a STRING_LIST where the
 * ->data holds a name=value string.
 *------------------------------------------------------------------*/
STRING_LIST *s_toplevel_get_component_attribs_in_sheet(char *refdes)
{
  STRING_LIST *new_attrib_list;
  STRING_LIST *local_attrib_list;
  STRING_LIST *row_list;
  int i;
  int row = -1;
  int count = 0;
  char *row_item;
  char *name_value_pair;
  char *new_attrib_value;
  char *new_attrib_name;
  char *temp;

#if DEBUG
  printf("-----  Entering s_toplevel_get_component_attribs_in_sheet.\n");
#endif


  /* First find pos of this refdes in the master list */
  row = s_table_get_index(sheet_head->master_comp_list_head, refdes);

  /* Sanity check */
  if (row == -1) {
    /* we didn't find the item in the list */
    fprintf(stderr, 
	    "In s_toplevel_get_component_attribs_in_sheet, we didn't find the refdes in the master list!\n");
    return NULL;
  }

  /* Now get all attribs associated with this refdes (in TABLE, indexed
   * by position), and insert them into new_attrib_list.  */
  new_attrib_list = s_string_list_new();  /* init new_attrib_list */

  i = 0;
  local_attrib_list = sheet_head->master_comp_attrib_list_head;
  while (local_attrib_list != NULL) {  /* iterate over all possible attribs */
    new_attrib_name = u_basic_strdup(local_attrib_list->data);  /* take attrib name from column headings */

    if ( ((sheet_head->component_table)[row][i]).attrib_value ) {
      new_attrib_value = u_basic_strdup( ((sheet_head->component_table)[row][i]).attrib_value );
      name_value_pair = u_basic_strdup_multiple(new_attrib_name, "=", new_attrib_value, NULL);
      free(new_attrib_value);      
    } else {
      name_value_pair = u_basic_strdup_multiple(new_attrib_name, "=", NULL);  /* empty attrib */
    }
    s_string_list_add_item(new_attrib_list, &count, name_value_pair);  /* add name=value to new list */
    free(new_attrib_name);
    free(name_value_pair);

    /* Sanity check */
    if (count != i+1) {
      /* for some reason, we have lost a name_value_pair somewhere . . .  */
      fprintf(stderr, 
	      "In s_toplevel_get_component_attribs_in_sheet, count != i!  Exiting . . . .\n");
      exit(-1);
    }

    /* iterate */
    i++;
    local_attrib_list = local_attrib_list->next;
  } /* while (local_attrib_list != NULL)  */
  
  return new_attrib_list;
}



/*------------------------------------------------------------------
 * For each attrib string attached to the component, update it using the value
 * held in new_comp_attrib_list.  Algorithm:
 * 1.  Loop over name=value pairs held in new_comp_attrib_list.
 * 2.  For each name=value pair, look for corresponding attrib on o_current.
 * 3.  If the attrib exists on o_current and in name=value pair, write the 
 *     new value in.
 * 4.  If the attrib exists on o_current, but is null in name=value pair,
 *     delete the attrib.
 * 5.  If the attribs doesn't exist on o_current, but is non-null in
 *     the name=value pair, create an attrib object and add it to the part.
 *
 * Calling args:  OBJECT *o_current -- component (complex) to be updated.
 *                STRING_LIST *new_comp... -- list of name=value attribute pairs.
 * Returns: Returns nothing because the changes are made in o_current, which
 *          is part of the global TOPLEVEL pr_current.
 *------------------------------------------------------------------*/
void s_toplevel_update_component_attribs_in_toplevel(OBJECT *o_current, 
					 STRING_LIST *new_comp_attrib_list)
{
  STRING_LIST *local_list;
  char *new_name_value_pair;
  char *new_attrib_name;
  char *new_attrib_value;
  char *old_attrib_name;
  char *old_attrib_value;
  ATTRIB *a_current;

#if DEBUG
  printf("-----  Entering s_toplevel_update_component_attribs_in_toplevel.\n");
#endif

  /* loop on name=value pairs held in new_comp_attrib_list */
  local_list = new_comp_attrib_list;
  while (local_list != NULL) {
    new_name_value_pair = u_basic_strdup(local_list->data);
#if DEBUG
  printf("        In s_toplevel_update_component_attribs_in_toplevel, handling entry in master list %s .\n", new_name_value_pair);
#endif
    new_attrib_name = u_basic_breakup_string(new_name_value_pair, '=', 0);
    new_attrib_value = u_basic_breakup_string(new_name_value_pair, '=', 1);
    if (strlen(new_attrib_value) == 0) {
      free(new_attrib_value);   /* I wonder if I should check for non-NULL first?  */
      new_attrib_value = NULL;  /* u_basic_breakup_string doesn't return NULL for empty substring. */
    }
    old_attrib_value = o_attrib_search_name_single_count(o_current, new_attrib_name, 0);

    /* -------  Four cases to consider: Case 1 ----- */
    if ( (old_attrib_value != NULL) && (new_attrib_value != NULL) && (strlen(new_attrib_value) != 0) ) {
      /* simply write new attrib into place of old one. */
#if DEBUG
      printf("In s_toplevel_update_component_attribs_in_toplevel, about to replace old attrib with name= %s, value= %s\n",
	     new_attrib_name, new_attrib_value);
#endif
      s_object_replace_attrib_in_object(o_current, new_attrib_name, new_attrib_value);
    }

    /* -------  Four cases to consider: Case 2 ----- */
    else if ( (old_attrib_value != NULL) && (new_attrib_value == NULL) ) {
      /* remove attrib from component*/
#if DEBUG
      printf("In s_toplevel_update_component_attribs_in_toplevel, about to remove old attrib with name= %s, value= %s\n",
	     new_attrib_name, old_attrib_value);
#endif
      s_object_remove_attrib_in_object(o_current, new_attrib_name);
    }

    /* -------  Four cases to consider: Case 3 ----- */
    else if ( (old_attrib_value == NULL) && (new_attrib_value != NULL) ) {
      /* add new attrib to component. */

#if DEBUG
      printf("In s_toplevel_update_component_attribs_in_toplevel, about to add new attrib with name= %s, value= %s\n",
	     new_attrib_name, new_attrib_value);
#endif 

      s_object_add_comp_attrib_to_object(o_current, new_attrib_name, new_attrib_value);

      /* -------  Four cases to consider: Case 4 ----- */
    } else {
      /* Do nothing. */
#if DEBUG
      printf("In s_toplevel_update_component_attribs_in_toplevel, nothing needs to be done.\n");
#endif
    }

    /* free everything and iterate */
    free(new_name_value_pair);
    free(new_attrib_name);
    if (new_attrib_value != NULL) {
      free(new_attrib_value);  /* Be careful, this can be NULL */
    }
    if (old_attrib_value != NULL) {
      free(old_attrib_value);  /* Be careful, this can be NULL */
    }
    local_list = local_list->next;
  }   /*   while (local_list != NULL)  */
  return;
}


/*------------------------------------------------------------------
 * 
 *------------------------------------------------------------------*/
STRING_LIST *s_toplevel_get_net_attribs_in_sheet(char *netname)
{
  /* must be filled in */
  return;
}


/*------------------------------------------------------------------
 * 
 *------------------------------------------------------------------*/
void s_toplevel_update_net_attribs_in_toplevel(OBJECT *o_current, 
				   STRING_LIST *new_net_attrib_list)
{
  /* must be filled in */
  return;
}


/*------------------------------------------------------------------
 * This fcn takes a pointer to the OBJECT pin, and returns a list
 * of attribs found attached to the pin.  The returned list is a 
 * STRING_LIST where the ->data holds a name=value string.
 * The algorithm is as follows:
 * 1.  Form refdes:pinnumber label for this pin.
 * 2.  Get row number of this refdes:pinnumber
 * 3.  Create a list of name=value pairs from entries in the pin_table
 *     on this row.
 * 4.  Return list of name=value pairs found.
 *------------------------------------------------------------------*/
STRING_LIST *s_toplevel_get_pin_attribs_in_sheet(char *refdes, OBJECT *pin)
{
  STRING_LIST *new_attrib_list;
  STRING_LIST *local_attrib_list;
  STRING_LIST *row_list;
  int i;
  int row = -1;
  int count = 0;
  char *pinnumber;
  char *row_label;
  char *name_value_pair;
  char *new_attrib_value;
  char *new_attrib_name;
  char *temp;

#if DEBUG
  printf("-----  Entering s_toplevel_get_pin_attribs_in_sheet.\n");
#endif

  /* First find pos of this pin in the master pin list */
  /* first convert refdes, pin to refdes:pinno text string. Then call table_get_index.  */

  pinnumber = o_attrib_search_name_single(pin, "pinnumber", NULL);

  if ( (refdes != NULL) && (pinnumber != NULL) ) {
    row_label = u_basic_strdup_multiple(refdes, ":", pinnumber, NULL);
  } else {
    fprintf(stderr, 
	    "In s_toplevel_get_pin_attribs_in_sheet, either refdes or pinnumber of object missing!\n");
    return NULL;
  }
  row = s_table_get_index(sheet_head->master_pin_list_head, row_label);

  /* Sanity check */
  if (row == -1) {
    /* we didn't find the item in the list */
    fprintf(stderr, 
	    "In s_toplevel_get_pin_attribs_in_sheet, we didn't find the refdes:pin in the master list!\n");
    return NULL;
  }

  /* Now get all attribs associated with this refdes (in TABLE, indexed
   * by position), and insert them into new_attrib_list.  */
  new_attrib_list = s_string_list_new();  /* init new_attrib_list */

  i = 0;
  local_attrib_list = sheet_head->master_pin_attrib_list_head;
  while (local_attrib_list != NULL) {  /* iterate over all possible attribs */
    new_attrib_name = u_basic_strdup(local_attrib_list->data);  /* take attrib name from column headings */

    if ( ((sheet_head->pin_table)[row][i]).attrib_value ) {
      new_attrib_value = u_basic_strdup( ((sheet_head->pin_table)[row][i]).attrib_value );
      name_value_pair = u_basic_strdup_multiple(new_attrib_name, "=", new_attrib_value, NULL);
      free(new_attrib_value);      
    } else {
      name_value_pair = u_basic_strdup_multiple(new_attrib_name, "=", NULL);  /* empty attrib */
    }
    s_string_list_add_item(new_attrib_list, &count, name_value_pair);  /* add name=value to new list */
    free(new_attrib_name);
    free(name_value_pair);

    /* Sanity check */
    if (count != i+1) {
      /* for some reason, we have lost a name_value_pair somewhere . . .  */
      fprintf(stderr, 
	      "In s_toplevel_get_pin_attribs_in_sheet, count != i!  Exiting . . . .\n");
      exit(-1);
    }

    /* iterate */
    i++;
    local_attrib_list = local_attrib_list->next;
  } /* while (local_attrib_list != NULL)  */
  
  return new_attrib_list;
}



/*------------------------------------------------------------------
 * For each attrib string attached to the pin, update it using the value
 * held in new_pin_attrib_list.  Algorithm:
 * 1.  Loop over name=value pairs held in new_pin_attrib_list.
 * 2.  For each name=value pair, look for corresponding attrib on pin.
 * 3.  If the attrib exists on pin and in name=value pair, write the 
 *     new value in.
 * 4.  If the attrib exists on pin, but is null in name=value pair,
 *     delete the attrib.
 * 5.  If the attribs doesn't exist on pin, but is non-null in
 *     the name=value pair, create an attrib object and add it to the pin.
 * Returns: Returns nothing because the changes are made in o_pin, which
 *          is part of the global TOPLEVEL pr_current.
 *------------------------------------------------------------------*/
void s_toplevel_update_pin_attribs_in_toplevel(char *refdes, OBJECT *o_pin, 
				   STRING_LIST *new_pin_attrib_list)
{
  STRING_LIST *local_list;
  char *new_name_value_pair;
  char *new_attrib_name;
  char *new_attrib_value;
  char *old_attrib_name;
  char *old_attrib_value;
  ATTRIB *a_current;

#if DEBUG
  printf("-----  Entering s_toplevel_update_pin_attribs_in_toplevel.\n");
#endif

  /* loop on name=value pairs held in new_pin_attrib_list */
  local_list = new_pin_attrib_list;
  while (local_list != NULL) {
    new_name_value_pair = u_basic_strdup(local_list->data);
#if DEBUG
  printf("        In s_toplevel_update_pin_attribs_in_toplevel, handling entry in master list %s .\n", new_name_value_pair);
#endif
    new_attrib_name = u_basic_breakup_string(new_name_value_pair, '=', 0);
    new_attrib_value = u_basic_breakup_string(new_name_value_pair, '=', 1);
    if (strlen(new_attrib_value) == 0) {
      free(new_attrib_value);   /* I wonder if I should check for non-NULL first?  */
      new_attrib_value = NULL;  /* u_basic_breakup_string doesn't return NULL for empty substring. */
    }
    old_attrib_value = o_attrib_search_name_single_count(o_pin, new_attrib_name, 0);
                                                                                                       
    /* -------  Four cases to consider: Case 1 ----- */
    if ( (old_attrib_value != NULL) && (new_attrib_value != NULL) && (strlen(new_attrib_value) != 0) ) {
      /* simply write new attrib into place of old one. */
#if DEBUG
      printf("In s_toplevel_update_pin_attribs_in_toplevel, about to replace old attrib with new one: name= %s, value= %s\n",
             new_attrib_name, new_attrib_value);
#endif
      s_object_replace_attrib_in_object(o_pin, new_attrib_name, new_attrib_value);
    }
                                                                                                       
    /* -------  Four cases to consider: Case 2 ----- */
    else if ( (old_attrib_value != NULL) && (new_attrib_value == NULL) ) {
      /* remove attrib from pin */
#if DEBUG
      printf("In s_toplevel_update_pin_attribs_in_toplevel, about to remove old attrib with name= %s, value= %s\n",
             new_attrib_name, old_attrib_value);
#endif
      s_object_remove_attrib_in_object(o_pin, new_attrib_name);
    }
                                                                                                       
    /* -------  Four cases to consider: Case 3 ----- */
    else if ( (old_attrib_value == NULL) && (new_attrib_value != NULL) ) {
      /* add new attrib to pin. */
                                                                                                       
#if DEBUG
      printf("In s_toplevel_update_pin_attribs_in_toplevel, about to add new attrib with name= %s, value= %s\n",
             new_attrib_name, new_attrib_value);
#endif
                                                                                                       
      s_object_add_pin_attrib_to_object(o_pin, new_attrib_name, new_attrib_value);
                                                                                                       
      /* -------  Four cases to consider: Case 4 ----- */
    } else {
      /* Do nothing. */
#if DEBUG
      printf("In s_toplevel_update_pin_attribs_in_toplevel, nothing needs to be done.\n");
#endif
    }
                                                                                                       
    /* free everything and iterate */
    free(new_name_value_pair);
    free(new_attrib_name);
    if (new_attrib_value != NULL) {
      free(new_attrib_value);  /* Be careful, this can be NULL */
    }
    if (old_attrib_value != NULL) {
      free(old_attrib_value);  /* Be careful, this can be NULL */
    }
    local_list = local_list->next;
  }   /*   while (local_list != NULL)  */

  return;
}




