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

/* I am putting all callbacks from the menubar here.  This is kind
 * of stupid, since most of the time I just use the fcns here to 
 * invoke a fcn in x_*.  Perhaps I'll change this later.
 */

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
 *  This fcn is the edit->new attrib column callback from the menu bar.
 *  It throws up the "enter new attrib name" dialog box, then returns.
 *------------------------------------------------------------------*/
void s_toplevel_menubar_edit_newattrib()
{
  gint cur_page;

  /* first verify that we are on the correct page (components) */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));

  /* Check that we are on components page. */
  if (cur_page == 0) {
#ifdef DEBUG
    printf("In s_toplevel_menubar_edit_newattrib, about to add new attrib column\n");
#endif
    x_dialog_newattrib_get_name();  /* This creates dialog box  */
  }

  return;
}  


/*------------------------------------------------------------------
 *  This fcn gets called when the user has entered a new attrib name,
 *  and clicked the OK button.  It does this:
 *  1. It figures out which attrib/sheet is being added to
 *  2. It destroys the old table in preparation for the new attrib.
 *  3. It adds the new attrib to the master lists.
 *  4. It creates a new table with the new attrib.
 *  5. It then adds the appropriate col to the gtksheet.
 *------------------------------------------------------------------*/
void s_toplevel_add_new_attrib(gchar *new_attrib_name) {
  gint cur_page;  /* current page in notbook  */

  if (strcmp(new_attrib_name, "_cancel") == 0) {
    return;  /* user pressed cancel or closed window with no value in entry */
  }

  /* Next must figure out which sheet the attrib belongs to. */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
#ifdef DEBUG
  printf("In s_toplevel_add_new_attrib, adding new attrib to page %d.\n", 
	 cur_page);
#endif

  switch (cur_page) {
  case 0:  /* component attribute  */

    /*  Eventually, I want to just resize the table to accomodate the 
     *  new attrib.  However, that is difficult.  Therefore, I will just
     *  destroy the old table and recreate it for now. */

    s_table_destroy(sheet_head->component_table, 
		    sheet_head->comp_count, sheet_head->comp_attrib_count);

#ifdef DEBUG 
    printf("In s_toplevel_menubar_edit_newattrib, before adding new comp attrib.\n");
    printf("                           comp_attrib_count = %d\n", sheet_head->comp_attrib_count);
#endif

    s_string_list_add_item(sheet_head->master_comp_attrib_list_head, 
			   &(sheet_head->comp_attrib_count), 
			   new_attrib_name);
    s_string_list_sort_master_comp_attrib_list();

#ifdef DEBUG
    printf("In s_toplevel_menubar_edit_newattrib, just updated comp_attrib string list.\n");
    printf("                             new comp_attrib_count = %d\n", sheet_head->comp_attrib_count);
#endif

    /* Now create new table */
    sheet_head->component_table = s_table_new(sheet_head->comp_count, 
					      sheet_head->comp_attrib_count);

    /* Fill out new sheet with new stuff from gtksheet */
    gtk_sheet_insert_columns(GTK_SHEET(sheets[0]), sheet_head->comp_attrib_count, 1);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[0]), 
			      sheet_head->comp_attrib_count, 
			      sheet_head->master_comp_attrib_list_head);

#ifdef DEBUG
    printf("In s_toplevel_menubar_edit_newattrib, just updated gtksheet.\n");
#endif

    break;

  case 1:  /* net attribute  */
    /* insert into net attribute list  */
    break;
    
  case 2:  /* pin attribute  */
    /* insert into pin attribute list  */
    break;
  }  /* switch  */

  return;
}


/*------------------------------------------------------------------
 *  This fcn is the edit->new attrib column callback from the menu bar.
 *  It throws up the "enter new attrib name" dialog box, then returns.
 *------------------------------------------------------------------*/
void s_toplevel_menubar_edit_delattrib()
{
#ifdef DEBUG
  printf("In s_toplevel_menubar_edit_delattrib, about to delete attribute column\n");
#endif

  x_dialog_delattrib_confirm();
  return;
}  


/*------------------------------------------------------------------
 *  This fcn gets called when the user has selected a single attrib
 *  column, selected the edit->delete attrib item from the pull-down
 *  menu, and then said "yes" to the confirm dialog.
 *------------------------------------------------------------------*/
void s_toplevel_delete_attrib_col() {
  gint cur_page;  /* current page in notbook  */
  gint mincol, maxcol;
  GtkSheet *sheet;
  gchar *attrib_name;

  /* Repeat previous checks  */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  sheet = GTK_SHEET(sheets[cur_page]);
  if (sheet == NULL) {
    return;
  }
  mincol = x_gtksheet_get_min_col(sheet);
  maxcol =  x_gtksheet_get_max_col(sheet);
  if ( (mincol != maxcol) || (mincol == -1) || (maxcol == -1) ) {
    return;
  }

#ifdef DEBUG
  printf("In s_toplevel_delete_attrib_col, checks were OK, now do real work\n");
#endif

  /*  Rebuild the gattrib-specific data structures  */
  switch (cur_page) {

#ifdef HAS_GTK22  
/* The above #ifdef is very heavy handed and was add by Ales to get gattrib
 * to build using gtk12.  gtk_sheet_column_button_get_label is not defined
 * in the gtksheet_1_2.c.  This should be fixed properly at some point. TBD
 */
  case 0:  /* component attribute  */

    /*  Eventually, I want to just resize the table after deleting the
     *  attrib.  However, that is difficult.  Therefore, I will just
     *  destroy the old table and recreate it for now. */

    s_table_destroy(sheet_head->component_table, 
		    sheet_head->comp_count, sheet_head->comp_attrib_count);

    /*  Get name (label) of the col to delete from the gtk sheet */
    attrib_name = u_basic_strdup( gtk_sheet_column_button_get_label(sheet, mincol) );
    
    if (attrib_name != NULL) {
#ifdef DEBUG
      printf("In s_toplevel_delete_attrib_col, attrib to delete = %s\n", attrib_name);
#endif
    } else {
      fprintf(stderr, "In s_toplevel_delete_attrib_col, can't get attrib name\n");
      return;
    }
    
#ifdef DEBUG 
    printf("In s_toplevel_delete_attrib_col, before deleting comp attrib.\n");
    printf("                           comp_attrib_count = %d\n", sheet_head->comp_attrib_count);
#endif
    s_string_list_delete_item(&(sheet_head->master_comp_attrib_list_head), 
			      &(sheet_head->comp_attrib_count), 
			      attrib_name);
    s_string_list_sort_master_comp_attrib_list(); /* this renumbers list also */
    free(attrib_name);
    
#ifdef DEBUG
    printf("In s_toplevel_delete_attrib_col, just updated comp_attrib string list.\n");
    printf("                             new comp_attrib_count = %d\n", sheet_head->comp_attrib_count);
#endif
    
    /* Now create new table with new attrib count*/
    sheet_head->component_table = s_table_new(sheet_head->comp_count, 
					      sheet_head->comp_attrib_count);

    
#ifdef DEBUG
    printf("In s_toplevel_delete_attrib_col, just updated SHEET_DATA info.\n");
#endif
    break;
#endif

  case 1:  /* net attribute  */
    /* insert into net attribute list  */
    break;
    
  case 2:  /* pin attribute  */
    /* insert into pin attribute list  */
    break;
  }  /* switch  */


  /* Delete col on gtksheet  */
#ifdef DEBUG
  printf("In s_toplevel_delete_attrib_col, about to delete col in gtksheet.\n");
#endif
  gtk_sheet_delete_columns (sheet, mincol, 1); 
#ifdef DEBUG
  printf("In s_toplevel_delete_attrib_col, done deleting col in gtksheet.\n");
#endif
  
  return;
}


/*------------------------------------------------------------------
 *  This fcn is the edit->new attrib column callback from the menu bar.
 *  It throws up the "enter new attrib name" dialog box, then returns.
 *------------------------------------------------------------------*/
void s_toplevel_menubar_unimplemented_feature()
{
#ifdef DEBUG
  printf("In s_toplevel_menubar_unimplemented_feature, telling the user that feature is lacking\n");
#endif

  x_dialog_unimplemented_feature();  /* This creates dialog box  */
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
 * This fcn 
 * loops through all objects on (PAGE page)->(OBJECT *start_obj).
 * It takes the updated SHEET_DATA->TABLE data and then updates the 
 * objects with the new attribs & attrib values.
 * For each component, it updates the attached 
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
  STRING_LIST *new_comp_attrib_pair_list;
  STRING_LIST *old_comp_attrib_pair_list;
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
	/* Must create a name=value pair list for each particular component
	 * which we can pass to fcn updating o_current.  This fcn places all attribs
	 * found in the row into new_comp_attrib_pair_list.  */
	new_comp_attrib_pair_list = s_table_create_attrib_pair(temp_uref,
							       sheet_head->component_table, 
							       sheet_head->master_comp_list_head,
							       sheet_head->comp_attrib_count);


	/* Now update attribs in toplevel using this list.  */
	s_toplevel_update_component_attribs_in_toplevel(o_current, 
							new_comp_attrib_pair_list);

	free(temp_uref);
      } else {
#ifdef DEBUG
	printf("In s_toplevel_sheetdata_to_toplevel, found complex with no refdes. name = %s\n", 
	       o_current->name);
#endif
      }
    }  /* if (o_current->type == OBJ_COMPLEX) */


    o_current = o_current->next;
  }  /* while (o_current != NULL) */


#if 0
  /* -----  Next deal with all nets on the page.  ----- */
  /* This is TBD */

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
 * 1.  Form list of all component attribs held on both the component
 *     (o_current), as well as in the attrib list (SHEET_DATA).
 * 2.  Loop over name=value pairs held in complete_comp_attrib_list.
 * 3.  For each name=value pair, look for corresponding attrib on o_current.
 * 4.  For each name=value pair, look for the corresponding attrib in 
 *     new_comp_attrib_list.
 * 5.  If the attrib exists on o_current and in new_comp_attrib_list, write the 
 *     new value (from new_comp_attrib_list) into o_current.
 * 6.  If the attrib exists on o_current, but is null in name=value pair,
 *     delete the attrib from o_current.
 * 7.  If the attribs doesn't exist on o_current, but is non-null in
 *     the name=value pair, create an attrib object and add it to the part
 *     on o_current.
 *
 * Calling args:  OBJECT *o_current -- component (complex) to be updated.
 *                STRING_LIST *new_comp... -- list of name=value attribute pairs
 *                                            from SHEET_DATA.
 * Returns: Returns nothing because the changes are made in o_current, which
 *          is part of the global TOPLEVEL pr_current.
 *------------------------------------------------------------------*/
void s_toplevel_update_component_attribs_in_toplevel(OBJECT *o_current, 
						     STRING_LIST *new_comp_attrib_list) 
{
  STRING_LIST *local_list;
  STRING_LIST *complete_comp_attrib_list;
  char *old_name_value_pair;
  char *new_attrib_name;
  char *new_attrib_value;
  char *old_attrib_name;
  char *old_attrib_value;
  ATTRIB *a_current;
  int count = 0;  /* This is to fake out a fcn called later */

#if DEBUG
  printf("-----  Entering s_toplevel_update_component_attribs_in_toplevel.\n");
#endif

  /* To remove dead attribs from o_current, we need to form a complete list of attribs
   * by taking the union of the new attribs from the SHEET_DATA, and the old attribs
   * living on o_current.  That's what we're doing here.
   * Later, we can delete those attribs in o_current which don't apear in 
   * new_comp_attrib_list.
   */
  /* First duplicate new_comp_attrib_list */
  complete_comp_attrib_list = s_string_list_duplicate_string_list(new_comp_attrib_list);

  /* Next augment complete_attrib_list with the attribs attached to o_current */
  a_current = o_current->attribs;
  while (a_current != NULL) {
    if (a_current->object->type == OBJ_TEXT
	&& a_current->object->text != NULL) {  /* found a name=value attribute pair. */
      /* may need to check more thoroughly here. . . . */
      old_name_value_pair = u_basic_strdup(a_current->object->text->string);
      old_attrib_name = u_basic_breakup_string(old_name_value_pair, '=', 0);
      if (strcmp(old_attrib_name, "refdes") != 0) {
	s_string_list_add_item(complete_comp_attrib_list, &count, old_name_value_pair);
      }
      free(old_name_value_pair);
      free(old_attrib_name);
    }
    a_current = a_current->next;
  }  /* while (a_current != NULL) */


  /* Now the main business of this fcn:  updating the attribs attached to this o_current.
   * Loop on name=value pairs held in complete_comp_attrib_list , and then use this to get the
   * name=value pairs out of new_comp_attrib_list and old_comp_attrib_list.
   */

  /* First handle a special case: the component has no attribs (beside refdes). */
  if (complete_comp_attrib_list->data == NULL) 
    return;

  /* Now the normal case. . . . */
  local_list = complete_comp_attrib_list;
  while (local_list != NULL) {

#if DEBUG
  printf("        In s_toplevel_update_component_attribs_in_toplevel, handling entry in master list %s .\n", 
	 local_list->data);
#endif

    /*  Now get the name=value pair off of o_current  */
  old_attrib_name = u_basic_breakup_string(local_list->data, '=', 0);
  old_attrib_value = o_attrib_search_name_single_count(o_current, old_attrib_name, 0);
  
  /* Next try to get this attrib from the list of new attribs */
  new_attrib_name = u_basic_breakup_string(local_list->data, '=', 0);      
  if (s_string_list_in_list(new_comp_attrib_list, local_list->data)) {
    new_attrib_value = u_basic_breakup_string(local_list->data, '=', 1);      
  } else {
    new_attrib_value = NULL;
  }
  

#if DEBUG
      printf("In s_toplevel_update_component_attribs_in_toplevel, old_attrib_name = %s, old_attrib_value = %s\n",
	     old_attrib_name, old_attrib_value);
      printf("In s_toplevel_update_component_attribs_in_toplevel, new_attrib_name = %s, new_attrib_value = %s\n",
	     new_attrib_name, new_attrib_value);
#endif


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
	     old_attrib_name, old_attrib_value);
#endif
      s_object_remove_attrib_in_object(o_current, old_attrib_name);
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
    if (new_attrib_name != NULL) {
      free(new_attrib_name);  /* Be careful, this can be NULL */
    }
    if (new_attrib_value != NULL) {
      free(new_attrib_value);  /* Be careful, this can be NULL */
    }
    if (old_attrib_name != NULL) {
      free(old_attrib_name);  /* Be careful, this can be NULL */
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




