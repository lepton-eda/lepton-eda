/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2014 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

/*------------------------------------------------------------------*/
/*! \file
 *  \brief Functions to manipulate the LeptonToplevel struct.
 *
 * This file holds functions involved in manipulating the LeptonToplevel data
 * structure.  LeptonToplevel is the data structure inherited from gEDA's
 * other programs, and holds all info about a project in a form
 * native to gEDA.
 */


#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <liblepton/liblepton.h>
#include <liblepton/libleptonguile.h>
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


/* ===================  Public Functions  ====================== */

/*! \brief Verify the entire design
 *
 * This function loops through all objects in the design looking
 * for missing components, that is, those components for which no
 * corresponding symbol files was found.
 *
 * \param toplevel The #LeptonToplevel object to be verified.
 */
void s_toplevel_verify_design (LeptonToplevel *toplevel)
{
  GList *p_iter;
  const GList *o_iter;

  int missing_sym_flag = 0;

  for (p_iter = lepton_list_get_glist (toplevel->pages);
       p_iter != NULL;
       p_iter = g_list_next (p_iter)) {
    LeptonPage *p_current = (LeptonPage*) p_iter->data;

    for (o_iter = s_page_objects (p_current);
         o_iter != NULL;
         o_iter = g_list_next (o_iter)) {
      LeptonObject *o_current = (LeptonObject*) o_iter->data;

      /* --- look for object, and verify that it has a symbol file attached. ---- */
      if (lepton_object_is_component (o_current) &&
          lepton_component_object_get_missing (o_current))
      {
        missing_sym_flag = 1;  /* flag to signal that problem exists.  */
      }
    }
  }

  if (missing_sym_flag) {
    x_dialog_missing_sym();  /* dialog gives user option to quit */
  }
}


/*! \brief Saves all the pages of a LeptonToplevel object.
 *  \par Function Description
 *  Saves all the pages in the <B>toplevel</B> parameter.
 *
 *  \param [in] toplevel  The LeptonToplevel to save pages from.
 *  \return The number of failed tries to save a page.
 */
gint
s_page_save_all (LeptonToplevel *toplevel)
{
  const GList *iter;
  LeptonPage *p_current;
  gint status = 0;

  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (LeptonPage *)iter->data;

    if (f_save (p_current, lepton_page_get_filename (p_current), NULL))
    {
      g_message (_("Saved [%1$s]"),
                 lepton_page_get_filename (p_current));
      /* reset the CHANGED flag of p_current */
      p_current->CHANGED = 0;

    } else {
      g_message (_("Could NOT save [%1$s]"),
                 lepton_page_get_filename (p_current));
      /* increase the error counter */
      status++;
    }

  }

  return status;
}


/*------------------------------------------------------------------*/
/*! \brief Copy data from gtksheet into LeptonToplevel struct
 *
 * Called when the user invokes "save".  It first
 * places all data from gtksheet into SHEET_DATA.  Then it
 * loops through all pages and saves them.
 */
void
s_toplevel_save_sheet ()
{
  GList *iter;
  LeptonPage *p_current;

  g_debug ("==== Enter s_toplevel_gtksheet_to_toplevel()\n");

  LeptonToplevel *toplevel = x_window_get_toplevel ();

  g_return_if_fail (toplevel != NULL);

  s_sheet_data_gtksheet_to_sheetdata();  /* read data from gtksheet into SHEET_DATA */
  g_debug ("s_toplevel_gtksheet_to_toplevel: "
           "Done writing stuff from gtksheet into SHEET_DATA.\n");

  /* must iterate over all pages in design */
  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (LeptonPage *)iter->data;
    /* only traverse pages which are toplevel */
    if (p_current->page_control == 0) {
      s_toplevel_sheetdata_to_toplevel (toplevel, p_current);    /* adds all objects from page */
    }
  }

  g_debug ("s_toplevel_gtksheet_to_toplevel: "
           "Done writing SHEEET_DATA text back into pr_currnet.\n");

  /* Save all pages in design. */
  s_page_save_all (toplevel);
  s_sheet_data_set_changed (sheet_head, FALSE);

  return;
}


/*------------------------------------------------------------------*/
/*! \brief Add a new attribute to the top level
 *
 *  This function gets called when the user has entered a new attrib name,
 *  and clicked the OK button.  It does this:
 *  -# It figures out which attrib/sheet is being added to
 *  -# It destroys the old table in preparation for the new attrib.
 *  -# It adds the new attrib to the master lists.
 *  -# It creates a new table with the new attrib.
 *  -# It then adds the appropriate col to the gtksheet.
 * \param new_attrib_name attribute to be added
 */
void s_toplevel_add_new_attrib(gchar *new_attrib_name) {
  gint cur_page;  /* current page in notbook  */
  gint old_comp_attrib_count;
  gint new_index;

  if (strcmp(new_attrib_name, N_("_cancel")) == 0) {
    return;  /* user pressed cancel or closed window with no value in entry */
  }

  /* Next must figure out which sheet the attrib belongs to. */
  cur_page = gtk_notebook_get_current_page(GTK_NOTEBOOK(notebook));
  g_debug ("s_toplevel_add_new_attrib: "
           "Adding new attrib to page %d.\n",
           cur_page);

  switch (cur_page) {
  case 0:  /* component attribute  */

    /*  Eventually, I want to just resize the table to accomodate the
     *  new attrib.  However, that is difficult.  Therefore, I will just
     *  destroy the old table and recreate it for now. */

    /*
    s_table_destroy(sheet_head->component_table,
                    sheet_head->comp_count, sheet_head->comp_attrib_count);
    */
    old_comp_attrib_count = sheet_head->comp_attrib_count;
    g_debug ("s_toplevel_add_new_attrib: "
             "Before adding new comp attrib: comp_attrib_count = %d\n",
             old_comp_attrib_count);

    s_string_list_add_item(sheet_head->master_comp_attrib_list_head,
                           &(sheet_head->comp_attrib_count),
                           new_attrib_name);
    s_string_list_sort_master_comp_attrib_list();

    /* Now, determine what index the new attrib ended up at
     * This is necessary to tell gtk_sheet_insert_columns
     * where the data should be shifted                    */
    new_index = s_string_list_find_in_list(sheet_head->master_comp_attrib_list_head,
                                           (char*)new_attrib_name);

    g_debug ("s_toplevel_add_new_attrib: "
            "Updated comp_attrib string list: new comp_attrib_count = %d\n",
            sheet_head->comp_attrib_count);

    /* Now create new table */
    /*     sheet_head->component_table = s_table_new(sheet_head->comp_count,
                                              sheet_head->comp_attrib_count);
    */

    /* resize table to accomodate new attrib col */
    sheet_head->component_table =
      s_table_resize(sheet_head->component_table,
                     sheet_head->comp_count,
                     old_comp_attrib_count, sheet_head->comp_attrib_count);

    g_debug ("s_toplevel_add_new_attrib: Resized component table.\n");

    /* Fill out new sheet with new stuff from gtksheet */
    gtk_sheet_insert_columns(GTK_SHEET(sheets[0]), new_index, 1);
    x_gtksheet_add_col_labels(GTK_SHEET(sheets[0]),
                              sheet_head->comp_attrib_count,
                              sheet_head->master_comp_attrib_list_head);

    g_debug ("s_toplevel_add_new_attrib: Updated gtksheet.\n");

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


/*------------------------------------------------------------------*/
/*! \brief Delete an attribute column
 *
 *  This function gets called when the user has selected a single attrib
 *  column, selected the edit->delete attrib item from the pull-down
 *  menu, and then said "yes" to the confirm dialog.
 */
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

  g_debug ("s_toplevel_delete_attrib_col: "
           "Checks were OK, now do real work\n");

  TABLE** table_new = NULL;

  /*  Rebuild the gattrib-specific data structures  */
  switch (cur_page) {

  case 0:  /* component attribute  */

    /*  Eventually, I want to just resize the table after deleting the
     *  attrib.  However, that is difficult.  Therefore, I will just
     *  destroy the old table and recreate it for now. */

    /*  Get name (label) of the col to delete from the gtk sheet */
    attrib_name = g_strdup( gtk_sheet_column_button_get_label(sheet, mincol) );

    if (attrib_name != NULL) {
      g_debug ("s_toplevel_delete_attrib_col: Attrib to delete = %s\n",
               attrib_name);
    } else {
      fprintf (stderr, "s_toplevel_delete_attrib_col: ");
      fprintf (stderr, _("Can't get attrib name\n"));
      return;
    }

    /* Make a copy of the TABLE array, minus data in col to delete:
    */
    table_new = s_table_copy (sheet_head->component_table,
                              mincol,
                              sheet_head->comp_count,
                              sheet_head->comp_attrib_count);

    /* Destroy the current TABLE array:
    */
    s_table_destroy(sheet_head->component_table,
        sheet_head->comp_count, sheet_head->comp_attrib_count);

    g_debug ("s_toplevel_delete_attrib_col: "
            "Before deleting comp attrib: comp_attrib_count = %d\n",
            sheet_head->comp_attrib_count);

    s_string_list_delete_item(&(sheet_head->master_comp_attrib_list_head),
                              &(sheet_head->comp_attrib_count),
                              attrib_name);
    s_string_list_sort_master_comp_attrib_list(); /* this renumbers list also */

    g_free(attrib_name);

    g_debug ("s_toplevel_delete_attrib_col: "
            "Just updated comp_attrib string list: new comp_attrib_count = %d\n",
            sheet_head->comp_attrib_count);

    /* Use the copy made above as the current TABLE array:
    */
    sheet_head->component_table = table_new;

    g_debug ("s_toplevel_delete_attrib_col: Updated SHEET_DATA info.\n");
    break;

  case 1:  /* net attribute  */
    /* insert into net attribute list  */
    break;

  case 2:  /* pin attribute  */
    /* insert into pin attribute list  */
    break;
  }  /* switch  */


  /* Delete col on gtksheet  */
  g_debug ("s_toplevel_delete_attrib_col: About to delete col in gtksheet.\n");
  gtk_sheet_delete_columns (sheet, mincol, 1);
  g_debug ("s_toplevel_delete_attrib_col: Done deleting col in gtksheet.\n");

  /* Set changed flag so user is prompted when exiting */
  s_sheet_data_set_changed (sheet_head, TRUE);

  return;
}


/* =======================  Private functions  ====================== */

/*------------------------------------------------------------------*/
/*! \brief Copy SHEET_DATA content to TOP_LEVEL
 *
 * This function
 * loops through all objects on (LeptonPage page)->(LeptonObject *start_obj).
 * It takes the updated SHEET_DATA->TABLE data and then updates the
 * objects with the new attribs & attrib values.
 * For each component, it updates the attached
 * attrib values using the updated values held in the SHEET_DATA->TABLE
 * structure.  It does so in three steps:
 * -# First find and update component attribs.
 * -# Then find and update net attribs.
 * -# Finally find and update pin attribs.
 * \param toplevel LeptonToplevel structure
 * \param page schematic page to copy
 */
void
s_toplevel_sheetdata_to_toplevel (LeptonToplevel *toplevel,
                                  LeptonPage *page)
{
  GList *copy_list;
  GList *o_iter, *prim_iter;
  char *temp_uref;
  STRING_LIST *new_comp_attrib_pair_list;
  STRING_LIST *new_pin_attrib_list;

  /* -----  First deal with all components on the page.  ----- */
  g_debug ("s_toplevel_sheetdata_to_toplevel: Handling components\n");

  /* Work from a copy list, as objects can be deleted
   * from the list during iteration over the list.
   */
  /* NB: g_list_copy doesn't declare its input const, so we cast */
  copy_list = g_list_copy ((GList *)s_page_objects (page));

  /* Iterate backwards since attributes are attached after their
   * parent objects in the list. Attributes can get deleted during
   * the iteration.
   */
  for (o_iter = g_list_last (copy_list);
       o_iter != NULL;
       o_iter = g_list_previous (o_iter)) {
    LeptonObject *o_current = (LeptonObject*) o_iter->data;

    /* ------- Object is a component.  Handle component attributes. ------- */
    if (lepton_object_is_component (o_current)) /* Note that OBJ_COMPONENT = component + attribs */
    {

#if 0
      if (o_attrib_search_object_attribs_by_name (o_current, "graphical", 0)) {
        break;  /* Ignore graphical components */
      }
#endif

      temp_uref = s_attrib_get_refdes(o_current);
      if (temp_uref != NULL) {
        /* Must create a name=value pair list for each particular component
         * which we can pass to function updating o_current.  This function
         * places all attribs
         * found in the row into new_comp_attrib_pair_list.  */
        new_comp_attrib_pair_list = s_table_create_attrib_pair(temp_uref,
                                                               sheet_head->component_table,
                                                               sheet_head->master_comp_list_head,
                                                               sheet_head->comp_attrib_count);


        /* Now update attribs in toplevel using this list.  */
        s_toplevel_update_component_attribs_in_toplevel(toplevel,
                                                        o_current,
                                                        new_comp_attrib_pair_list);

        g_free(temp_uref);
      } else {
        g_debug ("s_toplevel_sheetdata_to_toplevel: "
                 "Found component with no refdes. name = %s\n",
                 o_current->name);
      }
    }  /* if (lepton_object_is_component (o_current)) */

  }

  g_list_free (copy_list);

#if 0
  /* -----  Next deal with all nets on the page.  ----- */
  /* This is TBD */

#endif


  /* -----  Finally deal with all pins on the page.  ----- */
  /* -----  Next deal with all nets on the page.  ----- */
  g_debug ("s_toplevel_sheetdata_to_toplevel: Handling pins\n");

  /* Work from a copy list in case objects are
   * deleted from the list during its iteration.
   */
  /* NB: g_list_copy doesn't declare its input const, so we cast */
  copy_list = g_list_copy ((GList *)s_page_objects (page));

  for (o_iter = g_list_last (copy_list);
       o_iter != NULL;
       o_iter = g_list_previous (o_iter)) {
    LeptonObject *o_current = (LeptonObject*) o_iter->data;

    /* ------- Object is a component.  Handle pins by looking ------ */
    /* ------- for all pins attached to a component.        ------ */
    if (lepton_object_is_component (o_current))
    {
      /*  Upon finding a component, here's what to do:
       *  0.  Get refdes of component.
       *  1.  Loop over primitive objects, looking for pins.
       *  2.  When a pin is found, create refdes:pinnumber pair
       *      used in searching TABLE.
       *  3.  Search TABLE using refdes:pinnumber as key, and get list of
       *      attribs corresponding to this refdes:pinnumber
       *  4.  Stick the attribs into the LeptonToplevel data structure.
       */
      temp_uref =  s_attrib_get_refdes(o_current);
      /* Make sure object component has a refdes. */
      if (temp_uref != NULL)
      {
        for (prim_iter = lepton_component_object_get_contents (o_current);
             prim_iter != NULL;
             prim_iter = g_list_next (prim_iter)) {
          LeptonObject *comp_prim_obj = (LeptonObject*) prim_iter->data;

          if (lepton_object_is_pin (comp_prim_obj))
          {
            new_pin_attrib_list =
              s_toplevel_get_pin_attribs_in_sheet (temp_uref, comp_prim_obj);
           s_toplevel_update_pin_attribs_in_toplevel (toplevel,
                                                      temp_uref,
                                                      comp_prim_obj,
                                                      new_pin_attrib_list);
          }
        }
      }     /* if(temp_uref  */

      g_free(temp_uref);
    }
  }

  g_list_free (copy_list);

  return;
}


/*------------------------------------------------------------------*/
/*! \brief Get the component attributes from the top level
 *
 * This function returns a list of attributes attached to obj_name = comp
 * refdes or netlist.
 * \param refdes component refdes to return values from
 * \returns a STRING_LIST where the data field holds a name=value string.
 */
STRING_LIST *s_toplevel_get_component_attribs_in_sheet(char *refdes)
{
  STRING_LIST *new_attrib_list;
  STRING_LIST *local_attrib_list;
  int i;
  int row = -1;
  int count = 0;
  char *name_value_pair;
  char *new_attrib_value;
  char *new_attrib_name;

  g_debug ("==== Enter s_toplevel_get_component_attribs_in_sheet()\n");

  /* First find pos of this refdes in the master list */
  row = s_table_get_index(sheet_head->master_comp_list_head, refdes);

  /* Sanity check */
  if (row == -1) {
    /* we didn't find the item in the list */
    fprintf (stderr, "s_toplevel_get_component_attribs_in_sheet: ");
    fprintf (stderr, _("We didn't find the refdes in the master list.\n"));
    return NULL;
  }

  /* Now get all attribs associated with this refdes (in TABLE, indexed
   * by position), and insert them into new_attrib_list.  */
  new_attrib_list = s_string_list_new();  /* init new_attrib_list */

  i = 0;
  local_attrib_list = sheet_head->master_comp_attrib_list_head;
  while (local_attrib_list != NULL) {  /* iterate over all possible attribs */
    new_attrib_name = g_strdup(local_attrib_list->data);  /* take attrib name from column headings */

    if ( ((sheet_head->component_table)[row][i]).attrib_value ) {
      new_attrib_value = g_strdup( ((sheet_head->component_table)[row][i]).attrib_value );
      name_value_pair = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
      g_free(new_attrib_value);
    } else {
      name_value_pair = g_strconcat(new_attrib_name, "=", NULL);  /* empty attrib */
    }
    s_string_list_add_item(new_attrib_list, &count, name_value_pair);  /* add name=value to new list */
    g_free(new_attrib_name);
    g_free(name_value_pair);

    /* Sanity check */
    if (count != i+1) {
      /* for some reason, we have lost a name_value_pair somewhere . . .  */
      fprintf (stderr, "s_toplevel_get_component_attribs_in_sheet: ");
      fprintf (stderr, "count != i.\n");
      exit(-1);
    }

    /* iterate */
    i++;
    local_attrib_list = local_attrib_list->next;
  } /* while (local_attrib_list != NULL)  */

  return new_attrib_list;
}



/*------------------------------------------------------------------*/
/*! \brief Update component attributes in TOP_LEVEL
 *
 * For each attrib string attached to the component, update it using the value
 * held in new_comp_attrib_list.  Algorithm:
 * -# Form list of all component attribs held on both the component
 *    (o_current), as well as in the attrib list (SHEET_DATA).
 * -# Loop over name=value pairs held in complete_comp_attrib_list.
 * -# For each name=value pair, look for corresponding attrib on o_current.
 * -# For each name=value pair, look for the corresponding attrib in
 *    new_comp_attrib_list.
 * -# If the attrib exists on o_current and in new_comp_attrib_list, write the
 *    new value (from new_comp_attrib_list) into o_current.
 * -# If the attrib exists on o_current, but is null in name=value pair,
 *    delete the attrib from o_current.
 * -# If the attribs doesn't exist on o_current, but is non-null in
 *    the name=value pair, create an attrib object and add it to the part
 *    on o_current.
 * \param toplevel LeptonToplevel structure
 * \param o_current Component to be updated.
 * \param new_comp_attrib_list list of name=value attribute pairs
 *                             from SHEET_DATA.
 */
void
s_toplevel_update_component_attribs_in_toplevel (LeptonToplevel *toplevel,
                                                 LeptonObject *o_current,
                                                 STRING_LIST *new_comp_attrib_list)
{
  STRING_LIST *local_list;
  STRING_LIST *complete_comp_attrib_list;
  char *old_name_value_pair;
  char *new_attrib_name;
  char *new_attrib_value;
  char *old_attrib_name;
  char *old_attrib_value;
  gchar *refdes;
  GList *a_iter;
  LeptonObject *a_current;
  int count = 0;  /* This is to fake out a function called later */
  gint row, col;
  gint visibility = 0;
  gint show_name_value = 0;

  g_return_if_fail (o_current != NULL);

  g_debug ("==== Enter s_toplevel_update_component_attribs_in_toplevel()\n");

  /*
   * To remove dead attribs from o_current, we need to form a complete list of unique
   * attribs by taking the union of the new attribs from the SHEET_DATA, and
   * the old attribs living on o_current.  That's what we're doing here.
   * Later, we can delete those attribs in o_current which don't apear in
   * new_comp_attrib_list.
   */
  /* First duplicate new_comp_attrib_list */
  complete_comp_attrib_list = s_string_list_duplicate_string_list(new_comp_attrib_list);

  /* Now create a complete list of unique attribute names.  This will be used in
  *  the loop below when updating attributes.  */
  a_iter = lepton_object_get_attribs (o_current);
  while (a_iter != NULL) {
    a_current = (LeptonObject*) a_iter->data;
    if (lepton_object_is_text (a_current)
        && a_current->text != NULL) {  /* found a name=value attribute pair. */
      /* may need to check more thoroughly here. . . . */
      old_name_value_pair = g_strdup (lepton_text_object_get_string (a_current));

      /* Else clause is suggestion from Ales */
#if 1
      old_attrib_name = u_basic_breakup_string(old_name_value_pair, '=', 0);
      if ( (strcmp(old_attrib_name, "refdes") != 0) &&
           (strcmp(old_attrib_name, "net") != 0) &&
           (strcmp(old_attrib_name, "slot") != 0) &&
           (s_attrib_name_in_list(new_comp_attrib_list, old_attrib_name) == FALSE) ) {
        s_string_list_add_item(complete_comp_attrib_list, &count, old_name_value_pair);
      }
#else
      /* might now compile now, but this #if'd out branch isn't being built */
      gint status;
      old_attrib_name = g_strdup (lepton_text_object_get_name (a_current));
      if (old_attrib_name != NULL)
      {
        /* Don't put "refdes" or "slot" into list.  Don't put old name=value pair into list if a new
         * one is already in there. */
        if ( (strcmp(old_attrib_name, "refdes") != 0) &&
             (strcmp(old_attrib_name, "net") != 0) &&
             (strcmp(old_attrib_name, "slot") != 0) &&
             (s_attrib_name_in_list(new_comp_attrib_list, old_attrib_name) == FALSE) ) {
          s_string_list_add_item(complete_comp_attrib_list, &count, old_name_value_pair);
        }
        g_free (old_attrib_name);
      }
 #endif
     g_free(old_name_value_pair);
     g_free(old_attrib_name);
    }
    a_iter = g_list_next (a_iter);
  }  /* while (a_current != NULL) */


  /*
   *Now the main business of this function:  updating the attribs attached to this o_current.
   * Loop on name=value pairs held in complete_comp_attrib_list , and then use this to get the
   * name=value pairs out of new_comp_attrib_list and from o_current.
   */

  /* First handle a special case: the component has no attribs (beside refdes). */
  if (complete_comp_attrib_list->data == NULL)
    return;

  /* Now the normal case. . . . */
  local_list = complete_comp_attrib_list;
  while (local_list != NULL) {

    g_debug ("s_toplevel_update_component_attribs_in_toplevel: "
             "Handling entry in complete list %s.\n",
             local_list->data);

  /*  Now get the old attrib name & value from complete_comp_attrib_list
   *  and value from o_current  */
  old_attrib_name = u_basic_breakup_string(local_list->data, '=', 0);
  old_attrib_value = o_attrib_search_attached_attribs_by_name (o_current, old_attrib_name, 0);

  g_debug ("s_toplevel_update_component_attribs_in_toplevel: "
           "Old name = \"%s\".\n"
           "Old value = \"%s\".\n",
           old_attrib_name,
           old_attrib_value);

  /*  Next try to get this attrib from new_comp_attrib_list  */
  new_attrib_name = u_basic_breakup_string(local_list->data, '=', 0);
  if (s_string_list_in_list(new_comp_attrib_list, local_list->data)) {
    new_attrib_value = s_misc_remaining_string(local_list->data, '=', 1);
  } else {
    new_attrib_value = NULL;
  }
  g_debug ("s_toplevel_update_component_attribs_in_toplevel: "
           "New name = \"%s\".\n"
           "New value = \"%s\".\n",
           new_attrib_name,
           new_attrib_value);

  /* Now get row and col where this new attrib lives.  Then get
   * visibility of the new attrib stored in the component table */
  /* We'll need this later */
  refdes = g_strdup(s_attrib_get_refdes(o_current));
  row = s_table_get_index(sheet_head->master_comp_list_head, refdes);
  col = s_table_get_index(sheet_head->master_comp_attrib_list_head, new_attrib_name);
  /* if attribute has been deleted from the sheet, here is where we detect that */
  if ( (row == -1) || (col == -1) ) {
    new_attrib_value = NULL;  /* attrib will be deleted below */
  } else { /* we need a better place to get this info since the TABLE can be out of date */
    visibility = sheet_head->component_table[row][col].visibility;
    show_name_value = sheet_head->component_table[row][col].show_name_value;
  }
  g_free(refdes);


    /* -------  Four cases to consider: Case 1 ----- */
    if ( (old_attrib_value != NULL) && (new_attrib_value != NULL) && (strlen(new_attrib_value) != 0) ) {
      /* simply write new attrib into place of old one. */
      g_debug ("s_toplevel_update_component_attribs_in_toplevel: "
               "About to replace old attrib with name= %s, value= %s\n"
               "    visibility = %d, show_name_value = %d.\n",
               new_attrib_name, new_attrib_value,
               visibility, show_name_value);
      s_object_replace_attrib_in_object (o_current,
                                         new_attrib_name,
                                         new_attrib_value,
                                         visibility,
                                         show_name_value);
    }

    /* -------  Four cases to consider: Case 2 ----- */
    else if ( (old_attrib_value != NULL) && (new_attrib_value == NULL) ) {
      /* remove attrib from component*/
      g_debug ("s_toplevel_update_component_attribs_in_toplevel: "
               "About to remove old attrib with name= %s, value= %s\n",
               old_attrib_name,
               old_attrib_value);
      s_object_remove_attrib_in_object (toplevel, o_current, old_attrib_name);
    }

    /* -------  Four cases to consider: Case 3 ----- */
    else if ( (old_attrib_value == NULL) && (new_attrib_value != NULL) ) {
      /* add new attrib to component. */

      g_debug ("s_toplevel_update_component_attribs_in_toplevel: "
               "About to add new attrib with name= %s, value= %s\n",
               new_attrib_name,
               new_attrib_value);

      s_object_add_comp_attrib_to_object (toplevel,
                                          o_current,
                                          new_attrib_name,
                                          new_attrib_value,
                                          visibility,
                                          show_name_value);

      /* -------  Four cases to consider: Case 4 ----- */
    } else {
      /* Do nothing. */
      g_debug ("s_toplevel_update_component_attribs_in_toplevel: "
               "Nothing needs to be done.\n");
    }

    /* Toggle attribute visibility and name/value setting */


    /* free everything and iterate */
    g_free(new_attrib_name);
    g_free(new_attrib_value);
    g_free(old_attrib_name);
    g_free(old_attrib_value);
    local_list = local_list->next;
  }   /*   while (local_list != NULL)  */
  return;
}


/*------------------------------------------------------------------*/
/*!
 * \todo Function doesn't do anything - candidate for removal?
 */
STRING_LIST *s_toplevel_get_net_attribs_in_sheet(char *netname)
{
  /* must be filled in */
  return NULL;
}


/*------------------------------------------------------------------*/
/*!
 * \todo Function doesn't do anything - candidate for removal?
 */
void s_toplevel_update_net_attribs_in_toplevel(LeptonObject *o_current,
                                   STRING_LIST *new_net_attrib_list)
{
  /* must be filled in */
  return;
}


/*------------------------------------------------------------------*/
/*! \brief Get pin attributes
 *
 * This function takes a pointer to the LeptonObject pin, and
 * returns a list of attribs found attached to the pin.  The
 * returned list is a STRING_LIST where the ->data holds a
 * name=value string.
 * The algorithm is as follows:
 * -# Form refdes:pinnumber label for this pin.
 * -# Get row number of this refdes:pinnumber
 * -# Create a list of name=value pairs from entries in the pin_table
 *    on this row.
 * -# Return list of name=value pairs found.
 *
 * \param refdes Ref des string
 * \param pin Pin object
 * \returns name=value pair as a STRING_LIST
 */
STRING_LIST *s_toplevel_get_pin_attribs_in_sheet(char *refdes, LeptonObject *pin)
{
  STRING_LIST *new_attrib_list;
  STRING_LIST *local_attrib_list;
  int i;
  int row = -1;
  int count = 0;
  char *pinnumber;
  char *row_label;
  char *name_value_pair;
  char *new_attrib_value;
  char *new_attrib_name;

  g_debug ("==== Enter s_toplevel_get_pin_attribs_in_sheet()\n");

  /* First find pos of this pin in the master pin list */
  /* first convert refdes, pin to refdes:pinno text string. Then call table_get_index.  */

  pinnumber = o_attrib_search_object_attribs_by_name (pin, "pinnumber", 0);

  if ( (refdes != NULL) && (pinnumber != NULL) ) {
    row_label = g_strconcat(refdes, ":", pinnumber, NULL);
  } else {
    fprintf (stderr, "s_toplevel_get_pin_attribs_in_sheet: ");
    fprintf (stderr, _("Either refdes or pinnumber of object missing.\n"));
    return NULL;
  }
  row = s_table_get_index(sheet_head->master_pin_list_head, row_label);

  /* Sanity check */
  if (row == -1) {
    /* we didn't find the item in the list */
    fprintf (stderr, "s_toplevel_get_pin_attribs_in_sheet: ");
    fprintf (stderr, _("We didn't find the refdes:pin in the master list.\n"));
    return NULL;
  }

  /* Now get all attribs associated with this refdes (in TABLE, indexed
   * by position), and insert them into new_attrib_list.  */
  new_attrib_list = s_string_list_new();  /* init new_attrib_list */

  i = 0;
  local_attrib_list = sheet_head->master_pin_attrib_list_head;
  while (local_attrib_list != NULL) {  /* iterate over all possible attribs */
    new_attrib_name = g_strdup(local_attrib_list->data);  /* take attrib name from column headings */

    if ( ((sheet_head->pin_table)[row][i]).attrib_value ) {
      new_attrib_value = g_strdup( ((sheet_head->pin_table)[row][i]).attrib_value );
      name_value_pair = g_strconcat(new_attrib_name, "=", new_attrib_value, NULL);
      g_free(new_attrib_value);
    } else {
      name_value_pair = g_strconcat(new_attrib_name, "=", NULL);  /* empty attrib */
    }
    s_string_list_add_item(new_attrib_list, &count, name_value_pair);  /* add name=value to new list */
    g_free(new_attrib_name);
    g_free(name_value_pair);

    /* Sanity check */
    if (count != i+1) {
      /* for some reason, we have lost a name_value_pair somewhere . . .  */
      fprintf (stderr, "s_toplevel_get_pin_attribs_in_sheet: ");
      fprintf (stderr, "count != i.\n");
      exit(-1);
    }

    /* iterate */
    i++;
    local_attrib_list = local_attrib_list->next;
  } /* while (local_attrib_list != NULL)  */

  return new_attrib_list;
}



/*------------------------------------------------------------------*/
/*! \brief Update pin attributes in toplevel
 *
 * For each attrib string attached to the pin, update it using the value
 * held in new_pin_attrib_list.  Algorithm:
 * -# Loop over name=value pairs held in new_pin_attrib_list.
 * -# For each name=value pair, look for corresponding attrib on pin.
 * -# If the attrib exists on pin and in name=value pair, write the
 *    new value in.
 * -# If the attrib exists on pin, but is null in name=value pair,
 *    delete the attrib.
 * -# If the attribs doesn't exist on pin, but is non-null in
 *    the name=value pair, create an attrib object and add it to the pin.
 * \param toplevel LeptonToplevel structure
 * \param refdes Unused - needs refactored out
 * \param [in,out] o_pin pin to update
 * \param [in] new_pin_attrib_list New pin attribute list to apply
 */
void
s_toplevel_update_pin_attribs_in_toplevel (LeptonToplevel *toplevel,
                                           char *refdes,
                                           LeptonObject *o_pin,
                                           STRING_LIST *new_pin_attrib_list)
{
  STRING_LIST *local_list;
  char *new_name_value_pair;
  char *new_attrib_name;
  char *new_attrib_value;
  char *old_attrib_value;

  g_return_if_fail (o_pin != NULL);

  g_debug ("==== Enter s_toplevel_update_pin_attribs_in_toplevel()\n");

  /* loop on name=value pairs held in new_pin_attrib_list */
  local_list = new_pin_attrib_list;
  while (local_list != NULL) {
    new_name_value_pair = g_strdup(local_list->data);
    g_debug ("s_toplevel_update_pin_attribs_in_toplevel: "
             "Handling entry in master list %s.\n",
             new_name_value_pair);

  new_attrib_name = u_basic_breakup_string(new_name_value_pair, '=', 0);
  new_attrib_value = u_basic_breakup_string(new_name_value_pair, '=', 1);

  if (strlen(new_attrib_value) == 0) {
    g_free(new_attrib_value);
    new_attrib_value = NULL;  /* s_misc_remaining_string doesn't return NULL for empty substring. */
  }
  old_attrib_value = o_attrib_search_attached_attribs_by_name (o_pin, new_attrib_name, 0);

    /* -------  Four cases to consider: Case 1: old and new attribs exist ----- */
    if ( (old_attrib_value != NULL) && (new_attrib_value != NULL) && (strlen(new_attrib_value) != 0) ) {
      /* simply write new attrib into place of old one. */
      g_debug ("s_toplevel_update_pin_attribs_in_toplevel: "
               "About to replace old attrib with new one: name= %s, value= %s\n",
               new_attrib_name,
               new_attrib_value);
      s_object_replace_attrib_in_object (o_pin,
                                         new_attrib_name,
                                         new_attrib_value,
                                         LEAVE_VISIBILITY_ALONE,
                                         LEAVE_NAME_VALUE_ALONE);
    }

    /* -------  Four cases to consider: Case 2: old attrib exists, new one doesn't ----- */
    else if ( (old_attrib_value != NULL) && (new_attrib_value == NULL) ) {
      /* remove attrib from pin */
      g_debug ("s_toplevel_update_pin_attribs_in_toplevel: "
               "About to remove old attrib with name= %s, value= %s\n",
               new_attrib_name,
               old_attrib_value);
      s_object_remove_attrib_in_object (toplevel, o_pin, new_attrib_name);
    }

    /* -------  Four cases to consider: Case 3: No old attrib, new one exists. ----- */
    else if ( (old_attrib_value == NULL) && (new_attrib_value != NULL) ) {
      /* add new attrib to pin. */

      g_debug ("s_toplevel_update_pin_attribs_in_toplevel: "
               "About to add new attrib with name= %s, value= %s\n",
               new_attrib_name,
               new_attrib_value);

      s_object_add_pin_attrib_to_object (toplevel,
                                         o_pin,
                                         new_attrib_name,
                                         new_attrib_value);

      /* -------  Four cases to consider: Case 4 ----- */
    } else {
      /* Do nothing. */
      g_debug ("s_toplevel_update_pin_attribs_in_toplevel: "
               "Nothing needs to be done.\n");
    }

    /* free everything and iterate */
    g_free(new_name_value_pair);
    g_free(new_attrib_name);
    g_free(new_attrib_value);
    g_free(old_attrib_value);
    local_list = local_list->next;
  }   /*   while (local_list != NULL)  */

  return;
}
