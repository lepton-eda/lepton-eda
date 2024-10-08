/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2013 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

/*--------------------------------------------------------------*/
/*!
 * \file
 *
 * \brief Functions involved in manipulating an entire
 * SHEET_DATA structure.
 *
 * This file holds functions involved in manipulating an entire
 * SHEET_DATA structure.  The SHEET_DATA structure is the intermediate
 * structure between LeptonToplevel (gEDA's native format) and the graphical
 * gtksheet widget (from gtkextra), which is the spreadsheet widget
 * displaying the attribs.
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
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"
#include "../include/gettext.h"


int
s_sheet_data_changed (const SHEET_DATA* data)
{
  return data->CHANGED;
}


void
s_sheet_data_set_changed (SHEET_DATA* data, int changed)
{
  data->CHANGED = changed;

  x_window_set_title_changed (changed);

  if (!changed)
  {
    x_gtksheet_set_saved(); /* see comments in x_gtksheet_set_saved() */
  }
}


/*------------------------------------------------------------------*/
/*!
 * \brief Create a SHEET_DATA struct.
 *
 * Creates an initialised but empty SHEET_DATA struct.
 * \returns a pointer to a SHEET_DATA struct.
 */
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



/*------------------------------------------------------------------*/
/*! \brief Add components to master list
 *
 * Add to the master list of components refdeses by running through
 * the components and recording the comp refdeses it discovers. Then
 * it sorts them into alphabetical order.
 * \param[in] obj_list pointer to the component list to be added.
 */
void s_sheet_data_add_master_comp_list_items (const GList *obj_list) {
  char *temp_uref;
  const GList *iter;

  g_debug ("==== Enter s_sheet_data_add_master_comp_list_items()\n");

  if (verbose_mode) {
    printf (_("Start master component list creation.\n"));
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  for (iter = obj_list;
       iter != NULL;
       iter = g_list_next (iter)) {
    LeptonObject *o_current = (LeptonObject*) iter->data;

    g_debug ("s_sheet_data_add_master_comp_list_items: "
             "Examining o_current->name = %s\n", o_current->name);

      /*-----  only process if this is a component with attributes ----*/
    if (lepton_object_is_component (o_current) &&
        lepton_object_get_attribs (o_current) != NULL)
    {

        g_debug ("s_sheet_data_add_master_comp_list_items: "
                 "Found component on page: component basename = %s\n",
                 lepton_component_object_get_basename (o_current));
        verbose_print(" C");

        temp_uref = s_attrib_get_refdes(o_current);

        /* Now that we have refdes, store refdes and attach attrib list to component */
        if (temp_uref) {
          g_debug ("s_sheet_data_add_master_comp_list_items: "
                   "About to add to master list refdes = %s\n", temp_uref);
          s_string_list_add_item(sheet_head->master_comp_list_head,
                                  &(sheet_head->comp_count), temp_uref);
          g_free(temp_uref);
        }

      } /*  if (lepton_object_is_component (o_current) . . . . .) */

  }

  return;
}


/*------------------------------------------------------------------*/
/*! \brief Add attributes to master list
 *
 * Add to the master list of comp attributes by running
 * through each component on the page and recording all attribs
 * it discovers. Then it sorts them into an order used for the
 * horiz listing of the attribs on the spreadsheet.
 * Data struct being searched is:
 * sheet_head->component_list_head->attrib->name;
 * \param obj_list pointer to list of attributes being added
 */
void s_sheet_data_add_master_comp_attrib_list_items (const GList *obj_list) {
  char *attrib_text;
  char *attrib_name;
  const GList *o_iter;
  GList *a_iter;
  LeptonObject *a_current;

  g_debug ("==== Enter s_sheet_data_add_master_comp_attrib_list_items()\n");

  if (verbose_mode) {
    printf (_("Start master component attrib list creation.\n"));
  }

  /* -----  Iterate through all objects found on page looking for components (OBJ_COMPONENT) ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    LeptonObject *o_current = (LeptonObject*) o_iter->data;

    g_debug ("s_sheet_data_add_master_comp_attrib_list_items: "
             "Examining o_current->name = %s\n", o_current->name);

      /*-----  only process if this is a component with attributes ----*/
    if (lepton_object_is_component (o_current) &&
        lepton_object_get_attribs (o_current) != NULL)
    {

        verbose_print(" C");

        /*------ Iterate through all attribs found on component -----*/
        a_iter = lepton_object_get_attribs (o_current); /* This has a side effect.  Why? */
        while (a_iter != NULL) {
          a_current = (LeptonObject*) a_iter->data;
          if (lepton_object_is_text (a_current)
              && a_current->text != NULL) {  /* found an attribute */
            attrib_text = g_strdup (lepton_text_object_get_string (a_current));
            attrib_name = u_basic_breakup_string(attrib_text, '=', 0);

              /* Don't include "refdes" or "slot" because they form the row name */
              /* Also don't include "net" per bug found by Steve W. -- 4.3.2007, SDB */
            if ( (strcmp(attrib_name, "refdes") != 0) &&
                 (strcmp(attrib_name, "net") != 0) &&
                 (strcmp(attrib_name, "slot") != 0) ) {
              g_debug ("... from this component, "
                       "about to add to master comp attrib list attrib=%s\n",
                       attrib_name);
              s_string_list_add_item(sheet_head->master_comp_attrib_list_head,
                                     &(sheet_head->comp_attrib_count), attrib_name);
            }   /* if (strcmp(attrib_name, "refdes") != 0) */
            g_free(attrib_name);
            g_free(attrib_text);
          }
          a_iter = g_list_next (a_iter);
        }   /*  while  */

      }   /* if (lepton_object_is_component (o_current)) */

  }

  /* -----  Now sort component list into alphabetical order  ----- */

  return;
}



/*------------------------------------------------------------------*/
/*! \brief Add net names to master list.
 *
 * Build the master list of net names by running
 * through the individual cells and recording the net refdeses
 * it discovers.
 * It's currently empty, waiting for implementation of net
 * attributes.
 */
void s_sheet_data_add_master_net_list_items (const GList *obj_start) {
  return;
}


/*------------------------------------------------------------------*/
/*! \brief Add net attributes to master list.
 *
 * Build the master list of net attribs.
 * It's currently empty, waiting for implementation of net
 * attributes.
 */
void s_sheet_data_add_master_net_attrib_list_items (const GList *obj_start) {
  return;
}


/*------------------------------------------------------------------*/
/*! \brief Add pin names to master list.
 *
 * Build the master
 * list of pin names.  It writes the
 * label refdes:pinnumber into the global master pin list.
 * Algorithm:
 * -# Loop on objects looking for components.
 * -# When we find a component, save the refdes.
 * -# Dive down to primitives of the component.
 * -# Loop on the primitives looking for pins.
 * -# When we find a pin, find the pinnumber by calling
 *    lepton_attrib_search_object_attribs_by_name (o_lower_current, "pinnumber", 0)
 * -# Create the pin list label as "refdes=XXX", and stick it into
 *    the master pin list.
 * Since this function operates on the global sheet_data->master_pin_list,
 * it doesn't return a value.
 * \param obj_list pointer to list of pin names to be added.
 */
void s_sheet_data_add_master_pin_list_items (const GList *obj_list) {
  char *temp_uref;
  char *temp_pinnumber;
  char *row_label;
  const GList *o_iter;
  GList *o_lower_iter;

  g_debug ("==== Enter s_sheet_data_add_master_pin_list_items()\n");

  if (verbose_mode) {
    printf (_("Start master pin list creation.\n"));
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    LeptonObject *o_current = (LeptonObject*) o_iter->data;

    g_debug ("s_sheet_data_add_master_pin_list_items: "
             "Examining o_current->name = %s\n", o_current->name);

    if (lepton_object_is_component (o_current))
    {
      temp_uref = s_attrib_get_refdes (o_current);
      if (temp_uref != NULL) {      /* make sure object component has a refdes  */

        /* -----  Now iterate through lower level objects looking for pins.  ----- */
        for (o_lower_iter = lepton_component_object_get_contents (o_current);
             o_lower_iter != NULL;
             o_lower_iter = g_list_next (o_lower_iter)) {
          LeptonObject *o_lower_current = (LeptonObject*) o_lower_iter->data;
          g_debug ("s_sheet_data_add_master_pin_list_items: "
                   "Examining object name %s\n",
                   o_lower_current->name);
          if (lepton_object_is_pin (o_lower_current))
          {
            temp_pinnumber = lepton_attrib_search_object_attribs_by_name (o_lower_current, "pinnumber", 0);

            if (temp_pinnumber != NULL) {
              row_label = g_strconcat (temp_uref, ":", temp_pinnumber, NULL);
              g_debug ("s_sheet_data_add_master_pin_list_items: "
                       "About to add to master pin list row_label = %s\n",
                       row_label);
              s_string_list_add_item (sheet_head->master_pin_list_head, &(sheet_head->pin_count), row_label);

            } else {      /* didn't find pinnumber.  Report error to log. */
              fprintf (stderr, "s_sheet_data_add_master_pin_list_items: ");
              fprintf (stderr, _("Found component pin with no pinnumber: refdes = %1$s\n"),
                       temp_uref);
            }
            g_free (temp_pinnumber);

          }
        }

      } else {          /* didn't find refdes.  Report error to log. */
        g_debug ("s_sheet_data_add_master_pin_list_items: "
                 "Found component with no refdes: component basename = %s\n",
                 lepton_component_object_get_basename (o_current));
      }
      g_free (temp_uref);

    }  /*  if (lepton_object_is_component (o_current))  */
  }

  return;
}


/*------------------------------------------------------------------*/
/*! \brief Add pin attributes to master list.
 *
 * Build the master
 * list of pin attributes.  It writes
 * each attrib name into the master pin attrib list.
 * Algorithm:
 * -# Loop on objects, looking for components.
 * -# When we find a component, save the refdes.
 * -# Dive down to primitives of the component.
 * -# Loop on the primitives looking for pins.
 * -# When we find a pin, get its attribs.
 * -# Loop on the attribs looking for non-NULL text.
 * -# When we find a non-NULL text attrib, extract the attrib name
 *    and stick it in the master pin attrib list.
 * \param obj_list pointer to list of pin attributes to be added.
 */
void s_sheet_data_add_master_pin_attrib_list_items (const GList *obj_list) {
  char *temp_uref;
  char *attrib_text;
  char *attrib_name;
  char *attrib_value;
  const GList *o_iter;
  GList *o_lower_iter, *a_iter;
  LeptonObject *pin_attrib;

  g_debug ("==== Enter s_sheet_data_add_master_pin_attrib_list_items()\n");

  if (verbose_mode) {
    printf (_("Start master pin attrib list creation.\n"));
  }

  /* -----  Iterate through all objects found on page looking for components  ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    LeptonObject *o_current = (LeptonObject*) o_iter->data;

    g_debug ("s_sheet_data_add_master_pin_attrib_list_items: "
             "Examining o_current->name = %s\n",
             o_current->name);

    if (lepton_object_is_component (o_current))
    {
        temp_uref = s_attrib_get_refdes(o_current);
        if (temp_uref != NULL) {      /* make sure object component has a refdes  */

          /* -----  Now iterate through lower level objects looking for pins.  ----- */
          for (o_lower_iter = lepton_component_object_get_contents (o_current);
               o_lower_iter != NULL;
               o_lower_iter = g_list_next (o_lower_iter)) {
            LeptonObject *o_lower_current = (LeptonObject*) o_lower_iter->data;
            g_debug ("s_sheet_data_add_master_pin_attrib_list_items: "
                     "Examining component refdes = %s\n",
                     temp_uref);
            if (lepton_object_is_pin (o_lower_current))
            {
              /* -----  Found a pin.  Now get attrib head and loop on attribs.  ----- */
              a_iter = lepton_object_get_attribs (o_lower_current);
              while (a_iter != NULL) {
                pin_attrib = (LeptonObject*) a_iter->data;
                if (lepton_object_is_text (pin_attrib)
                    && pin_attrib->text != NULL) {  /* found an attribute */
                  attrib_text = g_strdup (lepton_text_object_get_string (pin_attrib));
                  attrib_name = u_basic_breakup_string(attrib_text, '=', 0);
                  attrib_value = s_misc_remaining_string(attrib_text, '=', 1);
                  if ( (strcmp(attrib_name, "pinnumber") != 0)
                       && (attrib_value != NULL) ) {
                    /* Don't include "pinnumber" because it is already in other master list.
                     * Also guard against pathalogical symbols which have non-attrib text inside pins. */

                    g_debug ("s_sheet_data_add_master_pin_attrib_list_items: "
                             "Found pin attrib = %s\n"
                             "Add it to master_pin_attrib_list.\n",
                             attrib_name);

                    s_string_list_add_item(sheet_head->master_pin_attrib_list_head,
                                           &(sheet_head->pin_attrib_count), attrib_name);
                  }   /* if (strcmp(attrib_name, "pinnumber") != 0) */
                  g_free(attrib_value);
                  g_free(attrib_name);
                  g_free(attrib_text);
                }
                a_iter = g_list_next (a_iter);
              }   /*   while (pin_attrib != NULL)  */
            }
          }

          g_free(temp_uref);
        }  /*  if (temp_uref != NULL )  */

      }  /* if (lepton_object_is_component (o_current))  */
  }
  return;

}




/*------------------------------------------------------------------*/
/*!
 * \brief Extract data from gtksheet
 *
 * This fcn extracts the attribs from the gtksheet widget
 * cells, and places them back into SHEET_DATA.  This is the
 * first step in saving out a project.  Right now I just invoke
 * s_table_gtksheet_to_table.  Do I need to do anything else here?
 */
void s_sheet_data_gtksheet_to_sheetdata() {
  s_table_gtksheet_to_all_tables();
  /* do I need to do anything else here? */

  return;
}
