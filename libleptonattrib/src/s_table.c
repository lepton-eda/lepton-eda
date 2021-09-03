/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2013 gEDA Contributors
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
 *  \brief Functions to manipulate the TABLE structure
 *
 * This file holds functions involved in manipulating the TABLE structure,
 * which is subsidiary to SHEET_DATA.  TABLE is a 2 dimensional array
 * of structs; each struct corresponds to the data about an element
 * in a single cell of the spreadsheet.
 * \todo TABLE should also store its dimensions in its own data
 *       structure to save carrying the dimensions around separately.
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

/* ===================  Public Functions  ====================== */


#ifdef DEBUG

/*! \brief Debug: print table's contents to STDOUT
 *
 * \param src   2-dimensional array of TABLE structures to print
 * \param rows  number of rows in src
 * \param cols  number of columns in src
 */
void
s_table_print (TABLE** src, int rows, int cols)
{
  printf( "\n\n ++ ++ s_table_print( rows: [%d] cols: [%d] )\n", rows, cols );

  for ( int j = 0; j < rows; j++ )
  {
    printf( "== row: [%d] name: [%s]\n", j, src[j][0].row_name );

    for ( int k = 0; k < cols; k++ )
    {
      printf( "  -- col: [%d] [%s] == [%s]\n", k,
                                               src[j][k].col_name,
                                               src[j][k].attrib_value );

      printf( "     ->row: [%d] ->col: [%d]\n", src[j][k].row,
                                                src[j][k].col );
      printf( "     ->vis: [%d] ->sho: [%d]\n", src[j][k].visibility,
                                                src[j][k].show_name_value );
    }
  }

  printf( "\n" );
}

#endif


/*------------------------------------------------------------------*/
/*! \brief Create a new table
 *
 * This is the table creator.  It returns a pointer to
 * an initialized TABLE struct.  As calling args, it needs
 * the number of rows and cols to allocate.  The table is a
 * dynamically allocated 2D array of structs.  To access data in
 * a cell in the table, you reference (for example):
 * ((sheet_data->comp_table)[i][j]).attrib_value
 * (Parens used only for clarity.  It works without parens.)
 * \param rows Number of rows required in the new table
 * \param cols Number of columns required in the new table
 * \returns a pointer to an initialized TABLE struct.
 */
TABLE **s_table_new(int rows, int cols)
{
  TABLE **new_table;
  int i, j;

  /* Here I am trying to create a 2 dimensional array of structs */
  new_table = (TABLE **) g_malloc(rows*sizeof(TABLE *));
  for (i = 0; i < rows; i++) {
    new_table[i] = (TABLE *) g_malloc(cols * sizeof(TABLE));
    /* Note that I should put some checks in here to verify that
     * malloc worked correctly. */
  }

  /* Now pre-load the table with NULLs */
  for (i = 0; i < rows; i++) {
    for (j = 0; j < cols; j++) {
      (new_table[i][j]).attrib_value = NULL;
      (new_table[i][j]).row_name = NULL;
      (new_table[i][j]).col_name = NULL;
      (new_table[i][j]).row = i;
      (new_table[i][j]).col = j;
      (new_table[i][j]).visibility = VISIBLE;
      (new_table[i][j]).show_name_value = SHOW_VALUE;
    }
  }

  return (new_table);

}


/*! \brief Make a copy of the \a src array
 *
 * \par Function Description
 * Returns a copy of the 2-dimensional array of the TABLE
 * structures \a src, excluding data in the culumn \a col_skip.
 * It's a helper function to be used in the "delete attrib
 * column" operation: s_toplevel_delete_attrib_col().
 * The resulting array has a dimensions of \a rows x \a cols - 1.
 *
 * \param src       an array of the TABLE structures to copy
 * \param col_skip  index of the column to skip (0-based)
 * \param rows      number of rows in \a src
 * \param cols      number of columns in \a src
 *
 * \return          a copy of \a src, minus data in the \a col_skip column
 */
TABLE**
s_table_copy (TABLE** src, int col_skip, int rows, int cols)
{
  g_return_val_if_fail (src != NULL, NULL);
  g_return_val_if_fail (col_skip < cols, NULL);

  TABLE** dst = s_table_new (rows, cols - 1);
  g_return_val_if_fail (dst != NULL, NULL);

  for (int j = 0; j < rows; j++)
  {
    int K = 0;

    for (int k = 0; k < cols - 1; k++)
    {
      if (k == col_skip)
      {
        K ++ ;
      }

      dst[j][k].row             = j;
      dst[j][k].col             = k;
      dst[j][k].visibility      = src[j][ K ].visibility;
      dst[j][k].show_name_value = src[j][ K ].show_name_value;

      dst[j][k].row_name     = g_strdup( src[j][ K ].row_name );
      dst[j][k].col_name     = g_strdup( src[j][ K ].col_name );
      dst[j][k].attrib_value = g_strdup( src[j][ K ].attrib_value );

      K ++ ;

    } /* for: columns */

  } /* for: rows */

  return dst;

} /* s_table_copy() */


/*------------------------------------------------------------------*/
/*! \brief Resize a TABLE
 *
 * This function recreates the table with
 * a new size.  It can only increase
 * the number of cols.  You can't increase the number of rows since
 * gattrib doesn't allow you to input new components.  Decreasing the
 * number of cols is also TBD.
 * \param table Table to resize
 * \param rows Number of rows in the table
 * \param old_cols Number of columns previously in the table
 * \param new_cols Number of columns required in the table
 * \returns a pointer to the resized table
 * \todo The row and column information could be stored in the
 *       TABLE struct.
 */
TABLE **s_table_resize(TABLE **table,
                       int rows, int old_cols, int new_cols)
{
  int i, j;

  /* Here I am trying to resize the 2 dimensional array of structs */
  for (i = 0; i < rows; i++) {
    table[i] = (TABLE *) realloc(table[i], new_cols*sizeof(TABLE) );
    if (table[i] == NULL) exit(-1);  /* die if failed to realloc new memory */
  }

  /* Now pre-load new cols with NULLs */
  for (i = 0; i < rows; i++) {
    for (j = old_cols; j < new_cols; j++) {
      (table[i][j]).attrib_value = NULL;
      (table[i][j]).row_name = NULL;
      (table[i][j]).col_name = NULL;
      (table[i][j]).row = i;
      (table[i][j]).col = j;
      (table[i][j]).visibility = VISIBLE;
      (table[i][j]).show_name_value = SHOW_VALUE;
    }
  }

  return table;
}


/*------------------------------------------------------------------*/
/*! \brief Destroy a table
 *
 * This function destroys the old table.
 * Use it after reading in a new
 * page to get rid of the old table before building a new one.
 * \param table Table to destroy
 * \param row_count Number of rows in table
 * \param col_count Number of columns in table
 */
void s_table_destroy(TABLE **table, int row_count, int col_count)
{
  int i, j;

  if (table == NULL)
    return;

  for (i = 0; i < row_count; i++) {
    for (j = 0; j < col_count; j++) {
      g_free( (table[i][j]).attrib_value );
      g_free( (table[i][j]).row_name );
      g_free( (table[i][j]).col_name );
    }
  }

  for (i = 0; i < row_count; i++) {
    g_free( table[i] );
  }

  g_free(table);
  table = NULL;

  return;
}



/*------------------------------------------------------------------*/
/*! \brief Get a string index number
 *
 * This function returns the index number
 * when given a STRING_LIST and a
 * string to match.  It finds the index
 * number by iterating through the master  list.
 * \param local_list
 * \param local_string
 * \returns the index of the string
 */
int s_table_get_index(STRING_LIST *local_list, char *local_string) {
  int count = 0;
  STRING_LIST *list_element;

  g_debug ("s_table_get_index: "
           "Examining %s to see if it is in the list.\n",
           local_string);

  list_element = local_list;
  while (list_element != NULL) {
    if (strcmp(list_element->data, local_string) == 0) {
      return count;
    }
    count++;
    list_element = list_element->next;
  }
  return(-1);  /* return code when string is not in master_list  */
}



/*------------------------------------------------------------------*/
/*! \brief Create attribute pair
 *
 * This function takes a table, a row list, and a row name,
 * and returns a list holding
 * name=value pairs for all attribs pertainent to that particular
 * row.
 * If the row holds no attribs, it just returns NULL.
 *
 * \param row_name Name of the row to search for
 * \param table Table to be searched
 * \param row_list list of rows
 * \param num_attribs
 * \returns STRING_LIST of name=value pairs
 */
STRING_LIST *s_table_create_attrib_pair(gchar *row_name,
                                        TABLE **table,
                                        STRING_LIST *row_list,
                                        int num_attribs)
{
  STRING_LIST *attrib_pair_list;
  char *attrib_name, *attrib_value, *name_value_pair;
  int row, col;
  int count = 0;

  attrib_pair_list = s_string_list_new();

  row = s_table_get_index(row_list, row_name);
  /* Sanity check */
  if (row == -1) {
    /* we didn't find the item in the list */
    fprintf (stderr, "s_table_create_attrib_pair: ");
    fprintf (stderr, _("We didn't find the row name in the row list!\n"));
    return attrib_pair_list;
  }

  for (col = 0; col < num_attribs; col++) {
    /* pull attrib from table.  If non-null, add it to attrib_pair_list  */
    if ( (table[row][col]).attrib_value != NULL) {
      attrib_name = (table[row][col]).col_name;
      attrib_value = (table[row][col]).attrib_value;
      name_value_pair = g_strconcat(attrib_name, "=", attrib_value, NULL);
      s_string_list_add_item(attrib_pair_list, &count, name_value_pair);
      g_free(name_value_pair);
    }
  }

  return attrib_pair_list;
}




/*------------------------------------------------------------------*/
/*! \brief Add components to the component table
 *
 * This fcn iterates over adds all
 * objects found on this page looking
 * for components.  When it finds a component, it finds all component
 * attribs and sticks them in the TABLE.
 * \param obj_list pointer to GList containing objects on this page
 */
void s_table_add_toplevel_comp_items_to_comp_table (const GList *obj_list) {
  gchar *temp_uref;
  int row, col;
  gchar *attrib_text;
  gchar *attrib_name;
  gchar *attrib_value;
  const GList *o_iter;
  GList *a_iter;
  LeptonObject *a_current;
  gint old_visibility, old_show_name_value;


  if (verbose_mode) {
    printf (_("Start internal component TABLE creation\n"));
  }

  g_debug ("==== Enter s_table_add_toplevel_comp_items_to_comp_table()\n");

  /* -----  Iterate through all objects found on page  ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    LeptonObject *o_current = (LeptonObject*) o_iter->data;

    g_debug ("s_table_add_toplevel_comp_items_to_comp_table: "
             "Examining o_current->name = %s\n",
             o_current->name);

    /* -----  Now process objects found on page  ----- */
    if (lepton_object_is_component (o_current) &&
        lepton_object_get_attribs (o_current) != NULL)
    {

      /* ---- Don't process part if it lacks a refdes ----- */
      temp_uref = g_strdup(s_attrib_get_refdes(o_current));
      if (temp_uref) {

        g_debug ("s_table_add_toplevel_comp_items_to_comp_table: "
                 "Found component on page. Refdes = %s\n",
                 temp_uref);
        verbose_print(" C");

        /* Having found a component, we loop over all attribs in this
         * component, and stick them
         * into cells in the table. */
        a_iter = lepton_object_get_attribs (o_current);
        while (a_iter != NULL) {
          a_current = (LeptonObject*) a_iter->data;
          if (lepton_object_is_text (a_current)
              && a_current->text != NULL) {  /* found an attribute */
            /* may need to check more thoroughly here. . . . */
            attrib_text = g_strdup (lepton_text_object_get_string (a_current));
            attrib_name = u_basic_breakup_string(attrib_text, '=', 0);
            attrib_value = s_misc_remaining_string(attrib_text, '=', 1);
            old_visibility = lepton_text_object_is_visible (a_current)
              ? VISIBLE : INVISIBLE;
            old_show_name_value = lepton_text_object_get_show (a_current);

            /* Don't include "refdes" or "slot" because they form the row name. */
            /* Also don't include "net" per bug found by Steve W.  4.3.2007 -- SDB */
            if ( (strcmp(attrib_name, "refdes") != 0) &&
                 (strcmp(attrib_name, "net") != 0) &&
                 (strcmp(attrib_name, "slot") != 0) ) {

              /* Get row and col where to put this attrib */
              row = s_table_get_index(sheet_head->master_comp_list_head, temp_uref);
              col = s_table_get_index(sheet_head->master_comp_attrib_list_head, attrib_name);
              /* Sanity check */
              if (row == -1 || col == -1) {
                /* we didn't find the item in the table */
                fprintf (stderr, "s_table_add_toplevel_comp_items_to_comp_table: ");
                fprintf (stderr, _("We didn't find either row or col in the lists!\n"));
              } else {

                g_debug ("s_table_add_toplevel_comp_items_to_comp_table: "
                         "About to add row %d, col %d, attrib_value = %s\n"
                         "    Current address of attrib_value cell is [%p]\n",
                         row, col, attrib_value,
                         &((sheet_head->component_table)[row][col]).attrib_value);
                /* Is there a compelling reason for me to put this into a separate fcn? */
                ((sheet_head->component_table)[row][col]).row = row;
                ((sheet_head->component_table)[row][col]).col = col;
                ((sheet_head->component_table)[row][col]).row_name = g_strdup(temp_uref);
                ((sheet_head->component_table)[row][col]).col_name = g_strdup(attrib_name);
                ((sheet_head->component_table)[row][col]).attrib_value = g_strdup(attrib_value);
                ((sheet_head->component_table)[row][col]).visibility = old_visibility;
                ((sheet_head->component_table)[row][col]).show_name_value = old_show_name_value;
              }
            }
            g_free(attrib_name);
            g_free(attrib_text);
            g_free(attrib_value);
          }
          a_iter = g_list_next (a_iter);

        }  /* while (a_current != NULL) */
        g_free(temp_uref);
      }  /* if (temp_uref) */
    }    /* if (lepton_object_is_component (o_current))  */
  }

  verbose_done();

}

#if 0
/*------------------------------------------------------------------*/
/*! \brief Add nets to net table
 *
 * This function iterates over adds all
 * items found on this page looking
 * for nets and adds them individually to the net table.  Looping over
 * objects occurs here.
 *
 * \param start_obj Pointer to first object
 *
 * \todo Why do the calling semantics of this function disagree with
 *       s_table_add_toplevel_pin_items_to_pin_table()?  That function
 *       takes a GList, this one takes a pointer to LeptonObject.
 */
void s_table_add_toplevel_net_items_to_net_table(LeptonObject *start_obj) {
  LeptonObject *o_current;
  char *temp_netname;
  int row, col;
  char *attrib_text;
  char *attrib_name;
  char *attrib_value;
  ATTRIB *a_current;

  /* -----  Iterate through all objects found on page  ----- */
  o_current = start_obj;
  while (o_current != NULL) {

    /* -----  Now process objects found on page  ----- */
    if (lepton_object_is_net (o_current)) {
      g_debug ("s_table_add_toplevel_net_items_to_net_table: "
               "Found net on page.\n");
      verbose_print(" N");

      /* Having found a net, we stick it into the table. */
      a_current = o_current->attribs;
      while (a_current != NULL) {
        if (lepton_object_is_text (a_current->object)
            && a_current->object->text != NULL) {  /* found an attribute */
          /* may need to check more thoroughly here. . . . */
          attrib_text = g_strdup (lepton_text_object_get_string (a_current->object));
          attrib_name = u_basic_breakup_string(attrib_text, '=', 0);
          attrib_value = s_misc_remaining_string(attrib_text, '=', 1);
          if (strcmp(attrib_name, "netname") != 0) {
            /* Don't include "netname" */

            /* Get row and col where to put this attrib */
            row = s_table_get_index(sheet_head->master_net_list_head, temp_netname);
            col = s_table_get_index(sheet_head->master_net_attrib_list_head, attrib_name);
            g_debug ("s_table_add_toplevel_net_items_to_net_table: "
                     "About to add row %d, col %d, attrib_value = %s\n"
                     "    Current address of attrib_value cell is [%p]\n",
                     row, col, attrib_value,
                     &((sheet_head->net_table)[row][col]).attrib_value);
            /* Is there a compelling reason for me to put this into a separate fcn? */
            ((sheet_head->net_table)[row][col]).row = row;
            ((sheet_head->net_table)[row][col]).col = col;
            ((sheet_head->net_table)[row][col]).row_name = g_strdup(temp_netname);
            ((sheet_head->net_table)[row][col]).col_name = g_strdup(attrib_name);
            ((sheet_head->net_table)[row][col]).attrib_value = g_strdup(attrib_value);
          }
          g_free(attrib_name);
          g_free(attrib_text);
          g_free(attrib_value);
        }
        a_current = a_current->next;

      }  /* while (a_current != NULL) */
      g_free(temp_netname);

    }    /*--- if (lepton_object_is_net (o_current))   ---*/


    o_current = o_current->next;  /* iterate to next object on page */
  }  /* while o_current != NULL */

  verbose_done();

  g_debug ("s_table_add_toplevel_net_items_to_net_table: Return.\n");
}
#endif


/*------------------------------------------------------------------*/
/*! \brief Add pins to pin table.
 *
 * This function iterates over adds all items found on this page
 * looking for pins.  WHen it finds a pin, it gathers all
 * pin attribs and sticks them into the pin table.
 * \param obj_list List of objects on page
 */
void s_table_add_toplevel_pin_items_to_pin_table (const GList *obj_list) {
  gchar *temp_uref;
  gchar *pinnumber;
  gchar *row_label;
  int row, col;
  gchar *attrib_text;
  gchar *attrib_name;
  gchar *attrib_value;
  const GList *o_iter;
  GList *a_iter;
  GList *o_lower_iter;
  LeptonObject *pin_attrib;

  if (verbose_mode) {
    printf (_("Start internal pin TABLE creation\n"));
  }

  g_debug ("==== Enter s_table_add_toplevel_pin_items_to_pin_table()\n");

  /* -----  Iterate through all objects found on page  ----- */
  for (o_iter = obj_list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    LeptonObject *o_current = (LeptonObject*) o_iter->data;

    g_debug ("s_table_add_toplevel_pin_items_to_pin_table: "
             "Examining o_current->name = %s\n",
             o_current->name);

    /* -----  Now process objects found on page  ----- */
    if (lepton_object_is_component (o_current) &&
        o_current->attribs != NULL) {

      /* ---- Don't process part if it lacks a refdes ----- */
      temp_uref = s_attrib_get_refdes(o_current);
      if (temp_uref) {

        /* -----  Now iterate through lower level objects looking for pins.  ----- */
        for (o_lower_iter = lepton_component_object_get_contents (o_current);
             o_lower_iter != NULL;
             o_lower_iter = g_list_next (o_lower_iter)) {
          LeptonObject *o_lower_current = (LeptonObject*) o_lower_iter->data;

          if (lepton_object_is_pin (o_lower_current))
          {
            /* -----  Found a pin.  First get its pinnumber.  then get attrib head and loop on attribs.  ----- */
            pinnumber = o_attrib_search_object_attribs_by_name (o_lower_current, "pinnumber", 0);
            row_label = g_strconcat(temp_uref, ":", pinnumber, NULL);

            g_debug ("s_table_add_toplevel_pin_items_to_pin_table: "
                     "Examining pin %s\n",
                     row_label);

            a_iter = o_lower_current->attribs;
            while (a_iter != NULL) {
              pin_attrib = (LeptonObject*) a_iter->data;
              if (lepton_object_is_text (pin_attrib)
                  && pin_attrib->text != NULL) {  /* found an attribute */
          attrib_text = g_strdup (lepton_text_object_get_string (pin_attrib));
                attrib_name = u_basic_breakup_string(attrib_text, '=', 0);
                attrib_value = s_misc_remaining_string(attrib_text, '=', 1);

                if ( (strcmp(attrib_name, "pinnumber") != 0)
                     && (attrib_value != 0) ) {
                  /* Don't include "pinnumber" because it is already in other master list.
                   * Also must ensure that value is non-null; certain symbols are not well formed.
                   */

                  /* Get row and col where to put this attrib */
                  row = s_table_get_index(sheet_head->master_pin_list_head, row_label);
                  col = s_table_get_index(sheet_head->master_pin_attrib_list_head, attrib_name);
                  /* Sanity check */
                  if (row == -1 || col == -1) {
                    /* we didn't find the item in the table */
                    fprintf (stderr, "s_table_add_toplevel_pin_items_to_pin_table: ");
                    fprintf (stderr, _("We didn't find either row or col in the lists!\n"));
                  } else {

                    g_debug ("s_table_add_toplevel_pin_items_to_pin_table: "
                             "About to add row %d, col %d, attrib_value = %s\n"
                             "    Current address of attrib_value cell is [%p]\n",
                             row, col, attrib_value,
                             &((sheet_head->component_table)[row][col]).attrib_value);
                    /* Is there a compelling reason for me to put this into a separate fcn? */
                    ((sheet_head->pin_table)[row][col]).row = row;
                    ((sheet_head->pin_table)[row][col]).col = col;
                    ((sheet_head->pin_table)[row][col]).row_name = g_strdup(row_label);
                    ((sheet_head->pin_table)[row][col]).col_name = g_strdup(attrib_name);
                    ((sheet_head->pin_table)[row][col]).attrib_value = g_strdup(attrib_value);
                  }
                }
                g_free(attrib_name);
                g_free(attrib_text);
                g_free(attrib_value);
              }
              a_iter = g_list_next (a_iter);

            }  /* while (pin_attrib != NULL) */
            g_free(pinnumber);
            g_free(row_label);
          }

        }
      }

      g_free(temp_uref);
    }

  }

  verbose_done();
}


/*------------------------------------------------------------------*/
/*! \brief Push spreadsheet data to TABLEs.
 *
 * This function traverses the spreadsheet,
 * extracts the attribs from
 * the cells, and places them back into TABLE.  This is the
 * first step in saving out a project.
 */
void s_table_gtksheet_to_all_tables() {

  int num_rows;
  int num_cols;
  STRING_LIST *master_row_list;
  STRING_LIST *master_col_list;
  TABLE **local_table;
  GtkSheet *local_gtk_sheet;

  /* First handle component sheet */
  num_rows = sheet_head->comp_count;
  num_cols = sheet_head->comp_attrib_count;
  local_gtk_sheet = sheets[0];
  master_row_list = sheet_head->master_comp_list_head;
  master_col_list = sheet_head->master_comp_attrib_list_head;

  local_table = sheet_head->component_table;

  /* now fill out new table */
  g_debug ("s_table_gtksheet_to_all_tables: "
           "Now about to fill out new component table.\n");
  s_table_gtksheet_to_table(local_gtk_sheet, master_row_list,
                       master_col_list, local_table,
                       num_rows, num_cols);

#if 0
  /* Next handle net sheet */
  num_rows = sheet_head->net_count;
  num_cols = sheet_head->net_attrib_count;
  local_gtk_sheet = sheets[1];
  master_row_list = sheet_head->master_net_list_head;
  master_col_list = sheet_head->master_net_attrib_list_head;
  local_table = sheet_head->net_table;

  s_table_gtksheet_to_table(local_gtk_sheet, master_row_list,
                       master_col_list, local_table,
                       num_rows, num_cols);
#endif

#ifdef UNIMPLEMENTED_FEATURES
  /* Finally, handle component pin sheet */
  num_rows = sheet_head->pin_count;
  num_cols = sheet_head->pin_attrib_count;
  local_gtk_sheet = sheets[2];
  master_row_list = sheet_head->master_pin_list_head;
  master_col_list = sheet_head->master_pin_attrib_list_head;
  /*  local_table = s_table_new(num_rows, num_cols);  */
  local_table = sheet_head->pin_table;

  s_table_gtksheet_to_table(local_gtk_sheet, master_row_list,
                       master_col_list, local_table,
                       num_rows, num_cols);
#endif

  return;
}


/* ===================  Private Functions  ====================== */
/*------------------------------------------------------------------*/
/*! \brief Extract attributes from gtksheet into TABLE
 *
 * This function does the actual heavy lifting of looping
 * through the spreadsheet, extracting the attribs from
 * the cells, and placing them back into TABLE.  This is the
 * first step in saving out a project.
 *
 * \param local_gtk_sheet GtkSheet to save
 * \param master_row_list STRING_LIST of rows
 * \param master_col_list STRING_LIST of columns
 * \param local_table TABLE structure to fill
 * \param num_rows Number of rows in table
 * \param num_cols Number of columns in table
 */
void s_table_gtksheet_to_table(GtkSheet *local_gtk_sheet, STRING_LIST *master_row_list,
                         STRING_LIST *master_col_list, TABLE **local_table,
                         int num_rows, int num_cols)
{
  int row, col;

  STRING_LIST *row_list_item;
  gchar *row_title;

  STRING_LIST *col_list_item;
  gchar *col_title;

  gchar *attrib_value;

  g_debug ("==== Enter s_table_gtksheet_to_table()\n");

  row_list_item = master_row_list;
  for (row = 0; row < num_rows; row++) {
    row_title = (gchar *) g_strdup(row_list_item->data);

    col_list_item = master_col_list;
    for (col = 0; col < num_cols; col++) {
      col_title = (gchar *) g_strdup(col_list_item->data);

      /* get value of attrib in cell  */
      attrib_value = (gchar *) gtk_sheet_cell_get_text(GTK_SHEET(local_gtk_sheet), row, col);

#if 0
      if (strlen(attrib_value) == 0) {
        /* g_free(attrib_value);  */   /* sometimes we have spurious, zero length strings creep */
        attrib_value = NULL;    /* into the GtkSheet                                     */
      }
#endif

      g_debug ("s_table_gtksheet_to_table: "
               "Found attrib_value = %s in cell row=%d, col=%d\n",
               attrib_value, row, col);

      /* first handle attrib value in cell */
      g_debug ("    Updating attrib_value %s\n", attrib_value);
      g_free( local_table[row][col].attrib_value );
      if (attrib_value != NULL) {
        local_table[row][col].attrib_value = (gchar *) g_strdup(attrib_value);
      } else {
        local_table[row][col].attrib_value = NULL;
      }

      /* next handle name of row (also held in TABLE cell) */
      g_debug ("    Updating row_name %s\n", row_title);
      g_free( local_table[row][col].row_name );
      if (row_title != NULL) {
        local_table[row][col].row_name = (gchar *) g_strdup(row_title);
      } else {
        local_table[row][col].row_name = NULL;
      }

      /* finally handle name of col */
      g_debug ("    Updating col_name %s\n", col_title);
      g_free( local_table[row][col].col_name );
      if (col_title != NULL) {
        local_table[row][col].col_name = (gchar *) g_strdup(col_title);
      } else {
        local_table[row][col].col_name = NULL;
      }

      /* get next col list item and then iterate. */
      col_list_item = col_list_item->next;
    }

    row_list_item = row_list_item->next;
  }

  return;
}
