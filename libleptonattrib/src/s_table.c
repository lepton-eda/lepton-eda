/* Lepton EDA attribute editor
 * Copyright (C) 2003-2010 Stuart D. Brorson.
 * Copyright (C) 2003-2013 gEDA Contributors
 * Copyright (C) 2017-2026 Lepton EDA Contributors
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

/*! \brief Init attrib value associated with a cell.
 *
 *  \par Function Description
 *
 *  Sets initial attrib value associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 */
void
attrib_table_init_attrib_value (TABLE **table,
                                int i,
                                int j)
{
  table[i][j].attrib_value = NULL;
}


/*! \brief Init row name associated with a cell.
 *
 *  \par Function Description
 *
 *  Sets initial row name associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 */
void
attrib_table_init_row_name (TABLE **table,
                            int i,
                            int j)
{
  table[i][j].row_name = NULL;
}


/*! \brief Init column name associated with a cell.
 *
 *  \par Function Description
 *
 *  Sets initial column name associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 */
void
attrib_table_init_column_name (TABLE **table,
                               int i,
                               int j)
{
  table[i][j].col_name = NULL;
}


/*! \brief Create a new table row.
 *
 *  \par Function Description
 *
 *  Allocates and returns a new table row.
 *
 *  \param [in] columns The number of columns in the row.
 */
TABLE*
attrib_table_row_new (int columns)
{
  return (TABLE *) g_malloc (columns * sizeof(TABLE));
}


/*! \brief Get the contents of a table row.
 *
 *  \par Function Description
 *
 *  Returns the contents of a table row.
 *
 *  \param [in] table The table.
 *  \param [in] row The row number.
 */
TABLE*
attrib_table_get_row_contents (TABLE **table,
                               int row)
{
  return table[row];
}


/*! \brief Set the contents of a table row.
 *
 *  \par Function Description
 *
 *  Sets the contents of a table row.
 *
 *  \param [in] table The table.
 *  \param [in] row The row number.
 *  \param [in] contents The new row contents.
 */
void
attrib_table_set_row_contents (TABLE **table,
                               int row,
                               TABLE *contents)
{
  table[row] = contents;
}


/*! \brief Reallocate a table row.
 *
 *  \par Function Description
 *
 *  Reallocates a table row allocating memory for given number of
 *  columns.
 *
 *  \param [in] table_row The table row.
 *  \param [in] columns The number of columns.
 */
TABLE*
attrib_table_realloc_row (TABLE *table_row,
                          int columns)
{
  return (TABLE *) realloc (table_row, columns * sizeof (TABLE));
}


/*! \brief Create a new table.
 *
 *  \par Function Description
 *
 *  Allocates a new table with given number of rows.
 *
 *  \param [in] rows The number of rows.
 */
TABLE**
attrib_table_new (int rows)
{
  return (TABLE **) g_malloc (rows * sizeof (TABLE *));
}


/*! \brief Get attrib value associated with a cell.
 *
 *  \par Function Description
 *
 *  Returns attrib value associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *
 *  \return The attrib value.
 */
char*
attrib_table_get_attrib_value (TABLE **table,
                               int i,
                               int j)
{
  return table[i][j].attrib_value;
}


/*! \brief Set attrib value associated with a cell.
 *
 *  \par Function Description
 *
 *  Sets attrib value associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *  \param [in] val The new attrib value.
 */
void
attrib_table_set_attrib_value (TABLE **table,
                               int i,
                               int j,
                               char *val)
{
  g_free (table[i][j].attrib_value);

  table[i][j].attrib_value = (val == NULL) ? NULL : g_strdup (val);
}


/*! \brief Get column name associated with a cell.
 *
 *  \par Function Description
 *
 *  Returns column name associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *
 *  \return The column name.
 */
char*
attrib_table_get_column_name (TABLE **table,
                              int i,
                              int j)
{
  return table[i][j].col_name;
}


/*! \brief Set column name associated with a cell.
 *
 *  \par Function Description
 *
 *  Sets column name associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *  \param [in] val The new column name.
 */
void
attrib_table_set_column_name (TABLE **table,
                              int i,
                              int j,
                              char *val)
{
  g_free (table[i][j].col_name);

  table[i][j].col_name = (val == NULL) ? NULL : g_strdup (val);
}


/*! \brief Get row name associated with a cell.
 *
 *  \par Function Description
 *
 *  Returns row name associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The row number of the cell.
 *
 *  \return The row name.
 */
char*
attrib_table_get_row_name (TABLE **table,
                           int i,
                           int j)
{
  return table[i][j].row_name;
}


/*! \brief Set row name associated with a cell.
 *
 *  \par Function Description
 *
 *  Sets row name associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *  \param [in] val The new row name.
 */
void
attrib_table_set_row_name (TABLE **table,
                           int i,
                           int j,
                           char *val)
{
  g_free (table[i][j].row_name);

  table[i][j].row_name = (val == NULL) ? NULL : g_strdup (val);
}


/*! \brief Get attrib visibility associated with a cell.
 *
 *  \par Function Description
 *
 *  Returns attrib visibility associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *
 *  \return The visibility value.
 */
int
attrib_table_get_visibility (TABLE **table,
                             int i,
                             int j)
{
  return table[i][j].visibility;
}


/*! \brief Set attrib visibility associated with a cell.
 *
 *  \par Function Description
 *
 *  Set visiblity of an attrib associated with a table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *  \param [in] val The new visibility value.
 */
void
attrib_table_set_visibility (TABLE **table,
                             int i,
                             int j,
                             int val)
{
  table[i][j].visibility = val;
}


/*! \brief Get \a show_name_value attrib property associated with
 *  a cell.
 *
 *  \par Function Description
 *
 *  Returns \a show_name_value attrib property associated with a
 *  table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *
 *  \return The \a show_name_value value.
 */
int
attrib_table_get_show_name_value (TABLE **table,
                                  int i,
                                  int j)
{
  return table[i][j].show_name_value;
}


/*! \brief Set \a show_name_value attrib property associated with
 *  a cell.
 *
 *  \par Function Description
 *
 *  Sets \a show_name_value attrib property associated with a
 *  table cell.
 *
 *  \param [in] table The table.
 *  \param [in] i The row number of the cell.
 *  \param [in] j The column number of the cell.
 *  \param [in] val The new \a show_name_value value.
 */
void
attrib_table_set_show_name_value (TABLE **table,
                                  int i,
                                  int j,
                                  int val)
{
  table[i][j].show_name_value = val;
}
