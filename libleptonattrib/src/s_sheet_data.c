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


/*! \brief Get the component list of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the component list of given sheet data structure.
 *
 *  \param [in] data The sheet data.
 *
 *  \return The component list.
 */
STRING_LIST*
attrib_sheet_data_get_component_list (SHEET_DATA *data)
{
  return data->master_comp_list_head;
}


/*! \brief Set the component list of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the component list of given sheet data instance to \p
 *  list.
 *
 *  \param [in] data The sheet data.
 *  \param [in] list The component list.
 */
void
attrib_sheet_data_set_component_list (SHEET_DATA *data,
                                      STRING_LIST *list)
{
  data->master_comp_list_head = list;
}


/*! \brief Get the component attribute list of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the component attribute list of given sheet data
 *  structure.
 *
 *  \param [in] data The sheet data.
 *
 *  \return The component attribute list.
 */
STRING_LIST*
attrib_sheet_data_get_component_attrib_list (SHEET_DATA *data)
{
  return data->master_comp_attrib_list_head;
}


/*! \brief Set the component attribute list of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the component attribute list of given sheet data instance
 *  to \p list.
 *
 *  \param [in] data The sheet data.
 *  \param [in] list The component attribute list.
 */
void
attrib_sheet_data_set_component_attrib_list (SHEET_DATA *data,
                                             STRING_LIST *list)
{
  data->master_comp_attrib_list_head = list;
}


/*! \brief Get the address of component attrib list of sheet
 *  data.
 *
 *  \par Function Description
 *
 *  Returns the address of component attrib list of given sheet
 *  data instance.
 *
 *  \return The address.
 */
STRING_LIST**
attrib_sheet_data_get_component_attrib_list_address (SHEET_DATA *data)
{
  return &(data->master_comp_attrib_list_head);
}


/*! \brief Get the net list of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the net list of given sheet data structure.
 *
 *  \param [in] data The sheet data.
 *
 *  \return The net list.
 */
STRING_LIST*
attrib_sheet_data_get_net_list (SHEET_DATA *data)
{
  return data->master_net_list_head;
}


/*! \brief Set the net list of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the net list of given sheet data instance to \p list.
 *
 *  \param [in] data The sheet data.
 *  \param [in] list The net list.
 */
void
attrib_sheet_data_set_net_list (SHEET_DATA *data,
                                STRING_LIST *list)
{
  data->master_net_list_head = list;
}


/*! \brief Get the net attribute list of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the net attribute list of given sheet data structure.
 *
 *  \param [in] data The sheet data.
 *
 *  \return The net attribute list.
 */
STRING_LIST*
attrib_sheet_data_get_net_attrib_list (SHEET_DATA *data)
{
  return data->master_net_attrib_list_head;
}


/*! \brief Set the net attribute list of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the net attribute list of given sheet data instance to \p
 *  list.
 *
 *  \param [in] data The sheet data.
 *  \param [in] list The net attribute list.
 */
void
attrib_sheet_data_set_net_attrib_list (SHEET_DATA *data,
                                       STRING_LIST *list)
{
  data->master_net_attrib_list_head = list;
}


/*! \brief Get the pin list of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the pin list of given sheet data structure.
 *
 *  \param [in] data The sheet data.
 *
 *  \return The pin list.
 */
STRING_LIST*
attrib_sheet_data_get_pin_list (SHEET_DATA *data)
{
  return data->master_pin_list_head;
}


/*! \brief Set the pin list of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the pin list of given sheet data instance to \p list.
 *
 *  \param [in] data The sheet data.
 *  \param [in] list The pin list.
 */
void
attrib_sheet_data_set_pin_list (SHEET_DATA *data,
                                STRING_LIST *list)
{
  data->master_pin_list_head = list;
}


/*! \brief Get the pin attribute list of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the pin attribute list of given sheet data structure.
 *
 *  \param [in] data The sheet data.
 *
 *  \return The pin attribute list.
 */
STRING_LIST*
attrib_sheet_data_get_pin_attrib_list (SHEET_DATA *data)
{
  return data->master_pin_attrib_list_head;
}


/*! \brief Set the pin attribute list of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the pin attribute list of given sheet data instance to \p
 *  list.
 *
 *  \param [in] data The sheet data.
 *  \param [in] list The pin attribute list.
 */
void
attrib_sheet_data_set_pin_attrib_list (SHEET_DATA *data,
                                       STRING_LIST *list)
{
  data->master_pin_attrib_list_head = list;
}


/*! \brief Get the component table of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the component table of given sheet data structure.
 *
 *  \return The component table.
 */
TABLE**
attrib_sheet_data_get_component_table (SHEET_DATA *data)
{
  return data->component_table;
}


/*! \brief Set the component table of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the component table of a sheet data structure to the
 *  given value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] table The new value of the component table.
 */
void
attrib_sheet_data_set_component_table (SHEET_DATA *data,
                                       TABLE** table)
{
  data->component_table = table;
}


/*! \brief Get the count of components of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the count of components of given sheet data structure.
 *
 *  \return The count.
 */
int
attrib_sheet_data_get_component_count (SHEET_DATA *data)
{
  return data->comp_count;
}


/*! \brief Set the count of components of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the count of components of a sheet data structure to the
 *  given value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] count The new count of components.
 */
void
attrib_sheet_data_set_component_count (SHEET_DATA *data,
                                       int count)
{
  data->comp_count = count;
}


/*! \brief Get the address of component counter of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the address of component counter of given sheet data
 *  structure.
 *
 *  \return The address.
 */
int*
attrib_sheet_data_get_component_counter_address (SHEET_DATA *data)
{
  return &(data->comp_count);
}


/*! \brief Get the count of component attribs of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the count of component attribs of given sheet data
 *  structure.
 *
 *  \return The count.
 */
int
attrib_sheet_data_get_component_attrib_count (SHEET_DATA *data)
{
  return data->comp_attrib_count;
}


/*! \brief Set the count of component attribs of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the count of component attribs of a sheet data structure
 *  to the given value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] count The new count of component attribs.
 */
void
attrib_sheet_data_set_component_attrib_count (SHEET_DATA *data,
                                              int count)
{
  data->comp_attrib_count = count;
}


/*! \brief Get the address of component attrib counter of sheet
 *  data.
 *
 *  \par Function Description
 *
 *  Returns the address of component attrib counter of given sheet
 *  data structure.
 *
 *  \return The address.
 */
int*
attrib_sheet_data_get_component_attrib_counter_address (SHEET_DATA *data)
{
  return &(data->comp_attrib_count);
}


/*! \brief Get the net table of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the net table of given sheet data structure.
 *
 *  \return The net table.
 */
TABLE**
attrib_sheet_data_get_net_table (SHEET_DATA *data)
{
  return data->net_table;
}


/*! \brief Set the net table of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the net table of a sheet data structure to the given
 *  value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] table The new value of the net table.
 */
void
attrib_sheet_data_set_net_table (SHEET_DATA *data,
                                 TABLE** table)
{
  data->net_table = table;
}


/*! \brief Get the count of nets of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the count of nets of given sheet data structure.
 *
 *  \return The count.
 */
int
attrib_sheet_data_get_net_count (SHEET_DATA *data)
{
  return data->net_count;
}


/*! \brief Set the count of nets of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the count of nets of a sheet data structure to the given
 *  value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] count The new count of nets.
 */
void
attrib_sheet_data_set_net_count (SHEET_DATA *data,
                                 int count)
{
  data->net_count = count;
}


/*! \brief Get the count of net attribs of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the count of net attribs of given sheet data
 *  structure.
 *
 *  \return The count.
 */
int
attrib_sheet_data_get_net_attrib_count (SHEET_DATA *data)
{
  return data->net_attrib_count;
}


/*! \brief Set the count of net attribs of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the count of net attribs of a sheet data structure to the
 *  given value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] count The new count of net attribs.
 */
void
attrib_sheet_data_set_net_attrib_count (SHEET_DATA *data,
                                        int count)
{
  data->net_attrib_count = count;
}


/*! \brief Get the pin table of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the pin table of given sheet data structure.
 *
 *  \return The pin table.
 */
TABLE**
attrib_sheet_data_get_pin_table (SHEET_DATA *data)
{
  return data->pin_table;
}


/*! \brief Set the pin table of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the pin table of a sheet data structure to the given
 *  value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] table The new value of the pin table.
 */
void
attrib_sheet_data_set_pin_table (SHEET_DATA *data,
                                 TABLE** table)
{
  data->pin_table = table;
}


/*! \brief Get the count of pins of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the count of pins of given sheet data structure.
 *
 *  \return The count.
 */
int
attrib_sheet_data_get_pin_count (SHEET_DATA *data)
{
  return data->pin_count;
}


/*! \brief Set the count of pins of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the count of pins of a sheet data structure to the given
 *  value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] count The new count of pins.
 */
void
attrib_sheet_data_set_pin_count (SHEET_DATA *data,
                                 int count)
{
  data->pin_count = count;
}


/*! \brief Get the address of pin counter of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the address of pin counter of given sheet data
 *  structure.
 *
 *  \return The address.
 */
int*
attrib_sheet_data_get_pin_counter_address (SHEET_DATA *data)
{
  return &(data->pin_count);
}


/*! \brief Get the count of pin attribs of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the count of pin attribs of given sheet data
 *  structure.
 *
 *  \return The count.
 */
int
attrib_sheet_data_get_pin_attrib_count (SHEET_DATA *data)
{
  return data->pin_attrib_count;
}


/*! \brief Set the count of pin attribs of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the count of pin attribs of a sheet data structure to the
 *  given value.
 *
 *  \param [in] data The sheet data.
 *  \param [in] count The new count of pin attribs.
 */
void
attrib_sheet_data_set_pin_attrib_count (SHEET_DATA *data,
                                        int count)
{
  data->pin_attrib_count = count;
}


/*! \brief Get the address of pin attrib counter of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the address of pin attrib counter of given sheet data
 *  structure.
 *
 *  \return The address.
 */
int*
attrib_sheet_data_get_pin_attrib_counter_address (SHEET_DATA *data)
{
  return &(data->pin_attrib_count);
}


/*! \brief Get the \a changed flag of sheet data.
 *
 *  \par Function Description
 *
 *  Returns the \a changed flag of given sheet data instance.
 *
 *  \param [in] data The sheet data.
 *
 *  \return The count.
 */
int
attrib_sheet_data_get_changed (const SHEET_DATA* data)
{
  return data->CHANGED;
}


/*! \brief Set the \a changed flag of sheet data.
 *
 *  \par Function Description
 *
 *  Sets the \a changed flag of given sheet data instance.
 *
 *  \param [in] data The sheet data.
 *  \param [in] changed The new \a changed flag value.
 */
void
attrib_sheet_data_set_changed (SHEET_DATA* data,
                               int changed)
{
  data->CHANGED = changed;
}


/*!
 * \brief Create a new SHEET_DATA struct.
 *
 *  \par Function Description
 *
 * Creates and returns a new SHEET_DATA struct.
 *
 * \return The SHEET_DATA struct.
 */
SHEET_DATA *attrib_sheet_data_new()
{
  return (SHEET_DATA *) g_malloc(sizeof(SHEET_DATA));
}
