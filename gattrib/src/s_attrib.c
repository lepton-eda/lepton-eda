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

#include <config.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

/*------------------------------------------------------------------
 * Gattrib specific includes
 *------------------------------------------------------------------*/
#include <libgeda/libgeda.h>       /* geda library fcns  */
#include "../include/struct.h"     /* typdef and struct declarations */
#include "../include/prototype.h"  /* function prototypes */
#include "../include/globals.h"


/*------------------------------------------------------------------
 * This fcn is passed a STRING_LIST of name=value pairs, and a 
 * name.  It returns 1 (TRUE) if the name is in the STRING_LIST, otherwise
 * it returns 0 (FALSE).
 *------------------------------------------------------------------*/
int s_attrib_name_in_list(STRING_LIST *name_value_list, char *name)
{
  STRING_LIST *local_list_item;
  char *local_name;

  local_list_item = name_value_list;
  while (local_list_item != NULL) {
    local_name =  u_basic_breakup_string(local_list_item->data, '=', 0);
    if (strcmp(local_name, name) == 0) {
      return TRUE;
    }
    local_list_item = local_list_item->next;
  }
  return FALSE;
}

