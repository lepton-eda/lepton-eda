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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


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


/*------------------------------------------------------------------
 * This fcn takes an object, finds its refdes and returns it.
 * If the component is slotted, it returns a refdes of the form 
 * refdes.slot.  If no refdes is found, it returns NULL.
 *------------------------------------------------------------------*/
char *s_attrib_get_refdes(OBJECT *object)
{
  char *temp_uref;
  char *numslots_value;
  char *slot_value;
  OBJECT *slot_text_object;

  /*------ Try to get the refdes -----*/
  temp_uref = o_attrib_search_name_single(object, "refdes", NULL);
  if (!temp_uref) {
    temp_uref = o_attrib_search_name_single(object, "uref", NULL); // deprecated
    if (temp_uref) {
      printf("WARNING: Found uref=%s, uref= is deprecated, please use refdes=\n", temp_uref);
    } else {        /* didn't find refdes.  Report error to log. */
#ifdef DEBUG
      printf("In s_attrib_get_refdes, found non-graphical component with no refdes.\n");
      printf(". . . . complex_basename = %s.\n", object->complex_basename);
#endif
      return NULL;
    } 
  }

#ifdef DEBUG
  printf("In s_attrib_get_refdes, found component with refdes %s.\n", temp_uref);
#endif   
  
  /*------- Now append .slot to refdes if part is slotted -------- */
  /* Find out if this is a multislotted component */
  numslots_value = o_attrib_search_numslots(object, NULL);
  if (numslots_value != NULL) {  /* this is a slotted component; 
				    append slot number to refdes. */
    slot_value = o_attrib_search_slot(object, &slot_text_object);
#if DEBUG
    printf(". . .  , found slotted component with slot = %s\n", slot_value);
#endif
    temp_uref = g_strconcat(temp_uref, ".", slot_value, NULL);
  }

#ifdef DEBUG
  printf(". . . .   returning refdes %s.\n", temp_uref);
#endif   
  
  return temp_uref;

}

