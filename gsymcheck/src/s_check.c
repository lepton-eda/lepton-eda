/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
 * Copyright (C) 1998-2000 Ales V. Hvezda
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

int
s_check_all(TOPLEVEL *pr_current)
{
  PAGE *p_current;
  int return_status=0;

  p_current = pr_current->page_head;


  while(p_current != NULL) {
    if (p_current->pid != -1) {

      if (p_current->object_head) {
        return_status = return_status + 
          s_check_symbol(pr_current, p_current, 
                         p_current->object_head);
        if (verbose_mode) s_log_message("\n");
      }

    }

    p_current = p_current->next;
  }

  return(return_status);
}

int
s_check_symbol(TOPLEVEL *pr_current, PAGE *p_current, OBJECT *object_head)
{
  SYMCHECK *s_symcheck=NULL;
  int errors=0;

  s_symcheck = s_symstruct_init();
  
  if (verbose_mode) s_log_message("Checking: %s\n", p_current->page_filename);

  /* check version number */
  /* s_check_version(object_head, s_symcheck); */

  /* check for device attribute */
  s_check_device(object_head, s_symcheck);
  
  /* check for graphical attribute */
  s_check_graphical(object_head, s_symcheck);

  /* check for pinseq attribute (and multiples) on all pins */
  s_check_pinseq(object_head, s_symcheck);

  /* check for pinnumber attribute (and multiples) on all pins */
  s_check_pinnumber(object_head, s_symcheck);

  /* check for slotdef attribute on all pins (if numslots exists) */
  s_check_slotdef(object_head, s_symcheck);

  /* check for old pin#=# attributes */
  s_check_oldpin(object_head, s_symcheck);

  /* check for old pin#=# attributes */
  s_check_oldslot(object_head, s_symcheck);

  /* now see if there were any errors and print out status */
  errors = s_symstruct_print(s_symcheck);

  if (errors) {
    s_log_message("ERROR: %s has %d errors\n",
           p_current->page_filename, s_symcheck->error_count);	
  } 

  s_symstruct_free(s_symcheck);
  return(errors);
}

void
s_check_graphical(OBJECT *o_current, SYMCHECK *s_current)
{
  char *temp;
  
  /* look for special graphical tag */
  temp = o_attrib_search_name(o_current, "graphical", 0);

  if (temp) {
    s_current->graphical_symbol=TRUE;
    free(temp);
  }
}

s_check_device(OBJECT *o_current, SYMCHECK *s_current)
{
  char *temp;
  
  /* search for device attribute */
  temp = o_attrib_search_name(o_current, "device", 0);
  if (!temp) {
    s_current->missing_device_attrib=TRUE;
    s_current->error_count++;
  } else {
    s_current->missing_device_attrib=FALSE;
    s_current->device_attribute =
      (char *) malloc(sizeof(char)*(strlen(temp)+1));
    strcpy(s_current->device_attribute, temp);
  }

  /* check for device = none for graphical symbols */
  if (s_current->graphical_symbol && temp) { 
    if ((strcmp(temp, "none") == 0)) {
      s_current->device_attribute_incorrect=FALSE;
    } else if (s_current->graphical_symbol) {
      s_current->device_attribute_incorrect=TRUE;
    } 
  }
  if (temp) 
    free(temp);
}

void
s_check_pinnumber(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *string;
  int found_first=FALSE;
  int missing_pinnumber_attrib_sum=0;
  int multiple_pinnumber_attrib_sum=0;
  int counter=0;

  o_current = object_head;
  while(o_current != NULL)
  {
    
    if (o_current->type == OBJ_PIN)
    {
      s_current->numpins++;
      
      missing_pinnumber_attrib_sum = 0;
      multiple_pinnumber_attrib_sum = 0;
      found_first = FALSE;
      counter = 0;
      
      string = o_attrib_search_name_single_count(o_current, "pinnumber",
                                                 counter);
      if (!string)
      {
        missing_pinnumber_attrib_sum++;
        s_current->error_count++;
      }

      while (string)
      {
        if (verbose_mode)
          s_log_message("- INFO: found pinnumber=%s attribute\n", string);
        
        free(string);

        if (found_first) {
          multiple_pinnumber_attrib_sum++;
          s_current->error_count++;
        }
        
        /* this is the first attribute found? */
        if (!found_first) {
          found_first=TRUE;
        }
        
        counter++;
        string = o_attrib_search_name_single_count(o_current, "pinnumber",
                                                   counter);
      }

      s_current->missing_pinnumber_attrib += missing_pinnumber_attrib_sum;
      s_current->multiple_pinnumber_attrib += multiple_pinnumber_attrib_sum;
    }

    
    o_current = o_current->next;
  }
}


void
s_check_pinseq(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *string;
  int found_first=FALSE;
  int missing_pinseq_attrib_sum=0;
  int multiple_pinseq_attrib_sum=0;
  int counter=0;

  o_current = object_head;
  while(o_current != NULL)
  {
    
    if (o_current->type == OBJ_PIN)
    {
      missing_pinseq_attrib_sum = 0;
      multiple_pinseq_attrib_sum = 0;
      found_first = FALSE;
      counter = 0;
      
      string = o_attrib_search_name_single_count(o_current, "pinseq",
                                                 counter);
      if (!string)
      {
        missing_pinseq_attrib_sum++;
        s_current->error_count++;
      }

      while (string)
      {
        if (verbose_mode)
          s_log_message("- INFO: found pinseq=%s attribute\n", string);
        
        free(string);

        if (found_first) {
          multiple_pinseq_attrib_sum++;
          s_current->error_count++;
        }
        
        /* this is the first attribute found? */
        if (!found_first) {
          found_first=TRUE;
        }
        
        counter++;
        string = o_attrib_search_name_single_count(o_current, "pinseq",
                                                   counter);
      }

      s_current->missing_pinseq_attrib += missing_pinseq_attrib_sum;
      s_current->multiple_pinseq_attrib += multiple_pinseq_attrib_sum;
    }

    
    o_current = o_current->next;
  }
}


void
s_check_slotdef(OBJECT *object_head, SYMCHECK *s_current)
{
  char* value = NULL;
  char* slotdef = NULL;
  char* slotnum = NULL;
  char* pins = NULL;
  char* temp = NULL;
  int numslots;
  int slot;
  int i,j;
  
  /* look for numslots to see if this symbol has slotting info */
  value = o_attrib_search_name(object_head, "numslots", 0);

  if (!value) {
    if (verbose_mode)
      s_log_message("- INFO: no slotting information found\n");

    return;
  }

  numslots=atoi(value);
  free(value);

  if (verbose_mode)
    s_log_message("- INFO: found numslots=%d\n", numslots);


  i = 0;
  /* get the slotdef attribute */
  slotdef = o_attrib_search_name(object_head, "slotdef", 0);
  while (slotdef != NULL)
  {

    if (i > numslots) {
      if (verbose_mode)
        s_log_message("- ERROR: Too many slotdef (should be %d slotdef attributes)\n", numslots);
      s_current->error_count++;
      return;
    }
    
    if (verbose_mode)
      s_log_message("- INFO: found slotdef=%s\n", slotdef);

    slotnum = u_basic_breakup_string(slotdef, ':', 0);
    if (!slotnum)
    {
      if (verbose_mode)
        s_log_message("- ERROR: Invalid slotdef %s\n", slotdef);
      s_current->error_count++;
      free(slotdef);
      return;
    }
    slot = atoi(slotnum);
    free(slotnum);

    /* make sure that the slot # is less than the number of slots */
    if (slot > numslots) {
      if (verbose_mode)
        s_log_message("- ERROR: Slot %d is larger then maximum number (%d) of slots\n", slot, numslots);
      s_current->error_count++;
    }

    /* skip over the : */
    pins = index(slotdef, ':');
    if (!pins) {
      if (verbose_mode)
        s_log_message("- ERROR: Invalid slotdef %s\n", slotdef);
      s_current->error_count++;
      free(slotdef);
      return;
    }
    pins++;  /* get past that : */
    if (!pins) {
      if (verbose_mode)
        s_log_message("- ERROR: Invalid slotdef %s\n", slotdef);
      s_current->error_count++;
      free(slotdef);
      return;
    }

    
    j = 0;
    do {
      if (temp) {
        free(temp);
        temp = NULL;
      }
        
      temp = u_basic_breakup_string(pins, ',', j);

      if (!temp && j < s_current->numpins) {
        if (verbose_mode)
          s_log_message("- ERROR: Not enough pins in slotdef=%s\n", slotdef);
        s_current->error_count++;
        break;
      }

      if (j > s_current->numpins) {
        if (verbose_mode)
          s_log_message("- ERROR: Too many pins in slotdef=%s\n", slotdef);
        s_current->error_count++;
        free(temp);
        temp = NULL;
        break;
      }
     
      j++;
    } while (temp); 

    if (temp)
      free(temp);

    free(slotdef);
   
    i++;
    slotdef = o_attrib_search_name(object_head, "slotdef", i);
  }
  
  if (!slotdef && i < numslots) {
    if (verbose_mode)
      s_log_message("- ERROR: missing slotdef (should be %d slotdef attributes)\n", numslots);
    s_current->error_count++;
    return;
  }
    
}


void
s_check_oldpin(OBJECT *object_head, SYMCHECK *s_current)
{

}


void
s_check_oldslot(OBJECT *object_head, SYMCHECK *s_current)
{

}
