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
        if (!quiet_mode) s_log_message("\n");
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
  int errors=0, warnings=0;

  s_symcheck = s_symstruct_init();
  
  if (!quiet_mode) {
    s_log_message("Checking: %s\n", p_current->page_filename);
  }
  
  /* check version number */
  /* s_check_version(object_head, s_symcheck); out */

  /* check for graphical attribute */
  s_check_graphical(object_head, s_symcheck);

  /* check for device attribute */
  s_check_device(object_head, s_symcheck);
 
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

  /* check for nets or buses within the symbol (completely disallowed) */
  s_check_nets_buses(object_head, s_symcheck);

  /* check for connections with in a symbol (completely disallowed) */
  s_check_connections(object_head, s_symcheck);

  /* check for missing attributes */
  s_check_missing_attributes(object_head, s_symcheck);
  
  /* check for obsolete attributes */
  s_check_obsolete_forbidden_attributes(object_head, s_symcheck);
  

  /* now report the info/warnings/errors to the user */
  if (!quiet_mode) {
    
    /* done, now print out the messages */
    s_symstruct_print(s_symcheck);
    
    if (s_symcheck->warning_count > 0) {
      s_log_message("%d warnings found ",
                    s_symcheck->warning_count);
      if (verbose_mode < 2) {
        s_log_message("(use -vv to view details)\n");
      } else {
        s_log_message("\n");
      }
    }
  
    if (s_symcheck->error_count == 0) {
      s_log_message("No errors found\n");
    } else if (s_symcheck->error_count == 1) {
      s_log_message("1 ERROR found ");
      if (verbose_mode < 1) {
        s_log_message("(use -v to view details)\n");
      } else {
        s_log_message("\n");
      }

    } else if (s_symcheck->error_count > 1) {
      s_log_message("%d ERRORS found ",
                    s_symcheck->error_count);
      if (verbose_mode < 1) {
        s_log_message("(use -v to view details)\n");
      } else {
        s_log_message("\n");
      }
    }
  }

  errors = s_symcheck->error_count;
  warnings = s_symcheck->warning_count;
  s_symstruct_free(s_symcheck);
  if (errors) {
    return(2);
  } else if (warnings) {
    return(1);
  } else {
    return(0);
  }
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

void
s_check_device(OBJECT *o_current, SYMCHECK *s_current)
{
  char *temp;
  char *message;
  
  /* search for device attribute */
  temp = o_attrib_search_name(o_current, "device", 0);
  if (!temp) {
    /* did not find device= attribute */
    message = u_basic_strdup("Missing device= attribute\n");
    s_current->error_messages = g_list_append(s_current->error_messages,
		                              message);
    s_current->missing_device_attrib=TRUE;
    s_current->error_count++;
  } else {
    /* found device= attribute */
    s_current->missing_device_attrib=FALSE;
    s_current->device_attribute =
      (char *) malloc(sizeof(char)*(strlen(temp)+1));
    strcpy(s_current->device_attribute, temp);
    message = u_basic_strdup_multiple("Found device=", temp, "\n", NULL);
    s_current->info_messages = g_list_append(s_current->info_messages,
		                             message);
  }

  /* check for device = none for graphical symbols */
  if (temp && s_current->graphical_symbol && (strcmp(temp, "none") == 0)) {
    s_current->device_attribute_incorrect=FALSE;
    message = u_basic_strdup("Found graphical symbol, device=none\n");
    s_current->info_messages = g_list_append(s_current->info_messages,
                                             message);
  } else if (s_current->graphical_symbol) {
    s_current->device_attribute_incorrect=TRUE;
    message = u_basic_strdup("Found graphical symbol, device= should be set to none\n");
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;
  } 

  if (temp) 
    free(temp);
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

  GList *found_numbers = NULL;
  GList *ptr1 = NULL;
  GList *ptr2 = NULL;
  char *number;
  char *message;
  
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
        message = u_basic_strdup("Missing pinseq= attribute\n");
        s_current->error_messages = g_list_append(s_current->error_messages,
                                                  message);
        missing_pinseq_attrib_sum++;
        s_current->error_count++;
      }

      while (string)
      {
        
        message = u_basic_strdup_multiple("Found pinseq=", string, 
				          " attribute\n", NULL);
        s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			         message);

        number = u_basic_strdup(string);

        if (strcmp(number, "0") == 0) {
          message = u_basic_strdup("Found pinseq=0 attribute\n");
          s_current->error_messages = g_list_append(s_current->error_messages,
	 	    			            message);
          s_current->error_count++;
        }

        if (found_first) {
          message = u_basic_strdup_multiple("Found multiple pinseq=", string, 
			                    " attributes on one pin\n", NULL);
          s_current->error_messages = g_list_append(s_current->error_messages,
	 	    			            message);
          multiple_pinseq_attrib_sum++;
          s_current->error_count++;
        }

        free(string);
        
        /* this is the first attribute found */
        if (!found_first) {
          found_numbers = g_list_append(found_numbers, number);
          found_first=TRUE;
        } else {
          if (number)
            free(number);
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

  ptr1 = found_numbers;
  while (ptr1)
  {
    char *string = (char *) ptr1->data;
    int found = 0;
    
    ptr2 = found_numbers;
    while(ptr2 && string)
    {
      char *current = (char *) ptr2->data;

      if (current && strcmp(string, current) == 0) {
        found++;
      }
      
      ptr2 = ptr2->next;
    }

    if (found > 1)
    {
      message = u_basic_strdup_multiple("Found duplicate pinseq=", string, 
                                        " attribute in the symbol\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->duplicate_pinseq_attrib++;
    }
    
    ptr1 = ptr1->next;
  }

  ptr1 = found_numbers;
  while (ptr1)
  {
    free(ptr1->data);
    ptr1 = ptr1->next;
  }
  g_list_free(found_numbers);
  
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

  GList *found_numbers = NULL;
  GList *ptr1 = NULL;
  GList *ptr2 = NULL;
  char *number;
  char *message;
  char tempstr[10];

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
        message = u_basic_strdup("Missing pinnumber= attribute\n");
        s_current->error_messages = g_list_append(s_current->error_messages,
                                                  message);
        missing_pinnumber_attrib_sum++;
        s_current->error_count++;
      }

      while (string)
      {
        message = u_basic_strdup_multiple("Found pinnumber=", string, 
				          " attribute\n", NULL);
        s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			         message);

        if (found_first) {
          message = u_basic_strdup_multiple("Found multiple pinnumber=", 
			                    string, 
			                    " attributes on one pin\n", NULL);
          s_current->error_messages = g_list_append(s_current->error_messages,
	 	    			            message);
          multiple_pinnumber_attrib_sum++;
          s_current->error_count++;
        }
        
        number = u_basic_strdup(string);
        free(string);

        if (strcmp(number, "0") == 0) {
          message = u_basic_strdup("Found pinnumber=0 attribute\n");
          s_current->error_messages = g_list_append(s_current->error_messages,
	 	    			            message);
          s_current->error_count++;
        }

        /* this is the first attribute found */
        if (!found_first) {
          found_numbers = g_list_append(found_numbers, number);
          found_first=TRUE;
        } else {
          if (number)
            free(number);
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

  ptr1 = found_numbers;
  while (ptr1)
  {
    char *string = (char *) ptr1->data;
    int found = 0;
    
    ptr2 = found_numbers;
    while(ptr2 && string)
    {
      char *current = (char *) ptr2->data;

      if (current && strcmp(string, current) == 0) {
        found++;
      }
      
      ptr2 = ptr2->next;
    }

    if (found > 1)
    {
      message = u_basic_strdup_multiple("Found duplicate pinnumber=", string, 
                                        " attribute in the symbol\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->duplicate_pinnumber_attrib++;
    }
    
    ptr1 = ptr1->next;
  }

  ptr1 = found_numbers;
  while (ptr1)
  {
    free(ptr1->data);
    ptr1 = ptr1->next;
  }
  g_list_free(found_numbers);


  sprintf(tempstr, "%d", s_current->numpins);
  message = u_basic_strdup_multiple("Found ", tempstr, " pins inside symbol\n", NULL);
  s_current->info_messages = g_list_append(s_current->info_messages,
                                           message);

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
  char numslots_str[10];
  int slot;
  int i,j;
  char *message;
  char tempstr1[10];
  
  /* look for numslots to see if this symbol has slotting info */
  value = o_attrib_search_name(object_head, "numslots", 0);

  if (!value) {
    message = u_basic_strdup("Did not find numslots= attribute, not checking slotting\n");
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;
    message = u_basic_strdup("If this symbol does not need slotting, set numslots to zero (numslots=0)\n");
    s_current->info_messages = g_list_append(s_current->info_messages,
                                             message);
    return;
  }

  numslots=atoi(value);
  sprintf(numslots_str, "%d", numslots);
  free(value);

  message = u_basic_strdup_multiple("Found numslots=", numslots_str, 
 			            " attribute\n", NULL);
  s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			   message);

  if (numslots == 0) {
    message = u_basic_strdup("numslots set to zero, symbol does not have slots\n");
    s_current->info_messages = g_list_append(s_current->info_messages,
                                             message);
    return;
  }
  
  i = 0;
  /* get the slotdef attribute */
  slotdef = o_attrib_search_name(object_head, "slotdef", 0);
  while (slotdef != NULL)
  {

    if (i > numslots-1) {

      sprintf(tempstr1, "%d", i+1); /* i starts at zero */
      message = 
        u_basic_strdup_multiple("Found ", tempstr1, 
                                " slotdef= attributes.  Expecting ", 
                                numslots_str, " slotdef= attributes\n", 
                                NULL); 
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);

      s_current->error_count++;
      s_current->slotting_errors++;
    }
    
    message = u_basic_strdup_multiple("Found slotdef=", slotdef, 
 			              " attribute\n", NULL);
    s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			     message);

    slotnum = u_basic_breakup_string(slotdef, ':', 0);
    if (!slotnum)
    {
      message = 
        u_basic_strdup_multiple("Invalid slotdef=", slotdef, 
                                " attributes, not continuing\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
      free(slotdef);
      return;
    }

    if (strcmp(slotnum, "0") == 0) {
      message = u_basic_strdup_multiple("Found a zero slot in slotdef=",
                                        slotdef, "\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
    }
  
    slot = atoi(slotnum);
    free(slotnum);

    /* make sure that the slot # is less than the number of slots */
    if (slot > numslots) {
      sprintf(tempstr1, "%d", slot);
      message = 
        u_basic_strdup_multiple("Slot ", tempstr1, 
                                " is larger then the maximum number (",
                                numslots_str, ") of slots\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
		      			        message);

      s_current->error_count++;
      s_current->slotting_errors++;
    }

    /* skip over the : */
    pins = index(slotdef, ':');
    if (!pins) {
      message = 
        u_basic_strdup_multiple("Invalid slotdef=", slotdef, 
                                " attributes, not continuing\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
      free(slotdef);
      return;
    }
    pins++;  /* get past that : */
    if (!pins) {
      message = 
        u_basic_strdup_multiple("Invalid slotdef=", slotdef, 
                                " attributes, not continuing\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
      free(slotdef);
      return;
    }

    if (*pins == '\0') {
      message = 
        u_basic_strdup_multiple("Invalid slotdef=", slotdef, 
                                " attributes, not continuing\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
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
        message = 
          u_basic_strdup_multiple("Not enough pins in slotdef=", slotdef, 
                                  "\n", NULL);
        s_current->error_messages = g_list_append(s_current->error_messages,
	    			                  message);
        s_current->error_count++;
        s_current->slotting_errors++;
        break;
      }

      if (j > s_current->numpins) {
        message = 
          u_basic_strdup_multiple("Too many pins in slotdef=", slotdef, 
                                  "\n", NULL);
        s_current->error_messages = g_list_append(s_current->error_messages,
	    			                  message);
        s_current->error_count++;
        s_current->slotting_errors++;
        free(temp);
        temp = NULL;
        break;
      }
      
      if (temp && strcmp(temp, "0") == 0) {
        message = u_basic_strdup_multiple("Found a zero pin in slotdef=",
                                          slotdef, "\n", NULL);
        s_current->error_messages = g_list_append(s_current->error_messages,
                                                  message);
        s_current->error_count++;
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
    message = 
      u_basic_strdup_multiple("Missing slotdef= (there should be ", 
                              numslots_str, " slotdef= attributes)\n", 
                              NULL);
    s_current->error_messages = g_list_append(s_current->error_messages,
			                      message);
    s_current->error_count++;
    s_current->slotting_errors++;
    return;
  }
}


void
s_check_oldpin(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *ptr;
  int found_old = FALSE;
  int number_counter = 0;
  char *message;
  
  o_current = object_head;
  while(o_current != NULL)
  {
    
    if (o_current->type == OBJ_TEXT)
    {
      if (strstr(o_current->text->string, "pin"))
      {
        /* skip over "pin" */
        ptr = o_current->text->string + 3;

        found_old = FALSE;
        number_counter = 0;
        while (ptr && *ptr > '0' && *ptr < '9')
        {
          number_counter++;
          ptr++;
        }

        if (ptr && *ptr == '=')
        {
          found_old++;
        }

        if (!ptr)
        {
          o_current = o_current->next;
          continue;
        }

        /* found no numbers inbetween pin and = */
        if (number_counter == 0)
        {
          o_current = o_current->next;
          continue;
        }
        
        /* skip over = char */
        ptr++;

        while (ptr && *ptr > '0' && *ptr < '9')
        {
          ptr++;
        }

        if (*ptr == '\0')
        {
          found_old++;
        } 

        /* 2 matches -> number found after pin and only numbers after = sign */
        if (found_old == 2)
        {
          message = 
            u_basic_strdup_multiple("Found old pin#=# attribute: ",
                                    o_current->text->string, 
                                    "\n", NULL);
          s_current->error_messages = g_list_append(s_current->error_messages,
                                                    message);

          s_current->found_oldpin_attrib += found_old;
          s_current->error_count++;

        }
      }
    }

    o_current = o_current->next;
  }
  
}


void
s_check_oldslot(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *ptr;
  int found_old = FALSE;
  int number_counter = 0;
  char *message;
  
  o_current = object_head;
  while(o_current != NULL)
  {
    
    if (o_current->type == OBJ_TEXT)
    {
      if (strstr(o_current->text->string, "slot"))
      {
        /* skip over "slot" */
        ptr = o_current->text->string + 4;

        found_old = FALSE;
        number_counter = 0;
        while (ptr && *ptr > '0' && *ptr < '9')
        {
          number_counter++;
          ptr++;
        }

        if (ptr && *ptr == '=')
        {
          found_old++;
        }

        if (!ptr)
        {
          o_current = o_current->next;
          continue;
        }

        /* found no numbers inbetween pin and = */
        if (number_counter == 0)
        {
          o_current = o_current->next;
          continue;
        }
        
        /* skip over = char */
        ptr++;

        while ((ptr && (*ptr > '0') && (*ptr < '9')) || (*ptr == ','))
        {
          ptr++;
        }

        if (*ptr == '\0')
        {
          found_old++;
        }

        /* 2 matches -> number found after slot and only numbers after = */
        if (found_old == 2)
        {
          message = 
            u_basic_strdup_multiple("Found old slot#=# attribute: ",
                                    o_current->text->string, 
                                    "\n", NULL);
          s_current->error_messages = g_list_append(s_current->error_messages,
                                                    message);
          s_current->found_oldslot_attrib += found_old;
          s_current->error_count++;

        }
      }
    }

    o_current = o_current->next;
  }
}


void
s_check_nets_buses(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *message;
  
  o_current = object_head;
  while(o_current != NULL)
  {
    if (o_current->type == OBJ_NET)
    {
      message = 
        u_basic_strdup("Found a net inside a symbol\n");
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->found_net++;
      s_current->error_count++;
    }

    if (o_current->type == OBJ_BUS)
    {
      message = 
        u_basic_strdup("Found a bus inside a symbol\n");
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->found_bus++;
      s_current->error_count++;
    }

    
    o_current = o_current->next;
  }
}

void
s_check_connections(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *message;
  
  o_current = object_head;
  while(o_current != NULL)
  {

    if (o_current->conn_list) {
      message = 
        u_basic_strdup("Found a connection inside a symbol\n");
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->found_connection++;
      s_current->error_count++;
    }
    
    o_current = o_current->next;
  }
}

void
s_check_obsolete_forbidden_attributes(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *message;
  char *attrib;
  
  o_current = object_head;
  while(o_current != NULL)
  {
    if (o_current->type == OBJ_TEXT)
    {

      attrib = strstr(o_current->text->string, "label=");
      /* make sure we only check for label= and not pinlabel= */
      if (attrib && attrib == o_current->text->string) {
        message = 
          u_basic_strdup_multiple("Found obsolete label= attribute: ",
                                  o_current->text->string, 
                                  "\n", NULL);
        s_current->warning_messages =
          g_list_append(s_current->warning_messages, message);
        s_current->found_label++;
        s_current->warning_count++;
      }
      
      if (strstr(o_current->text->string, "uref=")) {
        message = 
          u_basic_strdup_multiple("Found obsolete uref= attribute: ",
                                  o_current->text->string, 
                                  "\n", NULL);
        s_current->warning_messages =
          g_list_append(s_current->warning_messages, message);
        s_current->found_uref++;
        s_current->warning_count++;     
      }

      attrib = strstr(o_current->text->string, "type=");
      /* make sure we only check for type= and not pintype= */
      if (attrib && attrib == o_current->text->string) {
        message = 
          u_basic_strdup_multiple("Found forbidden type= attribute: ",
                                  o_current->text->string, 
                                  "\n", NULL);
        s_current->error_messages =
          g_list_append(s_current->error_messages, message);
        s_current->found_type++;
        s_current->error_count++;         
      }

      attrib = strstr(o_current->text->string, "name=");
      if (attrib && attrib == o_current->text->string) {
        message = 
          u_basic_strdup_multiple("Found forbidden name= attribute: ",
                                  o_current->text->string, 
                                  "\n", NULL);
        s_current->error_messages =
          g_list_append(s_current->error_messages, message);
        s_current->found_name++;
        s_current->error_count++;
      }
    }

    o_current = o_current->next;
  }
  
}


void
s_check_missing_attribute(OBJECT *object, char *attribute, SYMCHECK *s_current)
{
  char *string;
  int found_first=FALSE;
  int counter=0;
  char *message;

  if (!attribute) {
    return;
  }

  string = o_attrib_search_name_single_count(object, attribute,
                                             counter);
  if (!string)
  {
    message = u_basic_strdup_multiple("Missing ", attribute,
                                      "= attribute\n", NULL);
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;
  }

  while (string)
  {

    if (found_first) {
      message = u_basic_strdup_multiple("Found multiple ", attribute, "=", 
                                        string, 
                                        " attributes on one pin\n", NULL);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
    }
        
    /* this is the first attribute found */
    if (!found_first) {

      message = u_basic_strdup_multiple("Found ", attribute, "=", string, 
                                        " attribute\n", NULL);
      s_current->info_messages = g_list_append(s_current->info_messages,
                                               message);
      found_first=TRUE;
    }

    free(string);

    counter++;
    string = o_attrib_search_name_single_count(object, attribute,
                                               counter);
  }

}

void
s_check_missing_attributes(OBJECT *object_head, SYMCHECK *s_current)
{
  OBJECT *o_current;
  char *message;

  o_current = object_head;
  while(o_current != NULL)
  {
    if (o_current->type == OBJ_PIN)
    {
      s_check_missing_attribute(o_current, "pinlabel", s_current);
      s_check_missing_attribute(o_current, "pintype", s_current);
    }

    if (o_current->type == OBJ_TEXT)
    {
      if (strstr(o_current->text->string, "footprint=")) {
        message = u_basic_strdup_multiple("Found ", o_current->text->string, 
                                          " attribute\n", NULL);
        s_current->info_messages = g_list_append(s_current->info_messages,
                                               message);
        s_current->found_footprint++;
      }

      if (strstr(o_current->text->string, "refdes=")) {
        message = u_basic_strdup_multiple("Found ", o_current->text->string, 
                                          " attribute\n", NULL);
        s_current->info_messages = g_list_append(s_current->info_messages,
                                               message);
        s_current->found_refdes++;
      }

    }
    
    o_current = o_current->next;
  }

  if (s_current->found_footprint == 0) {
    message = u_basic_strdup("Missing footprint= attribute\n");
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;
  }

    if (s_current->found_footprint > 1) {
    message = u_basic_strdup("Multiple footprint= attributes found\n");
    s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
    s_current->error_count++;

  }
  
  if (s_current->found_refdes == 0) {
    message = u_basic_strdup("Missing refdes= attribute\n");
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;

  }

  if (s_current->found_refdes > 1) {
    message = u_basic_strdup("Multiple refdes= attributes found\n");
    s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
    s_current->error_count++;
  }
  
}
