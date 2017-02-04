/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"

int
s_check_all(TOPLEVEL *pr_current)
{
  GList *iter;
  PAGE *p_current;
  int return_status=0;


  for ( iter = geda_list_get_glist( pr_current->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;

    if (s_page_objects (p_current)) {
      return_status = return_status +
        s_check_symbol (pr_current, p_current,
                        s_page_objects (p_current));
      if (!quiet_mode) s_log_message("\n");
    }
  }

  return(return_status);
}


int
s_check_symbol (TOPLEVEL *pr_current, PAGE *p_current, const GList *obj_list)
{
  SYMCHECK *s_symcheck=NULL;
  int errors=0, warnings=0;

  s_symcheck = s_symstruct_init();
  
  if (!quiet_mode) {
    s_log_message(_("Checking: %s\n"), p_current->page_filename);
  }
  
  /* overal symbol structure test */
  s_check_symbol_structure (obj_list, s_symcheck);

  /* test all text elements */
  s_check_text (obj_list, s_symcheck);

  /* check for graphical attribute */
  s_check_graphical (obj_list, s_symcheck);

  /* check for device attribute */
  s_check_device (obj_list, s_symcheck);

  /* check for missing attributes */
  s_check_missing_attributes (obj_list, s_symcheck);
  
  /* check for pintype attribute (and multiples) on all pins */
  s_check_pintype (obj_list, s_symcheck);
    
  /* check for pinseq attribute (and multiples) on all pins */
  s_check_pinseq (obj_list, s_symcheck);

  /* check for pinnumber attribute (and multiples) on all pins */
  s_check_pinnumber (obj_list, s_symcheck);

  /* check for whether all pins are on grid */
  s_check_pin_ongrid (obj_list, s_symcheck);

  /* check for slotdef attribute on all pins (if numslots exists) */
  s_check_slotdef (obj_list, s_symcheck);

  /* check for old pin#=# attributes */
  s_check_oldpin (obj_list, s_symcheck);

  /* check for old pin#=# attributes */
  s_check_oldslot (obj_list, s_symcheck);

  /* check for nets or buses within the symbol (completely disallowed) */
  s_check_nets_buses (obj_list, s_symcheck);

  /* check for connections with in a symbol (completely disallowed) */
  s_check_connections (obj_list, s_symcheck);

  /* now report the info/warnings/errors to the user */
  if (!quiet_mode) {
    
    /* done, now print out the messages */
    s_symstruct_print(s_symcheck);
    
    if (s_symcheck->warning_count > 0) {
      s_log_message(_("%d warnings found "),
                    s_symcheck->warning_count);
      if (verbose_mode < 2) {
        s_log_message(_("(use -vv to view details)\n"));
      } else {
        s_log_message("\n");
      }
    }
  
    if (s_symcheck->error_count == 0) {
      s_log_message(_("No errors found\n"));
    } else if (s_symcheck->error_count == 1) {
      s_log_message(_("1 ERROR found "));
      if (verbose_mode < 1) {
        s_log_message(_("(use -v to view details)\n"));
      } else {
        s_log_message("\n");
      }

    } else if (s_symcheck->error_count > 1) {
      s_log_message(_("%d ERRORS found "),
                    s_symcheck->error_count);
      if (verbose_mode < 1) {
        s_log_message(_("(use -v to view details)\n"));
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


gboolean 
s_check_list_has_item(char **list , char *item)
{
  gint cur;
  for (cur = 0; list[cur] != NULL; cur++) {
    if (strcmp(item, list[cur]) == 0)
      return TRUE;
  }
  return FALSE;
}

void
s_check_symbol_structure (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;

  gchar *message;
  gchar **tokens;

  char *valid_pin_attributes[] = {"pinlabel", "pintype",
				  "pinseq", "pinnumber",
				  NULL};
  char *valid_attributes[] = {"device", "graphical", "description",
			      "author", "comment", "numslots",
			      "slotdef", "footprint", "documentation",
			      "refdes", "slot", "net", "value",
			      "symversion", "dist-license", "use-license",
			      NULL};
  char *obsolete_attributes[] = {"uref", "label", "email", 
				 NULL};
  char *forbidden_attributes[] = {"type", "name", 
				  NULL};
  /* pin# ?, slot# ? */
  
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_TEXT) {
      tokens = g_strsplit(geda_text_object_get_string (o_current),"=", 2);
      if (tokens[0] != NULL && tokens[1] != NULL) {
	if (s_check_list_has_item(forbidden_attributes, tokens[0])) {
	  message = g_strdup_printf (_("Found forbidden %s= attribute: [%s=%s]\n"),
				     tokens[0], tokens[0], tokens[1]);
	  s_current->error_messages =
	    g_list_append(s_current->error_messages, message);
	  s_current->error_count++;
	}
	else if (s_check_list_has_item(obsolete_attributes, tokens[0])) {
	  message = g_strdup_printf (_("Found obsolete %s= attribute: [%s=%s]\n"),
				     tokens[0], tokens[0], tokens[1]);
	  s_current->warning_messages =
	    g_list_append(s_current->warning_messages, message);
	  s_current->warning_count++;
	}
	else if (s_check_list_has_item(valid_pin_attributes, tokens[0])) {
	  if (o_current->attached_to == NULL 
	      || o_current->attached_to->type != OBJ_PIN) {
	    message = g_strdup_printf (_("Found misplaced pin attribute:"
				       " [%s=%s]\n"), tokens[0], tokens[1]);
	    s_current->error_messages =
	      g_list_append(s_current->error_messages, message);
	    s_current->error_count++;
	  }
	}
	else if (!s_check_list_has_item(valid_attributes, tokens[0])) {
	  message = g_strdup_printf (_("Found unknown %s= attribute: [%s=%s]\n"),
				     tokens[0], tokens[0], tokens[1]);
	  s_current->warning_messages =
	    g_list_append(s_current->warning_messages, message);
	  s_current->warning_count++;
	}
	else if (o_current->attached_to != NULL) {
	  message = g_strdup_printf (_("Found wrongly attached attribute: "
				     "[%s=%s]\n"),
				     tokens[0], tokens[1]);
	  s_current->error_messages =
	    g_list_append(s_current->error_messages, message);
	  s_current->error_count++;
	}	  
      } else { /* object is not an attribute */
        if (o_current->show_name_value != SHOW_NAME_VALUE) {
          message = g_strdup_printf (_("Found a simple text object with only SHOW_NAME"
                                     " or SHOW_VALUE set [%s]\n"),
                                     geda_text_object_get_string (o_current));
          s_current->warning_messages =
            g_list_append(s_current->warning_messages, message);
          s_current->warning_count++;
        }
      }
      g_strfreev(tokens);
    }
  }
}

void
s_check_text (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  OBJECT *o_current;
  gboolean overbar_started, escape, leave_parser;
  char *message;
  const char *text_string;
  const char *ptr;
  gunichar current_char;

  for (iter = obj_list; iter != NULL; iter = g_list_next(iter)) {
    o_current = iter->data;

    if (o_current->type != OBJ_TEXT)
      continue;

    overbar_started = escape = leave_parser = FALSE;
    text_string = geda_text_object_get_string (o_current);

    for (ptr = text_string;
         ptr != NULL && !leave_parser;
         ptr = g_utf8_find_next_char (ptr, NULL)) {

      current_char = g_utf8_get_char_validated (ptr, -1);

      /* state machine to interpret the string:
       * there are two independant state variables overbar_started and escape.
       */
      switch (current_char) {
      case '\0':
        /* end of the string */
        leave_parser = TRUE;
        break;
      case '\\':
        if (escape == TRUE) {
          escape = FALSE;
        } else {
          escape = TRUE;
        }
        break;
      case '_':
        if (escape == TRUE) {
          escape = FALSE;
          if (overbar_started == TRUE) {
            overbar_started = FALSE;
          } else {
            overbar_started = TRUE;
          }
        }
        break;
      default:
        if (escape == TRUE) {
          message = g_strdup_printf (_("Found text with a '\\' in it: consider"
                                     " to escape it with '\\\\' [%s]\n"),
                                     text_string);
          s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                      message);
          s_current->warning_count++;
          escape = FALSE;
        }
      }
    }

    if (escape == TRUE) {
      message = g_strdup_printf (_("Found text with a trailing '\': consider to "
                                 "escape it with '\\\\' [%s]\n"),
                                 text_string);
      s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                  message);
      s_current->warning_count++;
    }

    if (overbar_started == TRUE) {
      message = g_strdup_printf (_("Found text with unbalanced overbar "
                                 "markers '\\_' in it' [%s]\n"),
                                 text_string);
      s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                  message);
      s_current->warning_count++;
    }
  }
}

void
s_check_graphical (const GList *obj_list, SYMCHECK *s_current)
{
  char *temp;
  
  /* look for special graphical tag */
  temp = o_attrib_search_floating_attribs_by_name (obj_list, "graphical", 0);

  if (temp) {
    s_current->graphical_symbol=TRUE;
    g_free(temp);
  }
}

void
s_check_device (const GList *obj_list, SYMCHECK *s_current)
{
  char *temp;
  char *message;
  
  /* search for device attribute */
  temp = o_attrib_search_floating_attribs_by_name (obj_list, "device", 0);
  if (!temp) {
    /* did not find device= attribute */
    message = g_strdup (_("Missing device= attribute\n"));
    s_current->error_messages = g_list_append(s_current->error_messages,
		                              message);
    s_current->missing_device_attrib=TRUE;
    s_current->error_count++;
  } else {
    /* found device= attribute */
    s_current->missing_device_attrib=FALSE;
    s_current->device_attribute = g_strdup (temp);
    message = g_strdup_printf (_("Found device=%s\n"), temp);
    s_current->info_messages = g_list_append(s_current->info_messages,
		                             message);
  }

  /* check for device = none for graphical symbols */
  if (temp && s_current->graphical_symbol && (strcmp(temp, "none") == 0)) {
    s_current->device_attribute_incorrect=FALSE;
    message = g_strdup (_("Found graphical symbol, device=none\n"));
    s_current->info_messages = g_list_append(s_current->info_messages,
                                             message);
  } else if (s_current->graphical_symbol) {
    s_current->device_attribute_incorrect=TRUE;
    message = g_strdup (_("Found graphical symbol, device= should be set to none\n"));
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;
  } 

  g_free(temp);
}


void
s_check_pinseq (const GList *obj_list, SYMCHECK *s_current)
{
  char *string;
  int found_first=FALSE;
  int missing_pinseq_attrib_sum=0;
  int multiple_pinseq_attrib_sum=0;
  int counter=0;

  GList *found_numbers = NULL;
  GList *ptr1 = NULL;
  GList *ptr2 = NULL;
  const GList *iter;
  char *number;
  char *message;
  
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;
    
    if (o_current->type == OBJ_PIN)
    {
      missing_pinseq_attrib_sum = 0;
      multiple_pinseq_attrib_sum = 0;
      found_first = FALSE;
      counter = 0;
      
      string = o_attrib_search_object_attribs_by_name (o_current, "pinseq",
                                                       counter);
      if (!string)
      {
        message = g_strdup (_("Missing pinseq= attribute\n"));
        s_current->error_messages = g_list_append(s_current->error_messages,
                                                  message);
        missing_pinseq_attrib_sum++;
        s_current->error_count++;
      }

      while (string)
      {
        
        message = g_strdup_printf (_("Found pinseq=%s attribute\n"), string); 
        s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			         message);

        number = g_strdup (string);

        if (strcmp(number, "0") == 0) {
          message = g_strdup (_("Found pinseq=0 attribute\n"));
          s_current->error_messages = g_list_append(s_current->error_messages,
	 	    			            message);
          s_current->error_count++;
        }

        if (found_first) {
          message = g_strdup_printf (
            _("Found multiple pinseq=%s attributes on one pin\n"),
            string);
          s_current->error_messages = g_list_append(s_current->error_messages,
	 	    			            message);
          multiple_pinseq_attrib_sum++;
          s_current->error_count++;
        }

        g_free(string);
        
        /* this is the first attribute found */
        if (!found_first) {
          found_numbers = g_list_append(found_numbers, number);
          found_first=TRUE;
        } else {
          g_free(number);
        }
        
        counter++;
        string = o_attrib_search_object_attribs_by_name (o_current, "pinseq",
                                                         counter);
      }

      s_current->missing_pinseq_attrib += missing_pinseq_attrib_sum;
      s_current->multiple_pinseq_attrib += multiple_pinseq_attrib_sum;
    }

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
      
      ptr2 = g_list_next(ptr2);
    }

    if (found > 1)
    {
      message = g_strdup_printf (
        _("Found duplicate pinseq=%s attribute in the symbol\n"),
        string);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->duplicate_pinseq_attrib++;
    }
    
    ptr1 = g_list_next(ptr1);
  }

  ptr1 = found_numbers;
  while (ptr1)
  {
    g_free(ptr1->data);
    ptr1 = g_list_next(ptr1);
  }
  g_list_free(found_numbers);
  
}


void
s_check_pinnumber (const GList *obj_list, SYMCHECK *s_current)
{
  char *string;
  int missing_pinnumber_attrib_sum=0;
  int multiple_pinnumber_attrib_sum=0;
  int counter=0;
  int i;

  gchar **net_tokens;
  gchar **pin_tokens;
  GList *net_numbers = NULL;
  GList *pin_numbers = NULL;
  GList *cur = NULL;
  GList *cur2 = NULL;
  const GList *iter;
  char *message;
  char *net = NULL;
    
  /* collect all net pins */
  for (counter = 0;
       (net = o_attrib_search_floating_attribs_by_name (obj_list, "net", counter)) != NULL;
       counter++) {
    message = g_strdup_printf (_("Found net=%s attribute\n"), net);
    s_current->info_messages = g_list_append(s_current->info_messages,
					     message);

    net_tokens = g_strsplit(net,":", -1);
    /* length of net tokens have to be 2 */
    if (net_tokens[1] == NULL) {
      message = g_strdup_printf (_("Bad net= attribute [net=%s]\n"), net);
      s_current->error_messages = g_list_append(s_current->error_messages,
						message);
      s_current->error_count++;
      g_strfreev(net_tokens);
      continue;
    } else if (net_tokens[2] != NULL) { /* more than 2 tokens */
      message = g_strdup_printf (_("Bad net= attribute [net=%s]\n"), net);
      s_current->error_messages = g_list_append(s_current->error_messages,
						message);
      s_current->error_count++;
      g_strfreev(net_tokens);
      continue;
    }

    pin_tokens = g_strsplit(net_tokens[1],",",-1);
    
    for (i = 0; pin_tokens[i] != NULL; i++) {
      net_numbers = g_list_append(net_numbers, g_strdup(pin_tokens[i]));
      message = g_strdup_printf (_("Found pin number %s in net attribute\n"),
                                 pin_tokens[i]);
      s_current->info_messages = g_list_append(s_current->info_messages,
					       message);
      s_current->numnetpins++;
    }
    g_free(net);
    g_strfreev(net_tokens);
    g_strfreev(pin_tokens);
  }
  
  /* check for duplicate net pin numbers */
  net_numbers = g_list_sort(net_numbers, (GCompareFunc)strcmp);

  for (cur = net_numbers;
       cur != NULL && g_list_next(cur) != NULL;
       cur = g_list_next(cur)) {
    if (strcmp((gchar*)cur->data, (gchar*) cur->next->data) == 0) {
      message = g_strdup_printf (_("Found duplicate pin in net= "
				 "attributes [%s]\n"), (gchar*) cur->data);
      s_current->error_messages = g_list_append(s_current->error_messages,
						message);
      s_current->error_count++;
    }
    if (strcmp((gchar*) cur->data, "0") == 0) {
      message = g_strdup (_("Found pinnumber 0 in net= attribute\n"));
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
    }
  }

  /* collect all pin numbers */
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;
    
    if (o_current->type == OBJ_PIN) {
      s_current->numpins++;
      
      missing_pinnumber_attrib_sum = 0;
      multiple_pinnumber_attrib_sum = 0;
      
      for (counter = 0; 
	   (string = o_attrib_search_object_attribs_by_name (o_current, "pinnumber",
	                                                     counter)) != NULL;
	   counter++) {
	
        message = g_strdup_printf (_("Found pinnumber=%s attribute\n"), string);
        s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			         message);

	if (counter == 0) { /* collect the first appearance */
	  pin_numbers = g_list_append(pin_numbers, string);
	}
        if (counter >= 1) {
          message = g_strdup_printf (_("Found multiple pinnumber=%s attributes"
				     " on one pin\n"), string);
          s_current->error_messages = g_list_append(s_current->error_messages,
	 	    			            message);
          multiple_pinnumber_attrib_sum++;
          s_current->error_count++;
	  g_free(string); 
        }
      }
	   
      if (counter == 0) {
        message = g_strdup (_("Missing pinnumber= attribute\n"));
        s_current->error_messages = g_list_append(s_current->error_messages,
                                                  message);
        missing_pinnumber_attrib_sum++;
        s_current->error_count++;
      }

      s_current->missing_pinnumber_attrib += missing_pinnumber_attrib_sum;
      s_current->multiple_pinnumber_attrib += multiple_pinnumber_attrib_sum;
    }
  }

  /* check for duplicate pinlabel numbers */
  pin_numbers = g_list_sort(pin_numbers, (GCompareFunc)strcmp);
  for (cur = pin_numbers;
       cur != NULL && g_list_next(cur) != NULL;
       cur = g_list_next(cur)) { 
    if (strcmp((gchar*)cur->data, (gchar*) cur->next->data) == 0) {
      message = g_strdup_printf (_("Found duplicate pinnumber=%s attribute "
				 "in the symbol\n"), (gchar*) cur->data);
      s_current->error_messages = g_list_append(s_current->error_messages,
						message);
      s_current->error_count++;
      s_current->duplicate_pinnumber_attrib++;
    }
    if (strcmp((gchar*) cur->data, "0") == 0) {
      message = g_strdup (_("Found pinnumber=0 attribute\n"));
      s_current->error_messages = g_list_append(s_current->error_messages,
						message);
      s_current->error_count++;
    }
  }

  /* Check for all pins that are in both lists and print a warning.
     Sometimes this is useful and sometimes it's an error. */

  cur = net_numbers;
  cur2 = pin_numbers;

  while (cur != NULL && cur2 != NULL) {
    
    i = strcmp((gchar*)cur->data, (gchar*)cur2->data);

    if (i == 0) {
      message = g_strdup_printf (_("Found the same number in a pinnumber "
				 "attribute and in a net attribute [%s]\n"),
				 (gchar*) cur->data);
      s_current->warning_messages = g_list_append(s_current->warning_messages,
						  message);
      s_current->warning_count++;
      cur = g_list_next(cur);

    } else if ( i > 0 ) {
      cur2 = g_list_next(cur2);

    } else { /* i < 0 */
      cur = g_list_next(cur);
    }
  }

  /* FIXME: this is not correct if a pinnumber is defined as pinnumber and
     inside a net. We have to calculate the union set */
  message = g_strdup_printf (_("Found %d pins inside symbol\n"), 
			     s_current->numpins + s_current->numnetpins);
  s_current->info_messages = g_list_append(s_current->info_messages,
                                           message);

  g_list_foreach(pin_numbers, (GFunc) g_free, NULL);
  g_list_free(pin_numbers);
  g_list_foreach(net_numbers, (GFunc) g_free, NULL);
  g_list_free(net_numbers);
}

void
s_check_pin_ongrid (const GList *obj_list, SYMCHECK *s_current)
{
  int x1, x2, y1, y2;
  const GList *iter;
  char *message;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {
      x1 = o_current->line->x[0];
      y1 = o_current->line->y[0];
      x2 = o_current->line->x[1];
      y2 = o_current->line->y[1];
      
      if (x1 % 100 != 0 || y1 % 100 != 0) {
	message = g_strdup_printf(_("Found offgrid pin at location"
				  " (x1=%d,y1=%d)\n"), x1, y1);
	/* error if it is the whichend, warning if not */
	if (o_current->whichend == 0) {
	  s_current->error_messages = g_list_append(s_current->error_messages,
						    message);
	  s_current->error_count++;
	}
	else {
	  s_current->warning_messages = g_list_append(s_current->warning_messages,
						      message);
	  s_current->warning_count++;
	}
      }
      if (x2 % 100 != 0 || y2 % 100 != 0) {
	message = g_strdup_printf(_("Found offgrid pin at location"
				  " (x2=%d,y2=%d)\n"), x2, y2);
	/* error when whichend, warning if not */
	if (o_current-> whichend != 0) {
	  s_current->error_messages = g_list_append(s_current->error_messages,
						    message);
	  s_current->error_count++;
	}
	else {
	  s_current->warning_messages = g_list_append(s_current->warning_messages,
						      message);
	  s_current->warning_count++;
	}
      }
    }
  }
}


void
s_check_slotdef (const GList *obj_list, SYMCHECK *s_current)
{
  char* value = NULL;
  char* slotdef = NULL;
  char* slotnum = NULL;
  char* pins = NULL;
  char* temp = NULL;
  char numslots_str[10];
  int slot;
  int i,j;
  char *message;
  char tempstr1[10];
  /*  pinlist will store the pin definitions for each slot */
  /* example: pinlist[0] = 3,2,8,4,1 ; pinlist[1] = 5,6,8,4,7 */
  char** pinlist = NULL;
  int n,m;
  char* pin;
  char* cmp;
  int match;
  gboolean error_parsing = FALSE;
  int errors_found = 0;

  /* look for numslots to see if this symbol has slotting info */
  value = o_attrib_search_floating_attribs_by_name (obj_list, "numslots", 0);

  if (!value) {
    /* Since there's no numslots= attribute, don't check slotting at all. */
    return;
  }

  s_current->numslots=atoi(value);
  sprintf(numslots_str, "%d", s_current->numslots);
  g_free(value);

  message = g_strdup_printf (_("Found numslots=%s attribute\n"), numslots_str);
  s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			   message);

  if (s_current->numslots == 0) {
    message = g_strdup (_("numslots set to zero, symbol does not have slots\n"));
    s_current->info_messages = g_list_append(s_current->info_messages,
                                             message);
    return;
  }
  

  pinlist = (char**)g_malloc0(sizeof(*pinlist) * s_current->numslots);

  i = 0;
  /* get the slotdef attribute */
  slotdef = o_attrib_search_floating_attribs_by_name (obj_list, "slotdef", 0);
  while ((slotdef != NULL) && (!error_parsing))
  {

    if (i > s_current->numslots-1) {

      sprintf(tempstr1, "%d", i+1); /* i starts at zero */
      message = g_strdup_printf (
        _("Found %s slotdef= attributes.  Expecting %s slotdef= attributes\n"),
        tempstr1, numslots_str); 
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);

      s_current->error_count++;
      s_current->slotting_errors++;
    }
    
    message = g_strdup_printf (_("Found slotdef=%s attribute\n"), slotdef);
    s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			     message);

    slotnum = u_basic_breakup_string(slotdef, ':', 0);
    if (!slotnum)
    {
      message = g_strdup_printf (
        _("Invalid slotdef=%s attributes, not continuing\n"),
        slotdef);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
      error_parsing = TRUE;
      continue;
    }

    if (strcmp(slotnum, "0") == 0) {
      message = g_strdup_printf (
        _("Found a zero slot in slotdef=%s\n"),
        slotdef);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
    }
  
    slot = atoi(slotnum);
    g_free(slotnum);

    /* make sure that the slot # is less than the number of slots */
    if (slot > s_current->numslots) {
      sprintf(tempstr1, "%d", slot);
      message = g_strdup_printf (
        _("Slot %s is larger then the maximum number (%s) of slots\n"),
        tempstr1, numslots_str);
      s_current->error_messages = g_list_append(s_current->error_messages,
		      			        message);

      s_current->error_count++;
      s_current->slotting_errors++;
    }

    /* skip over the : */
    pins = strchr(slotdef, ':');
    if (!pins) {
      message = g_strdup_printf (
        _("Invalid slotdef=%s attributes, not continuing\n"),
        slotdef);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
      error_parsing = TRUE;
      continue;
    }
    pins++;  /* get past that : */
    if (!pins) {
      message = g_strdup_printf (
        _("Invalid slotdef=%s attributes, not continuing\n"),
        slotdef);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
      error_parsing = TRUE;
      continue;
    }

    if (*pins == '\0') {
      message = g_strdup_printf (
        _("Invalid slotdef=%s attributes, not continuing\n"),
        slotdef);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
      error_parsing = TRUE;
      continue;
    }

    if ((slot > 0) && (slot <= s_current->numslots)) {
      if (pinlist[slot-1]) {
        message = g_strdup_printf (_("Duplicate slot number in slotdef=%s\n"),
				   slotdef);
        s_current->error_messages = g_list_append(s_current->error_messages,
	    			                  message);
        s_current->error_count++;
        s_current->slotting_errors++;
      } else {
	pinlist[slot-1] = g_strdup_printf(",%s,", pins);
      }
    }
    
    j = 0;
    do {
      if (temp) {
        g_free(temp);
        temp = NULL;
      }
        
      temp = u_basic_breakup_string(pins, ',', j);

      if (!temp && j < s_current->numpins) {
        message = g_strdup_printf (
          _("Not enough pins in slotdef=%s\n"),
          slotdef);
        s_current->error_messages = g_list_append(s_current->error_messages,
	    			                  message);
        s_current->error_count++;
        s_current->slotting_errors++;
        break;
      }

      if (j > s_current->numpins) {
        message = g_strdup_printf (
          _("Too many pins in slotdef=%s\n"),
          slotdef);
        s_current->error_messages = g_list_append(s_current->error_messages,
	    			                  message);
        s_current->error_count++;
        s_current->slotting_errors++;
        g_free(temp);
        temp = NULL;
        break;
      }
      
      if (temp && strcmp(temp, "0") == 0) {
        message = g_strdup_printf (
          _("Found a zero pin in slotdef=%s\n"),
          slotdef);
        s_current->error_messages = g_list_append(s_current->error_messages,
                                                  message);
        s_current->error_count++;
      }
     
      j++;
    } while (temp);

    g_free(temp);

    g_free(slotdef);
    slotdef = NULL;
   
    i++;
    slotdef = o_attrib_search_floating_attribs_by_name (obj_list, "slotdef", i);
  }

  if (!slotdef && i < s_current->numslots) {
    message = g_strdup_printf (
      _("Missing slotdef= (there should be %s slotdef= attributes)\n"),
      numslots_str);
    s_current->error_messages = g_list_append(s_current->error_messages,
			                      message);
    s_current->error_count++;
    s_current->slotting_errors++;
  } else {

    /* Validate that pinslist does not contain a null entry.  If any entry */
    /* is null, that means the slotdef= attribute was malformed to start */
    /* with. */
    for (i = 0; i < s_current->numslots; i++) { 
      if (pinlist[i] == NULL) {
        errors_found++;
      }
    }

    if (errors_found) {
      message = g_strdup_printf(
               _("Malformed slotdef= (the format is #:#,#,#,...)\n"));
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
      s_current->slotting_errors++;
    } else { 
      /* Now compare each pin with the rest */
      s_current->numslotpins = 0;
      for (i = 0; i < s_current->numslots; i++) {
        for (n = 1; n <= s_current->numpins; n++) {
          /* Get the number of one pin */
          pin = u_basic_breakup_string(pinlist[i], ',', n);
          if (pin && *pin) {
            match = FALSE;
            for (j = i - 1; j >= 0 && !match; j--) {
              for (m = 1; m <= s_current->numpins && !match; m++) {
                /* Get the number of the other pin */
                cmp = u_basic_breakup_string(pinlist[j], ',', m);
                if (cmp && *cmp) {
                  match = (0 == strcmp (pin, cmp));
                  g_free(cmp);
                }
              }
            }
            if (!match) {
              /* If they don't match, then increase the number of pins */
              s_current->numslotpins++;
            }
            g_free(pin);
          }
        }
      }
      message = g_strdup_printf (_("Found %d distinct pins in slots\n"), 
                                 s_current->numslotpins);
      s_current->info_messages = g_list_append(s_current->info_messages,
                                               message);
    }
  }
  
  g_free(slotdef);
  if (pinlist) {
    /* Free the pinlist */
    for (i = 0; i < s_current->numslots; i++) {
      g_free(pinlist[i]);
    }
    g_free(pinlist);
  }
 
  return;
}


void
s_check_oldpin (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  const char *ptr;
  int found_old = FALSE;
  int number_counter = 0;
  char *message;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_TEXT)
    {
      if (strstr(geda_text_object_get_string (o_current), "pin"))
      {
        /* skip over "pin" */
        ptr = geda_text_object_get_string (o_current) + 3;

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
          continue;

        /* found no numbers inbetween pin and = */
        if (number_counter == 0)
          continue;
        
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
          message = g_strdup_printf (
            _("Found old pin#=# attribute: %s\n"),
            geda_text_object_get_string (o_current));
          s_current->error_messages = g_list_append(s_current->error_messages,
                                                    message);

          s_current->found_oldpin_attrib += found_old;
          s_current->error_count++;

        }
      }
    }
  }
  
}


void
s_check_oldslot (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  const char *ptr;
  int found_old = FALSE;
  int number_counter = 0;
  char *message;
  
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_TEXT)
    {
      if (strstr(geda_text_object_get_string (o_current), "slot"))
      {
        /* skip over "slot" */
        ptr = geda_text_object_get_string (o_current) + 4;

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
          continue;

        /* found no numbers inbetween pin and = */
        if (number_counter == 0)
          continue;
        
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
          message = g_strdup_printf (
            _("Found old slot#=# attribute: %s\n"),
            geda_text_object_get_string (o_current));
          s_current->error_messages = g_list_append(s_current->error_messages,
                                                    message);
          s_current->found_oldslot_attrib += found_old;
          s_current->error_count++;

        }
      }
    }
  }
}


void
s_check_nets_buses (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  char *message;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_NET)
    {
      message = 
        g_strdup (_("Found a net inside a symbol\n"));
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->found_net++;
      s_current->error_count++;
    }

    if (o_current->type == OBJ_BUS)
    {
      message = 
        g_strdup (_("Found a bus inside a symbol\n"));
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->found_bus++;
      s_current->error_count++;
    }

  }
}

void
s_check_connections (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  char *message;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->conn_list) {
      message = 
        g_strdup (_("Found a connection inside a symbol\n"));
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->found_connection++;
      s_current->error_count++;
    }
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

  string = o_attrib_search_object_attribs_by_name (object, attribute, counter);
  if (!string)
  {
    message = g_strdup_printf (
      _("Missing %s= attribute\n"),
      attribute);
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;
  }

  while (string)
  {

    if (found_first) {
      message = g_strdup_printf (
        _("Found multiple %s=%s attributes on one pin\n"),
        attribute, string);
      s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
      s_current->error_count++;
    }
        
    /* this is the first attribute found */
    if (!found_first) {

      message = g_strdup_printf (
        _("Found %s=%s attribute\n"),
        attribute, string);
      s_current->info_messages = g_list_append(s_current->info_messages,
                                               message);
      found_first=TRUE;
    }

    g_free(string);

    counter++;
    string = o_attrib_search_object_attribs_by_name (object, attribute, counter);
  }

}

void
s_check_missing_attributes (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  char *message;

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_PIN)
    {
      s_check_missing_attribute(o_current, "pinlabel", s_current);
      s_check_missing_attribute(o_current, "pintype", s_current);
    }

    if (o_current->type == OBJ_TEXT)
    {
      if (strstr(geda_text_object_get_string (o_current), "footprint=")) {
        message = g_strdup_printf (
          _("Found %s attribute\n"),
          geda_text_object_get_string (o_current));
        s_current->info_messages = g_list_append(s_current->info_messages,
                                               message);
        s_current->found_footprint++;
      }

      if (strstr(geda_text_object_get_string (o_current), "refdes=")) {
        message = g_strdup_printf (
          _("Found %s attribute\n"),
          geda_text_object_get_string (o_current));
        s_current->info_messages = g_list_append(s_current->info_messages,
                                               message);
        s_current->found_refdes++;
      }

    }
  }

  if (s_current->found_footprint == 0) {
    message = g_strdup (_("Missing footprint= attribute\n"));
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;
  }

    if (s_current->found_footprint > 1) {
    message = g_strdup (_("Multiple footprint= attributes found\n"));
    s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
    s_current->error_count++;

  }
  
  if (s_current->found_refdes == 0) {
    message = g_strdup (_("Missing refdes= attribute\n"));
    s_current->warning_messages = g_list_append(s_current->warning_messages,
                                                message);
    s_current->warning_count++;

  }

  if (s_current->found_refdes > 1) {
    message = g_strdup (_("Multiple refdes= attributes found\n"));
    s_current->error_messages = g_list_append(s_current->error_messages,
                                                message);
    s_current->error_count++;
  }
  
}

void s_check_pintype (const GList *obj_list, SYMCHECK *s_current)
{
  const GList *iter;
  int counter=0;
  char *pintype;
  char *message;
  char *pintypes[] = {"in", "out", "io", "oc", "oe",
		      "pas", "tp", "tri", "clk", "pwr",
		      NULL};
  
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = iter->data;

    if (o_current->type == OBJ_PIN) {

      for (counter = 0;
           (pintype = o_attrib_search_object_attribs_by_name (o_current, "pintype",
                                                              counter)) != NULL;
           counter++) {

        message = g_strdup_printf(_("Found pintype=%s attribute\n"), pintype);
        s_current->info_messages = g_list_append(s_current->info_messages,
	 	    			         message);

	if ( ! s_check_list_has_item(pintypes, pintype)) {
	  message = g_strdup_printf (_("Invalid pintype=%s attribute\n"), pintype);
	  s_current->error_messages = g_list_append(s_current->error_messages, 
						    message); 
	  s_current->error_count++; 
	}

        g_free(pintype);
      }
    }
  }
}
