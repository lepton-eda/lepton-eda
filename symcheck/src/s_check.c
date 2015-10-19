/* Lepton EDA
 * lepton-symcheck - Lepton Symbol Checker
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

#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"


GList *info_messages = NULL;
GList *warning_messages = NULL;
GList *error_messages = NULL;

gboolean graphical_symbol = FALSE;

guint found_footprint = FALSE;
guint found_refdes = FALSE;

guint numpins = 0;
guint numnetpins = 0;
guint numslots = 0;
guint numslotpins = 0;


SCM_DEFINE (symbol_check_glist_append, "%symbol-check-glist-append", 2, 0, 0,
            (SCM type_s, SCM message_s), "Check symbol text primitives")
{
  if (type_s == scm_string_to_symbol (scm_from_utf8_string ("error"))) {
    error_messages = g_list_append (error_messages, scm_to_utf8_string (message_s));
  } else if (type_s == scm_string_to_symbol (scm_from_utf8_string ("warning"))) {
    warning_messages = g_list_append (warning_messages, scm_to_utf8_string (message_s));
  } else if (type_s == scm_string_to_symbol (scm_from_utf8_string ("info"))) {
    info_messages = g_list_append (info_messages, scm_to_utf8_string (message_s));
  } else {
    g_assert_not_reached ();
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_graphical, "%check-symbol-graphical", 1, 0, 0,
            (SCM page_s), "Check graphical symbol attribute")
{
  char *temp;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  /* look for special graphical tag */
  temp = o_attrib_search_floating_attribs_by_name (obj_list, "graphical", 0);

  if (temp) {
    graphical_symbol=TRUE;
    g_free(temp);
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_device, "%check-symbol-device", 1, 0, 0,
            (SCM page_s), "Check symbol device attribute")
{
  char *temp;
  char *message;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  /* search for device attribute */
  temp = o_attrib_search_floating_attribs_by_name (obj_list, "device", 0);
  if (!temp) {
    /* did not find device= attribute */
    message = g_strdup (_("Missing device= attribute\n"));
    error_messages = g_list_append (error_messages, message);
  } else {
    /* found device= attribute */
    message = g_strdup_printf (_("Found device=%1$s\n"), temp);
    info_messages = g_list_append (info_messages, message);
  }

  /* check for device = none for graphical symbols */
  if (temp && graphical_symbol && (strcmp (temp, "none") == 0)) {
    message = g_strdup (_("Found graphical symbol, device=none\n"));
    info_messages = g_list_append (info_messages, message);
  } else if (graphical_symbol) {
    message = g_strdup (_("Found graphical symbol, device= should be set to none\n"));
    warning_messages = g_list_append (warning_messages, message);
  }

  g_free(temp);

  return SCM_BOOL_T;
}


SCM_DEFINE (check_symbol_pinseq, "%check-symbol-pinseq", 1, 0, 0,
            (SCM page_s), "Check symbol pinseq attribute")
{
  char *string;
  int found_first=FALSE;
  int counter=0;

  GList *found_numbers = NULL;
  GList *ptr1 = NULL;
  GList *ptr2 = NULL;
  const GList *iter;
  char *number;
  char *message;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;
    
    if (o_current->type == OBJ_PIN)
    {
      found_first = FALSE;
      counter = 0;
      
      string = o_attrib_search_object_attribs_by_name (o_current, "pinseq",
                                                       counter);
      if (!string)
      {
        message = g_strdup (_("Missing pinseq= attribute\n"));
        error_messages = g_list_append (error_messages, message);
      }

      while (string)
      {

        message = g_strdup_printf (_("Found pinseq=%1$s attribute\n"), string);
        info_messages = g_list_append (info_messages, message);

        number = g_strdup (string);

        if (strcmp(number, "0") == 0) {
          message = g_strdup (_("Found pinseq=0 attribute\n"));
          error_messages = g_list_append (error_messages, message);
        }

        if (found_first) {
          message = g_strdup_printf (
            _("Found multiple pinseq=%1$s attributes on one pin\n"),
            string);
          error_messages = g_list_append (error_messages, message);
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
        _("Found duplicate pinseq=%1$s attribute in the symbol\n"),
        string);
      error_messages = g_list_append (error_messages, message);
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

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_pinnumber, "%check-symbol-pinnumber", 1, 0, 0,
            (SCM page_s), "Check symbol pinnumber attribute")
{
  char *string;
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

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  /* collect all net pins */
  for (counter = 0;
       (net = o_attrib_search_floating_attribs_by_name (obj_list, "net", counter)) != NULL;
       counter++) {
    message = g_strdup_printf (_("Found net=%1$s attribute\n"), net);
    info_messages = g_list_append (info_messages, message);

    net_tokens = g_strsplit(net,":", -1);
    /* length of net tokens have to be 2 */
    if (net_tokens[1] == NULL) {
      message = g_strdup_printf (_("Bad net= attribute [net=%1$s]\n"), net);
      error_messages = g_list_append (error_messages, message);
      g_strfreev(net_tokens);
      continue;
    } else if (net_tokens[2] != NULL) { /* more than 2 tokens */
      message = g_strdup_printf (_("Bad net= attribute [net=%1$s]\n"), net);
      error_messages = g_list_append (error_messages, message);
      g_strfreev(net_tokens);
      continue;
    }

    pin_tokens = g_strsplit(net_tokens[1],",",-1);
    
    for (i = 0; pin_tokens[i] != NULL; i++) {
      net_numbers = g_list_append(net_numbers, g_strdup(pin_tokens[i]));
      message = g_strdup_printf (_("Found pin number %1$s in net attribute\n"),
                                 pin_tokens[i]);
      info_messages = g_list_append (info_messages, message);
      numnetpins++;
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
                                 "attributes [%1$s]\n"), (gchar*) cur->data);
      error_messages = g_list_append (error_messages, message);
    }
    if (strcmp((gchar*) cur->data, "0") == 0) {
      message = g_strdup (_("Found pinnumber 0 in net= attribute\n"));
      error_messages = g_list_append (error_messages, message);
    }
  }

  /* collect all pin numbers */
  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;
    
    if (o_current->type == OBJ_PIN) {
      numpins++;

      for (counter = 0;
	   (string = o_attrib_search_object_attribs_by_name (o_current, "pinnumber",
	                                                     counter)) != NULL;
	   counter++) {
	
        message = g_strdup_printf (_("Found pinnumber=%1$s attribute\n"), string);
        info_messages = g_list_append (info_messages, message);

	if (counter == 0) { /* collect the first appearance */
	  pin_numbers = g_list_append(pin_numbers, string);
	}
        if (counter >= 1) {
          message = g_strdup_printf (_("Found multiple pinnumber=%1$s attributes"
                                     " on one pin\n"), string);
          error_messages = g_list_append (error_messages, message);
          g_free(string);
        }
      }
	   
      if (counter == 0) {
        message = g_strdup (_("Missing pinnumber= attribute\n"));
        error_messages = g_list_append (error_messages, message);
      }
    }
  }

  /* check for duplicate pinlabel numbers */
  pin_numbers = g_list_sort(pin_numbers, (GCompareFunc)strcmp);
  for (cur = pin_numbers;
       cur != NULL && g_list_next(cur) != NULL;
       cur = g_list_next(cur)) { 
    if (strcmp((gchar*)cur->data, (gchar*) cur->next->data) == 0) {
      message = g_strdup_printf (_("Found duplicate pinnumber=%1$s attribute "
                                 "in the symbol\n"), (gchar*) cur->data);
      error_messages = g_list_append (error_messages, message);
    }
    if (strcmp((gchar*) cur->data, "0") == 0) {
      message = g_strdup (_("Found pinnumber=0 attribute\n"));
      error_messages = g_list_append (error_messages, message);
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
                                 "attribute and in a net attribute [%1$s]\n"),
                                 (gchar*) cur->data);
      warning_messages = g_list_append (warning_messages, message);
      cur = g_list_next(cur);

    } else if ( i > 0 ) {
      cur2 = g_list_next(cur2);

    } else { /* i < 0 */
      cur = g_list_next(cur);
    }
  }

  /* FIXME: this is not correct if a pinnumber is defined as pinnumber and
     inside a net. We have to calculate the union set */
  message = g_strdup_printf (_("Found %1$d pins inside symbol\n"),
                             numpins + numnetpins);
  info_messages = g_list_append(info_messages, message);

  g_list_foreach(pin_numbers, (GFunc) g_free, NULL);
  g_list_free(pin_numbers);
  g_list_foreach(net_numbers, (GFunc) g_free, NULL);
  g_list_free(net_numbers);

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_pins_on_grid, "%check-symbol-pins-on-grid", 1, 0, 0,
            (SCM page_s), "Check for whether all symbol pins are on grid")
{
  int x1, x2, y1, y2;
  const GList *iter;
  char *message;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

    if (o_current->type == OBJ_PIN) {
      x1 = o_current->line->x[0];
      y1 = o_current->line->y[0];
      x2 = o_current->line->x[1];
      y2 = o_current->line->y[1];
      
      if (x1 % 100 != 0 || y1 % 100 != 0) {
        message = g_strdup_printf(_("Found offgrid pin at location"
                                  " (x1=%1$d,y1=%2$d)\n"), x1, y1);
        /* error if it is the whichend, warning if not */
        if (o_current->whichend == 0) {
          error_messages = g_list_append (error_messages, message);
        }
        else {
          warning_messages = g_list_append (warning_messages, message);
        }
      }
      if (x2 % 100 != 0 || y2 % 100 != 0) {
        message = g_strdup_printf(_("Found offgrid pin at location"
                                  " (x2=%1$d,y2=%2$d)\n"), x2, y2);
        /* error when whichend, warning if not */
        if (o_current-> whichend != 0) {
          error_messages = g_list_append (error_messages, message);
        }
        else {
          warning_messages = g_list_append (warning_messages, message);
        }
      }
    }
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_slotdef, "%check-symbol-slotdef", 1, 0, 0,
            (SCM page_s), "Check symbol slotdef attribute")
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

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  /* look for numslots to see if this symbol has slotting info */
  value = o_attrib_search_floating_attribs_by_name (obj_list, "numslots", 0);

  if (!value) {
    /* Since there's no numslots= attribute, don't check slotting at all. */
    return SCM_BOOL_T;
  }

  numslots=atoi (value);
  sprintf(numslots_str, "%d", numslots);
  g_free(value);

  message = g_strdup_printf (_("Found numslots=%1$s attribute\n"), numslots_str);
  info_messages = g_list_append (info_messages, message);

  if (numslots == 0) {
    message = g_strdup (_("numslots set to zero, symbol does not have slots\n"));
    info_messages = g_list_append (info_messages, message);
    return SCM_BOOL_T;
  }
  

  pinlist = (char**)g_malloc0(sizeof(*pinlist) * numslots);

  i = 0;
  /* get the slotdef attribute */
  slotdef = o_attrib_search_floating_attribs_by_name (obj_list, "slotdef", 0);
  while ((slotdef != NULL) && (!error_parsing))
  {

    if (i > numslots-1) {

      sprintf(tempstr1, "%d", i+1); /* i starts at zero */
      message = g_strdup_printf (
        _("Found %1$s slotdef= attributes.  Expecting %2$s slotdef= attributes\n"),
        tempstr1, numslots_str);
      error_messages = g_list_append (error_messages, message);
    }
    
    message = g_strdup_printf (_("Found slotdef=%1$s attribute\n"), slotdef);
    info_messages = g_list_append (info_messages, message);

    slotnum = u_basic_breakup_string(slotdef, ':', 0);
    if (!slotnum)
    {
      message = g_strdup_printf (
        _("Invalid slotdef=%1$s attributes, not continuing\n"),
        slotdef);
      error_messages = g_list_append (error_messages, message);
      error_parsing = TRUE;
      continue;
    }

    if (strcmp(slotnum, "0") == 0) {
      message = g_strdup_printf (
        _("Found a zero slot in slotdef=%1$s\n"),
        slotdef);
      error_messages = g_list_append (error_messages, message);
    }
  
    slot = atoi(slotnum);
    g_free(slotnum);

    /* make sure that the slot # is less than the number of slots */
    if (slot > numslots) {
      sprintf(tempstr1, "%d", slot);
      message = g_strdup_printf (
        _("Slot %1$s is larger then the maximum number (%2$s) of slots\n"),
        tempstr1, numslots_str);
      error_messages = g_list_append (error_messages, message);
    }

    /* skip over the : */
    pins = strchr(slotdef, ':');
    if (!pins) {
      message = g_strdup_printf (
        _("Invalid slotdef=%1$s attributes, not continuing\n"),
        slotdef);
      error_messages = g_list_append (error_messages, message);
      error_parsing = TRUE;
      continue;
    }
    pins++;  /* get past that : */
    if (!pins) {
      message = g_strdup_printf (
        _("Invalid slotdef=%1$s attributes, not continuing\n"),
        slotdef);
      error_messages = g_list_append (error_messages, message);
      error_parsing = TRUE;
      continue;
    }

    if (*pins == '\0') {
      message = g_strdup_printf (
        _("Invalid slotdef=%1$s attributes, not continuing\n"),
        slotdef);
      error_messages = g_list_append (error_messages, message);
      error_parsing = TRUE;
      continue;
    }

    if ( (slot > 0) && (slot <= numslots)) {
      if (pinlist[slot-1]) {
        message = g_strdup_printf (_("Duplicate slot number in slotdef=%1$s\n"),
                                   slotdef);
        error_messages = g_list_append (error_messages, message);
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

      if (!temp && j < numpins) {
        message = g_strdup_printf (
          _("Not enough pins in slotdef=%1$s\n"),
          slotdef);
        error_messages = g_list_append (error_messages, message);
        break;
      }

      if (j > numpins) {
        message = g_strdup_printf (
          _("Too many pins in slotdef=%1$s\n"),
          slotdef);
        error_messages = g_list_append(error_messages, message);
        g_free(temp);
        temp = NULL;
        break;
      }
      
      if (temp && strcmp(temp, "0") == 0) {
        message = g_strdup_printf (
          _("Found a zero pin in slotdef=%1$s\n"),
          slotdef);
        error_messages = g_list_append (error_messages, message);
      }
     
      j++;
    } while (temp);

    g_free(temp);

    g_free(slotdef);
    slotdef = NULL;
   
    i++;
    slotdef = o_attrib_search_floating_attribs_by_name (obj_list, "slotdef", i);
  }

  if (!slotdef && i < numslots) {
    message = g_strdup_printf (
      _("Missing slotdef= (there should be %1$s slotdef= attributes)\n"),
      numslots_str);
    error_messages = g_list_append (error_messages, message);
  } else {

    /* Validate that pinslist does not contain a null entry.  If any entry */
    /* is null, that means the slotdef= attribute was malformed to start */
    /* with. */
    for (i = 0; i < numslots; i++) {
      if (pinlist[i] == NULL) {
        errors_found++;
      }
    }

    if (errors_found) {
      message = g_strdup_printf(
               _("Malformed slotdef= (the format is #:#,#,#,...)\n"));
      error_messages = g_list_append (error_messages,
                                                message);
    } else {
      /* Now compare each pin with the rest */
      numslotpins = 0;
      for (i = 0; i < numslots; i++) {
        for (n = 1; n <= numpins; n++) {
          /* Get the number of one pin */
          pin = u_basic_breakup_string(pinlist[i], ',', n);
          if (pin && *pin) {
            match = FALSE;
            for (j = i - 1; j >= 0 && !match; j--) {
              for (m = 1; m <= numpins && !match; m++) {
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
              numslotpins++;
            }
            g_free(pin);
          }
        }
      }
      message = g_strdup_printf (_("Found %1$d distinct pins in slots\n"),
                                 numslotpins);
      info_messages = g_list_append (info_messages,
                                               message);
    }
  }
  
  g_free(slotdef);
  if (pinlist) {
    /* Free the pinlist */
    for (i = 0; i < numslots; i++) {
      g_free(pinlist[i]);
    }
    g_free(pinlist);
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_oldpin, "%check-symbol-oldpin", 1, 0, 0,
            (SCM page_s), "Check symbol for old pin# attributes")
{
  const GList *iter;
  const char *ptr;
  int found_old = FALSE;
  int number_counter = 0;
  char *message;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

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
            _("Found old pin#=# attribute: %1$s\n"),
            geda_text_object_get_string (o_current));
          error_messages = g_list_append (error_messages, message);
        }
      }
    }
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_oldslot, "%check-symbol-oldslot", 1, 0, 0,
            (SCM page_s), "Check symbol for old slot# attributes")
{
  const GList *iter;
  const char *ptr;
  int found_old = FALSE;
  int number_counter = 0;
  char *message;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

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
            _("Found old slot#=# attribute: %1$s\n"),
            geda_text_object_get_string (o_current));
          error_messages = g_list_append (error_messages, message);
        }
      }
    }
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_nets_buses, "%check-symbol-nets-buses", 1, 0, 0,
            (SCM page_s), "Check symbol for nets or buses completely disallowed within it")
{
  const GList *iter;
  char *message;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

    if (o_current->type == OBJ_NET)
    {
      message = 
        g_strdup (_("Found a net inside a symbol\n"));
      error_messages = g_list_append (error_messages, message);
    }

    if (o_current->type == OBJ_BUS)
    {
      message = 
        g_strdup (_("Found a bus inside a symbol\n"));
      error_messages = g_list_append (error_messages, message);
    }

  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_connections, "%check-symbol-connections", 1, 0, 0,
            (SCM page_s), "Check symbol for connections completely disallowed within it")
{
  const GList *iter;
  char *message;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

    if (o_current->conn_list) {
      message = 
        g_strdup (_("Found a connection inside a symbol\n"));
      error_messages = g_list_append (error_messages, message);
    }
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_missing_attribute, "%check-symbol-missing-attribute", 2, 0, 0,
            (SCM object_s, SCM attrib_name), "Check for named symbol missing attribute")
{
  char *string;
  int found_first=FALSE;
  int counter=0;
  char *message;

  OBJECT *object = edascm_to_object (object_s);
  char *attribute = scm_to_utf8_string (attrib_name);

  if (!attribute) {
    return SCM_BOOL_T;
  }

  string = o_attrib_search_object_attribs_by_name (object, attribute, counter);
  if (!string)
  {
    message = g_strdup_printf (
      _("Missing %1$s= attribute\n"),
      attribute);
    warning_messages = g_list_append (warning_messages, message);
  }

  while (string)
  {

    if (found_first) {
      message = g_strdup_printf (
        _("Found multiple %1$s=%2$s attributes on one pin\n"),
        attribute, string);
      error_messages = g_list_append (error_messages, message);
    }
        
    /* this is the first attribute found */
    if (!found_first) {

      message = g_strdup_printf (
        _("Found %1$s=%2$s attribute\n"),
        attribute, string);
      info_messages = g_list_append (info_messages, message);
      found_first=TRUE;
    }

    g_free(string);

    counter++;
    string = o_attrib_search_object_attribs_by_name (object, attribute, counter);
  }

  return SCM_BOOL_T;
}

SCM_DEFINE (check_symbol_missing_attributes, "%check-symbol-missing-attributes", 1, 0, 0,
            (SCM page_s), "Check symbol missing attributes")
{
  const GList *iter;
  char *message;

  SCM obj_s;
  SCM attrib_name_s;

  PAGE* p_current = edascm_to_page (page_s);
  const GList *obj_list = s_page_objects (p_current);

  for (iter = obj_list; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *o_current = (OBJECT*) iter->data;

    if (o_current->type == OBJ_PIN)
    {
      obj_s = edascm_from_object (o_current);
      attrib_name_s = scm_from_utf8_string ("pinlabel");
      check_symbol_missing_attribute (obj_s, attrib_name_s);
      attrib_name_s = scm_from_utf8_string ("pintype");
      check_symbol_missing_attribute (obj_s, attrib_name_s);
    }

    if (o_current->type == OBJ_TEXT)
    {
      if (strstr(geda_text_object_get_string (o_current), "footprint=")) {
        message = g_strdup_printf (
          _("Found %1$s attribute\n"),
          geda_text_object_get_string (o_current));
        info_messages = g_list_append (info_messages, message);
        found_footprint++;
      }

      if (strstr(geda_text_object_get_string (o_current), "refdes=")) {
        message = g_strdup_printf (
          _("Found %1$s attribute\n"),
          geda_text_object_get_string (o_current));
        info_messages = g_list_append (info_messages, message);
        found_refdes++;
      }

    }
  }

  if (found_footprint == 0) {
    message = g_strdup (_("Missing footprint= attribute\n"));
    warning_messages = g_list_append (warning_messages, message);
  }

    if (found_footprint > 1) {
    message = g_strdup (_("Multiple footprint= attributes found\n"));
    error_messages = g_list_append (error_messages, message);
  }

  if (found_refdes == 0) {
    message = g_strdup (_("Missing refdes= attribute\n"));
    warning_messages = g_list_append (warning_messages, message);
  }

  if (found_refdes > 1) {
    message = g_strdup (_("Multiple refdes= attributes found\n"));
    error_messages = g_list_append (error_messages, message);
  }

  return SCM_BOOL_T;
}

/*! \brief Get a list of info messages.
 * \par Function Description
 * Retrieves a Scheme list of info messages.
 *
 * \return a Scheme list of #CHECK_INFO smobs.
 */
SCM_DEFINE (check_info_messages, "%check-info-messages", 0, 0, 0,
            (), "Retrieve a list of symcheck info messages")
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *msg_list = g_list_copy (info_messages);

  while (msg_list != NULL) {
    lst = scm_cons (scm_from_utf8_string ((char*) msg_list->data), lst);
    msg_list = g_list_next (msg_list);
  }

  rlst = scm_reverse (lst);
  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Get a list of warning messages.
 * \par Function Description
 * Retrieves a Scheme list of warning messages.
 *
 * \return a Scheme list of #CHECK_WARNING smobs.
 */
SCM_DEFINE (check_warning_messages, "%check-warning-messages", 0, 0, 0,
            (), "Retrieve a list of symcheck warning messages")
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *msg_list = g_list_copy (warning_messages);

  while (msg_list != NULL) {
    lst = scm_cons (scm_from_utf8_string ((char*) msg_list->data), lst);
    msg_list = g_list_next (msg_list);
  }

  rlst = scm_reverse (lst);
  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Get a list of error messages.
 * \par Function Description
 * Retrieves a Scheme list of error messages.
 *
 * \return a Scheme list of #CHECK_ERROR smobs.
 */
SCM_DEFINE (check_error_messages, "%check-error-messages", 0, 0, 0,
            (), "Retrieve a list of symcheck error messages")
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *msg_list = g_list_copy (error_messages);

  msg_list = g_list_copy (error_messages);

  while (msg_list != NULL) {
    lst = scm_cons (scm_from_utf8_string ((char*) msg_list->data), lst);
    msg_list = g_list_next (msg_list);
  }

  rlst = scm_reverse (lst);
  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Output state of the quiet_mode variable
 * \par Function Description
 * Outputs current state of the quiet_mode variable
 *
 * \return SCM_BOOL_F if the quiet_mode variable is 0, else return SCM_BOOL_T
 */
SCM_DEFINE (check_get_quiet_mode, "%check-get-quiet-mode", 0, 0, 0,
            (), "Get state of the quiet mode flag")
{
  return scm_from_bool (quiet_mode);
}

/*! \brief Output state of the verbose_mode variable
 * \par Function Description
 * Outputs current state of the verbose_mode variable
 *
 * \return SCM_BOOL_F if the verbose_mode variable is 0, else return SCM_BOOL_T
 */

SCM_DEFINE (check_get_verbose_mode, "%check-get-verbose-mode", 0, 0, 0,
            (), "Get state of the verbose mode flag")
{
  return scm_from_int (verbose_mode);
}

static void
init_module_symbol_core_check ()
{
  /* Register the functions */
  #include "s_check.x"

  /* Register the functions and add them to the module's public
   * definitions. */
  scm_c_export (s_check_symbol_graphical,
                s_check_symbol_device,
                s_check_symbol_missing_attribute,
                s_check_symbol_missing_attributes,
                s_check_symbol_pinseq,
                s_check_symbol_pinnumber,
                s_check_symbol_pins_on_grid,
                s_check_symbol_slotdef,
                s_check_symbol_oldpin,
                s_check_symbol_oldslot,
                s_check_symbol_nets_buses,
                s_check_symbol_connections,
                s_check_info_messages,
                s_check_warning_messages,
                s_check_error_messages,
                s_check_get_quiet_mode,
                s_check_get_verbose_mode,
                s_symbol_check_glist_append,
                NULL);
}

void
s_init_check ()
{
  /* Define the (symbol core check) module */
  scm_c_define_module ("symbol core check",
                       (void (*)(void*)) init_module_symbol_core_check,
                       NULL);
}
