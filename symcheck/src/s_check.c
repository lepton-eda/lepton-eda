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

guint found_footprint = FALSE;
guint found_refdes = FALSE;

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

SCM_DEFINE (check_symbol_slotdef, "%check-symbol-slotdef", 2, 0, 0,
            (SCM numpins_s, SCM page_s), "Check symbol slotdef attribute")
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
  int numpins = scm_to_int (numpins_s);

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
  scm_c_export (s_check_symbol_pins_on_grid,
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
