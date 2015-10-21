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
  scm_c_export (s_check_symbol_oldpin,
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
