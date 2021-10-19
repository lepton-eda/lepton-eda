/* Lepton EDA library - Scheme API
 * Copyright (C) 2011-2012 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*!
 * \file scheme_config.c
 * \brief Scheme API configuration procedures.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

SCM_SYMBOL (system_error_sym, "system-error");
SCM_SYMBOL (config_error_sym, "config-error");
SCM_SYMBOL (unknown_encoding_sym, "unknown-encoding");
SCM_SYMBOL (parse_sym, "parse");
SCM_SYMBOL (key_not_found_sym, "key-not-found");
SCM_SYMBOL (group_not_found_sym, "group-not-found");
SCM_SYMBOL (invalid_value_sym, "invalid-value");

#define ASSERT_CFG_GROUP_KEY(subr) do { \
  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, subr); \
  SCM_ASSERT (scm_is_string (group_s), group_s, SCM_ARG2, subr); \
  SCM_ASSERT (scm_is_string (key_s), key_s, SCM_ARG3, subr); \
  } while (0);

/*! \brief Convert a GError to a Scheme error.
 *
 * Raise a Scheme exception for the given \a error, with the procedure
 * name \a subr. The \a error will be freed with g_clear_error(). Does
 * not return.
 *
 * The \a error will be converted to a Scheme error according to the
 * following rules:
 *
 * - If \a error is a GFileError, it will be converted to a
 *   system-error.
 *
 * - If \a error is an EdaConfigError, it will be converted to a
 *   config-error.
 *
 * - Otherwise, it will be converted to a misc-error.
 *
 * \bug For GFileErrors, the GLib error code will be returned rather
 * than the system error code.
 *
 * \param subr   name of failed procedure, or NULL.
 * \param error  error to be converted to a Scheme exception.
 */
static void
error_from_gerror (const gchar *subr, GError **error)
{
  if (error == NULL || *error == NULL) {
    scm_misc_error (subr, "Unknown error", SCM_EOL);
  }

  GError *err = *error;

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  /* Make sure that the GError gets cleaned up when the non-local exit
   * occurs. */
  scm_dynwind_unwind_handler ((void (*)(void *)) g_clear_error, error,
                              SCM_F_WIND_EXPLICITLY);

  SCM rest;

  if (err->domain == G_IO_ERROR) {
    /* File-related errors */
    scm_error (system_error_sym, subr, err->message, SCM_EOL,
               scm_list_1 (scm_from_int (err->code)));
  }

  if (err->domain == EDA_CONFIG_ERROR) {
    /* Configuration context-related errors */
    switch (err->code) {
    case EDA_CONFIG_ERROR_UNKNOWN_ENCODING:
      rest = scm_list_1 (unknown_encoding_sym);
      break;
    case EDA_CONFIG_ERROR_PARSE:
      rest = scm_list_1 (parse_sym);
      break;
    case EDA_CONFIG_ERROR_KEY_NOT_FOUND:
      rest = scm_list_1 (key_not_found_sym);
      break;
    case EDA_CONFIG_ERROR_GROUP_NOT_FOUND:
      rest = scm_list_1 (group_not_found_sym);
      break;
    case EDA_CONFIG_ERROR_INVALID_VALUE:
      rest = scm_list_1 (invalid_value_sym);
      break;
    default:
      rest = SCM_BOOL_F;
      break;
    }
    scm_error (config_error_sym, subr, err->message, SCM_EOL, rest);
  }

  /* All other errors */
  scm_misc_error (subr, err->message, SCM_EOL);

  scm_dynwind_end ();
  g_warn_if_reached ();
}



/*! \brief Get a configuration parameter's value as a string list.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of strings.
 *
 * \see eda_config_get_string_list().
 *
 * \note Scheme API: Implements the \%config-string-list procedure in
 * the (lepton core config) module.
 *
 * \param cfg_s    #EdaConfig smob of configuration context.
 * \param group_s  Group name as a string.
 * \param key_s    Key name as a string.
 * \return configuration value as a list of strings.
 */
SCM_DEFINE (config_string_list, "%config-string-list", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a string list.")
{
  ASSERT_CFG_GROUP_KEY (s_config_string_list);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  EdaConfig *cfg = edascm_to_config (cfg_s);
  char *group = scm_to_utf8_string (group_s);
  scm_dynwind_free (group);
  char *key = scm_to_utf8_string (key_s);
  scm_dynwind_free (key);
  gsize length, i;
  GError *error = NULL;
  gchar **value = eda_config_get_string_list (cfg, group, key,
                                              &length, &error);
  if (value == NULL) error_from_gerror  (s_config_string_list, &error);
  scm_dynwind_unwind_handler ((void (*)(void *)) g_strfreev, value,
                              SCM_F_WIND_EXPLICITLY);
  SCM value_s = SCM_EOL;
  for (i = 0; i < length; i++) {
    value_s = scm_cons (scm_from_utf8_string (value[i]), value_s);
  }
  scm_dynwind_end ();
  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Get a configuration parameter's value as a boolean list.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of booleans.
 *
 * \see eda_config_get_boolean_list().
 *
 * \note Scheme API: Implements the \%config-boolean-list procedure in
 * the (lepton core config) module.
 *
 * \param cfg_s    #EdaConfig smob of configuration context.
 * \param group_s  Group name as a string.
 * \param key_s    Key name as a string.
 * \return configuration value as a list of booleans.
 */
SCM_DEFINE (config_boolean_list, "%config-boolean-list", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a boolean list.")
{
  ASSERT_CFG_GROUP_KEY (s_config_boolean_list);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  EdaConfig *cfg = edascm_to_config (cfg_s);
  char *group = scm_to_utf8_string (group_s);
  scm_dynwind_free (group);
  char *key = scm_to_utf8_string (key_s);
  scm_dynwind_free (key);
  gsize length, i;
  GError *error = NULL;
  gboolean *value = eda_config_get_boolean_list (cfg, group, key,
                                                 &length, &error);
  if (value == NULL) error_from_gerror  (s_config_boolean_list, &error);
  scm_dynwind_unwind_handler (g_free, value,
                              SCM_F_WIND_EXPLICITLY);
  SCM value_s = SCM_EOL;
  for (i = 0; i < length; i++) {
    value_s = scm_cons (value[i] ? SCM_BOOL_T : SCM_BOOL_F, value_s);
  }
  scm_dynwind_end ();
  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Get a configuration parameter's value as an integer list.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of integers.
 *
 * \see eda_config_get_int_list().
 *
 * \note Scheme API: Implements the \%config-int-list procedure in
 * the (lepton core config) module.
 *
 * \param cfg_s    #EdaConfig smob of configuration context.
 * \param group_s  Group name as a string.
 * \param key_s    Key name as a string.
 * \return configuration value as a list of integers.
 */
SCM_DEFINE (config_int_list, "%config-int-list", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as an integer list.")
{
  ASSERT_CFG_GROUP_KEY (s_config_int_list);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  EdaConfig *cfg = edascm_to_config (cfg_s);
  char *group = scm_to_utf8_string (group_s);
  scm_dynwind_free (group);
  char *key = scm_to_utf8_string (key_s);
  scm_dynwind_free (key);
  gsize length, i;
  GError *error = NULL;
  gint *value = eda_config_get_int_list (cfg, group, key,
                                         &length, &error);
  if (value == NULL) error_from_gerror  (s_config_int_list, &error);
  scm_dynwind_unwind_handler (g_free, value,
                              SCM_F_WIND_EXPLICITLY);
  SCM value_s = SCM_EOL;
  for (i = 0; i < length; i++) {
    value_s = scm_cons (scm_from_int (value[i]), value_s);
  }
  scm_dynwind_end ();
  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Get a configuration parameter's value as a list of reals.
 * \par Function Description
 * Get the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s, as a
 * list of inexact real numbers.
 *
 * \see eda_config_get_double_list().
 *
 * \note Scheme API: Implements the \%config-real-list procedure in
 * the (lepton core config) module.
 *
 * \param cfg_s    #EdaConfig smob of configuration context.
 * \param group_s  Group name as a string.
 * \param key_s    Key name as a string.
 * \return configuration value as a list of inexact real numbers.
 */
SCM_DEFINE (config_real_list, "%config-real-list", 3, 0, 0,
            (SCM  cfg_s, SCM group_s, SCM key_s),
            "Get a configuration parameter's value as a list of reals.")
{
  ASSERT_CFG_GROUP_KEY (s_config_real_list);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  EdaConfig *cfg = edascm_to_config (cfg_s);
  char *group = scm_to_utf8_string (group_s);
  scm_dynwind_free (group);
  char *key = scm_to_utf8_string (key_s);
  scm_dynwind_free (key);
  gsize length, i;
  GError *error = NULL;
  gdouble *value = eda_config_get_double_list (cfg, group, key,
                                               &length, &error);
  if (value == NULL) error_from_gerror  (s_config_real_list, &error);
  scm_dynwind_unwind_handler (g_free, value,
                              SCM_F_WIND_EXPLICITLY);
  SCM value_s = SCM_EOL;
  for (i = 0; i < length; i++) {
    value_s = scm_cons (scm_from_double (value[i]), value_s);
  }
  scm_dynwind_end ();
  return scm_reverse_x (value_s, SCM_EOL);
}

/*! \brief Set a configuration parameter's value.
 * \par Function Description
 * Set the value of the configuration parameter specified by \a
 * group_s and \a key_s in the configuration context \a cfg_s to \a
 * value_s.  The supported types for \a value_s are strings, integers,
 * real numbers, and booleans, along with homogenous lists of strings,
 * integers, real numbers or booleans.
 *
 * \note Scheme API: Implements the \%set-config! procedure in the
 * (lepton core config) module.
 *
 * \param cfg_s    #EdaConfig smob of configuration context.
 * \param group_s  Group name as a string.
 * \param key_s    Key name as a string.
 * \param value_s  New value for parameter.
 * \return \a cfg_s.
 */
SCM_DEFINE (set_config_x, "%set-config!", 4, 0, 0,
            (SCM cfg_s, SCM group_s, SCM key_s, SCM value_s),
            "Set a configuration parameter's value.")
{
  ASSERT_CFG_GROUP_KEY (s_set_config_x);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  EdaConfig *cfg = edascm_to_config (cfg_s);
  char *group = scm_to_utf8_string (group_s);
  scm_dynwind_free (group);
  char *key = scm_to_utf8_string (key_s);
  scm_dynwind_free (key);

  /* Figure out what value is */
  if (scm_is_string (value_s)) {
    char *value = scm_to_utf8_string (value_s);
    scm_dynwind_free (value);
    eda_config_set_string (cfg, group, key, value);
  } else if (scm_is_bool (value_s)) {
    gboolean value = scm_is_true (value_s);
    eda_config_set_boolean (cfg, group, key, value);
  } else if (scm_is_integer (value_s) && scm_is_true (scm_exact_p (value_s))) {
    gint value = scm_to_int (value_s);
    eda_config_set_int (cfg, group, key, value);
  } else if (scm_is_real (value_s)) {
    gdouble value = scm_to_double (value_s);
    eda_config_set_double (cfg, group, key, value);

  } else if (scm_is_true (scm_list_p (value_s))) {
    /* Find out what sort of list it is, then process it accordingly. */
    SCM first_s = scm_car (value_s);
    int len = scm_to_int (scm_length (value_s));
    SCM curr_s;
    int i = 0;

    if (scm_is_string (first_s)) {
      gchar **value = g_new0 (gchar *, len + 1);
      scm_dynwind_unwind_handler ((void (*)(void *)) g_strfreev, value,
                                  SCM_F_WIND_EXPLICITLY);
      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {
        char *tmp = scm_to_utf8_string (scm_car (curr_s));
        value [i++] = g_strdup (tmp);
        free (tmp);
      }

      /* NULL-terminate the list of strings to be passed to g_strfreev(): */
      value [i] = NULL;

      eda_config_set_string_list (cfg, group, key,
                                  (const gchar * const *) value, len);

    } else if (scm_is_bool (first_s)) {
      gboolean *value = g_new0 (gboolean, len);
      scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);
      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {
        value[i++] = scm_is_true (scm_car (curr_s));
      }
      eda_config_set_boolean_list (cfg, group, key, value, len);

    } else if (scm_is_integer (first_s)
               && scm_is_true (scm_exact_p (first_s))) {
      gint *value = g_new0 (gint, len);
      scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);
      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {
        value[i++] = scm_to_int (scm_car (curr_s));
      }
      eda_config_set_int_list (cfg, group, key, value, len);

    } else if (scm_is_real (first_s)) {
      gdouble *value = g_new0 (gdouble, len);
      scm_dynwind_unwind_handler (g_free, value, SCM_F_WIND_EXPLICITLY);
      for (curr_s = value_s; !scm_is_null (curr_s); curr_s = scm_cdr (curr_s)) {
        value[i++] = scm_to_double (scm_car (curr_s));
      }
      eda_config_set_double_list (cfg, group, key, value, len);

    } else {
      scm_wrong_type_arg (s_set_config_x, SCM_ARG4, value_s);
    }

  } else {
    scm_wrong_type_arg (s_set_config_x, SCM_ARG4, value_s);
  }

  scm_remember_upto_here_1 (value_s);
  scm_dynwind_end ();
  return cfg_s;
}

/*! \brief Dispatch to a Scheme configuration change event handler.
 * \par Function Description
 * Dispatcher function used by the Scheme API to run Scheme procedures
 * when a configuration change occurs.
 */
static void
edascm_config_event_dispatcher (EdaConfig *cfg, const char *group,
                                const char *key, void *user_data)
{
  SCM proc_s = SCM_PACK ((scm_t_bits) user_data);
  SCM expr = scm_list_4 (proc_s,
                         edascm_from_config (cfg),
                         scm_from_utf8_string (group),
                         scm_from_utf8_string (key));

  g_scm_eval_protected (expr, scm_interaction_environment ());
  scm_remember_upto_here_1 (expr);
}

/*! \brief Add a configuration change event handler.
 * \par Function Description
 * Add \a proc_s as a function to be called when configuration is
 * modified in the context \a cfg.  \a proc_s will be called with the
 * following prototype:
 *
 * \code
 * (proc CFG GROUP KEY)
 * \endcode
 *
 * If \a proc_s causes an Scheme error to be raised, the error will be
 * caught and logged.
 *
 * \note Scheme API: Implements the \%add-config-event! procedure in the
 * (lepton core config) module.
 *
 * \param cfg_s    #EdaConfig smob of configuration context.
 * \param proc_s   Procedure to add as configuration change handler.
 * \return \a cfg_s.
 */
SCM_DEFINE (add_config_event_x, "%add-config-event!", 2, 0, 0,
            (SCM cfg_s, SCM proc_s),
            "Add a configuration change event handler.")
{
  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, s_add_config_event_x);
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc_s)),
              proc_s, SCM_ARG2, s_add_config_event_x);

  EdaConfig *cfg = edascm_to_config (cfg_s);

  /* Test if proc_s was already connected. */
  guint signal_id = g_signal_lookup ("config-changed", EDA_TYPE_CONFIG);
  gulong handler_id =
    g_signal_handler_find (cfg,
                           (GSignalMatchType) (G_SIGNAL_MATCH_FUNC
                                               | G_SIGNAL_MATCH_DATA
                                               | G_SIGNAL_MATCH_ID),
                           signal_id,
                           0,
                           NULL,
                           (gpointer) edascm_config_event_dispatcher,
                           (gpointer) SCM_UNPACK (proc_s));
  if (handler_id) {
    return cfg_s;
  }

  /* Protect proc_s against garbage collection */
  g_signal_connect (cfg, "config-changed",
                    G_CALLBACK (edascm_config_event_dispatcher),
                    (gpointer) SCM_UNPACK (scm_gc_protect_object (proc_s)));
  return cfg_s;
}

/*! \brief Remove a configuration change event handler.
 * \par Function Description
 * Stop \a proc_s from being called when configuration is modified in
 * the context \a cfg.
 *
 * \note Scheme API: Implements the \%remove-config-event! procedure
 * in the (lepton core config) module.
 *
 * \param cfg_s    #EdaConfig smob of configuration context.
 * \param proc_s   Procedure to remove as configuration change handler.
 * \return \a cfg_s.
 */
SCM_DEFINE (remove_config_event_x, "%remove-config-event!", 2, 0, 0,
            (SCM cfg_s, SCM proc_s),
            "Remove a configuration change event handler.")
{
  SCM_ASSERT (EDASCM_CONFIGP (cfg_s), cfg_s, SCM_ARG1, s_add_config_event_x);
  SCM_ASSERT (scm_is_true (scm_procedure_p (proc_s)),
              proc_s, SCM_ARG2, s_add_config_event_x);

  EdaConfig *cfg = edascm_to_config (cfg_s);
  guint signal_id = g_signal_lookup ("config-changed", EDA_TYPE_CONFIG);
  guint found =
    g_signal_handlers_disconnect_matched (cfg,
                                          (GSignalMatchType) (G_SIGNAL_MATCH_FUNC
                                                              | G_SIGNAL_MATCH_DATA
                                                              | G_SIGNAL_MATCH_ID),
                                          signal_id,
                                          0,
                                          NULL,
                                          (gpointer) edascm_config_event_dispatcher,
                                          (gpointer) SCM_UNPACK (proc_s));
  g_warn_if_fail (found < 2);
  if (found) {
    scm_gc_unprotect_object (proc_s);
  }
  return cfg_s;
}



/*!
 * \brief Create the (lepton core config) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core config) module. The module can
 * be accessed using (use-modules (lepton core config)).
 */
static void
init_module_lepton_core_config (void *unused)
{
  /* Register the functions and symbols */
  #include "scheme_config.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_config_string_list,
                s_config_boolean_list,
                s_config_int_list,
                s_config_real_list,
                s_set_config_x,
                s_add_config_event_x,
                s_remove_config_event_x,
                NULL);
}

/*!
 * \brief Initialise the basic Lepton EDA configuration manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #EdaConfig
 * smobs. Should only be called by edascm_init().
 */
void
edascm_init_config ()
{
  /* Define the (lepton core config) module */
  scm_c_define_module ("lepton core config",
                       (void (*)(void*)) init_module_lepton_core_config,
                       NULL);
}
