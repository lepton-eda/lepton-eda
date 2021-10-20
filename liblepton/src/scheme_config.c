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
  scm_c_export (s_add_config_event_x,
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
