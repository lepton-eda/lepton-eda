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

/*! \brief Add a configuration change event handler.
 * \par Function Description
 * Add \a proc as a function to be called when configuration is
 * modified in the context \a cfg.  \a proc will be called with the
 * following prototype:
 *
 * \code
 * (proc CFG GROUP KEY)
 * \endcode
 *
 * \param cfg The configuration context.
 * \param proc  Procedure to add as configuration change handler.
 * \return TRUE if the procedure has been added, FALSE if it had
 *         been already added previously.
 */
gboolean
config_add_event (EdaConfig *cfg,
                  gpointer handler,
                  gpointer proc)
{
  /* Test if proc was already connected. */
  guint signal_id = g_signal_lookup ("config-changed", EDA_TYPE_CONFIG);
  gulong handler_id =
    g_signal_handler_find (cfg,
                           (GSignalMatchType) (G_SIGNAL_MATCH_FUNC
                                               | G_SIGNAL_MATCH_DATA
                                               | G_SIGNAL_MATCH_ID),
                           signal_id,
                           0,
                           NULL,
                           (gpointer) handler,
                           (gpointer) proc);
  if (handler_id) {
    return FALSE;
  }

  g_signal_connect (cfg,
                    "config-changed",
                    G_CALLBACK (handler),
                    (gpointer) proc);
  return TRUE;
}

/*! \brief Remove a configuration change event handler.
 * \par Function Description
 * Stop \a proc from being called when configuration is modified in
 * the context \a cfg.
 *
 * \param cfg The configuration context.
 * \param proc  Procedure to remove as configuration change handler.
 * \return TRUE if \a proc has been successfully removed, otherwise FALSE.
 */
gboolean
config_remove_event (EdaConfig *cfg,
                     gpointer handler,
                     gpointer proc)
{
  guint signal_id = g_signal_lookup ("config-changed", EDA_TYPE_CONFIG);
  guint found =
    g_signal_handlers_disconnect_matched (cfg,
                                          (GSignalMatchType) (G_SIGNAL_MATCH_FUNC
                                                              | G_SIGNAL_MATCH_DATA
                                                              | G_SIGNAL_MATCH_ID),
                                          signal_id,
                                          0,
                                          NULL,
                                          (gpointer) handler,
                                          (gpointer) proc);
  g_warn_if_fail (found < 2);

  return found;
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
  scm_c_export (NULL);
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
