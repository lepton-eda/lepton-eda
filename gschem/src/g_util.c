/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
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

/*! \file g_util.c
 * \brief Scheme utility functions
 */

#include <config.h>
#include <missing.h>

#include "gschem.h"

/*! \brief Launch default application for a URI.
 * \par Function Description
 * Launches the default application associated with \a uri_s on the
 * host platform.  Raises an error on failure.
 *
 * \note Scheme API: Implements the %show-uri procedure in the (gschem
 * core util) module.
 *
 * \sa x_show_uri().
 *
 * \param uri_s  URI to launch viewer for.
 * \return undefined value.
 */
SCM_DEFINE (show_uri, "%show-uri", 1, 0, 0, (SCM uri_s),
            "Show a URI in the associated default application")
{
  /* Check that we were passed a string. */
  SCM_ASSERT (scm_is_string (uri_s), uri_s, SCM_ARG1, s_show_uri);

  GSCHEM_TOPLEVEL *w_current = g_current_window ();
  const gchar *uri = scm_to_utf8_string (uri_s);
  GError *err = NULL;

  if (!x_show_uri (w_current, uri, &err)) {
    scm_misc_error (s_show_uri, _("Could not launch URI ~S: ~A"),
                    scm_list_2 (uri_s,
                                scm_from_utf8_string (err->message)));
  }
  return SCM_UNDEFINED;
}

/*! \brief Create the (gschem core util) Scheme module.
 * \par Function Description
 * Defines procedures in the (gschem core util) module. The module can
 * be accessed using (use-modules (gschem core util)).
 */
static void
init_module_gschem_core_util ()
{
  /* Register the functions */
  #include "g_util.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_show_uri,
                NULL);
}

/*!
 * \brief Initialise miscellaneous gschem utility procedures.
 * \par Function Description
 * Registers some Scheme utility procedures for e.g. accessing
 * miscellaneous system services.  Should only be called by
 * main_prog().
 */
void
g_init_util ()
{
  /* Define the (gschem core util) module */
  scm_c_define_module ("gschem core util",
                       init_module_gschem_core_util,
                       NULL);
}
