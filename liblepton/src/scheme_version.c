/* Lepton EDA library - Scheme API
 * Copyright (C) 2017-2019 Lepton Developers
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

/*!
 * \file scheme_version.c
 * \brief Scheme API procedures for working with Lepton EDA version list.
 */

#include <config.h>
#include <version.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

/*!
 * \brief Returns Lepton EDA version list.
 * \par Function Description
 * Returns the list consisting of Scheme strings representing
 * the following variables:
 * - PREPEND_VERSION_STRING
 * - PACKAGE_DOTTED_VERSION
 * - PACKAGE_GIT_COMMIT
 * - PACKAGE_BUGREPORT
 * - PACKAGE_URL
 * The last element in the list contains version message that
 * can be used in the --version output.
 */
SCM_DEFINE (lepton_version, "%lepton-version", 0, 0, 0,
            (),
            "Return Lepton EDA version string list.")
{
  char* msg = version_message();

  SCM res = scm_list_n (scm_from_utf8_string (PREPEND_VERSION_STRING),
                        scm_from_utf8_string (PACKAGE_DOTTED_VERSION),
                        scm_from_utf8_string (PACKAGE_DATE_VERSION),
                        scm_from_utf8_string (PACKAGE_GIT_COMMIT),
                        scm_from_utf8_string (PACKAGE_BUGREPORT),
                        scm_from_utf8_string (PACKAGE_URL),
                        scm_from_utf8_string (msg),
                        SCM_UNDEFINED);

  free (msg);
  return res;
}


/*!
 * \brief Create the (lepton core version) Scheme module
 * \par Function Description
 * Defines procedures in the (lepton core version) module. The module
 * can be accessed using (use-modules (lepton core version)).
 */
static void
init_module_lepton_core_version (void *unused)
{
  /* Register the functions */
  #include "scheme_version.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_lepton_version,
                NULL);
}

/*!
 * \brief Initialise the TOPLEVEL manipulation procedures.
 * \par Function Description
 * Registers Scheme procedures for working with Lepton EDA version.
 * Should only be called by edascm_init().
 */
void
edascm_init_version ()
{
  /* Define the (lepton core version) module */
  scm_c_define_module ("lepton core version",
                       (void (*)(void*)) init_module_lepton_core_version,
                       NULL);
}
