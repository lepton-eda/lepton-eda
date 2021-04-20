/* Lepton EDA library -- Scheme API
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
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
 * \file scheme_os.c
 * \brief Scheme API functions for misc. OS-related stuff.
 */

#include <config.h>

#include <liblepton_priv.h>
#include <libleptonguile_priv.h>

/*!
 * \brief Create the (lepton core os) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core os) module. The module can
 * be accessed using (use-modules (lepton core os)).
 */
static void
init_module_lepton_core_os (void *unused)
{
  /* Register the functions and symbols */
  #include "scheme_os.x"

  /* Add them to the module's public definitions. */
  scm_c_export (NULL);
}

/*!
 * \brief Initialise the host platform support procedures.
 * \par Function Description

 * Registers some Scheme procedures that provide cross-platform
 * support. Should only be called by edascm_init().
 */
void
edascm_init_os ()
{
  /* Define the (lepton core os) module */
  scm_c_define_module ("lepton core os",
                       (void (*)(void*)) init_module_lepton_core_os,
                       NULL);
}
