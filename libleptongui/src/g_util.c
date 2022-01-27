/* Lepton EDA Schematic Capture
 * Copyright (C) 2011 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2011-2015 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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

/*! \file g_util.c
 * \brief Scheme utility functions
 */

#include <config.h>

#include "gschem.h"

/*! \brief Create the (schematic core util) Scheme module.
 * \par Function Description
 * Defines procedures in the (schematic core util) module. The module can
 * be accessed using (use-modules (schematic core util)).
 */
static void
init_module_schematic_core_util (void *unused)
{
  /* Register the functions */
  #include "g_util.x"

  /* Add them to the module's public definitions. */
  scm_c_export (NULL);
}

/*!
 * \brief Initialise miscellaneous lepton-schematic utility procedures.
 * \par Function Description
 * Registers some Scheme utility procedures for e.g. accessing
 * miscellaneous system services.  Should only be called by
 * main_prog().
 */
void
g_init_util ()
{
  /* Define the (schematic core util) module */
  scm_c_define_module ("schematic core util",
                       (void (*)(void*)) init_module_schematic_core_util,
                       NULL);
}
