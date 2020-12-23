/* Lepton EDA Schematic Capture
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2013-2015 gEDA Contributors
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

/*! \file g_builtins.c
 * \brief lepton-schematic built-in actions
 */

#include <config.h>

#include "gschem.h"

struct BuiltinInfo {
  const char *name;
  int req, opt, rst;
  SCM (*func)();
};

static struct BuiltinInfo builtins[] = {
  { NULL, 0, 0, 0, NULL }, /* Custodian */
};

/*! \brief Create the (schematic core builtins) Scheme module.
 * \par Function Description
 * Defines procedures in the (schematic core builtins) module. The module can
 * be accessed using (use-modules (schematic core builtins)).
 */
static void
init_module_schematic_core_builtins (void *unused)
{
  int i;

  /* Register the functions and add them to the module's public
   * definitions. */
  for (i = 0; builtins[i].name != NULL; ++i) {
    struct BuiltinInfo b = builtins[i];
    scm_c_define_gsubr (b.name, b.req, b.opt, b.rst, (scm_t_subr) b.func);
    scm_c_export (b.name, NULL);
  }
}

/*!
 * \brief Initialise lepton-schematic built-in actions.
 * \par Function Description
 * Registers the Scheme procedures used to access
 * lepton-schematic's built-in editing actions implemented in C.
 * Should only be called by main_prog().
 */
void
g_init_builtins ()
{
  /* Define the (schematic core builtins) module */
  scm_c_define_module ("schematic core builtins",
                       (void (*)(void*)) init_module_schematic_core_builtins,
                       NULL);
}
