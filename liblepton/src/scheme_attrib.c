/* Lepton EDA library - Scheme API
 * Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2010-2016 gEDA Contributors
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
 * \file scheme_attrib.c
 * \brief Scheme API attribute manipulation procedures.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

SCM_SYMBOL (attribute_format_sym, "attribute-format");


/*!
 * \brief Create the (lepton core attrib) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core attrib) module. The module can
 * be accessed using (use-modules (lepton core attrib)).
 */
static void
init_module_lepton_core_attrib (void *unused)
{
  /* Register the functions */
  #include "scheme_attrib.x"

  /* Add them to the module's public definitions. */
  scm_c_export (NULL);
}

/*!
 * \brief Initialise the basic gEDA attribute manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with
 * attributes. Should only be called by edascm_init().
 */
void
edascm_init_attrib ()
{
  /* Define the (lepton core attrib) module */
  scm_c_define_module ("lepton core attrib",
                       (void (*)(void*)) init_module_lepton_core_attrib,
                       NULL);
}
