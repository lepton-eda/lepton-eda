/* Lepton EDA library - Scheme API
 * Copyright (C) 2010-2012 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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
 * \file scheme_toplevel.c
 * \brief Scheme API procedures for working with the LeptonToplevel.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

SCM scheme_toplevel_fluid = SCM_UNDEFINED;

/*!
 * \brief Set the #LeptonToplevel fluid in the current dynamic context.
 * \ingroup guile_c_iface
 * \par Function Description
 * This function must be used inside a pair of calls to
 * scm_dynwind_begin() and scm_dynwind_end(). During the dynwind
 * context, the #LeptonToplevel fluid is set to \a toplevel.
 *
 * \note This is a part of the public C interface to the Scheme API.
 */
void
edascm_dynwind_toplevel (LeptonToplevel *toplevel)
{
  SCM s_toplevel = edascm_from_toplevel (toplevel);

  scm_dynwind_fluid (scheme_toplevel_fluid, s_toplevel);
}

/*!
 * \brief Get the value of the #LeptonToplevel fluid.
 * \par Function Description
 * Return the value of the #LeptonToplevel fluid in the current dynamic
 * context.
 */
SCM
edascm_current_toplevel ()
{
  return scm_fluid_ref (scheme_toplevel_fluid);
}

/*!
 * \brief Get the value of the #LeptonToplevel fluid.
 * \ingroup guile_c_iface
 * \par Function Description
 * Return the value of the #LeptonToplevel fluid in the current dynamic
 * context.
 *
 * \note This is a part of the public C interface to the Scheme API.
 */
LeptonToplevel *
edascm_c_current_toplevel ()
{
  g_debug ("edascm_c_current_toplevel()\n");
  SCM s_toplevel = edascm_current_toplevel ();

  EDASCM_ASSERT_SMOB_VALID(s_toplevel);

  return (LeptonToplevel *) SCM_SMOB_DATA (s_toplevel);
}

/*!
 * \brief Set the current #LeptonToplevel temporarily.
 * \par Function Description
 * Set the #LeptonToplevel fluid to \a toplevel and call \a thunk.
 */
SCM_DEFINE (edascm_with_toplevel, "%with-toplevel", 2, 0, 0,
            (SCM toplevel, SCM thunk),
            "Call `thunk', setting the LeptonToplevel fluid to `toplevel'.")
{
  return scm_with_fluid (scheme_toplevel_fluid, toplevel, thunk);
}

/*!
 * \brief Create the (lepton core toplevel) Scheme module
 * \par Function Description
 * Defines procedures in the (lepton core toplevel) module. The module
 * can be accessed using (use-modules (lepton core toplevel)).
 */
static void
init_module_lepton_core_toplevel (void *unused)
{
  /* Register the functions */
  #include "scheme_toplevel.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_edascm_with_toplevel,
                NULL);
}

/*!
 * \brief Initialise the LeptonToplevel manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #LeptonToplevel smobs
 * and creates the #LeptonToplevel fluid. Should only be called by
 * edascm_init().
 */
void
edascm_init_toplevel ()
{
  scheme_toplevel_fluid = scm_permanent_object (scm_make_fluid ());

  /* Define the (lepton core toplevel) module */
  scm_c_define_module ("lepton core toplevel",
                       (void (*)(void*)) init_module_lepton_core_toplevel,
                       NULL);
}
