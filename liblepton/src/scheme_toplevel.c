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
  SCM s_toplevel = scm_from_pointer (toplevel, NULL);

  scm_dynwind_fluid (scheme_toplevel_fluid, s_toplevel);
}


/*!
 * \brief Initialize fluid for obtaining the #LeptonToplevel value.
 * \par Function Description
 * This function should be used in Scheme FFI code to initialize
 * the C variable \a scheme_toplevel_fluid with a Scheme fluid
 * SCM, which then will be used both in C and Scheme worlds as a
 * variable for defining the current #LeptonToplevel value.
 *
 * \param [in] fluid The Scheme fluid.
 */
void
lepton_init_toplevel_fluid (SCM fluid)
{
  scheme_toplevel_fluid = fluid;
}
