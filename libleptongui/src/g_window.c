/* Lepton EDA Schematic Capture
 * Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2010-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#include <config.h>

#include "schematic.h"

SCM scheme_window_fluid = SCM_UNDEFINED;

/*!
 * \brief Set the #SchematicWindow fluid in the current dynamic context.
 * \par Function Description
 *
 * This function must be used inside a pair of calls to
 * scm_dynwind_begin() and scm_dynwind_end().  During the dynwind
 * context, the #SchematicWindow fluid is set to \a w_current.
 *
 * \param [in] w_current The new SchematicWindow pointer.
 */
void
g_dynwind_window (SchematicWindow *w_current)
{
  g_assert (w_current != NULL);
  SCM window_s = scm_from_pointer (w_current, NULL);
  scm_dynwind_fluid (scheme_window_fluid, window_s);
  edascm_dynwind_toplevel (w_current->toplevel);
}

/*!
 * \brief Initialise the SchematicWindow manipulation procedures.
 * \par Function Description

 * Registers some Scheme procedures for working with #SchematicWindow
 * smobs and creates the #SchematicWindow fluid. Should only be called
 * by main_prog().
 */
void
g_init_window (SCM fluid)
{
  /* Create fluid */
  scheme_window_fluid = fluid;
}
