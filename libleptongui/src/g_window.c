/* Lepton EDA Schematic Capture
 * Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2010-2015 gEDA Contributors
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
#include <config.h>

#include "gschem.h"

SCM scheme_window_fluid = SCM_UNDEFINED;

/*! \brief Get the smob for a #GschemToplevel.
 * \par Function Description
 * Return a smob representing \a window.
 *
 * \param window #GschemToplevel to obtain a smob for.
 * \param a smob representing \a window.
 */
SCM
g_scm_from_window (GschemToplevel *w_current)
{
  g_assert (w_current != NULL);

  if (scm_is_eq (w_current->smob, SCM_UNDEFINED)) {
    w_current->smob = scm_from_pointer (w_current, NULL);
    scm_gc_protect_object (w_current->smob);
  }

  return w_current->smob;
}

/*!
 * \brief Set the #GschemToplevel fluid in the current dynamic context.
 * \par Function Description
 *
 * This function must be used inside a pair of calls to
 * scm_dynwind_begin() and scm_dynwind_end().  During the dynwind
 * context, the #GschemToplevel fluid is set to \a w_current.
 *
 * \param [in] w_current The new GschemToplevel pointer.
 */
void
g_dynwind_window (GschemToplevel *w_current)
{
  g_assert (w_current != NULL);
  SCM window_s = g_scm_from_window (w_current);
  scm_dynwind_fluid (scheme_window_fluid, window_s);
  edascm_dynwind_toplevel (w_current->toplevel);
}

/*!
 * \brief Get the value of the #GschemToplevel fluid.
 * \par Function Description
 * Return the value of the #GschemToplevel fluid in the current dynamic
 * context.
 * Signals an error if there is no valid window fluid
 * or the fluid value is NULL.
 * Never returns NULL.
 */
GschemToplevel *
g_current_window ()
{
  SCM window_s = scm_fluid_ref (scheme_window_fluid);
  GschemToplevel *w_current = (GschemToplevel *) scm_to_pointer (window_s);

  if (w_current == NULL)
  {
    scm_misc_error (NULL, _("Found invalid lepton-schematic window SCM: ~S"),
                    scm_list_1 (window_s));
  }

  return w_current;
}

/*!
 * \brief Create the (schematic core window) Scheme module
 * \par Function Description
 * Defines procedures in the (schematic core window) module. The module
 * can be accessed using (use-modules (schematic core window)).
 */
static void
init_module_schematic_core_window (void *unused)
{
  /* Register the functions */
  #include "g_window.x"

  /* Add them to the module's public definitions. */
  scm_c_export (NULL);
}

/*!
 * \brief Initialise the GschemToplevel manipulation procedures.
 * \par Function Description

 * Registers some Scheme procedures for working with #GschemToplevel
 * smobs and creates the #GschemToplevel fluid. Should only be called
 * by main_prog().
 */
void
g_init_window ()
{
  /* Create fluid */
  scheme_window_fluid = scm_permanent_object (scm_make_fluid ());
  scm_c_define ("%lepton-window", scheme_window_fluid);

  /* Define the (schematic core window) module */
  scm_c_define_module ("schematic core window",
                       (void (*)(void*)) init_module_schematic_core_window,
                       NULL);
}
