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
 * Return the value of the #GschemToplevel fluid in the current
 * dynamic context.
 */
SCM_DEFINE (current_window, "%current-window", 0, 0, 0,
            (),
            "Get the GschemToplevel for the current dynamic context.")
{
  return scm_fluid_ref (scheme_window_fluid);
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
  SCM window_s = current_window ();
  GschemToplevel *w_current = (GschemToplevel *) scm_to_pointer (window_s);

  if (w_current == NULL)
  {
    scm_misc_error (NULL, _("Found invalid lepton-schematic window SCM: ~S"),
                    scm_list_1 (window_s));
  }

  return w_current;
}

/*!
 * \brief Close a page
 * \par Function Description
 * Closes the page \a page_s.
 *
 * \note Scheme API: Implements the %close-page! procedure in the
 * (schematic core window) module.
 *
 * \param page_s Page to close.
 * \return SCM_UNDEFINED
 */
SCM_DEFINE (override_close_page_x, "%close-page!", 1, 0, 0,
            (SCM page_s), "Close a page.")
{
  /* Ensure that the argument is a page smob */
  SCM_ASSERT (edascm_is_page (page_s), page_s,
              SCM_ARG1, s_override_close_page_x);

  GschemToplevel *w_current = g_current_window ();
  LeptonPage *page = edascm_to_page (page_s);

  LeptonPage *active_page = schematic_window_get_active_page (w_current);

  if (page != active_page)
    /* If page is not the current page, switch pages, then switch
     * back after closing page. */
  {
    x_window_set_current_page (w_current, page);
    x_window_close_page (w_current, schematic_window_get_active_page (w_current));
    x_window_set_current_page (w_current, active_page);
  }
  else
  {
    x_window_close_page (w_current, page);
  }
  return SCM_UNDEFINED;
}

/*!
 * \brief Get the current pointer position
 * \par Function Description
 * Returns the current mouse pointer position, expressed in world
 * coordinates.  If the pointer is outside the schematic drawing area,
 * returns SCM_BOOL_F.
 *
 * The coordinates are returned as a cons:
 *
 * <code>(x . y)</code>
 *
 * \note Scheme API: Implements the %pointer-position procedure in the
 * (schematic core window) module.
 *
 * \return The current pointer position, or SCM_BOOL_F.
 */
SCM_DEFINE (pointer_position, "%pointer-position", 0, 0, 0,
            (), "Get the current pointer position.")
{
  int x, y;
  GschemToplevel *w_current = g_current_window ();
  if (x_event_get_pointer_position (w_current, FALSE, &x, &y)) {
    return scm_cons (scm_from_int (x), scm_from_int (y));
  }
  return SCM_BOOL_F;
}

/*!
 * \brief Snap a point to the snap grid.
 * \par Function Description
 * Snaps the point (\a x_s, \a y_s) to the snap grid, returning the
 * snapped point position as a cons in the form:
 *
 * <code>(x . y)</code>
 *
 * This always snaps the given point to the grid, disregarding the
 * current user snap settings.
 *
 * \note Scheme API: Implements the %snap-point procedure in the
 * (schematic core window) module.
 *
 * \param x_s the x-coordinate of the point to be snapped to grid.
 * \param y_s the y-coordinate of the point to be snapped to grid.
 * \return the snapped coordinates.
 */
SCM_DEFINE (snap_point, "%snap-point", 2, 0, 0,
            (SCM x_s, SCM y_s), "Get the current snap grid size.")
{
  SCM_ASSERT (scm_is_integer (x_s), x_s, SCM_ARG1, s_snap_point);
  SCM_ASSERT (scm_is_integer (y_s), y_s, SCM_ARG2, s_snap_point);

  /* We save and restore the current snap setting, because we want to
   * *always* snap the requested cordinates. */
  GschemToplevel *w_current = g_current_window ();
  SNAP_STATE save_snap = gschem_options_get_snap_mode (w_current->options);
  gschem_options_set_snap_mode (w_current->options, SNAP_GRID);
  int x = snap_grid (w_current, scm_to_int (x_s));
  int y = snap_grid (w_current, scm_to_int (y_s));
  gschem_options_set_snap_mode (w_current->options, save_snap);

  return scm_cons (scm_from_int (x), scm_from_int (y));
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
  scm_c_export (s_current_window,
                s_override_close_page_x, s_pointer_position,
                s_snap_point, NULL);
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
