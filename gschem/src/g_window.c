/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
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
#include <config.h>

#include "gschem.h"

SCM scheme_window_fluid = SCM_UNDEFINED;

scm_t_bits window_smob_tag;

/*! \brief Free a #GschemToplevel smob.
 * \par Function Description
 * Finalizes a window smob for deletion.
 *
 * Used internally to Guile
 */
static size_t
smob_free (SCM smob)
{
  GschemToplevel *window = (GschemToplevel *) SCM_SMOB_DATA (smob);

  /* If the weak ref has been cleared, do nothing */
  if (window == NULL) return 0;

  /* Otherwise, go away. */
  window->smob = SCM_UNDEFINED;

  return 0;
}

/*! \brief Print a representation of a #GschemToplevel smob.
 * \par Function Description
 * Outputs a string representing the \a smob to a Scheme output
 * \a port. The format used is "#<gschem-window b7ef65d0>".
 *
 * Used internally to Guile.
 */
static int
smob_print (SCM smob, SCM port, scm_print_state *pstate)
{
  gchar *hexstring;

  scm_puts ("#<gschem-window", port);

  scm_dynwind_begin (0);
  hexstring = g_strdup_printf (" %zx", SCM_SMOB_DATA (smob));
  scm_dynwind_unwind_handler (g_free, hexstring, SCM_F_WIND_EXPLICITLY);
  scm_puts (hexstring, port);
  scm_dynwind_end ();

  scm_puts (">", port);

  /* Non-zero means success */
  return 1;
}

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
    SCM_NEWSMOB (w_current->smob, window_smob_tag, w_current);
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
 */
GschemToplevel *
g_current_window ()
{
  SCM window_s = current_window ();

  if (!(SCM_SMOB_PREDICATE (window_smob_tag, window_s)
        &&  ((void *)SCM_SMOB_DATA (window_s) != NULL))) {
    scm_misc_error (NULL, _("Found invalid gschem window smob ~S"),
                    scm_list_1 (window_s));
  }

  return (GschemToplevel *) SCM_SMOB_DATA (window_s);
}

/*!
 * \brief Get the active page.
 * \par Function Description
 * Returns the page which is active in the current gschem window.  If
 * there is no active page, returns SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %active-page procedure in the
 * (gschem core window) module.
 *
 * \return the active page.
 */
SCM_DEFINE (active_page, "%active-page", 0, 0, 0,
            (), "Get the active page.")
{
  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  if (toplevel->page_current != NULL) {
    return edascm_from_page (toplevel->page_current);
  } else {
    return SCM_BOOL_F;
  }
}

/*!
 * \brief Set the active page.
 * \par Function Description
 * Sets the page which is active in the current gschem window to \a
 * page_s.
 *
 * \note Scheme API: Implements the %set-active-page! procedure in the
 * (gschem core window) module.
 *
 * \param page_s Page to switch to.
 * \return \a page_s.
 */
SCM_DEFINE (set_active_page_x, "%set-active-page!", 1, 0, 0,
            (SCM page_s), "Set the active page.")
{
  SCM_ASSERT (edascm_is_page (page_s), page_s, SCM_ARG1, s_set_active_page_x);

  PAGE *page = edascm_to_page (page_s);
  x_window_set_current_page (g_current_window (), page);

  return page_s;
}

/*!
 * \brief Close a page
 * \par Function Description
 * Closes the page \a page_s.
 *
 * \note Scheme API: Implements the %close-page! procedure in the
 * (gschem core window) module.  Overrides the %close-page! procedure
 * in the (geda core page) module.
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
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  PAGE *page = edascm_to_page (page_s);

  /* If page is not the current page, switch pages, then switch back
   * after closing page. */
  PAGE *curr_page = toplevel->page_current;
  int reset_page = (page != curr_page);
  if (reset_page)
    x_window_set_current_page (w_current, page);

  x_window_close_page (w_current, w_current->toplevel->page_current);

  if (reset_page)
    x_window_set_current_page (w_current, curr_page);

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
 * (gschem core window) module.
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
 * (gschem core window) module.
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
 * \brief Create the (gschem core window) Scheme module
 * \par Function Description
 * Defines procedures in the (gschem core window) module. The module
 * can be accessed using (use-modules (gschem core window)).
 */
static void
init_module_gschem_core_window ()
{
  /* Register the functions */
  #include "g_window.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_current_window, s_active_page, s_set_active_page_x,
                s_override_close_page_x, s_pointer_position,
                s_snap_point, NULL);

  /* Override procedures in the (geda core page) module */
  {
    SCM geda_page_module = scm_c_resolve_module ("geda core page");
    SCM close_page_proc =
      scm_variable_ref (scm_c_lookup (s_override_close_page_x));
    scm_c_module_define (geda_page_module, "%close-page!", close_page_proc);
  }
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
  /* Register gEDA smob type */
  window_smob_tag = scm_make_smob_type ("gschem-window", 0);
  scm_set_smob_free (window_smob_tag, smob_free);
  scm_set_smob_print (window_smob_tag, smob_print);

  /* Create fluid */
  scheme_window_fluid = scm_permanent_object (scm_make_fluid ());

  /* Define the (gschem core window) module */
  scm_c_define_module ("gschem core window",
                       init_module_gschem_core_window,
                       NULL);
}
