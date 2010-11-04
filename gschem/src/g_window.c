/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2010 Peter Brett
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
#include <libgeda/libgedaguile.h>

SCM scheme_window_fluid = SCM_UNDEFINED;

scm_t_bits window_smob_tag;

/*! \brief Free a #GSCHEM_TOPLEVEL smob.
 * \par Function Description
 * Finalizes a window smob for deletion.
 *
 * Used internally to Guile
 */
static size_t
smob_free (SCM smob)
{
  GSCHEM_TOPLEVEL *window = (GSCHEM_TOPLEVEL *) SCM_SMOB_DATA (smob);

  /* If the weak ref has been cleared, do nothing */
  if (window == NULL) return 0;

  /* Otherwise, go away. */
  window->smob = SCM_UNDEFINED;

  return 0;
}

/*! \brief Print a representation of a #GSCHEM_TOPLEVEL smob.
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

/*! \brief Get the smob for a #GSCHEM_TOPLEVEL.
 * \par Function Description
 * Return a smob representing \a window.
 *
 * \param window #GSCHEM_TOPLEVEL to obtain a smob for.
 * \param a smob representing \a window.
 */
SCM
g_scm_from_window (GSCHEM_TOPLEVEL *w_current)
{
  g_assert (w_current != NULL);

  if (w_current->smob == SCM_UNDEFINED) {
    SCM_NEWSMOB (w_current->smob, window_smob_tag, w_current);
  }

  return w_current->smob;
}

/*!
 * \brief Set the #GSCHEM_TOPLEVEL fluid in the current dynamic context.
 * \par Function Description
 *
 * This function must be used inside a pair of calls to
 * scm_dynwind_begin() and scm_dynwind_end().  During the dynwind
 * context, the #GSCHEM_TOPLEVEL fluid is set to \a w_current.
 *
 * \param [in] w_current The new GSCHEM_TOPLEVEL pointer.
 */
void
g_dynwind_window (GSCHEM_TOPLEVEL *w_current)
{
  SCM window_s = g_scm_from_window (w_current);
  scm_dynwind_fluid (scheme_window_fluid, window_s);
  edascm_dynwind_toplevel (w_current->toplevel);
}

/*!
 * \brief Get the value of the #GSCHEM_TOPLEVEL fluid.
 * \par Function Description
 * Return the value of the #GSCHEM_TOPLEVEL fluid in the current
 * dynamic context.
 */
SCM_DEFINE (current_window, "%current-window", 0, 0, 0,
            (),
            "Get the GSCHEM_TOPLEVEL for the current dynamic context.")
{
  return scm_fluid_ref (scheme_window_fluid);
}

/*!
 * \brief Get the value of the #GSCHEM_TOPLEVEL fluid.
 * \par Function Description
 * Return the value of the #GSCHEM_TOPLEVEL fluid in the current dynamic
 * context.
 */
GSCHEM_TOPLEVEL *
g_current_window ()
{
  SCM window_s = current_window ();

  if (!(SCM_SMOB_PREDICATE (window_smob_tag, window_s)
        &&  ((void *)SCM_SMOB_DATA (window_s) != NULL))) {
    scm_misc_error (NULL, "Found invalid gschem window smob ~S",
                    scm_list_1 (window_s));
  }

  return (GSCHEM_TOPLEVEL *) SCM_SMOB_DATA (window_s);
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
 * \param page_s
 * \return \a page_s.
 */
SCM_DEFINE (set_active_page, "%set-active-page!", 1, 0, 0,
            (SCM page_s), "Set the active page.")
{
  SCM_ASSERT (edascm_is_page (page_s), page_s, SCM_ARG1, s_set_active_page);

  PAGE *page = edascm_to_page (page_s);
  x_window_set_current_page (g_current_window (), page);

  return page_s;
}

/*!
 * \brief Create the (gschem core window) Scheme module
 * \par Function Description
 * Defines procedures in the (gschem core window) module. The module
 * can be accessed using (use-modules (geda core toplevel)).
 */
static void
init_module_gschem_core_window ()
{
  /* Register the functions */
  #include "g_window.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_current_window, s_active_page, s_set_active_page, NULL);
}

/*!
 * \brief Initialise the GSCHEM_TOPLEVEL manipulation procedures.
 * \par Function Description

 * Registers some Scheme procedures for working with #GSCHEM_TOPLEVEL
 * smobs and creates the #GSCHEM_TOPLEVEL fluid. Should only be called
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

  /* Define the (geda core toplevel) module */
  scm_c_define_module ("gschem core window",
                       init_module_gschem_core_window,
                       NULL);
}
