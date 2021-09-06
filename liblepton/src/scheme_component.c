/* Lepton EDA library - Scheme API
 * Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
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
 * \file scheme_component.c
 * \brief Scheme API component object manipulation procedures.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

/*! \brief Instantiate a component object from the component library.
 * \par Function Description

 * Searches the component library for a component with the given \a
 * basename.  If found, creates a new component object by instantiating
 * that library component.  It is initially set to be unembedded.  If
 * no match is found for \a basename in the library, returns
 * SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %make-component/library procedure in
 * the (lepton core component) module.
 *
 * \param basename component name to search for in the component
 *                 library.
 * \return a newly-created component object.
 */
SCM_DEFINE (make_component_library, "%make-component/library", 1, 0, 0,
            (SCM basename_s),
            "Instantiate a component object from the component library.")
{
  SCM_ASSERT (scm_is_string (basename_s), basename_s, SCM_ARG1,
              s_make_component_library);

  char *basename = scm_to_utf8_string (basename_s);
  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  scm_dynwind_unwind_handler (free, basename, SCM_F_WIND_EXPLICITLY);

  LeptonToplevel *toplevel = edascm_c_current_toplevel ();

  SCM result = SCM_BOOL_F;
  const CLibSymbol *clib = s_clib_get_symbol_by_name (basename);
  if (clib != NULL) {
    LeptonObject *obj = lepton_component_new (toplevel->page_current,
                                              default_color_id(),
                                              0,
                                              0,
                                              0,
                                              FALSE,
                                              clib,
                                              basename,
                                              TRUE);

    result = edascm_from_object (obj);

    /* At the moment, the only pointer to the object is owned by the
     * smob. */
    edascm_c_set_gc (result, TRUE);
  }

  scm_dynwind_end ();
  return result;
}


/*!
 * \brief Create the (lepton core component) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core component) module. The module can
 * be accessed using (use-modules (lepton core component)).
 */
static void
init_module_lepton_core_component (void *unused)
{
  /* Register the functions and symbols */
  #include "scheme_component.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_make_component_library,
                NULL);
}

/*!
 * \brief Initialise the basic Lepton EDA component object manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with component
 * #LeptonObject smobs. Should only be called by edascm_init().
 */
void
edascm_init_component ()
{
  /* Define the (lepton core component) module */
  scm_c_define_module ("lepton core component",
                       (void (*)(void*)) init_module_lepton_core_component,
                       NULL);
}
