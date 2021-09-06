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

/*! \brief Set component object parameters.
 * \par Function Description
 * Modifies the component object \a component_s by setting its parameters
 * to new values.
 *
 * \note Scheme API: Implements the %set-component! procedure in the
 * (lepton core component) module.
 *
 * \param component_s the component object to modify.
 * \param x_s       the new x-coordinate of the component object.
 * \param y_s       the new y-coordinate of the component object.
 * \param angle_s   the new rotation angle.
 * \param mirror_s  whether the component object should be mirrored.
 * \param locked_s  whether the component object should be locked.
 *
 * \return the modified \a component_s.
 */
SCM_DEFINE (set_component_x, "%set-component!", 6, 0, 0,
            (SCM component_s, SCM x_s, SCM y_s, SCM angle_s, SCM mirror_s,
             SCM locked_s), "Set component object parameters")
{
  SCM_ASSERT (edascm_is_object_type (component_s, OBJ_COMPONENT), component_s,
              SCM_ARG1, s_set_component_x);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, s_set_component_x);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, s_set_component_x);
  SCM_ASSERT (scm_is_integer (angle_s), angle_s, SCM_ARG4, s_set_component_x);
  SCM_ASSERT (scm_is_bool (mirror_s), mirror_s,  SCM_ARG5, s_set_component_x);
  SCM_ASSERT (scm_is_bool (locked_s), locked_s,  SCM_ARG6, s_set_component_x);

  LeptonObject *obj = edascm_to_object (component_s);

  /* Angle */
  int angle = scm_to_int (angle_s);
  switch (angle) {
  case 0:
  case 90:
  case 180:
  case 270:
    /* These are all fine. */
    break;
  default:
    /* Otherwise, not fine. */
    scm_misc_error (s_set_component_x,
                    _("Invalid component angle ~A. Must be 0, 90, 180, or 270 degrees"),
                    scm_list_1 (angle_s));
  }

  lepton_object_emit_pre_change_notify (obj);

  int x = scm_to_int (x_s);
  int y = scm_to_int (y_s);
  lepton_object_translate (obj, x - obj->component->x, y - obj->component->y);
  obj->component->angle = angle;
  obj->component->mirror = scm_is_true (mirror_s);
  obj->selectable = scm_is_false (locked_s);

  lepton_object_emit_change_notify (obj);

  lepton_object_page_set_changed (obj);

  return component_s;
}

/*! \brief Get component object parameters.
 * \par Function Description
 * Retrieves the parameters of a component object. The return value is a
 * list of parameters:
 *
 * -# Basename
 * -# Base x-coordinate.
 * -# Base y-coordinate.
 * -# Rotation angle.
 * -# Whether object is mirrored.
 * -# Whether object is locked.
 *
 * \note Scheme API: Implements the %component-info procedure in the
 * (lepton core component) module.
 *
 * \param component_s the component object to inspect.
 * \return a list of component object parameters.
 */
SCM_DEFINE (component_info, "%component-info", 1, 0, 0,
            (SCM component_s), "Get component object parameters.")
{
  SCM_ASSERT (edascm_is_object_type (component_s, OBJ_COMPONENT),
              component_s,
              SCM_ARG1, s_component_info);

  LeptonObject *obj = edascm_to_object (component_s);

  return scm_list_n (scm_from_utf8_string (obj->component_basename),
                     scm_from_int (obj->component->x),
                     scm_from_int (obj->component->y),
                     scm_from_int (obj->component->angle),
                     obj->component->mirror ? SCM_BOOL_T : SCM_BOOL_F,
                     obj->selectable ? SCM_BOOL_F : SCM_BOOL_T,
                     SCM_UNDEFINED);
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
                s_set_component_x,
                s_component_info,
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
