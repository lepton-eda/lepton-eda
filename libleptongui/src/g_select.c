/* Lepton EDA Schematic Capture
 * Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
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

SCM_SYMBOL (object_state_sym, "object-state");

/*! \brief Select an object.
 * \par Function Description
 * Add \a obj_s to its associated page's selection.  If \a obj_s is
 * not included directly in a page (i.e. inclusion in a component is
 * not permitted), throws a Scheme error.  If \a obj_s is already
 * selected, does nothing.
 *
 * \note Scheme API: Implements the %select-object! procedure in the
 * (schematic core selection) module.
 *
 * \param obj_s #LeptonObject smob for object to be selected.
 * \return obj_s.
 */
SCM_DEFINE (select_object_x, "%select-object!", 1, 0, 0,
            (SCM obj_s), "Select an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, s_select_object_x);

  LeptonObject *obj = edascm_to_object (obj_s);
  LeptonPage *page = lepton_object_get_page (obj);
  if ((page == NULL) ||
      (lepton_object_get_parent (obj) != NULL))
  {
    scm_error (object_state_sym,
               s_select_object_x,
               _("Object ~A is not directly included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (!lepton_object_get_selected (obj))
  {
    o_selection_add (lepton_page_get_selection_list (page), obj);
  }

  return obj_s;
}

/*! \brief Deselect an object.
 * \par Function Description
 * Remove \a obj_s from its associated page's selection.  If \a obj_s
 * is not included directly in a page (i.e. not via inclusion in a
 * component), throws a Scheme error.  If \a obj_s is not selected,
 * does nothing.
 *
 * \note Scheme API: Implements the %deselect-object! procedure in the
 * (schematic core selection) module.
 *
 * \param obj_s #LeptonObject smob for object to be deselected.
 * \return obj_s.
 */
SCM_DEFINE (deselect_object_x, "%deselect-object!", 1, 0, 0,
            (SCM obj_s), "Deselect an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, s_deselect_object_x);

  LeptonObject *obj = edascm_to_object (obj_s);
  LeptonPage *page = lepton_object_get_page (obj);
  if ((page == NULL) ||
      (lepton_object_get_parent (obj) != NULL))
  {
    scm_error (object_state_sym,
               s_deselect_object_x,
               _("Object ~A is not directly included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (lepton_object_get_selected (obj))
  {
    o_selection_remove (lepton_page_get_selection_list (page), obj);
  }

  return obj_s;
}

/*! \brief Create the (schematic core selection) Scheme module
 * \par Function Description
 * Defines procedures in the (schematic core selection) module. The module
 * can be accessed using (use-modules (schematic core selection)).
 */
static void
init_module_schematic_core_select (void *unused)
{
  /* Register the functions */
  #include "g_select.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_select_object_x, s_deselect_object_x,
                NULL);
}

/*! \brief Initialise the selection manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with the selection.
 * Should only be called by main_prog().
 */
void
g_init_select ()
{
  /* Define the (schematic core selection) module */
  scm_c_define_module ("schematic core selection",
                       (void (*)(void*)) init_module_schematic_core_select,
                       NULL);
}
