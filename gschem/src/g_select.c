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

SCM_SYMBOL (object_state_sym, "object-state");

/*! \brief Get a list of selected objects on a page.
 * \par Function Description
 * Retrieve a list of selected objects on \a page_s.
 *
 * \note Scheme API: Implements the %page-selection procedure in the
 * (gschem core selection) module.
 *
 * \param page_s #PAGE smob for the page from which to get the selection.
 * \return a list of #OBJECT smobs.
 */
SCM_DEFINE (page_selection, "%page-selection", 1, 0, 0,
            (SCM page_s), "Get a list of a page's selected objects")
{
  /* Ensure that the argument is a page smob */
  SCM_ASSERT (edascm_is_page (page_s), page_s,
              SCM_ARG1, s_page_selection);

  PAGE *page = edascm_to_page (page_s);
  GList *iter;
  SCM result = SCM_EOL;
  for (iter = geda_list_get_glist (page->selection_list);
       iter != NULL; iter = g_list_next (iter)) {
    result = scm_cons (edascm_from_object ((OBJECT *) iter->data), result);
  }

  return result;
}

/*! \brief Select an object.
 * \par Function Description
 * Add \a obj_s to its associated page's selection.  If \a obj_s is
 * not included directly in a page (i.e. inclusion in a component is
 * not permitted), throws a Scheme error.  If \a obj_s is already
 * selected, does nothing.
 *
 * \note Scheme API: Implements the %select-object! procedure in the
 * (gschem core selection) module.
 *
 * \param obj_s #OBJECT smob for object to be selected.
 * \return obj_s.
 */
SCM_DEFINE (select_object_x, "%select-object!", 1, 0, 0,
            (SCM obj_s), "Select an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, s_select_object_x);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (obj_s);
  PAGE *page = o_get_page (toplevel, obj);
  if ((page == NULL) || (obj->parent != NULL)) {
    scm_error (object_state_sym,
               s_select_object_x,
               _("Object ~A is not directly included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (!obj->selected) {
    o_selection_add (toplevel, page->selection_list, obj);
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
 * (gschem core selection) module.
 *
 * \param obj_s #OBJECT smob for object to be deselected.
 * \return obj_s.
 */
SCM_DEFINE (deselect_object_x, "%deselect-object!", 1, 0, 0,
            (SCM obj_s), "Deselect an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, s_deselect_object_x);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (obj_s);
  PAGE *page = o_get_page (toplevel, obj);
  if ((page == NULL) || (obj->parent != NULL)) {
    scm_error (object_state_sym,
               s_deselect_object_x,
               _("Object ~A is not directly included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (obj->selected) {
    o_selection_remove (toplevel, page->selection_list, obj);
  }

  return obj_s;
}

/*! \brief Test if an object is selected.
 * \par Function Description
 * If \a obj_s is selected, returns SCM_BOOL_T.  Otherwise, returns
 * SCM_BOOL_F.  If \a obj_s is not included directly in a page
 * (i.e. not via inclusion in a component), throws a Scheme error.
 *
 * \note Scheme API: Implements the %object-selected? procedure in the
 * (gschem core selection) module.
 *
 * \param obj_s #OBJECT smob to be tested.
 * \return SCM_BOOL_T if \a obj_s is selected, otherwise SCM_BOOL_F.
 */
SCM_DEFINE (object_selected_p, "%object-selected?", 1, 0, 0,
            (SCM obj_s), "Test if an object is selected.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, s_object_selected_p);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (obj_s);
  PAGE *page = o_get_page (toplevel, obj);
  if ((page == NULL) || (obj->parent != NULL)) {
    scm_error (object_state_sym,
               s_object_selected_p,
               _("Object ~A is not directly included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }
  return (obj->selected ? SCM_BOOL_T : SCM_BOOL_F);
}

/*! \brief Create the (gschem core selection) Scheme module
 * \par Function Description
 * Defines procedures in the (gschem core selection) module. The module
 * can be accessed using (use-modules (gschem core selection)).
 */
static void
init_module_gschem_core_select ()
{
  /* Register the functions */
  #include "g_select.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_page_selection, s_select_object_x, s_deselect_object_x,
                s_object_selected_p, NULL);
}

/*! \brief Initialise the selection manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with the selection.
 * Should only be called by main_prog().
 */
void
g_init_select ()
{
  /* Define the (gschem core selection) module */
  scm_c_define_module ("gschem core selection",
                       init_module_gschem_core_select,
                       NULL);
}
