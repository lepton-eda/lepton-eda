/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2010 Peter Brett <peter@peter-b.co.uk>
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

/*!
 * \file scheme_complex.c
 * \brief Scheme API complex object manipulation procedures.
 */

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

/*! \brief Create a new complex object.
 * \par Function Description
 * Creates a new, empty complex object, with the given \a basename and
 * with all other parameters set to default values.  It is initially set
 * to be embedded.
 *
 * \note Scheme API: Implements the %make-complex procedure in the
 * (geda core complex) module.
 *
 * \return a newly-created complex object.
 */
SCM_DEFINE (make_complex, "%make-complex", 1, 0, 0,
            (SCM basename_s), "Create a new complex object.")
{
  SCM_ASSERT (scm_is_string (basename_s), basename_s, SCM_ARG1, s_make_complex);

  char *tmp = scm_to_locale_string (basename_s);
  OBJECT *obj = o_complex_new_embedded (edascm_c_current_toplevel (),
                                        OBJ_COMPLEX, DEFAULT_COLOR, 0, 0, 0,
                                        FALSE, tmp, TRUE);
  free (tmp);

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Instantiate a complex object from the component library.
 * \par Function Description

 * Searches the component library for a component with the given \a
 * basename.  If found, creates a new complex object by instantiating
 * that library component.  It is initially set to be unembedded.  If
 * no match is found for \a basename in the library, returns
 * SCM_BOOL_F.
 *
 * \note Scheme API: Implements the %make-complex/library procedure in
 * the (geda core complex) module.
 *
 * \param basename component name to search for in the component
 *                 library.
 * \return a newly-created complex object.
 */
SCM_DEFINE (make_complex_library, "%make-complex/library", 1, 0, 0,
            (SCM basename_s),
            "Instantiate a complex object from the component library.")
{
  SCM_ASSERT (scm_is_string (basename_s), basename_s, SCM_ARG1,
              s_make_complex_library);

  char *basename = scm_to_locale_string (basename_s);
  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler (free, basename, SCM_F_WIND_EXPLICITLY);

  SCM result = SCM_BOOL_F;
  const CLibSymbol *clib = s_clib_get_symbol_by_name (basename);
  if (clib != NULL) {
    OBJECT *obj = o_complex_new (edascm_c_current_toplevel (),
                                 OBJ_COMPLEX, DEFAULT_COLOR, 0, 0, 0,
                                 FALSE, clib, basename, TRUE);

    result = edascm_from_object (obj);

    /* At the moment, the only pointer to the object is owned by the
     * smob. */
    edascm_c_set_gc (result, TRUE);
  }

  scm_dynwind_end ();
  return result;
}

/*! \brief Set complex object parameters.
 * \par Function Description
 * Modifies the complex object \a complex_s by setting its parameters
 * to new values.
 *
 * \note Scheme API: Implements the %set-complex! procedure in the
 * (geda core complex) module.
 *
 * \param complex_s the complex object to modify.
 * \param x_s       the new x-coordinate of the complex object.
 * \param y_s       the new y-coordinate of the complex object.
 * \param angle_s   the new rotation angle.
 * \param mirror_s  whether the complex object should be mirrored.
 * \param locked_s  whether the complex object should be locked.
 *
 * \return the modified \a complex_s.
 */
SCM_DEFINE (set_complex, "%set-complex!", 6, 0, 0,
            (SCM complex_s, SCM x_s, SCM y_s, SCM angle_s, SCM mirror_s,
             SCM locked_s), "Set complex object parameters")
{
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, s_set_complex);
  SCM_ASSERT (scm_is_integer (x_s),     x_s,     SCM_ARG2, s_set_complex);
  SCM_ASSERT (scm_is_integer (y_s),     y_s,     SCM_ARG3, s_set_complex);
  SCM_ASSERT (scm_is_integer (angle_s), angle_s, SCM_ARG4, s_set_complex);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *obj = edascm_to_object (complex_s);

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
    scm_misc_error (s_set_complex,
                    _("Invalid complex angle ~A. Must be 0, 90, 180, or 270 degrees"),
                    scm_list_1 (angle_s));
  }

  o_emit_pre_change_notify (toplevel, obj);

  int x = scm_to_int (x_s);
  int y = scm_to_int (y_s);
  o_translate_world (toplevel,
                     x - obj->complex->x,
                     y - obj->complex->y,
                     obj);
  obj->complex->angle = angle;
  obj->complex->mirror = scm_is_true (mirror_s);
  obj->sel_func = scm_is_true (locked_s) ? NULL : select_func;

  o_complex_recalc (toplevel, obj); /* We need to do this explicitly... */

  o_emit_change_notify (toplevel, obj);

  o_page_changed (toplevel, obj);

  return complex_s;
}

/*! \brief Get complex object parameters.
 * \par Function Description
 * Retrieves the parameters of a complex object. The return value is a
 * list of parameters:
 *
 * -# Basename
 * -# Base x-coordinate.
 * -# Base y-coordinate.
 * -# Rotation angle.
 * -# Whether object is mirrored.
 * -# Whether object is locked.
 *
 * \note Scheme API: Implements the %complex-info procedure in the
 * (geda core complex) module.
 *
 * \param complex_s the complex object to inspect.
 * \return a list of complex object parameters.
 */
SCM_DEFINE (complex_info, "%complex-info", 1, 0, 0,
            (SCM complex_s), "Get complex object parameters.")
{
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, s_complex_info);

  OBJECT *obj = edascm_to_object (complex_s);

  return scm_list_n (scm_from_locale_string (obj->complex_basename),
                     scm_from_int (obj->complex->x),
                     scm_from_int (obj->complex->y),
                     scm_from_int (obj->complex->angle),
                     obj->complex->mirror ? SCM_BOOL_T : SCM_BOOL_F,
                     (obj->sel_func == NULL) ? SCM_BOOL_T : SCM_BOOL_F,
                     SCM_UNDEFINED);
}

/*! \brief Get the contents of a complex object.
 * \par Function Description
 * Retrieves a list of the primitive objects that make up a complex object.
 *
 * \note Scheme API: Implements the %complex-contents procedure in the
 * (geda core complex) module.
 *
 * \param complex_s a complex object.
 * \return a list of primitive objects.
 */
SCM_DEFINE (complex_contents, "%complex-contents", 1, 0, 0,
            (SCM complex_s), "Get complex object contents.")
{
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, s_complex_contents);

  OBJECT *obj = edascm_to_object (complex_s);

  return edascm_from_object_glist (obj->complex->prim_objs);
}

/*! \brief Add a primitive object to a complex object.
 * \par Function Description
 * Adds \a obj_s to \a complex_s.  If \a obj_s is already attached to
 * another complex object or to a #PAGE, or if \a obj_s is itself a
 * complex object, throws a Scheme error.  If \a obj_s is already
 * attached to \a complex_s, does nothing.
 *
 * \note Scheme API: Implements the %complex-append! procedure of the
 * (geda core complex) module.
 *
 * \param complex_s complex object to modify.
 * \param obj_s     primitive object to add.
 * \return \a obj_s.
 */
SCM_DEFINE (complex_append, "%complex-append!", 2, 0, 0,
            (SCM complex_s, SCM obj_s),
            "Add a primitive object to a complex object")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, s_complex_append);
  SCM_ASSERT ((EDASCM_OBJECTP (obj_s)
               && !edascm_is_object_type (obj_s, OBJ_COMPLEX)
               && !edascm_is_object_type (obj_s, OBJ_PLACEHOLDER)),
              obj_s, SCM_ARG2, s_complex_append);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *parent = edascm_to_object (complex_s);
  OBJECT *child = edascm_to_object (obj_s);

  /* Check that object is not already attached to a page or a
     different complex. */
  if ((o_get_page (toplevel, child) != NULL)
      || ((child->parent != NULL) && (child->parent != parent))) {
    scm_error (edascm_object_state_sym,
               s_complex_append,
               _("Object ~A is already attached to something"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (child->parent == parent) return obj_s;

  /* Object cleanup now managed by C code. */
  edascm_c_set_gc (obj_s, 0);

  /* Don't need to emit change notifications for the child because
   * it's guaranteed not to be present in a page at this point. */
  o_emit_pre_change_notify (toplevel, parent);

  parent->complex->prim_objs =
    g_list_append (parent->complex->prim_objs, child);
  child->parent = parent;

  o_complex_recalc (toplevel, parent);

  /* We may need to update connections */
  s_tile_update_object (toplevel, child);
  s_conn_update_object (toplevel, child);

  o_emit_change_notify (toplevel, parent);

  o_page_changed (toplevel, parent);

  return obj_s;
}

/*! \brief Remove a primitive object from a complex object.
 * \par Function Description
 * Removes \a obj_s from \a complex_s.  If \a obj_s is attached to a
 * #PAGE or to a complex object other than \a complex_s, throws a
 * Scheme error.  If \a obj_s is unattached, does nothing.
 *
 * \note Scheme API: Implements the %complex-remove! procedure of the
 * (geda core complex) module.
 *
 * \param complex_s complex object to modify.
 * \param obj_s     primitive object to remove.
 * \return \a obj_s.
 */
SCM_DEFINE (complex_remove, "%complex-remove!", 2, 0, 0,
            (SCM complex_s, SCM obj_s),
            "Remove a primitive object from a complex object")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (edascm_is_object_type (complex_s, OBJ_COMPLEX), complex_s,
              SCM_ARG1, s_complex_remove);
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s, SCM_ARG2, s_complex_remove);

  TOPLEVEL *toplevel = edascm_c_current_toplevel ();
  OBJECT *parent = edascm_to_object (complex_s);
  OBJECT *child = edascm_to_object (obj_s);
  PAGE *child_page = o_get_page (toplevel, child);

  /* Check that object is not attached to a different complex. */
  if ((child->parent != NULL) && (child->parent != parent)) {
    scm_error (edascm_object_state_sym, s_complex_remove,
               _("Object ~A is attached to a different complex"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached to a page. */
  if ((child->parent == NULL) && (child_page != NULL)) {
    scm_error (edascm_object_state_sym, s_complex_remove,
               _("Object ~A is attached to a page"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached as an attribute. */
  if (child->attached_to != NULL) {
    scm_error (edascm_object_state_sym, s_complex_remove,
               _("Object ~A is attached as an attribute"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object doesn't have attributes. */
  if (child->attribs != NULL) {
    scm_error (edascm_object_state_sym, s_complex_remove,
               _("Object ~A has attributes"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (child->parent == NULL) return obj_s;

  /* Don't need to emit change notifications for the child because
   * only the parent will remain in the page. */
  o_emit_pre_change_notify (toplevel, parent);

  parent->complex->prim_objs =
    g_list_remove_all (parent->complex->prim_objs, child);
  child->parent = NULL;

  /* We may need to update connections */
  s_tile_remove_object (child);
  s_conn_remove_object (toplevel, child);

  o_emit_change_notify (toplevel, parent);

  o_page_changed (toplevel, parent);

  /* Object cleanup now managed by Guile. */
  edascm_c_set_gc (obj_s, 1);
  return obj_s;
}

/*!
 * \brief Create the (geda core complex) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core complex) module. The module can
 * be accessed using (use-modules (geda core complex)).
 */
static void
init_module_geda_core_complex ()
{
  /* Register the functions and symbols */
  #include "scheme_complex.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_make_complex, s_make_complex_library, s_set_complex,
                s_complex_info, s_complex_contents, s_complex_append,
                s_complex_remove, NULL);
}

/*!
 * \brief Initialise the basic gEDA complex object manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with complex #OBJECT
 * smobs. Should only be called by scheme_api_init().
 */
void
edascm_init_complex ()
{
  /* Define the (geda core object) module */
  scm_c_define_module ("geda core complex",
                       init_module_geda_core_complex,
                       NULL);
}
