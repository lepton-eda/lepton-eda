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

/*! \brief Get the contents of a component object.
 * \par Function Description
 * Retrieves a list of the primitive objects that make up a component object.
 *
 * \note Scheme API: Implements the %component-contents procedure in the
 * (lepton core component) module.
 *
 * \param component_s a component object.
 * \return a list of primitive objects.
 */
SCM_DEFINE (component_contents, "%component-contents", 1, 0, 0,
            (SCM component_s), "Get component object contents.")
{
  SCM_ASSERT (edascm_is_object_type (component_s, OBJ_COMPONENT),
              component_s, SCM_ARG1, s_component_contents);

  LeptonObject *obj = edascm_to_object (component_s);

  if (edascm_is_object_type (component_s, OBJ_COMPONENT)) {
    return edascm_from_object_glist (obj->component->prim_objs);
  } else {
    return SCM_EOL;
  }
}

/*! \brief Add a primitive object to a component object.
 * \par Function Description
 * Adds \a obj_s to \a component_s.  If \a obj_s is already
 * attached to another component object or to a #LeptonPage, or if
 * \a obj_s is itself a component object, throws a Scheme error.
 * If \a obj_s is already attached to \a component_s, does
 * nothing.
 *
 * \note Scheme API: Implements the %component-append! procedure
 * of the (lepton core component) module.
 *
 * \param component_s component object to modify.
 * \param obj_s     primitive object to add.
 * \return \a obj_s.
 */
SCM_DEFINE (component_append_x, "%component-append!", 2, 0, 0,
            (SCM component_s, SCM obj_s),
            "Add a primitive object to a component object")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (edascm_is_object_type (component_s, OBJ_COMPONENT), component_s,
              SCM_ARG1, s_component_append_x);
  SCM_ASSERT ((EDASCM_OBJECTP (obj_s)
               && !edascm_is_object_type (obj_s, OBJ_COMPONENT)),
              obj_s, SCM_ARG2, s_component_append_x);

  LeptonObject *parent = edascm_to_object (component_s);
  LeptonObject *child = edascm_to_object (obj_s);

  LeptonPage* page = lepton_object_get_page (child);
  /* Check that object is not already attached to a page or a
     different component. */
  if ((page != NULL)
      || ((child->parent != NULL) && (child->parent != parent))) {
    scm_error (edascm_object_state_sym,
               s_component_append_x,
               _("Object ~A is already attached to something"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (child->parent == parent) return obj_s;

  /* Object cleanup now managed by C code. */
  edascm_c_set_gc (obj_s, 0);

  /* Don't need to emit change notifications for the child because
   * it's guaranteed not to be present in a page at this point. */
  lepton_object_emit_pre_change_notify (parent);

  parent->component->prim_objs =
    g_list_append (parent->component->prim_objs, child);
  child->parent = parent;

  LeptonPage* parent_page = lepton_object_get_page (parent);
  /* We may need to update connections */
  if (parent_page != NULL) {
    s_conn_update_object (parent_page, child);
  }

  lepton_object_emit_change_notify (parent);

  lepton_object_page_set_changed (parent);

  return component_s;
}

/*! \brief Remove a primitive object from a component object.
 * \par Function Description
 * Removes \a obj_s from \a component_s.  If \a obj_s is attached
 * to a #LeptonPage or to a component object other than \a
 * component_s, throws a Scheme error.  If \a obj_s is unattached,
 * does nothing.
 *
 * \note Scheme API: Implements the %component-remove! procedure of the
 * (lepton core component) module.
 *
 * \param component_s component object to modify.
 * \param obj_s       primitive object to remove.
 * \return \a obj_s.
 */
SCM_DEFINE (component_remove_x, "%component-remove!", 2, 0, 0,
            (SCM component_s, SCM obj_s),
            "Remove a primitive object from a component object")
{
  /* Ensure that the arguments have the correct types. */
  SCM_ASSERT (edascm_is_object_type (component_s, OBJ_COMPONENT), component_s,
              SCM_ARG1, s_component_remove_x);
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s, SCM_ARG2, s_component_remove_x);

  LeptonObject *parent = edascm_to_object (component_s);
  LeptonObject *child = edascm_to_object (obj_s);
  LeptonPage *child_page = lepton_object_get_page (child);

  /* Check that object is not attached to a different component. */
  if ((child->parent != NULL) && (child->parent != parent)) {
    scm_error (edascm_object_state_sym, s_component_remove_x,
               _("Object ~A is attached to a different component"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached to a page. */
  if ((child->parent == NULL) && (child_page != NULL)) {
    scm_error (edascm_object_state_sym, s_component_remove_x,
               _("Object ~A is attached to a page"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object is not attached as an attribute. */
  if (child->attached_to != NULL) {
    scm_error (edascm_object_state_sym, s_component_remove_x,
               _("Object ~A is attached as an attribute"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  /* Check that object doesn't have attributes. */
  if (child->attribs != NULL) {
    scm_error (edascm_object_state_sym, s_component_remove_x,
               _("Object ~A has attributes"),
               scm_list_1 (obj_s), SCM_EOL);
  }

  if (child->parent == NULL) return obj_s;

  /* Don't need to emit change notifications for the child because
   * only the parent will remain in the page. */
  lepton_object_emit_pre_change_notify (parent);

  parent->component->prim_objs =
    g_list_remove_all (parent->component->prim_objs, child);
  child->parent = NULL;

  /* We may need to update connections */
  s_conn_remove_object (child_page, child);
  s_conn_remove_object_connections (child);

  lepton_object_emit_change_notify (parent);

  lepton_object_page_set_changed (parent);

  /* Object cleanup now managed by Guile. */
  edascm_c_set_gc (obj_s, 1);
  return component_s;
}

/*! \brief Get component's symbol full file name.
 *
 * \par Function Description
 * If a component has a symbol file associated with it, get that
 * file's full path.
 *
 * \note Scheme API: Implements the %component-filename procedure in the
 * (lepton core component) module.
 *
 * \param component_s  a component object.
 * \return             symbols's file path or #f.
 */
SCM_DEFINE (component_filename, "%component-filename", 1, 0, 0,
            (SCM component_s),
            "Get component's symbol full file name")
{
  SCM_ASSERT (edascm_is_object_type (component_s, OBJ_COMPONENT),
              component_s,
              SCM_ARG1,
              s_component_filename);

  LeptonObject* obj = edascm_to_object (component_s);
  const CLibSymbol* sym = s_clib_get_symbol_by_name (lepton_component_object_get_basename (obj));

  SCM result = SCM_BOOL_F;

  if (sym != NULL)
  {
    gchar* fname = s_clib_symbol_get_filename (sym);
    if (fname != NULL)
    {
      result = scm_from_utf8_string (fname);
      g_free (fname);
    }
  }

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
                s_set_component_x,
                s_component_info,
                s_component_contents,
                s_component_append_x,
                s_component_remove_x,
                s_component_filename,
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
