/* Lepton EDA library - Scheme API
 * Copyright (C) 2010-2012 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2011-2016 gEDA Contributors
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
 * \file scheme_object.c
 * \brief Scheme API object manipulation procedures.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

SCM_SYMBOL (moveto_sym , "moveto");
SCM_SYMBOL (lineto_sym , "lineto");
SCM_SYMBOL (curveto_sym , "curveto");
SCM_SYMBOL (closepath_sym , "closepath");

/*! \brief Convert a Scheme object list to a GList.
 * \par Function Description
 * Takes a Scheme list of #LeptonObject smobs, and returns a GList
 * containing the objects. If \a objs is not a list of #LeptonObject smobs,
 * throws a Scheme error.
 *
 * \warning If the #LeptonObject structures in the GList are to be stored by
 * C code and later free()'d directly, the smobs must be marked as
 * unsafe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a Scheme list of #LeptonObject smobs.
 * \param [in] subr the name of the Scheme subroutine (used for error
 *                  messages).
 * \return a #GList of #LeptonObject.
 */
GList *
edascm_to_object_glist (SCM objs, const char *subr)
{
  GList *result = NULL;
  SCM lst;

  SCM_ASSERT (scm_is_true (scm_list_p (objs)), objs, SCM_ARGn, subr);

  scm_dynwind_begin ((scm_t_dynwind_flags) 0);
  scm_dynwind_unwind_handler ((void (*)(void *)) g_list_free,
                              result,
                              (scm_t_wind_flags) 0);

  for (lst = objs; !scm_is_null (lst); lst = SCM_CDR (lst)) {
    SCM smob = SCM_CAR (lst);
    result = g_list_prepend (result, (gpointer) edascm_to_object (smob));
  }

  scm_remember_upto_here_1 (lst);

  scm_dynwind_end ();

  return g_list_reverse (result);
}

/*! \brief Convert a GList of objects into a Scheme list.
 * \par Function Description
 * Takes a GList of #LeptonObject and returns a Scheme list of corresponding
 * object smobs.
 *
 * \warning If the #LeptonObject structures are to be subsequently managed
 * only by Scheme, the smobs in the returned list must be marked as
 * safe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a #GList of #LeptonObject instances.
 * \return a Scheme list of smobs corresponding to each #LeptonObject.
 */
SCM
edascm_from_object_glist (const GList *objs)
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *iter = (GList *) objs;

  while (iter != NULL) {
    lst = scm_cons (edascm_from_object ((LeptonObject*) iter->data), lst);
    iter = g_list_next (iter);
  }

  rlst = scm_reverse (lst);

  scm_remember_upto_here_1 (lst);
  return rlst;
}

/*! \brief Test if an object smob is of a particular type.
 * \par Function Description
 * Checks if \a smob contains an #LeptonObject of the given \a type. This is
 * intended to be used by C-based Scheme procedures for working with
 * particular object types.
 *
 * \param [in] smob Scheme value to check type for.
 * \param [in] type Type to check against (e.g. OBJ_LINE).
 * \return non-zero if \a smob is an #LeptonObject smob of \a type.
 */
int
edascm_is_object_type (SCM smob, int type)
{
  if (!EDASCM_OBJECTP(smob)) return 0;

  LeptonObject *obj = edascm_to_object (smob);
  return (lepton_object_get_type (obj) == type);
}

/*! \brief Get objects that are connected to an object.
 * \par Function Description
 * Returns a list of all objects directly connected to \a obj_s.  If
 * \a obj_s is not included in a page, throws a Scheme error.  If \a
 * obj_s is not a pin, net, bus, or component object, returns the empty
 * list.
 *
 * \note Scheme API: Implements the %object-connections procedure of
 * the (lepton core object) module.
 *
 * \param obj_s #LeptonObject smob for object to get connections for.
 * \return a list of #LeptonObject smobs.
 */
SCM_DEFINE (object_connections, "%object-connections", 1, 0, 0,
            (SCM obj_s), "Get objects that are connected to an object.")
{
  /* Ensure that the argument is an object smob */
  SCM_ASSERT (edascm_is_object (obj_s), obj_s,
              SCM_ARG1, s_object_connections);

  LeptonObject *obj = edascm_to_object (obj_s);
  if (lepton_object_get_page (obj) == NULL) {
    scm_error (edascm_object_state_sym,
               s_object_connections,
               _("Object ~A is not included in a page."),
               scm_list_1 (obj_s), SCM_EOL);
  }

  GList *lst = s_conn_return_others (NULL, obj);
  SCM result = edascm_from_object_glist (lst);
  g_list_free (lst);
  return result;
}

/*! \brief Make a new, empty path object.
 * \par Function Description
 * Creates a new, empty path object with default color, stroke and
 * fill options.
 *
 * \note Scheme API: Implements the %make-path procedure in the
 * (lepton core object) module.
 *
 * \return a newly-created path object.
 */
SCM_DEFINE (make_path, "%make-path", 0, 0, 0,
            (), "Create a new path object")
{
  LeptonObject *obj = lepton_path_object_new (default_color_id(), "");

  SCM result = edascm_from_object (obj);

  /* At the moment, the only pointer to the object is owned by the
   * smob. */
  edascm_c_set_gc (result, TRUE);

  return result;
}

/*! \brief Get the number of elements in a path.
 * \par Function Description
 * Retrieves the number of path elements in the path object \a obj_s.
 *
 * \note Scheme API: Implements the %path-length procedure in the
 * (lepton core object) module.
 *
 * \param obj_s #LeptonObject smob for path object to inspect.
 * \return The number of path elements in \a obj_s.
 */
SCM_DEFINE (path_length, "%path-length", 1, 0, 0,
            (SCM obj_s), "Get number of elements in a path object.")
{
  /* Ensure that the argument is a path object */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_length);

  LeptonObject *obj = edascm_to_object (obj_s);
  return scm_from_int (lepton_path_object_get_num_sections (obj));
}

/*! \brief Get one of the elements from a path.
 * \par Function Description
 * Retrieves a path element at index \a index_s from the path object
 * \a obj_s.  If \a index_s is not a valid index, raises a Scheme
 * "out-of-range" error.
 *
 * The return value is a list.  The first element in the list is a
 * symbol indicating the type of path element ("moveto", "lineto",
 * "curveto" or "closepath"), and the remainder of the list contains
 * zero or more control point coordinates, depending on the type of
 * path element.  Each element is evaluated relative to the current
 * path position.
 *
 * - moveto: x and y coordinates of position to step to.
 * - lineto: x and y coordinates of straight line endpoint.
 * - curveto: coordinates of first Bezier control point; coordinates
 *   of second control point; and coordinates of curve endpoint.
 * - closepath: No coordinate parameters.
 *
 * All coordinates are absolute.
 *
 * \note Scheme API: Implements the %path-ref procedure in the
 * (lepton core object) module.
 *
 * \param obj_s   #LeptonObject smob of path object to get element from.
 * \param index_s Index of element to retrieve from \a obj_s
 * \return A list containing the requested path element data.
 */
SCM_DEFINE (path_ref, "%path-ref", 2, 0, 0,
            (SCM obj_s, SCM index_s),
            "Get a path element from a path object.")
{
  /* Ensure that the arguments are a path object and integer */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_ref);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, s_path_ref);

  LeptonObject *obj = edascm_to_object (obj_s);
  int idx = scm_to_int (index_s);

  /* Check index is valid for path */
  if ((idx < 0) || (idx >= lepton_path_object_get_num_sections (obj)))
  {
    scm_out_of_range (s_path_ref, index_s);
  }

  LeptonPathSection *section = &obj->path->sections[idx];

  switch (section->code) {
  case PATH_MOVETO:
  case PATH_MOVETO_OPEN:
    return scm_list_3 (moveto_sym,
                       scm_from_int (section->x3),
                       scm_from_int (section->y3));
  case PATH_LINETO:
    return scm_list_3 (lineto_sym,
                       scm_from_int (section->x3),
                       scm_from_int (section->y3));
  case PATH_CURVETO:
    return scm_list_n (curveto_sym,
                       scm_from_int (section->x1),
                       scm_from_int (section->y1),
                       scm_from_int (section->x2),
                       scm_from_int (section->y2),
                       scm_from_int (section->x3),
                       scm_from_int (section->y3),
                       SCM_UNDEFINED);
  case PATH_END:
    return scm_list_1 (closepath_sym);
  default:
    scm_misc_error (s_path_ref,
                    _("Path object ~A has invalid element type ~A at index ~A"),
                    scm_list_3 (obj_s, scm_from_int (section->code), index_s));
  }

}

/*! \brief Remove an element from a path.
 * \par Function Description
 * Removes the path element at index \a index_s from the path object
 * \a obj_s. If \a index_s is not a valid index, raises a Scheme
 * "out-of-range" error.
 *
 * \note Scheme API: Implements the %path-remove! procedure in the
 * (lepton core object) module.
 *
 * \param obj_s   #LeptonObject smob of path object to remove element from.
 * \param index_s Index of element to remove from \a obj_s.
 * \return \a obj_s.
 */
SCM_DEFINE (path_remove_x, "%path-remove!", 2, 0, 0,
            (SCM obj_s, SCM index_s),
            "Remove a path element from a path object.")
{
  /* Ensure that the arguments are a path object and integer */
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_remove_x);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, s_path_remove_x);

  LeptonObject *obj = edascm_to_object (obj_s);
  int idx = scm_to_int (index_s);

  if ((idx < 0) || (idx >= lepton_path_object_get_num_sections (obj)))
  {
    /* Index is valid for path */
    scm_out_of_range (s_path_remove_x, index_s);
  }

  /* This is the same object pointer.  Just avoid warnings on
   * unused function result by using the assignment. */
  obj = lepton_path_object_remove_section (obj, idx);

  lepton_object_page_set_changed (obj);

  return obj_s;
}

/*! \brief Insert an element into a path.
 * \par Function Description
 * Inserts a path element into the path object \a obj_s at index \a
 * index_s.  The type of element to be inserted is specified by the
 * symbol \a type_s, and the remaining optional integer arguments
 * provide as many absolute coordinate pairs as are required by that
 * element type:
 *
 * - "closepath" elements require no coordinate arguments;
 * - "moveto" and "lineto" elements require one coordinate pair, for
 *   the endpoint;
 * - "curveto" elements require the coordinates of the first control
 *   point, coordinates of the second control point, and coordinates
 *   of the endpoint.
 *
 * If the index is negative, or is greater than or equal to the number
 * of elements currently in the path, the new element will be appended
 * to the path.
 *
 * \note Scheme API: Implements the %path-insert! procedure of the
 * (lepton core object) module.
 *
 * \param obj_s   #LeptonObject smob for the path object to modify.
 * \param index_s Index at which to insert new element.
 * \param type_s  Symbol indicating what type of element to insert.
 * \param x1_s    X-coordinate of first coordinate pair.
 * \param y1_s    Y-coordinate of first coordinate pair.
 * \param x2_s    X-coordinate of second coordinate pair.
 * \param y2_s    Y-coordinate of second coordinate pair.
 * \param x3_s    X-coordinate of third coordinate pair.
 * \param y3_s    Y-coordinate of third coordinate pair.
 * \return \a obj_s.
 */
SCM_DEFINE (path_insert_x, "%path-insert", 3, 6, 0,
            (SCM obj_s, SCM index_s, SCM type_s,
             SCM x1_s, SCM y1_s, SCM x2_s, SCM y2_s, SCM x3_s, SCM y3_s),
            "Insert a path element into a path object.")
{
  SCM_ASSERT (edascm_is_object_type (obj_s, OBJ_PATH), obj_s,
              SCM_ARG1, s_path_insert_x);
  SCM_ASSERT (scm_is_integer (index_s), index_s, SCM_ARG2, s_path_insert_x);
  SCM_ASSERT (scm_is_symbol (type_s), type_s, SCM_ARG3, s_path_insert_x);

  LeptonObject *obj = edascm_to_object (obj_s);
  LeptonPathSection section = {(PATH_CODE) 0, 0, 0, 0, 0, 0, 0};
  int idx = scm_to_int (index_s);

  /* Check & extract path element type. */
  if      (scm_is_eq (type_s, closepath_sym)) { section.code = PATH_END;     }
  else if (scm_is_eq (type_s, moveto_sym))    { section.code = PATH_MOVETO;  }
  else if (scm_is_eq (type_s, lineto_sym))    { section.code = PATH_LINETO;  }
  else if (scm_is_eq (type_s, curveto_sym))   { section.code = PATH_CURVETO; }
  else {
    scm_misc_error (s_path_insert_x,
                    _("Invalid path element type ~A."),
                    scm_list_1 (type_s));
  }

  /* Check the right number of coordinates have been provided. */
  switch (section.code) {
  case PATH_CURVETO:
    SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG4, s_path_insert_x);
    section.x1 = scm_to_int (x1_s);
    SCM_ASSERT (scm_is_integer (y1_s), y1_s, SCM_ARG5, s_path_insert_x);
    section.y1 = scm_to_int (y1_s);
    SCM_ASSERT (scm_is_integer (x2_s), x2_s, SCM_ARG6, s_path_insert_x);
    section.x2 = scm_to_int (x2_s);
    SCM_ASSERT (scm_is_integer (y2_s), y2_s, SCM_ARG7, s_path_insert_x);
    section.y2 = scm_to_int (y2_s);
    SCM_ASSERT (scm_is_integer (x3_s), x3_s, 8, s_path_insert_x);
    section.x3 = scm_to_int (x3_s);
    SCM_ASSERT (scm_is_integer (y3_s), y3_s, 9, s_path_insert_x);
    section.y3 = scm_to_int (y3_s);
    break;
  case PATH_MOVETO:
  case PATH_MOVETO_OPEN:
  case PATH_LINETO:
    SCM_ASSERT (scm_is_integer (x1_s), x1_s, SCM_ARG4, s_path_insert_x);
    section.x3 = scm_to_int (x1_s);
    SCM_ASSERT (scm_is_integer (y1_s), y1_s, SCM_ARG5, s_path_insert_x);
    section.y3 = scm_to_int (y1_s);
    break;
  case PATH_END:
    break;
  }

  lepton_path_object_insert_section (obj, &section, idx);

  lepton_object_page_set_changed (obj);

  return obj_s;
}

/*!
 * \brief Create the (lepton core object) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core object) module. The module can
 * be accessed using (use-modules (lepton core object)).
 */
static void
init_module_lepton_core_object (void *unused)
{
  /* Register the functions and symbols */
  #include "scheme_object.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_object_connections,
                s_make_path,
                s_path_length,
                s_path_ref,
                s_path_remove_x,
                s_path_insert_x,
                NULL);
}

/*!
 * \brief Initialise the basic Lepton EDA object manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #LeptonObject
 * smobs. Should only be called by edascm_init().
 */
void
edascm_init_object ()
{
  /* Define the (lepton core object) module */
  scm_c_define_module ("lepton core object",
                       (void (*) (void*)) init_module_lepton_core_object,
                       NULL);
}
