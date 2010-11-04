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
 * \file scheme_object.c
 * \brief Scheme API object manipulation procedures.
 */

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

SCM_SYMBOL (wrong_type_arg_sym , "wrong-type-arg");

/*! \brief Convert a Scheme object list to a GList.
 * \par Function Description
 * Takes a Scheme list of #OBJECT smobs, and returns a GList
 * containing the objects. If \a objs is not a list of #OBJECT smobs,
 * throws a Scheme error.
 *
 * \warning If the #OBJECT structures in the GList are to be stored by
 * C code and later free()'d directly, the smobs must be marked as
 * unsafe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a Scheme list of #OBJECT smobs.
 * \param [in] subr the name of the Scheme subroutine (used for error
 *                  messages).
 * \return a #GList of #OBJECT.
 */
GList *
edascm_to_object_glist (SCM objs, const char *subr)
{
  GList *result = NULL;
  SCM lst;

  SCM_ASSERT (scm_is_true (scm_list_p (objs)), objs, SCM_ARGn, subr);

  scm_dynwind_begin (0);
  scm_dynwind_unwind_handler ((void (*)(void *))g_list_free, result, 0);

  for (lst = objs; lst != SCM_EOL; lst = SCM_CDR (lst)) {
    SCM smob = SCM_CAR (lst);
    EDASCM_ASSERT_SMOB_VALID (smob);
    if (!EDASCM_OBJECTP (smob)) {
      scm_error_scm (wrong_type_arg_sym,
                     scm_from_locale_string (subr),
                     scm_from_locale_string (_("Expected a gEDA object, found ~A")),
                     scm_list_1 (smob), scm_list_1 (smob));
    }
    result = g_list_prepend (result, (gpointer) edascm_to_object (smob));
  }

  scm_remember_upto_here_1 (lst);

  scm_dynwind_end ();

  return g_list_reverse (result);
}

/*! \brief Convert a GList of objects into a Scheme list.
 * \par Function Description
 * Takes a GList of #OBJECT and returns a Scheme list of corresponding
 * object smobs.
 *
 * \warning If the #OBJECT structures are to be subsequently managed
 * only by Scheme, the smobs in the returned list must be marked as
 * safe for garbage collection (by calling edascm_c_set_gc()).
 *
 * \param [in] objs a #GList of #OBJECT instances.
 * \return a Scheme list of smobs corresponding to each #OBJECT.
 */
SCM
edascm_from_object_glist (const GList *objs)
{
  SCM lst = SCM_EOL;
  SCM rlst;
  GList *iter = (GList *) objs;

  while (iter != NULL) {
    lst = scm_cons (edascm_from_object (iter->data), lst);
    iter = g_list_next (iter);
  }

  rlst = scm_reverse (lst);

  scm_remember_upto_here_1 (lst);
  return rlst;
}
