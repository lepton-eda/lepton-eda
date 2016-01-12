/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library - Scheme API
 * Copyright (C) 2013 Peter Brett <peter@peter-b.co.uk>
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
 * \file scheme_closure.c
 * \brief Scheme API support for C closures.
 */

#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

SCM_SYMBOL (lambda_sym, "lambda");
SCM_SYMBOL (args_sym, "args");

static SCM marshal_proc;

/*! \brief Unpack and call a C closure 
 * 
 * Unpack the C function pointer and user data pointer from a C
 * closure \a smob, and then make a function call of the form:
 *
 * \code
 * func (args, user_data)
 * \endcode
 *
 * \param args   Scheme list of closure arguments.
 * \param smob   closure smob containing C function and user data.
 *
 * \return the result returned by the closure.
 */
static SCM
edascm_closure_marshal (SCM args, SCM smob) {
#ifndef NDEBUG
  SCM_ASSERT (EDASCM_CLOSUREP (smob), smob, SCM_ARG2,
              "edascm_closure_marshal");
#endif
  EDASCM_ASSERT_SMOB_VALID (smob);

  SCM (*func)(SCM, gpointer) = (SCM (*)(SCM, gpointer)) SCM_SMOB_DATA (smob);
  gpointer *user_data = (gpointer) SCM_SMOB_DATA_2 (smob);

  return func (args, user_data);
}

/*! \brief Make a C closure.
 * \ingroup guile_c_iface
 * \par Function Description
 * Make and return a Scheme procedure that closes around the provided
 * \a func and \a user_data.
 *
 * The closure that is returned takes an arbitrary number of
 * arguments, and makes a function call of the form:
 *
 * \code
 * func (args, user_data)
 * \endcode
 *
 * where \a args is a Scheme list of arguments passed to the closure.
 *
 * The created closure is not protected from garbage collection;
 * depending on the application, it may be necessary to protect it
 * with scm_gc_protect_object() or scm_permanent_object().
 *
 * \param func      C function to close around.
 * \param user_data closure context
 *
 * \return a newly-created closure, or \c SCM_BOOL_F if an error
 * occurs.
 */
SCM
edascm_c_make_closure (SCM (*func)(SCM, gpointer), gpointer user_data)
{
  SCM smob = SCM_UNDEFINED;
  SCM expr;
  SCM result;
  smob = edascm_from_closure (func, user_data);

  /* (lambda args (marshal args smob)) */
  expr = scm_list_3 (lambda_sym, args_sym,
                     scm_list_3 (marshal_proc, args_sym, smob));
  result = g_scm_eval_protected (expr, scm_current_module ());
  g_warn_if_fail (scm_is_true (scm_procedure_p (result)));
  return result; 
}

/*!
 * \brief Initialise the C closure procedures.
 * \par Function Description
 * Creates some Scheme values used for creating and working with C
 * closures.  Should only be called by edascm_init().
 */
void
edascm_init_closure ()
{
  /* Register functions and symbols */
  #include "scheme_closure.x"

  marshal_proc =
    scm_permanent_object (scm_c_make_gsubr ("edascm_closure_marshal",
                                            2, 0, 0,
                                            edascm_closure_marshal));
}
