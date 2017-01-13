/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"


SCM g_scm_c_get_uref (OBJECT *object)
{
  SCM func = scm_variable_ref (scm_c_lookup ("get-uref"));
  SCM object_smob = edascm_from_object (object);
  SCM exp = scm_list_2 (func, object_smob);

  return g_scm_eval_protected (exp, SCM_UNDEFINED);
}


/*! \brief Indicate the verbosity level for messages.
 * \par Function Description
 * If the "-q" gnetlist command-line option was specified, returns -1.
 * If the "-v" gnetlist command-line option was specified, returns 1.
 * Otherwise, returns 0.
 */
SCM
g_get_verbosity ()
{
  if (verbose_mode) {
    return scm_from_int (1);
  } else if (quiet_mode) {
    return scm_from_int (-1);
  } else {
    return scm_from_int (0);
  }
}

/*! \brief Obtain a list of `-O' backend arguments.
 * \par Function Description
 * Returns a list of arguments passed to the gnetlist backend via the
 * `-O' gnetlist command-line option.
 */
SCM
g_get_backend_arguments()
{
  SCM result = SCM_EOL;
  GSList *iter;

  for (iter = backend_params; iter != NULL; iter = g_slist_next (iter)) {
    result = scm_cons (scm_from_locale_string ((char *) iter->data),
                       result);
  }

  return scm_reverse_x (result, SCM_UNDEFINED);
}


/*
 * This function is in s_rename.c:  SCM g_get_renamed_nets(SCM scm_level)
 */
