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
  scm_c_export (NULL);
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
