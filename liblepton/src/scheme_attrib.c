/* Lepton EDA library - Scheme API
 * Copyright (C) 2010-2011 Peter Brett <peter@peter-b.co.uk>
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
 * \file scheme_attrib.c
 * \brief Scheme API attribute manipulation procedures.
 */

#include <config.h>

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"

SCM_SYMBOL (attribute_format_sym, "attribute-format");

/*! \brief Detach an attribute from an object.
 * \par Function Description
 * Detach \a attrib_s from \a obj_s.  If \a attrib_s is not attached
 * as an attribute, does nothing silently.  If \a attrib_s is attached
 * as an attribute of an object other than \a obj_s, throws a Scheme
 * error.
 *
 * \note Scheme API: Implements the %detach-attrib! procedure of
 * the (lepton core attrib) module.
 *
 * \param obj_s the object from which to detach an attribute.
 * \param attrib_s the attribute to detach.
 * \return \a attrib_s.
 */
SCM_DEFINE (detach_attrib_x, "%detach-attrib!", 2, 0, 0,
            (SCM obj_s, SCM attrib_s), "Detach an attribute to an object.")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_detach_attrib_x);
  SCM_ASSERT (edascm_is_object_type (attrib_s, OBJ_TEXT), attrib_s,
              SCM_ARG2, s_detach_attrib_x);

  LeptonObject *obj = edascm_to_object (obj_s);
  LeptonObject *attrib = edascm_to_object (attrib_s);

  /* If attrib isn't attached, do nothing */
  if (attrib->attached_to == NULL) {
    return obj_s;
  }

  /* Check that attrib isn't attached elsewhere */
  if (attrib->attached_to != obj) {
    scm_error (edascm_object_state_sym, s_detach_attrib_x,
               _("Object ~A is attribute of wrong object"),
               scm_list_1 (attrib_s), SCM_EOL);
  }

  /* Detach object */
  lepton_object_emit_pre_change_notify (attrib);
  o_attrib_remove (&obj->attribs, attrib);
  lepton_object_set_color (attrib, DETACHED_ATTRIBUTE_COLOR);
  lepton_object_emit_change_notify (attrib);

  lepton_object_page_set_changed (obj);

  scm_remember_upto_here_1 (attrib_s);
  return obj_s;
}


/*!
 * \brief Create the (lepton core attrib) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core attrib) module. The module can
 * be accessed using (use-modules (lepton core attrib)).
 */
static void
init_module_lepton_core_attrib (void *unused)
{
  /* Register the functions */
  #include "scheme_attrib.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_detach_attrib_x,
                NULL);
}

/*!
 * \brief Initialise the basic gEDA attribute manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with
 * attributes. Should only be called by edascm_init().
 */
void
edascm_init_attrib ()
{
  /* Define the (lepton core attrib) module */
  scm_c_define_module ("lepton core attrib",
                       (void (*)(void*)) init_module_lepton_core_attrib,
                       NULL);
}
