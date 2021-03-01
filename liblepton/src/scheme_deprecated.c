/* Lepton EDA library - Scheme API
 * Copyright (C) 1998-2013 gEDA Contributors
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
 * \file scheme_deprecated.c
 * \brief Deprecated Scheme API functions
 */
#include <config.h>

#include "libgeda_priv.h"
#include "libleptonguile_priv.h"

/*! \brief Get the width of line used to draw an object
 * \par Function Description
 * Returns the line width used to draw an object. Deprecated because
 * it doesn't respect type restrictions, unlike the %object-stroke
 * function in (lepton core object).
 *
 * \param obj_s the object to get line width for.
 * \return the line width.
 */
SCM_DEFINE (get_line_width, "%get-line-width", 1, 0, 0,
            (SCM obj_s), "Get the width of line used to draw an object")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_get_line_width);

  LeptonObject *object = edascm_to_object (obj_s);

  return scm_from_int(object->line_width);
}

/*!
 * \brief Create the (lepton core deprecated) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core deprecated) module. The module can
 * be accessed using (use-modules (lepton core deprecated)).
 */
static void
init_module_lepton_core_deprecated (void *unused)
{
  /* Register the functions */
  #include "scheme_deprecated.x"

  /* Some other deprecated definitions */
  scm_c_define("OBJ_LINE",    SCM_MAKE_CHAR((unsigned char) OBJ_LINE));
  scm_c_define("OBJ_BOX",     SCM_MAKE_CHAR((unsigned char) OBJ_BOX));
  scm_c_define("OBJ_PICTURE", SCM_MAKE_CHAR((unsigned char) OBJ_PICTURE));
  scm_c_define("OBJ_CIRCLE",  SCM_MAKE_CHAR((unsigned char) OBJ_CIRCLE));
  scm_c_define("OBJ_NET",     SCM_MAKE_CHAR((unsigned char) OBJ_NET));
  scm_c_define("OBJ_BUS",     SCM_MAKE_CHAR((unsigned char) OBJ_BUS));
  scm_c_define("OBJ_COMPLEX", SCM_MAKE_CHAR((unsigned char) OBJ_COMPONENT));
  scm_c_define("OBJ_TEXT",    SCM_MAKE_CHAR((unsigned char) OBJ_TEXT));
  scm_c_define("OBJ_PIN",     SCM_MAKE_CHAR((unsigned char) OBJ_PIN));
  scm_c_define("OBJ_ARC",     SCM_MAKE_CHAR((unsigned char) OBJ_ARC));
  scm_c_define("OBJ_PLACEHOLDER", SCM_MAKE_CHAR((unsigned char) OBJ_PLACEHOLDER));
  scm_c_define("OBJ_PATH",    SCM_MAKE_CHAR((unsigned char) OBJ_PATH));

  /* Add them to the module's public definitions. */
  scm_c_export (s_get_line_width, "OBJ_LINE", "OBJ_BOX", "OBJ_PICTURE",
                "OBJ_CIRCLE", "OBJ_NET", "OBJ_BUS", "OBJ_COMPLEX", "OBJ_TEXT",
                "OBJ_PIN", "OBJ_ARC", "OBJ_PATH", "OBJ_PLACEHOLDER", NULL);
}

/*!
 * \brief Initialise the deprecated Lepton EDA / gEDA procedures.
 * \par Function Description
 * Should only be called by edascm_init().
 */
void
edascm_init_deprecated ()
{
  /* Define the (lepton core deprecated) module */
  scm_c_define_module ("lepton core deprecated",
                       (void (*)(void*)) init_module_lepton_core_deprecated,
                       NULL);
}
