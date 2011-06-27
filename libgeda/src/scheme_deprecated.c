/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

/*!
 * \file scheme_init.c
 * Deprecated Scheme API functions
 */
#include <config.h>

#include "libgeda_priv.h"
#include "libgedaguile_priv.h"

/*! \brief Get the width of line used to draw an object
 * \par Function Description
 * Returns the line width used to draw an object. Deprecated because
 * it doesn't respect type restrictions, unlike the %object-stroke
 * function in (geda core object).
 *
 * \param obj_s the object to get line width for.
 * \return the line width.
 */
SCM_DEFINE (get_line_width, "%get-line-width", 1, 0, 0,
            (SCM obj_s), "Get the width of line used to draw an object")
{
  SCM_ASSERT (EDASCM_OBJECTP (obj_s), obj_s,
              SCM_ARG1, s_get_line_width);

  OBJECT *object = edascm_to_object (obj_s);

  return scm_from_int(object->line_width);
}

/*!
 * \brief Create the (geda core deprecated) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core deprecated) module. The module can
 * be accessed using (use-modules (geda core deprecated)).
 */
static void
init_module_geda_core_deprecated ()
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
  scm_c_define("OBJ_COMPLEX", SCM_MAKE_CHAR((unsigned char) OBJ_COMPLEX));
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
 * \brief Initialise the basic gEDA page manipulation procedures.
 * \par Function Description
 * Registers some Scheme procedures for working with #PAGE
 * smobs. Should only be called by scheme_api_init().
 */
void
edascm_init_deprecated ()
{
  /* Define the (geda core page) module */
  scm_c_define_module ("geda core deprecated",
                       init_module_geda_core_deprecated,
                       NULL);
}
