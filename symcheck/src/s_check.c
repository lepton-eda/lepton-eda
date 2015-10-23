/* Lepton EDA
 * lepton-symcheck - Lepton Symbol Checker
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301 USA.
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <liblepton/liblepton.h>
#include <liblepton/libgedaguile.h>

#include "../include/globals.h"
#include "../include/prototype.h"
#include "../include/gettext.h"


/*! \brief Output state of the quiet_mode variable
 * \par Function Description
 * Outputs current state of the quiet_mode variable
 *
 * \return SCM_BOOL_F if the quiet_mode variable is 0, else return SCM_BOOL_T
 */
SCM_DEFINE (check_get_quiet_mode, "%check-get-quiet-mode", 0, 0, 0,
            (), "Get state of the quiet mode flag")
{
  return scm_from_bool (quiet_mode);
}

/*! \brief Output state of the verbose_mode variable
 * \par Function Description
 * Outputs current state of the verbose_mode variable
 *
 * \return SCM_BOOL_F if the verbose_mode variable is 0, else return SCM_BOOL_T
 */

SCM_DEFINE (check_get_verbose_mode, "%check-get-verbose-mode", 0, 0, 0,
            (), "Get state of the verbose mode flag")
{
  return scm_from_int (verbose_mode);
}

static void
init_module_symbol_core_check ()
{
  /* Register the functions */
  #include "s_check.x"

  /* Register the functions and add them to the module's public
   * definitions. */
  scm_c_export (s_check_get_quiet_mode,
                s_check_get_verbose_mode,
                NULL);
}

void
s_init_check ()
{
  /* Define the (symbol core check) module */
  scm_c_define_module ("symbol core check",
                       (void (*)(void*)) init_module_symbol_core_check,
                       NULL);
}
