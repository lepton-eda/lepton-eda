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
 * \file scheme_os.c
 * \brief Scheme API functions for misc. OS-related stuff.
 */

#include <config.h>

#include <libgeda_priv.h>
#include <libgedaguile_priv.h>

SCM_SYMBOL (carbon_sym , "carbon");
SCM_SYMBOL (cygwin_sym , "cygwin");
SCM_SYMBOL (linux_sym , "linux");
SCM_SYMBOL (win32_sym , "win32");
SCM_SYMBOL (win32_native_sym , "win32-native");

/*! \brief Get host operating system information.
 * \par Function Description
 * Returns a list of symbols describing the operating system. The
 * symbols may include:
 *
 * - win32 -- Windows
 * - win32-native -- Windows, not via Cygwin
 * - cygwin -- Cygwin
 * - carbon -- Mac OS X Carbon
 * - linux -- Linux
 *
 * \return a list of symbols.
 */
SCM_DEFINE (platform, "%platform", 0, 0, 0, (),
            "Return a list of symbols describing the host platform.")
{
  SCM result = SCM_EOL;

# if defined (OS_CARBON)
  result = scm_cons (carbon_sym, result);
# endif
# if defined (OS_CYGWIN)
  result = scm_cons (cygwin_sym, result);
# endif
# if defined (OS_LINUX)
  result = scm_cons (linux_sym, result);
# endif
# if defined (OS_WIN32)
  result = scm_cons (win32_sym, result);
# endif
# if defined (OS_WIN32_NATIVE)
  result = scm_cons (win32_native_sym, result);
# endif

  return result;
}

/*!
 * \brief Create the (geda core os) Scheme module.
 * \par Function Description
 * Defines procedures in the (geda core os) module. The module can
 * be accessed using (use-modules (geda core os)).
 */
static void
init_module_geda_core_os ()
{
  /* Register the functions and symbols */
  #include "scheme_os.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_platform,
                NULL);
}

/*!
 * \brief Initialise the host platform support procedures.
 * \par Function Description

 * Registers some Scheme procedures that provide cross-platform
 * support. Should only be called by scheme_api_init().
 */
void
edascm_init_os ()
{
  /* Define the (geda core os) module */
  scm_c_define_module ("geda core os",
                       init_module_geda_core_os,
                       NULL);
}
