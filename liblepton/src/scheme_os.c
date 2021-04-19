/* Lepton EDA library -- Scheme API
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
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
 * \file scheme_os.c
 * \brief Scheme API functions for misc. OS-related stuff.
 */

#include <config.h>

#include <liblepton_priv.h>
#include <libleptonguile_priv.h>

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

/*! \brief Get system config directory directories.
 * \par Function Description
 * Returns a list of directories to be searched for system
 * configuration information.
 *
 * \note Scheme API: Implements the %sys-config-dirs procedure in the
 * (lepton core os) module.
 *
 * \return a Scheme list of strings.
 */
SCM_DEFINE (sys_config_dirs, "%sys-config-dirs", 0, 0, 0, (),
            "Return a list of search directories for system configuration.")
{
  const gchar * const * dirs = eda_get_system_config_dirs();
  SCM lst_s = SCM_EOL;

  /* dirs contains raw environment strings, so assume it's in the
   * current locale's encoding. */
  for (gint i = 0; dirs[i]; ++i)
  {
    lst_s = scm_cons(scm_from_locale_string (dirs[i]), lst_s);
  }
  return scm_reverse_x(lst_s, SCM_EOL);
}

/*! \brief Get user data directory.
 * \par Function Description
 * Returns the directory where per-user data should be stored
 *
 * \note Scheme API: Implements the %user-data-dir procedure is the
 * (lepton core os) module.
 *
 * \return a string.
 */
SCM_DEFINE (user_data_dir, "%user-data-dir", 0, 0, 0, (),
            "Return the directory for user data.")
{
  /* eda_get_user_data_dir() returns a raw environment string, so assume
   * it's in the current locale's encoding. */
  return scm_from_locale_string(eda_get_user_data_dir());
}

/*! \brief Get user config directory.
 * \par Function Description
 * Returns the directory where per-user configuration information
 * should be stored
 *
 * \note Scheme API: Implements the %user-config-dir procedure is the
 * (lepton core os) module.
 *
 * \return a string.
 */
SCM_DEFINE (user_config_dir, "%user-config-dir", 0, 0, 0, (),
            "Return the directory for user configuration.")
{
  /* eda_get_user_config_dir() returns a raw environment string, so assume
   * it's in the current locale's encoding. */
  return scm_from_locale_string(eda_get_user_config_dir());
}

/*! \brief Get user cache directory.
 * \par Function Description
 * Returns the directory where per-user cache data should be
 * stored
 *
 * \note Scheme API: Implements the %user-cache-dir procedure is the
 * (lepton core os) module.
 *
 * \return a string.
 */
SCM_DEFINE (user_cache_dir, "%user-cache-dir", 0, 0, 0, (),
            "Return the directory for user cache data.")
{
  /* eda_get_user_cache_dir() returns a raw environment string, so assume
   * it's in the current locale's encoding. */
  return scm_from_locale_string (eda_get_user_cache_dir ());
}


/*!
 * \brief Create the (lepton core os) Scheme module.
 * \par Function Description
 * Defines procedures in the (lepton core os) module. The module can
 * be accessed using (use-modules (lepton core os)).
 */
static void
init_module_lepton_core_os (void *unused)
{
  /* Register the functions and symbols */
  #include "scheme_os.x"

  /* Add them to the module's public definitions. */
  scm_c_export (s_platform,
                s_sys_config_dirs,
                s_user_cache_dir,
                s_user_config_dir,
                s_user_data_dir,
                NULL);
}

/*!
 * \brief Initialise the host platform support procedures.
 * \par Function Description

 * Registers some Scheme procedures that provide cross-platform
 * support. Should only be called by edascm_init().
 */
void
edascm_init_os ()
{
  /* Define the (lepton core os) module */
  scm_c_define_module ("lepton core os",
                       (void (*)(void*)) init_module_lepton_core_os,
                       NULL);
}
