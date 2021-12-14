/* Lepton EDA library
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

#include <config.h>
#include "liblepton_priv.h"



#ifdef DEBUG
static void
dbg_out_dirs (const gchar* const* dirs, const gchar* name)
{
  fprintf (stderr, " >> %s:\n", name);
  for (const gchar* const* pp = dirs; *pp != NULL; ++pp)
  {
    fprintf (stderr, "      [%s]\n", *pp);
  }
  fprintf (stderr, "\n");
}
#endif



/*! \brief Get an ordered list of system-wide data directories.
 *
 *  \return Immutable NULL-terminated string array.
 */
const gchar* const*
eda_get_system_data_dirs()
{
  static const gchar** system_data_dirs;

  if (g_once_init_enter (&system_data_dirs))
  {
    const gchar* const* dirs = g_get_system_data_dirs();

    /* determine [dirs] array size:
     */
    int count = 0;
    for ( ; dirs[count] != NULL; ++count )
    {
    }

    ++ count; /* for LEPTONDATADIR */
    ++ count; /* for terminating NULL element */


    const gchar** result = g_new0 (const gchar*, count);

    int ndx = 0;
    for ( ; dirs[ ndx ] != NULL; ++ndx )
    {
      result[ ndx ] = g_build_filename (dirs[ ndx ], PACKAGE, NULL);
    }

    result[ ndx ]   = LEPTONDATADIR; /* defined in config.h */
    result[ ++ndx ] = NULL;

#ifdef DEBUG
    fprintf (stderr, "\n");
    dbg_out_dirs (result, "eda_get_system_data_dirs()");
#endif

    g_once_init_leave (&system_data_dirs, result);
  }

  return system_data_dirs;

} /* eda_get_system_data_dirs() */



/*! \brief Get an ordered list of system-wide configuration directories.
 *
 *  \return Immutable NULL-terminated string array.
 */
const gchar* const*
eda_get_system_config_dirs()
{
  static const gchar** system_config_dirs;

  if (g_once_init_enter (&system_config_dirs))
  {
    const gchar* const* dirs = g_get_system_config_dirs();

    /* determine [dirs] array size:
     */
    int count = 0;
    for ( ; dirs[count] != NULL; ++count )
    {
    }

    ++ count; /* for LEPTONDATADIR */
    ++ count; /* for terminating NULL element */


    const gchar** result = g_new0 (const gchar*, count);

    int ndx = 0;
    for ( ; dirs[ ndx ] != NULL; ++ndx )
    {
      result[ ndx ] = g_build_filename (dirs[ ndx ], PACKAGE, NULL);
    }

    result[ ndx ]   = LEPTONDATADIR; /* defined in config.h */
    result[ ++ndx ] = NULL;

#ifdef DEBUG
    fprintf (stderr, "\n");
    dbg_out_dirs (result, "eda_get_system_config_dirs()");
#endif

    g_once_init_leave (&system_config_dirs, result);
  }

  return system_config_dirs;

} /* eda_get_system_config_dirs() */



/*! \brief Get user's data directory (e.g. ~/.local/share/lepton-eda).
 */
const gchar*
eda_get_user_data_dir()
{
  static gchar* user_data_dir;

  if (g_once_init_enter (&user_data_dir))
  {
    gchar* dir = g_build_filename (g_get_user_data_dir(),
                                   PACKAGE, NULL);

    g_once_init_leave (&user_data_dir, dir);
  }

  return user_data_dir;
}



/*! \brief Get user's configuration directory (e.g. ~/.config/lepton-eda).
 */
const gchar*
eda_get_user_config_dir()
{
  static gchar* user_config_dir;

  if (g_once_init_enter (&user_config_dir))
  {
    gchar* dir = g_build_filename (g_get_user_config_dir(),
                                   PACKAGE, NULL);

    g_once_init_leave (&user_config_dir, dir);
  }

  return user_config_dir;
}



/*! \brief Get user's cache directory (e.g. ~/.cache/lepton-eda).
 */
const gchar*
eda_get_user_cache_dir()
{
  static gchar* user_cache_dir = NULL;

  if (g_once_init_enter (&user_cache_dir))
  {
    gchar* dir = g_build_filename (g_get_user_cache_dir(),
                                   PACKAGE, NULL);

    g_once_init_leave (&user_cache_dir, dir);
  }

  return user_cache_dir;
}



/*! \brief Initialise Lepton EDA data and configuration search paths.
 *
 * \par Function Description
 * Should only be called (once) by liblepton_init().
 */
void
eda_paths_init()
{
  /* These functions store their data in static local variables.
   * Calling them here forces data initialization:
  */
  eda_get_system_data_dirs();
  eda_get_system_config_dirs();
  eda_get_user_data_dir();
  eda_get_user_config_dir();
  eda_get_user_cache_dir();
}
