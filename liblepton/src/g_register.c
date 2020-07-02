/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"


/*! \brief Enable scheme loading from a shared data directory
 * \par Function Description
 * Helper function for g_register_libgeda_dirs().
 */
static void
g_register_scheme_data_dir (const gchar *data_dir)
{
  gchar *scheme_dir = g_build_filename (data_dir, "scheme", NULL);
  scheme_directory (scm_from_locale_string (scheme_dir));
  g_free (scheme_dir);
}

/*! \brief Register some libgeda directories with Scheme.
 * \par Function Description
 * Ensures that the gEDA Scheme directories are added to the Guile
 * load path.
 */
void
g_register_libgeda_dirs (void)
{
  const gchar * const *sys_dirs = eda_get_system_data_dirs();
  for (gint i = 0; sys_dirs[i]; ++i) {
    g_register_scheme_data_dir (sys_dirs[i]);

#ifdef DEBUG
    fprintf (stderr, " >> g_register_libgeda_dirs(): [%s]\n", sys_dirs[i]);
#endif
  }
  g_register_scheme_data_dir (eda_get_user_data_dir());
}
