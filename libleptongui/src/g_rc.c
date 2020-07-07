/* Lepton EDA Schematic Capture
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
#include "version.h"
#include "gschem.h"

/*! \brief Load GTK resource files
 *
 *  \par Function Description
 *  Search system and user configuration directories for
 *  lepton-gtkrc files and load them in sequence.
 */
void
g_rc_parse_gtkrc()
{
  gchar *filename;

  /* Search for the first gschem-gtkrc file in the system
   * configuration path. */
  const gchar * const * sys_dirs = eda_get_system_config_dirs();
  for (gint i = 0; sys_dirs[i]; ++i) {
    filename = g_build_filename (sys_dirs[i], "lepton-gtkrc", NULL);
    if (g_file_test(filename, G_FILE_TEST_EXISTS)) {
      gtk_rc_parse (filename);
    }
    g_free (filename);
  }

  filename = g_build_filename (eda_get_user_config_dir(),
                               "lepton-gtkrc", NULL);
  gtk_rc_parse (filename);
  g_free (filename);
}
