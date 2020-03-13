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

/*! \brief */
struct gsubr_t {
  const char* name;
  int req;
  int opt;
  int rst;
  SCM (*fnc)();
};

/*! \brief */
static struct gsubr_t libgeda_funcs[] = {
  { "eval-protected",            1, 1, 0, (SCM (*) ()) g_scm_eval_protected },
  { "eval-string-protected",     1, 0, 0, (SCM (*) ()) g_scm_eval_string_protected },

  { "rc-filename",               0, 0, 0, (SCM (*) ()) g_rc_rc_filename },
  { "parse-rc",                  2, 0, 0, (SCM (*) ()) g_rc_parse_rc },
  { NULL,                        0, 0, 0, NULL } };

/*! \brief Register all libgeda functions with scheme.
 *  \par Function Description
 *  Creates g_subr_t objects to make g_rc_* functions that are defined
 *  in g_rc.c visible to Scheme.
 */
void g_register_libgeda_funcs (void)
{
  struct gsubr_t *tmp = libgeda_funcs;
  
  while (tmp->name != NULL) {
    scm_c_define_gsubr (tmp->name, tmp->req, tmp->opt, tmp->rst, (scm_t_subr) tmp->fnc);
    tmp++;
  }
  
}

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
