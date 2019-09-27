/* Lepton EDA library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010, 2016 gEDA Contributors
 * Copyright (C) 2017-2019 Lepton EDA Contributors
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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRARG_H
#include <stdarg.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#include "libgeda_priv.h"
#include "liblepton/libgedaguile.h"

/*! \brief Perform runtime initialization of libgeda library.
 *  \par Function Description
 *  This function is responsible for making sure that any runtime
 *  initialization is done for all the libgeda routines.  It should
 *  be called before any other libgeda functions are called.
 *
 */
void libgeda_init(void)
{
#ifdef ENABLE_NLS
  /* Initialise gettext */
  bindtextdomain (LIBLEPTON_GETTEXT_DOMAIN, LOCALEDIR);
  bind_textdomain_codeset(LIBLEPTON_GETTEXT_DOMAIN, "UTF-8");
#endif

  eda_paths_init();

  s_clib_init();
  s_menu_init();
  s_attrib_init();
  s_color_init();

  g_register_libgeda_funcs();
  g_register_libgeda_dirs();

  edascm_init ();
}



/*! \brief Add Lepton compiled path to Guile compiled paths env var.
 *  \note  To take effect, must be called before scm_boot_guile().
 */
void
set_guile_compiled_path()
{
  char* path = getenv ("GUILE_LOAD_COMPILED_PATH");
  char buf[ PATH_MAX ] = "";

  if (path != NULL && strlen (path) > 0)
  {
    /* preserve already set $GUILE_LOAD_COMPILED_PATH:
    */
    snprintf (buf, sizeof (buf),
              "%s:%s",
              LEPTON_SCM_PRECOMPILE_DIR, path);
    path = buf;
  }
  else
  {
    path = (char*) LEPTON_SCM_PRECOMPILE_DIR;
  }

  setenv ("GUILE_LOAD_COMPILED_PATH", path, 1);
}

