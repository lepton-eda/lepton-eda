/* Lepton EDA library
 * Copyright (C) 1998, 1999, 2000 Kazu Hirata / Ales Hvezda
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010, 2016 gEDA Contributors
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

#include "config.h"
#include "version.h"

#include "libgeda_priv.h"
#include "liblepton/libgedaguile.h"

/*! \brief Perform runtime initialization of liblepton library.
 *
 *  \par Function Description
 *  This function is responsible for making sure that any runtime
 *  initialization is done for all the liblepton routines. It should
 *  be called before any other liblepton functions are called.
 */
void liblepton_init(void)
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
  if (getenv ("LEPTON_INHIBIT_RC_FILES") == NULL) {
    g_register_libgeda_dirs ();
  }

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



/*! \brief Returns a message to be used in the --version output.
 *  \note  Caller must free() the returned value.
 */
char*
version_message()
{
  const char* msg =
    _("Lepton EDA %s%s.%s (git: %.7s)\n"
    "Copyright (C) 1998-2016 gEDA developers\n"
    "Copyright (C) 2017-2020 Lepton EDA developers\n"
    "This is free software, and you are welcome to redistribute it\n"
    "under certain conditions. For details, see the file `COPYING',\n"
    "which is included in the Lepton EDA distribution.\n"
    "There is NO WARRANTY, to the extent permitted by law.");

  size_t sz = snprintf (NULL, 0, msg,
                        PREPEND_VERSION_STRING,
                        PACKAGE_DOTTED_VERSION,
                        PACKAGE_DATE_VERSION,
                        PACKAGE_GIT_COMMIT);

  char* res = (char*) malloc (++sz);

  snprintf (res, sz, msg,
            PREPEND_VERSION_STRING,
            PACKAGE_DOTTED_VERSION,
            PACKAGE_DATE_VERSION,
            PACKAGE_GIT_COMMIT);

  return res;
}

