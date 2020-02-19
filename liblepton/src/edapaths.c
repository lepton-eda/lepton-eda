/* Lepton EDA library
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>

#if !defined(G_OS_WIN32)
#	include <libgen.h>
#endif

#include "libgeda_priv.h"

/* For convenience in this file only! */
#if !defined(LEPTONDATADIR)
#	define LEPTONDATADIR NULL
#endif
#if !defined(LEPTONRCDIR)
#	define LEPTONRCDIR LEPTONDATADIR
#endif

static const gchar* const DATA_ENV        = "GEDADATA";
static const gchar* const CONFIG_ENV      = "GEDADATARC";
static const gchar* const DATA_XDG_SUBDIR = "lepton-eda";

#if defined(ENABLE_DEPRECATED)
static const gchar* const DATA_GUESS_FILE = "scheme/geda.scm";
static const gchar* const USER_DOTDIR     = ".gEDA";
#endif

/* ================================================================
 * Private initialisation functions
 * ================================================================ */

/*! \brief Get the Lepton EDA installation's data directory.
 * Attempt to find the "share/lepton-eda" directory in the prefix
 * where Lepton was installed.  Returns NULL if not on Windows and
 * not a relocatable build (in that case, the install location
 * should have been compiled in via configure options) */
static const gchar *
guess_install_data_dir(void)
{
	static const gchar *install_dir = NULL;
	static gsize is_init = 0;
	if (g_once_init_enter(&is_init)) {
		gchar *tmp_dir = NULL;

#if defined(G_OS_WIN32)
		/* The installation data directory should be the last element in
		 *  g_get_system_data_dirs(). */
		static const gchar * const *sys_dirs;
		for (gint i = 0; sys_dirs[i]; ++i, tmp_dir = sys_dirs[i]);

#endif

		/* Check that the directory actually exists */
		if (tmp_dir && !g_file_test(tmp_dir, G_FILE_TEST_IS_DIR)) {
			g_free(tmp_dir);
		} else {
			install_dir = tmp_dir;
		}

		g_once_init_leave(&is_init, 1);
	}
	return install_dir;
}

/*! \brief Copy search paths into result structure.
 * Helper function that duplicated search paths from the various
 * sources into an output buffer and returns the number of paths.  If
 * the output pointer is NULL, doesn't copy, but still returns the
 * number of paths.  Only exists for the use of
 * build_search_list(). */
static gsize
copy_search_list(const gchar **output,
                 const gchar * const * env_names,
                 const gchar * const * xdg_dirs,
                 const gchar * const * cfg_dirs)
{
	/* Copy in the entries.  Duplicate everything so the result is fully
	 * owned by us. */
	gsize copied = 0;

#if defined(ENABLE_DEPRECATED)
	/* If one of the environment variables is set, it takes precedence. */
	for (gsize i = 0; env_names && env_names[i]; ++i) {
		const gchar *env_value = g_getenv(env_names[i]);
		if (!env_value) continue;

		if (output) {
			output[0] = g_strdup(env_value);
			output[1] = NULL;
		}
		/* The first environment variable found overrides all. */
		return 2;
	}
#endif /* ENABLE_DEPRECATED */

	/* Otherwise, use the "lepton-eda" subdirectory of the
	 * configured standard directories. */
	for (gsize i = 0; xdg_dirs && xdg_dirs[i]; ++i) {
		/* Append "lepton-eda" to each XDG data path */
		if (output) {
			output[copied] =
				g_build_filename(xdg_dirs[i],
				                 DATA_XDG_SUBDIR, NULL);
		}
		++copied;
	}

#if !defined(G_OS_WIN32)
	/* Append the configured variables to the list */
	for (gsize i = 0; cfg_dirs && cfg_dirs[i]; ++i) {
		if (output) {
			output[copied] = g_strdup(cfg_dirs[i]);
		}
		++copied;
	}
#endif

	/* Append the guessed install directory to the list, if it's not in
	 * there already. */
	const gchar *install_dir = guess_install_data_dir();
	if (install_dir) {
		if (output) {
			/* Only append to search path if not already an element.
			 * N.b. we could use g_strv_contains() here, but it's only
			 * available in quite recent versions of GLib.*/
			gboolean found = FALSE;
			for (guint i = 0; !found && i < copied; ++i) {
				found = (0 == strcmp(install_dir, output[i]));
			}

			if (!found) {
				output[copied] = g_strdup(install_dir);
				++copied;
			}
		} else {
			/* Allow space just in case */
			++copied;
		}
	}

	if (output) output[copied] = NULL;
	++copied;

	return copied;
}

/*! \brief Build a set of search paths.
 * Build a null-terminated array of constant strings, using the input
 * parameters.
 *
 * \param env_names NULL-terminated list of environment variable
 *                  names.  If any of them is set, it is the only item
 *                  in the returned list.
 * \param xdg_dirs  NULL-terminated list of directory names.  All
 *                  of them have Lepton's XDG subdirectory
 *                  (i.e. "lepton-eda") appended to them and are
 *                  added to the list in order.
 * \param cfg_dirs NULL terminated list of additional directories to
 *                 append to the list.
 * \return a newly-allocated, NULL-terminated array of strings.
 */
static const gchar **
build_search_list(const gchar * const * env_names,
                  const gchar * const * xdg_dirs,
                  const gchar * const * cfg_dirs)
{
	
	gsize count = 0;
	const gchar **result = NULL;

	while (TRUE) {
		/* The first time copy_search_list() is called, it returns the
		 * length of buffer required. */
		count = copy_search_list(result, env_names, xdg_dirs, cfg_dirs);

		if (result) break;
		result = g_new0(const gchar *, count);
	}

	return result;
}

/* \brief Initialise backwards-compatible environment variables.
 * Ensure that the $GEDADATA environment variable is always set to
 * something sensible within Lepton programs.
 */
static void
eda_paths_init_env(void)
{
#if defined(ENABLE_DEPRECATED)
	static gsize is_init = 0;

	/* If $GEDADATA is not set, find a directory containing
	 * scheme/geda.scm and set $GEDADATA to it.  Note that this
	 * *doesn't* use guess_install_data_dir() because the user might
	 * have deliberately set $XDG_DATA_DIRS to put other directories
	 * ahead of the installation data directory. */
	if (g_once_init_enter(&is_init)) {

		gboolean found = FALSE;
		const gchar * const *dirs = eda_get_system_data_dirs();

		for (int i = 0; dirs[i]; ++i) {

			gchar *guess_file =
				g_build_filename(dirs[i], DATA_GUESS_FILE, NULL);
			found = g_file_test(guess_file, G_FILE_TEST_IS_REGULAR);
			g_free(guess_file);

			if (!found) continue;

			g_setenv(DATA_ENV, dirs[i], FALSE);
			break;
		}

		gsize setup = 1;
		g_once_init_leave(&is_init, setup);
	}
#endif /* ENABLE_DEPRECATED */
}

static gchar *
get_user_dotdir(void)
{
#if defined(ENABLE_DEPRECATED)
	gchar *dotdir = g_build_filename(g_get_home_dir(),
	                                 USER_DOTDIR, NULL);
	if (g_file_test(dotdir, G_FILE_TEST_IS_DIR)) {
		return dotdir;
	}
	g_free(dotdir);
#endif /* ENABLE_DEPRECATED */
	return NULL;
}

/* ================================================================
 * Public accessors
 * ================================================================ */

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



const gchar *
eda_get_user_data_dir(void)
{
	static gchar *user_data_dir;

	if (g_once_init_enter(&user_data_dir)) {
		gchar *dir = get_user_dotdir();

		if (!dir) {
			dir = g_build_filename(g_get_user_data_dir(),

			                       DATA_XDG_SUBDIR, NULL);
		}

		g_once_init_leave(&user_data_dir, dir);
	}

	return user_data_dir;
}

const gchar *
eda_get_user_config_dir(void)
{
	static gchar *user_config_dir;

	if (g_once_init_enter(&user_config_dir)) {
		gchar *dir = get_user_dotdir();

		if (!dir) {
			dir = g_build_filename(g_get_user_config_dir(),
			                       DATA_XDG_SUBDIR, NULL);
		}

		g_once_init_leave(&user_config_dir, dir);
	}

	return user_config_dir;
}

const gchar*
eda_get_user_cache_dir()
{
  static gchar* user_cache_dir = NULL;

  if (g_once_init_enter (&user_cache_dir))
  {
    gchar* dir = g_build_filename (g_get_user_cache_dir(),
                                   DATA_XDG_SUBDIR, NULL);

    g_once_init_leave (&user_cache_dir, dir);
  }

  return user_cache_dir;
}

/* ================================================================
 * Module initialisation
 * ================================================================ */

/*!
 * \brief Initialise Lepton EDA data and configuration search paths.
 * \par Function Description
 * Compute and initialise configuration and data search paths used by
 * Lepton, and set any related environment variables.  Should only be
 * called (once) by liblepton_init().
 */
void
eda_paths_init(void)
{
	/* Force initialisation */
	eda_get_system_data_dirs();
	eda_get_system_config_dirs();
	eda_get_user_data_dir();
	eda_get_user_config_dir();
	eda_get_user_cache_dir();

	eda_paths_init_env();
}
