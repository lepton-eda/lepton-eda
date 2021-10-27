/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*! \file g_rc.c
 *  \brief Execute Scheme initialisation files.
 *
 * Contains functions to open, parse and manage Scheme initialisation
 * (RC) files.
 */

#include <config.h>

#include <errno.h>
#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "liblepton_priv.h"
#include "libleptonguile_priv.h"


/*! \brief Mark an RC file as loaded.
 * \par Function Description
 * If the Scheme initialisation file \a filename has not already been
 * loaded, mark it as loaded and return TRUE, storing \a filename in
 * \a toplevel (\a filename should not subsequently be freed).
 * Otherwise, return FALSE, and set \a err appropriately.
 *
 * \note Should only be called by g_rc_parse_file().
 *
 * \param toplevel  The current #LeptonToplevel structure.
 * \param filename  The RC file name to test.
 * \param err       Return location for errors, or NULL.
 * \return TRUE if \a filename not already loaded, FALSE otherwise.
 */
static gboolean
g_rc_try_mark_read (LeptonToplevel *toplevel,
                    gchar *filename,
                    GError **err)
{
  GList *found = NULL;
  g_return_val_if_fail ((toplevel != NULL), FALSE);
  g_return_val_if_fail ((filename != NULL), FALSE);

  /* Test if marked read already */
  found = g_list_find_custom (toplevel->RC_list, filename,
                              (GCompareFunc) strcmp);
  if (found != NULL) {
    g_set_error (err, EDA_ERROR, EDA_ERROR_RC_TWICE,
                 _("RC file already loaded"));
    return FALSE;
  }

  toplevel->RC_list = g_list_append (toplevel->RC_list, filename);
  /* N.b. don't free name_norm here; it's stored in the LeptonToplevel. */
  return TRUE;
}

/*! \brief Load an RC file.
 * \par Function Description
 * Load and run the Scheme initialisation file \a rcfile, reporting
 * errors via \a err.
 *
 * \param toplevel  The current #LeptonToplevel structure.
 * \param rcfile    The filename of the RC file to load.
 * \param cfg       The configuration context to use while loading.
 * \param err       Return location for errors, or NULL;
 * \return TRUE on success, FALSE on failure.
 */
static gboolean
g_rc_parse_file (LeptonToplevel *toplevel,
                 const gchar *rcfile,
                 EdaConfig *cfg,
                 GError **err)
{
  gchar *name_norm = NULL;
  GError *tmp_err = NULL;
  gboolean status = FALSE;
  g_return_val_if_fail ((toplevel != NULL), FALSE);
  g_return_val_if_fail ((rcfile != NULL), FALSE);

  /* If no configuration file was specified, get the default
   * configuration file for the rc file. */
  if (cfg == NULL) {
    cfg = eda_config_get_context_for_path (rcfile);
  }
  /* If the configuration wasn't loaded yet, attempt to load
   * it. Config loading is on a best-effort basis; if we fail, just
   * print a warning. */
  if (!eda_config_is_loaded (cfg)) {
    eda_config_load (cfg, &tmp_err);
    if (tmp_err != NULL &&
        !g_error_matches (tmp_err, G_IO_ERROR, G_IO_ERROR_NOT_FOUND))
      g_warning (_("Failed to load config from '%1$s': %2$s\n"),
                 eda_config_get_filename (cfg), tmp_err->message);
    g_clear_error (&tmp_err);
  }

  /* Normalise filename */
  name_norm = f_normalize_filename (rcfile, err);
  if (name_norm == NULL) return FALSE;

  /* Attempt to load the RC file, if it hasn't been loaded already.
   * If g_rc_try_mark_read() succeeds, it stores name_norm in
   * toplevel, so we *don't* free it. */
  status = (g_rc_try_mark_read (toplevel, name_norm, &tmp_err)
            && g_read_file (toplevel, name_norm, &tmp_err));

  if (status) {
    g_message (_("Loaded RC file [%1$s]"), name_norm);
  } else {
    /* Copy tmp_err into err, with a prefixed message. */
    g_propagate_prefixed_error (err, tmp_err,
                                _("Failed to load RC file [%1$s]: "),
                                name_norm);
    g_free (name_norm);
  }
  return status;
}

/*! \brief Load a system RC file.
 * \par Function Description
 * Attempts to load and run the system Scheme initialisation file with
 * basename \a rcname.  The string "system-" is prefixed to \a rcname.
 * If \a rcname is NULL, the default value of "gafrc" is used.
 *
 * \param toplevel  The current #LeptonToplevel structure.
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_rc_parse_system (LeptonToplevel *toplevel,
                   const gchar *rcname,
                   GError **err)
{
  gchar *sysname = NULL;
  gchar *rcfile = NULL;
  gboolean status = TRUE;
  const gchar * const * sys_dirs = eda_get_system_config_dirs();
  EdaConfig *cfg = eda_config_get_system_context();

  /* Default to gafrc */
  rcname = (rcname != NULL) ? rcname : "gafrc";

  sysname = g_strdup_printf ("system-%s", rcname);
  for (gint i = 0; sys_dirs[i]; ++i)
  {
    rcfile = g_build_filename (sys_dirs[i], sysname, NULL);
    if (g_file_test(rcfile, G_FILE_TEST_IS_REGULAR))
    {
      break;
    }
    g_free(rcfile);
    rcfile = NULL;
  }

  if (rcfile)
  {
    status = g_rc_parse_file (toplevel, rcfile, cfg, err);
  }

  g_free (rcfile);
  g_free (sysname);
  return status;
}

/*! \brief Load a user RC file.
 * \par Function Description
 * Attempts to load the user Scheme initialisation file with basename
 * \a rcname.  If \a rcname is NULL, the default value of "gafrc" is
 * used.
 *
 * \param toplevel  The current #LeptonToplevel structure.
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_rc_parse_user (LeptonToplevel *toplevel,
                 const gchar *rcname,
                 GError **err)
{
  gchar *rcfile = NULL;
  gboolean status;

  /* Default to gafrc */
  rcname = (rcname != NULL) ? rcname : "gafrc";

  rcfile = g_build_filename (eda_get_user_config_dir (), rcname, NULL);
  status = g_rc_parse_file (toplevel, rcfile,
                            eda_config_get_user_context (), err);
  g_free (rcfile);
  return status;
}

/*! \brief Load a local RC file.
 * \par Function Description
 * Attempts to load the Scheme initialisation file with basename \a
 * rcname corresponding to \a path, reporting errors via \a err.  If
 * \a path is a directory, looks for a file named \a rcname in that
 * directory. Otherwise, looks for a file named \a rcname in the same
 * directory as \a path. If \a path is NULL, looks in the current
 * directory. If \a rcname is NULL, the default value of "gafrc" is
 * used.
 *
 * \param toplevel  The current #LeptonToplevel structure.
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param path      The path to load a RC file for, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_rc_parse_local (LeptonToplevel *toplevel,
                  const gchar *rcname,
                  const gchar *path,
                  GError **err)
{
  gchar *dir = NULL;
  gchar *rcfile = NULL;
  gboolean status;
  g_return_val_if_fail ((toplevel != NULL), FALSE);

  /* Default to gafrc */
  rcname = (rcname != NULL) ? rcname : "gafrc";
  /* Default to cwd */
  path = (path != NULL) ? path : ".";

  /* If path isn't a directory, get the dirname. */
  if (g_file_test (path, G_FILE_TEST_IS_DIR)) {
    dir = g_strdup (path);
  } else {
    dir = g_path_get_dirname (path);
  }

  rcfile = g_build_filename (dir, rcname, NULL);
  status = g_rc_parse_file (toplevel, rcfile, NULL, err);

  g_free (dir);
  g_free (rcfile);
  return status;
}

static void
g_rc_parse__process_error (GError **err, const gchar *pname)
{
  char *pbase;

  /* Take no chances; if err was not set for some reason, bail out. */
  if (*err == NULL) {
    const gchar *msgl =
      _("ERROR: An unknown error occurred while parsing configuration files.");
    g_message ("%1$s", msgl);
    fprintf(stderr, "%1$s\n", msgl);

  } else {
    /* RC files are allowed to be missing or skipped; check for
     * this. */
    if (g_error_matches (*err, G_FILE_ERROR, G_FILE_ERROR_NOENT) ||
        g_error_matches (*err, EDA_ERROR, EDA_ERROR_RC_TWICE)) {
      return;
    }

    g_message (_("ERROR: %1$s"), (*err)->message);
    fprintf (stderr, _("ERROR: %1$s\n"), (*err)->message);
  }

  /* g_path_get_basename() allocates memory, but we don't care
   * because we're about to exit. */
  pbase = g_path_get_basename (pname);
  fprintf (stderr, _("ERROR: The %1$s log may contain more information.\n"),
           pbase);
  exit (1);
}

/*! \brief General RC file parsing function.
 * \par Function Description
 * Calls g_rc_parse_handler() with the default error handler. If any
 * error other than ENOENT occurs while loading or running a Scheme
 * initialisation file, prints an informative message and calls
 * exit(1).
 *
 * \bug libgeda shouldn't call exit() - this function calls
 *      g_rc_parse__process_error(), which does.
 *
 * \warning Since this function may not return, it should only be used
 * on application startup or when there is no chance of data loss from
 * an unexpected exit().
 *
 * \param [in] pname     The name of the application (usually argv[0]).
 * \param [in] rcname    RC file basename, or NULL.
 * \param [in] rcfile    Specific RC file path, or NULL.
 */
void
g_rc_parse (const gchar *pname,
            const gchar *rcname,
            const gchar *rcfile)
{
  LeptonToplevel *toplevel = edascm_c_current_toplevel ();
  g_rc_parse_handler (toplevel, rcname, rcfile,
                      (ConfigParseErrorFunc) g_rc_parse__process_error,
                      (void *) pname);
}


/*! \brief General RC file parsing function.
 * \par Function Description
 * Attempt to load and run system, user and local (current working directory)
 * Scheme initialisation files, first with the default "gafrc"
 * basename and then with the basename \a rcname, if \a rcname is not
 * NULL.  Additionally, attempt to load and run \a rcfile
 * if \a rcfile is not NULL.
 *
 * If an error occurs, calls \a handler with the provided \a user_data
 * and a GError.
 *
 * \see g_rc_parse().
 *
 * \param toplevel  The current #LeptonToplevel structure.
 * \param rcname    RC file basename, or NULL.
 * \param rcfile    Specific RC file path, or NULL.
 * \param handler   Handler function for RC errors.
 * \param user_data Data to be passed to \a handler.
 */
void
g_rc_parse_handler (LeptonToplevel *toplevel,
                    const gchar *rcname,
                    const gchar *rcfile,
                    ConfigParseErrorFunc handler,
                    void *user_data)
{
  GError *err = NULL;

#ifdef HANDLER_DISPATCH
#  error HANDLER_DISPATCH already defined
#endif
#define HANDLER_DISPATCH \
  do { if (err == NULL) break;  handler (&err, user_data);        \
       g_clear_error (&err); } while (0)

  /* Load cache configuration: */
  g_rc_load_cache_config (toplevel, &err); HANDLER_DISPATCH;

  /* Load RC files in order. */
  /* First gafrc files. */
  g_rc_parse_system (toplevel, NULL, &err); HANDLER_DISPATCH;
  g_rc_parse_user (toplevel, NULL, &err); HANDLER_DISPATCH;
  g_rc_parse_local (toplevel, NULL, NULL, &err); HANDLER_DISPATCH;
  /* Next application-specific rcname. */
  if (rcname != NULL) {
    g_rc_parse_system (toplevel, rcname, &err); HANDLER_DISPATCH;
    g_rc_parse_user (toplevel, rcname, &err); HANDLER_DISPATCH;
    g_rc_parse_local (toplevel, rcname, NULL, &err); HANDLER_DISPATCH;
  }
  /* Finally, optional additional RC file.  Specifically use the
   * current working directory's configuration context here, no matter
   * where the rc file is located on disk. */
  if (rcfile != NULL) {
    EdaConfig *cwd_cfg = eda_config_get_context_for_path (".");
    g_rc_parse_file (toplevel, rcfile, cwd_cfg, &err); HANDLER_DISPATCH;
  }

#undef HANDLER_DISPATCH
}


/*! \brief Load cache configuration data.
 *
 * \param toplevel  The current #LeptonToplevel structure.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_rc_load_cache_config (LeptonToplevel* toplevel,
                        GError** err)
{
  g_return_val_if_fail (toplevel != NULL, FALSE);

  EdaConfig* cfg = eda_config_get_cache_context();

  gboolean status = FALSE;
  if (cfg != NULL)
  {
    GError* tmp_err = NULL;
    status = eda_config_load (cfg, &tmp_err);

    /* It's OK if file is not found (e.g. on first program run): */
    if (g_error_matches (tmp_err, G_IO_ERROR, G_IO_ERROR_NOT_FOUND))
    {
      g_clear_error (&tmp_err);
      status = TRUE;
    }
  }

  return status;
}
