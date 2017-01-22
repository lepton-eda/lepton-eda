/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
 * Copyright (C) 2016 Peter Brett <peter@peter-b.co.uk>
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

#include "libgeda_priv.h"
#include "libgedaguile.h"

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int vstbl_lookup_str(const vstbl_entry *table,
			    int size, const char *str)
{
  int i;

  for(i = 0; i < size; i++) {
    if(strcmp(table[i].m_str, str) == 0) {
      break;
    }
  }
  return i;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int vstbl_get_val(const vstbl_entry *table, int index)
{
  return table[index].m_val;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_mode_general(SCM scmmode, 
                      const char *rc_name,
                      int *mode_var,
                      const vstbl_entry *table,
                      int table_size)
{
  SCM ret;
  int index;
  char *mode;

  SCM_ASSERT (scm_is_string (scmmode), scmmode,
              SCM_ARG1, rc_name);
  
  mode = scm_to_utf8_string (scmmode);
  
  index = vstbl_lookup_str(table, table_size, mode);
  /* no match? */
  if(index == table_size) {
    fprintf(stderr,
            _("Invalid mode [%s] passed to %s\n"),
            mode,
            rc_name);
    ret = SCM_BOOL_F;
  } else {
    *mode_var = vstbl_get_val(table, index);
    ret = SCM_BOOL_T;
  }

  free (mode);

  return ret;
}

/*! \brief Mark an RC file as loaded.
 * \par Function Description
 * If the Scheme initialisation file \a filename has not already been
 * loaded, mark it as loaded and return TRUE, storing \a filename in
 * \a toplevel (\a filename should not subsequently be freed).
 * Otherwise, return FALSE, and set \a err appropriately.
 *
 * \note Should only be called by g_rc_parse_file().
 *
 * \param toplevel  The current #TOPLEVEL structure.
 * \param filename  The RC file name to test.
 * \param err       Return location for errors, or NULL.
 * \return TRUE if \a filename not already loaded, FALSE otherwise.
 */
static gboolean
g_rc_try_mark_read (TOPLEVEL *toplevel, gchar *filename, GError **err)
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
  /* N.b. don't free name_norm here; it's stored in the TOPLEVEL. */
  return TRUE;
}

SCM scheme_rc_config_fluid = SCM_UNDEFINED;

/*! \brief Load an RC file.
 * \par Function Description
 * Load and run the Scheme initialisation file \a rcfile, reporting
 * errors via \a err.
 *
 * \param toplevel  The current #TOPLEVEL structure.
 * \param rcfile    The filename of the RC file to load.
 * \param cfg       The configuration context to use while loading.
 * \param err       Return location for errors, or NULL;
 * \return TRUE on success, FALSE on failure.
 */
static gboolean
g_rc_parse_file (TOPLEVEL *toplevel, const gchar *rcfile,
                 EdaConfig *cfg, GError **err)
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
      g_warning (_("Failed to load config from '%s': %s\n"),
                 eda_config_get_filename (cfg), tmp_err->message);
    g_clear_error (&tmp_err);
  }

  /* If the fluid for storing the relevant configuration context for
   * RC file reading hasn't been created yet, create it. */
  if (scheme_rc_config_fluid == SCM_UNDEFINED)
    scheme_rc_config_fluid = scm_permanent_object (scm_make_fluid ());

  /* Normalise filename */
  name_norm = f_normalize_filename (rcfile, err);
  if (name_norm == NULL) return FALSE;

  /* Attempt to load the RC file, if it hasn't been loaded already.
   * If g_rc_try_mark_read() succeeds, it stores name_norm in
   * toplevel, so we *don't* free it. */
  scm_dynwind_begin (0);
  scm_dynwind_fluid (scheme_rc_config_fluid, edascm_from_config (cfg));
  status = (g_rc_try_mark_read (toplevel, name_norm, &tmp_err)
            && g_read_file (toplevel, name_norm, &tmp_err));
  scm_dynwind_end ();

  if (status) {
    s_log_message (_("Loaded RC file [%s]\n"), name_norm);
  } else {
    /* Copy tmp_err into err, with a prefixed message. */
    g_propagate_prefixed_error (err, tmp_err,
                                _("Failed to load RC file [%s]: "),
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
 * \param toplevel  The current #TOPLEVEL structure.
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_rc_parse_system (TOPLEVEL *toplevel, const gchar *rcname, GError **err)
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
		if (g_file_test(rcfile, G_FILE_TEST_IS_REGULAR)) {
			break;
		}
		g_free(rcfile);
		rcfile = NULL;
	}

	if (rcfile) {
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
 * \param toplevel  The current #TOPLEVEL structure.
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_rc_parse_user (TOPLEVEL *toplevel, const gchar *rcname, GError **err)
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
 * \param toplevel  The current #TOPLEVEL structure.
 * \param rcname    The basename of the RC file to load, or NULL.
 * \param path      The path to load a RC file for, or NULL.
 * \param err       Return location for errors, or NULL.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
g_rc_parse_local (TOPLEVEL *toplevel, const gchar *rcname, const gchar *path,
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
    s_log_message ("%s\n", msgl);
    fprintf(stderr, "%s\n", msgl);

  } else {
    /* RC files are allowed to be missing or skipped; check for
     * this. */
    if (g_error_matches (*err, G_FILE_ERROR, G_FILE_ERROR_NOENT) ||
        g_error_matches (*err, EDA_ERROR, EDA_ERROR_RC_TWICE)) {
      return;
    }

    s_log_message (_("ERROR: %s\n"), (*err)->message);
    fprintf (stderr, _("ERROR: %s\n"), (*err)->message);
  }

  /* g_path_get_basename() allocates memory, but we don't care
   * because we're about to exit. */
  pbase = g_path_get_basename (pname);
  fprintf (stderr, _("ERROR: The %s log may contain more information.\n"),
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
 * \param [in] toplevel  The current #TOPLEVEL structure.
 * \param [in] pname     The name of the application (usually argv[0]).
 * \param [in] rcname    RC file basename, or NULL.
 * \param [in] rcfile    Specific RC file path, or NULL.
 */
void
g_rc_parse (TOPLEVEL *toplevel, const gchar *pname,
            const gchar *rcname, const gchar *rcfile)
{
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
 * \param toplevel  The current #TOPLEVEL structure.
 * \param rcname    RC file basename, or NULL.
 * \param rcfile    Specific RC file path, or NULL.
 * \param handler   Handler function for RC errors.
 * \param user_data Data to be passed to \a handler.
 */
void
g_rc_parse_handler (TOPLEVEL *toplevel,
                    const gchar *rcname, const gchar *rcfile,
                    ConfigParseErrorFunc handler, void *user_data)
{
  GError *err = NULL;

#ifdef HANDLER_DISPATCH
#  error HANDLER_DISPATCH already defined
#endif
#define HANDLER_DISPATCH \
  do { if (err == NULL) break;  handler (&err, user_data);        \
       g_clear_error (&err); } while (0)

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
    EdaConfig *cwd_cfg = eda_config_get_context_for_file (NULL);
    g_rc_parse_file (toplevel, rcfile, cwd_cfg, &err); HANDLER_DISPATCH;
  }

#undef HANDLER_DISPATCH
}

/*! \brief
 *  \par Function Description
 *
 *  \param [in] path 
 *  \param [in] name Optional descriptive name for library directory.
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library(SCM path, SCM name)
{
  gchar *string;
  char *temp;
  char *namestr = NULL;

  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "component-library");

  scm_dynwind_begin (0);
  if (name != SCM_UNDEFINED) {
    SCM_ASSERT (scm_is_string (name), name,
		SCM_ARG2, "component-library");
    namestr = scm_to_utf8_string (name);
    scm_dynwind_free(namestr);
  }

  /* take care of any shell variables */
  temp = scm_to_utf8_string (path);
  string = s_expand_env_variables (temp);
  scm_dynwind_unwind_handler (g_free, string, SCM_F_WIND_EXPLICITLY);
  free (temp);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf(stderr,
            _("Invalid path [%s] passed to component-library\n"),
            string);
    scm_dynwind_end();
    return SCM_BOOL_F;
  }

  if (g_path_is_absolute (string)) {
    s_clib_add_directory (string, namestr);
  } else {
    gchar *cwd = g_get_current_dir ();
    gchar *temp;
    temp = g_build_filename (cwd, string, NULL);
    s_clib_add_directory (temp, namestr);
    g_free(temp);
    g_free(cwd);
  }

  scm_dynwind_end();
  return SCM_BOOL_T;
}

/*! \brief Guile callback for adding library commands.
 *  \par Function Description
 *  Callback function for the "component-library-command" Guile
 *  function, which can be used in the rc files to add a command to
 *  the component library.
 *
 *  \param [in] listcmd command to get a list of symbols
 *  \param [in] getcmd  command to get a symbol from the library
 *  \param [in] name    Optional descriptive name for component source.
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM
g_rc_component_library_command (SCM listcmd, SCM getcmd,
                                SCM name)
{
  const CLibSource *src;
  gchar *lcmdstr, *gcmdstr;
  char *tmp_str, *namestr;

  SCM_ASSERT (scm_is_string (listcmd), listcmd, SCM_ARG1, 
              "component-library-command");
  SCM_ASSERT (scm_is_string (getcmd), getcmd, SCM_ARG2, 
              "component-library-command");
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG3, 
              "component-library-command");

  scm_dynwind_begin(0);

  /* take care of any shell variables */
  /*! \bug this may be a security risk! */
  tmp_str = scm_to_utf8_string (listcmd);
  lcmdstr = s_expand_env_variables (tmp_str);
  scm_dynwind_unwind_handler (g_free, lcmdstr, SCM_F_WIND_EXPLICITLY);
  free (tmp_str); /* this should stay as free (allocated from guile) */

  /* take care of any shell variables */
  /*! \bug this may be a security risk! */
  tmp_str = scm_to_utf8_string (getcmd);
  gcmdstr = s_expand_env_variables (tmp_str);
  scm_dynwind_unwind_handler (g_free, gcmdstr, SCM_F_WIND_EXPLICITLY);
  free (tmp_str); /* this should stay as free (allocated from guile) */

  namestr = scm_to_utf8_string (name);

  src = s_clib_add_command (lcmdstr, gcmdstr, namestr);

  free (namestr); /* this should stay as free (allocated from guile) */

  scm_dynwind_end();

  if (src != NULL) {
    return SCM_BOOL_T;
  }

  return SCM_BOOL_F;
}

/*! \brief Guile callback for adding library functions.
 *  \par Function Description
 *  Callback function for the "component-library-funcs" Guile
 *  function, which can be used in the rc files to add a set of Guile
 *  procedures for listing and generating symbols.
 *
 *  \param [in] listfunc A Scheme procedure which takes no arguments
 *                       and returns a Scheme list of component names.
 *  \param [in] getfunc A Scheme procedure which takes a component
 *                      name as an argument and returns a symbol
 *                      encoded in a string in gEDA format, or the \b
 *                      \#f if the component name is unknown.
 *  \param [in] name    A descriptive name for this component source.
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library_funcs (SCM listfunc, SCM getfunc, SCM name)
{
  char *namestr;
  SCM result = SCM_BOOL_F;

  SCM_ASSERT (scm_is_true (scm_procedure_p (listfunc)), listfunc, SCM_ARG1,
	      "component-library-funcs");
  SCM_ASSERT (scm_is_true (scm_procedure_p (getfunc)), getfunc, SCM_ARG2,
	      "component-library-funcs");
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG3, 
	      "component-library-funcs");

  namestr = scm_to_utf8_string (name);

  if (s_clib_add_scm (listfunc, getfunc, namestr) != NULL) {
    result = SCM_BOOL_T;
  }

  free (namestr);
  return result;
}

/*!
 * \brief Get the name of the RC filename being evaluated.
 * \par Function Description
 *
 * Creates a Guile stack object, extracts the topmost frame from that
 * stack and gets the sourcefile name.
 *
 * \returns If the interpreter can resolve the filename, returns a
 * Scheme object with the full path to the RC file, otherwise #f
 */
SCM
g_rc_rc_filename()
{
  SCM stack, frame, source;

  stack = scm_make_stack (SCM_BOOL_T, SCM_EOL);
  if (scm_is_false (stack)) {
    return SCM_BOOL_F;
  }

  frame = scm_stack_ref (stack, scm_from_int(0));
  if (scm_is_false (frame)) {
    return SCM_BOOL_F;
  }

  source = scm_frame_source (frame);
  if (scm_is_false (source)) {
    return SCM_BOOL_F;
  }

  return scm_source_property (source, scm_sym_filename);
}

/*!
 * \brief Get a configuration context for the current RC file.
 * \par Function Description
 * Returns the configuration context applicable to the RC file being
 * evaluated.  This function is intended to support gEDA transition
 * from functions in RC files to static configuration files.
 *
 * \returns An EdaConfig smob.
 */
SCM
g_rc_rc_config()
{
  SCM cfg_s = scm_fluid_ref (scheme_rc_config_fluid);
  if (!scm_is_false (cfg_s)) return cfg_s;

  EdaConfig *cfg = eda_config_get_context_for_file (NULL);
  return edascm_from_config (cfg);
}

/*! \brief Add a directory to the Guile load path.
 * \par Function Description
 * Prepends \a s_path to the Guile system '%load-path', after
 * expanding environment variables.
 *
 *  \param [in] s_path  Path to be added.
 *  \return SCM_BOOL_T.
 */
SCM g_rc_scheme_directory(SCM s_path)
{
  char *temp;
  gchar *expanded;
  SCM s_load_path_var;
  SCM s_load_path;

  SCM_ASSERT (scm_is_string (s_path), s_path,
              SCM_ARG1, "scheme-directory");

  /* take care of any shell variables */
  temp = scm_to_utf8_string (s_path);
  expanded = s_expand_env_variables (temp);
  s_path = scm_from_utf8_string (expanded);
  free (temp);
  g_free (expanded);

  s_load_path_var = scm_c_lookup ("%load-path");
  s_load_path = scm_variable_ref (s_load_path_var);
  scm_variable_set_x (s_load_path_var, scm_cons (s_path, s_load_path));

  scm_remember_upto_here_2 (s_load_path_var, s_load_path);
  scm_remember_upto_here_1 (s_path);

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] path  
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_bitmap_directory(SCM path)
{
  gchar *string;
  char *temp;

  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "bitmap-directory");
  
  /* take care of any shell variables */
  temp = scm_to_utf8_string (path);
  string = s_expand_env_variables (temp);
  free (temp);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             _("Invalid path [%s] passed to bitmap-directory\n"),
             string);
    g_free(string);
    return SCM_BOOL_F;
  }

  g_free(default_bitmap_directory);
  default_bitmap_directory = string;

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] scmsymname  
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_bus_ripper_symname(SCM scmsymname)
{
  char *temp;

  SCM_ASSERT (scm_is_string (scmsymname), scmsymname,
              SCM_ARG1, "bus-ripper-symname");

  g_free(default_bus_ripper_symname);

  temp = scm_to_utf8_string (scmsymname);
  default_bus_ripper_symname = g_strdup (temp);
  free (temp);

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_reset_component_library(void)
{
  s_clib_init();
  
  return SCM_BOOL_T;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_attribute_promotion(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("attribute-promotion",
		   default_attribute_promotion,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_promote_invisible(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("promote-invisible",
		   default_promote_invisible,
		   2);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
SCM g_rc_keep_invisible(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("keep-invisible",
		   default_keep_invisible,
		   2);
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] attrlist
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_always_promote_attributes(SCM attrlist)
{
  GList *list=NULL;
  int length, i;
  gchar *attr;
  gchar **attr2;

  g_list_foreach(default_always_promote_attributes, (GFunc)g_free, NULL);
  g_list_free(default_always_promote_attributes);

  if (scm_is_string (attrlist)) {
    char *temp;
    s_log_message(_("WARNING: using a string for 'always-promote-attributes'"
		    " is deprecated. Use a list of strings instead\n"));

    /* convert the space separated strings into a GList */
    temp = scm_to_utf8_string (attrlist);
    attr2 = g_strsplit(temp," ", 0);
    free (temp);

    for (i=0; attr2[i] != NULL; i++) {
      if (strlen(attr2[i]) > 0) {
	list = g_list_prepend(list, g_strdup(attr2[i]));
      }
    }
    g_strfreev(attr2);
  } else {
    SCM_ASSERT(scm_list_p(attrlist), attrlist, SCM_ARG1, "always-promote-attributes");
    length = scm_ilength(attrlist);
    /* convert the scm list into a GList */
    for (i=0; i < length; i++) {
      char *temp;
      SCM_ASSERT(scm_is_string(scm_list_ref(attrlist, scm_from_int(i))), 
		 scm_list_ref(attrlist, scm_from_int(i)), SCM_ARG1, 
		 _("always-promote-attribute: list element is not a string"));
      temp = scm_to_utf8_string (scm_list_ref (attrlist, scm_from_int (i)));
      attr = g_strdup(temp);
      free (temp);
      list = g_list_prepend(list, attr);
    }
  }

  default_always_promote_attributes = g_list_reverse(list);

  return SCM_BOOL_T;
}

/*! \brief Enable the creation of backup files when saving
 *  \par Function Description
 *  If enabled then a backup file, of the form 'example.sch~', is created when
 *  saving a file.
 *
 *  \param [in] mode  String. 'enabled' or 'disabled'
 *  \return           Bool. False if mode is not a valid value; true if it is.
 *
 */
SCM g_rc_make_backup_files(SCM mode)
{
  static const vstbl_entry mode_table[] = {
    {TRUE , "enabled" },
    {FALSE, "disabled"},
  };

  RETURN_G_RC_MODE("make-backup-files",
                  default_make_backup_files,
                  2);
}

SCM g_rc_print_color_map (SCM scm_map)
{
  if (scm_map == SCM_UNDEFINED) {
    return s_color_map_to_scm (print_colors);
  }

  SCM_ASSERT (scm_is_true (scm_list_p (scm_map)),
              scm_map, SCM_ARG1, "print-color-map");

  s_color_map_from_scm (print_colors, scm_map, "print-color-map");
  return SCM_BOOL_T;
}
