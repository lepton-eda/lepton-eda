/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's Library
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include <stdio.h>
#include <sys/stat.h>
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"

#include "../include/i_vars.h"
#include "../include/papersizes.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

extern GHashTable *font_char_to_file;

/*! \brief Reads the gafrc file.
 *  \par Function Description
 *  This is the function which actually reads in the RC file.
 *  First, it looks in a list of previously read RC files.  If the file has
 *  already been read, it just says OK.  After reading the file, it places
 *  the filename in the list of read files.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] fname      RC file name to read.
 *  \param [in] ok_msg     Message to print if file is read ok.
 *  \param [in] err_msg    Message to print if file read error occurs
 *  \return 1 on success, 0 otherwise.
 */
gint g_rc_parse_general(TOPLEVEL *w_current,
			const gchar *fname, 
			const gchar *ok_msg, const gchar *err_msg)
{
  gint found_rc = FALSE;
  GList *found_rc_filename_element;

  /* First see if fname is in list of previously read RC files. */
  found_rc_filename_element = g_list_find_custom(w_current->RC_list, 
                                                 (gconstpointer) fname,
                                                 (GCompareFunc) strcmp);
  if (found_rc_filename_element != NULL) {
    /* We've already read this one in. */
    s_log_message("RC file [%s] already read in.\n", fname);
    return 0;
  }

  /* Now try to read in contents of RC file.  */
  if (access (fname, R_OK) == 0) {
    g_read_file (fname);
    found_rc = 1;
    /* Everything was OK.  Now add this file to list of read RC files. */
    w_current->RC_list = g_list_append (w_current->RC_list,
                                        g_strdup (fname));
    s_log_message (ok_msg, fname);
  } else {
    found_rc = 0;
    s_log_message (err_msg, fname);
  }

  return found_rc;
}


/*! \brief Read gEDA root path from RC file.
 *  \par Function Description
 *  This function will read the RC file and parse the root path for gEDA.
 *
 *  \return String containing rc root path
 *
 *  \warning Do not free the returned character string.
 */
const gchar* g_rc_parse_path(void)
{
  const gchar *rc_path;
  
  if (g_strcasecmp (GEDARCDIR, "none") == 0) {
    /* rc dir not specified at configure time, so search for config in */
    /* the normal GEDADATA directory */
    rc_path = g_getenv ("GEDADATA");
  } else {
    /* rc path specified at configure time, always return specified path */
    rc_path = GEDARCDIR;
  }

  return(rc_path);
}

/*! \brief Parses a system RC file.
 *  \par Function Description
 *  This function wil open and parse a system rc file.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] rcname     System RC file name to parse.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_system_rc(TOPLEVEL *w_current, const gchar *rcname)
{
  const gchar *geda_data = g_getenv ("GEDADATA");
  gint found_rc;
  gchar *tmp;
  char *filename;
  gchar *ok_msg, *err_msg;

  if (geda_data == NULL) {
    fprintf(stderr, "You must set the GEDADATA environment variable!\n");
    exit(-1);
  }

  tmp = g_strconcat (g_rc_parse_path (),
                     G_DIR_SEPARATOR_S,
                     "system-", rcname,
                     NULL);
  filename = f_normalize_filename(tmp);
  if (filename == NULL) {
    return 0;
  }

  ok_msg  = g_strdup_printf ("Read system-%s file [%%s]\n",
                             rcname);
  err_msg = g_strdup_printf ("Did not find required system-%s file [%%s]\n",
                             rcname);  
  found_rc = g_rc_parse_general(w_current, filename, ok_msg, err_msg);

  g_free(ok_msg);
  g_free(err_msg);  
  g_free(tmp);
  g_free(filename);

  return found_rc;
}

/*! \brief Parse a RC file in users home directory.
 *  \par Function Description
 *  This function will open and parse a RC file in the users home directory.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] rcname     User's RC file name.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_home_rc(TOPLEVEL *w_current, const gchar *rcname)
{
  const gchar *home = g_getenv ("HOME");
  gint found_rc;
  gchar *tmp;
  char *filename;
  gchar *ok_msg, *err_msg;

  if (home == NULL) {
    return 0;
  }

  tmp = g_strconcat (home,
                     G_DIR_SEPARATOR_S,
                     ".gEDA",
                     G_DIR_SEPARATOR_S,
                     rcname,
                     NULL);
  filename = f_normalize_filename(tmp);
  if (filename == NULL) {
    return 0;
  }

  ok_msg  = g_strdup_printf ("Read ~/.gEDA/%s file [%%s]\n",
                             rcname);
  err_msg = g_strdup_printf ("Did not find optional ~/.gEDA/%s file [%%s]\n",
                             rcname);  
  found_rc = g_rc_parse_general(w_current, filename, ok_msg, err_msg);
  
  g_free(ok_msg);
  g_free(err_msg);
  g_free(tmp);
  g_free(filename);

  return found_rc;
}

/*! \brief Parse rc file in current working directory.
 *  \par Function Description
 *  This function will open and parse a RC file in the current working directory.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] rcname     Local directory RC file name.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_local_rc(TOPLEVEL *w_current, const gchar *rcname)
{
  gint found_rc;
  gchar *tmp;
  char *filename;
  gchar *ok_msg;
  gchar *err_msg;

  tmp = g_strconcat (".", G_DIR_SEPARATOR_S, rcname, NULL);
  filename = f_normalize_filename (tmp);
  if (filename == NULL) {
    return 0;
  }

  ok_msg  = g_strdup_printf ("Read local %s file [%%s]\n",
                             rcname);
  err_msg = g_strdup_printf ("Did not find optional local %s file [%%s]\n",
                             rcname);  
  found_rc = g_rc_parse_general(w_current, filename, ok_msg, err_msg);

  g_free(ok_msg);
  g_free(err_msg);
  g_free(tmp);
  g_free(filename);

  return found_rc;
}

/*! \brief Parse a RC file from a specified location.
 *  \par Function Description
 *  This function will open and parse a RC file from a specified location.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] rcname     Specified location RC file name.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_specified_rc(TOPLEVEL *w_current, const gchar *rcname)
{
  gint found_rc = 0;
  char *filename;
  gchar *ok_msg;
  gchar *err_msg;

  if (rcname == NULL) {
    return 0;
  }

  filename = f_normalize_filename (rcname);

  ok_msg  = g_strdup_printf ("Read specified %s file [%%s]\n",
                             rcname);
  err_msg = g_strdup_printf ("Did not find specified %s file [%%s]\n",
                             rcname);  
  found_rc = g_rc_parse_general(w_current, filename, ok_msg, err_msg);
  
  g_free(ok_msg);
  g_free(err_msg);
  g_free(filename);

  return found_rc;
}

/*! \brief General RC file parsing function.
 *  \par Function Description
 *  This function will check for System, HOME and Local RC files matching
 *  the rcname input parameter.  If none of those three are found it will
 *  search for the specified_rc_filename.  When none are found it will
 *  call exit(-1) to terminate the program.
 *
 *  \param [in] w_current              The TOPLEVEL object.
 *  \param [in] rcname                 RC file name.
 *  \param [in] specified_rc_filename  Specific location RC file name.
 *  \return calls exit(-1) when no RC file matching either rcname or
 *          specified_rc_filename is found.
 */
void g_rc_parse(TOPLEVEL *w_current,
		const gchar *rcname, const gchar *specified_rc_filename)
{
  gint found_rc = 0;
  char *rc_path;
  char *geda_rcdata;

  /* set the GEDADATARC environment variable so that the rc files */
  /* know where to look for others */
  rc_path = f_normalize_filename (g_rc_parse_path ());
  /* Reversion to putenv inspired by W. Hoch, 2.17.2005 */
  /*  g_setenv ("GEDADATARC", rc_path, TRUE); */  /*requires glib 2.4.x*/
  geda_rcdata = g_strdup_printf("GEDADATARC=%s", rc_path);
  putenv(geda_rcdata);
  g_free(rc_path);
  
  /* visit rc files in order */
  /* Changed by SDB 1.2.2005 in response to Peter Kaiser's bug report.
   * Read gafrc files first */
  found_rc |= g_rc_parse_system_rc(w_current, "gafrc");
  found_rc |= g_rc_parse_home_rc(w_current, "gafrc");
  found_rc |= g_rc_parse_local_rc(w_current, "gafrc");
  /* continue support for individual rc files for each program.  */
  found_rc |= g_rc_parse_system_rc(w_current, rcname);
  found_rc |= g_rc_parse_home_rc(w_current, rcname);
  found_rc |= g_rc_parse_local_rc(w_current, rcname);

  /* New fcn introduced by SDB to consolidate this & make it available 
   * for other programs */
  found_rc |= g_rc_parse_specified_rc(w_current, specified_rc_filename);

  /* Oh well, I couldn't find any rcfile, exit! */
  if (!found_rc) {
    /*! \todo these two are basically the
     * same. Inefficient!
     */
    s_log_message("Could not find any %s file!\n", rcname);
    fprintf(stderr, "Could not find a %s file\n", rcname);
    exit(-1);
  }
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
  char *string;
  char *namestr = NULL;

  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path,
              SCM_ARG1, "component-library");
  
  if (name != SCM_UNDEFINED) {
    SCM_ASSERT (SCM_NIMP (name) && SCM_STRINGP (name), name,
		SCM_ARG2, "component-library");
    namestr = SCM_STRING_CHARS (name);
  }
  
  string = g_strdup (SCM_STRING_CHARS (path));
  /* take care of any shell variables */
  string = expand_env_variables(string);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf(stderr,
            "Invalid path [%s] passed to component-library\n",
            string);
    g_free(string);
    return SCM_BOOL_F;
  }

  if (g_path_is_absolute (string)) {
    s_clib_add_directory (string, namestr);
  } else {
    gchar *cwd = g_get_current_dir ();
    gchar *temp;
#ifdef __MINGW32__
    u_basic_strip_trailing(cwd, G_DIR_SEPARATOR);
#endif
    temp = g_strconcat (cwd, G_DIR_SEPARATOR_S, string, NULL);
    s_clib_add_directory (temp, namestr);
    g_free(temp);
    g_free(cwd);
  }

  if (string) {
    g_free(string);
  }

  return SCM_BOOL_T;
}

/*! \brief Guile callback for adding library commands.
 *  \par Function Description
 *  Callback function for the "component-library-command" Guile
 *  function, which can be used in the rc files to add a command to
 *  the component library.
 *
 *  \param [in] command Command to add.
 *  \param [in] name    Optional descriptive name for component source.
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library_command (SCM command, SCM name)
{
  gchar *namestr = NULL;
  gchar *cmdstr = NULL;
  SCM_ASSERT (SCM_STRINGP (command), command, SCM_ARG1, 
	      "component-library-command");
  cmdstr = g_strdup(SCM_STRING_CHARS (command));

  if (name != SCM_UNDEFINED) {
    SCM_ASSERT (SCM_STRINGP (name), name, SCM_ARG2, 
	      "component-library-command");
    namestr = SCM_STRING_CHARS (name);
  }
  /* take care of any shell variables */
  cmdstr = expand_env_variables(cmdstr);

  s_clib_add_command (cmdstr, namestr);

  g_free (cmdstr);

  return SCM_BOOL_T;
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
 *
 *  \returns SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library_funcs (SCM listfunc, SCM getfunc, SCM name)
{
  SCM_ASSERT (scm_is_true (scm_procedure_p (listfunc)), listfunc, SCM_ARG1,
	      "component-library-funcs");
  SCM_ASSERT (scm_is_true (scm_procedure_p (getfunc)), getfunc, SCM_ARG2,
	      "component-library-funcs");
  SCM_ASSERT (SCM_STRINGP (name), name, SCM_ARG1, 
	      "component-library-funcs");

  if (s_clib_add_scm (listfunc, getfunc, SCM_STRING_CHARS (name)) != NULL) {
    return SCM_BOOL_T;
  } else {
    return SCM_BOOL_F;
  }
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] path  
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_component_library_search(SCM path)
{
  char *string;
  GDir *dir;
  const gchar *entry;
  
  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path,
              SCM_ARG1, "component-library-search");

  string = g_strdup (SCM_STRING_CHARS (path));
  /* take care of any shell variables */
  string = expand_env_variables(string);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to component-library-search\n",
             string);
    g_free(string);
    return SCM_BOOL_F;
  }

  dir = g_dir_open (string, 0, NULL);
  if (dir == NULL) {
    fprintf (stderr,
             "Invalid path [%s] passed to component-library-search\n",
             string);
    g_free(string);
    return SCM_BOOL_F;
  }

  while ((entry = g_dir_read_name (dir))) {
    /* don't do . and .. and special case font */
    if ((g_strcasecmp (entry, ".")    != 0) && 
        (g_strcasecmp (entry, "..")   != 0) &&
        (g_strcasecmp (entry, "font") != 0))
    {
      gchar *fullpath = g_strconcat (string,
                                     G_DIR_SEPARATOR_S,
                                     entry,
                                     NULL);

      if (g_file_test (fullpath, G_FILE_TEST_IS_DIR)) {
        if (g_path_is_absolute (fullpath)) {
          s_clib_add_directory (fullpath, NULL);
        } else {
          gchar *cwd = g_get_current_dir ();
          gchar *temp;
#ifdef __MINGW32__
          u_basic_strip_trailing(cwd, G_DIR_SEPARATOR);
#endif
          temp = g_strconcat (cwd,
                              G_DIR_SEPARATOR_S,
                              fullpath,
                              NULL);
          s_clib_add_directory (temp, NULL);
          g_free(temp);
          g_free(cwd);
        }
      }
      g_free(fullpath);
    }
  }

  if (string) {
    g_free(string);
  }

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] path  
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_source_library(SCM path)
{
  char *string;
  
  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path,
              SCM_ARG1, "source-library");

  string = g_strdup (SCM_STRING_CHARS (path));
  /* take care of any shell variables */
  string = expand_env_variables(string);
  
  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to source-library\n",
             string);
    g_free(string);
    return SCM_BOOL_F;
  }

  if (g_path_is_absolute (string)) {
    s_slib_add_entry (string);
  } else {
    gchar *cwd = g_get_current_dir ();
    gchar *temp;
#ifdef __MINGW32__
    u_basic_strip_trailing(cwd, G_DIR_SEPARATOR);
#endif
    temp = g_strconcat (cwd,
                        G_DIR_SEPARATOR_S,
                        string,
                        NULL);
    s_slib_add_entry (temp);
    g_free(temp);
    g_free(cwd);
  }
  
  if (string) {
    g_free(string);
  }
  
  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] path  
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_source_library_search(SCM path)
{
  char *string;
  GDir *dir;
  const gchar *entry;
  
  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path,
              SCM_ARG1, "source-library-search");

  string = g_strdup (SCM_STRING_CHARS (path));
  /* take care of any shell variables */
  string = expand_env_variables(string);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to source-library-search\n",
             string);
    g_free(string);
    return SCM_BOOL_F;
  }

  dir = g_dir_open (string, 0, NULL);
  if (dir == NULL) {
    fprintf (stderr,
             "Invalid path [%s] passed to source-library-search\n",
             string);
    if (string) {
      g_free(string);
    }
    return SCM_BOOL_F;
  }

  while ((entry = g_dir_read_name (dir))) {
    /* don't do . and .. and special case font */
    if ((g_strcasecmp (entry, ".")    != 0) && 
        (g_strcasecmp (entry, "..")   != 0) &&
        (g_strcasecmp (entry, "font") != 0))
    {
      gchar *fullpath = g_strconcat (string,
                                     G_DIR_SEPARATOR_S,
                                     entry,
                                     NULL);

      if (g_file_test (fullpath, G_FILE_TEST_IS_DIR)) {
        if (s_slib_uniq (fullpath)) {
          if (g_path_is_absolute (fullpath)) {
            s_slib_add_entry (fullpath);
          } else {
            gchar *cwd = g_get_current_dir ();
            gchar *temp;
#ifdef __MINGW32__
            u_basic_strip_trailing(cwd, G_DIR_SEPARATOR);
#endif
            temp = g_strconcat (cwd,
                                G_DIR_SEPARATOR_S,
                                fullpath,
                                NULL);
            s_slib_add_entry (temp);
            g_free(temp);
            g_free(cwd);
          }
        }
      }
      g_free(fullpath);
    }
  }

  if (string) {
    g_free(string);
  }

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] width   
 *  \param [in] height  
 *  \param [in] border  
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_world_size(SCM width, SCM height, SCM border)
#define FUNC_NAME "world-size"
{
  int i_width, i_height, i_border;
  int init_right, init_bottom;

  SCM_ASSERT (SCM_NIMP (width) && SCM_REALP (width), width,
              SCM_ARG1, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (height) && SCM_REALP (height), height,
              SCM_ARG2, FUNC_NAME);
  SCM_ASSERT (SCM_NIMP (border) && SCM_REALP (border), border,
              SCM_ARG3, FUNC_NAME);
  
  /* yes this is legit, we are casing the resulting double to an int */
  i_width  = (int) (SCM_NUM2DOUBLE (0, width)  * MILS_PER_INCH);
  i_height = (int) (SCM_NUM2DOUBLE (0, height) * MILS_PER_INCH);
  i_border = (int) (SCM_NUM2DOUBLE (0, border) * MILS_PER_INCH);

  PAPERSIZEtoWORLD(i_width, i_height, i_border,
                   &init_right, &init_bottom);

#if DEBUG
  printf("%d %d\n", i_width, i_height);
  printf("%d %d\n", init_right, init_bottom);
#endif

  default_init_right  = init_right;
  default_init_bottom = init_bottom;

  return SCM_BOOL_T;
}
#undef FUNC_NAME

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] name
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_default_series_name(SCM name)
{
  SCM_ASSERT (SCM_NIMP (name) && SCM_STRINGP (name), name,
              SCM_ARG1, "default-series-name");

  if (default_series_name) {
    g_free(default_series_name);
  }

  default_series_name = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] name  
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_untitled_name(SCM name)
{
  SCM_ASSERT (SCM_NIMP (name) && SCM_STRINGP (name), name,
              SCM_ARG1, "untitled-name");

  if (default_untitled_name) {
    g_free(default_untitled_name);
  }

  default_untitled_name = g_strdup (SCM_STRING_CHARS (name));

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] path  
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_font_directory(SCM path)
{
  char *string;

  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path,
              SCM_ARG1, "font-directory");

  string = g_strdup (SCM_STRING_CHARS (path));
  /* take care of any shell variables */
  string = expand_env_variables(string);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to font-directory\n",
             string);
    if (string) {
      g_free(string);
    }
    return SCM_BOOL_F;
  }

  if (default_font_directory) {
    g_free(default_font_directory);
  }
  default_font_directory = string;

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] path  
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_scheme_directory(SCM path)
{
  char *string;

  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path,
              SCM_ARG1, "scheme-directory");

  string = g_strdup (SCM_STRING_CHARS (path));
  /* take care of any shell variables */
  string = expand_env_variables(string);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to scheme-directory\n",
             string);
    if (string) {
      g_free(string);
    }
    return SCM_BOOL_F;
  }

  if (default_scheme_directory) {
    g_free(default_scheme_directory);
  }
  default_scheme_directory = string;

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
  char *string;

  SCM_ASSERT (SCM_NIMP (path) && SCM_STRINGP (path), path,
              SCM_ARG1, "bitmap-directory");
  
  string = g_strdup (SCM_STRING_CHARS (path));
  /* take care of any shell variables */
  string = expand_env_variables(string);

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to bitmap-directory\n",
             string);
    if (string) {
      g_free(string);
    }
    return SCM_BOOL_F;
  }

  if (default_bitmap_directory) {
    g_free(default_bitmap_directory);
  }
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
  SCM_ASSERT (SCM_NIMP (scmsymname) && SCM_STRINGP (scmsymname), scmsymname,
              SCM_ARG1, "bus-ripper-symname");

  if (default_bus_ripper_symname) {
    g_free(default_bus_ripper_symname);
  }
  default_bus_ripper_symname = g_strdup (SCM_STRING_CHARS (scmsymname));

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] scmsymname  
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_postscript_prolog(SCM scmsymname)
{
  char *string;
  
  SCM_ASSERT (SCM_NIMP (scmsymname) && SCM_STRINGP (scmsymname), scmsymname,
              SCM_ARG1, "postsript-prolog");

  if (default_postscript_prolog) {
    g_free(default_postscript_prolog);
  }

  string = g_strdup (SCM_STRING_CHARS (scmsymname));
  /* take care of any shell variables */
  string = expand_env_variables(string);

  default_postscript_prolog = g_strdup (string);

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

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_reset_source_library(void)
{
  s_slib_free();
  s_slib_init();
  
  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] scmcharstr   
 *  \param [in] scmfilename  
 *  \return SCM_BOOL_T on success, SCM_BOOL_F otherwise.
 */
SCM g_rc_map_font_character_to_file(SCM scmcharstr, SCM scmfilename)
{
  gchar *charstr, *filename;
  gunichar character;

  SCM_ASSERT (SCM_STRINGP (scmcharstr), scmcharstr,
              SCM_ARG1, "map-font-character-to-file");
  SCM_ASSERT (SCM_STRINGP (scmfilename), scmfilename,
              SCM_ARG2, "map-font-character-to-file");

  charstr  = SCM_STRING_CHARS (scmcharstr);
  filename = SCM_STRING_CHARS (scmfilename);
  
  if (charstr == NULL || filename == NULL) {
    fprintf(stderr,
            "%s requires two strings as parameters\n",
            "map-font-character-to-file"
            );
    return SCM_BOOL_F;
  }

  /* take care of expansion of any shell variables in filename */
  filename = expand_env_variables (g_strdup (filename));

  character = g_utf8_get_char_validated (charstr, -1);
  
  /* insert the new character declaration in the hash table */
  g_hash_table_insert (font_char_to_file,
                       GUINT_TO_POINTER ((guint)character),
                       filename);

  return SCM_BOOL_T;
}

/*! \todo Finish function description!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] scmsymname  
 *  \return SCM_BOOL_T always.
 */
SCM g_rc_always_promote_attributes(SCM scmsymname)
{
  SCM_ASSERT (SCM_NIMP (scmsymname) && SCM_STRINGP (scmsymname), scmsymname,
              SCM_ARG1, "always-promote-attributes");

  if (default_always_promote_attributes) {
    g_free(default_always_promote_attributes);
  }
  default_always_promote_attributes = 
    g_strdup_printf(" %s ", SCM_STRING_CHARS (scmsymname));

  return SCM_BOOL_T;
}
