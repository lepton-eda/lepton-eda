/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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
  
  mode = SCM_STRING_CHARS (scmmode);
  
  index = vstbl_lookup_str(table, table_size, mode);
  /* no match? */
  if(index == table_size) {
    fprintf(stderr,
            "Invalid mode [%s] passed to %s\n",
            mode,
            rc_name);
    ret = SCM_BOOL_F;
  } else {
    *mode_var = vstbl_get_val(table, index);
    ret = SCM_BOOL_T;
  }
  
  return ret;
}

extern GHashTable *font_char_to_file;

/*! \brief Reads the gafrc file.
 *  \par Function Description
 *  This is the function which actually reads in the RC file.
 *  First, it looks in a list of previously read RC files.  If the file has
 *  already been read, it just says OK.  After reading the file, it places
 *  the filename in the list of read files.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] fname      RC file name to read.
 *  \param [in] ok_msg     Message to print if file is read ok.
 *  \param [in] err_msg    Message to print if file read error occurs
 *  \return 1 on success, 0 otherwise.
 */
gint g_rc_parse_general(TOPLEVEL *toplevel,
			const gchar *fname, 
			const gchar *ok_msg, const gchar *err_msg)
{
  gint found_rc = FALSE;
  GList *found_rc_filename_element;

  /* First see if fname is in list of previously read RC files. */
  found_rc_filename_element = g_list_find_custom(toplevel->RC_list,
                                                 (gconstpointer) fname,
                                                 (GCompareFunc) strcmp);
  if (found_rc_filename_element != NULL) {
    /* We've already read this one in. */
    s_log_message(_("RC file [%s] already read in.\n"), fname);
    return 0;
  }

  /* Now try to read in contents of RC file.  */
  if (access (fname, R_OK) == 0) {
    g_read_file (fname);
    found_rc = 1;
    /* Everything was OK.  Now add this file to list of read RC files. */
    toplevel->RC_list = g_list_append (toplevel->RC_list,
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
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] rcname     System RC file name to parse.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_system_rc(TOPLEVEL *toplevel, const gchar *rcname)
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
  filename = f_normalize_filename (tmp, NULL);
  if (filename == NULL) {
    return 0;
  }

  ok_msg  = g_strdup_printf (_("Read system-%s file [%%s]\n"),
                             rcname);
  err_msg = g_strdup_printf (_("Did not find required system-%s file [%%s]\n"),
                             rcname);  
  found_rc = g_rc_parse_general(toplevel, filename, ok_msg, err_msg);

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
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] rcname     User's RC file name.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_home_rc(TOPLEVEL *toplevel, const gchar *rcname)
{
  const gchar *home;
  gint found_rc;
  gchar *tmp;
  char *filename;
  gchar *ok_msg, *err_msg;

  home = g_getenv ("HOME");
  if (home == NULL)
     home = g_get_home_dir ();

  if (home == NULL) {
    return 0;
  }

  tmp = g_build_filename (home, ".gEDA", rcname, NULL);
  filename = f_normalize_filename (tmp, NULL);
  if (filename == NULL) {
    return 0;
  }

  ok_msg  = g_strdup_printf (_("Read ~/.gEDA/%s file [%%s]\n"),
                             rcname);
  err_msg = g_strdup_printf (_("Did not find optional ~/.gEDA/%s file [%%s]\n"),
                             rcname);  
  found_rc = g_rc_parse_general(toplevel, filename, ok_msg, err_msg);
  
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
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] rcname     Local directory RC file name.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_local_rc(TOPLEVEL *toplevel, const gchar *rcname)
{
  gint found_rc;
  char *filename;
  gchar *ok_msg;
  gchar *err_msg;

  filename = f_normalize_filename (rcname, NULL);
  if (filename == NULL) {
    return 0;
  }

  ok_msg  = g_strdup_printf (_("Read local %s file [%%s]\n"),
                             rcname);
  err_msg = g_strdup_printf (_("Did not find optional local %s file [%%s]\n"),
                             rcname);  
  found_rc = g_rc_parse_general(toplevel, filename, ok_msg, err_msg);

  g_free(ok_msg);
  g_free(err_msg);
  g_free(filename);

  return found_rc;
}

/*! \brief Parse a RC file from a specified location.
 *  \par Function Description
 *  This function will open and parse a RC file from a specified location.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] rcname     Specified location RC file name.
 *  \return 1 on success, 0 on failure.
 */
gint g_rc_parse_specified_rc(TOPLEVEL *toplevel, const gchar *rcname)
{
  gint found_rc = 0;
  char *filename;
  gchar *rcbasename;
  gchar *ok_msg;
  gchar *err_msg;

  if (rcname == NULL) {
    return 0;
  }

  filename = f_normalize_filename (rcname, NULL);
  if (filename == NULL) {
    return 0;
  }

  rcbasename = g_path_get_basename (rcname);

  ok_msg  = g_strdup_printf (_("Read specified %s file [%%s]\n"),
                             rcbasename);
  err_msg = g_strdup_printf (_("Did not find specified %s file [%%s]\n"),
                             rcbasename);
  found_rc = g_rc_parse_general(toplevel, filename, ok_msg, err_msg);
  
  g_free(ok_msg);
  g_free(err_msg);
  g_free(filename);
  g_free(rcbasename);

  return found_rc;
}

/*! \brief General RC file parsing function.
 *  \par Function Description
 *  This function will check for System, HOME and Local RC files matching
 *  the rcname input parameter.  If none of those three are found it will
 *  search for the specified_rc_filename.  When none are found it will
 *  call exit(-1) to terminate the program.
 *
 *  \param [in] toplevel              The TOPLEVEL object.
 *  \param [in] rcname                 RC file name.
 *  \param [in] specified_rc_filename  Specific location RC file name.
 *  \return calls exit(-1) when no RC file matching either rcname or
 *          specified_rc_filename is found.
 */
void g_rc_parse(TOPLEVEL *toplevel,
		const gchar *rcname, const gchar *specified_rc_filename)
{
  gint found_rc = 0;
  char *rc_path;

  /* set the GEDADATARC environment variable so that the rc files */
  /* know where to look for others */
  rc_path = f_normalize_filename (g_rc_parse_path (), NULL);

  g_setenv ("GEDADATARC", rc_path, TRUE);
  g_free (rc_path);
  
  /* visit rc files in order */
  /* Changed by SDB 1.2.2005 in response to Peter Kaiser's bug report.
   * Read gafrc files first */
  found_rc |= g_rc_parse_system_rc(toplevel, "gafrc");
  found_rc |= g_rc_parse_home_rc(toplevel, "gafrc");
  found_rc |= g_rc_parse_local_rc(toplevel, "gafrc");
  /* continue support for individual rc files for each program.  */
  found_rc |= g_rc_parse_system_rc(toplevel, rcname);
  found_rc |= g_rc_parse_home_rc(toplevel, rcname);
  found_rc |= g_rc_parse_local_rc(toplevel, rcname);

  /* New fcn introduced by SDB to consolidate this & make it available 
   * for other programs */
  found_rc |= g_rc_parse_specified_rc(toplevel, specified_rc_filename);

  /* Oh well, I couldn't find any rcfile, exit! */
  if (!found_rc) {
    /*! \todo these two are basically the
     * same. Inefficient!
     */
    s_log_message(_("Could not find any %s file!\n"), rcname);
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
  gchar *string;
  char *namestr = NULL;

  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "component-library");
  
  if (name != SCM_UNDEFINED) {
    SCM_ASSERT (scm_is_string (name), name,
		SCM_ARG2, "component-library");
    namestr = SCM_STRING_CHARS (name);
  }
  
  /* take care of any shell variables */
  string = s_expand_env_variables (SCM_STRING_CHARS (path));

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
    temp = g_build_filename (cwd, string, NULL);
    s_clib_add_directory (temp, namestr);
    g_free(temp);
    g_free(cwd);
  }

  g_free(string);

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
SCM g_rc_component_library_command (SCM listcmd, SCM getcmd, 
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

  /* take care of any shell variables */
  /*! \bug this may be a security risk! */
  tmp_str = scm_to_locale_string (listcmd);
  lcmdstr = s_expand_env_variables (tmp_str);
  free (tmp_str); /* this should stay as free (allocated from guile) */

  /* take care of any shell variables */
  /*! \bug this may be a security risk! */
  tmp_str = scm_to_locale_string (getcmd);
  gcmdstr = s_expand_env_variables (tmp_str);
  free (tmp_str); /* this should stay as free (allocated from guile) */

  namestr = scm_to_locale_string (name);

  src = s_clib_add_command (lcmdstr, gcmdstr, namestr);

  free (namestr); /* this should stay as free (allocated from guile) */
  g_free (lcmdstr);
  g_free (gcmdstr);

  if (src != NULL) return SCM_BOOL_T;

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
  SCM_ASSERT (scm_is_true (scm_procedure_p (listfunc)), listfunc, SCM_ARG1,
	      "component-library-funcs");
  SCM_ASSERT (scm_is_true (scm_procedure_p (getfunc)), getfunc, SCM_ARG2,
	      "component-library-funcs");
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG1, 
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
  gchar *string;
  GDir *dir;
  const gchar *entry;
  
  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "component-library-search");

  /* take care of any shell variables */
  string = s_expand_env_variables (SCM_STRING_CHARS (path));

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
      gchar *fullpath = g_build_filename (string, entry, NULL);

      if (g_file_test (fullpath, G_FILE_TEST_IS_DIR)) {
        if (g_path_is_absolute (fullpath)) {
          s_clib_add_directory (fullpath, NULL);
        } else {
          gchar *cwd = g_get_current_dir ();
          gchar *temp;
          temp = g_build_filename (cwd, fullpath, NULL);
          s_clib_add_directory (temp, NULL);
          g_free(temp);
          g_free(cwd);
        }
      }
      g_free(fullpath);
    }
  }

  g_free(string);

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
  gchar *string;
  
  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "source-library");

  /* take care of any shell variables */
  string = s_expand_env_variables (SCM_STRING_CHARS (path));
  
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
    temp = g_build_filename (cwd, string, NULL);
    s_slib_add_entry (temp);
    g_free(temp);
    g_free(cwd);
  }
  
  g_free(string);
  
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
  gchar *string;
  GDir *dir;
  const gchar *entry;
  
  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "source-library-search");

  /* take care of any shell variables */
  string = s_expand_env_variables (SCM_STRING_CHARS (path));

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
    g_free(string);
    return SCM_BOOL_F;
  }

  while ((entry = g_dir_read_name (dir))) {
    /* don't do . and .. and special case font */
    if ((g_strcasecmp (entry, ".")    != 0) && 
        (g_strcasecmp (entry, "..")   != 0) &&
        (g_strcasecmp (entry, "font") != 0))
    {
      gchar *fullpath = g_build_filename (string, entry, NULL);

      if (g_file_test (fullpath, G_FILE_TEST_IS_DIR)) {
        if (s_slib_uniq (fullpath)) {
          if (g_path_is_absolute (fullpath)) {
            s_slib_add_entry (fullpath);
          } else {
            gchar *cwd = g_get_current_dir ();
            gchar *temp;
            temp = g_build_filename (cwd, fullpath, NULL);
            s_slib_add_entry (temp);
            g_free(temp);
            g_free(cwd);
          }
        }
      }
      g_free(fullpath);
    }
  }

  g_free(string);

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
SCM g_rc_untitled_name(SCM name)
{
  SCM_ASSERT (scm_is_string (name), name,
              SCM_ARG1, "untitled-name");

  g_free(default_untitled_name);

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
  gchar *string;

  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "font-directory");

  /* take care of any shell variables */
  string = s_expand_env_variables (SCM_STRING_CHARS (path));

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to font-directory\n",
             string);
    g_free(string);
    return SCM_BOOL_F;
  }

  g_free(default_font_directory);
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
  gchar *string;

  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "scheme-directory");

  /* take care of any shell variables */
  string = s_expand_env_variables (SCM_STRING_CHARS (path));

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to scheme-directory\n",
             string);
    g_free(string);
    return SCM_BOOL_F;
  }

  g_free(default_scheme_directory);
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
  gchar *string;

  SCM_ASSERT (scm_is_string (path), path,
              SCM_ARG1, "bitmap-directory");
  
  /* take care of any shell variables */
  string = s_expand_env_variables (SCM_STRING_CHARS (path));

  /* invalid path? */
  if (!g_file_test (string, G_FILE_TEST_IS_DIR)) {
    fprintf (stderr,
             "Invalid path [%s] passed to bitmap-directory\n",
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
  SCM_ASSERT (scm_is_string (scmsymname), scmsymname,
              SCM_ARG1, "bus-ripper-symname");

  g_free(default_bus_ripper_symname);
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
  SCM_ASSERT (scm_is_string (scmsymname), scmsymname,
              SCM_ARG1, "postsript-prolog");

  g_free(default_postscript_prolog);

  /* take care of any shell variables */
  default_postscript_prolog =
    s_expand_env_variables (SCM_STRING_CHARS (scmsymname));

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

  SCM_ASSERT (scm_is_string (scmcharstr), scmcharstr,
              SCM_ARG1, "map-font-character-to-file");
  SCM_ASSERT (scm_is_string (scmfilename), scmfilename,
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
  filename = s_expand_env_variables (filename);

  character = g_utf8_get_char_validated (charstr, -1);
  
  /* insert the new character declaration in the hash table */
  g_hash_table_insert (font_char_to_file,
                       GUINT_TO_POINTER ((guint)character),
                       filename);

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
    s_log_message(_("WARNING: using a string for 'always-promote-attributes'"
		    " is deprecated. Use a list of strings instead\n"));

    /* convert the space separated strings into a GList */
    attr2 = g_strsplit(SCM_STRING_CHARS (attrlist)," ", 0);
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
      SCM_ASSERT(scm_is_string(scm_list_ref(attrlist, scm_from_int(i))), 
		 scm_list_ref(attrlist, scm_from_int(i)), SCM_ARG1, 
		 "always-promote-attribute: list element is not a string");
      attr = g_strdup(SCM_STRING_CHARS(scm_list_ref(attrlist, scm_from_int(i))));
      list = g_list_prepend(list, attr);
    }
  }

  default_always_promote_attributes = g_list_reverse(list);

  return SCM_BOOL_T;
}
