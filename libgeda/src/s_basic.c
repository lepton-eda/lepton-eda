/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2010 gEDA Contributors (see ChangeLog for details)
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
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef G_OS_WIN32
#  ifndef STRICT
#    define STRICT
#    include <windows.h>
#    undef STRICT
#  endif
#  ifndef GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
#    define GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT 2
#    define GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS 4
#  endif
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* used by o_text_read */
char *remove_nl(char *string)
{
  int i;

  if (!string)
    return NULL;

  i = 0;
  while(string[i] != '\0' && string[i] != '\n' && string[i] != '\r') {
    i++;
  }

  string[i] = '\0';

  return(string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* used by o_text_read */
char *remove_last_nl(char *string)
{
  int len;

  if (!string)
    return NULL;

  len = strlen(string);
  if (string[len-1] == '\n' || string[len-1] == '\r')
    string[len-1] = '\0';

  return(string);
}

/*! \brief Expand environment variables in string.
 *  \par Function Description
 *  This function returns the passed string with environment variables
 *  expanded.
 *
 *  The invocations of environment variable MUST be in the form
 *  '${variable_name}', '$variable_name' is not valid here. Environment
 *  variable names consists solely of letters, digits and '_'. It is
 *  possible to escape a '$' character in the string by repeating it
 *  twice.
 *
 *  It outputs error messages to console and leaves the malformed and
 *  bad variable names in the returned string.
 *
 *  \param [in] string The string with variables to expand.
 *  \return A newly-allocated string with variables expanded or NULL
 *  if input string was NULL.
 */
gchar*
s_expand_env_variables (const gchar *string)
{
  GString *gstring;
  gint i;

  if (string == NULL) {
    return NULL;
  }

  gstring = g_string_sized_new (strlen (string));
  i = 0;
  while (TRUE) {
    gint start;

    start = i;
    /* look for end of string or possible variable name start */
    while (string[i] != '\0' && string[i] != '$') i++;
    g_string_append_len (gstring, string + start, i - start);
    if (string[i] == '\0') {
      /* end of string, return built string */
      return g_string_free (gstring, FALSE);
    }

    i++;
    switch (string[i]) {
        case ('{'):
          /* look for the end of the variable name */
          start = i;
          while (string[i] != '\0' && string[i] != '}') i++;
          if (string[i] == '\0') {
            /* problem: no closing '}' to variable */
            fprintf (stderr,
                     "Found malformed environment variable in '%s'\n",
                     string);
            g_string_append (gstring, "$");
            g_string_append_len (gstring, string + start, i - start + 1);
          } else {
            gint j;

            /* discard "${" */
            start = start + 1;
            /* test characters of variable name */
            for (j = start;
                 j < i && (g_ascii_isalnum (string[j]) || string[j] == '_');
                 j++);
            if (i != j) {
              /* illegal character detected in variable name */
              fprintf (stderr,
                       "Found bad character [%c] in variable name.\n",
                       string[j]);
              g_string_append (gstring, "${");
              g_string_append_len (gstring, string + start, i - start + 1);
            } else {
              /* extract variable name from string and expand it */
              gchar *variable_name = g_strndup (string + start, i - start);
              const gchar *env = g_getenv (variable_name);
              g_free (variable_name);
              g_string_append (gstring, (env == NULL) ? "" : env);
            }
            i++;
          }
          break;

        case ('$'):
          /* an escaped '$' */
          g_string_append_c (gstring, string[i++]);
          break;

        default:
          /* an isolated '$', put it in output */
          g_string_append_c (gstring, '$');
    }
  }

  /* never reached */
  return NULL;
}


/* -------------------------------------------------- */

#ifdef G_OS_WIN32

/* Get a module handle for the libgeda DLL.
 *
 * Adapted from GLib, originally licensed under LGPLv2+. */
static gpointer
libgeda_module_handle ()
{
  typedef BOOL (WINAPI *t_GetModuleHandleExA) (DWORD, LPCTSTR, HMODULE *);
  static t_GetModuleHandleExA p_GetModuleHandleExA = NULL;
  static gconstpointer address = (void (*)(void)) &libgeda_module_handle;
  static HMODULE hmodule = NULL;

  if (hmodule != NULL) return (gpointer) hmodule;

  if (p_GetModuleHandleExA == NULL) {
    p_GetModuleHandleExA =
      (t_GetModuleHandleExA) GetProcAddress (GetModuleHandle ("kernel32.dll"),
                                             "GetModuleHandleExA");
  }

  if (p_GetModuleHandleExA == NULL ||
      !(*p_GetModuleHandleExA) (GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT |
                                GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS,
                                address, &hmodule)) {
    MEMORY_BASIC_INFORMATION mbi;
    VirtualQuery (address, &mbi, sizeof (mbi));
    hmodule = (HMODULE) mbi.AllocationBase;
  }

  return (gpointer) hmodule;
}

#endif /* G_OS_WIN32 */

/*! \brief Get the directory with the gEDA system data.
 *  \par Function description
 *  Returns the path to be searched for gEDA data shared between all
 *  users. If the GEDADATA environment variable is set, returns its
 *  value; otherwise, uses a compiled-in path.
 *
 *  On Windows, the compiled in path is *not* used, as it might not
 *  match the path where the user has installed gEDA.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \todo On UNIX platforms we should follow the XDG Base Directory
 *  Specification.
 *
 *  \return the gEDA shared data path, or NULL if none could be found.
 */
const char *s_path_sys_data () {
  static const char *p = NULL;
  /* If GEDADATA is set in the environment, use that path */
  if (p == NULL) {
    p = g_getenv ("GEDADATA");
  }
  if (p == NULL) {
# if defined (G_OS_WIN32)
    /* On Windows, guess the path from the location of the libgeda
     * DLL. */
    gchar *d =
      g_win32_get_package_installation_directory_of_module (libgeda_module_handle ());
    p = g_build_filename (d, "share", "gEDA", NULL);
    g_free (d);
# else
    /* On other platforms, use the compiled-in path */
    p = GEDADATADIR;
# endif
  }
  return p;
}

/*! \brief Get the directory with the gEDA system configuration.
 *  \par Function description
 *  Returns the path to be searched for gEDA configuration shared
 *  between all users. If the GEDADATARC environment variable is set,
 *  returns its value; otherwise, uses a compiled-in path. Finally
 *  fallback to using the system data path.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \todo On UNIX platforms we should follow the XDG Base Directory
 *  Specification.
 *
 *  \return the gEDA shared config path, or NULL if none could be
 *  found.
 */
const char *s_path_sys_config () {
  static const char *p = NULL;

  /* If GEDADATARC is set in the environment, use that path */
  if (p == NULL) {
    p = g_getenv ("GEDADATARC");
  }
  if (p == NULL) {
#if defined (GEDARCDIR) && !defined(_WIN32)
    /* If available, use the rc directory set during configure. */
    p = GEDARCDIR;
#else
    /* Otherwise, just use the data directory */
    p = s_path_sys_data ();
#endif
  }
  return p;
}

/*! \brief Get the directory with the gEDA user configuration.
 *  \par Function description
 *  Returns the path to be searched for the current user's gEDA
 *  configuration. Currently defaults to a directory ".gEDA" in the
 *  user's home directory.
 *
 *  \warning The returned string is owned by libgeda and should not be
 *  modified or free'd.
 *
 *  \todo On Windows, we should use APPDATA.
 *
 *  \todo On UNIX platforms we should follow the XDG Base Directory
 *  Specification.
 */
const char *s_path_user_config () {
  static const char *p = NULL;

  if (p == NULL) {
    const char *home = g_getenv ("HOME");
    if (home == NULL) home = g_get_home_dir ();
    p = g_build_filename(home, ".gEDA", NULL);
  }
  return p;
}
