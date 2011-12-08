/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 2011 gEDA Contributors (see ChangeLog for details)
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

#ifdef OS_WIN32
# define STRICT
# include <windows.h>
# undef STRICT
#endif

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#if defined (OS_WIN32)

/*! \brief Launch application to show URI on Windows.
 * \par Function Description
 * On native Windows, the ShellExecute Windows API function provides a
 * reliable way to open a URI in a default application.
 *
 * This function is called by x_show_uri().
 *
 * \param uri    URI to launch viewer for.
 * \param error  Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
static gboolean
show_uri__win32 (const gchar *uri, GError **error)
{

  /* On Windows, we need to use ShellExecute because allegedly GIO
   * doesn't cope very well with Windows. :-( */

  int status;
  gchar *msg = NULL;

  g_assert (uri);

  status =
    (int) ShellExecute (NULL, /* window handle */
                        "open",
                        uri,
                        NULL, /* No parameters (not launching application) */
                        NULL, /* Inherit working directory */
                        SW_SHOWNORMAL); /* Default application display mode */
  if (status > 32) {
    return TRUE;
  }

  if (status == 0) {
    msg = g_strdup (_("The operating system is out of memory or resources."));
  } else {
    LPVOID buf;
    FormatMessage ((FORMAT_MESSAGE_ALLOCATE_BUFFER |
                    FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS),
                   NULL,
                   status,
                   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                   (LPTSTR) &buf,
                   0,
                   NULL);
    msg = g_strdup ((gchar *) buf);
    LocalFree (buf);
  }
  /* \bug We should specify a domain and error code. */
  g_set_error (error, 0, 0, "%s", msg);
  g_free (msg);
  return FALSE;
}
#endif /* OS_WIN32 */

/*! \brief Launch default application for a URI.
 * \par Function Description
 * Launches the default application associated with \a uri on the host
 * platform.
 *
 * Depending on the way gEDA was configured, this may occur by one of
 * the following three methods:
 *
 * -# Calling gtk_show_uri() to use the GIO library (default on Linux)
 * -# Calling the ShellExecute() Windows API call (default on Windows)
 * -# Running an appropriate external tool.
 *
 * \param w_current  Current #GSCHEM_TOPLEVEL structure.
 * \param uri        URI to launch viewer for.
 * \param error      Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
x_show_uri (GSCHEM_TOPLEVEL *w_current, const gchar *uri,
            GError **error)
{
# if defined (SHOW_URI_GIO)
  GdkScreen *screen;

  g_assert (w_current);
  g_assert (uri);

  screen = gtk_window_get_screen (GTK_WINDOW (w_current->main_window));
  return gtk_show_uri (screen, uri, GDK_CURRENT_TIME, error);

# elif defined (OS_WIN32) && !defined (OS_CYGWIN)
  return show_uri__win32 (uri, error);

# else
  gboolean spawn_status;
  gint exit_status;
  gchar *argv[3];

  g_assert (uri);

  argv[0] = SHOW_URI_COMMAND;
  argv[1] = (gchar *) uri;
  argv[2] = NULL; /* Null-terminated */

  spawn_status = g_spawn_sync (NULL, /* Inherit working directory */
                               argv,
                               NULL, /* Inherit environment */
                               G_SPAWN_SEARCH_PATH, /* Flags */
                               NULL, /* No child setup function */
                               NULL, /* No child setup function data */
                               NULL, /* Don't need stdout */
                               NULL, /* Don't need stderr */
                               &exit_status,
                               error);

  if (!spawn_status) return FALSE;

  if (exit_status != 0) {
    g_set_error (error, G_SPAWN_ERROR, G_SPAWN_ERROR_FAILED,
                 _("%s failed to launch URI"), argv[0]);
    return FALSE;
  }

  return TRUE;

# endif
}
