/* Lepton EDA Schematic Capture
 * Copyright (C) 2011-2013 gEDA Contributors
 * Copyright (C) 2017-2018 Lepton EDA Contributors
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
 * \param w_current  Current #GschemToplevel structure.
 * \param uri        URI to launch viewer for.
 * \param error      Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
x_show_uri (GschemToplevel *w_current, const gchar *uri,
            GError **error)
{
# if defined (SHOW_URI_GIO)

  g_assert (w_current);
  g_assert (uri);

#ifdef ENABLE_GTK3
  return gtk_show_uri_on_window (GTK_WINDOW (w_current->main_window),
                                 uri,
                                 GDK_CURRENT_TIME,
                                 error);
#else /* GTK2 */
  GdkScreen *screen =
    gtk_window_get_screen (GTK_WINDOW (w_current->main_window));
  return gtk_show_uri (screen, uri, GDK_CURRENT_TIME, error);
#endif

# elif defined (OS_WIN32) && !defined (OS_CYGWIN)
  return show_uri__win32 (uri, error);

# else
  gboolean spawn_status;
  GPid pid;
  gchar *argv[3];

  g_assert (uri);

  argv[0] = (gchar *) SHOW_URI_COMMAND;
  argv[1] = (gchar *) uri;
  argv[2] = NULL; /* Null-terminated */

  spawn_status = g_spawn_async (NULL, /* Inherit working directory */
                                argv,
                                NULL, /* Inherit environment */
                                G_SPAWN_SEARCH_PATH, /* Flags */
                                NULL, /* No child setup function */
                                NULL, /* No child setup function data */
                                &pid,
                                error);

  g_spawn_close_pid (pid);

  if (!spawn_status)
    return FALSE;

  return TRUE;

# endif
}
