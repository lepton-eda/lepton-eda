/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
#include <sys/stat.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "liblepton_priv.h"

#include <time.h>

/*! Default setting for log update callback function. */
void (*x_log_update_func)(const gchar*, GLogLevelFlags, const gchar*) = NULL;

/*! Default setting for log enable. */
int do_logging = TRUE;

#define CATCH_LOG_LEVELS (G_LOG_LEVEL_MASK ^ \
                          (G_LOG_LEVEL_DEBUG | G_LOG_LEVEL_INFO))
#define PRINT_LOG_LEVELS (CATCH_LOG_LEVELS ^ \
                          (G_LOG_LEVEL_WARNING | G_LOG_LEVEL_MESSAGE))

#define LOG_OPEN_ATTEMPTS 5

static void s_log_handler (const gchar *log_domain,
                           GLogLevelFlags log_level,
                           const gchar *message,
                           gpointer user_data);

static int logfile_fd = -1;

static guint log_handler_id;


/*! \brief Test if logging is enabled.
 *
 *  \return Return 1 if logging is enabled, 0 otherwise.
 */
int
lepton_log_get_logging_enabled ()
{
  return do_logging;
}

/*! \brief Enable or disable logging.
 *
 *  \param [in] enable Disable logging if the value is 0,
 *  otherwise enable it.
 */
void
lepton_log_set_logging_enabled (int enable)
{
  do_logging = enable;
}


/*! \brief Initialize libgeda logging feature.
 *  \par Function Description
 *  This function opens the file <B>filename</B> to log to and registers the
 *  handler to redirect log message to this file.
 *
 *  \param [in] prefix  Character string with file name prefix to log to.
 */
void s_log_init (const gchar *prefix)
{
  /* FIXME we assume that the prefix is in the filesystem encoding. */

  time_t nowt;
  struct tm *nowtm;
  gchar *full_prefix = NULL;
  size_t full_prefix_len = 0;
  gchar *dir_path = NULL;
  gchar *filename = NULL;
  int s, i;
  int last_exist_logn = 0;
  GDir *logdir = NULL;

  if (logfile_fd != -1) {
    g_critical ("s_log_init: Log already initialised.\n");
    return;
  }
  if (do_logging == FALSE) {
    return;
  }

  time (&nowt);
  nowtm = gmtime (&nowt);

  /* create "real" prefix -- this has the form "<prefix>-<date>-" */
  full_prefix = g_strdup_printf ("%s-%04i%02i%02i-", prefix,
                                 nowtm->tm_year + 1900, nowtm->tm_mon + 1,
                                 nowtm->tm_mday);
  full_prefix_len = strlen (full_prefix);

  /* Find/create the directory where we're going to put the logs.
   * FIXME should this be configured somehow?
   *
   * Then run through it finding the "biggest" existing filename with
   * a matching prefix & date. */
  dir_path = g_build_filename (eda_get_user_cache_dir(), "logs", NULL);
  /* Try to create the directory. */
  s = g_mkdir_with_parents (dir_path, 0777/*octal*/);
  if (s != 0) {
    /* It's okay to use the logging functions from here, because
     * there's already a default handler. */
    g_warning ("Could not create log directory %1$s: %2$s\n",
               dir_path, strerror (errno));
    g_free (dir_path);
    g_free (full_prefix);
    return;
  }

  logdir = g_dir_open (dir_path, 0, NULL);
  while (TRUE) {
    const gchar *file = g_dir_read_name (logdir);
    int n;
    if (file == NULL) break;
    if (strncmp (full_prefix, file, full_prefix_len)) continue;

    s = sscanf (file + full_prefix_len, "%i", &n);
    if (s != 1) continue;

    if (n > last_exist_logn) last_exist_logn = n;
  }
  g_dir_close (logdir);

  /* Now try and create a new file. When we fail, increment the number. */
  i = 0;
  while (logfile_fd == -1 && (LOG_OPEN_ATTEMPTS > i++)) {
    filename = g_strdup_printf ("%s%s%s%i.log", dir_path,
                                G_DIR_SEPARATOR_S, full_prefix,
                                ++last_exist_logn);
    logfile_fd = open (filename, O_RDWR|O_CREAT|O_EXCL, 0600);

    if (logfile_fd == -1 && (errno != EEXIST)) break;
  }

  if (logfile_fd != -1) {

    /* install the log handler */
    log_handler_id = g_log_set_handler (NULL,
                                        (GLogLevelFlags) CATCH_LOG_LEVELS,
                                        s_log_handler,
                                        NULL);

  } else {
    /* It's okay to use the logging functions from here, because
     * there's already a default handler. */
    if (errno == EEXIST) {
      g_warning ("Could not create unique log filename in %1$s\n",
                 dir_path);
    } else {
      g_warning ("Could not create log file in %1$s: %2$s\n",
                 dir_path, strerror (errno));
    }
  }

  g_free (filename);
  g_free (dir_path);
  g_free (full_prefix);
}

/*! \brief Terminates the logging of messages.
 *  \par Function Description
 *  This function deregisters the handler for redirection to the log file
 *  and closes it.
 */
void s_log_close (void)
{
  do_logging = FALSE; /* subsequent messages are lost after the close */

  if (logfile_fd == -1)
  {
    return;
  }

  /* remove the handler */
  g_log_remove_handler (NULL, log_handler_id);

  /* close the file */
  if (logfile_fd != -1) {
    close (logfile_fd);
    logfile_fd = -1;
  }

}

/*! \brief  Reads the current log file and returns its contents.
 *  \par Function Description
 *  This function reads the current log file and returns its contents.
 *
 *  \return Character string with current log's contents.
 *
 */
gchar *s_log_read (void)
{
  gboolean tmp;
#define BUFSIZE 200
  gchar buf[BUFSIZE];
  GString *contents;
  gint len;

  if (logfile_fd == -1) {
    return NULL;
  }

  tmp = do_logging;
  do_logging = FALSE;

  /* rewind the file */
  lseek(logfile_fd, 0, SEEK_SET);

  /* read its contents and build a string */
  contents = g_string_new ("");
  while ((len = read (logfile_fd, &buf, BUFSIZE)) != 0) {
    contents = g_string_append_len (contents, buf, len);
  }

  do_logging = tmp;

  return g_string_free (contents, FALSE);
}

/*! \brief Write a message to the current log file.
 *  \par Function Description
 *  Writes <B>message</B> to the current log file whose file descriptor
 *  is <B>logfile_fd</B>.
 *
 *  It also sends <B>message</B> to the optional function <B>x_log_update</B>
 *  for further use.
 *
 *  \param [in] log_domain  (unused).
 *  \param [in] log_level   (unused).
 *  \param [in] message     Character string containing message to
 *                          write to log.
 *  \param [in] user_data   (unused).
 *
 */
static void s_log_handler (const gchar *log_domain,
                           GLogLevelFlags log_level,
                           const gchar *message,
                           gpointer user_data)
{
  if (do_logging == FALSE) {
    return;
  }
  g_return_if_fail (logfile_fd != -1);

  size_t len = strlen (message);
  int status = 0;
  if (status >= 0)
    status = write (logfile_fd, message, len);
  if (status >= 0)
    status = write (logfile_fd, "\n", 1);
  if (status == -1) {
    fprintf(stderr, "Could not write message to log file\n");
  }
  if ((status == -1) || (log_level & PRINT_LOG_LEVELS)) {
    /* If messages are serious or writing to file failed, call the
     * default handler to write to the console. */
    g_log_default_handler (log_domain, log_level, message, NULL);
  }

  if (x_log_update_func) {
    (*x_log_update_func) (log_domain, log_level, message);
  }

}


/* Helper functions to construct GLogLevelFlags values in
   Scheme.  We don't just list their current values in Scheme code
   since GLogLevelFlag is an opaque Glib enum and the flag values
   may change in future. */

GLogLevelFlags
lepton_log_flag_fatal ()
{
  return G_LOG_FLAG_FATAL;
}

GLogLevelFlags
lepton_log_level_error ()
{
  return G_LOG_LEVEL_ERROR;
}

GLogLevelFlags
lepton_log_level_critical ()
{
  return G_LOG_LEVEL_CRITICAL;
}

GLogLevelFlags
lepton_log_level_warning ()
{
  return G_LOG_LEVEL_WARNING;
}

GLogLevelFlags
lepton_log_level_message ()
{
  return G_LOG_LEVEL_MESSAGE;
}

GLogLevelFlags
lepton_log_level_info ()
{
  return G_LOG_LEVEL_INFO;
}

GLogLevelFlags
lepton_log_level_debug ()
{
  return G_LOG_LEVEL_DEBUG;
}
