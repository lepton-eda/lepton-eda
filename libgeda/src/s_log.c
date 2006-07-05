/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDARG_H
#include <stdarg.h>
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

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "globals.h"
#include "funcs.h"
#include "o_types.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


static void s_log_handler (const gchar *log_domain,
                           GLogLevelFlags log_level,
                           const gchar *message,
                           gpointer user_data);

static int logfile_fd = -1;

static guint log_handler_id;

/*! \brief Initialize libgeda logging feature.
 *  \par Function Description
 *  This function opens the file <B>filename</B> to log to and registers the
 *  handler to redirect log message to this file.
 *
 *  \param [in] filename  Character string with file name to log to.
 */
void s_log_init (const gchar *filename)
{
  if (do_logging == FALSE) {
    logfile_fd = -1;
    return;
  }

  /* create log file */
  logfile_fd = open (filename, O_RDWR|O_CREAT|O_TRUNC, 0600);
  if (logfile_fd == -1) {
    fprintf(stderr, "Could not open log file: %s\n", filename);
    fprintf(stderr, "Errno was: %d\n", errno);
    return;
  }

  /* install the log handler */
  log_handler_id = g_log_set_handler (NULL,
                                      G_LOG_LEVEL_MESSAGE,
                                      s_log_handler,
                                      NULL);

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
  int status;

  if (do_logging == FALSE) {
    return;
  }
  g_assert (logfile_fd != -1);
  
  status = write (logfile_fd, message, strlen (message));
  if (status == -1) {
    fprintf(stderr, "Could not write message to log file\n");
    fprintf(stderr, "Message was: %s\n", message);
    fprintf(stderr, "Errno was: %d\n", errno);
  }

  if (x_log_update_func) {
    (*x_log_update_func) (message);
  }

}
