/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998 Ales V. Hvezda
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdarg.h>
#include <fcntl.h>
#include <errno.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include "defines.h"
#include "struct.h"
#include "defines.h"
#include "s_passing.h"
#include "globals.h"

#include "funcs.h"
#include "o_types.h"
#include "../include/prototype.h"

#ifndef GTK_DEVEL
extern char* g_vsprintf (const gchar *fmt, va_list *args, va_list *args2);
#endif

/* This function goes and finds the associated source files and loads ALL up */
/* only works for schematic files though */
void
s_log_init(char *filename)
{
	if (do_logging == FALSE) {
		logfile_fd = -1;
		return;
	}

	/* create log file */
	logfile_fd = open(filename, O_RDWR|O_CREAT|O_TRUNC, 0600);	

	if (logfile_fd == -1) {
		logfile_fd = -1;
		do_logging = FALSE;
		fprintf(stderr, "Could not open log file: %s\n", "gschem.log");
		fprintf(stderr, "Errno was: %d\n", errno);
	}
}

/* limit on a message is 240 bytes */

#define MSG_MAXLEN 240

void
s_log_message(const gchar *format, ...)
{
#if GTK_DEVEL
	va_list args;
#else
	va_list args; 
#endif
	char *buf=NULL;
	int len;
	int status;

	if (do_logging == FALSE) {
		return;
	}

#if 0 /* blah I don't like any of this. */
/* delete it soon */
#if GTK_DEVEL
	printf("here!\n");
        va_start (args, format);
        buf = g_strdup_vprintf (format, &args);
        va_end (args);
#else
	va_start (args, format);
	va_start (args2, format);
	buf = g_vsprintf (format, &args, &args2);
	va_end (args);
	va_end (args2);
#endif
#endif
	
	buf = (char *) malloc(sizeof(char)*MSG_MAXLEN);	

	va_start(args, format);	
	vsnprintf(buf, MSG_MAXLEN, format, args);
	va_end(args);

	if (buf == NULL) {
		return;
	}


	if (logfile_fd == -1) 
		return;

	len = strlen(buf);

	status = write(logfile_fd, buf, len);

	/* I'm not sure if tty vs both vs window_log stuff is working hack */
	/* libhack */
	/* temp out of commission */
	if (x_log_update_func)
		(*x_log_update_func)(buf);

	if (status == -1) {
		fprintf(stderr, "Could not write message to log file\n");
		fprintf(stderr, "Errno was: %d\n", errno);
	}

	free(buf);
}


void
s_log_close(void)
{
	if (do_logging == FALSE) {
		return;
	}

	do_logging = FALSE; /* subsequent messages are lost after the close */

	if (logfile_fd != -1) {
		close(logfile_fd);
		logfile_fd = -1;
	}
}
