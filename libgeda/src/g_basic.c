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
#include <stdlib.h>
#include <assert.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"

#include "o_types.h"

#include "../include/prototype.h"

int
ORIG_g_read_file(char *filename)
{
	FILE *fp;
	/* TODO: isn'y this hack?  */
	char buf[200];

	if (filename == NULL) {
		return(-1);
	}

	if (access(filename, R_OK) != 0) {
		s_log_message("Could not find [%s] for interpretion\n",
			      filename);
		return(-1);
	}

	fp = fopen(filename, "r");

	if (fp == NULL) {
		s_log_message("Failed to open [%s] for interpretion\n",
			      filename);
		return(-1);
	}

	/* TODO: allow for one line to be 160 chars ! docmenent me */
	while (fgets(buf, 160, fp) != NULL) {
		gh_eval_str_with_stack_saving_handler (buf);
#if 0
		gh_display (gh_eval_str_with_stack_saving_handler (buf));
#endif
	}

	s_log_message("Interpreted [%s]\n", filename);
	return(0);
}

/* The following code was contributed by thi (with formating changes
 * by Ales) Thanks!  */

#if MAYBE
/* This `load()' is modeled after libguile/load.c, load().
 * Additionally, the most recent form read is saved in case something
 * goes wrong. */

static SCM most_recently_read_form = SCM_BOOL_F;

static SCM
load (void *data)
{
	SCM cur_out = scm_current_output_port (), port = (SCM) data, form;

	while (1) {
		form = scm_read(port);
		scm_display(scm_makfrom0str ("Form: "), cur_out);
		scm_display(form, cur_out);
		scm_newline(cur_out);

		if (SCM_EOF_OBJECT_P(form)) {
			break;
		}

		most_recently_read_form = form;
		scm_eval_x(form);
	}

	most_recently_read_form = SCM_BOOL_F;

	return SCM_BOOL_T;
}
#endif /* MAYBE */

static SCM
load_error_handler(void *data, SCM tag, SCM throw_args)
{
	SCM cur_out = scm_current_output_port ();

	scm_display(scm_makfrom0str((char *)data), cur_out);
	scm_display(scm_makfrom0str(": Weirdness alert -- tag is "), cur_out);
	scm_display(tag, cur_out);
	scm_newline(cur_out);

#if MAYBE
	if (most_recently_read_form != SCM_BOOL_F) {
		scm_display(scm_makfrom0str ("Most recently read form: "),
			    cur_out);
		scm_display(most_recently_read_form, cur_out);
		scm_newline(cur_out);
	}
#endif /* MAYBE */

	return SCM_BOOL_F;
}

int
g_read_file(char *filename)
{
#if MAYBE
	SCM port;
#endif

	if (filename == NULL) {
		return(-1);
	}

	if (access(filename, R_OK) != 0) {
		s_log_message("Could not find [%s] for interpretion\n",
			      filename);
		return(-1);
  	}

#if MAYBE
	/* Use this if you want to use the `load()' above.  Otherwise,
	 * the standard `gh_eval_file()' works but doesn't leave clues
	 * if there is a problem. */

	port = scm_open_file(scm_makfrom0str(filename),
			     scm_makfromstr("r",
					    (scm_sizet) sizeof (char),
					    0));

	return (gh_catch (SCM_BOOL_T,
			  (scm_catch_body_t) load, port,
			  (scm_catch_handler_t) load_error_handler, filename)
		== SCM_BOOL_T);
#endif /* MAYBE */

	return (gh_catch (SCM_BOOL_T,
			  (scm_catch_body_t) gh_eval_file, filename,
			  (scm_catch_handler_t) load_error_handler, filename)
		== SCM_BOOL_T);
}

