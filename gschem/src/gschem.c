/* gEDA - GNU Electronic Design Automation
 * gschem - GNU Schematic Capture
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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h>
#include <signal.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/struct.h>
#include <libgeda/globals.h>
#include <libgeda/defines.h>
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAS_LIBSTROKE
/* libstroke prototype */
void stroke_init(void);
#endif

void
gschem_quit(void)
{
	s_clib_cache_free();
        s_clib_free();
        s_slib_free();
        /* o_text_freeallfonts();*/
        s_attrib_free();
	s_papersizes_free();
	x_stroke_free_all();
	s_stroke_free();

	/* x_window_free_head(); can't do this since it causes a
	 * condition in which window_head->... is still being refered
	 * after this */

	/* g_mem_profile();*/
	gtk_main_quit();
}

void
main_prog(int argc, char *argv[])
{
	int i;
	TOPLEVEL *w_current;
	/* TODO: should the size be flexible? */
	char input_str[256];
	int argv_index;
        int first_page = 1;

	gtk_init(&argc, &argv);
	visual = gdk_visual_get_system();

	argv_index = parse_commandline(argc, argv);

	/* TODO: Probably the file name shuold be defined elsewhere */
	/* create log file right away even if logging is enabled */
	s_log_init("gschem.log");

	s_log_message("gEDA: gschem version %s - THIS IS AN ALPHA RELEASE!\n",
		      VERSION);

#if HAS_LIBSTROKE
	stroke_init(); /* libstroke function */
	s_stroke_init(); /* libgeda function */
#endif

	if (!quiet_mode) {
		fprintf(stderr,
			"THIS IS AN ALPHA RELEASE! version %s\n", VERSION);
	}

	/* register guile (scheme) functions */
	g_register_funcs();

	s_clib_init();
 	s_slib_init();
	s_attrib_init();

	g_rc_parse();

        colormap = gdk_colormap_get_system ();
	x_window_setup_colors();

	x_window_add_head();

	w_current = x_window_create_new();
	global_window_current = w_current;

	/* so we can call key funcs from guile */
	set_window_current_key(w_current);

	/* o_text_init(); goes away */
	o_ntext_init();
	x_repaint_background(w_current);

	i = argv_index;
	while (argv[i] != NULL) {
		if (first_page) {
			if (w_current->page_current->page_filename) {
				free(w_current->page_current->page_filename);
			}

			/* Page structure has already been created...
                         * so, just set the filename and open the
                         * schematic for the first page */
			w_current->page_current->page_filename =
				malloc(sizeof(char) * strlen(argv[i]) + 5);
                	strcpy(w_current->page_current->page_filename,
			       argv[i]);

			w_current->DONT_REDRAW = 1;
			/* this needed so that display is shown
			 * properly with negative coords */
			w_current->DONT_RECALC = 1;
			if (!quiet_mode) {
				printf("Loading schematic [%s]\n", argv[i]);
			}
                	f_open(w_current,
			       w_current->page_current->page_filename);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);

			a_zoom_limits(w_current,
				      w_current->page_current->object_head);

			/* now update the scrollbars */
			x_hscrollbar_update(w_current);
			x_vscrollbar_update(w_current);

			first_page = 0;
		} else {
			/* Much simpler	*/
			s_page_new(w_current, argv[i]);
			if (!quiet_mode) {
				printf("Loading schematic [%s]\n", argv[i]);
			}
                	f_open(w_current,
			       w_current->page_current->page_filename);
			i_set_filename(w_current,
				       w_current->page_current->page_filename);
			a_zoom_limits(w_current,
				      w_current->page_current->object_head);
			/* now update the scrollbars */
			x_hscrollbar_update(w_current);
			x_vscrollbar_update(w_current);
		}
		i++;
	}

	if (argv[argv_index] == NULL) {
		if (w_current->page_current->page_filename) {
			free(w_current->page_current->page_filename);
		}

		getcwd(w_current->cwd, 256);

		w_current->page_current->page_filename =
			malloc(sizeof(char) * (
				strlen(w_current->cwd) +
				strlen(w_current->untitled_name) +
                       		strlen("/_##########.sch") +
				1));

		w_current->num_untitled++;
		sprintf(w_current->page_current->page_filename,
			"%s/%s_%d.sch",
			w_current->cwd,
			w_current->untitled_name,
			w_current->num_untitled);

		i_set_filename(w_current,
			       w_current->page_current->page_filename);

		/* update the scrollbars */
		x_hscrollbar_update(w_current);
		x_vscrollbar_update(w_current);
        }

	w_current->DONT_REDRAW = 0;
	w_current->DONT_RECALC = 0;

	o_redraw_all(w_current);

#if DEBUG
	gh_eval_str ("(display \"hello guile\n\")");
#endif

	if (w_current->scheme_directory == NULL) {
		fprintf(stderr, "Scheme directory NOT set!\n");
		exit(-1);
	}

	sprintf(input_str, "%s/gschem.scm", w_current->scheme_directory);
	if (g_read_file(input_str) != -1) {
		s_log_message("Read init scm file [%s]\n", input_str);
	} else {
		/* TODO: These two messages are the same. Should be
                 * integrated. */
		s_log_message("Failed to read init scm file [%s]\n",
			      input_str);
		fprintf(stderr,
			"Failed to read init scm file [%s]\n", input_str);
	}

	/* Execute a script if it exists */
	if (script_filename) {
		s_log_message("Executing guile script [%s]\n",
			      script_filename);
		g_read_file(script_filename);
	}

	/* open up log window on startup */
	if (w_current->log_window == MAP_ON_STARTUP) {
		x_log_setup_win(w_current);
	}

	/* enter main loop */
        gtk_main();
}

int
main (int argc, char *argv[])
{
	gh_enter(argc, argv, main_prog);
	return 0;
}
