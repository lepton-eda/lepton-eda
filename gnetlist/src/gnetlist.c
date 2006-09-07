/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist 
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
#include <sys/param.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

void gnetlist_quit(void)
{
    s_clib_free();
    s_slib_free();
    s_rename_destroy_all();
    /* o_text_freeallfonts(); */
    
    /* Free GSList *backend_params */
    g_slist_free (backend_params);
}

void main_prog(void *closure, int argc, char *argv[])
{
    int i;
    char input_str[2048];
    int argv_index;
    char *cwd;
    GSList *list_pnt;
    gchar *logfile;

    TOPLEVEL *pr_current;
  
    /* set default output filename */
    output_filename =
	(char *) g_malloc(sizeof(char) * (strlen("output.net") + 1));
    strcpy(output_filename, "output.net");


    argv_index = parse_commandline(argc, argv);
    cwd = getcwd(NULL, MAXPATHLEN);
#ifdef __MINGW32__
    u_basic_strip_trailing(cwd, G_DIR_SEPARATOR);
#endif

    /* this is a kludge to make sure that spice mode gets set */
    /*  Hacked by SDB to allow spice netlisters of arbitrary name
     *	as long as they begin with "spice".  For example, this spice
     *  netlister is valid: "spice-sdb".
     */
    if (guile_proc) {
	if (strncmp(guile_proc, "spice", 5) == 0) {
	    netlist_mode = SPICE;
	    command_line = create_command_line(argc, argv);

	    printf("Command line passed = %s \n", command_line);

	}
    }

    libgeda_init();

    /* create log file right away */
    /* even if logging is enabled */
    logfile = g_build_path (G_DIR_SEPARATOR_S,
                            cwd,
                            "gnetlist.log",
                            NULL);
    s_log_init (logfile);
    g_free (logfile);

    s_log_message("gEDA/gnetlist version %s%s\n", VERSION, CUSTOM_VERSION);
    s_log_message
	("gEDA/gnetlist comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
    s_log_message
	("This is free software, and you are welcome to redistribute it under certain\n");
    s_log_message
	("conditions; please see the COPYING file for more details.\n\n");

    if (!quiet_mode) {
	fprintf(stderr, "gEDA/gnetlist version %s%s\n", VERSION, CUSTOM_VERSION);
	fprintf(stderr,
		"gEDA/gnetlist comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
	fprintf(stderr,
		"This is free software, and you are welcome to redistribute it under certain\n");
	fprintf(stderr,
		"conditions; please see the COPYING file for more details.\n\n");
    }

#ifdef __MINGW32__
    fprintf(stderr, "This is the MINGW32 port.\n\n");
#endif

    s_log_message ("Remember to check that your schematic has no errors using the drc2 backend.\n");
    s_log_message ("You can do it running 'gnetlist -g drc2 your_schematic.sch -o drc_output.txt'\n");
    s_log_message ("and seeing the contents of the file drc_output.txt.\n\n");

    if (!quiet_mode) {
      fprintf (stderr, "Remember to check that your schematic has no errors using the drc2 backend.\n");
      fprintf (stderr, "You can do it running 'gnetlist -g drc2 your_schematic.sch -o drc_output.txt'\n");
      fprintf (stderr, "and seeing the contents of the file drc_output.txt.\n\n");

    }
    /* register guile (scheme) functions */
    g_register_funcs();

    pr_current = s_toplevel_new ();
    g_rc_parse(pr_current, "gnetlistrc", rc_filename);
    /* immediately setup user params */
    i_vars_set (pr_current);

    s_rename_init();

    i = argv_index;
    while (argv[i] != NULL) {
      gchar *filename; 

#ifdef __MINGW32__
      if (argv[i][1] == ':' && (argv[i][2] == G_DIR_SEPARATOR ||
                                argv[i][2] == OTHER_PATH_SEPARATER_CHAR)) 
#else
      if (argv[i][0] == G_DIR_SEPARATOR) 
#endif
      {
        /* Path is already absolute so no need to do any concat of cwd */
        filename = g_strdup (argv[i]);
      } else {
        filename = g_build_path (G_DIR_SEPARATOR_S, cwd, argv[i], NULL);
      }

      if (!quiet_mode) {
        printf ("Loading schematic [%s]\n", filename);
      }

      s_page_goto (pr_current, s_page_new (pr_current, filename));
      
      if (!f_open (pr_current, filename)) {
        fprintf (stderr, "Couldn't load schematic [%s]\n", filename);
      }
      
      i++;
      g_free (filename);
    }
    
    /* Change back to the directory where we started.  This is done */
    /* since gnetlist is a command line utility and will deposit its output */
    /* in the current directory.  Having the output go to a different */
    /* directory will confuse the user (confused me, at first). */
    chdir(cwd);
    /* free(cwd); - Defered; see below */

    if (argv[argv_index] == NULL) {
	fprintf(stderr,
		"\nERROR! You must specify at least one filename\n\n");
	usage(argv[0]);
    }

    g_set_project_current(pr_current);
#if DEBUG
    s_page_print_all(pr_current);
#endif

    s_traverse_init();
    s_traverse_start(pr_current);
    /* s_traverse_start(pr_current, pr_current->page_current->object_head); */

    /* temporarly reuse input_str */

    sprintf(input_str, "%s%cgnetlist.scm", pr_current->scheme_directory, 
            G_DIR_SEPARATOR);

    /* Change back to the directory where we started AGAIN.  This is done */
    /* because the s_traverse functions can change the Current Working Directory. */
    chdir(cwd);
    free(cwd); /* allocated by getcwd, so this should stay as free() */

/* don't need either of these */
/*	gh_eval_str ("(primitive-load-path \"ice-9/boot-9.scm\")");*/
    /* scm_primitive_load_path (scm_makfrom0str ("ice-9/boot-9.scm")); */

    if (g_read_file(input_str) != -1) {
	s_log_message("Read init scm file [%s]\n", input_str);
    } else {
	s_log_message("Failed to read init scm file [%s]\n", input_str);
	fprintf(stderr, "Failed to read init scm file [%s]\n", input_str);
    }



    /* Load the first set of scm files */
    list_pnt = pre_backend_list;
    while (list_pnt) {
      if (g_read_file(list_pnt->data) != -1) {
        s_log_message("Read scm file [%s]\n", 
                      (char *) list_pnt->data);
      } else {
        s_log_message("Failed to read scm file [%s]\n", 
                      (char *) list_pnt->data);
        fprintf(stderr, "Failed to read scm file [%s]\n", 
                (char *) list_pnt->data);
      }
      list_pnt = g_slist_next(list_pnt);
    }
    /* Free now the list of configuration files */
    g_slist_free(pre_backend_list);


    if (guile_proc) {

	/* load the appropriate scm file */
	sprintf(input_str, "%s%cgnet-%s.scm", pr_current->scheme_directory,
		G_DIR_SEPARATOR, guile_proc);

	if (g_read_file(input_str) != -1) {
	    s_log_message("Read %s scm file [%s]\n", guile_proc,
			  input_str);
	} else {
	    s_log_message("Failed to read %s scm file [%s]\n",
			  guile_proc, input_str);

	    fprintf(stderr, "Failed to read %s scm file [%s]\n",
		    guile_proc, input_str);
	}


        /* Load second set of scm files */
        list_pnt = post_backend_list;
        while (list_pnt) {
          if (g_read_file(list_pnt->data) != -1) {
            s_log_message("Read scm file [%s]\n", 
                          (char *) list_pnt->data);
          } else {
            s_log_message("Failed to read scm file [%s]\n", 
                          (char *) list_pnt->data);
            fprintf(stderr, "Failed to read scm file [%s]\n", 
                 (char *) list_pnt->data);
          }
          list_pnt = g_slist_next(list_pnt);
        }
        /* Free now the list of configuration files */
        g_slist_free(post_backend_list);

	/* check size here hack */
	sprintf(input_str, "(%s \"%s\")", guile_proc, output_filename);
	scm_c_eval_string (input_str);
	/* gh_eval_str_with_stack_saving_handler (input_str); */
    } else if (interactive_mode) {
	scm_c_eval_string ("(set-repl-prompt! \"gnetlist> \")");
	scm_shell (0, NULL);
    } else {
	fprintf(stderr,
		"You gave neither backend to execute nor interactive mode!\n");

    }

    gnetlist_quit();
}

int main(int argc, char *argv[])
{
    /* disable the deprecated warnings in guile 1.6.3 */
    /* Eventually the warnings will need to be fixed */
    if(getenv("GUILE_WARN_DEPRECATED")==NULL)
      putenv("GUILE_WARN_DEPRECATED=no");

    scm_boot_guile (argc, argv, main_prog, 0);
    return 0;
}
