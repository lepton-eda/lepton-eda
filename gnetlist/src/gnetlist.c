/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
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
#include <version.h>
#include <missing.h>

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include <dirent.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#include <libgeda/libgeda.h>
#include <libgeda/libgedaguile.h>

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

    g_slist_free (input_files);
}


/* \brief Print a list of available backends.
 * \par Function Description
 * Prints a list of available gnetlist backends by searching for files
 * in each of the directories in the current Guile %load-path.  A file
 * is considered to be a gnetlist backend if its basename begins with
 * "gnet-" and ends with ".scm".
 *
 * \param pr_current  Current #TOPLEVEL structure.
 */
void
gnetlist_backends (TOPLEVEL *pr_current)
{
  SCM s_load_path;
  GList *backend_names = NULL, *iter = NULL;

  /* Look up the current Guile %load-path */
  s_load_path = scm_variable_ref (scm_c_lookup ("%load-path"));

  for ( ; s_load_path != SCM_EOL; s_load_path = scm_cdr (s_load_path)) {
    SCM s_dir_name = scm_car (s_load_path);
    char *dir_name;
    DIR *dptr;
    struct dirent *dentry;

    /* Get directory name from Scheme */
    g_assert (scm_is_true (scm_list_p (s_load_path))); /* Sanity check */
    g_assert (scm_is_string (scm_car (s_load_path))); /* Sanity check */
    dir_name = scm_to_utf8_string (s_dir_name);

    /* Open directory */
    dptr = opendir (dir_name);
    if (dptr == NULL) {
      g_warning ("Can't open directory %s: %s\n",
                 dir_name, strerror (errno));
      continue;
    }
    free (dir_name);

    while (1) {
      char *name;

      dentry = readdir (dptr);
      if (dentry == NULL) break;

      /* Check that filename has the right format to be a gnetlist
       * backend */
      if (!(g_str_has_prefix (dentry->d_name, "gnet-")
            && g_str_has_suffix (dentry->d_name, ".scm")))
        continue;

      /* Copy filename and remove prefix & suffix.  Add to list of
       * backend names. */
      name = g_strdup (dentry->d_name + 5);
      name[strlen(name)-4] = '\0';
      backend_names = g_list_prepend (backend_names, name);
    }

    /* Close directory */
    closedir (dptr);
  }

  /* Sort the list of backends */
  backend_names = g_list_sort (backend_names, (GCompareFunc) strcmp);

  printf ("List of available backends: \n\n");

  for (iter = backend_names; iter != NULL; iter = g_list_next (iter)) {
    printf ("%s\n", (char *) iter->data);
  }
  printf ("\n");

  scm_remember_upto_here_1 (s_load_path);
}


void main_prog(void *closure, int argc, char *argv[])
{
    int i;
    int argv_index;
    char *cwd;
    gchar *str;
    gchar *filename;

    TOPLEVEL *pr_current;

    /* set default output filename */
    output_filename = g_strdup("output.net");

    argv_index = parse_commandline(argc, argv);
    cwd = g_get_current_dir();

    scm_set_program_arguments (argc, argv, NULL);

    /* this is a kludge to make sure that spice mode gets set */
    /*  Hacked by SDB to allow spice netlisters of arbitrary name
     *        as long as they begin with "spice".  For example, this spice
     *  netlister is valid: "spice-sdb".
     */
    if (guile_proc) {
        if (strncmp(guile_proc, "spice", 5) == 0) {
            netlist_mode = SPICE;
        }
    }

    libgeda_init();

    /* create log file right away */
    /* even if logging is enabled */
    s_log_init ("gnetlist");

    s_log_message("gEDA/gnetlist version %s%s.%s\n", PREPEND_VERSION_STRING,
                  PACKAGE_DOTTED_VERSION, PACKAGE_DATE_VERSION);
    s_log_message
        ("gEDA/gnetlist comes with ABSOLUTELY NO WARRANTY; see COPYING for more details.\n");
    s_log_message
        ("This is free software, and you are welcome to redistribute it under certain\n");
    s_log_message
        ("conditions; please see the COPYING file for more details.\n\n");

#if defined(__MINGW32__) && defined(DEBUG)
    fprintf(stderr, "This is the MINGW32 port.\n\n");
#endif

    /* register guile (scheme) functions */
    g_register_funcs();

    scm_dynwind_begin (0);
    pr_current = s_toplevel_new ();
    edascm_dynwind_toplevel (pr_current);

    /* Evaluate Scheme expressions that need to be run before rc files
     * are loaded. */
    scm_eval (pre_rc_list, scm_current_module ());

    g_rc_parse (pr_current, argv[0], "gnetlistrc", rc_filename);
    /* immediately setup user params */
    i_vars_set (pr_current);

    s_rename_init();

    if(list_backends) {
      gnetlist_backends(pr_current);
      exit (0);
    }

    /* Evaluate the first set of Scheme expressions before we load any
     * schematic files */
    scm_eval (pre_backend_list, scm_current_module ());

    i = argv_index;
    while (argv[i] != NULL) {
      GError *err = NULL;

      if (g_path_is_absolute(argv[i])) {
        /* Path is already absolute so no need to do any concat of cwd */
        filename = g_strdup (argv[i]);
      } else {
        filename = g_build_filename (cwd, argv[i], NULL);
      }

      if (!quiet_mode) {
        s_log_message ("Loading schematic [%s]\n", filename);
        printf ("Loading schematic [%s]\n", filename);
      }

      s_page_goto (pr_current, s_page_new (pr_current, filename));

      if (!f_open (pr_current, pr_current->page_current, filename, &err)) {
        g_warning ("%s\n", err->message);
        fprintf (stderr, "%s\n", err->message);
        g_error_free (err);
	exit(2);
      }

      /* collect input filenames for backend use */
      input_files = g_slist_append(input_files, argv[i]);

      i++;
      g_free (filename);
    }

    /* Change back to the directory where we started.  This is done */
    /* since gnetlist is a command line utility and will deposit its output */
    /* in the current directory.  Having the output go to a different */
    /* directory will confuse the user (confused me, at first). */
    if (chdir (cwd)) {
      /* Error occured with chdir */
#warning FIME: What do we do?
    }
    /* free(cwd); - Defered; see below */

    if (argv[argv_index] == NULL) {
        fprintf (stderr, "ERROR: No schematics files specified for processing.\n");
        fprintf (stderr, "\nRun `%s --help' for more information.\n", argv[0]);
        exit (1);
    }

#if DEBUG
    s_page_print_all(pr_current);
#endif

    /* Load basic gnetlist functions */
    scm_primitive_load_path (scm_from_utf8_string ("gnetlist.scm"));

    if (guile_proc) {
      SCM s_backend_path;

      /* Search for backend scm file in load path */
      str = g_strdup_printf("gnet-%s.scm", guile_proc);
      s_backend_path = scm_sys_search_load_path (scm_from_locale_string (str));
      g_free (str);

      /* If it couldn't be found, fail. */
      if (scm_is_false (s_backend_path)) {
        fprintf (stderr, "ERROR: Could not find backend `%s' in load path.\n",
                 guile_proc);
        fprintf (stderr,
                 "\nRun `%s --list-backends' for a full list of available backends.\n",
                 argv[0]);
        exit (1);
      }

      /* Load backend code. */
      scm_primitive_load (s_backend_path);

      /* Evaluate second set of Scheme expressions. */
      scm_eval (post_backend_list, scm_current_module ());
    }

    s_traverse_init();
    s_traverse_start(pr_current);

    /* Change back to the directory where we started AGAIN.  This is done */
    /* because the s_traverse functions can change the Current Working Directory. */
    if (chdir (cwd)) {
      /* Error occured with chdir */
#warning FIXME: What do we do?
    }
    g_free(cwd);

    /* Run post-traverse code. */
    scm_primitive_load_path (scm_from_utf8_string ("gnetlist-post.scm"));

    if (guile_proc) {
        /* check size here hack */
        str = g_strdup_printf ("(%s \"%s\")", guile_proc, output_filename);
        scm_c_eval_string (str);
        g_free (str);
        /* gh_eval_str_with_stack_saving_handler (input_str); */
    } else if (interactive_mode) {
        scm_c_eval_string ("(set-repl-prompt! \"gnetlist> \")");
        scm_shell (0, NULL);
    } else {
        fprintf(stderr,
                "You gave neither backend to execute nor interactive mode!\n");
    }

    gnetlist_quit();

    scm_dynwind_end();
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
