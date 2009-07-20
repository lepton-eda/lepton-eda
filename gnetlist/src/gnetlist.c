/* gEDA - GPL Electronic Design Automation
 * gnetlist - gEDA Netlist
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
#include <sys/types.h>
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
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

#define BACKEND_LIST_COLS      3

void gnetlist_quit(void)
{
    s_clib_free();
    s_slib_free();
    s_rename_destroy_all();
    /* o_text_freeallfonts(); */

    /* Free GSList *backend_params */
    g_slist_free (backend_params);
}

/** @brief Prints a list of all installed gnetlist backends to standard output.
 *
 * @param current Pointer to the toplevel struct.
 */
void gnetlist_backends(TOPLEVEL *current)
{
    DIR *schemedir;
    struct dirent *entry;
    char *filename;
    int n;

    schemedir=opendir(current->scheme_directory);
    if(schemedir==NULL) {
        fprintf(stderr, "\nERROR! Can't open directory %s: %s\n",
                        current->scheme_directory,
                        strerror(errno));
        return;
    }

    printf("List of available backends:\n\n");

    n=1;
    while(1) {
        entry=readdir(schemedir);
        if(entry==NULL) break;

        filename=strdup(entry->d_name);

        if(g_str_has_prefix(filename, "gnet-")&&
           g_str_has_suffix(filename, ".scm")) {

            /* strip the suffix */
            filename[strlen(filename)-4]='\0';
            /* and skip the prefix */
            printf("%-25s", &filename[5]);
            if(n>=BACKEND_LIST_COLS) {
                printf("\n");
                n=0;
            }
            n++;
        }

        free(filename);
    }
    printf("\n");
}

void main_prog(void *closure, int argc, char *argv[])
{
    int i;
    int argv_index;
    char *cwd;
    GSList *list_pnt;
    gchar *str;
    gchar *filename;

    TOPLEVEL *pr_current;

    /* set default output filename */
    output_filename = g_strdup("output.net");

    argv_index = parse_commandline(argc, argv);
    cwd = g_get_current_dir();

    /* this is a kludge to make sure that spice mode gets set */
    /*  Hacked by SDB to allow spice netlisters of arbitrary name
     *        as long as they begin with "spice".  For example, this spice
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
    s_log_init ("gnetlist");

    s_log_message("gEDA/gnetlist version %s%s.%s\n", PREPEND_VERSION_STRING,
                  DOTTED_VERSION, DATE_VERSION);
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

    pr_current = s_toplevel_new ();
    g_rc_parse(pr_current, "gnetlistrc", rc_filename);
    /* immediately setup user params */
    i_vars_set (pr_current);

    s_rename_init();

    if(guile_proc!=NULL) {
        if(!strcmp(guile_proc, "help")) {
                gnetlist_backends(pr_current);
                exit(0);
            }
    }

    /* Load the first set of scm files before we load any schematic files */
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

      if (!f_open (pr_current, filename, &err)) {
        g_warning ("%s\n", err->message);
        fprintf (stderr, "%s\n", err->message);
        g_error_free (err);
      }

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
        fprintf(stderr,
                "\nERROR! You must specify at least one filename\n\n");
        usage(argv[0]);
    }

    g_set_project_current(pr_current);
#if DEBUG
    s_page_print_all(pr_current);
#endif

    filename = g_build_filename (pr_current->scheme_directory, "gnetlist.scm", NULL);
    if (g_read_file (filename) != -1) {
      s_log_message ("Read init scm file [%s]\n", filename);
    } else {
      s_log_message ("Failed to read init scm file [%s]\n", filename);
      fprintf (stderr, "Failed to read init scm file [%s]\n", filename);
    }
    g_free (filename);

    if (guile_proc) {
        /* load the appropriate scm file */
        str = g_strdup_printf("gnet-%s.scm", guile_proc);
        filename = g_build_filename (pr_current->scheme_directory, str, NULL);
        g_free (str);
        if (g_read_file (filename) != -1) {
          s_log_message ("Read %s scm file [%s]\n", guile_proc, filename);
        } else {
          s_log_message ("Failed to read %s scm file [%s]\n", guile_proc, filename);
          fprintf (stderr, "Failed to read %s scm file [%s]\n", guile_proc, filename);
        }
        g_free (filename);

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

    filename = g_build_filename (pr_current->scheme_directory, "gnetlist-post.scm", NULL);
    if (g_read_file (filename) != -1) {
      s_log_message ("Read post traversal scm file [%s]\n", filename);
    } else {
      s_log_message ("Failed to read post traversal scm file [%s]\n", filename);
      fprintf (stderr, "Failed to read post traversal scm file [%s]\n", filename);
    }
    g_free (filename);

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
