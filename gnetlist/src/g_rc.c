/* gEDA - GNU Electronic Design Automation
 * gnetlist - GNU Netlist 
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */

#include <config.h>
#include <stdio.h> 
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#ifndef __CYGWIN32__
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#include <libgeda/defines.h>
#include <libgeda/struct.h>
#include <libgeda/globals.h>  
#include <libgeda/prototype.h>

#include "../include/globals.h"
#include "../include/i_vars.h"
#include "../include/prototype.h"

/* for some really odd reason, the cygnus sys/stat.h doesn't want to work */
/* if it's included before the gtk stuff, so here it is */
#ifdef __CYGWIN32__
#include <sys/stat.h>
#endif

/* this is needed so that these routines know which window they are changing */
static TOPLEVEL *project_current;


typedef struct {
        int   m_val;
        char *m_str;
} vstbl_entry;

static int
vstbl_lookup_str(const vstbl_entry *table, int size, const char *str)
{
	int i;

	for(i = 0; i < size; i++) {
		if(strcmp(table[i].m_str, str) == 0) {
			break;
		}
	}
	return i;
}

static int
vstbl_get_val(const vstbl_entry *table, int index)
{
	return table[index].m_val;
}

/* written by Kazu, much much better rc handling routines */
static SCM
g_rc_mode_general(SCM mode,
		  const char *rc_name,
		  int *mode_var,
		  const vstbl_entry *table,
		  int table_size)
{
	int index;
	char *string;

	string = gh_scm2newstr(mode, NULL);
	index = vstbl_lookup_str(table, table_size, string);

	/* no match? */
	if(index == sizeof(table)) {
		fprintf(stderr,
			"Invalid mode [%s] passed to %s\n",
			string,
			rc_name);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	*mode_var = vstbl_get_val(table, index);

	if (string) {
		free(string);
	}
	return SCM_BOOL_T;
}

#define RETURN_G_RC_MODE(rc, var)                       \
        return g_rc_mode_general(mode,                  \
                                 (rc),                  \
                                 &(var),                \
                                 mode_table,            \
                                 sizeof(mode_table))


void
set_static_project_current(TOPLEVEL *pr_current)
{
	project_current = pr_current;
}

void
g_rc_parse(TOPLEVEL *pr_current)
{
	char *HOME;
	char filename[256]; /* hack */
	int found_rc=0;

	if (pr_current == NULL)
		return;

	set_static_project_current(pr_current);

	/* Let's try a the system one - GEDARCDIR/system-gnetlist */
	sprintf(filename, "%s/system-gnetlistrc", GEDARCDIR);
	if ( access(filename, R_OK) == 0 ) {
		strcpy(rc_filename, filename); /* size verify hack */
		g_read_file(filename);
		found_rc = 1;
		s_log_message("Read system-gnetlistrc file [%s]\n", filename);
	} else {
		s_log_message("Did not find system-gnetlistrc file [%s]\n", filename);
	}

	/* now search the proper rc location (in ~/.gEDA) */
	HOME = (char *)  getenv("HOME");
	if (HOME) {
		sprintf(filename, "%s/.gEDA/gnetlistrc", HOME);
		if ( access(filename, R_OK) == 0) {
			strcpy(rc_filename, filename); /* size verify hack */
			g_read_file(filename);
			found_rc = 1;
			s_log_message("Read ~/.gEDA/gnetlistrc file [%s]\n", filename);
		} else {
			s_log_message("Did not find ~/.gEDA/gnetlistrc file [%s]\n", filename);
		}
	}

	/* try the local directory for a gnetlist */ 
	sprintf(filename, "./gnetlistrc");
	if ( access(filename, R_OK) == 0 ) {
		strcpy(rc_filename, filename); /* size verify hack */
		g_read_file(filename);
		found_rc = 1;
		s_log_message("Read local gnetlist file [%s]\n", filename);
	} else {
		s_log_message("Did not find local gnetlistrc file [%s]\n", filename);
        }




	/* Oh well I couldn't find any rcfile, exit! */

	if (!found_rc) {
		s_log_message("Could not find any gnetlistrc file!\n");
		fprintf(stderr, "Could not find a gnetlistrc file\n");
		exit(-1);
	}
}

SCM
g_rc_gnetlist_version(SCM version)
{
	char *string;

	string = gh_scm2newstr(version, NULL);

	if ( strcmp(string, VERSION) != 0 ) {
		fprintf(stderr, "Found a version [%s] gnetlist file:\n[%s]\n", 
					string, rc_filename); 
		fprintf(stderr, "While gnetlist is in ALPHA, please be sure that you have the latest rc file.\n");
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_default_series_name(SCM name)
{
	char *string;

	string = gh_scm2newstr(name, NULL);

	if (project_current->series_name) {
		free(project_current->series_name);
	}

	project_current->series_name = malloc(sizeof(char)*(strlen(string)+1));
	strcpy(project_current->series_name, string);

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}


SCM
g_rc_untitled_name(SCM name)
{
	char *string;

	string = gh_scm2newstr(name, NULL);

	if (project_current->untitled_name) {
		free(project_current->untitled_name);
	}

	project_current->untitled_name = malloc(sizeof(char)*(
					strlen(string)+1));
	strcpy(project_current->untitled_name, string);

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}


SCM
g_rc_component_library(SCM path)
{
	int ret;
	struct stat buf;
	char *string;

	string = gh_scm2newstr(path, NULL);

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);

	if (ret < 0) {
		fprintf(stderr, "Invalid path [%s] passed to component-library\n", string);
	} else {

		if (S_ISDIR(buf.st_mode)) {
			/* only add path if it is uniq */	
			if (s_clib_uniq(string)) {
				s_clib_add_entry(string);
			} else {
				if (string) free(string);
				return(gh_int2scm(-1)); 
			}
		} else {
			if (string) free(string);
			return(gh_int2scm(-1)); 
		}
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}


SCM
g_rc_source_library(SCM path)
{
	int ret;
	struct stat buf;
	char *string;

	string = gh_scm2newstr(path, NULL);

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);
	
	if (ret < 0) {
		fprintf(stderr, "Invalid path [%s] passed to source-library\n", string);
	} else {
		if (S_ISDIR(buf.st_mode)) {
			if (s_slib_uniq(string)) {
				s_slib_add_entry(string);
			} else {
				if (string) free(string);
				return(gh_int2scm(-1)); 
			}
		} else {
			if (string) free(string);
			return(gh_int2scm(-1)); 
			
		}
	}
	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_scheme_directory(SCM path)
{
	int ret;
	struct stat buf;
	char *string;

	string = gh_scm2newstr(path, NULL);

	/* take care of any shell variables */
	string = expand_env_variables(string);
	
	ret = stat(string, &buf);

	if (ret < 0) {
		fprintf(stderr, "Invalid path [%s] passed to scheme-directory\n", string);
	} else {

		if (S_ISDIR(buf.st_mode)) {
			/* only add path if it is uniq */	

			if (project_current->scheme_directory)
				free(project_current->scheme_directory);

			project_current->scheme_directory = 
				 malloc(sizeof(char)*(strlen(string)+1));
			strcpy(project_current->scheme_directory, string);

		} else {
			if (string) free(string);
			return(gh_int2scm(-1)); 
		}
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}


SCM
g_rc_font_directory(SCM path)
{
	int ret;
	struct stat buf;
	char *string;

	string = gh_scm2newstr(path, NULL);

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);
	
	if (ret < 0) {
		fprintf(stderr, "Invalid path [%s] passed to font-directory\n", string);
	} else {

		if (S_ISDIR(buf.st_mode)) {
			if (project_current->font_directory)
				free(project_current->font_directory);

			project_current->font_directory = malloc(sizeof(char)*(
					strlen(string)+1));
			strcpy(project_current->font_directory, string);

		} else {
			if (string) free(string);
			return(gh_int2scm(-1)); 
		}
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_paper_size(SCM width, SCM height, SCM border) 
{
	int i_width, i_height, i_border;
	int init_right, init_bottom;

	/* yes this is legit, we are casing the resulting double to an int */	
	i_width = (int) gh_scm2double(width)*MILS_PER_INCH;
 	i_height = (int)gh_scm2double(height)*MILS_PER_INCH;
	i_border = (int) gh_scm2double(border)*MILS_PER_INCH;

	/* project_current->text_size = val; */

	PAPERSIZEtoWORLD(i_width, i_height, i_border, 
			 &init_right, &init_bottom);

#if DEBUG
	printf("%d %d\n", i_width, i_height);
	printf("%d %d\n", init_right, init_bottom); 
#endif
	
	project_current->init_right = init_right;
	project_current->init_bottom = init_bottom;
	
	s_project_setup_world(project_current);	
	return(gh_int2scm(0)); 
}

SCM
g_rc_net_naming_priority(SCM mode)
{
        static const vstbl_entry mode_table[] = {
                {NET_ATTRIBUTE, "net"},
                {LABEL_ATTRIBUTE , "label" }
        };

        RETURN_G_RC_MODE("net-naming-priority", default_net_naming_priority);
}


/*************************** GUILE end done *********************************/

