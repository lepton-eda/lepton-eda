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
#include <ctype.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/i_vars.h"
#include "../include/prototype.h"

typedef struct {
    int m_val;
    char *m_str;
} vstbl_entry;

static int
vstbl_lookup_str(const vstbl_entry * table, int size, const char *str)
{
    int i;

    for (i = 0; i < size; i++) {
	if (strcmp(table[i].m_str, str) == 0) {
	    break;
	}
    }
    return i;
}

static int vstbl_get_val(const vstbl_entry * table, int index)
{
    return table[index].m_val;
}

/* returned path should not be freed */
char *
g_rc_parse_path()
{
  char *rc_path = NULL;
  
  if (strcmp(GEDARCDIR, "none") == 0) {
    /* rc dir not specified at configure time, so search for config in */
    /* the normal GEDADATA directory */
    rc_path = getenv("GEDADATA");
  } else {
    /* rc path specified at configure time, always return specified path */
    rc_path = GEDARCDIR;
  }

  return(rc_path);
}


static int
g_rc_parse_general(const char *fname, const char *ok_msg, const char *err_msg)
{
	int found_rc = 0;
	char* tmp;

	if (access(fname, R_OK) == 0) {
		/* TODO: fix g_read_file to accept "const char *" */
		tmp = u_basic_strdup(fname);
		g_read_file(tmp);
		free(tmp);
		found_rc = 1;
		s_log_message(ok_msg, fname);
	} else {
		s_log_message(err_msg, fname);
	}
	return found_rc;
}

static int
g_rc_parse_system_rc()
{
	int found_rc;
	char *filename;
        char *geda_data = getenv("GEDADATA");
        char *path_to_rc;

        if (geda_data == NULL) {
          fprintf(stderr, "You must set the GEDADATA environment variable!\n");
          exit(-1);
        }
        
        path_to_rc = g_rc_parse_path(); /* do not free path_to_rc */

	filename = u_basic_strdup_multiple(path_to_rc,
					   PATH_SEPARATER_STRING, 
					   "system-gnetlistrc",
					   NULL);
	if (filename == NULL) {
		return 0;
	}

	found_rc = g_rc_parse_general(
		filename,
		"Read system-gnetlistrc file [%s]\n",
		"Did not find system-gnetlistrc file [%s]\n");

	free(filename);

	return found_rc;
}

static int
g_rc_parse_home_rc()
{
	int found_rc;
	char *filename;
	char *HOME;

	HOME = (char *) getenv("HOME");
	if (HOME == NULL) {
		return 0;
	}

	filename = u_basic_strdup_multiple(HOME,
					   PATH_SEPARATER_STRING,
					   ".gEDA", PATH_SEPARATER_STRING,
					   "gnetlistrc",
					   NULL);
	if (filename == NULL) {
		return 0;
	}

	found_rc = g_rc_parse_general(
		filename,
		"Read ~/.gEDA/gnetlistrc file [%s]\n",
		"Did not find ~/.gEDA/gnetlistrc file [%s]\n");

	free(filename);

	return found_rc;
}

static int
g_rc_parse_local_rc()
{
	int found_rc;
	char *filename;

	filename = u_basic_strdup_multiple(".", PATH_SEPARATER_STRING, 
				           "gnetlistrc", NULL);
	if (filename == NULL) {
		return 0;
	}

	found_rc = g_rc_parse_general(
		filename,
		"Read local gnetlistrc file [%s]\n",
		"Did not find local gnetlistrc file [%s]\n");

	free(filename);

	return found_rc;
}

void
g_rc_parse(void)
{
	int found_rc = 0;
        char *rc_path;
        char *geda_rcdata;

        rc_path = g_rc_parse_path(); /* do not free rc_path */
        geda_rcdata = u_basic_strdup_multiple("GEDADATARC=", rc_path, NULL);
        putenv(geda_rcdata);
        
	/* visit rc files in order */
	found_rc |= g_rc_parse_system_rc();
	found_rc |= g_rc_parse_home_rc();
	found_rc |= g_rc_parse_local_rc();

	if (rc_filename != NULL) {
		if (access(rc_filename, R_OK) == 0) {
			g_read_file(rc_filename);
			found_rc = 1;
			s_log_message(
				"Read specified rc file [%s]\n",
				rc_filename);
		} else {
			fprintf(stderr,
				"Did not find specified gnetlistrc file [%s]\n",
				rc_filename);
			s_log_message(
				"Did not find specified gnetlistrc file [%s]\n",
				rc_filename);
		}
	}

	/* Oh well, I couldn't find any rcfile, exit! */
	if (!found_rc) {
		/* TODO: these two are basically the
		 * same. Inefficient! */
		s_log_message("Could not find any gnetlistrc file!\n");
		fprintf(stderr, "Could not find a gnetlistrc file\n");
		exit(-1);
	}
}

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
	if(index == table_size) {
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

#define RETURN_G_RC_MODE(rc, var, size)			\
	return g_rc_mode_general(mode,			\
				 (rc),			\
				 &(var),		\
				 mode_table,		\
				 size)

SCM g_rc_gnetlist_version(SCM version)
{
    char *string;

    string = gh_scm2newstr(version, NULL);

    if (strcmp(string, VERSION) != 0) {
	fprintf(stderr, "Found a version [%s] gnetlist file:\n[%s]\n",
		string, rc_filename);
	fprintf(stderr,
		"While gnetlist is in ALPHA, please be sure that you have the latest rc file.\n");
    }

    if (string) {
	free(string);
    }

    return (gh_int2scm(0));
}


SCM
g_rc_default_series_name(SCM name)
{
	char *string = gh_scm2newstr(name, NULL);

	if (string == NULL) {
		fprintf(stderr,
			"%s requires a string as a parameter\n",
			"series-name"
			);
		return SCM_BOOL_F;
	}

	if (default_series_name) {
		free(default_series_name);
	}

	default_series_name = u_basic_strdup(string);

	free(string);
	return SCM_BOOL_T;
}

#if 1
SCM
g_rc_component_library(SCM path)
{
	int ret;
	struct stat buf;
	char *cwd;
	char *string = gh_scm2newstr(path, NULL);
	char *temp;

	/* TODO: don't I have to check string if it's NULL here? */

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);

	/* invalid path? */
	if (ret < 0) {
		fprintf(stderr,
			"Invalid path [%s] passed to component-library\n",
			string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not a directory? */
	if (!S_ISDIR(buf.st_mode)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not an unique path? */
	if (!s_clib_uniq(string)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}


#ifdef __MINGW32__
	if (string[1] == ':' && string[2] == PATH_SEPARATER_CHAR) {
#else
	if (string[0] == PATH_SEPARATER_CHAR) {
#endif
		s_clib_add_entry(string);
	} else {
		cwd = getcwd(NULL, 1024);
		temp = u_basic_strdup_multiple(cwd, PATH_SEPARATER_STRING, 
					       string, NULL);
		s_clib_add_entry(temp);
		free(temp);
		free(cwd);
	}

	if (string) {
		free(string);
	}

	return SCM_BOOL_T;
}

SCM
g_rc_component_library_search(SCM path)
{
	int ret;
	struct stat buf;
	DIR *top_ptr;
        struct dirent *dptr;
	char *string = gh_scm2newstr(path, NULL);
	char *fullpath;
	char *cwd, *temp;

	/* TODO: don't I have to check string if it's NULL here? */

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);

	/* invalid path? */
	if (ret < 0) {
		fprintf(stderr,
		     "Invalid path [%s] passed to component-library-search\n",
		     string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not a directory? */
	if (!S_ISDIR(buf.st_mode)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	top_ptr = opendir(string);

	if (top_ptr == NULL) {
		fprintf(stderr,
		     "Invalid path [%s] passed to component-library-search\n",
		     string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
        }

        while((dptr = readdir(top_ptr))) {

	  /* don't do . and .. and special case font */
	  if ((strcmp(dptr->d_name, ".") != 0) && 
	      (strcmp(dptr->d_name, "..") != 0) &&
              (strcmp(dptr->d_name, "font") != 0)) {

		fullpath=(char *)malloc(sizeof(char)*(strlen(string)+
						      strlen(dptr->d_name)+2));
		sprintf(fullpath, "%s%c%s", string, PATH_SEPARATER_CHAR,
					    dptr->d_name);
                stat(fullpath, &buf);
                if (S_ISDIR(buf.st_mode)) { 
			if (s_clib_uniq(fullpath)) {
#ifdef __MINGW32__
				if (fullpath[1] == ':' && 
				    fullpath[2] == PATH_SEPARATER_CHAR) {
#else
				if (fullpath[0] == PATH_SEPARATER_CHAR) {
#endif
					s_clib_add_entry(fullpath);
#if DEBUG
			printf("absolute: %s\n", fullpath);
#endif
				} else {
					cwd = getcwd(NULL, 1024);
					temp = u_basic_strdup_multiple(cwd, 
							PATH_SEPARATER_STRING, 
							fullpath, NULL);
#if DEBUG
			printf("relative: %s\n", temp);
#endif
					s_clib_add_entry(temp);
					free(temp);
					free(cwd);
				}
			} 
                }
		free(fullpath);
            }
        }       

	if (string) {
		free(string);
	}

	return SCM_BOOL_T;
}

SCM
g_rc_source_library(SCM path)
{
	int ret;
	struct stat buf;
	char *string = gh_scm2newstr(path, NULL);
	char *temp, *cwd;

	if (string == NULL) {
		fprintf(stderr,
			"%s requires a string as a parameter\n",
			"source-library"
			);
		return SCM_BOOL_F;
	}

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);

	/* invalid path? */
	if (ret < 0) {
		fprintf(stderr,
			"Invalid path [%s] passed to %s\n",
			string,
			"source-library");
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not a directory? */
	if (!S_ISDIR(buf.st_mode)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not a unique path? */
	if (!s_slib_uniq(string)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

#ifdef __MINGW32__
	if (string[1] == ':' && string[2] == PATH_SEPARATER_CHAR) {
#else
	if (string[0] == PATH_SEPARATER_CHAR) {
#endif
		s_slib_add_entry(string);
	} else {
		cwd = getcwd(NULL, 1024);
		temp = u_basic_strdup_multiple(cwd, PATH_SEPARATER_STRING, 
					       string, NULL);
		s_slib_add_entry(temp);
		free(temp);
		free(cwd);
	}

	if (string) {
		free(string);
	}
	return SCM_BOOL_T;
}

SCM
g_rc_source_library_search(SCM path)
{
	int ret;
	struct stat buf;
	DIR *top_ptr;
        struct dirent *dptr;
	char *string = gh_scm2newstr(path, NULL);
	char *fullpath;
	char *cwd, *temp;

	/* TODO: don't I have to check string if it's NULL here? */

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);

	/* invalid path? */
	if (ret < 0) {
		fprintf(stderr,
		     "Invalid path [%s] passed to source-library-search\n",
		     string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not a directory? */
	if (!S_ISDIR(buf.st_mode)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	top_ptr = opendir(string);

	if (top_ptr == NULL) {
		fprintf(stderr,
		     "Invalid path [%s] passed to source-library-search\n",
		     string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
        }

        while((dptr = readdir(top_ptr))) {

	  /* don't do . and .. and special case font */
	  if ((strcmp(dptr->d_name, ".") != 0) && 
	      (strcmp(dptr->d_name, "..") != 0) &&
              (strcmp(dptr->d_name, "font") != 0)) {

		fullpath=(char *)malloc(sizeof(char)*(strlen(string)+
						      strlen(dptr->d_name)+2));
		sprintf(fullpath, "%s%c%s", string, PATH_SEPARATER_CHAR,
					    dptr->d_name);
                stat(fullpath, &buf);
                if (S_ISDIR(buf.st_mode)) { 
			if (s_slib_uniq(fullpath)) {
#ifdef __MINGW32__
				if (fullpath[1] == ':' && 
				    fullpath[2] == PATH_SEPARATER_CHAR) {
#else
				if (fullpath[0] == PATH_SEPARATER_CHAR) {
#endif
					s_slib_add_entry(fullpath);
#if DEBUG
			printf("absolute: %s\n", fullpath);
#endif
				} else {
					cwd = getcwd(NULL, 1024);
					temp = u_basic_strdup_multiple(cwd, 
							PATH_SEPARATER_STRING, 
							fullpath, NULL);
#if DEBUG
			printf("relative: %s\n", temp);
#endif
					s_slib_add_entry(temp);
					free(temp);
					free(cwd);
				}
			} 
                }
		free(fullpath);
            }
        }       

	if (string) {
		free(string);
	}

	return SCM_BOOL_T;
}

#endif

SCM
g_rc_scheme_directory(SCM path)
{
	int ret;
	struct stat buf;
	char *string = gh_scm2newstr(path, NULL);

	if (string == NULL) {
		fprintf(stderr,
			"%s requires a string as a parameter\n",
			"scheme-directory"
			);
		return SCM_BOOL_F;
	}

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);

	/* invalid path? */
	if (ret < 0) {
		fprintf(stderr,
			"Invalid path [%s] passed to scheme-directory\n",
			string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not a directory? */
	if (!S_ISDIR(buf.st_mode)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	if (default_scheme_directory) {
		free(default_scheme_directory);
	}
	default_scheme_directory = u_basic_strdup(string);

	if (string) {
		free(string);
	}
	return SCM_BOOL_T;
}

SCM
g_rc_font_directory(SCM path)
{
	int ret;
	struct stat buf;
	char *string = gh_scm2newstr(path, NULL);

	if (string == NULL) {
		fprintf(stderr,
			"%s requires a string as a parameter\n",
			"font-direcoty"
			);
		return SCM_BOOL_F;
	}

	/* take care of any shell variables */
	string = expand_env_variables(string);

	ret = stat(string, &buf);

	/* invalid path? */
	if (ret < 0) {
		fprintf(stderr,
			"Invalid path [%s] passed to font-directory\n",
			string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	/* not a directory? */
	if (!S_ISDIR(buf.st_mode)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	if (default_font_directory) {
		free(default_font_directory);
	}
	default_font_directory = u_basic_strdup(string);

	if (string) {
		free(string);
	}
	return SCM_BOOL_T;
}

SCM
g_rc_world_size(SCM width, SCM height, SCM border)
{
	int i_width, i_height, i_border;
	int init_right, init_bottom;

	/* yes this is legit, we are casing the resulting double to an int */
	i_width  = (int) (gh_scm2double(width ) * MILS_PER_INCH);
 	i_height = (int) (gh_scm2double(height) * MILS_PER_INCH);
	i_border = (int) (gh_scm2double(border) * MILS_PER_INCH);

	PAPERSIZEtoWORLD(i_width, i_height, i_border,
			 &init_right, &init_bottom);

#if DEBUG
	printf("%d %d\n", i_width, i_height);
	printf("%d %d\n", init_right, init_bottom);
#endif

	default_init_right  = init_right;
	default_init_bottom = init_bottom;

	return SCM_BOOL_T;
}

SCM g_rc_net_naming_priority(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{NET_ATTRIBUTE, "net"},
	{LABEL_ATTRIBUTE, "label"}
    };

    RETURN_G_RC_MODE("net-naming-priority", default_net_naming_priority,
		     2);
}

SCM g_rc_hierarchy_traversal(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{TRUE, "enabled"},
	{FALSE, "disabled"}
    };

    RETURN_G_RC_MODE("hierarchy-traversal", default_hierarchy_traversal,
		     2);
}

SCM g_rc_hierarchy_uref_mangle(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{TRUE, "enabled"},
	{FALSE, "disabled"}
    };

    RETURN_G_RC_MODE("hierarchy-uref-mangle",
		     default_hierarchy_uref_mangle, 2);
}

SCM g_rc_hierarchy_netname_mangle(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{TRUE, "enabled"},
	{FALSE, "disabled"}
    };

    RETURN_G_RC_MODE("hierarchy-netname-mangle",
		     default_hierarchy_netname_mangle, 2);
}

SCM g_rc_hierarchy_netattrib_mangle(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{TRUE, "enabled"},
	{FALSE, "disabled"}
    };

    RETURN_G_RC_MODE("hierarchy-netattrib-mangle",
		     default_hierarchy_netattrib_mangle, 2);
}

SCM g_rc_hierarchy_netname_separator(SCM name)
{
  char *string = gh_scm2newstr(name, NULL);

  if (string == NULL) {
    fprintf(stderr,
            "%s requires a string as a parameter\n",
            "hierarchy-netname-separator"
            );
    return SCM_BOOL_F;
  }

  if (default_hierarchy_netname_separator) {
    free(default_hierarchy_netname_separator);
  }

  default_hierarchy_netname_separator = u_basic_strdup(string);

  free(string);
  return SCM_BOOL_T;
    
}

SCM g_rc_hierarchy_netattrib_separator(SCM name)
{
  char *string = gh_scm2newstr(name, NULL);

  if (string == NULL) {
    fprintf(stderr,
            "%s requires a string as a parameter\n",
            "hierarchy-netattrib-separator"
            );
    return SCM_BOOL_F;
  }

  if (default_hierarchy_netattrib_separator) {
    free(default_hierarchy_netattrib_separator);
  }

  default_hierarchy_netattrib_separator = u_basic_strdup(string);

  free(string);
  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_uref_separator(SCM name)
{
  char *string = gh_scm2newstr(name, NULL);

  if (string == NULL) {
    fprintf(stderr,
            "%s requires a string as a parameter\n",
            "hierarchy-uref-separator"
            );
    return SCM_BOOL_F;
  }

  if (default_hierarchy_uref_separator) {
    free(default_hierarchy_uref_separator);
  }

  default_hierarchy_uref_separator = u_basic_strdup(string);

  free(string);
  return SCM_BOOL_T;
}

SCM g_rc_hierarchy_netattrib_order(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{PREPEND, "prepend"},
	{APPEND, "append"}
    };

    RETURN_G_RC_MODE("hierarchy-netattrib-order",
		     default_hierarchy_netattrib_order, 2);
}

SCM g_rc_hierarchy_netname_order(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{PREPEND, "prepend"},
	{APPEND, "append"}
    };

    RETURN_G_RC_MODE("hierarchy-netname-order",
		     default_hierarchy_netname_order, 2);
}

SCM g_rc_hierarchy_uref_order(SCM mode)
{
    static const vstbl_entry mode_table[] = {
	{PREPEND, "prepend"},
	{APPEND, "append"}
    };

    RETURN_G_RC_MODE("hierarchy-uref-order",
		     default_hierarchy_uref_order, 2);
}

/*************************** GUILE end done *********************************/





