/* gEDA - GPL Electronic Design Automation
 * gsymcheck - gEDA Symbol Check 
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>
#include <stdio.h> 

#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <ctype.h>
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

#include "../include/struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"


/* this is needed so that these routines know which window they are changing */
static TOPLEVEL *project_current;

void
set_static_project_current(TOPLEVEL *pr_current)
{
  project_current = pr_current;
}

/* You should not free the returned character string. */
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

void
g_rc_parse(TOPLEVEL *pr_current)
{
  char *HOME=NULL;
  char filename[256]; /* hack */
  int found_rc=0;
  char *filename2=NULL;
  char *geda_data = getenv("GEDADATA");
  char *geda_rcdata;
  char *rc_path;

  if (pr_current == NULL)
    return;

  if (geda_data == NULL) {
    fprintf(stderr, "You must set the GEDADATA environment variable!\n");
    exit(-1);
  }
        
  set_static_project_current(pr_current);

  rc_path = g_rc_parse_path();
  geda_rcdata = u_basic_strdup_multiple("GEDADATARC=", rc_path, NULL);
  putenv(geda_rcdata);

  /* Let's try a the system one - GEDADATA/system-gsymcheck */
  filename2 = u_basic_strdup_multiple(rc_path, PATH_SEPARATER_STRING,
                                      "system-gsymcheckrc", NULL);

  if ( access(filename2, R_OK) == 0 ) {
    strcpy(rc_filename, filename2); /* size verify hack */
    g_read_file(filename2);
    found_rc = 1;
    s_log_message("Read system-gsymcheckrc file [%s]\n", filename2);
  } else {
    s_log_message("Did not find local gsymcheckrc file [%s]\n", filename2);
  }
  free(filename2);

  /* now search the proper rc location (in ~/.gEDA) */
  HOME = (char *)  getenv("HOME");
  if (HOME) {
    sprintf(filename, "%s%c.gEDA%cgsymcheckrc", HOME, 
            PATH_SEPARATER_CHAR, PATH_SEPARATER_CHAR);
    if ( access(filename, R_OK) == 0) {
      strcpy(rc_filename, filename); /* size verify hack */
      g_read_file(filename);
      found_rc = 1;
      s_log_message("Read ~/.gEDA/gsymcheckrc file [%s]\n", filename);
    } else {
      s_log_message("Did not find ~/.gEDA/gsymcheckc file [%s]\n", filename);
    }
  }

  /* try the local directory for a gsymcheck */ 
  sprintf(filename, ".%cgsymcheckrc", PATH_SEPARATER_CHAR);
  if ( access(filename, R_OK) == 0 ) {
    strcpy(rc_filename, filename); /* size verify hack */
    g_read_file(filename);
    found_rc = 1;
    s_log_message("Read local gsymcheckrc file [%s]\n", filename);
  } else {
    s_log_message("Did not find local gsymcheckc file [%s]\n", filename);
  }



  /* Oh well I couldn't find any rcfile, exit! */

  if (!found_rc) {
    s_log_message("Could not find any gsymcheckrc file!\n");
    fprintf(stderr, "Could not find a gsymcheckrc file\n");
    exit(-1);
  }
}

SCM
g_rc_gsymcheck_version(SCM version)
{
  char *string;

  string = gh_scm2newstr(version, NULL);

  if ( strcmp(string, VERSION) != 0 ) {
    fprintf(stderr, "Found a version [%s] gsymcheck file:\n[%s]\n", 
            string, rc_filename); 
    fprintf(stderr, "While gsymcheck is in ALPHA, please be sure that you have the latest rc file.\n");
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
				    (fullpath[2] == PATH_SEPARATER_CHAR ||
				     fullpath[2] == OTHER_PATH_SEPARATER_CHAR)) {
#else
				if (fullpath[0] == PATH_SEPARATER_CHAR) {
#endif

					s_clib_add_entry(fullpath);
#if DEBUG
			printf("absolute: %s\n", fullpath);
#endif
				} else {
					cwd = getcwd(NULL, 1024);
#ifdef __MINGW32__
					u_basic_strip_trailing(cwd, 
							PATH_SEPARATER_CHAR);
#endif

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
				    (fullpath[2] == PATH_SEPARATER_CHAR ||
				     fullpath[2] == OTHER_PATH_SEPARATER_CHAR)) {
#else
				if (fullpath[0] == PATH_SEPARATER_CHAR) {
#endif
					s_slib_add_entry(fullpath);
#if DEBUG
			printf("absolute: %s\n", fullpath);
#endif
				} else {
					cwd = getcwd(NULL, 1024);
#ifdef __MINGW32__
					u_basic_strip_trailing(cwd, 
							PATH_SEPARATER_CHAR);
#endif
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
g_rc_bitmap_directory(SCM path)
  {
    int ret;
    struct stat buf;
    char *string;

    string = gh_scm2newstr(path, NULL);

    /* take care of any shell variables */
    string = expand_env_variables(string);

    ret = stat(string, &buf);
	
    if (ret < 0) {
      fprintf(stderr, "Invalid path [%s] passed to bitmap-directory\n", string);
    } else {

      if (S_ISDIR(buf.st_mode)) {
        if (project_current->bitmap_directory)
          free(project_current->bitmap_directory);

        project_current->bitmap_directory = malloc(sizeof(char)*(
                                                                 strlen(string)+1));
        strcpy(project_current->bitmap_directory, string);

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

/*************************** GUILE end done *********************************/

