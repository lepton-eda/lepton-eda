/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#include <string.h>

#if defined(HAVE_DIRENT_H) 
#include <dirent.h>
#endif

#ifndef __CYGWIN32__
#include <sys/stat.h>
#endif

#include <ctype.h>
#include <stdlib.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


#include <libgeda/libgeda.h>

#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"

/* for some really odd reason, the cygnus sys/stat.h doesn't want to work */
/* if it's included before the gtk stuff, so here it is */
#ifdef __CYGWIN32__
#include <sys/stat.h>
#endif

/* a random int, used only as a place holder */
int default_dummy;

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

	filename = u_basic_strdup_multiple(GEDARCDIR,
					   "/system-gschemrc",
					   NULL);
	if (filename == NULL) {
		return 0;
	}

	found_rc = g_rc_parse_general(
		filename,
		"Read system-gschemrc file [%s]\n",
		"Did not find system-gschemrc file [%s]\n");

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
					   "/.gEDA/gschemrc",
					   NULL);
	if (filename == NULL) {
		return 0;
	}

	found_rc = g_rc_parse_general(
		filename,
		"Read ~/.gEDA/gschemrc file [%s]\n",
		"Did not find ~/.gEDA/gschemrc file [%s]\n");

	free(filename);

	return found_rc;
}

static int
g_rc_parse_local_rc()
{
	int found_rc;
	char *filename;

	filename = u_basic_strdup("./gschemrc");
	if (filename == NULL) {
		return 0;
	}

	found_rc = g_rc_parse_general(
		filename,
		"Read local gschemrc file [%s]\n",
		"Did not find local gschemrc file [%s]\n");

	free(filename);

	return found_rc;
}

void
g_rc_parse(void)
/* g_rc_parse(TOPLEVEL *w_current) */
{
	int found_rc = 0;

	/* TODO: why is this still here? */
#if 0
	if (w_current == NULL) {
		return;
	}
#endif

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
				"Did not find specified gschemrc file [%s]\n",
				rc_filename);
			s_log_message(
				"Did not find specified gschemrc file [%s]\n",
				rc_filename);
		}
	}

	/* Oh well, I couldn't find any rcfile, exit! */
	if (!found_rc) {
		/* TODO: these two are basically the
		 * same. Inefficient! */
		s_log_message("Could not find any gschemrc file!\n");
		fprintf(stderr, "Could not find a gschemrc file\n");
		exit(-1);
	}
}

SCM
g_rc_gschem_version(SCM version)
{
	char *string = gh_scm2newstr(version, NULL);

	if (string == NULL) {
		fprintf(stderr,
			"%s requires a string as a parameter\n",
			"gschem-version"
			);
		return SCM_BOOL_F;
	}

	if (strcmp(string, VERSION) != 0) {
		fprintf(stderr,
			"Found a version [%s] gschemrc file:\n[%s]\n",
			string, rc_filename);
		fprintf(stderr,
			"While gschem is in ALPHA, "
			"please be sure that you have the latest rc file.\n");
			free(string);
		return SCM_BOOL_F;
	}

	free(string);
	return SCM_BOOL_T;
}

/* general color-setting function */
static SCM
g_rc_color_general(SCM index, SCM color, SCM outline_color, 
		   SCM ps_color, SCM ir, SCM ig, SCM ib,
		   const char *rc_name, int *color_var)
{
	int status;
	int color_index = gh_scm2int(index);
	char *color_name = gh_scm2newstr(color, NULL);
	char *outline_color_name = gh_scm2newstr(outline_color, NULL);
	char *ps_color_string = gh_scm2newstr(ps_color, NULL);
	int image_red = gh_scm2int(ir);
	int image_green = gh_scm2int(ig);
	int image_blue = gh_scm2int(ib);

	status = s_color_request(color_index, color_name, outline_color_name,
			         ps_color_string, 
			         image_red, image_green, image_blue);


#if DEBUG
	printf("%d %s %s %s %d %d %d\n", color_index, color_name, 
				      outline_color_name, ps_color_string,
				      image_red, image_green, image_blue);
#endif

	/* invalid color? */
	if (status == -1) {
		fprintf(stderr,
			"Invalid color [%s] passed to %s\n",
			color_name,
			rc_name);
		if (color_name) {
			free(color_name);
		}
		if (outline_color_name) {
			free(outline_color_name);
		}
		if (ps_color_string) {
			free(ps_color_string);
		}
		return SCM_BOOL_F;
	}

	*color_var = color_index;

	if (color_name) {
		free(color_name);
	}
	if (outline_color_name) {
		free(outline_color_name);
	}
	if (ps_color_string) {
		free(ps_color_string);
	}
	return SCM_BOOL_T;
}

#define DEFINE_G_RC_COLOR(func, rc, var)			             \
SCM								             \
func(SCM index, SCM color, SCM outline_color, SCM ps_color,                  \
     SCM ir, SCM ig, SCM ib)						     \
{								             \
	return g_rc_color_general(index, color, outline_color,               \
                                  ps_color, ir, ig, ib, (rc), &(var));	     \
}

DEFINE_G_RC_COLOR(g_rc_override_net_color,
		  "override-net-color",
		  default_override_net_color)

DEFINE_G_RC_COLOR(g_rc_override_bus_color,
		  "override-bus-color",
		  default_override_bus_color)

DEFINE_G_RC_COLOR(g_rc_override_pin_color,
		  "override-pin-color",
		  default_override_pin_color)

DEFINE_G_RC_COLOR(g_rc_attribute_color,
		  "attribute-color",
		  default_attribute_color);

DEFINE_G_RC_COLOR(g_rc_detachedattr_color,
		  "detached-attribute-color",
		  default_detachattr_color);

DEFINE_G_RC_COLOR(g_rc_text_color,
		  "text-color",
		  default_text_color);

DEFINE_G_RC_COLOR(g_rc_net_color,
		  "net-color",
		  default_net_color);

DEFINE_G_RC_COLOR(g_rc_bus_color,
		  "bus-color",
		  default_bus_color);

DEFINE_G_RC_COLOR(g_rc_pin_color,
		  "pin-color",
		  default_pin_color);

DEFINE_G_RC_COLOR(g_rc_graphic_color,
		  "graphic-color",
		  default_graphic_color);

DEFINE_G_RC_COLOR(g_rc_grid_color,
		  "grid-color",
		  default_grid_color);

DEFINE_G_RC_COLOR(g_rc_background_color,
		  "background-color",
		  default_background_color);

DEFINE_G_RC_COLOR(g_rc_select_color,
		  "select-color",
		  default_select_color);

DEFINE_G_RC_COLOR(g_rc_boundingbox_color,
		  "boundingbox-color",
		  default_bb_color);

DEFINE_G_RC_COLOR(g_rc_zoom_box_color,
		  "zoom-box-color",
		  default_zoom_box_color);

DEFINE_G_RC_COLOR(g_rc_net_endpoint_color,
		  "net-endpoint-color",
		  default_net_endpoint_color);

DEFINE_G_RC_COLOR(g_rc_logic_bubble_color,
		  "logic-bubble-color",
		  default_logic_bubble_color);

DEFINE_G_RC_COLOR(g_rc_lock_color,
		  "lock-color",
		  default_lock_color);

DEFINE_G_RC_COLOR(g_rc_output_color_background,
		  "output-color-background",
		  default_print_color_background);

DEFINE_G_RC_COLOR(g_rc_stroke_color,
		  "stroke-color",
		  default_stroke_color);

DEFINE_G_RC_COLOR(g_rc_freestyle_color,
		  "freestyle-color",
		  default_dummy);

typedef struct {
	int   m_val;
	char *m_str;
} vstbl_entry;

/* currently unused */
#if 0
static int
vstbl_lookup_val(const vstbl_entry *table, int size, int val)
{
	int i;

	for(i = 0; i < size; i++) {
		if(table[i].m_val == val) {
			break;
		}
	}
	return i;
}
#endif

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

/* currently unused */
#if 0
static const char *
vstbl_get_str(const vstbl_entry *table, int index)
{
	return table[index].m_str;
}
#endif

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

SCM
g_rc_net_endpoint_mode(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{FILLEDBOX, "filledbox"},
		{EMPTYBOX , "emptybox" },
		{X        , "x"        },
		{NONE     , "none"     }
	};

	RETURN_G_RC_MODE("net-endpoint-mode", default_net_endpoint_mode, 4);
}

SCM
g_rc_net_midpoint_mode(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{FILLED, "filled"},
		{EMPTY , "empty" },
		{NONE  , "none"  }
	};

	RETURN_G_RC_MODE("net-midpoint-mode", default_net_midpoint_mode, 3);
}

SCM
g_rc_net_style(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{THIN , "thin" },
		{THICK, "thick"}
	};

	RETURN_G_RC_MODE("net-style", default_net_style, 2);
}

SCM
g_rc_bus_style(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{THIN , "thin" },
		{THICK, "thick"}
	};

	RETURN_G_RC_MODE("bus-style", default_bus_style, 2);
}

SCM
g_rc_pin_style(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{THIN , "thin" },
		{THICK, "thick"}
	};

	RETURN_G_RC_MODE("pin-style", default_pin_style, 2);
}

SCM
g_rc_action_feedback_mode(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{OUTLINE    , "outline"   },
		{BOUNDINGBOX, "boundingbox"}
	};

	RETURN_G_RC_MODE("action-feedback-mode",default_actionfeedback_mode,2);
}

SCM
g_rc_zoom_with_pan(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE,  "enabled" },
		{FALSE, "disabled"}
	};

	RETURN_G_RC_MODE("zoom-with-pan", default_zoom_with_pan, 2);
}

SCM
g_rc_text_feedback(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{ALWAYS            , "always"            },
		{ONLY_WHEN_READABLE, "only-when-readable"}
	};

	RETURN_G_RC_MODE("text-feedback", default_text_feedback, 2);
}

SCM
g_rc_text_display_zoomfactor(SCM zoomfactor)
{
	int val;

	val = gh_scm2int(zoomfactor);

	if (val == 0) {
		fprintf(stderr,
			"Invalid zoomfactor [%d] passed to %s\n",
			val,
			"text-display-zoom-factor");
		val = 10; /* absolute default */
	}

	default_text_display_zoomfactor = val;

	return SCM_BOOL_T;
}

SCM
g_rc_scrollbar_update(SCM mode)
{
	char *string = NULL;

#if 0
	string = gh_scm2newstr(mode, NULL);

	if (strcmp(string, "continuous") == 0) {
		gtk_range_set_update_policy (GTK_RANGE (
			window_current->v_scrollbar),
					     GTK_UPDATE_CONTINUOUS);
		gtk_range_set_update_policy (GTK_RANGE (
			window_current->h_scrollbar),
					     GTK_UPDATE_CONTINUOUS);
	} else if (strcmp(string, "delayed") == 0) {
		gtk_range_set_update_policy (GTK_RANGE (
			window_current->v_scrollbar),
					     GTK_UPDATE_DELAYED);
		gtk_range_set_update_policy (GTK_RANGE (
			window_current->h_scrollbar),
					     GTK_UPDATE_DELAYED);
	} else {
		fprintf(stderr,
			"Invalid mode [%s] passed to scrollbar-update\n",
			string);
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}
#endif
	if (string) {
		free(string);
	}

	return SCM_BOOL_T;
}

SCM
g_rc_object_clipping(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"}
	};

	RETURN_G_RC_MODE("object-clipping", default_object_clipping, 2);
}

SCM
g_rc_logging(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"}
	};

	RETURN_G_RC_MODE("logging", default_do_logging, 2);
}

SCM
g_rc_embed_components(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"}
	};

	RETURN_G_RC_MODE("embed-components", default_embed_complex, 2);
}

SCM
g_rc_text_size(SCM size)
{
	int val;

	val = gh_scm2int(size);

	if (val == 0) {
		fprintf(stderr,
			"Invalid size [%d] passed to text-size\n",
			val);
		val = 10; /* absolute default */
	}

	default_text_size = val;

	return SCM_BOOL_T;
}

/* TODO: inconsistant naming with keyword name and variable to hold
 * variable */
SCM
g_rc_text_caps_style(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{LOWER, "lower" },
		{UPPER, "upper" },
		{BOTH , "both"  }
	};

	RETURN_G_RC_MODE("text-caps-style", default_text_caps, 3);
}

SCM
g_rc_snap_size(SCM size)
{
	int val;

	val = gh_scm2int(size);

	if (val == 0) {
		fprintf(stderr, "Invalid size [%d] passed to snap-size\n",
			val);
		val = 100; /* absolute default */
	}

	default_snap_size = val;

	return SCM_BOOL_T;
}

SCM
g_rc_logging_destination(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{LOG_WINDOW         , "log_window" },
		{STDOUT_TTY         , "tty"        },
		{BOTH_LOGWIN_STDOUT , "both"       }
	};

	RETURN_G_RC_MODE("logging-destination", logging_dest, 3);
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

SCM
g_rc_untitled_name(SCM name)
{
	char *string = gh_scm2newstr(name, NULL);

	if (string == NULL) {
		fprintf(stderr,
			"%s requires a string as a parameter\n",
			"untitled-name"
			);
		return SCM_BOOL_F;
	}

	if (default_untitled_name) {
		free(default_untitled_name);
	}

	default_untitled_name = u_basic_strdup(string);

	free(string);
	return SCM_BOOL_T;
}

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


	if (string[0] == '/') {
		s_clib_add_entry(string);
	} else {
		cwd = getcwd(NULL, 1024);
		temp = u_basic_strdup_multiple(cwd, "/", string, NULL);
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
		sprintf(fullpath, "%s/%s", string, dptr->d_name);
                stat(fullpath, &buf);
                if (S_ISDIR(buf.st_mode)) { 
			if (s_clib_uniq(fullpath)) {
				if (fullpath[0] == '/') {
					s_clib_add_entry(fullpath);
#if DEBUG
			printf("absolute: %s\n", fullpath);
#endif
				} else {
					cwd = getcwd(NULL, 1024);
					temp = u_basic_strdup_multiple(cwd, 
							"/", fullpath, NULL);
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

	if (string[0] == '/') {
		s_slib_add_entry(string);
	} else {
		cwd = getcwd(NULL, 1024);
		temp = u_basic_strdup_multiple(cwd, "/", string, NULL);
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
		sprintf(fullpath, "%s/%s", string, dptr->d_name);
                stat(fullpath, &buf);
                if (S_ISDIR(buf.st_mode)) { 
			if (s_slib_uniq(fullpath)) {
				if (fullpath[0] == '/') {
					s_slib_add_entry(fullpath);
#if DEBUG
			printf("absolute: %s\n", fullpath);
#endif
				} else {
					cwd = getcwd(NULL, 1024);
					temp = u_basic_strdup_multiple(cwd, 
							"/", fullpath, NULL);
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
g_rc_attribute_name(SCM path)
{
	char *string = gh_scm2newstr(path, NULL);

	if (string == NULL) {
		fprintf(stderr,
			"%s requires a string as a parameter\n",
			"attribute-name");
		return SCM_BOOL_F;
	}

	/* not unique? */
	if (!s_attrib_uniq(string)) {
		free(string);
		return SCM_BOOL_F;
	}

	s_attrib_add_entry(string);

	free(string);
	return SCM_BOOL_T;
}

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

#if 0 /* old obsolete way of handling strokes */
SCM
g_rc_stroke(SCM scm_stroke, SCM scm_guile_func)
{
#if HAS_LIBSTROKE
	char *stroke = gh_scm2newstr(scm_stroke, NULL);

	if (!s_stroke_uniq(stroke)) {
		if (stroke_info_mode) {
			s_log_message("Duplicate stroke definition "
				      "passed to stroke! [%s]\n",
				      stroke);
			printf("Duplicate stroke definition "
			       "passed to stroke! [%s]\n",
			       stroke);
		}
		if (stroke) {
			free(stroke);
		}
		return SCM_BOOL_F;
	}

	s_stroke_add_entry(stroke, scm_guile_func);

	if (stroke) {
		free(stroke);
	}
#else
	if (stroke_info_mode) {
		printf("A stroke keyword has been found in an rc file, but gschem\n"
		       "is not compiled to support strokes, please recompile gschem\n"
		       "with LibStroke\n");
	}
#endif

	return SCM_BOOL_T;
}
#endif

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

#if 0
  	/* out for now */
	x_window_setup_world(window_current);
	set_window(window_current,
		   window_current->init_left,
		   window_current->init_right,
		   window_current->init_top,
		   window_current->init_bottom);
#endif

#if 0
	/* reset the scrollbars too */
	x_vscrollbar_set_ranges(window_current);
	x_hscrollbar_set_ranges(window_current);
#endif

	return SCM_BOOL_T;
}

SCM
g_rc_scrollbars(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("scrollbars", default_scrollbars_flag, 2);
}

SCM
g_rc_paper_size(SCM width, SCM height)
{
	int i_width, i_height;

	/* yes this is legit, we are casing the resulting double to an int */

	i_width  = (int) (gh_scm2double(width ) * MILS_PER_INCH);
 	i_height = (int) (gh_scm2double(height) * MILS_PER_INCH);

	default_paper_width  = i_width;
	default_paper_height = i_height;

	return SCM_BOOL_T;
}

SCM
g_rc_paper_sizes(SCM papername, SCM scm_width, SCM scm_height)
{
	int width;
	int height;
	char *string = gh_scm2newstr(papername, NULL);
	width  = (int) (gh_scm2double(scm_width)  * MILS_PER_INCH);
	height = (int) (gh_scm2double(scm_height) * MILS_PER_INCH);

	if (string == NULL) {
		fprintf(stderr, "Invalid parameters to paper-sizes\n");
		return SCM_BOOL_F;
	}

	if (!s_papersizes_uniq(string)) {
		if (string) {
			free(string);
		}
		return SCM_BOOL_F;
	}

	s_papersizes_add_entry(string, width, height);

	if (string) {
		free(string);
	}
	return SCM_BOOL_T;
}

SCM
g_rc_output_text(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{VECTOR_FONTS , "vector" },
		{PS_FONTS     , "ps"     },
	};

	RETURN_G_RC_MODE("output-text", default_text_output, 2);
}

/* this keyword needs a better name ... */
SCM
g_rc_output_type(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{WINDOW, "current window" },
		{LIMITS, "limits" },
	};

	RETURN_G_RC_MODE("output-type", default_print_output_type, 2);
}

SCM
g_rc_output_orientation(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{PORTRAIT , "portrait" },
		{LANDSCAPE, "landscape"},
	};

	RETURN_G_RC_MODE("output-orientation", default_print_orientation, 2);
}

SCM
g_rc_image_color(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("image-color", default_image_color, 2);
}

SCM
g_rc_output_color(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	/* this variable is inconsistantly named with the rest */
	RETURN_G_RC_MODE("output-color", default_print_color, 2);
}

SCM
g_rc_output_capstyle(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{BUTT_CAP , "butt" },
		{ROUND_CAP , "round" },
		{SQUARE_CAP, "square"},
	};

	RETURN_G_RC_MODE("output-capstyle", default_print_output_capstyle, 3);
}

SCM
g_rc_log_window(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{MAP_ON_STARTUP, "startup" },
		{MAP_LATER     , "later"   },
	};

	RETURN_G_RC_MODE("log-window", default_log_window, 2);
}

SCM
g_rc_log_window_type(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRANSIENT, "transient" },
		{DECORATED, "decorated" },
	};

	RETURN_G_RC_MODE("log-window-type", default_log_window_type, 2);
}

SCM
g_rc_third_button(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{POPUP_ENABLED   , "popup"   },
		{MOUSEPAN_ENABLED, "mousepan"},
	};

	RETURN_G_RC_MODE("third-button", default_third_button, 2);
}

SCM
g_rc_middle_button(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{STROKE, "stroke"},
		{REPEAT, "repeat"},
		{ACTION, "action"},
	};

	RETURN_G_RC_MODE("middle-button", default_middle_button, 3);
}

SCM
g_rc_net_consolidate(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("net-consolidate", default_net_consolidate, 2);
}

SCM
g_rc_file_preview(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	/* this variable is inconsistantly named with the rest */
	RETURN_G_RC_MODE("file-preview", default_file_preview, 2);
}

SCM
g_rc_enforce_hierarchy(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("enforce-hierarchy", default_enforce_hierarchy, 2);
}

SCM
g_rc_text_origin_marker(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("text-origin-marker", default_text_origin_marker, 2);
}

SCM
g_rc_fast_mousepan(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("fast-mousepan", default_fast_mousepan, 2);
}

SCM
g_rc_raise_dialog_boxes_on_expose(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("raise-dialog-boxes-on-expose", default_raise_dialog_boxes, 2);
}

SCM
g_rc_attribute_promotion(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("attribute-promotion", default_attribute_promotion, 2);
}

SCM
g_rc_promote_invisible(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("promote-invisible", default_promote_invisible, 2);
}

SCM
g_rc_keep_invisible(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("keep-invisible", default_keep_invisible, 2);
}

SCM
g_rc_continue_component_place(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("continue-component-place", default_continue_component_place, 2);
}

SCM
g_rc_undo_levels(SCM levels)
{
	int val;

	val = gh_scm2int(levels);

	if (val == 0) {
		fprintf(stderr, "Invalid num levels [%d] passed to undo-levels\n",
			val);
		val = 10; /* absolute default */
	}

	default_undo_levels = val;

	return SCM_BOOL_T;
}

SCM
g_rc_undo_control(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("undo-control", default_undo_control, 2);
}

SCM
g_rc_undo_type(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{UNDO_DISK  , "disk"   },
		{UNDO_MEMORY, "memory" },
	};

	RETURN_G_RC_MODE("undo-type", default_undo_type, 2);
}

SCM
g_rc_draw_grips(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("draw-grips", default_draw_grips, 2);
}

SCM
g_rc_netconn_rubberband(SCM mode)
{
	static const vstbl_entry mode_table[] = {
		{TRUE , "enabled" },
		{FALSE, "disabled"},
	};

	RETURN_G_RC_MODE("netconn-rubberband", default_netconn_rubberband, 2);
}
