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
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#include <libgeda/struct.h>
#include <libgeda/defines.h>
#include <libgeda/globals.h>  
#include <libgeda/prototype.h>

#include "../include/i_vars.h"
#include "../include/globals.h"
#include "../include/prototype.h"


void
g_rc_parse(void)
/*g_rc_parse(TOPLEVEL *w_current)*/
{
	char *HOME;
	char *filename; 
	int found_rc=0;
	int len;

#if 0
	if (w_current == NULL)
		return;
#endif


	/* Let's try a the system one - GEDARCDIR/system-gschemrc */
	
	len = strlen("/system-gschemrc") + strlen(GEDARCDIR) + 1; 	
	filename = (char *) malloc(sizeof(char)*len);
	sprintf(filename, "%s/system-gschemrc", GEDARCDIR);
	if ( access(filename, R_OK) == 0 ) {
		g_read_file(filename);
		found_rc = 1;
		s_log_message("Read system-gschemrc file [%s]\n", filename);
	} else {
		s_log_message("Did not find system-gschemrc file [%s]\n", filename);
	}
	free(filename);

	/* now search the proper rc location (in ~/.gEDA) */
	HOME = (char *)  getenv("HOME");
	if (HOME) {
		len = strlen("/.gEDA/gschemrc") + strlen(HOME) + 1; 	
		filename = (char *) malloc(sizeof(char)*len);
		sprintf(filename, "%s/.gEDA/gschemrc", HOME);
		if ( access(filename, R_OK) == 0) {
			g_read_file(filename);
			found_rc = 1;
			s_log_message("Read ~/.gEDA/gschemrc file [%s]\n", filename);
		} else {
			s_log_message("Did not find ~/.gEDA/gschemrc file [%s]\n", filename);
		}
		free(filename);
	}

	/* try the local directory for a gschemrc */ 
	len = strlen("./gschemrc") + 1; 	
	filename = (char *) malloc(sizeof(char)*len);
	strcpy(filename, "./gschemrc");
	if ( access(filename, R_OK) == 0 ) {
		g_read_file(filename);
		found_rc = 1;
		s_log_message("Read local gschemrc file [%s]\n", filename);
	} else {
		s_log_message("Did not find local gschemrc file [%s]\n", filename);
	}
	free(filename);

	if (rc_filename) {
		len = strlen(rc_filename) + 1; 	
		if ( access(rc_filename, R_OK) == 0 ) {
			g_read_file(rc_filename);
			found_rc = 1;
			s_log_message("Read specified rc file [%s]\n", rc_filename);
		} else {
			fprintf(stderr, "Did not find specified gschemrc file [%s]\n", rc_filename);
			s_log_message("Did not find specified gschemrc file [%s]\n", rc_filename);
		}
	}

	/* Oh well I couldn't find any rcfile, exit! */

	if (!found_rc) {
		s_log_message("Could not find any gschemrc file!\n");
		fprintf(stderr, "Could not find a gschemrc file\n");
		exit(-1);
	}
}

SCM
g_rc_gschem_version(SCM version)
{
	char *string;

	string = gh_scm2newstr(version, NULL);

	if ( strcmp(string, VERSION) != 0 ) {
		fprintf(stderr, "Found a version [%s] gschemrc file:\n[%s]\n", 
					string, rc_filename); 
		fprintf(stderr, "While gschem is in ALPHA, please be sure that you have the latest rc file.\n");
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_override_net_color(SCM color)
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);

	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to override-net-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_override_net_color = newcolor;
	}


	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_override_pin_color(SCM color)
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to override-pin-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_override_pin_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_attribute_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to attribute-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_attribute_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 

}

SCM
g_rc_detachedattr_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to detached-attribute-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_detachattr_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_text_color(SCM color) 
{

	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to text-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_text_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_net_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to net-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_net_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_pin_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to pin-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_pin_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_graphic_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to graphic-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_graphic_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_grid_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to grid-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_grid_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_background_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to background-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_background_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_select_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to select-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_select_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_boundingbox_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to boundingbox-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_bb_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_net_endpoint_color(SCM color)
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to net-endpoint-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_net_endpoint_color = newcolor;
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}



SCM 
g_rc_net_endpoint_mode(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "filledbox") == 0 ) {
		default_net_endpoint_mode = FILLEDBOX;		
	} else if ( strcmp(string, "emptybox") == 0 ) {
		default_net_endpoint_mode = EMPTYBOX;		
	} else if ( strcmp(string, "x") == 0 ) {
		default_net_endpoint_mode = X;		
	} else if ( strcmp(string, "none") == 0 ) {
		default_net_endpoint_mode = NONE;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to net-endpoint-mode\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_net_midpoint_mode(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "filled") == 0 ) {
		default_net_midpoint_mode = FILLED;		
	} else if ( strcmp(string, "empty") == 0 ) {
		default_net_midpoint_mode = EMPTY;		
	} else if ( strcmp(string, "none") == 0 ) {
		default_net_midpoint_mode = NONE;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to net-midpoint-mode\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_net_style(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "thin") == 0 ) {
		default_net_style = THIN;		
	} else if ( strcmp(string, "thick") == 0 ) {
		default_net_style = THICK;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to net-style\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}


SCM 
g_rc_pin_style(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "thin") == 0 ) {
		default_pin_style = THIN;		
	} else if ( strcmp(string, "thick") == 0 ) {
		default_pin_style = THICK;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to pin-style\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_action_feedback_mode(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "outline") == 0 ) {
		default_actionfeedback_mode = OUTLINE;		
	} else if ( strcmp(string, "boundingbox") == 0 ) {
		default_actionfeedback_mode = BOUNDINGBOX;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to action-feedback-mode\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_scrollbar_update(SCM mode)
{
	char *string=NULL;

#if 0
	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "continuous") == 0 ) {
		gtk_range_set_update_policy (GTK_RANGE (
				     window_current->v_scrollbar),
                                     GTK_UPDATE_CONTINUOUS);
		gtk_range_set_update_policy (GTK_RANGE (
				     window_current->h_scrollbar),
                                     GTK_UPDATE_CONTINUOUS);
	} else if ( strcmp(string, "delayed") == 0 ) {
		gtk_range_set_update_policy (GTK_RANGE (
				     window_current->v_scrollbar),
                                     GTK_UPDATE_DELAYED);
		gtk_range_set_update_policy (GTK_RANGE (
				     window_current->h_scrollbar),
                                     GTK_UPDATE_DELAYED);
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to scrollbar-update\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}
#endif
	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}          

SCM
g_rc_object_clipping(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "enabled") == 0 ) {
		default_object_clipping = TRUE;
	} else if ( strcmp(string, "disabled") == 0 ) {
		default_object_clipping = FALSE;
	} else {
		fprintf(stderr, "Invalid value [%s] passed to object-clipping\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}          


SCM
g_rc_logging(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "enabled") == 0 ) {
		default_do_logging = TRUE;
	} else if ( strcmp(string, "disabled") == 0 ) {
		default_do_logging = FALSE;
	} else {
		fprintf(stderr, "Invalid value [%s] passed to logging\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}          

SCM
g_rc_embed_components(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "enabled") == 0 ) {
		default_embed_complex = TRUE;
	} else if ( strcmp(string, "disabled") == 0 ) {
		default_embed_complex = FALSE;
	} else {
		fprintf(stderr, "Invalid value [%s] passed to embed-components\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}          

SCM 
g_rc_text_size(SCM size) 
{
	int val;
	
	val = gh_scm2int(size);

	if (val == 0) {
		fprintf(stderr, "Invalid size [%d] passed to text-size\n", val);
		val = 10; /* absolute default */
	}

	default_text_size = val;   	

	return(gh_int2scm(0)); 
}

/* inconsistant naming with keyword name and variable to hold variable hack */
SCM
g_rc_text_caps_style(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "lower") == 0 ) {
		default_text_caps = LOWER;
	} else if ( strcmp(string, "upper") == 0 ) {
		default_text_caps = UPPER;
	} else if ( strcmp(string, "both") == 0 ) {
		default_text_caps = BOTH;
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to text-caps-style\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}          


SCM 
g_rc_snap_size(SCM size) 
{
	int val;

	val = gh_scm2int(size);

	if (val == 0) {
		fprintf(stderr, "Invalid size [%d] passed to snap-size\n", val);
		val = 100; /* absolute default */
	}

	default_snap_size = val;        

	return(gh_int2scm(0)); 
}


SCM
g_rc_logging_destination(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "log_window") == 0 ) {
		logging_dest = LOG_WINDOW;
	} else if ( strcmp(string, "tty") == 0 ) {
		logging_dest = STDOUT_TTY;
	} else if ( strcmp(string, "both") == 0 ) {
		logging_dest = BOTH_LOGWIN_STDOUT;
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to logging-destination\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
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

	if (default_series_name) {
		free(default_series_name);
	}

	default_series_name = malloc(sizeof(char)*(strlen(string)+1));
	strcpy(default_series_name, string);

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

	if (default_untitled_name) {
		free(default_untitled_name);
	}

	default_untitled_name = malloc(sizeof(char)*(
					strlen(string)+1));
	strcpy(default_untitled_name, string);

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
g_rc_attribute_name(SCM path)
{
	char *string;

	string = gh_scm2newstr(path, NULL);

	if (string == NULL) {
		fprintf(stderr, "attribute-name requires a string as a parameter\n");
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		
		if (s_attrib_uniq(string)) {
			s_attrib_add_entry(string);
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

			if (default_scheme_directory)
				free(default_scheme_directory);

			default_scheme_directory = malloc(sizeof(char)*(
					strlen(string)+1));
			strcpy(default_scheme_directory, string);

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
g_rc_stroke(SCM scm_stroke, SCM scm_guile_func)
{
	char *stroke;

#if HAS_LIBSTROKE			
	stroke = gh_scm2newstr(scm_stroke, NULL);


	if (s_stroke_uniq(stroke)) {
		s_stroke_add_entry(stroke, scm_guile_func);
	} else {
if (stroke_info_mode) {
		s_log_message("Duplicate stroke definition passed to stroke! [%s]\n", stroke);
		printf("Duplicate stroke definition passed to stroke! [%s]\n", stroke);
}
		if (stroke) free(stroke);
		return(gh_int2scm(-1)); 
	}

	if (stroke) {
		free(stroke);
	}
#else 
if (stroke_info_mode) {
	printf("A stroke keyword has been found in an rc file, but gschem\n");
	printf("is not compiled to support strokes, please recompile gschem\n");
	printf("with LibStroke\n");
}
#endif

	return(gh_int2scm(0)); 
}

SCM
g_rc_stroke_color(SCM color) 
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);
	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to stroke-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_stroke_color = newcolor;
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
			if (default_font_directory)
				free(default_font_directory);

			default_font_directory = malloc(sizeof(char)*(
					strlen(string)+1));
			strcpy(default_font_directory, string);

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
g_rc_world_size(SCM width, SCM height, SCM border) 
{
	int i_width, i_height, i_border;
	int init_right, init_bottom;

	/* yes this is legit, we are casing the resulting double to an int */	
	i_width = (int) (gh_scm2double(width)*MILS_PER_INCH);
 	i_height = (int) (gh_scm2double(height)*MILS_PER_INCH);
	i_border = (int) (gh_scm2double(border)*MILS_PER_INCH);

	PAPERSIZEtoWORLD(i_width, i_height, i_border, 
			 &init_right, &init_bottom);

#if 0
	printf("%d %d\n", i_width, i_height);
	printf("%d %d\n", init_right, init_bottom); 
#endif
	
	default_init_right = init_right;
	default_init_bottom = init_bottom;

/*  	out for now
	x_window_setup_world(window_current);	
	set_window(window_current, window_current->init_left, 
		   window_current->init_right, window_current->init_top, 
		   window_current->init_bottom);
*/

#if 0
	/* reset the scrollbars too */
	x_vscrollbar_set_ranges(window_current);
	x_hscrollbar_set_ranges(window_current);
#endif

	return(gh_int2scm(0)); 
}

SCM
g_rc_scrollbars(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "enabled") == 0 ) {
		default_scrollbars_flag = TRUE;
	} else if ( strcmp(string, "disabled") == 0 ) {
		default_scrollbars_flag = FALSE;
	} else {
		fprintf(stderr, "Invalid value [%s] passed to scrollbars\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}          


SCM 
g_rc_paper_size(SCM width, SCM height) 
{
	int i_width, i_height;

	/* yes this is legit, we are casing the resulting double to an int */	

	i_width = (int) (gh_scm2double(width)*MILS_PER_INCH);
 	i_height = (int) (gh_scm2double(height)*MILS_PER_INCH);

	default_paper_width = i_width;
	default_paper_height = i_height;

	return(gh_int2scm(0)); 
}


SCM
g_rc_paper_sizes(SCM papername, SCM scm_width, SCM scm_height)
{
	char *string;
	int width;
	int height;

	string = gh_scm2newstr(papername, NULL);
	width = (int) (gh_scm2double(scm_width)*MILS_PER_INCH);
	height = (int) (gh_scm2double(scm_height)*MILS_PER_INCH);

	if (string == NULL) {
		fprintf(stderr, "Invalid parameters to paper-sizes\n");
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		
		if (s_papersizes_uniq(string)) {
			s_papersizes_add_entry(string, width, height);
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
g_rc_output_text(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "vector") == 0 ) {
		default_text_output = VECTOR_FONTS;		
	} else if ( strcmp(string, "ps") == 0 ) {
		default_text_output = PS_FONTS;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to output-text\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

/* this keyword needs a better name ... */
SCM 
g_rc_output_type(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "current window") == 0 ) {
		default_print_output_type = WINDOW;		
	} else if ( strcmp(string, "limits") == 0 ) {
		default_print_output_type = LIMITS;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to output-type\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_output_orientation(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "portrait") == 0 ) {
		default_print_orientation = PORTRAIT;		
	} else if ( strcmp(string, "landscape") == 0 ) {
		default_print_orientation = LANDSCAPE;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to output-orientation\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_image_orientation(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "portrait") == 0 ) {
		default_image_orientation = PORTRAIT;		
	} else if ( strcmp(string, "landscape") == 0 ) {
		default_image_orientation = LANDSCAPE;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to image-orientation\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_output_color(SCM mode)
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "enabled") == 0 ) {
		default_print_color = TRUE;
	} else if ( strcmp(string, "disabled") == 0 ) {
		default_print_color = FALSE;
	} else {
		fprintf(stderr, "Invalid value [%s] passed to output-color\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM
g_rc_output_color_background(SCM color)
{
	char *string;
	int newcolor;

	string = gh_scm2newstr(color, NULL);

	newcolor = colornametovalue(string);

	if (newcolor == -1) {
		fprintf(stderr, "Invalid color [%s] passed to output-color-background\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	} else {
		default_print_color_background = newcolor;
	}


	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_log_window(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "startup") == 0 ) {
		default_log_window = MAP_ON_STARTUP;		
	} else if ( strcmp(string, "later") == 0 ) {
		default_log_window = MAP_LATER;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to log-window\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

SCM 
g_rc_log_window_type(SCM mode) 
{
	char *string;

	string = gh_scm2newstr(mode, NULL);

	if ( strcmp(string, "transient") == 0 ) {
		default_log_window_type = TRANSIENT;		
	} else if ( strcmp(string, "decorated") == 0 ) {
		default_log_window_type = DECORATED;		
	} else {
		fprintf(stderr, "Invalid mode [%s] passed to log-window-type\n", string);
		if (string) free(string);
		return(gh_int2scm(-1)); 
	}

	if (string) {
		free(string);
	}

	return(gh_int2scm(0)); 
}

/*************************** GUILE end done *********************************/

