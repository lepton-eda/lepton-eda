/* gEDA - GNU Electronic Design Automation
 * libgeda - gEDA's library
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
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h> 
#include <strings.h>
#include <sys/types.h>
#include <ctype.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#ifdef HAS_LIBGD
#include <gd/gd.h>
#endif

#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif

#include "struct.h"
#include "defines.h"
#include "globals.h"

#include "../include/prototype.h"



/* slib stands for source (project/schematic/hdl/model source) library */

/* need to test everything at boundary conditions (exceed cache size etc...) */

/* memory is allocated here in these functions and sticks around */
/* ie if somebody returns a char * to the outside world then that memory */
/* is already allocated */
/* so all memory must be free in the calling routines */
 

struct st_slib {
	char *dir_name;
};

static int slib_index=0;

#define MAX_SLIBS	128

/* and eventually make this unlimited */
/* hack hack */
static struct st_slib slib[MAX_SLIBS];


int 
s_slib_add_entry(char *new_path) 
{
	if (new_path == NULL) {
		return(-1); 
	}

	if (slib_index >= MAX_SLIBS) {
		return(-1); 
	}

	slib[slib_index].dir_name = (char *) malloc(sizeof(char)*strlen(new_path)+1);

	strcpy(slib[slib_index].dir_name, new_path);

	slib_index++;
	return(slib_index);
}

/* return true if it finds it else zero */
int
s_slib_search_for_dirname(char *dir_name)
{
	int i;

	for (i = 0; i < slib_index; i++) {
		if (strcmp(slib[i].dir_name, dir_name) == 0) {
			return(1);	
		}	
	}

	return(0);
}

/* don't forget to free slib_path */ 
char *
s_slib_search_dirs(char *basename)
{
	int i;
	int len;
	DIR *ptr=NULL;
        struct dirent *dptr;
	char *slib_path=NULL;

	/* search slib paths backwards */
	for (i = slib_index-1 ; i >= 0; i--) {
	/* for (i = 0 ; i < slib_index; i++) {*/

#if DEBUG
		printf("searching: %d %s\n", i, slib[i].dir_name);
#endif

        	ptr = opendir(slib[i].dir_name);

		if (ptr == NULL) {
			fprintf(stderr, "Oops got a null dir_name!\n");
			exit(-1);
		}

		dptr = readdir(ptr);

		while(dptr != NULL) {

			/* Do a substring comp for a match */
			if (strstr(dptr->d_name, basename) != NULL)  {
				len = strlen(slib[i].dir_name);				
				slib_path = (char *) malloc(sizeof(char)*len+1);
				strcpy(slib_path, slib[i].dir_name);
	
				if (ptr) {
					closedir(ptr);
					ptr = NULL;
				}

				return(slib_path);
			}
			dptr = readdir(ptr);
		}

		if (ptr) {
			closedir(ptr);
			ptr = NULL;
		}

	}

	if (ptr) {
		closedir(ptr);
		ptr = NULL;
	}

	return(NULL);
}

char *
s_slib_search_lowlevel(char *basename) 
{
	char *slib_path=NULL;
	char *full_path=NULL;
	int len;

	slib_path = s_slib_search_dirs(basename);

	if (slib_path) {
		/* return type */

                s_log_message("Found [%s]\n", basename);
                /* s_log_message("Found [%s] in [%s]\n", basename, slib_path);*/

		len = strlen(basename)+strlen(slib_path);
	
		/* The 2 is for NULL char and the slash inbetween the two */
		/* strings */	
		full_path = (char *) malloc(sizeof(char)*(len+2));

		sprintf(full_path, "%s/%s", slib_path, basename);
		
		free(slib_path);

		return(full_path);
	} else {

		s_log_message("Could not find [%s] in any SourceLibrary\n", basename);

#if DEBUG 
		fprintf(stderr, "Could not find [%s] in any SourceLibrary\n", basename);
#endif
		return(NULL);
	}
}

/* This function takes a raw filename in and returns a processed filename */
/* which has the following things done to it: */
/* 	- copied everything up to the first . */
/* 	- removed any _# (where # is any number of digits */
/* returned_filename has to be freed somewhere */
char *
s_slib_getbasename(char *rawname)
{
	char *return_filename;
	char *copy;
	int i;
	int done=0;
	int lastchar;
	int valid=0;
	int len;
	int seen_underscore=0;
	
	if (!rawname) 
		return(NULL);

	len = strlen(rawname)+1;

	return_filename = (char *) malloc(sizeof(char)*len);

	i = 0;
	/* first get everything up to the leading dot */
	while(rawname[i] != '\0' && rawname[i] != '.') {
		return_filename[i] = rawname[i];
		i++;
	}


	return_filename[i] = '\0';


	/* keep filename for safe keeping */
	copy = return_filename;

	/* skip null terminator */
	i--;

	lastchar=i;

	/* this is a quick and dirty state machine to */
	/* go back and strip off any _#'s */
	/* if there is a better way let me know */
	while (i >= 0 && !done) {

  	    /* first we need to check to see if we have seen the first '_' */
	    /* if we have then we already removing chars, continue with that */
		if ( seen_underscore ) {
			if (return_filename[i] == '_') {
				done = 1;	
			}
			
			return_filename[i] = '\0';
		} else {
		/* we are still searching for the first underscore */

			/* first make sure char is a number */
			if (isdigit(return_filename[i])) {
				valid=1;
			} else if (return_filename[i] == '_' && valid) {
				/* yes it is okay to delete the chars */
				seen_underscore=1;
				/* incremented, since it is then */
				/* decremented */
				i = lastchar+1;  
			} else {
				valid = 0;
				done = 1;	
			}
		}

		i--;
	}

	/* be sure to free this somewhere */
	return(return_filename); 
}

/* This function takes one of the following: */
/* 	SLIB_SEARCH_START   starts a new search for a source file */
/* 	SLIB_SEARCH_NEXT    returns the next instance of the file if exist */
/* 	SLIB_SEARCH_DONE    finished searching */
/* filename is the raw symbol/whatever filename, this function does the all */
/* the required stripping (up to the first period) */
/* don't forget to free the returned string hack */
char *
s_slib_search(char *filename, int flag)
{
	char *processed_name=NULL;
	char *new_filename=NULL;
	char *string=NULL;
	char number_suffix[50]; /* a reasonablly large number hack ? */
	int len;
	int len2;
	static int count;

	switch(flag) {
		case(SLIB_SEARCH_START):
			count = 0;
			string=NULL;
		break;

		case(SLIB_SEARCH_NEXT):
			count++;

			/* be sure to free processed_name */
			processed_name = s_slib_getbasename(filename);	

#if DEBUG 
			printf("proced: %s\n", processed_name);
#endif

			len = strlen(processed_name);

			/* for now only look for .sch's */
			/* this needs to be *MUCH* more flexible */
			/* number_suffix is large enough ? */
			sprintf(number_suffix, "_%d.sch", count); 
			len2 = strlen(number_suffix);
			new_filename = (char *) 
				malloc (sizeof(char)*(len+len2+1));

			sprintf(new_filename, "%s%s", processed_name, number_suffix);
			string = s_slib_search_lowlevel(new_filename);

			free(new_filename);
		break;

		case(SLIB_SEARCH_DONE):
			count = 0;
			string=NULL;
		break;
	}

	if (processed_name)
		free(processed_name);

	/* don't forget to free this string */
	return(string);
}

void
s_slib_free()
{
	int i;

	for (i = 0; i < slib_index; i++) {
		if (slib[i].dir_name)
               		free(slib[i].dir_name);
	}

	slib_index=0;
}

void
s_slib_init()
{
	int i;
	for (i = 0; i < MAX_SLIBS; i++) {
		slib[i].dir_name = NULL;	
	} 
}

/* returns slibs */
char *
s_slib_getdir(int index)
{
	if (slib[index].dir_name != NULL)
		return(slib[index].dir_name);
	else 
		return(NULL);
}


/* This is TOTALLY BROKEN! */
/* statics are not allowed anymore */
/* flag can be either OPEN_DIR, READ_DIR, CLOSE_DIR */
/* OPEN_DIR opens the directory and returns null */
/* READ_DIR returns the next non "." element */
/* CLOSE_DIR closes the directory */
/* this function is not reentrant */
char *
s_slib_getfiles(char *directory, int flag)
{
	static DIR *ptr;
        static struct dirent *dptr;
	static char *whole_dir[256]; /* make this dynamic hack */
	static int count=0;
	static int current=0;

	int j;
	int len;

	switch(flag) {

		case(CLOSE_DIR):
			if (ptr) {
				closedir(ptr);
			}

			ptr = NULL;

			for (j = 0 ; j < count ;j++) {
				if (whole_dir[j]) 
					free(whole_dir[j]);
			}
			count = current = 0 ;

			return(NULL);
		break;

	/* open the directory and return first element (after if) */
		case(OPEN_DIR):

			if (ptr) {
				closedir(ptr);
			}

			ptr = NULL;

			for (j = 0 ; j < count ;j++) {
				if (whole_dir[j]) 
					free(whole_dir[j]);
			}
			count = current = 0 ;

        		ptr = opendir(directory); /* hack check for existance */

			if (ptr == NULL) 
				return(NULL);


			/* now read the entire directory */
			dptr = readdir(ptr);

			while (dptr != NULL) {

				/* skip .'s */
				while (dptr != NULL) {
					if (dptr->d_name[0] == '.') {
						dptr = readdir(ptr);
					} else {
						break;
					}
				}
		
				if (dptr == NULL) {
					break;
				}	

				if (dptr->d_name != NULL) {
					len = strlen(dptr->d_name);

					/* hack */
					if (count < 256) {

						whole_dir[count] = (char *)
						    malloc(sizeof(char)*len+1);
						strcpy(whole_dir[count], 
					            dptr->d_name);
						count++;
					} else {
						fprintf(stderr, 
				"uggg. too many files in s_slib_getfiles!\n");
						exit(-1);
					}
				}

				dptr = readdir(ptr);
			}
			return(NULL);

		break;

		case(READ_DIR):

		
			if (whole_dir[current] && current < count) {
				return(whole_dir[current++]);
			} else {
				return(NULL);
			}

		break;

		default:
			return(NULL);
	}

#if DEBUG
	for (j = 0;j < count; j++) {
		printf("string: %s\n", whole_dir[j]);
	}
#endif

}

void
s_slib_print(void)
{
	int i;

	for (i = 0; i < slib_index; i++) {
		printf("%s\n", slib[i].dir_name);
	}
}

int
s_slib_uniq(char *path)
{
	if (s_slib_search_for_dirname(path)) {

#if DEBUG 
		printf("found\n");
#endif
		return(0);
	} else {

#if DEBUG
		printf("NOT found\n");
#endif

		return(1);
	}
}

void
s_slib_print_dirs(void)
{
	int i;
	char *string;
	char *file;

	i = 0;
	string = s_slib_getdir(i);
	while(string != NULL) {

		s_clib_getfiles(string, OPEN_DIR);
		printf("Opened %s\n", string);

		file = (char *) s_clib_getfiles(string, READ_DIR);

		while(file != NULL) {
			printf("file: %s\n", file);
			file = (char *) s_clib_getfiles(string, READ_DIR);
		}

		printf("Closed %s\n", string);
		s_clib_getfiles(string, CLOSE_DIR);
		i++;
		string = s_slib_getdir(i);
	}
}
