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
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <config.h>

#include <stdio.h>
#include <stdlib.h> 
#include <sys/types.h>

#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/gdkx.h>

#include <guile/gh.h>

#if defined(HAVE_DIRENT_H) && !defined(WIN32)
#include <dirent.h>
#endif

#include "struct.h"
#include "defines.h"
#include "globals.h"

#include "../include/prototype.h"



/* clib stands for component library */

/* currently everytime you access a component directory, you will search   */
/* the directory.  This might be slow, but perhaps the OS provided caching */
/* of directories is enough? */
/* eventually you will want to cache the directories which hold components */
/* and only search for the components if the date changes on the directory */
/* but I'll add that later... hack */

/* need to test everything at boundary conditions (exceed cache size etc...) */

/* memory is allocated here in these functions and sticks around */
/* ie if somebody returns a char * to the outside world then that memory */
/* is already allocated */
/* so all memory must be free in the calling routines */
 

struct st_clib {
	char *dir_name;
};

static int clib_index=0;

#define MAX_CLIBS	128

/* and eventually make this unlimited */
/* hack hack */
static struct st_clib clib[MAX_CLIBS];



/* Cache for caching (!) files in clib directories */
struct st_clib_cache {
	char *basename;
	char *clib;
}; 

static int clib_cache_index=0;

#define MAX_CLIBS_CACHE	64	

/* This cache is fixed, and acts like a ring buffer */
static struct st_clib_cache clib_cache[MAX_CLIBS_CACHE];

int 
s_clib_add_entry(char *new_path) 
{
	if (new_path == NULL) {
		return(-1); 
	}

	if (clib_index >= MAX_CLIBS) {
		return(-1); 
	}

	clib[clib_index].dir_name = (char *) malloc(sizeof(char)*strlen(new_path)+1);

	strcpy(clib[clib_index].dir_name, new_path);

	clib_index++;
	return(clib_index);
}

/* return true if it finds it else zero */
int
s_clib_search_for_dirname(char *dir_name)
{
	int i;

	for (i = 0; i < clib_index; i++) {
		if (strcmp(clib[i].dir_name, dir_name) == 0) {
			return(1);	
		}	
	}

	return(0);
}


char *
s_clib_cache_search(char *basename) 
{
	char *clib_path=NULL;
	int i=0;

	/* can you have entries after the first null? hack */
	for (i = 0; i < clib_cache_index; i++) {
		if (strcmp(clib_cache[i].basename, basename) == 0) {
			clib_path = (char *) malloc(sizeof(char)*
					strlen(clib_cache[i].clib)+1);
			strcpy(clib_path, clib_cache[i].clib);
			return(clib_path);
		}
	}
	return(NULL);
}

int
s_clib_cache_add(char *clib_path, char *basename)
{
	int len;

	if (clib_path == NULL || basename == NULL) {
		return(-1); /* oops */
	}

	if (clib_cache[clib_cache_index].basename)
		free(clib_cache[clib_cache_index].basename);

	if (clib_cache[clib_cache_index].clib)
                free(clib_cache[clib_cache_index].clib);

	len = strlen(clib_path);
	clib_cache[clib_cache_index].clib = (char *) malloc(sizeof(char)*len+1);
	strcpy(clib_cache[clib_cache_index].clib, clib_path);

	len = strlen(basename);
	clib_cache[clib_cache_index].basename = (char *) malloc(sizeof(char)*len+1);
	strcpy(clib_cache[clib_cache_index].basename, basename);

	clib_cache_index = ( clib_cache_index + 1 ) % MAX_CLIBS_CACHE;

	return(0);	
}

char *
s_clib_search_dirs(char *basename)
{
	int i;
	int len;
	DIR *ptr=NULL;
        struct dirent *dptr;
	char *clib_path=NULL;

	/* search the list backward */
	for (i = clib_index-1 ; i >= 0; i--) {
	/* for (i = 0 ; i < clib_index; i++) {*/

#if DEBUG
		printf("searching %d _%s_\n", i, clib[i].dir_name);
#endif
        	ptr = opendir(clib[i].dir_name);

		if (ptr == NULL) {
			fprintf(stderr, "Oops got a null dir_name!\n");
			exit(-1);
		}

		dptr = readdir(ptr);

		while(dptr != NULL) {

			if (strcmp(dptr->d_name, basename) == 0)  {
				len = strlen(clib[i].dir_name);				
				clib_path = (char *) malloc(sizeof(char)*len+1);
				strcpy(clib_path, clib[i].dir_name);
	
				if (ptr) {
					closedir(ptr);
					ptr = NULL;
				}

				return(clib_path);
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
s_clib_search(char *basename) 
{
	char *clib_path=NULL;

	clib_path = s_clib_cache_search(basename);

	if (clib_path) {

#if DEBUG
		printf("Found [%s] in [%s] (cached)\n", basename, clib_path);
#endif
		return(clib_path);
	} else {
		/* not in cache so search for it in dirs */
		/* put it in the cache too */
		clib_path = s_clib_search_dirs(basename);

		if (clib_path) {
			/* return type */
#if DEBUG
			printf("Found [%s] in [%s]\n", basename, clib_path);
#endif
			s_clib_cache_add(clib_path, basename);
			return(clib_path);
		} else {
			fprintf(stderr, "Could not find [%s] in any ComponentLibrary\nSymbol will be removed from schematic\n", basename);
			return(NULL);
		}

	}
}

void
s_clib_print()
{
	int i;

	for (i = 0; i < clib_index; i++) {
		printf("%s\n", clib[i].dir_name);
	}
}


void
s_clib_cache_free()
{
	int i=0;	

	/* this has to be MAX_CLIBS_CACHE since clib_cache_index can wrap */
	/* around */
	for (i = 0; i < MAX_CLIBS_CACHE; i++) {
		if (clib_cache[i].basename) {
			free(clib_cache[i].basename);
			clib_cache[i].basename = NULL;	
		}

		if (clib_cache[i].clib) {
               		free(clib_cache[i].clib);
			clib_cache[i].clib = NULL;	
		}
	}

	clib_cache_index=0;
}

void
s_clib_free()
{
	int i;

	for (i = 0; i < clib_index; i++) {
		if (clib[i].dir_name) {
               		free(clib[i].dir_name);
			clib[i].dir_name = NULL;
		}		
	}

	clib_index=0;
}

void
s_clib_init()
{
	int i;
	for (i = 0; i < MAX_CLIBS; i++) {
		clib[i].dir_name = NULL;	
	} 

	for (i = 0; i < MAX_CLIBS_CACHE; i++) {
		clib_cache[i].basename = NULL;	
		clib_cache[i].clib = NULL;	
	} 
}

/* returns clibs */
char *
s_clib_getdir(int index)
{
	if (clib[index].dir_name != NULL)
		return(clib[index].dir_name);
	else 
		return(NULL);
}


/* This is TOTALLY BROKEN! */
/* statics are not allowed anymore */
/* flag can be either OPEN_DIR, READ_DIR, CLOSE_DIR */
/* OPEN_DIR opens the directory and null */
/* READ_DIR returns the next none "." element */
/* CLOSE_DIR closes the directory */
/* this function is not reentrant */
char *
s_clib_getfiles(char *directory, int flag)
{
	static DIR *ptr;
        static struct dirent *dptr;
	static char *whole_dir[1024]; /* make this dynamic hack */
	static int count=0;
	static int current=0;
	
	char *temp;
	int j;
	int len;
	int done=0;
	int last;
	int first;

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
					if (count < 1024) {

						whole_dir[count] = (char *)
						    malloc(sizeof(char)*len+1);
						strcpy(whole_dir[count], 
					            dptr->d_name);
						count++;
					} else {
						fprintf(stderr, 
				"uggg. too many files in s_clib_getfiles!\n");
						exit(-1);
					}
				}

				dptr = readdir(ptr);
			}

			/* now sort the list */
			/* use a simple bubble sort */	
			/* anybody have a better faster one? */
			first = 0;
			last = count;
			while(!done) {
				done = 1;
				for (j = first ; j < last-1; j++) {
					if (strcmp(whole_dir[j], 	
						whole_dir[j+1]) > 0) {
						temp = whole_dir[j];	
						whole_dir[j] = whole_dir[j+1];
						whole_dir[j+1] = temp;
						done = 0;
					}
				}
				last = last - 1;
#if DEBUG
			printf("pass\n");
#endif
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

int
s_clib_uniq(char *path) 
{
	if (s_clib_search_for_dirname(path)) {
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

