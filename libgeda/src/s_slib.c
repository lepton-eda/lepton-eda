/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
/*! slib stands for source (project/schematic/hdl/model source) library */

#include <config.h>

#include <stdio.h>
#include <sys/types.h>
#include <ctype.h>
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <dirent.h>

#include "libgeda_priv.h"

/* need to test everything at boundary conditions (exceed cache size etc...) */

/*! \brief */
struct st_slib {
  char *dir_name;
};

/*! \brief */
static int slib_index=0;

/*! \brief */
#define MAX_SLIBS	128

/*! \brief
 * and eventually make this unlimited
 * hack hack
 */
static struct st_slib slib[MAX_SLIBS];

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int s_slib_add_entry(char *new_path) 
{
  if (new_path == NULL) {
    return(-1); 
  }

  if (slib_index >= MAX_SLIBS) {
    return(-1); 
  }

  slib[slib_index].dir_name = g_strdup (new_path);

  slib_index++;
  return(slib_index);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \return 1 if directory is found, zero otherwise.
 */
int s_slib_search_for_dirname(char *dir_name)
{
  int i;

  for (i = 0; i < slib_index; i++) {
    if (strcmp(slib[i].dir_name, dir_name) == 0) {
      return(1);	
    }	
  }

  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *  Caller must g_free returned pointer.
 */
char *s_slib_search_dirs(const char *basename)
{
  int i;
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

    g_return_val_if_fail ((ptr != NULL), NULL);

    dptr = readdir(ptr);

    while(dptr != NULL) {

      /* Search for an exact match */
      if (strcmp(dptr->d_name, basename) == 0)  {
        slib_path = g_strdup (slib[i].dir_name);
	
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

  return(NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \warning
 *  Caller must g_free returned pointer.
 */
char *s_slib_search_lowlevel(const char *basename) 
{
  char *slib_path=NULL;
  char *full_path=NULL;

  slib_path = s_slib_search_dirs(basename);

  if (slib_path) {
    full_path = g_build_filename (slib_path, basename, NULL);
		
    g_free(slib_path);

    return(full_path);
  } else {
    return(NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief Search SLIB for a particular file name.
 *  \par Function Description
 *  This function will search the SLIB for a particular file name starting
 *  at a location specified by the <B>flag</B> parameter.
 *
 *  \param [in] filename  Character string with file name to search for.
 *
 *  Filename is the raw symbol/whatever file name. This function only looks
 *  for the file name as is and does no other changes to it.
 *
 *  \warning
 *  Caller must g_free returned pointer.
 */
char *s_slib_search_single(const char *filename)
{
  char *string=NULL;

  string = s_slib_search_lowlevel(filename);

  /* don't forget to g_free this string */
  return(string);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_slib_free()
{
  int i;

  for (i = 0; i < slib_index; i++) {
    g_free(slib[i].dir_name);
  }

  slib_index=0;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_slib_init()
{
  int i;
  for (i = 0; i < MAX_SLIBS; i++) {
    slib[i].dir_name = NULL;	
  } 
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
int s_slib_uniq(char *path)
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
