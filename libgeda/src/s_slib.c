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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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

      /* Do a substring comp for a match */
      if (strstr(dptr->d_name, basename) != NULL)  {
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

  if (ptr) {
    closedir(ptr);
    ptr = NULL;
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
    /* return type */

    s_log_message(_("Found [%s]\n"), basename);
    /* s_log_message("Found [%s] in [%s]\n", basename, slib_path);*/

    full_path = g_build_filename (slib_path, basename, NULL);
		
    g_free(slib_path);

    return(full_path);
  } else {

    s_log_message(_("Could not find [%s] in any SourceLibrary\n"), basename);

    return(NULL);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief Get the base file name from a raw file name string.
 *  \par Function Description
 *  This function takes a raw file name and returns a processed file name.
 *  It takes the raw file name and copies everything up to the first period
 *  and removes any _# (where # is any number of digits.
 *  
 *  \param [in] rawname  Character string with the raw file name to parse.
 *  \return The base file name in a character string.
 *
 *  \warning
 *  Caller must g_free returned pointer.
 */
char *s_slib_getbasename(const char *rawname)
{
  char *return_filename;
  int i;
  int done=0;
  int lastchar;
  int valid=0;
  int len;
  int seen_underscore=0;
	
  if (!rawname) 
  return(NULL);

  len = strlen(rawname)+1;

  return_filename = (char *) g_malloc(sizeof(char)*len);

  i = 0;
  /* first get everything up to the leading dot */
  while(rawname[i] != '\0' && rawname[i] != '.') {
    return_filename[i] = rawname[i];
    i++;
  }


  return_filename[i] = '\0';

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
      if (isdigit((int) return_filename[i])) {
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

  /* be sure to g_free this somewhere */
  return(return_filename); 
}

/*! \todo Finish function documentation!!!
 *  \brief Search SLIB for a particular file name.
 *  \par Function Description
 *  This function will search the SLIB for a particular file name starting
 *  at a location specified by the <B>flag</B> parameter.
 *
 *  \param [in] filename  Character string with file name to search for.
 *  \param [in] flag      Specifies search start location. (See below...)
 *
 *  The <B>flag</B> parameter can be one of the following values:
 *  <DL>
 *    <DT>SLIB_SEARCH_START</DT><DD>Starts a new search for a source file.
 *    <DT>SLIB_SEARCH_NEXT</DT><DD>Returns the next instance of the file if
 *                                 one exists.
 *    <DT>SLIB_SEARCH_DONE</DT><DD>Finish searching.
 *  </DL>
 *
 *  Filename is the raw symbol/whatever file name. This function does all the
 *  required stripping (up to the first period).
 *
 *  \warning
 *  Caller must g_free returned pointer.
 */
char *s_slib_search(const char *filename, int flag)
{
  char *processed_name=NULL;
  char *new_filename=NULL;
  char *string=NULL;
  static int count;

  switch(flag) {
    case(SLIB_SEARCH_START):
      count = 0;
      string=NULL;
      break;

    case(SLIB_SEARCH_NEXT):
      count++;

      /* be sure to g_free processed_name */
      processed_name = s_slib_getbasename(filename);	

#if DEBUG 
      printf("proced: %s\n", processed_name);
#endif

      /* for now only look for .sch's */
      /* this needs to be *MUCH* more flexible */
      /* number_suffix is large enough ? */
      new_filename = g_strdup_printf ("%s_%d.sch", processed_name, count);

      string = s_slib_search_lowlevel(new_filename);

      g_free(new_filename);
      break;

    case(SLIB_SEARCH_DONE):
      count = 0;
      string=NULL;
      break;
  }

  g_free(processed_name);

  /* don't forget to g_free this string */
  return(string);
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
 *  \warning
 *  Caller must not free the returned pointer.
 */
/* returns slibs */
char *s_slib_getdir(int index)
{
  if (slib[index].dir_name != NULL)
    return(slib[index].dir_name);
  else 
    return(NULL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in] directory  Character string with directory to get files from.
 *  \param [in] flag       Search control flag. (See below...)
 *  \return A file name if one is found, NULL otherwise.
 *
 *  \warning
 *  Caller must g_free returned pointer.
 *
 *  The flag parameter can be one of the following values:
 *  <DL>
 *    <DT>OPEN_DIR</DT><DD>Opens the directory and returns NULL.
 *    <DT>READ_DIR</DT><DD>Returns the next non "." entry.
 *    <DT>CLOSE_DIR</DT><DD>Closes the directory.
 *  </DL>
 *  \bug This is TOTTALLY BROKEN!
 *       statics are not allowed anymore
 *  \warning
 *  this function is not reentrant
 */
char *s_slib_getfiles(char *directory, int flag)
{
  static DIR *ptr;
  static struct dirent *dptr;
  static char *whole_dir[256]; /* make this dynamic hack */
  static int count=0;
  static int current=0;

  int j;

  switch(flag) {

    case(CLOSE_DIR):
      if (ptr) {
        closedir(ptr);
      }

      ptr = NULL;

      for (j = 0 ; j < count ;j++) {
        g_free(whole_dir[j]);
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
        g_free(whole_dir[j]);
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
          /* hack */
          if (count < 256) {

            whole_dir[count] = g_strdup (dptr->d_name);
            count++;
          } else {
            g_error ("uggg. too many files in s_slib_getfiles!\n");
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_slib_print(void)
{
  int i;

  for (i = 0; i < slib_index; i++) {
    printf("%s\n", slib[i].dir_name);
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void s_slib_print_dirs(void)
{
  int i;
  char *string;
  char *file;

  i = 0;
  string = s_slib_getdir(i);
  while(string != NULL) {

    s_slib_getfiles(string, OPEN_DIR);
    printf("Opened %s\n", string);

    file = (char *) s_slib_getfiles(string, READ_DIR);

    while(file != NULL) {
      printf("file: %s\n", file);
      file = (char *) s_slib_getfiles(string, READ_DIR);
    }

    printf("Closed %s\n", string);
    s_slib_getfiles(string, CLOSE_DIR);
    i++;
    string = s_slib_getdir(i);
  }
}
