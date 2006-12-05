/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
/*! \file s_clib.c
 *  <B>clib</B> stands for component library.
 *
 *  A component library is made of several directories gathering
 *  component files. 
 *
 *  It must first be initialized with #s_clib_init(). When it is no more
 *  useful, use #s_clib_free() to free memory and reset the component
 *  library.
 *
 *  To add a directory to the library, use #s_clib_add_directory().
 *
 *  To retrieve a list of the directories that make the library, use
 *  #s_clib_get_directories(). For a list of component files in a
 *  directory of the library, use #s_clib_get_files().
 *
 *  #s_clib_search_basename() let you find a specific component from
 *  its name. Please note that it returns a list of directories as there
 *  may be several places that contains a component with this name.
 */

#include <config.h>

#include <glib.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#include "defines.h"


void s_clib_free (void);

static GList *clib_directories = NULL;

static GHashTable *clib_cache = NULL;

/*! \brief Initializes the component library handling code.
 *  \par Function Description
 *  Initializes the component library handling code.
 *  \warning This function must be called before any other function
 *           of this file.
 */
void s_clib_init (void)
{
  if (clib_directories != NULL || clib_cache != NULL) {
    s_clib_free ();
  }

  clib_cache = g_hash_table_new (g_str_hash, g_str_equal);
  
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void clib_free_cache_entry (gpointer key, gpointer value,
				   gpointer user_data)
{
  g_free (key);
  if (value != NULL) {
    /* value is a singly-linked list of strings */
    g_list_foreach (value, (GFunc)g_free, NULL);
    g_slist_free ((GSList*)value);
  }
}

/*! \brief Frees memory used by the component library.
 *  \par Function Description
 *  Frees memory used by the component library.
 */
void s_clib_free (void)
{
  if (clib_directories != NULL) {
    g_list_foreach (clib_directories, (GFunc)g_free, NULL);
    g_list_free (clib_directories);
    clib_directories = NULL;
  }

  if (clib_cache != NULL) {
    g_hash_table_foreach (clib_cache, clib_free_cache_entry, NULL);
    g_hash_table_destroy (clib_cache);
    clib_cache = NULL;
  }
  
}

/*! \brief Adds a new directory to the component library.
 *  \par Function Description
 *  Adds <B>directory</B> as a new directory for the component library.
 *
 *  \param [in] directory  Character string with the new directory name.
 */
void s_clib_add_directory (const gchar *directory)
{
  /* search for directory in clib_directories */
  if (!g_list_find_custom (clib_directories,
			   directory,
			   (GCompareFunc) g_strcasecmp))
  {
    /* directory not yet in the list of known directories */
    /* add directory to list */
    clib_directories = g_list_append (clib_directories,
				      g_strdup (directory));
  }
  
}

/*! \brief Get list of component library directories.
 *  \par Function Description
 *  This function returns the list of directories part of
 *  the component library.
 *
 *  \return Global libgead #clib_directories variable.
 *  \warning
 *  The returned value is owned by libgeda and must not be modified or freed.
 *
 */
const GList *s_clib_get_directories()
{
  return clib_directories;
}

/*! \brief Get a list of files found a directory.
 *  \par Function Description
 *  This function returns a list of file names found in <B>directory</B> and
 *  that match <B>filter</B>
 *
 *  \param [in] directory  Character string with the path to search.
 *  \param [in] filter     Character string to compare file names against.
 *  \return List of file name that matched <B>filter</B>, NULL otherwise.
 */
GSList *s_clib_get_files (const gchar *directory, const gchar *filter)
{
  GDir *dir;
  const gchar *entry;
  GSList *ret = NULL;

  /* check directory is in clib_directories */
  if (g_list_find_custom (clib_directories,
			  directory,
			  (GCompareFunc) g_strcasecmp) == NULL)
  {
    /* no, unknown directory: report an error */
    s_log_message ("Directory [%s] is not part of the component library\n",
                   directory);
    return NULL;
  }

  /* open the directory */
  dir = g_dir_open (directory, 0, NULL);
  if (dir == NULL) {
    s_log_message ("Failed to open directory [%s]\n", directory);
    return NULL;
  }

  /* now read the entire directory */
  /* and build a list of filenames in directory that match filter */
  while ((entry = g_dir_read_name (dir)) != NULL) {
    /* skip .'s */
    if (entry[0] == '.') {
      continue;
    }

    /* identify filter-matching filenames */
    if (strstr (entry, filter)) {
      ret = g_slist_append (ret, (gpointer)g_strdup (entry));
    }
   
  }
  
  /* finished: close the directory stream */
  g_dir_close (dir);

  /* sort the list alphabetically */
  ret = g_slist_sort (ret, (GCompareFunc)g_strcasecmp);
  
  /* and return the sorted list of filenames */
  return ret;
}

/*! \brief Search for a symbol file in the component library.
 *  \par Function Description
 *  Searches in component library for a symbol file with name <B>basename</B>.
 *
 *  \param [in] basename  Character string with base symbol name to search for.
 *  \return List of directories where symbol file with this name was found,
 *          NULL otherwise.
 *
 *  \warning
 *  The returned value is owned by libgeda and must not be modified or freed.
 *
 */
const GSList *s_clib_search_basename(const gchar *basename)
{
  GSList *ret; 
  GList *tmp;
  
  /* first check if basename is in cache */
  ret = g_hash_table_lookup (clib_cache, basename);
  if (ret != NULL) {
    /* yes, found basename in cache, nothing more to do */
    return ret;
  }

  /* looks like we have to search for basename in the library */
  for (tmp = g_list_last(clib_directories); 
       tmp != NULL; tmp = g_list_previous (tmp)) {
    gchar *dir_name  = (gchar*)tmp->data;
    gchar *file_name = g_strconcat (dir_name,
                                    G_DIR_SEPARATOR_S,
                                    basename,
                                    NULL);

    if (g_file_test (file_name, G_FILE_TEST_EXISTS)) {
      /* add directory name to the list */
      ret = g_slist_append (ret, g_strdup (dir_name));
    }

    g_free (file_name);
  }

  /* have we found something? */
  if (ret != NULL) {
    /* yes, add the result to cache */
    g_hash_table_insert (clib_cache, g_strdup (basename), ret);
  }
  
  return ret;
}
