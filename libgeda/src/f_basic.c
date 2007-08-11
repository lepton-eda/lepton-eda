/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#ifdef HAVE_UNISTD_H
#include <unistd.h> 
#endif

#include <sys/param.h>
#include <limits.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <gtk/gtk.h>
#include <libguile.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "defines.h"
#include "struct.h"
#include "globals.h"  

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Opens the schematic file.
 *  \par Function Description
 *  Opens the schematic file by calling f_open_flags() with the
 *  F_OPEN_RC and F_OPEN_CHECK_BACKUP flags.
 *
 *  \param [in,out] w_current  The TOPLEVEL object to load the schematic into.
 *  \param [in]      filename  A character string containing the file name
 *                             to open.
 *  \return 0 on failure, 1 on success.
 */
int f_open(TOPLEVEL *w_current, const gchar *filename)
{
  return f_open_flags (w_current, filename, 
                       F_OPEN_RC | F_OPEN_CHECK_BACKUP);
}

/*! \brief Opens the schematic file with fine-grained control over behaviour.
 *  \par Function Description
 *  Opens the schematic file and carries out a number of actions
 *  depending on the \a flags set.  If #F_OPEN_RC is set, executes
 *  configuration files found in the target directory.  If
 *  #F_OPEN_CHECK_BACKUP is set, warns user if a backup is found for
 *  the file being loaded, and possibly prompts user for whether to
 *  load the backup instead.  If #F_OPEN_RESTORE_CWD is set, does not
 *  change the working directory to that of the file being loaded.
 *
 *  \param [in,out] w_current  The TOPLEVEL object to load the schematic into.
 *  \param [in]     filename   A character string containing the file name
 *                             to open.
 *  \param [in]     flags      Combination of #FOpenFlags values.
 *  \return 0 on failure, 1 on success.
 */
int f_open_flags(TOPLEVEL *w_current, const gchar *filename, 
                 const gint flags)
{
  int opened=FALSE;
  char *full_filename = NULL;
  char *full_rcfilename = NULL;
  char *file_directory = NULL;
  char *saved_cwd = NULL;
  char *backup_filename = NULL;
  char load_backup_file = 0;

  /* has the head been freed yet? */
  /* probably not hack PAGE */

  set_window(w_current, w_current->page_current,
             w_current->init_left, w_current->init_right,
             w_current->init_top,  w_current->init_bottom);


  /* Cache the cwd so we can restore it later. */
  /*! \bug Assumes cwd will be less than 1024 characters. */
  if (flags & F_OPEN_RESTORE_CWD) {
    saved_cwd = getcwd(NULL, 1024);
  }

  /* get full, absolute path to file */
  full_filename = f_normalize_filename(filename); 

  /* write full, absolute filename into page_current->page_filename */
  if (w_current->page_current->page_filename) {
    g_free(w_current->page_current->page_filename);
  }
  w_current->page_current->page_filename = g_strdup(full_filename);

  /* Before we open the page, let's load the corresponding gafrc. */
  /* First cd into file's directory. */
  file_directory = g_dirname (full_filename);

  if (file_directory) { 
    chdir(file_directory);  
    /*! \bug Probably should do some checking of chdir return values */
  }

  /* Now open RC and process file */
  if (flags & F_OPEN_RC) {
    full_rcfilename = g_strconcat (file_directory,  
                                   G_DIR_SEPARATOR_S, 
                                   "gafrc",
                                   NULL);
    g_rc_parse_specified_rc(w_current, full_rcfilename);
  }

  if (flags & F_OPEN_CHECK_BACKUP) {
    /* Check if there is a newer autosave backup file */
    backup_filename 
      = g_strdup_printf("%s%c"AUTOSAVE_BACKUP_FILENAME_STRING,
                        file_directory, G_DIR_SEPARATOR, 
                        g_path_get_basename(full_filename));

    g_free (file_directory);

    if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) && 
         (! g_file_test (backup_filename, G_FILE_TEST_IS_DIR))) {
      /* An autosave backup file exists. Check if it's newer */
      struct stat stat_backup;
      struct stat stat_file;
      char error_stat = 0;
      GString *message;
    
      if (stat (backup_filename, &stat_backup) != 0) {
        s_log_message ("f_open: Unable to get stat information of backup file %s.", 
                       backup_filename);
        error_stat = 1 ;
      }
      if (stat (full_filename, &stat_file) != 0) {
        s_log_message ("f_open: Unable to get stat information of file %s.", 
                       full_filename);
        error_stat = 1;
      }
      if ((difftime (stat_file.st_ctime, stat_backup.st_ctime) < 0) ||
          (error_stat == 1))
        {
          /* Found an autosave backup. It's newer if error_stat is 0 */
          message = g_string_new ("");
          g_string_append_printf(message, "\nWARNING: Found an autosave backup file:\n  %s.\n\n", backup_filename);
          if (error_stat == 1) {
            g_string_append(message, "I could not guess if it is newer, so you have to"
                            "do it manually.\n");
          }
          else {
            g_string_append(message, "The backup copy is newer than the schematic, so it seems you should load it instead of the original file.\n");
          }
          g_string_append (message, "Gschem usually makes backup copies automatically, and this situation happens when it crashed or it was forced to exit abruptly.\n");
          if (w_current->page_current->load_newer_backup_func == NULL) {
            s_log_message(message->str);
            s_log_message("\nRun gschem and correct the situation.\n\n");
            fprintf(stderr, message->str);
            fprintf(stderr, "\nRun gschem and correct the situation.\n\n");
          }
          else {
            /* Ask the user if load the backup or the original file */
            if (w_current->page_current->load_newer_backup_func 
                (w_current, message)) {
              /* Load the backup file */
              load_backup_file = 1;
            }
          }
          g_string_free (message, TRUE);
        }
    }
  }

  /* Now that we have set the current directory and read
   * the RC file, it's time to read in the file. */
  if (load_backup_file == 1) {
    /* Load the backup file */
    w_current->page_current->object_tail = (OBJECT *) 
    o_read(w_current, w_current->page_current->object_tail, 
	   backup_filename);
  } else if (g_file_test (full_filename, G_FILE_TEST_EXISTS)) {
    /* Load the original file */
    w_current->page_current->object_tail = (OBJECT *) 
    o_read(w_current, w_current->page_current->object_tail, 
	   full_filename);
  }

  if (w_current->page_current->object_tail != NULL) {
    s_log_message("Opened file [%s]\n", full_filename);
    opened = TRUE;
  } else {
    /* Failed to open page */
    opened = FALSE;	 
  }

  w_current->page_current->object_tail 
    = (OBJECT *) return_tail(w_current->page_current->object_head); 

  /* make sure you init net_consolide to false (default) in all */
  /* programs */
  if (w_current->net_consolidate == TRUE) {	
    o_net_consolidate(w_current);
  }

  if (load_backup_file == 0) {
    /* If it's not the backup file */
    w_current->page_current->CHANGED=0; /* added 4/7/98 */
  }
  else {
    /* We are loading the backup file, so gschem should ask
       the user if save it or not when closing the page. */
    w_current->page_current->CHANGED=1;
  }

  g_free(full_filename);
  g_free(full_rcfilename);
  g_free (backup_filename);

  /* Reset the directory to the value it had when f_open was
   * called. */
  if (flags & F_OPEN_RESTORE_CWD) {
    chdir(saved_cwd);
    g_free(saved_cwd);
  }

  if (!opened) {
    return (FALSE);
  } else {
    return (TRUE);
  }
}

/*! \brief Closes the schematic file
 *  \par Function Description
 *  Does nothing
 *
 *  \param [in,out] w_current  The TOPLEVEL object with schematic to be closed.
 */
void f_close(TOPLEVEL *w_current)
{

}

/*! \brief Save schematic file and close
 *  \par Function Description
 *  This function will save the current schematic file before closing it.
 *  It also deletes the page_current item in the TOPLEVEL structure.
 *
 *  \param [in,out] w_current  The TOPLEVEL object containing the schematic.
 *  \param [in]      filename  The file name to save the schematic to.
 */
void f_save_close(TOPLEVEL *w_current, char *filename)
{
  o_save(w_current, filename);
  s_page_delete (w_current, w_current->page_current);
}

/*! \brief Save the schematic file
 *  \par Function Description
 *  This function saves the current schematic file in the w_current object.
 *
 *  \param [in,out] w_current  The TOPLEVEL object containing the schematic.
 *  \param [in]      filename  The file name to save the schematic to.
 *  \return 1 on success, 0 on failure.
 */
int f_save(TOPLEVEL *w_current, const char *filename)
{
  gchar *backup_filename;
  gchar *real_filename;
  gchar *only_filename;
  gchar *dirname;
  mode_t saved_umask, mask;
  struct stat st;

  /* Get the real filename and file permissions */
  real_filename = follow_symlinks (filename, NULL);

  if (real_filename == NULL) {
    s_log_message ("Can't get the real filename of %s.", filename);
    fprintf (stderr, "Can't get the real filename of %s.\n", filename);
    return 0;
  }
  
  /* Get the directory in which the real filename lives */
  dirname = g_path_get_dirname (real_filename);
  only_filename = g_path_get_basename(real_filename);  

  /* Do a backup if it's not an undo file backup and it was never saved. */
  if (w_current->page_current->saved_since_first_loaded == 0) {    
    if ( (g_file_test (real_filename, G_FILE_TEST_EXISTS)) && 
	 (!g_file_test(real_filename, G_FILE_TEST_IS_DIR)) )
    {
      backup_filename = g_strdup_printf("%s%c%s~", dirname, 
					G_DIR_SEPARATOR, only_filename);

      /* Make the backup file read-write before saving a new one */
      if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) && 
	   (! g_file_test (backup_filename, G_FILE_TEST_IS_DIR))) {
	if (chmod(backup_filename, S_IREAD|S_IWRITE) != 0) {
	  s_log_message ("Could NOT set previous backup file [%s] read-write\n", 
			 backup_filename);	    
	}
      }
	
      if (rename(real_filename, backup_filename) != 0) {
	s_log_message ("Can't save backup file: %s.", backup_filename);
	fprintf (stderr, "Can't save backup file: %s.", backup_filename);
      }
      else {
	/* Make the backup file readonly so a 'rm *' command will ask 
	   the user before deleting it */
	saved_umask = umask(0);
	mask = (S_IWRITE|S_IWGRP|S_IEXEC|S_IXGRP|S_IXOTH);
	mask = (~mask)&0777;
	mask &= ((~saved_umask) & 0777);
	if (chmod(backup_filename, mask) != 0) {
	  s_log_message ("Could NOT set backup file [%s] readonly\n", 
			   backup_filename);	    
	}
	umask(saved_umask);
      }

      g_free(backup_filename);
    }
  }
    /* If there is not an existing file with that name, compute the
     * permissions and uid/gid that we will use for the newly-created file.
     */
       
  if (stat (real_filename, &st) != 0)
  {
    struct stat dir_st;
    int result;
    
    /* Use default permissions */
    saved_umask = umask(0);
    st.st_mode = 0666 & ~saved_umask;
    umask(saved_umask);
#ifdef HAVE_CHOWN
    st.st_uid = getuid ();
    
    result = stat (dirname, &dir_st);
    
    if (result == 0 && (dir_st.st_mode & S_ISGID))
	  st.st_gid = dir_st.st_gid;
    else
    st.st_gid = getgid ();
#endif /* HAVE_CHOWN */
  }
  g_free (dirname);
  g_free (only_filename);
  
  if (o_save(w_current, real_filename)) {

    w_current->page_current->saved_since_first_loaded = 1;

    /* Reset the last saved timer */
    g_get_current_time (&w_current->page_current->last_load_or_save_time);
    w_current->page_current->ops_since_last_backup = 0;
    w_current->page_current->do_autosave_backup = 0;

    /* Restore permissions. */
    chmod (real_filename, st.st_mode);
#ifdef HAVE_CHOWN
    chown (real_filename, st.st_uid, st.st_gid);
#endif

    g_free (real_filename);
    return 1;
  }
  else {
    g_free (real_filename);
    return 0;
  }
}

/*! \brief Reformats a filename as an absolute resolved filename
 *  \par Function Description
 *  Given a filename in any format, this returns the full, absolute
 *  resolved filename.
 *
 *  \param [in] filename  A character string containing the file
 *                        name to resolve.
 *  \return A character string with the resolved filename.
 */
char* f_normalize_filename(const gchar *filename)
{
  char filename_buffer[MAXPATHLEN];  /* nasty hack for realpath */
  char *full_filename;

  /*  Check for pathological case  */
  if (filename == NULL) {
    return NULL;
  }

  realpath(filename, filename_buffer);  /* places reult in filename_buffer */
  full_filename = g_strdup (filename_buffer);

#ifdef DEBUG
  printf("In f_normalize_filename, returning full_filename= %s \n", full_filename);
#endif 

  return full_filename;
}

/*! \brief Follow symlinks until a real file is found
 *  \par Function Description
 *  Does readlink() recursively until we find a real filename.
 *
 *  \param [in] filename  The filename to search for.
 *  \param [out]   error  Unused, set to NULL
 *  \return The path to real file on success, NULL otherwise.
 *
 *  \note Taken from gedit's source code.
 */
char *follow_symlinks (const gchar *filename, GError **error)
{
  gchar *followed_filename;
  gint link_count;
  
  g_return_val_if_fail (filename != NULL, NULL);
  
  g_return_val_if_fail (strlen (filename) + 1 <= MAXPATHLEN, NULL);
  
  followed_filename = g_strdup (filename);
  link_count = 0;
  
  while (link_count < MAX_LINK_LEVEL) {
    struct stat st;
    
    if (lstat (followed_filename, &st) != 0)
      /* We could not access the file, so perhaps it does not
       * exist.  Return this as a real name so that we can
       * attempt to create the file.
       */
      return followed_filename;
    
    if (S_ISLNK (st.st_mode)) {
      gint len;
      gchar linkname[MAXPATHLEN];
      
      link_count++;
      
      len = readlink (followed_filename, linkname, MAXPATHLEN - 1);
      
      if (len == -1) {
	s_log_message("Could not read symbolic link information for %s", followed_filename);
	fprintf(stderr, "Could not read symbolic link information for %s", followed_filename);
	g_free (followed_filename);
	return NULL;
      }
      
      linkname[len] = '\0';
      
      /* If the linkname is not an absolute path name, append
       * it to the directory name of the followed filename.  E.g.
       * we may have /foo/bar/baz.lnk -> eek.txt, which really
       * is /foo/bar/eek.txt.
       */
      
      if (linkname[0] != G_DIR_SEPARATOR) {
	gchar *slashpos;
	gchar *tmp;
	
	slashpos = strrchr (followed_filename, G_DIR_SEPARATOR);
	
	if (slashpos)
	  *slashpos = '\0';
	else {
	  tmp = g_strconcat ("./", followed_filename, NULL);
	  g_free (followed_filename);
	  followed_filename = tmp;
	}
	
	tmp = g_build_filename (followed_filename, linkname, NULL);
	g_free (followed_filename);
	followed_filename = tmp;
      } else {
	g_free (followed_filename);
	followed_filename = g_strdup (linkname);
      }
    } else
      return followed_filename;
  }

  /* Too many symlinks */
  
  s_log_message("The file has too many symbolic links.");
  fprintf(stderr, "The file has too many symbolic links.");
  
  return NULL;
}
