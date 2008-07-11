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
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Get the autosave filename for a file
 *  \par Function description
 *  Returns the expected autosave filename for the \a filename passed.
 *
 *  \warning The result should be freed when no longer needed.
 *
 *  \param [in] filename The filename to create an autosave filename for.
 *  \return A newly allocated string buffer.
 */
gchar *f_get_autosave_filename (const gchar *filename)
{
  gchar *result, *basename, *new_basename, *dirname;
  basename = g_path_get_basename(filename);
  dirname = g_path_get_dirname(filename);
  new_basename = g_strdup_printf(AUTOSAVE_BACKUP_FILENAME_STRING,
                                 basename);
  result = g_build_filename(dirname, new_basename, NULL);

  g_free(basename);
  g_free(new_basename);
  g_free(dirname);

  return result;
}

/*! \brief Check if a file has an active autosave file
 *  \par Function Description
 *  Checks whether an autosave file exists for the \a filename passed
 *  which has a modification time newer than the file itself.  If the
 *  check fails, sets \a err with the reason.  N.b. if the autosave
 *  file exists but it was not possible to get its modification time,
 *  returns TRUE.
 *
 *  \param [in] filename File to check
 *  \param [in,out] err  #GError structure for error reporting, or
 *                       NULL to disable error reporting
 *
 *  \returns TRUE if autosave active, FALSE otherwise
 */
gboolean f_has_active_autosave (const gchar *filename, GError **err)
{
  gboolean result = FALSE;
  gchar *auto_filename;
  gint file_err = 0;
  gint auto_err = 0;
  GFileError g_errcode = 0;
  struct stat file_stat, auto_stat;

  auto_filename = f_get_autosave_filename (filename);
  if (stat (filename, &file_stat) != 0) {
    file_err = errno;
  }
  if (stat (auto_filename, &auto_stat) != 0) {
    auto_err = errno;
  }

  /* A valid use of goto! (checks for raptors) */
  if (auto_err & ENOENT) {
    /* The autosave file does not exist. */
    result = FALSE;
    goto check_autosave_finish;
  }
  if (auto_err) {
    g_errcode = g_file_error_from_errno (auto_err);
    g_set_error (err, G_FILE_ERROR, g_errcode,
                 _("Failed to stat [%s]: %s"),
                 auto_filename, g_strerror (auto_err));
    result = TRUE;
    goto check_autosave_finish;
  }
  if (file_err & ENOENT) {
    /* The autosave file exists, but the actual file does not. */
    result = TRUE;
    goto check_autosave_finish;
  }
  if (file_err) {
    g_errcode = g_file_error_from_errno (file_err);
    g_set_error (err, G_FILE_ERROR, g_errcode,
                 _("Failed to stat [%s]: %s"),
                 auto_filename, g_strerror (file_err));
    result = TRUE;
    goto check_autosave_finish;
  }
  /* If we got this far, both files exist and we have managed to get
   * their stat info. */
  if (difftime (file_stat.st_mtime, auto_stat.st_mtime) < 0) {
    result = TRUE;
  }

 check_autosave_finish:
  g_free (auto_filename);
  return result;
}

/*! \brief Opens the schematic file.
 *  \par Function Description
 *  Opens the schematic file by calling f_open_flags() with the
 *  F_OPEN_RC and F_OPEN_CHECK_BACKUP flags.
 *
 *  \param [in,out] toplevel  The TOPLEVEL object to load the schematic into.
 *  \param [in]      filename  A character string containing the file name
 *                             to open.
 *  \param [in,out] err  #GError structure for error reporting, or
 *                       NULL to disable error reporting
 *
 *  \return 0 on failure, 1 on success.
 */
int f_open(TOPLEVEL *toplevel, const gchar *filename, GError **err)
{
  return f_open_flags (toplevel, filename,
                       F_OPEN_RC | F_OPEN_CHECK_BACKUP, err);
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
 *  \param [in,out] toplevel  The TOPLEVEL object to load the schematic into.
 *  \param [in]     filename   A character string containing the file name
 *                             to open.
 *  \param [in]     flags      Combination of #FOpenFlags values.
 *  \param [in,out] err  #GError structure for error reporting, or
 *                       NULL to disable error reporting
 *
 *  \return 0 on failure, 1 on success.
 */
int f_open_flags(TOPLEVEL *toplevel, const gchar *filename,
                 const gint flags, GError **err)
{
  int opened=FALSE;
  char *full_filename = NULL;
  char *full_rcfilename = NULL;
  char *file_directory = NULL;
  char *saved_cwd = NULL;
  char *backup_filename = NULL;
  char load_backup_file = 0;
  GError *tmp_err = NULL;

  /* has the head been freed yet? */
  /* probably not hack PAGE */

  set_window(toplevel, toplevel->page_current,
             toplevel->init_left, toplevel->init_right,
             toplevel->init_top,  toplevel->init_bottom);


  /* Cache the cwd so we can restore it later. */
  /*! \bug Assumes cwd will be less than 1024 characters. */
  if (flags & F_OPEN_RESTORE_CWD) {
    saved_cwd = getcwd(NULL, 1024);
  }

  /* get full, absolute path to file */
  full_filename = f_normalize_filename (filename, &tmp_err);
  if (full_filename == NULL) {
    g_set_error (err, G_FILE_ERROR, tmp_err->code,
                 _("Cannot find file %s: %s"),
                 filename, g_strerror (tmp_err->code));
    return 0;
  }

  /* write full, absolute filename into page_current->page_filename */
  g_free(toplevel->page_current->page_filename);
  toplevel->page_current->page_filename = g_strdup(full_filename);

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
    g_rc_parse_specified_rc(toplevel, full_rcfilename);
  }

  g_free (file_directory);

  if (flags & F_OPEN_CHECK_BACKUP) {
    /* Check if there is a newer autosave backup file */
    GString *message;
    gboolean active_backup = f_has_active_autosave (full_filename, &tmp_err);
    backup_filename = f_get_autosave_filename (full_filename);

    if (tmp_err != NULL) g_warning ("%s\n", tmp_err->message);
    if (active_backup) {
      message = g_string_new ("");
      g_string_append_printf(message, _("\nWARNING: Found an autosave backup file:\n  %s.\n\n"), backup_filename);
      if (tmp_err != NULL) {
        g_string_append(message, _("I could not guess if it is newer, so you have to do it manually.\n"));
      } else {
        g_string_append(message, _("The backup copy is newer than the schematic, so it seems you should load it instead of the original file.\n"));
      }
      g_string_append (message, _("Gschem usually makes backup copies automatically, and this situation happens when it crashed or it was forced to exit abruptly.\n"));
      if (toplevel->page_current->load_newer_backup_func == NULL) {
        g_warning (message->str);
        g_warning (_("\nRun gschem and correct the situation.\n\n"));
      } else {
        /* Ask the user if load the backup or the original file */
        if (toplevel->page_current->load_newer_backup_func
            (toplevel, message)) {
          /* Load the backup file */
          load_backup_file = 1;
        }
      }
      g_string_free (message, TRUE);
    }
    if (tmp_err != NULL) g_error_free (tmp_err);
  }

  /* Now that we have set the current directory and read
   * the RC file, it's time to read in the file. */
  if (load_backup_file == 1) {
    /* Load the backup file */
    toplevel->page_current->object_tail = (OBJECT *)
    o_read(toplevel, toplevel->page_current->object_tail,
	   backup_filename, err);
  } else {
    /* Load the original file */
    toplevel->page_current->object_tail = (OBJECT *)
    o_read(toplevel, toplevel->page_current->object_tail,
	   full_filename, err);
  }

  if (toplevel->page_current->object_tail != NULL) {
    opened = TRUE;
  } else {
    /* Failed to open page */
    opened = FALSE;	 
  }

  toplevel->page_current->object_tail
    = (OBJECT *) return_tail(toplevel->page_current->object_head);

  /* make sure you init net_consolide to false (default) in all */
  /* programs */
  if (toplevel->net_consolidate == TRUE) {
    o_net_consolidate(toplevel);
  }

  if (load_backup_file == 0) {
    /* If it's not the backup file */
    toplevel->page_current->CHANGED=0; /* added 4/7/98 */
  }
  else {
    /* We are loading the backup file, so gschem should ask
       the user if save it or not when closing the page. */
    toplevel->page_current->CHANGED=1;
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

  return opened;
}

/*! \brief Closes the schematic file
 *  \par Function Description
 *  Does nothing
 *
 *  \param [in,out] toplevel  The TOPLEVEL object with schematic to be closed.
 */
void f_close(TOPLEVEL *toplevel)
{

}

/*! \brief Save schematic file and close
 *  \par Function Description
 *  This function will save the current schematic file before closing it.
 *  It also deletes the page_current item in the TOPLEVEL structure.
 *
 *  \param [in,out] toplevel  The TOPLEVEL object containing the schematic.
 *  \param [in]      filename  The file name to save the schematic to.
 */
void f_save_close(TOPLEVEL *toplevel, char *filename)
{
  o_save(toplevel, filename);
  s_page_delete (toplevel, toplevel->page_current);
}

/*! \brief Save the schematic file
 *  \par Function Description
 *  This function saves the current schematic file in the toplevel object.
 *
 *  \param [in,out] toplevel  The TOPLEVEL object containing the schematic.
 *  \param [in]      filename  The file name to save the schematic to.
 *  \return 1 on success, 0 on failure.
 */
int f_save(TOPLEVEL *toplevel, const char *filename)
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
    s_log_message (_("Can't get the real filename of %s."), filename);
    return 0;
  }
  
  /* Get the directory in which the real filename lives */
  dirname = g_path_get_dirname (real_filename);
  only_filename = g_path_get_basename(real_filename);  

  /* Do a backup if it's not an undo file backup and it was never saved. */
  if (toplevel->page_current->saved_since_first_loaded == 0) {
    if ( (g_file_test (real_filename, G_FILE_TEST_EXISTS)) && 
	 (!g_file_test(real_filename, G_FILE_TEST_IS_DIR)) )
    {
      backup_filename = g_strdup_printf("%s%c%s~", dirname, 
					G_DIR_SEPARATOR, only_filename);

      /* Make the backup file read-write before saving a new one */
      if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) && 
	   (! g_file_test (backup_filename, G_FILE_TEST_IS_DIR))) {
	if (chmod(backup_filename, S_IREAD|S_IWRITE) != 0) {
	  s_log_message (_("Could NOT set previous backup file [%s] read-write\n"),
			 backup_filename);
	}
      }
	
      if (rename(real_filename, backup_filename) != 0) {
	s_log_message (_("Can't save backup file: %s."), backup_filename);
      }
      else {
	/* Make the backup file readonly so a 'rm *' command will ask 
	   the user before deleting it */
	saved_umask = umask(0);
	mask = (S_IWRITE|S_IWGRP|S_IEXEC|S_IXGRP|S_IXOTH);
	mask = (~mask)&0777;
	mask &= ((~saved_umask) & 0777);
	if (chmod(backup_filename, mask) != 0) {
	  s_log_message (_("Could NOT set backup file [%s] readonly\n"),
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
  
  if (o_save(toplevel, real_filename)) {

    toplevel->page_current->saved_since_first_loaded = 1;

    /* Reset the last saved timer */
    g_get_current_time (&toplevel->page_current->last_load_or_save_time);
    toplevel->page_current->ops_since_last_backup = 0;
    toplevel->page_current->do_autosave_backup = 0;

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

/*! \brief Builds an absolute pathname.
 *  \par Function Description
 *  This function derives an absolute pathname for the pathname
 *  pointed to by \a name with '.' and '..' resolved. It does not
 *  resolve symbolic links.
 *
 *  It returns NULL and sets the \a error (if not NULL) if it failed
 *  to build the pathname or the pathname does not exists.
 *
 *  \note
 *  The code for this function is derived from the realpath() of
 *  the GNU C Library (Copyright (C) 1996-2002, 2004, 2005, 2006 Free
 *  Software Foundation, Inc. / LGPL 2.1 or later).
 *
 *  The part for the resolution of symbolic links has been discarded
 *  and it has been adapted for glib and for use on Windows.
 *
 *  \param [in]     name  A character string containing the pathname
 *                        to resolve.
 *  \param [in,out] error Return location for a GError, or NULL.
 *  \return A newly-allocated string with the resolved absolute
 *  pathname on success, NULL otherwise.
 */
gchar *f_normalize_filename (const gchar *name, GError **error)
{
#ifdef G_OS_WIN32
#define ROOT_MARKER_LEN 3
#else
#define ROOT_MARKER_LEN 1
#endif /* G_OS_WIN32 */   
  GString *rpath;
  const char *start, *end;
  
  if (name == NULL) {
    g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_INVAL,
                 g_strerror (EINVAL));
    return NULL;
  }

  if (*name == '\0') {
    g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_NOENT,
                 g_strerror (ENOENT));
    return NULL;
  }

  rpath = g_string_sized_new (strlen (name));
  
  /* if relative path, prepend current dir */
  if (!g_path_is_absolute (name)) {
    gchar *cwd = g_get_current_dir ();
    g_string_append (rpath, cwd);
    g_free (cwd);
    if (!G_IS_DIR_SEPARATOR (rpath->str[rpath->len - 1])) {
      g_string_append_c (rpath, G_DIR_SEPARATOR);
    }
  } else {
    g_string_append_len (rpath, name, ROOT_MARKER_LEN);
    /* move to first path separator */
    name += ROOT_MARKER_LEN - 1;
  }

  for (start = end = name; *start != '\0'; start = end) {
    /* skip sequence of multiple path-separators */
    while (G_IS_DIR_SEPARATOR (*start)) {
      ++start;
    }

    /* find end of path component */
    for (end = start; *end != '\0' && !G_IS_DIR_SEPARATOR (*end); ++end);

    if (end - start == 0) {
      break;
    } else if (end - start == 1 && start[0] == '.') {
      /* nothing */;
    } else if (end - start == 2 && start[0] == '.' && start[1] == '.') {
      /* back up to previous component, ignore if at root already.  */
      if (rpath->len > ROOT_MARKER_LEN) {
        while (!G_IS_DIR_SEPARATOR (rpath->str[(--rpath->len) - 1]));
        g_string_set_size (rpath, rpath->len);
      }
    } else {
      /* path component, copy to new path */
      if (!G_IS_DIR_SEPARATOR (rpath->str[rpath->len - 1])) {
        g_string_append_c (rpath, G_DIR_SEPARATOR);
      }

      g_string_append_len (rpath, start, end - start);

      if (!g_file_test (rpath->str, G_FILE_TEST_EXISTS)) {
        g_set_error (error,G_FILE_ERROR, G_FILE_ERROR_NOENT,
                     g_strerror (ENOENT));
        goto error;
      } else if (!g_file_test (rpath->str, G_FILE_TEST_IS_DIR) &&
                 *end != '\0') {
        g_set_error (error,G_FILE_ERROR, G_FILE_ERROR_NOTDIR,
                     g_strerror (ENOTDIR));
        goto error;
      }
    }
  }
  
  if (G_IS_DIR_SEPARATOR (rpath->str[rpath->len - 1]) &&
      rpath->len > ROOT_MARKER_LEN) {
    g_string_set_size (rpath, rpath->len - 1);
  }
  
  return g_string_free (rpath, FALSE);

  error:
  g_string_free (rpath, TRUE);
  return NULL;
#undef ROOT_MARKER_LEN
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
  
#ifdef __MINGW32__
  /* MinGW does not have symlinks */
  return followed_filename;
#else

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
	s_log_message(_("Could not read symbolic link information for %s"), followed_filename);
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
  
  s_log_message(_("The file has too many symbolic links."));
  
  return NULL;
#endif /* __MINGW32__ */
}
