/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2014 gEDA Contributors
 * Copyright (C) 2017-2022 Lepton EDA Contributors
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

/*! \file f_basic.c
 *  \brief file related functions
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

# if defined (_WIN32)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h> /* for GetFullPathName */
# endif

#include "liblepton_priv.h"

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
  GFileError g_errcode = (GFileError) 0;
  struct stat file_stat, auto_stat;

  auto_filename = f_get_autosave_filename (filename);
  if (stat (filename, &file_stat) != 0) {
    file_err = errno;
  }
  if (stat (auto_filename, &auto_stat) != 0) {
    auto_err = errno;
  }

  /* A valid use of goto! (checks for raptors) */
  if (auto_err == ENOENT) {
    /* The autosave file does not exist. */
    result = FALSE;
    goto check_autosave_finish;
  }
  if (auto_err) {
    g_errcode = g_file_error_from_errno (auto_err);
    g_set_error (err, G_FILE_ERROR, g_errcode,
                 _("Failed to stat [%1$s]: %2$s"),
                 auto_filename, g_strerror (auto_err));
    result = TRUE;
    goto check_autosave_finish;
  }
  if (file_err == ENOENT) {
    /* The autosave file exists, but the actual file does not. */
    result = TRUE;
    goto check_autosave_finish;
  }
  if (file_err) {
    g_errcode = g_file_error_from_errno (file_err);
    g_set_error (err, G_FILE_ERROR, g_errcode,
                 _("Failed to stat [%1$s]: %2$s"),
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

GString*
f_backup_message (gchar *backup_filename,
                  gboolean stat_error)
{
  GString *message = g_string_new ("");

  g_string_append_printf (message,
                          _("\nWARNING: Found an autosave backup file:\n  %1$s.\n\n"),
                          backup_filename);

  if (stat_error) {
    g_string_append (message,
                     _("I could not guess if it is newer, so you have to do it manually.\n"));
  } else {
    g_string_append (message,
                     _("The backup copy is newer than the schematic, so it seems you should load it instead of the original file.\n"));
  }

  g_string_append (message,
                   _("lepton-schematic usually makes backup copies automatically, and this situation happens when it crashed or it was forced to exit abruptly.\n"));

  /* The user must g_string_free the message. */
  return message;
}


/*! \brief Opens the schematic file with fine-grained control over behaviour.
 *  \par Function Description
 *  Opens the schematic file and carries out a number of actions
 *  depending on the \a flags set.  If #F_OPEN_RC is set, executes RC
 *  files found in the target directory.  If #F_OPEN_CHECK_BACKUP is
 *  set, warns user if a backup is found for the file being loaded,
 *  and possibly prompts user for whether to load the backup instead.
 *  If #F_OPEN_RESTORE_CWD is set, does not change the working
 *  directory to that of the file being loaded.
 *
 *  \param [in,out] toplevel  The LeptonToplevel object to load the schematic into.
 *  \param [in]     filename   A character string containing the file name
 *                             to open.
 *  \param [in]     flags      Combination of #FOpenFlags values.
 *  \param [in,out] err  #GError structure for error reporting, or
 *                       NULL to disable error reporting
 *
 *  \return 0 on failure, 1 on success.
 */
int
f_open_flags (LeptonToplevel *toplevel,
              LeptonPage *page,
              const gchar *filename,
              const gint flags,
              GError **err)
{
  int opened=FALSE;
  char *full_filename = NULL;
  char *file_directory = NULL;
  char *saved_cwd = NULL;
  char *backup_filename = NULL;
  gboolean load_backup_file = FALSE;
  GError *tmp_err = NULL;

  /* has the head been freed yet? */
  /* probably not hack LeptonPage */

  /* Cache the cwd so we can restore it later. */
  if (flags & F_OPEN_RESTORE_CWD) {
    saved_cwd = g_get_current_dir();
  }

  /* get full, absolute path to file */
  full_filename = f_normalize_filename (filename, &tmp_err);
  if (full_filename == NULL) {
    g_set_error (err, G_FILE_ERROR, tmp_err->code,
                 _("Cannot find file %1$s: %2$s"),
                 filename, tmp_err->message);
    g_error_free(tmp_err);
    return 0;
  }

  /* write full, absolute filename into page->page_filename */
  lepton_page_set_filename (page, full_filename);

  /* Before we open the page, let's load the corresponding gafrc. */
  /* First cd into file's directory. */
  file_directory = g_path_get_dirname (full_filename);

  if (file_directory) {
    if (chdir (file_directory)) {
      /* Error occurred with chdir */
      /* FIXME[2017-02-21] Libraries should not be changing the
       * current working directory.  If it is not possible to avoid a
       * chdir() call, then the error needs to be handled and/or
       * reported. */
    }
  }

  /* Now open RC and process file */
  if (flags & F_OPEN_RC) {
    g_rc_parse_local (toplevel, "gafrc", file_directory, &tmp_err);
    if (tmp_err != NULL) {
      /* RC files are allowed to be missing or skipped; check for
       * this. */
      if (!g_error_matches (tmp_err, G_FILE_ERROR, G_FILE_ERROR_NOENT) &&
          !g_error_matches (tmp_err, EDA_ERROR, EDA_ERROR_RC_TWICE)) {
        g_message ("%s", tmp_err->message);
      }
      g_error_free (tmp_err);
      tmp_err = NULL;
    }
  }

  g_free (file_directory);

  if (flags & F_OPEN_FORCE_BACKUP) {
    backup_filename = f_get_autosave_filename (full_filename);
    load_backup_file = TRUE;
  }
  else if (flags & F_OPEN_CHECK_BACKUP) {
    /* Check if there is a newer autosave backup file */
    gboolean active_backup = f_has_active_autosave (full_filename, &tmp_err);
    backup_filename = f_get_autosave_filename (full_filename);

    gboolean stat_error = (tmp_err != NULL);
    if (stat_error) {
      g_warning ("%s\n", tmp_err->message);
      g_error_free (tmp_err);
    }

    if (active_backup) {
      GString *message =
        f_backup_message (backup_filename, stat_error);
      g_warning ("%s", message->str);
      g_warning (_("\nRun lepton-schematic and correct the situation.\n\n"));
      g_string_free (message, TRUE);
      /* load_backup_file = FALSE; */
    }
  }

  /* Now that we have set the current directory and read
   * the RC file, it's time to read in the file. */
  if (load_backup_file) {
    /* Load the backup file */
    o_read (page, backup_filename, &tmp_err);
  } else {
    /* Load the original file */
    o_read (page, full_filename, &tmp_err);
  }

  if (tmp_err == NULL)
    opened = TRUE;
  else
    g_propagate_error (err, tmp_err);

  if (!load_backup_file) {
    /* If it's not the backup file */
    lepton_page_set_changed (page, 0);
  } else {
    /* We are loading the backup file, so lepton should ask
       the user if save it or not when closing the page. */
    lepton_page_set_changed (page, 1);
  }

  g_free(full_filename);
  g_free (backup_filename);

  /* Reset the directory to the value it had when f_open was
   * called. */
  if (flags & F_OPEN_RESTORE_CWD) {
    if (chdir (saved_cwd)) {
      /* Error occurred with chdir */
      /* FIXME[2017-02-21] Libraries should not be changing the
       * current working directory.  If it is not possible to avoid a
       * chdir() call, then the error needs to be handled and/or
       * reported. */
    }
    g_free(saved_cwd);
  }

  return opened;
}


/*! \brief Save the schematic file
 *  \par Function Description
 *  This function saves the current schematic file in the toplevel object.
 *
 *  \bug g_access introduces a race condition in certain cases, but
 *  solves bug #698565 in the normal use-case
 *
 *  \param [in]      filename  The file name to save the schematic to.
 *  \param [in,out] err       #GError structure for error reporting, or
 *                            NULL to disable error reporting
 *  \return 1 on success, 0 on failure.
 */
int
f_save (LeptonPage *page,
        const char *filename,
        GError **err)
{
  gchar *backup_filename;
  gchar *real_filename;
  gchar *only_filename;
  gchar *dirname;
  mode_t saved_umask, mask;
  struct stat st;
  GError *tmp_err = NULL;
  gboolean make_backup_files;

  cfg_read_bool ("schematic.backup", "create-files",
                 default_make_backup_files, &make_backup_files);

  /* Get the real filename and file permissions */
  real_filename = follow_symlinks (filename, &tmp_err);

  if (real_filename == NULL) {
    g_set_error (err, tmp_err->domain, tmp_err->code,
                 _("Can't get the real filename of %1$s: %2$s"),
                 filename,
                 tmp_err->message);
    return 0;
  }

  /* Check to see if filename is writable */
  if (g_file_test(filename, G_FILE_TEST_EXISTS) &&
      g_access(filename, W_OK) != 0) {
    g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_PERM,
                 _("File %1$s is read-only"), filename);
    return 0;
  }

  /* Get the directory in which the real filename lives */
  dirname = g_path_get_dirname (real_filename);
  only_filename = g_path_get_basename(real_filename);

  /* Do a backup if it's not an undo file backup and it was never saved.
   * Only do a backup if backup files are enabled */
  if (page->saved_since_first_loaded == 0 && make_backup_files == TRUE) {
    if ( (g_file_test (real_filename, G_FILE_TEST_EXISTS)) &&
         (!g_file_test(real_filename, G_FILE_TEST_IS_DIR)) )
    {
      backup_filename = g_strdup_printf("%s%c%s~", dirname,
                                        G_DIR_SEPARATOR, only_filename);

      /* Make the backup file read-write before saving a new one */
      if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) &&
           (! g_file_test (backup_filename, G_FILE_TEST_IS_DIR))) {
        if (chmod(backup_filename, S_IREAD|S_IWRITE) != 0) {
          g_message (_("Could NOT set previous backup file [%1$s] read-write."),
                     backup_filename);
        }
      }

      if (rename(real_filename, backup_filename) != 0) {
        g_message (_("Can't save backup file: %1$s."), backup_filename);
      }
      else {
        /* Make the backup file readonly so a 'rm *' command will ask
           the user before deleting it */
        saved_umask = umask(0);
        mask = (S_IWRITE|S_IWGRP|S_IEXEC|S_IXGRP|S_IXOTH);
        mask = (~mask)&0777;
        mask &= ((~saved_umask) & 0777);
        if (chmod(backup_filename, mask) != 0) {
          g_message (_("Could NOT set backup file [%1$s] readonly."),
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

  if (o_save (lepton_page_objects (page), real_filename, &tmp_err))
  {
    page->saved_since_first_loaded = 1;

    /* Reset the last saved timer */
    page->ops_since_last_backup = 0;
    page->do_autosave_backup = 0;

    /* Restore permissions/ownership.  We restore both on a
       best-effort basis; rather than treating failure as an error, we
       just log a warning. */
    if (chmod (real_filename, st.st_mode)) {
      g_warning (_("Failed to restore permissions on '%1$s': %2$s\n"),
                 real_filename, g_strerror (errno));
    }
#ifdef HAVE_CHOWN
    if (chown (real_filename, st.st_uid, st.st_gid)) {
      g_warning (_("Failed to restore ownership on '%1$s': %2$s\n"),
                 real_filename, g_strerror (errno));
    }
#endif

    g_free (real_filename);
    return 1;
  }
  else {
    g_set_error (err, tmp_err->domain, tmp_err->code,
                 _("Could NOT save file: %1$s"), tmp_err->message);
    g_clear_error (&tmp_err);
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
 *  \param [in]     name  A character string containing the pathname
 *                        to resolve.
 *  \param [in,out] error Return location for a GError, or NULL.
 *  \return A newly-allocated string with the resolved absolute
 *  pathname on success, NULL otherwise.
 */
gchar *f_normalize_filename (const gchar *name, GError **error)
{
  GFile *file = g_file_new_for_path (name);

  if (!g_file_query_exists (file, NULL)) {
    g_set_error (error, G_FILE_ERROR, G_FILE_ERROR_NOENT,
                 "%s", g_strerror (ENOENT));
    return NULL;
  }

  gchar *normalized = g_file_get_path (file);
  g_object_unref (file);
  return normalized;
}

/*! \brief Follow symlinks until a real file is found
 *  \par Function Description
 *  Does readlink() recursively until we find a real filename.
 *
 *  \param [in]     filename  The filename to search for.
 *  \param [in,out] err       #GError structure for error reporting,
 *                            or NULL to disable error reporting.
 *  \return The newly-allocated path to real file on success, NULL
 *          otherwise.
 *
 *  \note Originally taken from gedit's source code.
 */
char *follow_symlinks (const gchar *filename, GError **err)
{
  gchar *followed_filename = NULL;
  gint link_count = 0;
  GError *tmp_err = NULL;

  if (filename == NULL) {
    g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_INVAL,
                 "%s", g_strerror (EINVAL));
    return NULL;
  }

  if (strlen (filename) + 1 > MAXPATHLEN) {
    g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_NAMETOOLONG,
                 "%s", g_strerror (ENAMETOOLONG));
    return NULL;
  }

  followed_filename = g_strdup (filename);

#ifdef __MINGW32__
  /* MinGW does not have symlinks */
  return followed_filename;
#else

  while (link_count < MAX_LINK_LEVEL) {
    struct stat st;
    gchar *linkname = NULL;

    if (lstat (followed_filename, &st) != 0) {
      /* We could not access the file, so perhaps it does not
       * exist.  Return this as a real name so that we can
       * attempt to create the file.
       */
      return followed_filename;
    }

    if (!S_ISLNK (st.st_mode)) {
      /* It's not a link, so we've found what we're looking for! */
      return followed_filename;
    }

    link_count++;

    linkname = g_file_read_link (followed_filename, &tmp_err);

    if (linkname == NULL) {
      g_propagate_error(err, tmp_err);
      g_free (followed_filename);
      return NULL;
    }

    /* If the linkname is not an absolute path name, append
     * it to the directory name of the followed filename.  E.g.
     * we may have /foo/bar/baz.lnk -> eek.txt, which really
     * is /foo/bar/eek.txt.
     */

    if (!g_path_is_absolute(linkname)) {
      gchar *dirname = NULL;
      gchar *tmp = NULL;

      dirname = g_path_get_dirname(followed_filename);

      tmp = g_build_filename (dirname, linkname, NULL);
      g_free (followed_filename);
      g_free (dirname);
      g_free (linkname);
      followed_filename = tmp;
    } else {
      g_free (followed_filename);
      followed_filename = linkname;
    }
  }

  /* Too many symlinks */
  g_set_error (err, G_FILE_ERROR, G_FILE_ERROR_LOOP,
               "%s: %s", g_strerror (EMLINK), followed_filename);
  g_free (followed_filename);
  return NULL;

#endif /* __MINGW32__ */
}
