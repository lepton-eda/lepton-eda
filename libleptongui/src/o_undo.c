/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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
#include <config.h>

#include <stdio.h>
#include <math.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#include <sys/stat.h>
#endif

#include "schematic.h"

static int undo_file_index=0;

static char* tmp_path = NULL;


/* this is additional number of levels (or history) at which point the */
/* undo stack will be trimmed, it's used a safety to prevent running out */
/* of entries to free */
#define UNDO_PADDING  5


/*! \brief Return current undo file index for backup names.
 *
 * \return The current file index.
 */
int
schematic_undo_get_file_index ()
{
  return undo_file_index;
}


/*! \brief Return temp directory for undo.
 *
 * \return The temp directory name.
 */
char*
schematic_undo_get_tmp_path ()
{
  return tmp_path;
}


/*! \brief Set temp directory path for undo.
 *
 * \par Function Description
 *
 * The function sets temp directory for undo to \a path.  Use
 * NULL to unset the value.
 *
 * \param [in] path A new temp path value.
 */
void
schematic_undo_set_tmp_path (char* path)
{
  g_free (tmp_path);
  tmp_path = g_strdup (path);
}


/*! \brief Return undo backup file name by index.
 *
 * \par Function Description
 *
 * This function returns an undo backup copy file name by given \a
 * index.  The name is absolute and is formed from the path to a
 * temporary directory, program file name, the PID of the running
 * program, and the index.
 *
 * \param [in] index An integer to create file name for.
 * \return A new undo backup file name string.
 *
 * \note
 * Caller must g_free returned character string.
 */
char*
schematic_undo_index_to_filename (int index)
{
  char *filename =
    g_strdup_printf ("%s%clepton-schematic.save%d_%d.sch",
                     schematic_undo_get_tmp_path (),
                     G_DIR_SEPARATOR,
                     getpid (),
                     index);
  return filename;
}


/*! \brief Do autosave on all pages that are marked.
 *  \par Function Description
 *  Looks for pages with the do_autosave_backup flag activated and
 *  autosaves them.
 *
 *  \param [in] w_current  The #SchematicWindow object to search for autosave's.
 */
static void
o_autosave_backups (SchematicWindow *w_current)
{
  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
  GList *iter;
  LeptonPage *p_save, *p_current;
  gchar *backup_filename;
  gchar *real_filename;
  gchar *only_filename;
  gchar *dirname;
  mode_t saved_umask;
  mode_t mask;
  struct stat st;

  /* save current page */
  p_save = schematic_window_get_active_page (w_current);
  LeptonPageList *pages = schematic_window_get_pages (w_current);

  for ( iter = lepton_list_get_glist (pages);
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (LeptonPage *)iter->data;

    if (p_current->do_autosave_backup == 0) {
      continue;
    }
    if (p_current->ops_since_last_backup != 0) {
      /* make p_current the current page of toplevel */
      lepton_toplevel_goto_page (toplevel, p_current);
      schematic_window_page_changed (w_current);

      /* Get the real filename and file permissions */
      real_filename = follow_symlinks (lepton_page_get_filename (p_current), NULL);

      if (real_filename == NULL) {
        g_message ("o_autosave_backups: ");
        g_message (_("Can't get the real filename of %1$s."),
                   lepton_page_get_filename (p_current));
      } else {
        /* Get the directory in which the real filename lives */
        dirname = g_path_get_dirname (real_filename);
        only_filename = g_path_get_basename(real_filename);

        backup_filename = g_strdup_printf("%s%c" AUTOSAVE_BACKUP_FILENAME_STRING,
                                          dirname, G_DIR_SEPARATOR, only_filename);

        /* If there is not an existing file with that name, compute the
         * permissions and uid/gid that we will use for the newly-created file.
         */

        if (stat (real_filename, &st) != 0) {
#if defined(HAVE_GETUID) && defined(HAVE_GETGID)
            struct stat dir_st;
            int result;
#endif

            /* Use default permissions */
            saved_umask = umask(0);
            st.st_mode = 0666 & ~saved_umask;
            umask(saved_umask);
#if defined(HAVE_GETUID) && defined(HAVE_GETGID)
            st.st_uid = getuid ();

            result = stat (dirname, &dir_st);

            if (result == 0 && (dir_st.st_mode & S_ISGID))
              st.st_gid = dir_st.st_gid;
            else
              st.st_gid = getgid ();
#endif
          }
        g_free (dirname);
        g_free (only_filename);
        g_free (real_filename);

        /* Make the backup file writable before saving a new one */
        if ( g_file_test (backup_filename, G_FILE_TEST_EXISTS) &&
             (! g_file_test (backup_filename, G_FILE_TEST_IS_DIR))) {
          saved_umask = umask(0);
          if (chmod(backup_filename, (S_IWRITE|S_IWGRP|S_IWOTH) &
                    ((~saved_umask) & 0777)) != 0) {
            g_message (_("Could NOT set previous backup file [%1$s] read-write"),
                       backup_filename);
          }
          umask(saved_umask);
        }

        if (o_save (lepton_page_objects (schematic_window_get_active_page (w_current)),
                    backup_filename, NULL)) {

          p_current->ops_since_last_backup = 0;
          p_current->do_autosave_backup = 0;

          /* Make the backup file readonly so a 'rm *' command will ask
             the user before deleting it */
          saved_umask = umask(0);
          mask = (S_IWRITE|S_IWGRP|S_IEXEC|S_IXGRP|S_IXOTH);
          mask = (~mask)&0777;
          mask &= ((~saved_umask) & 0777);
          if (chmod(backup_filename,mask) != 0) {
            g_message (_("Could NOT set backup file [%1$s] readonly"),
                       backup_filename);
          }
          umask(saved_umask);
        } else {
          g_message (_("Could NOT save backup file [%1$s]"),
                     backup_filename);
        }
        g_free (backup_filename);
      }
    }
  }
  /* restore current page */
  lepton_toplevel_goto_page (toplevel, p_save);
  schematic_window_page_changed (w_current);
}


/*! \brief Save current state of page onto the undo stack.
 *  \par Function Description
 *  The function saves the objects and/or viewport of the current
 *  page onto the head of the undo stack to possibly restore it
 *  later.
 *
 * \param [in] w_current The #SchematicWindow object the current
 *                       page belongs to.
 * \param [in] page The \c LeptonPage instance.
 * \param [in] only_viewport If only the current viewport size have
 *                           to be stored.
 */
void
o_undo_savestate (SchematicWindow *w_current,
                  LeptonPage *page,
                  gboolean only_viewport)
{
  LeptonToplevel *toplevel = schematic_window_get_toplevel (w_current);
  char *filename = NULL;
  GList *object_list = NULL;
  int levels;
  LeptonUndo *u_current;
  LeptonUndo *u_current_next;

  SchematicCanvas *view = schematic_window_get_current_canvas (w_current);
  g_return_if_fail (view != NULL);

  g_return_if_fail (page != NULL);

  SchematicViewport *geometry = schematic_canvas_get_viewport (view);

  /* save autosave backups if necessary */
  o_autosave_backups(w_current);

  if (w_current->undo_control == FALSE) {
    return;
  }

  if (!only_viewport)
  {
    /* Increment the number of operations since last backup if
       auto-save is enabled */
    if (toplevel->auto_save_interval != 0) {
      page->ops_since_last_backup++;
    }

    /* HACK */
    /* Before we save the undo state, consolidate nets as necessary */

    /* This is where the net consolidation call would have been
     * triggered before it was removed from o_save_buffer().
     */
    lepton_net_object_consolidate (page);
  }

  if (w_current->undo_type == UNDO_DISK && !only_viewport)
  {
    filename = schematic_undo_index_to_filename (undo_file_index++);

    /* Changed from f_save to o_save when adding backup copy creation. */
    /* f_save manages the creaton of backup copies.
       This way, f_save is called only when saving a file, and not when
       saving an undo backup copy */
    o_save (lepton_page_objects (page), filename, NULL);

  }
  else if (w_current->undo_type == UNDO_MEMORY && !only_viewport)
  {
    object_list = lepton_object_list_copy (lepton_page_objects (page),
                                           object_list);
  }

  /* Clear Anything above current */
  if (page->undo_current) {
    lepton_undo_remove_rest (page->undo_current->next);
    page->undo_current->next = NULL;
  } else { /* undo current is NULL */
    lepton_undo_remove_rest (page->undo_bottom);
    page->undo_bottom = NULL;
  }

  page->undo_tos = page->undo_current;

  if (geometry != NULL) {
    page->undo_tos =
      lepton_undo_add (page->undo_tos,
                       only_viewport,
                       filename,
                       object_list,
                       (geometry->viewport_left + geometry->viewport_right) / 2,
                       (geometry->viewport_top + geometry->viewport_bottom) / 2,
                       /* scale */
                       MAX (((double) abs (geometry->viewport_right - geometry->viewport_left) / geometry->screen_width),
                            ((double) abs (geometry->viewport_top - geometry->viewport_bottom) / geometry->screen_height)),
                       page->page_control,
                       page->up);
  } else {
    page->undo_tos = lepton_undo_add (page->undo_tos,
                                      only_viewport,
                                      filename,
                                      object_list,
                                      0, /* center x */
                                      0, /* center y */
                                      0, /* scale */
                                      page->page_control,
                                      page->up);
  }

  page->undo_current =
      page->undo_tos;

  if (page->undo_bottom == NULL) {
    page->undo_bottom =
        page->undo_tos;
  }

#if DEBUG
  printf("\n\n---Undo----\n");
  lepton_undo_print_all (page->undo_bottom);
  printf("BOTTOM: %s\n", page->undo_bottom->filename);
  printf("TOS: %s\n", page->undo_tos->filename);
  printf("CURRENT: %s\n", page->undo_current->filename);
  printf("----\n");
#endif

  g_free(filename);

  /* Now go through and see if we need to free/remove some undo levels */
  /* so we stay within the limits */

  /* only check history every 10 undo savestates */
  if (undo_file_index % 10) {
    return;
  }

  levels = lepton_undo_levels (page->undo_bottom);

#if DEBUG
  printf("levels: %d\n", levels);
#endif

  if (levels >= w_current->undo_levels + UNDO_PADDING) {
    levels = levels - w_current->undo_levels;

#if DEBUG
    printf("Trimming: %d levels\n", levels);
#endif

    u_current = page->undo_bottom;

    while (levels > 0) {
      /* Because we use a pad you are always guaranteed to never */
      /* exhaust the list */
      g_assert (u_current != NULL);

      u_current_next = u_current->next;

      if (u_current->filename) {
#if DEBUG
        printf("Freeing: %s\n", u_current->filename);
#endif
        unlink(u_current->filename);
        g_free(u_current->filename);
      }

      if (u_current->object_list) {
        lepton_object_list_delete (u_current->object_list);
        u_current->object_list = NULL;
      }

      u_current->next = NULL;
      u_current->prev = NULL;
      g_free(u_current);

      u_current = u_current_next;
      levels--;
    }

    g_assert (u_current != NULL);
    u_current->prev = NULL;
    page->undo_bottom = u_current;

#if DEBUG
    printf("New current is: %s\n", u_current->filename);
#endif
  }

#if DEBUG
  printf("\n\n---Undo----\n");
  lepton_undo_print_all (page->undo_bottom);
  printf("BOTTOM: %s\n", page->undo_bottom->filename);
  printf("TOS: %s\n", page->undo_tos->filename);
  printf("CURRENT: %s\n", page->undo_current->filename);
  printf("----\n");
#endif

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 */
void
o_undo_savestate_old (SchematicWindow *w_current)
{
  SchematicCanvas *page_view = schematic_window_get_current_canvas (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = schematic_canvas_get_page (page_view);

  o_undo_savestate (w_current, page, FALSE);
}


/*! \brief Save viewport undo information.
 *
 * \par Function Description
 *
 * This function saves undo information marking the record as a
 * viewport only one.
 *
 * \param [in] w_current The current schematic window instance.
 */
void
o_undo_savestate_viewport (SchematicWindow *w_current)
{
  g_return_if_fail (w_current != NULL);

  LeptonPage *page = schematic_window_get_active_page (w_current);

  o_undo_savestate (w_current, page, TRUE);
}
