/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2023 Lepton EDA Contributors
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
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <libgen.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "gschem.h"


/*! \brief Rotate all objects in list.
 *  \par Function Description
 *  Given an object <B>list</B>, and the center of rotation
 *  (<B>centerx</B>,<B>centery</B>, this function traverses all the selection
 *  list, rotating each object through angle <B>angle</B>.
 *  The list contains a given object and all its attributes
 *  (refdes, pinname, pinlabel, ...).
 *  There is a second pass to run the rotate hooks of non-simple objects,
 *  like pin or component objects, for example.
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] centerx    Center x coordinate of rotation.
 *  \param [in] centery    Center y coordinate of rotation.
 *  \param [in] angle      Angle to rotate the objects through.
 *  \param [in] list       The list of objects to rotate.
 */
void o_rotate_world_update(GschemToplevel *w_current,
                           int centerx, int centery, int angle, GList *list)
{
  LeptonObject *o_current;
  GList *o_iter;

  /* this is okay if you just hit rotate and have nothing selected */
  if (list == NULL) {
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
    return;
  }

  o_invalidate_glist (w_current, list);

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = (LeptonObject*) o_iter->data;

    s_conn_remove_object_connections (o_current);
  }

  lepton_object_list_rotate (list, centerx, centery, angle);

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = (LeptonObject*) o_iter->data;

    s_conn_update_object (o_current->page, o_current);
  }

  o_invalidate_glist (w_current, list);

  /* Run rotate-objects-hook */
  g_run_hook_object_list (w_current, "rotate-objects-hook", list);

  /* Don't save the undo state if we are inside an action */
  /* This is useful when rotating the selection while moving, for example */
  schematic_window_active_page_changed (w_current);
  if (!w_current->inside_action) {
    o_undo_savestate_old(w_current, UNDO_ALL);
  }

  if (schematic_window_get_action_mode (w_current) == ROTATEMODE)
  {
    i_set_state(w_current, SELECT);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_mirror_world_update(GschemToplevel *w_current, int centerx, int centery, GList *list)
{
  LeptonObject *o_current;
  GList *o_iter;

  if (list == NULL) {
    i_action_stop (w_current);
    i_set_state(w_current, SELECT);
    return;
  }

  o_invalidate_glist (w_current, list);

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = (LeptonObject*) o_iter->data;

    s_conn_remove_object_connections (o_current);
  }

  lepton_object_list_mirror (list, centerx, centery);

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = (LeptonObject*) o_iter->data;

    s_conn_update_object (o_current->page, o_current);
  }

  o_invalidate_glist (w_current, list);

  /* Run mirror-objects-hook */
  g_run_hook_object_list (w_current, "mirror-objects-hook", list);

  schematic_window_active_page_changed (w_current);
  o_undo_savestate_old(w_current, UNDO_ALL);

  if (schematic_window_get_action_mode (w_current) == MIRRORMODE)
  {
    i_set_state(w_current, SELECT);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_hidden_lowlevel (GschemToplevel *w_current,
                                  const GList *o_list)
{
  LeptonObject *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;
    if (lepton_object_is_text (o_current) &&
        !lepton_text_object_is_visible (o_current))
    {

      /* don't toggle the visibility flag */
      lepton_text_object_recreate (o_current);
    }

    if (lepton_object_is_component (o_current))
    {
      GList *primitives = lepton_component_object_get_contents (o_current);
      o_edit_show_hidden_lowlevel (w_current, primitives);
    }

    iter = g_list_next (iter);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_hidden (GschemToplevel *w_current, const GList *o_list)
{
  /* this function just shows the hidden text, but doesn't toggle it */
  /* this function does not change the CHANGED bit, no real changes are */
  /* made to the schematic */

  /* toggle show_hidden_text variable, which when it is true */
  /* means that hidden text IS drawn */
  g_signal_emit_by_name (gschem_toplevel_get_current_page_view (w_current),
                         "toggle-hidden-text");
  i_show_state(w_current, NULL); /* update screen status */

  o_edit_show_hidden_lowlevel(w_current, o_list);
  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));

  if (gschem_toplevel_get_show_hidden_text (w_current)) {
    g_message (_("Hidden text is now visible"));
  } else {
    g_message (_("Hidden text is now invisible"));
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_hide_specific_text (GschemToplevel *w_current,
                                const GList *o_list,
                                const char *stext)
{
  LeptonObject *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;

    if (lepton_object_is_text (o_current))
    {
      const gchar *str = lepton_text_object_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (lepton_text_object_is_visible (o_current)) {
          lepton_text_object_set_visibility (o_current, INVISIBLE);
          lepton_text_object_recreate (o_current);

          schematic_window_active_page_changed (w_current);
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate_old(w_current, UNDO_ALL);
  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_specific_text (GschemToplevel *w_current,
                                const GList *o_list,
                                const char *stext)
{
  LeptonObject *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;

    if (lepton_object_is_text (o_current))
    {
      const gchar *str = lepton_text_object_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (!lepton_text_object_is_visible (o_current)) {
          lepton_text_object_set_visibility (o_current, VISIBLE);
          lepton_text_object_recreate (o_current);

          schematic_window_active_page_changed (w_current);
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate_old(w_current, UNDO_ALL);
}


/*! \brief Do autosave on all pages that are marked.
 *  \par Function Description
 *  Looks for pages with the do_autosave_backup flag activated and
 *  autosaves them.
 *
 *  \param [in] w_current  The GschemToplevel object to search for autosave's.
 */
void o_autosave_backups(GschemToplevel *w_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
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

  for ( iter = lepton_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (LeptonPage *)iter->data;

    if (p_current->do_autosave_backup == 0) {
      continue;
    }
    if (p_current->ops_since_last_backup != 0) {
      /* make p_current the current page of toplevel */
      lepton_toplevel_goto_page (toplevel, p_current);
      gschem_toplevel_page_changed (w_current);

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
  gschem_toplevel_page_changed (w_current);
}
