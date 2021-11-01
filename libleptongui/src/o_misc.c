/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

/* break with the tradition here and input a list */
/*! \todo probably should go back and do the same for o_copy o_move
 *  o_delete...
 */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit(GschemToplevel *w_current, GList *list)
{
  LeptonObject *o_current;
  const gchar *str = NULL;

  if (list == NULL) {
    return;
  }

  o_current = (LeptonObject *) list->data;
  if (o_current == NULL) {
    fprintf (stderr, "o_edit: ERROR: Got an unexpected NULL\n");
    exit(-1);
  }

  /* for now deal with only the first item */
  switch (lepton_object_get_type (o_current)) {

    /* also add the ability to multi attrib edit: nets, busses, pins */
    case(OBJ_COMPONENT):
    case(OBJ_NET):
    case(OBJ_PIN):
    case(OBJ_BUS):
    x_multiattrib_open (w_current);
    break;

    case(OBJ_PICTURE):
    picture_change_filename_dialog(w_current);
    break;
    case(OBJ_ARC):
    arc_angle_dialog(w_current, o_current);
    break;
    case(OBJ_TEXT):
      str = lepton_text_object_get_string (o_current);
      if (lepton_object_is_attrib (o_current) &&
        /* attribute editor only accept 1-line values for attribute */
        o_text_num_lines (str) == 1) {
        attrib_edit_dialog(w_current,o_current, FROM_MENU);
    } else {
      text_edit_dialog (w_current);
    }
    break;
  }

  /* has to be more extensive in the future */
  /* some sort of redrawing? */
}

/*! \brief Lock selected objects
 *
 *  \par Function Description
 *  This locks the entire selected list. It does lock components,
 *  but does NOT change the color of primatives of the components.
 *
 *  \note This function cannot be called recursively.
 *
 *  \param w_current  The toplevel environment.
 */
void o_lock(GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);
  g_return_if_fail (w_current->toplevel->page_current != NULL);

  LeptonPage*  page = w_current->toplevel->page_current;
  GList* objs = lepton_list_get_glist (page->selection_list);

  /* lock selected objects:
  */
  LeptonObject* obj = NULL;
  for (GList* iter = objs; iter != NULL; iter = g_list_next (iter))
  {
    obj = (LeptonObject*) iter->data;
    lepton_object_set_selectable (obj, FALSE);

    /* for objects with attributes, also lock them:
    */
    GList *attribs = lepton_object_get_attribs (obj);
    if (attribs != NULL)
    {
      lepton_object_list_set_selectable (attribs, FALSE);
    }
  }

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);

  if (!w_current->SHIFTKEY)
    o_select_unselect_all(w_current);

  o_undo_savestate_old(w_current, UNDO_ALL);
  i_update_menus(w_current);

  /* refresh view to properly restore attributes' colors:
  */
  GschemPageView* view = gschem_toplevel_get_current_page_view (w_current);
  gschem_page_view_invalidate_all (view);
}

/*! \brief Unlock selected objects
 *
 *  \par Function Description
 *  Unlock objects currenly selected.
 *  Locked objects can be selected with a bounding box.
 *
 *  \note This function cannot be called recursively.
 *
 *  \param w_current  The toplevel environment.
 */
void o_unlock(GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);
  g_return_if_fail (w_current->toplevel->page_current != NULL);

  LeptonPage*  page = w_current->toplevel->page_current;
  GList* objs = lepton_list_get_glist (page->selection_list);

  /* unlock selected objects:
  */
  LeptonObject* obj = NULL;
  for (GList* iter = objs; iter != NULL; iter = g_list_next (iter))
  {
    obj = (LeptonObject*) iter->data;
    lepton_object_set_selectable (obj, TRUE);

    /* for objects with attributes, also unlock them:
    */
    GList *attribs = lepton_object_get_attribs (obj);
    if (attribs != NULL)
    {
      lepton_object_list_set_selectable (attribs, TRUE);
    }
  }

  gschem_toplevel_page_content_changed (w_current, page);
  o_undo_savestate_old(w_current, UNDO_ALL);

  /* refresh view to properly restore attributes' colors:
  */
  GschemPageView* view = gschem_toplevel_get_current_page_view (w_current);
  gschem_page_view_invalidate_all (view);
}

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
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
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
  g_run_hook_object_list (w_current, "%rotate-objects-hook", list);

  /* Don't save the undo state if we are inside an action */
  /* This is useful when rotating the selection while moving, for example */
  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
  if (!w_current->inside_action) {
    o_undo_savestate_old(w_current, UNDO_ALL);
  }

  if (w_current->event_state == ROTATEMODE) {
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
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
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
  g_run_hook_object_list (w_current, "%mirror-objects-hook", list);

  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
  o_undo_savestate_old(w_current, UNDO_ALL);

  if (w_current->event_state == MIRRORMODE) {
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
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
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

          gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
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
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
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

          gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate_old(w_current, UNDO_ALL);
}


/*! \brief Update a component.
 *
 * \par Function Description
 * Updates \a o_current to the latest version of the symbol available
 * in the symbol library, while preserving any attributes set in the
 * current schematic. On success, returns the new LeptonObject which
 * replaces \a o_current on the page; \a o_current is deleted. On
 * failure, returns NULL, and \a o_current is left unchanged.
 *
 * \param [in]     w_current The GschemToplevel object.
 * \param [in,out] o_current The LeptonObject to be updated.
 *
 * \return the new LeptonObject that replaces \a o_current.
 */
LeptonObject *
o_update_component (GschemToplevel *w_current, LeptonObject *o_current)
{
  LeptonToplevel *toplevel = gschem_toplevel_get_toplevel (w_current);
  LeptonObject *o_new;
  LeptonPage *page;
  GList *new_attribs;
  GList *old_attribs;
  GList *iter;
  const CLibSymbol *clib;

  g_return_val_if_fail (lepton_object_is_component (o_current), NULL);

  gchar *basename = lepton_component_object_get_basename (o_current);
  g_return_val_if_fail (basename != NULL, NULL);

  page = lepton_object_get_page (o_current);

  /* Force symbol data to be reloaded from source */
  clib = s_clib_get_symbol_by_name (basename);
  s_clib_symbol_invalidate_data (clib);

  if (clib == NULL) {
    g_message (_("Could not find symbol [%1$s] in library. Update failed."),
               basename);
    return NULL;
  }

  /* Unselect the old object. */
  o_selection_remove (page->selection_list, o_current);

  /* Create new object and set embedded */
  o_new = lepton_component_new (toplevel->page_current,
                                default_color_id(),
                                lepton_component_object_get_x (o_current),
                                lepton_component_object_get_y (o_current),
                                lepton_component_object_get_angle (o_current),
                                lepton_component_object_get_mirror (o_current),
                                clib,
                                basename,
                                1);
  /* Embed new object if the old one is embedded. */
  if (lepton_component_object_get_embedded (o_current)) {
    lepton_component_object_embed (o_new);
  }

  new_attribs = lepton_component_promote_attribs (o_new);

  /* Cull any attributes from new COMPONENT that are already attached to
   * old COMPONENT. Note that the new_attribs list is kept consistent by
   * setting GList data pointers to NULL if their LeptonObjects are
   * culled. At the end, the new_attribs list is updated by removing
   * all list items with NULL data. This is slightly magic, but
   * works. */
  for (iter = new_attribs; iter != NULL; iter = g_list_next (iter)) {
    LeptonObject *attr_new = (LeptonObject*) iter->data;
    gchar *name;
    gchar *value;

    g_assert (lepton_object_is_text (attr_new));

    name = g_strdup (lepton_text_object_get_name (attr_new));

    value = o_attrib_search_attached_attribs_by_name (o_current, name, 0);

    if (value != NULL) {
      lepton_object_delete (attr_new);
      iter->data = NULL;
    }

    g_free (name);
    g_free (value);
  }
  new_attribs = g_list_remove_all (new_attribs, NULL);

  /* Detach attributes from old LeptonObject and attach to new LeptonObject */
  old_attribs = g_list_copy (lepton_object_get_attribs (o_current));
  o_attrib_detach_all (o_current);
  o_attrib_attach_list (old_attribs, o_new, 1);
  g_list_free (old_attribs);

  /* Add new attributes to page */
  s_page_append_list (page, new_attribs);

  /* Update pinnumbers for current slot */
  s_slot_update_object (o_new);

  /* Replace old LeptonObject with new LeptonObject */
  s_page_replace (page, o_current, o_new);
  lepton_object_delete (o_current);

  /* Select new LeptonObject */
  o_selection_add (page->selection_list, o_new);

  /* mark the page as modified */
  gschem_toplevel_page_content_changed (w_current, toplevel->page_current);
  o_undo_savestate_old (w_current, UNDO_ALL);

  return o_new;
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
  p_save = toplevel->page_current;

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

        if (o_save (s_page_objects (toplevel->page_current),
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
