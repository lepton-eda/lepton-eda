/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2011 gEDA Contributors (see ChangeLog for details)
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
  OBJECT *o_current;
  const gchar *str = NULL;

  if (list == NULL) {
    return;
  }

  o_current = (OBJECT *) list->data;
  if (o_current == NULL) {
    fprintf(stderr, _("Got an unexpected NULL in o_edit\n"));
    exit(-1);
  }

  /* for now deal with only the first item */
  switch(o_current->type) {

    /* also add the ability to multi attrib edit: nets, busses, pins */
    case(OBJ_COMPLEX):
    case(OBJ_PLACEHOLDER):
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
      str = geda_text_object_get_string (o_current);
      if (o_attrib_is_attrib (o_current) &&
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

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* This locks the entire selected list.  It does lock components, but does NOT
 * change the color (of primatives of the components) though
 * this cannot be called recursively */
void o_lock(GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);
  g_return_if_fail (w_current->toplevel->page_current != NULL);

  geda_object_list_set_selectable (
      geda_list_get_glist (w_current->toplevel->page_current->selection_list),
      FALSE);

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  if (!w_current->SHIFTKEY) o_select_unselect_all(w_current);
  o_undo_savestate_old(w_current, UNDO_ALL);
  i_update_menus(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* You can unlock something by selecting it with a bounding box... */
/* this will probably change in the future, but for now it's a
   something.. :-) */
/* this cannot be called recursively */
void o_unlock(GschemToplevel *w_current)
{
  g_return_if_fail (w_current != NULL);
  g_return_if_fail (w_current->toplevel != NULL);
  g_return_if_fail (w_current->toplevel->page_current != NULL);

  geda_object_list_set_selectable (
      geda_list_get_glist (w_current->toplevel->page_current->selection_list),
      TRUE);

  gschem_toplevel_page_content_changed (w_current, w_current->toplevel->page_current);
  o_undo_savestate_old(w_current, UNDO_ALL);
}

/*! \brief Rotate all objects in list.
 *  \par Function Description
 *  Given an object <B>list</B>, and the center of rotation
 *  (<B>centerx</B>,<B>centery</B>, this function traverses all the selection
 *  list, rotating each object through angle <B>angle</B>.
 *  The list contains a given object and all its attributes
 *  (refdes, pinname, pinlabel, ...).
 *  There is a second pass to run the rotate hooks of non-simple objects,
 *  like pin or complex objects, for example.
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
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  OBJECT *o_current;
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
    o_current = o_iter->data;

    s_conn_remove_object_connections (toplevel, o_current);
  }

  geda_object_list_rotate ( list, centerx, centery, angle, toplevel );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

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
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  OBJECT *o_current;
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
    o_current = o_iter->data;

    s_conn_remove_object_connections (toplevel, o_current);
  }

  geda_object_list_mirror ( list, centerx, centery, toplevel );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

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
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  OBJECT *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;
    if (o_current->type == OBJ_TEXT && !o_is_visible (toplevel, o_current)) {

      /* don't toggle the visibility flag */
      o_text_recreate (toplevel, o_current);
    }

    if (o_current->type == OBJ_COMPLEX || o_current->type == OBJ_PLACEHOLDER) {
      o_edit_show_hidden_lowlevel(w_current, o_current->complex->prim_objs);
      o_current->w_bounds_valid_for = NULL;
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
  w_current->toplevel->show_hidden_text = !w_current->toplevel->show_hidden_text;
  i_show_state(w_current, NULL); /* update screen status */

  o_edit_show_hidden_lowlevel(w_current, o_list);
  gschem_page_view_invalidate_all (gschem_toplevel_get_current_page_view (w_current));

  if (w_current->toplevel->show_hidden_text) {
    s_log_message(_("Hidden text is now visible\n"));
  } else {
    s_log_message(_("Hidden text is now invisible\n"));
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
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  OBJECT *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (o_current->type == OBJ_TEXT) {
      const gchar *str = geda_text_object_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (o_is_visible (toplevel, o_current)) {
          o_set_visibility (toplevel, o_current, INVISIBLE);
          o_text_recreate(toplevel, o_current);

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
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  OBJECT *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (o_current->type == OBJ_TEXT) {
      const gchar *str = geda_text_object_get_string (o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (!o_is_visible (toplevel, o_current)) {
          o_set_visibility (toplevel, o_current, VISIBLE);
          o_text_recreate(toplevel, o_current);

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
 * current schematic. On success, returns the new OBJECT which
 * replaces \a o_current on the page; \a o_current is deleted. On
 * failure, returns NULL, and \a o_current is left unchanged.
 *
 * \param [in]     w_current The GschemToplevel object.
 * \param [in,out] o_current The OBJECT to be updated.
 *
 * \return the new OBJECT that replaces \a o_current.
 */
OBJECT *
o_update_component (GschemToplevel *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  OBJECT *o_new;
  PAGE *page;
  GList *new_attribs;
  GList *old_attribs;
  GList *iter;
  const CLibSymbol *clib;

  g_return_val_if_fail (o_current != NULL, NULL);
  g_return_val_if_fail (o_current->type == OBJ_COMPLEX, NULL);
  g_return_val_if_fail (o_current->complex_basename != NULL, NULL);

  page = o_get_page (toplevel, o_current);

  /* Force symbol data to be reloaded from source */
  clib = s_clib_get_symbol_by_name (o_current->complex_basename);
  s_clib_symbol_invalidate_data (clib);

  if (clib == NULL) {
    s_log_message (_("Could not find symbol [%s] in library. Update failed.\n"),
                   o_current->complex_basename);
    return NULL;
  }

  /* Unselect the old object. */
  o_selection_remove (toplevel, page->selection_list, o_current);

  /* Create new object and set embedded */
  o_new = o_complex_new (toplevel, OBJ_COMPLEX, DEFAULT_COLOR,
                         o_current->complex->x,
                         o_current->complex->y,
                         o_current->complex->angle,
                         o_current->complex->mirror,
                         clib, o_current->complex_basename,
                         1);
  if (o_complex_is_embedded (o_current)) {
    o_embed (toplevel, o_new);
  }

  new_attribs = o_complex_promote_attribs (toplevel, o_new);

  /* Cull any attributes from new COMPLEX that are already attached to
   * old COMPLEX. Note that the new_attribs list is kept consistent by
   * setting GList data pointers to NULL if their OBJECTs are
   * culled. At the end, the new_attribs list is updated by removing
   * all list items with NULL data. This is slightly magic, but
   * works. */
  for (iter = new_attribs; iter != NULL; iter = g_list_next (iter)) {
    OBJECT *attr_new = iter->data;
    gchar *name;
    gchar *value;

    g_assert (attr_new->type == OBJ_TEXT);

    o_attrib_get_name_value (attr_new, &name, NULL);

    value = o_attrib_search_attached_attribs_by_name (o_current, name, 0);
    if (value != NULL) {
      o_attrib_remove (toplevel, &o_new->attribs, attr_new);
      s_delete_object (toplevel, attr_new);
      iter->data = NULL;
    }

    g_free (name);
    g_free (value);
  }
  new_attribs = g_list_remove_all (new_attribs, NULL);

  /* Detach attributes from old OBJECT and attach to new OBJECT */
  old_attribs = g_list_copy (o_current->attribs);
  o_attrib_detach_all (toplevel, o_current);
  o_attrib_attach_list (toplevel, old_attribs, o_new, 1);
  g_list_free (old_attribs);

  /* Add new attributes to page */
  s_page_append_list (toplevel, page, new_attribs);

  /* Update pinnumbers for current slot */
  s_slot_update_object (toplevel, o_new);

  /* Replace old OBJECT with new OBJECT */
  s_page_replace (toplevel, page, o_current, o_new);
  s_delete_object (toplevel, o_current);

  /* Select new OBJECT */
  o_selection_add (toplevel, page->selection_list, o_new);

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
  TOPLEVEL *toplevel = gschem_toplevel_get_toplevel (w_current);
  GList *iter;
  PAGE *p_save, *p_current;
  gchar *backup_filename;
  gchar *real_filename;
  gchar *only_filename;
  gchar *dirname;
  mode_t saved_umask;
  mode_t mask;
  struct stat st;

  /* save current page */
  p_save = toplevel->page_current;

  for ( iter = geda_list_get_glist( toplevel->pages );
        iter != NULL;
        iter = g_list_next( iter ) ) {

    p_current = (PAGE *)iter->data;

    if (p_current->do_autosave_backup == 0) {
      continue;
    }
    if (p_current->ops_since_last_backup != 0) {
      /* make p_current the current page of toplevel */
      s_page_goto (toplevel, p_current);
      gschem_toplevel_page_changed (w_current);

      /* Get the real filename and file permissions */
      real_filename = follow_symlinks (p_current->page_filename, NULL);

      if (real_filename == NULL) {
        s_log_message (_("o_autosave_backups: Can't get the real filename of %s."), p_current->page_filename);
      } else {
        /* Get the directory in which the real filename lives */
        dirname = g_path_get_dirname (real_filename);
        only_filename = g_path_get_basename(real_filename);

        backup_filename = g_strdup_printf("%s%c"AUTOSAVE_BACKUP_FILENAME_STRING,
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
            s_log_message (_("Could NOT set previous backup file [%s] read-write\n"),
                           backup_filename);
          }
          umask(saved_umask);
        }

        if (o_save (toplevel,
                    s_page_objects (toplevel->page_current),
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
            s_log_message (_("Could NOT set backup file [%s] readonly\n"),
                           backup_filename);
          }
          umask(saved_umask);
        } else {
          s_log_message (_("Could NOT save backup file [%s]\n"),
                         backup_filename);
        }
        g_free (backup_filename);
      }
    }
  }
  /* restore current page */
  s_page_goto (toplevel, p_save);
  gschem_toplevel_page_changed (w_current);
}
