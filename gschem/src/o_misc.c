/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* break with the tradition here and input a list */
/*! \todo probably should go back and do the same for o_copy o_move
 *  o_delete...
 */
/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit(GSCHEM_TOPLEVEL *w_current, GList *list)
{
  OBJECT *o_current;
  const gchar *str = NULL;

  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
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
      str = o_text_get_string (w_current->toplevel, o_current);
      if (o_attrib_get_name_value (o_current, NULL, NULL) &&
        /* attribute editor only accept 1-line values for attribute */
        o_text_num_lines (str) == 1) {
        attrib_edit_dialog(w_current,o_current, FROM_MENU);
    } else {
      o_text_edit(w_current, o_current);
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
void o_lock(GSCHEM_TOPLEVEL *w_current)
{
  OBJECT *object = NULL;
  GList *s_current = NULL;

  /* skip over head */
  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;
    if (object) {
      /* check to see if locked_color is already being used */
      if (object->locked_color == -1) {
        object->sel_func = NULL;
        object->locked_color = object->color;
        object->color = LOCK_COLOR;
        w_current->toplevel->page_current->CHANGED=1;
      } else {
        s_log_message(_("Object already locked\n"));
      }
    }

    s_current = g_list_next(s_current);
  }

  if (!w_current->SHIFTKEY) o_select_unselect_all(w_current);
  o_undo_savestate(w_current, UNDO_ALL);
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
void o_unlock(GSCHEM_TOPLEVEL *w_current)
{
  OBJECT *object = NULL;
  GList *s_current = NULL;

  s_current = geda_list_get_glist( w_current->toplevel->page_current->selection_list );

  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;
    if (object) {
      /* only unlock if sel_func is not set to something */
      if (object->sel_func == NULL) {
        object->sel_func = select_func;
        object->color = object->locked_color;
        object->locked_color = -1;
        w_current->toplevel->page_current->CHANGED = 1;
      } else {
        s_log_message(_("Object already unlocked\n"));
      }
    }

    s_current = g_list_next(s_current);
  }
  o_undo_savestate(w_current, UNDO_ALL);
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] centerx    Center x coordinate of rotation.
 *  \param [in] centery    Center y coordinate of rotation.
 *  \param [in] angle      Angle to rotate the objects through.
 *  \param [in] list       The list of objects to rotate.
 */
void o_rotate_world_update(GSCHEM_TOPLEVEL *w_current,
                           int centerx, int centery, int angle, GList *list)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  GList *o_iter;

  /* this is okay if you just hit rotate and have nothing selected */
  if (list == NULL) {
    w_current->inside_action = 0;
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

    s_conn_remove_object (toplevel, o_current);
  }

  o_glist_rotate_world( toplevel, centerx, centery, angle, list );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

    s_conn_update_object (toplevel, o_current);
  }

  o_invalidate_glist (w_current, list);

  /* All objects were rotated. Run the rotate hooks */
  o_rotate_call_hooks (w_current, list);

  /* Don't save the undo state if we are inside an action */
  /* This is useful when rotating the selection while moving, for example */
  toplevel->page_current->CHANGED = 1;
  if (!w_current->inside_action) {
    o_undo_savestate(w_current, UNDO_ALL);
  }
}


void o_rotate_call_hooks (GSCHEM_TOPLEVEL *w_current, GList *list)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  GList *iter;

  /* Do not run any hooks for simple objects here, like text, since they
     were rotated in the previous pass, and the selection list can contain
     an object and all its attributes (text) */
  for (iter = list; iter != NULL; iter = g_list_next (iter)) {
    o_current = iter->data;

    switch (o_current->type) {
      case OBJ_PIN:
        /* Run the rotate pin hook */
        if (scm_hook_empty_p (rotate_pin_hook) == SCM_BOOL_F) {
          scm_run_hook (rotate_pin_hook,
                        scm_cons (g_make_object_smob (toplevel, o_current),
                                  SCM_EOL));
        }
        break;

      case OBJ_COMPLEX:
        /* Run the rotate hook */
        if (scm_hook_empty_p (rotate_component_object_hook) == SCM_BOOL_F) {
          scm_run_hook (rotate_component_object_hook,
                        scm_cons (g_make_object_smob (toplevel, o_current),
                                  SCM_EOL));
        }
        break;

      default:
        break;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_mirror_world_update(GSCHEM_TOPLEVEL *w_current, int centerx, int centery, GList *list)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  GList *o_iter;

  if (list == NULL) {
    w_current->inside_action = 0;
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

    s_conn_remove_object (toplevel, o_current);
  }

  o_glist_mirror_world( toplevel, centerx, centery, list );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it.
   */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

    s_conn_update_object (toplevel, o_current);
  }

  o_invalidate_glist (w_current, list);

  /* All objects were mirrored. Do a 2nd pass to run the mirror hooks */
  /* Do not run any hooks for simple objects here, like text, since they
     were mirrored in the previous pass, and the selection list can contain
     an object and all its attributes (text) */
  o_iter = list;
  while (o_iter != NULL) {
    o_current = (OBJECT *) o_iter->data;

    switch(o_current->type) {
      case(OBJ_PIN):
        /* Run the mirror pin hook */
        if (scm_hook_empty_p(mirror_pin_hook) == SCM_BOOL_F &&
            o_current != NULL) {
          scm_run_hook(mirror_pin_hook,
                       scm_cons(g_make_object_smob(toplevel, o_current),
                                SCM_EOL));
        }
        break;

      case (OBJ_COMPLEX):
        /* Run the mirror pin hook */
        if (scm_hook_empty_p(mirror_component_object_hook) == SCM_BOOL_F &&
            o_current != NULL) {
          scm_run_hook(mirror_component_object_hook,
                       scm_cons(g_make_object_smob(toplevel, o_current),
                                SCM_EOL));
        }
        break;
    default:
        break;
    }

    o_iter = g_list_next(o_iter);
  }

  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_hidden_lowlevel (GSCHEM_TOPLEVEL *w_current,
                                  const GList *o_list)
{
  TOPLEVEL *toplevel = w_current->toplevel;
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
      o_recalc_single_object(toplevel, o_current);
    }

    iter = g_list_next (iter);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_hidden (GSCHEM_TOPLEVEL *w_current, const GList *o_list)
{
  /* this function just shows the hidden text, but doesn't toggle it */
  /* this function does not change the CHANGED bit, no real changes are */
  /* made to the schematic */

  /* toggle show_hidden_text variable, which when it is true */
  /* means that hidden text IS drawn */
  w_current->toplevel->show_hidden_text = !w_current->toplevel->show_hidden_text;
  i_show_state(w_current, NULL); /* update screen status */

  o_edit_show_hidden_lowlevel(w_current, o_list);
  o_invalidate_all (w_current);

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
void o_edit_make_visible (GSCHEM_TOPLEVEL *w_current, const GList *o_list)
{
  /* this function actually changes the visibility flag */
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (o_current->type == OBJ_TEXT) {
      if (!o_is_visible (toplevel, o_current)) {
        o_set_visibility (toplevel, o_current, VISIBLE);
        o_text_recreate(toplevel, o_current);

        toplevel->page_current->CHANGED = 1;
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);

}

#define FIND_WINDOW_HALF_SIZE (5000)

OBJECT *last_o = NULL;
int skiplast;

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \todo Only descends into the first source schematic
 *
 */
int o_edit_find_text (GSCHEM_TOPLEVEL *w_current, const GList *o_list,
                      char *stext, int descend, int skip)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  char *attrib = NULL;
  int count = 0;
  PAGE *parent = NULL;
  char *current_filename = NULL;
  int page_control = 0;
  int pcount = 0;
  int rv;
  int x1, y1, x2, y2;
  int text_screen_height;
  const GList *iter;

  OBJECT *o_current;

  skiplast = skip;

  iter = o_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (descend) {
      if (o_current->type == OBJ_COMPLEX) {
        parent = toplevel->page_current;
        attrib = o_attrib_search_attached_attribs_by_name (o_current,
                                                           "source", count);

        /* if above is null, then look inside symbol */
        if (attrib == NULL) {
          attrib = o_attrib_search_inherited_attribs_by_name (o_current,
                                                              "source", count);
          /*          looking_inside = TRUE; */
        }

        if (attrib) {
          pcount = 0;
          current_filename = u_basic_breakup_string(attrib, ',', pcount);
          if (current_filename != NULL) {
            PAGE *child_page =
              s_hierarchy_down_schematic_single(toplevel,
                                                current_filename,
                                                parent,
                                                page_control,
                                                HIERARCHY_NORMAL_LOAD);

            if (child_page != NULL) {
              page_control = child_page->page_control;
              rv = o_edit_find_text (w_current,
                                     s_page_objects (child_page),
                                     stext, descend, skiplast);
              if (!rv) {
                s_page_goto( toplevel, child_page );
                return 0;
              }
            }
          }
        }
      }
    }

    if (o_current->type == OBJ_TEXT) {
      const gchar *str = o_text_get_string (toplevel, o_current);
     /* replaced strcmp with strstr to simplify the search */
      if (strstr (str,stext)) {
        if (!skiplast) {
          a_zoom(w_current, ZOOM_FULL, DONTCARE, A_PAN_DONT_REDRAW);
          world_get_single_object_bounds (toplevel, o_current, &x1, &y1, &x2, &y2);
          text_screen_height = SCREENabs (w_current, y2 - y1);
          /* this code will zoom/pan till the text screen height is about */
          /* 50 pixels high, perhaps a future enhancement will be to make */
          /* this number configurable */
          while (text_screen_height < 50) {
            a_zoom(w_current, ZOOM_IN, DONTCARE, A_PAN_DONT_REDRAW);
            text_screen_height = SCREENabs (w_current, y2 - y1);
          }
          a_pan_general(w_current,
                        o_current->text->x, o_current->text->y,
                        1, 0);

	  /* Make sure the titlebar and scrollbars are up-to-date */
	  x_window_set_current_page(w_current, 
                                    w_current->toplevel->page_current );

          last_o = o_current;
          break;
        }
        if (last_o == o_current) {
          skiplast = 0;
        }

      } /* if (strstr(o_current->text->string,stext)) */
    } /* if (o_current->type == OBJ_TEXT) */
    iter = g_list_next (iter);

    if (iter == NULL) {
      return 1;
    }
  }
  return (iter == NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_hide_specific_text (GSCHEM_TOPLEVEL *w_current,
                                const GList *o_list,
                                char *stext)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (o_current->type == OBJ_TEXT) {
      const gchar *str = o_text_get_string (w_current->toplevel, o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (o_is_visible (toplevel, o_current)) {
          o_set_visibility (toplevel, o_current, INVISIBLE);
          o_text_recreate(toplevel, o_current);

          toplevel->page_current->CHANGED = 1;
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);
  o_invalidate_all (w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_specific_text (GSCHEM_TOPLEVEL *w_current,
                                const GList *o_list,
                                char *stext)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_current;
  const GList *iter;

  iter = o_list;
  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (o_current->type == OBJ_TEXT) {
      const gchar *str = o_text_get_string (w_current->toplevel, o_current);
      if (!strncmp (stext, str, strlen (stext))) {
        if (!o_is_visible (toplevel, o_current)) {
          o_set_visibility (toplevel, o_current, VISIBLE);
          o_text_recreate(toplevel, o_current);

          toplevel->page_current->CHANGED = 1;
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);
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
 * \param [in]     w_current The GSCHEM_TOPLEVEL object.
 * \param [in,out] o_current The OBJECT to be updated.
 *
 * \return the new OBJECT that replaces \a o_current.
 */
OBJECT *
o_update_component (GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
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

  /* This should be replaced with API to invalidate only the specific
   * symbol name we want to update */
  s_clib_flush_symbol_cache ();
  clib = s_clib_get_symbol_by_name (o_current->complex_basename);

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
  toplevel->page_current->CHANGED = 1;
  o_undo_savestate (w_current, UNDO_ALL);

  return o_new;
}

/*! \brief Do autosave on all pages that are marked.
 *  \par Function Description
 *  Looks for pages with the do_autosave_backup flag activated and
 *  autosaves them.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object to search for autosave's.
 */
void o_autosave_backups(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
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
                    backup_filename)) {

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
}
