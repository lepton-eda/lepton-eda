/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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
      if (o_attrib_get_name_value (str, NULL, NULL) &&
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
  GList *prev_conn_objects=NULL;
  GList *connected_objects=NULL;

  /* this is okay if you just hit rotate and have nothing selected */
  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  if (!toplevel->DONT_REDRAW) {
    o_invalidate_glist (w_current, list);
  }

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it. The extra redraws
   * don't _really_ hurt though. */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

    prev_conn_objects = s_conn_return_others (prev_conn_objects, o_current);
    s_conn_remove_object (toplevel, o_current);
  }

  o_glist_rotate_world( toplevel, centerx, centery, angle, list );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it. The extra redraws dont
   * _really_ hurt though. */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

    s_conn_update_object (toplevel, o_current);
    connected_objects = s_conn_return_others (connected_objects, o_current);
  }

  if (!toplevel->DONT_REDRAW) {
    o_invalidate_glist (w_current, list);
    o_invalidate_glist (w_current, prev_conn_objects);
    o_invalidate_glist (w_current, connected_objects);
  }

  g_list_free (prev_conn_objects);
  prev_conn_objects = NULL;
  g_list_free (connected_objects);
  connected_objects = NULL;

  /* All objects were rotated. Do a 2nd pass to run the rotate hooks */
  /* Do not run any hooks for simple objects here, like text, since they
     were rotated in the previous pass, and the selection list can contain
     an object and all its attributes (text) */
  o_iter = list;
  while (o_iter != NULL) {
    o_current = o_iter->data;

    switch(o_current->type) {
      case(OBJ_PIN):
        /* Run the rotate pin hook */
        if (scm_hook_empty_p(rotate_pin_hook) == SCM_BOOL_F &&
            o_current != NULL) {
          scm_run_hook(rotate_pin_hook,
                       scm_cons(g_make_object_smob(toplevel, o_current),
                                SCM_EOL));
        }
        break;

      case (OBJ_COMPLEX):
        /* Run the rotate hook */
        if (scm_hook_empty_p(rotate_component_object_hook) == SCM_BOOL_F &&
            o_current != NULL) {
          scm_run_hook(rotate_component_object_hook,
                       scm_cons(g_make_object_smob(toplevel, o_current),
                                SCM_EOL));
        }
        break;
    default:
        break;
    }

    o_iter = g_list_next(o_iter);
  }

  /* Don't save the undo state if we are inside an action */
  /* This is useful when rotating the selection while moving, for example */
  toplevel->page_current->CHANGED = 1;
  if (!w_current->inside_action) {
    o_undo_savestate(w_current, UNDO_ALL);
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
  GList *prev_conn_objects=NULL;
  GList *connected_objects=NULL;

  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  o_invalidate_glist (w_current, list);

  /* Find connected objects, removing each object in turn from the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it. The extra redraws
   * don't _really_ hurt though. */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

    prev_conn_objects = s_conn_return_others (prev_conn_objects, o_current);
    s_conn_remove_object (toplevel, o_current);
  }

  o_glist_mirror_world( toplevel, centerx, centery, list );

  /* Find connected objects, adding each object in turn back to the
   * connection list. We only _really_ want those objects connected
   * to the selection, not those within in it. The extra redraws dont
   * _really_ hurt though. */
  for (o_iter = list; o_iter != NULL; o_iter = g_list_next (o_iter)) {
    o_current = o_iter->data;

    s_conn_update_object (toplevel, o_current);
    connected_objects = s_conn_return_others (connected_objects, o_current);
  }

  o_invalidate_glist (w_current, list);
  o_invalidate_glist (w_current, prev_conn_objects);
  o_invalidate_glist (w_current, connected_objects);

  g_list_free (prev_conn_objects);
  prev_conn_objects = NULL;
  g_list_free (connected_objects);
  connected_objects = NULL;

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
    if (o_current->type == OBJ_TEXT && o_current->visibility == INVISIBLE) {

      /* don't toggle the visibility flag */

      if (toplevel->show_hidden_text) {
        /* draw the text object if it hidden  */
        if (o_current->text->prim_objs == NULL) {
          o_text_recreate(toplevel, o_current);
        }
        o_recalc_single_object(toplevel, o_current);
        o_invalidate (w_current, o_current);
      } else {
        /* object is hidden and we are now NOT drawing it, so */
        /* get rid of the extra primitive data */
        o_text_recreate(toplevel, o_current);
        o_recalc_single_object(toplevel, o_current);
        /* unfortunately, you cannot erase the old visible text here */
        /* because o_text_draw will just return */
      }
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
      if (o_current->visibility == INVISIBLE) {
        o_current->visibility = VISIBLE;

        if (o_current->text->prim_objs == NULL) {
          o_text_recreate(toplevel, o_current);
        }

        o_invalidate (w_current, o_current);

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
        attrib = o_attrib_search_name_single_count(o_current, "source", count);

        /* if above is null, then look inside symbol */
        if (attrib == NULL) {
          attrib = o_attrib_search_name(o_current->complex->prim_objs,
                                        "source", count);
          /*          looking_inside = TRUE; */
        }

        if (attrib) {
          pcount = 0;
          current_filename = u_basic_breakup_string(attrib, ',', pcount);
          if (current_filename != NULL) {
            page_control =
              s_hierarchy_down_schematic_single(toplevel,
                                                current_filename,
                                                parent,
                                                page_control,
                                                HIERARCHY_NORMAL_LOAD);
            /* o_invalidate_all (w_current); */

            rv = o_edit_find_text (w_current,
                                   s_page_objects (toplevel->page_current),
                                   stext, descend, skiplast);
            if (!rv) {
              return 0;
            }
            s_page_goto( toplevel, parent );
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
          text_screen_height =
            SCREENabs (w_current, o_text_height (str, o_current->text->size));
          /* this code will zoom/pan till the text screen height is about */
          /* 50 pixels high, perhaps a future enhancement will be to make */
          /* this number configurable */
          while (text_screen_height < 50) {
            a_zoom(w_current, ZOOM_IN, DONTCARE, A_PAN_DONT_REDRAW);
            text_screen_height =
              SCREENabs (w_current, o_text_height (str, o_current->text->size));
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
        if (o_current->visibility == VISIBLE) {
          o_current->visibility = INVISIBLE;

          if (o_current->text->prim_objs == NULL) {
            o_text_recreate(toplevel, o_current);
          }
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
        if (o_current->visibility == INVISIBLE) {
          o_current->visibility = VISIBLE;

          if (o_current->text->prim_objs == NULL) {
            o_text_recreate(toplevel, o_current);
          }
          o_invalidate (w_current, o_current);
          toplevel->page_current->CHANGED = 1;
        }
      }
    }
    iter = g_list_next (iter);
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_update_component(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_complex;
  OBJECT *a_current;
  GList *temp_list;
  GList *a_iter;
  GList *po_iter;
  gboolean is_embedded;
  const CLibSymbol *clib;

  g_return_if_fail (o_current != NULL);

  is_embedded = o_complex_is_embedded (o_current);

  g_assert (o_current->complex_basename != NULL);

  /* This shuold be replaced with API to invalidate only the specific
   * symbol name we want to update */
  s_clib_flush_symbol_cache ();
  clib = s_clib_get_symbol_by_name (o_current->complex_basename);

  if (clib == NULL) {
    s_log_message (_("Could not find symbol [%s] in library. Update failed.\n"),
                   o_current->complex_basename);
    return;
  }

  /* ensure we repaint where the complex object was */
  o_invalidate (w_current, o_current);
  /* delete its connections */
  s_conn_remove_object (toplevel, o_current);
  /* and unselect it */
  o_selection_remove( toplevel->page_current->selection_list, o_current);

  new_complex = o_complex_new (toplevel, OBJ_COMPLEX, DEFAULT_COLOR,
                               o_current->complex->x,
                               o_current->complex->y,
                               o_current->complex->angle,
                               o_current->complex->mirror,
                               clib, o_current->complex_basename,
                               1);

  temp_list = o_complex_promote_attribs (toplevel, new_complex);
  temp_list = g_list_append (temp_list, new_complex);

  /* updating the old complex with data from the new one */
  /* first process the prim_objs: */
  /*   - delete the prim_objs of the old component */
  s_delete_object_glist (toplevel, o_current->complex->prim_objs);
  /*   - put the prim_objs of the new component in the old one */
  o_current->complex->prim_objs = new_complex->complex->prim_objs;

  /* set the parent field now */
  for (po_iter = o_current->complex->prim_objs;
       po_iter != NULL;
       po_iter = g_list_next (po_iter)) {
    OBJECT *tmp = po_iter->data;
    tmp->complex_parent = o_current;
  }

  /*   - reset the new complex prim_objs */
  new_complex->complex->prim_objs = NULL;

  /* then process the attributes: */
  /*   - check each attrib of the new complex */
  a_iter = new_complex->attribs;
  while (a_iter != NULL) {
    OBJECT *o_attrib;
    gchar *name;
    char *attrfound;

    a_current = a_iter->data;
    g_assert (a_current->type == OBJ_TEXT);

    o_attrib_get_name_value (o_text_get_string (toplevel, a_current),
                             &name, NULL);

    attrfound = o_attrib_search_name_single(o_current, name, NULL);

    /* free these now since they are no longer being used */
    g_free(name);

    if (attrfound == NULL) {
      /* attribute with same name not found in old component: */
      /* add new attribute to old component */

      /* make a copy of the attribute object */
      o_attrib = o_object_copy (toplevel, a_current, NORMAL_FLAG);
      s_page_append (toplevel->page_current, o_attrib);
      /* add the attribute to old */
      o_attrib_add (toplevel, o_current, o_attrib);
      /* redraw the attribute object */
      o_invalidate (w_current, o_attrib);
      /* note: this object is unselected (not added to selection). */
    }
    else
    {
      g_free(attrfound);
    }

    a_iter = g_list_next (a_iter);
  }

  s_delete_object_glist (toplevel, temp_list);

  /* update the pinnumbers to the current slot */
  o_attrib_slot_update(toplevel, o_current);

  /* Recalculate the bounds of the object */
  o_recalc_single_object(toplevel, o_current);

  /* reconnect, re-select and redraw */
  s_conn_update_object (toplevel, o_current);
  o_selection_add( toplevel->page_current->selection_list, o_current );
  o_invalidate (w_current, o_current);

  /* Re-flag as embedded if necessary */
  o_current->complex_embedded = is_embedded;

  /* mark the page as modified */
  toplevel->page_current->CHANGED = 1;
  o_undo_savestate (w_current, UNDO_ALL);

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

        if (o_save_curr_page (toplevel, backup_filename)) {

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
