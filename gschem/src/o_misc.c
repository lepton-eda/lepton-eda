/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
#include <ctype.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <libgen.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

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
void o_edit(TOPLEVEL *w_current, GList *list)
{
  char *equal_ptr;
  OBJECT *o_current;
  int num_lines = 0;

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
    case(OBJ_TEXT):
    if(strchr(o_current->text->string,'=')) {

      /* now really make sure it's an attribute by
       * checking that there are NO spaces around the ='s
       */
      equal_ptr = strchr(o_current->text->string, '=');

      /* and also make sure it is only a single line */
      num_lines = o_text_num_lines(o_current->text->string);

      /* there is a possiblity for core dump yes? */
      /* by exceeding the size of the text_string? */
      /* or maybe not, since if the ='s is at the end of */
      /* the string, there better be a null after it! */
      if ( (*(equal_ptr + 1) != ' ') &&
           (*(equal_ptr - 1) != ' ') &&
           (num_lines == 1) ) {
        attrib_edit_dialog(w_current,o_current, FROM_MENU);
        /* multi_attrib_edit(w_current, o_current); */

      } else {
        o_text_edit(w_current, o_current);
      }
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
void o_lock(TOPLEVEL *w_current)
{
  OBJECT *object = NULL;
  GList *s_current = NULL;

  /* skip over head */
  s_current = geda_list_get_glist( w_current->page_current->selection_list );

  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;
    if (object) {
      /* check to see if locked_color is already being used */
      if (object->locked_color == -1) {
        object->sel_func = NULL;
        object->locked_color = object->color;
        object->color = w_current->lock_color;
        w_current->page_current->CHANGED=1;
      } else {
        s_log_message(_("Object already locked\n"));
      }
    }

    s_current=s_current->next;
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
void o_unlock(TOPLEVEL *w_current)
{
  OBJECT *object = NULL;
  GList *s_current = NULL;

  s_current = geda_list_get_glist( w_current->page_current->selection_list );

  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;
    if (object) {
      /* only unlock if sel_func is not set to something */
      if (object->sel_func == NULL) {
        object->sel_func = select_func;
        object->color = object->locked_color;
        object->locked_color = -1;
        w_current->page_current->CHANGED = 1;
      } else {
        s_log_message(_("Object already unlocked\n"));
      }
    }

    s_current=s_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Rotate all objects in list.
 *  \par Function Description
 *  Given the selection <B>list</B>, and the center of rotation
 *  (<B>centerx</B>,<B>centery</B>, this function traverses all the selection
 *  list, rotating each object.
 *  The selection list contains a given object and all its attributes
 *  (refdes, pinname, pinlabel, ...).
 *  There is a second pass to run the rotate hooks of non-simple objects,
 *  like pin or complex objects, for example.
 *
 *  \param [in] w_current  The TOPLEVEL object.
 *  \param [in] list       The list of objects to rotate.
 *  \param [in] centerx    Center x coordinate of rotation.
 *  \param [in] centery    Center y coordinate of rotation.
 */
void o_rotate_90_world(TOPLEVEL *w_current, GList *list,
                       int centerx, int centery)
{
  OBJECT *object;
  GList *s_current;
  int new_angle;
  GList *other_objects=NULL;
  GList *connected_objects=NULL;
  OBJECT *o_current=NULL;

  /* this is okay if you just hit rotate and have nothing selected */
  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  s_current = list;

  while (s_current != NULL) {
    object = (OBJECT *) s_current->data;

    if (!object) {
      fprintf(stderr, _("ERROR: NULL object in o_rotate_90!\n"));
      return;
    }

    g_list_free(other_objects);
    other_objects = NULL;
    g_list_free(connected_objects);
    connected_objects = NULL;

    switch(object->type) {


      case(OBJ_NET):
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw(w_current, object);
          o_net_erase(w_current, object);
          o_line_erase_grips(w_current, object);
        }

        /* save the other objects */
        other_objects = s_conn_return_others(other_objects, object);
        s_conn_remove(w_current, object);

        o_net_rotate_world(w_current, centerx, centery, 90, object);
        s_conn_update_object(w_current, object);
        if (!w_current->DONT_REDRAW) {
          o_net_draw(w_current, object);

          /* draw the other objects */
          o_cue_undraw_list(w_current, other_objects);
          o_cue_draw_list(w_current, other_objects);
        }

        /* get other connected objects and redraw */
        connected_objects = s_conn_return_others(connected_objects, object);
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw_list(w_current, connected_objects);
          o_cue_draw_list(w_current, connected_objects);

          /* finally redraw the cues on the current object */
          o_cue_draw_single(w_current, object);
        }
        break;

      case(OBJ_BUS):
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw(w_current, object);
          o_bus_erase(w_current, object);
          o_line_erase_grips(w_current, object);
        }

        other_objects = s_conn_return_others(other_objects, object);
        s_conn_remove(w_current, object);

        o_bus_rotate_world(w_current, centerx, centery, 90, object);
        s_conn_update_object(w_current, object);
        if (!w_current->DONT_REDRAW) {
          o_bus_draw(w_current, object);

          /* draw the other objects */
          o_cue_undraw_list(w_current, other_objects);
          o_cue_draw_list(w_current, other_objects);
        }

        /* get other connected objects and redraw */
        connected_objects = s_conn_return_others(connected_objects, object);
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw_list(w_current, connected_objects);
          o_cue_draw_list(w_current, connected_objects);

          /* finally redraw the cues on the current object */
          o_cue_draw_single(w_current, object);
        }
        break;

      case(OBJ_PIN):
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw(w_current, object);
          o_pin_erase(w_current, object);
          o_line_erase_grips(w_current, object);
        }

        other_objects = s_conn_return_others(other_objects, object);
        s_conn_remove(w_current, object);

        o_pin_rotate_world(w_current, centerx, centery, 90, object);
        s_conn_update_object(w_current, object);
        if (!w_current->DONT_REDRAW) {
          o_pin_draw(w_current, object);

          /* draw the other objects */
          o_cue_undraw_list(w_current, other_objects);
          o_cue_draw_list(w_current, other_objects);
        }

        /* get other connected objects and redraw */
        connected_objects = s_conn_return_others(connected_objects, object);
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw_list(w_current, connected_objects);
          o_cue_draw_list(w_current, connected_objects);

          /* finally redraw the cues on the current object */
          o_cue_draw_single(w_current, object);
        }
        break;

      case(OBJ_COMPLEX):
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw_objects(w_current, object->complex->prim_objs);
          /* erase the current selection */
          o_complex_erase(w_current, object);
        }

        other_objects = s_conn_return_complex_others(other_objects, object);

        /* remove all conn references */
        o_current = object->complex->prim_objs;
        while(o_current != NULL) {
          s_conn_remove(w_current, o_current);
          o_current = o_current->next;
        }

        /* do the rotate */
        /*w_current->ADDING_SEL=1; NEWSEL: needed? */
        new_angle = (object->complex->angle + 90) % 360;
        o_complex_rotate_world(w_current, centerx, centery,
                         new_angle, 90, object);
        /*w_current->ADDING_SEL = 0; NEWSEL: needed? */
        s_conn_update_complex(w_current, object->complex->prim_objs);
        if (!w_current->DONT_REDRAW) {
          o_complex_draw(w_current, object);

          o_cue_undraw_list(w_current, other_objects);
          o_cue_draw_list(w_current, other_objects);
        }

        /* now draw the newly connected objects */
        connected_objects = s_conn_return_complex_others(connected_objects,
                                                         object);
        if (!w_current->DONT_REDRAW) {
          o_cue_undraw_list(w_current, connected_objects);
          o_cue_draw_list(w_current, connected_objects);
        }
        break;

      case(OBJ_LINE):
        if (!w_current->DONT_REDRAW) {
          o_line_erase_grips(w_current, object);
          o_line_erase(w_current, object);
        }

        o_line_rotate_world(w_current, centerx, centery,
                      90, object);

        if (!w_current->DONT_REDRAW) {
          o_line_draw(w_current, object);
        }
        break;

      case(OBJ_BOX):
        /* erase the current selection */
        if (!w_current->DONT_REDRAW) {
          o_box_erase_grips(w_current, object);
          o_box_erase(w_current, object);
        }

        o_box_rotate_world(w_current, centerx, centery,
                     90, object);

        if (!w_current->DONT_REDRAW) {
          o_box_draw(w_current, object);
        }
        break;

      case(OBJ_PICTURE):
        /* erase the current selection */

        if (!w_current->DONT_REDRAW) {
          o_picture_erase_grips(w_current, object);
          o_picture_erase(w_current, object);
        }

        o_picture_rotate_world(w_current, centerx, centery,
                     90, object);

        if (!w_current->DONT_REDRAW) {
          o_picture_draw(w_current, object);
        }
        break;

      case(OBJ_CIRCLE):
        if (!w_current->DONT_REDRAW) {
          o_circle_erase_grips(w_current, object);
          o_circle_erase(w_current, object);
        }

        o_circle_rotate_world(w_current, centerx, centery,
                        90, object);

        if (!w_current->DONT_REDRAW) {
          o_circle_draw(w_current, object);
        }
        break;

      case(OBJ_ARC):
        if (!w_current->DONT_REDRAW) {
          o_arc_erase(w_current, object);
        }

        o_arc_rotate_world(w_current, centerx, centery, 90, object);
        if (!w_current->DONT_REDRAW) {
          o_arc_draw(w_current, object);
        }
        break;

      case(OBJ_TEXT):
        /* erase the current selection */
        if (!w_current->DONT_REDRAW) {
          o_text_erase(w_current, object);
        }

        o_text_rotate_world(w_current, centerx, centery, 90, object);

        if (!w_current->DONT_REDRAW) {
          o_text_draw(w_current, object);
        }
        break;
    }
    s_current = s_current->next;
  }

  /* All objects were rotated. Do a 2nd pass to run the rotate hooks */
  /* Do not run any hooks for simple objects here, like text, since they
     were rotated in the previous pass, and the selection list can contain
     an object and all its attributes (text) */
  s_current = list;
  while (s_current != NULL) {
    object = (OBJECT *) s_current->data;

    if (!object) {
      fprintf(stderr, _("ERROR: NULL object in o_rotate_90!\n"));
      return;
    }

    switch(object->type) {
      case(OBJ_PIN):
        /* Run the rotate pin hook */
        if (scm_hook_empty_p(rotate_pin_hook) == SCM_BOOL_F &&
            object != NULL) {
          scm_run_hook(rotate_pin_hook,
                       scm_cons(g_make_object_smob(w_current, object),
                                SCM_EOL));
        }
        break;

      case (OBJ_COMPLEX):
        /* Run the rotate hook */
        if (scm_hook_empty_p(rotate_component_object_hook) == SCM_BOOL_F &&
            object != NULL) {
          scm_run_hook(rotate_component_object_hook,
                       scm_cons(g_make_object_smob(w_current, object),
                                SCM_EOL));
        }
        break;
    default:
        break;
    }

    s_current = s_current->next;
  }

  /* Don't save the undo state if we are inside an action */
  /* This is useful when rotating the selection while moving, for example */
  w_current->page_current->CHANGED = 1;
  if (!w_current->inside_action) {
    o_undo_savestate(w_current, UNDO_ALL);
  }
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_mirror_world(TOPLEVEL *w_current, GList *list, int centerx, int centery)
{
  OBJECT *object;
  GList *s_current;
  OBJECT *o_current = NULL;
  GList *other_objects=NULL;
  GList *connected_objects=NULL;

  if (list == NULL) {
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  s_current = list;

  while (s_current != NULL) {

    object = (OBJECT *) s_current->data;

    if (!object) {
      fprintf(stderr, _("ERROR: NULL object in o_mirror!\n"));
      return;
    }

    g_list_free(other_objects);
    other_objects = NULL;
    g_list_free(connected_objects);
    connected_objects = NULL;

    switch(object->type) {


      case(OBJ_NET):
        o_cue_undraw(w_current, object);
        o_net_erase(w_current, object);
        o_line_erase_grips(w_current, object);

        other_objects = s_conn_return_others(other_objects, object);
        s_conn_remove(w_current, object);

        o_net_mirror_world(w_current, centerx, centery, object);
        s_conn_update_object(w_current, object);
        o_net_draw(w_current, object);

        /* draw the other objects */
        o_cue_undraw_list(w_current, other_objects);
        o_cue_draw_list(w_current, other_objects);

        /* get other connected objects and redraw */
        connected_objects = s_conn_return_others(connected_objects, object);
        o_cue_undraw_list(w_current, connected_objects);
        o_cue_draw_list(w_current, connected_objects);

        /* finally redraw the cues on the current object */
        o_cue_draw_single(w_current, object);
        break;

      case(OBJ_PIN):
        o_cue_undraw(w_current, object);
        o_pin_erase(w_current, object);
        o_line_erase_grips(w_current, object);

        other_objects = s_conn_return_others(other_objects, object);
        s_conn_remove(w_current, object);

        o_pin_mirror_world(w_current, centerx, centery, object);
        s_conn_update_object(w_current, object);
        o_pin_draw(w_current, object);

        /* draw the other objects */
        o_cue_undraw_list(w_current, other_objects);
        o_cue_draw_list(w_current, other_objects);

        /* get other connected objects and redraw */
        connected_objects = s_conn_return_others(connected_objects, object);
        o_cue_undraw_list(w_current, connected_objects);
        o_cue_draw_list(w_current, connected_objects);

        /* finally redraw the cues on the current object */
        o_cue_draw_single(w_current, object);
        break;

      case(OBJ_BUS):
        o_bus_erase(w_current, object);
        o_line_erase_grips(w_current, object);

        other_objects = s_conn_return_others(other_objects, object);
        s_conn_remove(w_current, object);

        o_bus_mirror_world(w_current, centerx, centery, object);
        s_conn_update_object(w_current, object);
        o_bus_draw(w_current, object);

        /* draw the other objects */
        o_cue_undraw_list(w_current, other_objects);
        o_cue_draw_list(w_current, other_objects);

        /* get other connected objects and redraw */
        connected_objects = s_conn_return_others(connected_objects, object);
        o_cue_undraw_list(w_current, connected_objects);
        o_cue_draw_list(w_current, connected_objects);

        /* finally redraw the cues on the current object */
        o_cue_draw_single(w_current, object);
        break;

      case(OBJ_COMPLEX):
        o_cue_undraw_objects(w_current, object->complex->prim_objs);
        /* erase the current selection */
        o_complex_erase(w_current, object);

        other_objects = s_conn_return_complex_others(other_objects, object);

        /* remove all conn references */
        o_current = object->complex->prim_objs;
        while(o_current != NULL) {
          s_conn_remove(w_current, o_current);
          o_current = o_current->next;
        }

        o_complex_mirror_world(w_current, centerx, centery, object);
        s_conn_update_complex(w_current, object->complex->prim_objs);
        o_complex_draw(w_current, object);

        o_cue_undraw_list(w_current, other_objects);
        o_cue_draw_list(w_current, other_objects);

        /* now draw the newly connected objects */
        connected_objects = s_conn_return_complex_others(connected_objects,
                                                         object);
        o_cue_undraw_list(w_current, connected_objects);
        o_cue_draw_list(w_current, connected_objects);
        break;

      case(OBJ_LINE):
        o_line_erase_grips(w_current, object);
        o_line_erase(w_current, object);
        o_line_mirror_world(w_current, centerx, centery, object);
        o_line_draw(w_current, object);
        break;

      case(OBJ_BOX):
        o_box_erase_grips(w_current, object);
        o_box_erase(w_current, object);
        o_box_mirror_world(w_current, centerx, centery, object);
        o_box_draw(w_current, object);
        break;

      case(OBJ_PICTURE):
        o_picture_erase_grips(w_current, object);
        o_picture_erase(w_current, object);
        o_picture_mirror_world(w_current, centerx, centery, object);
        o_picture_draw(w_current, object);
        break;

      case(OBJ_CIRCLE):
        o_circle_erase_grips(w_current, object);
        o_circle_erase(w_current, object);
        o_circle_mirror_world(w_current, centerx, centery, object);
        o_circle_draw(w_current, object);
        break;

      case(OBJ_ARC):
        o_arc_erase(w_current, object);
        o_arc_mirror_world(w_current, centerx, centery, object);
        o_arc_draw(w_current, object);
        break;

      case(OBJ_TEXT):
        o_text_erase(w_current, object);
        o_text_mirror_world(w_current,
                      centerx, centery, object);
        o_text_draw(w_current, object);
        break;

    }

    s_current = s_current->next;

  }

  /* All objects were rotated. Do a 2nd pass to run the rotate hooks */
  /* Do not run any hooks for simple objects here, like text, since they
     were rotated in the previous pass, and the selection list can contain
     an object and all its attributes (text) */
  s_current = list;
  while (s_current != NULL) {
    object = (OBJECT *) s_current->data;

    if (!object) {
      fprintf(stderr, _("ERROR: NULL object in o_rotate_90!\n"));
      return;
    }

    switch(object->type) {
      case(OBJ_PIN):
        /* Run the rotate pin hook */
        if (scm_hook_empty_p(mirror_pin_hook) == SCM_BOOL_F &&
            object != NULL) {
          scm_run_hook(rotate_pin_hook,
                       scm_cons(g_make_object_smob(w_current, object),
                                SCM_EOL));
        }
        break;

      case (OBJ_COMPLEX):
        /* Run the rotate pin hook */
        if (scm_hook_empty_p(rotate_component_object_hook) == SCM_BOOL_F &&
            object != NULL) {
          scm_run_hook(mirror_component_object_hook,
                       scm_cons(g_make_object_smob(w_current, object),
                                SCM_EOL));
        }
        break;
    default:
        break;
    }

    s_current = s_current->next;
  }


  w_current->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_hidden_lowlevel(TOPLEVEL *w_current, OBJECT *o_list)
{
  OBJECT *o_current = o_list;

  if (o_current == NULL) {
    return;
  }

  while(o_current != NULL) {
    if (o_current->type == OBJ_TEXT && o_current->visibility == INVISIBLE) {

      /* don't toggle the visibility flag */

      if (w_current->show_hidden_text) {
        /* draw the text object if it hidden  */
        if (o_current->text->prim_objs == NULL) {
          o_text_recreate(w_current, o_current);
        }
        o_text_recalc(w_current, o_current);
        o_text_draw(w_current, o_current);
      } else {
        /* object is hidden and we are now NOT drawing it, so */
        /* get rid of the extra primitive data */
        o_text_recreate(w_current, o_current);
        o_text_recalc(w_current, o_current);
        /* unfortunately, you cannot erase the old visible text here */
        /* because o_text_draw will just return */
      }
    }

    if (o_current->type == OBJ_COMPLEX || o_current->type == OBJ_PLACEHOLDER) {
      o_edit_show_hidden_lowlevel(w_current, o_current->complex->prim_objs);
      o_complex_recalc(w_current, o_current);
    }

    o_current = o_current->next;
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_hidden(TOPLEVEL *w_current, OBJECT *o_list)
{
  /* this function just shows the hidden text, but doesn't toggle it */
  /* this function does not change the CHANGED bit, no real changes are */
  /* made to the schematic */

  /* toggle show_hidden_text variable, which when it is true */
  /* means that hidden text IS drawn */
  w_current->show_hidden_text = !w_current->show_hidden_text;
  i_show_state(w_current, NULL); /* update screen status */

  o_edit_show_hidden_lowlevel(w_current, o_list);
  o_redraw_all_fast(w_current);

  if (w_current->show_hidden_text) {
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
void o_edit_make_visible(TOPLEVEL *w_current, OBJECT *o_list)
{
  /* this function actually changes the visibility flag */
  OBJECT *o_current = NULL;

  if (o_list == NULL)
  return;
  o_current = o_list;

  while(o_current != NULL) {

    if (o_current->type == OBJ_TEXT) {
      if (o_current->visibility == INVISIBLE) {
        o_current->visibility = VISIBLE;

        if (o_current->text->prim_objs == NULL) {
          o_text_recreate(w_current, o_current);
        }

        o_text_draw(w_current, o_current);

        w_current->page_current->CHANGED = 1;
      }
    }
    o_current = o_current->next;
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
int o_edit_find_text(TOPLEVEL * w_current, OBJECT * o_list, char *stext,
                     int descend, int skip)
{

  char *attrib = NULL;
  int count = 0;
  PAGE *parent = NULL;
  char *current_filename = NULL;
  int page_control = 0;
  int pcount = 0;
  int rv;
  int text_screen_height;

  OBJECT *o_current = NULL;

  skiplast = skip;
  o_current = o_list;

  if (o_current == NULL) {
    return 1;
  }

  while (o_current != NULL) {

    if (descend) {
      if (o_current->type == OBJ_COMPLEX) {
        parent = w_current->page_current;
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
              s_hierarchy_down_schematic_single(w_current,
                                                current_filename,
                                                parent,
                                                page_control,
                                                HIERARCHY_NORMAL_LOAD);
            /* o_redraw_all(w_current); */

            rv = o_edit_find_text(w_current,
                                  w_current->page_current->object_head,
                                  stext, descend, skiplast);
            if (!rv) {
              return 0;
            }
            s_page_goto( w_current, parent );
          }
        }
      }
    }

    if (o_current->type == OBJ_TEXT) {
     /* replaced strcmp with strstr to simplify the search */
      if (strstr(o_current->text->string,stext)) {
        if (!skiplast) {
          a_zoom(w_current, ZOOM_FULL, DONTCARE, A_PAN_DONT_REDRAW);
          text_screen_height =
            SCREENabs(w_current, o_text_height(o_current->text->string,
                                               o_current->text->size));
          /* this code will zoom/pan till the text screen height is about */
          /* 50 pixels high, perhaps a future enhancement will be to make */
          /* this number configurable */
          while (text_screen_height < 50) {
            a_zoom(w_current, ZOOM_IN, DONTCARE, A_PAN_DONT_REDRAW);
            text_screen_height =
              SCREENabs(w_current, o_text_height(o_current->text->string,
                                                 o_current->text->size));
          }
          a_pan_general(w_current,
                        o_current->text->x, o_current->text->y,
                        1, 0);

          last_o = o_current;
          break;
        }
        if (last_o == o_current) {
          skiplast = 0;
        }

      } /* if (strstr(o_current->text->string,stext)) */
    } /* if (o_current->type == OBJ_TEXT) */
    o_current = o_current->next;

    if (o_current == NULL) {
      return 1;
    }
  }
  return (o_current == NULL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_hide_specific_text(TOPLEVEL * w_current, OBJECT * o_list,
                               char *stext)
{
  OBJECT *o_current = NULL;

  if (o_list == NULL)
    return;

  o_current = o_list;

  while (o_current != NULL) {

    if (o_current->type == OBJ_TEXT) {
      if (!strncmp(stext, o_current->text->string, strlen(stext))) {
        if (o_current->visibility == VISIBLE) {
          o_current->visibility = INVISIBLE;

          if (o_current->text->prim_objs == NULL) {
            o_text_recreate(w_current, o_current);
          }
          w_current->page_current->CHANGED = 1;
        }
      }
    }
    o_current = o_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
  o_redraw_all(w_current);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_edit_show_specific_text(TOPLEVEL * w_current, OBJECT * o_list,
                               char *stext)
{
  OBJECT *o_current = NULL;

  if (o_list == NULL)
    return;

  o_current = o_list;

  while (o_current != NULL) {

    if (o_current->type == OBJ_TEXT) {
      if (!strncmp(stext, o_current->text->string, strlen(stext))) {
        if (o_current->visibility == INVISIBLE) {
          o_current->visibility = VISIBLE;

          if (o_current->text->prim_objs == NULL) {
            o_text_recreate(w_current, o_current);
          }
          o_text_draw(w_current, o_current);
          w_current->page_current->CHANGED = 1;
        }
      }
    }
    o_current = o_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_update_component(TOPLEVEL *w_current, OBJECT *o_current)
{
  OBJECT *tmp_list, *new_complex;
  ATTRIB *new_attribs, *a_current;
  gboolean is_embedded;
  const CLibSymbol *clib;

  g_return_if_fail (o_current != NULL);

  is_embedded = o_complex_is_embedded (o_current);

  g_assert (o_current->complex_basename != NULL);
  clib = s_clib_get_symbol_by_name (o_current->complex_basename);

  if (clib == NULL) {
    s_log_message (_("Could not find symbol [%s] in library. Update failed.\n"),
                   o_current->complex_basename);
    return;
  }

  /* erase the complex object */
  o_erase_single (w_current, o_current);
  /* delete its connections */
  s_conn_remove_complex (w_current, o_current);
  /* and unselect it */
  o_selection_remove( w_current->page_current->selection_list, o_current);

  /* build a temporary list and add a complex to this list */
  tmp_list = s_basic_init_object ("update component");
  new_complex = o_complex_add (w_current,
                               tmp_list, NULL,
                               OBJ_COMPLEX,
                               WHITE,
                               o_current->complex->x,
                               o_current->complex->y,
                               o_current->complex->angle,
                               o_current->complex->mirror,
                               clib, o_current->complex_basename,
                               1, TRUE);

  /* updating the old complex with data from the new one */
  /* first process the prim_objs: */
  /*   - delete the prim_objs of the old component */
  s_delete_list_fromstart (w_current,
                           o_current->complex->prim_objs);
  /*   - put the prim_objs of the new component in the old one */
  o_current->complex->prim_objs = new_complex->complex->prim_objs;
  /*   - reset the new complex prim_objs */
  new_complex->complex->prim_objs = NULL;

  /* then process the attributes: */
  new_attribs = new_complex->attribs;
  /*   - check each attrib of the new complex */
  a_current = new_attribs ? new_attribs->next : NULL;
  while (a_current != NULL) {
    OBJECT *o_attrib;
    gchar *name, *value;
    char *attrfound;
    g_assert (a_current->object->type == OBJ_TEXT);
    o_attrib_get_name_value (a_current->object->text->string,
                             &name, &value);

    attrfound = o_attrib_search_name_single(o_current, name, NULL);

    /* free these now since they are no longer being used */
    if (name) { g_free(name); }
    if (value) { g_free(value); }

    if (attrfound == NULL) {
      /* attribute with same name not found in old component: */
      /* add new attribute to old component */

      /* make a copy of the attribute object */
      o_list_copy_to (w_current, o_current,
                      a_current->object, NORMAL_FLAG, &o_attrib);
      if (o_current->attribs == NULL) {
        /* object has no attribute list: create it */
        o_current->attribs = add_attrib_head(o_current);
      }
      /* add the attribute to old */
      o_attrib_add (w_current, o_current->attribs, o_attrib);
      /* redraw the attribute object */
      o_redraw_single (w_current, o_attrib);
      /* note: this object is unselected (not added to selection). */
    }
    else
    {
      g_free(attrfound);
    }


    a_current = a_current->next;
  }

  /* finally delete the temp list with the updated complex */
  s_delete_list_fromstart (w_current, tmp_list);

  /* Recalculate the bounds of the object */
  o_complex_recalc(w_current, o_current);

  /* reconnect, re-select and redraw */
  s_conn_update_complex (w_current, o_current->complex->prim_objs);
  o_selection_add( w_current->page_current->selection_list, o_current );
  o_redraw_single (w_current, o_current);

  /* Re-flag as embedded if necessary */
  o_current->complex_embedded = is_embedded;

  /* mark the page as modified */
  w_current->page_current->CHANGED = 1;
  o_undo_savestate (w_current, UNDO_ALL);

}

/*! \brief Do autosave on all pages that are marked.
 *  \par Function Description
 *  Looks for pages with the do_autosave_backup flag activated and
 *  autosaves them.
 *
 *  \param [in] toplevel  The TOPLEVEL object to search for autosave's.
 */
void o_autosave_backups(TOPLEVEL *toplevel)
{
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
        fprintf (stderr, "o_autosave_backups: Can't get the real filename of %s.\n", p_current->page_filename);
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
            struct stat dir_st;
            int result;

            /* Use default permissions */
            saved_umask = umask(0);
            st.st_mode = 0666 & ~saved_umask;
            umask(saved_umask);
            st.st_uid = getuid ();

            result = stat (dirname, &dir_st);

            if (result == 0 && (dir_st.st_mode & S_ISGID))
              st.st_gid = dir_st.st_gid;
            else
              st.st_gid = getgid ();
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

        if (o_save (toplevel, backup_filename)) {

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
