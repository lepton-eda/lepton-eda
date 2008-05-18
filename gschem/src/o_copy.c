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
#include <string.h>

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  if (geda_list_get_glist( toplevel->page_current->selection_list ) != NULL) {

  /* This is commented out since it breaks the copy of objects.  See below. */
#if 0
    /* Save the current state. When rotating the selection when copying,
       we have to come back to here */
    o_undo_savestate(w_current, UNDO_ALL);
#endif

    w_current->first_wx = w_current->second_wx = w_x;
    w_current->first_wy = w_current->second_wy = w_y;
    o_copy_rubbercopy_xor (w_current, TRUE);
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_end(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  SELECTION *temp_list = o_selection_new();
  GList *s_current = NULL;
  GList *new_objects = NULL;
  GList *connected_objects=NULL;
  OBJECT *new_object = NULL;
  OBJECT *complex_object = NULL;
  OBJECT *new_objects_head = NULL;
  OBJECT *object;
  int diff_x, diff_y;
  int color;
  /* int redraw_state;  not needed for now */

  object = o_select_return_first_object(w_current);

  if (object == NULL) {
    /*! \todo error condition */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }

  diff_x = w_current->second_wx - w_current->first_wx;
  diff_y = w_current->second_wy - w_current->first_wy;

  /* erase the bounding box */
  o_copy_rubbercopy_xor (w_current, FALSE);

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  new_objects_head = s_basic_init_object("object_head");

  while(s_current != NULL) {

    object = (OBJECT *) s_current->data;

    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object in o_copy_end!\n"));
      exit(-1);
    }

    switch(object->type) {

      case(OBJ_NET):

        /* ADDING_SEL is a bad name, rename hack */
        /* basically I don't want to add the */
        /* connections till much later */
        toplevel->ADDING_SEL=1;
        new_object = (OBJECT *) o_net_copy( toplevel,
                                            return_tail(new_objects_head),
                                            object );
        toplevel->ADDING_SEL=0;
        o_translate_world(toplevel, diff_x, diff_y, new_object);

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_net_draw(w_current, new_object);

        s_conn_update_object(toplevel, new_object);
        new_objects = g_list_append(new_objects, new_object);
        connected_objects = s_conn_return_others(connected_objects,
                                                 new_object);
        break;

      case(OBJ_PIN):
        /* ADDING_SEL is a bad name, rename hack */
        /* basically I don't want to add the */
        /* connections till much later */
        toplevel->ADDING_SEL=1;
        new_object = (OBJECT *) o_pin_copy(toplevel,
                                           return_tail(new_objects_head), 
                                           object);
        toplevel->ADDING_SEL=0;
        o_translate_world(toplevel, diff_x, diff_y, new_object);

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_pin_draw(w_current, new_object);

        s_conn_update_object(toplevel, new_object);
        new_objects = g_list_append(new_objects, new_object);
        connected_objects = s_conn_return_others(connected_objects,
                                                 new_object);
        break;

      case(OBJ_BUS):
        /* ADDING_SEL is a bad name, rename hack */
        /* basically I don't want to add the */
        /* connections till much later */
        toplevel->ADDING_SEL=1;
        new_object = (OBJECT *) o_bus_copy(toplevel,
                                           return_tail(new_objects_head),
                                           object);
        toplevel->ADDING_SEL=0;
        o_translate_world(toplevel, diff_x, diff_y, new_object);

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_bus_draw(w_current, new_object);

        s_conn_update_object(toplevel, new_object);
        new_objects = g_list_append(new_objects, new_object);
        connected_objects = s_conn_return_others(connected_objects,
                                                 new_object);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        toplevel->ADDING_SEL=1;
        if (o_complex_is_embedded(object)) {

          new_object = (OBJECT *) 
            o_complex_copy_embedded(toplevel,
                                    return_tail(new_objects_head),
                                    object);

        } else {
          new_object = (OBJECT *) o_complex_copy(toplevel,
                                                 return_tail(new_objects_head),
                                                 object);
        }
        toplevel->ADDING_SEL=0;

        complex_object = new_object;
        o_translate_world(toplevel, diff_x, diff_y, new_object);

        o_selection_add( temp_list, new_object );

        /* NEWSEL: this needs to be fixed too */
        /* this may not be needed anymore? */
        o_attrib_slot_copy(toplevel, object,
                           new_object);
        new_object->saved_color = object->saved_color;
        o_redraw_single(w_current, new_object);

        s_conn_update_complex(toplevel, new_object->complex->prim_objs);
        new_objects = g_list_append(new_objects, new_object);
        connected_objects = s_conn_return_complex_others(connected_objects,
                                                         new_object);
        break;

      case(OBJ_LINE):
        new_object = (OBJECT *) o_line_copy(toplevel,
                                            return_tail(new_objects_head),
                                            object);
        toplevel->ADDING_SEL=1;
        o_translate_world(toplevel, diff_x, diff_y, new_object);
        toplevel->ADDING_SEL=0;

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_line_draw(w_current, new_object);
        break;

      case(OBJ_BOX):
        new_object = (OBJECT *) o_box_copy(toplevel,
                                           return_tail(new_objects_head),
                                           object);
        toplevel->ADDING_SEL=1;
        o_translate_world(toplevel, diff_x, diff_y, new_object);
        toplevel->ADDING_SEL=0;

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_box_draw(w_current, new_object);

        break;

      case(OBJ_PICTURE):
        new_object = (OBJECT *) o_picture_copy(toplevel,
                                               return_tail(new_objects_head),
                                               object);
        toplevel->ADDING_SEL=1;
        o_translate_world(toplevel, diff_x, diff_y, new_object);
        toplevel->ADDING_SEL=0;

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_picture_draw(w_current, new_object);

        break;

      case(OBJ_CIRCLE):
        new_object = (OBJECT *) o_circle_copy(toplevel,
                                              return_tail(new_objects_head),
                                              object);
        toplevel->ADDING_SEL=1;
        o_translate_world(toplevel, diff_x, diff_y, new_object);
        toplevel->ADDING_SEL=0;

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_circle_draw(w_current, new_object);
        break;

      case(OBJ_ARC):
        new_object = (OBJECT *) o_arc_copy( toplevel,
                                            return_tail(new_objects_head),
                                            object );
        toplevel->ADDING_SEL=1;
        o_translate_world(toplevel, diff_x, diff_y, new_object);
        toplevel->ADDING_SEL=0;

        o_selection_add( temp_list, new_object );
        new_object->saved_color = object->saved_color;
        o_arc_draw(w_current, new_object);
        break;

    }

  /* Store a reference in the copied object to where it was copied.
   * Used to retain associations when copying attributes */
  object->copied_to = new_object;

    toplevel->page_current->object_tail =
      (OBJECT *) return_tail(toplevel->page_current->
                             object_head);
    s_current = g_list_next(s_current);
  }

  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  while(s_current != NULL) {

    object = (OBJECT *) s_current->data;

    if (object == NULL) {
      fprintf(stderr, _("ERROR: NULL object in o_copy_end!\n"));
      exit(-1);
    }

    switch(object->type) {

      case(OBJ_TEXT):
        toplevel->ADDING_SEL=1;
        new_object = (OBJECT *) o_text_copy(toplevel,
                                            return_tail(new_objects_head),
                                            object);
        toplevel->ADDING_SEL=0;

	/* this is also okay NEWSEL new_obj is single */
        if (object->attached_to) {
          if (object->attached_to->copied_to) {
            o_attrib_attach(toplevel, new_objects_head,
                            new_object, object->attached_to-> copied_to);

            /*! \todo I have no idea if this is
               really needed.... ? */
#if 0
            o_attrib_slot_update(
                                 w_current,
                                 object->attached_to->copied_to);
#endif
          }
        }
        toplevel->ADDING_SEL=1;
        o_translate_world(toplevel, diff_x, diff_y, new_object);
        toplevel->ADDING_SEL=0;

        /* old object was attr */
        if (!new_object->attribute &&
            object->attribute) {
          new_object->color = toplevel->detachedattr_color;
          o_complex_set_color(new_object, new_object->color);
          new_object->visibility = VISIBLE;
          color = new_object->color;
        } else {
          color = object->saved_color;
        }

        o_selection_add( temp_list, new_object );
        new_object->saved_color = color;

        /* signify that object is no longer an attribute */
        o_text_draw(w_current, new_object);

        o_complex_set_saved_color_only( new_object->text->prim_objs, 
                                        color);
        break;
    }

    toplevel->page_current->object_tail =
      (OBJECT *) return_tail( toplevel->page_current->object_head );
    s_current = g_list_next(s_current);
  }

  /* Clean up dangling ATTRIB.copied_to pointers */
  s_current = geda_list_get_glist( toplevel->page_current->selection_list );
  while(s_current != NULL) {
    object = s_current->data;
    object->copied_to = NULL;
    s_current = g_list_next (s_current);
  }

  /* This is commented out since it breaks the copy of objects.  */
  /* Required connection information is thrown away for some reason */
  /* Of course, commenting this out, will probably break the rotation */
  /* that this supported. */
#if 0 
  /* Go back to the state before copying, to restore possible rotations
     of the selection */
  redraw_state = toplevel->DONT_REDRAW;
  toplevel->DONT_REDRAW = 0;
  o_undo_callback(w_current, UNDO_ACTION);
  toplevel->DONT_REDRAW = redraw_state;
#endif

  /* Add the new objects */
  toplevel->page_current->object_tail = (OBJECT *)
    return_tail(toplevel->page_current->object_head);

  s_basic_link_object(new_objects_head, toplevel->page_current->object_tail);

  /* Run the copy component hook */
  object = new_objects_head->next;
  while (object != NULL) {
    if ( (object->type == OBJ_COMPLEX) &&
         (scm_hook_empty_p(copy_component_hook) == SCM_BOOL_F)) {
      scm_run_hook(copy_component_hook,
                   scm_cons (g_make_attrib_smob_list(w_current, object),
                   SCM_EOL));
    }
    object = object->next;
  }

  /* And redraw them */
  object = new_objects_head;
  while (object) {
    o_redraw_single(w_current, object);
    object=object->next;
  }

  /* Delete the new object head */
  /*  new_objects_head->next = NULL;
      s_delete_list_fromstart(toplevel, new_objects_head); */

  toplevel->page_current->object_tail = (OBJECT *)
  return_tail(toplevel->page_current->object_head);

  o_select_unselect_all( w_current );
  geda_list_add_glist( toplevel->page_current->selection_list, geda_list_get_glist( temp_list ) );

  g_object_unref( temp_list );

  toplevel->page_current->CHANGED = 1;

  /* not needed o_redraw(w_current, toplevel->page_current->object_head); */
  o_cue_draw_list(w_current, new_objects);
  o_cue_undraw_list(w_current, connected_objects);
  o_cue_draw_list(w_current, connected_objects);

  g_list_free(new_objects);
  g_list_free(connected_objects);
  new_objects = NULL;
  connected_objects = NULL;

  o_undo_savestate(w_current, UNDO_ALL);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_rubbercopy (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  o_copy_rubbercopy_xor (w_current, FALSE);
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  o_copy_rubbercopy_xor (w_current, TRUE);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_copy_rubbercopy_xor (GSCHEM_TOPLEVEL *w_current, int drawing)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  o_drawbounding (w_current,
                  geda_list_get_glist (toplevel->page_current->selection_list),
                  x_get_darkcolor (w_current->bb_color), drawing);
}
