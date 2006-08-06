/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2000 Ales V. Hvezda
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

#include <libgeda/libgeda.h>

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
void o_delete_net(TOPLEVEL *w_current, OBJECT *obj)
{
  int removing_sel_save;
  GList *other_objects = NULL;

  o_cue_undraw(w_current, obj);
  o_net_erase(w_current, obj);
  o_line_erase_grips(w_current, obj);

  other_objects = s_conn_return_others(other_objects, obj);
       
  removing_sel_save = w_current->REMOVING_SEL;
  w_current->REMOVING_SEL = 1;
  s_delete(w_current, obj);
  w_current->REMOVING_SEL = removing_sel_save;

  w_current->page_current->object_tail =
  (OBJECT *) return_tail(w_current->page_current->object_head);

  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete_bus(TOPLEVEL *w_current, OBJECT *obj)
{
  int removing_sel_save;
  GList *other_objects = NULL;
        
  o_cue_undraw(w_current, obj);
  o_bus_erase(w_current, obj);
  o_line_erase_grips(w_current, obj);

  other_objects = s_conn_return_others(other_objects, obj);

  removing_sel_save = w_current->REMOVING_SEL;
  w_current->REMOVING_SEL = 1;
  s_delete(w_current, obj);
  w_current->REMOVING_SEL = removing_sel_save;

  w_current->page_current->object_tail =
  (OBJECT *) return_tail(w_current->page_current->object_head);

  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_pin(TOPLEVEL *w_current, OBJECT *obj)
{
  GList *other_objects = NULL;
        
  o_cue_undraw(w_current, obj);
  o_pin_erase(w_current, obj);
  o_line_erase_grips(w_current, obj);
        
  other_objects = s_conn_return_others(other_objects, obj);
        
  s_delete(w_current, obj);
  w_current->page_current->object_tail =
  (OBJECT *) return_tail(w_current->page_current->object_head);

  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete_complex(TOPLEVEL *w_current, OBJECT *obj)
{
  GList *other_objects = NULL;

  o_cue_undraw_complex(w_current, obj);
  o_complex_erase(w_current, obj);

  other_objects = s_conn_return_complex_others(other_objects, obj);

  o_complex_delete(w_current, obj);

  /*! \todo special case hack no return_tail. why? */
  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_line(TOPLEVEL *w_current, OBJECT *obj)
{
  o_line_erase(w_current, obj);
  o_line_erase_grips(w_current, obj);
  
  s_delete(w_current, obj);
  w_current->page_current->object_tail =
    (OBJECT *) return_tail(w_current->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_box(TOPLEVEL *w_current, OBJECT *obj)
{
  o_box_erase(w_current, obj);
  o_box_erase_grips(w_current, obj);

  s_delete(w_current, obj);
  w_current->page_current->object_tail =
  (OBJECT *) return_tail(w_current->page_current->object_head);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_picture(TOPLEVEL *w_current, OBJECT *obj)
{
  o_picture_erase(w_current, obj);
  o_picture_erase_grips(w_current, obj);

  s_delete(w_current, obj);
  w_current->page_current->object_tail =
  (OBJECT *) return_tail(w_current->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_circle(TOPLEVEL *w_current, OBJECT *obj)
{
	o_circle_erase(w_current, obj);
	o_circle_erase_grips(w_current, obj);

	s_delete(w_current, obj);

	w_current->page_current->object_tail =
		(OBJECT *) return_tail(w_current->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete_text(TOPLEVEL *w_current, OBJECT *obj)
{
  o_text_erase(w_current, obj);

  s_delete(w_current, obj);
  w_current->page_current->object_tail =
  (OBJECT *) return_tail(w_current->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_arc(TOPLEVEL *w_current, OBJECT *obj)
{
  o_arc_erase(w_current, obj);

  s_delete(w_current, obj);
  w_current->page_current->object_tail =
  (OBJECT *) return_tail(w_current->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete(TOPLEVEL *w_current)
{
  SELECTION *s_current = NULL;
  OBJECT *object = NULL;

  object = o_select_return_first_object(w_current);
  if (object == NULL) {
    /*! \todo error condition */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }


  /* skip over head node */
  s_current = w_current->page_current->selection2_head->next;

  while(s_current != NULL) {

    object = s_current->selected_object;
    if (object == NULL) {
      fprintf(stderr, 
              _("ERROR: NULL object in o_delete_end!\n"));
      exit(-1);
    }

    switch(object->type) {
      case(OBJ_LINE):
        o_delete_line(w_current, object);
        break;

      case(OBJ_NET):
        o_delete_net(w_current, object);
        break;

      case(OBJ_BUS):
        o_delete_bus(w_current, object);
        break;

      case(OBJ_BOX):
        o_delete_box(w_current, object);
        break;

      case(OBJ_PICTURE):
        o_delete_picture(w_current, object);
        break;

      case(OBJ_CIRCLE):
        o_delete_circle(w_current, object);
        break;

      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        o_delete_complex(w_current, object);
        break;

      case(OBJ_PIN):
        o_delete_pin(w_current, object);
        break;

      case(OBJ_TEXT):
        o_delete_text(w_current, object);
        break;

      case(OBJ_ARC):
        o_delete_arc(w_current, object);
        break;
    }
    s_current = s_current->next;
  }

  w_current->inside_action = 0;

  o_selection_destroy_all(w_current->page_current->selection2_head);
  w_current->page_current->selection2_head = o_selection_new_head();
  w_current->page_current->CHANGED=1;

  /* no longer needed */
  /* o_redraw(w_current, w_current->page_current->object_head);*/

  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}
