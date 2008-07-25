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
void o_delete_net(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *other_objects = NULL;

  o_cue_undraw(w_current, obj);

  o_erase_single(w_current, obj);
  o_line_erase_grips(w_current, obj);

  other_objects = s_conn_return_others(other_objects, obj);
       
  s_delete(toplevel, obj);

  toplevel->page_current->object_tail =
    (OBJECT *) return_tail(toplevel->page_current->object_head);

  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete_bus(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *other_objects = NULL;
        
  o_cue_undraw(w_current, obj);
  o_erase_single(w_current, obj);
  o_line_erase_grips(w_current, obj);

  other_objects = s_conn_return_others(other_objects, obj);

  s_delete(toplevel, obj);

  toplevel->page_current->object_tail =
    (OBJECT *) return_tail(toplevel->page_current->object_head);

  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_pin(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *other_objects = NULL;
        
  o_cue_undraw(w_current, obj);
  o_erase_single(w_current, obj);
  o_line_erase_grips(w_current, obj);
        
  other_objects = s_conn_return_others(other_objects, obj);
        
  s_delete(toplevel, obj);
  toplevel->page_current->object_tail =
    (OBJECT *) return_tail(toplevel->page_current->object_head);

  o_cue_undraw_list(w_current, other_objects);
  o_cue_draw_list(w_current, other_objects);
  g_list_free(other_objects);

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete_complex(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *other_objects = NULL;

  o_cue_undraw_complex(w_current, obj);
  o_erase_single(w_current, obj);

  other_objects = s_conn_return_complex_others(other_objects, obj);

  s_delete(toplevel, obj);

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
static void o_delete_line(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  o_erase_single(w_current, obj);
  o_line_erase_grips(w_current, obj);
  
  s_delete(toplevel, obj);
  toplevel->page_current->object_tail =
    (OBJECT *) return_tail(toplevel->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_box(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  o_erase_single(w_current, obj);
  o_box_erase_grips(w_current, obj);

  s_delete(toplevel, obj);
  toplevel->page_current->object_tail =
  (OBJECT *) return_tail(toplevel->page_current->object_head);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_picture(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  o_erase_single(w_current, obj);
  o_picture_erase_grips(w_current, obj);

  s_delete(toplevel, obj);
  toplevel->page_current->object_tail =
  (OBJECT *) return_tail(toplevel->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_circle(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
	o_erase_single(w_current, obj);
	o_circle_erase_grips(w_current, obj);

	s_delete(toplevel, obj);

	toplevel->page_current->object_tail =
		(OBJECT *) return_tail(toplevel->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete_text(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *o_parent = obj->attached_to;

  o_erase_single(w_current, obj);

  s_delete(toplevel, obj);
  toplevel->page_current->object_tail =
    (OBJECT *) return_tail(toplevel->page_current->object_head);

  if (o_parent != NULL && o_parent->type == OBJ_COMPLEX)
    o_attrib_slot_update(toplevel, o_parent);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
static void o_delete_arc(GSCHEM_TOPLEVEL *w_current, OBJECT *obj)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  o_erase_single(w_current, obj);

  s_delete(toplevel, obj);
  toplevel->page_current->object_tail =
  (OBJECT *) return_tail(toplevel->page_current->object_head);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_delete_selected(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  GList *s_current = NULL;
  OBJECT *object = NULL;

  object = o_select_return_first_object(w_current);
  if (object == NULL) {
    /*! \todo error condition */
    w_current->inside_action = 0;
    i_set_state(w_current, SELECT);
    return;
  }


  /* skip over head node */
  s_current = geda_list_get_glist( toplevel->page_current->selection_list );

  while(s_current != NULL) {

    object = (OBJECT *) s_current->data;
    g_assert (object != NULL);

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
    s_current = g_list_next(s_current);
  }

  w_current->inside_action = 0;

  /* Objects in the selection list have been deleted. Empty the list without touching the objects */
  geda_list_remove_all( toplevel->page_current->selection_list );

  toplevel->page_current->CHANGED=1;

  /* no longer needed */
  /* o_redraw(w_current, toplevel->page_current->object_head);*/

  o_undo_savestate(w_current, UNDO_ALL);
  i_update_menus(w_current);
}
