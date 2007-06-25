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
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <libgeda/libgeda.h>

#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* No special type for attributes */
/* You can only edit text attributes */

/* be sure in o_copy o_move o_delete you maintain the attributes */
/* delete is a bare, because you will have to unattach the other end */
/* and in o_save o_read as well */
/* and in o_select when selecting objects, select the attributes */

/* there needs to be a modifier (in struct.h, such as a flag) which
 * signifies that this is an attribute */

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *  Copy all attributes select to the selection list.
 *
 *  \todo get a better name
 */
void o_attrib_add_selected(TOPLEVEL *w_current, GList** selection_list_ptr,
			   OBJECT *selected)
{
  ATTRIB *a_current;

  if (!(*selection_list_ptr)) return;

  /* deal with attributes here? */
  if (selected->attribs != NULL) {
    /* first node is head */
    a_current = selected->attribs->next;

    while (a_current != NULL) {

      if (a_current->object) {

				/* make sure object isn't selected already */
        if (a_current->object->saved_color == -1) {
          o_selection_add(selection_list_ptr,
			  /* w_current->page_current->
			     selection2_head,*/
			  a_current->object);
          o_redraw_single(w_current, a_current->object);
        }

      }

      a_current = a_current->next;
    }
  }

  return;
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_attrib_toggle_visibility(TOPLEVEL *w_current, GList *list)
{
  GList *s_current = NULL;
  OBJECT *object = NULL;

  if (list == NULL) {
    return;
  }

  s_current = list;

  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;
    if (object == NULL) {
      fprintf(stderr, _("Got NULL in o_attrib_toggle_visibility\n"));
      exit(-1);
    }

    if (object->type == OBJ_TEXT) {
      if (object->visibility == VISIBLE) {

        /* only erase if we are not showing hidden text */
        if (!w_current->show_hidden_text) {
          o_text_erase(w_current, object);
        }
        
        object->visibility = INVISIBLE;

        if (w_current->show_hidden_text) {
          /* draw text so that little I is drawn */
          o_text_draw(w_current, object); 
        }

        w_current->page_current->CHANGED=1;
      } else {
        /* if we are in the special show hidden mode, then erase text first */
        /* to get rid of the little I */
        if (w_current->show_hidden_text) {
          o_text_erase(w_current, object);
        }

        object->visibility = VISIBLE;
        
        /* you must do this since real->text->complex */
        /* might be null when text is invisible */
        if (object->text->prim_objs == NULL)
          o_text_recreate(w_current, object);

        
        o_text_draw(w_current, object);
        w_current->page_current->CHANGED = 1;
      }
    }
    s_current = s_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_attrib_toggle_show_name_value(TOPLEVEL *w_current, 
				     GList *list, int new_show_name_value)
{
  GList *s_current = NULL;
  OBJECT *object = NULL;

  if (list == NULL) {
    return;
  }

  s_current = list;

  while(s_current != NULL) {
    object = (OBJECT *) s_current->data;

    if (object == NULL) {
      fprintf(stderr, _("Got NULL in o_attrib_toggle_show_name_value\n"));
      exit(-1);
    }

    if (object->type == OBJ_TEXT) {
      o_text_erase(w_current, object);
      object->show_name_value = new_show_name_value;
      o_text_recreate(w_current, object);
      o_text_draw(w_current, object);
      w_current->page_current->CHANGED=1;
    }
    s_current = s_current->next;
  }
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_attrib_start(TOPLEVEL *w_current, int screen_x, int screen_y)
{
  int x, y;
  char *value;

  w_current->last_x = w_current->start_x = fix_x(w_current, screen_x);
  w_current->last_y = w_current->start_y = fix_y(w_current, screen_y);

  w_current->last_drawb_mode = -1;

  /* make sure list is null first, so that you don't have a mem leak */
  SCREENtoWORLD(w_current,
                w_current->start_x,
                w_current->start_y,
                &x,
                &y);

  /* remove the old attrib list if it exists */
  /* without killing the head structure */
  o_list_delete_rest(w_current,
                     w_current->page_current->attrib_place_head);

  /*! \todo change this so that it only changes the value not the
   * whole thing */
  /* attribute names are case sensitive */

  value = strstr(w_current->current_attribute, "=");

  if (value == NULL) {
    fprintf(stderr,
            _("ERROR! you can't get an attribute without an ='s\n"));
    exit(-1);
  }

  switch(w_current->text_caps) {
    case(LOWER):
    string_tolower(value, value);
    break;

    case(UPPER):
    string_toupper(value, value);
    break;

    case(BOTH):
    default:
    /* do nothing */
    break;
  }

  /* here you need to add OBJ_TEXT when it's done */
  w_current->page_current->attrib_place_tail = (OBJECT *)
  o_text_add(
             w_current,
             w_current->page_current->attrib_place_head,
             /* type changed from TEXT to TEXT */
             OBJ_TEXT, w_current->detachedattr_color,
             x,
             y,
             LOWER_LEFT,
             0, /* zero is angle */
             w_current->current_attribute,
             w_current->text_size,
             /* has to be visible so you can place it */
             /* visibility is set when you create the object */
             VISIBLE, w_current->current_show);

  o_drawbounding(w_current,
                 w_current->page_current->attrib_place_head->next,
                 NULL,
                 x_get_darkcolor(w_current->bb_color), TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_attrib_end(TOPLEVEL *w_current)
{
  int world_x, world_y; /* get consistant names hack */
  OBJECT *object;

  SCREENtoWORLD(w_current,
                w_current->last_x,
                w_current->last_y,
                &world_x,
                &world_y);

  world_x = snap_grid(w_current, world_x);
  world_y = snap_grid(w_current, world_y);

  /* here you need to add OBJ_TEXT when it's done */
  /* make this VIS and SHOW default configurable hack */
  w_current->page_current->object_tail =
  o_text_add(w_current, w_current->page_current->object_tail,
				/* type changed from TEXT to TEXT */
             OBJ_TEXT, w_current->detachedattr_color,
             world_x,
             world_y,
             LOWER_LEFT,
             0, /* zero is angle */
             w_current->current_attribute,
             w_current->text_size,
             w_current->current_visible,
             w_current->current_show);

  /* if the attribute is invisible then you need to erase the
   * outline left by the place */
  if (w_current->current_visible == INVISIBLE) {

    o_drawbounding(w_current,
                   w_current->page_current->
                   attrib_place_head->next,
                   NULL,
                   x_get_darkcolor(w_current->bb_color), FALSE);
  }

  /*! \todo you need to erase the bounding box if have that mode
   * set!!! hack */
  /* erase the old bounding box / outline */
  if (w_current->actionfeedback_mode == OUTLINE) {
    o_drawbounding(w_current,
                   w_current->page_current->
                   attrib_place_head->next,
                   NULL,
                   x_get_color(w_current->text_color), FALSE);
  } else {
    o_drawbounding(w_current,
                   w_current->page_current->
                   attrib_place_head->next,
                   NULL,
                   x_get_darkcolor(w_current->select_color), FALSE);
  }

  w_current->override_color = -1;

  (*w_current->page_current->
   object_tail->draw_func)(w_current, w_current->page_current->object_tail);

  w_current->page_current->CHANGED = 1;

  /* here is where you attach the stuff */
  /* if an object is selected, else just place it */
  /* selection_head should never be null since it has a head struct */
  object = (OBJECT *) g_list_first (w_current->page_current->selection_list)->data;
  if (object != NULL) {
    /* should attribute be selected? probably */
    /* this is probably okay, NEWSEL, since tail is a single obj */
    o_attrib_attach(w_current,
                    w_current->page_current->object_head,
                    w_current->page_current->object_tail,
                    object);
  }

  o_selection_add(&(w_current->page_current->selection_list),
		  w_current->page_current->object_tail);
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_attrib_rubberattrib(TOPLEVEL *w_current)
{
  o_drawbounding(w_current,
                 w_current->page_current->attrib_place_head->next,
                 NULL,
                 x_get_darkcolor(w_current->bb_color), FALSE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
/* This function no longer returns NULL, but will always return the new */
/* text item */
OBJECT *o_attrib_add_attrib(TOPLEVEL *w_current,
			    char *text_string, int visibility, 
			    int show_name_value, OBJECT *object)
{
  int world_x = - 1, world_y = -1;
  int color; 
  int left, right, top, bottom;
  OBJECT *o_current;

  color = w_current->detachedattr_color;

  o_current = object;

  /* creating a toplevel or unattached attribute */
  if (o_current) {
    /* get coordinates of where to place the text object */
    switch(o_current->type) {
      case(OBJ_COMPLEX):
      case(OBJ_PLACEHOLDER):
        world_x = o_current->complex->x;
        world_y = o_current->complex->y;
        color = w_current->attribute_color;
        break;

      case(OBJ_ARC):
        world_x = o_current->arc->x;
        world_y = o_current->arc->y;
        color = w_current->attribute_color;
        break;

      case(OBJ_CIRCLE):
        world_x = o_current->circle->center_x;
        world_y = o_current->circle->center_y;
        color = w_current->attribute_color;
        break;

      case(OBJ_BOX):
        world_x = o_current->box->upper_x;
        world_y = o_current->box->upper_y;
        color = w_current->attribute_color;
        break;

      case(OBJ_LINE):
      case(OBJ_NET):
      case(OBJ_PIN):
      case(OBJ_BUS):
        world_x = o_current->line->x[0];
        world_y = o_current->line->y[0];
        color = w_current->attribute_color;
        break;

      case(OBJ_TEXT):

        world_x = o_current->text->x;
        world_y = o_current->text->y;
			
        color = w_current->detachedattr_color;

	o_current = NULL;

#if 0 /* don't error out, instead treat text like another OBJECT, but */
        /* don't attach it anywhere */
        /* s_log_message("Cannot attach attribute to text item\n");*/
        /* return(NULL);*/
#endif
        break;
    }
  } else {
    world_get_object_list_bounds(w_current,
                                 w_current->page_current->object_head,
                                 &left, &top, &right, &bottom);
	
    /* this really is the lower left hand corner */	
    world_x = left; 
    world_y = top;  

    /* printf("%d %d\n", world_x, world_y); */
    color = w_current->detachedattr_color;
  }

  /* first create text item */
  w_current->page_current->object_tail =
  o_text_add(w_current, w_current->page_current->object_tail,
             OBJ_TEXT, color,
             world_x,
             world_y,
             LOWER_LEFT,
             0, /* zero is angle */
             text_string,
             w_current->text_size,  /* current text size */ 
             visibility, show_name_value);

  /* now w_current->page_current->object_tail contains new text item */

  /* now attach the attribute to the object (if o_current is not NULL) */
  /* remember that o_current contains the object to get the attribute */
  if (o_current) {
    o_attrib_attach(w_current, w_current->page_current->object_head,
                    w_current->page_current->object_tail,
                    o_current);
  }

  o_selection_add(&(w_current->page_current->selection_list),
		  w_current->page_current->object_tail);

  o_text_erase(w_current, w_current->page_current->object_tail); 
  o_text_draw(w_current, w_current->page_current->object_tail);

  /* handle slot= attribute, it's a special case */
  if (g_ascii_strncasecmp (text_string, "slot=", 5) == 0) {
    o_slot_end (w_current, text_string, strlen (text_string));
  }

  /* Run the add attribute hook */
  if (scm_hook_empty_p(add_attribute_hook) == SCM_BOOL_F &&
      o_current != NULL) {
	scm_run_hook(add_attribute_hook,
		     scm_cons(g_make_object_smob(w_current, 
						 o_current),
			      SCM_EOL));
      }
  
  w_current->page_current->CHANGED = 1;

  return(w_current->page_current->object_tail);
}
