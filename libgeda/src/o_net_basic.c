/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <math.h>

#include <gtk/gtk.h>
#include <libguile.h>

#include "defines.h"
#include "struct.h"
#include "globals.h"
#include "o_types.h"
#include "colors.h"
#include "funcs.h"

#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in]  w_current  The TOPLEVEL object.
 *  \param [in]  line
 *  \param [out] left
 *  \param [out] top
 *  \param [out] right
 *  \param [out] bottom
 */
void world_get_net_bounds(TOPLEVEL *w_current, OBJECT *object, int *left,
                          int *top, int *right, int *bottom)
{
  world_get_line_bounds( w_current, object, left, top, right, bottom );
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *  \param [in]     w_current    The TOPLEVEL object.
 *  \param [in,out] object_list
 *  \param [in]     type
 *  \param [in]     color
 *  \param [in]     x1
 *  \param [in]     y1
 *  \param [in]     x2
 *  \param [in]     y2
 *  \return OBJECT *
 */
OBJECT *o_net_add(TOPLEVEL *w_current, OBJECT *object_list, char type,
		  int color, int x1, int y1, int x2, int y2)
{
  int left, right, top, bottom;
  OBJECT *new_node;

  new_node = s_basic_init_object("net");
  new_node->type = type;
  new_node->color = color;

  new_node->line = (LINE *) g_malloc(sizeof(LINE));
  /* check for null */

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;
  new_node->line_width = NET_WIDTH;

  world_get_net_bounds(w_current, new_node, &left, &top, &right,
                 &bottom);

  new_node->w_left = left;
  new_node->w_top = top;
  new_node->w_right = right;
  new_node->w_bottom = bottom;

  new_node->draw_func = net_draw_func;
  new_node->sel_func = select_func;

  object_list = (OBJECT *) s_basic_link_object(new_node, object_list);


  if (!w_current->ADDING_SEL) {
    s_tile_add_object(w_current, object_list,
		      new_node->line->x[0], new_node->line->y[0],
		      new_node->line->x[1], new_node->line->y[1]);
    s_conn_update_object(w_current, object_list);
  }

  return (object_list);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
void o_net_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
  int left, right, top, bottom;

  if (o_current == NULL) {
    return;
  }

  if (o_current->line == NULL) {
    return;
  }

  world_get_net_bounds(w_current, o_current, &left, &top, &right,
                 &bottom);

  o_current->w_left = left;
  o_current->w_top = top;
  o_current->w_right = right;
  o_current->w_bottom = bottom;


}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
OBJECT *o_net_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[],
		   unsigned int release_ver, unsigned int fileformat_ver)
{
  char type;
  int x1, y1;
  int x2, y2;
  int d_x1, d_y1;
  int d_x2, d_y2;
  int color;

  sscanf(buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color);
  d_x1 = x1;
  d_y1 = y1;
  d_x2 = x2;
  d_y2 = y2;

  if (x1 == x2 && y1 == y2) {
    fprintf(stderr, "Found a zero length net [ %c %d %d %d %d %d ]\n",
            type, x1, y1, x2, y2, color);
    s_log_message("Found a zero length net [ %c %d %d %d %d %d ]\n",
                  type, x1, y1, x2, y2, color);
  }


  if (w_current->override_net_color != -1) {
    color = w_current->override_net_color;
  }

  if (color < 0 || color > MAX_COLORS) {
    fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
    s_log_message("Found an invalid color [ %s ]\n", buf);
    s_log_message("Setting color to WHITE\n");
    color = WHITE;
  }

  object_list =
  o_net_add(w_current, object_list, type, color, d_x1, d_y1, d_x2,
            d_y2);
  return (object_list);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
char *o_net_save(OBJECT *object)
{
  int x1, x2, y1, y2;
  int color;
  char *buf;

  x1 = object->line->x[0];
  y1 = object->line->y[0];
  x2 = object->line->x[1];
  y2 = object->line->y[1];

  /* Use the right color */
  if (object->saved_color == -1) {
    color = object->color;
  } else {
    color = object->saved_color;
  }

  buf = g_strdup_printf("%c %d %d %d %d %d", object->type, x1, y1, x2, y2, color);
  return (buf);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
void o_net_translate_world(TOPLEVEL *w_current, int x1, int y1,
			   OBJECT *object)
{
  int left, right, top, bottom;

  if (object == NULL)
  printf("ntw NO!\n");


  /* Update world coords */
  object->line->x[0] = object->line->x[0] + x1;
  object->line->y[0] = object->line->y[0] + y1;
  object->line->x[1] = object->line->x[1] + x1;
  object->line->y[1] = object->line->y[1] + y1;

  /* Update bounding box */
  world_get_net_bounds(w_current, object, &left, &top, &right, &bottom);

  object->w_left = left;
  object->w_top = top;
  object->w_right = right;
  object->w_bottom = bottom;

  s_tile_update_object(w_current, object);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
OBJECT *o_net_copy(TOPLEVEL *w_current, OBJECT *list_tail,
		   OBJECT *o_current)
{
  OBJECT *new_obj;
  ATTRIB *a_current;
  int color;

  if (o_current->saved_color == -1) {
    color = o_current->color;
  } else {
    color = o_current->saved_color;
  }

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = o_net_add(w_current, list_tail, OBJ_NET, color,
                      o_current->line->x[0], o_current->line->y[0],
                      o_current->line->x[1], o_current->line->y[1]);

  new_obj->line->x[0] = o_current->line->x[0];
  new_obj->line->y[0] = o_current->line->y[0];
  new_obj->line->x[1] = o_current->line->x[1];
  new_obj->line->y[1] = o_current->line->y[1];

  a_current = o_current->attribs;

  if (a_current) {
    while (a_current) {

      /* head attrib node has prev = NULL */
      if (a_current->prev != NULL) {
        a_current->copied_to = new_obj;
      }
      a_current = a_current->next;
    }
  }

  return (new_obj);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
void o_net_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
		 int origin_x, int origin_y)
{
  int offset, offset2;
  int cross, net_width;
  int x1, y1;
  int x2, y2;

  if (o_current == NULL) {
    printf("got null in o_net_print\n");
    return;
  }

  offset = 7 * 6;
  offset2 = 7;

  cross = offset;

  if (w_current->print_color) {
    f_print_set_color(fp, o_current->color);
  }

  net_width = 2;
  if (w_current->net_style == THICK) {
    net_width = NET_WIDTH;
  }

  x1 = o_current->line->x[0] - origin_x,
  y1 = o_current->line->y[0] - origin_y;
  x2 = o_current->line->x[1] - origin_x,
  y2 = o_current->line->y[1] - origin_y;

  fprintf(fp, "%d %d %d %d %d line\n", x1,y1,x2,y2,net_width);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
void o_net_rotate_world(TOPLEVEL *w_current,
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int newx, newy;

  if (angle == 0)
  return;

  /* translate object to origin */
  o_net_translate_world(w_current, -world_centerx, -world_centery,
                        object);

  rotate_point_90(object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  rotate_point_90(object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  o_net_translate_world(w_current, world_centerx, world_centery, object);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
void o_net_mirror_world(TOPLEVEL *w_current, int world_centerx,
			int world_centery, OBJECT *object)
{
  /* translate object to origin */
  o_net_translate_world(w_current, -world_centerx, -world_centery,
                        object);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  o_net_translate_world(w_current, world_centerx, world_centery, object);
}

int o_net_orientation(OBJECT *object)
{
    if (object->line->y[0] == object->line->y[1]) {
	return (HORIZONTAL);
    }

    if (object->line->x[0] == object->line->x[1]) {
	return (VERTICAL);
    }

    return (NEITHER);
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * this function does the actual work of making one net segment out of two
 * connected segments
 * The second object (del_object) is the object that should be deleted
 */
void o_net_consolidate_lowlevel(OBJECT *object, OBJECT *del_object,
				int orient)
{
  int temp1, temp2;
  int final1, final2;
  int changed = 0;
  ATTRIB *tail;

#if DEBUG
  printf("o %d %d %d %d\n", object->line->x[0], object->line->y[0],
         object->line->x[1], object->line->y[1]);
  printf("d %d %d %d %d\n", del_object->line->x[0],
         del_object->line->y[0], del_object->line->x[1],
         del_object->line->y[1]);
#endif


  if (orient == HORIZONTAL) {

    temp1 = min(object->line->x[0], del_object->line->x[0]);
    temp2 = min(object->line->x[1], del_object->line->x[1]);

    final1 = min(temp1, temp2);

    temp1 = max(object->line->x[0], del_object->line->x[0]);
    temp2 = max(object->line->x[1], del_object->line->x[1]);

    final2 = max(temp1, temp2);

    object->line->x[0] = final1;
    object->line->x[1] = final2;
    changed = 1;
  }

  if (orient == VERTICAL) {
    temp1 = min(object->line->y[0], del_object->line->y[0]);
    temp2 = min(object->line->y[1], del_object->line->y[1]);

    final1 = min(temp1, temp2);

    temp1 = max(object->line->y[0], del_object->line->y[0]);
    temp2 = max(object->line->y[1], del_object->line->y[1]);

    final2 = max(temp1, temp2);

    object->line->y[0] = final1;
    object->line->y[1] = final2;
    changed = 1;
  }
#if DEBUG
  printf("fo %d %d %d %d\n", object->line->x[0], object->line->y[0],
         object->line->x[1], object->line->y[1]);
#endif

  if (changed) {

    /* first check for attributes */
    if (del_object->attribs) {
#if DEBUG
      printf("yeah... del object has attributes\n");
      printf("reconnecting them to the right place\n");
#endif
      if (object->attribs) {

#if DEBUG
        printf("object DID have attributes\n");
#endif


        /* NEWSEL, this corrupts the selection / object_head badly */
        /* fix it, because you can't just go around deleting objects */
        /* this whole net conslidate needs to be re thought.. since you */
        /* don't really del the del_object */
#if 0
        printf("object->attribs\n");
        o_attrib_print(object->attribs);
        printf("--\n");
        printf("del_object->attribs\n");
        o_attrib_print(del_object->attribs);
        printf("--\n");
#endif
        tail = o_attrib_return_tail(object->attribs);

        /* skip over old attrib head */
        tail->next = del_object->attribs->next;

        /* step prev object to point to last object */
        if (tail->next) {
          tail->next->prev = tail;
        }

        /* delete old attrib head */
        /* and nothing else */
        del_object->attribs->object = NULL;
        del_object->attribs->next = NULL;
        del_object->attribs->prev = NULL;
        o_attrib_delete(del_object->attribs);

        /* you don't need to free the attribs list */
        /* since it's been relinked into object's */
        /* attribs list */

        del_object->attribs = NULL;
#if 0
        printf("\n\nfinal object->attribs\n");
        o_attrib_print(object->attribs);
        printf("--\n");
#endif

      } else {

#if DEBUG
        printf("object didn't have any attributes\n");
#endif
        object->attribs = del_object->attribs;
        /*! \todo what should this be? */
        object->attribs->prev = NULL;

        /* setup parent attribute */
        object->attribs->object = object;

        /* you don't need to free the attribs list */
        /* since it's been used by object */

        del_object->attribs = NULL;
      }
    }
  }

}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 * check to see if this connection also causes a midpoint
 * if so, return false, else return true
 */
int o_net_consolidate_nomidpoint(OBJECT *object, int x, int y)
{
  GList *c_current;
  CONN *conn;

  c_current = object->conn_list;
  while(c_current != NULL) {
    conn = (CONN *) c_current->data;
    if (conn->other_object) {
      if (conn->other_object->sid != object->sid &&
          conn->x == x && conn->y == y &&
          conn->type == CONN_MIDPOINT) {
#if DEBUG        
        printf("Found one! %s\n", conn->other_object->name); 
#endif         
        return(FALSE);
      }
    }
    
    c_current = c_current->next;
  }

  return(TRUE);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
int o_net_consolidate_segments(TOPLEVEL *w_current, OBJECT *object)
{
  int object_orient;
  int other_orient;
  GList *c_current;
  CONN *conn;
  OBJECT *other_object;
  int changed = 0;
  int reselect_new=FALSE;
  
  if (object == NULL) {
    return(0);
  }

  if (object->type != OBJ_NET) {
    return(0);
  }

  object_orient = o_net_orientation(object);

  /*screen_x = object->line->screen_x[j];
    screen_y = object->line->screen_y[i];*/

  c_current = object->conn_list;
  while(c_current != NULL) {
    conn = (CONN *) c_current->data;
    other_object = conn->other_object;

    /* only look at end points which have a valid end on the other side */
    if (other_object != NULL && conn->type == CONN_ENDPOINT &&
        conn->other_whichone != -1 && conn->whichone != -1 &&
        o_net_consolidate_nomidpoint(object, conn->x, conn->y) ) {
      
      if (other_object->type == OBJ_NET) {
        other_orient = o_net_orientation(other_object);

        /* - both objects have the same orientation (either vert or horiz) */
        /* - it's not the same object */
        if (object_orient == other_orient &&
            object->sid != other_object->sid &&
            other_orient != NEITHER) {

#if DEBUG          
          printf("consolidating %s to %s\n", object->name, other_object->name);
#endif          
          
          o_net_consolidate_lowlevel(object, other_object, other_orient);

          changed++;
          if (other_object->selected == TRUE ) {
            o_selection_remove(&(w_current->page_current->selection_list), 
                               other_object);
            reselect_new=TRUE;
          }

          if (reselect_new == TRUE) {
            o_selection_remove(&(w_current->page_current->selection_list), 
                               object);
	    o_selection_add(&(w_current->page_current->selection_list), 
			    object);
          }
				
          s_conn_remove(w_current, other_object);
          s_delete(w_current, other_object);
          o_net_recalc(w_current, object);
          s_tile_update_object(w_current, object);
          s_conn_update_object(w_current, object);
          w_current->page_current->object_tail = 	
            return_tail(w_current->page_current->object_head);
          return(-1);
        }
      }
      
    }

    c_current = c_current->next;
  }

  return(0);
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
void o_net_consolidate(TOPLEVEL *w_current)
{
  OBJECT *o_current;
  int status = 0;

  o_current = w_current->page_current->object_head;

  while (o_current != NULL) {

    if (o_current->type == OBJ_NET) {
      status = o_net_consolidate_segments(w_current, o_current);
    }

    if (status == -1) {
      o_current = w_current->page_current->object_head;
      status = 0;
    } else {
      o_current = o_current->next;
    }
  }
}

/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 *
 */
void o_net_modify(TOPLEVEL *w_current, OBJECT *object,
		  int x, int y, int whichone)
{
  int left, right, top, bottom;

  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  world_get_net_bounds(w_current, object, &left, &top, &right, &bottom);

  object->w_left = left;
  object->w_top = top;
  object->w_right = right;
  object->w_bottom = bottom;

  s_tile_update_object(w_current, object);
}
