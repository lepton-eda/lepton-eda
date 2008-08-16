/*! \todo No comments found in o_bus_basic.nw
 *        Finish file comments.
 */

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

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* \brief
 * \par Function Description
 *
 */
void world_get_bus_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top,
			  int *right, int *bottom)
{
  world_get_line_bounds( toplevel, object, left, top, right, bottom );
}

/* \brief
 * \par Function Description
 *
 */
OBJECT *o_bus_add(TOPLEVEL *toplevel, OBJECT *object_list,
		  char type, int color,
		  int x1, int y1, int x2, int y2,
		  int bus_ripper_direction)
{
  OBJECT *new_node;

  new_node = s_basic_new_object(type, "bus");
  new_node->color = color;

  new_node->line = (LINE *) g_malloc(sizeof(LINE));
  /* check for null */	

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;
  new_node->line_width = BUS_WIDTH;

  new_node->bus_ripper_direction = bus_ripper_direction;

  o_bus_recalc (toplevel, new_node);

  new_node->draw_func = bus_draw_func;  
  new_node->sel_func = select_func;  

  object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

  s_tile_add_line_object(toplevel, object_list);

  if (!toplevel->ADDING_SEL) {
    s_conn_update_object(toplevel, object_list);
  }

  return(object_list);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, right, top, bottom;

  if (o_current == NULL) {
    return;
  }

  if (o_current->line == NULL) {
    return;
  }

  world_get_bus_bounds(toplevel, o_current, &left, &top, &right, &bottom);

  o_current->w_left = left;
  o_current->w_top = top;
  o_current->w_right = right;
  o_current->w_bottom = bottom;


}

/* \brief
 * \par Function Description
 *
 */
OBJECT *o_bus_read(TOPLEVEL *toplevel, OBJECT *object_list, char buf[],
		   unsigned int release_ver, unsigned int fileformat_ver)
{
  char type; 
  int x1, y1;
  int x2, y2;
  int d_x1, d_y1;
  int d_x2, d_y2;
  int color;
  int ripper_dir;

  if(release_ver <= VERSION_20020825) {
    sscanf(buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color);
    ripper_dir = 0;
  } else {
    sscanf(buf, "%c %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color,
           &ripper_dir);
  }
    
  d_x1 = x1; 
  d_y1 = y1; 
  d_x2 = x2; 
  d_y2 = y2; 

  if (x1 == x2 && y1 == y2) {
    s_log_message(_("Found a zero length bus [ %c %d %d %d %d %d ]\n"),
                    type, x1, y1, x2, y2, color);
  }

  if (toplevel->override_bus_color != -1) {
    color = toplevel->override_bus_color;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message(_("Found an invalid color [ %s ]\n"), buf);
    s_log_message(_("Setting color to WHITE\n"));
    color = WHITE;
  }

  if (ripper_dir < -1 || ripper_dir > 1) {
    s_log_message(_("Found an invalid bus ripper direction [ %s ]\n"), buf);
    s_log_message(_("Resetting direction to neutral (no direction)\n"));
    ripper_dir = 0;
  }

  object_list = o_bus_add(toplevel, object_list, type, color,
                          d_x1, d_y1, d_x2, d_y2, ripper_dir);
  return(object_list);
}

/* \brief
 * \par Function Description
 *
 */
char *o_bus_save(OBJECT *object)
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

  buf = g_strdup_printf("%c %d %d %d %d %d %d", object->type,
          x1, y1, x2, y2, color, object->bus_ripper_direction);
  return(buf);
}
       
/* \brief
 * \par Function Description
 *
 */
void o_bus_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object)
{
  if (object == NULL) printf("btw NO!\n");


  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;

  /* Update bounding box */
  o_bus_recalc (toplevel, object);

  s_tile_update_object(toplevel, object);
}

/* \brief
 * \par Function Description
 *
 */
OBJECT *o_bus_copy(TOPLEVEL *toplevel, OBJECT *list_tail, OBJECT *o_current)
{
  OBJECT *new_obj;
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
  new_obj = o_bus_add(toplevel, list_tail, OBJ_BUS, color,
                      o_current->line->x[0], o_current->line->y[0],
                      o_current->line->x[1], o_current->line->y[1],
                      o_current->bus_ripper_direction);

  new_obj->line->x[0] = o_current->line->x[0];
  new_obj->line->y[0] = o_current->line->y[0];
  new_obj->line->x[1] = o_current->line->x[1];
  new_obj->line->y[1] = o_current->line->y[1];

  return(new_obj);
}

/* \brief
 * \par Function Description
 *
 */
/* need to make this bus specific */
void o_bus_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		 int origin_x, int origin_y)
{
  int offset, offset2;
  int cross, bus_width;
  int x1, y1;
  int x2, y2;

  if (o_current == NULL) {
    printf("got null in o_bus_print\n");
    return;
  }

  offset = 7*6;
  offset2 = 7;  

  cross = offset;

  if (toplevel->print_color) {
    f_print_set_color(fp, o_current->color);
  }

  bus_width = 2;
  if (toplevel->bus_style == THICK) {
    bus_width = BUS_WIDTH;	
  }

  x1 = o_current->line->x[0]-origin_x,
  y1 = o_current->line->y[0]-origin_y;
  x2 = o_current->line->x[1]-origin_x,
  y2 = o_current->line->y[1]-origin_y;

  fprintf(fp, "%d %d %d %d %d line\n",
	  x1,y1,x2,y2,bus_width);

}


/* \brief
 * \par Function Description
 *
 */
void o_bus_rotate_world(TOPLEVEL *toplevel,
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int newx, newy;

  if (angle == 0)
  return;

  /* translate object to origin */
  o_bus_translate_world(toplevel, -world_centerx, -world_centery, object);

  rotate_point_90(object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  rotate_point_90(object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  o_bus_translate_world(toplevel, world_centerx, world_centery, object);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_mirror_world(TOPLEVEL *toplevel,
			int world_centerx, int world_centery, OBJECT *object)
{
  /* translate object to origin */
  o_bus_translate_world(toplevel, -world_centerx, -world_centery, object);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  o_bus_translate_world(toplevel, world_centerx, world_centery, object);
}

/* \brief
 * \par Function Description
 *
 */
int o_bus_orientation(OBJECT *object)
{
  if (object->line->y[0] == object->line->y[1]) {
    return(HORIZONTAL);
  }

  if (object->line->x[0] == object->line->x[1]) {
    return(VERTICAL);
  }

  return(NEITHER);	
}


/* \brief
 * \par Function Description
 *
 */
/* this function does the actual work of making one net segment out of two */
/* connected segments */
/* The second object (del_object) is the object that should be deleted */
/* needs to be bus specific */
void o_bus_consolidate_lowlevel(OBJECT *object, OBJECT *del_object,
				int orient) 
{
  int temp1, temp2;
  int final1, final2;
  int changed=0;
  GList *a_iter;
  ATTRIB *a_current;

#if DEBUG
  printf("o %d %d %d %d\n", object->line->x[0], object->line->y[0], object->line->x[1], object->line->y[1]);
  printf("d %d %d %d %d\n", del_object->line->x[0], del_object->line->y[0], del_object->line->x[1], del_object->line->y[1]);
#endif


  if (orient == HORIZONTAL) {

    temp1 = min(object->line->x[0], 
                del_object->line->x[0]);
    temp2 = min(object->line->x[1], 
                del_object->line->x[1]);

    final1 = min(temp1, temp2);

    temp1 = max(object->line->x[0], 
                del_object->line->x[0]);
    temp2 = max(object->line->x[1], 
                del_object->line->x[1]);

    final2 = max(temp1, temp2);

    object->line->x[0] = final1;
    object->line->x[1] = final2;
    changed=1;
  }

  if (orient == VERTICAL) {
    temp1 = min(object->line->y[0], 
                del_object->line->y[0]);
    temp2 = min(object->line->y[1], 
                del_object->line->y[1]);

    final1 = min(temp1, temp2);

    temp1 = max(object->line->y[0], 
                del_object->line->y[0]);
    temp2 = max(object->line->y[1], 
                del_object->line->y[1]);

    final2 = max(temp1, temp2);

    object->line->y[0] = final1;
    object->line->y[1] = final2;
    changed=1;
  }

#if DEBUG
  printf("fo %d %d %d %d\n", object->line->x[0], object->line->y[0], object->line->x[1], object->line->y[1]);
#endif

  /* Move any attributes from the deleted object*/
  if (changed && del_object->attribs != NULL) {

    /* Reassign the attached_to pointer on attributes from the del object */
    a_iter = del_object->attribs;
    while (a_iter != NULL) {
      a_current = a_iter->data;
      a_current->object->attached_to = object;
      a_iter = g_list_next (a_iter);
    }

    object->attribs = g_list_concat (object->attribs, del_object->attribs);

    /* Don't free del_object->attribs as it's relinked into object's list */
    del_object->attribs = NULL;
  }
}

/* \brief
 * \par Function Description
 *
 */
/* needs to be bus specific */
int o_bus_consolidate_segments(TOPLEVEL *toplevel, OBJECT *object)
{

  return(0);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_consolidate(TOPLEVEL *toplevel)
{

}

/* \brief
 * \par Function Description
 *
 */
void o_bus_modify(TOPLEVEL *toplevel, OBJECT *object,
		  int x, int y, int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  o_bus_recalc (toplevel, object);

  s_tile_update_object(toplevel, object);
}


