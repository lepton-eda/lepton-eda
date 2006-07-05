/*! \todo No comments found in o_bus_basic.nw
 *        Finish file comments.
 */
/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <math.h>

#include <gtk/gtk.h>
#include <libguile.h>

#ifdef HAS_LIBGDGEDA
#include <gdgeda/gd.h>
#endif

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

/* \brief
 * \par Function Description
 *
 */
void get_bus_bounds(TOPLEVEL *w_current, LINE *line, int *left, int *top,
		    int *right, int *bottom)
{
  *left = w_current->width;
  *top = w_current->height;
  *right = 0;
  *bottom = 0;

  if (line->screen_x[0] < *left) *left = line->screen_x[0];
  if (line->screen_x[0] > *right) *right = line->screen_x[0];
  if (line->screen_y[0] < *top) *top = line->screen_y[0];
  if (line->screen_y[0] > *bottom) *bottom = line->screen_y[0];

  if (line->screen_x[1] < *left) *left = line->screen_x[1];
  if (line->screen_x[1] > *right) *right = line->screen_x[1];
  if (line->screen_y[1] < *top) *top = line->screen_y[1];
  if (line->screen_y[1] > *bottom) *bottom = line->screen_y[1];

  *left = *left - 4;
  *top = *top - 4;

  *right = *right + 4;
  *bottom = *bottom + 4;
}

/* \brief
 * \par Function Description
 *
 */
void world_get_bus_bounds(TOPLEVEL *w_current, LINE *line, int *left, int *top,
			  int *right, int *bottom)
{
  *left = w_current->init_right;
  *top = w_current->init_bottom;
  *right = 0;
  *bottom = 0;

  if (line->x[0] < *left) *left = line->x[0];
  if (line->x[0] > *right) *right = line->x[0];
  if (line->y[0] < *top) *top = line->y[0];
  if (line->y[0] > *bottom) *bottom = line->y[0];

  if (line->x[1] < *left) *left = line->x[1];
  if (line->x[1] > *right) *right = line->x[1];
  if (line->y[1] < *top) *top = line->y[1];
  if (line->y[1] > *bottom) *bottom = line->y[1];

}

/* \brief
 * \par Function Description
 *
 */
OBJECT *o_bus_add(TOPLEVEL *w_current, OBJECT *object_list,
		  char type, int color,
		  int x1, int y1, int x2, int y2,
		  int bus_ripper_direction)
{
  int screen_x, screen_y;
  int left, right, top, bottom;
  OBJECT *new_node;

  new_node = s_basic_init_object("bus");
  new_node->type = type;
  new_node->color = color;

  new_node->line = (LINE *) malloc(sizeof(LINE));
  /* check for null */	

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;

  WORLDtoSCREEN(w_current, 
                new_node->line->x[0], new_node->line->y[0], 
                &screen_x,
                &screen_y);  
	
  new_node->line->screen_x[0] = screen_x;
  new_node->line->screen_y[0] = screen_y;

  WORLDtoSCREEN(w_current, 
                new_node->line->x[1], new_node->line->y[1], 
                &screen_x,
                &screen_y);  

  new_node->line->screen_x[1] = screen_x;
  new_node->line->screen_y[1] = screen_y;

  new_node->bus_ripper_direction = bus_ripper_direction;

  get_bus_bounds(w_current, new_node->line, &left, &top, &right, &bottom);
	
  new_node->left = left;
  new_node->top = top;
  new_node->right = right;
  new_node->bottom = bottom;	

  /*! \todo questionable cast */
  new_node->draw_func = (void *) bus_draw_func;  
  /*! \todo questionable cast */
  new_node->sel_func = (void *) select_func;  

  object_list = (OBJECT *) s_basic_link_object(new_node, object_list);

  s_tile_add_object(w_current, object_list, 
                    new_node->line->x[0], new_node->line->y[0], 
                    new_node->line->x[1], new_node->line->y[1]);

  if (!w_current->ADDING_SEL) {
    s_conn_update_object(w_current, object_list);
  }

  return(object_list);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_recalc(TOPLEVEL *w_current, OBJECT *o_current)
{
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;	
  int left, right, top, bottom;

  if (o_current == NULL) {
    return;
  }

  if (o_current->line == NULL) {
    return;
  }

  WORLDtoSCREEN(w_current, o_current->line->x[0], 
                o_current->line->y[0], 
                &screen_x1,
                &screen_y1);  

  o_current->line->screen_x[0] = screen_x1;
  o_current->line->screen_y[0] = screen_y1;

  WORLDtoSCREEN(w_current, o_current->line->x[1], 
                o_current->line->y[1], 
                &screen_x2,
                &screen_y2);  

  o_current->line->screen_x[1] = screen_x2;
  o_current->line->screen_y[1] = screen_y2;


  get_bus_bounds(w_current, o_current->line, &left, &top, &right, &bottom);

  o_current->left = left;
  o_current->top = top;
  o_current->right = right;
  o_current->bottom = bottom;


}

/* \brief
 * \par Function Description
 *
 */
OBJECT *o_bus_read(TOPLEVEL *w_current, OBJECT *object_list, char buf[],
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
    fprintf(stderr, "Found a zero length bus [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
    s_log_message("Found a zero length bus [ %c %d %d %d %d %d ]\n", type, x1, y1, x2, y2, color);
  }

  if (w_current->override_bus_color != -1) {
    color = w_current->override_bus_color;
  }

  if (color < 0 || color > MAX_COLORS) {
    fprintf(stderr, "Found an invalid color [ %s ]\n", buf);
    s_log_message("Found an invalid color [ %s ]\n", buf);
    s_log_message("Setting color to WHITE\n");
    color = WHITE;
  }

  if (ripper_dir < -1 || ripper_dir > 1) {
    fprintf(stderr, "Found an invalid bus ripper direction [ %s ]\n", buf);
    s_log_message("Found an invalid bus ripper direction [ %s ]\n", buf);
    s_log_message("Resetting direction to neutral (no direction)\n");
    ripper_dir = 0;
  }

  object_list = o_bus_add(w_current, object_list, type, color,
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
void o_bus_translate(TOPLEVEL *w_current, int dx, int dy, OBJECT *object)
{
  int x, y;

  if (object == NULL) printf("nt NO!\n");


  /* Do world coords */
  object->line->screen_x[0] = object->line->screen_x[0] + dx;
  object->line->screen_y[0] = object->line->screen_y[0] + dy;
  object->line->screen_x[1] = object->line->screen_x[1] + dx;
  object->line->screen_y[1] = object->line->screen_y[1] + dy;

  /* do we want snap grid here? */
  SCREENtoWORLD(w_current, object->line->screen_x[0], 
                object->line->screen_y[0], 
                &x,
                &y);  
	
  object->line->x[0] = snap_grid(w_current, x);
  object->line->y[0] = snap_grid(w_current, y);
	
  SCREENtoWORLD(w_current, object->line->screen_x[1], 
                object->line->screen_y[1], 
                &x,
                &y);  
	
  object->line->x[1] = snap_grid(w_current, x);
  object->line->y[1] = snap_grid(w_current, y);

  s_tile_update_object(w_current, object);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_translate_world(TOPLEVEL *w_current, int x1, int y1, OBJECT *object)
{
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;	
  int left, right, top, bottom;

  if (object == NULL) printf("btw NO!\n");


  /* Do world coords */
  object->line->x[0] = object->line->x[0] + x1;
  object->line->y[0] = object->line->y[0] + y1;
  object->line->x[1] = object->line->x[1] + x1;
  object->line->y[1] = object->line->y[1] + y1;

  /* update screen coords */
  WORLDtoSCREEN(w_current, object->line->x[0], 
                object->line->y[0], 
                &screen_x1,
                &screen_y1);  

  object->line->screen_x[0] = screen_x1;
  object->line->screen_y[0] = screen_y1;

  WORLDtoSCREEN(w_current, object->line->x[1], 
                object->line->y[1], 
                &screen_x2,
                &screen_y2);  

  object->line->screen_x[1] = screen_x2;
  object->line->screen_y[1] = screen_y2;

  /* update bounding box */
  get_bus_bounds(w_current, object->line, &left, &top, &right, &bottom);

  object->left = left;
  object->top = top;
  object->right = right;
  object->bottom = bottom;

  s_tile_update_object(w_current, object);
}

/* \brief
 * \par Function Description
 *
 */
OBJECT *o_bus_copy(TOPLEVEL *w_current, OBJECT *list_tail, OBJECT *o_current)
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
  new_obj = o_bus_add(w_current, list_tail, OBJ_BUS, color, 
                      o_current->line->x[0], o_current->line->y[0],
                      o_current->line->x[1], o_current->line->y[1],
                      o_current->bus_ripper_direction);

  new_obj->line->screen_x[0] = o_current->line->screen_x[0];
  new_obj->line->screen_y[0] = o_current->line->screen_y[0];
  new_obj->line->screen_x[1] = o_current->line->screen_x[1];
  new_obj->line->screen_y[1] = o_current->line->screen_y[1];

  new_obj->line->x[0] = o_current->line->x[0];
  new_obj->line->y[0] = o_current->line->y[0];
  new_obj->line->x[1] = o_current->line->x[1];
  new_obj->line->y[1] = o_current->line->y[1];

  a_current = o_current->attribs;

  if (a_current) {
    while ( a_current ) {

      /* head attrib node has prev = NULL */
      if (a_current->prev != NULL) {
        a_current->copied_to = new_obj;
      }
      a_current = a_current->next;
    }
  }

  return(new_obj);
}

/* \brief
 * \par Function Description
 *
 */
/* need to make this bus specific */
void o_bus_print(TOPLEVEL *w_current, FILE *fp, OBJECT *o_current,
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

  if (w_current->print_color) {
    f_print_set_color(fp, o_current->color);
  }

  bus_width = 2;
  if (w_current->bus_style == THICK) {
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
void o_bus_image_write(TOPLEVEL *w_current, OBJECT *o_current,
		       int origin_x, int origin_y, int color_mode)
{
  int offset, offset2;
  int cross;
  int x1, y1;
  int x2, y2;
  int color;

  if (o_current == NULL) {
    printf("got null in o_bus_image_write\n");
    return;
  }

  if (color_mode == TRUE) {
    color = o_image_geda2gd_color(o_current->color);
  } else {
    color = image_black;
  }

  offset = SCREENabs(w_current, BUS_WIDTH);

  /* 
     offset = 7 * (float) w_current->height/ (float) w_current->width;
     offset2 = 7 * (float) w_current->height/ (float) w_current->width*2;  

     printf("%f %d %d\n", (float) ( (float) w_current->height/ (float) w_current->width), 
     offset, offset2);
  */

  offset2 = offset*2;

  cross = offset;

  x1 = o_current->line->screen_x[0];
  y1 = o_current->line->screen_y[0];
  x2 = o_current->line->screen_x[1];
  y2 = o_current->line->screen_y[1];

  /* assumes screen coords are already calculated correctly */
#ifdef HAS_LIBGDGEDA

  gdImageSetThickness(current_im_ptr, SCREENabs(w_current, BUS_WIDTH));

  gdImageLine(current_im_ptr, x1, y1, x2, y2, color);

#endif

}


/* \brief
 * \par Function Description
 *
 */
/* takes in screen coordinates for the centerx,y, and then does the rotate 
 * in world space */
/* also ignores angle argument... for now, rotate only in 90 degree 
 * increments */
/* fully functional */
void o_bus_rotate(TOPLEVEL *w_current, int centerx, int centery, int angle,
		  OBJECT *object)
{
  int world_centerx, world_centery;
  int newx, newy;

  SCREENtoWORLD(w_current, centerx, centery, 
                &world_centerx,
                &world_centery);  

  /* change the bus ripper orientation when the rotation is 90 degrees */
  /* yes it's okay to use o_net_orientation */
  if (o_net_orientation(object) == VERTICAL && angle == 90) {
    object->bus_ripper_direction = -object->bus_ripper_direction;
  }
  
  /* translate object to origin */
  o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

  rotate_point_90(object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  rotate_point_90(object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  o_bus_translate_world(w_current, world_centerx, world_centery, object);

}

/* \brief
 * \par Function Description
 *
 */
void o_bus_rotate_world(TOPLEVEL *w_current, 
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int newx, newy;

  if (angle == 0)
  return;

  /* translate object to origin */
  o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

  rotate_point_90(object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  rotate_point_90(object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  o_bus_translate_world(w_current, world_centerx, world_centery, object);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_mirror(TOPLEVEL *w_current,
		  int centerx, int centery, OBJECT *object)
{
  int world_centerx, world_centery;

  SCREENtoWORLD(w_current, centerx, centery, 
                &world_centerx,
                &world_centery);  

  /* translate object to origin */
  o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  o_bus_translate_world(w_current, world_centerx, world_centery, object);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_mirror_world(TOPLEVEL *w_current,
			int world_centerx, int world_centery, OBJECT *object)
{
  /* translate object to origin */
  o_bus_translate_world(w_current, -world_centerx, -world_centery, object);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  o_bus_translate_world(w_current, world_centerx, world_centery, object);
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
  ATTRIB *tail;

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

  if (changed) {

    /* first check for attributes */
    if (del_object->attribs) {
      printf("yeah... del object has attributes\n");
      printf("reconnecting them to the right place\n");
      if (object->attribs) {

        printf("object DID have attributes\n");

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
        tail->next->prev = tail; 


				/* delete old attrib head */
				/* and nothing else */
        del_object->attribs->object=NULL;
        del_object->attribs->next=NULL;
        del_object->attribs->prev=NULL;
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

        printf("object didn't have any attributes\n");
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

/* \brief
 * \par Function Description
 *
 */
/* needs to be bus specific */
int o_bus_consolidate_segments(TOPLEVEL *w_current, OBJECT *object)
{

  return(0);
}

/* \brief
 * \par Function Description
 *
 */
void o_bus_consolidate(TOPLEVEL *w_current)
{

}

/* \brief
 * \par Function Description
 *
 */
void o_bus_modify(TOPLEVEL *w_current, OBJECT *object, 
		  int x, int y, int whichone)
{
  int screen_x, screen_y;
  int left, right, top, bottom;

  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  WORLDtoSCREEN(w_current, 
                object->line->x[whichone], 
                object->line->y[whichone], 
                &screen_x, &screen_y);  
	
  object->line->screen_x[whichone] = screen_x;
  object->line->screen_y[whichone] = screen_y;

  get_bus_bounds(w_current, object->line, &left, &top, &right, &bottom);
	
  object->left = left;
  object->top = top;
  object->right = right;
  object->bottom = bottom;	
}


