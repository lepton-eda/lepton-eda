/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <math.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \file o_net_basic.c 
 *  \brief functions for the net object
 */

/*! Default setting for net draw function. */
void (*net_draw_func)() = NULL;

/*! \brief get the position of the first net point
 *  \par Function Description
 *  This function gets the position of the first point of a net object.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean o_net_get_position (TOPLEVEL *toplevel, gint *x, gint *y,
                              OBJECT *object)
{
  return o_line_get_position(toplevel, x, y, object);
}

/*! \brief calculate and return the boundaries of a net object
 *  \par Function Description
 *  This function calculates the object boudaries of a net \a object.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object    a net object
 *  \param [out] left      the left world coord
 *  \param [out] top       the top world coord
 *  \param [out] right     the right world coord
 *  \param [out] bottom    the bottom world coord
 */
void world_get_net_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left,
                          int *top, int *right, int *bottom)
{
  world_get_line_bounds( toplevel, object, left, top, right, bottom );
}

/*! \brief create a new net object
 *  \par Function Description
 *  This function creates and returns a new net object.
 *  
 *  \param [in]     toplevel    The TOPLEVEL object.
 *  \param [in]     type        The OBJECT type (usually OBJ_NET)
 *  \param [in]     color       The color of the net
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \return A new net OBJECT
 */
OBJECT *o_net_new(TOPLEVEL *toplevel, char type,
		  int color, int x1, int y1, int x2, int y2)
{
  OBJECT *new_node;

  new_node = s_basic_new_object(type, "net");
  new_node->color = color;

  new_node->line = (LINE *) g_malloc(sizeof(LINE));
  /* check for null */

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;
  new_node->line_width = NET_WIDTH;

  o_net_recalc (toplevel, new_node);

  new_node->draw_func = net_draw_func;
  new_node->sel_func = select_func;

  return new_node;
}

/*! \brief recalc the visual properties of a net object
 *  \par Function Description
 *  This function updates the visual coords of the \a o_current object.
 *  
 *  \param [in]     toplevel    The TOPLEVEL object.
 *  \param [in]     o_current   a net object.
 *
 */
void o_net_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, right, top, bottom;

  if (o_current == NULL) {
    return;
  }

  if (o_current->line == NULL) {
    return;
  }

  world_get_net_bounds(toplevel, o_current, &left, &top, &right,
                 &bottom);

  o_current->w_left = left;
  o_current->w_top = top;
  o_current->w_right = right;
  o_current->w_bottom = bottom;
  o_current->w_bounds_valid = TRUE;
}

/*! \brief read a net object from a char buffer
 *  \par Function Description
 *  This function reads a net object from the buffer \a buf.
 *  If the netobject was read successfully, a new net object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list
 *
 */
OBJECT *o_net_read (TOPLEVEL *toplevel, char buf[],
                    unsigned int release_ver, unsigned int fileformat_ver)
{
  OBJECT *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int color;

  sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color);

  if (x1 == x2 && y1 == y2) {
    s_log_message (_("Found a zero length net [ %c %d %d %d %d %d ]\n"),
                   type, x1, y1, x2, y2, color);
  }


  if (toplevel->override_net_color != -1) {
    color = toplevel->override_net_color;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message (_("Found an invalid color [ %s ]\n"), buf);
    s_log_message (_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
  }

  new_obj = o_net_new (toplevel, type, color, x1, y1, x2, y2);

  return new_obj;
}

/*! \brief Create a string representation of the net object
 *  \par Function Description
 *  This function takes a net \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] toplevel  a TOPLEVEL structure
 *  \param [in] object  a net OBJECT
 *  \return the string representation of the net OBJECT
 */
char *o_net_save(TOPLEVEL *toplevel, OBJECT *object)
{
  int x1, x2, y1, y2;
  char *buf;

  x1 = object->line->x[0];
  y1 = object->line->y[0];
  x2 = object->line->x[1];
  y2 = object->line->y[1];

  buf = g_strdup_printf("%c %d %d %d %d %d", object->type, x1, y1, x2, y2, object->color);
  return (buf);
}

/*! \brief move a net object
 *  \par Function Description
 *  This function changes the position of a net \a object.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] dx           The x-distance to move the object
 *  \param [in] dy           The y-distance to move the object
 *  \param [in] object       The net OBJECT to be moved
 *
 */
void o_net_translate_world(TOPLEVEL *toplevel, int dx, int dy,
			   OBJECT *object)
{
  if (object == NULL)
    printf("ntw NO!\n");

  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;

  /* Update bounding box */
  o_net_recalc (toplevel, object);

  s_tile_update_object(toplevel, object);
}

/*! \brief create a copy of a net object
 *  \par Function Description
 *  This function creates a copy of the net object \a o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] o_current    The object that is copied
 *  \return a new net object
 */
OBJECT *o_net_copy(TOPLEVEL *toplevel,  OBJECT *o_current)
{
  OBJECT *new_obj;

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = o_net_new (toplevel, OBJ_NET, o_current->color,
                       o_current->line->x[0], o_current->line->y[0],
                       o_current->line->x[1], o_current->line->y[1]);

  return new_obj;
}

/*! \brief postscript print command for a net object
 *  \par Function Description
 *  This function writes the postscript command of the net object \a o_current
 *  into the FILE \a fp points to.
 *  
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] o_current    The OBJECT to print
 *  \param [in] origin_x     x-coord of the postscript origin
 *  \param [in] origin_y     y-coord of the postscript origin
 */
void o_net_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
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

  f_print_set_color(toplevel, fp, o_current->color);

  net_width = 2;
  if (toplevel->net_style == THICK) {
    net_width = NET_WIDTH;
  }

  x1 = o_current->line->x[0] - origin_x,
  y1 = o_current->line->y[0] - origin_y;
  x2 = o_current->line->x[1] - origin_x,
  y2 = o_current->line->y[1] - origin_y;

  fprintf(fp, "%d %d %d %d %d line\n", x1,y1,x2,y2,net_width);
}


/*! \brief rotate a net object around a centerpoint
 *  \par Function Description
 *  This function rotates a net \a object around the point
 *  (\a world_centerx, \a world_centery).
 *  
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotat the net object
 *  \param [in] object        The net object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void o_net_rotate_world(TOPLEVEL *toplevel,
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int newx, newy;

  if (angle == 0)
    return;

  /* translate object to origin */
  o_net_translate_world(toplevel, -world_centerx, -world_centery,
                        object);

  rotate_point_90(object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  rotate_point_90(object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  o_net_translate_world(toplevel, world_centerx, world_centery, object);
}

/*! \brief mirror a net object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a net \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *  
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The net object
 */
void o_net_mirror_world(TOPLEVEL *toplevel, int world_centerx,
			int world_centery, OBJECT *object)
{
  /* translate object to origin */
  o_net_translate_world(toplevel, -world_centerx, -world_centery,
                        object);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  o_net_translate_world(toplevel, world_centerx, world_centery, object);
}

/*! \brief calculate the orientation of a net object
 *  \par Function Description
 *  This function calculates the orientation of a net object.
 *
 *  \param [in] object   The net object
 *  \return The orientation: HORIZONTAL, VERTICAL or NEITHER
 */
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


/*! \brief merge two net object
 *  \par Function Description
 *  This function does the actual work of making one net segment out of two
 *  connected segments. The first net segment is extended to the lenght of 
 *  both objects.
 *  The second object (\a del_object) is the object that should be deleted.
 *  
 *  \param [in] object     A net object to extend
 *  \param [in] del_object A net object to be merged into \a object
 *  \param [in] orient     The orientation of both net objects
 *
 *  \note The first net \a object gets the attributes of the second net 
 *  \a del_object if the two nets are merged together.
 */
static void o_net_consolidate_lowlevel (OBJECT *object,
                                        OBJECT *del_object, int orient)
{
  int temp1, temp2;
  int final1, final2;
  int changed = 0;
  GList *a_iter;
  OBJECT *a_current;

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

  /* Move any attributes from the deleted object*/
  if (changed && del_object->attribs != NULL) {

    /* Reassign the attached_to pointer on attributes from the del object */
    a_iter = del_object->attribs;
    while (a_iter != NULL) {
      a_current = a_iter->data;
      a_current->attached_to = object;
      a_iter = g_list_next (a_iter);
    }

    object->attribs = g_list_concat (object->attribs, del_object->attribs);

    /* Don't free del_object->attribs as it's relinked into object's list */
    del_object->attribs = NULL;
  }
}

/*! \brief Check if there's a midpoint connection at (x,y)
 *  \par Function Description
 *  This function checks if the \a object is connected to another net
 *  between it's endpoints. Net segment's only can be merged if there
 *  is no midpoint connection.
 *  
 *  \param object  a net OBJECT to check
 *  \param x       x-coord of the connection location
 *  \param y       y-coord of the connection location
 *  \return TRUE if there's no midpoint connection, else return FALSE
 */
static int o_net_consolidate_nomidpoint (OBJECT *object, int x, int y)
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
    
    c_current = g_list_next(c_current);
  }

  return(TRUE);
}

/*! \brief try to consolidate a net object
 *  \par Function Description
 *  This function tries to consolidate a net with any other object
 *  that is connected to the current \a object.
 *  
 *  \param toplevel   The TOPLEVEL object
 *  \param object     The object to consolidate
 *  \return 0 if no consolidation was possible, -1 otherwise
 *
 */
static int o_net_consolidate_segments (TOPLEVEL *toplevel, OBJECT *object)
{
  int object_orient;
  int other_orient;
  GList *c_current;
  CONN *conn;
  OBJECT *other_object;
  PAGE *page;
  int changed = 0;

  g_return_val_if_fail ((toplevel != NULL), 0);
  g_return_val_if_fail ((object != NULL), 0);
  g_return_val_if_fail ((object->type == OBJ_NET), 0);

  /* It's meaningless to do anything here if the object isn't in a page. */
  page = o_get_page (toplevel, object);
  g_return_val_if_fail ((page != NULL), 0);

  object_orient = o_net_orientation(object);

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
            o_selection_remove (toplevel, page->selection_list, other_object);

            /* If we're consolidating with a selected object,
             * ensure we select the resulting object.
             */
            if (object->selected == FALSE) {
              o_selection_add (toplevel, page->selection_list, object);
            }
          }

          s_delete_object (toplevel, other_object);
          o_net_recalc(toplevel, object);
          s_tile_update_object(toplevel, object);
          s_conn_update_object (toplevel, object);
          return(-1);
        }
      }
      
    }

    c_current = g_list_next (c_current);
  }

  return(0);
}

/*! \brief consolidate all net objects
 *  \par Function Description
 *  This function consolidates all net objects in a page until no more
 *  consolidations are possible.
 *
 *  \param toplevel  The TOPLEVEL object.
 *  \param page      The PAGE to consolidate nets in.
 */
void o_net_consolidate(TOPLEVEL *toplevel, PAGE *page)
{
  OBJECT *o_current;
  const GList *iter;
  int status = 0;

  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (page != NULL);

  iter = s_page_objects (page);

  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (o_current->type == OBJ_NET) {
      status = o_net_consolidate_segments(toplevel, o_current);
    }

    if (status == -1) {
      iter = s_page_objects (page);
      status = 0;
    } else {
      iter = g_list_next (iter);
    }
  }
}

/*! \brief modify one point of a net object
 *  \par Function Description
 *  This function modifies one point of a net \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *  
 *  \param toplevel   The TOPLEVEL object
 *  \param object     The net OBJECT to modify
 *  \param x          new x-coord of the net point
 *  \param y          new y-coord of the net point
 *  \param whichone   net point to modify
 *
 */
void o_net_modify(TOPLEVEL *toplevel, OBJECT *object,
		  int x, int y, int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  o_net_recalc (toplevel, object);

  s_tile_update_object(toplevel, object);
}
