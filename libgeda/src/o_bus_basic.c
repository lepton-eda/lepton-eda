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

/*! \file o_bus_basic.c 
 *  \brief functions for the bus object
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief get the position of the first bus point
 *  \par Function Description
 *  This function gets the position of the first point of a bus object.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean o_bus_get_position (TOPLEVEL *toplevel, gint *x, gint *y,
                              OBJECT *object)
{
  return o_line_get_position(toplevel, x, y, object);
}

/*! \brief calculate and return the boundaries of a bus object
 *  \par Function Description
 *  This function calculates the object boudaries of a bus \a object.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object    a bus object
 *  \param [out] left      the left world coord
 *  \param [out] top       the top world coord
 *  \param [out] right     the right world coord
 *  \param [out] bottom    the bottom world coord
 */
void world_get_bus_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left, int *top,
			  int *right, int *bottom)
{
  world_get_line_bounds( toplevel, object, left, top, right, bottom );
}

/*! \brief create a new bus object
 *  \par Function Description
 *  This function creates and returns a new bus object.
 *  
 *  \param [in]     toplevel    The TOPLEVEL object.
 *  \param [in]     type        The OBJECT type (usually OBJ_BUS)
 *  \param [in]     color       The color of the bus
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \param [in]  bus_ripper_direction direction of the bus rippers
 *  \return A new bus OBJECT
 */
OBJECT *o_bus_new(TOPLEVEL *toplevel,
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

  return new_node;
}

/*! \brief recalc the visual properties of a bus object
 *  \par Function Description
 *  This function updates the visual coords of the \a o_current object.
 *  
 *  \param [in]     toplevel    The TOPLEVEL object.
 *  \param [in]     o_current   a bus object.
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
  o_current->w_bounds_valid = TRUE;
}

/*! \brief read a bus object from a char buffer
 *  \par Function Description
 *  This function reads a bus object from the buffer \a buf.
 *  If the bus object was read successfully, a new bus object is
 *  allocated and appended to the \a object_list.
 *  
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list, or NULL on error.
 */
OBJECT *o_bus_read (TOPLEVEL *toplevel, char buf[],
                    unsigned int release_ver, unsigned int fileformat_ver, GError **err)
{
  OBJECT *new_obj;
  char type; 
  int x1, y1;
  int x2, y2;
  int color;
  int ripper_dir;

  if (release_ver <= VERSION_20020825) {
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_READ, _("Failed to parse bus object\n"));
      return NULL;
    }
    ripper_dir = 0;
  } else {
    if (sscanf (buf, "%c %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color,
		&ripper_dir) != 7) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_READ, _("Failed to parse bus object\n"));
      return NULL;
    }
  }

  if (x1 == x2 && y1 == y2) {
    s_log_message (_("Found a zero length bus [ %c %d %d %d %d %d ]\n"),
                    type, x1, y1, x2, y2, color);
  }

  if (toplevel->override_bus_color != -1) {
    color = toplevel->override_bus_color;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message (_("Found an invalid color [ %s ]\n"), buf);
    s_log_message (_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
  }

  if (ripper_dir < -1 || ripper_dir > 1) {
    s_log_message (_("Found an invalid bus ripper direction [ %s ]\n"), buf);
    s_log_message (_("Resetting direction to neutral (no direction)\n"));
    ripper_dir = 0;
  }

  new_obj = o_bus_new (toplevel, type, color, x1, y1, x2, y2, ripper_dir);

  return new_obj;
}

/*! \brief Create a string representation of the bus object
 *  \par Function Description
 *  This function takes a bus \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] toplevel  a TOPLEVEL structure
 *  \param [in] object  a bus OBJECT
 *  \return the string representation of the bus OBJECT
 */
char *o_bus_save(TOPLEVEL *toplevel, OBJECT *object)
{
  int x1, x2, y1, y2;
  char *buf;

  x1 = object->line->x[0];
  y1 = object->line->y[0];
  x2 = object->line->x[1];
  y2 = object->line->y[1];

  buf = g_strdup_printf("%c %d %d %d %d %d %d", object->type,
          x1, y1, x2, y2, object->color, object->bus_ripper_direction);
  return(buf);
}
       
/*! \brief move a bus object
 *  \par Function Description
 *  This function changes the position of a bus \a object.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] dx           The x-distance to move the object
 *  \param [in] dy           The y-distance to move the object
 *  \param [in] object       The bus OBJECT to be moved
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

/*! \brief create a copy of a bus object
 *  \par Function Description
 *  This function creates a copy of the bus object \a o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] o_current    The object that is copied
 *  \return a new bus object
 */
OBJECT *o_bus_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = o_bus_new (toplevel, OBJ_BUS, o_current->color,
                       o_current->line->x[0], o_current->line->y[0],
                       o_current->line->x[1], o_current->line->y[1],
                       o_current->bus_ripper_direction);

  return new_obj;
}

/*! \brief postscript print command for a bus object
 *  \par Function Description
 *  This function writes the postscript command of the bus object \a o_current
 *  into the FILE \a fp points to.
 *  
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] fp           pointer to a FILE structure
 *  \param [in] o_current    The OBJECT to print
 *  \param [in] origin_x     x-coord of the postscript origin
 *  \param [in] origin_y     y-coord of the postscript origin
 */
void o_bus_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		 int origin_x, int origin_y)
{
  int bus_width;
  int x1, y1;
  int x2, y2;

  if (o_current == NULL) {
    printf("got null in o_bus_print\n");
    return;
  }

  f_print_set_color(toplevel, fp, o_current->color);

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


/*! \brief rotate a bus object around a centerpoint
 *  \par Function Description
 *  This function rotates a bus \a object around the point
 *  (\a world_centerx, \a world_centery).
 *  
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotat the bus object
 *  \param [in] object        The bus object
 *  \note only steps of 90 degrees are allowed for the \a angle
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

/*! \brief mirror a bus object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a bus \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *  
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The bus object
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

/*! \brief calculate the orientation of a bus object
 *  \par Function Description
 *  This function calculates the orientation of a bus object.
 *
 *  \param [in] object   The bus object
 *  \return The orientation: HORIZONTAL, VERTICAL or NEITHER
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
 * This function does the actual work of making one bus segment out of two
 * connected segments.
 * The second object (del_object) is the object that should be deleted.
 *
 * \todo This function is currently not used. Check it before using it
 */
static void o_bus_consolidate_lowlevel (OBJECT *object,
                                        OBJECT *del_object, int orient)
{
  int temp1, temp2;
  int final1, final2;
  int changed=0;
  GList *a_iter;
  OBJECT *a_current;

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
      a_current->attached_to = object;
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
 * \todo Not Implemented Yet
 */
static int o_bus_consolidate_segments (TOPLEVEL *toplevel, OBJECT *object)
{

  return(0);
}

/* \brief
 * \par Function Description
 *
 * \todo Not Implemented Yet 
 */
void o_bus_consolidate(TOPLEVEL *toplevel)
{

}

/*! \brief modify one point of a bus object
 *  \par Function Description
 *  This function modifies one point of a bus \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *  
 *  \param toplevel   The TOPLEVEL object
 *  \param object     The bus OBJECT to modify
 *  \param x          new x-coord of the bus point
 *  \param y          new y-coord of the bus point
 *  \param whichone   bus point to modify
 */
void o_bus_modify(TOPLEVEL *toplevel, OBJECT *object,
		  int x, int y, int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  o_bus_recalc (toplevel, object);

  s_tile_update_object(toplevel, object);
}


