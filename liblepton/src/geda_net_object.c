/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2020 Lepton EDA Contributors
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


/*! \file o_net_basic.c
 *  \brief functions for the net object
 */

/*! \brief get the position of the first net point
 *  \par Function Description
 *  This function gets the position of the first point of a net object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
geda_net_object_get_position (const GedaObject *object, gint *x, gint *y)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->type == OBJ_NET, FALSE);
  g_return_val_if_fail (object->line != NULL, FALSE);

  if (x != NULL) {
    *x = object->line->x[0];
  }

  if (y != NULL) {
    *y = object->line->y[0];
  }

  return TRUE;
}

/*! \brief Get the x coordinate of first endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in] object The line
 *  \return The x coordinate for the first endpoint
 */
gint
geda_net_object_get_x0 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_NET, 0);

  return object->line->x[0];
}

/*! \brief Get the x coordinate of second endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in] object The line
 *  \return The x coordinate for the second endpoint
 */
gint
geda_net_object_get_x1 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_NET, 0);

  return object->line->x[1];
}

/*! \brief Get the y coordinate of first endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in] object The line
 *  \return The y coordinate for the first endpoint
 */
gint
geda_net_object_get_y0 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_NET, 0);

  return object->line->y[0];
}

/*! \brief Get the y coordinate of second endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in] object The line
 *  \return The y coordinate for the second endpoint
 */
gint
geda_net_object_get_y1 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_NET, 0);

  return object->line->y[1];
}

/*! \brief Set the x coordinate of first endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in,out] object The line
 *  \param [in] x The new x coordinate for the first endpoint
 */
void
geda_net_object_set_x0 (GedaObject *object, gint x)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_NET);

  object->line->x[0] = x;
}

/*! \brief Set the x coordinate of second endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in,out] object The line
 *  \param [in] x The new x coordinate for the second endpoint
 */
void
geda_net_object_set_x1 (GedaObject *object, gint x)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_NET);

  object->line->x[1] = x;
}

/*! \brief Set the y coordinate of first endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in,out] object The line
 *  \param [in] y The new y coordinate for the first endpoint
 */
void
geda_net_object_set_y0 (GedaObject *object, gint y)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_NET);

  object->line->y[0] = y;
}

/*! \brief Set the y coordinate of second endpoint
 *
 *  The coordinate properties are broken out individually to make it easier for
 *  the GUI. This way, the GUI does not need as many adapters to interface to
 *  a line boxed type.
 *
 *  \param [in,out] object The line
 *  \param [in] y The new y coordinate for the second endpoint
 */
void
geda_net_object_set_y1 (GedaObject *object, gint y)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_NET);

  object->line->y[1] = y;
}

/*! \brief Calculate the bounds of the net
 *
 *  On failure, this function sets the bounds to empty.
 *
 *  \param [in]  object   The net object
 *  \param [out] bounds   The bounds of the net
 */
void
geda_net_object_calculate_bounds (const OBJECT *object,
                                  GedaBounds *bounds)
{
  gint expand;

  geda_bounds_init (bounds);

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->type == OBJ_NET);
  g_return_if_fail (object->line != NULL);

  geda_line_calculate_bounds (object->line, bounds);

  expand = ceil (0.5 * G_SQRT2 * NET_WIDTH);

  /* This isn't strictly correct, but a 1st order approximation */
  geda_bounds_expand (bounds, bounds, expand, expand);
}

/*! \brief create a new net object
 *  \par Function Description
 *  This function creates and returns a new net object.
 *
 *  \param [in]     type        The OBJECT type (usually OBJ_NET)
 *  \param [in]     color       The color of the net
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \return A new net OBJECT
 */
OBJECT*
geda_net_object_new (char type,
                     int color,
                     int x1,
                     int y1,
                     int x2,
                     int y2)
{
  OBJECT *new_node;

  new_node = s_basic_new_object(type, "net");
  new_node->color = color;

  new_node->line = geda_line_new ();

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;
  new_node->line_width = NET_WIDTH;

  return new_node;
}

/*! \brief read a net object from a char buffer
 *  \par Function Description
 *  This function reads a net object from the buffer \a buf.
 *  If the netobject was read successfully, a new net object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list, or NULL on error.
 *
 */
OBJECT*
o_net_read (const char buf[],
            unsigned int release_ver,
            unsigned int fileformat_ver,
            GError **err)
{
  OBJECT *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int color;

  if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6) {
        g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse net object"));
    return NULL;
  }

  if (x1 == x2 && y1 == y2) {
    s_log_message (_("Found a zero length net "
                     "[ %1$c %2$d %3$d %4$d %5$d %6$d ]"),
                   type, x1, y1, x2, y2, color);
  }

  if (!color_id_valid (color)) {
    s_log_message (_("Found an invalid color [ %1$s ]"), buf);
    s_log_message (_("Setting color to default color."));
    color = DEFAULT_COLOR;
  }

  new_obj = geda_net_object_new (type, color, x1, y1, x2, y2);

  return new_obj;
}

/*! \brief Create a string representation of the net object
 *  \par Function Description
 *  This function takes a net \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a net OBJECT
 *  \return the string representation of the net OBJECT
 */
gchar*
geda_net_object_to_buffer (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->line != NULL, NULL);
  g_return_val_if_fail (object->type == OBJ_NET, NULL);

  return g_strdup_printf ("%c %d %d %d %d %d",
                          OBJ_NET,
                          geda_net_object_get_x0 (object),
                          geda_net_object_get_y0 (object),
                          geda_net_object_get_x1 (object),
                          geda_net_object_get_y1 (object),
                          geda_object_get_color (object));
}

/*! \brief move a net object
 *  \par Function Description
 *  This function changes the position of a net \a object.
 *
 *  \param [ref] object The net GedaObject to be moved
 *  \param [in]  dx     The x-distance to move the object
 *  \param [in]  dy     The y-distance to move the object
 */
void
geda_net_object_translate (GedaObject *object, int dx, int dy)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_NET);

  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;
}

/*! \brief create a copy of a net object
 *  \par Function Description
 *  This function creates a copy of the net object \a o_current.
 *
 *  \param [in] o_current    The object that is copied
 *  \return a new net object
 */
OBJECT*
geda_net_object_copy (OBJECT *o_current)
{
  OBJECT *new_obj;

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = geda_net_object_new (OBJ_NET,
                                 o_current->color,
                                 o_current->line->x[0],
                                 o_current->line->y[0],
                                 o_current->line->x[1],
                                 o_current->line->y[1]);

  return new_obj;
}

/*! \brief rotate a net object around a centerpoint
 *  \par Function Description
 *  This function rotates a net \a object around the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotat the net object
 *  \param [in] object        The net object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void
geda_net_object_rotate (int world_centerx,
                        int world_centery,
                        int angle,
			OBJECT *object)
{
  int newx, newy;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_NET);

  if (angle == 0)
    return;

  /* translate object to origin */
  geda_net_object_translate (object, -world_centerx, -world_centery);

  geda_point_rotate_90 (object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  geda_point_rotate_90 (object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  geda_net_object_translate (object, world_centerx, world_centery);
}

/*! \brief mirror a net object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a net \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The net object
 */
void
geda_net_object_mirror (int world_centerx,
			int world_centery,
                        OBJECT *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_NET);

  /* translate object to origin */
  geda_net_object_translate (object, -world_centerx, -world_centery);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  geda_net_object_translate (object, world_centerx, world_centery);
}

/*! \brief calculate the orientation of a net object
 *  \par Function Description
 *  This function calculates the orientation of a net object.
 *
 *  \param [in] object   The net object
 *  \return The orientation: HORIZONTAL, VERTICAL or NEITHER
 */
int
geda_net_object_orientation (OBJECT *object)
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
 *  connected segments. The first net segment is extended to the length of
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

    temp1 = MIN(object->line->x[0], del_object->line->x[0]);
    temp2 = MIN(object->line->x[1], del_object->line->x[1]);

    final1 = MIN(temp1, temp2);

    temp1 = MAX(object->line->x[0], del_object->line->x[0]);
    temp2 = MAX(object->line->x[1], del_object->line->x[1]);

    final2 = MAX(temp1, temp2);

    object->line->x[0] = final1;
    object->line->x[1] = final2;
    changed = 1;
  }

  if (orient == VERTICAL) {
    temp1 = MIN(object->line->y[0], del_object->line->y[0]);
    temp2 = MIN(object->line->y[1], del_object->line->y[1]);

    final1 = MIN(temp1, temp2);

    temp1 = MAX(object->line->y[0], del_object->line->y[0]);
    temp2 = MAX(object->line->y[1], del_object->line->y[1]);

    final2 = MAX(temp1, temp2);

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
      a_current = (OBJECT*) a_iter->data;
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
 *  \param object     The object to consolidate
 *  \return 0 if no consolidation was possible, -1 otherwise
 *
 */
static int
o_net_consolidate_segments (OBJECT *object)
{
  int object_orient;
  int other_orient;
  GList *c_current;
  CONN *conn;
  OBJECT *other_object;
  PAGE *page;
  int changed = 0;

  g_return_val_if_fail ((object != NULL), 0);
  g_return_val_if_fail ((object->type == OBJ_NET), 0);

  /* It's meaningless to do anything here if the object isn't in a page. */
  page = o_get_page (object);
  g_return_val_if_fail ((page != NULL), 0);

  object_orient = geda_net_object_orientation (object);

  c_current = object->conn_list;
  while(c_current != NULL) {
    conn = (CONN *) c_current->data;
    other_object = conn->other_object;

    /* only look at end points which have a valid end on the other side */
    if (other_object != NULL && conn->type == CONN_ENDPOINT &&
        conn->other_whichone != -1 && conn->whichone != -1 &&
        o_net_consolidate_nomidpoint(object, conn->x, conn->y) ) {

      if (other_object->type == OBJ_NET) {
        other_orient = geda_net_object_orientation (other_object);

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
            o_selection_remove (page->selection_list, other_object);

            /* If we're consolidating with a selected object,
             * ensure we select the resulting object.
             */
            if (object->selected == FALSE) {
              o_selection_add (page->selection_list, object);
            }
          }

          s_delete_object (other_object);
          s_conn_update_object (page, object);
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
 *  \param page      The PAGE to consolidate nets in.
 */
void
geda_net_object_consolidate (PAGE *page)
{
  OBJECT *o_current;
  const GList *iter;
  int status = 0;
  gboolean net_consolidate;

  g_return_if_fail (page != NULL);

  cfg_read_bool ("schematic", "net-consolidate",
                 default_net_consolidate, &net_consolidate);

  if (!net_consolidate)
    return;

  iter = s_page_objects (page);

  while (iter != NULL) {
    o_current = (OBJECT *)iter->data;

    if (o_current->type == OBJ_NET) {
      status = o_net_consolidate_segments (o_current);
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
 *  \param object     The net OBJECT to modify
 *  \param x          new x-coord of the net point
 *  \param y          new y-coord of the net point
 *  \param whichone   net point to modify
 *
 */
void
geda_net_object_modify (OBJECT *object,
                        int x,
                        int y,
                        int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;
}
