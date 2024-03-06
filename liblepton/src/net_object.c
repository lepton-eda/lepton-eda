/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

#include "liblepton_priv.h"


/*! \file net_object.c
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
lepton_net_object_get_position (const LeptonObject *object,
                                gint *x,
                                gint *y)
{
  g_return_val_if_fail (lepton_object_is_net (object), FALSE);
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
lepton_net_object_get_x0 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_net (object), 0);
  g_return_val_if_fail (object->line != NULL, 0);

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
lepton_net_object_get_x1 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_net (object), 0);
  g_return_val_if_fail (object->line != NULL, 0);

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
lepton_net_object_get_y0 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_net (object), 0);
  g_return_val_if_fail (object->line != NULL, 0);

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
lepton_net_object_get_y1 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_net (object), 0);
  g_return_val_if_fail (object->line != NULL, 0);

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
lepton_net_object_set_x0 (LeptonObject *object,
                          gint x)
{
  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

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
lepton_net_object_set_x1 (LeptonObject *object,
                          gint x)
{
  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

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
lepton_net_object_set_y0 (LeptonObject *object,
                          gint y)
{
  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

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
lepton_net_object_set_y1 (LeptonObject *object,
                          gint y)
{
  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

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
lepton_net_object_calculate_bounds (const LeptonObject *object,
                                    LeptonBounds *bounds)
{
  gint expand;

  lepton_bounds_init (bounds);

  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

  lepton_line_calculate_bounds (object->line, bounds);

  expand = ceil (0.5 * G_SQRT2 * NET_WIDTH);

  /* This isn't strictly correct, but a 1st order approximation */
  lepton_bounds_expand (bounds, bounds, expand, expand);
}

/*! \brief create a new net object
 *  \par Function Description
 *  This function creates and returns a new net object.
 *
 *  \param [in]     color       The color of the net
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \return A new net LeptonObject
 */
LeptonObject*
lepton_net_object_new (int color,
                       int x1,
                       int y1,
                       int x2,
                       int y2)
{
  LeptonObject *new_node;

  new_node = lepton_object_new (OBJ_NET, "net");
  lepton_object_set_color (new_node, color);

  new_node->line = lepton_line_new ();

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;
  lepton_object_set_stroke_width (new_node, NET_WIDTH);

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
 *  \param [in,out] err The \c GError structure storing the error
 *                      in case of failure.
 *  \return The object list, or NULL on error.
 *
 */
LeptonObject*
o_net_read (const char buf[],
            unsigned int release_ver,
            unsigned int fileformat_ver,
            GError **err)
{
  LeptonObject *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int color;

  if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6) {
        g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse net object"));
    return NULL;
  }

  if (x1 == x2 && y1 == y2) {
    g_message (_("Found a zero length net "
                 "[ %1$c %2$d %3$d %4$d %5$d %6$d ]"),
               type, x1, y1, x2, y2, color);
  }

  if (!color_id_valid (color)) {
    g_message (_("Found an invalid color [ %1$s ]"), buf);
    g_message (_("Setting color to default color."));
    color = default_color_id();
  }

  new_obj = lepton_net_object_new (color, x1, y1, x2, y2);

  return new_obj;
}

/*! \brief Create a string representation of the net object
 *  \par Function Description
 *  This function takes a net \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a net LeptonObject
 *  \return the string representation of the net LeptonObject
 */
gchar*
lepton_net_object_to_buffer (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_net (object), NULL);
  g_return_val_if_fail (object->line != NULL, NULL);

  return g_strdup_printf ("%c %d %d %d %d %d",
                          lepton_object_get_type (object),
                          lepton_net_object_get_x0 (object),
                          lepton_net_object_get_y0 (object),
                          lepton_net_object_get_x1 (object),
                          lepton_net_object_get_y1 (object),
                          lepton_object_get_color (object));
}

/*! \brief move a net object
 *  \par Function Description
 *  This function changes the position of a net \a object.
 *
 *  \param [ref] object The net LeptonObject to be moved
 *  \param [in]  dx     The x-distance to move the object
 *  \param [in]  dy     The y-distance to move the object
 */
void
lepton_net_object_translate (LeptonObject *object,
                             int dx,
                             int dy)
{
  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

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
LeptonObject*
lepton_net_object_copy (LeptonObject *o_current)
{
  LeptonObject *new_obj;

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = lepton_net_object_new (lepton_object_get_color (o_current),
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
lepton_net_object_rotate (int world_centerx,
                          int world_centery,
                          int angle,
                          LeptonObject *object)
{
  int newx, newy;

  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

  if (angle == 0)
    return;

  /* translate object to origin */
  lepton_net_object_translate (object, -world_centerx, -world_centery);

  lepton_point_rotate_90 (object->line->x[0],
                          object->line->y[0],
                          angle,
                          &newx,
                          &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  lepton_point_rotate_90 (object->line->x[1],
                          object->line->y[1],
                          angle,
                          &newx,
                          &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  lepton_net_object_translate (object, world_centerx, world_centery);
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
lepton_net_object_mirror (int world_centerx,
                          int world_centery,
                          LeptonObject *object)
{
  g_return_if_fail (lepton_object_is_net (object));
  g_return_if_fail (object->line != NULL);

  /* translate object to origin */
  lepton_net_object_translate (object, -world_centerx, -world_centery);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  lepton_net_object_translate (object, world_centerx, world_centery);
}

/*! \brief calculate the orientation of a net object
 *  \par Function Description
 *  This function calculates the orientation of a net object.
 *
 *  \param [in] object   The net object
 *  \return The orientation: HORIZONTAL, VERTICAL or NEITHER
 */
int
lepton_net_object_orientation (LeptonObject *object)
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
static void o_net_consolidate_lowlevel (LeptonObject *object,
                                        LeptonObject *del_object, int orient)
{
  int temp1, temp2;
  int final1, final2;
  int changed = 0;
  GList *a_iter;
  LeptonObject *a_current;

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

  GList * del_object_attribs = lepton_object_get_attribs (del_object);
  /* Move any attributes from the deleted object*/
  if (changed && del_object_attribs != NULL)
  {
    /* Reassign the attached_to pointer on attributes from the del object */
    a_iter = del_object_attribs;
    while (a_iter != NULL) {
      a_current = (LeptonObject*) a_iter->data;
      lepton_object_set_attached_to (a_current, object);
      a_iter = g_list_next (a_iter);
    }

    GList *all_attribs = g_list_concat (lepton_object_get_attribs (object),
                                        del_object_attribs);
    lepton_object_set_attribs (object, all_attribs);

    /* Don't free del_object's attribute list as it's relinked
     * into object's list. */
    lepton_object_set_attribs (del_object, NULL);
  }
}

/*! \brief Check if there's a midpoint connection at (x,y)
 *  \par Function Description
 *  This function checks if the \a object is connected to another net
 *  between it's endpoints. Net segment's only can be merged if there
 *  is no midpoint connection.
 *
 *  \param object  a net LeptonObject to check
 *  \param x       x-coord of the connection location
 *  \param y       y-coord of the connection location
 *  \return TRUE if there's no midpoint connection, else return FALSE
 */
static int o_net_consolidate_nomidpoint (LeptonObject *object, int x, int y)
{
  GList *c_current;
  LeptonConn *conn;

  c_current = object->conn_list;
  while(c_current != NULL) {
    conn = (LeptonConn *) c_current->data;
    if (conn->other_object) {
      if (lepton_object_get_id (conn->other_object) != lepton_object_get_id (object) &&
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
o_net_consolidate_segments (LeptonObject *object)
{
  int object_orient;
  int other_orient;
  GList *c_current;
  LeptonConn *conn;
  LeptonObject *other_object;
  LeptonPage *page;

  g_return_val_if_fail (lepton_object_is_net (object), 0);

  /* It's meaningless to do anything here if the object isn't in a page. */
  page = lepton_object_get_page (object);
  g_return_val_if_fail ((page != NULL), 0);

  object_orient = lepton_net_object_orientation (object);

  c_current = object->conn_list;
  while(c_current != NULL) {
    conn = (LeptonConn *) c_current->data;
    other_object = conn->other_object;

    /* only look at end points which have a valid end on the other side */
    if (other_object != NULL && conn->type == CONN_ENDPOINT &&
        conn->other_whichone != -1 && conn->whichone != -1 &&
        o_net_consolidate_nomidpoint(object, conn->x, conn->y) ) {

      if (lepton_object_is_net (other_object))
      {
        other_orient = lepton_net_object_orientation (other_object);

        /* - both objects have the same orientation (either vert or horiz) */
        /* - it's not the same object */
        if (object_orient == other_orient &&
            lepton_object_get_id (object) != lepton_object_get_id (other_object) &&
            other_orient != NEITHER) {

#if DEBUG
          printf("consolidating %s to %s\n", object->name, other_object->name);
#endif

          o_net_consolidate_lowlevel(object, other_object, other_orient);

          if (lepton_object_get_selected (other_object) == TRUE)
          {
            LeptonSelection *selection = lepton_page_get_selection_list (page);
            o_selection_remove (selection, other_object);

            /* If we're consolidating with a selected object,
             * ensure we select the resulting object.
             */
            if (lepton_object_get_selected (object) == FALSE)
            {
              o_selection_add (selection, object);
            }
          }

          lepton_object_delete (other_object);
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
 *  \param page      The LeptonPage to consolidate nets in.
 */
void
lepton_net_object_consolidate (LeptonPage *page)
{
  LeptonObject *o_current;
  const GList *iter;
  int status = 0;
  gboolean net_consolidate;

  g_return_if_fail (page != NULL);

  cfg_read_bool ("schematic", "net-consolidate",
                 default_net_consolidate, &net_consolidate);

  if (!net_consolidate)
    return;

  iter = lepton_page_objects (page);

  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;

    if (lepton_object_is_net (o_current))
    {
      status = o_net_consolidate_segments (o_current);
    }

    if (status == -1) {
      iter = lepton_page_objects (page);
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
 *  \param object     The net LeptonObject to modify
 *  \param x          new x-coord of the net point
 *  \param y          new y-coord of the net point
 *  \param whichone   net point to modify
 *
 */
void
lepton_net_object_modify (LeptonObject *object,
                          int x,
                          int y,
                          int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;
}
