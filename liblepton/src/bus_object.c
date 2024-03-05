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

/*! \file bus_object.c
 *
 *  \brief Functions operating on bus objects
 */

#include <config.h>

#ifdef HAVE_MATH_H
#include <math.h>
#endif

#include "liblepton_priv.h"

/*! \brief Get the ripper direction
 *
 *  \param [in] object The bus object
 *  \return The ripper direction
 */
gint
lepton_bus_object_get_ripper_direction (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_bus (object), 0);
  g_return_val_if_fail (object->bus_ripper_direction >= -1, -1);
  g_return_val_if_fail (object->bus_ripper_direction <= 1, 1);

  return object->bus_ripper_direction;
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
lepton_bus_object_get_x0 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_bus (object), 0);
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
lepton_bus_object_get_x1 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_bus (object), 0);
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
lepton_bus_object_get_y0 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_bus (object), 0);
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
lepton_bus_object_get_y1 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_bus (object), 0);
  g_return_val_if_fail (object->line != NULL, 0);

  return object->line->y[1];
}

/*! \brief Set the ripper direction
 *
 *  \param [in,out] object The bus object
 *  \param [in] direction The ripper direction
 */
void
lepton_bus_object_set_ripper_direction (LeptonObject *object,
                                        gint direction)
{
  g_return_if_fail (lepton_object_is_bus (object));
  g_return_if_fail (direction >= -1);
  g_return_if_fail (direction <= 1);

  object->bus_ripper_direction = direction;
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
lepton_bus_object_set_x0 (LeptonObject *object,
                          gint x)
{
  g_return_if_fail (lepton_object_is_bus (object));
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
lepton_bus_object_set_x1 (LeptonObject *object,
                          gint x)
{
  g_return_if_fail (lepton_object_is_bus (object));
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
lepton_bus_object_set_y0 (LeptonObject *object,
                          gint y)
{
  g_return_if_fail (lepton_object_is_bus (object));
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
lepton_bus_object_set_y1 (LeptonObject *object,
                          gint y)
{
  g_return_if_fail (lepton_object_is_bus (object));
  g_return_if_fail (object->line != NULL);

  object->line->y[1] = y;
}

/*! \brief get the position of the first bus point
 *  \par Function Description
 *  This function gets the position of the first point of a bus object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
lepton_bus_object_get_position (const LeptonObject *object,
                                gint *x,
                                gint *y)
{
  g_return_val_if_fail (lepton_object_is_bus (object), FALSE);
  g_return_val_if_fail (object->line != NULL, FALSE);

  if (x != NULL) {
    *x = object->line->x[0];
  }

  if (y != NULL) {
    *y = object->line->y[0];
  }

  return TRUE;
}

/*! \brief Calculate the bounds of the bus
 *
 *  On failure, this function sets the bounds to empty.
 *
 *  \param [in]  object   The bus object
 *  \param [out] bounds   The bounds of the bus
 */
void
lepton_bus_object_calculate_bounds (const LeptonObject *object,
                                    LeptonBounds *bounds)
{
  gint expand;

  lepton_bounds_init (bounds);

  g_return_if_fail (lepton_object_is_bus (object));
  g_return_if_fail (object->line != NULL);

  lepton_line_calculate_bounds (object->line, bounds);

  expand = ceil (0.5 * G_SQRT2 * BUS_WIDTH);

  /* This isn't strictly correct, but a 1st order approximation */
  lepton_bounds_expand (bounds, bounds, expand, expand);
}

/*! \brief create a new bus object
 *  \par Function Description
 *  This function creates and returns a new bus object.
 *
 *  \param [in]     color       The color of the bus
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \param [in]  bus_ripper_direction direction of the bus rippers
 *  \return A new bus LeptonObject
 */
LeptonObject*
lepton_bus_object_new (gint color,
                       gint x1,
                       gint y1,
                       gint x2,
                       gint y2,
                       gint bus_ripper_direction)
{
  LeptonObject *new_node;

  new_node = lepton_object_new (OBJ_BUS, "bus");
  lepton_object_set_color (new_node, color);

  new_node->line = lepton_line_new ();
  /* check for null */

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;
  lepton_object_set_stroke_width (new_node, BUS_WIDTH);

  new_node->bus_ripper_direction = bus_ripper_direction;

  return new_node;
}

/*! \brief read a bus object from a char buffer
 *  \par Function Description
 *  This function reads a bus object from the buffer \a buf.
 *  If the bus object was read successfully, a new bus object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] buf          a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \param [in,out] err  The \c GError structure storing the error
 *                       in case of failure.
 *  \return The object list, or NULL on error.
 */
LeptonObject*
o_bus_read (const char buf[],
            unsigned int release_ver,
            unsigned int fileformat_ver,
            GError **err)
{
  LeptonObject *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int color;
  int ripper_dir;

  if (release_ver <= VERSION_20020825) {
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse bus object"));
      return NULL;
    }
    ripper_dir = 0;
  } else {
    if (sscanf (buf, "%c %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color,
                &ripper_dir) != 7) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse bus object"));
      return NULL;
    }
  }

  if (x1 == x2 && y1 == y2) {
    g_message (_("Found a zero length bus "
                 "[ %1$c %2$d %3$d %4$d %5$d %6$d ]"),
               type, x1, y1, x2, y2, color);
  }

  if (!color_id_valid (color)) {
    g_message (_("Found an invalid color [ %1$s ]"), buf);
    g_message (_("Setting color to default color."));
    color = default_color_id();
  }

  if (ripper_dir < -1 || ripper_dir > 1) {
    g_message (_("Found an invalid bus ripper direction [ %1$s ]"), buf);
    g_message (_("Resetting direction to neutral (no direction)."));
    ripper_dir = 0;
  }

  new_obj = lepton_bus_object_new (color, x1, y1, x2, y2, ripper_dir);

  return new_obj;
}

/*! \brief Create a string representation of the bus object
 *  \par Function Description
 *  This function takes a bus \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a bus LeptonObject
 *  \return the string representation of the bus LeptonObject
 */
gchar*
lepton_bus_object_to_buffer (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_bus (object), NULL);
  g_return_val_if_fail (object->line != NULL, NULL);

  return g_strdup_printf ("%c %d %d %d %d %d %d",
                          lepton_object_get_type (object),
                          lepton_bus_object_get_x0 (object),
                          lepton_bus_object_get_y0 (object),
                          lepton_bus_object_get_x1 (object),
                          lepton_bus_object_get_y1 (object),
                          lepton_object_get_color (object),
                          lepton_bus_object_get_ripper_direction (object));
}

/*! \brief move a bus object
 *  \par Function Description
 *  This function changes the position of a bus \a object.
 *
 *  \param [in,out] object The bus LeptonObject to be moved
 *  \param [in]     dx     The x-distance to move the object
 *  \param [in]     dy     The y-distance to move the object
 */
void
lepton_bus_object_translate (LeptonObject *object,
                             gint dx,
                             gint dy)
{
  g_return_if_fail (lepton_object_is_bus (object));
  g_return_if_fail (object->line != NULL);

  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;
}

/*! \brief create a copy of a bus object
 *  \par Function Description
 *  This function creates a copy of the bus \a object.
 *
 *  \param [in] object    The object that is copied
 *  \return a new bus object
 */
LeptonObject*
lepton_bus_object_copy (const LeptonObject *object)
{
  LeptonObject *new_obj;

  g_return_val_if_fail (lepton_object_is_bus (object), NULL);
  g_return_val_if_fail (object->line != NULL, NULL);

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = lepton_bus_object_new (lepton_object_get_color (object),
                                   object->line->x[0],
                                   object->line->y[0],
                                   object->line->x[1],
                                   object->line->y[1],
                                   object->bus_ripper_direction);

  return new_obj;
}

/*! \brief rotate a bus object around a centerpoint
 *  \par Function Description
 *  This function rotates a bus \a object around the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in]     world_centerx x-coord of the rotation center
 *  \param [in]     world_centery y-coord of the rotation center
 *  \param [in]     angle         The angle to rotate the bus object
 *  \param [in,out] object        The bus object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void
lepton_bus_object_rotate (gint world_centerx,
                          gint world_centery,
                          gint angle,
                          LeptonObject *object)
{
  gint newx, newy;

  g_return_if_fail (lepton_object_is_bus (object));
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (lepton_angle_is_ortho (angle));

  if (angle == 0) {
    return;
  }

  /* translate object to origin */
  lepton_bus_object_translate (object, -world_centerx, -world_centery);

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

  lepton_bus_object_translate (object, world_centerx, world_centery);
}

/*! \brief mirror a bus object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a bus \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in]     world_centerx x-coord of the mirror position
 *  \param [in]     world_centery y-coord of the mirror position
 *  \param [in,out] object        The bus object
 */
void
lepton_bus_object_mirror (gint world_centerx,
                          gint world_centery,
                          LeptonObject *object)
{
  g_return_if_fail (lepton_object_is_bus (object));
  g_return_if_fail (object->line != NULL);

  /* translate object to origin */
  lepton_bus_object_translate (object, -world_centerx, -world_centery);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  lepton_bus_object_translate (object, world_centerx, world_centery);
}

/*! \brief calculate the orientation of a bus object
 *  \par Function Description
 *  This function calculates the orientation of a bus object.
 *
 *  \param [in] object   The bus object
 *  \return The orientation: HORIZONTAL, VERTICAL or NEITHER
 */
gint
lepton_bus_object_orientation (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_bus (object), NEITHER);
  g_return_val_if_fail (object->line != NULL, NEITHER);

  if (object->line->y[0] == object->line->y[1]) {
    return(HORIZONTAL);
  }

  if (object->line->x[0] == object->line->x[1]) {
    return(VERTICAL);
  }

  return(NEITHER);
}

/*! \brief modify one point of a bus object
 *  \par Function Description
 *  This function modifies one point of a bus \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *
 *  \param [in,out] object     The bus LeptonObject to modify
 *  \param [in]     x          new x-coord of the bus point
 *  \param [in]     y          new y-coord of the bus point
 *  \param [in]     whichone   bus point to modify
 */
void
lepton_bus_object_modify (LeptonObject *object,
                          gint x,
                          gint y,
                          gint whichone)
{
  g_return_if_fail (lepton_object_is_bus (object));
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (whichone >= LINE_END1);
  g_return_if_fail (whichone <= LINE_END2);

  object->line->x[whichone] = x;
  object->line->y[whichone] = y;
}
