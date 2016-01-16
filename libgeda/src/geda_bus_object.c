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
geda_bus_object_get_x0 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_BUS, 0);

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
geda_bus_object_get_x1 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_BUS, 0);

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
geda_bus_object_get_y0 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_BUS, 0);

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
geda_bus_object_get_y1 (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->line != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_BUS, 0);

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
geda_bus_object_set_x0 (GedaObject *object, gint x)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_BUS);

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
geda_bus_object_set_x1 (GedaObject *object, gint x)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_BUS);

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
geda_bus_object_set_y0 (GedaObject *object, gint y)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_BUS);

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
geda_bus_object_set_y1 (GedaObject *object, gint y)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_BUS);

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
geda_bus_object_get_position (const GedaObject *object, gint *x, gint *y)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->type == OBJ_BUS, FALSE);
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
 *  \param [in]  toplevel Unused
 *  \param [in]  object   The bus object
 *  \param [out] bounds   The bounds of the bus
 */
void
geda_bus_object_calculate_bounds (TOPLEVEL *toplevel,
                                  const OBJECT *object,
                                  GedaBounds *bounds)
{
  gint expand;

  geda_bounds_init (bounds);

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->type == OBJ_BUS);
  g_return_if_fail (object->line != NULL);

  geda_line_calculate_bounds (object->line, bounds);

  expand = ceil (0.5 * G_SQRT2 * BUS_WIDTH);

  /* This isn't strictly correct, but a 1st order approximation */
  geda_bounds_expand (bounds, bounds, expand, expand);
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
OBJECT*
geda_bus_object_new (TOPLEVEL *toplevel, char type, int color,
                     int x1, int y1, int x2, int y2,
                     int bus_ripper_direction)
{
  OBJECT *new_node;

  new_node = s_basic_new_object(type, "bus");
  new_node->color = color;

  new_node->line = geda_line_new ();
  /* check for null */

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;
  new_node->line_width = BUS_WIDTH;

  new_node->bus_ripper_direction = bus_ripper_direction;

  new_node->w_bounds_valid_for = NULL;

  return new_node;
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
OBJECT *o_bus_read (TOPLEVEL *toplevel, const char buf[],
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
    s_log_message (_("Found a zero length bus [ %c %d %d %d %d %d ]\n"),
                    type, x1, y1, x2, y2, color);
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

  new_obj = geda_bus_object_new (toplevel, type, color, x1, y1, x2, y2, ripper_dir);

  return new_obj;
}

/*! \brief Create a string representation of the bus object
 *  \par Function Description
 *  This function takes a bus \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object  a bus OBJECT
 *  \return the string representation of the bus OBJECT
 */
gchar*
geda_bus_object_to_buffer (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->line != NULL, NULL);
  g_return_val_if_fail (object->type == OBJ_BUS, NULL);

  return g_strdup_printf ("%c %d %d %d %d %d %d",
                          OBJ_BUS,
                          object->line->x[0],
                          object->line->y[0],
                          object->line->x[1],
                          object->line->y[1],
                          geda_object_get_color (object),
                          object->bus_ripper_direction);
}

/*! \brief move a bus object
 *  \par Function Description
 *  This function changes the position of a bus \a object.
 *
 *  \param [ref] object The bus GedaObject to be moved
 *  \param [in]  dx     The x-distance to move the object
 *  \param [in]  dy     The y-distance to move the object
 */
void
geda_bus_object_translate (GedaObject *object, int dx, int dy)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_BUS);

  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;

  /* Update bounding box */
  object->w_bounds_valid_for = NULL;
}

/*! \brief create a copy of a bus object
 *  \par Function Description
 *  This function creates a copy of the bus object \a o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] o_current    The object that is copied
 *  \return a new bus object
 */
OBJECT*
geda_bus_object_copy (TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  /* make sure you fix this in pin and bus as well */
  /* still doesn't work... you need to pass in the new values */
  /* or don't update and update later */
  /* I think for now I'll disable the update and manually update */
  new_obj = geda_bus_object_new (toplevel, OBJ_BUS, o_current->color,
                                 o_current->line->x[0], o_current->line->y[0],
                                 o_current->line->x[1], o_current->line->y[1],
                                 o_current->bus_ripper_direction);

  return new_obj;
}

/*! \brief rotate a bus object around a centerpoint
 *  \par Function Description
 *  This function rotates a bus \a object around the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotate the bus object
 *  \param [in] object        The bus object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void geda_bus_object_rotate (TOPLEVEL *toplevel,
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int newx, newy;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_BUS);

  if (angle == 0)
  return;

  /* translate object to origin */
  geda_bus_object_translate (object, -world_centerx, -world_centery);

  geda_point_rotate_90 (object->line->x[0], object->line->y[0], angle,
                  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  geda_point_rotate_90 (object->line->x[1], object->line->y[1], angle,
                  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  geda_bus_object_translate (object, world_centerx, world_centery);
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
void geda_bus_object_mirror (TOPLEVEL *toplevel,
			int world_centerx, int world_centery, OBJECT *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_BUS);

  /* translate object to origin */
  geda_bus_object_translate (object, -world_centerx, -world_centery);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  geda_bus_object_translate (object, world_centerx, world_centery);
}

/*! \brief calculate the orientation of a bus object
 *  \par Function Description
 *  This function calculates the orientation of a bus object.
 *
 *  \param [in] object   The bus object
 *  \return The orientation: HORIZONTAL, VERTICAL or NEITHER
 */
int
geda_bus_object_orientation (OBJECT *object)
{
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
 *  \param toplevel   The TOPLEVEL object
 *  \param object     The bus OBJECT to modify
 *  \param x          new x-coord of the bus point
 *  \param y          new y-coord of the bus point
 *  \param whichone   bus point to modify
 */
void
geda_bus_object_modify (TOPLEVEL *toplevel, OBJECT *object,
                        int x, int y, int whichone)
{
  object->line->x[whichone] = x;
  object->line->y[whichone] = y;

  object->w_bounds_valid_for = NULL;
}
