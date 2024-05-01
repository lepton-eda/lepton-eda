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


/*! \brief Test if object is a bus pin object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is bus pin, otherwise FALSE.
 */
gboolean
lepton_pin_object_is_bus_pin (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_pin (object), FALSE);

  return (object->pin_type == PIN_TYPE_BUS);
}


/*! \brief Test if object is a net pin object.
 *
 *  \param [in] object The object to test.
 *  \return TRUE, if the object is net pin, otherwise FALSE.
 */
gboolean
lepton_pin_object_is_net_pin (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_pin (object), FALSE);

  return (object->pin_type == PIN_TYPE_NET);
}


/*! \file pin_object.c
 *  \brief functions for the pin object
 */

/*! \brief Calculate the bounds of a pin
 *
 *  On failure, this function sets the bounds to empty.
 *
 *  \param [in]  object   The pin object
 *  \param [out] bounds   The bounds of the pin
 */
void
lepton_pin_object_calculate_bounds (const LeptonObject *object,
                                    LeptonBounds *bounds)
{
  gint expand;
  gint width;

  lepton_bounds_init (bounds);

  g_return_if_fail (lepton_object_is_pin (object));
  g_return_if_fail (object->line != NULL);

  lepton_line_calculate_bounds (object->line, bounds);

  width = lepton_pin_object_get_width (object);

  expand = ceil (0.5 * G_SQRT2 * width);

  /* This isn't strictly correct, but a 1st order approximation */
  lepton_bounds_expand (bounds, bounds, expand, expand);
}

/*! \brief Get the width to draw the pin
 *
 *  On failure, this function returns PIN_WIDTH_NET.
 *
 *  \param [in]  object   The pin object
 *  \return The line width to draw the pin
 */
gint
lepton_pin_object_get_width (const LeptonObject *object)
{
  gint width = PIN_WIDTH_NET;

  g_return_val_if_fail (lepton_object_is_pin (object), PIN_WIDTH_NET);

  switch (object->pin_type)
  {
    case PIN_TYPE_NET:
      width = PIN_WIDTH_NET;
      break;

    case PIN_TYPE_BUS:
      width = PIN_WIDTH_BUS;
      break;

    default:
      g_warning ("lepton_pin_object_calculate_bounds: invalid pin_type");
  }

  return width;
}

/*! \brief get the position of a whichend of the pin object
 *  \par Function Description
 *  This function gets the position of the whichend side of a pin object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
lepton_pin_object_get_position (const LeptonObject *object,
                                gint *x,
                                gint *y)
{
  g_return_val_if_fail (lepton_object_is_pin (object), FALSE);
  g_return_val_if_fail (object->line != NULL, FALSE);
  g_return_val_if_fail (object->whichend >= 0, FALSE);
  g_return_val_if_fail (object->whichend < 2, FALSE);

  if (x != NULL) {
    *x = object->line->x[object->whichend];
  }

  if (y != NULL) {
    *y = object->line->y[object->whichend];
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
lepton_pin_object_get_x0 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_pin (object), 0);
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
lepton_pin_object_get_x1 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_pin (object), 0);
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
lepton_pin_object_get_y0 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_pin (object), 0);
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
lepton_pin_object_get_y1 (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_pin (object), 0);
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
lepton_pin_object_set_x0 (LeptonObject *object,
                          gint x)
{
  g_return_if_fail (lepton_object_is_pin (object));
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
lepton_pin_object_set_x1 (LeptonObject *object,
                          gint x)
{
  g_return_if_fail (lepton_object_is_pin (object));
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
lepton_pin_object_set_y0 (LeptonObject *object,
                          gint y)
{
  g_return_if_fail (lepton_object_is_pin (object));
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
lepton_pin_object_set_y1 (LeptonObject *object,
                          gint y)
{
  g_return_if_fail (lepton_object_is_pin (object));
  g_return_if_fail (object->line != NULL);

  object->line->y[1] = y;
}

/*! \brief create a new pin object
 *  \par Function Description
 *  This function creates and returns a new pin object.
 *
 *  \param [in]     color       The color of the pin
 *  \param [in]     x1          x-coord of the first point
 *  \param [in]     y1          y-coord of the first point
 *  \param [in]     x2          x-coord of the second point
 *  \param [in]     y2          y-coord of the second point
 *  \param [in]     pin_type    type of pin (PIN_TYPE_NET or PIN_TYPE_BUS)
 *  \param [in]     whichend    The connectable end of the pin
 *  \return A new pin LeptonObject
 */
LeptonObject*
lepton_pin_object_new (int color,
                       int x1,
                       int y1,
                       int x2,
                       int y2,
                       int pin_type,
                       int whichend)
{
  LeptonObject *new_node;

  new_node = lepton_object_new (OBJ_PIN, "pin");
  lepton_object_set_color (new_node, color);

  new_node->line = lepton_line_new ();

  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;

  lepton_pin_object_set_type (new_node, pin_type);

  new_node->whichend = whichend;

  return new_node;
}

/*! \brief Create a new net pin object.
 *  \par Function Description
 *  This function creates and returns a new net pin object.
 *
 *  \param [in] color    The color of the pin.
 *  \param [in] x1       X-coord of the first point.
 *  \param [in] y1       Y-coord of the first point.
 *  \param [in] x2       X-coord of the second point.
 *  \param [in] y2       Y-coord of the second point.
 *  \param [in] whichend The connectible end of the pin.
 *  \return A new pin LeptonObject
 */
LeptonObject*
lepton_pin_object_new_net_pin (int color,
                               int x1,
                               int y1,
                               int x2,
                               int y2,
                               int whichend)
{
  return lepton_pin_object_new (color, x1, y1, x2, y2, PIN_TYPE_NET, whichend);
}


/*! \brief Create a new bus pin object.
 *  \par Function Description
 *  This function creates and returns a new bus pin object.
 *
 *  \param [in] color    The color of the pin.
 *  \param [in] x1       X-coord of the first point.
 *  \param [in] y1       Y-coord of the first point.
 *  \param [in] x2       X-coord of the second point.
 *  \param [in] y2       Y-coord of the second point.
 *  \param [in] whichend The connectible end of the pin.
 *  \return A new pin LeptonObject
 */
LeptonObject*
lepton_pin_object_new_bus_pin (int color,
                               int x1,
                               int y1,
                               int x2,
                               int y2,
                               int whichend)
{
  return lepton_pin_object_new (color, x1, y1, x2, y2, PIN_TYPE_BUS, whichend);
}

/*! \brief read a pin object from a char buffer
 *  \par Function Description
 *  This function reads a pin object from the buffer \a buf.
 *  If the pin object was read successfully, a new pin object is
 *  allocated and appended to the \a object_list.
 *
 *  \param [in] buf            Character string with pin description.
 *  \param [in] release_ver    liblepton release version number.
 *  \param [in] fileformat_ver liblepton file format version number.
 *  \param [in,out] err \c GError structure for error reporting,
 *                      or NULL to disable error reporting.
 *  \return The object list, or NULL on error.
 */
LeptonObject*
lepton_pin_object_read (const char buf[],
                        unsigned int release_ver,
                        unsigned int fileformat_ver,
                        GError **err)
{
  LeptonObject *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int color;
  int pin_type;
  int whichend;

  if (release_ver <= VERSION_20020825) {
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse pin object"));
      return NULL;
    }
    pin_type = PIN_TYPE_NET;
    whichend = -1;
  } else {
    if (sscanf (buf, "%c %d %d %d %d %d %d %d\n", &type, &x1, &y1, &x2, &y2,
                &color, &pin_type, &whichend) != 8) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse pin object"));
      return NULL;
    }
  }

  if (whichend == -1) {
    g_message (_("Found a pin which did not have the whichend field set.\n"
                 "Verify and correct manually."));
  } else if (whichend < -1 || whichend > 1) {
    g_message (_("Found an invalid whichend on a pin (reseting to zero): %d"),
               whichend);
    whichend = 0;
  }

  if (!color_id_valid (color)) {
    g_message (_("Found an invalid color [ %1$s ]"), buf);
    g_message (_("Setting color to default color."));
    color = default_color_id();
  }

  new_obj = lepton_pin_object_new (color,
                                   x1,
                                   y1,
                                   x2,
                                   y2,
                                   pin_type,
                                   whichend);

  return new_obj;
}

/*! \brief Create a string representation of the pin object
 *
 *  This function takes a pin \a object and returns a string
 *  according to the file format definition.
 *
 *  On failure, this function returns NULL.
 *
 *  The caller must free the returned string when no longer needed using
 *  g_free().
 *
 *  \param [in] object a pin object
 *  \return a string representation of the pin object
 */
gchar*
lepton_pin_object_to_buffer (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_pin (object), NULL);
  g_return_val_if_fail (object->line != NULL, NULL);

  return g_strdup_printf ("%c %d %d %d %d %d %d %d",
                          lepton_object_get_type (object),
                          lepton_pin_object_get_x0 (object),
                          lepton_pin_object_get_y0 (object),
                          lepton_pin_object_get_x1 (object),
                          lepton_pin_object_get_y1 (object),
                          lepton_object_get_color (object),
                          object->pin_type,
                          object->whichend);
}

/*! \brief move a pin object
 *  \par Function Description
 *  This function changes the position of a pin \a object.
 *
 *  \param [in,out] object The pin #LeptonObject to be moved.
 *  \param [in] dx       The x-distance to move the object
 *  \param [in] dy       The y-distance to move the object
 */
void
lepton_pin_object_translate (LeptonObject *object,
                             int dx,
                             int dy)
{
  g_return_if_fail (lepton_object_is_pin (object));
  g_return_if_fail (object->line != NULL);

  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;
}

/*! \brief create a copy of a pin object
 *  \par Function Description
 *  This function creates a copy of the pin object \a o_current.
 *
 *  \param [in] o_current    The object that is copied
 *  \return a new pin object
 */
LeptonObject*
lepton_pin_object_copy (LeptonObject *o_current)
{
  LeptonObject *new_obj;

  g_return_val_if_fail (lepton_object_is_pin (o_current), NULL);
  g_return_val_if_fail (o_current->line != NULL, NULL);

  new_obj = lepton_pin_object_new (lepton_object_get_color (o_current),
                                   o_current->line->x[0],
                                   o_current->line->y[0],
                                   o_current->line->x[1],
                                   o_current->line->y[1],
                                   o_current->pin_type,
                                   o_current->whichend);

  return new_obj;
}

/*! \brief rotate a pin object around a centerpoint
 *  \par Function Description
 *  This function rotates a pin \a object around the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotat the pin object
 *  \param [in] object        The pin object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void
lepton_pin_object_rotate (int world_centerx,
                          int world_centery,
                          int angle,
                          LeptonObject *object)
{
  int newx, newy;

  g_return_if_fail (lepton_object_is_pin (object));
  g_return_if_fail (object->line != NULL);

  if (angle == 0)
    return;

  /* translate object to origin */
  lepton_pin_object_translate (object, -world_centerx, -world_centery);

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

  lepton_pin_object_translate (object, world_centerx, world_centery);
}

/*! \brief mirror a pin object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a pin \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The pin object
 */
void
lepton_pin_object_mirror (int world_centerx,
                          int world_centery,
                          LeptonObject *object)
{
  g_return_if_fail (lepton_object_is_pin (object));
  g_return_if_fail (object->line != NULL);

  /* translate object to origin */
  lepton_pin_object_translate (object, -world_centerx, -world_centery);

  object->line->x[0] = -object->line->x[0];

  object->line->x[1] = -object->line->x[1];

  lepton_pin_object_translate (object, world_centerx, world_centery);
}

/*! \brief modify one point of a pin object
 *  \par Function Description
 *  This function modifies one point of a pin \a object. The point
 *  is specified by the \a whichone variable and the new coordinate
 *  is (\a x, \a y).
 *
 *  \param object     The pin LeptonObject to modify
 *  \param x          new x-coord of the pin point
 *  \param y          new y-coord of the pin point
 *  \param whichone   pin point to modify
 *
 */
void
lepton_pin_object_modify (LeptonObject *object,
                          int x,
                          int y,
                          int whichone)
{
  g_return_if_fail (lepton_object_is_pin (object));
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->whichend >= 0);
  g_return_if_fail (object->whichend < 2);

  object->line->x[whichone] = x;
  object->line->y[whichone] = y;
}

/*! \brief guess the whichend of pins of object list
 *  \par Function Description
 *  This function determines the whichend of the pins in the \a object_list.
 *  In older libgeda file format versions there was no information about the
 *  active end of pins.
 *  This function calculates the bounding box of all pins in the object list.
 *  The side of the pins that are closer to the boundary of the box are
 *  set as active ends of the pins.
 *
 *  \param object_list list of LeptonObjects
 *  \param force_boundingbox Use the whole symbol bounding box to
 *                           find pin connection points.
 */
void
lepton_pin_object_update_whichend (GList *object_list,
                                   gboolean force_boundingbox)
{
  LeptonObject *o_current;
  GList *iter;
  GList *pin_list = NULL;
  int top = 0, left = 0;
  int right = 0, bottom = 0;
  int d1, d2, d3, d4;
  int min0, min1;
  int min0_whichend, min1_whichend;

  /* No objects, nothing to update. */
  if (object_list == NULL) return;

  for (iter = object_list;
       iter != NULL;
       iter = g_list_next (iter)) {
    o_current = (LeptonObject *)iter->data;
    if (lepton_object_is_pin (o_current))
    {
      pin_list = g_list_prepend (pin_list, o_current);
    }
  }

  /* No pins, nothing to update. */
  if (pin_list == NULL) return;

  if (force_boundingbox) {
    /* Include text objects since we need full bounds. */
    world_get_object_glist_bounds (object_list,
                                   /* Never consider hidden text. */
                                   FALSE,
                                   &left,
                                   &top,
                                   &right,
                                   &bottom);
  } else {
    /* Only look at the pins to calculate symbol bounds. */
    world_get_object_glist_bounds (pin_list,
                                   FALSE,
                                   &left,
                                   &top,
                                   &right,
                                   &bottom);
  }

  iter = pin_list;
  while (iter != NULL) {
    o_current = (LeptonObject *)iter->data;
    /* Determine which end of the pin is on or nearest the boundary */
    if (o_current->whichend == -1) {
      if (o_current->line->y[0] == o_current->line->y[1]) {
        /* horizontal */

        d1 = abs(o_current->line->x[0] - left);
        d2 = abs(o_current->line->x[1] - left);
        d3 = abs(o_current->line->x[0] - right);
        d4 = abs(o_current->line->x[1] - right);

        if (d1 <= d2) {
          min0 = d1;
          min0_whichend = 0;
        } else {
          min0 = d2;
          min0_whichend = 1;
        }

        if (d3 <= d4) {
          min1 = d3;
          min1_whichend = 0;
        } else {
          min1 = d4;
          min1_whichend = 1;
        }

        if (min0 <= min1) {
          o_current->whichend = min0_whichend;
        } else {
          o_current->whichend = min1_whichend;
        }

      } else if (o_current->line->x[0] == o_current->line->x[1]) {
        /* vertical */

        d1 = abs(o_current->line->y[0] - top);
        d2 = abs(o_current->line->y[1] - top);
        d3 = abs(o_current->line->y[0] - bottom);
        d4 = abs(o_current->line->y[1] - bottom);

        if (d1 <= d2) {
          min0 = d1;
          min0_whichend = 0;
        } else {
          min0 = d2;
          min0_whichend = 1;
        }

        if (d3 <= d4) {
          min1 = d3;
          min1_whichend = 0;
        } else {
          min1 = d4;
          min1_whichend = 1;
        }

        if (min0 <= min1) {
          o_current->whichend = min0_whichend;
        } else {
          o_current->whichend = min1_whichend;
        }
      }
    }
    iter = g_list_next (iter);
  }

  g_list_free (pin_list);
}


/*! \brief Sets the type, and corresponding width of a pin
 *
 *  \par Function Description
 *  Sets the pin's type and width to a particular style.
 *
 *  \param [in] o_current  The pin LeptonObject being modified
 *  \param [in] pin_type   The new type of this pin
 */
void
lepton_pin_object_set_type (LeptonObject *o_current,
                            int pin_type)
{
  g_return_if_fail (lepton_object_is_pin (o_current));

  lepton_object_emit_pre_change_notify (o_current);
  switch (pin_type) {
    default:
      g_critical ("lepton_pin_object_set_type: Got invalid pin type %1$i\n", pin_type);
      /* Fall through */
    case PIN_TYPE_NET:
      lepton_object_set_stroke_width (o_current, PIN_WIDTH_NET);
      o_current->pin_type = PIN_TYPE_NET;
      break;
    case PIN_TYPE_BUS:
      lepton_object_set_stroke_width (o_current, PIN_WIDTH_BUS);
      o_current->pin_type = PIN_TYPE_BUS;
      break;
  }
  lepton_object_emit_change_notify (o_current);
}
