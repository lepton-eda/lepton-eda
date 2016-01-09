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

/*! \file o_arc_basic.c
 *  \brief functions for the arc object
 */

#include <config.h>

#include <stdio.h>
#include <math.h>

#include "libgeda_priv.h"

/*! \brief
 *  \par Function Description
 *  The function creates a new OBJECT of type arc.
 *
 *  The arc is defined by its center in parameters x and y.
 *  The radius parameter specifies the radius of the arc. The start
 *  angle is given by start_angle and the sweep angle by sweep_angle.
 *  The line and fill type of the created arc are set to default.
 *
 *  All dimensions are in world unit, except start_angle and
 *  sweep_angle in degrees.
 *
 *  A new object of type OBJECT is allocated. Its type and color
 *  are initilized. The description of the arc characteristics
 *  are stored in a new ARC structure.
 *
 *  Now fixed for world coordinates.
 *
 *  \param [in] toplevel    The TOPLEVEL object.
 *  \param [in] type
 *  \param [in] color
 *  \param [in] x
 *  \param [in] y
 *  \param [in] radius
 *  \param [in] start_angle
 *  \param [in] sweep_angle
 *  \return
 */
OBJECT*
geda_arc_object_new (TOPLEVEL *toplevel, char type, int color, int x, int y,
                     int radius, int start_angle, int sweep_angle)
{

  OBJECT *new_node;

  new_node = s_basic_new_object(type, "arc");

  new_node->color = color;

  new_node->arc = geda_arc_new ();

  /*! \note
   *  The ARC structure is initialized with the parameters.
   *  A default initialization is performed for the line and
   *  fill type to avoid misunderstanding.
   *
   *  The functions relative to the use of the object are sets.
   */

  /* World coordinates */
  new_node->arc->x      = x;
  new_node->arc->y      = y;
  new_node->arc->radius = radius;

  /* must check the sign of start_angle, sweep_angle ... */
  if(sweep_angle < 0) {
    start_angle = start_angle + sweep_angle;
    sweep_angle = -sweep_angle;
  }
  if(start_angle < 0) start_angle = 360 + start_angle;

  new_node->arc->start_angle = start_angle;
  new_node->arc->sweep_angle = sweep_angle;

  /* Default init */
  o_set_line_options(toplevel, new_node,
                     DEFAULT_OBJECT_END, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options(toplevel, new_node,
                     FILLING_HOLLOW, -1, -1, -1, -1, -1);

  new_node->w_bounds_valid_for = NULL;

  /* new_node->graphical = arc; eventually */

  return new_node;
}

/*! \brief
 *  \par Function Description
 *  This function creates a new object representing an arc.
 *
 *  The values of the <B>o_current</B> pointed OBJECT are then copied to the new object.
 *
 *  The arc, the line options are initialized whereas the fill options are
 *  initialized to passive values - as an arc can not be filled.
 *
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] o_current
 *  \return The new OBJECT
 */
OBJECT*
geda_arc_object_copy (TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  new_obj = geda_arc_object_new (toplevel, OBJ_ARC, o_current->color,
                                 o_current->arc->x, o_current->arc->y,
                                 o_current->arc->radius,
                                 o_current->arc->start_angle,
                                 o_current->arc->sweep_angle);
  o_set_line_options(toplevel, new_obj,
                     o_current->line_end, o_current->line_type,
                     o_current->line_width,
                     o_current->line_length, o_current->line_space);
  o_set_fill_options(toplevel, new_obj,
                     FILLING_HOLLOW, -1, -1, -1, -1, -1);

  return new_obj;
}

/*! \brief Get the x coordinate of the center of the arc
 *
 *  \param [in] object The arc
 *  \return The x coordinate of the center of the arc
 */
gint
geda_arc_object_get_center_x (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->arc != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_ARC, 0);

  return object->arc->x;
}

/*! \brief Get the y coordinate of the center of the arc
 *
 *  \param [in] object The arc
 *  \return The y coordinate of the center of the arc
 */
gint
geda_arc_object_get_center_y (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->arc != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_ARC, 0);

  return object->arc->y;
}

/*! \brief Get the radius of the arc
 *
 *  \param [in] object The arc
 *  \return The raduis of the arc
 */
gint
geda_arc_object_get_radius (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->arc != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_ARC, 0);

  return object->arc->radius;
}

/*! \brief Get the starting angle of the arc
 *
 *  \param [in] object The arc
 *  \return The starting angle of the arc
 */
gint
geda_arc_object_get_start_angle (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->arc != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_ARC, 0);

  return object->arc->start_angle;
}

/*! \brief Get the sweep angle of the arc
 *
 *  \param [in] object The arc
 *  \return The sweep angle of the arc
 */
gint
geda_arc_object_get_sweep_angle (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->arc != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_ARC, 0);

  return object->arc->sweep_angle;
}

/*! \brief Set the x coordinate of the center of the arc
 *
 *  \param [in,out] object The arc
 *  \param [in] x The new y coordinate for the arc center
 */
void
geda_arc_object_set_center_x (GedaObject *object, gint x)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  object->arc->x = x;
}

/*! \brief Set the y coordinate of the center of the arc
 *
 *  \param [in,out] object The arc
 *  \param [in] y The new y coordinate for the arc center
 */
void
geda_arc_object_set_center_y (GedaObject *object, gint y)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  object->arc->y = y;
}

/*! \brief Set the radius of the arc
 *
 *  \param [in,out] object The arc
 *  \param [in] radius The new raduis for the arc
 */
void
geda_arc_object_set_radius (GedaObject *object, gint radius)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  object->arc->radius = radius;
}

/*! \brief Set the starting angle of the arc
 *
 *  \param [in,out] object The arc
 *  \param [in] angle The new starting angle for the arc
 */
void
geda_arc_object_set_start_angle (GedaObject *object, gint angle)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  object->arc->start_angle = angle;
}

/*! \brief Set the sweep angle of the arc
 *
 *  \param [in,out] object The arc
 *  \param [in] angle The new sweep angle for the arc
 */
void
geda_arc_object_set_sweep_angle (GedaObject *object, gint angle)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  object->arc->sweep_angle = angle;
}

/*! \brief
 *  \par Function Description
 *  This function modifies the internal values of the arc object
 *  *object according to the whichone parameter.
 *
 *  The new values are given by <B>x</B> and/or <B>y</B>. Their meaning depends on the value of whichone.
 *
 *  If <B>whichone</B> is equal to #ARC_CENTER, the (<B>x</B>,<B>y</B>) point is taken as the new center
 *  of the arc in world unit.
 *
 *  If <B>whichone</B> is equal to #ARC_RADIUS, the <B>x</B> parameter is taken to be the radius
 *  of the arc in world unit. The <B>y</B> parameter is ignored.
 *
 *  If <B>whichone</B> is equal to #ARC_START_ANGLE, the <B>x</B> parameter is the starting angle of the arc.
 *  <B>x</B> is in degrees. <B>y</B> is ignored.
 *
 *  If <B>whichone</B> is equal to #ARC_SWEEP_ANGLE, the <B>x</B> parameter is the ending angle of the arc.
 *  <B>x</B> is in degrees. <B>y</B> is ignored.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object
 *  \param [in]     x
 *  \param [in]     y
 *  \param [in]     whichone
 */
void
geda_arc_object_modify (TOPLEVEL *toplevel, OBJECT *object, int x, int y, int whichone)
{

	o_emit_pre_change_notify (toplevel, object);

	switch(whichone) {
		case ARC_CENTER:
		/* modify the center of arc object */
		object->arc->x = x;
		object->arc->y = y;
		break;

		case ARC_RADIUS:
		/* modify the radius of arc object */
		object->arc->radius = x;
		break;

		case ARC_START_ANGLE:
		/* modify the start angle of the arc object */
		object->arc->start_angle = x;
		break;

		case ARC_SWEEP_ANGLE:
		/* modify the end angle of the arc object */
		object->arc->sweep_angle = x;
		break;

		default:
		break;
	}

	/* update the screen coords and the bounding box */
	object->w_bounds_valid_for = NULL;
	o_emit_change_notify (toplevel, object);
}

/*! \brief
 *  \par Function Description
 *  This function reads a formatted text buffer describing an arc
 *  in the gEDA file format and initializes the corresponding object.
 *
 *  Depending on the version of the file format the data extraction is
 *  performed differently : currently pre-20000704 and 20000704 on one
 *  hand and post-20000704 file format version on the other hand are supported.
 *  The version is specified in string pointed by <B>fileformat_ver</B>.
 *
 *  To get information on the various file formats have a
 *  look to the fileformats.html document.
 *
 *  The object is initialized with the functions #o_set_line_options() and #o_set_fill_options().
 *  The second one is only used to put initialize unused values for an arc as an arc can not be filled.
 *
 *  The arc is allocated initialized with the function #geda_arc_object_new().
 *
 *  A negative or null radius is not allowed.
 *
 *  \param [in] toplevel    The TOPLEVEL object.
 *  \param [in] buf
 *  \param [in] release_ver
 *  \param [in] fileformat_ver
 *  \return The ARC OBJECT that was created, or NULL on error.
 */
OBJECT *o_arc_read (TOPLEVEL *toplevel, const char buf[],
           unsigned int release_ver, unsigned int fileformat_ver, GError **err)
{
  OBJECT *new_obj;
  char type;
  int x1, y1;
  int radius;
  int start_angle;
  int sweep_angle;
  int color;
  int arc_width, arc_length, arc_space;
  int arc_type;
  int arc_end;

  /*! \note
   *  Depending on the version of the file format used to describe this arc,
   *  the buffer is parsed differently. The unknown parameters of the less
   *  restrictive - the oldest - file format are set to common values
   */
  if(release_ver <= VERSION_20000704) {
    if (sscanf(buf, "%c %d %d %d %d %d %d", &type,
	       &x1, &y1, &radius, &start_angle, &sweep_angle, &color) != 7) {
      g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse arc object"));
      return NULL;
    }

    arc_width = 0;
    arc_end   = END_NONE;
    arc_type  = TYPE_SOLID;
    arc_space = -1;
    arc_length= -1;
  } else {
    if (sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d", &type,
	       &x1, &y1, &radius, &start_angle, &sweep_angle, &color,
	       &arc_width, &arc_end, &arc_type, &arc_length, &arc_space) != 12) {
      g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse arc object"));
      return NULL;
    }
  }

  /* Error check */
  if (radius <= 0) {
    s_log_message (_("Found a zero radius arc [ %c %d, %d, %d, %d, %d, %d ]\n"),
                   type, x1, y1, radius, start_angle, sweep_angle, color);
    radius = 0;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message(_("Found an invalid color [ %s ]\n"), buf);
    s_log_message(_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
  }

  /* Allocation and initialization */
  new_obj = geda_arc_object_new (toplevel, OBJ_ARC, color,
                                 x1, y1, radius, start_angle, sweep_angle);
  o_set_line_options(toplevel, new_obj,
                     arc_end, arc_type, arc_width, arc_length,
                     arc_space);
  o_set_fill_options(toplevel, new_obj,
                     FILLING_HOLLOW, -1, -1, -1,
                     -1, -1);

  return new_obj;
}

/*! \brief create the string representation of an arc object
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buf</B> to describe
 *  the arc object <B>*object</B>.
 *  A pointer to the new allocated and formated string is returned.
 *  The string must be freed at some point.
 *
 *  \param [in] object
 *  \return the string representation of the arc object
 */
gchar*
geda_arc_object_to_buffer (const GedaObject *object)
{
  int x, y, radius, start_angle, sweep_angle;
  int arc_width, arc_length, arc_space;
  char *buf;
  OBJECT_END arc_end;
  OBJECT_TYPE arc_type;

  /* radius, center and angles of the arc */
  radius      = object->arc->radius;
  x           = object->arc->x;
  y           = object->arc->y;
  start_angle = object->arc->start_angle;
  sweep_angle = object->arc->sweep_angle;

  /* line type parameters */
  arc_width  = object->line_width;
  arc_end    = object->line_end;
  arc_type   = object->line_type;
  arc_length = object->line_length;
  arc_space  = object->line_space;

  /* Describe a circle with post-20000704 file format */
  buf = g_strdup_printf("%c %d %d %d %d %d %d %d %d %d %d %d", object->type,
			x, y, radius, start_angle, sweep_angle, object->color,
			arc_width, arc_end, arc_type, arc_length, arc_space);

  return(buf);
}

/*! \brief
 *  \par Function Description
 *  This function applies a translation of (<B>dx</B>,<B>dy</B>)
 *  to the arc described in <B>*object</B>. <B>dx</B> and <B>dy</B> are in world unit.
 *
 *  \param [in,out] object     Arc GedaObject to translate.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 */
void
geda_arc_object_translate (GedaObject *object, int dx, int dy)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  /* Do world coords */
  object->arc->x = object->arc->x + dx;
  object->arc->y = object->arc->y + dy;


  /* Recalculate screen coords from new world coords */
  object->w_bounds_valid_for = NULL;
}

/*! \brief
 *  \par Function Description
 *  This function rotates the world coordinates of an arc of an angle
 *  specified by <B>angle</B>. The center of the rotation is given by
 *  (<B>world_centerx</B>,<B>world_centery</B>).
 *
 *  The arc is translated in order to put the center of the rotation
 *  on the origin. The center of the arc is then rotated of the angle
 *  specified by <B>angle</B>. The start angle of the arc is incremented by <B>angle</B>.
 *
 *  The arc is finally back translated to its previous location on the page.
 *
 *  <B>world_centerx</B> and <B>world_centery</B> are in world units, <B>angle</B> is in degrees.
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in] world_centerx
 *  \param [in] world_centery
 *  \param [in] angle
 *  \param [in] object
 */
void geda_arc_object_rotate (TOPLEVEL *toplevel,
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int x, y, newx, newy;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  /* translate object to origin */
  object->arc->x -= world_centerx;
  object->arc->y -= world_centery;

  /* get center, and rotate center */
  x = object->arc->x;
  y = object->arc->y;
  if(angle % 90 == 0) {
    geda_point_rotate_90 (x, y, angle % 360, &newx, &newy);
  } else {
    geda_point_rotate (x, y, angle % 360, &newx, &newy);
  }
  object->arc->x = newx;
  object->arc->y = newy;

  /* apply rotation to angles */
  object->arc->start_angle = (object->arc->start_angle + angle) % 360;

  /* translate object to its previous place */
  object->arc->x += world_centerx;
  object->arc->y += world_centery;

  /* update the screen coords and the bounding box */
  object->w_bounds_valid_for = NULL;

}

/*! \brief Mirror the WORLD coordinates of an ARC.
 *  \par Function Description
 *  This function mirrors the world coordinates of an arc.
 *  The symetry axis is given by the vertical line going through the point (<B>world_centerx</B>,<B>world_centery</B>).
 *
 *  The arc is translated in order to put the point (<B>world_centerx</B>,<B>world_centery</B>)
 *  on the origin. The center of the arc is then mirrored. The start angle of the arc
 *  and the sweep of the arc are also mirrored.
 *
 *  The arc is finally back translated to its previous location on the page.
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in] world_centerx
 *  \param [in] world_centery
 *  \param [in] object
 */
void geda_arc_object_mirror (TOPLEVEL *toplevel,
			int world_centerx, int world_centery,
			OBJECT *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->arc != NULL);
  g_return_if_fail (object->type == OBJ_ARC);

  /* translate object to origin */
  object->arc->x -= world_centerx;

  /* get center, and mirror it (vertical mirror) */
  object->arc->x = -object->arc->x;

  /* apply mirror to angles (vertical mirror) */
  object->arc->start_angle = (180 - object->arc->start_angle) % 360;
  /* start_angle *MUST* be positive */
  if(object->arc->start_angle < 0) object->arc->start_angle += 360;
  object->arc->sweep_angle = -object->arc->sweep_angle;

  /* translate object back to its previous position */
  object->arc->x += world_centerx;

  /* update the screen coords and bounding box */
  object->w_bounds_valid_for = NULL;

}


/*! \brief
 *  \par Function Description
 *  This function calculates the smallest rectangle the arc can be drawn into.
 *  The <B>OBJECT</B> pointed by object is assumed to be an arc.
 *  The <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B> pointed integers define
 *  this rectangle at the end of the function. It is expressed in world units.
 *  The process is divided into two steps : the first step is to calculate the
 *  coordinates of the two ends of the arc and the coordinates of the center.
 *  They forms a first rectangle but (depending on the start angle and the
 *  sweep of the arc) not the right.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object
 *  \param [out] left
 *  \param [out] top
 *  \param [out] right
 *  \param [out] bottom
 */
void
geda_arc_object_calculate_bounds (TOPLEVEL *toplevel,
                                  const OBJECT *object,
                                  gint *left,
                                  gint *top,
                                  gint *right,
                                  gint *bottom)
{
  int x1, y1, x2, y2, x3, y3;
  int radius, start_angle, sweep_angle;
  int i, angle;
  int halfwidth;

  halfwidth = object->line_width / 2;

  radius      = object->arc->radius;
  start_angle = object->arc->start_angle;
  sweep_angle = object->arc->sweep_angle;

  x1 = object->arc->x;
  y1 = object->arc->y;
  x2 = x1 + radius * cos(start_angle * M_PI / 180);
  y2 = y1 + radius * sin(start_angle * M_PI / 180);
  x3 = x1 + radius * cos((start_angle + sweep_angle) * M_PI / 180);
  y3 = y1 + radius * sin((start_angle + sweep_angle) * M_PI / 180);

  *left   = (x1 < x2) ? ((x1 < x3) ? x1 : x3) : ((x2 < x3) ? x2 : x3);
  *right  = (x1 > x2) ? ((x1 > x3) ? x1 : x3) : ((x2 > x3) ? x2 : x3);
  *bottom = (y1 > y2) ? ((y1 > y3) ? y1 : y3) : ((y2 > y3) ? y2 : y3);
  *top    = (y1 < y2) ? ((y1 < y3) ? y1 : y3) : ((y2 < y3) ? y2 : y3);

  /*! \note
   *  The previous rectangle is extended to the final one
   *  by checking whether the arc is over a main axis (vertical or horizontal).
   *  If so, the rectangle is extended in these directions.
   *
   *  In the mirror mode, the sweep angle is negativ. To get a
   *  CCW arc before this calculation we have to move the
   *  start angle to the end angle and reverse the sweep angle.
   */
  if (sweep_angle < 0) {
    start_angle = (start_angle + sweep_angle + 360) % 360;
    sweep_angle = -sweep_angle;
  }
  angle = ((int) (start_angle / 90)) * 90;
  for(i = 0; i < 4; i++) {
    angle = angle + 90;
    if(angle < start_angle + sweep_angle) {
      if(angle % 360 == 0)   *right  = x1 + radius;
      if(angle % 360 == 90)  *bottom = y1 + radius;
      if(angle % 360 == 180) *left   = x1 - radius;
      if(angle % 360 == 270) *top    = y1 - radius;
    } else {
      break;
    }
  }

  /* This isn't strictly correct, but a 1st order approximation */
  *left   -= halfwidth;
  *top    -= halfwidth;
  *right  += halfwidth;
  *bottom += halfwidth;

}

/*! \brief get the position of the center point
 *  \par Function Description
 *  This function gets the position of the center point of an arc object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
geda_arc_object_get_position (const GedaObject *object, gint *x, gint *y)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->type == OBJ_ARC, FALSE);
  g_return_val_if_fail (object->arc != NULL, FALSE);

  if (x != NULL) {
    *x = object->arc->x;
  }

  if (y != NULL) {
    *y = object->arc->y;
  }

  return TRUE;
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the perimeter of the arc.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The arc OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double
geda_arc_object_shortest_distance (TOPLEVEL *toplevel, OBJECT *object,
                                   int x, int y, int force_solid)
{
  double shortest_distance;
  double radius;

  g_return_val_if_fail (object->arc != NULL, G_MAXDOUBLE);

  radius = (double)object->arc->radius;

  if (geda_arc_within_sweep (object->arc, x, y)) {
    double distance_to_center;

    distance_to_center = hypot (x - object->arc->x, y - object->arc->y);

    shortest_distance = fabs (distance_to_center - radius);

  } else {
    double angle;
    double distance_to_end0;
    double distance_to_end1;
    double dx, dy;

    angle = G_PI * ((double)object->arc->start_angle) / 180;

    dx = ((double)x) - radius * cos (angle) - ((double)object->arc->x);
    dy = ((double)y) - radius * sin (angle) - ((double)object->arc->y);

    distance_to_end0 = hypot (dx, dy);

    angle += G_PI * ((double)object->arc->sweep_angle) / 180;

    dx = ((double)x) - radius * cos (angle) - ((double)object->arc->x);
    dy = ((double)y) - radius * sin (angle) - ((double)object->arc->y);

    distance_to_end1 = hypot (dx, dy);

    shortest_distance = min (distance_to_end0, distance_to_end1);
  }

  return shortest_distance;
}
