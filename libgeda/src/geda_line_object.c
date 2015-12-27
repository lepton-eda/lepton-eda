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

/*! \file o_line_basic.c
 *  \brief functions for the line object
 */

#include <config.h>

#include <stdio.h>
#include <math.h>

#include "libgeda_priv.h"

/*! \brief Create and add line OBJECT to list.
 *  \par Function Description
 *  This function creates a new object representing a line.
 *
 *  The line is described by its two ends - <B>x1</B>,<B>y1</B> and
 *  <B>x2</B>,<B>y2</B>.
 *  The <B>type</B> parameter must be equal to #OBJ_LINE.
 *  The <B>color</B> parameter corresponds to the color the box
 *  will be drawn with.
 *
 *  The #OBJECT structure is allocated with the #s_basic_new_object()
 *  function. The structure describing the line is allocated and
 *  initialized with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default
 *  values : solid line type with a width of 0, and no filling.
 *  It can be changed after with the #o_set_line_options() and
 *  #o_set_fill_options().
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     type         Must be OBJ_LINE.
 *  \param [in]     color        Circle line color.
 *  \param [in]     x1           Upper x coordinate.
 *  \param [in]     y1           Upper y coordinate.
 *  \param [in]     x2           Lower x coordinate.
 *  \param [in]     y2           Lower y coordinate.
 *  \return A pointer to the new end of the object list.
 */
OBJECT *o_line_new(TOPLEVEL *toplevel,
		   char type, int color,
		   int x1, int y1, int x2, int y2)
{
  OBJECT *new_node;

  /* create the object */
  new_node = s_basic_new_object(type, "line");
  new_node->color = color;

  new_node->line  = (LINE *) g_malloc(sizeof(LINE));

  /* describe the line with its two ends */
  new_node->line->x[0] = x1;
  new_node->line->y[0] = y1;
  new_node->line->x[1] = x2;
  new_node->line->y[1] = y2;

  /* line type and filling initialized to default */
  o_set_line_options(toplevel, new_node,
		     DEFAULT_OBJECT_END, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options(toplevel, new_node,
		     FILLING_HOLLOW, -1, -1, -1, -1, -1);

  /* compute bounding box */
  new_node->w_bounds_valid_for = NULL;

  return new_node;
}

/*! \brief Create a copy of a line.
 *  \par Function Description
 *  This function creates a verbatim copy of the
 *  object pointed by <B>o_current</B> describing a line.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  o_current  Line OBJECT to copy.
 *  \return The new OBJECT
 */
OBJECT *o_line_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  /* A new line object is created with #o_line_new().
   * Values for its fields are default and need to be modified. */
  new_obj = o_line_new (toplevel, OBJ_LINE, o_current->color,
                        o_current->line->x[0], o_current->line->y[0],
                        o_current->line->x[1], o_current->line->y[1]);

  /*
   * The coordinates of the ends of the new line are set with the ones
   * of the original line. The two lines have the sale line type and
   * filling options.
   */

  /* copy the line type and filling options */
  o_set_line_options(toplevel, new_obj, o_current->line_end,
		     o_current->line_type, o_current->line_width,
		     o_current->line_length, o_current->line_space);
  o_set_fill_options(toplevel, new_obj,
		     o_current->fill_type, o_current->fill_width,
		     o_current->fill_pitch1, o_current->fill_angle1,
		     o_current->fill_pitch2, o_current->fill_angle2);

  /* calc the bounding box */
  o_current->w_bounds_valid_for = NULL;

  /* new_obj->attribute = 0;*/

  /* return the new tail of the object list */
  return new_obj;
}

/*! \brief Modify the description of a line OBJECT.
 *  \par Function Description
 *  This function modifies the coordinates of one of the two ends of
 *  the line described by <B>*object</B>. The new coordinates of this end,
 *  identified by <B>whichone</B>, are given by <B>x</B> and <B>y</B>
 *  in world unit.
 *
 *  The coordinates of the end of line is modified in the world
 *  coordinate system. Screen coordinates and boundings are then updated.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object     Line OBJECT to modify.
 *  \param [in]     x          New x coordinate.
 *  \param [in]     y          New y coordinate.
 *  \param [in]     whichone   Which line parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>LINE_END1
 *    <DT>*</DT><DD>LINE_END2
 *  </DL>
 */
void o_line_modify(TOPLEVEL *toplevel, OBJECT *object,
                   int x, int y, int whichone)
{
  o_emit_pre_change_notify (toplevel, object);

  /* change one of the end of the line */
  switch (whichone) {
    case LINE_END1:
      object->line->x[0] = x;
      object->line->y[0] = y;
      break;

    case LINE_END2:
      object->line->x[1] = x;
      object->line->y[1] = y;
      break;

    default:
      return;
  }

  /* recalculate the bounding box */
  object->w_bounds_valid_for = NULL;
  o_emit_change_notify (toplevel, object);
}

/*! \brief Create line OBJECT from character string.
 *  \par Function Description
 *  This function creates a line OBJECT from the character string
 *  <B>*buf</B> the description of a box.
 *
 *  The function returns a pointer on the new last element, that is
 *  the added line object.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 20010704 release.
 *    <DT>*</DT><DD>the file format used for the releases after 20010704.
 *  </DL>
 *
 *  \param [in]  toplevel       The TOPLEVEL object.
 *  \param [in]  buf             Character string with line description.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *  \return A pointer to the new line object, or NULL on error.
 */
OBJECT *o_line_read (TOPLEVEL *toplevel, const char buf[],
                     unsigned int release_ver, unsigned int fileformat_ver, GError ** err)
{
  OBJECT *new_obj;
  char type;
  int x1, y1;
  int x2, y2;
  int line_width, line_space, line_length;
  int line_end;
  int line_type;
  int color;

  if (release_ver <= VERSION_20000704) {
    /*
     * The old geda file format, i.e. releases 20000704 and older, does
     * not handle the line type and the filling - here filling is irrelevant.
     * They are set to default.
     */
    if (sscanf (buf, "%c %d %d %d %d %d\n", &type,
		&x1, &y1, &x2, &y2, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse line object"));
      return NULL;
    }

    line_width = 0;
    line_end   = END_NONE;
    line_type  = TYPE_SOLID;
    line_length= -1;
    line_space = -1;
  } else {
    /*
     * The current line format to describe a line is a space separated
     * list of characters and numbers in plain ASCII on a single line.
     * The meaning of each item is described in the file format documentation.
     */
      if (sscanf (buf, "%c %d %d %d %d %d %d %d %d %d %d\n", &type,
		  &x1, &y1, &x2, &y2, &color,
		  &line_width, &line_end, &line_type, &line_length, &line_space) != 11) {
        g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse line object"));
        return NULL;
      }
  }

  /*
   * Null length line are not allowed. If such a line is detected a
   * message is issued.
   *
   * It also checks is the required color is valid.
   */
  if (x1 == x2 && y1 == y2) {
    s_log_message (_("Found a zero length line [ %c %d %d %d %d %d ]\n"),
                   type, x1, y1, x2, y2, color);
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message (_("Found an invalid color [ %s ]\n"), buf);
    s_log_message (_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
  }

  /*
   * A line is internally described by its two ends. A new object is
   * allocated, initialized and added to the list of objects. Its line
   * type is set according to the values of the fields on the line.
   */
  /* create and add the line to the list */
  new_obj = o_line_new (toplevel, type, color, x1, y1, x2, y2);
  /* set its line options */
  o_set_line_options (toplevel, new_obj,
                      line_end, line_type, line_width, line_length,
                      line_space);
  /* filling is irrelevant for line, just set to default */
  o_set_fill_options (toplevel, new_obj,
                      FILLING_HOLLOW, -1, -1, -1, -1, -1);

  return new_obj;
}

/*! \brief Create a character string representation of a line OBJECT.
 *  \par Function Description
 *  The function formats a string in the buffer <B>*buff</B> to describe
 *  the box object <B>*object</B>.
 *  It follows the post-20000704 release file format that handle the
 *  line type and fill options - filling is irrelevant here.
 *
 *  \param [in] toplevel  a TOPLEVEL structure.
 *  \param [in] object  Line OBJECT to create string from.
 *  \return A pointer to the line OBJECT character string.
 *
 *  \note
 *  Caller must g_free returned character string.
 *
 */
char *o_line_save(TOPLEVEL *toplevel, OBJECT *object)
{
  int x1, x2, y1, y2;
  int line_width, line_space, line_length;
  char *buf;
  OBJECT_END line_end;
  OBJECT_TYPE line_type;

  /* get the two ends */
  x1 = object->line->x[0];
  y1 = object->line->y[0];
  x2 = object->line->x[1];
  y2 = object->line->y[1];

  /* description of the line type */
  line_width = object->line_width;
  line_end   = object->line_end;
  line_type  = object->line_type;
  line_length= object->line_length;
  line_space = object->line_space;

  buf = g_strdup_printf("%c %d %d %d %d %d %d %d %d %d %d", object->type,
			x1, y1, x2, y2, object->color,
			line_width, line_end, line_type,
			line_length, line_space);

  return(buf);
}

/*! \brief Translate a line position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the line
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 *  \param [in,out] object     Line GedaObject to translate.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 */
void
geda_line_object_translate (GedaObject *object, int dx, int dy)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_LINE);

  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;

  /* Update bounding box */
  object->w_bounds_valid_for = NULL;
}

/*! \brief Rotate Line OBJECT using WORLD coordinates.
 *  \par Function Description
 *  This function rotates the line described by
 *  <B>*object</B> around the (<B>world_centerx</B>,<B>world_centery</B>)
 *  point by <B>angle</B> degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      toplevel      The TOPLEVEL object.
 *  \param [in]      world_centerx  Rotation center x coordinate in WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Line OBJECT to rotate.
 */
void geda_line_object_rotate (TOPLEVEL *toplevel,
			 int world_centerx, int world_centery, int angle,
			 OBJECT *object)
{
  int newx, newy;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_LINE);

  if (angle == 0)
    return;

  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;

  /*
   * The center of rotation (<B>world_centerx</B>,<B>world_centery</B>)
   * is translated to the origin. The rotation of the two ends of
   * the line is performed. FInally, the rotated line is translated
   * back to its previous location.
   */
  /* translate object to origin */
  geda_line_object_translate (object, -world_centerx, -world_centery);

  /* rotate line end 1 */
  geda_point_rotate_90 (object->line->x[0], object->line->y[0], angle,
		  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;

  /* rotate line end 2 */
  geda_point_rotate_90 (object->line->x[1], object->line->y[1], angle,
		  &newx, &newy);

  object->line->x[1] = newx;
  object->line->y[1] = newy;

  /* translate object back to normal position */
  geda_line_object_translate (object, world_centerx, world_centery);

}

/*! \brief Mirror a line using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the line from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  The line if first translated to the origin, then mirrored
 *  and finally translated back at its previous position.
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Line OBJECT to mirror.
 */
void geda_line_object_mirror (TOPLEVEL *toplevel, int world_centerx,
			 int world_centery, OBJECT *object)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->line != NULL);
  g_return_if_fail (object->type == OBJ_LINE);

  /* translate object to origin */
  geda_line_object_translate (object, -world_centerx, -world_centery);

  /* mirror the line ends */
  object->line->x[0] = -object->line->x[0];
  object->line->x[1] = -object->line->x[1];

  /* translate back in position */
  geda_line_object_translate (object, world_centerx, world_centery);

}

/*! \brief Get line bounding rectangle in WORLD coordinates.
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and
 *  <B>bottom</B> parameters to the boundings of the line object described
 *  in <B>*line</B> in world units.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object     Line OBJECT to read coordinates from.
 *  \param [out] left       Left line coordinate in WORLD units.
 *  \param [out] top        Top line coordinate in WORLD units.
 *  \param [out] right      Right line coordinate in WORLD units.
 *  \param [out] bottom     Bottom line coordinate in WORLD units.
 */
void world_get_line_bounds(TOPLEVEL *toplevel, OBJECT *object,
                           int *left, int *top, int *right, int *bottom)
{
  int expand;

  expand = ceil (0.5 * G_SQRT2 * object->line_width);

  *left = min( object->line->x[0], object->line->x[1] );
  *top = min( object->line->y[0], object->line->y[1] );
  *right = max( object->line->x[0], object->line->x[1] );
  *bottom = max( object->line->y[0], object->line->y[1] );

  /* This isn't strictly correct, but a 1st order approximation */
  *left   -= expand;
  *top    -= expand;
  *right  += expand;
  *bottom += expand;
}

/*! \brief get the position of the first line point
 *  \par Function Description
 *  This function gets the position of the first point of a line object.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean o_line_get_position (TOPLEVEL *toplevel, gint *x, gint *y,
                              OBJECT *object)
{
  *x = object->line->x[0];
  *y = object->line->y[0];
  return TRUE;
}

/*! \brief
 *  \par Function Description
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] x_scale
 *  \param [in] y_scale
 *  \param [in] object
 */
void o_line_scale_world(TOPLEVEL *toplevel, int x_scale, int y_scale,
			OBJECT *object)
{
  /* scale the line world coords */
  object->line->x[0] = object->line->x[0] * x_scale;
  object->line->y[0] = object->line->y[0] * y_scale;
  object->line->x[1] = object->line->x[1] * x_scale;
  object->line->y[1] = object->line->y[1] * y_scale;

  /* update boundingbox */
  object->w_bounds_valid_for = NULL;

}


/*! \brief calculate the length of a line object
 *  \par Function Description
 *  This function calculates the length of a line object
 *
 *  \param [in] object  a line OBJECT
 *  \return The length of the line
 */
double o_line_length(OBJECT *object)
{
  double length;
  double dx, dy;

  if (!object->line) {
    return 0.0;
  }

  dx = object->line->x[0]-object->line->x[1];
  dy = object->line->y[0]-object->line->y[1];

  length = hypot(dx, dy);

  return(length);
}

/*! \brief Calculates the distance between the given point and the closest
 *  point on the given line segment.
 *
 *  If the closest point on the line resides beyond the line segment's
 *  end point, this function returns the distance from the given point to the
 *  closest end point.
 *
 *  If the line represents a single point (the endpoints are the same), this
 *  function calcualtes the distance to that point.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The line OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double o_line_shortest_distance (TOPLEVEL *toplevel, OBJECT *object,
                                 int x, int y, int force_solid)
{
  return geda_line_shortest_distance (object->line, x, y);
}

