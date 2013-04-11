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

/*! \file o_circle_basic.c
 *  \brief functions for the circle object
 */

#include <config.h>
#include <stdio.h>
#include <math.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief calculate the distance between two points
 *  \par Function Description
 *  This function calculates the distance between two points.
 *  The two points are defined by the (\a x1, \a y1) and (\a x2, \a y2)
 *  parameters.
 *  \param [in]  x1  x-value of the first point
 *  \param [in]  y1  y-value of the first point
 *  \param [in]  x2  x-value of the second point
 *  \param [in]  y2  y-value of the second point
 *  \return the distance
 *  \todo Move this function to a different place
 */
int dist(int x1, int y1, int x2, int y2)
{
  return sqrt(pow(x1-x2,2)+pow(y1-y2,2));
}

/*! \brief Create and add circle OBJECT to list.
 *  \par Function Description
 *  This function creates a new object representing a circle.
 *
 *  The circle is described by its center (<B>x</B>,<B>y</B>) and its radius
 *  <B>radius</B>.
 *  The <B>type</B> parameter must be equal to <B>OBJ_CIRCLE</B>. The <B>color</B>
 *  corresponds to the color the box will be drawn with.
 *
 *  The <B>OBJECT</B> structure is allocated with the #s_basic_new_object()
 *  function. The structure describing the circle is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default values : solid
 *  line type with a width of 0, and no filling. It can be changed after
 *  with #o_set_line_options() and #o_set_fill_options().
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     type         Must be OBJ_CIRCLE.
 *  \param [in]     color        Circle line color.
 *  \param [in]     x            Center x coordinate.
 *  \param [in]     y            Center y coordinate.
 *  \param [in]     radius       Radius of new circle.
 *  \return A pointer to the new end of the object list.
 */
OBJECT *o_circle_new(TOPLEVEL *toplevel,
		     char type, int color,
		     int x, int y, int radius)
{
  OBJECT *new_node;	

  /* create the object */
  new_node = s_basic_new_object(type, "circle");
  new_node->color  = color;
  
  new_node->circle = (CIRCLE *) g_malloc(sizeof(CIRCLE));
  
  /* describe the circle with its center and radius */
  new_node->circle->center_x = x;
  new_node->circle->center_y = y;
  new_node->circle->radius   = radius;
  
  /* line type and filling initialized to default */
  o_set_line_options(toplevel, new_node,
		     DEFAULT_OBJECT_END, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options(toplevel, new_node,
		     FILLING_HOLLOW, -1, -1, -1, -1, -1);

  /* compute the bounding box coords */
  o_circle_recalc(toplevel, new_node);

  return new_node;
}

/*! \brief Create a copy of a circle.
 *  \par Function Description
 *  The function #o_circle_copy() creates a verbatim copy of the object
 *  pointed by <B>o_current</B> describing a circle.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  o_current  Circle OBJECT to copy.
 *  \return The new OBJECT
 */
OBJECT *o_circle_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  /* A new circle object is created with #o_circle_new().
   * Values for its fields are default and need to be modified. */
  new_obj = o_circle_new (toplevel, OBJ_CIRCLE, o_current->color, 0, 0, 0);

  /*
   * The parameters of the new circle are set with the ones of the original
   * circle. The two circle have the same line type and the same filling
   * options.
   *
   * The bounding box coordinates are computed with
   * #o_circle_recalc().
   */
  /* modify */
  new_obj->circle->center_x = o_current->circle->center_x;
  new_obj->circle->center_y = o_current->circle->center_y;
  new_obj->circle->radius   = o_current->circle->radius;
  
  o_set_line_options(toplevel, new_obj, o_current->line_end,
		     o_current->line_type, o_current->line_width,
		     o_current->line_length, o_current->line_space);
  o_set_fill_options(toplevel, new_obj,
		     o_current->fill_type, o_current->fill_width,
		     o_current->fill_pitch1, o_current->fill_angle1,
		     o_current->fill_pitch2, o_current->fill_angle2);
  
  o_circle_recalc(toplevel, new_obj);

  /*	new_obj->attribute = 0;*/

  return new_obj;
}

/*! \brief Modify the description of a circle OBJECT.
 *  \par Function Description
 *  This function modifies the description of the circle object <B>*object</B>
 *  depending on <B>whichone</B> that give the meaning of the <B>x</B> and <B>y</B>
 *  parameters.
 *
 *  If <B>whichone</B> is equal to <B>CIRCLE_CENTER</B>, the new center of the
 *  circle is given by (<B>x</B>,<B>y</B>) where <B>x</B> and <B>y</B> are in world units.
 *
 *  If <B>whichone</B> is equal to <B>CIRCLE_RADIUS</B>, the radius is given by
 *  <B>x</B> - in world units. <B>y</B> is ignored.
 *
 *  The bounding box of the circle object is updated after the modification of its 
 *  parameters.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object     Circle OBJECT to modify.
 *  \param [in]     x          New center x coordinate, or radius value.
 *  \param [in]     y          New center y coordinate.
 *                             Unused if radius is being modified.
 *  \param [in]     whichone   Which circle parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>CIRCLE_CENTER
 *    <DT>*</DT><DD>CIRCLE_RADIUS
 *  </DL>
 */
void o_circle_modify(TOPLEVEL *toplevel, OBJECT *object,
		     int x, int y, int whichone)
{
  o_emit_pre_change_notify (toplevel, object);

  switch(whichone) {
    case CIRCLE_CENTER:
      /* modify the center of the circle */
      object->circle->center_x = x;
      object->circle->center_y = y;
      break;
    case CIRCLE_RADIUS:
      /* modify the radius of the circle */
      if (x == 0) {
	s_log_message(_("Null radius circles are not allowed\n"));
	return;
      }
      object->circle->radius = x;
      break;
    default:
      break;
  }

  /* recalculate the boundings */
  o_circle_recalc(toplevel, object);
  o_emit_change_notify (toplevel, object);
}

/*! \brief Create circle OBJECT from character string.
 *  \par Function Description
 *  The #o_circle_read() function gets from the character string <B>*buff</B> the
 *  description of a circle.
 *
 *  Depending on <B>*version</B>, the right file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 2000704 release.
 *    <DT>*</DT><DD>the file format used for the releases after 20000704.
 *  </DL>
 *
 *  \param [in]  toplevel       The TOPLEVEL object.
 *  \param [in]  buf             Character string with circle description.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *  \return A pointer to the new circle object, or NULL on error.
 */
OBJECT *o_circle_read (TOPLEVEL *toplevel, const char buf[],
              unsigned int release_ver, unsigned int fileformat_ver, GError ** err)
{
  OBJECT *new_obj;
  char type; 
  int x1, y1;
  int radius;
  int color;
  int circle_width, circle_space, circle_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  int circle_end;
  int circle_type;
  int circle_fill;

  if(release_ver <= VERSION_20000704) {
    /*
     * The old geda file format, i.e. releases 20000704 and older, does not
     * handle the line type and the filling of the box object. They are set
     * to default.
     */
    if (sscanf(buf, "%c %d %d %d %d\n", &type, &x1, &y1, &radius, &color) != 5) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse circle object"));
      return NULL;
    }

    circle_width = 0;
    circle_end   = END_NONE;
    circle_type  = TYPE_SOLID;
    circle_length= -1;
    circle_space = -1;
    
    circle_fill  = FILLING_HOLLOW;
    fill_width  = 0;
    angle1      = -1;
    pitch1      = -1;
    angle2      = -1;
    pitch2      = -1;
			
  } else {
	
    /*
     * The current line format to describe a circle is a space separated
     * list of characters and numbers in plain ASCII on a single line. The
     * meaning of each item is described in the file format documentation.
     */  
    if (sscanf(buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
	       &type, &x1, &y1, &radius, &color,
	       &circle_width, &circle_end, &circle_type,
	       &circle_length, &circle_space, &circle_fill,
	       &fill_width, &angle1, &pitch1, &angle2, &pitch2) != 16) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse circle object"));
      return NULL;
    }
  }


  if (radius <= 0) {
    s_log_message(_("Found a zero or negative radius circle [ %c %d %d %d %d ]\n"),
                  type, x1, y1, radius, color);
    s_log_message (_("Setting radius to 0\n"));
    radius = 0;
  }
  
  if (color < 0 || color > MAX_COLORS) {
    s_log_message(_("Found an invalid color [ %s ]\n"), buf);
    s_log_message(_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
  }

  /* 
   * A circle is internally described by its center and its radius.
   *
   * A new object is allocated, initialized and added to the object list.
   * Its filling and line type are set according to the values of the field
   * on the line.
   */
  new_obj = o_circle_new(toplevel, type, color, x1, y1, radius);
  o_set_line_options(toplevel, new_obj,
		     circle_end, circle_type, circle_width, 
		     circle_length, circle_space);
  o_set_fill_options(toplevel, new_obj,
		     circle_fill, fill_width, pitch1, angle1, pitch2, angle2);

  return new_obj;
}

/*! \brief Create a character string representation of a circle OBJECT.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe the
 *  circle object <B>*object</B>.
 *  It follows the post-20000704 release file format that handle the line
 *  type and fill options.
 *
 *  \param [in] toplevel  a TOPLEVEL structure.
 *  \param [in] object  Circle OBJECT to create string from.
 *  \return A pointer to the circle OBJECT character string.
 *
 *  \note
 *  Caller must g_free returned character string.
 *
 */
char *o_circle_save(TOPLEVEL *toplevel, OBJECT *object)
{
  int x,y;
  int radius;
  int circle_width, circle_space, circle_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  char *buf;
  OBJECT_END circle_end;
  OBJECT_TYPE circle_type;
  OBJECT_FILLING circle_fill;

  /* circle center and radius */
  x = object->circle->center_x;
  y = object->circle->center_y;
  radius = object->circle->radius;
  
  /* line type parameters */
  circle_width = object->line_width;
  circle_end   = object->line_end;
  circle_type  = object->line_type;
  circle_length= object->line_length;
  circle_space = object->line_space;
  
  /* filling parameters */
  circle_fill  = object->fill_type;
  fill_width   = object->fill_width;
  angle1       = object->fill_angle1;
  pitch1       = object->fill_pitch1;
  angle2       = object->fill_angle2;
  pitch2       = object->fill_pitch2;
  
  buf = g_strdup_printf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d", 
			object->type, x, y, radius, object->color,
			circle_width, circle_end, circle_type, circle_length, 
			circle_space, circle_fill,
			fill_width, angle1, pitch1, angle2, pitch2);
  return(buf);
}
           
/*! \brief Translate a circle position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the circle
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit. 
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 *  \param [in,out] object     Circle OBJECT to translate.
 */
void o_circle_translate_world(TOPLEVEL *toplevel,
			      int dx, int dy, OBJECT *object)
{
  /* Do world coords */
  object->circle->center_x = object->circle->center_x + dx;
  object->circle->center_y = object->circle->center_y + dy;
  
  /* recalc the screen coords and the bounding box */
  o_circle_recalc(toplevel, object);
  
}

/*! \brief Rotate Circle OBJECT using WORLD coordinates. 
 *  \par Function Description
 *  The function #o_circle_rotate_world() rotate the circle described by
 *  <B>*object</B> around the (<B>world_centerx</B>,<B>world_centery</B>) point by
 *  angle <B>angle</B> degrees.
 *  The center of rotation is in world unit.
 *
 *  \param [in]      toplevel      The TOPLEVEL object.
 *  \param [in]      world_centerx  Rotation center x coordinate in WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Circle OBJECT to rotate.
 */
void o_circle_rotate_world(TOPLEVEL *toplevel,
			   int world_centerx, int world_centery, int angle,
			   OBJECT *object)
{
  int newx, newy;
  int x, y;

  /* Only 90 degree multiple and positive angles are allowed. */
  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;
  
  /*
   * The center of rotation (<B>world_centerx</B>,<B>world_centery</B>) is
   * translated to the origin. The rotation of the center around the origin
   * is then performed. Finally, the rotated circle is translated back to
   * its previous location.
   */

  /* translate object to origin */
  object->circle->center_x -= world_centerx;
  object->circle->center_y -= world_centery;
  
  /* rotate the center of the circle around the origin */
  x = object->circle->center_x;
  y = object->circle->center_y;
  rotate_point_90(x, y, angle, &newx, &newy);
  object->circle->center_x = newx;
  object->circle->center_y = newy;
  
  /* translate back in position */
  object->circle->center_x += world_centerx;
  object->circle->center_y += world_centery;

  o_circle_recalc(toplevel, object);
  
}

/*! \brief Mirror circle using WORLD coordinates.
 *  \par Function Description
 *  This function recalculates the screen coords of the <B>o_current</B> pointed
 *  circle object from its world coords.
 *
 *  The circle coordinates and its bounding are recalculated as well as the
 *  OBJECT specific (line width, filling ...).
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Circle OBJECT to mirror.
 */
void o_circle_mirror_world(TOPLEVEL *toplevel,
			   int world_centerx, int world_centery,
			   OBJECT *object)
{
  /* translate object to origin */
  object->circle->center_x -= world_centerx;
  object->circle->center_y -= world_centery;

  /* mirror the center of the circle */
  object->circle->center_x = -object->circle->center_x;
  object->circle->center_y =  object->circle->center_y;

  /* translate back in position */
  object->circle->center_x += world_centerx;
  object->circle->center_y += world_centery;

  /* recalc boundings and screen coords */
  o_circle_recalc(toplevel, object);
  
}

/*! \brief Recalculate circle coordinates in SCREEN units.
 *  \par Function Description
 *  This function recalculates the screen coords of the <B>o_current</B> pointed
 *  circle object from its world coords.
 *
 *  The circle coordinates and its bounding are recalculated as well as the
 *  OBJECT specific (line width, filling ...).
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in,out] o_current  Circle OBJECT to be recalculated.
 */
void o_circle_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, right, top, bottom;

  if (o_current->circle == NULL) {
    return;
  }
  
  /* update the bounding box - world unit */
  world_get_circle_bounds(toplevel, o_current,
		    &left, &top, &right, &bottom);
  o_current->w_left   = left;
  o_current->w_top    = top;
  o_current->w_right  = right;
  o_current->w_bottom = bottom;
  o_current->w_bounds_valid_for = toplevel;
}

/*! \brief Get circle bounding rectangle in WORLD coordinates.
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B>
 *  parameters to the boundings of the circle object described in <B>*circle</B>
 *  in world units.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object     Circle OBJECT to read coordinates from.
 *  \param [out] left       Left circle coordinate in WORLD units.
 *  \param [out] top        Top circle coordinate in WORLD units.
 *  \param [out] right      Right circle coordinate in WORLD units.
 *  \param [out] bottom     Bottom circle coordinate in WORLD units.
 */
void world_get_circle_bounds(TOPLEVEL *toplevel, OBJECT *object, int *left,
                             int *top, int *right, int *bottom)
{
  int halfwidth;

  halfwidth = object->line_width / 2;

  *left   = object->circle->center_x - object->circle->radius;
  *top    = object->circle->center_y - object->circle->radius;
  *right  = object->circle->center_x + object->circle->radius;
  *bottom = object->circle->center_y + object->circle->radius;

  /* This isn't strictly correct, but a 1st order approximation */
  *left   -= halfwidth;
  *top    -= halfwidth;
  *right  += halfwidth;
  *bottom += halfwidth;

}

/*! \brief get the position of the center point
 *  \par Function Description
 *  This function gets the position of the center point of a circle object.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean o_circle_get_position (TOPLEVEL *toplevel, gint *x, gint *y,
                              OBJECT *object)
{
  *x = object->circle->center_x;
  *y = object->circle->center_y;
  return TRUE;
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the perimeter of the circle.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The circle OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point.  With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double o_circle_shortest_distance (TOPLEVEL *toplevel, OBJECT *object,
                                   int x, int y, int force_solid)
{
  int solid;

  g_return_val_if_fail (object->circle != NULL, G_MAXDOUBLE);

  solid = force_solid || object->fill_type != FILLING_HOLLOW;

  return m_circle_shortest_distance (object->circle, x, y, solid);
}

