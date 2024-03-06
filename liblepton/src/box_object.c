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

/*! \file box_object.c
 *  \brief functions for the box object
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "liblepton_priv.h"

/*! \brief Get the upper x coordinate of a box object.
 *
 *  \param [in] object The box object.
 *  \return The x coordinate of the upper corner of the box.
 */
int
lepton_box_object_get_upper_x (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_box (object), 0);
  g_return_val_if_fail (object->box != NULL, 0);

  return object->box->upper_x;
}

/*! \brief Get the upper y coordinate of a box object.
 *
 *  \param [in] object The box object.
 *  \return The y coordinate of the upper corner of the box.
 */
int
lepton_box_object_get_upper_y (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_box (object), 0);
  g_return_val_if_fail (object->box != NULL, 0);

  return object->box->upper_y;
}

/*! \brief Get the lower x coordinate of a box object.
 *
 *  \param [in] object The box object.
 *  \return The x coordinate of the lower corner of the box.
 */
int
lepton_box_object_get_lower_x (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_box (object), 0);
  g_return_val_if_fail (object->box != NULL, 0);

  return object->box->lower_x;
}

/*! \brief Get the lower y coordinate of a box object.
 *
 *  \param [in] object The box object.
 *  \return The y coordinate of the lower corner of the box.
 */
int
lepton_box_object_get_lower_y (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_box (object), 0);
  g_return_val_if_fail (object->box != NULL, 0);

  return object->box->lower_y;
}


/*! \brief Set the value of the upper x coordinate of a box object.
 *
 *  \param [in,out] object The box object.
 *  \param [in] val The new upper x coordinate of the box.
 */
void
lepton_box_object_set_upper_x (LeptonObject *object, int val)
{
  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);

  object->box->upper_x = val;
}

/*! \brief Set the value of the upper y coordinate of a box object.
 *
 *  \param [in,out] object The box object.
 *  \param [in] val The new upper y coordinate of the box.
 */
void
lepton_box_object_set_upper_y (LeptonObject *object, int val)
{
  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);

  object->box->upper_y = val;
}

/*! \brief Set the value of the lower x coordinate of a box object.
 *
 *  \param [in,out] object The box object.
 *  \param [in] val The new lower x coordinate of the box.
 */
void
lepton_box_object_set_lower_x (LeptonObject *object, int val)
{
  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);

  object->box->lower_x = val;
}

/*! \brief Set the value of the lower y coordinate of a box object.
 *
 *  \param [in,out] object The box object.
 *  \param [in] val The new lower y coordinate of the box.
 */
void
lepton_box_object_set_lower_y (LeptonObject *object, int val)
{
  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);

  object->box->lower_y = val;
}

/*! \brief Create a box LeptonObject
 *  \par Function Description
 *  This function creates a new object representing a box.
 *
 *  The box is described by its upper left corner - <B>x1</B>, <B>y1</B> - and
 *  its lower right corner - <B>x2</B>, <B>y2</B>.
 *  The <B>type</B> parameter must be equal to <B>OBJ_BOX</B>. The <B>color</B>
 *  corresponds to the color the box will be drawn with.
 *  The <B>LeptonObject</B> structure is allocated with the #lepton_object_new()
 *  function. The structure describing the box is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default
 *  values : solid line type with a width of 0, and no filling. It
 *  can be changed after with the
 *  #lepton_object_set_line_options() and
 *  #lepton_object_set_fill_options().
 *
 *  \param [in]     color        Box border color.
 *  \param [in]     x1           Upper x coordinate.
 *  \param [in]     y1           Upper y coordinate.
 *  \param [in]     x2           Lower x coordinate.
 *  \param [in]     y2           Lower y coordinate.
 *  \return The new LeptonObject
 */
LeptonObject*
lepton_box_object_new (int color,
                       int x1,
                       int y1,
                       int x2,
                       int y2)
{
  LeptonObject *new_node;
  LeptonBox *box;

  /* create the object */
  new_node = lepton_object_new (OBJ_BOX, "box");
  lepton_object_set_color (new_node, color);

  box = lepton_box_new ();
  new_node->box   = box;

  /* describe the box with its upper left and lower right corner */
  box->upper_x = x1;
  box->upper_y = y1;
  box->lower_x = x2;
  box->lower_y = y2;

  /* line type and filling initialized to default */
  lepton_object_set_line_options (new_node,
                                  DEFAULT_OBJECT_END,
                                  TYPE_SOLID,
                                  LINE_WIDTH,
                                  -1,
                                  -1);

  lepton_object_set_fill_options (new_node,
                                  FILLING_HOLLOW,
                                  -1,
                                  -1,
                                  -1,
                                  -1,
                                  -1);

  return new_node;
}

/*! \brief Copy a box to a list.
 *  \par Function Description
 *  The function #lepton_box_object_copy() creates a verbatim copy
 *  of the object pointed by <B>o_current</B> describing a box.
 *
 *  \param [in]      o_current  Box LeptonObject to copy.
 *  \return The new LeptonObject
 */
LeptonObject*
lepton_box_object_copy (LeptonObject *o_current)
{
  LeptonObject *new_obj;
  int upper_x, upper_y, lower_x, lower_y;

  /* A new box object is created with #lepton_box_object_new().
   * Values for its fields are default and need to be modified. */
  new_obj = lepton_box_object_new (lepton_object_get_color (o_current),
                                   0, 0, 0, 0);

  /*
   * The dimensions of the new box are set with the ones of the original box.
   * The two boxes have the same line type and the same filling options.
   */
  upper_x = lepton_box_object_get_upper_x (o_current);
  upper_y = lepton_box_object_get_upper_y (o_current);
  lower_x = lepton_box_object_get_lower_x (o_current);
  lower_y = lepton_box_object_get_lower_y (o_current);

  lepton_box_object_set_upper_x (new_obj, upper_x);
  lepton_box_object_set_upper_y (new_obj, upper_y);
  lepton_box_object_set_lower_x (new_obj, lower_x);
  lepton_box_object_set_lower_y (new_obj, lower_y);

  lepton_object_set_line_options (new_obj,
                                  lepton_object_get_stroke_cap_type (o_current),
                                  lepton_object_get_stroke_type (o_current),
                                  lepton_object_get_stroke_width (o_current),
                                  lepton_object_get_stroke_dash_length (o_current),
                                  lepton_object_get_stroke_space_length (o_current));
  lepton_object_set_fill_options (new_obj,
                                  lepton_object_get_fill_type (o_current),
                                  lepton_object_get_fill_width (o_current),
                                  lepton_object_get_fill_pitch1 (o_current),
                                  lepton_object_get_fill_angle1 (o_current),
                                  lepton_object_get_fill_pitch2 (o_current),
                                  lepton_object_get_fill_angle2 (o_current));

  return new_obj;
}

/*! \brief Modify a box LeptonObject's coordinates.
 *  \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the box. The new coordinates of the corner identified by <B>whichone</B>
 *  are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 *  \param [in,out] object     Box LeptonObject to be modified.
 *  \param [in]     x          x coordinate.
 *  \param [in]     y          y coordinate.
 *  \param [in]     whichone   coordinate to change.
 *
 *  \note
 *  <B>whichone</B> can take the following values:
 *  <DL>
 *    <DT>*</DT><DD>BOX_UPPER_LEFT
 *    <DT>*</DT><DD>BOX_LOWER_LEFT
 *    <DT>*</DT><DD>BOX_UPPER_RIGHT
 *    <DT>*</DT><DD>BOX_LOWER_RIGHT
 *  </DL>
 */
void
lepton_box_object_modify (LeptonObject *object,
                          int x,
                          int y,
                          int whichone)
{
  int tmp;

  lepton_object_emit_pre_change_notify (object);

  /* change the position of the selected corner */
  switch(whichone) {
  case BOX_UPPER_LEFT:
    lepton_box_object_set_upper_x (object, x);
    lepton_box_object_set_upper_y (object, y);
    break;

  case BOX_LOWER_LEFT:
    lepton_box_object_set_upper_x (object, x);
    lepton_box_object_set_lower_y (object, y);
    break;

  case BOX_UPPER_RIGHT:
    lepton_box_object_set_lower_x (object, x);
    lepton_box_object_set_upper_y (object, y);
    break;

  case BOX_LOWER_RIGHT:
    lepton_box_object_set_lower_x (object, x);
    lepton_box_object_set_lower_y (object, y);
    break;

  default:
    return;
  }

  int upper_x, upper_y, lower_x, lower_y;
  upper_x = lepton_box_object_get_upper_x (object);
  upper_y = lepton_box_object_get_upper_y (object);
  lower_x = lepton_box_object_get_lower_x (object);
  lower_y = lepton_box_object_get_lower_y (object);

  /* need to update the upper left and lower right corners */
  if (upper_x > lower_x)
  {
    tmp = upper_x;
    lepton_box_object_set_upper_x (object, lower_x);
    lepton_box_object_set_lower_x (object, tmp);
  }

  if (upper_y < lower_y)
  {
    tmp = upper_y;
    lepton_box_object_set_upper_y (object, lower_y);
    lepton_box_object_set_lower_y (object, tmp);
  }

  lepton_object_emit_change_notify (object);

}

/*! \brief Create a box from a character string.
 *  \par Function Description
 *  This function gets the description of a box from the <B>*buf</B> character
 *  string.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 20000704 release
 *    <DT>*</DT><DD>the file format used for the releases after 2000704.
 *  </DL>
 *
 *  \param [in]     buf             Character string with box description.
 *  \param [in]     release_ver     liblepton release version number.
 *  \param [in]     fileformat_ver  liblepton file format version number.
 *  \param [in,out] err \c GError structure for error reporting,
 *                      or NULL to disable error reporting.
 *  \return The box #LeptonObject that was created, or NULL on error.
 */
LeptonObject*
lepton_box_object_read (const char buf[],
                        unsigned int release_ver,
                        unsigned int fileformat_ver,
                        GError **err)
{
  LeptonObject *new_obj;
  char type;
  int x1, y1;
  int width, height;
  int d_x1, d_y1;
  int d_x2, d_y2;
  int color;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  int box_end;
  int box_type;
  int box_filling;

  if (release_ver <= VERSION_20000704) {

  /*! \note
   *  The old geda file format, i.e. releases 20000704 and older, does not
   *  handle the line type and the filling of the box object. They are set
   *  to default.
   */

    if (sscanf (buf, "%c %d %d %d %d %d\n",
                &type, &x1, &y1, &width, &height, &color) != 6) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse box object"));
      return NULL;
    }

    box_width   = 0;
    box_end     = END_NONE;
    box_type    = TYPE_SOLID;
    box_length  = -1;
    box_space   = -1;

    box_filling = FILLING_HOLLOW;
    fill_width  = 0;
    angle1      = -1;
    pitch1      = -1;
    angle2      = -1;
    pitch2      = -1;

  } else {

    /*! \note
     *  The current line format to describe a box is a space separated list of
     *  characters and numbers in plain ASCII on a single line. The meaning of
     *  each item is described in the file format documentation.
     */
    if (sscanf (buf, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
                &type, &x1, &y1, &width, &height, &color,
                &box_width, &box_end, &box_type, &box_length,
                &box_space, &box_filling,
                &fill_width, &angle1, &pitch1, &angle2, &pitch2) != 17) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse box object"));
      return NULL;
    }
  }

  if (width == 0 || height == 0) {
    g_message (_("Found a zero width/height box "
                 "[ %1$c %2$d %3$d %4$d %5$d %6$d ]"),
               type, x1, y1, width, height, color);
  }

  if (!color_id_valid (color)) {
    g_message (_("Found an invalid color [ %1$s ]"), buf);
    g_message (_("Setting color to default color."));
    color = default_color_id();
  }

  /*! \note
   *  A box is internally described by its lower right and upper left corner
   *  whereas the line describe it with the lower left corner and the width
   *  and height.
   *
   *  A new object is allocated, initialized and added to the object list.
   *  Its filling and line type are set according to the values of the field
   *  on the line.
   */

  /* upper left corner of the box */
  d_x1 = x1;
  d_y1 = y1 + height; /* move box origin to top left */

  /* lower right corner of the box */
  d_x2 = x1 + width;  /* end points of the box */
  d_y2 = y1;

  /* create a new box */
  new_obj = lepton_box_object_new (color, d_x1, d_y1, d_x2, d_y2);
  /* set its line options */
  lepton_object_set_line_options (new_obj,
                                  (LeptonStrokeCapType) box_end,
                                  (LeptonStrokeType) box_type,
                                  box_width,
                                  box_length,
                                  box_space);
  /* set its fill options */
  lepton_object_set_fill_options (new_obj,
                                  (LeptonFillType) box_filling,
                                  fill_width,
                                  pitch1,
                                  angle1,
                                  pitch2,
                                  angle2);

  return new_obj;
}

/*! \brief Create a character string representation of a box object.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe the
 *  box object <B>*object</B>.
 *  It follows the post-20000704 release file format that handle the line type
 *  and fill options.
 *
 *  \param [in] object  The box LeptonObject to create string from.
 *  \return A pointer to the box character string.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
gchar*
lepton_box_object_to_buffer (const LeptonObject *object)
{
  int x1, y1;
  int width, height;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  LeptonStrokeCapType box_end;
  LeptonStrokeType box_type;
  LeptonFillType box_fill;
  char *buf;
  int upper_x, upper_y, lower_x, lower_y;

  /*! \note
   *  A box is internally represented by its lower right and upper left corner
   *  whereas it is described in the file format as its lower left corner and
   *  its width and height.
   */

  upper_x = lepton_box_object_get_upper_x (object);
  upper_y = lepton_box_object_get_upper_y (object);
  lower_x = lepton_box_object_get_lower_x (object);
  lower_y = lepton_box_object_get_lower_y (object);

  /* calculate the width and height of the box */
  width  = abs (lower_x - upper_x);
  height = abs (upper_y - lower_y);

  /* calculate the lower left corner of the box */
  x1 = upper_x;
  y1 = upper_y - height; /* move the origin to 0, 0*/

#if DEBUG
  printf("box: %d %d %d %d\n", x1, y1, width, height);
#endif

  /* description of the line type for the outline */
  box_end    = lepton_object_get_stroke_cap_type (object);
  box_width  = lepton_object_get_stroke_width (object);
  box_type   = lepton_object_get_stroke_type (object);
  box_length = lepton_object_get_stroke_dash_length (object);
  box_space  = lepton_object_get_stroke_space_length (object);

  /* description of the filling of the box */
  box_fill   = lepton_object_get_fill_type (object);
  fill_width = lepton_object_get_fill_width (object);
  angle1     = lepton_object_get_fill_angle1 (object);
  pitch1     = lepton_object_get_fill_pitch1 (object);
  angle2     = lepton_object_get_fill_angle2 (object);
  pitch2     = lepton_object_get_fill_pitch2 (object);

  buf = g_strdup_printf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
                        lepton_object_get_type (object),
                        x1, y1, width, height, lepton_object_get_color (object),
                        box_width, box_end, box_type, box_length, box_space,
                        box_fill,
                        fill_width, angle1, pitch1, angle2, pitch2);

  return(buf);
}

/*! \brief Translate a box position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the box
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 *  \param [in,out] object     Box LeptonObject to translate.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 */
void
lepton_box_object_translate (LeptonObject *object,
                             int dx,
                             int dy)
{
  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);
  int upper_x, upper_y, lower_x, lower_y;

  /* Do world coords */
  upper_x = lepton_box_object_get_upper_x (object);
  upper_y = lepton_box_object_get_upper_y (object);
  lower_x = lepton_box_object_get_lower_x (object);
  lower_y = lepton_box_object_get_lower_y (object);

  lepton_box_object_set_upper_x (object, upper_x + dx);
  lepton_box_object_set_upper_y (object, upper_y + dy);
  lepton_box_object_set_lower_x (object, lower_x + dx);
  lepton_box_object_set_lower_y (object, lower_y + dy);
}

/*! \brief Rotate box object using WORLD coordinates.
 *  \par Function Description
 *  This function rotates the box \a object around the point
 *  (\a world_centerx, \a world_centery) by \a angle degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      world_centerx  Rotation center x coordinate in WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Box LeptonObject to rotate.
 *
 */
void
lepton_box_object_rotate (int world_centerx,
                          int world_centery,
                          int angle,
                          LeptonObject *object)
{
  int newx1, newy1;
  int newx2, newy2;

  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);

  /*! \note
   *  Only 90 degree multiple and positive angles are allowed.
   */

  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;

  /*! \note
   *  The center of rotation (<B>world_centerx</B>, <B>world_centery</B>) is
   *  translated to the origin. The rotation of the upper left and lower right
   *  corner are then performed. Finally, the rotated box is translated back
   *  to its previous location.
   */
  /* translate object to origin */
  lepton_box_object_translate (object, -world_centerx, -world_centery);

  /* rotate the upper left corner of the box */
  lepton_point_rotate_90 (lepton_box_object_get_upper_x (object),
                          lepton_box_object_get_upper_y (object),
                          angle,
                          &newx1,
                          &newy1);

  /* rotate the lower left corner of the box */
  lepton_point_rotate_90 (lepton_box_object_get_lower_x (object),
                          lepton_box_object_get_lower_y (object),
                          angle,
                          &newx2,
                          &newy2);

  /* reorder the corners after rotation */
  lepton_box_object_set_upper_x (object, MIN (newx1, newx2));
  lepton_box_object_set_upper_y (object, MAX (newy1, newy2));
  lepton_box_object_set_lower_x (object, MAX (newx1, newx2));
  lepton_box_object_set_lower_y (object, MIN (newy1, newy2));

  /* translate object back to normal position */
  lepton_box_object_translate (object, world_centerx, world_centery);
}

/*! \brief Mirror box using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the box from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  The box is first translated to the origin, then mirrored and finally
 *  translated back at its previous position.
 *
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Box LeptonObject to mirror.
 */
void
lepton_box_object_mirror (int world_centerx,
                          int world_centery,
                          LeptonObject *object)
{
  int upper_x, upper_y, lower_x, lower_y;
  int newx1, newy1;
  int newx2, newy2;

  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);

  /* translate object to origin */
  lepton_box_object_translate (object, -world_centerx, -world_centery);

  upper_x = lepton_box_object_get_upper_x (object);
  upper_y = lepton_box_object_get_upper_y (object);
  lower_x = lepton_box_object_get_lower_x (object);
  lower_y = lepton_box_object_get_lower_y (object);

  /* mirror the corners */
  newx1 = -upper_x;
  newy1 = upper_y;
  newx2 = -lower_x;
  newy2 = lower_y;

  /* reorder the corners */
  lepton_box_object_set_upper_x (object, MIN (newx1, newx2));
  lepton_box_object_set_upper_y (object, MAX (newy1, newy2));
  lepton_box_object_set_lower_x (object, MAX (newx1, newx2));
  lepton_box_object_set_lower_y (object, MIN (newy1, newy2));

  /* translate back in position */
  lepton_box_object_translate (object, world_centerx, world_centery);
}

/*! \brief Get box bounding rectangle in WORLD coordinates.
 *  \par Function Description
 *  This function calculates the boundings of the box \a object in
 *  world units and stores them in the given #LeptonBounds
 *  instance \a bounds.
 *
 *  \param [in]  object     Box LeptonObject to read coordinates from.
 *  \param [out] bounds     The resulting object bounds.
 */
void
lepton_box_object_calculate_bounds (const LeptonObject *object,
                                    LeptonBounds *bounds)
{
  gint expand;

  lepton_bounds_init (bounds);

  g_return_if_fail (lepton_object_is_box (object));
  g_return_if_fail (object->box != NULL);

  lepton_box_calculate_bounds (object->box, bounds);

  expand = (lepton_object_get_stroke_width (object) + 1) / 2;

  /* This isn't strictly correct, but a 1st order approximation */
  lepton_bounds_expand (bounds, bounds, expand, expand);
}

/*! \brief get the position of the left bottom point
 *  \par Function Description
 *  This function gets the position of the bottom left point of a box object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
lepton_box_object_get_position (const LeptonObject *object,
                                gint *x,
                                gint *y)
{
  g_return_val_if_fail (lepton_object_is_box (object), FALSE);
  g_return_val_if_fail (object->box != NULL, FALSE);

  if (x != NULL) {
    *x = MIN (lepton_box_object_get_lower_x (object),
              lepton_box_object_get_upper_x (object));
  }

  if (y != NULL) {
    *y = MIN (lepton_box_object_get_lower_y (object),
              lepton_box_object_get_upper_y (object));
  }

  return TRUE;
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the perimeter of the box.
 *
 *  \param [in] object         The box LeptonObject.
 *  \param [in] x              The x coordinate of the given point.
 *  \param [in] y              The y coordinate of the given point.
 *  \param [in] force_solid    If true, force treating the object as solid.
 *  \param [in] include_hidden Take hidden text into account.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double
lepton_box_object_shortest_distance (LeptonObject *object,
                                     int x,
                                     int y,
                                     int force_solid,
                                     gboolean include_hidden)
{
  int solid;

  g_return_val_if_fail (object->box != NULL, G_MAXDOUBLE);

  solid = force_solid ||
    lepton_object_get_fill_type (object) != FILLING_HOLLOW;

  return lepton_box_shortest_distance (object->box, x, y, solid);
}
