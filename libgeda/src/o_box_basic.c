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

/*! \file o_box_basic.c
 *  \brief functions for the box object
 */

#include <config.h>
#include <math.h>
#include <stdio.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! \brief Create a BOX OBJECT
 *  \par Function Description
 *  This function creates a new object representing a box.
 *
 *  The box is described by its upper left corner - <B>x1</B>, <B>y1</B> - and
 *  its lower right corner - <B>x2</B>, <B>y2</B>.
 *  The <B>type</B> parameter must be equal to <B>OBJ_BOX</B>. The <B>color</B>
 *  corresponds to the color the box will be drawn with.
 *  The <B>OBJECT</B> structure is allocated with the #s_basic_new_object()
 *  function. The structure describing the box is allocated and initialized
 *  with the parameters given to the function.
 *
 *  Both the line type and the filling type are set to default values : solid
 *  line type with a width of 0, and no filling. It can be changed after
 *  with the #o_set_line_options() and #o_set_fill_options().
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     type         Box type.
 *  \param [in]     color        Box border color.
 *  \param [in]     x1           Upper x coordinate.
 *  \param [in]     y1           Upper y coordinate.
 *  \param [in]     x2           Lower x coordinate.
 *  \param [in]     y2           Lower y coordinate.
 *  \return The new OBJECT
 */
OBJECT *o_box_new(TOPLEVEL *toplevel,
		  char type, int color,
		  int x1, int y1, int x2, int y2)
{
  OBJECT *new_node;
  BOX *box;

  /* create the object */
  new_node = s_basic_new_object(type, "box");
  new_node->color = color;

  box = (BOX *) g_malloc(sizeof(BOX));
  new_node->box   = box;

  /* describe the box with its upper left and lower right corner */
  box->upper_x = x1;
  box->upper_y = y1;
  box->lower_x = x2;
  box->lower_y = y2;

  /* line type and filling initialized to default */
  o_set_line_options(toplevel, new_node,
		     END_NONE, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options(toplevel, new_node,
		     FILLING_HOLLOW, -1, -1, -1, -1, -1);

  /* compute the bounding box */
  o_box_recalc(toplevel, new_node);

  return new_node;
}

/*! \brief Copy a box to a list.
 *  \par Function Description
 *  The function #o_box_copy() creates a verbatim copy of the object
 *  pointed by <B>o_current</B> describing a box.
 *
 *  \param [in]      toplevel  The TOPLEVEL object.
 *  \param [in]      o_current  BOX OBJECT to copy.
 *  \return The new OBJECT
 */
OBJECT *o_box_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  /* A new box object is created with #o_box_new().
   * Values for its fields are default and need to be modified. */
  new_obj = o_box_new (toplevel, OBJ_BOX, o_current->color, 0, 0, 0, 0);

  /*
   * The dimensions of the new box are set with the ones of the original box.
   * The two boxes have the same line type and the same filling options.
   *
   * The coordinates and the values in world unit are computed with
   *  #o_box_recalc().
   */

  new_obj->box->upper_x = o_current->box->upper_x;
  new_obj->box->upper_y = o_current->box->upper_y;
  new_obj->box->lower_x = o_current->box->lower_x;
  new_obj->box->lower_y = o_current->box->lower_y;

  o_set_line_options(toplevel, new_obj, o_current->line_end,
		     o_current->line_type, o_current->line_width,
		     o_current->line_length, o_current->line_space);
  o_set_fill_options(toplevel, new_obj,
		     o_current->fill_type, o_current->fill_width,
		     o_current->fill_pitch1, o_current->fill_angle1,
		     o_current->fill_pitch2, o_current->fill_angle2);

  o_box_recalc(toplevel, new_obj);

  /* new_obj->attribute = 0;*/

  return new_obj;
} 

/*! \brief Modify a BOX OBJECT's coordinates.
 * \par Function Description
 * Modifies the coordinates of all four corners of \a box, by setting
 * the box to the rectangle enclosed by the points (\a x1, \a y1) and
 * (\a x2, \a y2).
 *
 * \param [in]     toplevel current #TOPLEVEL.
 * \param [in,out] object   box #OBJECT to be modified.
 * \param [in]     x1       x coordinate of first corner of box.
 * \param [in]     y1       y coordinate of first corner of box.
 * \param [in]     x2       x coordinate of second corner of box.
 * \param [in]     y2       y coordinate of second corner of box,
 */
void
o_box_modify_all (TOPLEVEL *toplevel, OBJECT *object,
                  int x1, int y1, int x2, int y2)
{
  o_emit_pre_change_notify (toplevel, object);

  object->box->lower_x = (x1 > x2) ? x1 : x2;
  object->box->lower_y = (y1 > y2) ? y2 : y1;

  object->box->upper_x = (x1 > x2) ? x2 : x1;
  object->box->upper_y = (y1 > y2) ? y1 : y2;

  /* recalculate the world coords and bounds */
  o_box_recalc(toplevel, object);
  o_emit_change_notify (toplevel, object);
}

/*! \brief Modify a BOX OBJECT's coordinates.
 *  \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the box. The new coordinates of the corner identified by <B>whichone</B>
 *  are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object     BOX OBJECT to be modified.
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
void o_box_modify(TOPLEVEL *toplevel, OBJECT *object,
		  int x, int y, int whichone)
{
	int tmp;

	o_emit_pre_change_notify (toplevel, object);

	/* change the position of the selected corner */
	switch(whichone) {
		case BOX_UPPER_LEFT:
			object->box->upper_x = x;
			object->box->upper_y = y;
			break;
			
		case BOX_LOWER_LEFT:
			object->box->upper_x = x;
			object->box->lower_y = y;
			break;
			
		case BOX_UPPER_RIGHT:
			object->box->lower_x = x;
			object->box->upper_y = y;
			break;
			
		case BOX_LOWER_RIGHT:
			object->box->lower_x = x;
			object->box->lower_y = y;
			break;
			
		default:
			return;
	}
	
	/* need to update the upper left and lower right corners */
	if(object->box->upper_x > object->box->lower_x) {
		tmp                  = object->box->upper_x;
		object->box->upper_x = object->box->lower_x;
		object->box->lower_x = tmp;
	}
	
	if(object->box->upper_y < object->box->lower_y) {
		tmp                  = object->box->upper_y;
		object->box->upper_y = object->box->lower_y;
		object->box->lower_y = tmp;
	}
	
	/* recalculate the world coords and the boundings */
	o_box_recalc(toplevel, object);
	o_emit_change_notify (toplevel, object);
  
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
 *  \param [in]     toplevel       The TOPLEVEL object.
 *  \param [in]     buf             Character string with box description.
 *  \param [in]     release_ver     libgeda release version number.
 *  \param [in]     fileformat_ver  libgeda file format version number.
 *  \return The BOX OBJECT that was created, or NULL on error.
 */
OBJECT *o_box_read (TOPLEVEL *toplevel, char buf[],
                    unsigned int release_ver, unsigned int fileformat_ver, GError **err)
{
  OBJECT *new_obj;
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
      g_set_error(err, EDA_ERROR, EDA_ERROR_READ, _("Failed to parse box object\n"));
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
      g_set_error(err, EDA_ERROR, EDA_ERROR_READ, _("Failed to parse box object\n"));
      return NULL;
    }
  }

  if (width == 0 || height == 0) {
    s_log_message (_("Found a zero width/height box [ %c %d %d %d %d %d ]\n"),
                   type, x1, y1, width, height, color);
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message (_("Found an invalid color [ %s ]\n"), buf);
    s_log_message (_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
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
  new_obj = o_box_new (toplevel, type, color, d_x1, d_y1, d_x2, d_y2);
  /* set its line options */
  o_set_line_options (toplevel, new_obj,
                      box_end, box_type, box_width,
                      box_length, box_space);
  /* set its fill options */
  o_set_fill_options (toplevel, new_obj,
                      box_filling, fill_width,
                      pitch1, angle1, pitch2, angle2);

  return new_obj;
}

/*! \brief Create a character string representation of a BOX.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe the
 *  box object <B>*object</B>.
 *  It follows the post-20000704 release file format that handle the line type
 *  and fill options.
 *
 *  \param [in] toplevel  The TOPLEVEL structure.
 *  \param [in] object  The BOX OBJECT to create string from.
 *  \return A pointer to the BOX character string.
 *
 *  \warning
 *  Caller must g_free returned character string.
 */
char *o_box_save(TOPLEVEL *toplevel, OBJECT *object)
{
  int x1, y1; 
  int width, height;
  int box_width, box_space, box_length;
  int fill_width, angle1, pitch1, angle2, pitch2;
  OBJECT_END box_end;
  OBJECT_TYPE box_type;
  OBJECT_FILLING box_fill;
  char *buf;

  /*! \note
   *  A box is internally represented by its lower right and upper left corner
   *  whereas it is described in the file format as its lower left corner and
   *  its width and height.
   */

  /* calculate the width and height of the box */
  width  = abs(object->box->lower_x - object->box->upper_x); 
  height = abs(object->box->upper_y - object->box->lower_y);

  /* calculate the lower left corner of the box */
  x1 = object->box->upper_x;
  y1 = object->box->upper_y - height; /* move the origin to 0, 0*/

#if DEBUG
  printf("box: %d %d %d %d\n", x1, y1, width, height);
#endif

  /* description of the line type for the outline */
  box_end    = object->line_end;
  box_width  = object->line_width;
  box_type   = object->line_type;
  box_length = object->line_length;
  box_space  = object->line_space;
  
  /* description of the filling of the box */
  box_fill   = object->fill_type;
  fill_width = object->fill_width;
  angle1     = object->fill_angle1;
  pitch1     = object->fill_pitch1;
  angle2     = object->fill_angle2;
  pitch2     = object->fill_pitch2;

  buf = g_strdup_printf("%c %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d", 
			object->type,
			x1, y1, width, height, object->color,
			box_width, box_end, box_type, box_length, box_space, 
			box_fill,
			fill_width, angle1, pitch1, angle2, pitch2);
			
  return(buf);
}

/*! \brief Translate a BOX position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the box
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 *  \param [in,out] object     BOX OBJECT to translate.
 */
void o_box_translate_world(TOPLEVEL *toplevel, int dx, int dy, OBJECT *object)
{
  if (object == NULL) printf("btw NO!\n");

  /* Do world coords */
  object->box->upper_x = object->box->upper_x + dx;
  object->box->upper_y = object->box->upper_y + dy;
  object->box->lower_x = object->box->lower_x + dx;
  object->box->lower_y = object->box->lower_y + dy;

  /* recalc the screen coords and the bounding box */
  o_box_recalc(toplevel, object);
}

/*! \brief Rotate BOX OBJECT using WORLD coordinates. 
 *  \par Function Description
 *  The function #o_box_rotate_world() rotate the box described by
 *  <B>*object</B> around the (<B>world_centerx</B>, <B>world_centery</B>) point by
 *  <B>angle</B> degrees.
 *  The center of rotation is in world unit.
 *
 *  \param [in]      toplevel      The TOPLEVEL object.
 *  \param [in]      world_centerx  Rotation center x coordinate in WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         BOX OBJECT to rotate.
 *
 */
void o_box_rotate_world(TOPLEVEL *toplevel,
			int world_centerx, int world_centery, int angle,
			OBJECT *object)
{
  int newx1, newy1;
  int newx2, newy2;

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
  object->box->upper_x -= world_centerx;
  object->box->upper_y -= world_centery;
  object->box->lower_x -= world_centerx;
  object->box->lower_y -= world_centery;
  
  /* rotate the upper left corner of the box */
  rotate_point_90(object->box->upper_x, object->box->upper_y, angle,
		  &newx1, &newy1);
  
  /* rotate the lower left corner of the box */
  rotate_point_90(object->box->lower_x, object->box->lower_y, angle,
		  &newx2, &newy2);
  
  /* reorder the corners after rotation */
  object->box->upper_x = min(newx1,newx2);
  object->box->upper_y = max(newy1,newy2);
  object->box->lower_x = max(newx1,newx2);
  object->box->lower_y = min(newy1,newy2);
  
  /* translate object back to normal position */
  object->box->upper_x += world_centerx;
  object->box->upper_y += world_centery;
  object->box->lower_x += world_centerx;
  object->box->lower_y += world_centery;
  
  /* recalc boundings and world coords */
  o_box_recalc(toplevel, object);
}

/*! \brief Mirror BOX using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the box from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  The box is first translated to the origin, then mirrored and finally
 *  translated back at its previous position.
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         BOX OBJECT to mirror.
 */
void o_box_mirror_world(TOPLEVEL *toplevel,
			int world_centerx, int world_centery,
			OBJECT *object)
{
  int newx1, newy1;
  int newx2, newy2;

  /* translate object to origin */
  object->box->upper_x -= world_centerx;
  object->box->upper_y -= world_centery;
  object->box->lower_x -= world_centerx;
  object->box->lower_y -= world_centery;

  /* mirror the corners */
  newx1 = -object->box->upper_x;
  newy1 = object->box->upper_y;
  newx2 = -object->box->lower_x;
  newy2 = object->box->lower_y;

  /* reorder the corners */
  object->box->upper_x = min(newx1,newx2);
  object->box->upper_y = max(newy1,newy2);
  object->box->lower_x = max(newx1,newx2);
  object->box->lower_y = min(newy1,newy2);

  /* translate back in position */
  object->box->upper_x += world_centerx;
  object->box->upper_y += world_centery;
  object->box->lower_x += world_centerx;
  object->box->lower_y += world_centery;

  /* recalc boundings and world coords */
  o_box_recalc(toplevel, object);
  
}

/*! \brief Recalculate BOX coordinates in WORLD units.
 *  \par Function Description
 *  This function recalculates the box coordinates and its 
 *  bounding are recalculated as well.
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in,out] o_current  BOX OBJECT to be recalculated.
 */
void o_box_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, top, right, bottom;

  if (o_current->box == NULL) {
    return;
  }

  /* update the bounding box - world unit */
  world_get_box_bounds(toplevel, o_current, &left, &top, &right, &bottom);
  o_current->w_left   = left;
  o_current->w_top    = top;
  o_current->w_right  = right;
  o_current->w_bottom = bottom;
  o_current->w_bounds_valid = TRUE;
}

/*! \brief Get BOX bounding rectangle in WORLD coordinates.
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and <B>bottom</B>
 *  parameters to the boundings of the box object described in <B>*box</B>
 *  in world units.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object     BOX OBJECT to read coordinates from.
 *  \param [out] left       Left box coordinate in WORLD units.
 *  \param [out] top        Top box coordinate in WORLD units.
 *  \param [out] right      Right box coordinate in WORLD units.
 *  \param [out] bottom     Bottom box coordinate in WORLD units.
 */
void world_get_box_bounds(TOPLEVEL *toplevel, OBJECT *object,
                          int *left, int *top, int *right, int *bottom)
{
  int halfwidth;

  halfwidth = object->line_width / 2;

  *left   = min(object->box->upper_x, object->box->lower_x);
  *top    = min(object->box->upper_y, object->box->lower_y);
  *right  = max(object->box->upper_x, object->box->lower_x);
  *bottom = max(object->box->upper_y, object->box->lower_y);

  /* This isn't strictly correct, but a 1st order approximation */
  *left   -= halfwidth;
  *top    -= halfwidth;
  *right  += halfwidth;
  *bottom += halfwidth;
}

/*! \brief get the position of the left bottom point
 *  \par Function Description
 *  This function gets the position of the bottom left point of a box object.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean o_box_get_position (TOPLEVEL *toplevel, gint *x, gint *y,
                              OBJECT *object)
{
  *x = min(object->box->lower_x, object->box->upper_x);
  *y = min(object->box->lower_y, object->box->upper_y);
  return TRUE;
}
                 
/*! \brief Print BOX to Postscript document.
 *  \par Function Description
 *  This function prints the box described by the <B>o_current</B>
 *  parameter to a Postscript document. It takes into account its line
 *  type and fill type.
 *  The Postscript document is descibed by the file pointer <B>fp</B>.
 *
 *  The validity of the <B>o_current</B> parameter is verified : a null pointer
 *  causes an error message and a return.
 *
 *  The description of the box is extracted from
 *  the <B>o_current</B> parameter :
 *  the coordinates of the box - upper left corner and width and
 *  height of the box -, its line type, its fill type.
 *
 *  The outline and the inside of the box are successively handled by two
 *  differend sets of functions.
 *  
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to Postscript document.
 *  \param [in] o_current  BOX OBJECT to write to document.
 *  \param [in] origin_x   Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y   Page y coordinate to place BOX OBJECT.
 */
void o_box_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		 int origin_x, int origin_y)
{
  int x, y, width, height;
  int color;
  int line_width, length, space;
  int fill_width, angle1, pitch1, angle2, pitch2;
  void (*outl_func)() = NULL;
  void (*fill_func)() = NULL;

  if (o_current == NULL) {
    printf("got null in o_box_print\n");
    return;
  }

  x = o_current->box->upper_x;
  y = o_current->box->upper_y;
  width  = abs(o_current->box->lower_x - o_current->box->upper_x);
  height = abs(o_current->box->lower_y - o_current->box->upper_y);
  color  = o_current->color;

  /*! \note
   *  Depending on the type of the line for this particular box, the
   *  appropriate function is chosen among #o_box_print_solid(),
   *  #o_box_print_dotted(), #o_box_print_dashed(),
   *  #o_box_print_center() and #o_box_print_phantom().
   *
   *  The needed parameters for each of these type is extracted from the
   *  <B>o_current</B> object. Depending on the type, unused parameters are
   *  set to -1.
   *
   *  In the eventuality of a length and/or space null, the line is printed
   *  solid to avoid and endless loop produced by other functions in such a
   *  case.
   */
  line_width = o_current->line_width;
  
  if(line_width <=2) {
    if(toplevel->line_style == THICK) {
      line_width=LINE_WIDTH;
    } else {
      line_width=2;
    }
  }
  length = o_current->line_length;
  space  = o_current->line_space;

  switch(o_current->line_type) {
    case(TYPE_SOLID):
      length = -1; space  = -1;
      outl_func = o_box_print_solid;
      break;
      
    case(TYPE_DOTTED):
      length = -1;
      outl_func = o_box_print_dotted;
      break;
		
    case(TYPE_DASHED):
      outl_func = o_box_print_dashed;
      break;
      
    case(TYPE_CENTER):
      outl_func = o_box_print_center;
      break;
		
    case(TYPE_PHANTOM):
      outl_func = o_box_print_phantom;
      break;
		
    case(TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1; space  = -1;
      outl_func = o_box_print_solid;
      break;
  }

  if((length == 0) || (space == 0)) {
    length = -1; space  = -1;
    outl_func = o_box_print_solid;
  }

  (*outl_func)(toplevel, fp,
	       x, y, width, height,
	       color,
	       line_width,
	       length, space,
	       origin_x, origin_y);

  /*! \note
   *  If the filling type of the box is not <B>HOLLOW</B>, the appropriate
   *  function is chosen among #o_box_print_filled(), #o_box_print_mesh()
   *  and #o_box_print_hatch(). The corresponding parameters are extracted
   *  from the <B>o_current</B> object and corrected afterward.
   *
   *  The case where <B>pitch1</B> and <B>pitch2</B> are null or negative is
   *  avoided as it leads to an endless loop in most of the called functions.
   *  In such a case, the box is printed filled. Unused parameters for each of
   *  these functions are set to -1 or any passive value.
   */
  if(o_current->fill_type != FILLING_HOLLOW) {
    fill_width = o_current->fill_width;
    angle1     = o_current->fill_angle1;
    pitch1     = o_current->fill_pitch1;
    angle2     = o_current->fill_angle2;
    pitch2     = o_current->fill_pitch2;
	
    switch(o_current->fill_type) {
      case(FILLING_FILL):
	angle1 = -1; pitch1 = 1;
	angle2 = -1; pitch2 = 1;
	fill_width = -1;
	fill_func = o_box_print_filled;
	break;
			
      case(FILLING_MESH):
	fill_func = o_box_print_mesh;
	break;
			
      case(FILLING_HATCH):
	angle2 = -1; pitch2 = 1;
	fill_func = o_box_print_hatch;
	break;
			
      case(FILLING_VOID):
	/* Unused for now, print it filled */
	angle1 = -1; pitch1 = 1;
	angle2 = -1; pitch2 = 1;
	fill_width = -1;
	fill_func = o_box_print_filled;
	break;
      case(FILLING_HOLLOW):
	/* nop */
	break;
	
    }
    
    if((pitch1 <= 0) || (pitch2 <= 0)) {
      angle1 = -1; pitch1 = 1;
      angle2 = -1; pitch2 = 1;
      fill_func = o_box_print_filled;
    }
    
    (*fill_func)(toplevel, fp,
                 x, y, width, height,
                 color,
                 fill_width,
                 angle1, pitch1, angle2, pitch2,
                 origin_x, origin_y);
  }
}

/*! \brief Print a solid BOX to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a solid line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. 
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored.
 *
 *  It uses the function #o_line_print_solid() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] line_width  BOX Line width.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void
o_box_print_solid(TOPLEVEL *toplevel, FILE *fp,
                  int x, int y,
                  int width, int height,
                  int color,
                  int line_width, int length, int space, 
                  int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_solid(toplevel, fp,
                     x1, y1, x1 + width, y1,
                     color,
                     line_width, length, space,
                     origin_x, origin_y);
  o_line_print_solid(toplevel, fp,
                     x1 + width, y1, x1 + width, y1 + height,
                     color,
                     line_width, length, space,
                     origin_x, origin_y);
  o_line_print_solid(toplevel, fp,
                     x1 + width, y1 + height, x1, y1 + height,
                     color,
                     line_width, length, space,
                     origin_x, origin_y);
  o_line_print_solid(toplevel, fp,
                     x1, y1 + height, x1, y1,
                     color,
                     line_width, length, space,
                     origin_x, origin_y);
}

/*! \brief Print a dotted BOX to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a dotted line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. 
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> is ignored.
 *
 *  It uses the function #o_line_print_dotted() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] line_width  BOX Line width.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void o_box_print_dotted(TOPLEVEL *toplevel, FILE *fp,
			int x, int y,
			int width, int height,
			int color,
			int line_width, int length, int space, 
			int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_dotted(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_dotted(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_dotted(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_dotted(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
}

/*! \brief Print a dashed BOX to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a dashed line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. 
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #o_line_print_dashed() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] line_width  BOX Line width.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void o_box_print_dashed(TOPLEVEL *toplevel, FILE *fp,
			int x, int y,
			int width, int height,
			int color,
			int line_width, int length, int space, 
			int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  
  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_dashed(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_dashed(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_dashed(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_dashed(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
}

/*! \brief Print centered line type BOX to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a centered line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. 
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #o_line_print_center() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] line_width  BOX Line width.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void o_box_print_center(TOPLEVEL *toplevel, FILE *fp,
			int x, int y,
			int width, int height,
			int color,
			int line_width, int length, int space, 
			int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_center(toplevel, fp,
                      x1, y1, x1 + width, y1,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_center(toplevel, fp,
                      x1 + width, y1, x1 + width, y1 + height,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_center(toplevel, fp,
                      x1 + width, y1 + height, x1, y1 + height,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
  o_line_print_center(toplevel, fp,
                      x1, y1 + height, x1, y1,
                      color,
                      line_width, length, space,
                      origin_x, origin_y);
}

/*! \brief Print phantom line type BOX to Postscript document.
 *  \par Function Description
 *  This function prints the outline of a box when a phantom line type is
 *  required. The box is defined by the coordinates of its upper left corner
 *  in (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. 
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  It uses the function #o_line_print_phantom() to print the outline.
 *  It performs four calls to this function, one for each of its side.
 *
 *  All dimensions are in mils.
 *  
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] line_width  BOX Line width.
 *  \param [in] length      Dashed line length.
 *  \param [in] space       Amount of space between dashes.
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void o_box_print_phantom(TOPLEVEL *toplevel, FILE *fp,
			 int x, int y,
			 int width, int height,
			 int color,
			 int line_width, int length, int space, 
			 int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y - height; /* move the origin to 0, 0*/

  o_line_print_phantom(toplevel, fp,
                       x1, y1, x1 + width, y1,
                       color,
                       line_width, length, space,
                       origin_x, origin_y);
  o_line_print_phantom(toplevel, fp,
                       x1 + width, y1, x1 + width, y1 + height,
                       color,
                       line_width, length, space,
                       origin_x, origin_y);
  o_line_print_phantom(toplevel, fp,
                       x1 + width, y1 + height, x1, y1 + height,
                       color,
                       line_width, length, space,
                       origin_x, origin_y);
  o_line_print_phantom(toplevel, fp,
                       x1, y1 + height, x1, y1,
                       color,
                       line_width, length, space,
                       origin_x, origin_y);
}

/*! \brief Print a solid pattern BOX to Postscript document.
 *  \par Function Description
 *  The function prints a filled box with a solid pattern. No outline is
 *  printed. 
 *  The box is defined by the coordinates of its upper left corner in
 *  (<B>x</B>,<B>y</B>) and its width and height given by the <B>width</B> and
 *  <B>height</B> parameters. The postscript file is defined by the file
 *  pointer <B>fp</B>.
 *  <B>fill_width</B>, <B>angle1</B> and <B>pitch1</B>, <B>angle2</B> and <B>pitch2</B>
 *  parameters are ignored in this functions but kept for compatibility
 *  with other fill functions.
 *
 *  It uses the fbox postscript function defined in the prolog to
 *  specify a filled box.
 * 
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] fill_width  BOX fill width. (unused).
 *  \param [in] angle1      (unused).
 *  \param [in] pitch1      (unused).
 *  \param [in] angle2      (unused).
 *  \param [in] pitch2      (unused).
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void o_box_print_filled(TOPLEVEL *toplevel, FILE *fp,
			int x, int y,
			int width, int height,
			int color,
			int fill_width,
			int angle1, int pitch1,
			int angle2, int pitch2,
			int origin_x, int origin_y)
{
  int x1, y1;

  f_print_set_color(toplevel, fp, color);

  x1 = x;
  y1 = y-height; /* move the origin to 0, 0*/
  fprintf(fp, "%d %d %d %d fbox\n",
	  width, height,
	  x1-origin_x, y1-origin_y);
	  
}

/*! \brief Print a mesh pattern BOX to Postscript document.
 *  \par Function Description
 *  This function prints a meshed box. No outline is printed. The box is
 *  defined by the coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and
 *  its width and height given by the <B>width</B> and <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  The inside mesh is achieved by two successive call to the
 *  #o_box_print_hatch() function, given <B>angle1</B> and <B>pitch1</B> the first
 *  time and <B>angle2</B> and <B>pitch2</B> the second time.
 *
 *  Negative or null values for <B>pitch1</B> and/or <B>pitch2</B> are not allowed
 *  as it leads to an endless loop in #o_box_print_hatch().
 * 
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] fill_width  BOX fill width.
 *  \param [in] angle1      1st angle for mesh pattern.
 *  \param [in] pitch1      1st pitch for mesh pattern.
 *  \param [in] angle2      2nd angle for mesh pattern.
 *  \param [in] pitch2      2nd pitch for mesh pattern.
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void o_box_print_mesh(TOPLEVEL *toplevel, FILE *fp,
		      int x, int y,
		      int width, int height,
		      int color,
		      int fill_width,
		      int angle1, int pitch1,
		      int angle2, int pitch2,
		      int origin_x, int origin_y)
{
  o_box_print_hatch(toplevel, fp,
                    x, y, width, height,
                    color,
                    fill_width,
                    angle1, pitch1, -1, -1,
                    origin_x, origin_y);
  o_box_print_hatch(toplevel, fp,
                    x, y, width, height,
                    color,
                    fill_width,
                    angle2, pitch2, -1, -1,
                    origin_x, origin_y);

}

/*! \brief Print a hatch pattern BOX to Postscript document.
 *  \par Function Description
 *  The function prints a hatched box. No outline is printed. The box is
 *  defined by the coordinates of its upper left corner in (<B>x</B>,<B>y</B>) and
 *  its width and height given by the <B>width</B> and <B>height</B> parameters.
 *  The postscript file is defined by the file pointer <B>fp</B>. 
 *  <B>fill_width</B>, <B>angle1</B>, <B>pitch1</B> parameters define the way the box
 *  has to be hatched.
 *  <B>angle2</B> and <B>pitch2</B> parameters are unused but kept for compatibility
 *  with other fill functions.
 *
 *  Negative or null values for <B>pitch1</B> are not allowed as it leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  \param [in] toplevel   The TOPLEVEL object.
 *  \param [in] fp          FILE pointer to Postscript document.
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] color       BOX color.
 *  \param [in] fill_width  BOX fill width.
 *  \param [in] angle1      Angle of hatch pattern.
 *  \param [in] pitch1      Pitch of hatch pattern.
 *  \param [in] angle2      (unused).
 *  \param [in] pitch2      (unused).
 *  \param [in] origin_x    Page x coordinate to place BOX OBJECT.
 *  \param [in] origin_y    Page y coordinate to place BOX OBJECT.
 */
void o_box_print_hatch(TOPLEVEL *toplevel, FILE *fp,
		       int x, int y,
		       int width, int height,
		       int color,
		       int fill_width,
		       int angle1, int pitch1,
		       int angle2, int pitch2,
		       int origin_x, int origin_y)
{
  BOX box;
  gint index;
  GArray *lines;

  g_return_if_fail(toplevel != NULL);
  g_return_if_fail(fp != NULL);

  f_print_set_color(toplevel, fp, color);

  /* Avoid printing line widths too small */
  if (fill_width <= 1) fill_width = 2;

  lines = g_array_new(FALSE, FALSE, sizeof(LINE));

  box.upper_x = x;
  box.upper_y = y;
  box.lower_x = x + width;
  box.lower_y = y - height;    /* Hmmm... */

  m_hatch_box(&box, angle1, pitch1, lines);

  for(index=0; index<lines->len; index++) {
    LINE *line = &g_array_index(lines, LINE, index);

    fprintf(fp,"%d %d %d %d %d line\n",
            line->x[0], line->y[0],
            line->x[1], line->y[1],
            fill_width);
  }

  g_array_free(lines, TRUE);
}

/*! \brief Calculates the distance between the given point and the closest
 * point on the perimeter of the box.
 *
 *  \param [in] object       The box OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double o_box_shortest_distance (OBJECT *object, int x, int y, int force_solid)
{
  int solid;

  g_return_val_if_fail (object->box != NULL, G_MAXDOUBLE);

  solid = force_solid || object->fill_type != FILLING_HOLLOW;

  return m_box_shortest_distance (object->box, x, y, solid);
}

