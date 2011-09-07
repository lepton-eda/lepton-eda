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

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

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
		     END_NONE, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options(toplevel, new_node,
		     FILLING_HOLLOW, -1, -1, -1, -1, -1);

  /* compute bounding box */
  o_line_recalc(toplevel, new_node);

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
   *
   * The bounding box are computed with
   * #o_line_recalc().
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
  o_line_recalc(toplevel, o_current);
  
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
  o_line_recalc(toplevel, object);
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
OBJECT *o_line_read (TOPLEVEL *toplevel, char buf[],
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
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse line object\n"));
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
        g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse line object\n"));
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
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 *  \param [in,out] object     Line OBJECT to translate.
 */
void o_line_translate_world(TOPLEVEL *toplevel,
			    int dx, int dy, OBJECT *object)
{
  if (object == NULL) printf("ltw NO!\n");

  /* Update world coords */
  object->line->x[0] = object->line->x[0] + dx;
  object->line->y[0] = object->line->y[0] + dy;
  object->line->x[1] = object->line->x[1] + dx;
  object->line->y[1] = object->line->y[1] + dy;
  
  /* Update bounding box */
  o_line_recalc (toplevel, object);
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
void o_line_rotate_world(TOPLEVEL *toplevel,
			 int world_centerx, int world_centery, int angle,
			 OBJECT *object)
{
  int newx, newy;
	
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
  o_line_translate_world(toplevel, -world_centerx, -world_centery, object);

  /* rotate line end 1 */
  rotate_point_90(object->line->x[0], object->line->y[0], angle,
		  &newx, &newy);

  object->line->x[0] = newx;
  object->line->y[0] = newy;
  
  /* rotate line end 2 */
  rotate_point_90(object->line->x[1], object->line->y[1], angle,
		  &newx, &newy);
  
  object->line->x[1] = newx;
  object->line->y[1] = newy;

  /* translate object back to normal position */
  o_line_translate_world(toplevel, world_centerx, world_centery, object);
  
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
void o_line_mirror_world(TOPLEVEL *toplevel, int world_centerx,
			 int world_centery, OBJECT *object)
{
  /* translate object to origin */
  o_line_translate_world(toplevel, -world_centerx, -world_centery, object);

  /* mirror the line ends */
  object->line->x[0] = -object->line->x[0];
  object->line->x[1] = -object->line->x[1];

  /* translate back in position */
  o_line_translate_world(toplevel, world_centerx, world_centery, object);
  
}

/*! \brief Recalculate line coordinates in SCREEN units.
 *  \par Function Description
 *  This function recalculate the bounding box of the <B>o_current</B>
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in,out] o_current  Line OBJECT to be recalculated.
 */
void o_line_recalc(TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, right, top, bottom;

  if (o_current->line == NULL) {
    return;
  }
  
  /* update the bounding box - screen unit */
  world_get_line_bounds(toplevel, o_current,
		  &left, &top, &right, &bottom);
  o_current->w_left   = left;
  o_current->w_top    = top;
  o_current->w_right  = right;
  o_current->w_bottom = bottom;
  o_current->w_bounds_valid = TRUE;
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
  int halfwidth;

  halfwidth = object->line_width / 2;

  *left = min( object->line->x[0], object->line->x[1] );
  *top = min( object->line->y[0], object->line->y[1] );
  *right = max( object->line->x[0], object->line->x[1] );
  *bottom = max( object->line->y[0], object->line->y[1] );

  /* This isn't strictly correct, but a 1st order approximation */
  *left   -= halfwidth;
  *top    -= halfwidth;
  *right  += halfwidth;
  *bottom += halfwidth;
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


/*! \brief Print line to Postscript document.
 *  \par Function Description
 *  This function prints the line described by the <B>o_current</B>
 *  parameter to a Postscript document.
 *  The Postscript document is described by the <B>fp</B> file pointer.
 *
 *  Parameters of the line are extracted from object pointed by
 *  <B>o_current</B>.
 *  
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to Postscript document.
 *  \param [in] o_current  Line OBJECT to write to document.
 *  \param [in] origin_x   Page x coordinate to place line OBJECT.
 *  \param [in] origin_y   Page y coordinate to place line OBJECT.
 */
void o_line_print(TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
		  int origin_x, int origin_y)
{
  int x1, y1, x2, y2;
  int color;
  int line_width, length, space;
  void (*outl_func)() = NULL;
	
  if (o_current == NULL) {
    printf("got null in o_line_print\n");
    return;
  }

  x1    = o_current->line->x[0];
  y1    = o_current->line->y[0];
  x2    = o_current->line->x[1];
  y2    = o_current->line->y[1];
  color = o_current->color;

  /*
   * Depending on the type of the line for this particular line, the
   * appropriate function is chosen among
   * #o_line_print_solid(), #o_line_print_dotted()#, #o_line_print_dashed(),
   * #o_line_print_center() and #o_line_print_phantom().
   *
   * The needed parameters for each of these types are extracted from the
   * <B>o_current</B> object. Depending on the type, unused parameters are
   * set to -1.
   *
   * In the eventuality of a length and/or space null, the line is printed
   * solid to avoid and endless loop produced by other functions.
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
      length = -1; space = -1;
      outl_func = o_line_print_solid;
      break;
      
    case(TYPE_DOTTED):
      length = -1;
      outl_func = o_line_print_dotted;
      break;
      
    case(TYPE_DASHED):
      outl_func = o_line_print_dashed;
      break;
      
    case(TYPE_CENTER):
      outl_func = o_line_print_center;
      break;
      
    case(TYPE_PHANTOM):
      outl_func = o_line_print_phantom;
      break;
      
    case(TYPE_ERASE):
      /* Unused for now, print it solid */
      length = -1; space = -1;
      outl_func =  o_line_print_solid;
      break;
  }

  if((length == 0) || (space == 0)) {
    length = -1; space = -1;
    outl_func = o_line_print_solid;
  }
  
  (*outl_func)(toplevel, fp,
	       x1 - origin_x, y1 - origin_y,
	       x2 - origin_x, y2 - origin_y,
	       color,
	       line_width, length, space,
	       origin_x, origin_y);
}

/*! \brief Print a solid line to Postscript document.
 *  \par Function Description
 *  This function prints a line when a solid line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  The parameters <B>length</B> and <B>space</B> are ignored whereas
 *  <B>line_width</B> specifies the width of the printed line.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x1            Upper x coordinate.
 *  \param [in] y1            Upper y coordinate.
 *  \param [in] x2            Lower x coordinate.
 *  \param [in] y2            Lower y coordinate.
 *  \param [in] color         Line color.
 *  \param [in] line_width    Width of line.
 *  \param [in] length        (unused).
 *  \param [in] space         (unused).
 *  \param [in] origin_x      Page x coordinate to place line OBJECT.
 *  \param [in] origin_y      Page y coordinate to place line OBJECT.
 */
void o_line_print_solid(TOPLEVEL *toplevel, FILE *fp,
			int x1, int y1, int x2, int y2,
			int color,
			int line_width, int length, int space,
			int origin_x, int origin_y)
{
  f_print_set_color(toplevel, fp, color);
  
  fprintf(fp,"%d %d %d %d %d line\n",
	  x1,y1,x2,y2, line_width);
}

/*! \brief Print a dotted line to Postscript document.
 *  \par Function Description
 *  This function prints a line when a dotted line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *  The parameter <B>length</B> is ignored whereas <B>line_width</B>
 *  specifies the diameter of the dots and <B>space</B> the distance
 *  between two dots.
 *
 *  A negative value for <B>space</B> leads to an endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed with.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x1            Upper x coordinate.
 *  \param [in] y1            Upper y coordinate.
 *  \param [in] x2            Lower x coordinate.
 *  \param [in] y2            Lower y coordinate.
 *  \param [in] color         Line color.
 *  \param [in] line_width    Width of line.
 *  \param [in] length        (unused).
 *  \param [in] space         Space between dots.
 *  \param [in] origin_x      Page x coordinate to place line OBJECT.
 *  \param [in] origin_y      Page y coordinate to place line OBJECT.
 */
void o_line_print_dotted(TOPLEVEL *toplevel, FILE *fp,
			 int x1, int y1, int x2, int y2,
			 int color,
			 int line_width, int length, int space,
			 int origin_x, int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1;
  double xa, ya;
  
  f_print_set_color(toplevel, fp, color);
  
  /* The dotted line command takes an array of dots so print out the
   * beginnings of the array 
   */
  fprintf(fp,"[");
  /* is the width relevant for a dot (circle) ? */
  /* f_print_set_line_width(fp, line_width); */
  
  /*
   * Depending on the slope of the line the space parameter is
   * projected on each of the two directions x and y resulting
   * in <B>dx1</B> and <B>dy1</B>. Starting from one end by increments
   * of space the dots are printed.
   *
   * A dot is represented by a filled circle. Position of the
   * circle is (<B>xa</B>, <B>ya</B>) and its radius is the <B>line_width</B>
   * parameter.
   */
  
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);
  l = sqrt((dx * dx) + (dy * dy));
  
  dx1 = (dx * space) / l;
  dy1 = (dy * space) / l;
  
  d = 0;
  xa = x1; ya = y1;
  while(d < l) {
    
    fprintf(fp,"[%d %d] ",
	    (int)xa, (int)ya);
    d = d + space;
    xa = xa + dx1;
    ya = ya + dy1;
  }
  
  fprintf(fp,"] %d dashed\n",line_width);
  
}


/*! \brief Print a dashed line to Postscript document.
 *  \par Function Description
 *  This function prints a line when a dashed line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The postscript file is defined by the file pointer <B>fp</B>.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed and
 *  the width of the line - that is the width of the dashes.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x1            Upper x coordinate.
 *  \param [in] y1            Upper y coordinate.
 *  \param [in] x2            Lower x coordinate.
 *  \param [in] y2            Lower y coordinate.
 *  \param [in] color         Line color.
 *  \param [in] line_width    Width of line.
 *  \param [in] length        Length of a dash.
 *  \param [in] space         Space between dashes.
 *  \param [in] origin_x      Page x coordinate to place line OBJECT.
 *  \param [in] origin_y      Page y coordinate to place line OBJECT.
 */
void o_line_print_dashed(TOPLEVEL *toplevel, FILE *fp,
			 int x1, int y1, int x2, int y2,
			 int color,
			 int line_width, int length, int space,
			 int origin_x, int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;
  
  f_print_set_color(toplevel, fp, color);
  
  /* the dashed line function takes an array of start-finish pairs
   * output the beginnings of the array now
   */
  fprintf(fp,"[");
  
  /*
   * Depending on the slope of the line the <B>length</B> (resp. <B>space</B>)
   * parameter is projected on each of the two directions x and y
   * resulting in <B>dx1</B> and <B>dy1</B> (resp. <B>dx2</B> and <B>dy2</B>).
   * Starting from one end and incrementing alternatively by <B>space</B>
   * and <B>length</B> the dashes are printed.
   *
   * It prints as many dashes of length <B>length</B> as possible.
   */
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);
  l = sqrt((dx * dx) + (dy * dy));
  
  dx1 = (dx * length) / l;
  dy1 = (dy * length) / l;
  
  dx2 = (dx * space) / l;
  dy2 = (dy * space) / l;
  
  d = 0;
  xa = x1; ya = y1;
  while((d + length + space) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
    
    fprintf(fp, "[%d %d %d %d] ", 
	    (int) xa, (int) ya,
	    (int) xb, (int) yb);
    
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
  }
  /*
   * When the above condition is no more satisfied, then it is not possible
   * to print a dash of length <B>length</B>. However it may be possible to
   * print the complete dash or a shorter one.
   */

  if((d + length) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
  } else {
    xb = x2;
    yb = y2;
  }
	
  fprintf(fp, "[%d %d %d %d] ", 
	  (int) xa, (int) ya,
	  (int) xb, (int) yb);

  fprintf(fp,"] %d dashed\n", line_width);
}


/*! \brief Print a centered line type line to Postscript document.
 *  \par Function Description
 *  This function prints a line when a centered line type is required.
 *  The line is defined by the coordinates of its two ends in
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed and the
 *  width of the line - that is the width of the dashes and the diameter
 *  of the dots.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x1            Upper x coordinate.
 *  \param [in] y1            Upper y coordinate.
 *  \param [in] x2            Lower x coordinate.
 *  \param [in] y2            Lower y coordinate.
 *  \param [in] color         Line color.
 *  \param [in] line_width    Width of line.
 *  \param [in] length        Length of a dash.
 *  \param [in] space         Space between dashes.
 *  \param [in] origin_x      Page x coordinate to place line OBJECT.
 *  \param [in] origin_y      Page y coordinate to place line OBJECT.
 */
void o_line_print_center(TOPLEVEL *toplevel, FILE *fp,
			 int x1, int y1, int x2, int y2,
			 int color,
			 int line_width, int length, int space,
			 int origin_x, int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;
  
  f_print_set_color(toplevel, fp, color);
  
  fprintf(fp, "[");

  /*
   * Depending on the slope of the line the <B>length</B> (resp. <B>space</B>)
   * parameter is projected on each of the two directions x and y resulting
   * in <B>dx1</B> and <B>dy1</B> (resp. <B>dx2</B> and <B>dy2</B>).
   * Starting from one end and incrementing alternatively by <B>space</B>
   * and <B>length</B> the dashes and dots are printed.
   *
   * It prints as many sets of dash and dot as possible.
   */
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);
  l = sqrt((dx * dx) + (dy * dy));
  
  dx1 = (dx * length) / l;
  dy1 = (dy * length) / l;
  
  dx2 = (dx * space) / l;
  dy2 = (dy * space) / l;
  
  d = 0;
  xa = x1; ya = y1;
  while((d + length + 2 * space) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
    
    fprintf(fp, "[%d %d %d %d] ", 
	    (int) xa, (int) ya,
	    (int) xb, (int) yb);
    
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
    
    fprintf(fp,"[%d %d] ",(int) xa, (int) ya);
    
    d = d + space;
    xa = xa + dx2;
    ya = ya + dy2;
  }
  /*
   * When the above condition is no more satisfied, then it is not possible
   * to print a dash of length <B>length</B>.
   * However two cases are possible :
   * <DL>
   *   <DT>*</DT><DD>it is possible to print the dash and the dot.
   *   <DT>*</DT><DD>it is possible to print the dash or a part
   *                 of the original dash.
   * </DL>
   */

  if((d + length + space) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
    
    fprintf(fp, "[%d %d %d %d] ", 
	    (int) xa, (int) ya,
	    (int) xb, (int) yb);
    
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
    
    fprintf(fp,"[%d %d] ",(int) xa, (int) ya);
    
  } else {
    if(d + length < l) {
      xb = xa + dx1;
      yb = ya + dy1;
    } else {
      xb = x2;
      yb = y2;
    }
    
    fprintf(fp, "[%d %d %d %d] ", 
	    (int) xa, (int) ya,
	    (int) xb, (int) yb);
    
  }
  
  fprintf(fp,"] %d dashed\n", line_width);

  /*
   * A dot is represented by a filled circle. Position of the circle is
   * (<B>xa</B>, <B>ya</B>) and its radius by the <B>line_width</B> parameter.
   */
}

/*! \brief Print a phantom line type line to Postscript document.
 *  \par Function Description
 *  This function prints a line when a phantom line type is required.
 *  The line is defined by the coordinates of its two ends in 
 *  (<B>x1</B>,<B>y1</B>) and (<B>x2</B>,<B>y2</B>).
 *  The Postscript document is defined by the file pointer <B>fp</B>.
 *
 *  A negative value for <B>space</B> or <B>length</B> leads to an
 *  endless loop.
 *
 *  All dimensions are in mils.
 *
 *  The function sets the color in which the line will be printed and the
 *  width of the line - that is the width of the dashes and the diameter
 *  of the dots.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] fp            FILE pointer to Postscript document.
 *  \param [in] x1            Upper x coordinate.
 *  \param [in] y1            Upper y coordinate.
 *  \param [in] x2            Lower x coordinate.
 *  \param [in] y2            Lower y coordinate.
 *  \param [in] color         Line color.
 *  \param [in] line_width    Width of line.
 *  \param [in] length        Length of a dash.
 *  \param [in] space         Space between dashes.
 *  \param [in] origin_x      Page x coordinate to place line OBJECT.
 *  \param [in] origin_y      Page y coordinate to place line OBJECT.
 */
void o_line_print_phantom(TOPLEVEL *toplevel, FILE *fp,
			  int x1, int y1, int x2, int y2,
			  int color,
			  int line_width, int length, int space,
			  int origin_x, int origin_y)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;
  
  f_print_set_color(toplevel, fp, color);
  
  fprintf(fp,"[");

  /*
   * Depending on the slope of the line the <B>length</B> (resp. <B>space</B>)
   * parameter is projected on each of the two directions x and y resulting
   * in <B>dx1</B> and <B>dy1</B> (resp. <B>dx2</B> and <B>dy2</B>).
   * Starting from one end and incrementing alternatively by <B>space</B>
   * and <B>length</B> the dashes and dots are printed.
   *
   * It prints as many sets of dash-dot-dot as possible.
   */
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);
  l = sqrt((dx * dx) + (dy * dy));
  
  dx1 = (dx * length) / l;
  dy1 = (dy * length) / l;
  
  dx2 = (dx * space) / l;
  dy2 = (dy * space) / l;
  
  d = 0;
  xa = x1; ya = y1;
  while((d + length + 3 * space) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
    
    fprintf(fp,"[%d %d %d %d] ",
	    (int) xa, (int)ya,
	    (int) xb, (int)yb);
    
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
    
    fprintf(fp,"[%d %d] ",(int) xa, (int) ya);
    
    d = d + space;
    xa = xa + dx2;
    ya = ya + dy2;
    
    fprintf(fp,"[%d %d] ",(int) xa, (int) ya);
    
    d = d + space;
    xa = xa + dx2;
    ya = ya + dy2;
  }
  /*
   * When the above condition is no more satisfied, then it is not possible
   * to print a complete set of dash-dot-dot.
   * However three cases are possible :
   * <DL>
   *   <DT>*</DT><DD>it is possible to print a dash and a dot and a dot.
   *   <DT>*</DT><DD>it is possible to print a dash and a dot.
   *   <DT>*</DT><DD>it is possible to print the dash or a part
   *                 of the original dash.
   * </DL>
   */

  if((d + length + 2 * space) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
    
    fprintf(fp,"[%d %d %d %d] ",
	    (int) xa, (int)ya,
	    (int) xb, (int)yb);
    
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
    
    fprintf(fp,"[%d %d] ",(int) xa, (int)ya);
    
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
    
    fprintf(fp,"[%d %d] ",(int) xa, (int)ya);
    
  } else {
    if(d + length + space < l) {
      d = d + length;
      xb = xa + dx1;
      yb = ya + dy1;
      
      fprintf(fp,"[%d %d %d %d] ",
	      (int) xa, (int)ya,
	      (int) xb, (int)yb);
      
      d = d + space;
      xa = xb + dx2;
      ya = yb + dy2;
      
      fprintf(fp,"[%d %d] ",(int) xa, (int)ya);
      
    } else {
      if(d + length < l) {
	xb = xa + dx1;
	yb = ya + dy1;
      } else {
	xb = x2;
	yb = y2;
      }
      
      fprintf(fp,"[%d %d %d %d] ",
	      (int) xa, (int)ya,
	      (int) xb, (int)yb);
      
    }
  }
  
  fprintf(fp,"] %d dashed\n", line_width);
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
  if (object == NULL) printf("lsw NO!\n");

  /* scale the line world coords */
  object->line->x[0] = object->line->x[0] * x_scale;
  object->line->y[0] = object->line->y[0] * y_scale;
  object->line->x[1] = object->line->x[1] * x_scale;
  object->line->y[1] = object->line->y[1] * y_scale;

  /* update boundingbox */
  o_line_recalc(toplevel, object);
  
}


/*! \brief calculate the lenght of a line object
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

  length = sqrt((dx*dx) + (dy*dy));
                
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
 *  \param [in] object       The line OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double o_line_shortest_distance (OBJECT *object, int x, int y, int force_solid)
{
  return m_line_shortest_distance (object->line, x, y);
}

