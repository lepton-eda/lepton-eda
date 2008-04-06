/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>
#include <math.h>
#include <stdio.h>

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define GET_BOX_WIDTH(w)			\
	abs((w)->second_wx - (w)->first_wx)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->second_wy - (w)->first_wy)
#define GET_BOX_LEFT(w)				\
	min((w)->first_wx, (w)->second_wx)
#define GET_BOX_TOP(w)				\
        max((w)->first_wy, (w)->second_wy)

typedef void (*DRAW_FUNC)( GdkDrawable *w, GdkGC *gc, GdkColor *color,
                           GdkCapStyle cap, gint filled,
                           gint x, gint y, gint width, gint height,
                           gint line_width, gint length, gint space );

typedef void (*FILL_FUNC)( GdkDrawable *w, GdkGC *gc, GdkColor *color,
                           gint x, gint y, gint width, gint height,
                           gint fill_width, gint angle1, gint pitch1,
                           gint angle2, gint pitch2 );

/*! \brief Draw a box on the screen.
 *  \par Function Description
 *  This function is used to draw a box on screen. The box is described in
 *  the OBJECT which is referred by <B>o_current</B>. The box is displayed
 *  according to the current state, described in the GSCHEM_TOPLEVEL object
 *  pointed by <B>w_current</B>.
 *
 *  It first checks if the OBJECT pointed is valid or not. If not it
 *  returns and do not output anything. That should never happen though.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  BOX OBJECT to draw.
 */
void o_box_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int wleft, wright, wtop, wbottom; /* world bounds */
  int s_upper_x, s_upper_y, s_lower_x, s_lower_y;
  int line_width, length, space;
  int fill_width, angle1, pitch1, angle2, pitch2;
  GdkCapStyle box_end;
  GdkColor *color;
  DRAW_FUNC draw_func = NULL;
  FILL_FUNC fill_func;

  if (o_current->box == NULL) {
    return;
  }

	/* Get read to check for visibility of this line by using it's
	 * bounding box */
  world_get_box_bounds(toplevel, o_current,
                       &wleft, &wtop, &wright, &wbottom);
	
  if ( (toplevel->DONT_REDRAW == 1) ||
       (!visible(toplevel, wleft, wtop, wright, wbottom)) ) {
    return;
  }
	
#if DEBUG
  printf("drawing box\n\n");
	
  printf("drawing box: %d %d %d %d\n",
         o_current->box->upper_x,
         o_current->box->upper_y,
         o_current->box->upper_x +
         abs(o_current->box->lower_x -
             o_current->box->upper_x),
         o_current->box->upper_y +
         abs(o_current->box->lower_y -
             o_current->box->upper_y));
#endif

  /*
   * The drawing of the box is divided in two steps : first step is to
   * draw the outline, the second is to draw the filling pattern inside
   * (if any). Finally the function takes care of the grips.
   */
  if (toplevel->override_color != -1 ) {  /* Override */
    color = x_get_color(toplevel->override_color);
  } else {
    color = x_get_color(o_current->color);
  }

  /*
   * The values describing the line type are extracted from the <B>o_current</B>
   * pointed structure. These are the width of the line, the field called
   * length and the field called space and the desired end type for the line.
   *
   * Depending on the type of the line that has to be used to draw the box
   * the appropriate function is called. Values of space and length are
   * adapted to the type of line. The possible functions are the following :
   * #o_box_draw_solid(), #o_box_draw_dotted(), #o_box_draw_dashed() and
   * #o_box_draw_phantom().
   *
   * The combination <B>length</B> == 0 and <B>space</B> == 0 is avoided as it
   * lead to an endless loop in function called after. If such a case is
   * encountered the box is drawn as a solid box independently of its
   * initial type.
   */
  line_width = SCREENabs( toplevel, o_current->line_width );
  if(line_width <= 0) {
    line_width = 1;
  }

  switch(o_current->line_end) {
    case END_NONE:   box_end = GDK_CAP_BUTT;       break;
    case END_SQUARE: box_end = GDK_CAP_PROJECTING; break;
    case END_ROUND:  box_end = GDK_CAP_ROUND;      break;
    default: fprintf(stderr, _("Unknown end for box (%d)\n"), o_current->line_end);
    box_end = GDK_CAP_BUTT;
    break;
  }
	
  length = SCREENabs( toplevel, o_current->line_length );
  space = SCREENabs( toplevel, o_current->line_space );
	
  switch(o_current->line_type) {
    case TYPE_SOLID:
      length = -1;
      space = -1;
      draw_func = o_box_draw_solid;
      break;

    case TYPE_DOTTED:
      length = -1; /* ..._draw_dotted only space is used */
      draw_func = o_box_draw_dotted;
      break;

    case TYPE_DASHED:
      draw_func = o_box_draw_dashed;
      break;

    case TYPE_CENTER:
      draw_func = o_box_draw_center;
      break;

    case TYPE_PHANTOM:
      draw_func = o_box_draw_phantom;
      break;

    case TYPE_ERASE:
      break;
			
    default:
      length = -1;
      space = -1;
      line_width = 0; /* just to be careful */
      draw_func = o_box_draw_solid;
      fprintf(stderr, _("Unknown type for box !\n"));
      break;
  }

  if((length == 0) || (space == 0))
  draw_func = o_box_draw_solid;

  WORLDtoSCREEN( toplevel, o_current->box->upper_x, o_current->box->upper_y,
                 &s_upper_x, &s_upper_y );
  WORLDtoSCREEN( toplevel, o_current->box->lower_x, o_current->box->lower_y,
                 &s_lower_x, &s_lower_y );
	
  (*draw_func)(w_current->backingstore, w_current->gc, color, box_end,
               FALSE,
               s_upper_x, s_upper_y,
               abs(s_lower_x - s_upper_x),
               abs(s_lower_y - s_upper_y),
               line_width, length, space);

  /*
   * The values needed for the fill operation are taken from the
   * <B>o_current</B> pointed OBJECT. It include the type of fill required,
   * the width of the lines (if the fill use line) and angles and pitchs
   * for hatch based filling.
   *
   * Once again the width of the line is important as if it is equal to
   * 0 it may not be displayed. That is definetely not what we are looking for.
   *
   * Depending on the type of fill that has to be used inside the box the
   * appropriate function is called. Values of <B>angle1</B>,
   * <B>angle2</B>, <B>pitch1</B> and <B>pitch2</B> are adapted to the type of
   * filling. The possible functions are the following :
   * #o_box_fill_hollow(), #o_box_fill_fill(), #o_box_fill_mesh() and
   * #o_box_fill_hatch().
   *
   * The combination <B>pitch1</B> <= 0 and <B>pitch2</B> <= 0 is avoided as
   * it lead to an endless loop in function called after. It happens when
   * the zoom factor is too small for two lines separated by the pitch
   * to be distinct. If such a case is encountered the circle is filled
   * hollow (e.q. not filled).
   */
  fill_width = SCREENabs( toplevel, o_current->fill_width );
  if(fill_width <= 0) {
    fill_width = 1;
  }
	
  angle1 = o_current->fill_angle1;
  pitch1 = SCREENabs( toplevel, o_current->fill_pitch1 );
  angle2 = o_current->fill_angle2;
  pitch2 = SCREENabs( toplevel, o_current->fill_pitch2 );
	
  switch(o_current->fill_type) {
    case FILLING_HOLLOW:
      angle1 = -1; angle2 = -1;
      pitch1 = 1; pitch2 = 1;
      /* this function is empty ! however if it do not use it we have to add
       * a test before the call. Simply putting a return here instead is not
       * possible as it would prevent any hollow box from having its grips
       * drawn
       */
      fill_func = o_box_fill_hollow;
      break;
		
    case FILLING_FILL:
      angle1 = -1; angle2 = -1;
      pitch1 = 1; pitch2 = 1;
      fill_func = o_box_fill_fill;
      break;
			
    case FILLING_MESH:
      fill_func = o_box_fill_mesh;
      break;

    case FILLING_HATCH:
      angle2 = -1;
      pitch2 = 1;
      fill_func = o_box_fill_hatch;
      break;
			
    case FILLING_VOID:
    default:
      angle1 = -1; angle2 = -1;
      pitch1 = 1; pitch2 = 1;
      fill_func = o_box_fill_hollow;
      fprintf(stderr, _("Unknown type for box (fill)!\n"));
  }

  if((pitch1 <= 0) || (pitch2 <= 0)) {
    fill_func = o_box_fill_fill;
  }

  (*fill_func)(w_current->backingstore, w_current->gc, color,
               s_upper_x, s_upper_y,
               abs(s_lower_x - s_upper_x),
               abs(s_lower_y - s_upper_y),
               fill_width, angle1, pitch1, angle2, pitch2);

  if ((o_current->draw_grips == TRUE) && (w_current->draw_grips == TRUE)) {
    /* pb20011003 - modified to use the new o_box_[draw|erase]_grips() */
    if (!o_current->selected) {
      /* object is no more selected, erase the grips */
      o_current->draw_grips = FALSE;
      o_box_erase_grips(w_current, o_current);
    } else {
      /* object is selected, draw the grips on the box */
      o_box_draw_grips(w_current, o_current);
    } 
  }
}

/*! \brief Draw a box with a solid line type.
 *  \par Function Description
 *  This function draws a box with a solid line type. The length and space
 *  parameters are not used by this function.
 *
 *  The function uses the functions previously defined in #o_line.c. It is
 *  called four times for each of the side of the box. Therefore note that
 *  the cap parameter is significant here even if it is a box (i.e. a closed
 *  shape).
 *
 *  The box is defined in the same way as it is in GDK : one point and
 *  the width and height of the box.
 *
 *  The unit of <B>x</B>, <B>y</B>, <B>width</B>, <B>height</B>,
 *  <B>line_width</B>, <B>length</B> and <B>space</B> is pixel.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] filled      (unused)
 *  \param [in] x           Box upper x.
 *  \param [in] y           Box upper y.
 *  \param [in] width       Box width.
 *  \param [in] height      Box height
 *  \param [in] line_width  Width of line to draw box.
 *  \param [in] length      (unused)
 *  \param [in] space       (unused)
 */
void o_box_draw_solid(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		      GdkCapStyle cap, gint filled, gint x, gint y, gint width,
		      gint height, gint line_width, gint length, gint space)
{
  o_line_draw_solid(w, gc, color, cap,
                    x, y, x + width, y, line_width, length, space);
  o_line_draw_solid(w, gc, color, cap,
                    x + width, y, x + width, y + height, line_width, 
                    length, space);
  o_line_draw_solid(w, gc, color, cap,
                    x + width, y + height, x, y + height, line_width, 
                    length, space);
  o_line_draw_solid(w, gc, color, cap,
                    x, y + height, x, y, line_width, length, space);
}

/*! \brief Draw a box with a dotted line type.
 *  \par Function Description
 *  This function draws a box with a dotted line type. The parameter
 *  <B>space</B> represents the distance between two of the dots. The
 *  parameter <B>length</B> is unused. The diameter of the dots is given by
 *  the width of the line given by <B>width</B>.
 *
 *  The function uses the functions previously defined in #o_line.c. It is
 *  called four times for each of the side of the box.
 *
 *  The box is defined in the same way as it is in GDK : one point and
 *  the width and height of the box.
 *
 *  The unit of <B>x</B>, <B>y</B>, <B>width</B>, <B>height</B>,
 *  <B>line_width</B>, <B>length</B> and <B>space</B> is pixel.
 *
 *  A negative or null value for <B>space</B> leads to an endless loop
 *  in #o_line_draw_dotted().
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] filled      (unused)
 *  \param [in] x           Box upper x.
 *  \param [in] y           Box upper y.
 *  \param [in] width       Box width.
 *  \param [in] height      Box height
 *  \param [in] line_width  Width of line to draw box.
 *  \param [in] length      (unused)
 *  \param [in] space       Space in pixels between dots.
 */

void o_box_draw_dotted(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		       GdkCapStyle cap, gint filled, gint x, gint y,
		       gint width, gint height, gint line_width,
		       gint length, gint space)
{
  o_line_draw_dotted(w, gc, color, cap,
                     x, y, x + width, y, line_width, length, space);
  o_line_draw_dotted(w, gc, color, cap,
                     x + width, y, x + width, y + height, 
                     line_width, length, space);
  o_line_draw_dotted(w, gc, color, cap,
                     x + width, y + height, x, y+height, 
                     line_width, length, space);
  o_line_draw_dotted(w, gc, color, cap,
                     x, y + height, x, y, line_width, length, space);
	
}

/*! \brief Draw a box with a dashed line type.
 *  \par Function Description
 *  This function draws a box with a dashed line type. The parameter
 *  <B>space</B> represents the distance between two of the dash. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  The function uses the functions previously defined in #o_line.c. It is
 *  called four times for each of the side of the box.
 *
 *  The box is defined in the same way as it is in GDK : one point and
 *  the width and height of the box.
 *
 *  The unit of <B>x</B>, <B>y</B>, <B>width</B>, <B>height</B>,
 *  <B>line_width</B>, <B>length</B> and <B>space</B> is pixel.
 *
 *  A negative or null value for length or space leads to an endless
 *  loop in #o_line_draw_dashed().
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] filled      (unused)
 *  \param [in] x           Box upper x.
 *  \param [in] y           Box upper y.
 *  \param [in] width       Box width.
 *  \param [in] height      Box height
 *  \param [in] line_width  Width of line to draw box.
 *  \param [in] length      Length of dash in pixels.
 *  \param [in] space       Space between dashes in pixels.
 */
void o_box_draw_dashed(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		       GdkCapStyle cap, gint filled, gint x, gint y,
		       gint width, gint height, gint line_width,
		       gint length, gint space)
{
  o_line_draw_dashed(w, gc, color, cap,
                     x, y, x + width, y, line_width, length, space);
  o_line_draw_dashed(w, gc, color, cap,
                     x + width, y, x + width, y + height, 
                     line_width, length, space);
  o_line_draw_dashed(w, gc, color, cap,
                     x + width, y + height, x, y+height, 
                     line_width, length, space);
  o_line_draw_dashed(w, gc, color, cap,
                     x, y + height, x, y, line_width, length, space);
}

/*! \brief Draw a box with a centered line type.
 *  \par Function Description
 *  This function draws a box with a centered line type. The parameter
 *  <B>space</B> represents the distance between a dot and the dash. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  The function uses the functions previously defined in #o_line.c. It is
 *  called four times for each of the side of the box.
 *
 *  The box is defined in the same way as it is in GDK : one point and the
 *  width and height of the box.
 *
 *  The unit of <B>x</B>, <B>y</B>, <B>width</B>, <B>height</B>,
 *  <B>line_width</B>, <B>length</B> and <B>space</B> is pixel.
 *
 *  A negative or null value for length or space leads to an endless
 *  loop in #o_line_draw_center().
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] filled      (unused)
 *  \param [in] x           Box upper x.
 *  \param [in] y           Box upper y.
 *  \param [in] width       Box width.
 *  \param [in] height      Box height
 *  \param [in] line_width  Width of line to draw box.
 *  \param [in] length      (unused)?
 *  \param [in] space       (unused)?
 */
void o_box_draw_center(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		       GdkCapStyle cap, gint filled, gint x, gint y,
		       gint width, gint height, gint line_width,
		       gint length, gint space)
{
  o_line_draw_center(w, gc, color, cap,
                     x, y, x + width, y, line_width, length, space);
  o_line_draw_center(w, gc, color, cap,
                     x + width, y, x + width, y + height, 
                     line_width, length, space);
  o_line_draw_center(w, gc, color, cap,
                     x + width, y + height, x, y+height, 
                     line_width, length, space);
  o_line_draw_center(w, gc, color, cap,
                     x, y + height, x, y, line_width, length, space);
}

/*! \brief Draw a box with a phantom line type.
 *  \par Function Description
 *  This function draws a box with a phantom line type. The parameter
 *  <B>space</B> represents the distance between a dot and a dash.
 *  The parameter <B>length</B> represents the length of a dash.
 *
 *  The function uses the functions previously defined in #o_line.c.
 *  It is called four times for each of the side of the box.
 *
 *  The box is defined in the same way as it is in GDK : one point and the
 *  width and height of the box.
 *
 *  The unit of <B>x</B>, <B>y</B>, <B>width</B>, <B>height</B>,
 *  <B>line_width</B>, <B>length</B> and <B>space</B> is pixel.
 *
 *  A negative or null value for length or space leads to an endless loop
 *  in #o_line_draw_phantom().
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] filled      (unused)
 *  \param [in] x           Box upper x.
 *  \param [in] y           Box upper y.
 *  \param [in] width       Box width.
 *  \param [in] height      Box height
 *  \param [in] line_width  Width of line to draw box.
 *  \param [in] length      (unused)?
 *  \param [in] space       (unused)?
 */
void o_box_draw_phantom(GdkDrawable *w, GdkGC *gc, GdkColor *color,
			GdkCapStyle cap, gint filled, gint x, gint y,
			gint width, gint height, gint line_width,
			gint length, gint space)
{
  o_line_draw_phantom(w, gc, color, cap,
                      x, y, x + width, y, line_width, length, space);
  o_line_draw_phantom(w, gc, color, cap,
                      x + width, y, x + width, y+height, 
                      line_width, length, space);
  o_line_draw_phantom(w, gc, color, cap,
                      x + width, y + height, x, y+height, 
                      line_width, length, space);
  o_line_draw_phantom(w, gc, color, cap,
                      x, y + height, x, y, line_width, length, space);
}

/*! \brief Placeholder filling function.
 *  \par Function Description
 *  This function does nothing. It has the same prototype as all the
 *  filling functions. It prevent from making a difference between filling
 *  in function #o_box_draw().
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color. 
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
void o_box_fill_hollow(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		       gint x, gint y,
		       gint width, gint height,
		       gint fill_width,
		       gint angle1, gint pitch1,
		       gint angle2, gint pitch2)
{
  
}

/*! \brief Fill inside of box with a solid pattern.
 *  \par Function Description
 *  This function fills the inside of the box with a solid pattern.
 *  Parameters <B>angle1</B>, <B>pitch1</B> and <B>angle2</B>,
 *  <B>pitch2</B> and <B>fill_width</B> are unused here but kept for compatibility
 *  with other box filling functions.
 *
 *  The box is defined in the same way as it is in GDK : one point and
 *  the width and height of the box.
 *
 *  All parameters are given in pixel.
 *
 *  The solid fill is done with the #gdk_draw_rectangle() function and
 *  its parameters <B>filled</B> set. The box is filled with the color
 *  <B>color</B> given as a parameter to the function.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color. 
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      (unused)
 *  \param [in] pitch1      (unused)
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
void o_box_fill_fill(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		     gint x, gint y,
		     gint width, gint height,
		     gint fill_width,
		     gint angle1, gint pitch1, gint angle2, gint pitch2)
{
  gdk_gc_set_foreground(gc, color);
  gdk_gc_set_line_attributes(gc, 1, GDK_LINE_SOLID,
                             GDK_CAP_BUTT, GDK_JOIN_MITER);

  gdk_draw_rectangle(w, gc, TRUE, x, y, width, height);
	
}

/*! \brief Fill inside of box with single line pattern.
 *  \par Function Description
 *  This function fills the inside of the box with a pattern made of lines.
 *  The lines are drawn inside the box with an angle <B>angle1</B> from the
 *  horizontal. The distance between two of these lines is given by
 *  <B>pitch1</B> and their width by <B>fill_width</B>.
 *  Parameters <B>angle2</B> and <B>pitch2</B> are unused here but kept for
 *  compatbility with other box filling functions.
 *
 *  The box is defined in the same way as it is in GDK : one point and the
 *  width and height of the box.
 *
 *  All parameters are given in pixel.
 *
 *  Negative or null values for <B>pitch1</B> are not allowed as it leads to
 *  an endless loop.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color. 
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
void o_box_fill_hatch(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		      gint x, gint y,
		      gint width, gint height,
		      gint fill_width,
		      gint angle1, gint pitch1, gint angle2, gint pitch2)
{
  int x3, y3, x4, y4;
  double cos_a_, sin_a_;
  double x0, y0, r;
  double x1, y1, x2, y2;
  double amin, amax, a[4], min1, min2, max1, max2;

  gdk_gc_set_line_attributes(gc, fill_width, GDK_LINE_SOLID,
                             GDK_CAP_BUTT, GDK_JOIN_MITER);

  /*
   * The function uses a matrix. Its elements are obtained from the sinus
   * and the cosinus of the angle <B>angle1</B>. It represents the rotation
   * matrix that when applied to a point, rotate it of <B>angle1</B>.
   */
  cos_a_ = cos(((double) angle1) * M_PI/180);
  sin_a_ = sin(((double) angle1) * M_PI/180);

  /*
   * The function considers the smallest circle around the box. Its radius
   * is given by the following relation. Its center is given by the point
   * a the middle of the box horizontally and vertically (intersection of
   * its two diagonals.
   */
  r = sqrt((double) (pow(width, 2) + pow(height, 2))) / 2;

  /*
   * When drawing a line in a circle there is two intersections. With the
   * previously described circle, these intersections are out of the box.
   * They can be easily calculated, the first by resolution of an equation
   * and the second one by symetry in relation to the vertical axis going
   * through the center of the circle.
   *
   * These two points are then rotated of angle <B>angle1</B> using the matrix
   * previously mentionned.
   */
  y0 = 0;
  while(y0 < r) {
    x0 = pow(r, 2) - pow(y0, 2);
    x0 = sqrt(x0);
    
    x1 = (x0*cos_a_ - y0*sin_a_);
    y1 = (x0*sin_a_ + y0*cos_a_);
    x2 = ((-x0)*cos_a_ - y0*sin_a_);
    y2 = ((-x0)*sin_a_ + y0*cos_a_);
    
    /*
     * It now parametrizes the segment : first intersection is given
     * the value of 0 and the second is given the value of 1. The four
     * values for each intersection of the segment and the four
     * sides (vertical or horizontal) of the box are given by the
     * following relations :
     */
    if((int) (x2 - x1) != 0) {
      a[0] = ((-width/2) - x1) / (x2 - x1);
      a[1] = ((width/2)  - x1) / (x2 - x1);
    } else {
      a[0] = 0; a[1] = 1;
    }
    
    if((int) (y2 - y1) != 0) {
      a[2] = ((-height/2) - y1) / (y2 - y1);
      a[3] = ((height/2)  - y1) / (y2 - y1);
    } else {
      a[2] = 0; a[3] = 1;
    }
    /*
     * It now has to check which of these four values are for
     * intersections with the sides of the box (some values may be
     * for intersections out of the box). This is made by a min/max
     * function.
     */
    if(a[0] < a[1]) {
      min1 = a[0]; max1 = a[1];
    } else {
      min1 = a[1]; max1 = a[0];
    }
    
    if(a[2] < a[3]) {
      min2 = a[2]; max2 = a[3];
    } else {
      min2 = a[3]; max2 = a[2];
    }
    
    amin = (min1 < min2) ? min2 : min1;
    amin = (amin < 0) ? 0 : amin;
    
    amax = (max1 < max2) ? max1 : max2;
    amax = (amax < 1) ? amax : 1;
    
    /*
     * If the segment really go through the box it draws the line.
     * It also take the opportunity of the symetry in the box in
     * relation to its center to draw the second line at the same time.
     *
     * If there is no intersection of the segment with any of the sides,
     * then there is no need to continue : there would be no more
     * segment in the box to draw.
     */
    
    if((amax > amin) && (amax != 1) && (amin != 0)) {
      /* There is intersection between the line and the box edges */
      x3 = (int) (x1 + amin*(x2 - x1));
      y3 = (int) (y1 + amin*(y2 - y1));
      
      x4 = (int) (x1 + amax*(x2 - x1));
      y4 = (int) (y1 + amax*(y2 - y1));
      
      gdk_draw_line(w, gc, x3 + (x + width/2),
		    (y + height/2) - y3, x4 + (x + width/2),
		    (y + height/2) - y4);
      
      gdk_draw_line(w, gc, -x3 + (x + width/2),
		    +y3 + (y + height/2), -x4 + (x + width/2),
		    +y4 + (y + height/2));
      
    } else {
      break;
    }
    
    y0 = y0 + pitch1;
    
  }
}
  
/*! \brief Fill inside of box with mesh pattern.
 *  \par Function Description
 *  This function fills the inside of the box with a pattern made of two
 *  sets of parallel lines in two directions. The first set is drawn inside
 *  the box with an angle <B>angle1</B> from the horizontal. The distance
 *  between two of these lines is given by <B>pitch1</B>.
 *  The second set is drawn inside the box with an angle <B>angle2</B> from
 *  the horizontal. The distance between two of these lines is given
 *  by <B>pitch2</B>.
 *  Every lines have the same width given be <B>fill_width</B>.
 *
 *  This function simply makes two successive calls to the function
 *  #o_box_fill_hatch() respectively with <B>angle1</B>, <B>pitch1</B> and
 *  <B>angle2</B>, <B>pitch2</B> for parameters.
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color. 
 *  \param [in] x           Upper x coordinate of BOX.
 *  \param [in] y           Upper y coordinate of BOX.
 *  \param [in] width       Width of BOX.
 *  \param [in] height      Height of BOX.
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
void o_box_fill_mesh(GdkDrawable *w, GdkGC *gc, GdkColor *color,
		     gint x, gint y,
		     gint width, gint height,
		     gint fill_width,
		     gint angle1, gint pitch1,
		     gint angle2, gint pitch2)
{
  o_box_fill_hatch(w, gc, color, x, y, width, height,
		   fill_width, angle1, pitch1, -1, -1);
  o_box_fill_hatch(w, gc, color, x, y, width, height,
	  	   fill_width, angle2, pitch2, -1, -1);
}


/*! \todo Finish function documentation!!!
 *  \brief 
 *  \par Function Description
 * 
 *  \note
 *  used in button cancel code in x_events.c
 */
void o_box_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
  o_box_rubberbox_xor(w_current);
}

/*! \brief Draw a box described by OBJECT with translation
 *  \par Function Description
 *  This function daws the box object described by <B>*o_current</B> translated
 *  by the vector (<B>dx</B>,<B>dy</B>) with an xor-function over the current sheet.
 *  The translation vector is in screen unit.
 *
 *  The box is displayed with the color of the object.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for box.
 *  \param [in] dy         Delta y coordinate for box.
 *  \param [in] o_current  Box OBJECT to draw.
 */
void o_box_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int screen_x1, screen_y1;
  int screen_x2, screen_y2;
  int color;

  if (o_current->box == NULL) {
    return;
  }

  WORLDtoSCREEN( toplevel, o_current->box->upper_x, o_current->box->upper_y,
                 &screen_x1, &screen_y1 );
  WORLDtoSCREEN( toplevel, o_current->box->lower_x, o_current->box->lower_y,
                 &screen_x2, &screen_y2 );
	
  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  gdk_gc_set_foreground(w_current->outline_xor_gc,
                        x_get_darkcolor(color));
  gdk_draw_rectangle(w_current->backingstore,
                     w_current->outline_xor_gc, FALSE,
                     screen_x1 + dx,
                     screen_y1 + dy,
                     abs(screen_x2 - screen_x1),
                     abs(screen_y2 - screen_y1));
}

/*! \brief Start process to input a new box.
 *  \par Function Description
 *  This function starts the process to input a new box. Parameters for this
 *  box are put into/extracted from the <B>w_current</B> toplevel structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in world
 *  coordinates.
 *
 *  The first step is to input one corner of the box. This corner is
 *  (<B>w_x</B>,<B>w_y</B>) snapped to the grid and saved in <B>w_current->first_wx</B>
 *  and <B>w_current->first_wy</B>.
 *
 *  The other corner will be saved in (<B>w_current->second_wx</B>,
 *  <B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world.
 *  \param [in] w_y        Current y coordinate of pointer in world.
 */
void o_box_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe box */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;

  /* start to draw the box */
  o_box_rubberbox_xor(w_current);
}

/*! \brief End the input of a box.
 *  \par Function Description
 *  This function ends the input of the second corner of a box.
 *  The (<B>w_x</B>,<B>w_y</B>) point is set to be this second corner. The box is
 *  then defined by (<B>w_current->first_wx</B>,<B>w_current->first_wy</B> and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *  <B>w_x</B> and <B>w_y</B> are in screen unit.
 *
 *  The temporary box is erased ; a new box object is allocated, initialized
 *  and linked to the object list ; The object is finally drawn on the
 *  current sheet.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_box_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int box_width, box_height;
  int box_left, box_top;

  g_assert( w_current->inside_action != 0 );

  /* get the last coords of the pointer */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* erase the temporary box */
  o_box_rubberbox_xor(w_current);
  
  box_width  = GET_BOX_WIDTH (w_current);
  box_height = GET_BOX_HEIGHT(w_current);
  box_left   = GET_BOX_LEFT  (w_current);
  box_top    = GET_BOX_TOP   (w_current);

  /* boxes with null width or height are not allowed */
  if ((box_width == 0) || (box_height == 0)) {
	  /* cancel the object creation */
	  w_current->first_wx = (-1);
	  w_current->first_wy = (-1);
	  w_current->second_wx  = (-1);
	  w_current->second_wy  = (-1);
	  return;
  }

  /* create the object */
  toplevel->page_current->object_tail =
  o_box_add(toplevel,
            toplevel->page_current->object_tail,
            OBJ_BOX, w_current->graphic_color, 
	    box_left, box_top, box_left + box_width, box_top - box_height);

  /* draw it */
  o_redraw_single(w_current, toplevel->page_current->object_tail);
  
#if DEBUG
  printf("coords: %d %d %d %d\n", box_left, box_top, box_width, box_height);
#endif
	
  w_current->first_wx = (-1);
  w_current->first_wy = (-1);
  w_current->second_wx  = (-1);
  w_current->second_wy  = (-1);
	
  toplevel->page_current->CHANGED = 1;

  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw temporary box while dragging edge.
 *  \par Function Description
 *  This function is used to draw the box while dragging one of its edge or
 *  angle. It erases the previous temporary box drawn before, and draws a new
 *  updated one. <B>w_x</B> and <B>w_y</B> are the new position of the mobile point,
 *  ie the mouse.
 *
 *  The old values are inside the <B>w_current</B> pointed structure. Old width,
 *  height and left and top values are recomputed by the corresponding macros.
 *  The box is then erased by performing a xor-drawing over the box.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_box_rubberbox(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  g_assert( w_current->inside_action != 0 );

  /* erase the previous temporary box if it is visible */
  if (w_current->rubber_visible)
    o_box_rubberbox_xor(w_current);

  /*
   * New values are fixed according to the <B>w_x</B> and <B>w_y</B> parameters.
   * These are saved in <B>w_current</B> pointed structure as new temporary
   * values. The new box is then drawn.
   */

  /* update the coords of the corner */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* draw the new temporary box */
  o_box_rubberbox_xor(w_current);

  w_current->rubber_visible = 1;
  
}

/*! \brief Draw box from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws the box from the variables in the GSCHEM_TOPLEVEL
 *  structure <B>*w_current</B>.
 *  One corner of the box is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and the second corner is at
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>.
 *
 *  The box is drawn with a xor-function over the current sheet with the
 *  selection color.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_box_rubberbox_xor(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int box_width, box_height, box_left, box_top;
  
  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy,
		&x1, &y1);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy, 
		&x2, &y2);

  /* get the width/height and the upper left corner of the box */
  box_width  = abs(x2 - x1);
  box_height = abs(y2 - y1);
  box_left   = min(x1, x2);
  box_top    = min(y1, y2);
  
	/* draw the box from the previous variables */
  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color));
  gdk_gc_set_line_attributes(w_current->xor_gc, 0, 
			     GDK_LINE_SOLID, GDK_CAP_NOT_LAST, 
			     GDK_JOIN_MITER);
  gdk_draw_rectangle(w_current->backingstore, w_current->xor_gc,
		     FALSE, box_left, box_top, box_width, box_height);
  o_invalidate_rect(w_current, box_left, box_top,
		    box_left + box_width, box_top + box_height);
}

/*! \brief Draw grip marks on box.
 *  \par Function Description
 *  This function draws four grips on the corners of the box described
 *  by <B>*o_current</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Box OBJECT to draw grip points on.
 *
 *  \par Author's note
 *  p20011003 - modified the prototype : removed parameter 'GdkWindow *w'
 */
void o_box_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int s_upper_x, s_upper_y, s_lower_x, s_lower_y;

  if (w_current->draw_grips == FALSE)
	  return;
  
  WORLDtoSCREEN( toplevel, o_current->box->upper_x, o_current->box->upper_y,
                 &s_upper_x, &s_upper_y );
  WORLDtoSCREEN( toplevel, o_current->box->lower_x, o_current->box->lower_y,
                 &s_lower_x, &s_lower_y );

  /* grip on upper left corner (whichone = BOX_UPPER_LEFT) */
  o_grips_draw(w_current, s_upper_x, s_upper_y);

  /* grip on upper right corner (whichone = BOX_UPPER_RIGHT) */
  o_grips_draw(w_current, s_lower_x, s_upper_y);
  
  /* grip on lower left corner (whichone = BOX_LOWER_LEFT) */
  o_grips_draw(w_current, s_upper_x, s_lower_y);

  /* grip on lower right corner (whichone = BOX_LOWER_RIGHT) */
  o_grips_draw(w_current, s_lower_x, s_lower_y);

}

/*! \brief Erase grip marks from box.
 *  \par Function Description
 *  This function erases the four grips displayed on the <B>*o_current</B>
 *  box object. These grips are on each of the corner.
 * 
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Box OBJECT to erase grip marks from.
 */
void o_box_erase_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int s_upper_x, s_upper_y, s_lower_x, s_lower_y;

  if (w_current->draw_grips == FALSE)
	  return;
  
  WORLDtoSCREEN( toplevel, o_current->box->upper_x, o_current->box->upper_y,
                 &s_upper_x, &s_upper_y );
  WORLDtoSCREEN( toplevel, o_current->box->lower_x, o_current->box->lower_y,
                 &s_lower_x, &s_lower_y );

  /* grip on upper left corner (whichone = BOX_UPPER_LEFT) */
  o_grips_erase(w_current, s_upper_x, s_upper_y);

  /* grip on upper right corner (whichone = BOX_UPPER_RIGHT) */
  o_grips_erase(w_current, s_lower_x, s_upper_y);
  
  /* grip on lower left corner (whichone = BOX_LOWER_LEFT) */
  o_grips_erase(w_current, s_upper_x, s_lower_y);

  /* grip on lower right corner (whichone = BOX_LOWER_RIGHT) */
  o_grips_erase(w_current, s_lower_x, s_lower_y);

}

