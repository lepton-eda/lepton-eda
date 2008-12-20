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

#include <stdio.h>
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

typedef void (*DRAW_FUNC)( GdkDrawable *w, GdkGC *gc, GdkColor *color,
                           GdkCapStyle cap,
                           gint x1, gint y1, gint x2, gint y2,
                           gint line_width, gint length, gint space );

/*! \brief Draw a line on screen.
 *  \par Function Description
 *  This function is used to draw a line on screen. The line is described
 *  in the object which is referred by <B>o_current</B>. The line is displayed
 *  according to the current state, described in the GSCHEM_TOPLEVEL object pointed
 *  by <B>w_current</B>.
 *
 *  It first checks if the object is valid or not. If not it returns and do
 *  not output anything. That should never happen though.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  The line OBJECT to draw.
 */
void o_line_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;
  int line_width, length, space;
  GdkColor *color;
  GdkCapStyle line_end;
  DRAW_FUNC draw_func = NULL;
	
  if (o_current->line == NULL) {
    return;
  }

  if ( (toplevel->DONT_REDRAW == 1) ||
       (!o_line_visible(toplevel, o_current->line, &x1, &y1, &x2, &y2)) ) {
    return;
  }
	
#if DEBUG
  printf("drawing line\n\n");
  printf("drawing line : %d,%d to %d,%d\n",
         x1, y1,
         x2, y2);
#endif

  /*
   * As a line is definetely not a closed shape there is no need to define and
   * call any filling function. Another way to say that is that a line can
   * not be filled. It simply draws the line according to the type.
   *
   * The values describing the line type are extracted from the
   * <B>o_current</B> pointed structure. These are the width of the line, the
   * field called length and the field called space and the desired end type
   * for the line.
   *
   * Depending on the type of the line that has to be used to draw the box
   * the appropriate function is called. Values of space and length are
   * adapted to the type of line. The possible functions are the following :
   * #o_line_draw_solid(), #o_line_draw_dotted(), #o_line_draw_dashed() and
   * #o_line_draw_phantom().
   *
   * The combination <B>length</B> == 0 and <B>space</B> == 0 is avoided as it
   * leads to an endless loop in function called after. If such a case is
   * encountered the line is drawn as a solid line independently of its
   * initial type.
   *
   * Finally the function takes care of the grips.
   */
  if (toplevel->override_color != -1 )
  color = x_get_color(toplevel->override_color);
  else
  color = x_get_color(o_current->color);
	
  line_width = SCREENabs( toplevel, o_current->line_width );
  if( line_width <= 0) {
    line_width = 1;
  }
	
  switch(o_current->line_end) {
    case END_NONE:   line_end = GDK_CAP_BUTT;       break;
    case END_SQUARE: line_end = GDK_CAP_PROJECTING; break;
    case END_ROUND:  line_end = GDK_CAP_ROUND;      break;
    default: fprintf(stderr, _("Unknown end for line (%d)\n"),
                     o_current->line_end);
    line_end = GDK_CAP_BUTT; 
    break;
  }

  length = SCREENabs( toplevel, o_current->line_length );
  space = SCREENabs( toplevel, o_current->line_space );
	
  switch(o_current->line_type) {
    case TYPE_SOLID:
      length = -1;
      space = -1;
      draw_func = o_line_draw_solid;
      break;
			
    case TYPE_DOTTED:
      length = -1; /* in ..._draw_dotted, length is unused */
      draw_func = o_line_draw_dotted;
      break;
			
    case TYPE_DASHED:
      draw_func = o_line_draw_dashed;
      break;
			
    case TYPE_CENTER:
      draw_func = o_line_draw_center;
      break;
			
    case TYPE_PHANTOM:
      draw_func = o_line_draw_phantom;
      break;
			
    case TYPE_ERASE:
      break;
			
    default:
      length = -1;
      space = -1;
      line_width = 0; /* just to be careful */
      fprintf(stderr, _("Unknown type for line (%d) !\n"),
              o_current->line_type);
      draw_func = o_line_draw_solid;
      break;
  }

  if((length == 0) || (space == 0))
  draw_func = o_line_draw_solid;

  (*draw_func) (w_current->drawable, w_current->gc, color, line_end,
                x1, y1, x2, y2, line_width, length, space);

  /* reset line width and reset back to default */
  gdk_gc_set_line_attributes(w_current->gc, 0, GDK_LINE_SOLID,
                             GDK_CAP_NOT_LAST,
                             GDK_JOIN_MITER);

  if (o_current->selected && w_current->draw_grips) {
    o_line_draw_grips (w_current, o_current);
  }

#if DEBUG
  printf("drawing line\n");
#endif
}

/*! \brief Draw a line with a solid line type.
 *  \par Function Description
 *  This function draws a line with a solid line type. The line is defined
 *  by the coordinates of its two extremities. The parameters <B>length</B>
 *  and <B>space</B> are unused here.
 *
 *  The line attributes are settled. Then it simply make a call to the
 *  gdk original function.
 *
 *  \param [in] w           GdkWindow to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] cap         GdkCapStype line end cap style.
 *  \param [in] x1          Upper x coordinate of Line.
 *  \param [in] y1          Upper y coordinate of Line.
 *  \param [in] x2          Lower x coordinate of Line.
 *  \param [in] y2          Lower y coordinate of Line.
 *  \param [in] line_width  Line width.
 *  \param [in] length      (unused).
 *  \param [in] space       (unused).
 */
void o_line_draw_solid(GdkWindow *w, GdkGC *gc, GdkColor *color,
		       GdkCapStyle cap, gint x1, gint y1, gint x2, gint y2,
		       gint line_width, gint length, gint space)
{
  gdk_gc_set_foreground(gc, color);

  /* Set the width, end type and join style of the line */
  gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
                             cap, GDK_JOIN_MITER);

  /* Draw the line */
  gdk_draw_line(w, gc, x1, y1, x2, y2);

}

/*! \brief Draw a line with a dotted line type.
 *  \par Function Description
 *  This function draw a line with a dotted line type. The parameter
 *  <B>space</B> represents the distance between two of the dots. The parameter
 *  <B>length</B> is unused. The diameter of the dots is given by the width
 *  of the line given by <B>line_width</B>.
 *
 *  The unit of <B>x1</B>, <B>y1</B> and <B>x2</B>, <B>y2</B> and
 *  <B>line_width</B>, <B>length</B>, <B>space</B> is pixel.
 *
 *  A negative of null value for length or space leads to an endless loop.
 *
 *  \param [in] w           GdkWindow to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] cap         GdkCapStype line end cap style.
 *  \param [in] x1          Upper x coordinate of Line.
 *  \param [in] y1          Upper y coordinate of Line.
 *  \param [in] x2          Lower x coordinate of Line.
 *  \param [in] y2          Lower y coordinate of Line.
 *  \param [in] line_width  Line width.
 *  \param [in] length      (unused).
 *  \param [in] space       Space between dots in pixels.
 */
void o_line_draw_dotted(GdkWindow *w, GdkGC *gc, GdkColor *color,
			GdkCapStyle cap, gint x1, gint y1, gint x2, gint y2,
			gint line_width, gint length, gint space)
{
  double dx, dy, l, d;
  double dx1, dy1;
  double xa, ya;

  gdk_gc_set_foreground(gc, color);

  /*
   * It first finds the increments on x and y axis that match the space
   * on the line between two dots.
   *
   * Starting from one of the end of the line and incrementing the position
   * gives the coordinates of every dots on the line providing that the
   * second extremities is not exceeded.
   */
  dx = (double) (x2 - x1);
  dy = (double) (y2 - y1);
  l = sqrt((dx * dx) + (dy * dy));

  dx1 = (dx * space) / l;
  dy1 = (dy * space) / l;

  d = 0;
  xa = x1; ya = y1;
  while(d < l) {

    /*
     * Depending on the width of the line, dots has to be drawn in a
     * different manner : if the real world width is equal to 0, then the
     * width is translated to 1 in screen coordinates. Drawing a circle with
     * a 1-diameter and the GDK function #gdk_draw_arc() is not possible.
     * So we needs to test whether the width is 1 or not.
     */
    if(line_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - line_width/2, 
		   ((int) ya) - line_width/2,
		   line_width, line_width, 0, FULL_CIRCLE);
    }
    
    d = d + space;
    xa = xa + dx1;
    ya = ya + dy1;
  }
	
}

/*! \brief Draw a line with a dotted line type.
 *  \par Function Description
 *  This function draws a line with a dashed line type. The parameter
 *  <B>space</B> respresents the distance between two of the dashes. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  The unit of <B>x1</B>, <B>y1</B> and <B>x2</B>, <B>y2</B> and
 *  <B>line_width</B>, <B>length</B>, <B>space</B> is pixel.
 *
 *  A negative of null value for length or space leads to an endless loop.
 *
 *  \param [in] w           GdkWindow to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] cap         GdkCapStype line end cap style.
 *  \param [in] x1          Upper x coordinate of Line.
 *  \param [in] y1          Upper y coordinate of Line.
 *  \param [in] x2          Lower x coordinate of Line.
 *  \param [in] y2          Lower y coordinate of Line.
 *  \param [in] line_width  Line width.
 *  \param [in] length      Length of dashes in pixels.
 *  \param [in] space       Space between dashes in pixels.
 */
void o_line_draw_dashed(GdkWindow *w, GdkGC *gc, GdkColor *color,
			GdkCapStyle cap, gint x1, gint y1, gint x2, gint y2,
			gint line_width, gint length, gint space)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;

  gdk_gc_set_foreground(gc, color);
  gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
                             cap, GDK_JOIN_MITER);

  /*
   * The function determines the increments on x and y axis that match
   * the space on the line between two dots. The same thing is done for length.
   *
   * Starting from one of the end of the line and incrementing the position
   * gives the coordinates of every dots on the line providing that the
   * second extremities is not exceeded. This is checked by first computing
   * the distance between the two extremities and then checking whether this
   * limit is exceeded by a new dash or not.
   *
   * It draws as many dashes of length <B>length</B> as possible.
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
    gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

  }

  /*
   * When the above condition is not satisfied, then it is not possible
   * to draw a dash of length <B>length</B>. However it may be possible to
   * draw a shorter dash.
   */
  if((d + length) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
  } else {
    xb = x2;
    yb = y2;
  }

  gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);

}

/*! \brief Draw line with a centered line type.
 *  \par Function Description.
 *  This function draws a line with a centered line type. The parameter
 *  <B>space</B> represents the distance between a dot and the dash. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  The unit of <B>x1</B>, <B>y1</B> and <B>x2</B>, <B>y2</B> and
 *  <B>line_width</B>, <B>length</B>, <B>space</B> is pixel.
 *
 *  A negative of null value for length or space leads to an endless loop.
 *
 *  \param [in] w           GdkWindow to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] cap         GdkCapStype line end cap style.
 *  \param [in] x1          Upper x coordinate of Line.
 *  \param [in] y1          Upper y coordinate of Line.
 *  \param [in] x2          Lower x coordinate of Line.
 *  \param [in] y2          Lower y coordinate of Line.
 *  \param [in] line_width  Line width.
 *  \param [in] length      Length of dashes in pixels.
 *  \param [in] space       Space between dashes in pixels.
 */
void o_line_draw_center(GdkWindow *w, GdkGC *gc, GdkColor *color,
			GdkCapStyle cap, gint x1, gint y1, gint x2, gint y2,
			gint line_width, gint length, gint space)
{
  double dx, dy, l, d;
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;

  gdk_gc_set_foreground(gc, color);
  gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
                             cap, GDK_JOIN_MITER);

  /*
   * The function determines the increments on x and y axis that match
   * the space on the line between two dots. The same thing is done for length.
   *
   * Starting from one of the end of the line and incrementing the position
   * gives the coordinates of every dots and dashes on the line providing
   * that the second extremity is not exceeded. This is checked by first
   * computing the distance between the two extremities and then checking
   * whether this limit is exceeded by a new dash or not.
   *
   * It draws as many sets of 'dash of length <B>length</B> and dot' as possible.
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
    gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    /*
     * Depending on the width of the line, dots has to be drawn in a different
     * manner : if the real world width is equal to 0, then the width is
     * translated to 1 in screen coordinates. Drawing a circle with a
     * 1-diameter and the GDK function #gdk_draw_arc() is not possible.
     * So we needs to test whether the width is 1 or not.
     */
  if(line_width == 1) {
	gdk_draw_point(w, gc, (int) xa, (int) ya);
  } else {
	gdk_draw_arc(w, gc, TRUE,
	     ((int) xa) - line_width/2, 
	     ((int) ya) - line_width/2,
	     line_width, line_width, 0, FULL_CIRCLE);
  }

		
        d = d + space;
    xa = xa + dx2;
    ya = ya + dy2;
  }
}


/*! \brief Draw a line with a phantom line type.
 *  \par Function Description
 *  This function draws a line with a phantom line type. The parameter
 *  <B>space</B> represents the distance between a dot and a dash. The parameter
 *  <B>length</B> represents the length of a dash.
 *
 *  The unit of <B>x1</B>, <B>y1</B> and <B>x2</B>, <B>y2</B> and
 *  <B>line_width</B>, <B>length</B>, <B>space</B> is pixel.
 *
 *  A negative of null value for length or space leads to an endless loop.
 *
 *  \param [in] w           GdkWindow to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] cap         GdkCapStype line end cap style.
 *  \param [in] x1          Upper x coordinate of Line.
 *  \param [in] y1          Upper y coordinate of Line.
 *  \param [in] x2          Lower x coordinate of Line.
 *  \param [in] y2          Lower y coordinate of Line.
 *  \param [in] line_width  Line width.
 *  \param [in] length      Length of dashes in pixels.
 *  \param [in] space       Space between dashes in pixels.
 */
void o_line_draw_phantom(GdkWindow *w, GdkGC *gc, GdkColor *color,
			 GdkCapStyle cap, gint x1, gint y1, gint x2, gint y2,
			 gint line_width, gint length, gint space)
{
  double dx, dy, l, d; 
  double dx1, dy1, dx2, dy2;
  double xa, ya, xb, yb;

  gdk_gc_set_foreground(gc, color);
  gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
			     cap, GDK_JOIN_MITER);

  /*
   * The function determines the increments on x and y axis that match
   * the space on the line between two dots. The same thing is done for length.
   *
   * Starting from one of the end of the line and incrementing the position
   * gives the coordinates of every dots and dashes on the line providing
   * that the second extremity is not exceeded. This is checked by first
   * computing the distance between the two extremities and then checking
   * whether this limit is exceeded by a new dash or not.
   *
   * It draws as many sets of 'dash of length <B>length</B> and two dots'
   * as possible.
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
    gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
		
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    
    /*
     * Depending on the width of the line, dots has to be drawn in a
     * different manner : if the real world width is equal to 0, then the
     * width is translated to 1 in screen coordinates. Drawing a circle with
     * a 1-diameter and the GDK function #gdk_draw_arc() is not possible.
     * So we needs to test whether the width is 1 or not.
     */
    if(line_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - line_width/2, 
		   ((int) ya) - line_width/2,
		   line_width, line_width, 0, FULL_CIRCLE);
    }
    
    d = d + space;
    xa = xa + dx2;
    ya = ya + dy2;

    /*
     * Depending on the width of the line, dots has to be drawn in a different
     * manner : if the real world width is equal to 0, then the width is
     * translated to 1 in screen coordinates. Drawing a circle with a
     * 1-diameter and the GDK function #gdk_draw_arc() is not possible.
     * So we needs to test whether the width is 1 or not.
     */
    
    if(line_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - line_width/2, 
		   ((int) ya) - line_width/2,
		   line_width, line_width, 0, FULL_CIRCLE);
    }
    
    d = d + space;
    xa = xa + dx2;
    ya = ya + dy2;
  }
  
  /*
   * If the above condition is not satisfied, it may still be possible to
   * continue drawing a part of the original pattern.
   * Here three cases are possible :
   * <DL>
   *   <DT>*</DT><DD>it is possible to draw a dash and the two dots.
   *   <DT>*</DT><DD>it is possible to draw a dash and one of the two dots.
   *   <DT>*</DT><DD>it is possible to draw at least a part of the initial
   *                 dash.
   * </DL>
   */

  if((d + length + 2 * space) < l) {
    d = d + length;
    xb = xa + dx1;
    yb = ya + dy1;
    gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
    
    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;
    
    /*
     * Depending on the width of the line, dots has to be drawn in a
     * different manner : if the real world width is equal to 0, then the
     * width is translated to 1 in screen coordinates. Drawing a circle with
     * a 1-diameter and the GDK function #gdk_draw_arc() is not possible.
     * So we needs to test whether the width is 1 or not.
     */  
    if(line_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - line_width/2, 
		   ((int) ya) - line_width/2,
		   line_width, line_width, 0, FULL_CIRCLE);
    }

    d = d + space;
    xa = xb + dx2;
    ya = yb + dy2;

    
    /*
     * Depending on the width of the line, dots has to be drawn in a different
     * manner : if the real world width is equal to 0, then the width is
     * translated to 1 in screen coordinates. Drawing a circle with a
     * 1-diameter and the GDK function #gdk_draw_arc() is not possible.
     * So we needs to test whether the width is 1 or not.
     */  
    if(line_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - line_width/2, 
		   ((int) ya) - line_width/2,
		   line_width, line_width, 0, FULL_CIRCLE);
    }
  } else {
    if(d + length + space < l) {
      d = d + length;
      xb = xa + dx1;
      yb = ya + dy1;
      gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
      
      d = d + space;
      xa = xb + dx2;
      ya = yb + dy2;
      
      /*
       * Depending on the width of the line, dots has to be drawn in a
       * different manner : if the real world width is equal to 0, then the
       * width is translated to 1 in screen coordinates. Drawing a circle
       * with a 1-diameter and the GDK function #gdk_draw_arc() is not
       * possible. So we needs to test whether the width is 1 or not.
       */
      if(line_width == 1) {
	gdk_draw_point(w, gc, (int) xa, (int) ya);
      } else {
	gdk_draw_arc(w, gc, TRUE,
		     ((int) xa) - line_width/2, 
		     ((int) ya) - line_width/2,
		     line_width, line_width, 0, FULL_CIRCLE);
      }
    } else {
      if(d + length < l) {
	xb = xa + dx1;
	yb = ya + dy1;
      } else {
	xb = x2;
	yb = y2;
      }	
      gdk_draw_line(w, gc, (int) xa, (int) ya, (int) xb, (int) yb);
    }
  }
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 *
 *  \note
 *  used in button cancel code in x_events.c
 */
void o_line_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
  o_line_rubberline_xor(w_current);
}

/*! \brief Draw a line object after applying translation.
 *  \par Function Description
 *  This function is used to draw the line object described by
 *  <B>*o_current</B> after applying a translation on the two directions of
 *  <B>dx</B> and <B>dy</B> in world units. It uses and XOR function to draw the
 *  translated line over the current sheet.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for line.
 *  \param [in] dy         Delta y coordinate for line.
 *  \param [in] o_current  Line OBJECT to draw.
 */
void o_line_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int color;
  int sx[2], sy[2];

  if (o_current->line == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  /* changed for dark color stuff */
  gdk_gc_set_foreground(w_current->outline_xor_gc,
                        x_get_darkcolor(color));
  
  WORLDtoSCREEN(toplevel, o_current->line->x[0] + dx, o_current->line->y[0] + dy, &sx[0], &sy[0]);
  WORLDtoSCREEN(toplevel, o_current->line->x[1] + dx, o_current->line->y[1] + dy, &sx[1], &sy[1]);

  gdk_draw_line (w_current->drawable, w_current->outline_xor_gc,
                 sx[0], sy[0], sx[1], sy[1]);
}

/*! \brief Start process to input a new line.
 *  \par Function Description
 *  This function starts the process of interactively adding a line to
 *  the current sheet.
 *
 *  During all the process, the line is internally represented by the two
 *  ends of the line as (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  A temporary line is xor-drawn during the process with the selection color
 *  and changed according to the position of the mouse pointer.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* init first_w[x|y], second_w[x|y] to describe line */
  w_current->first_wx = w_current->second_wx = w_x;
  w_current->first_wy = w_current->second_wy = w_y;
  
  /* draw init xor */
  o_line_rubberline_xor(w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of a line.
 *  \par Function Description
 *  This function ends the process of interactively adding a line to the
 *  current sheet.
 *
 *  It first erases the last temporary line displayed, calculates the
 *  corresponding world coordinates of the two ends of the line and finally
 *  adds a new initialized line object to the list of object of the current
 *  sheet.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_line_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;

  g_assert( w_current->inside_action != 0 );

  /* erase xor image */
  o_line_rubberline_xor(w_current);
  w_current->rubber_visible = 0;

  /* don't allow zero length lines */
  if ( (w_current->first_wx == w_current->second_wx) &&
       (w_current->first_wy == w_current->second_wy) ) {
    return;
  }

  /* create the line object and draw it */
  new_obj = o_line_new(toplevel, OBJ_LINE,
                       w_current->graphic_color,
                       w_current->first_wx, w_current->first_wy,
                       w_current->second_wx, w_current->second_wy);
  s_page_append (toplevel->page_current, new_obj);

  /* draw it */
  o_invalidate (w_current, new_obj);
  
  toplevel->page_current->CHANGED=1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw temporary line while dragging end.
 *  \par Function Description
 *  This function manages the erase/update/draw process of temporary line
 *  when modifying one end of the line.
 *  The line is described by four <B>*w_current</B> variables : the first end
 *  of the line is (<B>first_wx</B>,<B>first_wy</B>), the second end is
 *  (<B>second_wx</B>,<B>second_wy</B>).
 *  The first end is constant. The second end is updated to the (<B>w_x</B>,<B>w_y</B>).
 * 
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_line_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  g_assert( w_current->inside_action != 0 );

  /*
   * The current temporary line is described by the two points
   * (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and
   * (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>) as end of the line.
   *
   * This line is xor-drawn : if the line was already displayed, it is
   * erased. If the line was not already displayed it is drawn.
   */
  if (w_current->rubber_visible)
    o_line_rubberline_xor(w_current);

  /*
   * The coordinates of the moving end of the line are updated. Its new
   * coordinates are in <B>w_x</B> and <B>w_y</B> parameters and saved to
   * <B>w_current->second_wx</B> and <B>w_current->second_wy</B> respectively.
   */ 
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  
  /* if the control key was pressed then draw ortho lines */
  if (w_current->CONTROLKEY) {
    diff_x = abs(w_current->second_wx - w_current->first_wx);
    diff_y = abs(w_current->second_wy - w_current->first_wy);
    
    if (diff_x >= diff_y) {
      w_current->second_wy = w_current->first_wy;
    } else {
      w_current->second_wx = w_current->first_wx;
    }
  }
  
  /*
   * The updated line is finally again xor-drawn : before the call to this
   * function, if the line was displayed, it has been erased, updated and
   * displayed again.
   */
  o_line_rubberline_xor(w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Draw line from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws a line with an exclusive or function over the sheet.
 *  The color of the box is <B>w_current->select_color</B>. The line is
 *  described by the two points (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_line_rubberline_xor(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x1, y1, x2, y2;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN(toplevel, w_current->second_wx, w_current->second_wy, &x2, &y2);

  /* draw the circle from the w_current variables */
  /* with xor-function */
  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color));
  gdk_gc_set_line_attributes(w_current->xor_gc, 0,
			     GDK_LINE_SOLID, GDK_CAP_NOT_LAST, 
			     GDK_JOIN_MITER);
  gdk_draw_line (w_current->drawable, w_current->xor_gc, x1, y1, x2, y2);
}

/*! \brief Draw grip marks on line.
 *  \par Function Description
 *  This function draws the grips on the line object <B>o_current</B>.
 *
 *  A line has a grip at each end.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Line OBJECT to draw grip points on.
 */
void o_line_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x[2], y[2];

  if (w_current->draw_grips == FALSE)
	  return;

  WORLDtoSCREEN( toplevel, o_current->line->x[0], o_current->line->y[0], &x[0], &y[0] );
  WORLDtoSCREEN( toplevel, o_current->line->x[1], o_current->line->y[1], &x[1], &y[1] );

  /* draw the grip on line end 1 */
  o_grips_draw(w_current, x[LINE_END1], y[LINE_END1]);
  
  /* draw the grip on line end 2 */
  o_grips_draw(w_current, x[LINE_END2], y[LINE_END2]);
}
