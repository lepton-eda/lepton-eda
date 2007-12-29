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

#include <libgeda/libgeda.h>

#include "../include/gschem_struct.h"
#include "../include/globals.h"
#include "../include/prototype.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/* Kazu on July 16, 1999 - Added these macros to simplify the code */
#define GET_BOX_WIDTH(w)			\
	abs((w)->last_x - (w)->start_x)
#define GET_BOX_HEIGHT(w)			\
	abs((w)->last_y - (w)->start_y)

typedef void (*DRAW_FUNC)( GdkDrawable *w, GdkGC *gc, GdkColor *color,
                           GdkCapStyle cap, gint x, gint y, gint radius,
                           gint angle1, gint angle2,
                           gint arc_width, gint length, gint space );

typedef void (*FILL_FUNC)( GdkDrawable *w, GdkGC *gc, GdkColor *color,
                           gint x, gint y, gint radius,
                           gint fill_width, gint angle1, gint pitch1,
                           gint angle2, gint pitch2 );

/*! \brief Draw a circle on the screen.
 *  \par Function Description
 *  This function is used to draw a circle on screen. The circle is described
 *  by the OBJECT which is referred by <B>o_current</B>. The display is done 
 *  according to the current state, given by the GSCHEM_TOPLEVEL object pointed by
 *  <B>w_current</B>.
 *
 *  It first checks if the OBJECT pointed is valid or not. If not it
 *  returns and do not output anything. That should never happen though.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Circle OBJECT to draw.
 */
void o_circle_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int wleft, wright, wtop, wbottom; /* world bounds */
  int s_x, s_y;
  int radius;
  int circle_width, length, space;
  int fill_width, angle1, pitch1, angle2, pitch2;
  GdkCapStyle circle_end;
  GdkColor *color;
  DRAW_FUNC draw_func = NULL;
  FILL_FUNC fill_func;

  if (o_current->circle == NULL) {
    return;
  }

  /*
   * Get read to check for visibility of this line by using it's
   * bounding box
   */
  world_get_circle_bounds(toplevel, o_current,
                          &wleft, &wtop, &wright, &wbottom);
	
  if ( (toplevel->DONT_REDRAW == 1) ||
       (!visible(toplevel, wleft, wtop, wright, wbottom)) ) {
    return;
  }
	
#if DEBUG
  printf("drawing circle\n\n");
#endif
	
  /*
   * The draw of the circle is divided in two steps : first step is to draw
   * the outline, the second is to draw the filling pattern inside (if any).
   *
   * Finally the function takes care of the grips.
   */
  if (toplevel->override_color != -1 ) {
    color = x_get_color(toplevel->override_color);
  } else {
    color = x_get_color(o_current->color);
  }

  radius = SCREENabs( toplevel, o_current->circle->radius );

  /*
   * The values describing the line type are extracted from the
   * <B>o_current</B> pointed structure. These are the width of the line, the
   * field called length and the field called space and the desired end type
   * for the line.
   *
   * Depending on the type of the line that has to be used to draw the circle
   * the appropriate function is called. Values of space and length are
   * adapted to the type of line. The possible functions are the following :
   * #o_arc_draw_solid(), #o_arc_draw_dotted(), #o_arc_draw_dashed() and
   * #o_arc_draw_phantom(). Therefore it reuses the code from arc primitive.
   *
   * The combination <B>length</B> == 0 and <B>space</B> == 0 is avoided as it lead
   * to an endless loop in function called after. If such a case is encountered
   * the circle is drawn as a solid circle independently of its initial type.
   */
  circle_width = SCREENabs( toplevel, o_current->line_width );
  if(circle_width <= 0) {
    circle_width = 1;
  }

  length = SCREENabs( toplevel, o_current->line_length );
  space = SCREENabs( toplevel, o_current->line_space );

  switch(o_current->line_end) {
    case END_NONE:   circle_end = GDK_CAP_BUTT;       break;
    case END_SQUARE: circle_end = GDK_CAP_PROJECTING; break;
    case END_ROUND:  circle_end = GDK_CAP_ROUND;      break;
    default: fprintf(stderr, _("Unknown end for circle\n"));
      circle_end = GDK_CAP_BUTT;
      break;
  }

  switch(o_current->line_type) {
    case TYPE_SOLID:
    length = -1;
    space = -1;
    draw_func = o_arc_draw_solid;
    break;
			
    case TYPE_DOTTED:
    length = -1; /* ..._draw_dotted only space used */
    draw_func = o_arc_draw_dotted;
    break;
			
    case TYPE_DASHED:
    draw_func = o_arc_draw_dashed;
    break;
			
    case TYPE_CENTER:
    draw_func = o_arc_draw_center;
    break;
			
    case TYPE_PHANTOM:
    draw_func = o_arc_draw_phantom;
    break;
			
    case TYPE_ERASE:
    break;
			
    default:
    length = -1;
    space = -1;
    circle_width = 0; /* just to be careful */
    fprintf(stderr, _("Unknown type for circle!\n"));
    draw_func = o_arc_draw_solid;			
    break;
  }

  if((length == 0) || (space == 0))
  draw_func = o_arc_draw_solid;

  WORLDtoSCREEN( toplevel, o_current->circle->center_x, o_current->circle->center_y,
                 &s_x, &s_y );
	
  (*draw_func)(w_current->backingstore, w_current->gc, color,
               circle_end,
               s_x, s_y,
               radius,
               0, FULL_CIRCLE / 64,
               circle_width, length, space);

  /*
   * The values needed for the fill operation are taken from the
   * <B>o_current</B> pointed OBJECT. It include the type of fill required, the
   * width of the lines (if the fill use line) and angles and pitchs for hatch
   * based filling.
   *
   * Once again the width of the line is important as if it is equal to 0 it
   * may not be displayed. That is definetely not what we are looking for.
   *
   * Depending on the type of fill that has to be used inside the circle the
   * right function is called. Values of <B>angle1</B>, <B>angle2</B>,
   * <B>pitch1</B> and <B>pitch2</B> are adapted to the type of filling. The
   * possible functions are the following : #o_circle_fill_hollow(),
   * #o_circle_fill_fill(), #o_circle_fill_mesh() and #o_circle_fill_hatch().
   *
   * The combination <B>pitch1</B> <= 0 and <B>pitch2</B> <= 0 is avoided as it
   * lead to an endless loop in function called after. It happens when the
   * zoom factor is too small for two lines separated by the pitch to be
   * distinct. If such a case is encountered the circle is filled hollow
   * (e.q. not filled).
   */
  fill_width = SCREENabs( toplevel, o_current->fill_width );
  if( fill_width <= 0) {
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
      /*
       * this function is empty ! however if it do not use it we have to add
       * a test before the call. Simply putting a return here instead is not
       * possible as it would prevent any hollow circle from having its grips
       */
      fill_func = o_circle_fill_hollow;
      break;
		
    case FILLING_FILL:
      angle1 = -1; angle2 = -1;
      pitch1 = 1; pitch2 = 1;
      fill_func = o_circle_fill_fill;
      break;
			
    case FILLING_MESH:
      fill_func = o_circle_fill_mesh;
      break;

    case FILLING_HATCH:
      angle2 = -1;
      pitch2 = 1;
      fill_func = o_circle_fill_hatch;
      break;
			
    case FILLING_VOID:
    default:
      angle1 = -1; angle2 = -1;
      pitch1 = 1; pitch2 = 1;
      fill_func = o_circle_fill_hollow;			
      fprintf(stderr, _("Unknown type for circle (fill)!\n"));
  }

  if((pitch1 <= 0) || (pitch2 <= 0)) {
    fill_func = o_circle_fill_fill;
  }

  (*fill_func)(w_current->backingstore, w_current->gc, color,
               s_x, s_y,
               radius,
               fill_width, angle1, pitch1, angle2, pitch2);

#if DEBUG
  printf("drawing circle\n");
#endif

  if (o_current->draw_grips && w_current->draw_grips == TRUE) {	
    
    /* pb20011010 - modified to use the new o_circle_[draw|erase]_grips() */
    if (!o_current->selected) {
      /* object is no more selected, erase the grips */
      o_current->draw_grips = FALSE;
      o_circle_erase_grips(w_current, o_current);
    } else {
      /* object is selected, draw the grips */
      o_circle_draw_grips(w_current, o_current);
    }
  }
}

/*! \brief Placeholder filling function.
 *  \par Function Description
 *  This function does nothing. It has the same prototype as all the filling
 *  functions. It prevent from making a difference between filling in function
 *  #o_circle_draw().
 *
 *  The unit for <B>width</B>, <B>pitch1</B> and <B>pitch2</B> is pixel and unit
 *  for <B>angle1</B> and <B>angle2</B> is degree.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] x           Center x coordinate of Circle.
 *  \param [in] y           Center y coordinate of Circle.
 *  \param [in] radius      Radius of Circle.
 *  \param [in] width       Circle pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
void o_circle_fill_hollow(GdkDrawable *w, GdkGC *gc, GdkColor *color,
			  gint x, gint y, gint radius,
			  gint width,
			  gint angle1, gint pitch1, gint angle2, gint pitch2)
{
  
}

/*! \brief Fill inside of circle with a solid pattern.
 *  \par Function Description
 *  This function fills the inside of the circle with a solid pattern.
 *  Parameters <B>angle1</B>, <B>pitch1</B> and <B>angle2</B>, <B>pitch2</B>
 *  and <B>width</B> are unused here but kept for compatibility with other
 *  circle filling functions.
 *
 *  The circle is described by the coordinates of its center and its radius.
 *  Please not that it is not the way GDK take it. Translation is made
 *  afterward.
 *
 *  The unit for <B>width</B>, <B>pitch1</B> and <B>pitch2</B> is pixel and unit
 *  for <B>angle1</B> and <B>angle2</B> is degree.
 *
 *  The solid fill is done with the #gdk_draw_arc() function and its
 *  parameters <B>filled</B> set. The circle is filled with the color
 *  <B>color</B> given as a parameter to the function.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] x           Center x coordinate of Circle.
 *  \param [in] y           Center y coordinate of Circle.
 *  \param [in] radius      Radius of Circle.
 *  \param [in] width       (unused)
 *  \param [in] angle1      (unused)
 *  \param [in] pitch1      (unused)
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
void o_circle_fill_fill(GdkDrawable *w, GdkGC *gc, GdkColor *color,
			gint x, gint y, gint radius,
			gint width,
			gint angle1, gint pitch1, gint angle2, gint pitch2)
{
  gdk_gc_set_foreground(gc, color);
  gdk_gc_set_line_attributes(gc, 1, GDK_LINE_SOLID,
                             GDK_CAP_BUTT, GDK_JOIN_MITER);

  gdk_draw_arc(w, gc,
               TRUE, x-radius, y-radius, 2*radius, 2*radius, 0, FULL_CIRCLE);

}

/*! \brief Fill inside of circle with single line pattern.
 *  \par Function Description
 *  This function fills the inside of the circle with a pattern made of lines.
 *  The lines are drawn inside the circle with an angle <B>angle1</B> from the
 *  horizontal. The distance between two of these lines is given by
 *  <B>pitch1</B> and their width by <B>width</B>.
 *  Parameters <B>angle2</B>, <B>pitch2</B> are unused here but kept for
 *  compatibility with other circle filling functions.
 *
 *  The circle is described by the coordinates of its center and its radius.
 *  Please not that it is not the way GDK take it. Translation is made
 *  afterward. 
 *
 *  The unit for <B>width</B>, <B>pitch1</B> and <B>pitch2</B> is pixel and unit
 *  for <B>angle1</B> and <B>angle2</B> is degree.
 *
 *  The only attribute of line here is its width from the parameter <B>width</B>.
 *
 *  Negative or null values for <B>pitch1</B> are not allowed as it leads to
 *  an endless loop.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] x           Center x coordinate of Circle.
 *  \param [in] y           Center y coordinate of Circle.
 *  \param [in] radius      Radius of Circle.
 *  \param [in] width       Circle pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
void o_circle_fill_hatch(GdkDrawable *w, GdkGC *gc, GdkColor *color,
			 gint x, gint y, gint radius,
			 gint width,
			 gint angle1, gint pitch1, gint angle2, gint pitch2)
{
  double x0, y0, x1, y1, x2, y2;
  double cos_a_, sin_a_;

  gdk_gc_set_line_attributes(gc, width, GDK_LINE_SOLID,
                             GDK_CAP_BUTT, GDK_JOIN_MITER);

  /*
   * The function use a matrix. Its elements are obtained from the sinus and
   * cosinus of the angle <B>angle1</B>. It represent the rotation matrix that
   * when applied to a point, rotate it of <B>angle1</B>.
   */
  cos_a_ = cos(((double) angle1) * M_PI/180);
  sin_a_ = sin(((double) angle1) * M_PI/180);

  /*
   * When drawing a line in a circle there is two intersections. It looks for
   * the coordinates of one of these points when the line is horizontal.
   * The second one can be easily obtained by symmetry in relation to the
   * vertical axis going through the centre of the circle.
   *
   * These two points are then rotated of angle <B>angle1</B> using the
   * elements of the rotation matrix previously computed.
   *
   * The corresponding line can be drawn providing that the coordinates
   * are rounded.
   *
   * These operations are repeated for every horizontal line that can fit
   * in the upper half of the circle (using and incrementing the variable
   * <B>y0</B>).
   */
  y0 = 0;
  while(y0 < (double) radius) {
    x0 = pow((double) radius, 2) - pow(y0, 2);
    x0 = sqrt(x0);

    x1 = (x0*cos_a_ - y0*sin_a_) + x;
    y1 = y - (x0*sin_a_ + y0*cos_a_);
    x2 = ((-x0)*cos_a_ - y0*sin_a_) + x;
    y2 = y - ((-x0)*sin_a_ + y0*cos_a_);
		
    gdk_draw_line(w, gc,
                  (int) x1, (int) y1, (int) x2, (int) y2);
        
    /*
     * The function use the symetry in relation to the centre of the circle.
     * It avoid repetitive computation for the second half of the surface
     * of the circle.
     */
    x1 = (x0*cos_a_ - (-y0)*sin_a_) + x;
    y1 = y- (x0*sin_a_ + (-y0)*cos_a_);
    x2 = ((-x0)*cos_a_ - (-y0)*sin_a_) + x;
    y2 = y- ((-x0)*sin_a_ + (-y0)*cos_a_);
    
    gdk_draw_line(w, gc, (int) x1, (int) y1, (int) x2, (int) y2);
    
    y0 = y0 + pitch1;
  }
}

/*! \brief Fill inside of circle with mesh pattern.
 *  \par Function Description
 *  This function fills the inside of the circle with a pattern made of set
 *  of parallel lines in two directions. The first set is drawn inside the
 *  circle with an angle <B>angle1</B> from the horizontal. The distance between
 *  two of these lines is given by <B>pitch1</B>.
 *  The second set is drawn inside the circle with an angle <B>angle2</B> from
 *  the horizontal. The distance between two of these lines is given by
 *  <B>pitch2</B>.
 *  Every lines have the same width given by <B>width</B>.
 *
 *  The unit for <B>width</B>, <B>pitch1</B> and <B>pitch2</B> is pixel and unit
 *  for <B>angle1</B> and <B>angle2</B> is degree.
 *
 *  This function simply makes two successive calls to the function
 *  #o_circle_fill_hatch() respectively with <B>angle1</B>, <B>pitch1</B> and
 *  <B>angle2</B>, <B>pitch2</B> for parameters.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Circle fill color. 
 *  \param [in] x           Center x coordinate of Circle.
 *  \param [in] y           Center y coordinate of Circle.
 *  \param [in] radius      Radius of Circle.
 *  \param [in] width       Circle pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
void o_circle_fill_mesh(GdkDrawable *w, GdkGC *gc, GdkColor *color,
			gint x, gint y, gint radius,
			gint width,
			gint angle1, gint pitch1, gint angle2, gint pitch2)
{
  o_circle_fill_hatch(w, gc, color,
                      x, y, radius,
                      width, angle1, pitch1, -1, -1);
  o_circle_fill_hatch(w, gc, color,
                      x, y, radius,
                      width, angle2, pitch2, -1, -1);
	
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_circle_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
   o_circle_rubbercircle_xor(w_current);
}

/*! \brief Draw a circle described by OBJECT with translation
 *  \par Function Description
 *  This function draws the circle object described by <B>*o_current</B>
 *  translated by the vector (<B>dx</B>,<B>dy</B>) with an xor-function over
 *  the current sheet.
 *  The translation vector is in screen unit.
 *
 *  The circle is displayed with the color of the object.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for circle.
 *  \param [in] dy         Delta y coordinate for circle.
 *  \param [in] o_current  Circle OBJECT to draw.
 *
 *  \todo
 *  add in offsets, get rid of global diffs_x,y
 */
void o_circle_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x, y, radius;
  int color;

  if (o_current->circle == NULL) {
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  /* radius of the circle */
  radius = SCREENabs( toplevel, o_current->circle->radius );
  /* upper left corner of the square the circle is inscribed in */
  /* gdk coords system */
  WORLDtoSCREEN( toplevel,
                 o_current->circle->center_x - o_current->circle->radius,
                 o_current->circle->center_y + o_current->circle->radius,
                 &x, &y );
  
  /* translate the upper left corner */
  x = x + dx;
  y = y + dy;
  
  /* To draw be sure to setup width height */
  gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));
  gdk_draw_arc(w_current->backingstore, w_current->outline_xor_gc,
	       FALSE,
	       x, y,
	       2 * radius, 2 * radius,
	       0, FULL_CIRCLE);
  
  /* backing store ?  not appropriate here */
}

/*! \brief Start process to input a new circle.
 *  \par Function Description
 *  This function starts the process to input a new circle. Parameters for
 *  this circle are pu into/extracted from the <B>w_current</B> toplevel
 *  structure.
 *  <B>x</B> and <B>y</B> are current coordinates of the mouse pointer in
 *  screen units.
 *
 *  The first step of the circle input is to set the center of the arc.
 *  This center is kept in (<B>w_current->start_x</B>,<B>w_current->start_y</B>). 
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] x          Current x coordinate of pointer in screen units.
 *  \param [in] y          Current y coordinate of pointer in screen units.
 */
void o_circle_start(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
	/* center of circle */
	w_current->last_x = w_current->start_x = fix_x(toplevel, x);
	w_current->last_y = w_current->start_y = fix_y(toplevel, y);
	/* radius */
	w_current->distance = 0;

	/* first temporary circle */
	o_circle_rubbercircle_xor(w_current);

}

/*! \brief End the input of a circle.
 *  \par Function Description
 *  This function ends the input of the radius of the circle.
 *  The (<B>x</B>,<B>y</B>) point is taken as the other end of the radius
 *  segment, i.e. on the circle. The distance between this point and the
 *  center is the radius of the circle.
 *  <B>x</B> and <B>y</B> are in screen coords.
 *
 *  The center has previously been input and saved as
 *  (<B>w_current->start_x</B>,<B>w_current->start_y</B>).
 *
 *  The temporary circle drawn during the input of the radius is erased.
 *  A new object is allocated, initialized and linked in the object list.
 *  This new object is finally drawn.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] x          Current x coordinate of pointer in screen units.
 *  \param [in] y          Current y coordinate of pointer in screen units.
 */
void o_circle_end(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int center_x, center_y;
  int fx, fy;
  int radius;

  g_assert( w_current->inside_action != 0 );

  /* erase the temporary circle */
  o_circle_rubbercircle_xor(w_current);
  
  /* get the last coords of the pointer */
  fx = fix_x(toplevel, x);
  fy = fix_y(toplevel, y);
  /* compute the radius in screen unit */
  w_current->distance = dist(w_current->start_x, w_current->start_y,
                             fx, fy);

  /* circle with null radius are not allowed */
  if (w_current->distance == 0) {
    /* cancel the object creation */
    w_current->start_x  = -1;
    w_current->start_y  = -1;
    w_current->last_x   = -1;
    w_current->last_y   = -1;
    w_current->distance = -1;
    return;
  }

  /* get center coords in world unit */
  SCREENtoWORLD(toplevel,
                w_current->start_x, w_current->start_y,
                &center_x, &center_y);
  /* get radius in world unit */
  radius = snap_grid(toplevel,
                     WORLDabs(toplevel, w_current->distance));

  /* create the object */
  toplevel->page_current->object_tail =
  o_circle_add(toplevel,
               toplevel->page_current->object_tail,
               OBJ_CIRCLE, w_current->graphic_color,
               center_x, center_y, radius);

  /* draw it */
  o_redraw_single(w_current, toplevel->page_current->object_tail);
  
  w_current->start_x = (-1);
  w_current->start_y = (-1);
  w_current->last_x  = (-1);
  w_current->last_y  = (-1);
  w_current->loc_x   = (-1);
  w_current->loc_y   = (-1);
  w_current->distance = (-1);
  
  toplevel->page_current->CHANGED = 1;

  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw temporary circle while dragging edge.
 *  \par Function Description
 *  This function draws a circle according to its internal representation and
 *  allows the modification of its radius. The radius is updated according to
 *  the current mouse position in <B>x</B> and <B>y</B>.
 *  It draws a full circle and the horizontal segment of the radius in the
 *  right half of the circle.
 *
 *  The previous temporary circle is erased, the radius is then computed and
 *  updated and finally a new temporary circle is drawn.
 *
 *  The arc is internally described by :
 *  <DL>
 *    <DT>*</DT><DD>(<B>w_current->start_x</B>,<B>w_current->start_y</B>) as its
 *                   center ;
 *    <DT>*</DT><DD><B>w_current->distance</B> as its radius.
 *  </DL>
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] x          Current x coordinate of pointer in screen units.
 *  \param [in] y          Current y coordinate of pointer in screen units.
 */
void o_circle_rubbercircle(GSCHEM_TOPLEVEL *w_current, int x, int y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int diff_x, diff_y;

  g_assert( w_current->inside_action != 0 );

  /* erase the previous temporary circle */
  o_circle_rubbercircle_xor(w_current);

  /*
   * The radius is taken as the biggest distance on the x and y axis between
   * the center of the circle and the mouse position.
   */
  /* update the radius */
  w_current->last_x = fix_x(toplevel, x);
  w_current->last_y = fix_y(toplevel, y);
  
  diff_x = GET_BOX_WIDTH (w_current);
  diff_y = GET_BOX_HEIGHT(w_current);
  if (diff_x >= diff_y) {
    w_current->last_y   = w_current->start_y;
    w_current->distance = diff_x;
  } else {
    w_current->last_x   = w_current->start_x;
    w_current->distance = diff_y;
  }

  /* draw the new temporary circle */
  o_circle_rubbercircle_xor(w_current);

}

/*! \brief Draw circle from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws the circle from the variables in the GSCHEM_TOPLEVEL
 *  structure <B>*w_current</B>.
 *  The center of the circle is at (<B>w_current->start_x</B>,
 *  <B>w_current->start_y</B>) and its radius is in <B>w_current->distance</B>.
 *
 *  It draws a horizontal radius segment on the right half of the circle and
 *  the circle with the selection color and an xor-function over the current
 *  sheet..
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_circle_rubbercircle_xor(GSCHEM_TOPLEVEL *w_current)
{
  /* draw the circle from the w_current variables */
  gdk_gc_set_foreground(w_current->xor_gc, 
			x_get_darkcolor(w_current->select_color));
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		w_current->start_x, w_current->start_y,
		w_current->start_x + w_current->distance,
		w_current->start_y);
  gdk_draw_arc(w_current->backingstore, w_current->xor_gc, FALSE,
	       w_current->start_x - w_current->distance,
	       w_current->start_y - w_current->distance,
	       w_current->distance * 2,
	       w_current->distance * 2,
	       0, FULL_CIRCLE);
  o_invalidate_rect(w_current, w_current->start_x - w_current->distance,
                               w_current->start_y - w_current->distance,
                               w_current->start_x + w_current->distance,
                               w_current->start_y + w_current->distance);
}

/*! \brief Draw grip marks on circle.
 *  \par Function Description
 *  This function draws the grip that match the circle object <B>*o_current</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Circle OBJECT to draw grip points on.
 */
void o_circle_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x, y;

  if (w_current->draw_grips == FALSE)
	  return;

  /* coords of the lower right corner of the square */
  WORLDtoSCREEN( toplevel,
                 o_current->circle->center_x + o_current->circle->radius,
                 o_current->circle->center_y - o_current->circle->radius,
                 &x, &y );
  
  /* grip on lower right corner of the square */
  o_grips_draw(w_current, x, y);

}

/*! \brief Erase grip marks from circle.
 *  \par Function Description
 *  The function erases the grips displayed on a circle object.
 *
 *  A circle has a single grip on the lower right corner of the square it
 *  is inscribed in.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Circle OBJECT to erase grip marks from.
 */
void o_circle_erase_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x, y;

  if (w_current->draw_grips == FALSE)
	  return;

  /* coords of the lower right corner of square */
  WORLDtoSCREEN( toplevel,
                 o_current->circle->center_x + o_current->circle->radius,
                 o_current->circle->center_y - o_current->circle->radius,
                 &x, &y );
  
  /* grip on lower right corner of the square */
  o_grips_erase(w_current, x, y);

}
