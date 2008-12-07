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
                           GdkCapStyle cap, gint x, gint y, gint radius,
                           gint angle1, gint angle2,
                           gint arc_width, gint length, gint space );

/*! \brief Draw an arc on the screen.
 *  \par Function Description
 *  This function is used to draw an arc on screen. The arc is described
 *  in the object which is referred by <B>o_current</B>. The arc is displayed
 *  according to the current state, described in the GSCHEM_TOPLEVEL object
 *  pointed by <B>w_current</B>.
 *
 *  It first checkes if the object is valid or not. If not it returns
 *  and do not output anything. That should never happen though.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  The arc OBJECT to draw.
 */
void o_arc_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int wleft, wright, wtop, wbottom;
  int x, y, radius, start_angle, end_angle;
  int arc_width;
  GdkCapStyle arc_end;
  GdkColor *color;
  DRAW_FUNC draw_func = NULL;
  int length, space;

  if (o_current->arc == NULL) {
    return;
  }

  world_get_single_object_bounds(toplevel, o_current,
                                 &wleft, &wtop, &wright, &wbottom);

  if ( (toplevel->DONT_REDRAW == 1) ||
       (!visible(toplevel, wleft, wtop, wright, wbottom)) ) {
    return;
  }

  /*
   * As an arc is definetely not a closed shape there is no need to
   * define and call any filling function. Another way to say that is
   * that an arc can not be filled. It simply draws the arc according
   * to the type line.
   *
   * The values describing the line type are extracted from the
   * <B>o_current</B> pointed structure. These are the width of the line,
   * the field called length and the field called space and the desired
   * end type for the arc.
   *
   * Depending on the type of line desired the appropriate function is
   * called. Values of space and length are adapted to the type of line.
   * The possible functions are the following : #o_arc_draw_solid(),
   * #o_arc_draw_dotted(), #o_arc_draw_dashed() and #o_arc_draw_phantom().
   *
   * The combination <B>length</B> == 0 and <B>space</B> == 0 is avoided as it
   * leads to an endless loop in function called after. If such a case is
   * encountered the arc is drawn as a solid arc independently of its
   * initial type.
   */
  WORLDtoSCREEN( toplevel, o_current->arc->x, o_current->arc->y, &x, &y );
  radius = SCREENabs( toplevel, o_current->arc->width / 2 );
  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;

#if DEBUG 
  printf("drawing arc x: %d y: %d sa: %d ea: %d width: %d height: %d\n",
         x,
         y, 
         o_current->arc->start_angle, 
         o_current->arc->end_angle,
         radius, 
         radius);
#endif

  if (toplevel->override_color != -1 )
    color = x_get_color(toplevel->override_color);
  else
    color = x_get_color(o_current->color);

  arc_width = SCREENabs( toplevel, o_current->line_width );
  if(arc_width <= 0) {
    arc_width = 1;
  }
	
  switch(o_current->line_end) {
  case END_NONE:   arc_end = GDK_CAP_BUTT;       break;
  case END_SQUARE: arc_end = GDK_CAP_PROJECTING; break;
  case END_ROUND:  arc_end = GDK_CAP_ROUND;      break;
  default: fprintf(stderr, _("Unknown end for arc (%d)\n"), o_current->line_end); 
    arc_end = GDK_CAP_BUTT; 
    break;
  }

  length = SCREENabs( toplevel, o_current->line_length );
  space = SCREENabs( toplevel, o_current->line_space );
	
  switch(o_current->line_type) {
  case TYPE_SOLID:
    length = -1;
    space = -1;
    draw_func = o_arc_draw_solid;
    break;
			
  case TYPE_DOTTED:
    length = -1; /* o_arc_draw_dotted uses space parameter only */
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
    arc_width = 0; /* just to be careful */
    draw_func = o_arc_draw_solid;
    fprintf(stderr, _("Unknown type for arc !\n"));
    break;
  }

  if((length == 0) || (space == 0))
    draw_func =  o_arc_draw_solid;

  (*draw_func)(w_current->backingstore, w_current->gc, color,
               arc_end,
               x, y, radius, start_angle, end_angle,
               arc_width, length, space);


  if (o_current->draw_grips && w_current->draw_grips == TRUE) {
    if(!o_current->selected) {
      /* object is no more selected, erase the grips */
      o_current->draw_grips = FALSE;
      o_arc_erase_grips(w_current, o_current);
    } else {
      /* object is selected, draw the grips on the arc */
      o_arc_draw_grips(w_current, o_current);
    }
  }

#if DEBUG
  printf("drawing arc\n");
#endif
}

/*! \brief Draw an arc with a solid line type.
 *  \par Function Description
 *  This function draw an arc with a solid line type. The arc is defined
 *  by two angle (start and end of arc) and the definition of a box by
 *  which the arc is restricted by. The parameters <B>length</B> and
 *  <B>space</B> are unused here.
 *
 *  The unit of <B>x</B>, <B>y</B> and <B>width</B>, <B>height</B> and <B>arc_width</B>,
 *  <B>space</B>, <B>length</B> is pixel.
 *
 *  <B>angle1</B> and <B>angle2</B> are in degrees.
 *
 *  The lines attributes are settled. Then it simply make a call to
 *  the gdk original function.
 *
 *  \param [in] w          GdkWindow to draw in.
 *  \param [in] gc         GdkGC graphics context to draw on.
 *  \param [in] color      Arc line color.
 *  \param [in] cap        Arc line end cap type (unused).
 *  \param [in] x          Arc center x.
 *  \param [in] y          Arc center y.
 *  \param [in] radius     Arc radius.
 *  \param [in] angle1     Start angle in degrees.
 *  \param [in] angle2     End angle in degrees.
 *  \param [in] arc_width  Width of arc line.
 *  \param [in] length     (unused)
 *  \param [in] space      (unused)
 */
void o_arc_draw_solid(GdkWindow *w, GdkGC *gc,
		      GdkColor *color, GdkCapStyle cap,
		      gint x, gint y, gint radius,
		      gint angle1, gint angle2,
		      gint arc_width, gint length, gint space)
{
  gdk_gc_set_foreground(gc, color);
  /* Set the width, end type and join style of the line */
  gdk_gc_set_line_attributes(gc, arc_width, 
                             GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

  /* Draw the arc */
  gdk_draw_arc(w, gc, FALSE,
               x - radius, y - radius, 2 * radius, 2 * radius,
               angle1 * 64, angle2 * 64);

}

/*! \brief Draw an arch with a dotted line type.
 *  \par Function Description
 *  This functions draws an arc with a dotted line type. The parameter
 *  <B>space</B> represents the distance between two of the dots. The
 *  parameter <B>length</B> is unused. The diameter of the dots is given
 *  by the width of the line given by <B>arc_width</B>.
 *
 *  The unit of <B>x</B>, <B>y</B> and <B>width</B>, <B>height</B> and
 *  <B>arc_width</B>, <B>space</B>, <B>length</B> is pixel.
 *
 *  <B>angle1</B> and <B>angle2</B> are in degrees.
 *  A negative or null value for length leads to an endless loop.
 *  length parameter is not used here.
 *
 *  \param [in] w          GdkWindow to draw in.
 *  \param [in] gc         GdkGC graphics context to draw on.
 *  \param [in] color      Arc line color.
 *  \param [in] cap        Arc line end cap type (unused).
 *  \param [in] x          Arc center x.
 *  \param [in] y          Arc center y.
 *  \param [in] radius     Arc radius.
 *  \param [in] angle1     Start angle in degrees.
 *  \param [in] angle2     End angle in degrees.
 *  \param [in] arc_width  Width of arc line.
 *  \param [in] length     (unused).
 *  \param [in] space      Spacing between dots in pixels.
 */
void o_arc_draw_dotted(GdkWindow *w, GdkGC *gc,
		       GdkColor *color, GdkCapStyle cap,
		       gint x, gint y, gint radius,
		       gint angle1, gint angle2,
		       gint arc_width, gint length, gint space)
{
  double xa, ya;
  int da, d;

  gdk_gc_set_foreground(gc, color);

  /*
   * It calculates the angle that match the length <B>space</B> as <B>da</B>.
   * If <B>da</B> is negative or null, the way the function uses it after
   * leads to an andless loop. In fact such a case occur when it is not
   * possible to distinguish a solid line from a dotted line because of
   * zoom factor. The arc is therefore drawn solid.
   */
  /* inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if(angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }

  da = space * 180 / (M_PI * radius);

  /* If da is too small for arc to be displayed as dotted,
     draw a solid arc */
  if(da <= 0) {
    gdk_draw_arc(w, gc, FALSE,
                 x - radius, y - radius, 2 * radius, 2 * radius,
                 angle1 * 64, angle2 * 64);
    return;
  }
  /*
   * It starts from <B>angle1</B> and increments <B>angle1</B> by <B>da</B> as
   * a loop until all the arc has been browsed. For each angle value a
   * dot is drawing after calculating back its coordinates from the angle.
   */
  d = angle1;
  while(d < (angle2 + angle1)) {
    xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

    /*
     * Depending on the width of the line, dots has to be drawn in a
     * different manner : if the real worl width is equal to 0, then the
     * width is translated to 1 in screen coordinates to be visible.
     * Drawing a circle with a 1-diameter and the GDK function
     * #gdk_draw_arc() is not possible. So we needs to test
     * whether the width is 1 or not.
     */
    if(arc_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - arc_width/2, 
		   ((int) ya) - arc_width/2,
		   arc_width, arc_width, 0, FULL_CIRCLE);
    }

    d = d + da;
  }

}

/*! \brief Draw an arc with a dashed line type.
 *  This functions draws an arc with a dashed line type. The parameter
 *  <B>space</B> represents the distance between two of the dash. The
 *  parameter <B>length</B> represents the length of the dash.
 *
 *  The unit of <B>x</B>, <B>y</B> and <B>width</B>, <B>height</B> and
 *  <B>arc_width</B>, <B>space</B>, <B>length</B> is pixel.
 *
 *  <B>angle1</B> and <B>angle2</B> are in degrees.
 *
 *  A negative or null value for <B>length</B> and <B>space</B> leads to an
 *  endless loop.
 *
 *  \param [in] w          GdkWindow to draw in.
 *  \param [in] gc         GdkGC graphics context to draw on.
 *  \param [in] color      Arc line color.
 *  \param [in] cap        Arc line end cap type (unused).
 *  \param [in] x          Arc center x.
 *  \param [in] y          Arc center y.
 *  \param [in] radius     Arc radius.
 *  \param [in] angle1     Start angle in degrees.
 *  \param [in] angle2     End angle in degrees.
 *  \param [in] arc_width  Width of arc line.
 *  \param [in] length     Length of dash in pixels..
 *  \param [in] space      Spacing between dashes in pixels.
 */
void o_arc_draw_dashed(GdkWindow *w, GdkGC *gc,
		       GdkColor *color, GdkCapStyle cap,
		       gint x, gint y,
		       gint radius,
		       gint angle1, gint angle2,
		       gint arc_width, gint length, gint space)
{
  int da, db, a1, a2, d;

  gdk_gc_set_foreground(gc, color);	
  gdk_gc_set_line_attributes(gc, arc_width, GDK_LINE_SOLID, cap, 
                             GDK_JOIN_MITER);

  /*
   * The function first determines the radius and the center of the arc.
   *
   * The function determines the angular increments that match the
   * <B>length</B> and <B>space</B> parameter. These are <B>db</B> and <B>da</B>.
   * Whenever one or both of these variables are null or negative, the
   * line is drawn solid. It means that it is not possible to distinguish
   * a dashed line from a solid line because of current value of zoom.
   */

  /* inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if(angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }
  da = length * 180 / (M_PI * radius);
  db = space  * 180 / (M_PI * radius);

  /* If da or db too small for arc to be displayed as dotted,
     draw a solid arc */
  if((da <= 0) || (db <= 0)) {
    gdk_draw_arc(w, gc, FALSE,
                 x - radius, y - radius, 2 * radius, 2 * radius,
                 angle1 * 64, angle2 * 64);
    return;
  }

  /*
   * It starts from <B>angle1</B> and increments <B>angle1</B> by <B>da</B> as a
   * loop until the whole arc has been browsed. At each position a dash
   * of length <B>length</B> - represented by the <B>da</B> increment - is drawn.
   *
   * It draws as many dashes of length <B>length</B> as possible.
   */

  d = angle1;
  while((d + da + db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;
    gdk_draw_arc(w, gc, FALSE,
                 x - radius, y - radius, 2 * radius, 2 * radius,
                 a1 * 64, da * 64);

    d = d + db;
		
  }

  /*
   * When the above condition is no more satisfied, the it is possible
   * to draw a dash of length <B>length</B>. However it may be possible to
   * draw a shorter dash.
   */

  if((d + da) < (angle1 + angle2)) {
    a1 = d;
    a2 = da;
  } else {
    a1 = d;
    a2 = (angle1 + angle2) - d;
  }
  gdk_draw_arc(w, gc, FALSE,
               x - radius, y - radius, 2 * radius, 2 * radius,
               a1 * 64, a2 * 64);
	
}


/*! \brief Draw arc with a centered line type.
 *  \par Function Description
 *  This functions draws an arc with a centered line type. The parameter
 *  <B>space</B> represents the distance between a dot and the dash. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  The unit of <B>x</B>, <B>y</B> and <B>width</B>, <B>height</B> and
 *  <B>arc_width</B>, <B>space</B>, <B>length</B> is pixel.
 *
 *  <B>angle1</B> and <B>angle2</B> are in units of degrees.
 *
 *  A negative or null value for <B>length</B> and <B>space</B> leads to an
 *  endless loop.
 *
 *  \param [in] w          GdkWindow to draw in.
 *  \param [in] gc         GdkGC graphics context to draw on.
 *  \param [in] color      Arc line color.
 *  \param [in] cap        Arc line end cap type (unused).
 *  \param [in] x          Arc center x.
 *  \param [in] y          Arc center y.
 *  \param [in] radius     Arc radius.
 *  \param [in] angle1     Start angle in degrees.
 *  \param [in] angle2     End angle in degrees.
 *  \param [in] arc_width  Width of arc line.
 *  \param [in] length     Length of dash in pixels..
 *  \param [in] space      Spacing between dashes in pixels.
 */
void o_arc_draw_center(GdkWindow *w, GdkGC *gc,
		       GdkColor *color, GdkCapStyle cap,
		       gint x, gint y,
		       gint radius,
		       gint angle1, gint angle2,
		       gint arc_width, gint length, gint space)
{
  double xa, ya; /* coordinate of center */
  int da, db, a1, a2, d;

  gdk_gc_set_foreground(gc, color);	
  gdk_gc_set_line_attributes(gc, arc_width, 
                             GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

  /*
   * The function determines the angular increments that match the
   * <B>length</B> and <B>space</B> parameter. These are <B>db</B> and <B>da</B>.
   * Whenever one or both of these variables are null or negative, the
   * line is drawn solid. It means that it is not possible to distinguish
   * a dashed line from a solid line because of current value of zoom.
   */

  /* inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if(angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }
  da = length * 180 / (M_PI * radius);
  db = space  * 180 / (M_PI * radius);

  /* If da or db too small to be displayed, draw an arc */
  if((da <= 0) || (db <= 0)) {
    gdk_draw_arc(w, gc, FALSE,
                 x - radius, y - radius, 2* radius, 2 * radius,
                 angle1 * 64, angle2 * 64);
    return;
  }

  /*
   * Starting from <B>angle1</B> and incrementing the position gives the
   * coordinates of every dots and dashes on the arc providing that the
   * second extremity is not exceeded. 
   *
   * It draws as many sets of 'dash of length <B>length</B> and dot' as possible.
   */

  d = angle1;
  while((d + da + 2 * db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;
    gdk_draw_arc(w, gc, FALSE,
                 x - radius, y - radius, 2 * radius, 2 * radius,
                 a1 * 64, da * 64);

    d = d + db;
    xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);


    if(arc_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - arc_width/2, 
		   ((int) ya) - arc_width/2,
		   arc_width, arc_width, 0, FULL_CIRCLE);
    }

    /*
     * If the above condition is no more satisfied, it may still be possible
     * to continue drawing a part of the initial pattern. Here two cases are
     * possible :
     * <DL>
     *   <DT>*</DT><DD>it is possible to draw a dash and a dot.
     *   <DT>*</DT><DD>it is possible to draw a dash or at least
     *                 a part of the original dash.
     * </DL>
     */

    d = d + db;
  }


  if((d + da) < (angle1 + angle2)) {
    a1 = d;
    a2 = da;
		
    d = d + da;
  } else {
    a1 = d;
    a2 = (angle1 + angle2) - d;

    d = d + da;
  }
  gdk_draw_arc(w, gc, FALSE,
               x - radius, y - radius, 2 * radius, 2 * radius,
               a1 * 64, da * 64);

  if((d + db) < (angle1 + angle2)) {
    xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

    /*
     * Depending on the width of the line, dots has to be drawn in a
     * different manner : if the real worl width is equal to 0, then the
     * width is translated to 1 in screen coordinates to be visible.
     * Drawing a circle with a 1-diameter and the GDK function
     * #gdk_draw_arc() is not possible. So we needs to test whether the
     * width is 1 or not.
     */

    if(arc_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - arc_width/2, 
		   ((int) ya) - arc_width/2,
		   arc_width, arc_width, 0, FULL_CIRCLE);
    }


  }
	
}


/*! \brief Draw an arc with a phantom line type.
 *  \par Function Description
 *  This function draws an arc with a phantom line type. The parameter
 *  <B>space</B> represents the distance between a dot and a dash. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  The unit of <B>x</B>, <B>y</B> and <B>width</B>, <B>height</B> and
 *  <B>arc_width</B>, <B>space</B>, <B>length</B> is pixel.
 *
 *  <B>angle1</B> and <B>angle2</B> are in units of degrees.
 *
 *  A negative or null value for <B>length</B> and <B>space</B> leads to an
 *  endless loop.
 *
 *  \param [in] w          GdkWindow to draw in.
 *  \param [in] gc         GdkGC graphics context to draw on.
 *  \param [in] color      Arc line color.
 *  \param [in] cap        Arc line end cap type (unused).
 *  \param [in] x          Arc center x.
 *  \param [in] y          Arc center y.
 *  \param [in] radius     Arc radius.
 *  \param [in] angle1     Start angle in degrees.
 *  \param [in] angle2     End angle in degrees.
 *  \param [in] arc_width  Width of arc line.
 *  \param [in] length     Length of dash in pixels.
 *  \param [in] space      Spacing between dashes in pixels.
 */
void o_arc_draw_phantom(GdkWindow *w, GdkGC *gc,
			GdkColor *color, GdkCapStyle cap,
			gint x, gint y,
			gint radius,
			gint angle1, gint angle2,
			gint arc_width, gint length, gint space)
{
  double xa, ya;
  int da, db, a1, a2, d;

  gdk_gc_set_foreground(gc, color);	
  gdk_gc_set_line_attributes(gc, arc_width, 
                             GDK_LINE_SOLID, cap, GDK_JOIN_MITER);

  /* The function determines the angular increments that match the
   * <B>length</B> and <B>space</B> parameters. These are <B>db</B> and <B>da</B>.
   * Whenever one or both of these variables are null or negative, the
   * line is drawn solid. It means that it is not possible to distinguish
   * a dashed line from a solid line because of current value of zoom.
   */

  /* inverting angle2 if < 0 and changing angle1 accordingly */
  /* the loop test assume that da > 0 */
  if(angle2 < 0) {
    angle1 = angle1 + angle2;
    angle2 = -angle2;
  }
  da = length * 180 / (M_PI * radius);
  db = space  * 180 / (M_PI * radius);

  /* If da or db too small for arc to be displayed as dotted,
     draw a solid arc */
  if((da <= 0) || (db <= 0)) {
    gdk_draw_arc(w, gc, FALSE,
                 x - radius, y - radius, 2 * radius, 2 * radius,
                 angle1 * 64, angle2 * 64);
    return;
  }

  /*
   * Starting from <B>angle1</B> and incrementing the position gives the
   * coordinates of every dots and dashes on the arc providing that the
   * second extremity is not exceeded. 
   *
   * It draws as many sets of 'dash of length <B>length</B> and two dots'
   * as possible.
   */

  d = angle1;
  while((d + da + 3 * db) < (angle1 + angle2)) {
    a1 = d;
    d = d + da;
    gdk_draw_arc(w, gc, FALSE,
                 x - radius, y - radius, 2 * radius, 2 * radius,
                 a1 * 64, da * 64);

    d = d + db;
    xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

    /*
     * Depending on the width of the line, dots has to be drawn in a
     * different manner : if the real worl width is equal to 0, then the
     * width is translated to 1 in screen coordinates to be visible.
     * Drawing a circle with a 1-diameter and the GDK function
     * #gdk_draw_arc() is not possible. So we needs to test whether the
     * width is 1 or not.
     */

    if(arc_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - arc_width/2, 
		   ((int) ya) - arc_width/2,
		   arc_width, arc_width, 0, FULL_CIRCLE);
    }


    d = d + db;
    xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);

    
    /*
     * Depending on the width of the line, dots has to be drawn in a
     * different manner : if the real world width is equal to 0, then the
     * width is translated to 1 in screen coordinates to be visible. Drawing
     * a circle with a 1-diameter and the GDK function #gdk_draw_arc() is
     * not possible. So we needs to test whether the width is 1 or not.
     */

    if(arc_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
		   ((int) xa) - arc_width/2, 
		   ((int) ya) - arc_width/2,
		   arc_width, arc_width, 0, FULL_CIRCLE);
    }



    d = d + db;
  }

  /*
   * If the above condition is no more satisfied, it may still be possible
   * to continue drawing a part of the original pattern. Here three cases
   * are possible :
   * <DL>
   *   <DT>*</DT><DD>it is possible to draw a dash and the two dots.
   *   <DT>*</DT><DD>it is possible to draw a dash and one of the two dots.
   *   <DT>*</DT><DD>it is possible to draw at least a part of the initial
   *                 dash.
   */

  if((d + da) < (angle1 + angle2)) {
    a1 = d;
    a2 = da;
    d = d + da;
  } else {
    a1 = d;
    a2 = (angle1 + angle2) - d;
    d = d + da;
  }
  gdk_draw_arc(w, gc, FALSE,
               x - radius, y - radius, 2 * radius, 2 * radius,
               a1 * 64, a2 * 64);

  if((d + db) < (angle1 + angle2)) {
    d = d + db;

    xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);
		
    if(arc_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
                   ((int) xa) - arc_width/2, 
                   ((int) ya) - arc_width/2,
                   arc_width, arc_width, 0, FULL_CIRCLE);
    }
  }

  if((d + db) < (angle1 + angle2)) {
    d = d + db;

    xa = ((double) x) + ((double) radius) * cos(d * M_PI / 180);
    ya = ((double) y) - ((double) radius) * sin(d * M_PI / 180);
		
    if(arc_width == 1) {
      gdk_draw_point(w, gc, (int) xa, (int) ya);
    } else {
      gdk_draw_arc(w, gc, TRUE,
                   ((int) xa) - arc_width/2, 
                   ((int) ya) - arc_width/2,
                   arc_width, arc_width, 0, FULL_CIRCLE);
    }
  }
	
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_arc_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
  o_arc_rubberarc_xor(w_current);
}

/*! \brief Draw an arc described by OBJECT with translation
 *  \par Function Description
 *  This function draws the arc object described by <B>*o_current</B>
 *  translated by the vector (<B>dx</B>,<B>dy</B>) with an xor-function over
 *  the current sheet.
 *  The translation vector is in screen unit.
 *
 *  The arc is displayed with the color of the object.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for arc.
 *  \param [in] dy         Delta y coordinate for arc.
 *  \param [in] o_current  Arc OBJECT to draw.
 */
void o_arc_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int x, y, width, height, start_angle, end_angle;
  int color;

  if (o_current->arc == NULL) {
    return;
  }

  /* diameter */
  width       = SCREENabs( toplevel, o_current->arc->width );
  /* height MUST be equal to width, just another name for diameter */
  height      = SCREENabs( toplevel, o_current->arc->height );
  /* center */
  WORLDtoSCREEN(toplevel, o_current->arc->x + dx, o_current->arc->y + dy, &x, &y);
  x -= (width  / 2);
  y -= (height / 2);
  /* start and end angles */
  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;

  /* check the size of the displayed arc */
  /* do not allow null diameter = arc always displayed */
  if (height < 1) {
    height = 1;
  }
  if (width < 1) {
    width = 1;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  gdk_gc_set_foreground(w_current->outline_xor_gc,
			x_get_darkcolor(color));
  /* better to set the line attributes here ? */
  gdk_draw_arc(w_current->backingstore, w_current->outline_xor_gc, FALSE,
	       x, y, width, height,
	       start_angle * 64, end_angle * 64);

  /* backing store? not appropriate here  */

}

/*! \brief Start process to input a new arc.
 *  \par Function Description
 *  This function starts the process to input a new arc. Parameters for
 *  this arc are put into/extracted from the <B>w_current</B> toplevel structure.
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the pointer in screen unit.
 *
 *  First step of the arc input is to set the radius of the arc. The center
 *  of the arc is kept in (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>).
 *  The radius of the arc is in <B>w_current->distance</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_arc_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* set the center of the arc */
  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  /* set the radius */
  w_current->distance = 0;

  /* set the start and end angles */
  w_current->second_wx = w_current->second_wy = 0;

  /* start the rubberbanding process of the radius */
  o_arc_rubberarc_xor(w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of an arc.
 *  \par Function Description
 *  This function ends the input of the radius of the arc.
 *  The (<B>w_x</B>,<B>w_y</B>) point is taken as the other end of the radius segment.
 *  The distance between this point and the center is the radius of the arc.
 *  <B>w_x</B> and <B>w_y</B> are in world coords.
 *
 *  At the end of this function, the center of the arc is at
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and its radius is
 *  <B>w_current->distance</B>.
 *
 *  The two angles needs to be input to fully define the arc.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_arc_end1(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  g_assert( w_current->inside_action != 0 );

  /* erases the previous temporary radius segment */
  o_arc_rubberarc_xor(w_current);
  w_current->rubber_visible = 0;

  /* ack! zero length radius */
  if (w_current->distance == 0) {
    return;
  }

#if DEBUG
  printf("DIST: %d\n", w_current->distance);
#endif

  /* open a dialog to input the start and end angle */
  arc_angle_dialog(w_current, NULL);
}

/*! \brief Ends the process of arc input.
 *  \par Function Description
 *  The #o_arc_end4() function ends the process of the input of an arc.
 *  <B>start_angle</B> and <B>end_angle</B> are the start and end angle of the
 *  arc in degrees. The partial internal representation of the arc, i.e.
 *  the center and the radius of the arc, are converted in world units.
 *  A new object is created and linked to the object list.
 *
 *  \param [in] w_current    The GSCHEM_TOPLEVEL object.
 *  \param [in] radius       Radius of the arc
 *  \param [in] start_angle  Start of angle in degrees.
 *  \param [in] end_angle    End of angle in degrees.
 */
void o_arc_end4(GSCHEM_TOPLEVEL *w_current, int radius, 
		int start_angle, int end_angle)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;

  /* create, initialize and link the new arc object */
  new_obj = o_arc_new(toplevel, OBJ_ARC, w_current->graphic_color,
                      w_current->first_wx, w_current->first_wy,
                      radius, start_angle, end_angle);
  toplevel->page_current->object_tail =
    s_basic_link_object(new_obj, toplevel->page_current->object_tail);

  /* draw the new object */
  o_redraw_single(w_current, toplevel->page_current->object_tail);

  w_current->first_wx  = -1;
  w_current->first_wy  = -1;
  w_current->distance = 0;

  toplevel->page_current->CHANGED = 1;
  
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw an arc using one angle modification.
 *  \par Function Description
 *  This function draws an arc according to its internal representation
 *  and allows the modification of one of its angle. The start or end
 *  angle of the arc is updated according to <B>whichone</B> with the angle
 *  that the current pointer and the arc center are making with the horizontal.
 *
 *  The previous temporary arc is erased, the angle is then computed
 *  and updated and finally a new temporary arc with the new angle is drawn.
 *
 *  The arc is internally described by :
 *  <DL>
 *    <DT>*</DT><DD>(<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) as
 *                   its center.
 *    <DT>*</DT><DD><B>w_current->distance</B> as its radius.
 *    <DT>*</DT><DD><B>w_current->second_wx</B> and <B>w_current->second_wx</B> as its
 *                  start and end angle respectively.
 *  </DL>
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 *  \param [in] whichone   Which angle to change.
 *
 *  <B>whichone</B> can have one of the following values:
 *  <DL>
 *    <DT>ARC_RADIUS</DT>
 *    <DD>at the center of the arc. This grip is used to modify
 *        the radius of the arc.
 *    <DT>ARC_START_ANGLE</DT>
 *    <DD>at one end of the arc. It corresponds to the starting
 *        angle of the arc.
 *    <DT>ARC_END_ANGLE</DT>
 *    <DD>at the other end of the arc. It corresponds to the
 *        ending angle of the arc.
 *  </DL>
 */
void o_arc_rubberarc(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y, int whichone)
{
  int diff_x, diff_y, angle_deg;

  /* erase the previous temporary arc */
  if (w_current->rubber_visible)
    o_arc_rubberarc_xor(w_current);

  if(whichone == ARC_RADIUS) {
    /*
     * The radius is taken as the biggest distance on the x and y
     * axis between the center of the arc and the mouse position.
     */		
    diff_x = abs(w_current->first_wx - snap_grid(w_current->toplevel, w_x));
    diff_y = abs(w_current->first_wy - snap_grid(w_current->toplevel, w_y));
    w_current->distance = max(diff_x, diff_y);
  }
  else if((whichone == ARC_START_ANGLE) || (whichone == ARC_END_ANGLE)) {
    /* compute the angle */
    diff_x = w_current->first_wx - w_x;
    diff_y = w_current->first_wy - w_y;
    angle_deg = atan2(diff_y, diff_x) * 180 / M_PI;

    /* set the start or end angle with this angle */
    switch(whichone) {
    case ARC_START_ANGLE:
      w_current->second_wx = (angle_deg + 360 + 180) % 360;
      break;
	
    case ARC_END_ANGLE:
      w_current->second_wy = (angle_deg - w_current->second_wx + 720 + 180) % 360;
      break;
	
    default:
      return;
    }

  }
	
  /* draw the new temporary arc */
  o_arc_rubberarc_xor(w_current);
  w_current->rubber_visible = 1;
}

/*! \brief Draw arc from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws the arc from the variables in the GSCHEM_TOPLEVEL
 *  structure <B>*w_current</B>.
 *  The center of the arc is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>), its radius equal to <B>w_current->distance</B>,
 *  and the start and end angle are given by <B>w_current->second_wx</B> and
 *  <B>w_current->second_wy</B>.
 *
 *  The arc is drawn with a xor function over the current sheet with the
 *  selection color.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_arc_rubberarc_xor(GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;

  double rad_angle;
  int cx, cy, x1, y1, radius;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &cx, &cy);
  radius = SCREENabs(toplevel, w_current->distance);
  
  gdk_gc_set_foreground(w_current->xor_gc,
			x_get_darkcolor(w_current->select_color));
  gdk_gc_set_line_attributes(w_current->xor_gc, 0, 
			     GDK_LINE_SOLID, GDK_CAP_NOT_LAST, 
			     GDK_JOIN_MITER);

  /* draw the arc from the w_current variables */
  gdk_draw_arc(w_current->backingstore, w_current->xor_gc, FALSE,
	       cx - radius, cy - radius,
	       radius * 2, radius * 2,
	       w_current->second_wx * 64,
	       w_current->second_wy * 64);

  /* draw the radius segment from the w_current variables */
  rad_angle = ((double) w_current->second_wx) * M_PI / 180;
  x1 = cx + radius*cos(rad_angle);
  y1 = cy - radius*sin(rad_angle);
  gdk_draw_line(w_current->backingstore, w_current->xor_gc,
		cx, cy, x1, y1);

  /* FIXME: This isn't a tight bounding box for now, but the code
   *        to compute a better bounds it complex, and might wait
   *        until we're considered having real OBJECT data during
   *        rubberbanding and using world_get_arc_bounds().
   */
  o_invalidate_rect(w_current, 
		    cx - radius, cy - radius,
		    cx + radius, cy + radius);
}

/*! \brief Draw grip marks on arc.
 *  \par Function Description
 *  This function draws three grips on the center and on the ends of
 *  the arc object described by <B>*o_current</B>.
 *
 *  \param [in] w_current  The GSCHE_TOPLEVEL object.
 *  \param [in] o_current  Arc OBJECT to draw grip points on.
 */
void o_arc_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int radius, x, y, start_angle, end_angle;
  int x1, y1, x2, y2;

  if (w_current->draw_grips == FALSE)
    return;

  /*
   * An arc has three grips:
   * <DL>
   *   <DT>*</DT><DD>one at the center that allows changes on the
   *                 radius - at (<B>x</B>,<B>y</B>).
   *   <DT>*</DT><DD>one at the start of the arc - at (<B>x1</B>,<B>y1</B>).
   *   <DT>*</DT><DD>one at the end of the arc - at (<B>x2</B>,<B>y2</B>).
   */

  WORLDtoSCREEN( toplevel, o_current->arc->x, o_current->arc->y, &x, &y );
  radius      = SCREENabs( toplevel, o_current->arc->width / 2 );
  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;

  x1 = x + radius * cos(((double) start_angle) * M_PI / 180);
  y1 = y - radius * sin(((double) start_angle) * M_PI / 180);
  x2 = x + radius * cos(((double) (start_angle + end_angle)) * M_PI / 180);
  y2 = y - radius * sin(((double) (start_angle + end_angle)) * M_PI / 180);

  /* draw the grip at the center */
  o_grips_draw(w_current,  x,  y);

  /* draw the grip at the start_angle end of the arc */
  o_grips_draw(w_current, x1, y1);

  /* draw the grip at the end_angle end of the arc */
  o_grips_draw(w_current, x2, y2);

}

/*! \brief Erase grip marks from arc.
 *  \par Function Description
 *  This function erases the three grips displayed on the <B>o_current</B>
 *  arc object. These grips are on the center of the arc and on each end
 *  of the arc.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Arc OBJECT to remove grip marks from.
 */
void o_arc_erase_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int radius, x, y, start_angle, end_angle;
  int x1, y1, x2, y2;

  if (w_current->draw_grips == FALSE)
    return;

  /*
   * The coordinates of the three grips are determined by the parameters
   * of the arc. The grips are centered at (<B>x</B>,<B>y</B>), (<B>x1</B>,<B>y1</B>)
   * and (<B>x2</B>,<B>y2</B>).
   */

  WORLDtoSCREEN( toplevel, o_current->arc->x, o_current->arc->y, &x, &y );
  radius      = SCREENabs( toplevel, o_current->arc->width / 2 );
  start_angle = o_current->arc->start_angle;
  end_angle   = o_current->arc->end_angle;

  x1 = x + radius * cos(((double) start_angle) * M_PI / 180);
  y1 = y - radius * sin(((double) start_angle) * M_PI / 180);
  x2 = x + radius * cos(((double) start_angle + end_angle) * M_PI / 180);
  y2 = y - radius * sin(((double) start_angle + end_angle) * M_PI / 180);

  /* erase the grip at the center */
  o_grips_erase(w_current,  x,  y);

  /* erase the grip at the start_angle end of the arc */
  o_grips_erase(w_current, x1, y1);

  /* erase the grip at the end_angle end of the arc */
  o_grips_erase(w_current, x2, y2);

}
