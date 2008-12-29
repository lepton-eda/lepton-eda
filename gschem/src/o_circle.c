/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2008 Ales Hvezda
 * Copyright (C) 1998-2008 gEDA Contributors (see ChangeLog for details)
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


typedef void (*FILL_FUNC)( GdkDrawable *w, GdkGC *gc, COLOR *color,
                           GSCHEM_TOPLEVEL *w_current, CIRCLE *circle,
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
  int line_width, length, space;
  int fill_width, angle1, pitch1, angle2, pitch2;
  COLOR *color;
  FILL_FUNC fill_func;

  if (o_current->circle == NULL) {
    return;
  }

  /*
   * Get read to check for visibility of this line by using it's
   * bounding box
   */
  world_get_single_object_bounds(toplevel, o_current,
                                 &wleft, &wtop, &wright, &wbottom);
	
  if (toplevel->DONT_REDRAW == 1)
    return;
	
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
    color = x_color_lookup (toplevel->override_color);
  } else {
    color = x_color_lookup (o_current->color);
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
  line_width = SCREENabs( toplevel, o_current->line_width );
  if (line_width <= 0) {
    line_width = 1;
  }

  length = SCREENabs( toplevel, o_current->line_length );
  space = SCREENabs( toplevel, o_current->line_space );

  WORLDtoSCREEN( toplevel, o_current->circle->center_x, o_current->circle->center_y,
                 &s_x, &s_y );

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
  pitch1 = o_current->fill_pitch1;
  angle2 = o_current->fill_angle2;
  pitch2 = o_current->fill_pitch2;
	
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

  (*fill_func) (w_current->drawable, w_current->gc,
                color, w_current, o_current->circle,
                fill_width, angle1, pitch1, angle2, pitch2);

  gschem_cairo_set_source_color (w_current->cr, color);
  cairo_new_sub_path (w_current->cr);
  cairo_arc (w_current->cr, s_x + 0.5, s_y + 0.5, radius, 0., 2 * M_PI);

  if (o_current->fill_type == FILLING_FILL)
    cairo_fill_preserve (w_current->cr);

  gschem_cairo_stroke (w_current->cr, o_current->line_type,
                       o_current->line_end, line_width, length, space);

#if DEBUG
  printf("drawing circle\n");
#endif

  if (o_current->selected && w_current->draw_grips) {
    o_circle_draw_grips (w_current, o_current);
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
void o_circle_fill_hollow (GdkDrawable *w, GdkGC *gc, COLOR *color,
                           GSCHEM_TOPLEVEL *w_current, CIRCLE *circle,
                           gint fill_width,
                           gint angle1, gint pitch1,
                           gint angle2, gint pitch2)
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
void o_circle_fill_fill (GdkDrawable *w, GdkGC *gc, COLOR *color,
                         GSCHEM_TOPLEVEL *w_current, CIRCLE *circle,
                         gint fill_width,
                         gint angle1, gint pitch1,
                         gint angle2, gint pitch2)
{
  /* NOP: We'll fill it when we do the stroking */
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
void o_circle_fill_hatch (GdkDrawable *w, GdkGC *gc, COLOR *color,
                          GSCHEM_TOPLEVEL *w_current, CIRCLE *circle,
                          gint fill_width,
                          gint angle1, gint pitch1,
                          gint angle2, gint pitch2)
{
  int i;
  GArray *lines;

  gschem_cairo_set_source_color (w_current->cr, color);

  lines = g_array_new (FALSE, FALSE, sizeof (LINE));
  m_hatch_circle (circle, angle1, pitch1, lines);

  for (i=0; i < lines->len; i++) {
    int x1, y1, x2, y2;
    LINE *line = &g_array_index (lines, LINE, i);

    WORLDtoSCREEN (w_current->toplevel, line->x[0], line->y[0], &x1, &y1);
    WORLDtoSCREEN (w_current->toplevel, line->x[1], line->y[1], &x2, &y2);
    gschem_cairo_line (w_current->cr, END_NONE, fill_width, x1, y1, x2, y2);
    gschem_cairo_stroke (w_current->cr, TYPE_SOLID, END_NONE,
                                        fill_width, -1, -1);

  }

  g_array_free (lines, TRUE);
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
void o_circle_fill_mesh (GdkDrawable *w, GdkGC *gc, COLOR *color,
                         GSCHEM_TOPLEVEL *w_current, CIRCLE *circle,
                         gint fill_width,
                         gint angle1, gint pitch1,
                         gint angle2, gint pitch2)
{
  o_circle_fill_hatch (w, gc, color, w_current, circle,
                       fill_width, angle1, pitch1, -1, -1);
  o_circle_fill_hatch (w, gc, color, w_current, circle,
                       fill_width, angle2, pitch2, -1, -1);
	
}


/*! \todo Finish function documentation!!!
 *  \brief
 *  \par Function Description
 *
 */
void o_circle_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int cx, cy, radius;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &cx, &cy);
  radius = SCREENabs(toplevel, w_current->distance);

  o_invalidate_rect (w_current, cx - radius, cy - radius,
                                cx + radius, cy + radius);
}

/*! \brief Draw a circle described by OBJECT with translation
 *  \par Function Description
 *  This function draws the circle object described by <B>*o_current</B>
 *  translated by the vector (<B>dx</B>,<B>dy</B>).
 *  The translation vector is in world unit.
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
void o_circle_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
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
                 o_current->circle->center_x - o_current->circle->radius + dx,
                 o_current->circle->center_y + o_current->circle->radius + dy,
                 &x, &y );

  /* To draw be sure to setup width height */
  gdk_gc_set_foreground (w_current->gc, x_get_darkcolor (color));
  gdk_draw_arc (w_current->drawable, w_current->gc,
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
 *  <B>w_x</B> and <B>w_y</B> are current coordinates of the mouse pointer in
 *  world units.
 *
 *  The first step of the circle input is to set the center of the arc.
 *  This center is kept in (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>). 
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_circle_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* center of circle */
  w_current->first_wx = w_x;
  w_current->first_wy = w_y;

  /* radius */
  w_current->distance = 0;

  /* first temporary circle */
  o_circle_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}

/*! \brief End the input of a circle.
 *  \par Function Description
 *  This function ends the input of the radius of the circle.
 *  The (<B>w_x</B>,<B>w_y</B>) point is taken as the other end of the radius
 *  segment, i.e. on the circle. The distance between this point and the
 *  center is the radius of the circle.
 *  <B>w_x</B> and <B>w_y</B> are in world coords.
 *
 *  The center has previously been input and saved as
 *  (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>).
 *
 *  The temporary circle drawn during the input of the radius is erased.
 *  A new object is allocated, initialized and linked in the object list.
 *  This new object is finally drawn.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_circle_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  OBJECT *new_obj;

  g_assert( w_current->inside_action != 0 );

  /* erase the temporary circle */
  /* o_circle_invalidate_rubber (w_current); */
  w_current->rubber_visible = 0;
  
  /* circle with null radius are not allowed */
  if (w_current->distance == 0) {
    /* cancel the object creation */
    return;
  }

  /* create the object */
  new_obj = o_circle_new (toplevel, OBJ_CIRCLE, GRAPHIC_COLOR,
                          w_current->first_wx, w_current->first_wy,
                          w_current->distance);
  s_page_append (toplevel->page_current, new_obj);

  /* draw it */
  o_invalidate (w_current, new_obj);

  toplevel->page_current->CHANGED = 1;
  o_undo_savestate(w_current, UNDO_ALL);
}

/*! \brief Draw temporary circle while dragging edge.
 *  \par Function Description
 *  This function draws a circle according to its internal representation and
 *  allows the modification of its radius. The radius is updated according to
 *  the current mouse position in <B>w_x</B> and <B>w_y</B>.
 *  It draws a full circle and the horizontal segment of the radius in the
 *  right half of the circle.
 *
 *  The previous temporary circle is erased, the radius is then computed and
 *  updated and finally a new temporary circle is drawn.
 *
 *  The arc is internally described by :
 *  <DL>
 *    <DT>*</DT><DD>(<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) as its
 *                   center ;
 *    <DT>*</DT><DD><B>w_current->distance</B> as its radius.
 *  </DL>
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_circle_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  int diff_x, diff_y;

  g_assert( w_current->inside_action != 0 );

  /* erase the previous temporary circle if it is visible */
  if (w_current->rubber_visible)
    o_circle_invalidate_rubber (w_current);

  /*
   * The radius is taken as the biggest distance on the x and y axis between
   * the center of the circle and the mouse position.
   */
  diff_x = abs(w_current->first_wx - w_x);
  diff_y = abs(w_current->first_wy - w_y);
  w_current->distance = max(diff_x, diff_y);

  /* draw the new temporary circle */
  o_circle_invalidate_rubber (w_current);
  w_current->rubber_visible =1;
}

/*! \brief Draw circle from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws the circle from the variables in the GSCHEM_TOPLEVEL
 *  structure <B>*w_current</B>.
 *  The center of the circle is at (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and its radius is in <B>w_current->distance</B>.
 *
 *  It draws a horizontal radius segment on the right half of the circle and
 *  the circle with the selection color.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_circle_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int cx, cy, radius;

  WORLDtoSCREEN(toplevel, w_current->first_wx, w_current->first_wy, &cx, &cy);
  radius = SCREENabs(toplevel, w_current->distance);

  /* draw the circle from the w_current variables */
  gdk_gc_set_foreground (w_current->gc, x_get_darkcolor (SELECT_COLOR));
  gdk_draw_line (w_current->drawable, w_current->gc,
                 cx, cy, cx + radius, cy);
  gdk_draw_arc (w_current->drawable, w_current->gc, FALSE,
                cx - radius, cy - radius, 2 * radius, 2* radius,
                0, FULL_CIRCLE);
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
