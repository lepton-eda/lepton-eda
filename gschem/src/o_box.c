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
#include <math.h>
#include <stdio.h>

#include "gschem.h"

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

typedef void (*FILL_FUNC)( GdkDrawable *w, GdkGC *gc, COLOR *color,
                           GSCHEM_TOPLEVEL *w_current, BOX *box,
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
  int angle1, pitch1, angle2, pitch2;
  FILL_FUNC fill_func;

  if (o_current->box == NULL) {
    return;
  }

  if (toplevel->DONT_REDRAW == 1)
    return;

  /*
   * The drawing of the box is divided in two steps : first step is to
   * draw the outline, the second is to draw the filling pattern inside
   * (if any). Finally the function takes care of the grips.
   */
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

  angle1 = o_current->fill_angle1;
  pitch1 = o_current->fill_pitch1;
  angle2 = o_current->fill_angle2;
  pitch2 = o_current->fill_pitch2;
	
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

  (*fill_func) (w_current->drawable, w_current->gc,
                o_drawing_color (w_current, o_current),
                w_current, o_current->box,
                o_current->fill_width, angle1, pitch1, angle2, pitch2);

  gschem_cairo_set_source_color (w_current,
                                 o_drawing_color (w_current, o_current));
  gschem_cairo_box (w_current, o_current->line_width,
                    o_current->box->lower_x, o_current->box->lower_y,
                    o_current->box->upper_x, o_current->box->upper_y);

  if (o_current->fill_type == FILLING_FILL)
    cairo_fill_preserve (w_current->cr);

  gschem_cairo_stroke (w_current, o_current->line_type,
                                  o_current->line_end,
                                  o_current->line_width,
                                  o_current->line_length,
                                  o_current->line_space);

  if (o_current->selected && w_current->draw_grips) {
    o_box_draw_grips (w_current, o_current);
  }
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
 *  \param [in] w_current   Schematic top level
 *  \param [in] box         Box definition
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
void o_box_fill_hollow (GdkDrawable *w, GdkGC *gc, COLOR *color,
                        GSCHEM_TOPLEVEL *w_current, BOX *box,
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
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color.
 *  \param [in] w_current   Schematic top level
 *  \param [in] box         Box definition
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      (unused)
 *  \param [in] pitch1      (unused)
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
void o_box_fill_fill (GdkDrawable *w, GdkGC *gc, COLOR *color,
                      GSCHEM_TOPLEVEL *w_current, BOX *box,
                      gint fill_width,
                      gint angle1, gint pitch1,
                      gint angle2, gint pitch2)
{
  /* NOP: We'll fill it when we do the stroking */
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
 *  \param [in] w_current   Schematic top level
 *  \param [in] box         Box definition
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
void o_box_fill_hatch (GdkDrawable *w, GdkGC *gc, COLOR *color,
                       GSCHEM_TOPLEVEL *w_current, BOX *box,
                       gint fill_width,
                       gint angle1, gint pitch1,
                       gint angle2, gint pitch2)
{
  int i;
  GArray *lines;

  gschem_cairo_set_source_color (w_current, color);

  lines = g_array_new (FALSE, FALSE, sizeof (LINE));
  m_hatch_box (box, angle1, pitch1, lines);

  for (i=0; i < lines->len; i++) {
    LINE *line = &g_array_index (lines, LINE, i);

    gschem_cairo_line (w_current, END_NONE, fill_width, line->x[0], line->y[0],
                                                        line->x[1], line->y[1]);
  }
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, fill_width, -1, -1);

  g_array_free (lines, TRUE);
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
 *  \param [in] w_current   Schematic top level
 *  \param [in] box         Box definition
 *  \param [in] fill_width  BOX pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
void o_box_fill_mesh (GdkDrawable *w, GdkGC *gc, COLOR *color,
                      GSCHEM_TOPLEVEL *w_current, BOX *box,
                      gint fill_width,
                      gint angle1, gint pitch1,
                      gint angle2, gint pitch2)
{
  o_box_fill_hatch (w, gc, color, w_current, box,
                    fill_width, angle1, pitch1, -1, -1);
  o_box_fill_hatch (w, gc, color, w_current, box,
                    fill_width, angle2, pitch2, -1, -1);
}


/*! \todo Finish function documentation!!!
 *  \brief 
 *  \par Function Description
 *
 */
void o_box_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  int x1, y1, x2, y2;

  WORLDtoSCREEN (w_current, w_current->first_wx, w_current->first_wy, &x1, &y1);
  WORLDtoSCREEN (w_current, w_current->second_wx, w_current->second_wy, &x2, &y2);

  o_invalidate_rect (w_current, x1, y1, x2, y1);
  o_invalidate_rect (w_current, x1, y1, x1, y2);
  o_invalidate_rect (w_current, x2, y1, x2, y2);
  o_invalidate_rect (w_current, x1, y2, x2, y2);
}

/*! \brief Draw a box described by OBJECT with translation
 *  \par Function Description
 *  This function daws the box object described by <B>*o_current</B> translated
 *  by the vector (<B>dx</B>,<B>dy</B>).
 *  The translation vector is in world unit.
 *
 *  The box is displayed with the color of the object.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for box.
 *  \param [in] dy         Delta y coordinate for box.
 *  \param [in] o_current  Box OBJECT to draw.
 */
void o_box_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  if (o_current->box == NULL) {
    return;
  }

  gschem_cairo_box (w_current, 0, o_current->box->upper_x + dx,
                                  o_current->box->upper_y + dy,
                                  o_current->box->lower_x + dx,
                                  o_current->box->lower_y + dy);
  gschem_cairo_set_source_color (w_current,
                                 x_color_lookup_dark (o_current->color));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 1, -1, -1);
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
  o_box_invalidate_rubber (w_current);
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
  OBJECT *new_obj;
  int box_width, box_height;
  int box_left, box_top;

  g_assert( w_current->inside_action != 0 );

  /* get the last coords of the pointer */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* erase the temporary box */
  /* o_box_invalidate_rubber (w_current); */
  w_current->rubber_visible = 0;
  
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
  new_obj = o_box_new (toplevel, OBJ_BOX, GRAPHIC_COLOR,
                       box_left, box_top,
                       box_left + box_width, box_top - box_height);
  s_page_append (toplevel, toplevel->page_current, new_obj);

  /* draw it */
  o_invalidate (w_current, new_obj);

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
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_box_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{

  g_assert( w_current->inside_action != 0 );

  /* erase the previous temporary box if it is visible */
  if (w_current->rubber_visible)
    o_box_invalidate_rubber (w_current);

  /*
   * New values are fixed according to the <B>w_x</B> and <B>w_y</B> parameters.
   * These are saved in <B>w_current</B> pointed structure as new temporary
   * values. The new box is then drawn.
   */

  /* update the coords of the corner */
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  /* draw the new temporary box */
  o_box_invalidate_rubber (w_current);
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
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_box_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  gschem_cairo_box (w_current, 0, w_current->first_wx,  w_current->first_wy,
                                  w_current->second_wx, w_current->second_wy);
  gschem_cairo_set_source_color (w_current,
                                 x_color_lookup_dark (SELECT_COLOR));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);
}

/*! \brief Draw grip marks on box.
 *  \par Function Description
 *  This function draws four grips on the corners of the box described
 *  by <B>*o_current</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Box OBJECT to draw grip points on.
 */
void o_box_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  if (w_current->draw_grips == FALSE)
    return;

  /* grip on upper left corner (whichone = BOX_UPPER_LEFT) */
  o_grips_draw (w_current, o_current->box->upper_x, o_current->box->upper_y);

  /* grip on upper right corner (whichone = BOX_UPPER_RIGHT) */
  o_grips_draw (w_current, o_current->box->lower_x, o_current->box->upper_y);

  /* grip on lower left corner (whichone = BOX_LOWER_LEFT) */
  o_grips_draw (w_current, o_current->box->upper_x, o_current->box->lower_y);

  /* grip on lower right corner (whichone = BOX_LOWER_RIGHT) */
  o_grips_draw (w_current, o_current->box->lower_x, o_current->box->lower_y);
}
