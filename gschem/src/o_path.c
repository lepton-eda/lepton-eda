/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
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

#define NUM_BEZIER_SEGMENTS 100


typedef void (*DRAW_FUNC) (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                           GSCHEM_TOPLEVEL *w_current, PATH *path,
                           GdkCapStyle cap,
                           gint line_width, gint length, gint space);

typedef void (*FILL_FUNC) (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                           GSCHEM_TOPLEVEL *w_currentm, PATH *path,
                           gint fill_width,
                           gint angle1, gint pitch1, gint angle2, gint pitch2);


static void path_to_points_modify (GSCHEM_TOPLEVEL *w_current, PATH *path,
                                   int dx, int dy, int new_x, int new_y, int whichone,
                                   GdkPoint **points, int *num_points)

{
  TOPLEVEL *toplevel = w_current->toplevel;
  PATH_SECTION *section;
  int x1, y1, x2, y2, x3, y3;
  int i;
  int grip_no = 0;

  sPOINT point = { 0, 0 };
  GArray *polygon_points;
  BEZIER bezier;

  polygon_points = g_array_new (FALSE, FALSE, sizeof (sPOINT));


  for (i = 0; i <  path->num_sections; i++) {
    section = &path->sections[i];

    x1 = section->x1 + dx; y1 = section->y1 + dy;
    x2 = section->x2 + dx; y2 = section->y2 + dy;
    x3 = section->x3 + dx; y3 = section->y3 + dy;

    switch (section->code) {
      case PATH_CURVETO:
        /* Two control point grips */
        if (whichone == grip_no++) {
          x1 = new_x; y1 = new_y;
        }
        if (whichone == grip_no++) {
          x2 = new_x; y2 = new_y;
        }
        WORLDtoSCREEN (toplevel, x1, y1, &x1, &y1);
        WORLDtoSCREEN (toplevel, x2, y2, &x2, &y2);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        if (whichone == grip_no++) {
          x3 = new_x; y3 = new_y;
        }
        WORLDtoSCREEN (toplevel, x3, y3, &x3, &y3);
      case PATH_END:
        break;
    }

    switch (section->code) {
      case PATH_CURVETO:
        bezier.x[0] = point.x;
        bezier.y[0] = point.y;
        bezier.x[1] = x1;
        bezier.y[1] = y1;
        bezier.x[2] = x2;
        bezier.y[2] = y2;
        point.x = bezier.x[3] = x3;
        point.y = bezier.y[3] = y3;
        m_polygon_append_bezier (polygon_points, &bezier, NUM_BEZIER_SEGMENTS);
        break;

      case PATH_MOVETO_OPEN:
        /* Unsupported, just fall through and draw a line */
        /* Fall through */

      case PATH_MOVETO:
      case PATH_LINETO:
        point.x = x3;
        point.y = y3;
        m_polygon_append_point (polygon_points, point.x, point.y);
        break;

      case PATH_END:
        break;
    }
  }

  /* WARNING:
   * Relies on the fact that sPOINT and GdkPoint are compatible */

  *num_points = polygon_points->len;
  *points = (GdkPoint *)g_array_free (polygon_points, FALSE);
}


static void path_to_points (GSCHEM_TOPLEVEL *w_current, PATH *path,
                            int dx, int dy,
                            GdkPoint **points, int *num_points)
{
  path_to_points_modify (w_current, path,
                         dx, dy, 0, 0, -1,
                         points, num_points);
}


/*! \brief Draw a path with a solid line type.
 *  \par Function Description
 *  This function draws a path with a solid line type. The length and space
 *  parameters are not used by this function.
 *
 *  \param [in] w           GdkDrawable to draw in
 *  \param [in] gc          GdkGC graphics context to draw on
 *  \param [in] color       Box line color
 *  \param [in] cap         Box line end cap type (unused)
 *  \param [in] path        The PATH object to draw
 *  \param [in] line_width  Width of line to draw path
 *  \param [in] length      (unused)
 *  \param [in] space       (unused)
 */
void o_path_draw_solid(GdkDrawable *w, GdkGC *gc, GdkColor *color,
                      GSCHEM_TOPLEVEL *w_current, PATH *path,
                      GdkCapStyle cap, gint line_width,
                      gint length, gint space)
{
  GdkPoint *points;
  int num_points;

  path_to_points (w_current, path, 0, 0, &points, &num_points);

  if (num_points == 0) {
    g_free (points);
    return;
  }


  gdk_gc_set_foreground(gc, color);

  /* Set the width, end type and join style of the line */
  gdk_gc_set_line_attributes(gc, line_width, GDK_LINE_SOLID,
                             cap, GDK_JOIN_MITER);

  if (path->sections[path->num_sections - 1].code == PATH_END) {
    /* Closed path */
    gdk_draw_polygon (w_current->drawable, w_current->gc,
                      FALSE, points, num_points);
  } else {
    /* Open path */
    gdk_draw_lines (w_current->drawable, w_current->gc,
                    points, num_points);
  }

  g_free (points);
}

/*! \brief Draw a path with a dotted line type.
 *  \par Function Description
 *  This function draws a path with a dotted line type. The parameter
 *  <B>space</B> represents the distance between two of the dots. The
 *  parameter <B>length</B> is unused. The diameter of the dots is given by
 *  the width of the line given by <B>width</B>.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] path        The PATH object to draw
 *  \param [in] line_width  Width of line to draw path.
 *  \param [in] length      (unused)
 *  \param [in] space       Space in pixels between dots.
 */

static void o_path_draw_dotted (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                                GSCHEM_TOPLEVEL *w_current, PATH *path,
                                GdkCapStyle cap, gint line_width,
                                gint length, gint space)
{
  o_path_draw_solid (w, gc, color, w_current, path, cap,
                     line_width, length, space);
}

/*! \brief Draw a path with a dashed line type.
 *  \par Function Description
 *  This function draws a path with a dashed line type. The parameter
 *  <B>space</B> represents the distance between two of the dash. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] path        The PATH object to draw
 *  \param [in] line_width  Width of line to draw path.
 *  \param [in] length      Length of dash in pixels.
 *  \param [in] space       Space between dashes in pixels.
 */
static void o_path_draw_dashed (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                                GSCHEM_TOPLEVEL *w_current, PATH *path,
                                GdkCapStyle cap, gint line_width,
                                gint length, gint space)
{
  o_path_draw_solid (w, gc, color, w_current, path, cap,
                     line_width, length, space);
}

/*! \brief Draw a path with a centered line type.
 *  \par Function Description
 *  This function draws a path with a centered line type. The parameter
 *  <B>space</B> represents the distance between a dot and the dash. The
 *  parameter <B>length</B> represents the length of a dash.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] path        The PATH object to draw
 *  \param [in] line_width  Width of line to draw path.
 *  \param [in] length      (unused)?
 *  \param [in] space       (unused)?
 */
static void o_path_draw_center (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                                GSCHEM_TOPLEVEL *w_current, PATH *path,
                                GdkCapStyle cap, gint line_width,
                                gint length, gint space)
{
  o_path_draw_solid (w, gc, color, w_current, path, cap,
                     line_width, length, space);
}

/*! \brief Draw a path with a phantom line type.
 *  \par Function Description
 *  This function draws a path with a phantom line type. The parameter
 *  <B>space</B> represents the distance between a dot and a dash.
 *  The parameter <B>length</B> represents the length of a dash.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box line color.
 *  \param [in] cap         Box line end cap type (unused).
 *  \param [in] path        The PATH object to draw
 *  \param [in] line_width  Width of line to draw path.
 *  \param [in] length      (unused)?
 *  \param [in] space       (unused)?
 */
static void o_path_draw_phantom (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                                 GSCHEM_TOPLEVEL *w_current, PATH *path,
                                 GdkCapStyle cap, gint line_width,
                                 gint length, gint space)
{
  o_path_draw_solid (w, gc, color, w_current, path, cap,
                     line_width, length, space);
}

/*! \brief Placeholder filling function.
 *  \par Function Description
 *  This function does nothing. It has the same prototype as all the
 *  filling functions. It prevent from making a difference between filling
 *  in function #o_path_draw().
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color.
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
static void o_path_fill_hollow (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                                GSCHEM_TOPLEVEL *w_current, PATH *path,
                                gint fill_width,
                                gint angle1, gint pitch1,
                                gint angle2, gint pitch2)
{
  /* NOP */
}

/*! \brief Fill inside of path with a solid pattern.
 *  \par Function Description
 *  This function fills the inside of the path with a solid pattern.
 *  Parameters <B>angle1</B>, <B>pitch1</B> and <B>angle2</B>,
 *  <B>pitch2</B> and <B>fill_width</B> are unused here but kept for compatibility
 *  with other path filling functions.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color.
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      (unused)
 *  \param [in] pitch1      (unused)
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
static void o_path_fill_fill (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                              GSCHEM_TOPLEVEL *w_current, PATH *path,
                              gint fill_width,
                              gint angle1, gint pitch1,
                              gint angle2, gint pitch2)
{
  GdkPoint *points;
  int num_points;

  gdk_gc_set_foreground(gc, color);
  gdk_gc_set_line_attributes(gc, 1, GDK_LINE_SOLID,
                             GDK_CAP_BUTT, GDK_JOIN_MITER);
  path_to_points (w_current, path, 0, 0, &points, &num_points);

  if (num_points == 0) {
    g_free (points);
    return;
  }

  gdk_draw_polygon (w_current->drawable, w_current->gc,
                    TRUE, points, num_points);

  g_free (points);
}

/*! \brief Fill inside of path with single line pattern.
 *  \par Function Description
 *  This function fills the inside of the path with a pattern made of lines.
 *  The lines are drawn inside the path with an angle <B>angle1</B> from the
 *  horizontal. The distance between two of these lines is given by
 *  <B>pitch1</B> and their width by <B>fill_width</B>.
 *  Parameters <B>angle2</B> and <B>pitch2</B> are unused here but kept for
 *  compatbility with other path filling functions.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color.
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
static void o_path_fill_hatch (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                               GSCHEM_TOPLEVEL *w_current, PATH *path,
                               gint fill_width,
                               gint angle1, gint pitch1,
                               gint angle2, gint pitch2)
{
  int i;
  GArray *lines;

  lines = g_array_new (FALSE, FALSE, sizeof (LINE));

  m_hatch_path (path, angle1, pitch1, lines);

  for (i=0; i < lines->len; i++) {
    int x1, y1, x2, y2;
    LINE *line = &g_array_index (lines, LINE, i);

    WORLDtoSCREEN (w_current->toplevel, line->x[0], line->y[0], &x1, &y1);
    WORLDtoSCREEN (w_current->toplevel, line->x[1], line->y[1], &x2, &y2);
    o_line_draw_solid (w, gc, color, GDK_CAP_BUTT,
                       x1, y1, x2, y2, fill_width, -1, -1);
  }

  g_array_free (lines, TRUE);
}


/*! \brief Fill inside of path with mesh pattern.
 *  \par Function Description
 *  This function fills the inside of the path with a pattern made of two
 *  sets of parallel lines in two directions. The first set is drawn inside
 *  the path with an angle <B>angle1</B> from the horizontal. The distance
 *  between two of these lines is given by <B>pitch1</B>.
 *  The second set is drawn inside the path with an angle <B>angle2</B> from
 *  the horizontal. The distance between two of these lines is given
 *  by <B>pitch2</B>.
 *
 *  \param [in] w           GdkDrawable to draw in.
 *  \param [in] gc          GdkGC graphics context to draw on.
 *  \param [in] color       Box fill color.
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
static void o_path_fill_mesh (GdkDrawable *w, GdkGC *gc, GdkColor *color,
                              GSCHEM_TOPLEVEL *w_current, PATH *path,
                              gint fill_width,
                              gint angle1, gint pitch1,
                              gint angle2, gint pitch2)
{
  o_path_fill_hatch (w, gc, color, w_current, path,
                     fill_width, angle1, pitch1, -1, -1);
  o_path_fill_hatch (w, gc, color, w_current, path,
                     fill_width, angle2, pitch2, -1, -1);
}

/*! \brief Draw a path on screen.
 *  \par Function Description
 *  This function is used to draw a path on screen. The path is described
 *  in the object which is referred by <B>o_current</B>. The path is displayed
 *  according to the current state, described in the GSCHEM_TOPLEVEL object pointed
 *  by <B>w_current</B>.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  The path OBJECT to draw.
 */
void o_path_draw(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  PATH *path = o_current->path;
  int wleft, wtop, wright, wbottom;
  int line_width, length, space;
  int fill_width, angle1, pitch1, angle2, pitch2;
  DRAW_FUNC draw_func = NULL;
  FILL_FUNC fill_func;

  GdkColor *color;
  GdkCapStyle path_end;

  if (path == NULL) {
    return;
  }

  /* Get read to check for visibility of this line by using it's
   * bounding box */
  world_get_single_object_bounds(toplevel, o_current,
                                 &wleft, &wtop, &wright, &wbottom);

  if ( (toplevel->DONT_REDRAW == 1) ||
       (!visible(toplevel, wleft, wtop, wright, wbottom)) ) {
    return;
  }

  if (toplevel->override_color != -1 )
    color = x_get_color(toplevel->override_color);
  else
    color = x_get_color(o_current->color);

  line_width = SCREENabs( toplevel, o_current->line_width );
  if( line_width <= 0) {
    line_width = 1;
  }

  switch(o_current->line_end) {
    case END_NONE:   path_end = GDK_CAP_BUTT;       break;
    case END_SQUARE: path_end = GDK_CAP_PROJECTING; break;
    case END_ROUND:  path_end = GDK_CAP_ROUND;      break;
    default:
      fprintf(stderr, _("Unknown end for path (%d)\n"),
                      o_current->line_end);
      path_end = GDK_CAP_BUTT;
    break;
  }

  length = SCREENabs( toplevel, o_current->line_length );
  space = SCREENabs( toplevel, o_current->line_space );

  switch(o_current->line_type) {
    case TYPE_SOLID:
      length = -1;
      space = -1;
      draw_func = o_path_draw_solid;
      break;

    case TYPE_DOTTED:
      length = -1; /* ..._draw_dotted only space is used */
      draw_func = o_path_draw_dotted;
      break;

    case TYPE_DASHED:
      draw_func = o_path_draw_dashed;
      break;

    case TYPE_CENTER:
      draw_func = o_path_draw_center;
      break;

    case TYPE_PHANTOM:
      draw_func = o_path_draw_phantom;
      break;

    case TYPE_ERASE:
      break;

    default:
      length = -1;
      space = -1;
      line_width = 0; /* just to be careful */
      draw_func = o_path_draw_solid;
      fprintf(stderr, _("Unknown type for path !\n"));
      break;
  }

  if((length == 0) || (space == 0))
  draw_func = o_path_draw_solid;

  (*draw_func) (w_current->drawable, w_current->gc, color, w_current,
                o_current->path, path_end, line_width, length, space);


  /*
   * The values needed for the fill operation are taken from the
   * <B>o_current</B> pointed OBJECT. It include the type of fill required,
   * the width of the lines (if the fill use line) and angles and pitchs
   * for hatch based filling.
   *
   * Once again the width of the line is important as if it is equal to
   * 0 it may not be displayed. That is definetely not what we are looking for.
   *
   * Depending on the type of fill that has to be used inside the path the
   * appropriate function is called. Values of <B>angle1</B>,
   * <B>angle2</B>, <B>pitch1</B> and <B>pitch2</B> are adapted to the type of
   * filling. The possible functions are the following :
   * #o_path_fill_hollow(), #o_path_fill_fill(), #o_path_fill_mesh() and
   * #o_path_fill_hatch().
   */
  fill_width = SCREENabs( toplevel, o_current->fill_width );
  if(fill_width <= 0) {
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
      /* this function is empty ! however if it do not use it we have to add
       * a test before the call. Simply putting a return here instead is not
       * possible as it would prevent any hollow path from having its grips
       * drawn
       */
      fill_func = o_path_fill_hollow;
      break;

    case FILLING_FILL:
      angle1 = -1; angle2 = -1;
      pitch1 = 1; pitch2 = 1;
      fill_func = o_path_fill_fill;
      break;

    case FILLING_MESH:
      fill_func = o_path_fill_mesh;
      break;

    case FILLING_HATCH:
      angle2 = -1;
      pitch2 = 1;
      fill_func = o_path_fill_hatch;
      break;

    case FILLING_VOID:
    default:
      angle1 = -1; angle2 = -1;
      pitch1 = 1; pitch2 = 1;
      fill_func = o_path_fill_hollow;
      fprintf(stderr, _("Unknown type for path (fill)!\n"));
  }

  if((pitch1 <= 0) || (pitch2 <= 0)) {
    fill_func = o_path_fill_fill;
  }

  (*fill_func) (w_current->drawable, w_current->gc, color,
                w_current, path, fill_width, angle1, pitch1, angle2, pitch2);

  if (o_current->draw_grips && w_current->draw_grips == TRUE) {
    if (!o_current->selected) {
      o_current->draw_grips = FALSE;
    } else {
      /* object is selected, draw the grips */
      o_path_draw_grips(w_current, o_current);
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
void o_path_eraserubber(GSCHEM_TOPLEVEL *w_current)
{
  o_path_rubberpath_xor (w_current);
}


/*! \brief Draw a path object after applying translation.
 *  \par Function Description
 *  This function is used to draw the path object described by
 *  <B>*o_current</B> after applying a translation on the two directions of
 *  <B>dx</B> and <B>dy</B> in world units. It uses and XOR function to draw the
 *  translated path over the current sheet.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for path.
 *  \param [in] dy         Delta y coordinate for path.
 *  \param [in] o_current  Line OBJECT to draw.
 */
void o_path_draw_xor(GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  PATH *path = o_current->path;
  int color;
  int num_points;
  GdkPoint *points;

  path_to_points (w_current, path, dx, dy, &points, &num_points);

  if (num_points == 0) {
    g_free (points);
    return;
  }

  if (o_current->saved_color != -1) {
    color = o_current->saved_color;
  } else {
    color = o_current->color;
  }

  gdk_gc_set_foreground(w_current->outline_xor_gc,
                        x_get_darkcolor(color));
  gdk_gc_set_line_attributes(w_current->xor_gc, 0, GDK_LINE_SOLID,
                             GDK_CAP_NOT_LAST, GDK_JOIN_MITER);

  /* Stroke only, no fill for XOR */
  if (path->sections[path->num_sections - 1].code == PATH_END)
    gdk_draw_polygon (w_current->drawable, w_current->xor_gc,
                      FALSE, points, num_points);
  else
    gdk_draw_lines (w_current->drawable, w_current->xor_gc,
                    points, num_points);

  g_free (points);
}

/*! \brief Start process to input a new path.
 *  \par Function Description
 *  This function starts the process of interactively adding a path to
 *  the current sheet.
 *
 *  During all the process, the path is internally represented by the two
 *  ends of the path as (<B>w_current->first_wx</B>,<B>w_current->first_wy</B>) and
 *  (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  A temporary path is xor-drawn during the process with the selection color
 *  and changed according to the position of the mouse pointer.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_path_start(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* TODO: Implement support for drawing paths from within gschem */
}


/*! \brief End the input of a path.
 *  \par Function Description
 *  This function ends the process of interactively adding a path to the
 *  current sheet.
 *
 *  It first erases the last temporary path displayed, calculates the
 *  corresponding world coordinates of the two ends of the path and finally
 *  adds a new initialized path object to the list of object of the current
 *  sheet.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void o_path_end(GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  /* TODO: Implement support for drawing paths from within gschem */
}


/*! \brief Draw temporary path while dragging end.
 *  \par Function Description
 *  This function manages the erase/update/draw process of temporary path
 *  when modifying one end of the path.
 *  The path is described by four <B>*w_current</B> variables : the first end
 *  of the path is (<B>first_wx</B>,<B>first_wy</B>), the second end is
 *  (<B>second_wx</B>,<B>second_wy</B>).
 *  The first end is constant. The second end is updated to the (<B>w_x</B>,<B>w_y</B>).
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_path_motion (GSCHEM_TOPLEVEL *w_current, int w_x, int w_y)
{
  if (w_current->rubber_visible)
    o_path_rubberpath_xor (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_path_rubberpath_xor (w_current);
  w_current->rubber_visible = 1;
}


/*! \brief Draw path from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws a path with an exclusive or function over the sheet.
 *  The color of the box is <B>w_current->select_color</B>. The path is
 *  described by the two points (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_path_rubberpath_xor(GSCHEM_TOPLEVEL *w_current)
{
  PATH *path;
  int num_points;
  GdkPoint *points;

  g_return_if_fail (w_current->which_object != NULL);
  g_return_if_fail (w_current->which_object->path != NULL);

  path = w_current->which_object->path;

  path_to_points_modify (w_current, path, 0, 0,
                         w_current->second_wx, w_current->second_wy,
                         w_current->which_grip, &points, &num_points);

  if (num_points == 0) {
    g_free (points);
    return;
  }

  gdk_gc_set_foreground(w_current->xor_gc,
                        x_get_darkcolor(w_current->select_color));
  gdk_gc_set_line_attributes(w_current->xor_gc, 0, GDK_LINE_SOLID,
                             GDK_CAP_NOT_LAST, GDK_JOIN_MITER);

  /* Stroke only, no fill for rubberbanding */
  if (path->sections[path->num_sections - 1].code == PATH_END)
    gdk_draw_polygon (w_current->drawable, w_current->xor_gc,
                      FALSE, points, num_points);
  else
    gdk_draw_lines (w_current->drawable, w_current->xor_gc,
                    points, num_points);

  g_free (points);
}


/*! \brief Draw lines between curve segment end-point and their control point.
 *
 *  \par Function Description
 *  This function XOR draws lines between the end-points and respective
 *  control-points of curve segments in the path.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  The path OBJECT.
 */
static void o_path_xor_control_lines (GSCHEM_TOPLEVEL *w_current,
                                      OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int i;
  int x, y;
  int next_x, next_y;
  int last_x = 0, last_y = 0;
  PATH_SECTION *section;
  GdkGC *gc;
  GdkColor *color;

  /* If the override color is set, we're erasing, and should paint
   * solid rather than XOR */

  if (toplevel->override_color != -1 ) {
    /* override : use the override_color instead */
    color = x_get_color(toplevel->override_color);
    gc = w_current->gc;
  } else {
    /* use the normal selection color */
    color = x_get_darkcolor(w_current->select_color);
    gc = w_current->outline_xor_gc;
  }

  /* set the color for the grip */
  gdk_gc_set_foreground (gc, color);

  for (i = 0; i <  o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

    if (section->code != PATH_END)
      WORLDtoSCREEN (toplevel, section->x3, section->y3, &next_x, &next_y);


    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      WORLDtoSCREEN (toplevel, section->x1, section->y1, &x, &y);
      gdk_draw_line (w_current->drawable, gc, last_x, last_y, x, y);
      WORLDtoSCREEN (toplevel, section->x2, section->y2, &x, &y);
      gdk_draw_line (w_current->drawable, gc, next_x, next_y, x, y);
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      last_x = next_x;
      last_y = next_y;
      break;
    case PATH_END:
      break;
    }
  }
}


/*! \brief Draw grip marks on path.
 *  \par Function Description
 *  This function draws the grips on the path object <B>o_current</B>.
 *
 *  A path has a grip at each end.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  Line OBJECT to draw grip points on.
 */
void o_path_draw_grips(GSCHEM_TOPLEVEL *w_current, OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  PATH_SECTION *section;
  int i;
  int x, y;

  if (w_current->draw_grips == FALSE)
    return;

  o_path_xor_control_lines (w_current, o_current);

  for (i = 0; i <  o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      WORLDtoSCREEN (toplevel, section->x1, section->y1, &x, &y);
      o_grips_draw (w_current, x, y);
      WORLDtoSCREEN (toplevel, section->x2, section->y2, &x, &y);
      o_grips_draw (w_current, x, y);
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      WORLDtoSCREEN (toplevel, section->x3, section->y3, &x, &y);
      o_grips_draw (w_current, x, y);
      break;
    case PATH_END:
      break;
    }
  }
}
