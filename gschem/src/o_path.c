/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
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
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA
 */
#include <config.h>

#include <stdio.h>
#include <math.h>
#include <cairo.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

#define NUM_BEZIER_SEGMENTS 100


typedef void (*FILL_FUNC) (GdkDrawable *w, GdkGC *gc, COLOR *color,
                           GSCHEM_TOPLEVEL *w_currentm, PATH *path,
                           gint fill_width,
                           gint angle1, gint pitch1, gint angle2, gint pitch2);


static void hint_coordinates (int x, int y, double *fx, double *fy, int width)
{
  double offset = ((width % 2) == 0) ? 0 : 0.5;
  *fx = (double)x + offset;
  *fy = (double)y + offset;
}


static void path_path (GSCHEM_TOPLEVEL *w_current, OBJECT *object)
{
  PATH *path = object->path;
  int line_width;
  int i;
  int x1, y1, x2, y2, x3, y3;
  double fx1 = 0.0, fy1 = 0.0;
  double fx2 = 0.0, fy2 = 0.0;
  double fx3 = 0.0, fy3 = 0.0;

  line_width = SCREENabs (w_current, object->line_width);
  if (line_width <= 0) {
    line_width = 1;
  }

  for (i = 0; i <  path->num_sections; i++) {
    PATH_SECTION *section = &path->sections[i];

    switch (section->code) {
      case PATH_CURVETO:
        /* Two control point grips */
        WORLDtoSCREEN (w_current, section->x1, section->y1, &x1, &y1);
        WORLDtoSCREEN (w_current, section->x2, section->y2, &x2, &y2);
        hint_coordinates (x1, y1, &fx1, &fy1, line_width);
        hint_coordinates (x2, y2, &fx2, &fy2, line_width);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        WORLDtoSCREEN (w_current, section->x3, section->y3, &x3, &y3);
        hint_coordinates (x3, y3, &fx3, &fy3, line_width);
      case PATH_END:
        break;
    }

    switch (section->code) {
      case PATH_MOVETO:
        cairo_close_path (w_current->cr);
        /* fall-through */
      case PATH_MOVETO_OPEN:
        cairo_move_to (w_current->cr, fx3, fy3);
        break;
      case PATH_CURVETO:
        cairo_curve_to (w_current->cr, fx1, fy1, fx2, fy2, fx3, fy3);
        break;
      case PATH_LINETO:
        cairo_line_to (w_current->cr, fx3, fy3);
        break;
      case PATH_END:
        cairo_close_path (w_current->cr);
        break;
    }
  }
}


static PATH *path_copy_modify (PATH *path, int dx, int dy,
                               int new_x, int new_y, int whichone)
{
  PATH *new_path;
  int x1, y1, x2, y2, x3, y3;
  int i;
  int grip_no = 0;

  new_path = g_malloc (sizeof (PATH));
  new_path->sections = g_malloc (path->num_sections * sizeof (PATH_SECTION));
  new_path->num_sections = path->num_sections;
  new_path->num_sections_max = path->num_sections;

  for (i = 0; i <  path->num_sections; i++) {
    PATH_SECTION *section     = &path->sections[i];
    PATH_SECTION *new_section = &new_path->sections[i];

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
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        if (whichone == grip_no++) {
          x3 = new_x; y3 = new_y;
        }
      case PATH_END:
        break;
    }

    new_section->code = section->code;
    new_section->x1 = x1;  new_section->y1 = y1;
    new_section->x2 = x2;  new_section->y2 = y2;
    new_section->x3 = x3;  new_section->y3 = y3;
  }
  return new_path;
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
 *  \param [in] w_current   Schematic top level
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
static void o_path_fill_hollow (GdkDrawable *w, GdkGC *gc, COLOR *color,
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
 *  \param [in] w_current   Schematic top level
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      (unused)
 *  \param [in] pitch1      (unused)
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
static void o_path_fill_fill (GdkDrawable *w, GdkGC *gc, COLOR *color,
                              GSCHEM_TOPLEVEL *w_current, PATH *path,
                              gint fill_width,
                              gint angle1, gint pitch1,
                              gint angle2, gint pitch2)
{
  /* NOP: We'll fill it when we do the stroking */
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
 *  \param [in] w_current   Schematic top level
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      (unused)
 *  \param [in] pitch2      (unused)
 */
static void o_path_fill_hatch (GdkDrawable *w, GdkGC *gc, COLOR *color,
                               GSCHEM_TOPLEVEL *w_current, PATH *path,
                               gint fill_width,
                               gint angle1, gint pitch1,
                               gint angle2, gint pitch2)
{
  int i;
  GArray *lines;

  gschem_cairo_set_source_color (w_current, color);

  lines = g_array_new (FALSE, FALSE, sizeof (LINE));
  m_hatch_path (path, angle1, pitch1, lines);

  for (i=0; i < lines->len; i++) {
    LINE *line = &g_array_index (lines, LINE, i);

    gschem_cairo_line (w_current, END_NONE, fill_width, line->x[0], line->y[0],
                                                        line->x[1], line->y[1]);
  }
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, fill_width, -1, -1);

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
 *  \param [in] w_current   Schematic top level
 *  \param [in] path        The PATH object to draw
 *  \param [in] fill_width  PATH pattern fill width.
 *  \param [in] angle1      1st angle for pattern.
 *  \param [in] pitch1      1st pitch for pattern.
 *  \param [in] angle2      2nd angle for pattern.
 *  \param [in] pitch2      2nd pitch for pattern.
 */
static void o_path_fill_mesh (GdkDrawable *w, GdkGC *gc, COLOR *color,
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
  int angle1, pitch1, angle2, pitch2;
  FILL_FUNC fill_func;

  if (path == NULL) {
    return;
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

  (*fill_func) (w_current->drawable, w_current->gc,
                o_drawing_color (w_current, o_current),
                w_current, path, o_current->fill_width,
                angle1, pitch1, angle2, pitch2);

  path_path (w_current, o_current);

  gschem_cairo_set_source_color (w_current,
                                 o_drawing_color (w_current, o_current));

  if (o_current->fill_type == FILLING_FILL)
    cairo_fill_preserve (w_current->cr);

  gschem_cairo_stroke (w_current, o_current->line_type,
                                  o_current->line_end,
                                  o_current->line_width,
                                  o_current->line_length,
                                  o_current->line_space);

  if (o_current->selected && w_current->draw_grips) {
    o_path_draw_grips (w_current, o_current);
  }
}


/*! \todo Finish function documentation
 *  \brief
 *  \par Function Description
 */
void o_path_invalidate_rubber (GSCHEM_TOPLEVEL *w_current)
{
  PATH *path = w_current->which_object->path;
  int min_x, min_y, max_x, max_y;
  int x1, y1, x2, y2, x3, y3;
  int new_x, new_y, whichone;
  int grip_no = 0;
  int i;

  min_x = G_MAXINT;  max_x = G_MININT;
  min_y = G_MAXINT;  max_y = G_MININT;

  new_x = w_current->second_wx;
  new_y = w_current->second_wy;
  whichone = w_current->which_grip;

  for (i = 0; i <  path->num_sections; i++) {
    PATH_SECTION *section = &path->sections[i];

    x1 = section->x1; y1 = section->y1;
    x2 = section->x2; y2 = section->y2;
    x3 = section->x3; y3 = section->y3;

    switch (section->code) {
      case PATH_CURVETO:
        /* Two control point grips */
        if (whichone == grip_no++) {
          x1 = new_x; y1 = new_y;
        }
        if (whichone == grip_no++) {
          x2 = new_x; y2 = new_y;
        }
        min_x = MIN (min_x, x1);  min_y = MIN (min_y, y1);
        max_x = MAX (max_x, x1);  max_y = MAX (max_y, y1);
        min_x = MIN (min_x, x2);  min_y = MIN (min_y, y2);
        max_x = MAX (max_x, x2);  max_y = MAX (max_y, y2);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        if (whichone == grip_no++) {
          x3 = new_x; y3 = new_y;
        }
        min_x = MIN (min_x, x3);  min_y = MIN (min_y, y3);
        max_x = MAX (max_x, x3);  max_y = MAX (max_y, y3);
      case PATH_END:
        break;
    }
  }

  WORLDtoSCREEN (w_current, min_x, max_y, &x1, &y1);
  WORLDtoSCREEN (w_current, max_x, min_y, &x2, &y2);
  o_invalidate_rect (w_current, x1, y1, x2, y2);
}


/*! \brief Draw a path object after applying translation.
 *  \par Function Description
 *  This function is used to draw the path object described by
 *  <B>*o_current</B> after applying a translation on the two directions of
 *  <B>dx</B> and <B>dy</B> in world units.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] dx         Delta x coordinate for path.
 *  \param [in] dy         Delta y coordinate for path.
 *  \param [in] o_current  Line OBJECT to draw.
 */
void o_path_draw_place (GSCHEM_TOPLEVEL *w_current, int dx, int dy, OBJECT *o_current)
{
  OBJECT object;

  g_return_if_fail (o_current->path != NULL);

  /* Setup a fake object to pass the drawing routine */
  object.line_width = 0; /* clamped to 1 pixel in circle_path */
  object.path = path_copy_modify (o_current->path, dx, dy, 0, 0, -1);

  path_path (w_current, &object);
  g_free (object.path->sections);
  g_free (object.path);

  gschem_cairo_set_source_color (w_current,
                                 x_color_lookup_dark (o_current->color));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);
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
    o_path_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_path_invalidate_rubber (w_current);
  w_current->rubber_visible = 1;
}


/*! \brief Draw path from GSCHEM_TOPLEVEL object.
 *  \par Function Description
 *  This function draws a path with an exclusive or function over the sheet.
 *  The color of the box is <B>SELECT_COLOR</B>. The path is
 *  described by the two points (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 */
void o_path_draw_rubber (GSCHEM_TOPLEVEL *w_current)
{
  OBJECT object;

  /* Setup a fake object to pass the drawing routine */
  object.line_width = 0; /* clamped to 1 pixel in circle_path */
  object.path = path_copy_modify (w_current->which_object->path, 0, 0,
                                  w_current->second_wx,
                                  w_current->second_wy, w_current->which_grip);

  path_path (w_current, &object);
  g_free (object.path->sections);
  g_free (object.path);

  gschem_cairo_set_source_color (w_current, x_color_lookup (SELECT_COLOR));
  gschem_cairo_stroke (w_current, TYPE_SOLID, END_SQUARE, 0, -1, -1);
}


/*! \brief Draw lines between curve segment end-point and their control point.
 *
 *  \par Function Description
 *  This function Draws lines between the end-points and respective
 *  control-points of curve segments in the path.
 *
 *  \param [in] w_current  The GSCHEM_TOPLEVEL object.
 *  \param [in] o_current  The path OBJECT.
 */
static void draw_control_lines (GSCHEM_TOPLEVEL *w_current,
                                OBJECT *o_current)
{
  TOPLEVEL *toplevel = w_current->toplevel;
  int i;
  int next_x, next_y;
  int last_x = 0, last_y = 0;
  PATH_SECTION *section;
  COLOR *color;

  if (toplevel->override_color != -1 ) {
    /* override : use the override_color instead */
    color = x_color_lookup (toplevel->override_color);
  } else {
    /* use the normal selection color */
    color = x_color_lookup_dark (SELECT_COLOR);
  }

  /* set the color for the grip */
  gschem_cairo_set_source_color (w_current, color);

  for (i = 0; i <  o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

    if (section->code != PATH_END) {
      next_x = section->x3;
      next_y = section->y3;
    }

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      gschem_cairo_line (w_current, END_NONE, 0,
                         last_x, last_y, section->x1, section->y1);
      gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);

      gschem_cairo_line (w_current, END_NONE, 0,
                         next_x, next_y, section->x2, section->y2);
      gschem_cairo_stroke (w_current, TYPE_SOLID, END_NONE, 0, -1, -1);

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
  PATH_SECTION *section;
  int i;

  if (w_current->draw_grips == FALSE)
    return;

  draw_control_lines (w_current, o_current);

  for (i = 0; i <  o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      o_grips_draw (w_current, section->x1, section->y1);
      o_grips_draw (w_current, section->x2, section->y2);
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      o_grips_draw (w_current, section->x3, section->y3);
      break;
    case PATH_END:
      break;
    }
  }
}
