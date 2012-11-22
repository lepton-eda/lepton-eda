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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
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


typedef void (*FILL_FUNC) (GSCHEM_TOPLEVEL *w_current,
                           COLOR *color, PATH *path,
                           gint fill_width,
                           gint angle1, gint pitch1, gint angle2, gint pitch2);

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
void o_path_draw_rubber (GSCHEM_TOPLEVEL *w_current, EdaRenderer *renderer)
{
  OBJECT object;

  /* Setup a fake object to pass the drawing routine */
  object.type = OBJ_PATH;
  object.color = SELECT_COLOR;
  object.line_width = 0; /* clamped to 1 pixel in circle_path */
  object.path = path_copy_modify (w_current->which_object->path, 0, 0,
                                  w_current->second_wx,
                                  w_current->second_wy, w_current->which_grip);

  eda_renderer_draw (renderer, &object);
  g_free (object.path->sections);
  g_free (object.path);
}
