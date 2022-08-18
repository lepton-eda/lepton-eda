/* Lepton EDA Schematic Capture
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
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

#define NUM_BEZIER_SEGMENTS 100


static LeptonPath*
path_copy_modify (LeptonPath *path,
                  int dx,
                  int dy,
                  int new_x,
                  int new_y,
                  int whichone)
{
  LeptonPath *new_path;
  int x1, y1, x2, y2, x3, y3;
  int i;
  int grip_no = 0;

  new_path = (LeptonPath*) g_malloc (sizeof (LeptonPath));
  new_path->sections =
    (LeptonPathSection*) g_malloc (path->num_sections * sizeof (LeptonPathSection));
  new_path->num_sections = path->num_sections;
  new_path->num_sections_max = path->num_sections;

  for (i = 0; i <  path->num_sections; i++) {
    LeptonPathSection *section     = &path->sections[i];
    LeptonPathSection *new_section = &new_path->sections[i];

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

/*! \brief Calculate path bounding box for rubber purposes
 *  \par Function Description
 * Calculate the bounding box of \a path, returning its bounds in \a
 * min_x, \a max_y, \a max_x and \a min_y.  If \a path is NULL, the
 * LeptonPath object currently being edited is used, with any required
 * control point changes applied.
 */
static void
path_rubber_bbox (GschemToplevel *w_current,
                  LeptonPath *path,
                  int *min_x,
                  int *max_y,
                  int *max_x,
                  int *min_y)
{
  int x1, y1, x2, y2, x3, y3;
  int new_x, new_y, whichone;
  int grip_no = 0;
  int i;

  g_assert (w_current);

  if (path == NULL)
    path = w_current->which_object->path;

  *min_x = G_MAXINT;  *max_x = G_MININT;
  *min_y = G_MAXINT;  *max_y = G_MININT;

  new_x = w_current->second_wx;
  new_y = w_current->second_wy;
  whichone = w_current->which_grip;

  for (i = 0; i <  path->num_sections; i++) {
    LeptonPathSection *section = &path->sections[i];

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
        *min_x = MIN (*min_x, x1);  *min_y = MIN (*min_y, y1);
        *max_x = MAX (*max_x, x1);  *max_y = MAX (*max_y, y1);
        *min_x = MIN (*min_x, x2);  *min_y = MIN (*min_y, y2);
        *max_x = MAX (*max_x, x2);  *max_y = MAX (*max_y, y2);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        if (whichone == grip_no++) {
          x3 = new_x; y3 = new_y;
        }
        *min_x = MIN (*min_x, x3);  *min_y = MIN (*min_y, y3);
        *max_x = MAX (*max_x, x3);  *max_y = MAX (*max_y, y3);
      case PATH_END:
        break;
    }
  }
}

/*! Default capacity of newly created path objects, in path
 * sections. */
#define TEMP_PATH_DEFAULT_SIZE 8

/*! \brief Add elements to the temporary LeptonPath.
 * \par Function Description
 * Check if the temporary \c LeptonPath object used when interactively
 * creating paths has room for additional sections.  If not, doubles
 * its capacity.
 */
static void
path_expand (GschemToplevel *w_current)
{
  LeptonPath *p = w_current->temp_path;
  if (p->num_sections == p->num_sections_max) {
    p->num_sections_max *= 2;
    p->sections = g_renew (LeptonPathSection, p->sections,
                           p->num_sections_max);
  }
}

/*! \brief Add new sections to the temporary path while drawing.
 * \par Function Description
 * Calculates the next section to be added to a path while drawing.
 * The temporary slots in the #GschemToplevel structure are used as
 * follows:
 *   - first_wx and first_wy contain the location of the next point
 *     that will lie on the path
 *   - second_wx and second_wy contain the location of the next
 *     point's control point.
 *   - third_wx and third_wy contain the location of the previous
 *     point's control point.
 *   - temp_path is the new \c LeptonPath object (i.e. sequence of
 *     path sections that comprise the path drawn so far).
 *
 * path_next_sections() adds up to two additional sections to the
 * temporary path, and returns the number of sections added, on the
 * basis that: a path starts with a MOVETO the first point; two cusp
 * nodes (control points coincident with the node position) generate a
 * LINETO section; and a path ends either whenever the user clicks on
 * either the first or the current node.
 *
 * \return the number of path sections added.
 */
static int
path_next_sections (GschemToplevel *w_current)
{
  gboolean cusp_point, cusp_prev, close_path, end_path, start_path;
  LeptonPath *p;
  LeptonPathSection *section, *prev_section;
  int x1, y1, x2, y2, x3, y3;
  int save_num_sections;

  g_assert (w_current);
  g_assert (w_current->temp_path != NULL);
  g_assert (w_current->temp_path->sections != NULL);

  x1 = w_current->first_wx;
  y1 = w_current->first_wy;
  x2 = w_current->second_wx;
  y2 = w_current->second_wy;
  x3 = w_current->third_wx;
  y3 = w_current->third_wy;
  p = w_current->temp_path;

  save_num_sections = p->num_sections;

  /* Check whether the section that's being added is the initial
   * MOVETO.  This is detected if the path is currently empty. */
  start_path = (p->num_sections == 0);

  prev_section = start_path ? NULL : &p->sections[p->num_sections - 1];

  /* Check whether the point that's being added has a handle offset. */
  cusp_point = (w_current->first_wx == w_current->second_wx
                && w_current->first_wy == w_current->second_wy);

  /* Check whether there's a leftover control handle from the previous
   * point. */
  cusp_prev = (!start_path
               && prev_section->x3 == x3
               && prev_section->y3 == y3);

  /* Check whether the section that's being added closes the path.
   * This is detected if the location of the node is the same as the
   * location of the starting node, and there is at least one section
   * in the path in addition to the initial MOVETO section. */
  section = &p->sections[0];
  close_path = (!start_path
                && x1 == section->x3
                && y1 == section->y3);

  /* Check whether the section that's being added ends the path. This
   * is detected if the location of the node is the same as the
   * location of the previous node. */
  end_path = (!start_path
              && x1 == prev_section->x3
              && y1 == prev_section->y3);

  /* Create section */
  if (start_path) {
    /* At the start of the path, just create the initial MOVETO. */
    path_expand (w_current);
    section = &p->sections[p->num_sections++];
    section->code = PATH_MOVETO;
    section->x3 = x1;
    section->y3 = y1;

  } else if (!end_path) {
    path_expand (w_current);
    section = &p->sections[p->num_sections++];

    /* If there are two cusp points, then add a line segment. If the
     * path is being closed, closing the path adds an implicit line
     * segment. */
    if (cusp_prev && cusp_point && close_path) {
      section->code = PATH_END;

    } else if (cusp_prev && cusp_point) {
      section->code = PATH_LINETO;
      section->x3 = x1;
      section->y3 = y1;

    } else {
      /* If there are one or more Bezier control points, the section
       * needs to be a CURVETO.  The control point of the current
       * point is mirrored about the point (i.e. the line is kept
       * continuous through the point). */
      section->code = PATH_CURVETO;
      section->x1 = x3;
      section->y1 = y3;
      section->x2 = x1 + (x1 - x2);
      section->y2 = y1 + (y1 - y2);
      section->x3 = x1;
      section->y3 = y1;

      if (close_path) {
        path_expand (w_current);
        section = &p->sections[p->num_sections++];
        section->code = PATH_END;
      }
    }
  }
  /* Return the number of sections added */
  return p->num_sections - save_num_sections;
}

/*! \brief Invalidate current path creation screen region.
 * \par Function Description
 * Invalidates the screen region occupied by the current path creation
 * preview and control handle helpers.
 */
void
o_path_invalidate_rubber (GschemToplevel *w_current)
{
  int added_sections;
  int min_x, min_y, max_x, max_y;

  g_return_if_fail (w_current != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  /* Calculate any new sections */
  added_sections = path_next_sections (w_current);

  path_rubber_bbox (w_current, w_current->temp_path,
                    &min_x, &max_y, &max_x, &min_y);

  /* Expand the bounding box to include any control handles
   * that are currently being drawn. */
  min_x = MIN (min_x, w_current->second_wx);
  max_x = MAX (max_x, w_current->second_wx);
  min_y = MIN (min_y, w_current->second_wy);
  max_y = MAX (max_y, w_current->second_wy);

  gschem_page_view_invalidate_world_rect (page_view,
                                          min_x,
                                          min_y,
                                          max_x,
                                          max_y);

  w_current->temp_path->num_sections -= added_sections;
}

/*! \brief Start process to input a new path.
 *  \par Function Description
 *  This function starts the process of interactively adding a path to
 *  the current sheet by resetting the path creation state and
 *  enabling preview ("rubber") drawing.
 *
 *  For details of how #GschemToplevel fields are used during the
 *  path creation process, see path_next_sections().
 *
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void
o_path_start(GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert (w_current);

  w_current->pathcontrol = TRUE;
  i_action_start (w_current);

  /* Reset path creation state */
  if (w_current->temp_path != NULL) {
    w_current->temp_path->num_sections = 0;
  } else {
    LeptonPath *p = g_new0 (LeptonPath, 1);
    p->sections = g_new0 (LeptonPathSection, TEMP_PATH_DEFAULT_SIZE);
    p->num_sections = 0;
    p->num_sections_max = TEMP_PATH_DEFAULT_SIZE;
    w_current->temp_path = p;
  }

  w_current->which_grip = -1;
  w_current->first_wx = w_x;
  w_current->first_wy = w_y;
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;
  w_current->third_wx = w_x;
  w_current->third_wy = w_y;

  /* Enable preview drawing */
  w_current->rubber_visible = TRUE;
}

/* \brief Begin inputting a new path node.
 * \par Function Description
 * Re-enters path creation mode, saving the current pointer location
 * as the location of the next path control point.
 */
void
o_path_continue (GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert (w_current);
  g_assert (schematic_window_get_inside_action (w_current) != 0);

  w_current->pathcontrol = TRUE;

  o_path_invalidate_rubber (w_current);

  w_current->first_wx = w_x;
  w_current->first_wy = w_y;
  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_path_invalidate_rubber (w_current);
}

/* \brief Give feedback on path creation during mouse movement.
 * \par Function Description
 * If the user is currently in the process of creating a path node
 * (i.e. has mouse button pressed), moves the next node's control
 * point.  If the user has not yet pressed the mouse button to start
 * defining a path node, moves the next node's location and control
 * point together.
 */
void
o_path_motion (GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert (w_current);
  g_assert (w_current->inside_action != 0);

  o_path_invalidate_rubber (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  if (!w_current->pathcontrol) {
    w_current->first_wx = w_x;
    w_current->first_wy = w_y;
  }

  o_path_invalidate_rubber (w_current);
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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        (unused)
 *  \param [in] w_y        (unused)
 */
void
o_path_end(GschemToplevel *w_current, int w_x, int w_y)
{
  gboolean close_path, end_path, start_path;
  LeptonPath *p;
  LeptonPathSection *section, *prev_section;
  int x1, y1, x2, y2;

  g_assert (w_current);
  g_assert (schematic_window_get_inside_action (w_current) != 0);
  g_assert (w_current->temp_path != NULL);
  g_assert (w_current->temp_path->sections != NULL);

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  LeptonPage *page = gschem_page_view_get_page (page_view);
  g_return_if_fail (page != NULL);

  o_path_invalidate_rubber (w_current);

  x1 = w_current->first_wx;
  y1 = w_current->first_wy;
  x2 = w_current->second_wx;
  y2 = w_current->second_wy;
  p = w_current->temp_path;

  /* Check whether the section that's being added is the initial
   * MOVETO.  This is detected if the path is currently empty. */
  start_path = (p->num_sections == 0);

  prev_section = start_path ? NULL : &p->sections[p->num_sections - 1];

  /* Check whether the section that's being added closes the path.
   * This is detected if the location of the node is the same as the
   * location of the starting node, and there is at least one section
   * in the path in addition to the initial MOVETO section. */
  section = &p->sections[0];
  close_path = (!start_path
                && x1 == section->x3
                && y1 == section->y3);

  /* Check whether the section that's being added ends the path. This
   * is detected if the location of the node is the same as the
   * location of the previous node. */
  end_path = (!start_path
              && x1 == prev_section->x3
              && y1 == prev_section->y3);

  /* Add predicted next sections */
  path_next_sections (w_current);

  if (end_path || close_path) {
    /* Add object to page and clean up path drawing state */
    LeptonObject *obj = lepton_path_object_new_take_path (GRAPHIC_COLOR, p);
    w_current->temp_path = NULL;
    w_current->first_wx = -1;
    w_current->first_wy = -1;
    w_current->second_wx = -1;
    w_current->second_wy = -1;
    w_current->third_wx = -1;
    w_current->third_wy = -1;

    lepton_page_append (page, obj);
    g_run_hook_object (w_current, "add-objects-hook", obj);
    gschem_toplevel_page_content_changed (w_current, page);
    o_undo_savestate (w_current, page, UNDO_ALL);

    w_current->rubber_visible = FALSE;

    i_action_stop (w_current);
  } else {
    /* Leave state as it is and continue path drawing... */

    /* Save the control point coordinates for the next section */
    w_current->third_wx = x2;
    w_current->third_wy = y2;

    w_current->pathcontrol = FALSE;
  }
}

/*! \brief Draw path creation preview.
 * \par Function Description
 * Draw a preview of the path currently being drawn, including a
 * helper line showing the control point of the node being drawn (if
 * applicable).
 */
void
o_path_draw_rubber (GschemToplevel *w_current, EdaRenderer *renderer)
{
  LeptonObject object;
  int added_sections = 0;

  /* Draw a helper for when we're dragging a control point */
  if (w_current->first_wx != w_current->second_wx
      || w_current->first_wy != w_current->second_wy) {
    double wwidth = 0;
    cairo_t *cr = eda_renderer_get_cairo_context (renderer);
    GArray *color_map = eda_renderer_get_color_map (renderer);
    int flags = eda_renderer_get_cairo_flags (renderer);

    eda_cairo_line (cr, flags, END_NONE, wwidth,
                    w_current->first_wx, w_current->first_wy,
                    w_current->second_wx, w_current->second_wy);

    eda_cairo_set_source_color (cr, SELECT_COLOR, color_map);
    eda_cairo_stroke (cr, flags, TYPE_SOLID, END_NONE, wwidth, -1, -1);
  }
  /* Now draw the rest of the path */

  /* Calculate any new sections */
  added_sections = path_next_sections (w_current);

  /* Setup a fake object to pass the drawing routine */
  memset (&object, 0, sizeof (LeptonObject));
  LeptonStroke *stroke = lepton_stroke_new ();
  LeptonFill *fill = lepton_fill_new ();
  lepton_object_set_type (&object, OBJ_PATH);
  lepton_object_set_color (&object, SELECT_COLOR);
  lepton_object_set_stroke (&object, stroke);
  lepton_object_set_fill (&object, fill);
  lepton_object_set_stroke_width (&object, 0); /* clamped to 1 pixel in circle_path */
  object.path = w_current->temp_path;

  eda_renderer_draw (renderer, &object);

  /* Throw away the added sections again */
  w_current->temp_path->num_sections -= added_sections;
}

void
o_path_invalidate_rubber_grips (GschemToplevel *w_current)
{
  int min_x, min_y, max_x, max_y;

  GschemPageView *page_view = gschem_toplevel_get_current_page_view (w_current);
  g_return_if_fail (page_view != NULL);

  path_rubber_bbox (w_current, NULL,
                    &min_x, &max_y, &max_x, &min_y);

  gschem_page_view_invalidate_world_rect (page_view,
                                          min_x,
                                          min_y,
                                          max_x,
                                          max_y);
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
 *  \param [in] w_current  The GschemToplevel object.
 *  \param [in] w_x        Current x coordinate of pointer in world units.
 *  \param [in] w_y        Current y coordinate of pointer in world units.
 */
void o_path_motion_grips (GschemToplevel *w_current, int w_x, int w_y)
{
  g_assert (w_current->inside_action != 0);

  if (w_current->rubber_visible)
    o_path_invalidate_rubber_grips (w_current);

  w_current->second_wx = w_x;
  w_current->second_wy = w_y;

  o_path_invalidate_rubber_grips (w_current);
  w_current->rubber_visible = 1;
}


/*! \brief Draw path from #GschemToplevel object.
 *  \par Function Description
 *  This function draws a path with an exclusive or function over
 *  the sheet using \a renderer.
 *  The color of the box is <B>SELECT_COLOR</B>. The path is
 *  described by the two points (<B>w_current->first_wx</B>,
 *  <B>w_current->first_wy</B>) and (<B>w_current->second_wx</B>,<B>w_current->second_wy</B>).
 *
 *  \param [in] w_current  The #GschemToplevel object.
 *  \param [in] renderer   The \c EdaRenderer object.
 */
void
o_path_draw_rubber_grips (GschemToplevel *w_current, EdaRenderer *renderer)
{
  LeptonObject object;

  /* Setup a fake object to pass the drawing routine */
  memset (&object, 0, sizeof (LeptonObject));
  lepton_object_set_type (&object, OBJ_PATH);
  lepton_object_set_color (&object, SELECT_COLOR);
  LeptonStroke *stroke = lepton_stroke_new ();
  LeptonFill *fill = lepton_fill_new ();
  lepton_object_set_stroke (&object, stroke);
  lepton_object_set_fill (&object, fill);
  lepton_object_set_stroke_width (&object, 0); /* clamped to 1 pixel in circle_path */
  object.path = path_copy_modify (w_current->which_object->path, 0, 0,
                                  w_current->second_wx,
                                  w_current->second_wy, w_current->which_grip);

  eda_renderer_draw (renderer, &object);
  lepton_path_free (object.path);
  lepton_fill_free (object.fill);
  lepton_stroke_free (object.stroke);
}
