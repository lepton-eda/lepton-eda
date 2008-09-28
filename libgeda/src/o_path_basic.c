/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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
#include <string.h>

#include "libgeda_priv.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif

/*! Default setting for path draw function. */
void (*path_draw_func)() = NULL;

/*! \brief Create and add path OBJECT to list.
 *  \par Function Description
 *  This function creates a new object representing a path.
 *  This object is added to the end of the list <B>object_list</B>
 *  pointed object belongs to.
 *  The path is described by its two ends - <B>x1</B>,<B>y1</B> and
 *  <B>x2</B>,<B>y2</B>.
 *  The <B>type</B> parameter must be equal to #OBJ_PATH.
 *  The <B>color</B> parameter corresponds to the color the path
 *  will be drawn with.
 *
 *  The #OBJECT structure is allocated with the
 *  #s_basic_init_object() function. The structure describing
 *  the path is allocated and initialized with the parameters given
 *  to the function.
 *
 *  Both the path type and the filling type are set to default
 *  values : solid path type with a width of 0, and no filling.
 *  It can be changed after with the #o_set_path_options() and
 *  #o_set_fill_options().
 *
 *  The object is added to the end of the list described by the
 *  <B>object_list</B> parameter by the #s_basic_link_object().
 *
 *  \param [in]     toplevel     The TOPLEVEL object.
 *  \param [in]     type         Must be OBJ_PATH.
 *  \param [in]     color        Circle path color.
 *  \return A pointer to the new end of the object list.
 */
OBJECT *o_path_new (TOPLEVEL *toplevel,
                    char type, int color, const char *path_string)
{
  OBJECT *new_node;

  /* create the object */
  new_node        = s_basic_new_object (type, "path");
  new_node->color = color;

  new_node->path  = s_path_parse (path_string);

  /* path type and filling initialized to default */
  o_set_line_options (toplevel, new_node,
                      END_NONE, TYPE_SOLID, 0, -1, -1);
  o_set_fill_options (toplevel, new_node,
                      FILLING_HOLLOW, -1, -1, -1, -1, -1);

  new_node->draw_func = path_draw_func;
  new_node->sel_func = select_func;

  /* compute bounding box */
  o_path_recalc (toplevel, new_node);

  return new_node;
}


/*! \brief Create a copy of a path.
 *  \par Function Description
 *  This function creates a verbatim copy of the
 *  object pointed by <B>o_current</B> describing a path. The new object
 *  is added at the end of the list following the <B>list_tail</B>
 *  parameter.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  o_current  Line OBJECT to copy.
 *  \return A new pointer to the end of the object list.
 */
OBJECT *o_path_copy (TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;
  char *path_string;
  int color;

  if (o_current->saved_color == -1) {
    color = o_current->color;
  } else {
    color = o_current->saved_color;
  }

  path_string = s_path_string_from_path (o_current->path);
  new_obj = o_path_new (toplevel, OBJ_PATH, color, path_string);
  g_free (path_string);

  /* copy the path type and filling options */
  o_set_line_options (toplevel, new_obj, o_current->line_end,
                      o_current->line_type, o_current->line_width,
                      o_current->line_length, o_current->line_space);
  o_set_fill_options (toplevel, new_obj,
                      o_current->fill_type, o_current->fill_width,
                      o_current->fill_pitch1, o_current->fill_angle1,
                      o_current->fill_pitch2, o_current->fill_angle2);

  /* calc the bounding box */
  o_path_recalc (toplevel, o_current);

  /* return the new tail of the object list */
  return new_obj;
}


/*! \brief Create path OBJECT from character string.
 *  \par Function Description
 *  This function creates a path OBJECT from the character string
 *  <B>*buf</B> and a number of lines following that describing the
 *  path, read from <B>*tb</B>. The new path is added to the
 *  list of objects of which <B>*object_list</B> is the last element
 *  before the call.
 *  The function returns a pointer on the new last element, that is
 *  the added path object.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 20010704 release.
 *    <DT>*</DT><DD>the file format used for the releases after 20010704.
 *  </DL>
 *
 *  \param [in]  toplevel       The TOPLEVEL object.
 *  \param [out] object_list     OBJECT list to create path in.
 *  \param [in]  first_line      Character string with path description.
 *  \param [in]  tb              Text buffer containing the path string.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *  \return A pointer to the new path object.
 */
OBJECT *o_path_read (TOPLEVEL *toplevel, OBJECT *object_list,
                     const char *first_line, TextBuffer *tb,
                     unsigned int release_ver, unsigned int fileformat_ver)
{
  OBJECT *new_obj;
  char type;
  int color;
  int line_width, line_space, line_length;
  int line_end;
  int line_type;
  int fill_type, fill_width, angle1, pitch1, angle2, pitch2;
  int num_lines = 0;
  int i;
  char *string;
  GString *pathstr;

  /*
   * The current path format to describe a line is a space separated
   * list of characters and numbers in plain ASCII on a single path.
   * The meaning of each item is described in the file format documentation.
   */
  /* Allocate enough space */
  sscanf (first_line, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
          &type, &color, &line_width, &line_end, &line_type,
          &line_length, &line_space, &fill_type, &fill_width, &angle1,
          &pitch1, &angle2, &pitch2, &num_lines);

  /*
   * Checks if the required color is valid.
   */
  if (color < 0 || color > MAX_COLORS) {
    s_log_message (_("Found an invalid color [ %s ]\n"), first_line);
    s_log_message (_("Setting color to WHITE\n"));
    color = WHITE;
  }

  /*
   * A path is internally described by its two ends. A new object is
   * allocated, initialized and added to the list of objects. Its path
   * type is set according to the values of the fields on the path.
   */

  pathstr = g_string_new ("");
  for (i = 0; i < num_lines; i++) {
    gchar *line;

    line = s_textbuffer_next_line (tb);

    if (line != NULL) {
      pathstr = g_string_append (pathstr, line);
    }
  }

  /* retrieve the character string from the GString */
  string = g_string_free (pathstr, FALSE);
  string = remove_last_nl (string);

  /* create a new path */
  new_obj = o_path_new (toplevel, type, color, string);
  g_free (string);

  /* set its line options */
  o_set_line_options (toplevel, object_list,
                      line_end, line_type, line_width, line_length, line_space);
  /* set its fill options */
  o_set_fill_options (toplevel, object_list,
                      fill_type, fill_width, pitch1, angle1, pitch2, angle2);

  /* Add the path to the object list */
  object_list = s_basic_link_object(new_obj, object_list);
  return object_list;
}


/*! \brief Create a character string representation of a path OBJECT.
 *  \par Function Description
 *  The function formats a string in the buffer <B>*buff</B> to describe
 *  the path object <B>*object</B>.
 *
 *  \param [in] object  path OBJECT to create string from.
 *  \return A pointer to the path OBJECT character string.
 *
 *  \note
 *  Caller must g_free returned character string.
 *
 */
char *o_path_save (OBJECT *object)
{
  int color;
  int line_width, line_space, line_length;
  char *buf;
  int num_lines;
  OBJECT_END line_end;
  OBJECT_TYPE line_type;
  OBJECT_FILLING fill_type;
  int fill_width, angle1, pitch1, angle2, pitch2;
  char *path_string;

  /* description of the line type */
  line_width  = object->line_width;
  line_end    = object->line_end;
  line_type   = object->line_type;
  line_length = object->line_length;
  line_space  = object->line_space;

  /* filling parameters */
  fill_type    = object->fill_type;
  fill_width   = object->fill_width;
  angle1       = object->fill_angle1;
  pitch1       = object->fill_pitch1;
  angle2       = object->fill_angle2;
  pitch2       = object->fill_pitch2;

  /* Use the right color */
  if (object->saved_color == -1) {
    color = object->color;
  } else {
    color = object->saved_color;
  }

  path_string = s_path_string_from_path (object->path);
  num_lines = o_text_num_lines (path_string);
  buf = g_strdup_printf ("%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n%s",
                         object->type, color, line_width, line_end,
                         line_type, line_length, line_space, fill_type,
                         fill_width, angle1, pitch1, angle2, pitch2,
                         num_lines, path_string);
  g_free (path_string);

  return buf;
}


/*! \brief Modify controol point location
 *
 *  \par Function Description
 *  This function modifies a control point location of the path object
 *  *object. The control point being modified is selected according to
 *  the whichone parameter.
 *
 *  The new position is given by <B>x</B> and <B>y</B>.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in,out] object    The path OBJECT
 *  \param [in]     x         New x coordinate for the control point
 *  \param [in]     y         New y coordinate for the control point
 *  \param [in]     whichone  Which control point is being modified
 */
void o_path_modify (TOPLEVEL *toplevel, OBJECT *object,
                    int x, int y, int whichone)
{
  int i;
  int grip_no = 0;
  PATH_SECTION *section;

  for (i = 0; i <  object->path->num_sections; i++) {
    section = &object->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      if (whichone == grip_no++) {
        section->x1 = x;
        section->y1 = y;
      }
      if (whichone == grip_no++) {
        section->x2 = x;
        section->y2 = y;
      }
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      if (whichone == grip_no++) {
        section->x3 = x;
        section->y3 = y;
      }
      break;
    case PATH_END:
      break;
    }
  }

  /* Update bounding box */
  o_path_recalc (toplevel, object);
}


/*! \brief Translate a path position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the path
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 *  \param [in]     toplevel  The TOPLEVEL object.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 *  \param [in,out] object     Line OBJECT to translate.
 */
void o_path_translate_world (TOPLEVEL *toplevel,
                             int dx, int dy, OBJECT *object)
{
  PATH_SECTION *section;
  int i;

  for (i = 0; i < object->path->num_sections; i++) {
    section = &object->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      section->x1 += dx;
      section->y1 += dy;
      section->x2 += dx;
      section->y2 += dy;
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      section->x3 += dx;
      section->y3 += dy;
      break;
    case PATH_END:
      break;
    }
  }

  /* Update bounding box */
  o_path_recalc (toplevel, object);
}


/*! \brief Rotate Line OBJECT using WORLD coordinates.
 *  \par Function Description
 *  This function rotates the path described by
 *  <B>*object</B> around the (<B>world_centerx</B>,<B>world_centery</B>)
 *  point by <B>angle</B> degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      toplevel      The TOPLEVEL object.
 *  \param [in]      world_centerx  Rotation center x coordinate in WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Line OBJECT to rotate.
 */
void o_path_rotate_world (TOPLEVEL *toplevel,
                          int world_centerx, int world_centery, int angle,
                          OBJECT *object)
{
  PATH_SECTION *section;
  int i;

  for (i = 0; i < object->path->num_sections; i++) {
    section = &object->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      section->x1 -= world_centerx; section->y1 -= world_centery;
      section->x2 -= world_centerx; section->y2 -= world_centery;
      rotate_point_90 (section->x1, section->y1, angle, &section->x1, &section->y1);
      rotate_point_90 (section->x2, section->y2, angle, &section->x2, &section->y2);
      section->x1 += world_centerx; section->y1 += world_centery;
      section->x2 += world_centerx; section->y2 += world_centery;
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      section->x3 -= world_centerx; section->y3 -= world_centery;
      rotate_point_90 (section->x3, section->y3, angle, &section->x3, &section->y3);
      section->x3 += world_centerx; section->y3 += world_centery;
      break;
    case PATH_END:
      break;
    }
  }
  o_path_recalc (toplevel, object);
}


/*! \brief Mirror a path using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the path from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  \param [in]     toplevel      The TOPLEVEL object.
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Line OBJECT to mirror.
 */
void o_path_mirror_world (TOPLEVEL *toplevel, int world_centerx,
                          int world_centery, OBJECT *object)
{
  PATH_SECTION *section;
  int i;

  for (i = 0; i < object->path->num_sections; i++) {
    section = &object->path->sections[i];

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      section->x1 = 2 * world_centerx - section->x1;
      section->x2 = 2 * world_centerx - section->x2;
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      section->x3 = 2 * world_centerx - section->x3;
      break;
    case PATH_END:
      break;
    }
  }

  o_path_recalc (toplevel, object);
}


/*! \brief Recalculate path coordinates in SCREEN units.
 *  \par Function Description
 *  This function recalculate the bounding box of the <B>o_current</B>
 *
 *  \param [in] toplevel      The TOPLEVEL object.
 *  \param [in,out] o_current  Line OBJECT to be recalculated.
 */
void o_path_recalc (TOPLEVEL *toplevel, OBJECT *o_current)
{
  int left, right, top, bottom;

  if (o_current->path == NULL) {
    return;
  }

  /* Update the bounding box */
  world_get_path_bounds (toplevel, o_current, &left, &top, &right, &bottom);
  o_current->w_left   = left;
  o_current->w_top    = top;
  o_current->w_right  = right;
  o_current->w_bottom = bottom;
  o_current->w_bounds_valid = TRUE;
}


/*! \brief Get path bounding rectangle in WORLD coordinates.
 *  \par Function Description
 *  This function sets the <B>left</B>, <B>top</B>, <B>right</B> and
 *  <B>bottom</B> parameters to the boundings of the path object described
 *  in <B>*path</B> in world units.
 *
 *  \note Bounding box for bezier curves is loose because we just consider
 *        the convex hull of the curve control and end-points.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  OBJECT     Line OBJECT to read coordinates from.
 *  \param [out] left       Left path coordinate in WORLD units.
 *  \param [out] top        Top path coordinate in WORLD units.
 *  \param [out] right      Right path coordinate in WORLD units.
 *  \param [out] bottom     Bottom path coordinate in WORLD units.
 */
void world_get_path_bounds (TOPLEVEL *toplevel, OBJECT *object,
                            int *left, int *top, int *right, int *bottom)
{
  PATH_SECTION *section;
  int halfwidth;
  int i;
  int found_bound = FALSE;

  /* Find the bounds of the path region */
  for (i = 0; i < object->path->num_sections; i++) {
    section = &object->path->sections[i];
    switch (section->code) {
      case PATH_CURVETO:
        /* Bezier curves with this construction of control points will lie
         * within the convex hull of the control and curve end points */
        *left   = (found_bound) ? MIN (*left,   section->x1) : section->x1;
        *top    = (found_bound) ? MIN (*top,    section->y1) : section->y1;
        *right  = (found_bound) ? MAX (*right,  section->x1) : section->x1;
        *bottom = (found_bound) ? MAX (*bottom, section->y1) : section->y1;
        found_bound = TRUE;
        *left   = MIN (*left,   section->x2);
        *top    = MIN (*top,    section->y2);
        *right  = MAX (*right,  section->x2);
        *bottom = MAX (*bottom, section->y2);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        *left   = (found_bound) ? MIN (*left,   section->x3) : section->x3;
        *top    = (found_bound) ? MIN (*top,    section->y3) : section->y3;
        *right  = (found_bound) ? MAX (*right,  section->x3) : section->x3;
        *bottom = (found_bound) ? MAX (*bottom, section->y3) : section->y3;
        found_bound = TRUE;
        break;
      case PATH_END:
        break;
    }
  }

  if (found_bound) {
    /* This isn't strictly correct, but a 1st order approximation */
    halfwidth = object->line_width / 2;
    *left   -= halfwidth;
    *top    -= halfwidth;
    *right  += halfwidth;
    *bottom += halfwidth;
  }
}


/*! \brief Print path to Postscript document.
 *  \par Function Description
 *  This function prints the path described by the <B>o_current</B>
 *  parameter to a Postscript document.
 *  The Postscript document is described by the <B>fp</B> file pointer.
 *
 *  Parameters of the path are extracted from object pointed by
 *  <B>o_current</B>.
 *
 *  \param [in] toplevel  The TOPLEVEL object.
 *  \param [in] fp         FILE pointer to Postscript document.
 *  \param [in] o_current  Line OBJECT to write to document.
 *  \param [in] origin_x   Page x coordinate to place path OBJECT.
 *  \param [in] origin_y   Page y coordinate to place path OBJECT.
 */
void o_path_print (TOPLEVEL *toplevel, FILE *fp, OBJECT *o_current,
                   int origin_x, int origin_y)
{
  PATH_SECTION *section;
  GString *path_string;
  int line_width;
  int i;

  line_width = o_current->line_width;
  if (line_width <=2) {
    if (toplevel->line_style == THICK) {
      line_width = LINE_WIDTH;
    } else {
      line_width = 2;
    }
  }

  path_string = g_string_new ("");

  f_print_set_color (fp, o_current->color);
  f_print_set_line_width (fp, line_width);

  for (i = 0; i < o_current->path->num_sections; i++) {
    section = &o_current->path->sections[i];

    if (i > 0)
      fprintf (fp, " ");

    switch (section->code) {
      case PATH_MOVETO:
        fprintf (fp, "closepath ");
        /* Fall through */
      case PATH_MOVETO_OPEN:
        fprintf (fp, "%i %i moveto",
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_CURVETO:
        fprintf (fp, "%i %i %i %i %i %i curveto",
                     section->x1 - origin_x, section->y1 - origin_y,
                     section->x2 - origin_x, section->y2 - origin_y,
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_LINETO:
        fprintf (fp, "%i %i lineto",
                     section->x3 - origin_x, section->y3 - origin_y);
        break;
      case PATH_END:
        fprintf (fp, "closepath ");
        break;
    }
  }

  if (o_current->fill_type == FILLING_HOLLOW) {
    fprintf (fp, "stroke\n");
  } else {
    fprintf (fp, "gsave fill grestore stroke\n");
  }
}


/*! \brief Calculates the distance between the given point and the closest
 *  point on the given path segment.
 *
 *  \todo Support for bezier path segments.
 *
 *  \param [in] object The path OBJECT
 *  \param [in] x The x coordinate of the given point.
 *  \param [in] y The y coordinate of the given point.
 *  \return The shortest distance from the object to the point.  With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
gdouble o_path_shortest_distance (OBJECT *object, gint x, gint y)
{
  PATH_SECTION *section;
  LINE line;
  gdouble shortest = G_MAXDOUBLE;
  int last_x = 0, last_y = 0;
  int i;

  for (i = 0; i < object->path->num_sections; i++) {
    section = &object->path->sections[i];
    switch (section->code) {

      case PATH_CURVETO:
        /* TODO: Shortest distance to a besier section of the path.
         *       For now, pretend it is a straight line. */
        /* Fall through */
      case PATH_LINETO:
        line.x[0] = last_x;
        line.y[0] = last_y;
        line.x[1] = last_x = section->x3;
        line.y[1] = last_y = section->y3;
        shortest = MIN (shortest, o_line_shortest_distance (&line, x, y));
        break;

      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
        last_x = section->x3;
        last_y = section->y3;
        break;

      case PATH_END:
        /* Need to consider the line back to the first point in the path */
        line.x[0] = last_x;
        line.y[0] = last_y;
        line.x[1] = last_x = object->path->sections[0].x3;
        line.y[1] = last_y = object->path->sections[0].y3;
        shortest = MIN (shortest, o_line_shortest_distance (&line, x, y));
        break;
    }
  }

  return shortest;
}

