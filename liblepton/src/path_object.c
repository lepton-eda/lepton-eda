/* Lepton EDA library
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
#include <string.h>

#include "liblepton_priv.h"

/*! \file path_object.c
 *  \brief functions for the path object
 */

/*! \brief Return path object section with given number.
 *  \par Function Description
 *  Returns the #LeptonPathSection object pointer with the number
 *  \a i for #LeptonObject.
 *
 *  \param [in] object The #LeptonObject.
 *  \param [in] i The id number of wanted section.
 *  \return The pointer to the requested #LeptonPathSection.
 */
LeptonPathSection*
lepton_path_object_get_section (const LeptonObject *object,
                                int i)
{
  g_return_val_if_fail (lepton_object_is_path (object), NULL);
  g_return_val_if_fail (object->path != NULL, NULL);

  g_return_val_if_fail ((i >= 0) &&
                        (i < lepton_path_object_get_num_sections (object)),
                        NULL);

  return &object->path->sections[i];
}


/*! \brief Create a new path object.
 *  \par Function Description
 *  This function creates a new object representing a path.
 *  This object is added to the end of the list <B>object_list</B>
 *  pointed object belongs to.
 *  The <B>type</B> parameter must be equal to #OBJ_PATH.
 *  The <B>color</B> parameter corresponds to the color the path
 *  will be drawn with.
 *  The path shape is created by parsing \a path_string.
 *
 *  The #LeptonObject structure is allocated with the
 *  #lepton_object_new() function. The structure describing the
 *  path is allocated and initialized with the parameters given to
 *  the function.
 *
 *  Both the path type and the filling type are set to default
 *  values : solid path type with a width of 0, and no filling.
 *  It can be changed after with the #lepton_object_set_line_options() and
 *  #lepton_object_set_fill_options().
 *
 *  \param [in]     color        The path color.
 *  \param [in]     path_string  The string representation of the path
 *  \return A pointer to the new end of the object list.
 */
LeptonObject*
lepton_path_object_new (int color,
                        const char *path_string)
{
  return lepton_path_object_new_take_path (color,
                                           lepton_path_parse (path_string));
}


/*! \brief Create a new path object.
 *  \par Function Description
 *  This function creates and returns a new LeptonObject representing a path
 *  using the path shape data stored in \a path_data.  The \a
 *  path_data is subsequently owned by the returned LeptonObject.
 *
 *  \see lepton_path_object_new().
 *
 *  \param [in]     color        The path color.
 *  \param [in]     path_data    The #LeptonPath data structure to use.
 *  \return A pointer to the new end of the object list.
 */
LeptonObject*
lepton_path_object_new_take_path (int color,
                                  LeptonPath *path_data)
{
  LeptonObject *new_node;

  /* create the object */
  new_node        = lepton_object_new (OBJ_PATH, "path");
  lepton_object_set_color (new_node, color);

  new_node->path  = path_data;

  /* path type and filling initialized to default */
  lepton_object_set_line_options (new_node,
                                  DEFAULT_OBJECT_END,
                                  TYPE_SOLID,
                                  LINE_WIDTH,
                                  -1,
                                  -1);

  lepton_object_set_fill_options (new_node,
                                  FILLING_HOLLOW,
                                  -1,
                                  -1,
                                  -1,
                                  -1,
                                  -1);

  return new_node;
}


/*! \brief Create a copy of a path.
 *  \par Function Description
 *  This function creates a verbatim copy of the
 *  object pointed by <B>o_current</B> describing a path. The new object
 *  is added at the end of the list following the <B>list_tail</B>
 *  parameter.
 *
 *  \param [in]  o_current  Line LeptonObject to copy.
 *  \return A new pointer to the end of the object list.
 */
LeptonObject*
lepton_path_object_copy (LeptonObject *o_current)
{
  LeptonObject *new_obj;
  char *path_string;

  path_string = lepton_path_string_from_path (o_current->path);
  new_obj = lepton_path_object_new (lepton_object_get_color (o_current),
                                    path_string);
  g_free (path_string);

  /* copy the path type and filling options */
  lepton_object_set_line_options (new_obj,
                                  lepton_object_get_stroke_cap_type (o_current),
                                  lepton_object_get_stroke_type (o_current),
                                  lepton_object_get_stroke_width (o_current),
                                  lepton_object_get_stroke_dash_length (o_current),
                                  lepton_object_get_stroke_space_length (o_current));
  lepton_object_set_fill_options (new_obj,
                                  lepton_object_get_fill_type (o_current),
                                  lepton_object_get_fill_width (o_current),
                                  lepton_object_get_fill_pitch1 (o_current),
                                  lepton_object_get_fill_angle1 (o_current),
                                  lepton_object_get_fill_pitch2 (o_current),
                                  lepton_object_get_fill_angle2 (o_current));

  /* return the new tail of the object list */
  return new_obj;
}


/*! \brief Create path LeptonObject from character string.
 *  \par Function Description
 *  This function creates a path LeptonObject from the character string
 *  <B>*buf</B> and a number of lines following that describing the
 *  path, read from <B>*tb</B>.
 *
 *  Depending on <B>*version</B>, the correct file format is considered.
 *  Currently two file format revisions are supported :
 *  <DL>
 *    <DT>*</DT><DD>the file format used until 20010704 release.
 *    <DT>*</DT><DD>the file format used for the releases after 20010704.
 *  </DL>
 *
 *  \param [in]  first_line      Character string with path description.
 *  \param [in]  tb              Text buffer containing the path string.
 *  \param [in]  release_ver     libgeda release version number.
 *  \param [in]  fileformat_ver  libgeda file format version number.
 *  \param [in,out] err \c GError structure for error reporting,
 *                      or NULL to disable error reporting.
 *  \return A pointer to the new path object, or NULL on error;
 */
LeptonObject*
o_path_read (const char *first_line,
             TextBuffer *tb,
             unsigned int release_ver,
             unsigned int fileformat_ver,
             GError **err)
{
  LeptonObject *new_obj;
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
  if (sscanf (first_line, "%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
              &type, &color, &line_width, &line_end, &line_type,
              &line_length, &line_space, &fill_type, &fill_width, &angle1,
              &pitch1, &angle2, &pitch2, &num_lines) != 14) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse path object"));
    return NULL;
  }

  /*
   * Checks if the required color is valid.
   */
  if (!color_id_valid (color)) {
    g_message (_("Found an invalid color [ %1$s ]"), first_line);
    g_message (_("Setting color to default color."));
    color = default_color_id();
  }

  /*
   * A path is internally described by its two ends. A new object is
   * allocated, initialized and added to the list of objects. Its path
   * type is set according to the values of the fields on the path.
   */

  pathstr = g_string_new ("");
  for (i = 0; i < num_lines; i++) {
    const gchar *line;

    line = s_textbuffer_next_line (tb);

    if (line == NULL) {
      g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Unexpected end-of-file when reading path"));
      return NULL;
    }

    pathstr = g_string_append (pathstr, line);
  }

  /* retrieve the character string from the GString */
  string = g_string_free (pathstr, FALSE);
  string = lepton_str_remove_ending_newline (string);

  /* create a new path */
  new_obj = lepton_path_object_new (color, string);
  g_free (string);

  /* set its line options */
  lepton_object_set_line_options (new_obj,
                                  (LeptonStrokeCapType) line_end,
                                  (LeptonStrokeType) line_type,
                                  line_width,
                                  line_length,
                                  line_space);
  /* set its fill options */
  lepton_object_set_fill_options (new_obj,
                                  (LeptonFillType) fill_type,
                                  fill_width,
                                  pitch1,
                                  angle1,
                                  pitch2,
                                  angle2);

  return new_obj;
}


/*! \brief Create a character string representation of a path LeptonObject.
 *  \par Function Description
 *  The function formats a string in the buffer <B>*buff</B> to describe
 *  the path object <B>*object</B>.
 *
 *  \param [in] object  path LeptonObject to create string from.
 *  \return A pointer to the path LeptonObject character string.
 *
 *  \note
 *  Caller must g_free returned character string.
 *
 */
gchar*
lepton_path_object_to_buffer (const LeptonObject *object)
{
  int line_width, line_space, line_length;
  char *buf;
  int num_lines;
  LeptonStrokeCapType line_end;
  LeptonStrokeType line_type;
  LeptonFillType fill_type;
  int fill_width, angle1, pitch1, angle2, pitch2;
  char *path_string;

  /* description of the line type */
  line_width  = lepton_object_get_stroke_width (object);
  line_end    = lepton_object_get_stroke_cap_type (object);
  line_type   = lepton_object_get_stroke_type (object);
  line_length = lepton_object_get_stroke_dash_length (object);
  line_space  = lepton_object_get_stroke_space_length (object);

  /* filling parameters */
  fill_type    = lepton_object_get_fill_type (object);
  fill_width   = lepton_object_get_fill_width (object);
  angle1       = lepton_object_get_fill_angle1 (object);
  pitch1       = lepton_object_get_fill_pitch1 (object);
  angle2       = lepton_object_get_fill_angle2 (object);
  pitch2       = lepton_object_get_fill_pitch2 (object);

  path_string = lepton_path_string_from_path (object->path);
  num_lines = o_text_num_lines (path_string);
  buf = g_strdup_printf ("%c %d %d %d %d %d %d %d %d %d %d %d %d %d\n%s",
                         lepton_object_get_type (object),
                         lepton_object_get_color (object),
                         line_width, line_end,
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
 *  \param [in,out] object    The path LeptonObject
 *  \param [in]     x         New x coordinate for the control point
 *  \param [in]     y         New y coordinate for the control point
 *  \param [in]     whichone  Which control point is being modified
 */
void
lepton_path_object_modify (LeptonObject *object,
                           int x,
                           int y,
                           int whichone)
{
  int i;
  int grip_no = 0;
  LeptonPathSection *section;

  lepton_object_emit_pre_change_notify (object);

  for (i = 0; i < lepton_path_object_get_num_sections (object); i++)
  {
    section = lepton_path_object_get_section (object, i);

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

  lepton_object_emit_change_notify (object);
}


/*! \brief Translate a path position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the path
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world unit.
 *
 *  \param [in,out] object     Line LeptonObject to translate.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 */
void
lepton_path_object_translate (LeptonObject *object,
                              int dx,
                              int dy)
{
  LeptonPathSection *section;
  int i;

  g_return_if_fail (lepton_object_is_path (object));
  g_return_if_fail (object->path != NULL);

  for (i = 0; i < lepton_path_object_get_num_sections (object); i++)
  {
    section = lepton_path_object_get_section (object, i);

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
}


/*! \brief Rotate Line LeptonObject using WORLD coordinates.
 *  \par Function Description
 *  This function rotates the path described by
 *  <B>*object</B> around the (<B>world_centerx</B>,<B>world_centery</B>)
 *  point by <B>angle</B> degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      world_centerx  Rotation center x coordinate in WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Line LeptonObject to rotate.
 */
void
lepton_path_object_rotate (int world_centerx,
                           int world_centery,
                           int angle,
                           LeptonObject *object)
{
  LeptonPathSection *section;
  int i;

  g_return_if_fail (lepton_object_is_path (object));
  g_return_if_fail (object->path != NULL);

  for (i = 0; i < lepton_path_object_get_num_sections (object); i++)
  {
    section = lepton_path_object_get_section (object, i);

    switch (section->code) {
    case PATH_CURVETO:
      /* Two control point grips */
      section->x1 -= world_centerx; section->y1 -= world_centery;
      section->x2 -= world_centerx; section->y2 -= world_centery;
      lepton_point_rotate_90 (section->x1, section->y1, angle, &section->x1, &section->y1);
      lepton_point_rotate_90 (section->x2, section->y2, angle, &section->x2, &section->y2);
      section->x1 += world_centerx; section->y1 += world_centery;
      section->x2 += world_centerx; section->y2 += world_centery;
      /* Fall through */
    case PATH_MOVETO:
    case PATH_MOVETO_OPEN:
    case PATH_LINETO:
      /* Destination point grip */
      section->x3 -= world_centerx; section->y3 -= world_centery;
      lepton_point_rotate_90 (section->x3,
                              section->y3,
                              angle,
                              &section->x3,
                              &section->y3);
      section->x3 += world_centerx; section->y3 += world_centery;
      break;
    case PATH_END:
      break;
    }
  }
}


/*! \brief Mirror a path using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the path from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Line LeptonObject to mirror.
 */
void
lepton_path_object_mirror (int world_centerx,
                           int world_centery,
                           LeptonObject *object)
{
  LeptonPathSection *section;
  int i;

  g_return_if_fail (lepton_object_is_path (object));
  g_return_if_fail (object->path != NULL);

  for (i = 0; i < lepton_path_object_get_num_sections (object); i++)
  {
    section = lepton_path_object_get_section (object, i);

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
}


/*! \brief Calculate the bounds of the path
 *
 *  On failure, this function sets the bounds to empty.
 *
 *  \param [in]  object    The path to calculate bounds of.
 *  \param [out] bounds    The bounds of the path
 */
void
lepton_path_object_calculate_bounds (const LeptonObject *object,
                                     LeptonBounds *bounds)
{
  gint expand;
  gint i;

  lepton_bounds_init (bounds);

  g_return_if_fail (lepton_object_is_path (object));
  g_return_if_fail (object->path != NULL);

  /* Find the bounds of the path region */
  for (i = 0; i < lepton_path_object_get_num_sections (object); i++)
  {
    LeptonPathSection *section = lepton_path_object_get_section (object, i);

    switch (section->code) {
      case PATH_CURVETO:
        /* Bezier curves with this construction of control points will lie
         * within the convex hull of the control and curve end points */
        bounds->min_x = MIN (bounds->min_x, section->x1);
        bounds->min_y = MIN (bounds->min_y, section->y1);
        bounds->max_x = MAX (bounds->max_x, section->x1);
        bounds->max_y = MAX (bounds->max_y, section->y1);
        bounds->min_x = MIN (bounds->min_x, section->x2);
        bounds->min_y = MIN (bounds->min_y, section->y2);
        bounds->max_x = MAX (bounds->max_x, section->x2);
        bounds->max_y = MAX (bounds->max_y, section->y2);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        bounds->min_x = MIN (bounds->min_x, section->x3);
        bounds->min_y = MIN (bounds->min_y, section->y3);
        bounds->max_x = MAX (bounds->max_x, section->x3);
        bounds->max_y = MAX (bounds->max_y, section->y3);
        break;
      case PATH_END:
        break;
    }
  }

  expand = ceil (0.5 * G_SQRT2 * lepton_object_get_stroke_width (object));

  /* This isn't strictly correct, but a 1st order approximation */
  lepton_bounds_expand (bounds, bounds, expand, expand);
}

/*! \brief get the position of the first path point
 *  \par Function Description
 *  This function gets the position of the first point of an path object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
lepton_path_object_get_position (const LeptonObject *object,
                                 gint *x,
                                 gint *y)
{
  g_return_val_if_fail (lepton_object_is_path (object), FALSE);
  g_return_val_if_fail (object->path != NULL, FALSE);

  if (lepton_path_object_get_num_sections (object) == 0)
  {
    return FALSE;
  }

  if (x != NULL) {
    *x = object->path->sections[0].x3;
  }

  if (y != NULL) {
    *y = object->path->sections[0].y3;
  }

  return TRUE;
}

/*! \brief Calculates the distance between the given point and the closest
 *  point on the given path segment.
 *
 *  \param [in] object         The path LeptonObject.
 *  \param [in] x              The x coordinate of the given point.
 *  \param [in] y              The y coordinate of the given point.
 *  \param [in] force_solid    If true, force treating the object as solid.
 *  \param [in] include_hidden Take hidden text into account.
 *  \return The shortest distance from the object to the point.  With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double
lepton_path_object_shortest_distance (LeptonObject *object,
                                      int x,
                                      int y,
                                      int force_solid,
                                      gboolean include_hidden)
{
  g_return_val_if_fail (lepton_object_is_path (object), G_MAXDOUBLE);
  g_return_val_if_fail (object->path != NULL, G_MAXDOUBLE);

  int solid;

  solid = force_solid ||
    lepton_object_get_fill_type (object) != FILLING_HOLLOW;

  return lepton_path_shortest_distance (object->path, x, y, solid);
}


/*! \brief Insert a new path section into path object.
 *  \par Function description
 *  Given the object #LeptonObject, section #LeptonPathSection,
 *  and position id \a i, inserts the section into the object in
 *  specified position.  If the position number is less than zero
 *  or greater than the number of path object sections, appends
 *  the section to the end of the path.
 *
 *  \param [in] object  The path #LeptonObject to insert section into.
 *  \param [in] section The #LeptonPathSection to insert.
 *  \param [in] i       The position id of the new section in the path.
 *  \return The modified #LeptonObject.
 */
LeptonObject*
lepton_path_object_insert_section (LeptonObject *object,
                                   LeptonPathSection *section,
                                   int i)
{
  LeptonPath *path = object->path;

  /* Start making changes. */
  lepton_object_emit_pre_change_notify (object);

  /* Make sure there's enough space for the new element. */
  if (path->num_sections == path->num_sections_max) {
    path->sections =
      (LeptonPathSection*) g_realloc (path->sections,
                                      (path->num_sections_max <<= 1) *
                                      sizeof (LeptonPathSection));
  }

  /* Move path contents to make a gap in the right place. */
  if ((i < 0) || (i > path->num_sections)) {
    i = path->num_sections;
  } else {
    memmove (&path->sections[i+1], &path->sections[i],
             sizeof (LeptonPathSection) * (path->num_sections - i));
  }

  path->num_sections++;
  path->sections[i] = *section;

  lepton_object_emit_change_notify (object);

  return object;
}


/*! \brief Remove a path section from path object.
 *  \par Function description
 *  Given the object #LeptonObject, and the section number \a i,
 *  removes the section with the specified number from the path
 *  object and returns the modified object.
 *
 *  \param [in] object  The path #LeptonObject to remove section from.
 *  \param [in] i       The id number of the section to remove.
 *  \return The modified #LeptonObject.
 */
LeptonObject*
lepton_path_object_remove_section (LeptonObject *object,
                                   int i)
{
  int num_sections = lepton_path_object_get_num_sections (object);

  if ((i < 0) || (i >= num_sections))
  {
    /* Index is invalid for path.  Return the object unchanged. */
    return object;
  }

  lepton_object_emit_pre_change_notify (object);

  /* If section is not last in path, remove section at index by
   * moving all sections above index one location down. */
  if (i + 1 != num_sections)
  {
    memmove (&object->path->sections[i],
             &object->path->sections[i+1],
             sizeof (LeptonPathSection) * (num_sections - i - 1));
  }

  /* Decrement the number of sections. */
  lepton_path_object_set_num_sections (object, num_sections - 1);

  lepton_object_emit_change_notify (object);

  return object;
}

/*! \brief Get the number of sections of path object.
 *
 *  \param [in] object The path #LeptonObject.
 *  \return The number of sections.
 */
int
lepton_path_object_get_num_sections (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_path (object), 0);
  g_return_val_if_fail (object->path != NULL, 0);

  return object->path->num_sections;
}

/*! \brief Set the number of sections of path object.
 *
 *  \param [in] object The path #LeptonObject.
 *  \param [in] num The new number of sections.
 */
void
lepton_path_object_set_num_sections (LeptonObject *object,
                                     int num)
{
  g_return_if_fail (lepton_object_is_path (object));
  g_return_if_fail (object->path != NULL);

  object->path->num_sections = num;
}
