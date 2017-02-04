/* gEDA - GPL Electronic Design Automation
 * libgeda - gEDA's library
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

/*! \file geda_text_object.c
 *
 *  \brief functions for the text and fonts
 *
 *  \par The text definitions
 *
 *  The text is stored and printed in several different representations.
 *
 *  In the gEDA files the text is just a string. It is stored unmodified
 *  in <b>OBJECT->text->string</b>.
 *
 *  If the string is an attribute with an equal sign as delimiter between
 *  an attribute name and an attribute value, then it is possible to
 *  hide some parts of the text. The still visible part of an attribute
 *  is stored in <b>OBJECT->text->disp_string</b>.
 *
 *  \image html o_text_text_overview.png
 *  \image latex o_text_text_overview.pdf "text overview" width=14cm
 */

#include <config.h>

#include <stdio.h>
#include <math.h>
#include <sys/stat.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "libgeda_priv.h"

/*! \brief Scale factor between legacy gschem font units and postscript points.
 *
 *  \par Description
 *  gschem fonts are nominally specified in points, however there is a
 *  difference in how the specified font size corresponds to the metrics of
 *  the font when compared to typical typographic usage.
 *
 *  The following factor was empirically determined to approximately match the
 *  cap-height between the legacy gschem font, and fonts rendered using pango.
 */
#define GEDA_FONT_FACTOR 1.3

/*! Size of a tab in characters */
int tab_in_chars = 8;


/*! \brief calculate and return the boundaries of a text object
 *
 *  The responsibility of calculating the bounds of any object should probably
 *  be moved to EdaRenderer. And, this method should not be a virtual method
 *  of GedaObject.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  object    a text object
 *  \param [out] bounds    the bounds of the text
 *  \return TRUE if successful, FALSE if unsuccessful
 */
gboolean
geda_text_object_calculate_bounds (TOPLEVEL *toplevel,
                                   const GedaObject *object,
                                   GedaBounds *bounds)
{
  geda_bounds_init (bounds);

  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->text != NULL, FALSE);
  g_return_val_if_fail (object->type == OBJ_TEXT, FALSE);
  g_return_val_if_fail (toplevel != NULL, FALSE);
  g_return_val_if_fail (toplevel->rendered_text_bounds_func != NULL, FALSE);

  return toplevel->rendered_text_bounds_func (toplevel->rendered_text_bounds_data,
                                              object,
                                              &bounds->min_x,
                                              &bounds->min_y,
                                              &bounds->max_x,
                                              &bounds->max_y);
}

/*! \brief Get the text alignment
 *
 *  \param [in] object The text object
 *  \return The text alignmemt
 */
gint
geda_text_object_get_alignment (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, LOWER_LEFT);
  g_return_val_if_fail (object->text != NULL, LOWER_LEFT);
  g_return_val_if_fail (object->type == OBJ_TEXT, LOWER_LEFT);
  g_return_val_if_fail (object->text->alignment >= LOWER_LEFT, LOWER_LEFT);
  g_return_val_if_fail (object->text->alignment <= UPPER_RIGHT, LOWER_LEFT);

  return object->text->alignment;
}

/*! \brief Get the text angle
 *
 *  \param [in] object The text object
 *  \return The text angle
 */
gint
geda_text_object_get_angle (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->text != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_TEXT, 0);

  return object->text->angle;
}

/*! \brief Get the insertion point of the text
 *
 *  This function returns FALSE when encountering a logic/programming error. In
 *  this case, both output parameters will remain uninitialized.
 *
 *  \param [in] object the object to get the position.
 *  \param [out] x the x coordinate of the insertion point [allow none]
 *  \param [out] y the y coordinate of the insertion point [allow none]
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
geda_text_object_get_position (const GedaObject *object, gint *x, gint *y)
{
  g_return_val_if_fail (object != NULL, FALSE);
  g_return_val_if_fail (object->text != NULL, FALSE);
  g_return_val_if_fail (object->type == OBJ_TEXT, FALSE);

  if (x != NULL) {
    *x = geda_text_object_get_x (object);
  }

  if (y != NULL) {
    *y = geda_text_object_get_y (object);
  }

  return TRUE;
}

/*! \brief Get the text size
 *
 *  \param [in] object The text object
 *  \return The text size
 */
gint
geda_text_object_get_size (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, DEFAULT_TEXT_SIZE);
  g_return_val_if_fail (object->text != NULL, DEFAULT_TEXT_SIZE);
  g_return_val_if_fail (object->type == OBJ_TEXT, DEFAULT_TEXT_SIZE);
  g_return_val_if_fail (object->text->size >= MINIMUM_TEXT_SIZE,
                        DEFAULT_TEXT_SIZE);

  return object->text->size;
}

/*! \brief Get the text size in postscript points
 *
 *  gEDA fonts are specified in a non-standard unit. This function applies an
 *  appopriate scaling to return the font size in postscript points.
 *
 *  \param [in] object The text object
 *  \return The text size in postscript points.
 */
gdouble
geda_text_object_get_size_in_points (const GedaObject *object)
{
  return GEDA_FONT_FACTOR * geda_text_object_get_size (object);
}

/*! \brief Get the text string
 *
 *  \param [in] object The text object
 *  \return Returns the text string. The returned value is owned by the text
 *  object -- do not free.
 */
const gchar*
geda_text_object_get_string (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->text != NULL, NULL);
  g_return_val_if_fail (object->text->string != NULL, NULL);
  g_return_val_if_fail (object->type == OBJ_TEXT, NULL);

  return object->text->string;
}

/*! \brief Get the x coordinate of the text insertion point
 *
 *  \param [in] object The text object
 *  \return The x coordinate of the insertion point
 */
gint
geda_text_object_get_x (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->text != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_TEXT, 0);

  return object->text->x;
}

/*! \brief Get the y coordinate of the text insertion point
 *
 *  \param [in] object The text object
 *  \return The y coodinate of the insertion point
 */
gint
geda_text_object_get_y (const GedaObject *object)
{
  g_return_val_if_fail (object != NULL, 0);
  g_return_val_if_fail (object->text != NULL, 0);
  g_return_val_if_fail (object->type == OBJ_TEXT, 0);

  return object->text->y;
}

/*! \brief Set the text alignment
 *
 *  In case of an invalid text alignment, the property remains unchanged.
 *
 *  \param [in,out] object The text object
 *  \param [in] alignment The text alignmemt
 */
void
geda_text_object_set_alignment (GedaObject *object, gint alignment)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);
  g_return_if_fail (alignment >= LOWER_LEFT);
  g_return_if_fail (alignment <= UPPER_RIGHT);

  object->text->alignment = alignment;
}

/*! \brief Set the text angle
 *
 *  The text angle must be orthagonal to an axis, i.e., the text angle must be
 *  a multiple of 90 degrees. In case of an invalid text angle, the property
 *  remains unchanged.
 *
 *  If the text angle is not normal [0,360), then the angle will be normalized.
 *
 *  \param [in,out] object The text object
 *  \param [in] angle The text angle in degrees.
 */
void
geda_text_object_set_angle (GedaObject *object, gint angle)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);
  g_return_if_fail (geda_angle_is_ortho (angle));

  object->text->angle = geda_angle_normalize (angle);
}

/*! \brief Set the text size
 *
 *  The text size must be greater than or equal to the MINUMUM_TEXT_SIZE. In
 *  the case of an invalid text size, the property remains unchanged.
 *
 *  \param [in,out] object The text object
 *  \param [in] size The text size
 */
void
geda_text_object_set_size (GedaObject *object, gint size)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);
  g_return_if_fail (size >= MINIMUM_TEXT_SIZE);

  object->text->size = size;
}

/*! \brief Set the x coordinate of the text insertion point
 *
 *  \param [in,out] object The text object
 *  \param [in] x the x coordinate of the text insertion point
 */
void
geda_text_object_set_x (GedaObject *object, gint x)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);

  object->text->x = x;
}

/*! \brief Set the y coordinate of the text insertion point
 *
 *  \param [in,out] object The text object
 *  \param [in] y the y coordinate of the text insertion point
 */
void
geda_text_object_set_y (GedaObject *object, gint y)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);

  object->text->y = y;
}

/*! \brief update the visible part of a string
 *  \par Function Description
 *  If a string is an attribute, then it is possible to hide
 *  the name or the value part of the attribute string.
 *  This functions updates the text->disp_string according
 *  to the object->show_name_value settings
 *
 *  \param [in] object  The OBJECT to update
 */
static void
update_disp_string (OBJECT *object)
{
  char *name = NULL;
  char *value = NULL;
  TEXT *text = object->text;

  g_free (text->disp_string);

  if (o_attrib_get_name_value (object, &name, &value)) {
    switch (object->show_name_value) {
      case (SHOW_NAME_VALUE):
        text->disp_string = g_strdup (text->string);
        break;

      case (SHOW_NAME):
        if (name[0] != '\0') {
          text->disp_string = g_strdup (name);
        } else {
          g_critical ("Got an improper attribute: %s\n",
                      text->string);
          text->disp_string = g_strdup ("invalid");
        }
        break;

      case (SHOW_VALUE):
        if (value[0] != '\0') {
          text->disp_string = g_strdup(value);
        } else {
          g_critical ("Got an improper attribute: %s\n",
                      text->string);
          text->disp_string = g_strdup ("invalid");
        }
        break;
    }

    text->name = g_intern_string (name);

    /* free the strings allocated by o_attrib_get_name_value */
    g_free(name);
    g_free(value);
  } else {
    text->disp_string = g_strdup (text->string);
    text->name = NULL;
  }
}

/*! \brief Creates a text OBJECT and the graphical objects representing it
 *  \par Function Description
 *  Create an OBJECT of type OBJ_TEXT.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [in]  type                   OBJ_TEXT (TODO: why bother)
 *  \param [in]  color                  The color of the text.
 *  \param [in]  x                      World x coord of text.
 *  \param [in]  y                      World y coord of text.
 *  \param [in]  alignment              How text bounding box aligns on (x, y).
 *  \param [in]  angle                  Angle at which text will appear.
 *  \param [in]  string                 The text (TODO: can be char const *)!
 *  \param [in]  size                   Text size.
 *  \param [in]  visibility             VISIBLE or INVISIBLE.
 *  \param [in]  show_name_value        SHOW_NAME_VALUE or friends.
 *  \return Pointer to text OBJECT.
 *
 *  \note
 *  Caller is responsible for string; this function allocates its own copy.
 */
GedaObject*
geda_text_object_new (TOPLEVEL *toplevel,
                      gint color,
                      gint x,
                      gint y,
                      gint alignment,
                      gint angle,
                      const gchar *string,
                      gint size,
                      gint visibility,
                      gint show_name_value)
{
  GedaObject *new_node=NULL;
  TEXT *text;

  g_return_val_if_fail (string != NULL, NULL);

  new_node = s_basic_new_object (OBJ_TEXT, "text");

  text = (TEXT *) g_malloc(sizeof(TEXT));

  text->string = g_strdup (string);
  text->disp_string = NULL; /* We'll fix this up later */
  text->length = strlen(string);
  text->size = size;
  text->alignment = alignment;
  text->x = x;
  text->y = y;
  text->angle = angle;
  text->name = NULL;

  new_node->text = text;

  new_node->color = color;
  o_set_visibility (toplevel, new_node, visibility);
  new_node->show_name_value = show_name_value;

  update_disp_string (new_node);

  /* Update bounding box */
  new_node->w_bounds_valid_for = NULL;

  return new_node;
}

/*! \brief read a text object from a char buffer
 *  \par Function Description
 *  This function reads a text object from the textbuffer \a tb and
 *  the text starting with the line \a firstline.
 *  If the line object was read successfully, a new object is
 *  create and appended to the \a object_list.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] first_line   the first line of the text
 *  \param [in] tb           a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list, or NULL on error.
 */
OBJECT*
o_text_read (TOPLEVEL *toplevel,
             const char *first_line,
             TextBuffer *tb,
             unsigned int release_ver,
             unsigned int fileformat_ver,
             GError **err)
{
  OBJECT *new_obj;
  char type;
  int x, y;
  int color;
  int size;
  int visibility;
  int show_name_value;
  int angle;
  int alignment;
  int num_lines = 0;
  int i;
  char* string = NULL;
  GString *textstr;

  if (fileformat_ver >= 1) {
    if (sscanf(first_line, "%c %d %d %d %d %d %d %d %d %d\n", &type, &x, &y,
	       &color, &size,
	       &visibility, &show_name_value,
	       &angle, &alignment, &num_lines) != 10) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse text object"));
      return NULL;
    }
  } else if (release_ver < VERSION_20000220) {
    /* yes, above less than (not less than and equal) is correct. The format */
    /* change occurred in 20000220 */
    if (sscanf(first_line, "%c %d %d %d %d %d %d %d\n", &type, &x, &y,
	       &color, &size,
	       &visibility, &show_name_value,
	       &angle) != 8) {
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse text object"));
      return NULL;
    }
    alignment = LOWER_LEFT; /* older versions didn't have this */
    num_lines = 1; /* only support a single line */
  } else {
    if (sscanf(first_line, "%c %d %d %d %d %d %d %d %d\n", &type, &x, &y,
	       &color, &size,
	       &visibility, &show_name_value,
           &angle, &alignment) != 9) {
      g_set_error (err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse text object"));
      return NULL;
    }
    num_lines = 1; /* only support a single line */
  }

  if (size < MINIMUM_TEXT_SIZE) {
    s_log_message (_("Found an invalid text size [ %s ]\n"), first_line);
    size = DEFAULT_TEXT_SIZE;
    s_log_message (_("Setting text size to %d\n"), size);
  }

  if (!geda_angle_is_ortho (angle)) {
    s_log_message (_("Found an unsupported text angle [ %s ]\n"), first_line);
    angle = geda_angle_make_ortho (angle);
    s_log_message (_("Setting angle to %d\n"), angle);
  }

  switch(alignment) {
    case(LOWER_LEFT):
    case(MIDDLE_LEFT):
    case(UPPER_LEFT):
    case(LOWER_MIDDLE):
    case(MIDDLE_MIDDLE):
    case(UPPER_MIDDLE):
    case(LOWER_RIGHT):
    case(MIDDLE_RIGHT):
    case(UPPER_RIGHT):

    break;

    default:
      s_log_message (_("Found an unsupported text alignment [ %s ]\n"),
                     first_line);
      alignment = LOWER_LEFT;
      s_log_message(_("Setting alignment to LOWER_LEFT\n"));
      break;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message(_("Found an invalid color [ %s ]\n"), first_line);
    color = DEFAULT_COLOR;
    s_log_message(_("Setting color to default color\n"));
  }

  g_assert(num_lines && num_lines > 0);

  textstr = g_string_new ("");
  for (i = 0; i < num_lines; i++) {
    const gchar *line;

    line = s_textbuffer_next_line (tb);

    if (line == NULL) {
      g_string_free (textstr, TRUE);
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Unexpected end-of-file after %d lines"), i);
      return NULL;
    }

    textstr = g_string_append (textstr, line);
  }
  /* retrieve the character string from the GString */
  string = g_string_free (textstr, FALSE);

  string = geda_string_remove_ending_newline (string);

  /* convert the character string to UTF-8 if necessary */
  if (!g_utf8_validate (string, -1, NULL)) {
    /* if it is not utf-8, it is ISO_8859-15 */
    gchar *tmp = g_convert (string, strlen (string),
                            "UTF-8", "ISO_8859-15",
                            NULL, NULL, NULL);
    if (tmp == NULL) {
      fprintf (stderr, "Failed to convert text string to UTF-8: %s.\n",
               string);
    } else {
      /* successfully converted string, now use tmp as string */
      g_free (string);
      string = tmp;
    }
  }

  new_obj = geda_text_object_new (toplevel,
                                  color,
                                  x,
                                  y,
                                  alignment,
                                  angle,
                                  string,
                                  size,
                                  visibility,
                                  show_name_value);
  g_free(string);

  return new_obj;
}


/*! \brief Create a string representation of the text object
 *  \par Function Description
 *  This function takes a text \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] object the text object
 *  \return the string representation of the text object
 */
gchar*
geda_text_object_to_buffer (const GedaObject *object)
{
  const gchar *string;

  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->text != NULL, NULL);
  g_return_val_if_fail (object->type == OBJ_TEXT, NULL);

  string = geda_text_object_get_string (object);

  g_return_val_if_fail (string != NULL, NULL);

  return g_strdup_printf ("%c %d %d %d %d %d %d %d %d %d\n%s",
                          OBJ_TEXT,
                          geda_text_object_get_x (object),
                          geda_text_object_get_y (object),
                          geda_object_get_color (object),
                          geda_text_object_get_size (object),
                          geda_object_get_visible (object),
                          object->show_name_value,
                          geda_text_object_get_angle (object),
                          geda_text_object_get_alignment (object),
                          o_text_num_lines (string),
                          string);
}

/*! \brief recreate the graphics of a text object
 *  \par Function Description
 *  This function updates the underlying primary of the text object
 *  \a o_current.
 *
 *  \param toplevel  The TOPLEVEL object
 *  \param o_current The text object to update
 */
void
o_text_recreate (TOPLEVEL *toplevel, OBJECT *o_current)
{
  o_emit_pre_change_notify (toplevel, o_current);
  update_disp_string (o_current);
  o_current->w_bounds_valid_for = NULL;
  o_emit_change_notify (toplevel, o_current);
}

/*! \brief move a text object
 *  \par Function Description
 *  This function changes the position of a text object.
 *
 *  \param [ref] object  The text GedaObject to be moved
 *  \param [in]  dx      The x-distance to move the object
 *  \param [in]  dy      The y-distance to move the object
 */
void
geda_text_object_translate (GedaObject *object, int dx, int dy)
{
  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);

  object->text->x = object->text->x + dx;
  object->text->y = object->text->y + dy;

  /* Update bounding box */
  object->w_bounds_valid_for = NULL;
}

/*! \brief create a copy of a text object
 *  \par Function Description
 *  This function creates a copy of the text object \a o_current.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] object    The object that is copied
 *  \return a new text object
 */
GedaObject*
geda_text_object_copy (TOPLEVEL *toplevel, const GedaObject *object)
{
  GedaObject *new_obj;

  g_return_val_if_fail (object != NULL, NULL);
  g_return_val_if_fail (object->text != NULL, NULL);
  g_return_val_if_fail (object->type == OBJ_TEXT, NULL);

  new_obj = geda_text_object_new (toplevel,
                                  object->color,
                                  object->text->x,
                                  object->text->y,
                                  object->text->alignment,
                                  object->text->angle,
                                  object->text->string,
                                  object->text->size,
                                  geda_object_get_visible (object),
                                  object->show_name_value);

  return new_obj;
}

/*! \brief rotate a text object around a centerpoint
 *  \par Function Description
 *  This function rotates a text \a object around the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotate the text object
 *  \param [in] object        The text object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void
geda_text_object_rotate (TOPLEVEL *toplevel,
                         int world_centerx,
                         int world_centery,
                         int angle,
                         OBJECT *object)
{
  int x, y;
  int newx, newy;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);
  g_return_if_fail (geda_angle_is_ortho (angle));

  object->text->angle = geda_angle_normalize (object->text->angle + angle);

  x = object->text->x + (-world_centerx);
  y = object->text->y + (-world_centery);

  geda_point_rotate_90 (x, y, angle, &newx, &newy);

  x = newx + (world_centerx);
  y = newy + (world_centery);

  geda_text_object_translate (object, x-object->text->x, y-object->text->y);

  o_text_recreate(toplevel, object);
}


/*! \brief mirror a text object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a text \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] toplevel      The TOPLEVEL object
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The text object
 */
void
geda_text_object_mirror (TOPLEVEL *toplevel,
                         int world_centerx,
                         int world_centery,
                         OBJECT *object)
{
  int origx, origy;
  int x, y;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);

  origx = object->text->x;
  origy = object->text->y;

  x = origx + (-world_centerx);
  y = origy + (-world_centery);

  if ((object->text->angle%180)==0) {
    switch(object->text->alignment) {
      case(LOWER_LEFT):
        object->text->alignment=LOWER_RIGHT;
        break;

      case(MIDDLE_LEFT):
        object->text->alignment=MIDDLE_RIGHT;
        break;

      case(UPPER_LEFT):
        object->text->alignment=UPPER_RIGHT;
        break;

      case(LOWER_RIGHT):
        object->text->alignment=LOWER_LEFT;
        break;

      case(MIDDLE_RIGHT):
        object->text->alignment=MIDDLE_LEFT;
        break;

      case(UPPER_RIGHT):
        object->text->alignment=UPPER_LEFT;
        break;

      default:
        break;
    }
  } else {
    switch(object->text->alignment) {
      case(LOWER_LEFT):
      object->text->alignment=UPPER_LEFT;
      break;

      case(UPPER_LEFT):
      object->text->alignment=LOWER_LEFT;
      break;

      case(LOWER_RIGHT):
      object->text->alignment=UPPER_RIGHT;
      break;

      case(UPPER_RIGHT):
      object->text->alignment=LOWER_RIGHT;
      break;

      case(LOWER_MIDDLE):
      object->text->alignment=UPPER_MIDDLE;
      break;

      case(UPPER_MIDDLE):
      object->text->alignment=LOWER_MIDDLE;
      break;

      default:
      break;
    }
  }

  object->text->x = -x + (world_centerx);
  object->text->y =  y + (world_centery);

  o_text_recreate(toplevel, object);
}

/*! \brief Calculates the distance between the given point and the closest
 *  point on the text.
 *
 *  This function will calculate the distance to the text regardless
 *  if the text is visible or not.
 *
 *  \param [in] toplevel     The TOPLEVEL object.
 *  \param [in] object       The text OBJECT.
 *  \param [in] x            The x coordinate of the given point.
 *  \param [in] y            The y coordinate of the given point.
 *  \param [in] force_solid  If true, force treating the object as solid.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  With an invalid parameter, this funciton
 *  returns G_MAXDOUBLE.
 */
double
geda_text_object_shortest_distance (TOPLEVEL *toplevel,
                                    OBJECT *object,
                                    int x,
                                    int y,
                                    int force_solid)
{
  int left, top, right, bottom;
  double dx, dy;

  g_return_val_if_fail (object->text != NULL, G_MAXDOUBLE);

  if (!geda_object_calculate_visible_bounds(toplevel, object,
                                      &left, &top, &right, &bottom))
    return G_MAXDOUBLE;

  dx = min (x - left, right - x);
  dy = min (y - top, bottom - y);

  dx = min (dx, 0);
  dy = min (dy, 0);

  return hypot (dx, dy);
}

/*! \brief Set the string displayed by a text object.
 *  \par Function Description
 *  Updates the text object with a new text string.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [in]  obj                   The text object.
 *  \param [in]  new_string            The new value.
 */
void
o_text_set_string (TOPLEVEL *toplevel, OBJECT *obj, const gchar *new_string)
{
  g_return_if_fail (toplevel != NULL);
  g_return_if_fail (obj != NULL);
  g_return_if_fail (obj->type == OBJ_TEXT);
  g_return_if_fail (obj->text != NULL);
  g_return_if_fail (new_string != NULL);

  g_free (obj->text->string);
  obj->text->string = g_strdup (new_string);

  o_text_recreate (toplevel, obj);
}

/*! \brief Set the font-renderer-specific bounds function.
 *  \par Function Description
 *  Set the function to be used to calculate text bounds for a given
 *  #TOPLEVEL.
 *
 *  \param [in] toplevel     The TOPLEVEL object
 *  \param [in] func      Function to use.
 *  \param [in] user_data User data to be passed to the function.
 */
void
o_text_set_rendered_bounds_func (TOPLEVEL *toplevel,
                                 RenderedBoundsFunc func,
                                 void *user_data)
{
  toplevel->rendered_text_bounds_func = func;
  toplevel->rendered_text_bounds_data = user_data;
}
