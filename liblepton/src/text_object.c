/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2021 Lepton EDA Contributors
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

/*! \file text_object.c
 *
 *  \brief functions for the text and fonts
 *
 *  \par The text definitions
 *
 *  The text is stored and printed in several different representations.
 *
 *  In the gEDA files the text is just a string. It is stored unmodified
 *  in <b>LeptonObject->text->string</b>.
 *
 *  If the string is an attribute with an equal sign as delimiter between
 *  an attribute name and an attribute value, then it is possible to
 *  hide some parts of the text. The still visible part of an attribute
 *  is stored in <b>LeptonObject->text->disp_string</b>.
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

#include "liblepton_priv.h"
#include <liblepton/edarenderer.h>


/*! \brief Scale factor between legacy lepton-schematic font units
 *  and postscript points.
 *
 *  \par Description
 *  lepton-schematic fonts are nominally specified in points, however there is a
 *  difference in how the specified font size corresponds to the metrics of
 *  the font when compared to typical typographic usage.
 *
 *  The following factor was empirically determined to
 *  approximately match the cap-height between the legacy
 *  lepton-schematic font, and fonts rendered using pango.
 */
#define GEDA_FONT_FACTOR 1.3

/*! Size of a tab in characters */
int tab_in_chars = 8;


/*! \brief calculate and return the boundaries of a text object
 *
 *  The responsibility of calculating the bounds of any object should probably
 *  be moved to EdaRenderer. And, this method should not be a virtual method
 *  of LeptonObject.
 *
 *  \param [in]  object         a text object
 *  \param [in]  include_hidden if hidden text should be taken into
 *                              account
 *  \param [out] bounds         the bounds of the text
 *  \return TRUE if successful, FALSE if unsuccessful
 */
gboolean
lepton_text_object_calculate_bounds (const LeptonObject *object,
                                     gboolean include_hidden,
                                     LeptonBounds *bounds)
{
  if (! (o_is_visible (object) || include_hidden))
    return FALSE;

  lepton_bounds_init (bounds);

  g_return_val_if_fail (lepton_object_is_text (object), FALSE);
  g_return_val_if_fail (object->text != NULL, FALSE);

  double t, l, r, b;
  gboolean result = eda_renderer_get_text_user_bounds (object,
                                                       include_hidden,
                                                       &l, &t, &r, &b);

  /* Round bounds to nearest integer */
  bounds->min_x = lrint (fmin (l, r));
  bounds->min_y = lrint (fmin (t, b));
  bounds->max_x = lrint (fmax (l, r));
  bounds->max_y = lrint (fmax (t, b));

  return result;
}

/*! \brief Get the text alignment
 *
 *  \param [in] object The text object
 *  \return The text alignmemt
 */
gint
lepton_text_object_get_alignment (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_text (object), LOWER_LEFT);
  g_return_val_if_fail (object->text != NULL, LOWER_LEFT);
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
lepton_text_object_get_angle (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_text (object), 0);
  g_return_val_if_fail (object->text != NULL, 0);

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
lepton_text_object_get_position (const LeptonObject *object,
                                 gint *x,
                                 gint *y)
{
  g_return_val_if_fail (lepton_object_is_text (object), FALSE);
  g_return_val_if_fail (object->text != NULL, FALSE);

  if (x != NULL) {
    *x = lepton_text_object_get_x (object);
  }

  if (y != NULL) {
    *y = lepton_text_object_get_y (object);
  }

  return TRUE;
}

/*! \brief Get the text size
 *
 *  \param [in] object The text object
 *  \return The text size
 */
gint
lepton_text_object_get_size (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_text (object), DEFAULT_TEXT_SIZE);
  g_return_val_if_fail (object->text != NULL, DEFAULT_TEXT_SIZE);
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
lepton_text_object_get_size_in_points (const LeptonObject *object)
{
  return GEDA_FONT_FACTOR * lepton_text_object_get_size (object);
}

/*! \brief Get the text string
 *
 *  \param [in] object The text object
 *  \return Returns the text string. The returned value is owned by the text
 *  object -- do not free.
 */
const gchar*
lepton_text_object_get_string (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_text (object), NULL);
  g_return_val_if_fail (object->text != NULL, NULL);
  g_return_val_if_fail (object->text->string != NULL, NULL);

  return object->text->string;
}

/*! \brief Get the x coordinate of the text insertion point
 *
 *  \param [in] object The text object
 *  \return The x coordinate of the insertion point
 */
gint
lepton_text_object_get_x (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_text (object), 0);
  g_return_val_if_fail (object->text != NULL, 0);

  return object->text->x;
}

/*! \brief Get the y coordinate of the text insertion point
 *
 *  \param [in] object The text object
 *  \return The y coodinate of the insertion point
 */
gint
lepton_text_object_get_y (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_text (object), 0);
  g_return_val_if_fail (object->text != NULL, 0);

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
lepton_text_object_set_alignment (LeptonObject *object,
                                  gint alignment)
{
  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);
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
lepton_text_object_set_angle (LeptonObject *object,
                              gint angle)
{
  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (lepton_angle_is_ortho (angle));

  object->text->angle = lepton_angle_normalize (angle);
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
lepton_text_object_set_size (LeptonObject *object,
                             gint size)
{
  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (size >= MINIMUM_TEXT_SIZE);

  object->text->size = size;
}

/*! \brief Set the x coordinate of the text insertion point
 *
 *  \param [in,out] object The text object
 *  \param [in] x the x coordinate of the text insertion point
 */
void
lepton_text_object_set_x (LeptonObject *object,
                          gint x)
{
  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);

  object->text->x = x;
}

/*! \brief Set the y coordinate of the text insertion point
 *
 *  \param [in,out] object The text object
 *  \param [in] y the y coordinate of the text insertion point
 */
void
lepton_text_object_set_y (LeptonObject *object,
                          gint y)
{
  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);

  object->text->y = y;
}

/*! \brief update the visible part of a string
 *  \par Function Description
 *  If a string is an attribute, then it is possible to hide
 *  the name or the value part of the attribute string.
 *  This functions updates the text->disp_string according
 *  to the object->show_name_value settings
 *
 *  \param [in] object  The LeptonObject to update
 */
static void
update_disp_string (LeptonObject *object)
{
  char *name = NULL;
  char *value = NULL;
  LeptonText *text = object->text;

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
          g_critical ("Got an improper attribute: %1$s\n",
                      text->string);
          text->disp_string = g_strdup ("invalid");
        }
        break;

      case (SHOW_VALUE):
        if (value[0] != '\0') {
          text->disp_string = g_strdup(value);
        } else {
          g_critical ("Got an improper attribute: %1$s\n",
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

/*! \brief Creates a text LeptonObject and the graphical objects representing it
 *  \par Function Description
 *  Create an LeptonObject of type OBJ_TEXT.
 *
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
 *  \return Pointer to text LeptonObject.
 *
 *  \note
 *  Caller is responsible for string; this function allocates its own copy.
 */
LeptonObject*
lepton_text_object_new (gint color,
                        gint x,
                        gint y,
                        gint alignment,
                        gint angle,
                        const gchar *string,
                        gint size,
                        gint visibility,
                        gint show_name_value)
{
  LeptonObject *new_node=NULL;
  LeptonText *text;

  g_return_val_if_fail (string != NULL, NULL);

  new_node = s_basic_new_object (OBJ_TEXT, "text");

  text = (LeptonText *) g_malloc (sizeof (LeptonText));

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

  lepton_object_set_color (new_node, color);
  o_set_visibility (new_node, visibility);
  new_node->show_name_value = show_name_value;

  update_disp_string (new_node);

  return new_node;
}

/*! \brief read a text object from a char buffer
 *  \par Function Description
 *  This function reads a text object from the textbuffer \a tb and
 *  the text starting with the line \a firstline.
 *  If the line object was read successfully, a new object is
 *  create and appended to the \a object_list.
 *
 *  \param [in] first_line   the first line of the text
 *  \param [in] tb           a text buffer (usually a line of a schematic file)
 *  \param [in] release_ver  The release number gEDA
 *  \param [in] fileformat_ver a integer value of the file format
 *  \return The object list, or NULL on error.
 */
LeptonObject*
o_text_read (const char *first_line,
             TextBuffer *tb,
             unsigned int release_ver,
             unsigned int fileformat_ver,
             GError **err)
{
  LeptonObject *new_obj;
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
    g_message (_("Found an invalid text size [ %1$s ]"), first_line);
    size = DEFAULT_TEXT_SIZE;
    g_message (_("Setting text size to %1$d."), size);
  }

  if (!lepton_angle_is_ortho (angle))
  {
    g_message (_("Found an unsupported text angle [ %1$s ]"), first_line);
    angle = lepton_angle_make_ortho (angle);
    g_message (_("Setting angle to %1$d."), angle);
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
      g_message (_("Found an unsupported text alignment [ %1$s ]"),
                 first_line);
      alignment = LOWER_LEFT;
      g_message (_("Setting alignment to LOWER_LEFT."));
      break;
  }

  if (!color_id_valid (color)) {
    g_message(_("Found an invalid color [ %1$s ]"), first_line);
    color = default_color_id();
    g_message(_("Setting color to default color."));
  }

  g_assert(num_lines && num_lines > 0);

  textstr = g_string_new ("");
  for (i = 0; i < num_lines; i++) {
    const gchar *line;

    line = s_textbuffer_next_line (tb);

    if (line == NULL) {
      g_string_free (textstr, TRUE);
      g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Unexpected end-of-file after %1$d lines"), i);
      return NULL;
    }

    textstr = g_string_append (textstr, line);
  }
  /* retrieve the character string from the GString */
  string = g_string_free (textstr, FALSE);

  string = lepton_str_remove_ending_newline (string);

  /* convert the character string to UTF-8 if necessary */
  if (!g_utf8_validate (string, -1, NULL)) {
    /* if it is not utf-8, it is ISO_8859-15 */
    gchar *tmp = g_convert (string, strlen (string),
                            "UTF-8", "ISO_8859-15",
                            NULL, NULL, NULL);
    if (tmp == NULL) {
      fprintf (stderr, "Failed to convert text string to UTF-8: %1$s.\n",
               string);
    } else {
      /* successfully converted string, now use tmp as string */
      g_free (string);
      string = tmp;
    }
  }

  new_obj = lepton_text_object_new (color,
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
lepton_text_object_to_buffer (const LeptonObject *object)
{
  const gchar *string;

  g_return_val_if_fail (lepton_object_is_text (object), NULL);
  g_return_val_if_fail (object->text != NULL, NULL);

  string = lepton_text_object_get_string (object);

  g_return_val_if_fail (string != NULL, NULL);

  return g_strdup_printf ("%c %d %d %d %d %d %d %d %d %d\n%s",
                          lepton_object_get_type (object),
                          lepton_text_object_get_x (object),
                          lepton_text_object_get_y (object),
                          lepton_object_get_color (object),
                          lepton_text_object_get_size (object),
                          lepton_object_get_visible (object),
                          object->show_name_value,
                          lepton_text_object_get_angle (object),
                          lepton_text_object_get_alignment (object),
                          o_text_num_lines (string),
                          string);
}

/*! \brief recreate the graphics of a text object
 *  \par Function Description
 *  This function updates the underlying primary of the text object
 *  \a o_current.
 *
 *  \param o_current The text object to update
 */
void
o_text_recreate (LeptonObject *o_current)
{
  o_emit_pre_change_notify (o_current);
  update_disp_string (o_current);
  o_emit_change_notify (o_current);
}

/*! \brief move a text object
 *  \par Function Description
 *  This function changes the position of a text object.
 *
 *  \param [ref] object  The text LeptonObject to be moved
 *  \param [in]  dx      The x-distance to move the object
 *  \param [in]  dy      The y-distance to move the object
 */
void
lepton_text_object_translate (LeptonObject *object,
                              int dx,
                              int dy)
{
  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);

  object->text->x = object->text->x + dx;
  object->text->y = object->text->y + dy;
}

/*! \brief create a copy of a text object
 *  \par Function Description
 *  This function creates a copy of the text object \a o_current.
 *
 *  \param [in] object    The object that is copied
 *  \return a new text object
 */
LeptonObject*
lepton_text_object_copy (const LeptonObject *object)
{
  LeptonObject *new_obj;

  g_return_val_if_fail (lepton_object_is_text (object), NULL);
  g_return_val_if_fail (object->text != NULL, NULL);

  new_obj = lepton_text_object_new (lepton_object_get_color (object),
                                    object->text->x,
                                    object->text->y,
                                    object->text->alignment,
                                    object->text->angle,
                                    object->text->string,
                                    object->text->size,
                                    lepton_object_get_visible (object),
                                    object->show_name_value);

  return new_obj;
}

/*! \brief rotate a text object around a centerpoint
 *  \par Function Description
 *  This function rotates a text \a object around the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] world_centerx x-coord of the rotation center
 *  \param [in] world_centery y-coord of the rotation center
 *  \param [in] angle         The angle to rotate the text object
 *  \param [in] object        The text object
 *  \note only steps of 90 degrees are allowed for the \a angle
 */
void
lepton_text_object_rotate (int world_centerx,
                           int world_centery,
                           int angle,
                           LeptonObject *object)
{
  int x, y;
  int newx, newy;

  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (lepton_angle_is_ortho (angle));

  object->text->angle = lepton_angle_normalize (object->text->angle + angle);

  x = object->text->x + (-world_centerx);
  y = object->text->y + (-world_centery);

  lepton_point_rotate_90 (x, y, angle, &newx, &newy);

  x = newx + (world_centerx);
  y = newy + (world_centery);

  lepton_text_object_translate (object, x-object->text->x, y-object->text->y);

  o_text_recreate (object);
}


/*! \brief mirror a text object horizontaly at a centerpoint
 *  \par Function Description
 *  This function mirrors a text \a object horizontaly at the point
 *  (\a world_centerx, \a world_centery).
 *
 *  \param [in] world_centerx x-coord of the mirror position
 *  \param [in] world_centery y-coord of the mirror position
 *  \param [in] object        The text object
 */
void
lepton_text_object_mirror (int world_centerx,
                           int world_centery,
                           LeptonObject *object)
{
  int origx, origy;
  int x, y;

  g_return_if_fail (lepton_object_is_text (object));
  g_return_if_fail (object->text != NULL);

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

  o_text_recreate (object);
}

/*! \brief Calculates the distance between the given point and the closest
 *  point on the text.
 *
 *  This function will calculate the distance to the text regardless
 *  if the text is visible or not.
 *
 *  \param [in] object         The text LeptonObject.
 *  \param [in] x              The x coordinate of the given point.
 *  \param [in] y              The y coordinate of the given point.
 *  \param [in] force_solid    If true, force treating the object as solid.
 *  \param [in] include_hidden Take hidden text into account.
 *  \return The shortest distance from the object to the point. If the
 *  distance cannot be calculated, this function returns a really large
 *  number (G_MAXDOUBLE).  With an invalid parameter, this funciton
 *  returns G_MAXDOUBLE.
 */
double
lepton_text_object_shortest_distance (LeptonObject *object,
                                      int x,
                                      int y,
                                      int force_solid,
                                      gboolean include_hidden)
{
  int left, top, right, bottom;
  double dx, dy;

  g_return_val_if_fail (object->text != NULL, G_MAXDOUBLE);

  if (!lepton_object_calculate_visible_bounds (object,
                                               include_hidden,
                                               &left,
                                               &top,
                                               &right,
                                               &bottom))
    return G_MAXDOUBLE;

  dx = MIN (x - left, right - x);
  dy = MIN (y - top, bottom - y);

  dx = MIN (dx, 0);
  dy = MIN (dy, 0);

  return hypot (dx, dy);
}

/*! \brief Set the string displayed by a text object.
 *  \par Function Description
 *  Updates the text object with a new text string.
 *
 *  \param [in]  obj                   The text object.
 *  \param [in]  new_string            The new value.
 */
void
o_text_set_string (LeptonObject *obj,
                   const gchar *new_string)
{
  g_return_if_fail (lepton_object_is_text (obj));
  g_return_if_fail (obj->text != NULL);
  g_return_if_fail (new_string != NULL);

  g_free (obj->text->string);
  obj->text->string = g_strdup (new_string);

  o_text_recreate (obj);
}
