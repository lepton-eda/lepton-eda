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

/*! \file o_text_basic.c
 *  \brief functions for the text and fonts
 *
 *  \par The font definitions
 *
 *  Each letter of the font is defined in a single font symbol file. In
 *  the font symbol file, the character width is defined in the second
 *  line. The first line contains the file format version.
 *
 *  All remaining lines are basic graphical lines. They build the
 *  appearance of the character.
 *
 *  \image html o_text_font_overview.png
 *  \image latex o_text_font_overview.pdf "font overview" width=14cm
 *
 *  The height of capital characters in the font files is 26. The size
 *  of small letters is 16. The space below the zero line is used by
 *  characters like <b>g</b>, <b>p</b> or <b>q</b>. The space above 26
 *  is used by diacritic marks like accents, breve, circumflex mostly in
 *  european characters.
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
 *
 *  To draw the text in gschem, the string is interpreted and converted
 *  to a list of basic graphical objects. The basic line objects are
 *  collected from the font character objects.
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

/*! \brief update the visible part of a string
 *  \par Function Description
 *  If a string is an attribute, then it is possible to hide
 *  the name or the value part of the attribute string.
 *  This functions updates the text->disp_string according
 *  to the object->show_name_value settings
 *
 *  \param [in] object  The OBJECT to update
 */
static void update_disp_string (OBJECT *object)
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
    /* free the strings allocated by o_attrib_get_name_value */
    g_free(name);
    g_free(value);
  } else {
    text->disp_string = g_strdup (text->string);
  }
}

/*! \brief calculate and return the boundaries of a text object
 *  \par Function Description
 *  This function calculates the object boudaries of a text \a object.
 *
 *  \param [in]  toplevel  The TOPLEVEL object.
 *  \param [in]  o_current a text object
 *  \param [out] left      the left world coord
 *  \param [out] top       the top world coord
 *  \param [out] right     the right world coord
 *  \param [out] bottom    the bottom world coord
 */
int world_get_text_bounds(TOPLEVEL *toplevel, OBJECT *o_current, int *left,
                          int *top, int *right, int *bottom)
{
  if (toplevel->rendered_text_bounds_func != NULL) {
    return
      toplevel->rendered_text_bounds_func (toplevel->rendered_text_bounds_data,
                                           o_current,
                                           left, top, right, bottom);
  }

  return FALSE;
}

/*! \brief get the position of a text object
 *  \par Function Description
 *  This function gets the position of the base point of a text object.
 *
 *  \param [in] toplevel The toplevel environment.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \param [in] object   The object to get the position.
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean o_text_get_position (TOPLEVEL *toplevel, gint *x, gint *y,
                              OBJECT *object)
{
  *x = object->text->x;
  *y = object->text->y;
  return TRUE;
}


/*! \brief count the lines of a text string
 *  \par Function Description
 *  This function just counts the number of lines that are
 *  in the \a string.

 *  \param [in] string  text string to count the lines
 *  \return the number of lines
 */
int o_text_num_lines(const char *string)
{
  int line_count = 0;
  const gchar *aux;
  gunichar current_char;

  if (string == NULL) {
    return 0;
  }

  /* if it's not null, then we have at least one line */
  line_count++;
  /* Count how many \n are in the string */
  aux = string;
  while (aux && ((gunichar) (*aux) != 0) ) {
    current_char = g_utf8_get_char_validated(aux, -1);
    if (current_char == '\n')
      line_count++;
    aux = g_utf8_find_next_char(aux, NULL);
  }

  return (line_count);
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
OBJECT *o_text_new(TOPLEVEL *toplevel,
		   char type, int color, int x, int y, int alignment,
		   int angle, const char *string, int size,
		   int visibility, int show_name_value)
{
  OBJECT *new_node=NULL;
  TEXT *text;

  if (string == NULL) {
    return(NULL);
  }

  new_node = s_basic_new_object(type, "text");

  text = (TEXT *) g_malloc(sizeof(TEXT));

  text->string = g_strdup (string);
  text->disp_string = NULL; /* We'll fix this up later */
  text->length = strlen(string);
  text->size = size;
  text->alignment = alignment;
  text->x = x;
  text->y = y;
  text->angle = angle;

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
OBJECT *o_text_read (TOPLEVEL *toplevel,
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

  if (size == 0) {
    s_log_message(_("Found a zero size text string [ %c %d %d %d %d %d %d %d %d ]\n"), type, x, y, color, size, visibility, show_name_value, angle, alignment);
  }

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
    break;

    default:
      s_log_message(_("Found an unsupported text angle [ %c %d %d %d %d %d %d %d %d ]\n"),
                    type, x, y, color, size, visibility, show_name_value, angle, alignment);
      s_log_message(_("Setting angle to 0\n"));
      angle=0;
      break;

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
      s_log_message(_("Found an unsupported text alignment [ %c %d %d %d %d %d %d %d %d ]\n"),
                    type, x, y, color, size, visibility, show_name_value, angle, alignment);
      s_log_message(_("Setting alignment to LOWER_LEFT\n"));
      alignment = LOWER_LEFT;
      break;
  }

  if (color < 0 || color > MAX_COLORS) {
    s_log_message(_("Found an invalid color [ %s ]\n"), first_line);
    s_log_message(_("Setting color to default color\n"));
    color = DEFAULT_COLOR;
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

  string = remove_last_nl(string);

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

  new_obj = o_text_new(toplevel, type, color, x, y,
                       alignment, angle, string,
                       size, visibility, show_name_value);
  g_free(string);

  return new_obj;
}


/*! \brief Create a string representation of the text object
 *  \par Function Description
 *  This function takes a text \a object and return a string
 *  according to the file format definition.
 *
 *  \param [in] toplevel  a TOPLEVEL structure
 *  \param [in] object  a text OBJECT
 *  \return the string representation of the text OBJECT
 */
char *o_text_save(TOPLEVEL *toplevel, OBJECT *object)
{
  int x, y;
  int size;
  char *string;
  char *buf;
  int num_lines;

  x = object->text->x;
  y = object->text->y;

  string = object->text->string;
  size = object->text->size;

  /* string can have multiple lines (seperated by \n's) */
  num_lines = o_text_num_lines(string);

  buf = g_strdup_printf ("%c %d %d %d %d %d %d %d %d %d\n%s", object->type,
                         x, y, object->color, size,
                         o_is_visible (toplevel, object) ? VISIBLE : INVISIBLE,
                         object->show_name_value, object->text->angle,
                         object->text->alignment, num_lines, string);

  return(buf);
}

/*! \brief recreate the graphics of a text object
 *  \par Function Description
 *  This function updates the underlying primary of the text object
 *  \a o_current.
 *
 *  \param toplevel  The TOPLEVEL object
 *  \param o_current The text object to update
 */
void o_text_recreate(TOPLEVEL *toplevel, OBJECT *o_current)
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
 *  \param [in] o_current    The object that is copied
 *  \return a new text object
 */
OBJECT *o_text_copy(TOPLEVEL *toplevel, OBJECT *o_current)
{
  OBJECT *new_obj;

  new_obj = o_text_new (toplevel, OBJ_TEXT, o_current->color,
                        o_current->text->x, o_current->text->y,
                        o_current->text->alignment,
                        o_current->text->angle,
                        o_current->text->string,
                        o_current->text->size,
                        o_is_visible (toplevel, o_current) ? VISIBLE : INVISIBLE,
                        o_current->show_name_value);

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
void geda_text_object_rotate (TOPLEVEL *toplevel,
                         int world_centerx, int world_centery,
                         int angle, OBJECT *object)
{
  int x, y;
  int newx, newy;

  g_return_if_fail (object != NULL);
  g_return_if_fail (object->text != NULL);
  g_return_if_fail (object->type == OBJ_TEXT);

  object->text->angle = ( object->text->angle + angle ) % 360;

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
void geda_text_object_mirror (TOPLEVEL *toplevel,
			 int world_centerx, int world_centery,
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
double o_text_shortest_distance (TOPLEVEL *toplevel, OBJECT *object,
                                 int x, int y, int force_solid)
{
  int left, top, right, bottom;
  double dx, dy;

  g_return_val_if_fail (object->text != NULL, G_MAXDOUBLE);

  if (!world_get_single_object_bounds(toplevel, object,
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
void o_text_set_string (TOPLEVEL *toplevel, OBJECT *obj,
                        const gchar *new_string)
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



/*! \brief Get the string displayed by a text object.
 *  \par Function Description
 *  Retrieve the text string from a text object. The returned string
 *  should be treated as constant.
 *
 *  \param [in]  toplevel              The TOPLEVEL object.
 *  \param [in]  obj                   The text object.
 *  \return The text object's string, or NULL on failure.
 */
const gchar *o_text_get_string (TOPLEVEL *toplevel, OBJECT *obj)
{
  g_return_val_if_fail (toplevel != NULL, NULL);
  g_return_val_if_fail (obj != NULL, NULL);
  g_return_val_if_fail (obj->type == OBJ_TEXT, NULL);
  g_return_val_if_fail (obj->text != NULL, NULL);

  return obj->text->string;
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
void o_text_set_rendered_bounds_func (TOPLEVEL *toplevel,
                                      RenderedBoundsFunc func,
                                      void *user_data) {
  toplevel->rendered_text_bounds_func = func;
  toplevel->rendered_text_bounds_data = user_data;
}


/*! \brief Return font size of a text object in postscript points.
 *
 *  \par Description
 *  gEDA fonts are specified in a non-standard unit. This
 *  function applies an appopriate scaling to return the
 *  font size in postscript points.
 *
 *  \param [in] toplevel  The TOPLEVEL object
 *  \param [in] object    The text OBJECT whos font size to return
 *  \return The font size converted to postscript points.
 */
double o_text_get_font_size_in_points (TOPLEVEL *toplevel, OBJECT *object)
{
  g_return_val_if_fail (object->type == OBJ_TEXT, 0.);

  return object->text->size * GEDA_FONT_FACTOR;
}
