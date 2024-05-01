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

/*! \file picture_object.c
 *  \brief functions for the picture object
 */

#include <config.h>

#include <stdio.h>
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#include <math.h>

#include <gio/gio.h>

#include "liblepton_priv.h"
#include <liblepton/glib_compat.h>


/*! \brief Create picture LeptonObject from character string.
 *  \par Function Description
 *  Parses \a first_line and subsequent lines from \a tb, and returns
 *  a newly-created picture #LeptonObject.
 *
 *  \param [in]  first_line      Character string with picture description.
 *  \param [in]  tb              Text buffer to load embedded data from.
 *  \param [in]  release_ver     liblepton release version number.
 *  \param [in]  fileformat_ver  liblepton file format version number.
 *  \param [in,out] err \c GError structure for error reporting,
 *                      or NULL to disable error reporting.
 *  \return A pointer to the new picture object, or NULL on error.
 */
LeptonObject*
lepton_picture_object_read (const char *first_line,
                            TextBuffer *tb,
                            unsigned int release_ver,
                            unsigned int fileformat_ver,
                            GError **err)
{
  LeptonObject *new_obj;
  int x1, y1;
  int width, height, angle;
  int mirrored, embedded;
  int num_conv;
  gchar type;
  const gchar *line = NULL;
  gchar *filename;
  gchar *file_content = NULL;
  guint file_length = 0;

  num_conv = sscanf(first_line, "%c %d %d %d %d %d %d %d\n",
                    &type, &x1, &y1, &width, &height, &angle, &mirrored, &embedded);

  if (num_conv != 8) {
    g_set_error(err, EDA_ERROR, EDA_ERROR_PARSE, _("Failed to parse picture definition"));
    return NULL;
  }

  if (width == 0 || height == 0) {
    g_message (_("Found a zero width/height picture "
                 "[ %1$c %2$d %3$d %4$d %5$d ]"),
               type, x1, y1, width, height);
  }

  if ( (mirrored > 1) || (mirrored < 0)) {
    g_message (_("Found a picture with a wrong 'mirrored' parameter: %1$d."),
               mirrored);
    g_message (_("Setting mirrored to 0."));
    mirrored = 0;
  }

  if ( (embedded > 1) || (embedded < 0)) {
    g_message (_("Found a picture with a wrong 'embedded' parameter: %1$d."),
               embedded);
    g_message (_("Setting embedded to 0."));
    embedded = 0;
  }

  switch(angle) {

    case(0):
    case(90):
    case(180):
    case(270):
    break;

    default:
      g_message (_("Found an unsupported picture angle [ %1$d ]"), angle);
      g_message (_("Setting angle to 0."));
      angle=0;
      break;

  }

  filename = g_strdup(s_textbuffer_next_line(tb));
  filename = lepton_str_remove_ending_newline (filename);

  /* Handle empty filenames */
  if (strlen (filename) == 0) {
    g_message (_("Found an image with no filename."));
    g_free (filename);
    filename = NULL;
  }

  if (embedded == 1) {
    GString *encoded_picture=g_string_new("");
    char finished = 0;

    /* Read the encoded picture */
    do {

      line = s_textbuffer_next_line(tb);
      if (line == NULL) break;

      if (strcmp (line, ".\n") != 0) {
        encoded_picture = g_string_append (encoded_picture, line);
      } else {
        finished = 1;
      }
    } while (finished == 0);

    /* Decode the picture */
    if (encoded_picture != NULL) {
      file_content = s_encoding_base64_decode(encoded_picture->str,
                                              encoded_picture->len,
                                              &file_length);
      g_string_free (encoded_picture, TRUE);
    }

    if (file_content == NULL) {
      g_message (_("Failed to load image from embedded data [%1$s]: %2$s"),
                 filename, _("Base64 decoding failed."));
      g_message (_("Falling back to file loading. Picture unembedded."));
      embedded = 0;
    }
  }

  /* create the picture */
  /* The picture is described by its upper left and lower right corner */
  new_obj = lepton_picture_object_new (file_content,
                                       file_length,
                                       filename,
                                       x1,
                                       y1+height,
                                       x1+width,
                                       y1,
                                       angle,
                                       mirrored,
                                       embedded);
  g_free (file_content);
  g_free (filename);

  return new_obj;
}

/*! \brief Create a character string representation of a picture LeptonObject.
 *  \par Function Description
 *  This function formats a string in the buffer <B>*buff</B> to describe
 *  the picture object <B>*object</B>.
 *
 *  \param [in] object  Picture LeptonObject to create string from.
 *  \return A pointer to the picture LeptonObject character string.
 *
 *  \note
 *  Caller must g_free returned character string.
 *
 */
gchar*
lepton_picture_object_to_buffer (const LeptonObject *object)
{
  int width, height, x1, y1;
  gchar *encoded_picture=NULL;
  gchar *out=NULL;
  guint encoded_picture_length;
  const gchar *filename = NULL;
  int lower_x, lower_y, upper_x, upper_y;

  lower_x = lepton_picture_object_get_lower_x (object);
  lower_y = lepton_picture_object_get_lower_y (object);
  upper_x = lepton_picture_object_get_upper_x (object);
  upper_y = lepton_picture_object_get_upper_y (object);

  /* calculate the width and height of the box */
  width  = abs (lower_x - upper_x);
  height = abs (upper_y - lower_y);

  /* calculate the lower left corner of the box */
  x1 = upper_x;
  y1 = upper_y - height; /* move the origin to 0, 0*/

#if DEBUG
  printf("picture: %d %d %d %d\n", x1, y1, width, height);
#endif

  /* Encode the picture if it's embedded */
  if (lepton_picture_object_get_embedded (object)) {
    encoded_picture =
      s_encoding_base64_encode( (char *)object->picture->file_content,
                                object->picture->file_length,
                                &encoded_picture_length,
                                TRUE);
    if (encoded_picture == NULL) {
      g_message (_("ERROR: unable to encode the picture."));
    }
  }

  /* Cope with null filename */
  filename = lepton_picture_object_get_filename (object);
  if (filename == NULL) filename = "";

  if (lepton_picture_object_get_embedded (object) &&
      encoded_picture != NULL) {
    out = g_strdup_printf("%c %d %d %d %d %d %d %d\n%s\n%s\n%s",
                          lepton_object_get_type (object),
                          x1, y1, width, height,
                          lepton_picture_object_get_angle (object),
                          lepton_picture_object_get_mirrored (object),
                          TRUE,
                          filename,
                          encoded_picture,
                          ".");
  }
  else {
    out = g_strdup_printf("%c %d %d %d %d %d %d %d\n%s",
                          lepton_object_get_type (object),
                          x1, y1, width, height,
                          lepton_picture_object_get_angle (object),
                          lepton_picture_object_get_mirrored (object),
                          FALSE,
                          filename);
  }
  g_free(encoded_picture);

  return(out);
}


/*! \brief Create a picture object.
 *  \par Function Description
 *  This function creates a new object representing a picture.
 *
 *  The picture is described by its upper left corner (\a x1, \a y1)
 *  and its lower right corner (\a x2, \a y2).  The \a type parameter
 *  must be equal to #OBJ_PICTURE.
 *
 *  If \a file_content is non-NULL, it must be a pointer to a buffer
 *  containing raw image data.  If loading data from \a file_content
 *  is unsuccessful, and \a filename is non-NULL, an image will
 *  attempt to be loaded from \a filename.  Otherwise, the picture
 *  object will be initially empty.
 *
 *  \param [in]     file_content  Raw data of the image file, or NULL.
 *  \param [in]     file_length   Length of raw data buffer
 *  \param [in]     filename      File name backing this picture, or NULL.
 *  \param [in]     x1            Upper x coordinate.
 *  \param [in]     y1            Upper y coordinate.
 *  \param [in]     x2            Lower x coordinate.
 *  \param [in]     y2            Lower y coordinate.
 *  \param [in]     angle         Picture rotation angle.
 *  \param [in]     mirrored      Whether the image should be mirrored or not.
 *  \param [in]     embedded      Whether the embedded flag should be set or not.
 *  \return A pointer to a new picture #LeptonObject.
 */
LeptonObject*
lepton_picture_object_new (const gchar *file_content,
                           gsize file_length,
                           const gchar *filename,
                           int x1,
                           int y1,
                           int x2,
                           int y2,
                           int angle,
                           gboolean mirrored,
                           gboolean embedded)
{
  LeptonObject *new_node;
  LeptonPicture *picture;

  /* create the object */
  new_node = lepton_object_new (OBJ_PICTURE, "picture");

  picture = lepton_picture_new ();
  new_node->picture = picture;

  /* describe the picture with its upper left and lower right corner */
  lepton_picture_object_set_upper_x (new_node, (x1 > x2) ? x2 : x1);
  lepton_picture_object_set_upper_y (new_node, (y1 > y2) ? y1 : y2);
  lepton_picture_object_set_lower_x (new_node, (x1 > x2) ? x1 : x2);
  lepton_picture_object_set_lower_y (new_node, (y1 > y2) ? y2 : y1);

  picture->pixbuf = NULL;
  picture->file_content = NULL;
  picture->file_length = 0;

  lepton_picture_object_set_ratio (new_node, fabs ((double) (x1 - x2) / (y1 - y2)));
  picture->filename = g_strdup (filename);
  lepton_picture_object_set_angle (new_node, angle);
  lepton_picture_object_set_mirrored (new_node, mirrored);
  lepton_picture_object_set_embedded (new_node, embedded);

  if (file_content != NULL) {
    GError *error = NULL;
    if (!lepton_picture_object_set_from_buffer (new_node,
                                                filename,
                                                file_content,
                                                file_length,
                                                &error))
    {
      g_message (_("Failed to load buffer image [%1$s]: %2$s"),
                 filename, error->message);
      g_error_free (error);

      /* Force the data into the object anyway, so as to prevent data
       * loss of embedded images. */
      picture->file_content = (gchar*) g_memdup2 (file_content, file_length);
      picture->file_length = file_length;
    }
  }
  if (picture->pixbuf == NULL && filename != NULL) {
    GError *error = NULL;
    if (!lepton_picture_object_set_from_file (new_node, filename, &error))
    {
      g_message (_("Failed to load image from [%1$s]: %2$s"),
                 filename, error->message);
      g_error_free (error);
      /* picture not found; try to open a fall back pixbuf */
      picture->pixbuf = lepton_picture_get_fallback_pixbuf ();
    }
  }

  return new_node;
}

/*! \brief Get picture bounding rectangle in WORLD coordinates.
 *
 *  On failure, this function sets the bounds to empty.
 *
 *  \param [in]  object    Picture LeptonObject to read coordinates from.
 *  \param [out] bounds    The bounds of the picture
 */
void
lepton_picture_object_calculate_bounds (const LeptonObject *object,
                                        LeptonBounds *bounds)
{
  lepton_bounds_init (bounds);

  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  lepton_bounds_init_with_points (bounds,
                                  lepton_picture_object_get_lower_x (object),
                                  lepton_picture_object_get_lower_y (object),
                                  lepton_picture_object_get_upper_x (object),
                                  lepton_picture_object_get_upper_y (object));
}

/*! \brief get the position of the left bottom point
 *  \par Function Description
 *  This function gets the position of the bottom left point of a picture object.
 *
 *  \param [in] object   The object to get the position.
 *  \param [out] x       pointer to the x-position
 *  \param [out] y       pointer to the y-position
 *  \return TRUE if successfully determined the position, FALSE otherwise
 */
gboolean
lepton_picture_object_get_position (const LeptonObject *object,
                                    gint *x,
                                    gint *y)
{
  g_return_val_if_fail (lepton_object_is_picture (object), FALSE);
  g_return_val_if_fail (object->picture != NULL, FALSE);

  if (x != NULL) {
    *x = MIN (lepton_picture_object_get_lower_x (object),
              lepton_picture_object_get_upper_x (object));
  }

  if (y != NULL) {
    *y = MIN (lepton_picture_object_get_lower_y (object),
              lepton_picture_object_get_upper_y (object));
  }

  return TRUE;
}


/*! \brief Get the width/height ratio of an image.
 * \par Function Description

 * Returns the width/height ratio of picture \a object.  The
 * function does not take into account the image rotation but
 * simply returns the value of the 'ratio' field of the
 * #LeptonPicture structure.
 *
 * \param object Picture #LeptonObject to inspect.
 * \return Width/height ratio for the picture object.
 */
double
lepton_picture_object_get_ratio (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), 0);
  g_return_val_if_fail (object->picture != NULL, 0);

  return object->picture->ratio;
}

/*! \brief Get the width/height ratio of an image.
 * \par Function Description

 * Returns the width/height ratio of picture \a object, taking the
 * image rotation into account.
 *
 * \param object    Picture #LeptonObject to inspect.
 * \return width/height ratio for \a object.
 */
double
lepton_picture_object_get_real_ratio (LeptonObject *object)
{
  int angle;
  double ratio;

  g_return_val_if_fail (lepton_object_is_picture (object), 1);
  g_return_val_if_fail (object->picture != NULL, 1);

  angle = lepton_picture_object_get_angle (object);
  ratio = lepton_picture_object_get_ratio (object);

  /* The effective ratio varies depending on the rotation of the
   * image. */
  switch (angle) {
  case 0:
  case 180:
    return ratio;
  case 90:
  case 270:
    return 1.0 / ratio;
  default:
    g_critical (_("Picture %1$p has invalid angle %2$i\n"), object, angle);
  }
  return 0;
}

/*! \brief Set the width/height ratio of an image.
 * \par Function Description

 * Sets the width/height ratio of picture \a object.
 *
 * \param object Picture #LeptonObject to amend.
 * \param ratio  New width/height ratio for the picture.
 */
void
lepton_picture_object_set_ratio (LeptonObject *object,
                                 double ratio)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  object->picture->ratio = ratio;
}

/*! \brief Modify the description of a picture LeptonObject.
 *  \par Function Description
 *  This function modifies the coordinates of one of the four corner of
 *  the picture. The new coordinates of the corner identified by
 *  <B>whichone</B> are given by <B>x</B> and <B>y</B> in world unit.
 *
 *  The coordinates of the corner is modified in the world coordinate system.
 *  Screen coordinates and boundings are then updated.
 *
 *  \param [in,out] object     Picture LeptonObject to modify.
 *  \param [in]     x          New x coordinate.
 *  \param [in]     y          New y coordinate.
 *  \param [in]     whichone   Which picture parameter to modify.
 *
 *  <B>whichone</B> can have the following values:
 *  <DL>
 *    <DT>*</DT><DD>PICTURE_UPPER_LEFT
 *    <DT>*</DT><DD>PICTURE_LOWER_LEFT
 *    <DT>*</DT><DD>PICTURE_UPPER_RIGHT
 *    <DT>*</DT><DD>PICTURE_LOWER_RIGHT
 *  </DL>
 */
void
lepton_picture_object_modify (LeptonObject *object,
                              int x,
                              int y,
                              int whichone)
{
  int tmp;
  int lower_x, lower_y, upper_x, upper_y;
  double ratio = lepton_picture_object_get_real_ratio (object);

  lepton_object_emit_pre_change_notify (object);

  /* change the position of the selected corner */
  switch(whichone) {
    case PICTURE_UPPER_LEFT:
      lepton_picture_object_set_upper_x (object, x);
      tmp = abs (lepton_picture_object_get_upper_x (object) -
                 lepton_picture_object_get_lower_x (object)) / ratio;
      if (y < lepton_picture_object_get_lower_y (object))
      {
        tmp = -tmp;
      }
      lepton_picture_object_set_upper_y (object,
                                         lepton_picture_object_get_lower_y (object) + tmp);
      break;

    case PICTURE_LOWER_LEFT:
      lepton_picture_object_set_upper_x (object, x);
      tmp = abs (lepton_picture_object_get_upper_x (object) -
                 lepton_picture_object_get_lower_x (object)) / ratio;
      if (y > lepton_picture_object_get_upper_y (object))
      {
        tmp = -tmp;
      }
      lepton_picture_object_set_lower_y (object,
                                         lepton_picture_object_get_upper_y (object) - tmp);
      break;

    case PICTURE_UPPER_RIGHT:
      lepton_picture_object_set_lower_x (object, x);
      tmp = abs (lepton_picture_object_get_upper_x (object) -
                 lepton_picture_object_get_lower_x (object)) / ratio;
      if (y < lepton_picture_object_get_lower_y (object))
      {
        tmp = -tmp;
      }
      lepton_picture_object_set_upper_y (object,
                                         lepton_picture_object_get_lower_y (object) + tmp);
      break;

    case PICTURE_LOWER_RIGHT:
      lepton_picture_object_set_lower_x (object, x);
      tmp = abs (lepton_picture_object_get_upper_x (object) -
                 lepton_picture_object_get_lower_x (object)) / ratio;
      if (y > lepton_picture_object_get_upper_y (object))
      {
        tmp = -tmp;
      }
      lepton_picture_object_set_lower_y (object,
                                         lepton_picture_object_get_upper_y (object) - tmp);
      break;

    default:
      return;
  }

  lower_x = lepton_picture_object_get_lower_x (object);
  lower_y = lepton_picture_object_get_lower_y (object);
  upper_x = lepton_picture_object_get_upper_x (object);
  upper_y = lepton_picture_object_get_upper_y (object);

  /* need to update the upper left and lower right corners */
  if (upper_x > lower_x)
  {
    tmp = upper_x;
    lepton_picture_object_set_upper_x (object, lower_x);
    lepton_picture_object_set_lower_x (object, tmp);
  }

  if (upper_y < lower_y)
  {
    tmp = upper_y;
    lepton_picture_object_set_upper_y (object, lower_y);
    lepton_picture_object_set_lower_y (object, tmp);
  }

  lepton_object_emit_change_notify (object);
}

/*! \brief Rotate picture LeptonObject using WORLD coordinates.
 *  \par Function Description
 *  This function rotates the picture described by <B>*object</B> around
 *  the (<B>world_centerx</B>, <B>world_centery</B>) point by <B>angle</B>
 *  degrees.
 *  The center of rotation is in world units.
 *
 *  \param [in]      world_centerx  Rotation center x coordinate in
 *                                  WORLD units.
 *  \param [in]      world_centery  Rotation center y coordinate in
 *                                  WORLD units.
 *  \param [in]      angle          Rotation angle in degrees (See note below).
 *  \param [in,out]  object         Picture LeptonObject to rotate.
 */
void
lepton_picture_object_rotate (int world_centerx,
                              int world_centery,
                              int angle,
                              LeptonObject *object)
{
  int newx1, newy1;
  int newx2, newy2;
  int new_angle;

  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  /* Only 90 degree multiple and positive angles are allowed. */
  /* angle must be positive */
  if(angle < 0) angle = -angle;
  /* angle must be a 90 multiple or no rotation performed */
  if((angle % 90) != 0) return;

  new_angle = (lepton_picture_object_get_angle (object) + angle) % 360;

  lepton_picture_object_set_angle (object, new_angle) ;

  /* The center of rotation (<B>world_centerx</B>, <B>world_centery</B>) is
   * translated to the origin. The rotation of the upper left and lower
   * right corner are then performed. Finally, the rotated picture is
   * translated back to its previous location.
   */
  /* translate object to origin */
  lepton_picture_object_translate (object, -world_centerx, -world_centery);

  /* rotate the upper left corner of the picture */
  lepton_point_rotate_90 (lepton_picture_object_get_upper_x (object),
                          lepton_picture_object_get_upper_y (object),
                          angle,
                          &newx1,
                          &newy1);

  /* rotate the lower left corner of the picture */
  lepton_point_rotate_90 (lepton_picture_object_get_lower_x (object),
                          lepton_picture_object_get_lower_y (object),
                          angle,
                          &newx2,
                          &newy2);

  /* reorder the corners after rotation */
  lepton_picture_object_set_upper_x (object, MIN (newx1, newx2));
  lepton_picture_object_set_upper_y (object, MAX (newy1, newy2));
  lepton_picture_object_set_lower_x (object, MAX (newx1, newx2));
  lepton_picture_object_set_lower_y (object, MIN (newy1, newy2));

  /* translate object back to normal position */
  lepton_picture_object_translate (object, world_centerx, world_centery);
}

/*! \brief Mirror a picture using WORLD coordinates.
 *  \par Function Description
 *  This function mirrors the picture from the point
 *  (<B>world_centerx</B>,<B>world_centery</B>) in world unit.
 *
 *  The picture is first translated to the origin, then mirrored and
 *  finally translated back at its previous position.
 *
 *  \param [in]     world_centerx  Origin x coordinate in WORLD units.
 *  \param [in]     world_centery  Origin y coordinate in WORLD units.
 *  \param [in,out] object         Picture LeptonObject to mirror.
 */
void
lepton_picture_object_mirror (int world_centerx,
                              int world_centery,
                              LeptonObject *object)
{
  int newx1, newy1;
  int newx2, newy2;

  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  /* Set info in object. Sometimes it's necessary to change the
   * rotation angle as well as the mirror flag. */
  lepton_picture_object_set_mirrored (object,
                                      !lepton_picture_object_get_mirrored (object));
  switch (lepton_picture_object_get_angle (object)) {
  case 90:
    lepton_picture_object_set_angle (object, 270);
    break;
  case 270:
    lepton_picture_object_set_angle (object, 90);
    break;
  }

  /* translate object to origin */
  lepton_picture_object_translate (object, -world_centerx, -world_centery);

  /* mirror the corners */
  newx1 = -lepton_picture_object_get_upper_x (object);
  newy1 = lepton_picture_object_get_upper_y (object);
  newx2 = -lepton_picture_object_get_lower_x (object);
  newy2 = lepton_picture_object_get_lower_y (object);

  /* reorder the corners */
  lepton_picture_object_set_upper_x (object, MIN (newx1, newx2));
  lepton_picture_object_set_upper_y (object, MAX (newy1, newy2));
  lepton_picture_object_set_lower_x (object, MAX (newx1, newx2));
  lepton_picture_object_set_lower_y (object, MIN (newy1, newy2));

  /* translate back in position */
  lepton_picture_object_translate (object, world_centerx, world_centery);
}

/*! \brief Translate a picture position in WORLD coordinates by a delta.
 *  \par Function Description
 *  This function applies a translation of (<B>x1</B>,<B>y1</B>) to the picture
 *  described by <B>*object</B>. <B>x1</B> and <B>y1</B> are in world units.
 *
 *  \param [in,out] object     Picture LeptonObject to translate.
 *  \param [in]     dx         x distance to move.
 *  \param [in]     dy         y distance to move.
 */
void
lepton_picture_object_translate (LeptonObject *object,
                                 int dx,
                                 int dy)
{
  int lower_x, lower_y, upper_x, upper_y;

  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  upper_x = lepton_picture_object_get_upper_x (object);
  upper_y = lepton_picture_object_get_upper_y (object);
  lower_x = lepton_picture_object_get_lower_x (object);
  lower_y = lepton_picture_object_get_lower_y (object);

  /* Do world coords */
  lepton_picture_object_set_upper_x (object, upper_x + dx);
  lepton_picture_object_set_upper_y (object, upper_y + dy);
  lepton_picture_object_set_lower_x (object, lower_x + dx);
  lepton_picture_object_set_lower_y (object, lower_y + dy);
}

/*! \brief Create a copy of a picture.
 *  \par Function Description
 *  This function creates a verbatim copy of the object pointed by
 *  <B>o_current</B> describing a picture.
 *
 *  \param [in]  object     Picture LeptonObject to copy.
 *  \return The new LeptonObject
 */
LeptonObject*
lepton_picture_object_copy (LeptonObject *object)
{
  LeptonObject *new_node;
  LeptonPicture *picture;

  /* create the object */
  new_node = lepton_object_new (lepton_object_get_type (object), "picture");

  picture = (LeptonPicture*) g_malloc (sizeof (LeptonPicture));
  new_node->picture = picture;

  lepton_object_set_color (new_node, lepton_object_get_color (object));
  lepton_object_set_selectable (new_node, lepton_object_get_selectable (object));

  /* describe the picture with its upper left and lower right corner */
  lepton_picture_object_set_upper_x (new_node, lepton_picture_object_get_upper_x (object));
  lepton_picture_object_set_upper_y (new_node, lepton_picture_object_get_upper_y (object));
  lepton_picture_object_set_lower_x (new_node, lepton_picture_object_get_lower_x (object));
  lepton_picture_object_set_lower_y (new_node, lepton_picture_object_get_lower_y (object));

  if (object->picture->file_content != NULL) {
    picture->file_content = (gchar*) g_memdup2 (object->picture->file_content,
                                               object->picture->file_length);
  } else {
    picture->file_content = NULL;
  }

  picture->file_length = object->picture->file_length;
  picture->filename    = g_strdup (object->picture->filename);
  lepton_picture_object_set_ratio (new_node, lepton_picture_object_get_ratio (object));
  lepton_picture_object_set_angle (new_node, lepton_picture_object_get_angle (object));
  lepton_picture_object_set_mirrored (new_node, lepton_picture_object_get_mirrored (object));
  lepton_picture_object_set_embedded (new_node, lepton_picture_object_get_embedded (object));

  /* Get the picture data */
  picture->pixbuf = lepton_picture_object_get_pixbuf (object);

  return new_node;
}

/*! \brief Embed a picture object into its schematic.
 *  \par Function Description
 *  This functions embeds a picture object into its schematic if
 *  it has valid image data associated with it.
 *
 *  \param object The picture #LeptonObject to embed.
 */
void
lepton_picture_object_embed (LeptonObject *object)
{
  LeptonPage *page;
  const gchar *filename;
  gchar *basename;

  g_return_if_fail (lepton_object_is_picture (object));

  page = lepton_object_get_page (object);

  /* Return if the picture is already embedded. */
  if (lepton_picture_object_get_embedded (object))
    return;

  filename = lepton_picture_object_get_filename (object);

  if (object->picture->file_content == NULL)
  {
    /* Image has no data: signal an error. */
    g_message (_("Picture [%1$s] has no image data."), filename);
    g_message (_("Falling back to file loading. Picture is still unembedded."));
  }
  else
  {
    /* Set the embedded flag. */
    lepton_picture_object_set_embedded (object, TRUE);

    basename = g_path_get_basename (filename);
    g_message (_("Picture [%1$s] has been embedded."), basename);
    g_free (basename);

    if (page != NULL)
    {
      lepton_page_set_changed (page, 1);
    }
  }
}


/*! \brief Unembed a picture object from its schematic.
 *  \par Function Description
 *  This function unembeds a picture object from its schematic
 *  after checking that it is possible.  It verifies that the file
 *  associated with the picture exists on disk and is usable.  If
 *  so, the picture is reloaded and marked as not embedded.
 *
 *  \param [in] object The picture #LeptonObject to unembed.
 */
void
lepton_picture_object_unembed (LeptonObject *object)
{
  LeptonPage *page;
  const gchar *filename;
  gchar *basename;
  GError *err = NULL;

  g_return_if_fail (lepton_object_is_picture (object));

  page = lepton_object_get_page (object);

  /* Return if the picture is already not embedded. */
  if (!lepton_picture_object_get_embedded (object))
    return;

  filename = lepton_picture_object_get_filename (object);

  /* Get picture data from file. */
  lepton_picture_object_set_from_file (object, filename, &err);

  if (err != NULL)
  {
    /* Warn the user that the picture could not be loaded. */
    g_message (_("Failed to load image from file [%1$s]: %2$s"),
               filename, err->message);
    g_message (_("Picture is still embedded."));
    g_error_free (err);
  }
  else
  {
    /* Clear the embedded flag. */
    lepton_picture_object_set_embedded (object, FALSE);

    basename = g_path_get_basename (filename);
    g_message (_("Picture [%1$s] has been unembedded."), basename);
    g_free (basename);

    if (page != NULL)
    {
      lepton_page_set_changed (page, 1);
    }
  }
}


/*! \brief Calculates the distance between the given point and the closest
 * point in the picture.
 *
 *  Interrior points within the picture return a distance of zero.
 *
 *  \param [in] object         The picture LeptonObject.
 *  \param [in] x              The x coordinate of the given point.
 *  \param [in] y              The y coordinate of the given point.
 *  \param [in] force_solid    If true, force treating the object as solid.
 *  \param [in] include_hidden Take hidden text into account.
 *  \return The shortest distance from the object to the point. With an
 *  invalid parameter, this function returns G_MAXDOUBLE.
 */
double
lepton_picture_object_shortest_distance (LeptonObject *object,
                                         int x,
                                         int y,
                                         int force_solid,
                                         gboolean include_hidden)
{
  double dx, dy;
  double x1, y1, x2, y2;

  g_return_val_if_fail (lepton_object_is_picture (object), G_MAXDOUBLE);
  g_return_val_if_fail (object->picture != NULL, G_MAXDOUBLE);

  x1 = (double) MIN (lepton_picture_object_get_upper_x (object),
                     lepton_picture_object_get_lower_x (object));
  y1 = (double) MIN (lepton_picture_object_get_upper_y (object),
                     lepton_picture_object_get_lower_y (object));
  x2 = (double) MAX (lepton_picture_object_get_upper_x (object),
                     lepton_picture_object_get_lower_x (object));
  y2 = (double) MAX (lepton_picture_object_get_upper_y (object),
                     lepton_picture_object_get_lower_y (object));

  dx = MIN (((double)x) - x1, x2 - ((double)x));
  dy = MIN (((double)y) - y1, y2 - ((double)y));

  dx = MIN (dx, 0);
  dy = MIN (dy, 0);

  return hypot (dx, dy);
}

/*! \brief Test whether a picture object is embedded.
 * \par Function Description
 * Returns TRUE if the picture \a object will have its data embedded
 * in a schematic or symbol file; returns FALSE if its data will be
 * obtained from a separate file.
 *
 * \param object    The picture #LeptonObject to inspect.
 * \return TRUE if \a object is embedded.
 */
gboolean
lepton_picture_object_get_embedded (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), FALSE);
  g_return_val_if_fail (object->picture != NULL, FALSE);

  return object->picture->embedded;
}

/*! \brief Set the \a embedded flag of a picture object.
 *
 * \param object   The picture #LeptonObject to amend.
 * \param embedded If the picture object should be embedded.
 */
void
lepton_picture_object_set_embedded (LeptonObject *object,
                                    gboolean embedded)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  object->picture->embedded = embedded;
}


/*! \brief Get a pixel buffer for a picture object.
 * \par Function Description
 * Returns a \c GdkPixbuf for the picture object \a object, or
 * NULL if the picture could not be loaded.
 *
 * The returned value should have its reference count decremented with
 * g_object_unref() when no longer needed.
 *
 * \param object    The picture #LeptonObject to inspect.
 * \return A \c GdkPixbuf for the picture.
 */
GdkPixbuf *
lepton_picture_object_get_pixbuf (LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  if (object->picture->pixbuf != NULL) {
    return GDK_PIXBUF (g_object_ref (object->picture->pixbuf));
  } else {
    return NULL;
  }
}


/*! \brief Set a picture object's contents from a buffer.
 * \par Function Description
 * Sets the contents of the picture \a object by reading image data
 * from a buffer.  The buffer should be in on-disk format.
 *
 * \param object   The picture #LeptonObject to modify.
 * \param filename The new filename for the picture.
 * \param data     The new image data buffer.
 * \param len      The size of the data buffer.
 * \param error    Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
lepton_picture_object_set_from_buffer (LeptonObject *object,
                                       const gchar *filename,
                                       const gchar *data,
                                       size_t len,
                                       GError **error)
{
  GdkPixbuf *pixbuf;
  GInputStream *stream;
  gchar *tmp;

  g_return_val_if_fail (lepton_object_is_picture (object), FALSE);
  g_return_val_if_fail (object->picture != NULL, FALSE);
  g_return_val_if_fail (data != NULL, FALSE);

  /* Check that we can actually load the data before making any
   * changes to the object. */
  stream = G_INPUT_STREAM (g_memory_input_stream_new_from_data (data, len, NULL));
  pixbuf = gdk_pixbuf_new_from_stream (stream, NULL, error);
  g_object_unref (stream);
  if (pixbuf == NULL) return FALSE;

  lepton_object_emit_pre_change_notify (object);

  if (object->picture->pixbuf != NULL) {
    g_object_unref (object->picture->pixbuf);
  }
  object->picture->pixbuf = pixbuf;

  lepton_picture_object_set_ratio (object,
                                   ((double) gdk_pixbuf_get_width(pixbuf) /
                                    gdk_pixbuf_get_height(pixbuf)));

  tmp = g_strdup (filename);
  g_free (object->picture->filename);
  object->picture->filename = tmp;

  gchar *buf = (gchar*) g_realloc (object->picture->file_content,
                                   len);
  /* It's possible that these buffers might overlap, because the
   * library user hates us. */
  memmove (buf, data, len);
  object->picture->file_content = buf;
  object->picture->file_length = len;

  lepton_object_emit_change_notify (object);
  return TRUE;
}

/*! \brief Set a picture object's contents from a file.
 * \par Function Description
 * Sets the contents of the picture \a object by reading image data
 * from a file.
 *
 * \param object   The picture #LeptonObject to modify.
 * \param filename The filename to load image data from.
 * \param error    Location to return error information.
 * \return TRUE on success, FALSE on failure.
 */
gboolean
lepton_picture_object_set_from_file (LeptonObject *object,
                                     const gchar *filename,
                                     GError **error)
{
  gchar *buf;
  size_t len;
  gboolean status;

  g_return_val_if_fail (filename != NULL, FALSE);

  if (!g_file_get_contents (filename, &buf, &len, error)) {
    return FALSE;
  }

  status = lepton_picture_object_set_from_buffer (object,
                                                  filename,
                                                  buf,
                                                  len,
                                                  error);
  g_free (buf);
  return status;
}

/*! \brief Get a picture's corresponding filename.
 * \par Function Description
 * Returns the filename associated with the picture \a object.
 *
 * \param object   The picture #LeptonObject to inspect.
 * \return the filename associated with \a object.
 */
const gchar *
lepton_picture_object_get_filename (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), NULL);
  g_return_val_if_fail (object->picture != NULL, NULL);

  return object->picture->filename;
}

/*! \brief Get the upper X coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to get the upper X value of.
 *  \return upper_x The angle value.
 */
int
lepton_picture_object_get_upper_x (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), 0);
  g_return_val_if_fail (object->picture != NULL, 0);

  return object->picture->upper_x;
}


/*! \brief Set the upper X coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to set the upper X value of.
 *  \param [in] x  The new upper X value.
 */
void
lepton_picture_object_set_upper_x (LeptonObject *object,
                                   int x)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  object->picture->upper_x = x;
}


/*! \brief Get the lower X coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to get the lower X value of.
 *  \return lower_x The angle value.
 */
int
lepton_picture_object_get_lower_x (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), 0);
  g_return_val_if_fail (object->picture != NULL, 0);

  return object->picture->lower_x;
}


/*! \brief Set the lower X coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to set the lower X value of.
 *  \param [in] x  The new lower X value.
 */
void
lepton_picture_object_set_lower_x (LeptonObject *object,
                                   int x)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  object->picture->lower_x = x;
}


/*! \brief Get the upper Y coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to get the upper Y value of.
 *  \return upper_y The angle value.
 */
int
lepton_picture_object_get_upper_y (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), 0);
  g_return_val_if_fail (object->picture != NULL, 0);

  return object->picture->upper_y;
}


/*! \brief Set the upper Y coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to set the upper Y value of.
 *  \param [in] y  The new upper Y value.
 */
void
lepton_picture_object_set_upper_y (LeptonObject *object,
                                   int y)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  object->picture->upper_y = y;
}


/*! \brief Get the lower Y coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to get the lower Y value of.
 *  \return lower_y The angle value.
 */
int
lepton_picture_object_get_lower_y (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), 0);
  g_return_val_if_fail (object->picture != NULL, 0);

  return object->picture->lower_y;
}


/*! \brief Set the lower Y coordinate of a picture.
 *
 *  \param [in] object Picture LeptonObject to set the lower Y value of.
 *  \param [in] y  The new lower Y value.
 */
void
lepton_picture_object_set_lower_y (LeptonObject *object,
                                   int y)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);

  object->picture->lower_y = y;
}


/*! \brief Get the angle of a picture.
 *
 *  \param [in] object Picture LeptonObject to get the angle value of.
 *  \return angle  The angle value.
 */
int
lepton_picture_object_get_angle (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), 0);
  g_return_val_if_fail (object->picture != NULL, 0);

  return object->picture->angle;
}


/*! \brief Set the angle of a picture.
 *
 *  \param [in] object Picture LeptonObject to set the angle value of.
 *  \param [in] angle  The angle value.
 */
void
lepton_picture_object_set_angle (LeptonObject *object,
                                 int angle)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);
  g_return_if_fail ((angle == 0)
                    || (angle == 90)
                    || (angle == 180)
                    || (angle == 270));

  object->picture->angle = angle;
}


/*! \brief Get the mirrored flag of a picture.
 *
 *  \param [in] object Picture LeptonObject to get the mirrored value of.
 *  \return mirrored  The mirrored flag value.
 */
gboolean
lepton_picture_object_get_mirrored (const LeptonObject *object)
{
  g_return_val_if_fail (lepton_object_is_picture (object), 0);
  g_return_val_if_fail (object->picture != NULL, 0);

  return object->picture->mirrored;
}


/*! \brief Set the mirrored flag of a picture.
 *
 *  \param [in] object Picture LeptonObject to set the mirrored value of.
 *  \param [in] mirrored The mirrored flag value.
 */
void
lepton_picture_object_set_mirrored (LeptonObject *object,
                                    gboolean mirrored)
{
  g_return_if_fail (lepton_object_is_picture (object));
  g_return_if_fail (object->picture != NULL);
  g_return_if_fail ((mirrored == 0) || (mirrored == 1));

  object->picture->mirrored = mirrored;
}
