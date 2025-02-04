/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2021-2025 Lepton EDA Contributors
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
/*! \file picture.h
 */

G_BEGIN_DECLS

typedef struct st_picture LeptonPicture;

struct st_picture
{
  /* Picture pixbuf. */
  GdkPixbuf *pixbuf;
  /* A pointer to a buffer containing Base64 encoded raw image
     data, or NULL. */
  gchar *file_content;
  /* The length of the above raw image data buffer. */
  gsize file_length;

  /* Width/height ratio. */
  double ratio;
  /* File name backing the picture, or NULL. */
  char *filename;
  /* Rotation angle. It must be orthogonal. */
  int angle;
  /* Whether the image is mirrored. */
  gboolean mirrored;
  /* Whether the image is embedded. */
  gboolean embedded;

  /* World coordinates. Upper is considered the origin */
  int upper_x, upper_y;
  int lower_x, lower_y;
};

LeptonPicture*
lepton_picture_new ();

void
lepton_picture_free (LeptonPicture *picture);

GdkPixbuf*
lepton_picture_get_fallback_pixbuf () G_GNUC_WARN_UNUSED_RESULT;

G_END_DECLS
