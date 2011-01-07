/* Functions gdk_pixbuf_rotate and gdk_pixbuf_add are taken from gtkam,
   and is covered by this copyright. */
/*
 * Copyright © 2001 Lutz Müller <lutz@users.sf.net>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details. 
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA
 */

/* Taken from gtkam's sources */
#include "config.h"

#include <gdk-pixbuf/gdk-pixbuf.h>

/*! \def COPY90 */
#define COPY90                                                          \
        if ((r2 < h1) && (c2 < w1)) {                                   \
          if ((w1 <= h1) && (r1 < h2))                                  \
            for (i = 0; i < c; i++)                                     \
              new_pixels[r1 * rs2 + c1 * c + i] =                      	\
                pixels[r2 * rs1 + c2 * c + i];                  	\
          if ((w1 > h1) && (c1 > (w1 - h2)))                            \
            for (i = 0; i < c; i++)                                     \
              new_pixels[r1 * rs2 + (c1 - (w1 - h1)) * c + i] =        	\
                pixels[r2 * rs1 + c2 * c + i];                  	\
        }

/*! \def COPY270 */
#define COPY270                                                         \
        if ((r2 < h1) && (c2 < w1)) {                                   \
          if ((h1 > w1) && (r1 > (h1 - w1)))                            \
            for (i = 0; i < c; i++)                                     \
              new_pixels[(r1 - (h1 - w1)) * rs2 + c1 * c + i] =        	\
                pixels[r2 * rs1 + c2 * c + i];                  	\
          if ((h1 <= w1) && (c1 < w2))                                  \
            for (i = 0; i < c; i++)                                     \
              new_pixels[r1 * rs2 + c1 * c + i] =               	\
                pixels[r2 * rs1 + c2 * c + i];                  	\
        }


/*! \brief Rotate a GdkPixbuf by a given angle
 *  \par Function Description
 *  This function will take a GdkPixbuf and rotate it by the specified angle.
 *
 *  \param [in] pixbuf  The pixel buffer to rotate.
 *  \param [in] angle   The rotation angle.
 *  \return The rotated pixbuf.
 */
GdkPixbuf *gdk_pixbuf_rotate (GdkPixbuf *pixbuf, guint angle)
{
        GdkPixbuf *new = NULL;
        guint row, col, w1, h1, w2, h2;
        guint r1, r2, c1, c2;
        guint rs1, rs2;
        guint c;
        guint i;
	guchar *pixels, *new_pixels;

	if (pixbuf == NULL) {
	  return NULL;
	}	
        g_return_val_if_fail (GDK_IS_PIXBUF (pixbuf), NULL);
	
	/* Swapped original definitions, so the picture turns counter-clockwise */
	if (angle == 90)
	  angle = 270;
	else  if (angle == 270)
	  angle = 90;

        switch (angle) {
        case 0:
                return (gdk_pixbuf_copy (pixbuf));
        case 180:
                new = gdk_pixbuf_new (
			gdk_pixbuf_get_colorspace (pixbuf),
			gdk_pixbuf_get_has_alpha (pixbuf),
			gdk_pixbuf_get_bits_per_sample (pixbuf),
			gdk_pixbuf_get_width (pixbuf),
			gdk_pixbuf_get_height (pixbuf));
                break;
        case 90:
        case 270:
                new = gdk_pixbuf_new (
			gdk_pixbuf_get_colorspace (pixbuf),
			gdk_pixbuf_get_has_alpha (pixbuf),
			gdk_pixbuf_get_bits_per_sample (pixbuf),
			gdk_pixbuf_get_height (pixbuf),
			gdk_pixbuf_get_width (pixbuf));
                break;
        default:
                g_warning ("Rotation by %i not implemented.", angle);
                break;
        }

        rs1 = gdk_pixbuf_get_rowstride (pixbuf);
        rs2 = gdk_pixbuf_get_rowstride (new);

        c = gdk_pixbuf_get_has_alpha (pixbuf) ? 4 : 3;

        w1 = gdk_pixbuf_get_width (pixbuf);
        h1 = gdk_pixbuf_get_height (pixbuf);
        w2 = gdk_pixbuf_get_width (new);
        h2 = gdk_pixbuf_get_height (new);

	pixels = gdk_pixbuf_get_pixels (pixbuf);
	new_pixels = gdk_pixbuf_get_pixels (new);

        /*
         * For rotation by 90 or 270, we assume the pixbuf to be a
         * square and move (r2,c2) to (r1,c1):
         */
        switch (angle) {
        case 90:
                for (row = 0; row < MAX (w1, h1) / 2; row++) {
                        for (col = row; col < MAX (w1, h1) - row - 1; col++) {
                                r1 = row;
                                c1 = col;
                                r2 = MAX (w1, h1) - col - 1;
                                c2 = row;
                                COPY90;
                                r1 = r2;
                                c1 = c2;
                                r2 = MAX (w1, h1) - row - 1;
                                c2 = MAX (w1, h1) - col - 1;
                                COPY90;
                                r1 = r2;
                                c1 = c2;
                                r2 = col;
                                c2 = MAX (w1, h1) - row - 1;
                                COPY90;
                                r1 = r2;
                                c1 = c2;
                                r2 = row;
                                c2 = col;
                                COPY90;
                        }
                }
                break;
        case 270:
                for (row = 0; row < MAX (w1, h1) / 2; row++) {
                        for (col = row; col < MAX (w1, h1) - row - 1; col++) {
                                r1 = row;
                                c1 = col;
                                r2 = col;
                                c2 = MAX (w1, h1) - row - 1;
                                COPY270;
                                r1 = r2;
                                c1 = c2;
                                r2 = MAX (w1, h1) - row - 1;
                                c2 = MAX (w1, h1) - col - 1;
                                COPY270;
                                r1 = r2;
                                c1 = c2;
                                r2 = MAX (w1, h1) - col - 1;
                                c2 = row;
                                COPY270;
                                r1 = r2;
                                c1 = c2;
                                r2 = row;
                                c2 = col;
                                COPY270;
                        }
                }
                break;
        case 180:
                for (row = 0; row < h1; row++) {
                        for (col = 0; col < w1; col++) {
                                r1 = row;
                                c1 = col;
                                r2 = h1 - row - 1;
                                c2 = w1 - col - 1;
                                for (i = 0; i < c; i++) {
                                  new_pixels[r2 * rs2 + c2 * c + i] =
                                    pixels[r1 * rs1 + c1 * c + i];
                                }
                        }
                }
                break;
        default:
                g_warning ("Rotation by %i not implemented.", angle);
                break;
        }

        return (new);
}

/*! \brief Maps a GdkPixbuf at a given offset onto another GdkPixbuf.
 *  \par Function Description
 *  This function will take a GdkPixbuf and map it onto an existing one
 *  at the given x/y offset.
 *
 *  \param [in,out] pixbuf         The GdkPixbuf to map onto.
 *  \param [in]     offset_x       The x offset to start map operation.
 *  \param [in]     offset_y       The y offset to start map operation.
 *  \param [in]     pixbuf_to_add  The GdkPixbuf to map onto pixbuf.
 *  \return Updated GdkPixbuf is returned in pixbuf.
 */
void gdk_pixbuf_add (GdkPixbuf *pixbuf, int offset_x, int offset_y,
		     GdkPixbuf *pixbuf_to_add)
{
	guchar *p1, *p2, a1, a2;
	guint w1, h1, w2, h2, r1, r2;
	guint row, col, i, pos1, pos2;

	g_return_if_fail (pixbuf != NULL);
	g_return_if_fail (pixbuf_to_add != NULL);

	w1 = gdk_pixbuf_get_width (pixbuf);
	h1 = gdk_pixbuf_get_height (pixbuf);
	w2 = gdk_pixbuf_get_width (pixbuf_to_add);
	h2 = gdk_pixbuf_get_height (pixbuf_to_add);
	g_return_if_fail (w1 >= offset_x + w2);
	g_return_if_fail (h1 >= offset_y + h2);

	p1 = gdk_pixbuf_get_pixels (pixbuf);
	p2 = gdk_pixbuf_get_pixels (pixbuf_to_add);
	r1 = gdk_pixbuf_get_rowstride (pixbuf);
	r2 = gdk_pixbuf_get_rowstride (pixbuf_to_add);
	for (row = 0; row < h2; row++) {
		for (col = 0; col < w2; col++) {
			pos1 = (row + offset_y) * r1 + (col + offset_x) * 4;
			pos2 = row * r2 + col * 4;
			a1 = p1[pos1 + 3];
			a2 = p2[pos2 + 3];

			for (i = 0; i < 3; i++) {
				p1[pos1 + i] *=
					((gfloat) (0xff - a2) / (gfloat) 0xff);
				p1[pos1 + i] += (p2[pos2 + i] * 
					((gfloat) a2 / (gfloat) 0xff));
			}
			p1[pos1 + 3] = MAX (a1, a2);
		}
	}
}

/*! \note
 *  The following function was taken from GQview, and is covered by this copyright. 
 *  The name of the function was changed from pixbuf_copy_mirror to 
 *  gdk_pixbuf_mirror_rotate.
 *
 * GQview
 * (C) 2002 John Ellis
 *
 * Author: John Ellis
 *
 * This software is released under the GNU General Public License (GNU GPL).
 * Please read the included file COPYING for more information.
 * This software comes with no warranty of any kind, use at your own risk!
 */

/*! \brief Mirror and flip a GdkPixbuf.
 *  \par Function Description
 *  This function will mirror and/or flip the source GdkPixbuf.
 *  To do a 180 degree rotation, set both mirror and flip to TRUE.
 *  If mirror and flip are FALSE, the source image is copied.
 *
 *  \param [in] src     Source image to operate on.
 *  \param [in] mirror  Mirror image if set to TRUE.
 *  \param [in] flip    Flipped image if set to TRUE.
 *  \return Updated copy of source image with operations performed.
 */
GdkPixbuf *gdk_pixbuf_mirror_flip(GdkPixbuf *src, gint mirror, gint flip)
{
	GdkPixbuf *dest;
	gint has_alpha;
	gint w, h, srs;
	gint drs;
	guchar *s_pix;
        guchar *d_pix;
	guchar *sp;
        guchar *dp;
	gint i, j;
	gint a;

	if (!src) return NULL;

	w = gdk_pixbuf_get_width(src);
	h = gdk_pixbuf_get_height(src);
	has_alpha = gdk_pixbuf_get_has_alpha(src);
	srs = gdk_pixbuf_get_rowstride(src);
	s_pix = gdk_pixbuf_get_pixels(src);

	dest = gdk_pixbuf_new(GDK_COLORSPACE_RGB, has_alpha, 8, w, h);
	drs = gdk_pixbuf_get_rowstride(dest);
	d_pix = gdk_pixbuf_get_pixels(dest);

	a = has_alpha ? 4 : 3;

	for (i = 0; i < h; i++)
		{
		sp = s_pix + (i * srs);
		if (flip)
			{
			dp = d_pix + ((h - i - 1) * drs);
			}
		else
			{
			dp = d_pix + (i * drs);
			}
		if (mirror)
			{
			dp += (w - 1) * a;
			for (j = 0; j < w; j++)
				{
				*(dp++) = *(sp++);	/* r */
				*(dp++) = *(sp++);	/* g */
				*(dp++) = *(sp++);	/* b */
				if (has_alpha) *(dp) = *(sp++);	/* a */
				dp -= (a + 3);
				}
			}
		else
			{
			for (j = 0; j < w; j++)
				{
				*(dp++) = *(sp++);	/* r */
				*(dp++) = *(sp++);	/* g */
				*(dp++) = *(sp++);	/* b */
				if (has_alpha) *(dp++) = *(sp++);	/* a */
				}
			}
		}

	return dest;
}
