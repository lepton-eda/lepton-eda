/* gEDA - GPL Electronic Design Automation
 * gschem - gEDA Schematic Capture
 * Copyright (C) 1998-2007 Ales Hvezda
 * Copyright (C) 1998-2007 gEDA Contributors (see ChangeLog for details)
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

#include <cairo.h>
#include <math.h>

#include "gschem.h"

#ifdef HAVE_LIBDMALLOC
#include <dmalloc.h>
#endif


static inline int screen_width (GSCHEM_TOPLEVEL *w_current, int w_width)
{
  int width = SCREENabs (w_current, w_width);
  if (width < 1)
    width = 1;

  return width;
}

void gschem_cairo_line (GSCHEM_TOPLEVEL *w_current, int line_end,
                        int w_line_width,
                        int w_x1, int w_y1, int w_x2, int w_y2)
{
  int x1, y1, x2, y2, line_width;
  double offset;
  double xoffset = 0;
  double yoffset = 0;
  int horizontal = 0;
  int vertical = 0;

  WORLDtoSCREEN (w_current, w_x1, w_y1, &x1, &y1);
  WORLDtoSCREEN (w_current, w_x2, w_y2, &x2, &y2);
  line_width = screen_width (w_current, w_line_width);
  offset = ((line_width % 2) == 0) ? 0 : 0.5;

  if (y1 == y2) horizontal = 1;
  if (x1 == x2) vertical = 1;

  /* Hint so the length of the line runs along a pixel boundary */

  if (horizontal)
    yoffset = offset;
  else if (vertical)
    xoffset = offset;
  else
    xoffset = yoffset = offset;

  /* Now hint the ends of the lines */

  switch (line_end) {
    case END_NONE:
      /* Line terminates at the passed coordinate */

      /* Add an extra pixel to give an inclusive span */
      if (horizontal) {
        if (x1 > x2) x1 += 1; else x2 += 1;
      } else if (vertical) {
        if (y1 > y2) y1 += 1; else y2 += 1;
      }
      break;

    case END_SQUARE:
    case END_ROUND:
      /* Line terminates half a width away from the passed coordinate */
      if (horizontal) {
        xoffset = offset;
      } else if (vertical) {
        yoffset = offset;
      }
      break;
  }

  cairo_move_to (w_current->cr, x1 + xoffset, y1 + yoffset);
  cairo_line_to (w_current->cr, x2 + xoffset, y2 + yoffset);
}


void gschem_cairo_box (GSCHEM_TOPLEVEL *w_current, int line_width,
                       int x1, int y1, int x2, int y2)
{
  int s_line_width;
  int s_x1, s_y1, s_x2, s_y2;
  double offset;

  WORLDtoSCREEN (w_current, x1, y1, &s_x1, &s_y1);
  WORLDtoSCREEN (w_current, x2, y2, &s_x2, &s_y2);
  s_line_width = screen_width (w_current, line_width);
  offset = (line_width == -1 || (s_line_width % 2) == 0) ? 0 : 0.5;

  /* Allow filled boxes (inferred from line_width == -1)
   * to touch an extra pixel, so the filled span is inclusive */
  if (line_width == -1) {
    if (x1 > x2) x1 += 1; else x2 += 1;
    if (y1 > y2) y1 += 1; else y2 += 1;
  }

  cairo_move_to (w_current->cr, s_x2 + offset, s_y2 + offset);
  cairo_line_to (w_current->cr, s_x1 + offset, s_y2 + offset);
  cairo_line_to (w_current->cr, s_x1 + offset, s_y1 + offset);
  cairo_line_to (w_current->cr, s_x2 + offset, s_y1 + offset);
  cairo_close_path (w_current->cr);
}


void gschem_cairo_center_box (GSCHEM_TOPLEVEL *w_current,
                              int center_width,
                              int line_width, int x, int y,
                              int half_width, int half_height)
{
  int s_center_width, s_line_width;
  int s_width, s_height;
  double s_half_width, s_half_height;
  int s_x, s_y;
  int even_center_width;
  int even_line_width;
  int even_width, even_height;
  double x1, y1, x2, y2;
  double center_offset;
  int do_width_hint = TRUE;
  int do_height_hint = TRUE;

  WORLDtoSCREEN (w_current, x, y, &s_x, &s_y);
  s_width  = SCREENabs (w_current, 2 * half_width);
  s_height = SCREENabs (w_current, 2 * half_height);
  even_width  = (s_width % 2 == 0);
  even_height = (s_width % 2 == 0);
  s_half_width  = (double) s_width  / 2.;
  s_half_height = (double) s_height / 2.;

#if 0 /* Not as nice an effect as with arcs */
  /* Switch off radius hinting for small radii. If we don't, then we get
   * a very abrupt transition once the box reaches a single pixel size. */
  if (s_half_width  <= 1.)  do_width_hint  = FALSE;
  if (s_half_height <= 1.)  do_height_hint = FALSE;
#endif

  /* Hint the center of the box based on where a line
   * of thickness center_width (world) would drawn */
  s_center_width = screen_width (w_current, center_width);
  even_center_width = (center_width == -1 || (s_center_width % 2) == 0);
  center_offset = even_center_width ? 0. : 0.5;

  /* Hint the half-widths to land the stroke on the pixel grid */
  s_line_width = screen_width (w_current, line_width);
  even_line_width = (line_width == -1 || (s_line_width % 2) == 0);
  if (do_width_hint)
    s_half_width  += ((even_center_width ==
                             even_line_width) == even_width ) ? 0. : 0.5;
  if (do_height_hint)
    s_half_height += ((even_center_width ==
                             even_line_width) == even_height) ? 0. : 0.5;

  x1 = (double) s_x + center_offset - s_half_width;
  y1 = (double) s_y + center_offset - s_half_height;
  x2 = (double) s_x + center_offset + s_half_width;
  y2 = (double) s_y + center_offset + s_half_height;

  /* Allow filled boxes (inferred from line_width == -1)
   * to touch an extra pixel, so the filled span is inclusive */
  if (line_width == -1) {
    x2 += 1;  y2 += 1;
  }

  cairo_move_to (w_current->cr, x2, y2);
  cairo_line_to (w_current->cr, x1, y2);
  cairo_line_to (w_current->cr, x1, y1);
  cairo_line_to (w_current->cr, x2, y1);
  cairo_close_path (w_current->cr);
}


static inline void do_arc (cairo_t *cr, double x, double y, double radius,
                                        int start_angle, int end_angle)
{
  cairo_new_sub_path (cr);
  if (start_angle > start_angle + end_angle) {
    cairo_arc (cr, x, y, radius, -start_angle * (M_PI / 180.),
                   (-start_angle - end_angle) * (M_PI / 180.));
  } else {
    cairo_arc_negative (cr, x, y, radius, -start_angle * (M_PI / 180.),
                            (-start_angle - end_angle) * (M_PI / 180.));
  }
}


void gschem_cairo_arc (GSCHEM_TOPLEVEL *w_current,
                       int width, int x, int y,
                       int radius, int start_angle, int end_angle)
{
  int s_width;
  int x1, y1, x2, y2;
  double s_x, s_y, s_radius;
  double offset;

  WORLDtoSCREEN (w_current, x - radius, y + radius, &x1, &y1);
  WORLDtoSCREEN (w_current, x + radius, y - radius, &x2, &y2);
  s_width = screen_width (w_current, width);
  offset = ((s_width % 2) == 0) ? 0 : 0.5;

  s_x = (double)(x1 + x2) / 2.;
  s_y = (double)(y1 + y2) / 2.;
  s_radius = (double)(y2 - y1) / 2.;

  cairo_save (w_current->cr);
  cairo_translate (w_current->cr, s_x + offset, s_y + offset);

  /* Adjust for non-uniform X/Y scale factor. Note that the + 1
     allows for the case where x2 == x1 or y2 == y1 */
  cairo_scale (w_current->cr, (double)(x2 - x1 + 1) /
                              (double)(y2 - y1 + 1), 1.);

  do_arc (w_current->cr, 0., 0., (double) s_radius, start_angle, end_angle);

  cairo_restore (w_current->cr);
}


void gschem_cairo_center_arc (GSCHEM_TOPLEVEL *w_current,
                              int center_width,
                              int line_width, int x, int y,
                              int radius, int start_angle, int end_angle)
{
  int s_center_width, s_line_width;
  int s_x, s_y, s_diameter;
  int even_center_width;
  int even_line_width;
  int even_diameter;
  double center_offset;
  double s_radius;
  int do_radius_hint = TRUE;

  WORLDtoSCREEN (w_current, x, y, &s_x, &s_y);
  s_diameter = SCREENabs (w_current, 2 * radius);
  even_diameter = ((s_diameter % 2) == 0);
  s_radius = (double) s_diameter / 2.;

  /* Switch off radius hinting for small radii. If we don't, then we get
   * a very abrupt transition once the arc reaches a single pixel size. */
  if (s_radius <= 1.) do_radius_hint = FALSE;

  /* Hint the center of the arc based on where a line
   * of thickness center_width (world) would drawn */
  s_center_width = screen_width (w_current, center_width);
  even_center_width = (center_width == -1 || (s_center_width % 2) == 0);
  center_offset = even_center_width ? 0. : 0.5;

  /* Hint the radius to land its extermity on the pixel grid */
  s_line_width = screen_width (w_current, line_width);
  even_line_width = (line_width == -1 || (s_line_width % 2) == 0);
  if (do_radius_hint)
    s_radius += ((even_center_width ==
                        even_line_width) == even_diameter) ? 0. : 0.5;

  do_arc (w_current->cr, (double) s_x + center_offset,
                         (double) s_y + center_offset,
                         (double) s_radius,
                         start_angle, end_angle);
}


void gschem_cairo_stroke (GSCHEM_TOPLEVEL *w_current, int line_type, int line_end,
                          int wwidth, int wlength, int wspace)
{
  double offset;
  double dashes[4];
  cairo_line_cap_t cap;
  cairo_line_cap_t round_cap_if_legible;
  int num_dashes;
  int width, length, space;

  width  = screen_width (w_current, wwidth);
  length = SCREENabs (w_current, wlength);
  space  = SCREENabs (w_current, wspace);
  offset = ((width % 2) == 0) ? 0 : 0.5;

  cairo_set_line_width (w_current->cr, width);
  cairo_set_line_join (w_current->cr, CAIRO_LINE_JOIN_MITER);

  round_cap_if_legible = (width <= 1) ? CAIRO_LINE_CAP_SQUARE :
                                        CAIRO_LINE_CAP_ROUND;

  switch (line_end) {
    case END_NONE:   cap = CAIRO_LINE_CAP_BUTT;   break;
    case END_SQUARE: cap = CAIRO_LINE_CAP_SQUARE; break;
    case END_ROUND:  cap = round_cap_if_legible;  break;
    default:
      fprintf(stderr, _("Unknown end for line (%d)\n"), line_end);
      cap = CAIRO_LINE_CAP_BUTT;
    break;
  }

  switch (line_type) {

    default:
      fprintf(stderr, _("Unknown type for stroke (%d) !\n"), line_type);
      /* Fall through */

    case TYPE_SOLID:
      num_dashes = 0;

      cairo_set_dash (w_current->cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (w_current->cr, cap);
      cairo_stroke (w_current->cr);
      break;

    case TYPE_DOTTED:
      dashes[0] = 0;                    /* DOT */
      dashes[1] = space;
      num_dashes = 2;

      cairo_set_dash (w_current->cr, dashes, num_dashes, offset);
      cairo_set_line_cap (w_current->cr, round_cap_if_legible);
      cairo_stroke (w_current->cr);
      break;

    case TYPE_DASHED:
      dashes[0] = length;               /* DASH */
      dashes[1] = space;
      num_dashes = 2;

      cairo_set_dash (w_current->cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (w_current->cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke (w_current->cr);
      break;

    case TYPE_CENTER:
      dashes[0] = length;               /* DASH */
      dashes[1] = 2 * space;
      num_dashes = 2;

      cairo_set_dash (w_current->cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (w_current->cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke_preserve (w_current->cr);

      dashes[0] = 0;                    /* DOT */
      dashes[1] = 2 * space + length;
      num_dashes = 2;

      cairo_set_dash (w_current->cr, dashes, num_dashes, -length - space + offset);
      cairo_set_line_cap (w_current->cr, round_cap_if_legible);
      cairo_stroke (w_current->cr);
      break;

    case TYPE_PHANTOM:
      dashes[0] = length;               /* DASH */
      dashes[1] = 3 * space;
      num_dashes = 2;

      cairo_set_dash (w_current->cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (w_current->cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke_preserve (w_current->cr);

      dashes[0] = 0;                    /* DOT */
      dashes[1] = space;
      dashes[2] = 0;                    /* DOT */
      dashes[3] = 2 * space + length;
      num_dashes = 4;

      cairo_set_dash (w_current->cr, dashes, num_dashes, -length - space + offset);
      cairo_set_line_cap (w_current->cr, round_cap_if_legible);
      cairo_stroke (w_current->cr);
      break;
  }

  cairo_set_dash (w_current->cr, NULL, 0, 0.);
}


void gschem_cairo_set_source_color (GSCHEM_TOPLEVEL *w_current, COLOR *color)
{
  cairo_set_source_rgba (w_current->cr, (double)color->r / 255.0,
                                        (double)color->g / 255.0,
                                        (double)color->b / 255.0,
                                        (double)color->a / 255.0);
}
