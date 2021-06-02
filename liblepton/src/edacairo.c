/* libleptonrenderer - Rendering Lepton EDA schematics with Cairo
 * Copyright (C) 1998-2015 gEDA Contributors
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>

#include <cairo.h>
#include <math.h>

#include <liblepton/liblepton.h>
#include <liblepton/edacairo.h>

/* We don't use gettext */
#define _(x) (x)

static inline int
screen_width (cairo_t *cr, double width)
{
  double dummy = 0;
  cairo_user_to_device_distance (cr, &width, &dummy);
  if (width < 1)
    width = 1;

  return rint (width);
}

static inline int
SCREENabs (cairo_t *cr, double dist)
{
  double dummy = 0;
  cairo_user_to_device_distance (cr, &dist, &dummy);
  return rint (dist);
}

static inline void
WORLDtoSCREEN (cairo_t *cr, double wx, double wy, double *sx, double *sy)
{
  cairo_user_to_device (cr, &wx, &wy);
  *sx = round (wx); *sy = round (wy);
}

void
eda_cairo_set_source_color (cairo_t *cr, int color, GArray *map)
{
  LeptonColor c;

  g_return_if_fail (color >= 0);
  g_return_if_fail (map != NULL);
  g_return_if_fail ((color >= 0) && ((int) map->len > color));

  c = g_array_index (map, LeptonColor, color);

  cairo_set_source_rgba (cr, c.red,
                             c.green,
                             c.blue,
                             c.alpha);
}

void
eda_cairo_line (cairo_t *cr, int flags, int line_end,
                double w_line_width,
                double w_x1, double w_y1, double w_x2, double w_y2)
{
  double x1, y1, x2, y2;
  int line_width;
  double offset;
  double xoffset = 0;
  double yoffset = 0;
  double horizontal = 0;
  double vertical = 0;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    cairo_move_to (cr, w_x1, w_y1);
    cairo_line_to (cr, w_x2, w_y2);
    return;
  }

  WORLDtoSCREEN (cr, w_x1, w_y1, &x1, &y1);
  WORLDtoSCREEN (cr, w_x2, w_y2, &x2, &y2);
  line_width = screen_width (cr, w_line_width);
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

  x1 += xoffset; y1 += yoffset;
  x2 += xoffset; y2 += yoffset;
  cairo_device_to_user (cr, &x1, &y1);
  cairo_device_to_user (cr, &x2, &y2);
  cairo_move_to (cr, x1, y1);
  cairo_line_to (cr, x2, y2);
}


void
eda_cairo_box (cairo_t *cr, int flags, double line_width,
               double x1, double y1, double x2, double y2)
{
  int s_line_width;
  double s_x1, s_y1, s_x2, s_y2;
  double offset;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    cairo_rectangle (cr, x1, y1, (x2 - x1), (y2 - y1));
    return;
  }

  WORLDtoSCREEN (cr, x1, y1, &s_x1, &s_y1);
  WORLDtoSCREEN (cr, x2, y2, &s_x2, &s_y2);
  s_line_width = screen_width (cr, line_width);
  offset = (line_width == -1 || (s_line_width % 2) == 0) ? 0 : 0.5;

  /* Allow filled boxes (inferred from line_width == -1)
   * to touch an extra pixel, so the filled span is inclusive */
  if (line_width == -1) {
    if (s_x1 > s_x2) s_x1 += 1; else s_x2 += 1;
    if (s_y1 > s_y2) s_y1 += 1; else s_y2 += 1;
  }

  s_x1 += offset; s_y1 += offset;
  s_x2 += offset; s_y2 += offset;
  cairo_device_to_user (cr, &s_x1, &s_y1);
  cairo_device_to_user (cr, &s_x2, &s_y2);
  cairo_move_to (cr, s_x2, s_y2);
  cairo_line_to (cr, s_x1, s_y2);
  cairo_line_to (cr, s_x1, s_y1);
  cairo_line_to (cr, s_x2, s_y1);
  cairo_close_path (cr);
}


void
eda_cairo_center_box (cairo_t *cr, int flags,
                      double center_width,
                      double line_width, double x, double y,
                      double half_width, double half_height)
{
  int s_center_width, s_line_width;
  int s_width, s_height;
  double s_half_width, s_half_height;
  double s_x, s_y;
  double even_center_width;
  double even_line_width;
  double even_width, even_height;
  double x1, y1, x2, y2;
  double center_offset;
  int do_width_hint = TRUE;
  int do_height_hint = TRUE;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    cairo_rectangle (cr, (x - half_width), (y - half_height),
                     2*half_width, 2*half_height);
    return;
  }

  WORLDtoSCREEN (cr, x, y, &s_x, &s_y);
  s_width  = SCREENabs (cr, 2 * half_width);
  s_height = SCREENabs (cr, 2 * half_height);
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
  s_center_width = screen_width (cr, center_width);
  even_center_width = (center_width == -1 || (s_center_width % 2) == 0);
  center_offset = even_center_width ? 0. : 0.5;

  /* Hint the half-widths to land the stroke on the pixel grid */
  s_line_width = screen_width (cr, line_width);
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

  cairo_device_to_user (cr, &x1, &y1);
  cairo_device_to_user (cr, &x2, &y2);
  cairo_move_to (cr, x2, y2);
  cairo_line_to (cr, x1, y2);
  cairo_line_to (cr, x1, y1);
  cairo_line_to (cr, x2, y1);
  cairo_close_path (cr);
}


static inline void
do_arc (cairo_t *cr, double x, double y, double radius,
        double start_angle, double sweep_angle)
{
  cairo_new_sub_path (cr);
  if (sweep_angle > 0) {
    cairo_arc (cr, x, y, radius, start_angle * (M_PI / 180.),
                   (start_angle + sweep_angle) * (M_PI / 180.));
  } else {
    cairo_arc_negative (cr, x, y, radius, start_angle * (M_PI / 180.),
                            (start_angle + sweep_angle) * (M_PI / 180.));
  }
}


void
eda_cairo_arc (cairo_t *cr, int flags,
               double width, double x, double y,
               double radius, double start_angle, double sweep_angle)
{
  int s_width;
  double x1, y1, x2, y2;
  double s_x, s_y, s_radius;
  double offset, dummy = 0;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    do_arc (cr, x, y, radius, start_angle, sweep_angle);
    return;
  }

  WORLDtoSCREEN (cr, x - radius, y + radius, &x1, &y1);
  WORLDtoSCREEN (cr, x + radius, y - radius, &x2, &y2);
  s_width = screen_width (cr, width);
  offset = ((s_width % 2) == 0) ? 0 : 0.5;

  s_x = (double)(x1 + x2) / 2.;
  s_y = (double)(y1 + y2) / 2.;
  s_radius = (double)(y2 - y1) / 2.;

  cairo_device_to_user (cr, &s_x, &s_y);
  cairo_device_to_user_distance (cr, &offset, &dummy);
  cairo_device_to_user_distance (cr, &s_radius, &dummy);

  do_arc (cr, s_x + offset, s_y - offset,
          s_radius, start_angle, sweep_angle);
}


void
eda_cairo_center_arc (cairo_t *cr, int flags,
                      double center_width,
                      double line_width, double x, double y,
                      double radius, double start_angle, double sweep_angle)
{
  int s_center_width, s_line_width;
  double s_x, s_y, dummy = 0;
  int s_diameter;
  double even_center_width;
  double even_line_width;
  double even_diameter;
  double center_offset;
  double s_radius;
  int do_radius_hint = TRUE;

  if (!(flags & EDA_CAIRO_ENABLE_HINTS)) {
    do_arc (cr, x, y, radius, start_angle, sweep_angle);
    return;
  }

  WORLDtoSCREEN (cr, x, y, &s_x, &s_y);
  s_diameter = SCREENabs (cr, 2 * radius);
  even_diameter = ((s_diameter % 2) == 0);
  s_radius = (double) s_diameter / 2.;

  /* Switch off radius hinting for small radii. If we don't, then we get
   * a very abrupt transition once the arc reaches a single pixel size. */
  if (s_radius <= 1.) do_radius_hint = FALSE;

  /* Hint the center of the arc based on where a line
   * of thickness center_width (world) would drawn */
  s_center_width = screen_width (cr, center_width);
  even_center_width = (center_width == -1 || (s_center_width % 2) == 0);
  center_offset = even_center_width ? 0. : 0.5;

  /* Hint the radius to land its extermity on the pixel grid */
  s_line_width = screen_width (cr, line_width);
  even_line_width = (line_width == -1 || (s_line_width % 2) == 0);
  if (do_radius_hint)
    s_radius += ((even_center_width ==
                        even_line_width) == even_diameter) ? 0. : 0.5;

  s_x += center_offset;
  s_y += center_offset;
  cairo_device_to_user (cr, &s_x, &s_y);
  cairo_device_to_user_distance (cr, &s_radius, &dummy);

  do_arc (cr, s_x, s_y, s_radius, start_angle, sweep_angle);
}


void
eda_cairo_stroke (cairo_t *cr, int flags, int line_type, int line_end,
                  double wwidth, double wlength, double wspace)
{
  double offset = 0;
  double dashes[4];
  double dummy = 0;
  cairo_line_cap_t cap;
  cairo_line_cap_t round_cap_if_legible = CAIRO_LINE_CAP_ROUND;
  cairo_line_join_t join = CAIRO_LINE_JOIN_MITER;
  int num_dashes;
  int iwidth;
  double width = wwidth, length = wlength, space = wspace;

  if (flags & EDA_CAIRO_ENABLE_HINTS) {
    width  = iwidth = screen_width (cr, wwidth);
    length = screen_width (cr, wlength);
    space  = screen_width (cr, wspace);
    cairo_device_to_user_distance (cr, &width, &dummy);
    cairo_device_to_user_distance (cr, &length, &dummy);
    cairo_device_to_user_distance (cr, &space, &dummy);

    offset = ((iwidth % 2) == 0) ? 0 : 0.5;

    round_cap_if_legible =
      (iwidth <= 1) ? CAIRO_LINE_CAP_SQUARE : CAIRO_LINE_CAP_ROUND;
  }

  switch (line_end) {
    case END_NONE:
      cap = CAIRO_LINE_CAP_BUTT;
      join = CAIRO_LINE_JOIN_BEVEL;
      break;
    case END_SQUARE:
      cap = CAIRO_LINE_CAP_SQUARE;
      join = CAIRO_LINE_JOIN_MITER;
      break;
    case END_ROUND:
      cap = round_cap_if_legible;
      join = CAIRO_LINE_JOIN_ROUND;
      break;
    default:
      g_warn_if_reached ();
      cap = CAIRO_LINE_CAP_BUTT;
      join = CAIRO_LINE_JOIN_MITER;
    break;
  }

  cairo_set_line_width (cr, width);
  cairo_set_line_join (cr, join);

  switch (line_type) {

    default:
      g_warn_if_reached ();
      /* Fall through */

    case TYPE_SOLID:
      num_dashes = 0;

      cairo_set_dash (cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (cr, cap);
      cairo_stroke (cr);
      break;

    case TYPE_DOTTED:
      dashes[0] = 0;                    /* DOT */
      dashes[1] = space;
      num_dashes = 2;

      cairo_set_dash (cr, dashes, num_dashes, offset);
      cairo_set_line_cap (cr, round_cap_if_legible);
      cairo_stroke (cr);
      break;

    case TYPE_DASHED:
      dashes[0] = length;               /* DASH */
      dashes[1] = space;
      num_dashes = 2;

      cairo_set_dash (cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke (cr);
      break;

    case TYPE_CENTER:
      dashes[0] = length;               /* DASH */
      dashes[1] = 2 * space;
      num_dashes = 2;

      cairo_set_dash (cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke_preserve (cr);

      dashes[0] = 0;                    /* DOT */
      dashes[1] = 2 * space + length;
      num_dashes = 2;

      cairo_set_dash (cr, dashes, num_dashes, -length - space + offset);
      cairo_set_line_cap (cr, round_cap_if_legible);
      cairo_stroke (cr);
      break;

    case TYPE_PHANTOM:
      dashes[0] = length;               /* DASH */
      dashes[1] = 3 * space;
      num_dashes = 2;

      cairo_set_dash (cr, dashes, num_dashes, 0.);
      cairo_set_line_cap (cr, CAIRO_LINE_CAP_BUTT);
      cairo_stroke_preserve (cr);

      dashes[0] = 0;                    /* DOT */
      dashes[1] = space;
      dashes[2] = 0;                    /* DOT */
      dashes[3] = 2 * space + length;
      num_dashes = 4;

      cairo_set_dash (cr, dashes, num_dashes, -length - space + offset);
      cairo_set_line_cap (cr, round_cap_if_legible);
      cairo_stroke (cr);
      break;
  }

  cairo_set_dash (cr, NULL, 0, 0.);
}

static inline void
eda_cairo_path_hint (cairo_t *cr, int flags,
                     double *x, double *y, int width)
{
  double offset;
  if (flags & EDA_CAIRO_ENABLE_HINTS) {
    cairo_user_to_device (cr, x, y);
    offset = ((width % 2) == 0) ? 0 : 0.5;
    *x += offset; *y += offset;
    cairo_device_to_user (cr, x, y);
  }
}

void
eda_cairo_path (cairo_t *cr,
                int flags,
                double line_width,
                int nsections,
                LeptonPathSection *sections)
{
  int i;
  int s_line_width;
  double dummy = 0;

  if (flags & EDA_CAIRO_ENABLE_HINTS) {
    if (line_width == 0) {
      s_line_width = 1;
    } else {
      s_line_width = screen_width (cr, line_width);
    }
  } else {
    cairo_user_to_device (cr, &line_width, &dummy);
    s_line_width = line_width;
  }

  for (i = 0; i < nsections; i++) {
    LeptonPathSection *section = sections + i;
    double x1 = section->x1;
    double x2 = section->x2;
    double x3 = section->x3;
    double y1 = section->y1;
    double y2 = section->y2;
    double y3 = section->y3;

    switch (section->code) {
      case PATH_CURVETO:
        /* Two control point grips */
        eda_cairo_path_hint (cr, flags, &x1, &y1, s_line_width);
        eda_cairo_path_hint (cr, flags, &x2, &y2, s_line_width);
        /* Fall through */
      case PATH_MOVETO:
      case PATH_MOVETO_OPEN:
      case PATH_LINETO:
        /* Destination point grip */
        eda_cairo_path_hint (cr, flags, &x3, &y3, s_line_width);
      case PATH_END:
        break;
    }

    switch (section->code) {
      case PATH_MOVETO:
        cairo_close_path (cr);
        /* fall-through */
      case PATH_MOVETO_OPEN:
        cairo_move_to (cr, x3, y3);
        break;
      case PATH_CURVETO:
        cairo_curve_to (cr, x1, y1, x2, y2, x3, y3);
        break;
      case PATH_LINETO:
        cairo_line_to (cr, x3, y3);
        break;
      case PATH_END:
        cairo_close_path (cr);
        break;
    }
  }
}
