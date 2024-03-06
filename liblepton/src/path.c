/* Lepton EDA library
 * Copyright (C) 1998-2016 gEDA Contributors
 * Copyright (C) 2017-2024 Lepton EDA Contributors
 *
 * Code originally from librsvg 2.22.2 (LGPL) Copyright (C) 2000 Eazel, Inc.
 *
 *   Author: Raph Levien <raph@artofcode.com>
 *     rsvg-path.c:       Parse SVG path element data into bezier path.
 *     rsvg-bpath-util.c: Data structure and convenience functions for
 *                        creating bezier paths.
 *
 *  Adapted for gEDA by Peter Clifton <pcjc2@cam.ac.uk>
 *
 *  THIS FILE IS LGPL LICENSED, gEDA AS A WHOLE IS GPL LICENSED
 *
 * This program is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Library General Public License as
 *  published by the Free Software Foundation; either version 2 of the
 *  License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public
 *  License along with this program; if not, write to the
 *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301 USA
 *
 */
/*! \file path.c
 */

#include "config.h"

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "liblepton_priv.h"

#define NUM_BEZIER_SEGMENTS 100


/*! \brief Create a new LeptonPath structure.
 *
 *  \return The newly created LeptonPath structure.
 */
LeptonPath*
lepton_path_new ()
{
  LeptonPath *path;

  path = g_new (LeptonPath, 1);
  path->num_sections = 0;
  path->num_sections_max = 16;
  path->sections = g_new (LeptonPathSection, path->num_sections_max);

  return path;
}


/*! \brief Free memory associated with the LeptonPath object.
 *
 *  \param [in] path The #LeptonPath object to be freed.
 */
void
lepton_path_free (LeptonPath * path)
{
  if (path != NULL) {
    g_free (path->sections);
    g_free (path);
  }
}

/*! \brief Get the code type of path section.
 *  \par Function Description
 *  This function returns the PATH_CODE name of a
 *  #LeptonPathSection object.
 *
 *  \param [in] section The #LeptonPathSection object.
 *  \return PATH_CODE enum value of the path section object.
 */
PATH_CODE
lepton_path_section_get_code (LeptonPathSection *section)
{
  return section->code;
}

/*! \brief Get X coordinate of the first point of path section.
 *  \par Function Description
 *  Given a #LeptonPathSection variable, this getter returns the X
 *  coordinate of its first point.
 *
 *  \param [in] section The #LeptonPathSection variable.
 *  \return The X coordinate value of the first point of the path
 *  section.
 */
int
lepton_path_section_get_x1 (LeptonPathSection *section)
{
  return section->x1;
}

/*! \brief Get Y coordinate of the first point of path section.
 *  \par Function Description
 *  Given a #LeptonPathSection variable, this getter returns the Y
 *  coordinate of its first point.
 *
 *  \param [in] section The #LeptonPathSection variable.
 *  \return The Y coordinate value of the first point of the path
 *  section.
 */
int
lepton_path_section_get_y1 (LeptonPathSection *section)
{
  return section->y1;
}

/*! \brief Get X coordinate of the second point of path section.
 *  \par Function Description
 *  Given a #LeptonPathSection variable, this getter returns the X
 *  coordinate of its second point.
 *
 *  \param [in] section The #LeptonPathSection variable.
 *  \return The X coordinate value of the second point of the path
 *  section.
 */
int
lepton_path_section_get_x2 (LeptonPathSection *section)
{
  return section->x2;
}

/*! \brief Get Y coordinate of the second point of path section.
 *  \par Function Description
 *  Given a #LeptonPathSection variable, this getter returns the Y
 *  coordinate of its second point.
 *
 *  \param [in] section The #LeptonPathSection variable.
 *  \return The Y coordinate value of the second point of the path
 *  section.
 */
int
lepton_path_section_get_y2 (LeptonPathSection *section)
{
  return section->y2;
}

/*! \brief Get X coordinate of the third point of path section.
 *  \par Function Description
 *  Given a #LeptonPathSection variable, this getter returns the X
 *  coordinate of its third point.
 *
 *  \param [in] section The #LeptonPathSection variable.
 *  \return The X coordinate value of the third point of the path
 *  section.
 */
int
lepton_path_section_get_x3 (LeptonPathSection *section)
{
  return section->x3;
}

/*! \brief Get Y coordinate of the third point of path section.
 *  \par Function Description
 *  Given a #LeptonPathSection variable, this getter returns the Y
 *  coordinate of its third point.
 *
 *  \param [in] section The #LeptonPathSection variable.
 *  \return The Y coordinate value of the third point of the path
 *  section.
 */
int
lepton_path_section_get_y3 (LeptonPathSection *section)
{
  return section->y3;
}

/*! \brief Return a path section code enum value from a string.
 * \par Function Description
 * Given a string \a s, returns the #PATH_CODE enum value
 * corresponding to it.  This is mainly intended to be used for
 * value conversion in Scheme FFI functions.
 *
 * \param [in] s The string.
 */
int
lepton_path_section_code_from_string (char *s)
{
  int result = PATH_END;

  if      (strcmp (s, "moveto") == 0) { result = PATH_MOVETO; }
  else if (strcmp (s, "lineto") == 0) { result = PATH_LINETO; }
  else if (strcmp (s, "curveto") == 0) {result = PATH_CURVETO; }
  else if (strcmp (s, "closepath") == 0) {result = PATH_END; }

  return result;
}

/*! \brief Return a string holding the representation of #PATH_CODE value.
 * \par Function Description
 * Given a #PATH_CODE value, returns its external representation
 * as a string.  This is mainly intended to be used in Scheme FFI
 * functions.
 *
 *  \param [in] code The #PATH_CODE value.
 */
const char*
lepton_path_section_code_to_string (int code)
{
  const char *result = NULL;

  switch (code)
  {
  case PATH_MOVETO:
  case PATH_MOVETO_OPEN: result = "moveto"; break;
  case PATH_LINETO: result = "lineto"; break;
  case PATH_CURVETO: result = "curveto"; break;
  case PATH_END: result = "closepath"; break;
  default: break;
  }

  return result;
}

/*! \brief Create a new "moveto" section in #LeptonPath object.
 *  \par Function Description
 *  The function modifies #LeptonPath object by adding a new
 *  "moveto" section with given coordinates x and y.  If the last
 *  section of the path is already a "moveto" one, amend it
 *  instead of adding a new section.
 *
 *  \param [in,out] path The #LeptonPath object.
 *  \param [in] x The X coordinate of the "moveto" point.
 *  \param [in] y The Y coordinate of the "moveto" point.
 */
void
lepton_path_moveto (LeptonPath *path,
                    double x,
                    double y)
{
  LeptonPathSection *sections;
  int num_sections;

  g_return_if_fail (path != NULL);

  /* if the last command was a moveto then change that last moveto instead of
     creating a new one */
  sections = path->sections;
  num_sections = path->num_sections;

  if (num_sections > 0)
    if (sections[num_sections - 1].code == PATH_MOVETO_OPEN) {
      sections[num_sections - 1].x3 = x;
      sections[num_sections - 1].y3 = y;
      return;
    }

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max)
    path->sections =
      (LeptonPathSection*) g_realloc (path->sections,
                                      (path->num_sections_max <<= 1) *
                                      sizeof (LeptonPathSection));
  sections = path->sections;
  sections[num_sections].code = PATH_MOVETO_OPEN;
  sections[num_sections].x3 = x;
  sections[num_sections].y3 = y;
}


/*! \brief Create a new "lineto" section in #LeptonPath object.
 *  \par Function Description
 *  The function modifies #LeptonPath object by adding a new
 *  "lineto" section with given coordinates x and y.
 *
 *  \param [in,out] path The #LeptonPath object.
 *  \param [in] x The X coordinate of the "lineto" point.
 *  \param [in] y The Y coordinate of the "lineto" point.
 */
void
lepton_path_lineto (LeptonPath *path,
                    double x,
                    double y)
{
  LeptonPathSection *sections;
  int num_sections;

  g_return_if_fail (path != NULL);

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max)
    path->sections =
      (LeptonPathSection*) g_realloc (path->sections,
                                      (path->num_sections_max <<= 1) *
                                      sizeof (LeptonPathSection));
  sections = path->sections;
  sections[num_sections].code = PATH_LINETO;
  sections[num_sections].x3 = x;
  sections[num_sections].y3 = y;
}


/*! \brief Create a new "curveto" section in #LeptonPath object.
 *  \par Function Description
 *  The function modifies #LeptonPath object by adding a new
 *  "curveto" section with given coordinates of three control
 *  points.
 *
 *  \param [in,out] path The #LeptonPath object.
 *  \param [in] x1 The X coordinate of the first control point.
 *  \param [in] y1 The Y coordinate of the first control point.
 *  \param [in] x2 The X coordinate of the second control point.
 *  \param [in] y2 The Y coordinate of the second control point.
 *  \param [in] x3 The X coordinate of the third control point.
 *  \param [in] y3 The Y coordinate of the third control point.
 */
void
lepton_path_curveto (LeptonPath *path,
                     double x1,
                     double y1,
                     double x2,
                     double y2,
                     double x3,
                     double y3)
{
  LeptonPathSection *sections;
  int num_sections;

  g_return_if_fail (path != NULL);

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max)
    path->sections =
      (LeptonPathSection*) g_realloc (path->sections,
                                      (path->num_sections_max <<= 1) *
                                      sizeof (LeptonPathSection));
  sections = path->sections;
  sections[num_sections].code = PATH_CURVETO;
  sections[num_sections].x1 = x1;
  sections[num_sections].y1 = y1;
  sections[num_sections].x2 = x2;
  sections[num_sections].y2 = y2;
  sections[num_sections].x3 = x3;
  sections[num_sections].y3 = y3;
}


/*! \brief Finish #LeptonPath object creation.
 *  \par Function Description
 *  Finishes #LeptonPath object creation by adding a last ending
 *  section.
 *
 *  \param [in,out] path The #LeptonPath object.
 */
void
lepton_path_art_finish (LeptonPath * path)
{
  int num_sections;

  g_return_if_fail (path != NULL);

  num_sections = path->num_sections++;

  if (num_sections == path->num_sections_max)
    path->sections =
      (LeptonPathSection*) g_realloc (path->sections,
                                      (path->num_sections_max <<= 1) *
                                      sizeof (LeptonPathSection));
  path->sections[num_sections].code = PATH_END;
}


/* This module parses an SVG style path element into a LeptonPath.

  At present, there is no support for <marker> or any other contextual
  information from the SVG file. The API will need to change rather
  significantly to support these.

  Reference: SVG working draft 3 March 2000, section 8.
*/

typedef struct _RSVGParsePathCtx RSVGParsePathCtx;

struct _RSVGParsePathCtx {
  LeptonPath *path;
  double cpx, cpy;    /* current point */
  double rpx, rpy;    /* reflection point (for 's' and 't' commands) */
  double mpx, mpy;    /* Last moved to point (for path closures) */
  char cmd;           /* current command (lowercase) */
  int param;          /* parameter number */
  gboolean rel;       /* true if relative coords */
  double params[7];   /* parameters that have been parsed */
};


static void
lepton_path_arc_segment (RSVGParsePathCtx * ctx,
                         double xc,
                         double yc,
                         double th0,
                         double th1,
                         double rx,
                         double ry,
                         double x_axis_rotation)
{
  double sin_th, cos_th;
  double a00, a01, a10, a11;
  double x1, y1, x2, y2, x3, y3;
  double t;
  double th_half;

  sin_th = sin (x_axis_rotation * (M_PI / 180.0));
  cos_th = cos (x_axis_rotation * (M_PI / 180.0));
  /* inverse transform compared with lepton_path_arc */
  a00 = cos_th * rx;
  a01 = -sin_th * ry;
  a10 = sin_th * rx;
  a11 = cos_th * ry;

  th_half = 0.5 * (th1 - th0);
  t = (8.0 / 3.0) * sin (th_half * 0.5) * sin (th_half * 0.5) / sin (th_half);
  x1 = xc + cos (th0) - t * sin (th0);
  y1 = yc + sin (th0) + t * cos (th0);
  x3 = xc + cos (th1);
  y3 = yc + sin (th1);
  x2 = x3 + t * sin (th1);
  y2 = y3 - t * cos (th1);
  lepton_path_curveto (ctx->path,
                       a00 * x1 + a01 * y1, a10 * x1 + a11 * y1,
                       a00 * x2 + a01 * y2, a10 * x2 + a11 * y2,
                       a00 * x3 + a01 * y3, a10 * x3 + a11 * y3);
}


/*
 * lepton_path_arc: Add an arc to the path context.
 * @ctx: Path context.
 * @rx: Radius in x direction (before rotation).
 * @ry: Radius in y direction (before rotation).
 * @x_axis_rotation: Rotation angle for axes.
 * @large_arc_flag: 0 for arc length <= 180, 1 for arc >= 180.
 * @sweep: 0 for "negative angle", 1 for "positive angle".
 * @x: New x coordinate.
 * @y: New y coordinate.
 *
 */
static void
lepton_path_arc (RSVGParsePathCtx * ctx,
                 double rx,
                 double ry,
                 double x_axis_rotation,
                 int large_arc_flag,
                 int sweep_flag,
                 double x,
                 double y)
{
  double sin_th, cos_th;
  double a00, a01, a10, a11;
  double x0, y0, x1, y1, xc, yc;
  double d, sfactor, sfactor_sq;
  double th0, th1, th_arc;
  int i, n_segs;

  /* Check that neither radius is zero, since its isn't either
     geometrically or mathematically meaningful and will
     cause divide by zero and subsequent NaNs.  We should
     really do some ranged check ie -0.001 < x < 000.1 rather
     can just a straight check again zero.
   */
  if ((rx == 0.0) || (ry == 0.0))
    return;

  sin_th = sin (x_axis_rotation * (M_PI / 180.0));
  cos_th = cos (x_axis_rotation * (M_PI / 180.0));
  a00 = cos_th / rx;
  a01 = sin_th / rx;
  a10 = -sin_th / ry;
  a11 = cos_th / ry;
  x0 = a00 * ctx->cpx + a01 * ctx->cpy;
  y0 = a10 * ctx->cpx + a11 * ctx->cpy;
  x1 = a00 * x + a01 * y;
  y1 = a10 * x + a11 * y;
  /* (x0, y0) is current point in transformed coordinate space.
     (x1, y1) is new point in transformed coordinate space.

     The arc fits a unit-radius circle in this space.
   */
  d = (x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0);
  sfactor_sq = 1.0 / d - 0.25;
  if (sfactor_sq < 0)
    sfactor_sq = 0;
  sfactor = sqrt (sfactor_sq);
  if (sweep_flag == large_arc_flag)
    sfactor = -sfactor;
  xc = 0.5 * (x0 + x1) - sfactor * (y1 - y0);
  yc = 0.5 * (y0 + y1) + sfactor * (x1 - x0);
  /* (xc, yc) is center of the circle. */

  th0 = atan2 (y0 - yc, x0 - xc);
  th1 = atan2 (y1 - yc, x1 - xc);

  th_arc = th1 - th0;
  if (th_arc < 0 && sweep_flag)
    th_arc += 2 * M_PI;
  else if (th_arc > 0 && !sweep_flag)
    th_arc -= 2 * M_PI;

  n_segs = ceil (fabs (th_arc / (M_PI * 0.5 + 0.001)));

  for (i = 0; i < n_segs; i++)
    lepton_path_arc_segment (ctx,
                             xc, yc,
                             th0 + i * th_arc / n_segs,
                             th0 + (i + 1) * th_arc / n_segs,
                             rx, ry,
                             x_axis_rotation);

  ctx->cpx = x;
  ctx->cpy = y;
}


/* supply defaults for missing parameters, assuming relative coordinates
   are to be interpreted as x,y */
static void
lepton_path_parse_default_xy (RSVGParsePathCtx * ctx,
                              int n_params)
{
  int i;

  if (ctx->rel) {
    for (i = ctx->param; i < n_params; i++) {
      if (i > 2)
        ctx->params[i] = ctx->params[i - 2];
      else if (i == 1)
        ctx->params[i] = ctx->cpy;
      else if (i == 0)
        /* we shouldn't get here (usually ctx->param > 0 as
           precondition) */
        ctx->params[i] = ctx->cpx;
    }
  } else {
    for (i = ctx->param; i < n_params; i++)
      ctx->params[i] = 0.0;
  }
}


static void
lepton_path_parse_do_cmd (RSVGParsePathCtx *ctx,
                          gboolean final)
{
  double x1, y1, x2, y2, x3, y3;

  switch (ctx->cmd) {
  case 'm':
    /* moveto */
    if (ctx->param == 2 || final) {
      lepton_path_parse_default_xy (ctx, 2);
      lepton_path_moveto (ctx->path, ctx->params[0], ctx->params[1]);
      ctx->mpx = ctx->cpx = ctx->rpx = ctx->params[0];
      ctx->mpy = ctx->cpy = ctx->rpy = ctx->params[1];
      ctx->param = 0;
      ctx->cmd = 'l'; /* implicit linetos after a moveto */
    }
    break;
  case 'l':
    /* lineto */
    if (ctx->param == 2 || final) {
      lepton_path_parse_default_xy (ctx, 2);
      lepton_path_lineto (ctx->path, ctx->params[0], ctx->params[1]);
      ctx->cpx = ctx->rpx = ctx->params[0];
      ctx->cpy = ctx->rpy = ctx->params[1];
      ctx->param = 0;
    }
    break;
  case 'c':
    /* curveto */
    if (ctx->param == 6 || final) {
      lepton_path_parse_default_xy (ctx, 6);
      x1 = ctx->params[0];
      y1 = ctx->params[1];
      x2 = ctx->params[2];
      y2 = ctx->params[3];
      x3 = ctx->params[4];
      y3 = ctx->params[5];
      lepton_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = x2;
      ctx->rpy = y2;
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    }
    break;
  case 's':
    /* smooth curveto */
    if (ctx->param == 4 || final) {
      lepton_path_parse_default_xy (ctx, 4);
      x1 = 2 * ctx->cpx - ctx->rpx;
      y1 = 2 * ctx->cpy - ctx->rpy;
      x2 = ctx->params[0];
      y2 = ctx->params[1];
      x3 = ctx->params[2];
      y3 = ctx->params[3];
      lepton_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = x2;
      ctx->rpy = y2;
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    }
    break;
  case 'h':
    /* horizontal lineto */
    if (ctx->param == 1) {
      lepton_path_lineto (ctx->path, ctx->params[0], ctx->cpy);
      ctx->cpx = ctx->rpx = ctx->params[0];
      ctx->param = 0;
    }
    break;
  case 'v':
    /* vertical lineto */
    if (ctx->param == 1) {
      lepton_path_lineto (ctx->path, ctx->cpx, ctx->params[0]);
      ctx->cpy = ctx->rpy = ctx->params[0];
      ctx->param = 0;
    }
    break;
  case 'q':
    /* quadratic bezier curveto */

    /* non-normative reference:
       http://www.icce.rug.nl/erikjan/bluefuzz/beziers/beziers/beziers.html
     */
    if (ctx->param == 4 || final) {
      lepton_path_parse_default_xy (ctx, 4);
      /* raise quadratic bezier to cubic */
      x1 = (ctx->cpx + 2 * ctx->params[0]) * (1.0 / 3.0);
      y1 = (ctx->cpy + 2 * ctx->params[1]) * (1.0 / 3.0);
      x3 = ctx->params[2];
      y3 = ctx->params[3];
      x2 = (x3 + 2 * ctx->params[0]) * (1.0 / 3.0);
      y2 = (y3 + 2 * ctx->params[1]) * (1.0 / 3.0);
      lepton_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = ctx->params[0];
      ctx->rpy = ctx->params[1];
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    }
    break;
  case 't':
    /* Truetype quadratic bezier curveto */
    if (ctx->param == 2 || final) {
      double xc, yc;    /* quadratic control point */

      xc = 2 * ctx->cpx - ctx->rpx;
      yc = 2 * ctx->cpy - ctx->rpy;
      /* generate a quadratic bezier with control point = xc, yc */
      x1 = (ctx->cpx + 2 * xc) * (1.0 / 3.0);
      y1 = (ctx->cpy + 2 * yc) * (1.0 / 3.0);
      x3 = ctx->params[0];
      y3 = ctx->params[1];
      x2 = (x3 + 2 * xc) * (1.0 / 3.0);
      y2 = (y3 + 2 * yc) * (1.0 / 3.0);
      lepton_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
      ctx->rpx = xc;
      ctx->rpy = yc;
      ctx->cpx = x3;
      ctx->cpy = y3;
      ctx->param = 0;
    } else if (final) {
      if (ctx->param > 2) {
        lepton_path_parse_default_xy (ctx, 4);
        /* raise quadratic bezier to cubic */
        x1 = (ctx->cpx + 2 * ctx->params[0]) * (1.0 / 3.0);
        y1 = (ctx->cpy + 2 * ctx->params[1]) * (1.0 / 3.0);
        x3 = ctx->params[2];
        y3 = ctx->params[3];
        x2 = (x3 + 2 * ctx->params[0]) * (1.0 / 3.0);
        y2 = (y3 + 2 * ctx->params[1]) * (1.0 / 3.0);
        lepton_path_curveto (ctx->path, x1, y1, x2, y2, x3, y3);
        ctx->rpx = ctx->params[0];
        ctx->rpy = ctx->params[1];
        ctx->cpx = x3;
        ctx->cpy = y3;
      } else {
        lepton_path_parse_default_xy (ctx, 2);
        lepton_path_lineto (ctx->path, ctx->params[0], ctx->params[1]);
        ctx->cpx = ctx->rpx = ctx->params[0];
        ctx->cpy = ctx->rpy = ctx->params[1];
      }
      ctx->param = 0;
    }
    break;
  case 'a':
    if (ctx->param == 7 || final) {
      lepton_path_arc (ctx,
                       ctx->params[0],
                       ctx->params[1],
                       ctx->params[2],
                       ctx->params[3],
                       ctx->params[4],
                       ctx->params[5],
                       ctx->params[6]);
      ctx->param = 0;
    }
    break;
  default:
    ctx->param = 0;
  }
}


static void
lepton_path_parse_data (RSVGParsePathCtx *ctx,
                        const char *data)
{
  int i = 0;
  double val = 0;
  char c = 0;
  gboolean in_num = FALSE;
  gboolean in_frac = FALSE;
  gboolean in_exp = FALSE;
  gboolean exp_wait_sign = FALSE;
  int sign = 0;
  int exp = 0;
  int exp_sign = 0;
  double frac = 0.0;

  in_num = FALSE;
  for (i = 0;; i++) {
    c = data[i];
    if (c >= '0' && c <= '9') {
      /* digit */
      if (in_num) {
        if (in_exp) {
          exp = (exp * 10) + c - '0';
          exp_wait_sign = FALSE;
        } else if (in_frac)
          val += (frac *= 0.1) * (c - '0');
        else
          val = (val * 10) + c - '0';
      } else {
        in_num = TRUE;
        in_frac = FALSE;
        in_exp = FALSE;
        exp = 0;
        exp_sign = 1;
        exp_wait_sign = FALSE;
        val = c - '0';
        sign = 1;
      }
    } else if (c == '.') {
      if (!in_num) {
        in_num = TRUE;
        val = 0;
      }
      in_frac = TRUE;
      frac = 1;
    } else if ((c == 'E' || c == 'e') && in_num) {
      in_exp = TRUE;
      exp_wait_sign = TRUE;
      exp = 0;
      exp_sign = 1;
    } else if ((c == '+' || c == '-') && in_exp) {
      exp_sign = c == '+' ? 1 : -1;
    } else if (in_num) {
      /* end of number */

      val *= sign * pow (10, exp_sign * exp);
      if (ctx->rel) {
        /* Handle relative coordinates. This switch statement attempts
           to determine _what_ the coords are relative to. This is
           underspecified in the 12 Apr working draft. */
        switch (ctx->cmd) {
        case 'l':
        case 'm':
        case 'c':
        case 's':
        case 'q':
        case 't':
#ifndef RSVGV_RELATIVE
          /* rule: even-numbered params are x-relative, odd-numbered
             are y-relative */
          if ((ctx->param & 1) == 0)
            val += ctx->cpx;
          else if ((ctx->param & 1) == 1)
            val += ctx->cpy;
          break;
#else
          /* rule: even-numbered params are x-relative, odd-numbered
             are y-relative */
          if (ctx->param == 0 || (ctx->param % 2 == 0))
            val += ctx->cpx;
          else
            val += ctx->cpy;
          break;
#endif
        case 'a':
          /* rule: sixth and seventh are x and y, rest are not
             relative */
          if (ctx->param == 5)
            val += ctx->cpx;
          else if (ctx->param == 6)
            val += ctx->cpy;
          break;
        case 'h':
          /* rule: x-relative */
          val += ctx->cpx;
          break;
        case 'v':
          /* rule: y-relative */
          val += ctx->cpy;
          break;
        }
      }
      ctx->params[ctx->param++] = val;
      lepton_path_parse_do_cmd (ctx, FALSE);

      in_num = FALSE;
    }

    if (c == '\0')
      break;
    else if ((c == '+' || c == '-') && !exp_wait_sign) {
      sign = c == '+' ? 1 : -1;
      val = 0;
      in_num = TRUE;
      in_frac = FALSE;
      in_exp = FALSE;
      exp = 0;
      exp_sign = 1;
      exp_wait_sign = FALSE;
    } else if (c == 'z' || c == 'Z') {
      if (ctx->param)
        lepton_path_parse_do_cmd (ctx, TRUE);
      /* s_path_closepath (ctx->path); */
      /* lepton_path_lineto (ctx->path, ctx->mpx, ctx->mpy); */
      lepton_path_art_finish (ctx->path);

      ctx->cpx = ctx->rpx = ctx->path->sections[ctx->path->num_sections - 1].x3;
      ctx->cpy = ctx->rpy = ctx->path->sections[ctx->path->num_sections - 1].y3;
    } else if (c >= 'A' && c <= 'Z' && c != 'E') {
      if (ctx->param)
        lepton_path_parse_do_cmd (ctx, TRUE);
      ctx->cmd = c + 'a' - 'A';
      ctx->rel = FALSE;
    } else if (c >= 'a' && c <= 'z' && c != 'e') {
      if (ctx->param)
        lepton_path_parse_do_cmd (ctx, TRUE);
      ctx->cmd = c;
      ctx->rel = TRUE;
    }
    /* else c _should_ be whitespace or , */
  }
}


/*! \brief Return #LeptonPath pointer object from path string.
 *  \par Function Description
 *  Given a path string representation in gEDA file format creates
 *  a #LeptonPath object corresponding to it and returns the
 *  pointer to the object.
 *
 *  \param path_str [in] The representation of path as a string.
 *  \return The #LeptonPath object pointer.
 */
LeptonPath*
lepton_path_parse (const char *path_str)
{
  RSVGParsePathCtx ctx;

  ctx.path = lepton_path_new ();
  ctx.cpx = 0.0;
  ctx.cpy = 0.0;
  ctx.mpx = 0.0;
  ctx.mpy = 0.0;
  ctx.cmd = 0;
  ctx.param = 0;

  lepton_path_parse_data (&ctx, path_str);

  if (ctx.param)
    lepton_path_parse_do_cmd (&ctx, TRUE);

  return ctx.path;
}


/*! \brief Return string representation of #LeptonPath object.
 *  \par Function Description
 *  Given a #LeptonPath object pointer, returns its representation
 *  as a string in gEDA file format.
 *
 *  \param path [in] The #LeptonPath object pointer.
 *  \return The string representing the object.
 */
char*
lepton_path_string_from_path (const LeptonPath *path)
{
  LeptonPathSection *section;
  GString *path_string;
  int i;

  path_string = g_string_new ("");

  for (i = 0; i < path->num_sections; i++) {
    section = &path->sections[i];

    if (i > 0)
      g_string_append_c (path_string, '\n');

    switch (section->code) {
      case PATH_MOVETO:
        g_string_append_printf (path_string, "M %i,%i",
                                section->x3, section->y3);
        break;
      case PATH_MOVETO_OPEN:
        g_string_append_printf (path_string, "M %i,%i",
                                section->x3, section->y3);
        break;
      case PATH_CURVETO:
        g_string_append_printf (path_string, "C %i,%i %i,%i %i,%i",
                                section->x1, section->y1,
                                section->x2, section->y2,
                                section->x3, section->y3);
        break;
      case PATH_LINETO:
        g_string_append_printf (path_string, "L %i,%i",
                                section->x3, section->y3);
        break;
      case PATH_END:
        g_string_append_printf (path_string, "z");
        break;
    }
  }

  return g_string_free (path_string, FALSE);
}

/*! \brief Converts a path to a polygon
 *
 *  \param path [in] The path to convert to a polygon.  This parameter must not
 *  be NULL.
 *  \param points [out] An array of the polygon's vertices.  This parameter
 *  must not be NULL.
 *  \return TRUE if the path is closed, FALSE if it is open.
 */
int
lepton_path_to_polygon (LeptonPath *path,
                        GArray *points)
{
  int closed = FALSE;
  int i;
  LeptonPoint point = { 0, 0 };

  if (points->len > 0) {
    g_array_remove_range (points, 0, points->len - 1);
  }

  for (i = 0; i < path->num_sections; i++) {
    LeptonBezier bezier;
    LeptonPathSection *section = &path->sections[i];

    switch (section->code) {
      case PATH_CURVETO:
        bezier.x[0] = point.x;
        bezier.y[0] = point.y;
        bezier.x[1] = section->x1;
        bezier.y[1] = section->y1;
        bezier.x[2] = section->x2;
        bezier.y[2] = section->y2;
        point.x = bezier.x[3] = section->x3;
        point.y = bezier.y[3] = section->y3;
        m_polygon_append_bezier (points, &bezier, NUM_BEZIER_SEGMENTS);
        break;

      case PATH_MOVETO_OPEN:
        /* Unsupported, just fall through and draw a line */
        /* Fall through */

      case PATH_MOVETO:
      case PATH_LINETO:
        point.x = section->x3;
        point.y = section->y3;
        m_polygon_append_point (points, point.x, point.y);
        break;

      case PATH_END:
        closed = TRUE;
        break;
    }
  }

  return closed;
}


/*! \brief Calculates the distance between the given point and the closest
 *  point on the given path segment.
 *
 *  \param [in] path    The path.
 *  \param [in] x       The x coordinate of the given point.
 *  \param [in] y       The y coordinate of the given point.
 *  \param [in] solid   TRUE if the path should be treated as solid, FALSE if
 *  the path should be treated as hollow.
 *  \return The shortest distance from the path to the point.  With a solid
 *  shape, this function returns a distance of zero for interior points.  With
 *  an invalid parameter, this function returns G_MAXDOUBLE.
 */
double
lepton_path_shortest_distance (LeptonPath *path,
                               int x,
                               int y,
                               int solid)
{
  double shortest_distance = G_MAXDOUBLE;
  int closed;
  GArray *points;

  points = g_array_new (FALSE, FALSE, sizeof (LeptonPoint));

  closed = lepton_path_to_polygon (path, points);

  if (!solid) {
    shortest_distance = m_polygon_shortest_distance (points, x, y, closed);

  } else if (m_polygon_interior_point (points, x, y)) {
    shortest_distance = 0;

  } else {
    shortest_distance = m_polygon_shortest_distance (points, x, y, TRUE);
  }

  g_array_free (points, TRUE);

  return shortest_distance;
}
