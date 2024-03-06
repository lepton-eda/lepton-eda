/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
 * Copyright (C) 1998-2015 gEDA Contributors
 * Copyright (C) 2021-2024 Lepton EDA Contributors
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
#include "liblepton_priv.h"

/*!
 * \file  fill.c
 * \brief Functions for working with object filling.
 */

#define DEFAULT_FILL_WIDTH 10
#define DEFAULT_FILL_PITCH1 100
#define DEFAULT_FILL_ANGLE1 45
#define DEFAULT_FILL_PITCH2 100
#define DEFAULT_FILL_ANGLE2 135


/*! \brief Init a new #LeptonFill.
 */
LeptonFill*
lepton_fill_new ()
{
  LeptonFill *fill;

  fill = g_new (LeptonFill, 1);
  fill->type = FILLING_HOLLOW;
  fill->width = DEFAULT_FILL_WIDTH;
  fill->pitch1 = DEFAULT_FILL_PITCH1;
  fill->angle1 = DEFAULT_FILL_ANGLE1;
  fill->pitch2 = DEFAULT_FILL_PITCH2;
  fill->angle2 = DEFAULT_FILL_ANGLE2;

  return fill;
}

/*! \brief Free a #LeptonFill.
 */
void
lepton_fill_free (LeptonFill *fill)
{
  g_free (fill);
}


/*! \brief Get the type of a fill.
 *
 *  \param [in] fill The fill.
 *  \return type The line type of the fill.
 */
LeptonFillType
lepton_fill_get_type (const LeptonFill *fill)
{
  g_return_val_if_fail (fill != NULL, FILLING_HOLLOW);

  return fill->type;
}

/*! \brief Set the type of a fill.
 *
 *  \param [in] fill The fill.
 *  \param [in] type The new fill type.
 */
void
lepton_fill_set_type (LeptonFill *fill,
                      LeptonFillType type)
{
  g_return_if_fail (fill != NULL);

  fill->type = type;
}


/*! \brief Get the width of a fill.
 *
 *  \param [in] fill The fill.
 *  \return The width of lines of the fill.
 */
int
lepton_fill_get_width (const LeptonFill *fill)
{
  g_return_val_if_fail (fill != NULL, DEFAULT_FILL_WIDTH);

  return fill->width;
}

/*! \brief Set the width of lines of a fill.
 *
 *  \param [in] fill The fill.
 *  \param [in] width The new fill line width.
 */
void
lepton_fill_set_width (LeptonFill *fill,
                       int width)
{
  g_return_if_fail (fill != NULL);

  fill->width = width;
}


/*! \brief Get the first pitch of a fill.
 *
 *  \param [in] fill The fill.
 *  \return The first pitch of the fill.
 */
int
lepton_fill_get_pitch1 (const LeptonFill *fill)
{
  g_return_val_if_fail (fill != NULL, DEFAULT_FILL_PITCH1);

  return fill->pitch1;
}

/*! \brief Set the pitch1 of a fill.
 *
 *  \param [in] fill   The fill.
 *  \param [in] pitch  The new fill first pitch.
 */
void
lepton_fill_set_pitch1 (LeptonFill *fill,
                        int pitch)
{
  g_return_if_fail (fill != NULL);
  g_return_if_fail (pitch != 0);

  fill->pitch1 = pitch;
}

/*! \brief Get the first angle of a fill.
 *
 *  \param [in] fill The fill.
 *  \return The first angle of the fill.
 */
int
lepton_fill_get_angle1 (const LeptonFill *fill)
{
  g_return_val_if_fail (fill != NULL, DEFAULT_FILL_ANGLE1);

  return fill->angle1;
}

/*! \brief Set the first angle of a fill.
 *
 *  \param [in] fill  The fill.
 *  \param [in] angle The new fill first angle.
 */
void
lepton_fill_set_angle1 (LeptonFill *fill,
                        int angle)
{
  g_return_if_fail (fill != NULL);

  fill->angle1 = angle;
}


/*! \brief Get the second pitch of a fill.
 *
 *  \param [in] fill The fill.
 *  \return The second pitch of the fill.
 */
int
lepton_fill_get_pitch2 (const LeptonFill *fill)
{
  g_return_val_if_fail (fill != NULL, DEFAULT_FILL_PITCH2);

  return fill->pitch2;
}

/*! \brief Set the second pitch of a fill.
 *
 *  \param [in] fill   The fill.
 *  \param [in] pitch  The new fill second pitch.
 */
void
lepton_fill_set_pitch2 (LeptonFill *fill,
                        int pitch)
{
  g_return_if_fail (fill != NULL);
  g_return_if_fail (pitch != 0);

  fill->pitch2 = pitch;
}


/*! \brief Get the second angle of a fill.
 *
 *  \param [in] fill The fill.
 *  \return The second angle of the fill.
 */
int
lepton_fill_get_angle2 (const LeptonFill *fill)
{
  g_return_val_if_fail (fill != NULL, DEFAULT_FILL_ANGLE2);

  return fill->angle2;
}

/*! \brief Set the second angle of a fill.
 *
 *  \param [in] fill  The fill.
 *  \param [in] angle The new second angle of the fill.
 */
void
lepton_fill_set_angle2 (LeptonFill *fill,
                        int angle)
{
  g_return_if_fail (fill != NULL);

  fill->angle2 = angle;
}


const char*
lepton_fill_type_to_string (LeptonFillType fill_type)
{
  const char *result = NULL;

  switch (fill_type)
  {
  case FILLING_HOLLOW: result = "hollow"; break;
  case FILLING_FILL:   result = "solid";  break;
  case FILLING_MESH:   result = "mesh";   break;
  case FILLING_HATCH:  result = "hatch";  break;
  default: break;
  }

  return result;
}


LeptonFillType
lepton_fill_type_from_string (char *s)
{
  LeptonFillType result = FILLING_HOLLOW;

  if      (strcmp (s, "hollow") == 0) { result = FILLING_HOLLOW; }
  else if (strcmp (s, "solid")  == 0) { result = FILLING_FILL;   }
  else if (strcmp (s, "mesh")   == 0) { result = FILLING_MESH;   }
  else if (strcmp (s, "hatch")  == 0) { result = FILLING_HATCH;  }

  return result;
}


/*! \brief Check if the first hatch pattern needs to be drawn
 *
 *  \param [in] fill_type The fill type
 *  \return TRUE if the first hatch pattern needs to be drawn
 */
gboolean
lepton_fill_type_draw_first_hatch (int fill_type)
{
  return ((fill_type == FILLING_MESH) || (fill_type == FILLING_HATCH));
}


/*! \brief Check if the second hatch pattern needs to be drawn
 *
 *  \param [in] fill_type The fill type
 *  \return TRUE if the second hatch pattern needs to be drawn
 */
gboolean
lepton_fill_type_draw_second_hatch (int fill_type)
{
  return (fill_type == FILLING_MESH);
}
