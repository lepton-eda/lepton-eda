/* Lepton EDA library
 * Copyright (C) 1998-2010 Ales Hvezda
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */
/*! \file fill_type.h
 */

G_BEGIN_DECLS

/*! \brief The fill type of objects like box, circle, and path
 *
 *  The numeric values of this enumeration are used inside files and must be
 *  preserved for compatibility.
 */
enum _LeptonFillType
{
  FILLING_HOLLOW,
  FILLING_FILL,
  FILLING_MESH,
  FILLING_HATCH,
  FILLING_VOID
};

typedef enum _LeptonFillType LeptonFillType;

typedef struct _LeptonFill LeptonFill;

struct _LeptonFill
{
  LeptonFillType type;
  int width;
  int pitch1;
  int angle1;
  int pitch2;
  int angle2;
};



LeptonFill*
lepton_fill_new ();

void
lepton_fill_free (LeptonFill *fill);

LeptonFillType
lepton_fill_get_type (const LeptonFill *fill);

void
lepton_fill_set_type (LeptonFill *fill,
                      LeptonFillType type);
int
lepton_fill_get_width (const LeptonFill *fill);

void
lepton_fill_set_width (LeptonFill *fill,
                       int width);
int
lepton_fill_get_pitch1 (const LeptonFill *fill);

void
lepton_fill_set_pitch1 (LeptonFill *fill,
                        int pitch);
int
lepton_fill_get_angle1 (const LeptonFill *fill);

void
lepton_fill_set_angle1 (LeptonFill *fill,
                        int angle);
int
lepton_fill_get_pitch2 (const LeptonFill *fill);

void
lepton_fill_set_pitch2 (LeptonFill *fill,
                        int pitch);
int
lepton_fill_get_angle2 (const LeptonFill *fill);

void
lepton_fill_set_angle2 (LeptonFill *fill,
                        int angle);
const char*
lepton_fill_type_to_string (LeptonFillType type);

LeptonFillType
lepton_fill_type_from_string (char *s);

gboolean
lepton_fill_type_draw_first_hatch (int fill_type);

gboolean
lepton_fill_type_draw_second_hatch (int fill_type);

G_END_DECLS
