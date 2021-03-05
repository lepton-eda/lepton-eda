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

/*! \file stroke.h
 *
 *  \brief Line stroke style of objects such as arcs, boxes, circles, and lines.
 */

enum _LeptonStrokeCapType
{
  END_NONE,
  END_SQUARE,
  END_ROUND,
  END_VOID
};

typedef enum _LeptonStrokeCapType LeptonStrokeCapType;

enum _LeptonStrokeType
{
  TYPE_SOLID,
  TYPE_DOTTED,
  TYPE_DASHED,
  TYPE_CENTER,
  TYPE_PHANTOM,
  TYPE_ERASE
};

typedef enum _LeptonStrokeType LeptonStrokeType;


typedef struct _LeptonStroke LeptonStroke;

struct _LeptonStroke
{
  LeptonStrokeType line_type;
  LeptonStrokeCapType cap_type;
  int line_width;
  int line_space;
  int line_length;
};

LeptonStroke*
lepton_stroke_new ();

void
lepton_stroke_free (LeptonStroke * stroke);
